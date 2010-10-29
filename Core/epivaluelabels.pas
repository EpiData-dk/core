unit epivaluelabels;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epicustombase, epidatafilestypes, variants, DOM;

type

  { TEpiCustomValueLabel }

  TEpiCustomValueLabel = class(TEpiCustomItem)
  private
    FIsMissingValue: boolean;
    FOrder: Integer;
    FLabel: TEpiTranslatedText;
    function GetValueAsString: string; virtual; abstract;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    function SaveToXml(Content: String; Lvl: integer): string; override;
    procedure LoadFromXml(Root: TDOMNode); override;
    property Order: integer read FOrder write FOrder;
    property TheLabel: TEpiTranslatedText read FLabel write FLabel;
    property IsMissingValue: boolean read FIsMissingValue write FIsMissingValue;
  end;

  { TEpiIntValueLabel }

  TEpiIntValueLabel = class(TEpiCustomValueLabel)
  private
    FValue: EpiInteger;
    function GetValueAsString: string; override;
  public
    procedure LoadFromXml(Root: TDOMNode); override;
    property Value: EpiInteger read FValue write FValue;
  end;

  { TEpiFloatValueLabel }

  TEpiFloatValueLabel = class(TEpiCustomValueLabel)
  private
    FValue: EpiFloat;
    function GetValueAsString: string; override;
  public
    procedure LoadFromXml(Root: TDOMNode); override;
    property Value: EpiFloat read FValue write FValue;
  end;

  { TEpiStringValueLabel }

  TEpiStringValueLabel = class(TEpiCustomValueLabel)
  private
    FValue: EpiString;
    function GetValueAsString: string; override;
  public
    procedure LoadFromXml(Root: TDOMNode); override;
    property Value: EpiString read FValue write FValue;
  end;


  { TEpiValueLabelSet }

  TValueLabelSetScope = (vlsInternal, vlsExternal);

  TEpiValueLabelSet = class(TEpiCustomList)
  { External Valuelabel Set Properties }
  private
    FExtId: string;
    FExtLabelField: string;
    FExtName: string;
    FExtValField: string;
  protected
    property    ExtName: string read FExtName write FExtName;
    property    ExtId: string read FExtId write FExtId;
    property    ExtValField: string read FExtValField write FExtValField;
    property    ExtLabelField: string read FExtLabelField write FExtLabelField;
  private
    FLabelScope: TValueLabelSetScope;
    FLabelType: TEpiFieldType;
    function    GetIsMissingValue(const AValue: variant): boolean;
    function    GetValueLabel(const AValue: variant): string;
    function    GetValueLabels(const index: integer): TEpiCustomValueLabel;
    procedure   SetLabelType(const AValue: TEpiFieldType);
  protected
    procedure   LoadInternal(Root: TDOMNode); virtual;
    function    SaveInternal(Lvl: integer): string; virtual;
    procedure   LoadExternal(Root: TDOMNode); virtual;
    function    SaveExternal(Lvl: integer): string; virtual;
    class function IdString: string; override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor  Destroy; override;
    function    XMLName: string; override;
    function    SaveToXml(Content: String; Lvl: integer): string; override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    function    NewValueLabel: TEpiCustomValueLabel;
    property    LabelScope: TValueLabelSetScope read FLabelScope write FLabelScope;
    property    LabelType: TEpiFieldType read FLabelType write SetLabelType;
    property    ValueLabels[Const index: integer]: TEpiCustomValueLabel read GetValueLabels;
    property    ValueLabel[Const AValue: variant]: string read GetValueLabel;
    property    IsMissingValue[Const AValue: variant]: boolean read GetIsMissingValue;
  end;

  { TEpiValueLabelSets }

  TEpiValueLabelSets = class(TEpiCustomList)
  private
    function    GetValueLabels(index: integer): TEpiValueLabelSet;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor  Destroy; override;
    function    XMLName: string; override;
    function    SaveToXml(Content: String; Lvl: integer): string; override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    function    NewValueLabelSet(ALabelType: TEpiFieldType): TEpiValueLabelSet;
    property    ValueLabels[index: integer]: TEpiValueLabelSet read GetValueLabels; default;
  end;

implementation

uses
  strutils, math;

{ TEpiCustomValueLabel }

constructor TEpiCustomValueLabel.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  IsMissingValue := false;
  FLabel := TEpiTranslatedText.Create(Self, rsLabel);
  RegisterClasses([FLabel]);
end;

function TEpiCustomValueLabel.SaveToXml(Content: String; Lvl: integer): string;
begin
  with TEpiValueLabelSet(Owner) do
  begin
    if LabelScope = vlsExternal then exit;
    if Items[0] = Self then
      result := Indent(Lvl) + '<' + rsInternal + '>' + LineEnding;
  end;

  // Print order and value:
  Result +=
    Indent(LvL + 1) +
      '<' + rsValueLabel +
         ' order="' + IntToStr(Order) +
        '" value="' + GetValueAsString;

  // Add missing if set
  if IsMissingValue then
    Result += '" missing="true">'
  else
    Result += '">';

  // Inset labels (language dependant)
  Result += LineEnding + FLabel.SaveToXml('', Lvl + 2) +
    Indent(Lvl + 1) + '</' + rsValueLabel + '>' + LineEnding;

  with TEpiValueLabelSet(Owner) do
    if Items[Count - 1] = Self then
      result += Indent(Lvl) + '</' + rsInternal + '>' + LineEnding;
end;

procedure TEpiCustomValueLabel.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMElement;
begin
  // Root = <ValueLabel>
  Node := TDOMElement(Root);

  Order    := StrToIntDef(Node.AttribStrings['order'], -1);
  TheLabel.LoadFromXml(Root);
end;

{ TEpiIntValueLabel }

function TEpiIntValueLabel.GetValueAsString: string;
begin
  Result := IntToStr(Value);
end;

procedure TEpiIntValueLabel.LoadFromXml(Root: TDOMNode);
begin
  inherited LoadFromXml(Root);
  Value := StrToInt(TDOMElement(Root).AttribStrings['value']);
end;

{ TEpiFloatValueLabel }

function TEpiFloatValueLabel.GetValueAsString: string;
begin
  Result := FloatToStr(Value);
end;

procedure TEpiFloatValueLabel.LoadFromXml(Root: TDOMNode);
begin
  inherited LoadFromXml(Root);
  Value := StrToFloat(TDOMElement(Root).AttribStrings['value']);
end;

{ TEpiStringValueLabel }

function TEpiStringValueLabel.GetValueAsString: string;
begin
  Result := Value;
end;

procedure TEpiStringValueLabel.LoadFromXml(Root: TDOMNode);
begin
  inherited LoadFromXml(Root);
  Value := UTF8Encode(TDOMElement(Root).AttribStrings['value']);
end;

{ TEpiValueLabelSet }

function OrderCompare(Item1, Item2: Pointer): Integer;
var
  VL1: TEpiCustomValueLabel absolute Item1;
  VL2: TEpiCustomValueLabel absolute Item2;
begin
  Result := VL1.Order - VL2.Order;
end;

function FloatCompare(Item1, Item2: Pointer): Integer;
var
  VL1: TEpiFloatValueLabel absolute Item1;
  VL2: TEpiFloatValueLabel absolute Item2;
begin
  if (Vl1.Order = -1) then
    Result := Math.Sign(VL1.Value - VL2.Value)
  else
    Result := OrderCompare(Item1, Item2);
end;

function IntCompare(Item1, Item2: Pointer): Integer;
var
  VL1: TEpiIntValueLabel absolute Item1;
  VL2: TEpiIntValueLabel absolute Item2;
begin
  if (Vl1.Order = -1) then
    Result := VL1.Value - VL2.Value
  else
    Result := OrderCompare(Item1, Item2);
end;

function StringCompare(Item1, Item2: Pointer): Integer;
var
  VL1: TEpiStringValueLabel absolute Item1;
  VL2: TEpiStringValueLabel absolute Item2;
begin
  if (Vl1.Order = -1) then
    Result := CompareStr(VL1.Value, VL2.Value)
  else
    Result := OrderCompare(Item1, Item2);
end;

procedure TEpiValueLabelSet.SetLabelType(const AValue: TEpiFieldType);
begin
  if AValue = FLabelType then exit;
  FLabelType := AValue;
end;

function TEpiValueLabelSet.GetValueLabel(const AValue: variant): string;
var
  i: Integer;
begin
  case LabelType of
    ftString:
      begin
        for i := 0 to Count - 1 do
          if SameText(AValue, TEpiStringValueLabel(Items[i]).FValue) then
            exit(ValueLabels[i].TheLabel.Text)
      end;
    ftFloat:
      begin
        for i := 0 to Count -1 do
          if SameValue(AValue, TEpiFloatValueLabel(Items[i]).FValue) then
            exit(ValueLabels[i].TheLabel.Text);
      end;
    ftInteger:
      begin
        for i := 0 to Count - 1 do
          if AValue = TEpiIntValueLabel(Items[i]).FValue then
            exit(ValueLabels[i].TheLabel.Text)
      end;
  end;
  result := VarToStr(AValue);
end;

function TEpiValueLabelSet.GetValueLabels(const index: integer
  ): TEpiCustomValueLabel;
begin
  result := TEpiCustomValueLabel(Items[index]);
end;

function TEpiValueLabelSet.GetIsMissingValue(const AValue: variant): boolean;
var
  i: Integer;
begin
  case LabelType of
    ftString:
      begin
        for i := 0 to Count - 1 do
          if SameText(AValue, TEpiStringValueLabel(Items[i]).FValue) then
            exit(ValueLabels[i].IsMissingValue);
      end;
    ftFloat:
      begin
        for i := 0 to Count -1 do
          if SameValue(AValue, TEpiFloatValueLabel(Items[i]).FValue) then
            exit(ValueLabels[i].IsMissingValue);
      end;
    ftInteger:
      begin
        for i := 0 to Count - 1 do
          if AValue = TEpiIntValueLabel(Items[i]).FValue then
            exit(ValueLabels[i].IsMissingValue);
      end;
  end;
  Result := false;
end;

procedure TEpiValueLabelSet.LoadInternal(Root: TDOMNode);
var
  Node: TDOMNode;
  NValueLabel: TEpiCustomValueLabel;
begin
  // Root = <Internal>
  LabelScope := vlsInternal;

  Node := Root.FirstChild;
  while Assigned(Node) do
  begin
    // Node = <ValueLabel ... />
    CheckNode(Node, rsValueLabel);

    NValueLabel := NewValueLabel;
    NValueLabel.LoadFromXml(Node);

    Node := TDOMElement(Node.NextSibling);
  end;
end;

function TEpiValueLabelSet.SaveInternal(Lvl: integer): string;
var
  S: String;
begin
  S := SaveNode(Lvl + 2, rsType, Integer(LabelType));
  Result := inherited SaveToXml(S, Lvl + 1);
end;

procedure TEpiValueLabelSet.LoadExternal(Root: TDOMNode);
begin
  // TODO : Load External Value Labels.
end;

function TEpiValueLabelSet.SaveExternal(Lvl: integer): string;
var
  S: String;
begin
  Inc(Lvl);
  Result := SaveNode(Lvl, rsFile, ExtName);
  if ExtId <> '' then
    Result += SaveNode(Lvl, rsDataFileId, ExtId);
  if ExtValField <> '' then
    Result += SaveNode(Lvl, rsValueField, ExtValField);
  if ExtLabelField <> '' then
    Result += SaveNode(Lvl, rsLabelField, ExtLabelField);
  result := TEpiCustomItem(Self).SaveToXml(Result, Lvl);
end;

class function TEpiValueLabelSet.IdString: string;
begin
  Result := 'valuelabelset_id_';
end;

constructor TEpiValueLabelSet.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
end;

destructor TEpiValueLabelSet.Destroy;
begin
  inherited Destroy;
end;

function TEpiValueLabelSet.XMLName: string;
begin
  Result := rsValueLabelSet;
end;

function TEpiValueLabelSet.SaveToXml(Content: String; Lvl: integer): string;
begin
  case LabelScope of
    vlsExternal:
      Result := SaveExternal(Lvl - 1);
    vlsInternal:
      Result := SaveInternal(Lvl - 1);
  end;
end;

procedure TEpiValueLabelSet.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMNode;
begin
  inherited LoadFromXml(Root);
  // Root = <ValueLabel>
  if LoadNode(Node, Root, rsInternal, false) then
    LoadInternal(Node);

  if LoadNode(Node, Root, rsExternal, false) then
    LoadExternal(Node);
end;

function TEpiValueLabelSet.NewValueLabel: TEpiCustomValueLabel;
begin
  case LabelType of
    ftInteger: result := TEpiIntValueLabel.Create(Self);
    ftFloat:   result := TEpiFloatValueLabel.Create(Self);
    ftString:  result := TEpiStringValueLabel.Create(Self);
  end;
  result.Order := Count;
  AddItem(result);
end;

{ TEpiValueLabelSets }

function TEpiValueLabelSets.GetValueLabels(index: integer): TEpiValueLabelSet;
begin
  result := TEpiValueLabelSet(Items[Index]);
end;

constructor TEpiValueLabelSets.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
end;

destructor TEpiValueLabelSets.Destroy;
begin
  inherited Destroy;
end;

function TEpiValueLabelSets.XMLName: string;
begin
  result := rsValueLabelSets;
end;

function TEpiValueLabelSets.SaveToXml(Content: String; Lvl: integer): string;
begin
  if Count = 0 then exit;
  Result := inherited SaveToXml(Content, Lvl);
end;

procedure TEpiValueLabelSets.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMNode;
  NValueLabelSet: TEpiValueLabelSet;
begin
  inherited LoadFromXml(Root);

  // Root = <ValueLabelSets>
  Node := Root.FirstChild;
  while Assigned(Node) do
  begin
    CheckNode(Node, rsValueLabelSet);

    NValueLabelSet := NewValueLabelSet(TEpiFieldType(LoadNodeInt(Node, rsType)));
    NValueLabelSet.LoadFromXml(Node);

    Node := Node.NextSibling;
  end;
end;

function TEpiValueLabelSets.NewValueLabelSet(ALabelType: TEpiFieldType
  ): TEpiValueLabelSet;
begin
  result := TEpiValueLabelSet(NewItem(TEpiValueLabelSet));
  result.LabelType := ALabelType;
end;

end.

