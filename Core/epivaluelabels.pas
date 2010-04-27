unit epivaluelabels;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epicustombase, epidatafilestypes, DOM, AVL_Tree;

type

  { TEpiCustomValueLabel }

  TEpiCustomValueLabel = class(TEpiCustomItem)
  private
    FOrder: Integer;
    FLabel: TEpiTranslatedText;
    function GetValueAsString: string; virtual; abstract;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    function SaveToXml(Content: String; Lvl: integer): string; override;
    procedure LoadFromXml(Root: TDOMNode); override;
    property Order: integer read FOrder write FOrder;
    property TheLabel: TEpiTranslatedText read FLabel write FLabel;
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
    FData: TAVLTree;
    FLabelScope: TValueLabelSetScope;
    FLabelType: TEpiFieldType;
    procedure   SetLabelType(const AValue: TEpiFieldType);
  protected
    procedure   LoadInternal(Root: TDOMNode); virtual;
    function    SaveInternal(Lvl: integer): string; virtual;
    procedure   LoadExternal(Root: TDOMNode); virtual;
    function    SaveExternal(Lvl: integer): string; virtual;
  { TEpiCustomList overrides }
{  public
    function    GetCount: Integer; override;
    procedure   AddItem(Item: TEpiCustomItem); override;
    procedure   RemoveItem(Item: TEpiCustomItem); override;
    property    Items[Index: integer]: TEpiCustomItem    }
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor  Destroy; override;
    function    XMLName: string; override;
    function    SaveToXml(Content: String; Lvl: integer): string; override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    function    NewValueLabel: TEpiCustomValueLabel;
    property    LabelScope: TValueLabelSetScope read FLabelScope write FLabelScope;
    property    LabelType: TEpiFieldType read FLabelType write SetLabelType;
  public
    { "Publiclised" inherited properties }
    property    Id;
    property    Name;
  end;

  { TEpiValueLabelSets }

  TEpiValueLabelSets = class(TEpiCustomList)
  private
    function GetValueLabels(index: integer): TEpiValueLabelSet;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor  Destroy; override;
    function XMLName: string; override;
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

  Result +=
    Indent(LvL + 1) +
      '<' + rsValueLabel + ' order="' + IntToStr(Order) + '" value="' + GetValueAsString + '">' + LineEnding +
      FLabel.SaveToXml('', Lvl + 2) +
    Indent(Lvl + 1) +
      '</' + rsValueLabel + '>' + LineEnding;

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
  // Hack to fix improper handling of setting TAVL_Tree OnCompare event.
  // should not be required to check for Count > 0.
//  if FData.Count > 0 then
  begin
    FLabelType := AValue;
    Case LabelType of
      ftString:  FData.OnCompare := @StringCompare;
      ftInteger: FData.OnCompare := @IntCompare;
      ftFloat:   FData.OnCompare := @FloatCompare;
    end;
  end;
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
  DataNode: TAVLTreeNode;
  S: String;
begin
  Result := Indent(Lvl) + '<' + rsInternal + '>' + LineEnding;

{  DataNode := FData.FindLowest;
  while Assigned(DataNode) do
  begin
    Result += TEpiCustomValueLabel(DataNode.Data).SaveToXml('', Lvl + 1);
    DataNode := FData.FindSuccessor(DataNode);
  end;     }
  Result += Indent(Lvl) + '</' + rsInternal + '>' + LineEnding;

  Result := inherited SaveToXml(Result, Lvl + 1);
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

{function TEpiValueLabelSet.GetCount: Integer;
begin
  Result := FData.Count;
end;

procedure TEpiValueLabelSet.AddItem(Item: TEpiCustomItem);
begin
  FData.Add(Item);
  DoChange(eegCustomBase, Word(ecceAddItem), nil);
end;

procedure TEpiValueLabelSet.RemoveItem(Item: TEpiCustomItem);
begin
  FData.Remove(Item);
  DoChange(eegCustomBase, Word(ecceDelItem), Item);
end;
                     }
constructor TEpiValueLabelSet.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  FData := TAVLTree.Create;
end;

destructor TEpiValueLabelSet.Destroy;
begin
  FData.FreeAndClear;
  FData.Free;
  inherited Destroy;
end;

function TEpiValueLabelSet.XMLName: string;
begin
  Result := rsValueLabelSet;
end;

function TEpiValueLabelSet.SaveToXml(Content: String; Lvl: integer): string;
begin
  // TODO : Save Value Labels correct.
  Content := SaveNode(Lvl + 1, rsType, Integer(LabelType));

{  case LabelScope of
    vlsExternal:
      Content += SaveExternal(Lvl + 1);
    vlsInternal:
      Content += SaveInternal(Lvl + 1);
  end;}

  Result := inherited SaveToXml(Content, Lvl);
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
  result := TEpiValueLabelSet.Create(Self);
  result.LabelType := ALabelType;
  result.Id := 'valuelabelset_id_' + IntToStr(Count);
  AddItem(Result);
end;

end.

