unit epivaluelabels;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epicustombase, epidatafilestypes, variants, Laz2_DOM;

type

  TEpiValueLabelChangeEvent = (evceValue, evceMissing);
  TEpiValueLabelSetChangeEvent = (evlsMaxValueLength);

  { TEpiCustomValueLabel }

  TEpiCustomValueLabel = class(TEpiCustomItem)
  private
    FIsMissingValue: boolean;
    FOrder: Integer;
    FLabel: TEpiTranslatedText;
    procedure SetIsMissingValue(AValue: boolean);
  protected
    function GetValueAsString: string; virtual; abstract;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase =
       nil): TEpiCustomBase; override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    function    XMLName: string; override;
    function    SaveToXml(Content: String; Lvl: integer): string; override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    procedure   Assign(const AEpiCustomBase: TEpiCustomBase); override;
    property    Order: integer read FOrder write FOrder;
    property    TheLabel: TEpiTranslatedText read FLabel write FLabel;
    property    IsMissingValue: boolean read FIsMissingValue write SetIsMissingValue;
    property    ValueAsString: string read GetValueAsString;
  end;
  TEpiCustomValueLabelClass = class of TEpiCustomValueLabel;

  { TEpiIntValueLabel }

  TEpiIntValueLabel = class(TEpiCustomValueLabel)
  private
    FValue: EpiInteger;
    procedure SetValue(AValue: EpiInteger);
  protected
    function GetValueAsString: string; override;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase =
       nil): TEpiCustomBase; override;
  public
    procedure LoadFromXml(Root: TDOMNode); override;
    procedure Assign(const AEpiCustomBase: TEpiCustomBase); override;
    property Value: EpiInteger read FValue write SetValue;
  end;

  { TEpiFloatValueLabel }

  TEpiFloatValueLabel = class(TEpiCustomValueLabel)
  private
    FValue: EpiFloat;
    procedure SetValue(AValue: EpiFloat);
  protected
    function GetValueAsString: string; override;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase =
       nil): TEpiCustomBase; override;
  public
    procedure LoadFromXml(Root: TDOMNode); override;
    procedure Assign(const AEpiCustomBase: TEpiCustomBase); override;
    property Value: EpiFloat read FValue write SetValue;
  end;

  { TEpiStringValueLabel }

  TEpiStringValueLabel = class(TEpiCustomValueLabel)
  private
    FValue: EpiString;
    procedure SetValue(AValue: EpiString);
  protected
    function GetValueAsString: string; override;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase =
       nil): TEpiCustomBase; override;
  public
    procedure LoadFromXml(Root: TDOMNode); override;
    procedure Assign(const AEpiCustomBase: TEpiCustomBase); override;
    property Value: EpiString read FValue write SetValue;
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
    FWriteNameToXml: boolean;
    function    GetValueLabel(const AValue: variant): TEpiCustomValueLabel;
    function    GetValueLabelIndex(const AValue: variant): integer;
    function    GetIsMissingValue(const AValue: variant): boolean;
    function    GetValueLabelString(const AValue: variant): string;
    function    GetValueLabelExists(const AValue: variant): boolean;
    function    GetValueLabels(const index: integer): TEpiCustomValueLabel;
    procedure   SetLabelType(const AValue: TEpiFieldType);
  private
    { House-keeping for MaxValueLengt }
    FDirtyCache: boolean;
    FCachedLength: LongInt;
    procedure   DirtyCacheAndSendChangeEvent;
    procedure   ItemChangeHook(Sender: TObject; EventGroup: TEpiEventGroup;
                  EventType: Word; Data: Pointer);
  protected
    procedure   LoadInternal(Root: TDOMNode); virtual;
    function    SaveInternal(Lvl: integer): string; virtual;
    procedure   LoadExternal(Root: TDOMNode); virtual;
    function    SaveExternal(Lvl: integer): string; virtual;
    function    WriteNameToXml: boolean; override;
    procedure   DoAssignList(const EpiCustomList: TEpiCustomList); override;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase =
       nil): TEpiCustomBase; override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor  Destroy; override;
    function    XMLName: string; override;
    function    SaveToXml(Content: String; Lvl: integer): string; override;
    function    SaveAttributesToXml: string; override;
    function    ItemClass: TEpiCustomItemClass; override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    function    NewValueLabel: TEpiCustomValueLabel;
    procedure   InsertItem(const Index: integer; Item: TEpiCustomItem); override;
    function    DeleteItem(Index: integer): TEpiCustomItem; override;
    property    LabelScope: TValueLabelSetScope read FLabelScope write FLabelScope;
    property    LabelType: TEpiFieldType read FLabelType write SetLabelType;
    property    ValueLabels[Const index: integer]: TEpiCustomValueLabel read GetValueLabels; default;
    property    ValueLabel[Const AValue: variant]: TEpiCustomValueLabel read GetValueLabel;
    property    ValueLabelString[Const AValue: variant]: string read GetValueLabelString;
    property    ValueLabelExists[Const AValue: variant]: boolean read GetValueLabelExists;
    property    IsMissingValue[Const AValue: variant]: boolean read GetIsMissingValue;
  public
    { Aux. functions. }
    function    MissingCount: LongInt;
    function    MaxValueLength: LongInt;
  end;

  { TEpiValueLabelSets }

  TEpiValueLabelSets = class(TEpiCustomList)
  private
    function    GetValueLabels(index: integer): TEpiValueLabelSet;
    function    Prefix: string; override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor  Destroy; override;
    function    XMLName: string; override;
    function    SaveToXml(Content: String; Lvl: integer): string; override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    function    ValidateRename(ValueLabelSet: TEpiValueLabelSet; NewName: string): boolean;
    function    NewValueLabelSet(ALabelType: TEpiFieldType): TEpiValueLabelSet;
    function    GetValueLabelSetByName(Const AName: string): TEpiValueLabelSet;
    property    ValueLabels[index: integer]: TEpiValueLabelSet read GetValueLabels; default;
  end;

implementation

uses
  strutils, math, LazUTF8, epidocument;

{ TEpiCustomValueLabel }

procedure TEpiCustomValueLabel.SetIsMissingValue(AValue: boolean);
var
  Val: Boolean;
begin
  if FIsMissingValue = AValue then Exit;
  Val := FIsMissingValue;
  FIsMissingValue := AValue;
  DoChange(eegValueLabel, Word(evceMissing), @Val);
end;

function TEpiCustomValueLabel.DoClone(AOwner: TEpiCustomBase;
  Dest: TEpiCustomBase): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest);
  with TEpiCustomValueLabel(Result) do
  begin
    FIsMissingValue := Self.FIsMissingValue;
    FOrder          := Self.FOrder;
  end;
end;

constructor TEpiCustomValueLabel.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  IsMissingValue := false;
  FLabel := TEpiTranslatedText.Create(Self, rsLabel);
  RegisterClasses([FLabel]);
end;

function TEpiCustomValueLabel.XMLName: string;
begin
  Result := rsValueLabel;
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
  BackupFormatSettings(TEpiDocument(RootOwner).XMLSettings.FormatSettings);
  Result +=
    Indent(LvL + 1) +
    '<ValueLabel' +
    SaveAttr(rsOrder, Order) +
    SaveAttr(rsValue, GetValueAsString);
  RestoreFormatSettings;

  // Add missing if set
  if IsMissingValue then
    Result += SaveAttr(rsMissing, IsMissingValue);

  Result += '>';

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
  Attr: TDOMAttr;
begin
  // Root = <ValueLabel>
  Node := TDOMElement(Root);

  Order    := LoadAttrInt(Root, rsOrder);
  if LoadAttr(Attr, Root, rsMissing, false) then
    IsMissingValue := LoadAttrBool(Root, rsMissing);
  TheLabel.LoadFromXml(Root);
end;

procedure TEpiCustomValueLabel.Assign(const AEpiCustomBase: TEpiCustomBase);
var
  OrgVL: TEpiCustomValueLabel absolute AEpiCustomBase;
begin
  inherited Assign(AEpiCustomBase);
  BeginUpdate;
  FIsMissingValue := OrgVL.FIsMissingValue;
  FOrder          := OrgVL.FOrder;
  FLabel.Assign(OrgVL.FLabel);
  EndUpdate;
end;

{ TEpiIntValueLabel }

procedure TEpiIntValueLabel.SetValue(AValue: EpiInteger);
var
  Val: EpiInteger;
begin
  if FValue = AValue then Exit;
  Val := FValue;
  FValue := AValue;
  DoChange(eegValueLabel, Word(evceValue), @Val);
end;

function TEpiIntValueLabel.GetValueAsString: string;
begin
  Result := IntToStr(Value);
end;

function TEpiIntValueLabel.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase
  ): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest);
  TEpiIntValueLabel(Result).FValue := FValue;
end;

procedure TEpiIntValueLabel.LoadFromXml(Root: TDOMNode);
begin
  inherited LoadFromXml(Root);
  Value := LoadAttrInt(Root, rsValue);
end;

procedure TEpiIntValueLabel.Assign(const AEpiCustomBase: TEpiCustomBase);
begin
  inherited Assign(AEpiCustomBase);
  BeginUpdate;
  FValue := TEpiIntValueLabel(AEpiCustomBase).FValue;
  EndUpdate;
end;

{ TEpiFloatValueLabel }

procedure TEpiFloatValueLabel.SetValue(AValue: EpiFloat);
var
  Val: EpiFloat;
begin
  if FValue = AValue then Exit;
  Val := FValue;
  FValue := AValue;
  DoChange(eegValueLabel, Word(evceValue), @Val);
end;

function TEpiFloatValueLabel.GetValueAsString: string;
begin
  Result := FloatToStr(Value);
end;

function TEpiFloatValueLabel.DoClone(AOwner: TEpiCustomBase;
  Dest: TEpiCustomBase): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest);
  TEpiFloatValueLabel(Result).FValue := FValue;
end;

procedure TEpiFloatValueLabel.LoadFromXml(Root: TDOMNode);
begin
  inherited LoadFromXml(Root);
  Value := LoadAttrFloat(Root, rsValue);
end;

procedure TEpiFloatValueLabel.Assign(const AEpiCustomBase: TEpiCustomBase);
begin
  inherited Assign(AEpiCustomBase);
  BeginUpdate;
  FValue := TEpiFloatValueLabel(AEpiCustomBase).FValue;
  EndUpdate;
end;

{ TEpiStringValueLabel }

procedure TEpiStringValueLabel.SetValue(AValue: EpiString);
var
  Val: EpiString;
begin
  if FValue = AValue then Exit;
  Val := FValue;
  FValue := AValue;
  DoChange(eegValueLabel, Word(evceValue), @Val);
end;

function TEpiStringValueLabel.GetValueAsString: string;
begin
  Result := Value;
end;

function TEpiStringValueLabel.DoClone(AOwner: TEpiCustomBase;
  Dest: TEpiCustomBase): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest);
  TEpiStringValueLabel(Result).FValue := FValue;
end;

procedure TEpiStringValueLabel.LoadFromXml(Root: TDOMNode);
begin
  inherited LoadFromXml(Root);
  Value := LoadAttrString(Root, rsValue);
end;

procedure TEpiStringValueLabel.Assign(const AEpiCustomBase: TEpiCustomBase);
begin
  inherited Assign(AEpiCustomBase);
  BeginUpdate;
  FValue := TEpiStringValueLabel(AEpiCustomBase).FValue;
  EndUpdate;
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

procedure TEpiValueLabelSet.DirtyCacheAndSendChangeEvent;
begin
  FDirtyCache := true;
  DoChange(eegValueLabelSet, Word(evlsMaxValueLength), @FCachedLength);
end;

function TEpiValueLabelSet.GetValueLabelString(const AValue: variant): string;
var
  i: Integer;
begin
  I := GetValueLabelIndex(AValue);
  if I = -1 then
    result := VarToStr(AValue)
  else
    result := ValueLabels[i].TheLabel.Text
end;

function TEpiValueLabelSet.GetValueLabelExists(const AValue: variant): boolean;
begin
  result := GetValueLabelIndex(AValue) <> -1;
end;

function TEpiValueLabelSet.GetValueLabels(const index: integer
  ): TEpiCustomValueLabel;
begin
  result := TEpiCustomValueLabel(Items[index]);
end;

procedure TEpiValueLabelSet.ItemChangeHook(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  if (EventGroup <> eegValueLabel) then Exit;

  if TEpiValueLabelChangeEvent(EventType) = evceValue then
    DirtyCacheAndSendChangeEvent;
end;

function TEpiValueLabelSet.GetValueLabel(const AValue: variant): TEpiCustomValueLabel;
var
  I: LongInt;
begin
  result := nil;

  I := GetValueLabelIndex(AValue);
  if I <> -1 then
    Result := ValueLabels[I];
end;

function TEpiValueLabelSet.GetValueLabelIndex(const AValue: variant): integer;
begin
  case LabelType of
    ftString:
      begin
        for result := 0 to Count - 1 do
          if AnsiSameStr(AValue, TEpiStringValueLabel(Items[result]).FValue) then
            exit;
      end;
    ftFloat:
      begin
        for result := 0 to Count -1 do
          if SameValue(AValue, TEpiFloatValueLabel(Items[result]).FValue) then
            exit;
      end;
    ftInteger:
      begin
        for result := 0 to Count - 1 do
          if AValue = TEpiIntValueLabel(Items[result]).FValue then
            exit;
      end;
  end;
  result := -1;
end;

function TEpiValueLabelSet.GetIsMissingValue(const AValue: variant): boolean;
var
  i: Integer;
begin
  I := GetValueLabelIndex(AValue);
  Result := (I <> -1) and
            ValueLabels[i].IsMissingValue;
end;

procedure TEpiValueLabelSet.LoadInternal(Root: TDOMNode);
var
  Node: TDOMNode;
  NValueLabel: TEpiCustomValueLabel;
begin
  // Root = <Internal>
  LabelScope := vlsInternal;

  FWriteNameToXml := false;
  inherited LoadFromXml(Root);
  FWriteNameToXml := true;
{
  Node := Root.FirstChild;
  while Assigned(Node) do
  begin
    // Node = <ValueLabel ... />
    CheckNode(Node, rsValueLabel);

    NValueLabel := NewValueLabel;
    NValueLabel.LoadFromXml(Node);

    Node := TDOMElement(Node.NextSibling);
  end;        }
end;

function TEpiValueLabelSet.SaveInternal(Lvl: integer): string;
var
  S: String;
begin
//  S := SaveNode(Lvl + 2, rsType, Integer(LabelType)) +
//       SaveNode(Lvl + 2, rsName, Name);
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

function TEpiValueLabelSet.WriteNameToXml: boolean;
begin
  Result := FWriteNameToXml;
end;

procedure TEpiValueLabelSet.DoAssignList(const EpiCustomList: TEpiCustomList);
var
  Item: TEpiCustomValueLabel;
  i: Integer;
begin
  BeginUpdate;
  if EpiCustomList.Count > 0 then
  begin
    for i := 0 to EpiCustomList.Count - 1 do
    begin
      Item := NewValueLabel;
      Item.Assign(EpiCustomList[i]);
    end;
  end;
  EndUpdate;
end;

function TEpiValueLabelSet.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase
  ): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest);
  with TEpiValueLabelSet(Result) do
  begin
    FLabelScope := Self.FLabelScope;
    FLabelType := Self.FLabelType;
    FName := Self.FName;
  end;
end;

constructor TEpiValueLabelSet.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  FWriteNameToXml := true;
  FLabelScope := vlsInternal;
  FLabelType  := ftInteger;
  FName       := '';
  FDirtyCache := false;
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

function TEpiValueLabelSet.SaveAttributesToXml: string;
begin
  Result := inherited SaveAttributesToXml +
    SaveAttrEnum(rsType, Integer(LabelType), TypeInfo(TEpiFieldType));
end;

function TEpiValueLabelSet.ItemClass: TEpiCustomItemClass;
begin
  case LabelType of
    ftInteger: Result := TEpiIntValueLabel;
    ftFloat:   Result := TEpiFloatValueLabel;
    ftString:  Result := TEpiStringValueLabel;
  end;
end;

procedure TEpiValueLabelSet.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMNode;
  Attr: TDOMAttr;
begin
  if LoadAttr(Attr, Root, rsId, True) then
    FName := LoadAttrString(Root, rsId);

  // Root = <ValueLabel>
  if LoadNode(Node, Root, rsInternal, false) then
    LoadInternal(Node);

  if LoadNode(Node, Root, rsExternal, false) then
    LoadExternal(Node);
end;

function TEpiValueLabelSet.NewValueLabel: TEpiCustomValueLabel;
begin
  Result := TEpiCustomValueLabel(NewItem);
  result.Order := Count + 1;
end;

procedure TEpiValueLabelSet.InsertItem(const Index: integer;
  Item: TEpiCustomItem);
begin
  inherited InsertItem(Index, Item);
  Item.RegisterOnChangeHook(@ItemChangeHook, true);
  DirtyCacheAndSendChangeEvent;
end;

function TEpiValueLabelSet.DeleteItem(Index: integer): TEpiCustomItem;
begin
  Result := inherited DeleteItem(Index);
  Result.UnRegisterOnChangeHook(@ItemChangeHook);
  DirtyCacheAndSendChangeEvent;
end;

function TEpiValueLabelSet.MissingCount: LongInt;
var
  i: Integer;
begin
  result := 0;
  for i := 0 to Count - 1 do
    if ValueLabels[i].IsMissingValue then Inc(result);
end;

function TEpiValueLabelSet.MaxValueLength: LongInt;
var
  i: Integer;
begin
  if not FDirtyCache then
    Exit(FCachedLength);

  Result := 0;
  for i := 0 to Count -1 do
    Result := Max(Result, UTF8Length(ValueLabels[i].ValueAsString));
  FCachedLength := Result;
  FDirtyCache := false;
end;

{ TEpiValueLabelSets }

function TEpiValueLabelSets.GetValueLabels(index: integer): TEpiValueLabelSet;
begin
  result := TEpiValueLabelSet(Items[Index]);
end;

function TEpiValueLabelSets.Prefix: string;
begin
  Result := 'valuelabel_id_';
end;

function TEpiValueLabelSets.ValidateRename(ValueLabelSet: TEpiValueLabelSet;
  NewName: string): boolean;
var
  i: Integer;
begin
  result := false;
  for i := 0 to Count - 1 do
    if CompareStr(ValueLabels[i].Name, NewName) = 0 then
      exit;
  result := true;
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
  Attr: TDOMAttr;
begin
  // Root = <ValueLabelSets>
  Node := Root.FirstChild;
  while Assigned(Node) do
  begin
    while NodeIsWhiteSpace(Node) do
      Node := Node.NextSibling;
    if not Assigned(Node) then exit;

    CheckNode(Node, rsValueLabelSet);

    NValueLabelSet := NewValueLabelSet(TEpiFieldType(LoadAttrEnum(Node, rsType, TypeInfo(TEpiFieldType))));
    NValueLabelSet.LoadFromXml(Node);

    Node := Node.NextSibling;
  end;
end;

function TEpiValueLabelSets.NewValueLabelSet(ALabelType: TEpiFieldType
  ): TEpiValueLabelSet;
begin
  result := TEpiValueLabelSet(NewItem(TEpiValueLabelSet));
  result.LabelType := ALabelType;
  result.ItemOwner := true;
end;

function TEpiValueLabelSets.GetValueLabelSetByName(const AName: string
  ): TEpiValueLabelSet;
begin
  result := TEpiValueLabelSet(GetItemByName(AName));
end;

end.

