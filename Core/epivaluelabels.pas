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
    FLabel: String;
    function GetValueAsString: string; virtual; abstract;
  public
    procedure SaveToStream(St: TStream; Lvl: integer); override;
    procedure LoadFromXml(Root: TDOMNode); override;
    property Order: integer read FOrder write FOrder;
    property TheLabel: string read FLabel write FLabel;
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
    procedure   SaveInternal(St: TStream; Lvl: integer); virtual;
    procedure   LoadExternal(Root: TDOMNode); virtual;
    procedure   SaveExternal(St: TStream; Lvl: integer); virtual;
  { Add/Remove overrides }
  public
    procedure   AddItem(Item: TEpiCustomItem); override;
    procedure   RemoveItem(Item: TEpiCustomItem); override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor  Destroy; override;
    procedure   SaveToStream(St: TStream; Lvl: integer); override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    function    NewValueLabel: TEpiCustomValueLabel;
    property    LabelScope: TValueLabelSetScope read FLabelScope write FLabelScope;
    property    LabelType: TEpiFieldType read FLabelType write SetLabelType;
  end;

  { TEpiValueLabelSets }

  TEpiValueLabelSets = class(TEpiCustomList)
  private
    function GetValueLabels(index: integer): TEpiValueLabelSet;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor  Destroy; override;
    procedure   SaveToStream(St: TStream; Lvl: integer); override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    function    NewValueLabelSet(ALabelType: TEpiFieldType): TEpiValueLabelSet;
    property    ValueLabels[index: integer]: TEpiValueLabelSet read GetValueLabels; default;
  end;

implementation

uses
  strutils, math;

{ TEpiCustomValueLabel }

procedure TEpiCustomValueLabel.SaveToStream(St: TStream; Lvl: integer);
var
  S: String;
begin
  S := DupeString(' ', LvL) + '<Set ';
  S := S + 'order="' + IntToStr(Order) + '" ';
  S := S + 'value="' + GetValueAsString + '" ';
  S := S + 'label="' + TheLabel + '"';
  S := S + '/>' + LineEnding;
  SaveStream(St, S);
end;

procedure TEpiCustomValueLabel.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMElement;
begin
  // Root = <Set>
  Node := TDOMElement(Root);

  Order    := StrToIntDef(Node.AttribStrings['order'], -1);
  TheLabel := UTF8Encode(Node.AttribStrings['label']);
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
  if FData.Count > 0 then
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
    // Node = <Set ... />
    CheckNode(Node, rsSet);

    NValueLabel := NewValueLabel;
    NValueLabel.LoadFromXml(Node);

    Node := TDOMElement(Node.NextSibling);
  end;
end;

procedure TEpiValueLabelSet.SaveInternal(St: TStream; Lvl: integer);
var
  DataNode: TAVLTreeNode;
  S: String;
begin
  S := Ins(Lvl) + '<' + rsInternal + '>' + LineEnding;
  SaveStream(St, S);

  DataNode := FData.FindLowest;
  while Assigned(DataNode) do
  begin
    TEpiCustomValueLabel(DataNode.Data).SaveToStream(St, Lvl + 1);
    DataNode := FData.FindSuccessor(DataNode);
  end;

  S := Ins(Lvl) + '</' + rsInternal + '>' + LineEnding;
  SaveStream(St, S);
end;

procedure TEpiValueLabelSet.LoadExternal(Root: TDOMNode);
begin
  // TODO : Load External Value Labels.
end;

procedure TEpiValueLabelSet.SaveExternal(St: TStream; Lvl: integer);
var
  S: String;
begin
  Inc(Lvl);
  S := SaveNode(Lvl, rsFile, ExtName);
  if ExtId <> '' then
    S += SaveNode(Lvl, rsDataFileId, ExtId);
  if ExtValField <> '' then
    S += SaveNode(Lvl, rsValueField, ExtValField);
  if ExtLabelField <> '' then
    S += SaveNode(Lvl, rsLabelField, ExtLabelField);
  S += SaveSection(Lvl, rsExternal, S);
  SaveStream(St, S);
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

procedure TEpiValueLabelSet.SaveToStream(St: TStream; Lvl: integer);
begin
  // TODO : Save Value label set.
  inherited SaveToStream(St, Lvl);
end;

procedure TEpiValueLabelSet.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMNode;
begin
  // Root = <ValueLabel>
  Id := TDOMElement(Root).AttribStrings['id'];
  Name := LoadNodeString(Root, rsName);

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

procedure TEpiValueLabelSets.SaveToStream(St: TStream; Lvl: integer);
begin
  if Count = 0 then exit;

  SaveList(St, Lvl, Self, rsValueLabels);
end;

procedure TEpiValueLabelSets.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMNode;
  NValueLabelSet: TEpiValueLabelSet;
begin
  // Root = <ValueLabels>
  Node := Root.FirstChild;
  while Assigned(Node) do
  begin
    CheckNode(Node, rsValueLabel);

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

