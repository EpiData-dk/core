unit epivaluelabels;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, epidatatypes, AVL_Tree, Variants, epicustomclass, DOM;
  
type

  { TEpiCustomValueLabel }

  TEpiCustomValueLabel = class(TEpiCustomClass)
  private
    FOrder: Integer;
    FLabel: String;
    FMissing: Boolean;
    FNoValue: Variant;
    function GetValueAsString: string; virtual; abstract;
  public
    procedure SaveToStream(St: TStream; Lvl: integer); override;
    procedure LoadFromXml(Root: TDOMNode); override;
    property Order: integer read FOrder write FOrder;
    property Value: Variant read FNoValue write FNoValue;
    property TheLabel: string read FLabel write FLabel;
    property Missing: boolean read FMissing write FMissing;
  end;
  TEpiCustomValueLabelClass = class of TEpiCustomValueLabel;

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

  // vlsLocal retained for compatability with old .REC/.CHK format.
  TValueLabelSetScope = (vlsInternal, vlsExternal);

  { TValueLabelSet }

  TValueLabelSet = class(TEpiCustomClass)
  private
    FData:     TAVLTree;
    FExtId: string;
    FExtLabelField: string;
    FExtName:  string;
    FExtValField: string;
    FId:       string;
    FName:     string;
    FLabelScope: TValueLabelSetScope;
    FLabelType:  TFieldType;
    FCurrentIndex: Integer;
    FCurrentNode: TAVLTreeNode;
    function GetCount: integer;
    function FindValuePair(Const Value: Variant): Pointer;
    function GetLabels(const Index: Integer): string;
    function GetMissingValue(const aValue: Variant): boolean;
    function GetMissingValues(const Index: Integer): boolean;
    function GetValueLabel(const aValue: Variant): string;
    function GetValues(const Index: Integer): Variant;
    procedure SetLabelType(const AValue: TFieldType); inline;
    procedure SetMissingValue(const aValue: Variant; const AMissing: boolean);
    procedure SetValueLabel(const aValue: Variant; const ALabel: string); inline;
    // For traversion the tree of nodes using "random access" method.
    procedure PositionNode(Const Index: integer);
    function GetCurrentValueLabelPair: Pointer; inline;
  protected
    procedure   LoadInternal(Root: TDOMNode); virtual;
    procedure   SaveInternal(St: TStream; Lvl: integer); virtual;
    procedure   LoadExternal(Root: TDOMNode); virtual;
    procedure   SaveExternal(St: TStream; Lvl: integer); virtual;
    property    ExtName: string read FExtName write FExtName;
    property    ExtId: string read FExtId write FExtId;
    property    ExtValField: string read FExtValField write FExtValField;
    property    ExtLabelField: string read FExtLabelField write FExtLabelField;
  public
    constructor Create(AOwner: TObject; aLabelType: TFieldType = ftInteger);
    destructor  Destroy; override;
    procedure   AddValueLabelPair(Const aValue: Variant; Const aLabel: string; aMissing: Boolean = false);
    procedure   Clone(var Dest: TValueLabelSet);
    procedure   Clear;
    procedure   SaveToStream(St: TStream; Lvl: Integer); override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    property    Id:   string read FId write FId;
    property    Name: string read FName write FName;
    property    Count: Integer read GetCount;
    property    LabelScope: TValueLabelSetScope read FLabelScope write FLabelScope;
    property    LabelType: TFieldType read FLabelType write SetLabelType;
  public
    // Data accessing properties and methods...
    property    ValueLabel[Const aValue: Variant]: string read GetValueLabel write SetValueLabel;
    property    MissingValue[Const aValue: Variant]: boolean read GetMissingValue write SetMissingValue;
    // Should only be used to read and traverse the tree.
    property    Values[Const Index: Integer]: Variant read GetValues;
    // Should only be used to read and traverse the tree.
    property    Labels[Const Index: Integer]: string read GetLabels;
    // Should only be used to read and traverse the tree.
    property    MissingValues[Const Index: Integer]: boolean read GetMissingValues;
  end;

  { TValueLabelSets }

  TValueLabelSets = class(TEpiCustomClass)
  private
    FList: TStringList;
    function GetCount:integer;
    function GetItem(index:integer):TValueLabelSet;
  public
    constructor Create(AOwner: TObject); override;
    destructor  Destroy; override;
    procedure   Clear;
    procedure   Clone(var dest: TValueLabelSets);
    procedure   Assign(Const Src: TValueLabelSets);
    procedure   SaveToStream(St: TStream; Lvl: Integer); override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    function    ValueLabelSetByName(Const Id: string): TValueLabelSet;
    function    ValueLabelSetExits(Const Id: string; var aValueLabelSet: TValueLabelSet): boolean;
    procedure   AddValueLabelSet(aValueLabelSet: TValueLabelSet);
    procedure   DeleteValueLabelSet(Const Id: string);
    property    Count:integer read GetCount;
    property    Items[index: integer]: TValueLabelSet read GetItem; default;
  end;

implementation

uses
  SysUtils, epistringutils, epidataglobals, epiutils, math, epiimportexport,
  epidatafile;

type
  TValuePair = record
    FOrder: Integer;
    FValue: Variant;
    FLabel: String;
    FMissing: Boolean;
  end;
  PValuePair = ^TValuePair;

{ TValueLabelSets }

procedure TValueLabelSets.AddValueLabelSet(aValueLabelSet: TValueLabelSet);
begin
  FList.AddObject(trim(aValueLabelSet.Id), aValueLabelSet);
end;

procedure TValueLabelSets.DeleteValueLabelSet(Const Id: string);
var
  idx: integer;
begin
  idx := FList.IndexOf(Id);
  if idx>-1 then FList.Delete(idx);
end;

procedure TValueLabelSets.Clear;
var
  i: integer;
begin
  for i := 0 to FList.Count - 1 do
  begin
    TValueLabelSet(FList.Objects[i]).Free;
  end;
end;

procedure TValueLabelSets.Clone(var dest: TValueLabelSets);
var
  i: integer;
  tmp: TValueLabelSet;
begin
  if (not assigned(FList)) then exit;
  if not Assigned(Dest) then
    Dest := TValueLabelSets.Create(nil);

  Dest.Clear;
  for i := 0 to FList.Count - 1 do
  begin
    Tmp := nil;
    TValueLabelSet(FList.Objects[i]).Clone(tmp);
    Dest.AddValueLabelSet(tmp);
  end;
end;

constructor TValueLabelSets.Create(AOwner: TObject);
begin
  Inherited;
  Flist := TStringList.Create;
  FList.Sorted := true;
end;

destructor TValueLabelSets.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

function TValueLabelSets.ValueLabelSetByName(
  Const Id: string): TValueLabelSet;
var
  idx: integer;
begin
  result := nil;
  if FList.Find(trim(Id), idx) then
    result := TValueLabelSet(FList.Objects[idx]);
end;

function TValueLabelSets.ValueLabelSetExits(const Id: string;
  var aValueLabelSet: TValueLabelSet): boolean;
begin
  aValueLabelSet := ValueLabelSetByName(Id);
  Result := Assigned(aValueLabelSet);
end;

function TValueLabelSets.GetCount:integer;
begin
  result:=FList.count;
end;

function TValueLabelSets.GetItem(index:integer):TValueLabelSet;
begin
  Result := NIL;
  if (index >= 0) and (index < FList.count) then
     Result := TValueLabelSet(FList.objects[index]);
end;

procedure TValueLabelSets.Assign(const Src: TValueLabelSets);
var
  TmpValueSet: TValueLabelSet;
  i: integer;
begin
  if not Assigned(Src) then exit;

  Clear;
  for i := 0 to Src.Count - 1 do
  begin
    TmpValueSet := nil;
    Src[i].Clone(TmpValueSet);
    AddValueLabelSet(TmpValueSet);
  end;
end;

procedure TValueLabelSets.SaveToStream(St: TStream; Lvl: Integer);
var
  S: String;
  i: Integer;
begin
  if Count = 0 then exit;

  S :=
    Ins(Lvl) + '<ValueLabels>' + LineEnding;
  St.Write(S[1], Length(S));

  for i := 0 to Count - 1 do
    Items[i].SaveToStream(St, Lvl + 1);

  S := Ins(Lvl) + '</ValueLabels>' + LineEnding;
  St.Write(S[1], Length(S));
end;

procedure TValueLabelSets.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMNode;
  NewValueLabel: TValueLabelSet;
begin
  // Root = <ValueLabels>

  Node := Root.FirstChild;
  while Assigned(Node) do
  begin
    if Node.CompareName('ValueLabel') <> 0 then
      ReportXmlError(EPI_XML_TAG_MISSING, 0, '', []);

    NewValueLabel := TValueLabelSet.Create(Self, XmlNameToFieldType(Node.FindNode('Type').TextContent));
    NewValueLabel.LoadFromXml(Node);
    AddValueLabelSet(NewValueLabel);

    Node := Node.NextSibling;
  end;
end;

{ TValueLabelSet }

procedure TValueLabelSet.AddValueLabelPair(Const aValue: Variant; Const aLabel: string; aMissing: Boolean = false);
var
  NValuePair: PValuePair;
begin
  NValuePair := FindValuePair(aValue);
  if Assigned(NValuePair) then
  begin
    // This handles old .REC/.CHK style missing values.
    // Since AddValueLabelPair is used in CheckFileIO to insert missingvalue, we may
    // overwrite previous defined valuelabel or missing.
    if ((NValuePair^.FLabel <> '') and aMissing) then
      // Val. lab. exists and we are inserting missing.
      // keep label - update missing.
      NValuePair^.FMissing := aMissing
    else if (NValuePair^.FMissing and (aLabel <> '')) then
      // Missing exists and we are inserting val. lab.
      // keep missing - update label
      NValuePair^.FLabel := aLabel
    else begin
      // Normal update of situation.
      NValuePair^.FLabel := aLabel;
      NValuePair^.FMissing := aMissing;
    end;
  end else begin
    NValuePair := New(PValuePair);
    NValuePair^.FValue := aValue;
    NValuePair^.FLabel := aLabel;
    NValuePair^.FMissing := aMissing;
    FData.Add(NValuePair);
  end;
end;

procedure TValueLabelSet.Clear;
var
  AVLNode: TAVLTreeNode;
begin
  FCurrentIndex := -1;
  FName := '';
  FId := '';
  FCurrentNode := nil;
  AVLNode := FData.FindLowest;
  while Assigned(AVLNode) do
  with PValuePair(AVLNode.Data)^ do
  begin
    // Nil strings to decrease ref. count.
    FLabel := '';
    VarClear(FValue);
    AVLNode := FData.FindSuccessor(AVLNode);
  end;
  FData.Clear;
end;

procedure TValueLabelSet.SaveToStream(St: TStream; Lvl: Integer);
var
  S: String;
  i: Integer;
begin
  S :=
    Ins(Lvl)     + '<ValueLabel id="' + StringToXml(Id) + '">' + LineEnding +
    Ins(Lvl + 1) + '<Type>' + FieldTypeXmlNames[LabelType] + '</Type>' + LineEnding +
    Ins(Lvl + 1) + '<Name>' + StringToXml(Name) + '</Name>' + LineEnding;
  St.Write(S[1], Length(S));

  case LabelScope of
    vlsExternal:
      SaveInternal(St, Lvl + 1);
    vlsInternal:
      SaveExternal(St, Lvl + 1);
  end;

  S := S +
    Ins(Lvl) + '</ValueLabel>' + LineEnding;
  St.Write(S[1], Length(S));
end;

procedure TValueLabelSet.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMNode;
begin
  // Root = <ValueLabel>

  Id := TDOMElement(Root).AttribStrings['id'];

  Node := Root.FindNode('Name');
  Name := Node.TextContent;

  Node := Root.FindNode('Internal');
  if Assigned(Node) then
    LoadInternal(Node)
  else
    Node := Root.FindNode('External');

  if Assigned(Node) then
    LoadExternal(Node);
end;

procedure TValueLabelSet.Clone(var Dest: TValueLabelSet);
var
  OldValuePair, NewValuePair: PValuePair;
  AVLNode: TAVLTreeNode;
begin
  if not Assigned(Dest) then
    Dest := TValueLabelSet.Create(nil, LabelType);

  Dest.Clear;
  Dest.Name := Name;
  Dest.Id := Id;
  Dest.LabelScope := LabelScope;
  AVLNode := FData.FindLowest;
  while Assigned(AVLNode) do
  begin
    // Use simple copy records since using AddValueLabelPair require
    // a lookup in AVL tree for each insert (timeconsuming for large valuelabelsets).
    OldValuePair := PValuePair(AVLNode.Data);
    NewValuePair := New(PValuePair);
    NewValuePair^.FValue := OldValuePair^.FValue;
    NewValuePair^.FLabel := OldValuePair^.FLabel;
    NewValuePair^.FMissing := OldValuePair^.FMissing;
    Dest.FData.Add(NewValuePair);
    AVLNode := FData.FindSuccessor(AVLNode);
  end;
end;

function OrderCompare(Item1, Item2: Pointer): Integer; inline;
var
  VL1: TEpiCustomValueLabel absolute Item1;
  VL2: TEpiCustomValueLabel absolute Item2;
begin
  Result := VL2.Order - VL1.Order;
end;

function FloatCompare(Item1, Item2: Pointer): Integer;
var
  VL1: TEpiFloatValueLabel absolute Item1;
  VL2: TEpiFloatValueLabel absolute Item2;
begin
  if (Vl1.Order = -1) then
    Result := Math.Sign(VL2.Value - VL2.Value)
  else
    Result := OrderCompare(Item1, Item2);
end;

function IntCompare(Item1, Item2: Pointer): Integer;
var
  VL1: TEpiIntValueLabel absolute Item1;
  VL2: TEpiIntValueLabel absolute Item2;
begin
  if (Vl1.Order = -1) then
    Result := VL2.Value - VL2.Value
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

function TValueLabelSet.GetCount: integer;
begin
  result := FData.Count;
end;

function TValueLabelSet.FindValuePair(const Value: Variant): Pointer;
var
  LValuePair: TValuePair;
  AVLNode: TAVLTreeNode;
begin
  Result := nil;
  LValuePair.FValue := Value;
  AVLNode := FData.Find(@LValuePair);
  if Assigned(AVLNode) then
    Result := AVLNode.Data;
end;

function TValueLabelSet.GetLabels(const Index: Integer): string;
begin
  PositionNode(Index);
  result := PValuePair(GetCurrentValueLabelPair)^.FLabel;
end;

function TValueLabelSet.GetMissingValue(const aValue: Variant): boolean;
var
  AValuePair: PValuePair;
begin
  Result := False;
  AValuePair := PValuePair(FindValuePair(aValue));
  if Assigned(AValuePair) then
    Result := AValuePair^.FMissing;
end;

function TValueLabelSet.GetMissingValues(const Index: Integer): boolean;
begin
  PositionNode(Index);
  result := PValuePair(GetCurrentValueLabelPair)^.FMissing;
end;

function TValueLabelSet.GetValueLabel(const aValue: Variant): string;
var
  AValuePair: PValuePair;
begin
  Result := AValue;
  AValuePair := PValuePair(FindValuePair(aValue));
  if Assigned(AValuePair) then
    Result := AValuePair^.FLabel;
end;

function TValueLabelSet.GetValues(const Index: Integer): Variant;
begin
  PositionNode(Index);
  result := PValuePair(GetCurrentValueLabelPair)^.FValue;
end;

procedure TValueLabelSet.SetLabelType(const AValue: TFieldType);
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

procedure TValueLabelSet.SetMissingValue(const aValue: Variant;
  const AMissing: boolean);
var
  AValuePair: PValuePair;
begin
  AValuePair := FindValuePair(aValue);
  if Assigned(AValuePair) then
    AValuePair^.FMissing := AMissing;
end;

procedure TValueLabelSet.SetValueLabel(const aValue: Variant;
  const ALabel: string);
var
  AValuePair: PValuePair;
begin
  AValuePair := PValuePair(FindValuePair(aValue));
  if Assigned(AValuePair) then
    AValuePair^.FLabel := ALabel;
end;

procedure TValueLabelSet.PositionNode(const Index: integer);
var
  i: Integer;
begin
  if (Index < 0) or (Index > FData.Count - 1) then
    Raise Exception.CreateFmt('TValueLabelSet: Index out of bounds %d', [Index]);

  if Index = FCurrentIndex then exit;

  // TODO : Enhance method to run both ways and start from the nearest end point.
  if Index = 0 then
    FCurrentNode := FData.FindLowest
  else
    for i := 1 to Index - FCurrentIndex do
      FCurrentNode := FData.FindSuccessor(FCurrentNode);
  FCurrentIndex := Index;
end;

function TValueLabelSet.GetCurrentValueLabelPair: Pointer;
begin
  Result := nil;
  if Assigned(FCurrentNode) then
    Result := PValuePair(FCurrentNode.Data);
end;

procedure TValueLabelSet.LoadInternal(Root: TDOMNode);
var
  Node: TDOMElement;
  Order: LongInt;
  NewValueLabel: TEpiCustomValueLabel;
  VLClass: TEpiCustomValueLabelClass;
begin
  // Root = <Internal>
  LabelScope := vlsInternal;

  Node := TDOMElement(Root.FirstChild);
  while Assigned(Node) do
  begin
    // Node = <Set ... />
    if Node.CompareName('Set') <> 0 then
      ReportXmlError(EPI_XML_TAG_MISSING, 0, '', []);

    case LabelType of
      ftInteger: VLClass := TEpiIntValueLabel;
      ftFloat:   VLClass := TEpiFloatValueLabel;
      ftString:  VLClass := TEpiStringValueLabel;
    end;
    NewValueLabel := VLClass.Create(Self);
    NewValueLabel.LoadFromXml(Node);
    FData.Add(NewValueLabel);

    Node := TDOMElement(Node.NextSibling);
  end;
end;

procedure TValueLabelSet.SaveInternal(St: TStream; Lvl: integer);
var
  DataNode: TAVLTreeNode;
  S: String;
begin
  S := Ins(Lvl) + '<Internal>' + LineEnding;
  St.Write(S[1], Length(S));

  DataNode := FData.FindLowest;
  while Assigned(DataNode) do
  begin
    TEpiCustomValueLabel(DataNode.Data).SaveToStream(St, Lvl + 1);
    DataNode := FData.FindSuccessor(DataNode);
  end;

  S := Ins(Lvl) + '</Internal>' + LineEnding;
  St.Write(S[1], Length(S));
end;

procedure TValueLabelSet.LoadExternal(Root: TDOMNode);
var
  Importer: TEpiImportExport;
  Node: TDOMNode;
  TmpStr: String;
  LocalDf: TEpiDataFile;
  Idx: Integer;
  ValueField: TEpiField;
  LabelField: TEpiField;
  VLClass: TEpiCustomValueLabelClass;
  NewValueLabel: TEpiCustomValueLabel;
  i: Integer;
begin
  // Root = <External>

  Node := Root.FindNode('FileName');
  if not Assigned(Node) then
    ReportXmlError(EPI_XML_TAG_MISSING, 0,
      'External Valuelabel: File TAG not specified.', []);

  // TODO : Load new XML format the right way...
  // TODO : Include usage of DatafileId.

  Importer := TEpiImportExport.Create;
  // TODO : OnPassWord
{  Importer.OnPassword := OnPassword;
  Importer.OnProgress := OnProgress;
  Importer.OnTranslate := OnTranslate; }
  TmpStr := UTF8Encode(Node.TextContent);
  if not Importer.Import(TmpStr, LocalDf, dftNone) then
    ReportXmlError(EPI_DATAFILE_NOT_OPEN, 0,
      'Datafile %s could not be opened', [TmpStr]);

  if LocalDf.FieldCount < 2 then
    ReportXmlError(EPI_DATAFILE_NOT_OPEN, 0,
      'Datafile %s must have at least 2 fields', [TmpStr]);

  if LocalDf.Size = 0 then
    ReportXmlError(EPI_DATAFILE_NOT_OPEN, 0,
      'Datafile %s does not contain any records', [TmpStr]);


  Idx := -1;
  Node := Root.FindNode('ValueField');
  if Assigned(Node) then
  begin
    ValueField := LocalDf.FieldByName(UTF8Decode(Node.TextContent));
    ExtValField := ValueField.FieldName;
  end else
    ValueField := LocalDf[++Idx];

  Node := Root.FindNode('LabelField');
  if Assigned(Node) then
  begin
    LabelField := LocalDf.FieldByName(UTF8Decode(Node.TextContent));
    ExtLabelField := LabelField.FieldName;
  end else
    LabelField := LocalDf[++Idx];

  // Check for same field.
  if (LabelField = ValueField) then
    // Case where the two tags are the same.
    if Idx = -1 then
      ReportXmlError(EPI_XML_ERROR, 0,
        'ValueField and LabelField must be different', [])
    else
    // Case where ValueField is defined, but not LabelField.
      LabelField := LocalDf[++Idx];

  Case LabelType of
    ftInteger: VLClass := TEpiIntValueLabel;
    ftFloat:   VLClass := TEpiFloatValueLabel;
    ftString:  VLClass := TEpiStringValueLabel;
  end;

  for i := 1 to LocalDf.Size do
  begin
    NewValueLabel := VLClass.Create(Self);
    NewValueLabel.TheLabel := LabelField.AsString[i];
    Case LabelType of
      ftInteger: NewValueLabel.Value := ValueField.AsInteger[i];
      ftFloat:   NewValueLabel.Value := ValueField.AsFloat[i];
      ftString:  NewValueLabel.Value := ValueField.AsString[i];
    end;
    FData.Add(NewValueLabel);
  end;
  FreeAndNil(LocalDf);
  Importer.Free;
end;

procedure TValueLabelSet.SaveExternal(St: TStream; Lvl: integer);
var
  S: string;
begin
  S :=
    Ins(Lvl) + '<External>' + LineEnding +
    Ins(Lvl + 1) + '<File>' + StringToXml(ExtName) + '</File>' + LineEnding +
    Ins(Lvl + 1) + '<DatafileId>' + StringToXml(ExtId) + '</DatafileId>' + LineEnding;
  if ExtValField <> '' then
    S := S +
    Ins(Lvl + 1) + '<ValueField>' + StringToXml(ExtValField) + '</ValueField>' + LineEnding;
  if ExtLabelField <> '' then
    S := S +
    Ins(Lvl + 1) + '<LabelField>' + StringToXml(ExtLabelField) + '</LabelField>' + LineEnding;
  S := S +
    Ins(Lvl) + '</External>' + LineEnding;
  St.Write(S[1], Length(S));
end;

constructor TValueLabelSet.Create(AOwner: TObject; aLabelType: TFieldType);
begin
  inherited Create(AOwner);

  FName := '';
  FId := '';
  FCurrentNode := nil;
  FCurrentIndex := -1;
  FLabelType := aLabelType;

  ExtName := '';
  ExtValField := '';
  ExtLabelField := '';
  Case LabelType of
    ftString:  FData := TAVLTree.Create(@StringCompare);
    ftInteger: FData := TAVLTree.Create(@IntCompare);
    ftFloat:   FData := TAVLTree.Create(@FloatCompare);
  end;
end;

destructor TValueLabelSet.Destroy;
begin
  Clear;
  FreeAndNil(FData);
  inherited;
end;

procedure TEpiCustomValueLabel.SaveToStream(St: TStream; Lvl: integer);
var
  S: String;
begin
  S := Ins(LvL) + '<Set ';

  // Order
  if Order > -1 then
    S := S + 'order="' + IntToStr(Order) + '" ';

  // Value
  S := S + 'value="' + StringToXml(GetValueAsString) + '" ';

  // Label
  if TheLabel <> '' then
    S := S  + ' label="' + TheLabel + '" ';

  // Missing
  if Missing then
    S := S  + ' missing="true" ';
  S := S  +
    '/>' + LineEnding;
end;

{ TEpiCustomValueLabel }

procedure TEpiCustomValueLabel.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMElement;
begin
  // Root = <Set>
  Node := TDOMElement(Root);

  Order    := StrToInt64Def(Node.AttribStrings['order'], -1);
  TheLabel := UTF8Encode(Node.AttribStrings['label']);
  Missing  := WideLowerCase(Node.AttribStrings['missing']) = 'true';
end;

function TEpiIntValueLabel.GetValueAsString: string;
begin
  Result := IntToStr(Value);
end;

{ TEpiIntValueLabel }

procedure TEpiIntValueLabel.LoadFromXml(Root: TDOMNode);
begin
  inherited LoadFromXml(Root);
  Value := StrToInt(TDOMElement(Root).AttribStrings['value']);
end;

function TEpiFloatValueLabel.GetValueAsString: string;
begin
  Result := FloatToStr(Value, EpiInternalFormatSettings);
end;

{ TEpiFloatValueLabel }

procedure TEpiFloatValueLabel.LoadFromXml(Root: TDOMNode);
begin
  inherited LoadFromXml(Root);
  Value := StrToFloat(TDOMElement(Root).AttribStrings['value'], EpiInternalFormatSettings);
end;

function TEpiStringValueLabel.GetValueAsString: string;
begin
  Result := Value;
end;

{ TEpiStringValueLabel }

procedure TEpiStringValueLabel.LoadFromXml(Root: TDOMNode);
begin
  inherited LoadFromXml(Root);
  Value := UTF8Encode(TDOMElement(Root).AttribStrings['value']);
end;

end.
