unit epivaluelabels;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, epidatatypes, AVL_Tree, Variants;
  
type
  // vlsLocal retained for compatability with old .REC/.CHK format.
  TValueLabelSetScope = (vlsGlobal, vlsLocal, vlsFile);

  { TValueLabelSet }

  TValueLabelSet = class(TObject)
  private
    FData:     TAVLTree;
    FExtName: string;
    FId:       string;
    FName:     string;
    FLabelScope: TValueLabelSetScope;
    FLabelType: TFieldType;
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
  public
    constructor Create(aLabelType: TFieldType = ftInteger);
    destructor  Destroy; override;
    procedure   AddValueLabelPair(Const aValue: Variant; Const aLabel: string; aMissing: Boolean = false);
    procedure   Clone(var Dest: TValueLabelSet);
    procedure   Clear;
    property    Id:   string read FId write FId;
    property    Name: string read FName write FName;
    property    ExtName: string read FExtName write FExtName;
    property    ValueLabel[Const aValue: Variant]: string read GetValueLabel write SetValueLabel;
    property    MissingValue[Const aValue: Variant]: boolean read GetMissingValue write SetMissingValue;
    // Should only be used to read and traverse the tree.
    property    Values[Const Index: Integer]: Variant read GetValues;
    // Should only be used to read and traverse the tree.
    property    Labels[Const Index: Integer]: string read GetLabels;
    // Should only be used to read and traverse the tree.
    property    MissingValues[Const Index: Integer]: boolean read GetMissingValues;
    property    Count: Integer read GetCount;
    property    LabelScope: TValueLabelSetScope read FLabelScope write FLabelScope;
    property    LabelType: TFieldType read FLabelType write SetLabelType;
  end;

  { TValueLabelSets }

  TValueLabelSets = class(TObject)
  private
    FList: TStringList;
    function GetCount:integer;
    function GetItem(index:integer):TValueLabelSet;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Clear;
    procedure   Clone(var dest: TValueLabelSets);
    procedure   Assign(Const Src: TValueLabelSets);
    function    ValueLabelSetByName(Const Id: string): TValueLabelSet;
    function    ValueLabelSetExits(Const Id: string; var aValueLabelSet: TValueLabelSet): boolean;
    procedure   AddValueLabelSet(aValueLabelSet: TValueLabelSet);
    procedure   DeleteValueLabelSet(Const Id: string);
    property    Count:integer read GetCount;
    property    Items[index: integer]: TValueLabelSet read GetItem; default;
  end;

implementation

uses
  SysUtils;

type
  TValuePair = record
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
    Dest := TValueLabelSets.Create();

  Dest.Clear;
  for i := 0 to FList.Count - 1 do
  begin
    Tmp := nil;
    TValueLabelSet(FList.Objects[i]).Clone(tmp);
    Dest.AddValueLabelSet(tmp);
  end;
end;

constructor TValueLabelSets.Create;
begin
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


procedure TValueLabelSet.Clone(var Dest: TValueLabelSet);
var
  OldValuePair, NewValuePair: PValuePair;
  AVLNode: TAVLTreeNode;
begin
  if not Assigned(Dest) then
    Dest := TValueLabelSet.Create(LabelType);

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

function NumberCompare(Item1, Item2: Pointer): Integer;
var
  V: Variant;
begin
  V := PValuePair(Item1)^.FValue - PValuePair(Item2)^.FValue;
  if V > 0 then
    result := 1
  else if V < 0 then
    result := -1
  else
    result := 0;
end;

function StringCompare(Item1, Item2: Pointer): Integer;
begin
  Result := strcomp(PChar(VarToStr(PValuePair(Item1)^.FValue)), PChar(VarToStr(PValuePair(Item2)^.FValue)));
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
    if LabelType = ftString then
      FData.OnCompare := @StringCompare
    else
      FData.OnCompare := @NumberCompare;
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

constructor TValueLabelSet.Create(aLabelType: TFieldType);
begin
  FName := '';
  FId := '';
  FCurrentNode := nil;
  FCurrentIndex := -1;
  FLabelType := aLabelType;
  if LabelType = ftString then
    FData := TAVLTree.Create(@StringCompare)
  else
    FData := TAVLTree.Create(@NumberCompare);
end;

destructor TValueLabelSet.Destroy;
begin
  Clear;
  FreeAndNil(FData);
  inherited;
end;

end.
