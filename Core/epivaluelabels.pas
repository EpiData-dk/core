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
    function GetValueAsString: string; virtual; abstract;
  public
    procedure SaveToStream(St: TStream; Lvl: integer); override;
    procedure LoadFromXml(Root: TDOMNode); override;
    property Order: integer read FOrder write FOrder;
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
    procedure SetId(const AValue: string);
    procedure SetLabelType(const AValue: TFieldType);
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
    procedure   SaveToStream(St: TStream; Lvl: Integer); override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    function    NewValueLabel: TEpiCustomValueLabel;
    procedure   Add(AValueLabel: TEpiCustomValueLabel);
    procedure   Clone(var Dest: TValueLabelSet);
    procedure   Clear;
    property    Id:   string read FId write SetId;
    property    Name: string read FName write FName;
    property    Count: Integer read GetCount;
    property    LabelScope: TValueLabelSetScope read FLabelScope write FLabelScope;
    property    LabelType: TFieldType read FLabelType write SetLabelType;
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
    procedure   SaveToStream(St: TStream; Lvl: Integer); override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    procedure   Clear;
    procedure   Clone(var dest: TValueLabelSets);
    procedure   Assign(Const Src: TValueLabelSets);
    function    NewValueLabelSet(ALabelType: TFieldType): TValueLabelSet;
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

function TValueLabelSets.NewValueLabelSet(ALabelType: TFieldType
  ): TValueLabelSet;
begin
  result := TValueLabelSet.Create(Self, ALabelType);
  result.Id := 'valuelabelset_id_' + IntToStr(Count);
  AddValueLabelSet(Result);
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
  NValueLabelSet: TValueLabelSet;
begin
  // Root = <ValueLabels>

  Node := Root.FirstChild;
  while Assigned(Node) do
  begin
    if Node.CompareName('ValueLabel') <> 0 then
      ReportXmlError(EPI_XML_TAG_MISSING, 0, '', []);

    NValueLabelSet := TValueLabelSet.Create(Self, XmlNameToFieldType(Node.FindNode('Type').TextContent));
    NValueLabelSet.LoadFromXml(Node);
    AddValueLabelSet(NValueLabelSet);

    Node := Node.NextSibling;
  end;
end;

{ TValueLabelSet }

procedure TValueLabelSet.Clear;
begin
  FCurrentIndex := -1;
  FName := '';
  FId := '';
  FCurrentNode := nil;

  // This disposes of the objects too.
  FData.FreeAndClear;
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
      SaveExternal(St, Lvl + 1);
    vlsInternal:
      SaveInternal(St, Lvl + 1);
  end;

  S :=
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
  begin
    LoadInternal(Node);
    Node := nil;
  end
  else
    Node := Root.FindNode('External');

  if Assigned(Node) then
    LoadExternal(Node);
end;

function TValueLabelSet.NewValueLabel: TEpiCustomValueLabel;
begin
  case LabelType of
    ftInteger: result := TEpiIntValueLabel.Create(Self);
    ftFloat:   result := TEpiFloatValueLabel.Create(Self);
    ftString:  result := TEpiStringValueLabel.Create(Self);
  end;
  result.Order := Count;
  Add(result);
end;

procedure TValueLabelSet.Clone(var Dest: TValueLabelSet);
var
  AVLNode: TAVLTreeNode;
begin
{  if not Assigned(Dest) then
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
  end;   }
end;

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

function TValueLabelSet.GetCount: integer;
begin
  result := FData.Count;
end;

procedure TValueLabelSet.SetId(const AValue: string);
begin
  if FId = AValue then exit;
  if Assigned(Owner) then
  with TValueLabelSets(Owner) do
  begin
    DeleteValueLabelSet(FId);
    FId := AValue;
    AddValueLabelSet(Self);
  end else
    FId := AValue;
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

procedure TValueLabelSet.LoadInternal(Root: TDOMNode);
var
  Node: TDOMElement;
  Order: LongInt;
  NValueLabel: TEpiCustomValueLabel;
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

    NValueLabel := NewValueLabel;
    NValueLabel.LoadFromXml(Node);
//    FData.Add(NValueLabel);

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
  NValueLabel: TEpiCustomValueLabel;
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
    NValueLabel := VLClass.Create(Self);
    NValueLabel.TheLabel := LabelField.AsString[i];
    Case LabelType of
      ftInteger: TEpiIntValueLabel(NValueLabel).Value := ValueField.AsInteger[i];
      ftFloat:   TEpiFloatValueLabel(NValueLabel).Value := ValueField.AsFloat[i];
      ftString:  TEpiStringValueLabel(NValueLabel).Value := ValueField.AsString[i];
    end;
    FData.Add(NValueLabel);
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

procedure TValueLabelSet.Add(AValueLabel: TEpiCustomValueLabel);
begin
  FData.Add(AValueLabel);
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
  St.Write(S[1], Length(S));
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
