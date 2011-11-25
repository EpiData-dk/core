unit epirelations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, epicustombase, epidatafiles, epidatafilestypes;

type
 TEpiRelations = class;
 TEpiPrimaryKeys = class;
 TEpiPrimaryKey = class;
 TEpiRelates = class;
 TEpiRelate = class;

 { TEpiRelations }

 TEpiRelations = class(TEpiCustomBase)
 private
   FPrimaryKeys: TEpiPrimaryKeys;
   FRelates: TEpiRelates;
   function    GetPrimaryKey(Index: integer): TEpiPrimaryKey;
   function    GetRelate(Index: integer): TEpiRelate;
 protected
   function    ScrambleXml: boolean; override;
 public
   constructor Create(AOwner: TEpiCustomBase); override;
   destructor  Destroy; override;
   function    XMLName: string; override;
   function    SaveToXml(Content: String; Lvl: integer): string; override;
   procedure   LoadFromXml(Root: TDOMNode); override;
   function    NewPrimaryKey: TEpiPrimaryKey;
   function    NewRelate: TEpiRelate;
   property    PrimaryKeys: TEpiPrimaryKeys read FPrimaryKeys;
   property    PrimaryKey[Index: integer]: TEpiPrimaryKey read GetPrimaryKey;
   property    Relates: TEpiRelates read FRelates;
   property    Relate[Index: integer]: TEpiRelate read GetRelate;
 end;

 { TEpiPrimaryKeys }

 TEpiPrimaryKeys = class(TEpiCustomList)
 protected
   function Prefix: string; override;
 public
   constructor Create(AOwner: TEpiCustomBase); override;
   destructor  Destroy; override;
   function    XMLName: string; override;
   procedure   LoadFromXml(Root: TDOMNode); override;
   function    NewPrimaryKey: TEpiPrimaryKey;
 end;

 { TEpiPrimaryKey }

 TEpiPrimaryKey = class(TEpiCustomItem)
 private
   FDataFile: TEpiDataFile;
   FFields: TEpiFields;
 protected
   function WriteNameToXml: boolean; override;
 public
   constructor Create(AOwner: TEpiCustomBase); override;
   destructor  Destroy; override;
   function    XMLName: string; override;
   function    SaveToXml(Content: String; Lvl: integer): string; override;
   procedure   LoadFromXml(Root: TDOMNode); override;
   property    DataFile: TEpiDataFile read FDataFile write FDataFile;
   property    Fields: TEpiFields read FFields;
 end;

 { TEpiRelates }
 TEpiRelateChangeEventType = (
   ercaSetDataFile, ercaSetDest, ercaSetField, ercaSetType, ercaSetValue
 );

 TEpiRelates = class(TEpiCustomList)
 protected
   function Prefix: string; override;
 public
   constructor Create(AOwner: TEpiCustomBase); override;
   destructor  Destroy; override;
   function XMLName: string; override;
   procedure   LoadFromXml(Root: TDOMNode); override;
   function    NewRelate: TEpiRelate;
 end;

 { TEpiRelate }

 TEpiRelate = class(TEpiCustomItem)
 private
   FDataFile: TEpiDataFile;
   FDestination: TEpiDataFile;
   FField: TEpiField;
   FRelateType: integer;
   FValue: EpiVariant;
   procedure SetDataFile(const AValue: TEpiDataFile);
   procedure SetDestination(const AValue: TEpiDataFile);
   procedure SetField(const AValue: TEpiField);
   procedure SetRelateType(const AValue: integer);
   procedure SetValue(const AValue: EpiVariant);
 protected
   function WriteNameToXml: boolean; override;
 public
   constructor Create(AOwner: TEpiCustomBase); override;
   destructor Destroy; override;
   function XMLName: string; override;
   function    SaveToXml(Content: String; Lvl: integer): string; override;
   procedure   LoadFromXml(Root: TDOMNode); override;
   property    DataFile: TEpiDataFile read FDataFile write SetDataFile;
   property    Field: TEpiField read FField write SetField;
   // TODO : Relate - do something better than variants!
   property    Value: EpiVariant read FValue write SetValue;
   property    Destination: TEpiDataFile read FDestination write SetDestination;
   property    RelateType: integer read FRelateType write SetRelateType;
 end;

implementation

uses
  epidocument, epistringutils;

{ TEpiRelations }

function TEpiRelations.GetPrimaryKey(Index: integer): TEpiPrimaryKey;
begin
  result := TEpiPrimaryKey(PrimaryKeys.Items[Index]);
end;

function TEpiRelations.GetRelate(Index: integer): TEpiRelate;
begin
  result := TEpiRelate(Relates.Items[Index]);
end;

function TEpiRelations.ScrambleXml: boolean;
begin
  Result := TEpiDocument(Owner).XMLSettings.Scrambled;
end;

constructor TEpiRelations.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  FPrimaryKeys := TEpiPrimaryKeys.Create(Self);
  FPrimaryKeys.ItemOwner := true;

  FRelates := TEpiRelates.Create(Self);
  FRelates.ItemOwner := true;

  RegisterClasses([FPrimaryKeys, FRelates]);
end;

destructor TEpiRelations.Destroy;
begin
  FRelates.Free;
  FPrimaryKeys.Free;
  inherited Destroy;
end;

function TEpiRelations.XMLName: string;
begin
  Result := rsRelations;
end;

function TEpiRelations.SaveToXml(Content: String; Lvl: integer): string;
begin
  if (PrimaryKeys.Count = 0) and (Relates.Count = 0) then exit;
  Result := inherited SaveToXml(Content, Lvl);
end;

procedure TEpiRelations.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMNode;
begin
  inherited LoadFromXml(Root);

  // Root = <Relations>
  if LoadNode(Node, Root, rsPrimaryKeys, false) then
    PrimaryKeys.LoadFromXml(Node);
  if LoadNode(Node, Root, rsRelates, false) then
    Relates.LoadFromXml(Node);
end;

function TEpiRelations.NewPrimaryKey: TEpiPrimaryKey;
begin
  result := PrimaryKeys.NewPrimaryKey;
end;

function TEpiRelations.NewRelate: TEpiRelate;
begin
  result := Relates.NewRelate;
end;

{ TEpiPrimaryKeys }

function TEpiPrimaryKeys.Prefix: string;
begin
  Result := 'primary_key_';
end;

constructor TEpiPrimaryKeys.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
end;

destructor TEpiPrimaryKeys.Destroy;
begin
  inherited Destroy;
end;

function TEpiPrimaryKeys.XMLName: string;
begin
  Result := rsPrimaryKeys;
end;

procedure TEpiPrimaryKeys.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMNode;
  NPrimaryKey: TEpiPrimaryKey;
begin
  inherited LoadFromXml(Root);
  // Root = <PrimaryKeys>
  Node := Root.FirstChild;
  while Assigned(Node) do
  begin
    CheckNode(Node, rsPrimaryKey);

    NPrimaryKey := NewPrimaryKey;
    NPrimaryKey.LoadFromXml(Node);

    Node := Node.NextSibling;
  end;
end;

function TEpiPrimaryKeys.NewPrimaryKey: TEpiPrimaryKey;
begin
  Result := TEpiPrimaryKey(NewItem(TEpiPrimaryKey));
end;

{ TEpiPrimaryKey }

function TEpiPrimaryKey.WriteNameToXml: boolean;
begin
  Result := false;
end;

constructor TEpiPrimaryKey.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  FFields := TEpiFields.Create(Self);
end;

destructor TEpiPrimaryKey.Destroy;
begin
  FFields.Free;
  inherited Destroy;
end;

function TEpiPrimaryKey.XMLName: string;
begin
  Result := rsPrimaryKey;
end;

function TEpiPrimaryKey.SaveToXml(Content: String; Lvl: integer): string;
var
  i: Integer;
begin
  Content := Fields[0].Name;
  for i := 1 to Fields.Count - 1 do
    Content += ',' + Fields[i].Name;
  Content :=
    SaveNode(Lvl + 1, rsDataFileId, DataFile.Name) +
    SaveNode(Lvl + 1, rsFieldIdList, Content);
  Result := inherited SaveToXml(Content, Lvl);
end;

procedure TEpiPrimaryKey.LoadFromXml(Root: TDOMNode);
var
  FieldList: TStrings;
  i: Integer;
begin
  inherited LoadFromXml(Root);

  DataFile := TEpiDataFile(TEpiDocument(RootOwner).DataFiles.GetItemByName(LoadNodeString(Root, rsDataFileId)));

  FieldList := nil;
  SplitString(LoadNodeString(Root, rsFieldIdList), FieldList, [',']);
  for i := 0 to FieldList.Count - 1 do
    Fields.AddItem(DataFile.Fields.GetItemByName(FieldList[i]));
end;

{ TEpiRelates }

function TEpiRelates.Prefix: string;
begin
  Result := 'relate_id_';
end;

constructor TEpiRelates.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
end;

destructor TEpiRelates.Destroy;
begin
  inherited Destroy;
end;

function TEpiRelates.XMLName: string;
begin
  Result := rsRelates;
end;

procedure TEpiRelates.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMNode;
  NRelate: TEpiRelate;
begin
  inherited LoadFromXml(Root);
  // Root = <Relates>

  Node := Root.FirstChild;
  while Assigned(Node) do
  begin
    CheckNode(Node, rsRelate);

    NRelate := NewRelate;
    NRelate.LoadFromXml(Node);

    Node := Node.NextSibling;
  end;
end;

function TEpiRelates.NewRelate: TEpiRelate;
begin
  result := TEpiRelate(NewItem(TEpiRelate));
end;

{ TEpiRelate }

procedure TEpiRelate.SetDataFile(const AValue: TEpiDataFile);
var
  Val: TEpiDataFile;
begin
  if FDataFile = AValue then exit;
  Val := FDataFile;
  FDataFile := AValue;
  DoChange(eegRelates, Word(ercaSetDataFile), Val);
end;

procedure TEpiRelate.SetDestination(const AValue: TEpiDataFile);
var
  Val: TEpiDataFile;
begin
  if FDestination = AValue then exit;
  Val := FDestination;
  FDestination := AValue;
  DoChange(eegRelates, Word(ercaSetDest), Val);
end;

procedure TEpiRelate.SetField(const AValue: TEpiField);
var
  Val: TEpiField;
begin
  if FField = AValue then exit;
  Val := FField;
  FField := AValue;
  DoChange(eegRelates, Word(ercaSetField), Val);
end;

procedure TEpiRelate.SetRelateType(const AValue: integer);
var
  Val: LongInt;
begin
  if FRelateType = AValue then exit;
  Val := FRelateType;
  FRelateType := AValue;
  DoChange(eegRelates, Word(ercaSetType), @Val);
end;

procedure TEpiRelate.SetValue(const AValue: EpiVariant);
var
  Val: EpiVariant;
begin
  if FValue = AValue then exit;
  Val := FValue;
  FValue := AValue;
  DoChange(eegRelates, Word(ercaSetValue), @Val);
end;

function TEpiRelate.WriteNameToXml: boolean;
begin
  Result := false;
end;

constructor TEpiRelate.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
end;

destructor TEpiRelate.Destroy;
begin
  inherited Destroy;
end;

function TEpiRelate.XMLName: string;
begin
  Result := rsRelate;
end;

function TEpiRelate.SaveToXml(Content: String; Lvl: integer): string;
begin
  Content :=
    SaveNode(Lvl + 1, rsDataFileId, DataFile.Name) +
    SaveNode(Lvl + 1, rsFieldId, Field.Name) +
    SaveNode(Lvl + 1, rsValue, String(Value)) +
    SaveNode(Lvl + 1, rsDestDataFileId, Destination.Name) +
    SaveNode(Lvl + 1, rsType, RelateType);
  Result := inherited SaveToXml(Content, Lvl);
end;

procedure TEpiRelate.LoadFromXml(Root: TDOMNode);
var
  DFS: TEpiDataFiles;
begin
  inherited LoadFromXml(Root);

  // Root = <Relate>
  DFS      := TEpiDocument(RootOwner).DataFiles;
  DataFile := TEpiDataFile(DFS.GetItemByName(LoadNodeString(Root, rsDataFileId)));
  Field    := TEpiField(DataFile.Fields.GetItemByName(LoadNodeString(Root, rsFieldId)));
  Value    := LoadNodeString(Root, rsValue);

  Destination := TEpiDataFile(DFS.GetItemByName(LoadNodeString(Root, rsDestDataFileId)));
  RelateType  := LoadNodeInt(Root, rsType);
end;

end.

