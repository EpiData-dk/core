unit epirelate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidatafile, epicustomclass, DOM;

type
  TEpiRelates = class;
  TEpiPrimaryKeys= class;
  TEpiPrimaryKey = class;
  TEpiRelate = class;

  { TEpiRelates }

  TEpiRelates = class(TEpiCustomClass)
  private
    FRelateList: TFPList;
    FPrimaryKeys: TEpiPrimaryKeys;
    function GetCount: integer;
    function GetRelate(Index: integer): TEpiRelate;
    procedure SetPrimaryKeys(const AValue: TEpiPrimaryKeys);
  public
    constructor Create(AOwner: TObject); override;
    destructor  Destroy; override;
    procedure   SaveToStream(St: TStream; Lvl: integer); override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    procedure   Add(aRelate: TEpiRelate);
    Property    PrimaryKeys: TEpiPrimaryKeys read FPrimaryKeys write SetPrimaryKeys;
    Property    Relate[Index: integer]: TEpiRelate read GetRelate; default;
    property    Count: integer read GetCount;
  end;

  { TEpiPrimaryKeys }

  TEpiPrimaryKeys = class(TEpiCustomClass)
  private
    FList: TFPList;
    function GetCount: integer;
    function GetPrimaryKey(Index: integer): TEpiPrimaryKey;
  public
    constructor Create(AOwner: TObject); override;
    destructor Destroy; override;
    procedure  SaveToStream(St: TStream; Lvl: integer); override;
    procedure  LoadFromXml(Root: TDOMNode); override;
    procedure  Add(APrimaryKey: TEpiPrimaryKey);
    Property   PrimaryKey[Index: integer]: TEpiPrimaryKey read GetPrimaryKey; default;
    Property   Count: integer read GetCount;
  end;

  TEpiPrimaryKey = class(TEpiCustomClass)
  private
    FDataFile: TEpiDataFile;
    FFields: TEpiFields;
    procedure SetDataFile(const AValue: TEpiDataFile);
    procedure SetFields(const AValue: TEpiFields);
  public
    constructor Create(AOwner: TObject); override;
    destructor  Destroy; override;
    procedure   SaveToStream(St: TStream; Lvl: integer); override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    Property    DataFile: TEpiDataFile read FDataFile write SetDataFile;
    Property    Fields: TEpiFields read FFields write SetFields;
  end;

  TEpiRelate = class(TEpiCustomClass)
  private
    FDataFile: TEpiDataFile;
    FDestination: TEpiDataFile;
    FField: TEpiField;
    FRelateType: integer;
    FValue: string;
    procedure SetDataFile(const AValue: TEpiDataFile);
    procedure SetDestination(const AValue: TEpiDataFile);
    procedure SetField(const AValue: TEpiField);
    procedure SetRelateType(const AValue: integer);
    procedure SetValue(const AValue: string);
  public
    constructor Create(AOwner: TObject); override;
    destructor  Destroy; override;
    procedure   SaveToStream(St: TStream; Lvl: integer); override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    Property    DataFile: TEpiDataFile read FDataFile write SetDataFile;
    Property    Field: TEpiField read FField write SetField;
    Property    Value: string read FValue write SetValue;
    Property    Destination: TEpiDataFile read FDestination write SetDestination;
    Property    RelateType: integer read FRelateType write SetRelateType;
  end;

implementation

uses
  epidataglobals, epidocument, epistringutils;

{ TEpiRelates }

function TEpiRelates.GetRelate(Index: integer): TEpiRelate;
begin
  result := TEpiRelate(FRelateList[Index]);
end;

function TEpiRelates.GetCount: integer;
begin
  result := FRelateList.Count;
end;

procedure TEpiRelates.SetPrimaryKeys(const AValue: TEpiPrimaryKeys);
begin
  if FPrimaryKeys = AValue then exit;
  FPrimaryKeys := AValue;
end;

constructor TEpiRelates.Create(AOwner: TObject);
begin
  Inherited;
  FPrimaryKeys := TEpiPrimaryKeys.Create(Self);
  FRelateList := TFPList.Create;
end;

destructor TEpiRelates.Destroy;
begin
  FRelateList.Free;
  inherited Destroy;
end;

procedure TEpiRelates.SaveToStream(St: TStream; Lvl: integer);
var
  S: String;
  i: Integer;
begin
  if count = 0 then exit;

  S :=
    Ins(Lvl) + '<Relates>' + LineEnding;
  St.Write(S[1], Length(S));

  PrimaryKeys.SaveToStream(St, Lvl + 1);
  for i := 0 to Count - 1 do
    Relate[i].SaveToStream(St, Lvl + 1);

  S :=
    Ins(Lvl) + '</Relates>' + LineEnding;
  St.Write(S[1], Length(S));
end;

procedure TEpiRelates.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMNode;
  NewRelate: TEpiRelate;
begin
  // Root = <Relates>
  Node := Root.FindNode('PrimaryKeys');
  PrimaryKeys.LoadFromXml(Node);

  Node := Root.FirstChild;
  while Assigned(Node) do
  begin
    if Node.CompareName('PrimaryKeys') = 0 then
    begin
      Node := Node.NextSibling;
      Continue;
    end;

    if Node.CompareName('Relate') <> 0 then
      ReportXmlError(EPI_XML_TAG_MISSING, 0, '', []);

    NewRelate := TEpiRelate.Create(Self);
    NewRelate.LoadFromXml(Node);
    Add(NewRelate);

    Node := Node.NextSibling;
  end;
end;

procedure TEpiRelates.Add(aRelate: TEpiRelate);
begin
  FRelateList.Add(aRelate);
end;

{ TEpiPrimaryKeys }

function TEpiPrimaryKeys.GetCount: integer;
begin
  Result := FList.Count;
end;

function TEpiPrimaryKeys.GetPrimaryKey(Index: integer): TEpiPrimaryKey;
begin
  Result := TEpiPrimaryKey(FList[Index]);
end;

constructor TEpiPrimaryKeys.Create(AOwner: TObject);
begin
  Inherited;
  FList := TFPList.Create;
end;

destructor TEpiPrimaryKeys.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TEpiPrimaryKeys.SaveToStream(St: TStream; Lvl: integer);
var
  S: String;
  i: Integer;
begin
  S :=
    Ins(Lvl) + '<PrimaryKeys>' + LineEnding;
  St.Write(S[1], Length(S));

  For i := 0 to Count - 1 do
    PrimaryKey[i].SaveToStream(St, Lvl + 1);

  S :=
    Ins(Lvl) + '</PrimaryKeys>' + LineEnding;
  St.Write(S[1], Length(S));
end;

procedure TEpiPrimaryKeys.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMNode;
  NewPrimaryKey: TEpiPrimaryKey;
begin
  // Root = <PrimaryKeys>

  Node := Root.FirstChild;
  while Assigned(Node) do
  begin
    if Node.CompareName('PrimaryKey') <> 0 then
      ReportXmlError(EPI_XML_TAG_MISSING, 0, '', []);

    NewPrimaryKey := TEpiPrimaryKey.Create(Self);
    NewPrimaryKey.LoadFromXml(Node);
    Add(NewPrimaryKey);

    Node := Node.NextSibling;
  end;
end;

procedure TEpiPrimaryKeys.Add(APrimaryKey: TEpiPrimaryKey);
begin
  FList.Add(APrimaryKey);
end;

{ TEpiPrimaryKey }

procedure TEpiPrimaryKey.SetDataFile(const AValue: TEpiDataFile);
begin
  if FDataFile = AValue then exit;
  FDataFile := AValue;
end;

procedure TEpiPrimaryKey.SetFields(const AValue: TEpiFields);
begin
  if FFields = AValue then exit;
  FFields := AValue;
end;

constructor TEpiPrimaryKey.Create(AOwner: TObject);
begin
  inherited;
  FFields := TEpiFields.Create(Self);
  FFields.Owned := false;
end;

destructor TEpiPrimaryKey.Destroy;
begin
  inherited Destroy;
end;

procedure TEpiPrimaryKey.SaveToStream(St: TStream; Lvl: integer);
var
  S: String;
  i: Integer;
begin
  S :=
    Ins(Lvl) +   '<PrimaryKey>' + LineEnding +
    Ins(Lvl + 1) + '<DataFileId>' + StringToXml(DataFile.Id) + '</DataFileId>' + LineEnding +
    Ins(Lvl + 1) + '<FieldIdList>' + StringToXml(Fields[0].Id);
  for i := 1 to Fields.Count - 1 do
    S += ',' + StringToXml(Fields[i].Id);
  S +=             '</FieldIdList>' + LineEnding +
    Ins(Lvl) +   '</PrimaryKey>' + LineEnding;
  St.Write(S[1], Length(S));
end;

procedure TEpiPrimaryKey.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMNode;
  List: TStrings;
  i: Integer;
  DfId: String;
begin
  // Root = <PrimaryKey>
  Node := Root.FindNode('DataFileId');
  DfId := UTF8Encode(Node.TextContent);
  // TODO : Optimize casting...
  DataFile := TEpiDocument(TEpiRelates(TEpiPrimaryKeys(Owner).Owner).Owner).DataFiles.DataFileById(DfID);

  Node := Root.FindNode('FieldIdList');
  List := nil;
  SplitString(UTF8Encode(Node.TextContent), List, [',']);
  for i := 0 to List.Count - 1 do
    Fields.Add(DataFile.FieldById(List[i]));
end;

{ TEpiRelate }

procedure TEpiRelate.SetDataFile(const AValue: TEpiDataFile);
begin
  if FDataFile = AValue then exit;
  FDataFile := AValue;
end;

procedure TEpiRelate.SetDestination(const AValue: TEpiDataFile);
begin
  if FDestination = AValue then exit;
  FDestination := AValue;
end;

procedure TEpiRelate.SetField(const AValue: TEpiField);
begin
  if FField = AValue then exit;
  FField := AValue;
end;

procedure TEpiRelate.SetRelateType(const AValue: integer);
begin
  if FRelateType = AValue then exit;
  FRelateType := AValue;
end;

procedure TEpiRelate.SetValue(const AValue: string);
begin
  if FValue = AValue then exit;
  FValue := AValue;
end;

constructor TEpiRelate.Create(AOwner: TObject);
begin
  Inherited;
end;

destructor TEpiRelate.Destroy;
begin
  inherited Destroy;
end;

procedure TEpiRelate.SaveToStream(St: TStream; Lvl: integer);
var
  S: String;
begin
  S :=
    Ins(Lvl) +   '<Relate>' + LineEnding +
    Ins(Lvl + 1) + '<DataFileId>' + StringToXml(DataFile.Id) + '</DataFileId>' + LineEnding +
    Ins(Lvl + 1) + '<FieldId>' + StringToXml(Field.Id) + '</FieldId>' + LineEnding +
    Ins(Lvl + 1) + '<Value>' + StringToXml(Value) + '</Value>' + LineEnding +
    Ins(Lvl + 1) + '<DestDataFileId>' + StringToXml(Destination.Id) + '</DestDataFileId>' + LineEnding +
    // TODO : Relation types...
//    Ins(Lvl + 1) + '<Type>' + StringToXml(Field.Id) + '</Type>' + LineEnding +
    Ins(Lvl) +   '</Relate>' + LineEnding;
  St.Write(S[1], Length(S));
end;

procedure TEpiRelate.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMNode;
  TmpStr: String;
begin
  // Root = <Relate>
  Node        := Root.FindNode('DataFileId');
  TmpStr      := UTF8Encode(Node.TextContent);
  DataFile    := TEpiDocument(TEpiRelates(Owner).Owner).DataFiles.DataFileById(TmpStr);

  TmpStr      := UTF8Encode(Root.FindNode('FieldId').TextContent);
  Field       := DataFile.FieldById(TmpStr);

  Value       := UTF8Encode(Root.FindNode('Value').TextContent);

  Node        := Root.FindNode('DestDataFileId');
  TmpStr      := UTF8Encode(Node.TextContent);
  Destination := TEpiDocument(TEpiRelates(Owner).Owner).DataFiles.DataFileById(TmpStr);

  // TODO : Relation type...
end;

end.

