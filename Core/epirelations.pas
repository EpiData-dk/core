unit epirelations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz2_DOM, epicustombase, epidatafiles, epidatafilestypes;

type
  TEpiRelationList = class;
  TEpiMasterRelation = class;
  TEpiDetailRelation = class;

  { TEpiMasterRelation }

  TEpiMasterRelation = class(TEpiCustomItem)
  private
    FDatafile: TEpiDataFile;
    FDetailRelations: TEpiRelationList;
    procedure DataFileHook(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
    function GetDetailRelation(Index: integer): TEpiDetailRelation;
    procedure UpdateDataFileHook(Const OldDf, NewDf: TEpiDataFile);
    procedure SetDatafile(AValue: TEpiDataFile);
  protected
    { Cloning }
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase =
       nil): TEpiCustomBase; override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
    procedure LoadFromXml(Root: TDOMNode); override;
    function SaveAttributesToXml: string; override;
    function XMLName: string; override;
    function NewDetailRelation: TEpiDetailRelation;
    property Datafile: TEpiDataFile read FDatafile write SetDatafile;
    property DetailRelation[Index: integer]: TEpiDetailRelation read GetDetailRelation; default;
    property DetailRelations: TEpiRelationList read FDetailRelations;
  end;

  { TEpiDetailRelation }

  TEpiDetailRelation = class(TEpiMasterRelation)
  private
    FFieldList: TEpiFields;
    FValueList: TStringList;
    procedure FieldHook(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
    function  GetMasterRelation: TEpiMasterRelation;
    function  GetRelateField(Index: Integer): TEpiField;
    function  GetRelateValue(Index: Integer): string;
    procedure UpdateFieldHook(Const OldField, NewField: TEpiField);
  protected
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase =
       nil): TEpiCustomBase; override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
    procedure LoadFromXml(Root: TDOMNode); override;
    function SaveAttributesToXml: string; override;
    procedure AddFieldValuePair(Const Field: TEpiField; Const Value: EpiString);
    procedure RemoveFieldValuePair(Const Field: TEpiField; Const Value: EpiString);
    property RelateField[Index: Integer]: TEpiField read GetRelateField;
    property RelateValue[Index: Integer]: string read GetRelateValue;
    property MasterRelation: TEpiMasterRelation read GetMasterRelation;
  end;

  { TEpiRelationList }

  TEpiRelationList = class(TEpiCustomList)
  private
    function GetMasterRelation(Index: integer): TEpiMasterRelation;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    function XMLName: string; override;
    function NewMasterRelation: TEpiMasterRelation;
    function ItemClass: TEpiCustomItemClass; override;
    property MasterRelation[Index: integer]: TEpiMasterRelation read GetMasterRelation; default;
  end;

implementation

uses
  epidocument;

{ TEpiMasterRelation }

procedure TEpiMasterRelation.DataFileHook(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
begin
  if Initiator <> FDatafile then exit;

  if NOT
      (
       (EventGroup = eegCustomBase) and
       (EventType  = Word(ecceDestroy))
      )
  then
    Exit;

  Data := Datafile;
  Datafile := nil;
  DoChange(eegCustomBase, Word(ecceReferenceDestroyed), Data);
end;

function TEpiMasterRelation.GetDetailRelation(Index: integer
  ): TEpiDetailRelation;
begin
  result := TEpiDetailRelation(FDetailRelations.Items[Index]);
end;

procedure TEpiMasterRelation.UpdateDataFileHook(const OldDf, NewDf: TEpiDataFile
  );
begin
  if Assigned(OldDF) then
    OldDf.UnRegisterOnChangeHook(@DataFileHook);

  if Assigned(NewDf) then
    NewDf.RegisterOnChangeHook(@DataFileHook, true);
end;

procedure TEpiMasterRelation.SetDatafile(AValue: TEpiDataFile);
begin
  if FDatafile = AValue then Exit;
  UpdateDataFileHook(FDatafile, AValue);
  FDatafile := AValue;
end;

function TEpiMasterRelation.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase
  ): TEpiCustomBase;
var
  Cloned: Boolean;
begin
  // Always assume full clone!
  with TEpiMasterRelation(Dest) do
    DataFile := TEpiDataFile(TEpiDocument(RootOwner).DataFiles.GetItemByName(Self.Datafile.Name));

  Result := inherited DoClone(AOwner, Dest);
end;

constructor TEpiMasterRelation.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  Datafile := nil;

  FDetailRelations := TEpiRelationList.Create(Self);
  FDetailRelations.ItemOwner := true;

  RegisterClasses([FDetailRelations]);
end;

destructor TEpiMasterRelation.Destroy;
begin
  Datafile := nil;
  FDetailRelations.Free;
  inherited Destroy;
end;

procedure TEpiMasterRelation.LoadFromXml(Root: TDOMNode);
var
  DfId: EpiString;
  Node: TDOMNode;
begin
  inherited LoadFromXml(Root);

  DfId := LoadAttrString(Root, rsDataFileRef);
  Datafile := TEpiDataFile(TEpiDocument(RootOwner).DataFiles.GetItemByName(DfId));

  if not Assigned(FDatafile) then
    Raise Exception.Create('MasterRelation - DatafileId not found: ' + DfId);

  if LoadNode(Node, Root, rsRelations, false) then
    FDetailRelations.LoadFromXml(Node);
end;

function TEpiMasterRelation.SaveAttributesToXml: string;
begin
  Result := inherited SaveAttributesToXml;

  Result +=
    SaveAttr(rsDataFileRef, Datafile.Name);
end;

function TEpiMasterRelation.XMLName: string;
begin
  Result := rsRelation;
end;

function TEpiMasterRelation.NewDetailRelation: TEpiDetailRelation;
begin
  result := TEpiDetailRelation(FDetailRelations.NewItem(TEpiDetailRelation));
end;

{ TEpiDetailRelation }

procedure TEpiDetailRelation.FieldHook(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
begin
{  if Initiator <> FRelateField then exit;

  if NOT
      (
       (EventGroup = eegCustomBase) and
       (EventType  = Word(ecceDestroy))
      )
  then
    Exit;

  Data := FRelateField;
  RelateField := nil;
  DoChange(eegCustomBase, Word(ecceReferenceDestroyed), Data);    }
end;

procedure TEpiDetailRelation.UpdateFieldHook(const OldField, NewField: TEpiField
  );
begin
  if Assigned(OldField) then
    OldField.UnRegisterOnChangeHook(@FieldHook);

  if Assigned(NewField) then
    NewField.RegisterOnChangeHook(@FieldHook, True);
end;

function TEpiDetailRelation.GetMasterRelation: TEpiMasterRelation;
begin
  result := nil;

  if Assigned(Owner) then
    result := TEpiMasterRelation(Owner.Owner);
end;

function TEpiDetailRelation.GetRelateField(Index: Integer): TEpiField;
begin

end;

function TEpiDetailRelation.GetRelateValue(Index: Integer): string;
begin

end;

function TEpiDetailRelation.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase
  ): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest);
end;

constructor TEpiDetailRelation.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  FFieldList := TEpiFields.Create(self);
  FFieldList.ItemOwner := false;
  FFieldList.Sorted := false;
  FFieldList.UniqueNames := false;

  FValueList := TStringList.Create;
  FValueList.Sorted := false;
  FValueList.Duplicates := dupAccept;
end;

destructor TEpiDetailRelation.Destroy;
begin
  FFieldList.Destroy;
  FValueList.Destroy;
  inherited Destroy;
end;

procedure TEpiDetailRelation.LoadFromXml(Root: TDOMNode);
var
  FieldId: EpiString;
begin
  inherited LoadFromXml(Root);

{  FRelateValue := LoadAttrString(Root, rsRelateValue);
  FieldId := LoadAttrString(Root, rsFieldRef);
  RelateField := MasterRelation.Datafile.Fields.FieldByName[FieldId];
  if not Assigned(FRelateField) then
    Raise Exception.Create('DetailRelation - Relate Field not found: ' + FieldId);   }
end;

function TEpiDetailRelation.SaveAttributesToXml: string;
begin
  Result := inherited SaveAttributesToXml;
{  Result +=
    SaveAttr(rsFieldRef, RelateField.Name) +
    SaveAttr(rsRelateValue, RelateValue); }
end;

procedure TEpiDetailRelation.AddFieldValuePair(const Field: TEpiField;
  const Value: EpiString);
begin
  FFieldList.AddItem(Field);
  FValueList.Add(Value);
end;

procedure TEpiDetailRelation.RemoveFieldValuePair(const Field: TEpiField;
  const Value: EpiString);
var
  Idx: Integer;
begin
  Idx := FFieldList.IndexOf(Field);

  for Idx := Idx to FValueList.Count - 1 do
  begin

  end;
end;

{ TEpiRelationList }

function TEpiRelationList.GetMasterRelation(Index: integer): TEpiMasterRelation;
begin
  result := TEpiMasterRelation(Items[Index]);
end;

constructor TEpiRelationList.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
end;

function TEpiRelationList.XMLName: string;
begin
  Result := rsRelations;
end;

function TEpiRelationList.NewMasterRelation: TEpiMasterRelation;
begin
  result := TEpiMasterRelation(NewItem());
end;

function TEpiRelationList.ItemClass: TEpiCustomItemClass;
begin
  Result := TEpiMasterRelation;
end;

end.

