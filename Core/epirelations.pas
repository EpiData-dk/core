unit epirelations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz2_DOM, epicustombase, epidatafiles, epidatafilestypes;

type
  TEpiCustomRelationList = class;
  TEpiMasterRelation = class;
  TEpiDetailRelation = class;

  { TEpiCustomRelationList }

  TEpiCustomRelationList = class(TEpiCustomList)
  protected
    procedure DoSort; override;
  public
    function XMLName: string; override;
  end;

  { TEpiMasterRelation }

  TEpiMasterRelation = class(TEpiCustomRelationList)
  private
    FDatafile: TEpiDataFile;
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
    procedure InsertItem(const Index: integer; Item: TEpiCustomItem); override;
    function NewDetailRelation: TEpiDetailRelation;
    function ItemClass: TEpiCustomItemClass; override;
    property Datafile: TEpiDataFile read FDatafile write SetDatafile;
    property DetailRelation[Index: integer]: TEpiDetailRelation read GetDetailRelation; default;
  end;

  { TEpiDetailRelation }

  TEpiDetailRelation = class(TEpiMasterRelation)
  private
    FRelateField: TEpiField;
    FRelateValue: string;
    procedure FieldHook(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
    procedure UpdateFieldHook(Const OldField, NewField: TEpiField);
    function GetMasterRelation: TEpiMasterRelation;
    procedure SetRelateField(AValue: TEpiField);
    procedure SetRelateValue(AValue: string);
  protected
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase =
       nil): TEpiCustomBase; override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
    procedure LoadFromXml(Root: TDOMNode); override;
    function SaveAttributesToXml: string; override;
    property RelateField: TEpiField read FRelateField write SetRelateField;
    property RelateValue: string read FRelateValue write SetRelateValue;
    property MasterRelation: TEpiMasterRelation read GetMasterRelation;
  end;

  { TEpiRelationList }

  TEpiRelationList = class(TEpiCustomRelationList)
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

{ TEpiCustomRelationList }

procedure TEpiCustomRelationList.DoSort;
begin
  // Relations cannot be sorted!
end;

function TEpiCustomRelationList.XMLName: string;
begin
  Result := rsRelation;
end;

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
  result := TEpiDetailRelation(Items[Index]);
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
  Cloned := false;

  if Assigned(Dest) then
    begin
      // Always assume full clone!
      with TEpiMasterRelation(Dest) do
        DataFile := TEpiDataFile(TEpiDocument(RootOwner).DataFiles.GetItemByName(Self.FDatafile.Name));
      Cloned := true;
    end;

  Result := inherited DoClone(AOwner, Dest);

  // Always assume full clone!
  if not Cloned then
    with TEpiMasterRelation(Result) do
      DataFile := TEpiDataFile(TEpiDocument(RootOwner).DataFiles.GetItemByName(Self.FDatafile.Name));
end;

constructor TEpiMasterRelation.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  ItemOwner := true;
  Datafile := nil;
end;

destructor TEpiMasterRelation.Destroy;
begin
  Datafile := nil;
  inherited Destroy;
end;

procedure TEpiMasterRelation.LoadFromXml(Root: TDOMNode);
var
  DfId: EpiString;
begin
  DfId := LoadAttrString(Root, rsDataFileRef);
  Datafile := TEpiDataFile(TEpiDocument(RootOwner).DataFiles.GetItemByName(DfId));
  if not Assigned(FDatafile) then
    Raise Exception.Create('MasterRelation - DatafileId not found: ' + DfId);

  inherited LoadFromXml(Root);
end;

function TEpiMasterRelation.SaveAttributesToXml: string;
begin
  Result := inherited SaveAttributesToXml;

  Result +=
    SaveAttr(rsDataFileRef, Datafile.Name);
end;

procedure TEpiMasterRelation.InsertItem(const Index: integer;
  Item: TEpiCustomItem);
begin
  if not Item.InheritsFrom(TEpiDetailRelation) then
    Raise Exception.Create('A TEpiMasterRelation can only hold TEpiDetailRelation as Items!');

  inherited InsertItem(Index, Item);
end;

function TEpiMasterRelation.NewDetailRelation: TEpiDetailRelation;
begin
  result := TEpiDetailRelation(NewItem(TEpiDetailRelation));
end;

function TEpiMasterRelation.ItemClass: TEpiCustomItemClass;
begin
  Result := TEpiDetailRelation;
end;

{ TEpiDetailRelation }

procedure TEpiDetailRelation.SetRelateField(AValue: TEpiField);
begin
  if FRelateField = AValue then Exit;
  UpdateFieldHook(FRelateField, AValue);
  FRelateField := AValue;
end;

procedure TEpiDetailRelation.FieldHook(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
begin
  if Initiator <> FRelateField then exit;

  if NOT
      (
       (EventGroup = eegCustomBase) and
       (EventType  = Word(ecceDestroy))
      )
  then
    Exit;

  Data := FRelateField;
  RelateField := nil;
  DoChange(eegCustomBase, Word(ecceReferenceDestroyed), Data);
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
  result := TEpiMasterRelation(Owner);
end;

procedure TEpiDetailRelation.SetRelateValue(AValue: string);
begin
  if FRelateValue = AValue then Exit;
  FRelateValue := AValue;
end;

function TEpiDetailRelation.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase
  ): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest);

  // Always assume full clone!
  with TEpiDetailRelation(Result) do
  begin
    RelateField := MasterRelation.Datafile.Fields.FieldByName[Self.FRelateField.Name];
    RelateValue := Self.FRelateValue;
  end;
end;

constructor TEpiDetailRelation.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  RelateField := nil;
  RelateValue := '';
end;

destructor TEpiDetailRelation.Destroy;
begin
  RelateField := nil;
  RelateValue := '';

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

