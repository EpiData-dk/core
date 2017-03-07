unit epidatafilerelations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz2_DOM, epicustombase, epidatafiles, epidatafilestypes,
  epicustomrelations;

type
  TEpiDatafileRelationList = class;
  TEpiMasterRelation = class;
  TEpiDetailRelation = class;


  { TEpiMasterRelation }

  TEpiMasterRelation = class(TEpiCustomRelationItem)
  private
    FDatafile: TEpiDataFile;
    procedure DataFileHook(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
    function DetailItemClass(Sender: TEpiCustomList;
      DefaultItemClass: TEpiCustomItemClass): TEpiCustomItemClass;
    function GetDetailRelation(Index: integer): TEpiDetailRelation;
    function GetDetailRelations: TEpiDatafileRelationList;
    procedure UpdateDataFileHook(Const OldDf, NewDf: TEpiDataFile);
    procedure SetDatafile(AValue: TEpiDataFile);
  protected
    procedure FixupReferences(EpiClassType: TEpiCustomBaseClass;
       ReferenceType: Byte; const ReferenceId: string); override;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
      RefenceMap: TEpiReferenceMap): TEpiCustomBase; override;
  protected
    class function GetRelationListClass: TEpiCustomRelationListClass; override;
    function SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
    procedure LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    function XMLName: string; override;
    function NewDetailRelation: TEpiDetailRelation;
    function IsChild(Relation: TEpiMasterRelation; Recurse: Boolean): boolean;
    property Datafile: TEpiDataFile read FDatafile write SetDatafile;
    property DetailRelation[Index: integer]: TEpiDetailRelation read GetDetailRelation; default;
    property DetailRelations: TEpiDatafileRelationList read GetDetailRelations;
  end;

  { TEpiDetailRelation }

  TEpiDetailRelation = class(TEpiMasterRelation)
  private
    FMaxRecordCount: Cardinal;
    function  GetMasterRelation: TEpiMasterRelation;
    procedure SetMaxRecordCount(AValue: Cardinal);
  protected
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
      RefenceMap: TEpiReferenceMap): TEpiCustomBase; override;
  protected
    function SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
    procedure LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    procedure Assign(const AEpiCustomBase: TEpiCustomBase); override;
    property MasterRelation: TEpiMasterRelation read GetMasterRelation;
    // MaxRecordCount: Is the maximum number of records allowed in a detail datafile. Set to 0 for unbounded.
    property MaxRecordCount: Cardinal read FMaxRecordCount write SetMaxRecordCount;
  end;

  TEpiDatafileRelationListEnumerator = class;

  { TEpiDatafileRelationList }

  TEpiDatafileRelationList = class(TEpiCustomRelationItemList)
  private
    function GetMasterRelation(Index: integer): TEpiMasterRelation;
  protected
    function Prefix: string; override;
    function NewItemLoad(const AName: EpiString;
      AItemClass: TEpiCustomItemClass = nil): TEpiCustomItem; override;
    procedure AddItem(Item: TEpiCustomItem); override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    function XMLName: string; override;
    function NewMasterRelation: TEpiMasterRelation;
    function ItemClass: TEpiCustomItemClass; override;
    function GetEnumerator: TEpiDatafileRelationListEnumerator;
    property MasterRelation[Index: integer]: TEpiMasterRelation read GetMasterRelation; default;
  end;

  { TEpiDatafileRelationListEnumerator }

  TEpiDatafileRelationListEnumerator = class(TEpiCustomListEnumerator)
  protected
    function GetCurrent: TEpiMasterRelation; override;
  public
    property Current: TEpiMasterRelation read GetCurrent;
  end;


implementation

uses
  epidocument, episecuritylog, epiglobals;

{ TEpiDatafileRelationListEnumerator }

function TEpiDatafileRelationListEnumerator.GetCurrent: TEpiMasterRelation;
begin
  Result := TEpiMasterRelation(inherited GetCurrent);
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

function TEpiMasterRelation.DetailItemClass(Sender: TEpiCustomList;
  DefaultItemClass: TEpiCustomItemClass): TEpiCustomItemClass;
begin
  result := TEpiDetailRelation;
end;

function TEpiMasterRelation.GetDetailRelation(Index: integer
  ): TEpiDetailRelation;
begin
  result := TEpiDetailRelation(DetailRelations.Items[Index]);
end;

function TEpiMasterRelation.GetDetailRelations: TEpiDatafileRelationList;
begin
  result := TEpiDatafileRelationList(RelationList);
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
  DoSendAssignObjectChangeEvent('DataFile', Datafile);
end;

procedure TEpiMasterRelation.FixupReferences(EpiClassType: TEpiCustomBaseClass;
  ReferenceType: Byte; const ReferenceId: string);
begin
  if (EpiClassType = TEpiMasterRelation)
  then
    begin
      case ReferenceType of
        0: // Datafile
          DataFile := TEpiDataFile(TEpiDocument(RootOwner).DataFiles.GetItemByName(ReferenceId));
      end;
    end
  else
    inherited FixupReferences(EpiClassType, ReferenceType, ReferenceId);
end;

function TEpiMasterRelation.DoClone(AOwner: TEpiCustomBase;
  Dest: TEpiCustomBase; RefenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest, RefenceMap);

  if Assigned(Datafile) then
    RefenceMap.AddFixupReference(Result, TEpiMasterRelation, 0, Datafile.Name);
end;

class function TEpiMasterRelation.GetRelationListClass: TEpiCustomRelationListClass;
begin
  result := TEpiDatafileRelationList;
end;

function TEpiMasterRelation.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);

  SaveDomAttr(Result, rsDataFileRef, Datafile.Name);
end;

constructor TEpiMasterRelation.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  Datafile := nil;

  RelationList.OnNewItemClass := @DetailItemClass;
end;

destructor TEpiMasterRelation.Destroy;
begin
  Datafile := nil;
  inherited Destroy;
end;

procedure TEpiMasterRelation.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
var
  Node: TDOMNode;
  S: String;
  RO: TEpiCustomBase;
begin
  inherited LoadFromXml(Root, ReferenceMap);

  ReferenceMap.AddFixupReference(Self, TEpiMasterRelation, 0, LoadAttrString(Root, rsDataFileRef));

  // XML tag changed from <Relation(s)> to <DataFileRelation(s)> in version 4.
  RO := RootOwner;
  if (RO is TEpiDocument) and
     (TEpiDocument(RO).Loading) and
     (TEpiDocument(RO).Version = 3)
  then
    S := rsRelations
  else
    S := rsDataFileRelations;

  if LoadNode(Node, Root, S, false) then
    DetailRelations.LoadFromXml(Node, ReferenceMap);
end;

function TEpiMasterRelation.XMLName: string;
var
  RO: TEpiCustomBase;
begin
  // XML tag changed from <Relation(s)> to <DataFileRelation(s)> in version 4.
  RO := RootOwner;
  if (RO is TEpiDocument) and
     (TEpiDocument(RO).Loading) and
     (TEpiDocument(RO).Version = 3)
  then
    result := rsRelation
  else
    Result := rsDataFileRelation;
end;

function TEpiMasterRelation.NewDetailRelation: TEpiDetailRelation;
begin
  result := TEpiDetailRelation(DetailRelations.NewItem(TEpiDetailRelation));
end;

function TEpiMasterRelation.IsChild(Relation: TEpiMasterRelation;
  Recurse: Boolean): boolean;
var
  LRelation: TEpiMasterRelation;
begin
  result := false;

  for LRelation in DetailRelations do
  begin
    result := (LRelation = Relation);

    if Recurse then
      Result :=
        Result or
        (LRelation.IsChild(Relation, Recurse));

    if Result then exit;
  end;
end;

{ TEpiDetailRelation }

function TEpiDetailRelation.GetMasterRelation: TEpiMasterRelation;
begin
  result := nil;

  if Assigned(Owner) then
    result := TEpiMasterRelation(Owner.Owner);
end;

procedure TEpiDetailRelation.SetMaxRecordCount(AValue: Cardinal);
begin
  if FMaxRecordCount = AValue then Exit;
  FMaxRecordCount := AValue;
end;

function TEpiDetailRelation.DoClone(AOwner: TEpiCustomBase;
  Dest: TEpiCustomBase; RefenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest, RefenceMap);

  TEpiDetailRelation(Result).MaxRecordCount := MaxRecordCount;
end;

function TEpiDetailRelation.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);

  SaveDomAttr(Result, rsMaxRecordCount, MaxRecordCount);
end;

constructor TEpiDetailRelation.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  MaxRecordCount := 1;
end;

destructor TEpiDetailRelation.Destroy;
begin
  inherited Destroy;
end;

procedure TEpiDetailRelation.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
var
  FieldId: EpiString;
begin
  inherited LoadFromXml(Root, ReferenceMap);

  MaxRecordCount := LoadAttrInt(Root, rsMaxRecordCount, 0, false);
end;

procedure TEpiDetailRelation.Assign(const AEpiCustomBase: TEpiCustomBase);
begin
  inherited Assign(AEpiCustomBase);

  TEpiDetailRelation(AEpiCustomBase).MaxRecordCount := MaxRecordCount;
end;

{ TEpiDatafileRelationList }

function TEpiDatafileRelationList.GetMasterRelation(Index: integer): TEpiMasterRelation;
begin
  result := TEpiMasterRelation(Items[Index]);
end;

function TEpiDatafileRelationList.Prefix: string;
begin
  Result := 'relation_id_';
end;

function TEpiDatafileRelationList.NewItemLoad(const AName: EpiString;
  AItemClass: TEpiCustomItemClass): TEpiCustomItem;
begin
  case AName of
    EpiSecurityLogRelationName:
      result := GetItemByName(EpiSecurityLogRelationName);

    EpiSecurityLogDataRelationName:
      result := GetItemByName(EpiSecurityLogDataRelationName);

    EpiSecurityLogKeyDataRelationName:
      result := GetItemByName(EpiSecurityLogKeyDataRelationName);

  else
    result := inherited NewItemLoad(AName, AItemClass);
  end;
end;

procedure TEpiDatafileRelationList.AddItem(Item: TEpiCustomItem);
var
  Idx: Integer;
begin
  Idx := Count - 1;

  if (Idx >= 0) and
     (not Item.ProtectedItem)
  then
    begin
      // Override AddItem here - such that if using the Extended Access, the protected
      // MasterRelation is always sorted last.
      while (Idx >= 0) and
            (Items[Idx].ProtectedItem)
      do
        Dec(Idx);
    end;

  InsertItem(Idx + 1, Item);
end;

constructor TEpiDatafileRelationList.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
end;

function TEpiDatafileRelationList.XMLName: string;
begin
  Result := rsDataFileRelations;
end;

function TEpiDatafileRelationList.NewMasterRelation: TEpiMasterRelation;
begin
  result := TEpiMasterRelation(NewItem());
end;

function TEpiDatafileRelationList.ItemClass: TEpiCustomItemClass;
begin
  Result := TEpiMasterRelation;
end;

function TEpiDatafileRelationList.GetEnumerator: TEpiDatafileRelationListEnumerator;
begin
  result := TEpiDatafileRelationListEnumerator.Create(Self);
end;

end.

