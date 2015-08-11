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
    function DetailItemClass(Sender: TEpiCustomList;
      DefaultItemClass: TEpiCustomItemClass): TEpiCustomItemClass;
    function GetDetailRelation(Index: integer): TEpiDetailRelation;
    procedure UpdateDataFileHook(Const OldDf, NewDf: TEpiDataFile);
    procedure SetDatafile(AValue: TEpiDataFile);
  protected
    procedure FixupReferences(EpiClassType: TEpiCustomBaseClass;
       ReferenceType: Byte; const ReferenceId: string); override;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
      RefenceMap: TEpiReferenceMap): TEpiCustomBase; override;
  protected
    function SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
    procedure LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    function SaveAttributesToXml: string; override;
    function XMLName: string; override;
    function NewDetailRelation: TEpiDetailRelation;
    function IsChild(Relation: TEpiMasterRelation; Recurse: Boolean): boolean;
    property Datafile: TEpiDataFile read FDatafile write SetDatafile;
    property DetailRelation[Index: integer]: TEpiDetailRelation read GetDetailRelation; default;
    property DetailRelations: TEpiRelationList read FDetailRelations;
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
    function SaveAttributesToXml: string; override;
    procedure Assign(const AEpiCustomBase: TEpiCustomBase); override;
    property MasterRelation: TEpiMasterRelation read GetMasterRelation;
    // MaxRecordCount: Is the maximum number of records allowed in a detail datafile. Set to 0 for unbounded.
    property MaxRecordCount: Cardinal read FMaxRecordCount write SetMaxRecordCount;
  end;

  TEpiRelationListEnumerator = class;

  { TEpiRelationList }

  TEpiRelationList = class(TEpiCustomList)
  private
    function GetMasterRelation(Index: integer): TEpiMasterRelation;
    function RecursiveValidateRename(Const NewName: string): boolean;
    function RecursiveGetItemByName(Const AName: string): TEpiCustomItem;
  protected
    function Prefix: string; override;
    procedure DoChange(const Initiator: TEpiCustomBase;
      EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer); override;
      overload;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    function XMLName: string; override;
    function NewMasterRelation: TEpiMasterRelation;
    function ItemClass: TEpiCustomItemClass; override;
    function GetItemByName(Const AName: string): TEpiCustomItem; override;
    function ValidateRename(const NewName: string; RenameOnSuccess: boolean
       ): boolean; override;
    function GetEnumerator: TEpiRelationListEnumerator;
    property MasterRelation[Index: integer]: TEpiMasterRelation read GetMasterRelation; default;
  end;

  { TEpiRelationListEnumerator }

  TEpiRelationListEnumerator = class(TEpiCustomListEnumerator)
  protected
    function GetCurrent: TEpiMasterRelation; override;
  public
    property Current: TEpiMasterRelation read GetCurrent;
  end;

implementation

uses
  epidocument;

{ TEpiRelationListEnumerator }

function TEpiRelationListEnumerator.GetCurrent: TEpiMasterRelation;
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

function TEpiMasterRelation.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);

  SaveDomAttr(Result, rsDataFileRef, Datafile.Name);
end;

constructor TEpiMasterRelation.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  Datafile := nil;

  FDetailRelations := TEpiRelationList.Create(Self);
  FDetailRelations.ItemOwner := true;
  FDetailRelations.OnNewItemClass := @DetailItemClass;

  RegisterClasses([FDetailRelations]);
end;

destructor TEpiMasterRelation.Destroy;
begin
  Datafile := nil;
  FDetailRelations.Free;
  inherited Destroy;
end;

procedure TEpiMasterRelation.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
var
  DfId: EpiString;
  Node: TDOMNode;
begin
  inherited LoadFromXml(Root, ReferenceMap);

  ReferenceMap.AddFixupReference(Self, TEpiMasterRelation, 0, LoadAttrString(Root, rsDataFileRef));

  if LoadNode(Node, Root, rsRelations, false) then
    FDetailRelations.LoadFromXml(Node, ReferenceMap);
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

function TEpiMasterRelation.IsChild(Relation: TEpiMasterRelation;
  Recurse: Boolean): boolean;
var
  LRelation: TEpiMasterRelation;
  Val: Boolean;
begin
  // In FPC 2.6.4, this method does not return a correct result
  // when compiled with -O2 optimization on. Test if this is
  // still the case when version 3.0 is out.
  result := false;

  for LRelation in DetailRelations do
  begin
    Result := (LRelation = Relation);

    if Recurse then
      Result :=
        Result or
        (LRelation.IsChild(Relation, Recurse));

    if Result then Break;
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
  MaxRecordCount := 0;
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

function TEpiDetailRelation.SaveAttributesToXml: string;
begin
  Result := inherited SaveAttributesToXml +
    SaveAttr(rsMaxRecordCount, MaxRecordCount);
end;

procedure TEpiDetailRelation.Assign(const AEpiCustomBase: TEpiCustomBase);
begin
  inherited Assign(AEpiCustomBase);

  TEpiDetailRelation(AEpiCustomBase).MaxRecordCount := MaxRecordCount;
end;

{ TEpiRelationList }

function TEpiRelationList.GetMasterRelation(Index: integer): TEpiMasterRelation;
begin
  result := TEpiMasterRelation(Items[Index]);
end;

function TEpiRelationList.RecursiveValidateRename(const NewName: string
  ): boolean;
var
  i: Integer;
begin
  Result := true;

  for i := 0 to Count - 1 do
  begin
    Result := Result and
      (MasterRelation[i].Name <> NewName) and
      (MasterRelation[i].DetailRelations.RecursiveValidateRename(NewName));
  end;
end;

function TEpiRelationList.RecursiveGetItemByName(const AName: string
  ): TEpiCustomItem;
var
  i: Integer;
begin
  result := nil;

  for i := 0 to Count - 1 do
  begin
    if MasterRelation[i].Name = AName then
      Result := Items[i]
    else
      Result := MasterRelation[i].DetailRelations.RecursiveGetItemByName(AName);

    if Assigned(Result) then exit;
  end;
end;

function TEpiRelationList.Prefix: string;
begin
  Result := 'relation_id_';
end;

procedure TEpiRelationList.DoChange(const Initiator: TEpiCustomBase;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
var
  Item: TEpiCustomItem;
begin
  inherited DoChange(Initiator, EventGroup, EventType, Data);

  if not Initiator.InheritsFrom(TEpiCustomItem) then exit;
  Item := TEpiCustomItem(Initiator);

  // Not my child!
  if (IndexOf(Item) < 0) then exit;
  if (EventGroup <> eegCustomBase) then exit;
  if (EventType  <> Word(ecceReferenceDestroyed)) then exit;

  RemoveItem(Item);
  Item.Free;
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

function TEpiRelationList.GetItemByName(const AName: string): TEpiCustomItem;
begin
  // Override this to make an easy traversal of the relationship tree.
  result := TEpiDocument(RootOwner).Relations.RecursiveGetItemByName(AName);
end;

function TEpiRelationList.ValidateRename(const NewName: string;
  RenameOnSuccess: boolean): boolean;
begin
  // Override this to make an easy traversal of the relationship tree.
  result := TEpiDocument(RootOwner).Relations.RecursiveValidateRename(NewName);
end;

function TEpiRelationList.GetEnumerator: TEpiRelationListEnumerator;
begin
  result := TEpiRelationListEnumerator.Create(Self);
end;

end.

