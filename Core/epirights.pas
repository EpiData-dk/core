unit epirights;

{$mode objfpc}{$H+}

interface

uses
  Classes, epicustombase, epiadmin, Laz2_DOM;

type

  TEpiEntryRight = (
    // Data access - do NOT change order!
    // It is the "normal" entry hierachy!
    eerRead = 0,
    eerUpdate,
    eerCreate,
    eerDelete
  );
  TEpiEntryRights = set of TEpiEntryRight;

const
  EpiEntryRightCaption: array[TEpiEntryRight] of string = (
    'Read',
    'Update',
    'Create',
    'Delete'
  );

  EpiEntryRightCaptionShort: array[TEpiEntryRight] of string = (
    'R',
    'U',
    'C',
    'D'
  );

  EpiEntryRightsAll = [eerRead..eerDelete];

type
  TEpiGroupRightEvent = (
    egreSetGroup,
    egreSetEntryRights
  );

  TEpiGroupRight = class;

  { TEpiGroupRights }

  TEpiGroupRights = class(TEpiCustomList)
  private
    procedure AdminGroupChangeEvent(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
    function GetGroupRight(Index: integer): TEpiGroupRight;
  protected
    procedure DoChange(const Initiator: TEpiCustomBase;
      EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer); override;
      overload;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor  Destroy; override;
    procedure   LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    function    XMLName: string; override;
    function    NewGroupRight: TEpiGroupRight;
    function    ItemClass: TEpiCustomItemClass; override;
    function    GroupRightFromGroup(Const Group: TEpiGroup): TEpiGroupRight;
    property    GroupRight[Index: integer]: TEpiGroupRight read GetGroupRight; default;
  end;

  TEpiGroupRight = class(TEpiCustomItem)
  private
    FEntryRights: TEpiEntryRights;
    FGroup: TEpiGroup;
    procedure SetGroup(AValue: TEpiGroup);
    procedure SetEntryRights(AValue: TEpiEntryRights);
    procedure GroupHook(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
  protected
    procedure FixupReferences(EpiClassType: TEpiCustomBaseClass;
                ReferenceType: Byte; const ReferenceId: string); override;
  protected
    procedure ReferenceDestroyed(Item: TEpiCustomItem; PropertyName: shortstring); override;
    function WriteNameToXml: boolean; override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    function   XMLName: string; override;
    procedure  LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    function   SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
  public
    function   PrintEntryRights: string;
  public
    { Porperties }
    property   Group: TEpiGroup read FGroup write SetGroup;
    property   EntryRights: TEpiEntryRights read FEntryRights write SetEntryRights;
  end;


implementation

uses
  typinfo, epidocument;

{ TEpiGroupRights }

procedure TEpiGroupRights.AdminGroupChangeEvent(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
var
  RO: TEpiCustomBase;
begin
  if (Sender <> Initiator) then exit;
  if (EventGroup <> eegCustomBase) then exit;
  if (TEpiCustomChangeEventType(EventType) <> ecceAddItem) then Exit;

  RO := RootOwner;
  if (RO is TEpiDocument) and (TEpiDocument(RO).Loading) then exit;

  NewGroupRight.Group := TEpiGroup(Data);
end;

function TEpiGroupRights.GetGroupRight(Index: integer): TEpiGroupRight;
begin
  result := TEpiGroupRight(Items[Index]);
end;

procedure TEpiGroupRights.DoChange(const Initiator: TEpiCustomBase;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  inherited DoChange(Initiator, EventGroup, EventType, Data);

  if (EventGroup <> eegCustomBase) then exit;
  if (EventType  <> Word(ecceReferenceDestroyed)) then exit;

  // Not my child!
  if IndexOf(TEpiCustomItem(Initiator)) < 0  then exit;

  RemoveItem(TEpiCustomItem(Initiator));
  Initiator.Free;
end;

constructor TEpiGroupRights.Create(AOwner: TEpiCustomBase);
var
  RO: TEpiCustomBase;
  G: TEpiGroup;
  GR: TEpiGroupRight;
begin
  inherited Create(AOwner);

  RO := RootOwner;
  if RO is TEpiDocument then
    with TEpiDocument(RO) do
      begin
        Admin.Groups.RegisterOnChangeHook(@AdminGroupChangeEvent, true);

        if (not Loading) then
          for G in Admin.Groups do
          begin
            GR := NewGroupRight;
            GR.Group := G;
            if G = Admin.Admins then
              GR.EntryRights := EpiEntryRightsAll;
          end;
      end;
end;

destructor TEpiGroupRights.Destroy;
begin
  inherited Destroy;
end;

procedure TEpiGroupRights.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
begin
  inherited LoadFromXml(Root, ReferenceMap);
end;

function TEpiGroupRights.XMLName: string;
begin
  Result := rsGroupRights;
end;

function TEpiGroupRights.NewGroupRight: TEpiGroupRight;
begin
  result := TEpiGroupRight(NewItem(TEpiGroupRight));
end;

function TEpiGroupRights.ItemClass: TEpiCustomItemClass;
begin
  Result := TEpiGroupRight;
end;

function TEpiGroupRights.GroupRightFromGroup(const Group: TEpiGroup
  ): TEpiGroupRight;
var
  Runner: TEpiCustomItem;
begin
  result := nil;

  for Runner in Self do
    if (TEpiGroupRight(Runner).Group = Group) then
      begin
        Result := TEpiGroupRight(Runner);
        Break;
      end;
end;

{ TEpiGroupRight }

procedure TEpiGroupRight.SetGroup(AValue: TEpiGroup);
var
  Val: TEpiGroup;
begin
  if FGroup = AValue then Exit;
  Val := FGroup;
  FGroup := AValue;
  ObserveReference(FGroup, 'Group');

  if Assigned(Val) then
    Val.UnRegisterOnChangeHook(@GroupHook);
  if Assigned(Group) then
    Group.RegisterOnChangeHook(@GroupHook, true);

  DoChange(eegRights, Word(egreSetGroup), Val);
end;

procedure TEpiGroupRight.SetEntryRights(AValue: TEpiEntryRights);
var
  Val: TEpiEntryRights;
begin
  if FEntryRights = AValue then Exit;
  Val := FEntryRights;
  FEntryRights := AValue;
  DoChange(eegRights, Word(egreSetEntryRights), @Val);
end;

procedure TEpiGroupRight.GroupHook(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
begin
  if (Initiator <> Group) then exit;
  if (EventGroup <> eegAdmin) then exit;
  if (TEpiAdminChangeEventType(EventType) <> eaceGroupSetManageRights) then exit;

  if not (earViewData in Group.ManageRights) then
    EntryRights := [];
end;

procedure TEpiGroupRight.FixupReferences(EpiClassType: TEpiCustomBaseClass;
  ReferenceType: Byte; const ReferenceId: string);
var
  Obj: TEpiCustomBase;
begin
  inherited FixupReferences(EpiClassType, ReferenceType, ReferenceId);

  if (EpiClassType <> ClassType) then exit;
  case ReferenceType of
    0:
      begin
        Obj := RootOwner;
        if not Obj.InheritsFrom(TEpiDocument) then
          Exit; //TODO: raise an exception

        Group := TEpiGroup(TEpiDocument(Obj).Admin.Groups.GetItemByName(ReferenceId));
      end;
  end;
end;

procedure TEpiGroupRight.ReferenceDestroyed(Item: TEpiCustomItem;
  PropertyName: shortstring);
begin
  if PropertyName = 'Group' then
    Group := nil;

  inherited ReferenceDestroyed(Item, PropertyName);
end;

function TEpiGroupRight.WriteNameToXml: boolean;
begin
  Result := false;
end;

constructor TEpiGroupRight.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
end;

function TEpiGroupRight.XMLName: string;
begin
  Result := rsGroupRight;
end;

procedure TEpiGroupRight.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
begin
  inherited LoadFromXml(Root, ReferenceMap);

  ReferenceMap.AddFixupReference(Self, TEpiGroupRight, 0, LoadAttrString(Root, rsGroupRef));

  FEntryRights :=  TEpiEntryRights(LoadAttrEnum(Root, rsEntryRights, TypeInfo(TEpiEntryRights)));
end;

function TEpiGroupRight.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);

  SaveDomAttr(Result, rsGroupRef, Group.Name);
  SaveDomAttrEnum(Result, rsEntryRights, EntryRights, TypeInfo(TEpiEntryRights));
end;

function TEpiGroupRight.PrintEntryRights: string;
var
  Item: TEpiEntryRight;
begin
  Result := '';

  for Item in EntryRights do
    Result += EpiEntryRightCaptionShort[Item] + ', ';

  Delete(Result, Length(Result) - 1, 2);
end;

end.

