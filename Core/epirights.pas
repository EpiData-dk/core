unit epirights;

{$mode objfpc}{$H+}

interface

uses
  Classes, epicustombase, epiadmin, Laz2_DOM;

type

  TEpiEntryRight = (
    // Data access
    eerCreate, eerRead, eerUpdate, eerDelete
  );
  TEpiEntryRights = set of TEpiEntryRight;

  TEpiGroupRight = class;

  { TEpiGroupRights }

  TEpiGroupRights = class(TEpiCustomList)
  private
    function GetGroupRight(Index: integer): TEpiGroupRight;
  protected
    procedure DoChange(const Initiator: TEpiCustomBase;
      EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer); override;
      overload;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor  Destroy; override;
    function    XMLName: string; override;
    function    NewGroupRight: TEpiGroupRight;
    function    ItemClass: TEpiCustomItemClass; override;
    Property    GroupRight[Index: integer]: TEpiGroupRight read GetGroupRight; default;
  end;

  TEpiGroupRight = class(TEpiCustomItem)
  private
    FEntryRights: TEpiEntryRights;
    FGroup: TEpiGroup;
    procedure SetGroup(AValue: TEpiGroup);
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
    { Porperties }
    property   Group: TEpiGroup read FGroup write SetGroup;
    property   EntryRights: TEpiEntryRights read FEntryRights write FEntryRights;
  end;


implementation

uses
  typinfo, epidocument;

{ TEpiGroupRights }

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
begin
  inherited Create(AOwner);
end;

destructor TEpiGroupRights.Destroy;
begin
  inherited Destroy;
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

{ TEpiGroupRight }

procedure TEpiGroupRight.SetGroup(AValue: TEpiGroup);
begin
  if FGroup = AValue then Exit;
  FGroup := AValue;

  ObserveReference(FGroup, 'Group');
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

//        Group := TEpiGroup(TEpiDocument(Obj).Admin.Groups.GetItemByName(ReferenceId));
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

end.

