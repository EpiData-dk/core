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
  public
    destructor  Destroy; override;
    function    XMLName: string; override;
    function    NewGroupRight: TEpiGroupRight;
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
  typinfo;

{ TEpiGroupRights }

function TEpiGroupRights.GetGroupRight(Index: integer): TEpiGroupRight;
begin
  result := TEpiGroupRight(Items[Index]);
end;

destructor TEpiGroupRights.Destroy;
begin
  inherited Destroy;
end;

function TEpiGroupRights.XMLName: string;
begin
  Result := 'GroupRights';
end;

function TEpiGroupRights.NewGroupRight: TEpiGroupRight;
begin
  result := TEpiGroupRight(NewItem(TEpiGroupRight));
end;

{ TEpiGroupRight }

procedure TEpiGroupRight.SetGroup(AValue: TEpiGroup);
begin
  if FGroup = AValue then Exit;
  FGroup := AValue;
end;

procedure TEpiGroupRight.FixupReferences(EpiClassType: TEpiCustomBaseClass;
  ReferenceType: Byte; const ReferenceId: string);
begin
  inherited FixupReferences(EpiClassType, ReferenceType, ReferenceId);

  if (EpiClassType <> ClassType) then exit;
  case ReferenceType of
    0:
      begin
//        FGroup := ;
      end;
  end;
end;

constructor TEpiGroupRight.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
end;

function TEpiGroupRight.XMLName: string;
begin
  Result := 'GroupRight';
end;

procedure TEpiGroupRight.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
begin
  inherited LoadFromXml(Root, ReferenceMap);

  ReferenceMap.AddFixupReference(Self, TEpiGroupRight, 0, LoadAttrString(Root, 'groupRef'));

  FEntryRights :=  TEpiEntryRights(LoadAttrEnum(Root, 'entryRights', TypeInfo(TEpiEntryRights)));
end;

function TEpiGroupRight.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);

  SaveDomAttr(Result, 'groupRef', Group.Name);
  SaveDomAttrEnum(Result, 'entryRights', EntryRights, TypeInfo(TEpiEntryRights));
end;

end.

