unit epicustomrelations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epicustombase;

type
  TEpiCustomRelationItemList = class;
  TEpiCustomRelationListClass = class of TEpiCustomRelationItemList;

  { TEpiCustomRelationItem }

  TEpiCustomRelationItem = class(TEpiCustomItem)
  private
    FRelationList: TEpiCustomRelationItemList;
  protected
    class function GetRelationListClass: TEpiCustomRelationListClass; virtual; abstract;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
    property RelationList: TEpiCustomRelationItemList read FRelationList;
  end;

  TEpiCustomRelationItemListEnumerator = class;

  { TEpiCustomRelationItemList }

  TEpiCustomRelationItemList = class(TEpiCustomList)
  private
    function RecursiveValidateRename(Const NewName: string): boolean;
    function RecursiveGetItemByName(Const AName: string): TEpiCustomItem;
    function GetRootList: TEpiCustomRelationItemList;
  protected
    procedure DoChange(const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
  EventType: Word; Data: Pointer); override; overload;
  public
    function GetItemByName(Const AName: string): TEpiCustomItem; override;
    function ValidateRename(const NewName: string; RenameOnSuccess: boolean
       ): boolean; override;
    function GetEnumerator: TEpiCustomRelationItemListEnumerator;
  end;

  { TEpiCustomRelationItemListEnumerator }

  TEpiCustomRelationItemListEnumerator = class(TEpiCustomListEnumerator)
  protected
    function GetCurrent: TEpiCustomRelationItem; override;
  public
    property Current: TEpiCustomRelationItem read GetCurrent;
  end;

implementation

{ TEpiCustomRelationItemListEnumerator }

function TEpiCustomRelationItemListEnumerator.GetCurrent: TEpiCustomRelationItem;
begin
  Result := TEpiCustomRelationItem(inherited GetCurrent);
end;

{ TEpiCustomRelationItem }

constructor TEpiCustomRelationItem.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);

  FRelationList := GetRelationListClass.Create(Self);
  FRelationList.ItemOwner := true;

  RegisterClasses([FRelationList]);
end;

destructor TEpiCustomRelationItem.Destroy;
begin
  FRelationList.Free;
  inherited Destroy;
end;

{ TEpiCustomRelationItemList }

function TEpiCustomRelationItemList.RecursiveValidateRename(const NewName: string
  ): boolean;
var
  Item: TEpiCustomRelationItem;
begin
  Result := true;

  for Item in Self do
  begin
    Result := Result and
      (Item.Name <> NewName) and
      (Item.RelationList.RecursiveValidateRename(NewName));

    if (not Result) then
      Break;
  end;
end;

function TEpiCustomRelationItemList.RecursiveGetItemByName(const AName: string
  ): TEpiCustomItem;
var
  Item: TEpiCustomRelationItem;
begin
  result := nil;

  for Item in Self do
  begin
    if Item.Name = AName then
      Result := Item
    else
      Result := Item.RelationList.RecursiveGetItemByName(AName);

    if Assigned(Result) then exit;
  end;
end;

function TEpiCustomRelationItemList.GetRootList: TEpiCustomRelationItemList;
var
  Runner: TEpiCustomBase;
begin
  Result := nil;

  Runner := Self;
  While Assigned(Runner) and (Runner.Owner is TEpiCustomRelationItemList) do
    Runner := Runner.Owner;

  Result := TEpiCustomRelationItemList(Runner);
end;

procedure TEpiCustomRelationItemList.DoChange(const Initiator: TEpiCustomBase;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
var
  Item: TEpiCustomRelationItem;
begin
  inherited DoChange(Initiator, EventGroup, EventType, Data);

  if not Initiator.InheritsFrom(TEpiCustomRelationItem) then exit;
  Item := TEpiCustomRelationItem(Initiator);

  // Not my child!
  if (IndexOf(Item) < 0) then exit;
  if (EventGroup <> eegCustomBase) then exit;
  if (EventType  <> Word(ecceReferenceDestroyed)) then exit;

  RemoveItem(Item);
  Item.Free;
end;

function TEpiCustomRelationItemList.GetItemByName(const AName: string
  ): TEpiCustomItem;
begin
  // Override this to make an easy traversal of the relationship tree.
  Result := GetRootList.RecursiveGetItemByName(AName);
end;

function TEpiCustomRelationItemList.ValidateRename(const NewName: string;
  RenameOnSuccess: boolean): boolean;
begin
  // Override this to make an easy traversal of the relationship tree.
  Result := GetRootList.RecursiveValidateRename(NewName);
end;

function TEpiCustomRelationItemList.GetEnumerator: TEpiCustomRelationItemListEnumerator;
begin
  result := TEpiCustomRelationItemListEnumerator.Create(Self);
end;

end.

