unit epicustomrelations_helper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epicustombase, epicustomrelations;

type
  TEpiCustomRelationGetOrderedItem = function(
    InputItem: TEpiCustomRelationItem
  ): TEpiCustomItem of object;

  TEpiCustomRelationItemListCallBack = procedure(
    Const Relation: TEpiCustomRelationItem;
    Const Depth: Cardinal;
    Const Index: Cardinal;
    var aContinue: boolean;
    Data: Pointer = nil
  ) of object;

  { TEpiCustomRelationItemListHelper }

  TEpiCustomRelationItemListHelper = class helper for TEpiCustomRelationItemList
  private
    function InternalGetOrderedItem(InputItem: TEpiCustomRelationItem): TEpiCustomItem;
  public
    function GetOrderedItems(ListClass: TEpiCustomListClass;
      ItemMethod: TEpiCustomRelationGetOrderedItem = nil): TEpiCustomList;
    procedure OrderedWalk(Const CallBackMethod: TEpiCustomRelationItemListCallBack;
      Data: Pointer = nil);
    function IsMultiLeveled: boolean;     // Returns true if any top-level Master relation have a Detail relation.
  end;


implementation

uses
  epimiscutils;

{ TEpiCustomRelationItemListHelper }

function TEpiCustomRelationItemListHelper.InternalGetOrderedItem(
  InputItem: TEpiCustomRelationItem): TEpiCustomItem;
begin
  result := InputItem;
end;

function TEpiCustomRelationItemListHelper.GetOrderedItems(
  ListClass: TEpiCustomListClass; ItemMethod: TEpiCustomRelationGetOrderedItem
  ): TEpiCustomList;

  procedure BuildOrderedItems(Const Item: TEpiCustomRelationItem);
  var
    NewItem: TEpiCustomRelationItem;
  begin
    result.AddItem(ItemMethod(Item));

    for NewItem in Item.RelationList do
      BuildOrderedItems(NewItem);
  end;

var
  Item: TEpiCustomRelationItem;
begin
  if not Assigned(ItemMethod) then
    ItemMethod := @InternalGetOrderedItem;

  result := ListClass.Create(nil);
  result.ItemOwner := false;
  result.Sorted := false;

  for Item in Self do
    BuildOrderedItems(Item);
end;

procedure TEpiCustomRelationItemListHelper.OrderedWalk(
  const CallBackMethod: TEpiCustomRelationItemListCallBack; Data: Pointer);
var
  Depth: Cardinal;
  aContinue: Boolean;
  Item: TEpiCustomRelationItem;
  Idx: Integer;

  procedure RecurseRelations(ARelation: TEpiCustomRelationItem; Idx: Integer);
  var
    Item: TEpiCustomRelationItem;
  begin
    CallBackMethod(ARelation, Depth, Idx, aContinue, Data);
    if not aContinue then exit;

    Inc(Depth);
    Idx  := 0;
    for Item in ARelation.RelationList do
    begin
      RecurseRelations(Item, PostInc(Idx));
      if not aContinue then exit;
    end;
    Dec(Depth);
  end;

begin
  Depth := 0;
  aContinue := true;

  Idx := 0;
  for Item in Self do
    RecurseRelations(Item, PostInc(Idx));
end;

function TEpiCustomRelationItemListHelper.IsMultiLeveled: boolean;
var
  Item: TEpiCustomRelationItem;
begin
  for Item in Self do
    if Item.RelationList.Count > 0 then
      Exit(true);

  Result := false;
end;

end.

