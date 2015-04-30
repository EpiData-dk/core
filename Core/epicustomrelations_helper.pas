unit epicustomrelations_helper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epicustombase, epicustomrelations;

type

  TEpiCustomRelationItemListCallBack = procedure(
    Const Relation: TEpiCustomRelationItem;
    Const Depth: Cardinal;
    Const Index: Cardinal;
    var aContinue: boolean;
    Data: Pointer = nil
  ) of object;

  { TEpiCustomRelationItemListHelper }

  TEpiCustomRelationItemListHelper = class helper for TEpiCustomRelationItemList
    function CreateOrderedItemsList: TEpiCustomList;
    function GetOrderedItems: TEpiCustomList;
    procedure OrderedWalk(Const CallBackMethod: TEpiCustomRelationItemListCallBack;
      Data: Pointer = nil);
    function IsMultiLeveled: boolean;     // Returns true if any top-level Master relation have a Detail relation.
  end;


implementation

uses
  epimiscutils;

{ TEpiCustomRelationItemListHelper }

function TEpiCustomRelationItemListHelper.CreateOrderedItemsList: TEpiCustomList;
begin
  result := nil;
  raise Exception.Create('TEpiCustomRelationItemListHelper.CreateOrderedItemsList not overriden!');
end;

function TEpiCustomRelationItemListHelper.GetOrderedItems: TEpiCustomList;

  procedure BuildOrderedItems(Const Item: TEpiCustomRelationItem);
  var
    NewItem: TEpiCustomRelationItem;
  begin
    result.AddItem(Item);

    for NewItem in Item.RelationList do
      BuildOrderedItems(NewItem);
  end;

var
  Item: TEpiCustomRelationItem;
begin
  result := CreateOrderedItemsList;
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

