unit epigrouprelation_helper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epiadmin, epicustombase, epicustomrelations, epicustomrelations_helper;

type

  TEpiGroupRelationListCallBack = procedure(
    Const Relation: TEpiGroupRelation;
    Const Depth: Cardinal;
    Const Index: Cardinal;
    var aContinue: boolean;
    Data: Pointer = nil
  ) of object;

  { TEpiGroupRelationItemListHelper }

  TEpiGroupRelationItemListHelper = class helper(TEpiCustomRelationItemListHelper) for TEpiGroupRelationList
  private
    procedure InternalCallBack(
      Const Relation: TEpiCustomRelationItem;
      Const Depth: Cardinal;
      Const Index: Cardinal;
      var aContinue: boolean;
      Data: Pointer = nil
    );
  public
    function GetOrderedItems: TEpiGroupRelationList;
    procedure OrderedWalk(Const CallBackMethod: TEpiGroupRelationListCallBack;
      Data: Pointer = nil);
  end;

implementation

type
  InternalData = record
    ExternalMethod: TEpiGroupRelationListCallBack;
    ExternalData:   Pointer;
  end;
  PInternalData = ^InternalData;

{ TEpiGroupRelationItemListHelper }

procedure TEpiGroupRelationItemListHelper.InternalCallBack(
  const Relation: TEpiCustomRelationItem; const Depth: Cardinal;
  const Index: Cardinal; var aContinue: boolean; Data: Pointer);
begin
  with PInternalData(Data)^ do
    ExternalMethod(TEpiGroupRelation(Relation), Depth, Index, aContinue, ExternalData);
end;

function TEpiGroupRelationItemListHelper.GetOrderedItems: TEpiGroupRelationList;
begin
  result := TEpiGroupRelationList(inherited GetOrderedItems(TEpiGroupRelationList));
end;

procedure TEpiGroupRelationItemListHelper.OrderedWalk(
  const CallBackMethod: TEpiGroupRelationListCallBack; Data: Pointer);
var
  NewData: PInternalData;
begin
  NewData := new(PInternalData);
  with NewData^ do
  begin
    ExternalMethod := CallBackMethod;
    ExternalData   := Data;
  end;

  inherited OrderedWalk(@InternalCallBack, NewData);

  Dispose(NewData);
end;

end.

