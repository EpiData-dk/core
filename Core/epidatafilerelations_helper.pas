unit epidatafilerelations_helper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epiadmin, epicustombase, epicustomrelations, epicustomrelations_helper,
  epidatafilerelations, epidatafiles;

type

  TEpiDatafileRelationListCallBack = procedure(
    Const Relation: TEpiMasterRelation;
    Const Depth: Cardinal;
    Const Index: Cardinal;
    var aContinue: boolean;
    Data: Pointer = nil
  ) of object;

  { TEpiDataFileRelationItemListHelper }

  TEpiDataFileRelationItemListHelper = class helper(TEpiCustomRelationItemListHelper) for TEpiDatafileRelationList
  private
    procedure InternalCallBack(
      Const Relation: TEpiCustomRelationItem;
      Const Depth: Cardinal;
      Const Index: Cardinal;
      var aContinue: boolean;
      Data: Pointer = nil
    );
    function DataFileItemMethod(InputItem: TEpiCustomRelationItem): TEpiCustomItem;
  public
    function GetOrderedDataFiles: TEpiDataFiles;
    function GetOrderedMasterRelations: TEpiDatafileRelationList;
    procedure OrderedWalk(Const CallBackMethod: TEpiDatafileRelationListCallBack;
      Data: Pointer = nil);
  end;


implementation


type
  InternalData = record
    ExternalMethod: TEpiDatafileRelationListCallBack;
    ExternalData:   Pointer;
  end;
  PInternalData = ^InternalData;

{ TEpiDataFileRelationItemListHelper }

procedure TEpiDataFileRelationItemListHelper.InternalCallBack(
  const Relation: TEpiCustomRelationItem; const Depth: Cardinal;
  const Index: Cardinal; var aContinue: boolean; Data: Pointer);
begin
  with PInternalData(Data)^ do
    ExternalMethod(TEpiMasterRelation(Relation), Depth, Index, aContinue, ExternalData);
end;

function TEpiDataFileRelationItemListHelper.DataFileItemMethod(
  InputItem: TEpiCustomRelationItem): TEpiCustomItem;
begin
  result := TEpiMasterRelation(InputItem).Datafile;
end;

function TEpiDataFileRelationItemListHelper.GetOrderedDataFiles: TEpiDataFiles;
begin
  result := TEpiDataFiles(inherited GetOrderedItems(TEpiDataFiles, @DataFileItemMethod));
end;

function TEpiDataFileRelationItemListHelper.GetOrderedMasterRelations: TEpiDatafileRelationList;
begin
  result := TEpiDatafileRelationList(inherited GetOrderedItems(TEpiDatafileRelationList));
end;

procedure TEpiDataFileRelationItemListHelper.OrderedWalk(
  const CallBackMethod: TEpiDatafileRelationListCallBack; Data: Pointer);
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

