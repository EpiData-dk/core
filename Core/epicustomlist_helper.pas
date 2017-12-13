unit epicustomlist_helper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epicustombase;

type

  { TEpiCustomListHelper }

  TEpiCustomListHelper = class helper for TEpiCustomList
  private
    function GetProtectedCount: Integer;
    function GetUnprotectedCount: Integer;
  public
    function GetItemNames(WithObjects: boolean): TStrings;
    property ProtectedCount: Integer read GetProtectedCount;
    property UnprotectedCount: Integer read GetUnprotectedCount;
  end;

implementation

{ TEpiCustomListHelper }

function TEpiCustomListHelper.GetProtectedCount: Integer;
var
  Item: TEpiCustomItem;
begin
  Result := 0;
  for Item in Self do
    if Item.ProtectedItem then
      Inc(Result);
end;

function TEpiCustomListHelper.GetUnprotectedCount: Integer;
begin
  result := Count - ProtectedCount;
end;

function TEpiCustomListHelper.GetItemNames(WithObjects: boolean): TStrings;
var
  I: TEpiCustomItem;
begin
  Result := TStringList.Create;

  if WithObjects then
    for I in Self do
      Result.AddObject(I.Name, I)
  else
    for I in Self do
      Result.Add(I.Name);
end;

end.

