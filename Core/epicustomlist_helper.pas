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
  public
    property ProtectedCount: Integer read GetProtectedCount;
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

end.

