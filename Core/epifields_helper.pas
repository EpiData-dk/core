unit epifields_helper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidatafiles, epidatafilestypes;

type

  { TEpiFieldHelper }

  TEpiFieldHelper = class helper for TEpiField
  public
    function MaxByteLength: Cardinal;
    function MaxUTF8Length: Cardinal;
  end;

  { TEpiIntFieldHelper }

  TEpiIntFieldHelper = class helper(TEpiFieldHelper) for TEpiIntField
  public
    function MaxValue: EpiInteger;
  end;

  { TEpiFloatFieldHelper }

  TEpiFloatFieldHelper = class helper(TEpiFieldHelper) for TEpiFloatField
  public
    function MaxValue: EpiFloat;
  end;

  TEpiStringFieldHelper = class helper(TEpiFieldHelper) for TEpiStringField

  end;

  { TEpiDateFieldHelper }

  TEpiDateFieldHelper = class helper(TEpiFieldHelper) for TEpiDateField
  public
    function MaxValue: EpiDate;
  end;

  TEpiBoolFieldHelper = class helper(TEpiFieldHelper) for TEpiBoolField

  end;

  { TEpiDateTimeFieldHelper }

  TEpiDateTimeFieldHelper = class helper(TEpiFieldHelper) for TEpiDateTimeField
  public
    function MaxValue: EpiTime;
  end;

implementation

uses
  Math, LazUTF8;

{ TEpiFieldHelper }

function TEpiFieldHelper.MaxByteLength: Cardinal;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Size - 1 do
    result := Max(Result, System.Length(AsString[i]));
end;

function TEpiFieldHelper.MaxUTF8Length: Cardinal;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Size - 1 do
    result := Max(Result, UTF8Length(AsString[i]));
end;

{ TEpiIntFieldHelper }

function TEpiIntFieldHelper.MaxValue: EpiInteger;
var
  i: Integer;
begin
  Result := Low(EpiInteger);
  for i := 0 to Size - 1 do
    if (not IsMissing[i]) then
      Result := Max(Result, AsInteger[i]);
end;

{ TEpiFloatFieldHelper }

function TEpiFloatFieldHelper.MaxValue: EpiFloat;
var
  i: Integer;
begin
  Result := -(MaxExtended);
  for i := 0 to Size - 1 do
    if (not IsMissing[i]) then
      Result := Max(Result, AsFloat[i]);
end;

{ TEpiDateFieldHelper }

function TEpiDateFieldHelper.MaxValue: EpiDate;
begin

end;

{ TEpiDateTimeFieldHelper }

function TEpiDateTimeFieldHelper.MaxValue: EpiTime;
begin

end;

end.

