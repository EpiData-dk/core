unit epifields_helper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidatafiles, epidatafilestypes, epivaluelabels;

type

  { TEpiFieldHelper }
  TEpiGetValueLabelType = (
    gvtValue,
    gvtLabel,
    gvtValueLabel,
    gvtLabelValue
  );

  TEpiFieldHelper = class helper for TEpiField
  public
    // Returns string based on enum (see description above). If no valuelabel is present, return value.
    function GetValueLabel(Const Index: Integer; Gvt: TEpiGetValueLabelType = gvtLabel): String;
    function MaxByteLength: Cardinal;
    function MaxUTF8Length: Cardinal;
  end;

  { TEpiIntFieldHelper }

  TEpiIntFieldHelper = class helper(TEpiFieldHelper) for TEpiIntField
  public
    function MinValue: EpiInteger;
    function MaxValue: EpiInteger;
  end;

  { TEpiFloatFieldHelper }

  TEpiFloatFieldHelper = class helper(TEpiFieldHelper) for TEpiFloatField
  public
    function MinValue: EpiFloat;
    function MaxValue: EpiFloat;
  end;

  TEpiStringFieldHelper = class helper(TEpiFieldHelper) for TEpiStringField

  end;

  { TEpiDateFieldHelper }

  TEpiDateFieldHelper = class helper(TEpiFieldHelper) for TEpiDateField
  public

  end;

  TEpiBoolFieldHelper = class helper(TEpiFieldHelper) for TEpiBoolField

  end;

  { TEpiDateTimeFieldHelper }

  TEpiDateTimeFieldHelper = class helper(TEpiFieldHelper) for TEpiDateTimeField
  public

  end;

implementation

uses
  Math, LazUTF8;

{ TEpiFieldHelper }

function TEpiFieldHelper.GetValueLabel(const Index: Integer;
  Gvt: TEpiGetValueLabelType): String;
var
  VL: TEpiCustomValueLabel;
begin
  if (not Assigned(ValueLabelSet)) or
     (Gvt = gvtValue)
  then
    begin
      Result := AsString[Index];
      Exit;
    end;

  VL := ValueLabelSet.ValueLabel[AsValue[Index]];
  if not Assigned(VL) then
    begin
      Result := AsString[Index];
      Exit;
    end;

  case Gvt of
    gvtLabel:
      Result := VL.TheLabel.Text;
    gvtValueLabel:
      Result := AsString[Index] + ' ' + VL.TheLabel.Text;
    gvtLabelValue:
      Result := VL.TheLabel.Text + ' ' + AsString[Index];
  end;
end;

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

function TEpiIntFieldHelper.MinValue: EpiInteger;
var
  i: Integer;
begin
  Result := High(EpiInteger);
  for i := 0 to Size - 1 do
    if (not IsMissing[i]) then
      Result := Min(Result, AsInteger[i]);
end;

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

function TEpiFloatFieldHelper.MinValue: EpiFloat;
var
  i: Integer;
begin
  Result := MaxFloat;
  for i := 0 to Size - 1 do
    if (not IsMissing[i]) then
      Result := Min(Result, AsInteger[i]);
end;

function TEpiFloatFieldHelper.MaxValue: EpiFloat;
var
  i: Integer;
begin
  Result := -(MaxFloat);
  for i := 0 to Size - 1 do
    if (not IsMissing[i]) then
      Result := Max(Result, AsFloat[i]);
end;

end.

