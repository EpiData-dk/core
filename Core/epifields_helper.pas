unit epifields_helper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidatafiles, epidatafilestypes, epivaluelabels, math;

type

  TEpiGetValueLabelType = (
    gvtValue,
    gvtLabel,
    gvtValueLabel,
    gvtLabelValue
  );

  TEpiGetVariableLabelType = (
    gvtVarName,
    gvtVarLabel,
    gvtVarNameLabel,
    gvtVarLabelName
  );

  { TEpiFieldsHelper }

  TEpiFieldsHelper = class helper for TEpiFields
  public
    function CompareRecords(IndexA, IndexB: Integer): TValueSign;
  end;


  { TEpiFieldHelper }

  TEpiFieldHelper = class helper for TEpiField
  public
    // Returns name and/or label based on enum. If no Question/Label exists then name is always returned
    function GetVariableLabel(Gvt: TEpiGetVariableLabelType = gvtVarLabel): UTF8String;
    // Returns string based on enum (see description above). If no valuelabel is present, return value.
    function GetValueLabel(Const Index: Integer; Gvt: TEpiGetValueLabelType = gvtLabel): String;
    // Returns string based on enum (see description above). If no valuelabel is present, return value.
    // - in addition the values are formatted according to Length/Decimals
    function GetValueLabelFormatted(Const Index: Integer; Gvt: TEpiGetValueLabelType = gvtLabel): String;
    function AsFormatString(Const Index: Integer; Const FillSpaces: boolean = false): string;
    function AcceptsValuelabelSet(VL: TEpiValueLabelSet): boolean; overload;
    function AcceptsValuelabelSet(VL: TEpiValueLabelSet; ALength, ADecimals: Integer): boolean; overload;
    function MaxByteLength: Cardinal;
    function MaxUTF8Length: Cardinal;
    function IsKeyfield: boolean;
    function NonMissingSize: Integer;
    // Copy data from SrcField[SrcIdx] into self[DstIdx] - return false if fieldtypes do not match.
    function CopyValue(SrcField: TEpiField; SrcIdx, DstIdx: Integer): boolean;
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
  LazUTF8;

{ TEpiFieldsHelper }

function TEpiFieldsHelper.CompareRecords(IndexA, IndexB: Integer): TValueSign;
var
  F: TEpiField;
begin
  Result := ZeroValue;

  for F in Self do
    begin
      Result := F.Compare(IndexA, IndexB);
      if Result <> ZeroValue then
        Exit;
    end;
end;

{ TEpiFieldHelper }

function TEpiFieldHelper.GetVariableLabel(Gvt: TEpiGetVariableLabelType
  ): UTF8String;
begin
  if (Question.Text = '') then
    begin
      Result := Name;
      Exit;
    end;


  case Gvt of
    gvtVarName:
      Result := Name;

    gvtVarLabel:
      Result := Question.Text;

    gvtVarNameLabel:
      Result := Name + ' ' + Question.Text;

    gvtVarLabelName:
      Result := Question.Text + ' ' + Name;
  end;
end;

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

function TEpiFieldHelper.GetValueLabelFormatted(const Index: Integer;
  Gvt: TEpiGetValueLabelType): String;
var
  VL: TEpiCustomValueLabel;
begin
  if (not Assigned(ValueLabelSet)) or
     (Gvt = gvtValue)
  then
    begin
      Result := AsFormatString(Index);
      Exit;
    end;

  VL := ValueLabelSet.ValueLabel[AsValue[Index]];
  if not Assigned(VL) then
    begin
      Result := AsFormatString(Index);
      Exit;
    end;

  case Gvt of
    gvtLabel:
      Result := VL.TheLabel.Text;
    gvtValueLabel:
      Result := AsFormatString(Index) + ' ' + VL.TheLabel.Text;
    gvtLabelValue:
      Result := VL.TheLabel.Text + ' ' + AsFormatString(Index);
  end;
end;

function TEpiFieldHelper.AsFormatString(const Index: Integer;
  const FillSpaces: boolean): string;
begin
  if IsMissing[Index] then
    result := AsString[Index]
  else
    case FieldType of
      ftBoolean:
        result := AsString[Index];

      ftAutoInc,
      ftInteger:
        result := format(FormatString(FillSpaces), [AsInteger[Index]]);

      ftFloat:
        result := format(FormatString(FillSpaces), [AsFloat[Index]]);

      ftDMYDate,
      ftMDYDate,
      ftYMDDate,
      ftDMYAuto,
      ftMDYAuto,
      ftYMDAuto:
        result := AsString[Index];

      ftTime,
      ftTimeAuto:
        result := AsString[Index];

      ftUpperString,
      ftString,
      ftMemo:
        result := format(FormatString(FillSpaces), [AsString[Index]]);
    end;
end;

function TEpiFieldHelper.AcceptsValuelabelSet(VL: TEpiValueLabelSet): boolean;
begin
  result := AcceptsValuelabelSet(VL, Length, Decimals);
end;

function TEpiFieldHelper.AcceptsValuelabelSet(VL: TEpiValueLabelSet; ALength,
  ADecimals: Integer): boolean;
var
  S: String;
  l: SizeInt;
  DecL, IntL: Integer;
  V: TEpiCustomValueLabel;
begin
  case VL.LabelType of
    ftInteger:
      begin
        Result := (FieldType in [ftInteger, ftFloat]) and
                  (VL.MaxValueLength <= ALength)
      end;

    ftFloat:
      begin
        Result := (FieldType = ftFloat);

        if Result then
        begin
          IntL := 0;
          DecL := 0;

          for V in VL do
          begin
            S := V.ValueAsString;
            l := Pos(DecimalSeparator, S);
            if l = 0 then
              IntL := System.Length(S)
            else
              IntL := Max(IntL, l - 1);
            DecL := Max(DecL, System.Length(S) - (IntL+1));
          end;
        end;

        Result := Result and
          (IntL <= ALength) and
          (DecL <= ADecimals);
      end;

    ftString,
    ftUpperString:
      Result := (FieldType in [ftString, ftUpperString]);

    ftTime:
      Result := (FieldType = ftTime);

    ftDMYDate,
    ftMDYDate,
    ftYMDDate:
      Result := (FieldType in (DateFieldTypes - AutoFieldTypes));
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

function TEpiFieldHelper.IsKeyfield: boolean;
begin
  result := DataFile.KeyFields.FieldExists(Self);
end;

function TEpiFieldHelper.NonMissingSize: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Size - 1 do
    if (not IsMissing[i]) then
      Inc(Result);
end;

function TEpiFieldHelper.CopyValue(SrcField: TEpiField; SrcIdx, DstIdx: Integer
  ): boolean;
var
  SrcFieldTypes, DstFieldTypes: TEpiFieldTypes;
begin
  Result := false;

  SrcFieldTypes := NativeFieldTypeSetFromFieldType(SrcField.FieldType);
  DstFieldTypes := NativeFieldTypeSetFromFieldType(FieldType);

  if (SrcFieldTypes * DstFieldTypes) = [] then
    Exit;

  case FieldType of
    ftBoolean:
      AsBoolean[DstIdx] := SrcField.AsBoolean[SrcIdx];

    ftInteger,
    ftAutoInc:
      AsInteger[DstIdx] := SrcField.AsInteger[SrcIdx];

    ftFloat:
      AsFloat[DstIdx] := SrcField.AsFloat[SrcIdx];

    ftDMYDate,
    ftMDYDate,
    ftYMDDate,
    ftDMYAuto,
    ftMDYAuto,
    ftYMDAuto:
      AsDate[DstIdx] := SrcField.AsDate[SrcIdx];

    ftTime,
    ftTimeAuto:
      AsTime[DstIdx] := SrcField.AsTime[SrcIdx];

    ftUpperString,
    ftString,
    ftMemo:
      AsString[DstIdx] := SrcField.AsString[SrcIdx];
  end;

  Result := true;
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

