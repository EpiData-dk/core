unit epidatafileutils;

{$mode objfpc}{$H+}

interface

uses
  sysutils, epidatafiles, epidatafilestypes, math;

function FieldClassFromFieldType(FieldType: TEpiFieldType): TEpiFieldClass;

function CompareFieldRecords(Out CmpResult: TValueSign;
  Const F1, F2: TEpiField; Const Idx1, Idx2: Integer;
  Const Casesensitive: boolean = false): boolean;

procedure DumpDatafileRecords(Const DF: TEpiDataFile);

implementation

uses
  epistringutils, LazUTF8;

function FieldClassFromFieldType(FieldType: TEpiFieldType): TEpiFieldClass;
begin
  case FieldType of
    ftInteger, ftAutoInc:
      Result := TEpiIntField;

    ftDMYDate,  ftMDYDate,  ftYMDDate,
    ftDMYAuto, ftMDYAuto, ftYMDAuto:
      Result := TEpiDateField;

    ftTime, ftTimeAuto:
      result := TEpiDateTimeField;

    ftFloat:
      Result := TEpiFloatField;

    ftBoolean:
      Result := TEpiBoolField;

    ftString, ftUpperString:
      Result := TEpiStringField;
  else
    result := nil;
  end;
end;

function CompareFieldRecords(out CmpResult: TValueSign;
  const F1, F2: TEpiField; const Idx1, Idx2: Integer;
  Const Casesensitive: boolean = false): boolean;
var
  Val: Integer;
begin
  // TODO : Perhaps reult=false if two types cannot be compared?
  case F1.FieldType of
    ftBoolean,
    ftInteger,
    ftAutoInc,
    ftDMYDate,
    ftMDYDate,
    ftYMDDate,
    ftDMYAuto,
    ftMDYAuto,
    ftYMDAuto:
      Val := F1.AsInteger[Idx1] - F2.AsInteger[Idx2];
    ftFloat,
    ftTime,
    ftTimeAuto:
      Val := CompareValue(F1.AsFloat[Idx1], F2.AsFloat[Idx2], 0.0);
    ftString,
    ftUpperString:
      if Casesensitive then
        Val := AnsiCompareStr(F1.AsString[Idx1], F2.AsString[Idx2])
      else
        Val := AnsiCompareText(F1.AsString[Idx1], F2.AsString[Idx2]);
  end;
  CmpResult := Sign(Val);
  Result := true;
end;

procedure DumpDatafileRecords(const DF: TEpiDataFile);
const
  Width = 10;
var
  i: Integer;
  j: Integer;
begin
  for i := 0 to DF.Fields.Count - 1 do
    Write(Format('%-' + IntToStr(Width+1) +'s', [UTF8ToSys(EpiCutString(Df.Fields[i].Name, Width, false))]));
  WriteLn('');
  WriteLn('----------------------------------');

  for j := 0 to DF.Size - 1 do
  begin
    for i := 0 to DF.Fields.Count -1 do
      Write(Format('%-' + IntToStr(Width+1) +'s', [UTF8ToSys(EpiCutString(DF.Fields[i].AsString[j], Width, false))]));
    WriteLn('');
  end;
end;

end.


