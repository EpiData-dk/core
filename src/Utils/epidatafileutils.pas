unit epidatafileutils;

{$mode objfpc}{$H+}

interface

uses
  sysutils, epidatafiles, epidatafilestypes, epicustombase, math;

function FieldClassFromFieldType(FieldType: TEpiFieldType): TEpiFieldClass;

function CompareFieldRecords(Out CmpResult: TValueSign;
  Const F1, F2: TEpiField; Const Idx1, Idx2: Integer;
  Const Casesensitive: boolean = false): boolean;

procedure DumpDatafileRecords(Const DF: TEpiDataFile);
procedure DumpDatafileControlItems(Const DF: TEpiDataFile);
procedure DumpField(Const F: TEpiField);

function Max(Const Ft1, Ft2: TEpiFieldType): TEpiFieldType; overload;
function Min(Const Ft1, Ft2: TEpiFieldType): TEpiFieldType; overload;
function CompareFieldTypeOrder(Const Ft1, Ft2: TEpiFieldType): integer;

implementation

uses
  epistringutils, LazUTF8, epimiscutils,
  epireport_report_fieldinfo, epireport_generator_txt;

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

    ftMemo:
      result := TEpiMemoField;
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
    ftFloat:
      Val := CompareValue(F1.AsFloat[Idx1], F2.AsFloat[Idx2], 0.0);
    ftTime,
    ftTimeAuto:
      Val := CompareValue(F1.AsTime[Idx1], F2.AsTime[Idx2], Double(0.0));
    ftString,
    ftUpperString,
    ftMemo:
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
  if not IsConsole then exit;

  for i := 0 to DF.Fields.Count - 1 do
    Write(Format('[%s] %-' + IntToStr(Width) +'s',
          [EpiTypeNamesShort[Df.Field[i].FieldType],
           UTF8ToSys(EpiCutString(Df.Fields[i].Name, Width-1, false))
          ])
    );
  WriteLn('');
  WriteLn('----------------------------------');

  for j := 0 to DF.Size - 1 do
  begin
    for i := 0 to DF.Fields.Count -1 do
      Write(Format('%-' + IntToStr(Width+4) +'s', [UTF8ToSys(EpiCutString(DF.Fields[i].AsString[j], Width + 4 - 1, false))]));
    WriteLn('');
  end;
end;

procedure DumpDatafileControlItems(const DF: TEpiDataFile);
var
  i: Integer;
  CI: TEpiCustomControlItem;
begin
  if not IsConsole then exit;

  for i := 0 to DF.ControlItems.Count - 1 do
  begin
    CI := DF.ControlItem[i];
    Write(CI.Name);

    if CI is TEpiField then
      Write(' ', EpiTypeNamesShort[TEpiField(CI).FieldType]);
    WriteLn();
  end;
end;

procedure DumpField(const F: TEpiField);
var
  R: TEpiReportFieldInfo;
begin
  R := TEpiReportFieldInfo.Create(TEpiReportTXTGenerator);
  R.Field := F;
  R.RunReport;
  WriteLn(R.ReportText);
  R.Free;
end;

function Max(const Ft1, Ft2: TEpiFieldType): TEpiFieldType;
begin
  if CompareFieldTypeOrder(Ft1, Ft2) < 0
  then
    Result := Ft1
  else
    Result := Ft2;
end;

function Min(const Ft1, Ft2: TEpiFieldType): TEpiFieldType;
begin
  if CompareFieldTypeOrder(Ft1, Ft2) > 0
  then
    Result := Ft1
  else
    Result := Ft2;
end;

function CompareFieldTypeOrder(const Ft1, Ft2: TEpiFieldType): integer;
var
  CompareSet: TEpiFieldTypes;
  Ft1Set: TEpiFieldTypes;
  Ft2Set: TEpiFieldTypes;
begin
  Ft1Set := OrderedFieldTypeSetFromFieldType(Ft1);
  Ft2Set := OrderedFieldTypeSetFromFieldType(Ft2);

  if Ft1Set = Ft2Set then Exit(0);
  Result := 1;

  // The "order" of field types should be as follows:
  //  1: Bool
  //  2: Int/Date
  //  3: Float/Time
  //  4: String
  //
  // Reason:
  //  It is always possible to "convert" a higher type to a
  //  lower type without loss of data/precission. But the
  //  other way is not always possible.

  CompareSet := BoolFieldTypes;
  if (Ft2 in BoolFieldTypes) and
     (not (Ft1 in CompareSet))
  then
    Exit(-1);

  CompareSet := CompareSet + (IntFieldTypes + DateFieldTypes);
  if (Ft2 in IntFieldTypes + DateFieldTypes) and
     (not (Ft1 in CompareSet))
  then
    Exit(-1);

  CompareSet := CompareSet + (FloatFieldTypes + TimeFieldTypes);
  if (Ft2 in FloatFieldTypes + TimeFieldTypes) and
     (not (Ft1 in CompareSet))
  then
    Exit(-1);

  CompareSet := CompareSet + StringFieldTypes;
  if (Ft2 in StringFieldTypes) and
     (not (Ft1 in CompareSet))
  then
    Exit(-1);
end;

end.


