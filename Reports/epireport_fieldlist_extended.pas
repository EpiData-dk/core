unit epireport_fieldlist_extended;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_datafilesreport_base, epireport_types,
  epidatafiles, epireport_generator_base;

type

  { TEpiReportExtendedFieldList }

  TEpiReportExtendedFieldList = class(TEpiReportDataFilesBase)
  private
    FSortType:  TEpiReportFieldListSortType;
  protected
    procedure PrintDataFile(const DataFile: TEpiDataFile); override;
  public
    constructor Create(ReportGenerator: TEpiReportGeneratorBase);
       override;
    procedure   RunReport; override;
    property    SortType: TEpiReportFieldListSortType read FSortType write FSortType;
  end;

implementation

uses
  epimiscutils, epidatafilestypes, epistringutils;

{ TEpiReportExtendedFieldList }

function FieldNameSort(Item1, Item2: Pointer): integer;
var
  Field1: TEpiField absolute Item1;
  Field2: TEpiField absolute Item2;
begin
  result:= AnsiCompareStr(Field1.Name, Field2.Name);
end;

procedure TEpiReportExtendedFieldList.PrintDatafile(const DataFile: TEpiDataFile
  );
var
  FieldList: TEpiFields;
  ExtendedList: TList;
  i: Integer;
  j: Integer;
  S: String;
  k: Integer;
begin
  FieldList := TEpiFields.Create(nil);
  FieldList.Sorted := true;
  for i := 0 to DataFile.Fields.Count - 1 do FieldList.AddItem(DataFile.Field[i]);
  case SortType of
    stFieldName: FieldList.OnSort := @FieldNameSort;
    stEntryFlow: ;// Do nothing - Fields is alread sorted by flow (in Core v1.3)
  end;
  FieldList.Sort;

  DoHeading(DataFile.Caption.Text);
  DoTableHeader('Overview:', 14, DataFile.Fields.Count + 1);
  DoTableCell(0, 0, 'Name');
  DoTableCell(1, 0, 'Type');
  DoTableCell(2, 0, 'ValueLabel');
  DoTableCell(3, 0, 'Question');
  DoTableCell(4, 0, 'Length');
  DoTableCell(5, 0, 'Range');
  DoTableCell(6, 0, 'Entry Mode');
  DoTableCell(7, 0, 'Confirm');
  DoTableCell(8, 0, 'Repeat');
  DoTableCell(9, 0, 'Default Value');
  DoTableCell(10, 0, 'Show valuelabel');
  DoTableCell(11, 0, 'Show picklist');
  DoTableCell(12, 0, 'Write valuelabel');
  DoTableCell(13, 0, 'Extended');

  ExtendedList := TList.Create;
  for i := 0 to FieldList.Count - 1 do
  with FieldList[i] do
    begin
    DoTableCell(0, i+1, Name);
    DoTableCell(1, i+1, EpiTypeNames[FieldType]);

    if Assigned(ValueLabelSet) then
      DoTableCell(2, i+1, ValueLabelSet.Name)
    else
      DoTableCell(2, i+1, '');
    DoTableCell(3, i+1, Question.Text);
    if FieldType in FloatFieldTypes then
      DoTableCell(4, i+1, Format('%d.%d', [Length - Decimals - 1, Decimals]))
    else
      DoTableCell(4, i+1, IntToStr(Length));

    if Assigned(Ranges) then
      DoTableCell(5, i+1, Ranges.RangesToText)
    else
      DoTableCell(5, i+1, '');

    Case EntryMode of
      emDefault:   DoTableCell(6, i+1, '');
      emMustEnter: DoTableCell(6, i+1, 'Must Enter');
      emNoEnter:   DoTableCell(6, i+1, 'No Enter');
    end;
    DoTableCell(7, i+1, BoolToStr(ConfirmEntry, 'x', ''));
    DoTableCell(8, i+1, BoolToStr(RepeatValue, 'x', ''));
    DoTableCell(9, i+1, DefaultValueAsString);
    DoTableCell(10, i+1, BoolToStr(ShowValueLabel, 'x', ''));
    DoTableCell(11, i+1, BoolToStr(ForcePickList, 'x', ''));
    if Assigned(ValueLabelWriteField) then
      DoTableCell(12, i+1, ValueLabelWriteField.Name)
    else
      DoTableCell(12, i+1, '');

    if Assigned(Comparison) or
       Assigned(Jumps) or
       Assigned(Calculation) or
       Assigned(Ranges) or
       (EntryMode <> emDefault) or
       ConfirmEntry or
       RepeatValue or
       (DefaultValueAsString <> '') or
       ShowValueLabel or
       ForcePickList or
       Assigned(ValueLabelWriteField) or
       (Notes.Text <> '') then
    begin
      DoTableCell(13, i+1, 'x');
      ExtendedList.Add(TEpiField(FieldList[i]));
    end else
      DoTableCell(13, i+1, '');
  end;
  DoTableFooter('');

  for i := 0 to ExtendedList.Count - 1 do
  with TEpiField(ExtendedList[i]) do
  begin
    j := 0;
    if Assigned(Comparison)  then inc(j);
    if Assigned(Jumps)       then inc(j);
    if Assigned(Calculation) then inc(j);
    if Assigned(Ranges)      then inc(j);
    if (EntryMode <> emDefault) then inc(j);
    if ConfirmEntry             then inc(j);
    if RepeatValue              then inc(j);
    if (DefaultValueAsString <> '') then inc(j);
    if ShowValueLabel               then inc(j);
    if ForcePickList                then inc(j);
    if Assigned(ValueLabelWriteField) then inc(j);
    if (Notes.Text <> '')             then inc(j);

    DoTableHeader(Name + ': ' + Question.Text, 2, j + 1);
    DoTableCell(0, 0, 'Extension:');
    DoTableCell(1, 0, 'Value:');

    j := 1;
    if Assigned(Comparison)  then
    begin
      DoTableCell(0, j, 'Comparison');
      S := Name + ComparisonTypeToString(Comparison.CompareType) + Comparison.CompareField.Name;
      DoTableCell(1, j, S);
      Inc(j);
    end;

    if Assigned(Jumps)       then
    begin
      DoTableCell(0, j, 'Jumps');
      S := '';
      for k := 0 to Jumps.Count -1 do
      with Jumps[k] do
      begin
        S += JumpValueAsString + ' > ';
        case JumpType of
          jtSaveRecord:    S += 'Save Record';
          jtExitSection:   S += 'Exit Section';
          jtSkipNextField: S += 'Skip Next Field';
          jtToField:       S += JumpToField.Name;
        end;
        S += LineEnding;
      end;
      DoTableCell(1, j, S);
      Inc(j);
    end;

    if Assigned(Calculation) then
    begin
      DoTableCell(0, j, 'Calculation');

      case Calculation.CalcType of
        ctTimeDiff:
          with TEpiTimeCalc(Calculation) do
          begin
            S := 'Time Difference:' + LineEnding +
                 ResultField.Name + '=(';
            if Assigned(StartDate) then S += StartDate.Name + '+';
            if Assigned(StartTime) then S += StartTime.Name;
            S += ')-(';
            if Assigned(EndDate) then S += EndDate.Name + '+';
            if Assigned(EndTime) then S += EndTime.Name;
            S += ')';
          end;
        ctCombineDate:
          with TEpiCombineDateCalc(Calculation) do
          begin
            S := 'Create Date:' + LineEnding +
                 ResultField.Name + '=DMY(';
            S += Day.Name + ',';
            S += Month.Name + ',';
            S += Year.Name + ')';
          end;
        ctCombineString:
          with TEpiCombineStringCalc(Calculation) do
          begin
            S := 'Combine String:' + LineEnding +
                 ResultField.Name + '=';
            if Assigned(Field1) then S += Field1.Name + ' + ';
            S += Delim1 + ' + ';
            if Assigned(Field2) then S += Field2.Name + ' + ';
            S += Delim2 + ' + ';
            if Assigned(Field3) then S += Field3.Name;
          end;
      end;

      DoTableCell(1, j, S);
      Inc(j);
    end;

    if Assigned(Ranges)       then
    begin
      DoTableCell(0, j, 'Range');
      DoTableCell(1, j, Ranges[0].AsString[true] + '-' + Ranges[0].AsString[false]);
      inc(j);
    end;

    if (EntryMode <> emDefault) then
    begin
      DoTableCell(0, j, 'Entry Mode');
      Case EntryMode of
        emMustEnter: DoTableCell(1, j, 'Must Enter');
        emNoEnter:   DoTableCell(1, j, 'No Enter');
      end;
      inc(j);
    end;

    if ConfirmEntry             then
    begin
      DoTableCell(0, j, 'Confirm Entry');
      DoTableCell(1, j, 'true');
      inc(j);
    end;

    if RepeatValue              then
    begin
      DoTableCell(0, j, 'Repeat Value');
      DoTableCell(1, j, 'true');
      inc(j);
    end;

    if (DefaultValueAsString <> '') then
    begin
      DoTableCell(0, j, 'Default Value');
      DoTableCell(1, j, DefaultValueAsString);
      inc(j);
    end;

    if ShowValueLabel               then
    begin
      DoTableCell(0, j, 'Show Value Label');
      DoTableCell(1, j, 'true');
      inc(j);
    end;

    if ForcePickList                then
    begin
      DoTableCell(0, j, 'Show Picklist');
      DoTableCell(1, j, 'true');
      inc(j);
    end;

    if Assigned(ValueLabelWriteField) then
    begin
      DoTableCell(0, j, 'Write Value Label text to Field');
      DoTableCell(1, j, ValueLabelWriteField.Name + ': ' + EpiCutString(ValueLabelWriteField.Question.Text, 15));
      inc(j);
    end;

    if (Notes.Text <> '') then
    begin
      DoTableCell(0, j, 'Notes');
      DoTableCell(1, j, Notes.Text);
      Inc(j);
    end;
    DoTableFooter('');
  end;
end;

constructor TEpiReportExtendedFieldList.Create(
  ReportGenerator: TEpiReportGeneratorBase);
begin
  inherited Create(ReportGenerator);
  FSortType := stEntryFlow;
end;

procedure TEpiReportExtendedFieldList.RunReport;
var
  i: Integer;
begin
  inherited RunReport;

  for i := 0 to EpiDataFiles.Count - 1 do
    PrintDatafile(EpiDataFiles[i]);
end;

end.

