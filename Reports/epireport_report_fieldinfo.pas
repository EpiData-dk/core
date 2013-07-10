unit epireport_report_fieldinfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_base,
  epidatafiles;

type

  EEpiReportFieldInfo = class(EEpiReportBaseException);

  { TEpiReportProjectHeader }

  { TEpiReportFieldInfo }

  TEpiReportFieldInfo = class(TEpiReportBase)
  private
    FField: TEpiField;
  protected
    procedure DoSanityCheck; override;
  public
    procedure RunReport; override;
    property Field: TEpiField read FField write FField;
  end;

implementation

uses
  epireport_report_valuelabelsetlist, epidatafilestypes,
  epistringutils, epimiscutils, epireport_types;

resourcestring
  SEpiReportFieldInfoNoField = 'EpiReport: No field assigned to field info.';

{ TEpiReportFieldInfo }

procedure TEpiReportFieldInfo.DoSanityCheck;
begin
  inherited DoSanityCheck;

  if not Assigned(Field) then
    DoError(EEpiReportFieldInfo, SEpiReportFieldInfoNoField);
end;

procedure TEpiReportFieldInfo.RunReport;
var
  j: Integer;
  Report: TEpiReportValueLabelSetList;
  S: String;
  k: Integer;
begin
  inherited RunReport;

  with Field do
  begin
    j := 1;  // Type + Length
    if Assigned(Comparison)     then inc(j);
    if Assigned(Jumps)          then inc(j);
    if Assigned(Calculation)    then inc(j);
    if Assigned(Ranges)         then inc(j);
    if (EntryMode <> emDefault) then inc(j);
    if ConfirmEntry             then inc(j);
    if RepeatValue              then inc(j);
    if (DefaultValueAsString <> '') then inc(j);
    if ShowValueLabel               then inc(j);
    if ForcePickList                then inc(j);
    if Assigned(ValueLabelWriteField) then inc(j);
    if (Notes.Text <> '')             then inc(j);

    DoTableHeader('Field: ' + Name + ': ' + Question.Text, 2, j + 1, []);
//    DoTableCell(0, 0, 'Extension:');
//    DoTableCell(1, 0, 'Content/Value:');

    DoTableCell(0, 0, 'Type');
    DoTableCell(1, 0, EpiTypeNames[FieldType], tcaRightAdjust);

    DoTableCell(0, 1, 'Length');
    if FieldType in FloatFieldTypes then
      DoTableCell(1, 1, Format('%d.%d', [Length-Decimals-1, Decimals]), tcaRightAdjust)
    else
      DoTableCell(1, 1, IntToStr(Length), tcaRightAdjust);

    j := 2;
    if (EntryMode <> emDefault) then
    begin
      DoTableCell(0, j, 'Entry Mode');
      Case EntryMode of
        emMustEnter: DoTableCell(1, j, 'Must Enter', tcaRightAdjust);
        emNoEnter:   DoTableCell(1, j, 'No Enter', tcaRightAdjust);
      end;
      inc(j);
    end;

    if Assigned(Comparison)  then
    begin
      DoTableCell(0, j, 'Comparison');
      S := Name + ComparisonTypeToString(Comparison.CompareType) + Comparison.CompareField.Name;
      DoTableCell(1, j, S, tcaRightAdjust);
      Inc(j);
    end;

    if Assigned(Ranges)       then
    begin
      DoTableCell(0, j, 'Range');
      DoTableCell(1, j, Ranges[0].AsString[true] + '-' + Ranges[0].AsString[false], tcaRightAdjust);
      inc(j);
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

    if ConfirmEntry             then
    begin
      DoTableCell(0, j, 'Confirm Entry');
      DoTableCell(1, j, 'true', tcaRightAdjust);
      inc(j);
    end;

    if RepeatValue              then
    begin
      DoTableCell(0, j, 'Repeat Value');
      DoTableCell(1, j, 'true', tcaRightAdjust);
      inc(j);
    end;

    if (DefaultValueAsString <> '') then
    begin
      DoTableCell(0, j, 'Default Value');
      DoTableCell(1, j, DefaultValueAsString, tcaRightAdjust);
      inc(j);
    end;

    if Assigned(ValueLabelWriteField) then
    begin
      DoTableCell(0, j, 'Write Value Label text to Field');
      DoTableCell(1, j, ValueLabelWriteField.Name + ': ' + EpiCutString(ValueLabelWriteField.Question.Text, 15), tcaRightAdjust);
      inc(j);
    end;

    if ShowValueLabel               then
    begin
      DoTableCell(0, j, 'Show Value Label');
      DoTableCell(1, j, 'true', tcaRightAdjust);
      inc(j);
    end;

    if ForcePickList                then
    begin
      DoTableCell(0, j, 'Show Picklist');
      DoTableCell(1, j, 'true', tcaRightAdjust);
      inc(j);
    end;

    if (Notes.Text <> '') then
    begin
      DoTableCell(0, j, 'Notes');
      DoTableCell(1, j, Notes.Text, tcaRightAdjust);
      Inc(j);
    end;
    DoTableFooter('');
  end;

  DoLineText('');
  if Assigned(Field.ValueLabelSet) then
  begin
    Report := TEpiReportValueLabelSetList.Create(FReportGenerator);
    Report.ValueLabelSet := Field.ValueLabelSet;
    Report.RunReport;
    Report.Free;
  end;
end;

end.

