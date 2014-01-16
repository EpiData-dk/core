unit epireport_report_projectvalidator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_base, epidocument, epidatafiles,
  epireport_generator_base,
  epitools_projectvalidate;

type

  { TEpiReportProjectValidator }

  TEpiReportProjectValidator = class(TEpiReportBase)
  private
    FDocument: TEpiDocument;
    FKeyFields: TEpiFields;
    FOptions: TEpiToolsProjectValidateOptions;
    FProjectValidationOptions: TEpiToolsProjectValidateOptions;
    FValidationFields: TEpiFields;
    function   RunTool(out RecordResult: TEpiProjectResultArray;
      out StudyResult: TEpiProjectStudyArray): Boolean;

  private
    { Report parts }
    procedure DoReportStart;
    procedure DoReportResultTable(Const RecordArray: TEpiProjectResultArray;
      Const StudyArray: TEpiProjectStudyArray);
    procedure DoStudyReport(Const StudyResult: TEpiProjectStudyArray);
    procedure DoRecordsReport(Const RecordResult: TEpiProjectResultArray);
  protected
    procedure DoSanityCheck; override;
  public
    constructor Create(ReportGenerator: TEpiReportGeneratorBase); override;
    procedure   RunReport; override;
    property    Document: TEpiDocument read FDocument write FDocument;
    property    ProjectValidationOptions: TEpiToolsProjectValidateOptions read FProjectValidationOptions write FProjectValidationOptions;
    property    KeyFields: TEpiFields read FKeyFields write FKeyFields;
    property    ValidationFields: TEpiFields read FValidationFields write FValidationFields;
    property    Options: TEpiToolsProjectValidateOptions read FOptions write FOptions;
  end;

implementation

uses
  LazUTF8, typinfo, epireport_types, epireport_report_fieldlist,
  epidatafilestypes;

{ TEpiReportProjectValidator }
resourcestring
  SEpiReportProjectValidationNoDocument = 'EpiReport: No Document assigned to Project Validation';

function TEpiReportProjectValidator.RunTool(out
  RecordResult: TEpiProjectResultArray; out StudyResult: TEpiProjectStudyArray
  ): Boolean;
var
  T: TEpiProjectValidationTool;
  Dummy: TEpiProjectStudyArray;
begin
  T := TEpiProjectValidationTool.Create;
  T.Document := Document;
  T.ValidationFields := ValidationFields;
  T.KeyFields        := KeyFields;
  T.ValidateProject(RecordResult, StudyResult, Options);
  T.Free;
  Result := true;
end;

procedure TEpiReportProjectValidator.DoReportStart;
var
  OptCount: Integer;
  Opt: TEpiToolsProjectValidateOption;
  I: Integer;
  S: String;
  R: TEpiReportFieldList;
begin
  DoSection('Selections for validation:');
  DoLineText('');

  DoTableHeader('Options:', 2, Integer(High(TEpiToolsProjectValidateOption)) + 2);
  DoTableCell(0, 0, 'Option');
  DoTableCell(1, 0, 'Selected');

  I := 1;
  for Opt in TEpiToolsProjectValidateOption do
  begin
    DoTableCell(0, I, EpiToolProjectValidationOptionText[Opt]);
    DoTableCell(1, I, BoolToStr(Opt in Options, 'Yes', 'No'));
    Inc(I);
  end;
  DoTableFooter('');

  if Assigned(KeyFields) and
     (KeyFields.Count > 0)
  then
    begin
      DoLineText('');
      DoHeading('Key Fields:');
      S := '';
      for i := 0 to KeyFields.Count - 1 do
        S := S + KeyFields[i].Name + ' ';
      DoLineText(S);
    end;

  DoLineText('');
  R := TEpiReportFieldList.Create(FReportGenerator);
  R.Fields := ValidationFields;
  R.TableHeader := 'Validation Fields:';
  R.RunReport;
  R.Free;
end;

procedure TEpiReportProjectValidator.DoReportResultTable(
  const RecordArray: TEpiProjectResultArray;
  const StudyArray: TEpiProjectStudyArray);
var
  RecordErrorCount: Integer;
  i: Integer;
  FieldErrorCount: Integer;
  DF: TEpiDataFile;
  RecordCount: Integer;
  OptCount: Integer;
  SumTable: array[TEpiToolsProjectValidateOption] of array of cardinal;
  TotalTable: array[TEpiToolsProjectValidateOption] of cardinal;
  Opt: TEpiToolsProjectValidateOption;
  Rec: TEpiProjectValidateResultRecord;
  S: String;
  j: Integer;

  function CalcErrorPct: Extended;
  begin
    Result := RecordErrorCount / RecordCount;
  end;

  function CalcErrorFieldPct: Extended;
  begin
    Result := FieldErrorCount / (RecordCount * ValidationFields.Count);
  end;

begin
  RecordErrorCount := 0;
  FieldErrorCount  := Length(RecordArray);

  RecordCount := 0;
  DF := Document.DataFiles[0];
  for i := 0 to DF.Size - 1 do
    if not DF.Deleted[i] then
      Inc(RecordCount);

  if Length(RecordArray) > 0 then
    Inc(RecordErrorCount);
  for i := Low(RecordArray) + 1 to High(RecordArray) do
  begin
    if RecordArray[i-1].RecNo <> RecordArray[i].RecNo then
      Inc(RecordErrorCount);
  end;

  DoTableHeader('Overview', 2, 8);
  DoTableCell(0, 0, 'Test');                               DoTableCell(1, 0, 'Result');
  DoTableCell(0, 1, 'Unspecified studyinformation items'); DoTableCell(1, 1, IntToStr(Length(StudyArray)));
  DoTableCell(0, 2, 'Number of fields checked');           DoTableCell(1, 2, IntToStr(ValidationFields.Count));
  DoTableCell(0, 3, 'Number of records checked');          DoTableCell(1, 3, IntToStr(RecordCount));
  DoTableCell(0, 4, 'Records with errors');                DoTableCell(1, 4, IntToStr(RecordErrorCount));
  DoTableCell(0, 5, 'Field entries with errors');          DoTableCell(1, 5, IntToStr(FieldErrorCount));
  DoTableCell(0, 6, 'Error percentage (#records)');        DoTableCell(1, 6, FormatFloat('##0.00', CalcErrorPct * 100));
  DoTableCell(0, 7, 'Error percentage (#fields)');         DoTableCell(1, 7, FormatFloat('##0.00', CalcErrorFieldPct * 100));
  DoTableFooter('');


  DoLineText('');
  OptCount := 0;
  for Opt in TEpiToolsProjectValidateOption do
  begin
    SetLength(SumTable[Opt], ValidationFields.Count);
    Inc(OptCount);
    TotalTable[Opt] := 0;
  end;
  Dec(OptCount, 2);

  for i := Low(RecordArray) to High(RecordArray) do
  begin
    Rec := RecordArray[i];
    Inc(SumTable[Rec.FailedCheck, ValidationFields.IndexOf(Rec.Field)]);
    Inc(TotalTable[Rec.FailedCheck]);
  end;

  DoTableHeader('Summarised overview', OptCount + 1, ValidationFields.Count + 2);
  for i := 0 to ValidationFields.Count - 1 do
    DoTableCell(0, i + 1, ValidationFields[i].Name);
  DoTableCell(0, ValidationFields.Count + 1, 'Total', tcaLeftAdjust, [tcoTopBorder]);

  for i := 0 to OptCount - 1 do
    DoTableCell(i + 1, 0, EpiToolProjectValidationOptionTextShort[TEpiToolsProjectValidateOption(i + 1)]);

  for i := 0 to ValidationFields.Count - 1 do
    for j := 0 to OptCount - 1 do
    DoTableCell(j + 1, i + 1, IntToStr(SumTable[TEpiToolsProjectValidateOption(j + 1), i]), tcaCenter);

  for i := 0 to OptCount - 1 do
    DoTableCell(
      i + 1,
      ValidationFields.Count + 1,
      IntToStr(TotalTable[TEpiToolsProjectValidateOption(i + 1)]),
      tcaCenter,
      [tcoTopBorder]);

  DoTableFooter('Counts indicate number of records.');
end;

procedure TEpiReportProjectValidator.DoStudyReport(
  const StudyResult: TEpiProjectStudyArray);
var
  Res: TEpiProjectValidateStudyRecord;
begin
  if Length(StudyResult) = 0 then exit;

  DoHeading('Study Information:');
  DoLineText('');

  DoLineText('No study information for:');
  for Res in StudyResult do
    DoLineText('  ' + Res.StudyObjectName);

  DoLineText('');
end;

procedure TEpiReportProjectValidator.DoRecordsReport(
  const RecordResult: TEpiProjectResultArray);
var
  S: String;
  i: Integer;
  j: Integer;
  ResRecord: TEpiProjectValidateResultRecord;
  Jmp: TEpiJump;
begin
  i := Low(RecordResult);
  while i <= High(RecordResult) do
  begin
    DoHeading('Record no: ' + IntToStr(RecordResult[i].RecNo + 1));

    if Assigned(KeyFields) and
       (KeyFields.Count > 0)
    then
      begin
        S := 'Key Fields:';

        for j := 0 to KeyFields.Count -1 do
          S += '  ' + KeyFields[j].Name + ' = ' + KeyFields[j].AsString[RecordResult[i].RecNo];

        DoLineText(S);
      end;

    repeat
      ResRecord := RecordResult[i];

      S := ResRecord.Field.Name + ': ';
      case ResRecord.FailedCheck of
        pvCheckSystemMissing:
          S += 'System Missing';
        pvCheckMustEnter:
          S += 'Must Enter has system missing';
        pvCheckKeyFields:
          S += 'Key Field has system missing';
        pvCheckDataRange:
          with ResRecord do
          begin
            S += 'Value = ' + Field.AsString[RecNo] + ', ';
            if Assigned(Field.Ranges) and
               (not Field.Ranges.InRange(Field.AsValue[RecNo]))
            then
              S += Format('Not in range = (%s, %s) ',
                [Field.Ranges[0].AsString[true],
                 Field.Ranges[0].AsString[false]]
              );

            if Assigned(Field.ValueLabelSet) and
               (not Field.ValueLabelSet.ValueLabelExists[Field.AsValue[RecNo]])
            then
              S += 'Not a valid value label!'
          end;
        pvCheckComparison:
          with ResRecord.Field do
            S += Format('Comparison: %s=%s %s %s=%s',
                        [Name, AsString[ResRecord.RecNo],
                         ComparisonTypeToString(Comparison.CompareType),
                         Comparison.CompareField.Name, Comparison.CompareField.AsString[ResRecord.RecNo]
                        ]);
        pvCheckDataLength:
          with ResRecord.Field do
            S += Format('Field length = %d, Data length: %d',
                        [Length, UTF8Length(AsString[ResRecord.RecNo])]);
        pvCheckJumpReset:
          // Either a reset value is NOT missing OR a jump back in flow.
          with ResRecord.Field do
          begin
            Jmp := Jumps.JumpFromValue[AsString[ResRecord.RecNo]];
            if not (Jmp.ResetType in [jrMaxMissing, jr2ndMissing]) then
              S += Format('Value = %s, is not a valid jump reset value (MaxMissing or 2ndMaxMissing)!', [AsString[ResRecord.RecNo]])
            else
              S += Format('Value = %s, is a jump backward in flow!', [AsString[ResRecord.RecNo]]);
          end;
      else
          S += Format('Report not implemented for ToolCheck: %s',
                      [GetEnumName(TypeInfo(TEpiToolsProjectValidateOption), Integer(ResRecord.FailedCheck))]);
      end;
      DoLineText(S);
      Inc(i);
    until (i > High(RecordResult)) or (RecordResult[i].RecNo <> ResRecord.RecNo);
    DoLineText('');
  end;
end;

procedure TEpiReportProjectValidator.DoSanityCheck;
begin
  inherited DoSanityCheck;

  if not Assigned(Document) then
    DoError(EEpiReportBaseException, SEpiReportProjectValidationNoDocument);
end;

constructor TEpiReportProjectValidator.Create(
  ReportGenerator: TEpiReportGeneratorBase);
begin
  inherited Create(ReportGenerator);
  FProjectValidationOptions := EpiProjectValidationOptionsAll;
end;

procedure TEpiReportProjectValidator.RunReport;
var
  RecordResult: TEpiProjectResultArray;
  StudyResult: TEpiProjectStudyArray;
begin
  inherited RunReport;

  RunTool(RecordResult, StudyResult);

  DoReportStart;
  DoLineText('');

  DoSection('Result of Validation:');
  DoReportResultTable(RecordResult, StudyResult);
  DoLineText('');

  DoStudyReport(StudyResult);
  DoLineText('');

  DoRecordsReport(RecordResult);
end;

end.

