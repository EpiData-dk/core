unit epireport_report_projectvalidator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_base, epidocument, epidatafiles,
  epireport_generator_base, epirelations,
  epitools_projectvalidate;

type

  { TEpiReportProjectValidateOption }

  TEpiReportProjectValidateOption = record
    Document: TEpiDocument;
    Options:  TEpiToolsProjectValidateOptions;
    FieldLists: array of
      record
        Relation: TEpiMasterRelation;
        SortFields: TStrings;
        CompareFields: TStrings;
      end;
  end;

  { TEpiReportProjectValidator }

  TEpiReportProjectValidator = class(TEpiReportBase)
  private
    FDocument: TEpiDocument;
    FKeyFields: TEpiFields;
    FOption: TEpiReportProjectValidateOption;
    FProjectValidationOptions: TEpiToolsProjectValidateOptions;
    FValidationFields: TEpiFields;
    procedure ToolDataFileResult(const Sender: TObject;
      const Relation: TEpiMasterRelation;
      const ResultArray: TEpiProjectResultArray);
    function ToolSortFields(const Sender: TObject;
      const Relation: TEpiMasterRelation): TStrings;
    procedure ToolStudyResult(const Sender: TObject;
      const ResultArray: TEpiProjectStudyArray);
    function ToolValidateFields(const Sender: TObject;
      const Relation: TEpiMasterRelation): TStrings;

    procedure RunTool();

  private
    procedure AssignCustomData();
    function SortFieldsFromRelation(Const Relation: TEpiMasterRelation): TStrings;
    function CompareFieldsFromRelation(Const Relation: TEpiMasterRelation): TStrings;

    procedure DoReportStart;
    procedure DoReportResultTable(Const RecordArray: TEpiProjectResultArray;
      Const StudyArray: TEpiProjectStudyArray);
    procedure DoRecordsReport(Const RecordResult: TEpiProjectResultArray);
  protected
    procedure DoSanityCheck; override;
  public
    constructor Create(ReportGenerator: TEpiReportGeneratorBase); override;
    procedure   RunReport; override;
    property    Option: TEpiReportProjectValidateOption read FOption write FOption;
  end;

implementation

uses
  LazUTF8, typinfo, epireport_types, epireport_report_fieldlist,
  epidatafilestypes;

resourcestring
  SEpiReportProjectValidationNoOption = 'EpiReport: No Option assigned to Project Validation';

const
  SORT_FIELDS_KEY = 'SORT_FIELDS_KEY';
  COMPARE_FIELDS_KEY = 'COMPARE_FIELDS_KEY';

{ TEpiReportProjectValidator }

procedure TEpiReportProjectValidator.ToolDataFileResult(const Sender: TObject;
  const Relation: TEpiMasterRelation; const ResultArray: TEpiProjectResultArray
  );
var
  SortFields: TStrings;
  FName: String;
  ValidationFields: TStrings;
  Fields: TEpiFields;
  S: String;
  R: TEpiReportFieldList;
begin
  DoSection('DataForm: ' + Relation.Datafile.Caption.Text);

  SortFields := SortFieldsFromRelation(Relation);

  if Assigned(SortFields) and
     (SortFields.Count > 0)
  then
    begin
      DoLineText('');
      DoHeading('Sort Fields:');
      S := '';
      for FName in SortFields do
        S := S + FName + ' ';
      DoLineText(S);
    end;

  DoLineText('');

  ValidationFields := CompareFieldsFromRelation(Relation);
  Fields := TEpiFields.Create(nil);

  for FName in ValidationFields do
    Fields.AddItem(Relation.Datafile.Fields.FieldByName[FName]);

  R := TEpiReportFieldList.Create(FReportGenerator);
  R.Fields := Fields;
  R.TableHeader := 'Validation Fields:';
  R.RunReport;
  R.Free;

  Fields.Free;
end;

function TEpiReportProjectValidator.ToolSortFields(const Sender: TObject;
  const Relation: TEpiMasterRelation): TStrings;
begin
  Result := SortFieldsFromRelation(Relation);
end;

procedure TEpiReportProjectValidator.ToolStudyResult(const Sender: TObject;
  const ResultArray: TEpiProjectStudyArray);
var
  Res: TEpiProjectValidateStudyRecord;
begin
  if Length(ResultArray) = 0 then exit;

  DoHeading('Study Information:');
  DoLineText('');

  DoLineText('No study information for:');
  for Res in ResultArray do
    DoLineText('  ' + Res.StudyObjectName);

  DoLineText('');
end;

function TEpiReportProjectValidator.ToolValidateFields(const Sender: TObject;
  const Relation: TEpiMasterRelation): TStrings;
begin
  Result := CompareFieldsFromRelation(Relation);
end;

procedure TEpiReportProjectValidator.RunTool;
var
  T: TEpiProjectValidationTool;
begin
  T := TEpiProjectValidationTool.Create;
  T.OnDataFileResult      := @ToolDataFileResult;
  T.OnGetSortFields       := @ToolSortFields;
  T.OnGetValidationFields := @ToolValidateFields;
  T.OnStudyResult         := @ToolStudyResult;
  T.ValidateProject(Option.Document, Option.Options);
  T.Free;
end;

procedure TEpiReportProjectValidator.AssignCustomData;
var
  i: Integer;
begin
  for i := 0 to Length(Option.FieldLists) -1 do
  with Option.FieldLists[i] do
  begin
    Relation.AddCustomData(SORT_FIELDS_KEY, SortFields);
    Relation.AddCustomData(COMPARE_FIELDS_KEY, CompareFields);
  end;
end;

function TEpiReportProjectValidator.SortFieldsFromRelation(
  const Relation: TEpiMasterRelation): TStrings;
begin
  result := TStrings(Relation.FindCustomData(SORT_FIELDS_KEY));
end;

function TEpiReportProjectValidator.CompareFieldsFromRelation(
  const Relation: TEpiMasterRelation): TStrings;
begin
  result := TStrings(Relation.FindCustomData(COMPARE_FIELDS_KEY));
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
  DoHeading('All records have the following checks performed:');

  for Opt in TEpiToolsProjectValidateOptions do
  begin
    if not (Opt in EpiProjectValidationOptionsSelectable) then
      DoLineText(EpiToolProjectValidationOptionText[Opt]);
  end;

  DoLineText('');

  I := 1;
  for Opt in EpiProjectValidationOptionsSelectable do
    Inc(I);

  DoTableHeader('Selectable Options:', 2, I);
  DoTableCell(0, 0, 'Option');
  DoTableCell(1, 0, 'Selected');

  I := 1;
  for Opt in EpiProjectValidationOptionsSelectable do
  begin
    DoTableCell(0, I, EpiToolProjectValidationOptionText[Opt]);
    DoTableCell(1, I, BoolToStr(Opt in Option.Options, 'Yes', 'No'));
    Inc(I);
  end;
  DoTableFooter('');
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

{  function CalcErrorPct: Extended;
  begin
    Result := RecordErrorCount / RecordCount;
  end;

  function CalcErrorFieldPct: Extended;
  begin
    Result := FieldErrorCount / (RecordCount * ValidationFields.Count);
  end;                  }

begin
  {RecordErrorCount := 0;
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

  DoTableFooter('Counts indicate number of errors.');  }
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
end;

procedure TEpiReportProjectValidator.DoSanityCheck;
begin
  inherited DoSanityCheck;

//  if not Assigned(Option) then
//    DoError(EEpiReportBaseException, SEpiReportProjectValidationNoOption);
end;

constructor TEpiReportProjectValidator.Create(
  ReportGenerator: TEpiReportGeneratorBase);
begin
  inherited Create(ReportGenerator);
  FProjectValidationOptions := EpiProjectValidationOptionsAll;
end;

procedure TEpiReportProjectValidator.RunReport;
begin
  inherited RunReport;

  AssignCustomData();

  DoReportStart;
  DoLineText('');

  RunTool();
end;

end.

