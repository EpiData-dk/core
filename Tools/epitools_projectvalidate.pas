unit epitools_projectvalidate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, epidocument, epidatafiles;

type
  TEpiToolsProjectValidateOption = (
    pvIgnoreDeleted,                  // Ignores if a record i marked for deletion
    pvCheckSystemMissing,             // Check data if it is system missing
    pvCheckMustEnter,                 // Check data for must enter
    pvCheckDataRange,                 // Check data for range and valuelabel
    pvCheckComparison,                // Check data for compared value
    pvCheckDataLength,                // Check data for valid length
    pvCheckJumpReset,                 // Check data for jumps with reset value no being (2nd)max missing.
    pvCheckKeyFields,                 // Check keyfield for missing data
//    pvCheckKeyData,                   // Check keyfields for unique data.

    // StudyInfo should always be last - used in report.
    pvCheckStudyInfo                  // Check study information for completeness
  );
  TEpiToolsProjectValidateOptions = set of TEpiToolsProjectValidateOption;

const
  EpiProjectValidationOptionsAll =
    [pvIgnoreDeleted..pvCheckStudyInfo];

  EpiProjectValidationOptionsSelectable =
    [pvIgnoreDeleted, pvCheckSystemMissing, pvCheckJumpReset, pvCheckStudyInfo];

  EpiToolProjectValidationOptionText: Array[TEpiToolsProjectValidateOption] of string =
    ('Ignore records marked for deletion',
     'Check data if it has system missing',
     'Check data for must enter',
     'Check data for range and/or valid valuelabel',
     'Check data for compared value',
     'Check data for valid length',
     'Check data for jumps with no reset',
     'Check keyfields for missing data',
//     'Check keyfields for unique data',
     'Check study information for completeness'
    );

  EpiToolProjectValidationOptionTextShort: Array[TEpiToolsProjectValidateOption] of string =
    ('Del rec.',
     'Sys.mis.',
     'Must Enter',
     'Range/VL',
     'Compare',
     'Length',
     'Jumps',
     'KeyField',
//     'Unique',
     'Study'
    );
type

  TEpiProjectValidateResultRecord = record
    RecNo: Integer;                               // Record where something failed.
    Field: TEpiField;                             // Reference to field that failed
    FailedCheck: TEpiToolsProjectValidateOption;  // Indicates which check that failed.
  end;
  PEpiProjectResultArray = ^TEpiProjectValidateResultRecord;
  // NOTE: Consecutive records may have same RecNo, if the same record have more than one failed field.
  TEpiProjectResultArray = array of TEpiProjectValidateResultRecord;


  TEpiProjectValidateStudyRecord = record
    StudyObject: Pointer;
    StudyObjectName: String;
  end;
  TEpiProjectStudyArray = array of TEpiProjectValidateStudyRecord;

  { TEpiProjectValidationTool }

  TEpiProjectValidationTool = class
  private
    FDocument: TEpiDocument;
    FKeyFields: TEpiFields;
    FResultArray: TEpiProjectResultArray;
    FStudyArray: TEpiProjectStudyArray;
    FValidationFields: TEpiFields;
    function    NewResultRecord: PEpiProjectResultArray;
    procedure   NewStudyRecord(Const AObject: Pointer;
      Const AName: string);
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   ValidateProject(out FieldResultArray: TEpiProjectResultArray;
      Out StudyResultArray: TEpiProjectStudyArray;
      Options: TEpiToolsProjectValidateOptions = EpiProjectValidationOptionsAll);
    property    Document: TEpiDocument read FDocument write FDocument;
    property    ValidationFields: TEpiFields read FValidationFields write FValidationFields;
    property    KeyFields: TEpiFields read FKeyFields write FKeyFields;
  end;

implementation

uses
  epidatafilestypes, epidatafileutils, LazUTF8;

{ TEpiProjectValidationTool }

function TEpiProjectValidationTool.NewResultRecord: PEpiProjectResultArray;
var
  L: Integer;
begin
  L := Length(FResultArray);
  Inc(L);
  SetLength(FResultArray, L);
  Result := @FResultArray[L-1];
end;

procedure TEpiProjectValidationTool.NewStudyRecord(const AObject: Pointer;
  const AName: string);
var
  L: Integer;
begin
  L := Length(FStudyArray);
  Inc(L);
  SetLength(FStudyArray, L);

  with FStudyArray[L-1] do
  begin
    StudyObject := AObject;
    StudyObjectName := AName;
  end;
end;

constructor TEpiProjectValidationTool.Create;
begin
  //
end;

destructor TEpiProjectValidationTool.Destroy;
begin
  inherited Destroy;
end;

procedure TEpiProjectValidationTool.ValidateProject(out
  FieldResultArray: TEpiProjectResultArray; out
  StudyResultArray: TEpiProjectStudyArray;
  Options: TEpiToolsProjectValidateOptions);
var
  i, j: Integer;
  Df: TEpiDataFile;
  F: TEpiField;
  CompareField: TEpiField;
  Res: Boolean;
  CmpResult: TValueSign;
  S: String;
  LValidationFields: TEpiFields;
  MainSortField: TEpiField;
  Jmp: TEpiJump;
  TmpResult: Boolean;
begin
  Df := Document.DataFiles[0];

  if (pvCheckStudyInfo in Options) then
  with Document.Study do
  begin
    if AbstractText.Text = '' then NewStudyRecord(AbstractText, 'Abstract');
    if Author = ''            then NewStudyRecord(@Author,      'Author');
    if Agency = ''            then NewStudyRecord(@Agency,      'Agency');
    if Citations.Text = ''    then NewStudyRecord(Citations,    'Citations');
    if (DataCollectionStart = MaxDateTime) or
       (DataCollectionEnd = MaxDateTime)
    then
      NewStudyRecord(nil, 'Data Time Coverage');
    if Design.Text = ''       then NewStudyRecord(Design,       'Design');
    if Funding.Text = ''      then NewStudyRecord(Funding,      'Funding');
    if GeographicalCoverage.Text = '' then NewStudyRecord(GeographicalCoverage, 'Geographical Coverage');
    if Publisher.Text = ''    then NewStudyRecord(Publisher,    'Publisher');
    if Purpose.Text = ''      then NewStudyRecord(Purpose,      'Purpose');
    if Population.Text = ''   then NewStudyRecord(Population,   'Population');
    if Rights.Text = ''       then NewStudyRecord(Rights,       'Rights');
    if Title.Text = ''        then NewStudyRecord(Title,        'Title');
    if Keywords = ''          then NewStudyRecord(@Keywords,    'Keywords');
    if UnitOfObservation.Text = '' then NewStudyRecord(UnitOfObservation, 'Unit of obs.');
  end;

  if Assigned(ValidationFields) and
     (ValidationFields.Count > 0)
  then
    LValidationFields := ValidationFields
  else
    LValidationFields := DF.Fields;

  MainSortField := Df.NewField(ftInteger);
  for i := 0 to Df.Size -1 do
    MainSortField.AsInteger[i] := i;

  if Assigned(KeyFields) and
     (KeyFields.Count > 0)
  then
    Df.SortRecords(KeyFields);

  for i := 0 to Df.Size - 1 do
  begin
    if Df.Deleted[i] and (pvIgnoreDeleted in Options) then continue;

    for j := 0 to ValidationFields.Count - 1 do
    begin
      F := ValidationFields[j];

      if (pvCheckSystemMissing in Options) then
      begin
        // Check for system missing

        if F.IsMissing[i] then
        with NewResultRecord^ do
        begin
          RecNo := MainSortField.AsInteger[i];
          Field := F;
          FailedCheck := pvCheckSystemMissing;
        end;
      end;

      if (pvCheckMustEnter in Options) then
      begin
        if (F.EntryMode = emMustEnter) and
           (F.IsMissing[i])
        then
          with NewResultRecord^ do
          begin
            RecNo := MainSortField.AsInteger[i];
            Field := F;
            FailedCheck := pvCheckMustEnter;
          end;
      end;

      if (pvCheckKeyFields in Options) then
      begin
        if (Df.KeyFields.IndexOf(F) > -1) and
           (F.IsMissing[i])
        then
          with NewResultRecord^ do
          begin
            RecNo := MainSortField.AsInteger[i];
            Field := F;
            FailedCheck := pvCheckKeyFields;
          end;
      end;

      if (pvCheckDataRange in Options) and
         ((Assigned(F.Ranges)) or
          (Assigned(F.ValueLabelSet))
         )
      then
      begin
        TmpResult := false;

        if Assigned(F.Ranges) then
          TmpResult := F.Ranges.InRange(F.AsValue[i]);

        if Assigned(F.ValueLabelSet) then
          TmpResult := TmpResult or F.ValueLabelSet.ValueLabelExists[F.AsValue[i]];

        if not TmpResult
        then
          with NewResultRecord^ do
          begin
            RecNo := MainSortField.AsInteger[i];
            Field := F;
            FailedCheck := pvCheckDataRange;
          end;
      end;

      if (pvCheckComparison in Options) and
         (Assigned(F.Comparison))
      then
      begin
        CompareField := F.Comparison.CompareField;

        if CompareFieldTypeOrder(F.FieldType, CompareField.FieldType) <> 0 then
          ;  // Cannot compare fields

        CompareFieldRecords(CmpResult, F, CompareField, i, i, true);

        case F.Comparison.CompareType of
          fcEq:  Res := CmpResult = 0;
          fcNEq: Res := CmpResult <> 0;
          fcLT:  Res := CmpResult < 0;
          fcLEq: Res := CmpResult <= 0;
          fcGEq: Res := CmpResult > 0;
          fcGT:  Res := CmpResult >= 0;
        end;
        if not Res then
        with NewResultRecord^ do
        begin
          RecNo := MainSortField.AsInteger[i];
          Field := F;
          FailedCheck := pvCheckComparison;
        end;
      end;

      if (pvCheckDataLength in Options) then
      begin
        S := F.AsString[i];

        if UTF8Length(S) > F.Length then
        with NewResultRecord^ do
        begin
          RecNo := MainSortField.AsInteger[i];
          Field := F;
          FailedCheck := pvCheckDataLength;
        end;
      end;

      if (pvCheckJumpReset in Options) and
         (Assigned(F.Jumps))
      then
      begin
        Jmp := F.Jumps.JumpFromValue[F.AsString[i]];

        if Assigned(Jmp) and
           (not (Jmp.ResetType in [jrMaxMissing, jr2ndMissing]))
        then
          with NewResultRecord^ do
          begin
            RecNo := MainSortField.AsInteger[i];
            Field := F;
            FailedCheck := pvCheckJumpReset;
          end
      end;
    end;
  end;

  Df.SortRecords(MainSortField);
  MainSortField.Free;

  // No "real" changes were made...

  if Assigned(Df.RootOwner) then
    DF.RootOwner.Modified := false;

  FieldResultArray := FResultArray;
  StudyResultArray := FStudyArray;
end;

end.

