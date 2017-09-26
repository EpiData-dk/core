unit epitools_projectvalidate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, epidocument, epidatafiles, epidatafilerelations;

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
    pvCheckKeyData,                   // Check keyfields for unique data.
    pvCheckRelateData,                // Check data in childDF has a valid record in parentDF

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
     'Check keyfields for unique data',
     'Check Child/Parent data',
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
     'Unique',
     'Relate',
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

  TEpiProjectValidationGetFieldsEvent = function(
    Const Sender: TObject;
    Const Relation: TEpiMasterRelation): TStrings of object;
  TEpiProjectValidationDataFileResultEvent = procedure(
    Const Sender: TObject;
    Const Relation: TEpiMasterRelation;
    Const ResultArray: TEpiProjectResultArray) of object;
  TEpiProjectValidationStudyResultEvent = procedure(
    Const Sender: TObject;
    Const ResultArray: TEpiProjectStudyArray) of object;

  TEpiProjectValidationTool = class
  private
    FDocument: TEpiDocument;
    FKeyFields: TEpiFields;
    FResultArray: TEpiProjectResultArray;
    FStudyArray: TEpiProjectStudyArray;
    FValidationFields: TEpiFields;
    FOptions: TEpiToolsProjectValidateOptions;
    function    NewResultRecord: PEpiProjectResultArray;
    procedure   NewStudyRecord(Const AObject: Pointer;
      Const AName: string);
    procedure DoValidateDataFile(const Relation: TEpiMasterRelation;
      const Depth: Cardinal; const Index: Cardinal; var aContinue: boolean;
      Data: Pointer = nil);
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   ValidateProject(Const Doc: TEpiDocument;
      Options: TEpiToolsProjectValidateOptions = EpiProjectValidationOptionsAll);
    procedure   ValidateDatafile(Const Relation: TEpiMasterRelation;
      Options: TEpiToolsProjectValidateOptions = EpiProjectValidationOptionsAll);

  private
    FOnDataFileResult: TEpiProjectValidationDataFileResultEvent;
    FOnGetSortFields: TEpiProjectValidationGetFieldsEvent;
    FOnGetValidationFields: TEpiProjectValidationGetFieldsEvent;
    FOnStudyResult: TEpiProjectValidationStudyResultEvent;
    function    DoGetSortFields(Const Relation: TEpiMasterRelation): TStrings;
    function    DoGetValidationFields(Const Relation: TEpiMasterRelation): TStrings;
    procedure   DoDataFileResult(Const Relation: TEpiMasterRelation;
      Const ResultArray: TEpiProjectResultArray);
    procedure   DoStudyResult(Const ResultArray: TEpiProjectStudyArray);
  public
    property    OnDataFileResult: TEpiProjectValidationDataFileResultEvent read FOnDataFileResult write FOnDataFileResult;
    Property    OnGetSortFields: TEpiProjectValidationGetFieldsEvent read FOnGetSortFields write FOnGetSortFields;
    property    OnGetValidationFields: TEpiProjectValidationGetFieldsEvent read FOnGetValidationFields write FOnGetValidationFields;
    property    OnStudyResult: TEpiProjectValidationStudyResultEvent read FOnStudyResult write FOnStudyResult;
  end;

implementation

uses
  epidatafilestypes, epidatafileutils, LazUTF8, epidatafilerelations_helper;

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

procedure TEpiProjectValidationTool.DoValidateDataFile(
  const Relation: TEpiMasterRelation; const Depth: Cardinal;
  const Index: Cardinal; var aContinue: boolean; Data: Pointer);
var
  DF, ParentDF: TEpiDataFile;
  LValidationFields: TStrings;
  F: TEpiField;
  MainSortField: TEpiField;
  SortFields: TStrings;
  Fields: TEpiFields;
  i, j, runner: Integer;
  TmpResult, CheckRelatedData: Boolean;
  CmpResult: TValueSign;
  Res: Boolean;
  CompareField, KF: TEpiField;
  S: String;
  Jmp: TEpiJump;

  procedure DoCheckRelatedData(ParentDF, CurrentDF: TEpiDataFile; CurIdx: Integer);
  var
    pKF, cKF: TEpiField;
  begin
    if (Runner >= ParentDF.Size) then
      CmpResult := 1
    else
      for pKF in ParentDF.KeyFields do
      begin

        cKF := CurrentDF.KeyFields.FieldByName[pKF.Name];
        CompareFieldRecords(CmpResult, pKF, cKF, Runner, CurIdx);

        case CmpResult of
          -1: begin
                Inc(Runner);
                DoCheckRelatedData(ParentDF, CurrentDF, CurIdx);
                Exit;
              end;
          0:  Continue;
          1:  Break;
        end;
      end;

    if (CmpResult > 0)  then
      with NewResultRecord^ do
      begin
        RecNo := MainSortField.AsInteger[CurIdx];
        Field := CurrentDF.KeyFields[0];
        FailedCheck := pvCheckRelateData;
      end;
  end;

begin
  DF := Relation.Datafile;

  LValidationFields := DoGetValidationFields(Relation);

  if not Assigned(LValidationFields) then
  begin
    LValidationFields := TStringList.Create;
    for F in DF.Fields do
      LValidationFields.Add(F.Name);
  end;

  MainSortField := nil;
  SortFields := DoGetSortFields(Relation);

  MainSortField := Df.NewField(ftInteger);
  for i := 0 to Df.Size -1 do
    MainSortField.AsInteger[i] := i;

  CheckRelatedData := (pvCheckRelateData in FOptions) and (Depth > 0);
  ParentDF := nil;

  if ([pvCheckKeyData, pvCheckRelateData] * FOptions <> []) and
     (DF.KeyFields.Count > 0) and
     (DF.Size > 0)
  then
    begin
      DF.SortRecords(DF.KeyFields);
      Runner := 0;

      if CheckRelatedData then
        begin
          ParentDF := TEpiDataFile(TEpiDetailRelation(Relation).MasterRelation.Datafile.Clone(nil));
          ParentDF.SortRecords(ParentDF.KeyFields);
          DoCheckRelatedData(ParentDF, Df, 0);
        end;

      for i := 1 to DF.Size - 1 do
      begin;
        if (pvCheckKeyData in FOptions) then
        begin
          TmpResult := true;
          for KF in DF.KeyFields do
            TmpResult := TmpResult and (KF.Compare(i-1, i) = 0);

          if TmpResult then
          with NewResultRecord^ do
            begin
              RecNo := MainSortField.AsInteger[i];
              Field := DF.KeyFields[0];
              FailedCheck := pvCheckKeyData;
            end;
        end;

        if CheckRelatedData then
          DoCheckRelatedData(ParentDF, DF, i);
      end;
    end;


  if Assigned(SortFields) then
  begin
    Fields := TEpiFields.Create(nil);

    for i := 0 to SortFields.Count -1 do
    begin
      F := DF.Fields.FieldByName[SortFields[i]];
      Fields.AddItem(F);
    end;

    if (Fields.Count > 0)
    then
      Df.SortRecords(Fields);

    Fields.Free;
  end;

  for i := 0 to Df.Size - 1 do
  begin
    if Df.Deleted[i] and (pvIgnoreDeleted in FOptions) then continue;

    for j := 0 to LValidationFields.Count - 1 do
    begin
      S := LValidationFields[j];
      F := DF.Fields.FieldByName[S];

      if (pvCheckSystemMissing in FOptions) then
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

      // Check that MUST ENTER fields are filled
      if (pvCheckMustEnter in FOptions) then
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

      // Check that Fields in Key all have data.
      if (pvCheckKeyFields in FOptions) then
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

      // Check that fields with Range / Valuelabel are correct
      if (pvCheckDataRange in FOptions) and
         ((Assigned(F.Ranges)) or
          (Assigned(F.ValueLabelSet))
         )
      then
      begin
        TmpResult := false;

        if Assigned(F.Ranges) then
          TmpResult := F.Ranges.InRange(F.AsValue[i]);

        if Assigned(F.ValueLabelSet) then
          if (F.IsMissing[i]) then
            TmpResult := true
          else
            TmpResult := TmpResult or
                         F.ValueLabelSet.ValueLabelExists[F.AsValue[i]];

        if not TmpResult
        then
          with NewResultRecord^ do
          begin
            RecNo := MainSortField.AsInteger[i];
            Field := F;
            FailedCheck := pvCheckDataRange;
          end;
      end;

      if (pvCheckComparison in FOptions) and
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

      if (pvCheckDataLength in FOptions) then
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

      if (pvCheckJumpReset in FOptions) and
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

  if Assigned(SortFields) then
    Df.SortRecords(MainSortField);

  MainSortField.Free;

  // No "real" changes were made...
  if Assigned(Df.RootOwner) then
    DF.RootOwner.Modified := false;

  DoDataFileResult(Relation, FResultArray);
end;

constructor TEpiProjectValidationTool.Create;
begin
  //
end;

destructor TEpiProjectValidationTool.Destroy;
begin
  inherited Destroy;
end;

procedure TEpiProjectValidationTool.ValidateProject(const Doc: TEpiDocument;
  Options: TEpiToolsProjectValidateOptions);
begin
  FOptions := Options;

  if (pvCheckStudyInfo in FOptions) then
  with Doc.Study do
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

    DoStudyResult(FStudyArray);
  end;

  Doc.Relations.OrderedWalk(@ValidateDataFile);
end;

procedure TEpiProjectValidationTool.ValidateDatafile(
  const Relation: TEpiMasterRelation; Options: TEpiToolsProjectValidateOptions);
var
  Depth: Integer;
  AContinue: Boolean;
begin
  Depth := 0;
  if (Relation is TEpiDetailRelation) then
    Depth := 1;

  AContinue := true;
  DoValidateDataFile(Relation, Depth, 0, AContinue, nil);
end;

function TEpiProjectValidationTool.DoGetSortFields(
  const Relation: TEpiMasterRelation): TStrings;
begin
  Result := nil;
  if Assigned(OnGetSortFields) then
    Result := OnGetSortFields(Self, Relation);
end;

function TEpiProjectValidationTool.DoGetValidationFields(
  const Relation: TEpiMasterRelation): TStrings;
begin
  Result := nil;
  if Assigned(OnGetValidationFields) then
    Result := OnGetValidationFields(Self, Relation);
end;

procedure TEpiProjectValidationTool.DoDataFileResult(
  const Relation: TEpiMasterRelation; const ResultArray: TEpiProjectResultArray
  );
  procedure SortResultArray(L, R: Integer);
  var
    I, J, P: Integer;
    TmpRec: TEpiProjectValidateResultRecord;
  begin
    I:=L;
    J:=R;
    P:=(L + R) shr 1;
    repeat
      while ResultArray[I].RecNo < ResultArray[P].RecNo do Inc(I);
      while ResultArray[J].RecNo > ResultArray[P].RecNo do Dec(J);

      if I <= J then
      begin
        TmpRec := ResultArray[I];
        ResultArray[I] := ResultArray[J];
        ResultArray[J] := TmpRec;
        if P = I then
          P := J
        else
          if P = J then
            P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then SortResultArray(L, J);
    if I < R then SortResultArray(I, R);
  end;

begin
  if Assigned(OnDataFileResult) then
  begin
    if Length(ResultArray) > 1 then
      SortResultArray(0, Length(ResultArray) - 1);
    OnDataFileResult(Self, Relation, ResultArray);
  end;
end;

procedure TEpiProjectValidationTool.DoStudyResult(
  const ResultArray: TEpiProjectStudyArray);
begin
  if Assigned(OnStudyResult) then
    OnStudyResult(Self, ResultArray);
end;

end.

