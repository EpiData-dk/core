unit epitools_val_dbl_entry;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidatafiles;

type
  TEpiToolsDblEntryValidateOption = (
    devIgnoreDeleted,                  // Ignores if a record i marked for deletion in either Datafile
    devCaseSensitiveText,              // When comparing text, do it case-sensitive
//    devIgnoreMissingRecords,           // Ignore reporting if no matching records is found in dublicate datafile (but still reports differences)
    devAddResultToField                // Add a field to Main Datafile with result (and a matching valuelabel set)
  );
  TEpiToolsDblEntryValidateOptions = set of TEpiToolsDblEntryValidateOption;

const
  EpiDefaultDblEntryValidateOptions = [devIgnoreDeleted, {devIgnoreMissingRecords,} devAddResultToField];

type

  { TEpiToolsDblEntryValidator }

  EEpiDFNotAssigned = class(Exception);
  EEpiFieldsNotAssigned = class(Exception);
  EEpiInvalidCompare = class(Exception);

  TEpiToolsDblEntryValidator = class
  private
    FCmpFields: TEpiFields;
    FMainDF: TEpiDataFile;
    FDuplDF: TEpiDataFile;
    FSortFields: TEpiFields;
    FMainCmpFields: TEpiFields;
    FDuplCmpFields: TEpiFields;
    FValidateOptions: TEpiToolsDblEntryValidateOptions;
    procedure   AddResult(Const Index: integer; Const Value: Integer);
    procedure   ValidateSequencial;
    procedure   ValidateWithSort;
    procedure   RaiseError(ErrorClass: ExceptClass; Const msg: String);
    function    DoCompareFields(Const MIndex, DIndex: Integer): integer;
  public
    constructor Create;
    procedure   ValidateDataFiles(
      out Result: TBoundArray;
      ValidateOptions: TEpiToolsDblEntryValidateOptions = EpiDefaultDblEntryValidateOptions);
    property    MainDF: TEpiDataFile read FMainDF write FMainDF;
    property    DuplDF: TEpiDataFile read FDuplDF write FDuplDF;
    // Sort fields is a list of common fields in which the Datafiles are sorted.
    // The list of fields MUST belong to MainDF.
    property    SortFields: TEpiFields read FSortFields write FSortFields;
    // Compare fields is a list of common fields for which the Datafiles are compared.
    // The list of fields MUST belong to MainDF.
    property    CompareFields: TEpiFields read FCmpFields write FCmpFields;
  end;

implementation

uses
  epidatafilestypes, epidatafileutils, math;

const
  ValOk           = 0;
  ValNoExists     = 1;
  ValTextFail     = 2;
  ValValueFail    = 3;
  ValDupKeyFail   = 4;

  ValTexts: array[ValOK..ValDupKeyFail] of string = (
    'Validated',
    'Record does not exists in duplicate file',
//    'Failed due to different case in text',
    'Failed due to different text',
    'Failed due to different values',
    'Duplicate key exists');

{ TEpiToolsDblEntryValidator }

procedure TEpiToolsDblEntryValidator.AddResult(const Index: integer;
  const Value: Integer);
begin
  //
end;

procedure TEpiToolsDblEntryValidator.ValidateSequencial;
begin

end;

procedure TEpiToolsDblEntryValidator.ValidateWithSort;
var
  MainSortField: TEpiField;
  DuplSortField: TEpiField;
  DuplSortFields: TEpiFields;
  MRunner: Integer;
  DRunner: Integer;
  i: Integer;
  MSize: Integer;
  DSize: Integer;

  function CompareSortFieldRecords(Const FL1, FL2: TEpiFields; Const Idx1, Idx2: Integer): TValueSign;
  var
    i: Integer;
  begin
    for i := 0 to FL1.Count - 1 do
    begin
      CompareFieldRecords(Result, FL1[i], FL2[i], Idx1, Idx2);
      if Result <> ZeroValue then Exit;
    end;
  end;

begin
  MainSortField := nil;
  DuplSortField := nil;
  DuplSortFields := nil;

  try
    // Create temporary fields to preserver sorting.
    MainSortField := MainDF.NewField(ftInteger);
    for i := 0 to MainDF.Size - 1 do
      MainSortField.AsInteger[i] := i;

    DuplSortField := DuplDF.NewField(ftInteger);
    for i := 0 to DuplDF.Size - 1 do
      DuplSortField.AsInteger[i] := i;

    // Create a copy of the list of field to sort.
    DuplSortFields := TEpiFields.Create(nil);
    for i := 0 to SortFields.Count - 1 do
      DuplSortFields.AddItem(DuplDF.Fields.FieldByName[SortFields.Field[i].Name]);

    // Now sort both DF's before we are going to compare.
    MainDF.SortRecords(SortFields);
    DuplDF.SortRecords(DuplSortFields);

    // Do the validation!
    MRunner := 0;
    MSize   := MainDF.Size;
    DRunner := 0;
    DSize   := DuplDF.Size;
    while (MRunner < MSize) and (DRunner < DSize) do
    begin
      if (MRunner > 0) and
         (CompareSortFieldRecords(SortFields, SortFields, MRunner, MRunner - 1) = ZeroValue) then
      begin
        AddResult(MRunner, ValDupKeyFail);
        Inc(MRunner);
        Continue;
      end;

      case CompareSortFieldRecords(SortFields, DuplSortFields, MRunner, DRunner) of
        NegativeValue:
          begin
            AddResult(MRunner, ValNoExists);
            Inc(MRunner);
          end;
        ZeroValue:
          begin
            DoCompareFields(MRunner, DRunner);
            Inc(MRunner);
            Inc(DRunner);
          end;
        PositiveValue:
          begin
            // AddResult, for extra records in duplicate file.
            Inc(DRunner);
          end;
      end;
    end;

  finally
    // Restore sorting on both DF's
    if Assigned(MainSortField) then
      MainDF.SortRecords(MainSortField);
    if Assigned(DuplSortField) then
      DuplDF.SortRecords(DuplSortField);

    MainSortField.Free;
    DuplSortField.Free;
    DuplSortFields.Free;
  end;
end;

procedure TEpiToolsDblEntryValidator.RaiseError(ErrorClass: ExceptClass;
  const msg: String);
begin
  raise ErrorClass.Create(Msg);
end;

function TEpiToolsDblEntryValidator.DoCompareFields(const MIndex,
  DIndex: Integer): integer;
var
  i: Integer;
  CmpResult: TValueSign;
begin
  for i := 0 to CompareFields.Count - 1 do
  begin
    if not CompareFieldRecords(CmpResult, FCmpFields[i], FDuplCmpFields[i], MIndex, DIndex) then
      RaiseError(EEpiInvalidCompare, 'Comparison failed due to different field types!');

    if CmpResult = ZeroValue then continue;

    if FCmpFields[i].FieldType in StringFieldTypes then
      AddResult(MIndex, ValTextFail)
    else
      AddResult(MIndex, ValValueFail);
    Break;
  end;
end;

constructor TEpiToolsDblEntryValidator.Create;
begin

end;

procedure TEpiToolsDblEntryValidator.ValidateDataFiles(out Result: TBoundArray;
  ValidateOptions: TEpiToolsDblEntryValidateOptions);
var
  i: Integer;
begin
  if (not Assigned(MainDF)) or (not Assigned(DuplDF)) then
    RaiseError(EEpiDFNotAssigned, 'Main or Duplicate datafile not assigned');

  if (not Assigned(CompareFields)) then
    RaiseError(EEpiFieldsNotAssigned, 'Compare fields not assigned');

  // Make duplicate list of fields to compare.
  FDuplCmpFields := TEpiFields.Create(nil);
  for i := 0 to CompareFields.Count - 1 do
    FDuplCmpFields.AddItem(FDuplDF.Fields.FieldByName[CompareFields.Field[i].Name]);

  FValidateOPtions := ValidateOptions;

  if Assigned(SortFields) then
    ValidateWithSort
  else
    ValidateSequencial;
end;

end.

