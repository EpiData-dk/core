unit epitools_val_dbl_entry;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidatafiles, epivaluelabels;

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

const
  ValInDuplDF     = -1;
  ValOk           = 0;
  ValNoExists     = 1;
  ValTextFail     = 2;
  ValValueFail    = 3;
  ValDupKeyFail   = 4;

type
  TEpiDblEntryResultRecord = record
    ValResult: Integer;
    MRecNo: Integer;
    DRecNo: Integer;
    KFValues: Array of String;
    CmpFieldNames: Array of string;
    MCmpFieldVals: Array of string;
    DCmpFieldVals: Array of string;
  end;
  PEpiDblEntryResultRecord = ^TEpiDblEntryResultRecord;
  TEpiDblEntryResultArray = array of TEpiDblEntryResultRecord;


  EEpiDFNotAssigned = class(Exception);
  EEpiFieldsNotAssigned = class(Exception);
  EEpiInvalidCompare = class(Exception);

  { TEpiToolsDblEntryValidator }

  TEpiToolsDblEntryValidator = class
  { Common }
  private
    FCmpFields: TEpiFields;
    FMainDF: TEpiDataFile;
    FDuplDF: TEpiDataFile;
    FMainCmpFields: TEpiFields;
    FDuplCmpFields: TEpiFields;
    FResultField:   TEpiField;
    FResultArray:   TEpiDblEntryResultArray;
    FExtraDuplRecords:   TBoundArray;
    FValidateOptions: TEpiToolsDblEntryValidateOptions;
    procedure   ValidateSequencial;
    procedure   RaiseError(ErrorClass: ExceptClass; Const msg: String);

  { KeyFields Validation }
  private
    FSortFields: TEpiFields;
    MainSortField: TEpiField;
    DuplSortField: TEpiField;
    procedure   ValidateWithSort;
    function    NewResultRecord: PEpiDblEntryResultRecord;
    procedure   AddResult(Const SortedRecNo: integer; Const Value: Integer);
    function    DoCompareFields(Const MIndex, DIndex: Integer): integer;
  public
    constructor Create;
    procedure   ValidateDataFiles(
      out ResultArray: TEpiDblEntryResultArray;
      out ExtraDuplRecords: TBoundArray;
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
  epidatafilestypes, epidatafileutils, math, epiglobals;

const
  ValTexts: array[ValOK..ValDupKeyFail] of string = (
    'Validated',
    'Record does not exists in duplicate file',
//    'Failed due to different case in text',
    'Failed due to different text',
    'Failed due to different values',
    'Duplicate key exists');

{ TEpiToolsDblEntryValidator }

procedure TEpiToolsDblEntryValidator.AddResult(const SortedRecNo: integer;
  const Value: Integer);
var
  L: Integer;
  i: Integer;
begin
  if Value = ValInDuplDF then
  begin
    // Result indicate that a record was found in duplicate DF that did not exist in Main DF.
    L := Length(FExtraDuplRecords);
    Inc(L);
    SetLength(FExtraDuplRecords, L);
    FExtraDuplRecords[L-1] := SortedRecNo;
    Exit;
  end;

  if Assigned(FResultField) then
    FResultField.AsInteger[SortedRecNo] := Value;

  if Value = ValOk then exit;

  with NewResultRecord^ do
  begin
    ValResult := Value;

    MRecNo := MainSortField.AsInteger[SortedRecNo];
    DRecNo := DuplSortField.AsBoolean[SortedRecNo];
    SetLength(KFValues, FSortFields.Count);
    for i := 0 to FSortFields.Count - 1 do
      KFValues[i] := FSortFields[i].AsString[SortedRecNo];
  end;
end;

procedure TEpiToolsDblEntryValidator.ValidateSequencial;
begin

end;

procedure TEpiToolsDblEntryValidator.ValidateWithSort;
var
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
            // Record does not exists in duplicate file
            AddResult(MRunner, ValNoExists);
            Inc(MRunner);
          end;
        ZeroValue:
          begin
            // Record exists in both files - compare fields.
            DoCompareFields(MRunner, DRunner);
            Inc(MRunner);
            Inc(DRunner);
          end;
        PositiveValue:
          begin
            // Record does not exists in main file
            AddResult(DuplSortField.AsInteger[DRunner], ValInDuplDF);
            Inc(DRunner);
          end;
      end;
    end;

    while MRunner < MSize do
    begin
      // If MRunner < MSize, then records exists in Main DF with no matching
      // records in Dupl. DF. Hence mark them as NoExists.
      AddResult(MRunner, ValNoExists);
      Inc(MRunner);
    end;

    while DRunner < DSize do
    begin
      // If DRunner < DSize, then records exists in Dupl. DF with no matching
      // records in Main DF. Hence mark them as InDulpDF
      AddResult(DuplSortField.AsInteger[DRunner], ValInDuplDF);
      Inc(DRunner);
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

function TEpiToolsDblEntryValidator.NewResultRecord: PEpiDblEntryResultRecord;
var
  L: Integer;
begin
  L := Length(FResultArray);
  Inc(L);
  SetLength(FResultArray, L);
  with FResultArray[L-1] do
  begin
    CmpFieldNames := nil;
    MCmpFieldVals := nil;
    DCmpFieldVals := nil;
  end;
  Result := @FResultArray[L-1];
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
  ValResult: Integer;
  ResultRecord: PEpiDblEntryResultRecord;
  j: Integer;
  L: Integer;

begin
  ResultRecord := nil;
  for i := 0 to CompareFields.Count - 1 do
  begin
    if not CompareFieldRecords(CmpResult, FCmpFields[i], FDuplCmpFields[i], MIndex, DIndex) then
      RaiseError(EEpiInvalidCompare, 'Comparison failed due to different field types!');

    if CmpResult = ZeroValue then
    begin
      AddResult(MIndex, ValOk);
      continue;
    end;

    if FCmpFields[i].FieldType in StringFieldTypes then
      ValResult := ValTextFail
    else
      ValResult := ValValueFail;

    if not Assigned(ResultRecord) then
    begin
      ResultRecord := NewResultRecord;
      with ResultRecord^ do
      begin
        ValResult := ValResult;

        MRecNo := MainSortField.AsInteger[MIndex];
        DRecNo := DuplSortField.AsBoolean[DIndex];
        SetLength(KFValues, FSortFields.Count);
        for j := 0 to FSortFields.Count - 1 do
          KFValues[j] := FSortFields[j].AsString[MIndex];
      end;
    end;

    with ResultRecord^ do
    begin
      L := Length(CmpFieldNames);
      Inc(L);
      SetLength(CmpFieldNames, L);
      CmpFieldNames[L-1] := FCmpFields[i].Name;

      L := Length(MCmpFieldVals);
      Inc(L);
      SetLength(MCmpFieldVals, L);
      MCmpFieldVals[L-1] := FCmpFields[i].AsString[MIndex];

      L := Length(DCmpFieldVals);
      Inc(L);
      SetLength(DCmpFieldVals, L);
      DCmpFieldVals[L-1] := FDuplCmpFields[i].AsString[DIndex];
    end;
  end;
end;

constructor TEpiToolsDblEntryValidator.Create;
begin

end;

procedure TEpiToolsDblEntryValidator.ValidateDataFiles(out
  ResultArray: TEpiDblEntryResultArray; out ExtraDuplRecords: TBoundArray;
  ValidateOptions: TEpiToolsDblEntryValidateOptions);
var
  i: Integer;
  F: TEpiField;
  V: TEpiValueLabelSet;
  VL: TEpiIntValueLabel;
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
  if devAddResultToField in FValidateOptions then
  begin
    FResultField := FMainDF.Fields.FieldByName[EpiDoubleEntryFieldName];
    if not Assigned(FResultField) then
    begin
      FResultField := FMainDF.NewField(ftInteger);
      FResultField.Name := EpiDoubleEntryFieldName;
      FResultField.Length := 1;
    end;

    if not Assigned(FResultField.ValueLabelSet) then
    begin
      V := FMainDF.ValueLabels.GetValueLabelSetByName(EpiDoubleEntryValueLabelSetName);
      if not Assigned(V) then
      begin
        V := FMainDF.ValueLabels.NewValueLabelSet(ftInteger);
        V.Name := EpiDoubleEntryValueLabelSetName;

        VL := TEpiIntValueLabel(V.NewValueLabel);
        VL.Value := ValOk;
        VL.TheLabel.Text := ValTexts[ValOk];

        VL := TEpiIntValueLabel(V.NewValueLabel);
        VL.Value := ValNoExists;
        VL.TheLabel.Text := ValTexts[ValNoExists];

        VL := TEpiIntValueLabel(V.NewValueLabel);
        VL.Value := ValTextFail;
        VL.TheLabel.Text := ValTexts[ValTextFail];

        VL := TEpiIntValueLabel(V.NewValueLabel);
        VL.Value := ValValueFail;
        VL.TheLabel.Text := ValTexts[ValValueFail];

        VL := TEpiIntValueLabel(V.NewValueLabel);
        VL.Value := ValDupKeyFail;
        VL.TheLabel.Text := ValTexts[ValDupKeyFail];
      end;
      FResultField.ValueLabelSet := V;
    end;
  end;

  if Assigned(SortFields) then
    ValidateWithSort
  else
    ValidateSequencial;

  if Length(FExtraDuplRecords) > 0 then
    ExtraDuplRecords := FExtraDuplRecords;
  ResultArray := FResultArray;
end;

end.

