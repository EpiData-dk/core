unit epitools_val_dbl_entry;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidatafiles, epivaluelabels;

type
  TEpiToolsDblEntryValidateOption = (
    devIgnoreDeleted,                  // Ignores if a record i marked for deletion in either Datafile
    devCaseSensitiveText,              // When comparing text, do it case-sensitive
    devIgnoreMissingRecords,           // Ignore reporting if no matching records is found in dublicate datafile (but still reports differences)
    devAddResultToField                // Add a field to Main Datafile with result (and a matching valuelabel set)
  );
  TEpiToolsDblEntryValidateOptions = set of TEpiToolsDblEntryValidateOption;

const
  EpiDefaultDblEntryValidateOptions = [devIgnoreDeleted, {devIgnoreMissingRecords,} devAddResultToField];

type
  TEpiDblEntryRecordResult =
    (
      rrValOk,                    // Compared records are OK - not included in array, internal use!
      rrValNoExistsMain,          // Record not found in main.
      rrValNoExistsDupl,          // Record not found in duplicate file.
      rrValValueFail,             // Compared records failed in a non-text field
      rrValTextFail,              // Compared records failed only in a text field.
      rrValDupKeyMain,            // Duplicate key found in main
      rrValDupKeyDupl             // Duplicate key found in duplicate file.
    );

  {
    TEpiDblEntryResultRecord:
      ValResult:   Result of comparison -> see above.

      MRecNo:      Case ValResult:
                     rrValOk,
                     rrValNoExistsDupl,
                     rrValValueFail,
                     rrValTextFail:
                       Contains the record no. for main datafile.

                     rrValDupKeyMain:
                       Contains the record no. for "original" key.

                     rrValDupKeyDupl:
                       Contains the record no. for the conflicting key.

                     rrValNoExistsMain:
                       The value is always -1

      DRecNo:      Case ValResult:
                     rrValOk,
                     rrValNoExistsMain,
                     rrValValueFail,
                     rrValTextFail:
                       Contains the record no. for duplicate datafile.

                     rrValDupKeyMain:
                       Contains the record no. for the conflicting key.

                     rrValDupKeyDupl:
                       Contains the record no. for "original" key.

                     rrValNoExistsDupl:
                       The value is always -1

     CmpFieldNames:  List of fields names where comparison failed.
                     Only relevant for rrValValueFail, rrValTextFail!

  }
  TEpiDblEntryResultRecord = record
    ValResult: TEpiDblEntryRecordResult;
    MRecNo: Integer;
    DRecNo: Integer;
    CmpFieldNames: Array of string;
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
    FValidateOptions: TEpiToolsDblEntryValidateOptions;
    procedure   RaiseError(ErrorClass: ExceptClass; Const msg: String);

  { KeyFields Validation }
  private
    FDuplKeyFields: TEpiFields;
    FSortFields: TEpiFields;
    MainSortField: TEpiField;
    DuplSortField: TEpiField;
    procedure   InternalValidate;
    function    NewResultRecord: PEpiDblEntryResultRecord;
    function    AddResult(Const MSortedRecNo, DSortedRecNo: integer;
      CompareResult: TEpiDblEntryRecordResult): PEpiDblEntryResultRecord;
    procedure   DoCompareFields(Const MIndex, DIndex: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure   ValidateDataFiles(
      out ResultArray: TEpiDblEntryResultArray;
      ValidateOptions: TEpiToolsDblEntryValidateOptions = EpiDefaultDblEntryValidateOptions);
    property    MainDF: TEpiDataFile read FMainDF write FMainDF;
    property    DuplDF: TEpiDataFile read FDuplDF write FDuplDF;
    // Sort fields is a list of common fields in which the Datafiles are sorted.
    // The list of fields MUST belong to MainDF.
    property    SortFields: TEpiFields read FSortFields write FSortFields;
    // Compare fields is a list of common fields for which the Datafiles are compared.
    // The list of fields MUST belong to MainDF.
    property    CompareFields: TEpiFields read FCmpFields write FCmpFields;
    property    DuplCompareFields: TEpiFields read FDuplCmpFields;
    property    DuplKeyFields: TEpiFields read FDuplKeyFields;

  { Helper }
  private
    class procedure InternalSort(var ResultArray: TEpiDblEntryResultArray; L, R: Integer);
  public
    class procedure SortDblEntryResultArray(var ResultArray: TEpiDblEntryResultArray);
  end;

implementation

uses
  epidocument, epidatafilestypes, epidatafileutils, math, epiglobals;

const
  ValTexts: array[TEpiDblEntryRecordResult] of string = (
    'Validated',
    'Record not found in main file',
    'Record not found in duplicate file',
    'Failed due to different values',
    'Failed due to different text',
    'Duplicate key exists',
    'Duplicate key exists');

{ TEpiToolsDblEntryValidator }

function TEpiToolsDblEntryValidator.AddResult(const MSortedRecNo,
  DSortedRecNo: integer; CompareResult: TEpiDblEntryRecordResult
  ): PEpiDblEntryResultRecord;
begin
  if CompareResult = rrValOk then exit;

  Result := NewResultRecord;
  with Result^ do
  begin
    ValResult := CompareResult;

    case CompareResult of
      rrValOk: ;  // Do nothing, should not reach! :D

      rrValNoExistsMain:
        DRecNo := DuplSortField.AsInteger[DSortedRecNo];

      rrValNoExistsDupl:
        MRecNo := MainSortField.AsInteger[MSortedRecNo];

      rrValValueFail,
      rrValTextFail:
        begin
          MRecNo := MainSortField.AsInteger[MSortedRecNo];
          DRecNo := DuplSortField.AsInteger[DSortedRecNo];
        end;

      rrValDupKeyMain:
        begin
          MRecNo := MainSortField.AsInteger[MSortedRecNo];
          DRecNo := MainSortField.AsInteger[DSortedRecNo];
        end;
      rrValDupKeyDupl:
        begin
          MRecNo := DuplSortField.AsInteger[DSortedRecNo];
          DRecNo := DuplSortField.AsInteger[MSortedRecNo];
        end;
    end;
  end;
end;

procedure TEpiToolsDblEntryValidator.InternalValidate;
var
  MRunner: Integer;
  DRunner: Integer;
  i: Integer;
  MSize: Integer;
  DSize: Integer;
  SortedCompare: Boolean;
  TestResult: TValueSign;

  function CompareSortFieldRecords(Const FL1, FL2: TEpiFields; Const Idx1, Idx2: Integer): TValueSign;
  var
    i: Integer;
    CaseSensitive: boolean;
  begin
    CaseSensitive := (devCaseSensitiveText in FValidateOptions);
    for i := 0 to FL1.Count - 1 do
    begin
      CompareFieldRecords(Result, FL1[i], FL2[i], Idx1, Idx2, CaseSensitive);
      if Result <> ZeroValue then Exit;
    end;
  end;

begin
  SortedCompare := Assigned(SortFields) and (SortFields.Count > 0);

  MainSortField := nil;
  DuplSortField := nil;
  FDuplKeyFields := nil;
  MainDF.BeginUpdate;
  DuplDF.BeginUpdate;

  try

    // Create temporary fields to preserver sorting.
    MainSortField := MainDF.NewField(ftInteger);
    for i := 0 to MainDF.Size - 1 do
      MainSortField.AsInteger[i] := i;

    DuplSortField := DuplDF.NewField(ftInteger);
    for i := 0 to DuplDF.Size - 1 do
      DuplSortField.AsInteger[i] := i;

    if SortedCompare then
    begin
      // Create a copy of the list of field to sort.
      FDuplKeyFields := TEpiFields.Create(nil);
      for i := 0 to SortFields.Count - 1 do
        FDuplKeyFields.AddItem(DuplDF.Fields.FieldByName[SortFields.Field[i].Name]);

      // Now sort both DF's before we are going to compare.
      MainDF.SortRecords(SortFields);
      DuplDF.SortRecords(FDuplKeyFields);
    end;

    // Do the validation!
    MRunner := 0;
    MSize   := MainDF.Size;
    DRunner := 0;
    DSize   := DuplDF.Size;
    while (MRunner < MSize) and (DRunner < DSize) do
    begin
      if (devIgnoreDeleted in FValidateOptions) and (MainDF.Deleted[MRunner]) then
      begin
        Inc(MRunner);
        Continue;
      end;

      if (devIgnoreDeleted in FValidateOptions) and (DuplDF.Deleted[DRunner]) then
      begin
        Inc(DRunner);
        Continue;
      end;

      if (SortedCompare) and
         (MRunner > 0) and
         (CompareSortFieldRecords(SortFields, SortFields, MRunner, MRunner - 1) = ZeroValue) then
      begin
        AddResult(MRunner, DRunner, rrValDupKeyMain);
        Inc(MRunner);
        Continue;
      end;

      if (SortedCompare) and
         (DRunner > 0) and
         (CompareSortFieldRecords(FDuplKeyFields, FDuplKeyFields, DRunner, DRunner - 1) = ZeroValue) then
      begin
        AddResult(MRunner, DRunner, rrValDupKeyDupl);
        Inc(DRunner);
        Continue;
      end;

      if SortedCompare then
        TestResult := CompareSortFieldRecords(SortFields, FDuplKeyFields, MRunner, DRunner)
      else
        TestResult := ZeroValue;

      case TestResult of
        NegativeValue:
          begin
            // Record does not exists in duplicate file
            if not (devIgnoreMissingRecords in FValidateOptions) then
              AddResult(MRunner, DRunner, rrValNoExistsDupl);
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
            AddResult(MRunner, DRunner, rrValNoExistsMain);
            Inc(DRunner);
          end;
      end;
    end;

    if not (devIgnoreMissingRecords in FValidateOptions) then
      while MRunner < MSize do
      begin
        // If MRunner < MSize, then records exists in Main DF with no matching
        // records in Dupl. DF. Hence mark them as NoExists.
          AddResult(MRunner, DRunner, rrValNoExistsDupl);
        Inc(MRunner);
      end;

    while DRunner < DSize do
    begin
      // If DRunner < DSize, then records exists in Dupl. DF with no matching
      // records in Main DF. Hence mark them as InDulpDF
      AddResult(MRunner, DRunner, rrValNoExistsMain);
      Inc(DRunner);
    end;

  finally
    if SortedCompare then
    begin
      // Restore sorting on both DF's is only nessesary if comparing DF's with key-fields.
      MainDF.SortRecords(MainSortField);
      DuplDF.SortRecords(DuplSortField);
    end;

    MainSortField.Free;
    DuplSortField.Free;

    MainDF.EndUpdate;
    DuplDF.EndUpdate;
  end;
end;

function TEpiToolsDblEntryValidator.NewResultRecord: PEpiDblEntryResultRecord;
var
  L: Integer;
begin
  L := Length(FResultArray);
  Inc(L);
  SetLength(FResultArray, L);
  FResultArray[L-1].CmpFieldNames := nil;
  Result := @FResultArray[L-1];
  Result^.MRecNo := -1;
  Result^.DRecNo := -1;
end;

procedure TEpiToolsDblEntryValidator.RaiseError(ErrorClass: ExceptClass;
  const msg: String);
begin
  raise ErrorClass.Create(Msg);
end;

procedure TEpiToolsDblEntryValidator.DoCompareFields(const MIndex,
  DIndex: Integer);
var
  i: Integer;
  CmpResult: TValueSign;
  lValResult: TEpiDblEntryRecordResult;
  ResultRecord: PEpiDblEntryResultRecord;
  L: Integer;
  CaseSensitive: Boolean;

begin
  CaseSensitive := (devCaseSensitiveText in FValidateOptions);
  ResultRecord := nil;
  for i := 0 to CompareFields.Count - 1 do
  begin
    if not CompareFieldRecords(CmpResult, FCmpFields[i], FDuplCmpFields[i], MIndex, DIndex, CaseSensitive) then
      RaiseError(EEpiInvalidCompare, 'Comparison failed due to different field types!');

    if CmpResult = ZeroValue then
      Continue;

    if FCmpFields[i].FieldType in StringFieldTypes then
      lValResult := rrValTextFail
    else
      lValResult := rrValValueFail;

    if not Assigned(ResultRecord) then
      ResultRecord := AddResult(MIndex, DIndex, lValResult);

    with ResultRecord^ do
    begin
      if (ValResult = rrValTextFail) and
         (lValResult = rrValValueFail)
      then
        ValResult := rrValValueFail;

      L := Length(CmpFieldNames);
      Inc(L);
      SetLength(CmpFieldNames, L);
      CmpFieldNames[L-1] := FCmpFields[i].Name;
    end;
  end;
end;

constructor TEpiToolsDblEntryValidator.Create;
begin

end;

destructor TEpiToolsDblEntryValidator.Destroy;
begin
  FDuplKeyFields.Free;
  FDuplCmpFields.Free;
  inherited Destroy;
end;

procedure TEpiToolsDblEntryValidator.ValidateDataFiles(out
  ResultArray: TEpiDblEntryResultArray;
  ValidateOptions: TEpiToolsDblEntryValidateOptions);
var
  i: Integer;
  V: TEpiValueLabelSet;
  VL: TEpiIntValueLabel;
  RR: TEpiDblEntryRecordResult;
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
      FResultField.Question.Text := 'Double Entry Verification Status';
      FResultField.Length := 1;
    end;

    if not Assigned(FResultField.ValueLabelSet) then
    begin
      V := FMainDF.ValueLabels.GetValueLabelSetByName(EpiDoubleEntryValueLabelSetName);
      if not Assigned(V) then
      begin
        V := FMainDF.ValueLabels.NewValueLabelSet(ftInteger);
        V.Name := EpiDoubleEntryValueLabelSetName;

        for RR in TEpiDblEntryRecordResult do
        begin
          VL := TEpiIntValueLabel(V.NewValueLabel);
          VL.Value := Integer(RR);
          VL.TheLabel.Text := ValTexts[RR];
        end;
      end;
      FResultField.ValueLabelSet := V;
    end;
  end;

  InternalValidate;

  // Set DF's to non-modified, since all sorting changes
  // were reverted.
  if not (devAddResultToField in FValidateOptions) then
  begin
    if Assigned(TEpiDocument(MainDF.RootOwner)) then
      TEpiDocument(MainDF.RootOwner).Modified := false;
    if Assigned(TEpiDocument(DuplDF.RootOwner)) then
      TEpiDocument(DuplDF.RootOwner).Modified := false;
  end;

  ResultArray := FResultArray;
end;

class procedure TEpiToolsDblEntryValidator.InternalSort(
  var ResultArray: TEpiDblEntryResultArray; L, R: Integer);
var
   I, J, P: Integer;

   procedure Exchange(J, I: Integer);
   var
     TValRes: TEpiDblEntryRecordResult;
     TMRecNo: Integer;
     TDRecNo: Integer;
     TCmpNames: Array of string;
   begin
     TValRes := ResultArray[J].ValResult;
     TMRecNo := ResultArray[J].MRecNo;
     TDRecNo := ResultArray[J].DRecNo;
     TCmpNames := ResultArray[j].CmpFieldNames;

     ResultArray[J].ValResult     := ResultArray[I].ValResult;
     ResultArray[J].MRecNo        := ResultArray[I].MRecNo;
     ResultArray[J].DRecNo        := ResultArray[I].DRecNo;
     ResultArray[j].CmpFieldNames := ResultArray[I].CmpFieldNames;

     ResultArray[I].ValResult     := TValRes;
     ResultArray[I].MRecNo        := TMRecNo;
     ResultArray[I].DRecNo        := TDRecNo;
     ResultArray[I].CmpFieldNames := TCmpNames;
   end;

   function CompareResults(Const A, B: TEpiDblEntryResultRecord): Integer;
   begin
     if (A.MRecNo = -1) or
        (B.MRecNo = -1)
     then
       Result := A.DRecNo - B.DRecNo
     else
       Result := A.MRecNo - B.MRecNo;
   end;

begin
  I:=L;
  J:=R;
  P:=(L + R) shr 1;
  repeat
    while CompareResults(ResultArray[I], ResultArray[P]) < 0 do Inc(I);
    while CompareResults(ResultArray[J], ResultArray[P]) > 0 do Dec(J);
    if I <= J then
    begin
      Exchange(J,I);
      if p=i then p:=j
      else if p=j then p:=i;
      Inc(I);
      Dec(J);
    end;
  until I>J;
  if L<J then InternalSort(ResultArray, L,J);
  if I < R then InternalSort(ResultArray, I, R);
end;

class procedure TEpiToolsDblEntryValidator.SortDblEntryResultArray(
  var ResultArray: TEpiDblEntryResultArray);
begin
  if Length(ResultArray) > 0 then
    InternalSort(ResultArray, 0, Length(ResultArray) - 1);
end;

end.

