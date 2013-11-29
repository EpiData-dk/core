unit epitools_projectvalidate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, epidocument, epidatafiles;

type
  TEpiToolsProjectValidateOption = (
    pvIgnoreDeleted,                  // Ignores if a record i marked for deletion
    pvCheckSystemMissing,             // Check data if it is system missing
    pvCheckRange,                     // Check data for range correctness
    pvCheckValueLabels,               // Check data for valid valuelabel
    pvCheckComparison,                // Check data for compared value
    pvCheckDataLength                 // Check data for valid length
  );
  TEpiToolsProjectValidateOptions = set of TEpiToolsProjectValidateOption;

const
  EpiDefaultProjectValidationOptions = [pvIgnoreDeleted..pvCheckDataLength];

type

  TEpiProjectValidateResultRecord = record
    RecNo: Integer;                               // Record where something failed.
    Field: TEpiField;                             // Reference to field that failed
    FailedCheck: TEpiToolsProjectValidateOption;  // Indicates which check that failed.
  end;
  PEpiProjectResultArray = ^TEpiProjectValidateResultRecord;

  // NOTE: Consecutive records may have same RecNo, if the same record have more than one failed field.
  TEpiProjectResultArray = array of TEpiProjectValidateResultRecord;

  { TEpiProjectValidationTool }

  TEpiProjectValidationTool = class
  private
    FDocument: TEpiDocument;
    FResultArray: TEpiProjectResultArray;
    function    NewResultRecord: PEpiProjectResultArray;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   ValidateProject(out ResultArray: TEpiProjectResultArray;
      Options: TEpiToolsProjectValidateOptions = EpiDefaultProjectValidationOptions);
    property    Document: TEpiDocument read FDocument write FDocument;
  end;

implementation

uses
  epidatafileutils, LazUTF8;

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

constructor TEpiProjectValidationTool.Create;
begin
  //
end;

destructor TEpiProjectValidationTool.Destroy;
begin
  inherited Destroy;
end;

procedure TEpiProjectValidationTool.ValidateProject(out
  ResultArray: TEpiProjectResultArray; Options: TEpiToolsProjectValidateOptions
  );
var
  i, j: Integer;
  Df: TEpiDataFile;
  F: TEpiField;
  CompareField: TEpiField;
  Res: Boolean;
  CmpResult: TValueSign;
  S: String;
begin
  Df := Document.DataFiles[0];

  for i := 0 to Df.Size - 1 do
  begin
    if Df.Deleted[i] and (pvIgnoreDeleted in Options) then continue;

    for j := 0 to Df.Fields.Count - 1 do
    begin
      F := Df.Field[j];

      if (pvCheckSystemMissing in Options) then
      begin
        // Check for system missing

        if F.IsMissing[i] then
        with NewResultRecord^ do
        begin
          RecNo := i;
          Field := F;
          FailedCheck := pvCheckSystemMissing;
        end;
      end;

      if (pvCheckRange in Options) and
         (Assigned(F.Ranges))
      then
      begin
        // Check for data is within specified range
        if not F.Ranges.InRange(F.AsValue[i]) then
        with NewResultRecord^ do
        begin
          RecNo := i;
          Field := F;
          FailedCheck := pvCheckRange;
        end;
      end;

      if (pvCheckValueLabels in Options) and
         (Assigned(F.ValueLabelSet))
      then
      begin
        if not F.ValueLabelSet.ValueLabelExists[F.AsValue[i]] then
        with NewResultRecord^ do
        begin
          RecNo := i;
          Field := F;
          FailedCheck := pvCheckValueLabels;
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
          RecNo := i;
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
          RecNo := i;
          Field := F;
          FailedCheck := pvCheckDataLength;
        end;
      end;
    end;
  end;

  ResultArray := FResultArray;
end;

end.

