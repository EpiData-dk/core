unit epireport_doubleentry_validate;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_base, epireport_generator_base,
  epidocument, epidatafiles, epitools_val_dbl_entry;

const
  SEpiReportDEVNoDataFile = 'Datafile "%s" not assigned';
  SEpiReportDEVNoFields   = 'Fields "%s" not assigned';

type
  EEpiReportDEVException = class(Exception);

  { TEpiReportDoubleEntryValidation }

  TEpiReportDoubleEntryValidation = class(TEpiReportBase)
  private
    FCompareFields: TEpiFields;
    FDblEntryValidateOptions: TEpiToolsDblEntryValidateOptions;
    FDuplDF: TEpiDataFile;
    FDuplEpiDocument: TEpiDocument;
    FKeyFields: TEpiFields;
    FMainDF: TEpiDataFile;
    FResultArray: TEpiDblEntryResultArray;
    FExtraRecs:   TBoundArray;
    FVAlidator:   TEpiToolsDblEntryValidator;
    function    CalcMissingInMainDF: Integer;
    function    CalcMissingInDuplDF: Integer;
    function    CalcCommonRecords: integer;
    function    CalcErrorRecords:  integer;
    function    CalcErrorFields:  integer;
    function    CalcErrorPct: Extended;
    function    CalcErrorFieldPct: Extended;
  protected
    procedure DoSanityCheck; override;
  public
    constructor Create(ReportGenerator: TEpiReportGeneratorBase); override;
    procedure   RunReport; override;
    property    MainDF: TEpiDataFile read FMainDF write FMainDF;
    property    DuplDF: TEpiDataFile read FDuplDF write FDuplDF;
    property    KeyFields: TEpiFields read FKeyFields write FKeyFields;
    property    CompareFields: TEpiFields read FCompareFields write FCompareFields;
    property    DblEntryValidateOptions: TEpiToolsDblEntryValidateOptions read FDblEntryValidateOptions write FDblEntryValidateOptions;
  end;

implementation

uses
  math;

{ TEpiReportDoubleEntryValidation }

function TEpiReportDoubleEntryValidation.CalcMissingInMainDF: Integer;
begin
  result := Length(FExtraRecs);
end;

function TEpiReportDoubleEntryValidation.CalcMissingInDuplDF: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Length(FResultArray) - 1 do
    if FResultArray[i].ValResult = ValNoExists then inc(Result);
end;

function TEpiReportDoubleEntryValidation.CalcCommonRecords: integer;
begin
  Result := Math.Min(FValidator.MainDF.Size, FValidator.DuplDF.Size) - Length(FExtraRecs);
end;

function TEpiReportDoubleEntryValidation.CalcErrorRecords: integer;
var
  i: Integer;
begin
  Result  := 0;
  for i := 0 to Length(FResultArray) - 1 do
    if FResultArray[i].ValResult in [ValTextFail, ValValueFail] then inc(Result);
end;

function TEpiReportDoubleEntryValidation.CalcErrorFields: integer;
var
  i: Integer;
begin
  Result  := 0;
  for i := 0 to Length(FResultArray) - 1 do
    if FResultArray[i].ValResult in [ValTextFail, ValValueFail] then inc(Result, Length(FResultArray[i].CmpFieldNames));
end;

function TEpiReportDoubleEntryValidation.CalcErrorPct: Extended;
begin
  Result := CalcErrorRecords / CalcCommonRecords;
end;

function TEpiReportDoubleEntryValidation.CalcErrorFieldPct: Extended;
begin
  result := CalcErrorFields / (CalcCommonRecords * FVAlidator.CompareFields.Count);
end;

procedure TEpiReportDoubleEntryValidation.DoSanityCheck;
begin
  inherited DoSanityCheck;

  if not Assigned(MainDF) then
    DoError(EEpiReportDEVException, Format(SEpiReportDEVNoDataFile, ['MainDF']));
  if not Assigned(DuplDF) then
    DoError(EEpiReportDEVException, Format(SEpiReportDEVNoDataFile, ['DuplDF']));
  if not Assigned(KeyFields) then
    DoError(EEpiReportDEVException, Format(SEpiReportDEVNoFields, ['KeyFields']));
  if not Assigned(CompareFields) then
    DoError(EEpiReportDEVException, Format(SEpiReportDEVNoFields, ['CompareFields']));
end;

constructor TEpiReportDoubleEntryValidation.Create(
  ReportGenerator: TEpiReportGeneratorBase);
begin
  inherited Create(ReportGenerator);
  FDblEntryValidateOptions := EpiDefaultDblEntryValidateOptions;
end;

procedure TEpiReportDoubleEntryValidation.RunReport;
var
  S: String;
  i: Integer;
  BadCount: Integer;
  MText: String;
  DText: String;
  j: Integer;
  MCmpField: TEpiField;
  DCmpField: TEpiField;
  CommonCount: Integer;
begin
  inherited RunReport;

  FValidator := TEpiToolsDblEntryValidator.Create;
  FValidator.MainDF := MainDF;
  FValidator.DuplDF := DuplDF;
  FValidator.SortFields := KeyFields;
  FValidator.CompareFields := CompareFields;
  FValidator.ValidateDataFiles(FResultArray, FExtraRecs, DblEntryValidateOptions);
  FValidator.SortDblEntryResultArray(FResultArray);

  DoSection('Selections for validation:');
  DoTableHeader('Options', 2, 5);
  DoTableCell(0, 0, 'Option');                 DoTableCell(1, 0, 'Selected');
  DoTableCell(0, 1, 'Ignore deleted records'); DoTableCell(1, 1, BoolToStr(devIgnoreDeleted in DblEntryValidateOptions, 'Yes', 'No'));
  DoTableCell(0, 2, 'Ignore missing records'); DoTableCell(1, 2, BoolToStr(devIgnoreMissingRecords in DblEntryValidateOptions, 'Yes', 'No'));
  DoTableCell(0, 3, 'Add result to field');    DoTableCell(1, 3, BoolToStr(devAddResultToField in DblEntryValidateOptions, 'Yes', 'No'));
  DoTableCell(0, 4, 'Case sensitive text');    DoTableCell(1, 4, BoolToStr(devCaseSensitiveText in DblEntryValidateOptions, 'Yes', 'No'));
  DoTableFooter('');

  DoHeading('Key Fields:');
  S := '';
  for i := 0 to FKeyFields.Count - 1 do
    S := S + FKeyFields[i].Name + ' ';
  DoLineText(S);

  DoHeading('Compared Fields:');
  for i := 0 to FCompareFields.Count - 1 do
    DoLineText(FCompareFields[i].Name + ': ' + FCompareFields[i].Question.Text);


  DoSection('Result of Validation:');
  DoTableHeader('Overview', 2, 9);
  DoTableCell(0, 0, 'Test');                              DoTableCell(1, 0, 'Result');
  DoTableCell(0, 1, 'Records missing in main file');      DoTableCell(1, 1, IntToStr(CalcMissingInMainDF));
  DoTableCell(0, 2, 'Records missing in duplicate file'); DoTableCell(1, 2, IntToStr(CalcMissingInDuplDF));
  DoTableCell(0, 3, 'Number of fields checked');          DoTableCell(1, 3, IntToStr(FCompareFields.Count));
  DoTableCell(0, 4, 'Common records');                    DoTableCell(1, 4, IntToStr(CalcCommonRecords));
  DoTableCell(0, 5, 'Records with errors');               DoTableCell(1, 5, IntToStr(CalcErrorRecords));
  DoTableCell(0, 6, 'Field entries with errors');         DoTableCell(1, 6, IntToStr(CalcErrorFields));
  DoTableCell(0, 7, 'Error percentage (#records)');       DoTableCell(1, 7, FormatFloat('##0.00', CalcErrorPct * 100));
  DoTableCell(0, 8, 'Error percentage (#fields)');        DoTableCell(1, 8, FormatFloat('##0.00', CalcErrorFieldPct * 100));
  DoTableFooter('');

  DoTableHeader('Datasets comparison:', 2, Length(FResultArray) + Length(FExtraRecs) + 1);
  DoTableCell(0,0, 'Main Dataset:');  DoTableCell(1, 0, 'Duplicate dataset:');
  for i := 0 to Length(FResultArray) -1 do
  with FResultArray[i] do
  begin
    MText := 'Record no: ' + IntToStr(MRecNo + 1) + LineEnding;
    if not ((ValResult = ValNoExists) or (ValResult = ValDupKeyFail)) then
      DText := 'Record no: ' + IntToStr(DRecNo + 1) + LineEnding
    else
      DText := '';

    MText += 'Key Fields: ' + LineEnding;
    DText += LineEnding;
    for j := 0 to FKeyFields.Count - 1 do
    begin
      MText += FKeyFields[j].Name + ' = ' + FKeyFields[j].AsString[MRecNo] + LineEnding;
      DText += LineEnding;
    end;

    case ValResult of
      ValNoExists:
        begin
          MText += 'Record not found in duplicate file';
          DText += 'Record not found';
        end;
      ValTextFail,
      ValValueFail:
        begin
          MText += 'Compared Fields:' + LineEnding;
          DText += 'Compared Fields:' + LineEnding;

          for j := 0 to Length(CmpFieldNames) - 1 do
          begin
            MCmpField := FValidator.CompareFields.FieldByName[CmpFieldNames[j]];
            DCmpField := FValidator.DuplCompareFields.FieldByName[CmpFieldNames[j]];

            MText += ' ' + MCmpField.Name + ' = ' + MCmpField.AsString[MRecNo] + LineEnding;
            DText += ' ' + DCmpField.Name + ' = ' + DCmpField.AsString[DRecNo] + LineEnding;
          end;
        end;
      ValDupKeyFail:
        begin
          MText += 'Duplicate key record found: ' + IntToStr(DRecNo + 1);
        end;
    end;

    DoTableCell(0, i + 1, MText);
    DoTableCell(1, i + 1, DText);
  end;

  for i := 0 to Length(FExtraRecs) - 1 do
  begin
    MText := LineEnding;
    DText := 'Record no: ' + IntToStr(FExtraRecs[i] + 1) + LineEnding;

    MText += LineEnding;
    DText += 'Key Fields: ' + LineEnding;
    for j := 0 to FValidator.DuplKeyFields.Count - 1 do
    begin
      DCmpField := FValidator.DuplKeyFields[j];
      MText += LineEnding;
      DText += DCmpField.Name + ' = ' + DCmpField.AsString[FExtraRecs[i]] + LineEnding;
    end;

    MText += 'Record not found';
    DText += 'Record not found in main datafile!';

    DoTableCell(0, Length(FResultArray) + i + 1, MText);
    DoTableCell(1, Length(FResultArray) + i + 1, DText);
  end;
  DoTableFooter('');

  FValidator.Free;
end;

end.

