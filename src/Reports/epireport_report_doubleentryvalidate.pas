unit epireport_report_doubleentryvalidate;

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

  TEpiReportDEVCalcAllCallback = procedure(CommonRecords, MissingInMain, MissingInDupl, NonUniqueMain,
      NonUniqueDupl, ErrorRec, ErrorFields: integer) of object;


  TEpiReportDEVOption = (
    erdoShowOverview,
    erdoShowDetailList
  );
  TEpiReportDEVOptions = set of TEpiReportDEVOption;

  TEpiReportDoubleEntryValidation = class(TEpiReportBase)
  private
    FCompareFields: TEpiFields;
    FDblEntryValidateOptions: TEpiToolsDblEntryValidateOptions;
    FDuplDF: TEpiDataFile;
    FKeyFields: TEpiFields;
    FMainDF: TEpiDataFile;
    FResultArray: TEpiDblEntryResultArray;
    FVAlidator:   TEpiToolsDblEntryValidator;
  private
    FCommonRecords: integer;
    FMissingInMain: integer;
    FMissingInDupl: integer;
    FNonUniqueMain: integer;
    FNonUniqueDupl: integer;
    FErrorRec: integer;
    FErrorFields: integer;
    function    CalcCommonRecords: integer;
    function    CalcErrorPct: Extended;
    function    CalcErrorFieldPct: Extended;
    procedure   CalcAll;
  private
    FOnCallAllDone: TEpiReportDEVCalcAllCallback;
    FReportOptions: TEpiReportDEVOptions;
  protected
    procedure DoSanityCheck; override;
    procedure ResetCalculations;
  public
    constructor Create(ReportGenerator: TEpiReportGeneratorBase); override;
    procedure   RunReport; override;
    property    MainDF: TEpiDataFile read FMainDF write FMainDF;
    property    DuplDF: TEpiDataFile read FDuplDF write FDuplDF;
    property    KeyFields: TEpiFields read FKeyFields write FKeyFields;
    property    CompareFields: TEpiFields read FCompareFields write FCompareFields;
    property    DblEntryValidateOptions: TEpiToolsDblEntryValidateOptions read FDblEntryValidateOptions write FDblEntryValidateOptions;
    property    ReportOptions: TEpiReportDEVOptions read FReportOptions write FReportOptions;
    property    OnCallAllDone: TEpiReportDEVCalcAllCallback read FOnCallAllDone write FOnCallAllDone;
  end;

implementation

uses
  math, epireport_types, epimiscutils;

{ TEpiReportDoubleEntryValidation }

function TEpiReportDoubleEntryValidation.CalcCommonRecords: integer;
begin
  Result := FCommonRecords;
end;

function TEpiReportDoubleEntryValidation.CalcErrorPct: Extended;
begin
  Result := CalcCommonRecords;
  if Result = 0 then
    Exit(1);

  Result := FErrorRec / Result;
end;

function TEpiReportDoubleEntryValidation.CalcErrorFieldPct: Extended;
begin
  Result := CalcCommonRecords * FVAlidator.CompareFields.Count;
  if Result = 0 then
    Exit(1);

  result := FErrorFields / Result;
end;

procedure TEpiReportDoubleEntryValidation.CalcAll;
var
  R: TEpiDblEntryResultRecord;
begin
  for R in FResultArray do
    case R.ValResult of
      rrValOk: ;

      rrValNoExistsMain:
        Inc(FMissingInMain);

      rrValNoExistsDupl:
        Inc(FMissingInDupl);

      rrValValueFail,
      rrValTextFail:
        begin
          Inc(FErrorRec);
          Inc(FErrorFields, Length(R.CmpFieldNames));
        end;

      rrValDupKeyMain:
        Inc(FNonUniqueMain);

      rrValDupKeyDupl:
        Inc(FNonUniqueDupl);
    end;

  FCommonRecords := FVAlidator.MainDF.Size - FNonUniqueMain - FMissingInDupl;

  if Assigned(OnCallAllDone) then
    OnCallAllDone(FCommonRecords, FMissingInMain, FMissingInDupl, FNonUniqueMain, FNonUniqueDupl, FErrorRec, FErrorFields);

  // The same is true for this:
  // FCommonRecords := FVAlidator.DuplDF.Size - FNonUniqueDupl - FMissingInMain;
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

  ResetCalculations;
end;

procedure TEpiReportDoubleEntryValidation.ResetCalculations;
begin
  FMissingInMain := 0;
  FMissingInDupl := 0;
  FErrorRec      := 0;
  FErrorFields   := 0;
  FNonUniqueMain := 0;
  FNonUniqueDupl := 0;
end;

constructor TEpiReportDoubleEntryValidation.Create(
  ReportGenerator: TEpiReportGeneratorBase);
begin
  inherited Create(ReportGenerator);
  FDblEntryValidateOptions := EpiDefaultDblEntryValidateOptions;
  FReportOptions := [erdoShowDetailList, erdoShowOverview];
end;

procedure TEpiReportDoubleEntryValidation.RunReport;
var
  S: String;
  i: Integer;
  MText: String;
  DText: String;
  j: Integer;
  MCmpField: TEpiField;
  DCmpField: TEpiField;
  SortedCompare: Boolean;
  AText: String;
  BText: String;
  ARecNo: Integer;

  procedure KeyFieldText(Const Fields: TEpiFields; Const RecNo: Integer;
    var AText, BText: string);
  var
    i: integer;
  begin
    AText += 'Key Fields: ' + LineEnding;
    BText += LineEnding;

    for i := 0 to Fields.Count - 1 do
    begin
      AText += ' ' + Fields[i].Name + ' = ' + Fields[i].AsString[RecNo] + LineEnding;
      BText += LineEnding;
    end;
  end;


begin
  inherited RunReport;

  SortedCompare := Assigned(KeyFields) and (KeyFields.Count > 0);

  FValidator := TEpiToolsDblEntryValidator.Create;
  FValidator.MainDF := MainDF;
  FValidator.DuplDF := DuplDF;
  FValidator.SortFields := KeyFields;
  FValidator.CompareFields := CompareFields;
  FValidator.ValidateDataFiles(FResultArray, DblEntryValidateOptions);
  FValidator.SortDblEntryResultArray(FResultArray);
  CalcAll;

  if (erdoShowOverview in FReportOptions) then
    begin
      DoHeading('Selections for validation:');

      DoLineText('');
      DoTableHeader('Options:', 2, 5);
      DoTableCell(0, 0, 'Option');                 DoTableCell(1, 0, 'Selected');
      DoTableCell(0, 1, 'Ignore deleted records'); DoTableCell(1, 1, BoolToStr(devIgnoreDeleted in DblEntryValidateOptions, 'Yes', 'No'));
      DoTableCell(0, 2, 'Ignore missing records'); DoTableCell(1, 2, BoolToStr(devIgnoreMissingRecords in DblEntryValidateOptions, 'Yes', 'No'));
      DoTableCell(0, 3, 'Add result to field');    DoTableCell(1, 3, BoolToStr(devAddResultToField in DblEntryValidateOptions, 'Yes', 'No'));
      DoTableCell(0, 4, 'Case sensitive text');    DoTableCell(1, 4, BoolToStr(devCaseSensitiveText in DblEntryValidateOptions, 'Yes', 'No'));
      DoTableFooter('');

      if SortedCompare then
      begin
        DoLineText('');
        DoHeading('Key Fields:');
        S := '';
        for i := 0 to FKeyFields.Count - 1 do
          S := S + FKeyFields[i].Name + ' ';
        DoLineText(S);
      end;

      DoLineText('');
      DoHeading('Compared Fields:');
      for i := 0 to FCompareFields.Count - 1 do
        DoLineText(FCompareFields[i].Name + ': ' + FCompareFields[i].Question.Text);


      DoLineText('');
      DoHeading('Result of Validation:');

      DoLineText('');
      if SortedCompare then
        DoTableHeader('Overview', 2, 11)
      else
        DoTableHeader('Overview', 2, 9);

      I := 0;
      DoTableCell(0, I, 'Test');                              DoTableCell(1, PostInc(I), 'Result');
      DoTableCell(0, I, 'Records missing in main file');      DoTableCell(1, PostInc(I), IntToStr(FMissingInMain));
      DoTableCell(0, I, 'Records missing in duplicate file'); DoTableCell(1, PostInc(I), IntToStr(FMissingInDupl));
      if SortedCompare then
      begin
        DoTableCell(0, I, 'Non-unique records in main file');      DoTableCell(1, PostInc(I), IntToStr(FNonUniqueMain));
        DoTableCell(0, I, 'Non-unique records in duplicate file'); DoTableCell(1, PostInc(I), IntToStr(FNonUniqueDupl));
      end;
      DoTableCell(0, I, 'Number of fields checked');          DoTableCell(1, PostInc(I), IntToStr(FCompareFields.Count));
      DoTableCell(0, I, 'Common records');                    DoTableCell(1, PostInc(I), IntToStr(CalcCommonRecords));
      DoTableCell(0, I, 'Records with errors');               DoTableCell(1, PostInc(I), IntToStr(FErrorRec));
      DoTableCell(0, I, 'Field entries with errors');         DoTableCell(1, PostInc(I), IntToStr(FErrorFields));
      DoTableCell(0, I, 'Error percentage (#records)');       DoTableCell(1, PostInc(I), FormatFloat('##0.00', CalcErrorPct * 100));
      DoTableCell(0, I, 'Error percentage (#fields)');        DoTableCell(1, PostInc(I), FormatFloat('##0.00', CalcErrorFieldPct * 100));
      DoTableFooter('');
    end;

  if (erdoShowDetailList in FReportOptions) then
    begin
      DoLineText('');
      DoTableHeader('Datasets comparison:', 2, Length(FResultArray) + 1);
      DoTableCell(0,0, 'Main Dataset:');  DoTableCell(1, 0, 'Duplicate dataset:');

      for i := 0 to Length(FResultArray) -1 do
      with FResultArray[i] do
      begin
        if (ValResult in [rrValNoExistsMain, rrValDupKeyDupl]) then
          MTExt := ''
        else
          MText := 'Record no: ' + IntToStr(MRecNo + 1) + LineEnding;

        if (ValResult in [rrValNoExistsDupl, rrValDupKeyMain]) then
          DText := ''
        else
          DText := 'Record no: ' + IntToStr(DRecNo + 1) + LineEnding;


        if SortedCompare then
        begin
          if (ValResult in [rrValNoExistsMain, rrValDupKeyDupl]) then
            KeyFieldText(FVAlidator.DuplKeyFields, DRecNo, DText, MText)
          else
            KeyFieldText(FKeyFields,               MRecNo, MText, DText);
        end;

        case ValResult of
          rrValOk: ;  // Should not exists!

          rrValNoExistsMain:
            MText += 'Record not found';

          rrValNoExistsDupl:
            DText += 'Record not found';

          rrValValueFail,
          rrValTextFail:
            begin
              MText += 'Compared Fields:' + LineEnding;
              DText += LineEnding;

              for j := 0 to Length(CmpFieldNames) - 1 do
              begin
                MCmpField := FValidator.CompareFields.FieldByName[CmpFieldNames[j]];
                DCmpField := FValidator.DuplCompareFields.FieldByName[CmpFieldNames[j]];

                MText += ' '  + MCmpField.Name + ' = ' + MCmpField.AsString[MRecNo] + LineEnding;
                DText += '  ' + DCmpField.Name + ' = ' + DCmpField.AsString[DRecNo] + LineEnding;
              end;
            end;
          rrValDupKeyMain:
            MText += 'Duplicate key record found: ' + IntToStr(DRecNo + 1);

          rrValDupKeyDupl:
            DText += 'Duplicate key record found: ' + IntToStr(MRecNo + 1);
        end;
        DoTableCell(0, i + 1, MText, tcaLeftAdjust, [tcoBottomBorder, tcoTopBorder]);
        DoTableCell(1, i + 1, DText, tcaLeftAdjust, [tcoBottomBorder, tcoTopBorder]);
      end;
      DoTableFooter('');
    end;

  FValidator.Free;
end;

end.

