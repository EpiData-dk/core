unit epireport_doubleentry_validate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_base, epireport_htmlgenerator,
  epidocument, epidatafiles, epitools_val_dbl_entry;

type

  { TEpiReportDoubleEntryValidation }

  TEpiReportDoubleEntryValidation = class(TEpiReportBase)
  private
    FCompareFields: TEpiFields;
    FDblEntryValidateOptions: TEpiToolsDblEntryValidateOptions;
    FDuplEpiDocument: TEpiDocument;
    FKeyFields: TEpiFields;
  public
    constructor Create(const MainEpiDocument, DuplEpiDocument: TEpiDocument);
    procedure   RunReport; override;
    property    KeyFields: TEpiFields read FKeyFields write FKeyFields;
    property    CompareFields: TEpiFields read FCompareFields write FCompareFields;
    property    DblEntryValidateOptions: TEpiToolsDblEntryValidateOptions read FDblEntryValidateOptions write FDblEntryValidateOptions;
  end;

  { TEpiReportDoubleEntryValidationHtml }

  TEpiReportDoubleEntryValidationHtml = class(TEpiReportDoubleEntryValidation)
  private
    FHtmlGenerator: TEpiReportHTMLGenerator;
    FCompleteHtml: Boolean;
  protected
    function GetReportText: string; override;
  public
    constructor Create(const MainEpiDocument, DuplEpiDocument: TEpiDocument;
      Const CompleteHtml: boolean);
    destructor Destroy; override;
    procedure RunReport; override;
    property HtmlGenerator: TEpiReportHTMLGenerator read FHtmlGenerator;
  end;

implementation

{ TEpiReportDoubleEntryValidation }

constructor TEpiReportDoubleEntryValidation.Create(const MainEpiDocument,
  DuplEpiDocument: TEpiDocument);
begin
  inherited Create(MainEpiDocument);
  FDuplEpiDocument := DuplEpiDocument;
  FDblEntryValidateOptions := EpiDefaultDblEntryValidateOptions;
end;

procedure TEpiReportDoubleEntryValidation.RunReport;
var
  Validator: TEpiToolsDblEntryValidator;
  ExtraDulpRecords: TBoundArray;
  S: String;
  i: Integer;
  BadCount: Integer;
  ResultArray: TEpiDblEntryResultArray;
  MText: String;
  DText: String;
  j: Integer;
  MCmpField: TEpiField;
  DCmpField: TEpiField;
begin
  inherited RunReport;

  Validator := TEpiToolsDblEntryValidator.Create;
  Validator.MainDF := EpiDocument.DataFiles[0];
  Validator.DuplDF := FDuplEpiDocument.DataFiles[0];
  Validator.SortFields := FKeyFields;
  Validator.CompareFields := FCompareFields;
  Validator.ValidateDataFiles(ResultArray, ExtraDulpRecords, DblEntryValidateOptions);
  Validator.SortDblEntryResultArray(ResultArray);

  DoSection('Result of Validation:');
  DoTableHeader('', 2, 4);
  DoTableCell(0, 0, 'Records missing in main file');      DoTableCell(1, 0, IntToStr(Validator.MainDF.DeletedCount));
  DoTableCell(0, 1, 'Records missing in duplicate file'); DoTableCell(1, 1, IntToStr(Validator.DuplDF.DeletedCount));
  DoTableCell(0, 2, 'Common records found');              DoTableCell(1, 2, IntToStr(Length(ResultArray)));
  DoTableCell(0, 3, 'Number of fields checked');          DoTableCell(1, 3, IntToStr(FCompareFields.Count));
  DoTableFooter('');

  DoHeading('Key Fields:');
  S := '';
  for i := 0 to FKeyFields.Count - 1 do
    S := S + FKeyFields[i].Name + ' ';
  DoLineText(S);

  DoHeading('Compared Fields:');
  for i := 0 to FCompareFields.Count - 1 do
    DoLineText(FCompareFields[i].Name + ': ' + FCompareFields[i].Question.Text);

  DoTableHeader('Datasets comparison:', 2, Length(ResultArray) + Length(ExtraDulpRecords) + 1);
  DoTableCell(0,0, 'Main Dataset:');  DoTableCell(1, 0, 'Duplicate dataset:');
  for i := 0 to Length(ResultArray) -1 do
  with ResultArray[i] do
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
            MCmpField := Validator.CompareFields.FieldByName[CmpFieldNames[j]];
            DCmpField := Validator.DuplCompareFields.FieldByName[CmpFieldNames[j]];

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

  for i := 0 to Length(ExtraDulpRecords) - 1 do
  begin
    MText := LineEnding;
    DText := 'Record no: ' + IntToStr(ExtraDulpRecords[i] + 1) + LineEnding;

    MText += LineEnding;
    DText += 'Key Fields: ' + LineEnding;
    for j := 0 to Validator.DuplKeyFields.Count - 1 do
    begin
      DCmpField := Validator.DuplKeyFields[j];
      MText += LineEnding;
      DText += DCmpField.Name + ' = ' + DCmpField.AsString[ExtraDulpRecords[i]] + LineEnding;
    end;

    MText += 'Record not found';
    DText += 'Record not found in main datafile!';

    DoTableCell(0, Length(ResultArray) + i + 1, MText);
    DoTableCell(1, Length(ResultArray) + i + 1, DText);
  end;
  DoTableFooter('');
end;

{ TEpiReportDoubleEntryValidationHtml }

function TEpiReportDoubleEntryValidationHtml.GetReportText: string;
begin
  Result := FHtmlGenerator.GetReportText;
end;

constructor TEpiReportDoubleEntryValidationHtml.Create(const MainEpiDocument,
  DuplEpiDocument: TEpiDocument; const CompleteHtml: boolean);
begin
  inherited Create(MainEpiDocument, DuplEpiDocument);
  FHtmlGenerator := TEpiReportHTMLGenerator.Create(Self);
  FCompleteHtml := CompleteHtml;
end;

destructor TEpiReportDoubleEntryValidationHtml.Destroy;
begin
  FHtmlGenerator.Free;
  inherited Destroy;
end;

procedure TEpiReportDoubleEntryValidationHtml.RunReport;
begin
  if FCompleteHtml then
    FHtmlGenerator.InitHtml('Double Entry Validation');

  inherited RunReport;

  if FCompleteHtml then
    FHtmlGenerator.CloseHtml;
end;

end.

