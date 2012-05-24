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
begin
  inherited RunReport;

  Validator := TEpiToolsDblEntryValidator.Create;
  Validator.MainDF := EpiDocument.DataFiles[0];
  Validator.DuplDF := FDuplEpiDocument.DataFiles[0];
  Validator.SortFields := FKeyFields;
  Validator.CompareFields := FCompareFields;
  Validator.ValidateDataFiles(ResultArray, ExtraDulpRecords, DblEntryValidateOptions);

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

  DoTableHeader('Field comparisons:', 2, Length(ResultArray) + 1);
  DoTableCell(0,0, 'Record no:');  DoTableCell(1, 0, 'Status');
  BadCount := 0;
  for i := 0 to Length(ResultArray) -1 do
  begin
    DoTableCell(0, i + 1, IntToStr(i + 1));
    case ResultArray[i].ValResult of
      ValNoExists:    S := 'Record not found in duplicate file';
      ValTextFail:    S := 'Text mismatch';
      ValValueFail:   S := 'Value mismatch';
      ValDupKeyFail:  S := 'Duplicate key exists';
    end;
    DoTableCell(1, i + 1, S);
  end;
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

