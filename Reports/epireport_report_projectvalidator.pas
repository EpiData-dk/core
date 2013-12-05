unit epireport_report_projectvalidator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_base, epidocument, epidatafiles,
  epireport_generator_base,
  epitools_projectvalidate;

type

  { TEpiReportProjectValidator }

  TEpiReportProjectValidator = class(TEpiReportBase)
  private
    FDocument: TEpiDocument;
    FKeyFields: TEpiFields;
    FOptions: TEpiToolsProjectValidateOptions;
    FProjectValidationOptions: TEpiToolsProjectValidateOptions;
    FValidationFields: TEpiFields;
    function   RunTool: TEpiProjectResultArray;
  protected
    procedure DoSanityCheck; override;
  public
    constructor Create(ReportGenerator: TEpiReportGeneratorBase); override;
    procedure   RunReport; override;
    property    Document: TEpiDocument read FDocument write FDocument;
    property    ProjectValidationOptions: TEpiToolsProjectValidateOptions read FProjectValidationOptions write FProjectValidationOptions;
    property    KeyFields: TEpiFields read FKeyFields write FKeyFields;
    property    ValidationFields: TEpiFields read FValidationFields write FValidationFields;
    property    Options: TEpiToolsProjectValidateOptions read FOptions write FOptions;
  end;

implementation

uses
  LazUTF8, typinfo;

{ TEpiReportProjectValidator }
resourcestring
  SEpiReportProjectValidationNoDocument = 'EpiReport: No Document assigned to Project Validation';

function TEpiReportProjectValidator.RunTool: TEpiProjectResultArray;
var
  T: TEpiProjectValidationTool;
begin
  T := TEpiProjectValidationTool.Create;
  T.Document := Document;
  T.ValidationFields := ValidationFields;
  T.KeyFields        := KeyFields;
  T.ValidateProject(Result, Options);
  T.Free;
end;

procedure TEpiReportProjectValidator.DoSanityCheck;
begin
  inherited DoSanityCheck;

  if not Assigned(Document) then
    DoError(EEpiReportBaseException, SEpiReportProjectValidationNoDocument);
end;

constructor TEpiReportProjectValidator.Create(
  ReportGenerator: TEpiReportGeneratorBase);
begin
  inherited Create(ReportGenerator);
  FProjectValidationOptions := EpiDefaultProjectValidationOptions;
end;

procedure TEpiReportProjectValidator.RunReport;
var
  ResultArray: TEpiProjectResultArray;
  ResRecord: TEpiProjectValidateResultRecord;
  S: String;
  i: Integer;
  j: Integer;
begin
  inherited RunReport;

  ResultArray := RunTool;

  i := Low(ResultArray);
  while i <= High(ResultArray) do
  begin
    DoSection('Record no: ' + IntToStr(ResultArray[i].RecNo + 1));

    if Assigned(KeyFields) and
       (KeyFields.Count > 0)
    then
      begin
        S := 'Key Fields:';

        for j := 0 to KeyFields.Count -1 do
          S += '  ' + KeyFields[j].Name + ' = ' + KeyFields[j].AsString[ResultArray[i].RecNo];

        DoLineText(S);
      end;

    repeat
      ResRecord := ResultArray[i];

      S := ResRecord.Field.Name + ': ';
      case ResRecord.FailedCheck of
        pvCheckSystemMissing:
          S += 'System Missing';
        pvCheckRange:
          with ResRecord.Field do
            S += Format('Value = %s, Range = (%s, %s)',
                        [AsString[ResRecord.RecNo],
                         Ranges[0].AsString[true],
                         Ranges[0].AsString[false]
                        ]);
        pvCheckValueLabels:
          S += Format('Value = %s, is not a valid valuelabel value!',
                      [ResRecord.Field.AsString[ResRecord.RecNo]]);
        pvCheckComparison:
          with ResRecord.Field do
            S += Format('Comparison: %s=%s %s %s=%s',
                        [Name, AsString[ResRecord.RecNo],
                         ComparisonTypeToString(Comparison.CompareType),
                         Comparison.CompareField.Name, Comparison.CompareField.AsString[ResRecord.RecNo]
                        ]);
        pvCheckDataLength:
          with ResRecord.Field do
            S += Format('Field length = %d, Data length: %d',
                        [Length, UTF8Length(AsString[ResRecord.RecNo])]);
        pvCheckJumpValues:
          with ResRecord.Field do
            S += Format('Value = %s, is not a valid jump value!', [AsString[ResRecord.RecNo]]);
      else
          S += Format('Report not implemented for ToolCheck: %s',
                      [GetEnumName(TypeInfo(TEpiToolsProjectValidateOption), Integer(ResRecord.FailedCheck))]);
      end;
      DoLineText(S);
      Inc(i);
    until (i > High(ResultArray)) or (ResultArray[i].RecNo <> ResRecord.RecNo);
    DoLineText('');
  end;
end;

end.

