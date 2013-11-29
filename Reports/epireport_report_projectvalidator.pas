unit epireport_report_projectvalidator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_base, epidocument, epireport_generator_base,
  epitools_projectvalidate;

type

  { TEpiReportProjectValidator }

  TEpiReportProjectValidator = class(TEpiReportBase)
  private
    FDocument: TEpiDocument;
    FProjectValidationOptions: TEpiToolsProjectValidateOptions;
    function   RunTool: TEpiProjectResultArray;
  protected
    procedure DoSanityCheck; override;
  public
    constructor Create(ReportGenerator: TEpiReportGeneratorBase); override;
    procedure   RunReport; override;
    property    Document: TEpiDocument read FDocument write FDocument;
    property    ProjectValidationOptions: TEpiToolsProjectValidateOptions read FProjectValidationOptions write FProjectValidationOptions;
  end;

implementation

uses
  epidatafiles, LazUTF8;

{ TEpiReportProjectValidator }
resourcestring
  SEpiReportProjectValidationNoDocument = 'EpiReport: No Document assigned to Project Validation';

function TEpiReportProjectValidator.RunTool: TEpiProjectResultArray;
var
  T: TEpiProjectValidationTool;
begin
  T := TEpiProjectValidationTool.Create;
  T.Document := Document;
  T.ValidateProject(Result{, ProjectValidationOptions});
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
begin
  inherited RunReport;

  ResultArray := RunTool;

  i := Low(ResultArray);

  while i <= High(ResultArray) do
  begin
    DoSection('Record no: ' + IntToStr(ResultArray[i].RecNo + 1));

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
          S += Format('Value = %s, has no valid valuelabel!',
                      [ResRecord.Field.AsString[ResRecord.RecNo]]);
        pvCheckComparison:
          with ResRecord.Field do
            S += Format('Comparison: %s=%s %s %s=%s',
                        [Name, AsString[ResRecord.RecNo],
                         ComparisonTypeToString(Comparison.CompareType),
                         Comparison.CompareField.Name, Comparison.CompareField.AsString[ResRecord.RecNo]
                        ]);
{
            'Comparison: ' + Name + ':' + AsString[ResRecord.RecNo] + ' ' +
                 ComparisonTypeToString(Comparison.CompareType) +
                 Comparison.CompareField.Name + ':' + Comparison.CompareField.AsString[ResRecord.RecNo];     }
        pvCheckDataLength:
          with ResRecord.Field do
            S += Format('Field length = %d, Data length: %d',
                        [Length, UTF8Length(AsString[ResRecord.RecNo])]);
      end;
      DoLineText(S);
      Inc(i);
    until (i > High(ResultArray)) or (ResultArray[i].RecNo <> ResRecord.RecNo);
    DoLineText('');
  end;
end;

end.

