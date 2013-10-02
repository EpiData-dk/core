unit epireport_report_studyinfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_base, epidocument;

type

  EEpiReportStudyInfo = class(EEpiReportBaseException);

  { TEpiReportStudyInfo }

  TEpiReportStudyInfo = class(TEpiReportBase)
  private
    FDocument: TEpiDocument;
  protected
    procedure DoSanityCheck; override;
  public
    procedure RunReport; override;
    property Document: TEpiDocument read FDocument write FDocument;
  end;

implementation

resourcestring
  SEpiReportStudyInfoNoDocument = 'EpiReport: No document assigned to project header.';

{ TEpiReportStudyInfo }

procedure TEpiReportStudyInfo.DoSanityCheck;
begin
  inherited DoSanityCheck;

  if not Assigned(FDocument) then
    DoError(EEpiReportStudyInfo, SEpiReportStudyInfoNoDocument);
end;

procedure TEpiReportStudyInfo.RunReport;

  function PostInc(var I: integer): Integer;
  begin
    Result := I;
    Inc(I);
  end;

var
  I: Integer;
  S: String;
begin
  inherited RunReport;

  DoTableHeader('Study information:', 2, 17);

  DoTableCell(0, 0, 'Name:');
  DoTableCell(1, 0, 'Value:');

  I := 1;
  with FDocument.Study do
  begin
    DoTableCell(0, I         , 'Title');
    DoTableCell(1, PostInc(i), Title.Text);

    DoTableCell(0, I         , 'Purpose');
    DoTableCell(1, PostInc(i), Purpose.Text);

    DoTableCell(0, I         , 'Geographical Coverage');
    DoTableCell(1, PostInc(i), GeographicalCoverage.Text);

    DoTableCell(0, I         , 'Data Collection');
    S := '';
    if (DataCollectionStart <> MaxDateTime) then
      S += DateToStr(DataCollectionStart);
    if (DataCollectionEnd <> MaxDateTime) then
      S += ' - ' + DateToStr(DataCollectionEnd);
    DoTableCell(1, PostInc(i), S);

    DoTableCell(0, I         , 'Author');
    DoTableCell(1, PostInc(i), Author);

    DoTableCell(0, I         , 'Created');
    DoTableCell(1, PostInc(i), DateTimeToStr(Created));

    DoTableCell(0, I         , 'Last Edited');
    DoTableCell(1, PostInc(i), DateTimeToStr(ModifiedDate));

    DoTableCell(0, I         , 'Version');
    DoTableCell(1, PostInc(i), Version);

    DoTableCell(0, I         , 'Funding');
    DoTableCell(1, PostInc(i), Funding.Text);

    DoTableCell(0, I         , 'Identifier');
    DoTableCell(1, PostInc(i), Identifier);

    DoTableCell(0, I         , 'Language');
    DoTableCell(1, PostInc(i), DefaultLang);

    DoTableCell(0, I         , 'Publisher');
    DoTableCell(1, PostInc(i), Publisher.Text);

    DoTableCell(0, I         , 'Rights');
    DoTableCell(1, PostInc(i), Rights.Text);

    DoTableCell(0, I         , 'Abstract');
    DoTableCell(1, PostInc(i), AbstractText.Text);

    DoTableCell(0, I         , 'Design');
    DoTableCell(1, PostInc(i), Design.Text);

    DoTableCell(0, I         , 'Unit of Observation');
    DoTableCell(1, PostInc(i), UnitOfObservation.Text);
  end;
  DoTableFooter('');
end;

end.

