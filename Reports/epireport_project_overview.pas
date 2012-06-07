unit epireport_project_overview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_base, epireport_htmlgenerator,
  epidocument, epidatafiles;

{
type

  { TEpiReportProjectOverView }

  TEpiReportProjectOverView = class(TEpiReportBase)
  private
    FDocument: TEpiDocument;
    FEpiDataFiles: TEpiDataFiles;
  public
    constructor Create(const AEpiDocument: TEpiDocument); override;
    procedure   RunReport; override;
    property    EpiDataFiles: TEpiDataFiles read FEpiDataFiles;
  end;

  { TEpiReportProjectOverViewHtml }

  TEpiReportProjectOverViewHtml = class(TEpiReportProjectOverView)
  private
    FHtmlGenerator: TEpiReportHTMLGenerator;
    FCompleteHtml: Boolean;
  protected
    function GetReportText: string; override;
  public
    constructor Create(const AEpiDocument: TEpiDocument;
      Const CompleteHtml: boolean = false);
    destructor Destroy; override;
    procedure RunReport; override;
    property HtmlGenerator: TEpiReportHTMLGenerator read FHtmlGenerator;
  end;               }

implementation
                       {
{ TEpiReportProjectOverView }

constructor TEpiReportProjectOverView.Create(const AEpiDocument: TEpiDocument);
begin
  inherited Create(AEpiDocument);
  FDocument := AEpiDocument;
end;

procedure TEpiReportProjectOverView.RunReport;
var
  i: Integer;
begin
  DoTableHeader('Study information:', 2, 15);

  DoTableCell(0, 0, 'Name:');
  DoTableCell(1, 0, 'Value:');

  with FDocument.Study do
  begin
    DoTableCell(0, 1, 'Abstract');
    DoTableCell(1, 1, AbstractText.Text);

    DoTableCell(0, 2, 'Author');
    DoTableCell(1, 2, Author);

    DoTableCell(0, 3, 'Created');
    DoTableCell(1, 3, DateToStr(Created));

    DoTableCell(0, 4, 'Funding');
    DoTableCell(1, 4, Funding.Text);

    DoTableCell(0, 5, 'GeographicalCoverage');
    DoTableCell(1, 5, GeographicalCoverage.Text);

    DoTableCell(0, 6, 'Identifier');
    DoTableCell(1, 6, Identifier);

    DoTableCell(0, 7, 'Language');
    DoTableCell(1, 7, DefaultLang);

    DoTableCell(0, 8, 'Modified');
    DoTableCell(1, 8, DateToStr(ModifiedDate));

    DoTableCell(0, 9, 'Publisher');
    DoTableCell(1, 9, Publisher.Text);

    DoTableCell(0, 10, 'Purpose');
    DoTableCell(1, 10, Purpose.Text);

    DoTableCell(0, 11, 'Rights');
    DoTableCell(1, 11, Rights.Text);

    DoTableCell(0, 12, 'TimeCoverage');
    DoTableCell(1, 12, TimeCoverage.Text);

    DoTableCell(0, 13, 'Title');
    DoTableCell(1, 13, Title.Text);

    DoTableCell(0, 14, 'Version');
    DoTableCell(1, 14, Version);
  end;
  DoTableFooter('');

  DoTableHeader('Databases:', 5, FDocument.DataFiles.Count + 1);
  DoTableCell(0, 0, 'Database title');
  DoTableCell(1, 0, 'Fields');
  DoTableCell(2, 0, 'Section');
  DoTableCell(3, 0, 'Records');
  DoTableCell(4, 0, 'Deleted');

  for i := 0 to FDocument.DataFiles.Count -1 do
  with FDocument.DataFiles[i] do
  begin
    DoTableCell(0, 1, Caption.Text);
    DoTableCell(1, 1, IntToStr(Fields.Count));
    DoTableCell(2, 1, IntToStr(Sections.Count));
    DoTableCell(3, 1, IntToStr(Size));
    DoTableCell(4, 1, IntToStr(DeletedCount));
  end;
  DoTableFooter('');
end;

{ TEpiReportProjectOverViewHtml }

function TEpiReportProjectOverViewHtml.GetReportText: string;
begin
  Result := FHtmlGenerator.GetReportText;
end;

constructor TEpiReportProjectOverViewHtml.Create(
  const AEpiDocument: TEpiDocument; const CompleteHtml: boolean);
begin
  inherited Create(AEpiDocument);
  FHtmlGenerator := TEpiReportHTMLGenerator.Create(Self);
  FCompleteHtml := CompleteHtml;
end;

destructor TEpiReportProjectOverViewHtml.Destroy;
begin
  FHtmlGenerator.Free;
  inherited Destroy;
end;

procedure TEpiReportProjectOverViewHtml.RunReport;
begin
  if FCompleteHtml then
    FHtmlGenerator.InitHtml('Project Overview');

  inherited RunReport;

  if FCompleteHtml then
    FHtmlGenerator.CloseHtml;
end;
}
end.

