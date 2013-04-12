unit epireport_report_projectheading;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_base,
  epidocument;

type

  EEpiReportProjectHeader = class(EEpiReportBaseException);

  { TEpiReportProjectHeader }

  TEpiReportProjectHeader = class(TEpiReportBase)
  private
    FDocument: TEpiDocument;
    FFilename: string;
  protected
    procedure DoSanityCheck; override;
  public
    procedure RunReport; override;
    property Document: TEpiDocument read FDocument write FDocument;
    property Filename: string read FFilename write FFilename;
  end;

implementation

resourcestring
  SEpiReportProjectHeaderNoDocument = 'EpiReport: No document assigned to project header.';
  SEpiReportProjectHeaderNoFilename = 'EpiReport: No filename for project header.';

{ TEpiReportProjectHeader }

procedure TEpiReportProjectHeader.DoSanityCheck;
begin
  inherited DoSanityCheck;

  if not Assigned(FDocument) then
    DoError(EEpiReportProjectHeader, SEpiReportProjectHeaderNoDocument);
  if FFilename = '' then
    DoError(EEpiReportProjectHeader, SEpiReportProjectHeaderNoFilename);
end;

procedure TEpiReportProjectHeader.RunReport;
var
  i: Integer;
begin
  inherited RunReport;

  DoTableHeader('Project File Overview:', 5, 2);
  // Header row:
  DoTableCell(0, 0, 'Filename');
  DoTableCell(1, 0, 'Created');
  DoTableCell(2, 0, 'Last Edited');
  DoTableCell(3, 0, 'Version');
  DoTableCell(4, 0, 'Project Title');
  // Content:
  DoTableCell(0, 1, ExtractFileName(Filename));
  DoTableCell(1, 1, DateTimeToStr(Document.Study.Created));
  DoTableCell(2, 1, DateTimeToStr(Document.Study.ModifiedDate));
  DoTableCell(3, 1, Document.Study.Version);
  DoTableCell(4, 1, Document.Study.Title.Text);
  DoTableFooter('');

  DoLineText('');

  DoTableHeader('Dataforms:', 9, Document.DataFiles.Count + 1);
  // Header row:
  DoTableCell(0, 0, 'Name');
  DoTableCell(1, 0, 'Created');
  DoTableCell(2, 0, 'Last Edited');
  DoTableCell(3, 0, 'Version');
  DoTableCell(4, 0, 'Sections');
  DoTableCell(5, 0, 'Fields');
  DoTableCell(6, 0, 'Records');
  DoTableCell(7, 0, 'Deleted');
  DoTableCell(8, 0, 'Title');
  for i := 0 to Document.DataFiles.Count -1 do
  with Document.DataFiles[i] do
  begin
    DoTableCell(0, 1, Name);
    DoTableCell(1, 1, DateTimeToStr(Created));
    DoTableCell(2, 1, '----');
    DoTableCell(3, 1, Version);
    DoTableCell(4, 1, IntToStr(Sections.Count));
    DoTableCell(5, 1, IntToStr(Fields.Count));
    DoTableCell(6, 1, IntToStr(Size));
    DoTableCell(7, 1, IntToStr(DeletedCount));
    DoTableCell(8, 1, Caption.Text);
  end;
  DoTableFooter('');
end;

end.

