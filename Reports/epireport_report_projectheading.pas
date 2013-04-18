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
    FFileNo: integer;
  protected
    procedure DoSanityCheck; override;
  public
    procedure RunReport; override;
    property Document: TEpiDocument read FDocument write FDocument;
    property Filename: string read FFilename write FFilename;
    property FileNo: integer read FFileNo write FFileNo;
  end;

implementation

uses
  epireport_types;

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

  DoTableHeader('File ' + IntToStr(FileNo) + ': ' + ExtractFileName(Filename), 2, 4, []);
  DoTableCell(0, 0, 'Title');
  DoTableCell(0, 1, 'Created');
  DoTableCell(0, 2, 'Last Edited');
  DoTableCell(0, 3, 'Version');
  DoTableCell(1, 0, Document.Study.Title.Text);
  DoTableCell(1, 1, DateTimeToStr(Document.Study.Created));
  DoTableCell(1, 2, DateTimeToStr(Document.Study.ModifiedDate));
  DoTableCell(1, 3, Document.Study.Version, tcaLeftAdjust);
  S := '';
  if Document.ProjectSettings.BackupOnShutdown then
    S += 'Backup on shutdown: active';
  if Document.PassWord <> '' then
    S += 'Encrypted data: active';
  DoTableFooter(S);

  DoLineText('');

  DoTableHeader('Dataforms:', 8{9}, Document.DataFiles.Count + 1);
  // Header row:
  DoTableCell(0, 0, 'Name');
  DoTableCell(1, 0, 'Created');
  DoTableCell(2, 0, 'Last Edited');
  DoTableCell(3, 0, 'Version');
  DoTableCell(4, 0, 'Sections');
  DoTableCell(5, 0, 'Fields');
  DoTableCell(6, 0, 'Records');
  DoTableCell(7, 0, 'Deleted');
//  DoTableCell(8, 0, 'Title');
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
//    DoTableCell(8, 1, Caption.Text);
  end;
  DoTableFooter('');
end;

end.

