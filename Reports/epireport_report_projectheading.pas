unit epireport_report_projectheading;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_base,
  epidocument, epirelations, epicustombase;

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
  epireport_types, math, epidatafiles;

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
  S: String;
  j: Integer;
  LastEdit: TDateTime;
  OrderedDataFiles: TEpiDataFiles;
  List: TEpiRelationList;
  Relations: TEpiRelationList;

begin
  inherited RunReport;

  LastEdit := Document.Study.ModifiedDate;
  for i := 0 to Document.DataFiles.Count -1 do
  with Document.DataFiles[i] do
    LastEdit := Max(LastEdit, Max(RecModifiedDate, StructureModifiedDate));

  DoTableHeader('File ' + IntToStr(FileNo) + ': ' + ExtractFileName(Filename), 2, 5, []);
  DoTableCell(0, 0, 'Title');
  DoTableCell(0, 1, 'Created');
  DoTableCell(0, 2, 'Last Edited');
  DoTableCell(0, 3, 'Version');
  DoTableCell(0, 4, 'Cycle');
  DoTableCell(1, 0, Document.Study.Title.Text);
  DoTableCell(1, 1, DateTimeToStr(Document.Study.Created));
  DoTableCell(1, 2, DateTimeToStr(LastEdit));
  DoTableCell(1, 3, Document.Study.Version, tcaLeftAdjust);
  DoTableCell(1, 4, IntToStr(Document.CycleNo), tcaLeftAdjust);
  S :=
    'Backup on shutdown: ' + BoolToStr(Document.ProjectSettings.BackupOnShutdown, 'yes', 'no') + LineEnding +
    'Encrypted data: '     + BoolToStr(Document.PassWord <> '',                   'yes', 'no');
  DoTableFooter(Trim(S));

  DoLineText('');


  OrderedDataFiles := Document.Relations.GetOrderedDataFiles;

  DoTableHeader('Dataforms:', 9, OrderedDataFiles.Count + 1);
  // Header row:
  DoTableCell(0, 0, 'Name');
  DoTableCell(1, 0, 'Created');
  DoTableCell(2, 0, 'Structure Edited');
  DoTableCell(3, 0, 'Data Edited');
  DoTableCell(4, 0, 'Sections');
  DoTableCell(5, 0, 'Fields');
  DoTableCell(6, 0, 'Records');
  DoTableCell(7, 0, 'Deleted');
  for i := 0 to OrderedDataFiles.Count -1 do
  with OrderedDataFiles[i] do
  begin
    DoTableCell(0, i + 1, Caption.Text);
    DoTableCell(1, i + 1, DateTimeToStr(Created));
    DoTableCell(2, i + 1, DateTimeToStr(StructureModifiedDate));
    DoTableCell(3, i + 1, DateTimeToStr(RecModifiedDate));
    DoTableCell(4, i + 1, IntToStr(Sections.Count));
    DoTableCell(5, i + 1, IntToStr(Fields.Count));
    DoTableCell(6, i + 1, IntToStr(Size));
    DoTableCell(7, i + 1, IntToStr(DeletedCount));
  end;
  DoTableFooter('');

  for i := 0 to OrderedDataFiles.Count -1 do
  with OrderedDataFiles[i] do
  begin
    if KeyFields.Count = 0 then continue;

    DoHeading('Key Fields for ' + Caption.Text);
    for j := 0 to KeyFields.Count -1 do
      DoLineText(KeyFields[j].Name + ' - ' + KeyFields[j].Question.Text);
    DoLineText('');
  end;

  OrderedDataFiles.Free;
end;

end.

