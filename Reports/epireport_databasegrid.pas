unit epireport_databasegrid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_base, epireport_htmlgenerator,
  epidocument, epidatafiles;

type

  { TEpiReportDataSetsGrid }

  TEpiReportDataSetsGrid = class(TEpiReportBase)
  private
    FEpiDataFiles: TEpiDataFiles;
    procedure   PrintDataSet(Const DataFile: TEpiDataFile; Const RowNo: Integer);
  public
    constructor Create(const AEpiDocument: TEpiDocument); override;
    procedure   RunReport; override;
    property    DataFiles: TEpiDataFiles read FEpiDataFiles;
  end;

  { TEpiReportDataSetsGridHtml }

  TEpiReportDataSetsGridHtml = class(TEpiReportDataSetsGrid)
  private
    FHtmlGenerator: TEpiReportHTMLGenerator;
    FCompleteHtml: boolean;
  protected
    function GetReportText: string; override;
  public
    constructor Create(const AEpiDocument: TEpiDocument; Const CompleteHtml: boolean);
    destructor Destroy; override;
    procedure RunReport; override;
  end;

implementation

procedure TEpiReportDataSetsGrid.PrintDataSet(const DataFile: TEpiDataFile;
  const RowNo: Integer);
begin
  with DataFile do
  begin
    DoTableCell(0, RowNo, Caption.Text);
    DoTableCell(1, RowNo, DateTimeToStr(Created));
    DoTableCell(2, RowNo, IntToStr(Sections.Count));
    DoTableCell(3, RowNo, IntToStr(Fields.Count));
    DoTableCell(4, RowNo, IntToStr(Size));
    DoTableCell(5, RowNo, IntToStr(DeletedCount));
  end;
end;

constructor TEpiReportDataSetsGrid.Create(const AEpiDocument: TEpiDocument);
begin
  inherited Create(AEpiDocument);
  FEpiDataFiles := AEpiDocument.DataFiles;
end;

procedure TEpiReportDataSetsGrid.RunReport;
var
  i: Integer;
begin
  inherited RunReport;

  DoSection('List of DataSets:');

  DoTableHeader('', 6, DataFiles.Count + 1);

  DoTableCell(0, 0, 'Dataset');
  DoTableCell(1, 0, 'Created');
  DoTableCell(2, 0, 'Sections');
  DoTableCell(3, 0, 'Fields');
  DoTableCell(4, 0, 'Records');
  DoTableCell(5, 0, 'Deleted');

  for i := 0 to DataFiles.Count - 1 do
    PrintDataSet(DataFiles[i], i+1);

  DoTableFooter('');
end;

{ TEpiReportDataSetsGridHtml }

function TEpiReportDataSetsGridHtml.GetReportText: string;
begin
  Result := FHtmlGenerator.GetReportText;
end;

constructor TEpiReportDataSetsGridHtml.Create(const AEpiDocument: TEpiDocument;
  const CompleteHtml: boolean);
begin
  inherited Create(AEpiDocument);
  FHtmlGenerator := TEpiReportHTMLGenerator.Create(Self);
  FCompleteHtml := CompleteHtml;
end;

destructor TEpiReportDataSetsGridHtml.Destroy;
begin
  FHtmlGenerator.Free;
  inherited Destroy;
end;

procedure TEpiReportDataSetsGridHtml.RunReport;
begin
  if FCompleteHtml then
    FHtmlGenerator.InitHtml('List of Datesets');
  inherited RunReport;
  if FCompleteHtml then
    FHtmlGenerator.CloseHtml;
end;

end.

