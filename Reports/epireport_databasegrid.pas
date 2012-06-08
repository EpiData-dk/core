unit epireport_databasegrid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_datafilesreport_base, epireport_generator_base,
  epidocument, epidatafiles;

type

  { TEpiReportDataSetsGrid }

  TEpiReportDataSetsGrid = class(TEpiReportDataFilesBase)
  private
    FRowNo: integer;
  protected
    procedure PrintDataFile(Const DataFile: TEpiDataFile); override;
  public
    procedure   RunReport; override;
  end;

implementation

procedure TEpiReportDataSetsGrid.PrintDataFile(const DataFile: TEpiDataFile);
begin
  with DataFile do
  begin
    DoTableCell(0, FRowNo, Caption.Text);
    DoTableCell(1, FRowNo, DateTimeToStr(Created));
    DoTableCell(2, FRowNo, IntToStr(Sections.Count));
    DoTableCell(3, FRowNo, IntToStr(Fields.Count));
    DoTableCell(4, FRowNo, IntToStr(Size));
    DoTableCell(5, FRowNo, IntToStr(DeletedCount));
  end;
end;

procedure TEpiReportDataSetsGrid.RunReport;
var
  i: Integer;
begin
  inherited RunReport;

  DoSection('List of DataSets:');

  DoTableHeader('', 6, EpiDataFiles.Count + 1);

  DoTableCell(0, 0, 'Dataset');
  DoTableCell(1, 0, 'Created');
  DoTableCell(2, 0, 'Sections');
  DoTableCell(3, 0, 'Fields');
  DoTableCell(4, 0, 'Records');
  DoTableCell(5, 0, 'Deleted');

  for i := 0 to EpiDataFiles.Count - 1 do
  begin
    FRowNo := i + 1;
    PrintDataFile(EpiDataFiles[i]);
  end;

  DoTableFooter('');
end;

end.

