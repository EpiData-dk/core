unit epireport_report_mainheader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_base, epiopenfile;

type

  EEpiReportMainHeader = class(Exception);

  { TEpiReportMainHeader }

  TEpiReportMainHeader = class(TEpiReportBase)
  private
    FProjectList: TEpiDocumentFileList;
    FTitle: string;
  protected
    procedure DoSanityCheck; override;
  public
    procedure RunReport; override;
    property ProjectList: TEpiDocumentFileList read FProjectList write FProjectList;
    property Title: string read FTitle write FTitle;
  end;

implementation

resourcestring
  SEpiReportMainHeaderNoProjects = 'EpiReport: No projects assigned to main header.';


{ TEpiReportMainHeader }

procedure TEpiReportMainHeader.DoSanityCheck;
begin
  inherited DoSanityCheck;
  if not Assigned(FProjectList) then
    DoError(EEpiReportMainHeader, SEpiReportMainHeaderNoProjects);
  if FProjectList.Count = 0 then
    DoError(EEpiReportMainHeader, SEpiReportMainHeaderNoProjects);
end;

procedure TEpiReportMainHeader.RunReport;
var
  S: String;
  i: Integer;
begin
  inherited RunReport;

  DoSection(
    'Report:  ' + FTitle + LineEnding +
    'Created: ' + DateTimeToStr(Now)
  );

  DoLineText('');
  DoTableHeader('', 2, ProjectList.Count, []);
  for i := 0 to ProjectList.Count - 1 do
  begin
    DoTableCell(0, i, 'File ' + IntToStr(i+1) + ':');
    DoTableCell(1, i, ProjectList[i].FileName);
  end;
  DoTableFooter('');
end;

end.

