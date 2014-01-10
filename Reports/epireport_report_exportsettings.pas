unit epireport_report_exportsettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_base, epireport_generator_base, epiexportsettings;

type

  EEpiReportExportSettings = class(EEpiReportBaseException);

  { TEpiReportExportSettings }

  TEpiReportExportSettings = class(TEpiReportBase)
  private
    FExportSetting: TEpiExportSetting;
  protected
    procedure DoSanityCheck; override;
  public
    procedure RunReport; override;
    property  ExportSetting: TEpiExportSetting read FExportSetting write FExportSetting;
  end;

implementation

uses
  epireport_report_exportsettings_visitors;

resourcestring
  rsEpiReportExportSettingsNoExportSettingsAssigned =
    'No export setting assigned to export';

{ TEpiReportExportSettings }

procedure TEpiReportExportSettings.DoSanityCheck;
begin
  inherited DoSanityCheck;

  if not Assigned(ExportSetting) then
    DoError(EEpiReportBaseException, rsEpiReportExportSettingsNoExportSettingsAssigned);
end;

procedure TEpiReportExportSettings.RunReport;
var
  OptionCount: Integer;
  CountVisitor: TSettingCountVisitor;
  CurrentSetting: TEpiExportSetting;
  TableVisitor: TSettingsTableOutputVisitor;
begin
  inherited RunReport;

  CountVisitor := TSettingCountVisitor.Create;
  FExportSetting.AcceptVisitor(CountVisitor);
  OptionCount := CountVisitor.Count;
  CountVisitor.Free;

  DoTableHeader('Options:', 2, OptionCount + 1);
  DoTableCell(0, 0, 'Option');
  DoTableCell(1, 0, 'Value');

  TableVisitor := TSettingsTableOutputVisitor.Create;
  TableVisitor.VisitorTraversal := vtInheritedFirst;
  TableVisitor.RowIndex := 1;
  TableVisitor.ReportGenerator := FReportGenerator;
  FExportSetting.AcceptVisitor(TableVisitor);

  DoTableFooter('');
end;

end.

