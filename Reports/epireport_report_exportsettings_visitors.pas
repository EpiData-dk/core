unit epireport_report_exportsettings_visitors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epiexportsettings, epireport_generator_base;

type

  { TSettingsTableOutputVisitor }

  TSettingsTableOutputVisitor = class(TEpiExportSettingCustomVisitor)
  private
    FReportGenerator: TEpiReportGeneratorBase;
    FRowIndex: integer;
  public
    constructor Create; override;
    procedure Visit(const ExportSetting: TEpiExportSetting); override; overload;
    procedure Visit(const ExportSetting: TEpiCustomValueLabelExportSetting);
      override; overload;
    procedure Visit(const ExportSetting: TEpiStataExportSetting); override;
      overload;
    procedure Visit(const ExportSetting: TEpiCSVExportSetting); override;
      overload;
    procedure Visit(const ExportSetting: TEpiCustomTextExportSettings);
      override; overload;
    procedure Visit(const ExportSetting: TEpiDDIExportSetting); override;
      overload;
    procedure Visit(const ExportSetting: TEpiEPXExportSetting); override;
      overload;
    procedure Visit(const ExportSetting: TEpiSASExportSetting); override;
      overload;
    procedure Visit(const ExportSetting: TEpiSPSSExportSetting); override;
      overload;
  public
    property RowIndex: integer read FRowIndex write FRowIndex;
    property ReportGenerator: TEpiReportGeneratorBase read FReportGenerator write FReportGenerator;
  end;



  { TSettingCountVisitor }

  TSettingCountVisitor = class(TEpiExportSettingCustomVisitor)
  private
    FCount: integer;
    procedure DebugStr(Const Msg: string);
  public
    constructor Create; override;
    procedure Visit(const ExportSetting: TEpiExportSetting); override; overload;
    procedure Visit(const ExportSetting: TEpiCustomValueLabelExportSetting);
      override; overload;
    procedure Visit(const ExportSetting: TEpiStataExportSetting); override;
      overload;
    procedure Visit(const ExportSetting: TEpiCSVExportSetting); override;
      overload;
    procedure Visit(const ExportSetting: TEpiCustomTextExportSettings);
      override; overload;
    procedure Visit(const ExportSetting: TEpiDDIExportSetting); override;
      overload;
    procedure Visit(const ExportSetting: TEpiEPXExportSetting); override;
      overload;
    procedure Visit(const ExportSetting: TEpiSASExportSetting); override;
      overload;
    procedure Visit(const ExportSetting: TEpiSPSSExportSetting); override;
      overload;
  public
    property Count: Integer read FCount;
  end;

implementation

uses
  epieximtypes;

{ TSettingsTableOutputVisitor }

constructor TSettingsTableOutputVisitor.Create;
begin
  inherited Create;
end;

procedure TSettingsTableOutputVisitor.Visit(
  const ExportSetting: TEpiExportSetting);
begin
  FReportGenerator.TableCell(
    'From',
    0,
    RowIndex);

  FReportGenerator.TableCell(
    IntToStr(ExportSetting.FromRecord),
    1,
    RowIndex);

  Inc(FRowIndex);

  FReportGenerator.TableCell(
    'To',
    0,
    RowIndex);

  FReportGenerator.TableCell(
    IntToStr(ExportSetting.ToRecord),
    1,
    RowIndex);

  Inc(FRowIndex);

  FReportGenerator.TableCell(
    'Encoding',
    0,
    RowIndex);

  FReportGenerator.TableCell(
    EpiEncodingToString[ExportSetting.Encoding],
    1,
    RowIndex);

  Inc(FRowIndex);

  FReportGenerator.TableCell(
    'Export Deleted',
    0,
    RowIndex);

  FReportGenerator.TableCell(
    BoolToStr(ExportSetting.ExportDeleted, true),
    1,
    RowIndex);

  Inc(FRowIndex);
end;

procedure TSettingsTableOutputVisitor.Visit(
  const ExportSetting: TEpiCustomValueLabelExportSetting);
begin

end;

procedure TSettingsTableOutputVisitor.Visit(
  const ExportSetting: TEpiStataExportSetting);
begin

end;

procedure TSettingsTableOutputVisitor.Visit(
  const ExportSetting: TEpiCSVExportSetting);
begin

end;

procedure TSettingsTableOutputVisitor.Visit(
  const ExportSetting: TEpiCustomTextExportSettings);
begin

end;

procedure TSettingsTableOutputVisitor.Visit(
  const ExportSetting: TEpiDDIExportSetting);
begin

end;

procedure TSettingsTableOutputVisitor.Visit(
  const ExportSetting: TEpiEPXExportSetting);
begin

end;

procedure TSettingsTableOutputVisitor.Visit(
  const ExportSetting: TEpiSASExportSetting);
begin

end;

procedure TSettingsTableOutputVisitor.Visit(
  const ExportSetting: TEpiSPSSExportSetting);
begin

end;

{ TSetttingCountVisitor }

procedure TSettingCountVisitor.DebugStr(const Msg: string);
begin
{  if IsConsole then
    Writeln(Msg);}
end;

constructor TSettingCountVisitor.Create;
begin
  inherited Create;
  FCount := 0;
end;

procedure TSettingCountVisitor.Visit(const ExportSetting: TEpiExportSetting);
begin
  DebugStr('TEpiExportSetting');
  inc(FCount, 4);
end;

procedure TSettingCountVisitor.Visit(
  const ExportSetting: TEpiCustomValueLabelExportSetting);
begin
  DebugStr('TEpiCustomValueLabelExportSetting');
  inc(FCount, 1);
end;

procedure TSettingCountVisitor.Visit(
  const ExportSetting: TEpiStataExportSetting);
begin
  DebugStr('TEpiStataExportSetting');
  inc(FCount, 2);
end;

procedure TSettingCountVisitor.Visit(const ExportSetting: TEpiCSVExportSetting);
begin
  DebugStr('TEpiCSVExportSetting');
  inc(FCount, 6);
end;

procedure TSettingCountVisitor.Visit(
  const ExportSetting: TEpiCustomTextExportSettings);
begin
  DebugStr('TEpiCustomTextExportSettings');
  inc(FCount, 3);
end;

procedure TSettingCountVisitor.Visit(const ExportSetting: TEpiDDIExportSetting);
begin
  DebugStr('TEpiDDIExportSetting');
  inc(FCount, 8);
end;

procedure TSettingCountVisitor.Visit(const ExportSetting: TEpiEPXExportSetting);
begin
  DebugStr('TEpiEPXExportSetting');
end;

procedure TSettingCountVisitor.Visit(const ExportSetting: TEpiSASExportSetting);
begin
  DebugStr('TEpiSASExportSetting');
end;

procedure TSettingCountVisitor.Visit(const ExportSetting: TEpiSPSSExportSetting
  );
begin
  DebugStr('TEpiSPSSExportSetting');
  inc(FCount, 1);
end;

end.

