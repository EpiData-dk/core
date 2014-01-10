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
    procedure DoTableCell(Const Col: Integer; Const Text: string);
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
  epieximtypes, epireport_types;

{ TSettingsTableOutputVisitor }

procedure TSettingsTableOutputVisitor.DoTableCell(const Col: Integer;
  const Text: string);
var
  Adjust: TEpiReportGeneratorTableCellAdjustment;
begin
  Adjust := tcaAutoAdjust;
  if Col = 1 then
    Adjust := tcaRightAdjust;
  FReportGenerator.TableCell(Text, Col, RowIndex, Adjust);
end;

constructor TSettingsTableOutputVisitor.Create;
begin
  inherited Create;
end;

procedure TSettingsTableOutputVisitor.Visit(
  const ExportSetting: TEpiExportSetting);
begin
  DoTableCell(0, 'From');
  DoTableCell(1, IntToStr(ExportSetting.FromRecord));
  Inc(FRowIndex);

  DoTableCell(0, 'To');
  DoTableCell(1, IntToStr(ExportSetting.ToRecord));
  Inc(FRowIndex);

  DoTableCell(0, 'Encoding');
  DoTableCell(1, EpiEncodingToString[ExportSetting.Encoding]);
  Inc(FRowIndex);

  DoTableCell(0, 'Export Deleted');
  DoTableCell(1, BoolToStr(ExportSetting.ExportDeleted, true));
  Inc(FRowIndex);
end;

procedure TSettingsTableOutputVisitor.Visit(
  const ExportSetting: TEpiCustomValueLabelExportSetting);
begin
  DoTableCell(0, 'Export Valuelabels');
  DoTableCell(1, BoolToStr(ExportSetting.ExportValueLabels, true));
  Inc(FRowIndex);
end;

procedure TSettingsTableOutputVisitor.Visit(
  const ExportSetting: TEpiStataExportSetting);
begin
  DoTableCell(0, 'Field name case');
  DoTableCell(1, EpiFieldNamingCaseToString[ExportSetting.FieldNameCase]);
  Inc(FRowIndex);

  DoTableCell(0, 'Stata Version');
  DoTableCell(1, EpiStataVersionToString(ExportSetting.Version));
  Inc(FRowIndex);
end;

procedure TSettingsTableOutputVisitor.Visit(
  const ExportSetting: TEpiCSVExportSetting);
begin
  DoTableCell(0, 'Field Separator');
  DoTableCell(1, ExportSetting.FieldSeparator);
  Inc(FRowIndex);

  DoTableCell(0, 'Date Separator');
  DoTableCell(1, ExportSetting.DateSeparator);
  Inc(FRowIndex);

  DoTableCell(0, 'Time Separator');
  DoTableCell(1, ExportSetting.TimeSeparator);
  Inc(FRowIndex);

  DoTableCell(0, 'Decimal Separtor');
  DoTableCell(1, ExportSetting.DecimalSeparator);
  Inc(FRowIndex);

  DoTableCell(0, 'New Line Style');
  DoTableCell(1, '');
  Inc(FRowIndex);

  DoTableCell(0, 'Fixed Format');
  DoTableCell(1, BoolToStr(ExportSetting.FixedFormat, true));
  Inc(FRowIndex);
end;

procedure TSettingsTableOutputVisitor.Visit(
  const ExportSetting: TEpiCustomTextExportSettings);
begin
  DoTableCell(0, 'Byte Order Mark');
  DoTableCell(1, BoolToStr(ExportSetting.ByteOrderMark, true));
  Inc(FRowIndex);

  DoTableCell(0, 'Export Field Names (first row)');
  DoTableCell(1, BoolToStr(ExportSetting.ExportFieldNames, true));
  Inc(FRowIndex);

  DoTableCell(0, 'Quote Character');
  DoTableCell(1, ExportSetting.QuoteChar);
  Inc(FRowIndex);
end;

procedure TSettingsTableOutputVisitor.Visit(
  const ExportSetting: TEpiDDIExportSetting);
begin
  DoTableCell(0, 'Remove Value Labels indicating missing');
  DoTableCell(1, BoolToStr(ExportSetting.RemoveMissingVL, true));
  Inc(FRowIndex);

  DoTableCell(0, 'Use <UserID> for filter');
  DoTableCell(1, BoolToStr(ExportSetting.FilterTagIsUserId, true));
  Inc(FRowIndex);

  DoTableCell(0, 'Use section caption as <Question Text> for subquestions');
  DoTableCell(1, BoolToStr(ExportSetting.SectionCaptionIsQText, true));
  Inc(FRowIndex);

  DoTableCell(0, 'Rename variables');
  DoTableCell(1, BoolToStr(ExportSetting.RenameVariablesPrefix <> '', true));
  Inc(FRowIndex);

  if ExportSetting.RenameVariablesPrefix <> '' then
    begin
      DoTableCell(0, 'Variable prefix');
      DoTableCell(1, ExportSetting.RenameVariablesPrefix);
      Inc(FRowIndex);
    end;
end;

procedure TSettingsTableOutputVisitor.Visit(
  const ExportSetting: TEpiEPXExportSetting);
begin
  // Do nothing
end;

procedure TSettingsTableOutputVisitor.Visit(
  const ExportSetting: TEpiSASExportSetting);
begin
  // Do nothing
end;

procedure TSettingsTableOutputVisitor.Visit(
  const ExportSetting: TEpiSPSSExportSetting);
begin
  DoTableCell(0, 'Delimiter');
  DoTableCell(1, ExportSetting.Delimiter);
  Inc(FRowIndex);
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
  inc(FCount, 4);

  if ExportSetting.RenameVariablesPrefix <> '' then
    inc(FCount);
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

