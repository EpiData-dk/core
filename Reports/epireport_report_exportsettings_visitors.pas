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
    FPreviousSetting: TEpiExportSetting;
    procedure DoTableCell(Const Col: Integer; Const Text: string);
    function  BoolToStr(B: Boolean; Unused: Boolean): string;
    function  CanVisit(Const ExportSetting: TEpiExportSetting;
      ExportSettingClass: TEpiExportSettingClass): Boolean;
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
    FPreviousSettings: TEpiExportSetting;
    procedure DebugStr(Const Msg: string);
    procedure AdditionalVisit(Const ExportSetting: TEpiExportSetting);
    procedure CommonVisit(Const IncCount: Integer; CompareClass: TEpiExportSettingClass);
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

function TSettingsTableOutputVisitor.BoolToStr(B: Boolean; Unused: Boolean
  ): string;
begin
  if B then
    Result := 'Yes'
  else
    Result := 'No';
end;

function TSettingsTableOutputVisitor.CanVisit(
  const ExportSetting: TEpiExportSetting;
  ExportSettingClass: TEpiExportSettingClass): Boolean;
begin
  result := not(
    (Assigned(FPreviousSetting)) and
    (FPreviousSetting.InheritsFrom(ExportSettingClass))
  );
end;

constructor TSettingsTableOutputVisitor.Create;
begin
  inherited Create;
end;

procedure TSettingsTableOutputVisitor.Visit(
  const ExportSetting: TEpiExportSetting);
begin
  if Assigned(ExportSetting.AdditionalExportSettings) then
  begin
    FPreviousSetting := ExportSetting;
    ExportSetting.AdditionalExportSettings.AcceptVisitor(Self);
    FPreviousSetting := nil;
  end;

  if not CanVisit(ExportSetting, TEpiExportSetting) then exit;

  DoTableCell(0, 'Records Exported');
  DoTableCell(1, Format('%d - %d', [ExportSetting.FromRecord+1, ExportSetting.ToRecord+1]));
  Inc(FRowIndex);

  DoTableCell(0, 'Encoding');
  DoTableCell(1, EpiEncodingToString[ExportSetting.Encoding]);
  Inc(FRowIndex);

  DoTableCell(0, 'Export records marked for deletion');
  DoTableCell(1, BoolToStr(ExportSetting.ExportDeleted, true));
  Inc(FRowIndex);
end;

procedure TSettingsTableOutputVisitor.Visit(
  const ExportSetting: TEpiCustomValueLabelExportSetting);
begin
  if not CanVisit(ExportSetting, TEpiCustomValueLabelExportSetting) then exit;

  DoTableCell(0, 'Export Valuelabels');
  DoTableCell(1, BoolToStr(ExportSetting.ExportValueLabels, true));
  Inc(FRowIndex);
end;

procedure TSettingsTableOutputVisitor.Visit(
  const ExportSetting: TEpiStataExportSetting);
begin
  if not CanVisit(ExportSetting, TEpiStataExportSetting) then exit;

  DoTableCell(0, 'Field name case');
  DoTableCell(1, EpiFieldNamingCaseToString[ExportSetting.FieldNameCase]);
  Inc(FRowIndex);

  DoTableCell(0, 'Stata Version');
  DoTableCell(1, EpiStataVersionToString(ExportSetting.Version));
  Inc(FRowIndex);
end;

procedure TSettingsTableOutputVisitor.Visit(
  const ExportSetting: TEpiCSVExportSetting);
var
  S: String;
begin
  if not CanVisit(ExportSetting, TEpiCSVExportSetting) then exit;

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
  S := ExportSetting.NewLine;
  if S[1] = #10
  then
    S := 'Linux'
  else
    if Length(S) = 1
    then
      S := 'Max'
    else
      S := 'Windows';

  DoTableCell(1, S);
  Inc(FRowIndex);

  DoTableCell(0, 'Fixed Format');
  DoTableCell(1, BoolToStr(ExportSetting.FixedFormat, true));
  Inc(FRowIndex);
end;

procedure TSettingsTableOutputVisitor.Visit(
  const ExportSetting: TEpiCustomTextExportSettings);
begin
  if not CanVisit(ExportSetting, TEpiCustomTextExportSettings) then exit;

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
  if not CanVisit(ExportSetting, TEpiDDIExportSetting) then exit;

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
  if not CanVisit(ExportSetting, TEpiSPSSExportSetting) then exit;

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

procedure TSettingCountVisitor.AdditionalVisit(
  const ExportSetting: TEpiExportSetting);
begin
  if Assigned(ExportSetting.AdditionalExportSettings) then
  begin
    FPreviousSettings := ExportSetting;
    ExportSetting.AdditionalExportSettings.AcceptVisitor(Self);
    FPreviousSettings := nil;
  end;
end;

procedure TSettingCountVisitor.CommonVisit(const IncCount: Integer;
  CompareClass: TEpiExportSettingClass);
var
  AddCount: Boolean;
begin
  AddCount := true;
  if (Assigned(FPreviousSettings)) and
     (FPreviousSettings.InheritsFrom(CompareClass))
  then
    AddCount := false;

  if AddCount then
    inc(FCount, IncCount);
end;

constructor TSettingCountVisitor.Create;
begin
  inherited Create;
  FCount := 0;
end;

procedure TSettingCountVisitor.Visit(const ExportSetting: TEpiExportSetting);
begin
  CommonVisit(3, TEpiExportSetting);
  AdditionalVisit(ExportSetting);
end;

procedure TSettingCountVisitor.Visit(
  const ExportSetting: TEpiCustomValueLabelExportSetting);
begin
  CommonVisit(1, TEpiCustomValueLabelExportSetting);
end;

procedure TSettingCountVisitor.Visit(
  const ExportSetting: TEpiStataExportSetting);
begin
  CommonVisit(2, TEpiStataExportSetting);
end;

procedure TSettingCountVisitor.Visit(const ExportSetting: TEpiCSVExportSetting);
begin
  CommonVisit(6, TEpiCSVExportSetting);
end;

procedure TSettingCountVisitor.Visit(
  const ExportSetting: TEpiCustomTextExportSettings);
begin
  CommonVisit(3, TEpiCustomTextExportSettings);
end;

procedure TSettingCountVisitor.Visit(const ExportSetting: TEpiDDIExportSetting);
var
  IncCount: Integer;
begin
  IncCount := 4;
  if ExportSetting.RenameVariablesPrefix <> '' then
    inc(IncCount);

  CommonVisit(IncCount, TEpiDDIExportSetting);
end;

procedure TSettingCountVisitor.Visit(const ExportSetting: TEpiEPXExportSetting);
begin
  // Nothing to output
  CommonVisit(0, TEpiEPXExportSetting);
end;

procedure TSettingCountVisitor.Visit(const ExportSetting: TEpiSASExportSetting);
begin
  // Nothing to output
  CommonVisit(0, TEpiSASExportSetting);
end;

procedure TSettingCountVisitor.Visit(const ExportSetting: TEpiSPSSExportSetting
  );
begin
  CommonVisit(1, TEpiSPSSExportSetting);
end;

end.

