unit epiexportsettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epieximtypes, epidocument;

type

  { TEpiExportSetting }

  TEpiExportSetting = class
  public
    // Basic properties
    ExportFileName: string;
    Doc: TEpiDocument;
    DataFileIndex: integer;
    // For use with multi-file export (eg. SPSS, SAS, DDI, ...)
    // (usually used for secondary file export settings, assigned during export).
    AdditionalExportSettings: TEpiExportSetting;

    // Filters
    FromRecord: integer;
    ToRecord:   integer;
    Encoding:   TEpiEncoding;
    Condition:  string;
    ExportDeleted: boolean;
    Fields:     TList;

    // Helpers
    constructor Create; virtual;
    destructor Destroy; override;
    procedure   Assign(Const ASettings: TEpiExportSetting); virtual;
    function SanetyCheck: boolean; virtual;
  end;
  TEpiExportSettingClass = class of TEpiExportSetting;

  { TEpiCustomValueLabelExportSetting }

  TEpiCustomValueLabelExportSetting = class(TEpiExportSetting)
  public
    ExportValueLabels: boolean;
    procedure Assign(Const ASettings: TEpiExportSetting); override;
  end;

  { TEpiStataExportSetting }

  TEpiStataExportSetting = class(TEpiCustomValueLabelExportSetting)
  public
    FieldNameCase: TEpiFieldNamingCase;
    Version:       TEpiStataVersion;
    ExportLines:   TStrings;

    // Helper
    constructor Create; override;
    procedure Assign(Const ASettings: TEpiExportSetting); override;
    function SanetyCheck: boolean; override;
  end;


  { TEpiSPSSExportSetting }

  TEpiSPSSExportSetting = class(TEpiCustomValueLabelExportSetting)
  public

  end;

  { TEpiSASExportSetting }

  TEpiSASExportSetting = class(TEpiCustomValueLabelExportSetting)
  public

  end;

  { TEpiDDIExportSetting }

  TEpiDDIExportSetting = class(TEpiCustomValueLabelExportSetting)
  { Funding }
  { Other }
  private
    FSoftwareName: string;
    FSoftwareVersion: string;
    FVersion: string;
  public
    property SoftwareName: string read FSoftwareName write FSoftwareName;
    property SoftwareVersion: string read FSoftwareVersion write FSoftwareVersion;
    property Version: string read FVersion write FVersion;

  { Common }
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(const ASettings: TEpiExportSetting); override;
    function SanetyCheck: boolean; override;
  end;

  { TEpiCustomTextExportSettings }

  TEpiCustomTextExportSettings = class(TEpiExportSetting)
  public
    ExportFieldNames: boolean;
    QuoteChar: string;

    constructor Create; override;
    procedure Assign(const ASettings: TEpiExportSetting); override;
  end;

  { TEpiCSVExportSetting }

  TEpiCSVExportSetting = class(TEpiCustomTextExportSettings)
  public
    FieldSeparator: string;
    DateSeparator: string;
    TimeSeparator: string;
    DecimalSeparator: string;
    NewLine: string;
    FixedFormat: boolean;

    constructor Create; override;
    procedure Assign(const ASettings: TEpiExportSetting); override;
    function SanetyCheck: boolean; override;
  end;

{  TEpiSpreadSheetExportSetting = class(TEpiCustomTextExportSettings)
  public
    SpreadSheetVersion: byte;  //TODO: Export to spreadsheet using TFPSpreadSheet.
  end;}

implementation

{ TEpiDDIExportSetting }

constructor TEpiDDIExportSetting.Create;
begin
  inherited Create;
//  FCoverTopKeyWords := TStringList.Create;
//  FCoverTopSubjects := TStringList.Create;
end;

destructor TEpiDDIExportSetting.Destroy;
begin
//  FCoverTopKeyWords.Free;
//  FCoverTopSubjects.Free;
  inherited Destroy;
end;

procedure TEpiDDIExportSetting.Assign(const ASettings: TEpiExportSetting);
begin
  inherited Assign(ASettings);
  // TODO
end;

function TEpiDDIExportSetting.SanetyCheck: boolean;
begin
  Result :=
    (inherited SanetyCheck);
end;

{ TEpiCustomValueLabelExportSetting }

procedure TEpiCustomValueLabelExportSetting.Assign(
  const ASettings: TEpiExportSetting);
begin
  inherited Assign(ASettings);
  if not (ASettings is TEpiCustomTextExportSettings) then exit;

  ExportValueLabels := TEpiCustomValueLabelExportSetting(ASettings).ExportValueLabels;
end;

{ TEpiExportSetting }

constructor TEpiExportSetting.Create;
begin
  Fields := TList.Create;

  // Basic
  ExportFileName := '';
  Doc            := nil;
  DataFileIndex  := -1;
  AdditionalExportSettings := nil;

  // Filters
  FromRecord     := -1;
  ToRecord       := -1;
  Encoding       := eeUTF8;
  Condition      := '';
  ExportDeleted  := false;
end;

destructor TEpiExportSetting.Destroy;
begin
//  Fields.Free;
  if Assigned(AdditionalExportSettings) then
    AdditionalExportSettings.Free;
  inherited Destroy;
end;

procedure TEpiExportSetting.Assign(const ASettings: TEpiExportSetting);
begin
  if not Assigned(ASettings) then exit;
  ExportFileName := ASettings.ExportFileName;
  Doc            := ASettings.Doc;
  DataFileIndex  := ASettings.DataFileIndex;

  // Filters
  FromRecord     := ASettings.FromRecord;
  ToRecord       := ASettings.ToRecord;
  Encoding       := ASettings.Encoding;
  Condition      := ASettings.Condition;
  ExportDeleted  := ASettings.ExportDeleted;
  Fields.Assign(ASettings.Fields);
end;

function TEpiExportSetting.SanetyCheck: boolean;
begin
  if not (Assigned(Doc)) and
     not ((DataFileIndex >= 0) and (DataFileIndex < Doc.DataFiles.Count)) then exit(False);

  if FromRecord = -1 then FromRecord := 0;
  if ToRecord   = -1 then ToRecord := Doc.DataFiles[DataFileIndex].Size - 1;

  result :=
    (ExportFileName <> '') and
    (Fields.Count > 0) and
    (FromRecord >= 0) and
    (ToRecord < Doc.DataFiles[DataFileIndex].Size);
end;

{ TEpiStataExportSetting }

constructor TEpiStataExportSetting.Create;
begin
  inherited Create;

  ExportLines   := TStringList.Create;
  FieldNameCase := fncAsIs;
  Version       := dta10;
end;

procedure TEpiStataExportSetting.Assign(const ASettings: TEpiExportSetting);
begin
  inherited Assign(ASettings);

  if not (ASettings is TEpiStataExportSetting) then exit;

  FieldNameCase := TEpiStataExportSetting(ASettings).FieldNameCase;
  Version       := TEpiStataExportSetting(ASettings).Version;
  ExportLines.Assign(TEpiStataExportSetting(ASettings).ExportLines);
end;

function TEpiStataExportSetting.SanetyCheck: boolean;
begin
  Result := inherited SanetyCheck;
end;

{ TEpiCustomTextExportSettings }

constructor TEpiCustomTextExportSettings.Create;
begin
  inherited Create;
  QuoteChar := '"';
end;

procedure TEpiCustomTextExportSettings.Assign(const ASettings: TEpiExportSetting
  );
begin
  inherited Assign(ASettings);
  if not (ASettings is TEpiCustomTextExportSettings) then exit;

  ExportFieldNames := TEpiCustomTextExportSettings(ASettings).ExportFieldNames;
  QuoteChar        := TEpiCustomTextExportSettings(ASettings).QuoteChar;
end;

{ TEpiCSVExportSetting }

constructor TEpiCSVExportSetting.Create;
begin
  inherited Create;

  FieldSeparator    := ',';
  DateSeparator     := DefaultFormatSettings.DateSeparator;
  TimeSeparator     := DefaultFormatSettings.TimeSeparator;
  DecimalSeparator  := DefaultFormatSettings.DecimalSeparator;
  NewLine           := LineEnding;
end;

procedure TEpiCSVExportSetting.Assign(const ASettings: TEpiExportSetting);
begin
  inherited Assign(ASettings);
  if not (ASettings is TEpiCSVExportSetting) then exit;

  with (TEpiCSVExportSetting(ASettings)) do
  begin
    Self.FieldSeparator   := FieldSeparator;
    Self.DateSeparator    := DateSeparator;
    Self.TimeSeparator    := TimeSeparator;
    Self.DecimalSeparator := DecimalSeparator;
    Self.NewLine          := NewLine;
    Self.FixedFormat      := FixedFormat;
  end;
end;

function TEpiCSVExportSetting.SanetyCheck: boolean;
begin
  Result :=
    (inherited SanetyCheck) and
    // FieldSep compare
    (
      (not FixedFormat) and
        (
          (FieldSeparator <> DateSeparator) and
          (FieldSeparator <> TimeSeparator) and
          (FieldSeparator <> DecimalSeparator) and
          (FieldSeparator <> QuoteChar)
        )
      or
      ( FixedFormat )
    ) and
    // Date compare
    (DateSeparator  <> TimeSeparator) and
    (DateSeparator  <> DecimalSeparator) and
    (DateSeparator  <> QuoteChar) and
    // Time compare
    (TimeSeparator  <> DecimalSeparator) and
    (TimeSeparator  <> QuoteChar) and
    // Decimal compare
    (DecimalSeparator <> QuoteChar);
end;

end.

