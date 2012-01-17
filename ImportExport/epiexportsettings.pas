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

    // Filters
    FromRecord: integer;
    ToRecord:   integer;
    Encoding:   TEpiEncoding;
    Condition:  string;
    ExportDeleted: boolean;
    Fields:     TList;

    // Helpers
    constructor Create; virtual;
    function SanetyCheck: boolean; virtual;
  end;
  TEpiExportSettingClass = class of TEpiExportSetting;

  TEpiCustomValueLabelExportSetting = class(TEpiExportSetting)
  public
    ExportValueLabels: boolean;
  end;

  TEpiStataFieldNamingCase = (fncUpper, fncLower, fncAsIs);

  { TEpiStataExportSetting }

  TEpiStataExportSetting = class(TEpiCustomValueLabelExportSetting)
  public
    FieldNameCase: TEpiStataFieldNamingCase;
    Version:       TEpiStataVersion;
    ExportLines:   TStrings;

    // Helper
    constructor Create; override;
    function SanetyCheck: boolean; override;
  end;

  TEpiDDIExportSetting = class(TEpiCustomValueLabelExportSetting)
  public

  end;

  { TEpiCustomTextExportSettings }

  TEpiCustomTextExportSettings = class(TEpiExportSetting)
  public
    ExportFieldNames: boolean;
    QuoteChar: string;

    constructor Create; override;
  end;

  { TEpiCSVExportSetting }

  TEpiCSVExportSetting = class(TEpiCustomTextExportSettings)
  public
    FieldSeparator: string;
    DateSeparator: string;
    TimeSeparator: string;
    DecimalSeparator: string;
    NewLine: string;

    constructor Create; override;
    function SanetyCheck: boolean; override;
  end;

  TEpiSpreadSheetExportSetting = class(TEpiCustomTextExportSettings)
  public
    SpreadSheetVersion: byte;  //TODO: Export to spreadsheet using TFPSpreadSheet.
  end;

implementation

{ TEpiExportSetting }

constructor TEpiExportSetting.Create;
begin
  Fields := TList.Create;

  // Basic
  ExportFileName := '';
  Doc            := nil;
  DataFileIndex  := -1;

  // Filters
  FromRecord     := -1;
  ToRecord       := -1;
  Encoding       := eeUTF8;
  Condition      := '';
  ExportDeleted  := false;
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

function TEpiCSVExportSetting.SanetyCheck: boolean;
begin
  Result :=
    (inherited SanetyCheck) and
    // FieldSep compare
    (FieldSeparator <> DateSeparator) and
    (FieldSeparator <> TimeSeparator) and
    (FieldSeparator <> DecimalSeparator) and
    (FieldSeparator <> QuoteChar) and
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

