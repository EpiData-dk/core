unit epiexportsettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epieximtypes, epidocument, epidatafiles;

type

  TEpiExportSettingCustomVisitor = class;

  TEpiExportDatafileSettings = class
  private
    FCreatedStream: boolean;
  public
    Datafile: TEpiDataFile;

    ExportStream: TStream;
    ExportFileName: string;

    FromRecord: integer;
    ToRecord:   integer;

    Condition:  string;
    Fields:     TList;
    // For use with multi-file export (eg. SPSS, SAS, DDI, ...)
    // (usually used for secondary file export settings, assigned during export).
    AdditionalExportSettings: TEpiExportDatafileSettings;
  end;


  { TEpiExportSetting }

  TEpiExportSetting = class
  private
    FCreatedStream: boolean;
  public
    // Basic properties
    ExportStream: TStream;
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
    DataformSettings: TList;

    // Helpers
    constructor Create; virtual;
    destructor  Destroy; override;
    procedure   Assign(Const ASettings: TEpiExportSetting); virtual;
    function    SanetyCheck: boolean; virtual;
  public
    // Visitor Pattern
    procedure   AcceptVisitor(Const Visitor: TEpiExportSettingCustomVisitor); virtual;
  end;
  TEpiExportSettingClass = class of TEpiExportSetting;

  { TEpiCustomValueLabelExportSetting }

  TEpiCustomValueLabelExportSetting = class(TEpiExportSetting)
  public
    ExportValueLabels: boolean;
    procedure Assign(Const ASettings: TEpiExportSetting); override;
  public
    // Visitor Pattern
    procedure   AcceptVisitor(Const Visitor: TEpiExportSettingCustomVisitor); override;
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
  public
    // Visitor Pattern
    procedure   AcceptVisitor(Const Visitor: TEpiExportSettingCustomVisitor); override;
  end;


  { TEpiSPSSExportSetting }

  TEpiSPSSExportSetting = class(TEpiCustomValueLabelExportSetting)
  public
    Delimiter: char;
  public
    // Visitor Pattern
    procedure   AcceptVisitor(Const Visitor: TEpiExportSettingCustomVisitor); override;
  end;

  { TEpiSASExportSetting }

  TEpiSASExportSetting = class(TEpiCustomValueLabelExportSetting)
  public
    // Visitor Pattern
    procedure   AcceptVisitor(Const Visitor: TEpiExportSettingCustomVisitor); override;
  end;


  { TEpiCustomCompleteProjectExportSetting }

  TEpiCustomCompleteProjectExportSetting = class(TEpiCustomValueLabelExportSetting)
  private
    FExportCompleteProject: boolean;
  public
    property ExportCompleteProject: boolean read FExportCompleteProject write FExportCompleteProject;
  public
    constructor Create; override;
    procedure Assign(const ASettings: TEpiExportSetting); override;
  public
    // Visitor Pattern
    procedure   AcceptVisitor(Const Visitor: TEpiExportSettingCustomVisitor); override;
  end;

  { TEpiDDIExportSetting }

  TEpiDDIExportSetting = class(TEpiCustomCompleteProjectExportSetting)
  private
    FExportLang: string;
    FFilterTagIsUserId: boolean;
    FRemoveMissingVL: boolean;
    FRenameVariablesPrefix: string;
    FSectionCaptionIsQText: boolean;
    FSoftwareName: string;
    FSoftwareVersion: string;
    FVersion: string;
  public
    property SoftwareName: string read FSoftwareName write FSoftwareName;
    property SoftwareVersion: string read FSoftwareVersion write FSoftwareVersion;
    property Version: string read FVersion write FVersion;
    property ExportLang: string read FExportLang write FExportLang;
    property RemoveMissingVL: boolean read FRemoveMissingVL write FRemoveMissingVL;
    property FilterTagIsUserId: boolean read FFilterTagIsUserId write FFilterTagIsUserId;
    property SectionCaptionIsQText: boolean read FSectionCaptionIsQText write FSectionCaptionIsQText;
    property RenameVariablesPrefix: string read FRenameVariablesPrefix write FRenameVariablesPrefix;

  { Common }
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(const ASettings: TEpiExportSetting); override;
    function SanetyCheck: boolean; override;
  public
    // Visitor Pattern
    procedure   AcceptVisitor(Const Visitor: TEpiExportSettingCustomVisitor); override;
  end;

  { TEpiCustomTextExportSettings }

  TEpiCustomTextExportSettings = class(TEpiExportSetting)
  public
    ByteOrderMark: boolean;
    ExportFieldNames: boolean;
    QuoteChar: string;

    constructor Create; override;
    procedure Assign(const ASettings: TEpiExportSetting); override;
  public
    // Visitor Pattern
    procedure   AcceptVisitor(Const Visitor: TEpiExportSettingCustomVisitor); override;
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
  public
    // Visitor Pattern
    procedure   AcceptVisitor(Const Visitor: TEpiExportSettingCustomVisitor); override;
  end;

  { TEpiEPXExportSetting }

  TEpiEPXExportSetting = class(TEpiCustomCompleteProjectExportSetting)
  public
    // Visitor Pattern
    procedure   AcceptVisitor(Const Visitor: TEpiExportSettingCustomVisitor); override;
  end;

{  TEpiSpreadSheetExportSetting = class(TEpiCustomTextExportSettings)
  public
    SpreadSheetVersion: byte;  //TODO: Export to spreadsheet using TFPSpreadSheet.
  end;}

  TEpiExportSettingVisitorTraversal = (
    vtSingle,                       // Only visit this instance in the hierachy
    vtDerivedFirst,                 // Visit most derived class first, then inherited classes afterwards
    vtInheritedFirst                // Visit most inherited class first, then derived classes afterwards
  );

  { TEpiExportSettingCustomVisitor }

  TEpiExportSettingCustomVisitor = class
  private
    FVisitorTraversal: TEpiExportSettingVisitorTraversal;
  public
    constructor Create; virtual;
    procedure Visit(Const ExportSetting: TEpiExportSetting); virtual; abstract; overload;
    procedure Visit(Const ExportSetting: TEpiCustomValueLabelExportSetting); virtual; abstract; overload;
    procedure Visit(Const ExportSetting: TEpiCustomCompleteProjectExportSetting); virtual; abstract; overload;
    procedure Visit(Const ExportSetting: TEpiStataExportSetting); virtual; abstract; overload;
    procedure Visit(Const ExportSetting: TEpiSPSSExportSetting); virtual; abstract; overload;
    procedure Visit(Const ExportSetting: TEpiSASExportSetting); virtual; abstract; overload;
    procedure Visit(Const ExportSetting: TEpiDDIExportSetting); virtual; abstract; overload;
    procedure Visit(Const ExportSetting: TEpiCustomTextExportSettings); virtual; abstract; overload;
    procedure Visit(Const ExportSetting: TEpiCSVExportSetting); virtual; abstract; overload;
    procedure Visit(Const ExportSetting: TEpiEPXExportSetting); virtual; abstract; overload;
 public
    property  VisitorTraversal: TEpiExportSettingVisitorTraversal read FVisitorTraversal write FVisitorTraversal;
  end;

implementation

uses
  LazUTF8Classes;

{ TEpiCustomCompleteProjectExportSetting }

constructor TEpiCustomCompleteProjectExportSetting.Create;
begin
  inherited Create;

  FExportCompleteProject := false;
end;

procedure TEpiCustomCompleteProjectExportSetting.Assign(
  const ASettings: TEpiExportSetting);
begin
  inherited Assign(ASettings);
  if not (ASettings is TEpiCustomCompleteProjectExportSetting) then exit;

  ExportCompleteProject := TEpiCustomCompleteProjectExportSetting(ASettings).ExportCompleteProject;
end;

procedure TEpiCustomCompleteProjectExportSetting.AcceptVisitor(
  const Visitor: TEpiExportSettingCustomVisitor);
begin
  if Visitor.VisitorTraversal = vtInheritedFirst then
    inherited AcceptVisitor(Visitor);

  Visitor.Visit(Self);

  if Visitor.VisitorTraversal = vtDerivedFirst then
    inherited AcceptVisitor(Visitor);
end;

{ TEpiEPXExportSetting }

procedure TEpiEPXExportSetting.AcceptVisitor(
  const Visitor: TEpiExportSettingCustomVisitor);
begin
  if Visitor.VisitorTraversal = vtInheritedFirst then
    inherited AcceptVisitor(Visitor);

  Visitor.Visit(Self);

  if Visitor.VisitorTraversal = vtDerivedFirst then
    inherited AcceptVisitor(Visitor);
end;

{ TEpiSASExportSetting }

procedure TEpiSASExportSetting.AcceptVisitor(
  const Visitor: TEpiExportSettingCustomVisitor);
begin
  if Visitor.VisitorTraversal = vtInheritedFirst then
    inherited AcceptVisitor(Visitor);

  Visitor.Visit(Self);

  if Visitor.VisitorTraversal = vtDerivedFirst then
    inherited AcceptVisitor(Visitor);
end;

{ TEpiSPSSExportSetting }

procedure TEpiSPSSExportSetting.AcceptVisitor(
  const Visitor: TEpiExportSettingCustomVisitor);
begin
  if Visitor.VisitorTraversal = vtInheritedFirst then
    inherited AcceptVisitor(Visitor);

  Visitor.Visit(Self);

  if Visitor.VisitorTraversal = vtDerivedFirst then
    inherited AcceptVisitor(Visitor);
end;

{ TEpiExportSettingCustomVisitor }

constructor TEpiExportSettingCustomVisitor.Create;
begin
  FVisitorTraversal := vtInheritedFirst;
end;

{ TEpiDDIExportSetting }

constructor TEpiDDIExportSetting.Create;
begin
  inherited Create;
  RemoveMissingVL := false;
  FilterTagIsUserId := false;
end;

destructor TEpiDDIExportSetting.Destroy;
begin
  inherited Destroy;
end;

procedure TEpiDDIExportSetting.Assign(const ASettings: TEpiExportSetting);
begin
  inherited Assign(ASettings);
  with TEpiDDIExportSetting(ASettings) do
  begin
    Self.FExportLang            := FExportLang;
    Self.FFilterTagIsUserId     := FFilterTagIsUserId;
    Self.FRemoveMissingVL       := FRemoveMissingVL;
    Self.FRenameVariablesPrefix := FRenameVariablesPrefix;
    Self.FSectionCaptionIsQText := FSectionCaptionIsQText;
    Self.FSoftwareName          := FSoftwareName;
    Self.FSoftwareVersion       := FSoftwareVersion;
    Self.FVersion               := FVersion;
  end;
end;

function TEpiDDIExportSetting.SanetyCheck: boolean;
begin
  Result :=
    (ExportLang <> '') and
    (inherited SanetyCheck);
end;

procedure TEpiDDIExportSetting.AcceptVisitor(
  const Visitor: TEpiExportSettingCustomVisitor);
begin
  if Visitor.VisitorTraversal = vtInheritedFirst then
    inherited AcceptVisitor(Visitor);

  Visitor.Visit(Self);

  if Visitor.VisitorTraversal = vtDerivedFirst then
    inherited AcceptVisitor(Visitor);
end;

{ TEpiCustomValueLabelExportSetting }

procedure TEpiCustomValueLabelExportSetting.Assign(
  const ASettings: TEpiExportSetting);
begin
  inherited Assign(ASettings);
  if not (ASettings is TEpiCustomValueLabelExportSetting) then exit;

  ExportValueLabels := TEpiCustomValueLabelExportSetting(ASettings).ExportValueLabels;
end;

procedure TEpiCustomValueLabelExportSetting.AcceptVisitor(
  const Visitor: TEpiExportSettingCustomVisitor);
begin
  if Visitor.VisitorTraversal = vtInheritedFirst then
    inherited AcceptVisitor(Visitor);

  Visitor.Visit(Self);

  if Visitor.VisitorTraversal = vtDerivedFirst then
    inherited AcceptVisitor(Visitor);
end;

{ TEpiExportSetting }

constructor TEpiExportSetting.Create;
begin
  FCreatedStream := false;
  Fields := TList.Create;
  DataformSettings := TList.Create;

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
  if Assigned(AdditionalExportSettings) then
    AdditionalExportSettings.Free;
  if FCreatedStream then
    ExportStream.Free;
  inherited Destroy;
end;

procedure TEpiExportSetting.Assign(const ASettings: TEpiExportSetting);
begin
  if not Assigned(ASettings) then exit;
  ExportFileName := ASettings.ExportFileName;
  ExportStream   := ASettings.ExportStream;
//  FCreatedStream := ASettings.FCreatedStream;
  Doc            := ASettings.Doc;
  DataFileIndex  := ASettings.DataFileIndex;

  // Filters
  FromRecord     := ASettings.FromRecord;
  ToRecord       := ASettings.ToRecord;
  Encoding       := ASettings.Encoding;
  Condition      := ASettings.Condition;
  ExportDeleted  := ASettings.ExportDeleted;

  Fields.Assign(ASettings.Fields);

  if Assigned(ASettings.AdditionalExportSettings) then
  begin
    AdditionalExportSettings := TEpiExportSettingClass(ASettings.AdditionalExportSettings.ClassType).Create;
    AdditionalExportSettings.Assign(ASettings.AdditionalExportSettings);
  end;
end;

function TEpiExportSetting.SanetyCheck: boolean;
begin
  if not (Assigned(Doc)) and
     not ((DataFileIndex >= 0) and (DataFileIndex < Doc.DataFiles.Count)) then exit(False);

  if FromRecord = -1 then FromRecord := 0;
  if ToRecord   = -1 then ToRecord := Doc.DataFiles[DataFileIndex].Size - 1;

  if (ExportStream = nil) and (ExportFileName <> '') then
  begin
    ExportStream := TFileStreamUTF8.Create(ExportFileName, fmCreate);
    FCreatedStream := true;
  end;


  result :=
    (Assigned(ExportStream)) and
//    (Fields.Count > 0) and
    (FromRecord >= 0) and
    (ToRecord < Doc.DataFiles[DataFileIndex].Size);
end;

procedure TEpiExportSetting.AcceptVisitor(
  const Visitor: TEpiExportSettingCustomVisitor);
begin
  Visitor.Visit(Self);
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

procedure TEpiStataExportSetting.AcceptVisitor(
  const Visitor: TEpiExportSettingCustomVisitor);
begin
  if Visitor.VisitorTraversal = vtInheritedFirst then
    inherited AcceptVisitor(Visitor);

  Visitor.Visit(Self);

  if Visitor.VisitorTraversal = vtDerivedFirst then
    inherited AcceptVisitor(Visitor);
end;

{ TEpiCustomTextExportSettings }

constructor TEpiCustomTextExportSettings.Create;
begin
  inherited Create;
  QuoteChar := '"';
  ByteOrderMark := false;
end;

procedure TEpiCustomTextExportSettings.Assign(const ASettings: TEpiExportSetting
  );
begin
  inherited Assign(ASettings);
  if not (ASettings is TEpiCustomTextExportSettings) then exit;

  ExportFieldNames := TEpiCustomTextExportSettings(ASettings).ExportFieldNames;
  QuoteChar        := TEpiCustomTextExportSettings(ASettings).QuoteChar;
  ByteOrderMark    := TEpiCustomTextExportSettings(ASettings).ByteOrderMark;
end;

procedure TEpiCustomTextExportSettings.AcceptVisitor(
  const Visitor: TEpiExportSettingCustomVisitor);
begin
  if Visitor.VisitorTraversal = vtInheritedFirst then
    inherited AcceptVisitor(Visitor);

  Visitor.Visit(Self);

  if Visitor.VisitorTraversal = vtDerivedFirst then
    inherited AcceptVisitor(Visitor);
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
  Result := inherited SanetyCheck;

  // With fixed format delimiters do not interfere.
  if FixedFormat then Exit;

  // Only make sure that FieldSeparator <> QuoteChar
  // otherwise an CSV export will enclose data with a delimiter if it
  // is the same as the FieldSeparator.

  Result := Result and
    (FieldSeparator <> QuoteChar);
end;

procedure TEpiCSVExportSetting.AcceptVisitor(
  const Visitor: TEpiExportSettingCustomVisitor);
begin
  if Visitor.VisitorTraversal = vtInheritedFirst then
    inherited AcceptVisitor(Visitor);

  Visitor.Visit(Self);

  if Visitor.VisitorTraversal = vtDerivedFirst then
    inherited AcceptVisitor(Visitor);
end;

end.

