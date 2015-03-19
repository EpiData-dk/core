unit epiexportsettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epieximtypes, epidocument, epidatafiles;

type

  TEpiExportSettingCustomVisitor = class;

  { TEpiExportDatafileSettings }

  TEpiExportDatafileSettings = class
  private
    FCreatedStream: boolean;
  public
    DatafileName:   string;

    ExportStream:   TStream;
    ExportFileName: string;

    FromRecord:     integer;
    ToRecord:       integer;

    Condition:      string;
    ExportItems:    TStrings;

    // For use with multi-file export (eg. SPSS, SAS, DDI, ...)
    // (usually used for secondary file export settings, assigned during export).
    AdditionalExportSettings: TEpiExportDatafileSettings;
  public
    constructor Create;
    destructor  Destroy; override;
  public
    procedure   Assign(Const OriginalSetting: TEpiExportDatafileSettings);
    function    SanetyCheck: boolean; virtual;
  end;

  { TEpiExportDatafileSettingsList }

  TEpiExportDatafileSettingsList = class(TList)
  private
    function GetExportItem(const Index: integer): TEpiExportDatafileSettings;
  public
    procedure ClearAndFree;
    property Items[Const Index: integer]: TEpiExportDatafileSettings read GetExportItem; default;
  end;


  { TEpiExportSetting }

  TEpiExportSetting = class
  public
    Doc: TEpiDocument;

    // For use with multi-file export (eg. SPSS, SAS, DDI, ...)
    // (usually used for secondary file export settings, assigned during export).
    AdditionalExportSettings: TEpiExportSetting;

    Encoding:   TEpiEncoding;
    ExportDeleted: boolean;

    // A list of individual datafile settings
    DatafileSettings: TEpiExportDatafileSettingsList;

    // Helpers
    constructor Create; virtual;
    destructor  Destroy; override;
    function    SanetyCheck: boolean; virtual;
  public
    procedure   Assign(Const OriginalSettings: TEpiExportSetting); virtual;
    // Visitor Pattern
    procedure   AcceptVisitor(Const Visitor: TEpiExportSettingCustomVisitor); virtual;
  end;
  TEpiExportSettingClass = class of TEpiExportSetting;

  { TEpiCustomValueLabelExportSetting }

  TEpiCustomValueLabelExportSetting = class(TEpiExportSetting)
  public
    ExportValueLabels: boolean;
  public
    procedure   Assign(Const OriginalSettings: TEpiCustomValueLabelExportSetting); reintroduce;
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
    function SanetyCheck: boolean; override;
  public
    procedure   Assign(Const OriginalSettings: TEpiStataExportSetting); reintroduce;
    // Visitor Pattern
    procedure   AcceptVisitor(Const Visitor: TEpiExportSettingCustomVisitor); override;
  end;


  { TEpiSPSSExportSetting }

  TEpiSPSSExportSetting = class(TEpiCustomValueLabelExportSetting)
  public
    Delimiter: char;
  public
    procedure   Assign(Const OriginalSettings: TEpiSPSSExportSetting); reintroduce;
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
  public
    procedure   Assign(Const OriginalSettings: TEpiCustomCompleteProjectExportSetting); reintroduce;
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
    function SanetyCheck: boolean; override;
  public
    procedure   Assign(Const OriginalSettings: TEpiDDIExportSetting); reintroduce;
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
  public
    procedure   Assign(Const OriginalSettings: TEpiCustomTextExportSettings); reintroduce;
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
    function SanetyCheck: boolean; override;
  public
    procedure   Assign(Const OriginalSettings: TEpiCSVExportSetting); reintroduce;
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

{ TEpiExportDatafileSettingsList }

function TEpiExportDatafileSettingsList.GetExportItem(const Index: integer
  ): TEpiExportDatafileSettings;
begin
  result := TEpiExportDatafileSettings(Get(Index));
end;

procedure TEpiExportDatafileSettingsList.ClearAndFree;
var
  i: Integer;
begin
  for i := count - 1 downto 0 do
    Items[i].Free;

  Clear;
end;


{ TEpiExportDatafileSettings }

constructor TEpiExportDatafileSettings.Create;
begin
  DatafileName    := '';

  ExportStream    := nil;
  ExportFileName  := '';

  FromRecord      := -1;
  ToRecord        := -1;

  Condition       := '';
  ExportItems     := TStringListUTF8.Create;
end;

destructor TEpiExportDatafileSettings.Destroy;
begin
  ExportItems.Free;

  if FCreatedStream then
    ExportStream.Free;

  inherited Destroy;
end;

procedure TEpiExportDatafileSettings.Assign(
  const OriginalSetting: TEpiExportDatafileSettings);
begin
  DatafileName    := OriginalSetting.DatafileName;

  ExportFileName  := OriginalSetting.ExportFileName;

  FromRecord      := OriginalSetting.FromRecord;
  ToRecord        := OriginalSetting.ToRecord;

  Condition       := OriginalSetting.Condition;
  ExportItems.Assign(OriginalSetting.ExportItems);
end;

function TEpiExportDatafileSettings.SanetyCheck: boolean;
begin
  if (ExportStream = nil) and (ExportFileName <> '') then
  begin
    ExportStream := TFileStreamUTF8.Create(ExportFileName, fmCreate);
    FCreatedStream := true;
  end;

  result := true;
end;

{ TEpiCustomCompleteProjectExportSetting }

constructor TEpiCustomCompleteProjectExportSetting.Create;
begin
  inherited Create;

  FExportCompleteProject := false;
end;

procedure TEpiCustomCompleteProjectExportSetting.Assign(
  const OriginalSettings: TEpiCustomCompleteProjectExportSetting);
begin
  inherited Assign(OriginalSettings);
  ExportCompleteProject := OriginalSettings.ExportCompleteProject;
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

procedure TEpiSPSSExportSetting.Assign(
  const OriginalSettings: TEpiSPSSExportSetting);
begin
  inherited Assign(OriginalSettings);
  Delimiter := OriginalSettings.Delimiter;
end;

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

function TEpiDDIExportSetting.SanetyCheck: boolean;
begin
  Result :=
    (ExportLang <> '') and
    (inherited SanetyCheck);
end;

procedure TEpiDDIExportSetting.Assign(
  const OriginalSettings: TEpiDDIExportSetting);
begin
  inherited Assign(OriginalSettings);

  SoftwareName          := OriginalSettings.SoftwareName;
  SoftwareVersion       := OriginalSettings.SoftwareVersion;
  Version               := OriginalSettings.Version;
  ExportLang            := OriginalSettings.ExportLang;
  RemoveMissingVL       := OriginalSettings.RemoveMissingVL;
  FilterTagIsUserId     := OriginalSettings.FilterTagIsUserId;
  SectionCaptionIsQText := OriginalSettings.SectionCaptionIsQText;
  RenameVariablesPrefix := OriginalSettings.RenameVariablesPrefix;
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
  const OriginalSettings: TEpiCustomValueLabelExportSetting);
begin
  inherited Assign(OriginalSettings);
  ExportValueLabels := OriginalSettings.ExportValueLabels;
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
  DatafileSettings := TEpiExportDatafileSettingsList.Create;

  Doc            := nil;
  AdditionalExportSettings := nil;

  Encoding       := eeUTF8;
  ExportDeleted  := false;
end;

destructor TEpiExportSetting.Destroy;
var
  i: Integer;
begin
  DatafileSettings.ClearAndFree;
  DatafileSettings.Free;

  if Assigned(AdditionalExportSettings) then
    AdditionalExportSettings.Free;
  inherited Destroy;
end;

function TEpiExportSetting.SanetyCheck: boolean;
var
  i: Integer;
begin
  if not (Assigned(Doc)) then exit(False);

  Result := true;

  for i := 0 to DatafileSettings.Count - 1 do
    result := result and DatafileSettings[i].SanetyCheck;
end;

procedure TEpiExportSetting.Assign(const OriginalSettings: TEpiExportSetting);
var
  NewDFSetting: TEpiExportDatafileSettings;
  i: Integer;
begin
  Doc               := OriginalSettings.Doc;

  Encoding          := OriginalSettings.Encoding;
  ExportDeleted     := OriginalSettings.ExportDeleted;

  for i := 0 to OriginalSettings.DatafileSettings.Count - 1 do
  begin
    NewDFSetting := TEpiExportDatafileSettings.Create;
    NewDFSetting.Assign(OriginalSettings.DatafileSettings[i]);
    DatafileSettings.Add(NewDFSetting);
  end;
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

function TEpiStataExportSetting.SanetyCheck: boolean;
begin
  Result := inherited SanetyCheck;
end;

procedure TEpiStataExportSetting.Assign(
  const OriginalSettings: TEpiStataExportSetting);
begin
  inherited Assign(OriginalSettings);
  FieldNameCase  := OriginalSettings.FieldNameCase;
  Version        := OriginalSettings.Version;
  ExportLines.Assign(OriginalSettings.ExportLines);
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

procedure TEpiCustomTextExportSettings.Assign(
  const OriginalSettings: TEpiCustomTextExportSettings);
begin
  inherited Assign(OriginalSettings);
  ByteOrderMark    := OriginalSettings.ByteOrderMark;
  ExportFieldNames := OriginalSettings.ExportFieldNames;
  QuoteChar        := OriginalSettings.QuoteChar;
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

procedure TEpiCSVExportSetting.Assign(
  const OriginalSettings: TEpiCSVExportSetting);
begin
  inherited Assign(OriginalSettings);
  FieldSeparator  := OriginalSettings.FieldSeparator;
  DateSeparator   := OriginalSettings.DateSeparator;
  TimeSeparator   := OriginalSettings.TimeSeparator;
  DecimalSeparator := OriginalSettings.DecimalSeparator;
  NewLine          := OriginalSettings.NewLine;
  FixedFormat      := OriginalSettings.FixedFormat;
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

