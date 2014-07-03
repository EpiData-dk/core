unit episettings;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epicustombase, Laz2_DOM, epidatafilestypes;

type

  { TEpiXMLSettings }

  // esce = Epi Setting Change Event
  TEpiSettingChangeEvent = (
    esceDateSep, esceTimeSep, esceDecSep, esceScramble
  );

  TEpiXMLSettings = class(TEpiCustomBase)
  private
    FDateSeparator: string;
    FDecimalSeparator: string;
    FFormatSettings: TFormatSettings;
    FMissingString: string;
    FScrambled: boolean;
    FTimeSeparator: string;
    procedure SetDateSeparator(const AValue: string);
    procedure SetDecimalSeparator(const AValue: string);
    procedure SetScrambled(const AValue: boolean);
    procedure SetTimeSeparator(const AValue: string);
  protected
    function SaveAttributesToXml: string; override;
  protected
    function    SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor  Destroy; override;
    function    XMLName: string; override;
    procedure   LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    property    DateSeparator: string read FDateSeparator write SetDateSeparator;
    property    TimeSeparator: string read FTimeSeparator write SetTimeSeparator;
    property    DecimalSeparator: string read FDecimalSeparator write SetDecimalSeparator;
    property    Scrambled: boolean read FScrambled write SetScrambled;
    property    FormatSettings: TFormatSettings read FFormatSettings;
  { Cloning }
  protected
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
      ReferenceMap: TEpiReferenceMap): TEpiCustomBase; override;
  end;

  { TEpiProjectSettings }

  TEpiProjectSettingChangeEvent = (
    epceFieldName, epceFieldBorder, epceBackupInterval, epceBackupShutdown, epceAutoIncStart
  );

  TEpiProjectSettings = class(TEpiCustomBase)
  private
    FAutoIncStartValue: EpiInteger;
    FBackupInterval: Integer;
    FBackupOnShutdown: Boolean;
    FShowFieldBorders: Boolean;
    FShowFieldNames: Boolean;
    procedure   SetAutoIncStartValue(const AValue: EpiInteger);
    procedure   SetBackupInterval(const AValue: Integer);
    procedure   SetBackupOnShutdown(const AValue: Boolean);
    procedure   SetShowFieldBorders(const AValue: Boolean);
    procedure   SetShowFieldNames(const AValue: Boolean);
  protected
    function SaveAttributesToXml: string; override;
  protected
    function SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor  Destroy; override;
    function    XMLName: string; override;
    function    SaveToXml(Content: String; Lvl: integer): string; override;
    procedure   LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    function    ScrambleXml: boolean; override;
    property    ShowFieldNames: Boolean read FShowFieldNames write SetShowFieldNames;
    property    ShowFieldBorders: Boolean read FShowFieldBorders write SetShowFieldBorders;
    property    BackupInterval: Integer read FBackupInterval write SetBackupInterval;
    property    BackupOnShutdown: Boolean read FBackupOnShutdown write SetBackupOnShutdown;
    property    AutoIncStartValue: EpiInteger read FAutoIncStartValue write SetAutoIncStartValue;
  { Cloning }
  protected
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
      ReferenceMap: TEpiReferenceMap): TEpiCustomBase; override;
  end;

implementation

{ TEpiXMLSettings }

procedure TEpiXMLSettings.SetDateSeparator(const AValue: string);
var
  Val: String;
begin
  if FDateSeparator = AValue then exit;
  Val := FDateSeparator;
  FDateSeparator := AValue;
  FFormatSettings.DateSeparator := FDateSeparator[1];
  DoChange(eegXMLSetting, Word(esceDateSep), @Val);
end;

procedure TEpiXMLSettings.SetDecimalSeparator(const AValue: string);
var
  Val: String;
begin
  if FDecimalSeparator = AValue then exit;
  Val := FDecimalSeparator;
  FDecimalSeparator := AValue;
  FFormatSettings.DecimalSeparator := FDecimalSeparator[1];
  DoChange(eegXMLSetting, Word(esceDecSep), @Val);
end;

procedure TEpiXMLSettings.SetScrambled(const AValue: boolean);
var
  Val: Boolean;
begin
  if FScrambled = AValue then exit;
  Val := FScrambled;
  FScrambled := AValue;
  DoChange(eegXMLSetting, Word(esceScramble), @Val);
end;

procedure TEpiXMLSettings.SetTimeSeparator(const AValue: string);
var
  Val: String;
begin
  if FTimeSeparator = AValue then exit;
  Val := FTimeSeparator;
  FTimeSeparator := AValue;
  FFormatSettings.TimeSeparator := TimeSeparator[1];
  DoChange(eegXMLSetting, Word(esceTimeSep), @Val);
end;

function TEpiXMLSettings.SaveAttributesToXml: string;
begin
  Result := inherited SaveAttributesToXml +
    SaveAttr(rsDateSep,    DateSeparator) +
    SaveAttr(rsTimeSep,    TimeSeparator) +
    SaveAttr(rsDecSep,     DecimalSeparator);
end;

function TEpiXMLSettings.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);

  SaveDomAttr(Result, rsDateSep, DateSeparator);
  SaveDomAttr(Result, rsTimeSep, TimeSeparator);
  SaveDomAttr(Result, rsDecSep,  DecimalSeparator);
end;

constructor TEpiXMLSettings.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  Scrambled := false;

  FFormatSettings := DefaultFormatSettings;
  FFormatSettings.ShortDateFormat := 'YYYY/MM/DD HH:NN:SS';
  FFormatSettings.LongDateFormat  := 'YYYY/MM/DD HH:NN:SS';
  FFormatSettings.ShortTimeFormat := 'HH:NN:SS';
  FFormatSettings.LongTimeFormat  := 'HH:NN:SS';

  DateSeparator := '/';
  TimeSeparator := '.';
  DecimalSeparator := ',';
end;

destructor TEpiXMLSettings.Destroy;
begin
  inherited Destroy;
end;

function TEpiXMLSettings.XMLName: string;
begin
  Result := rsSettings;
end;

procedure TEpiXMLSettings.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
var
  Node: TDOMNode;
begin
  // Root = <Settings>
  DateSeparator    := LoadAttrString(Root, rsDateSep)[1];
  TimeSeparator    := LoadAttrString(Root, rsTimeSep)[1];
  DecimalSeparator := LoadAttrString(Root, rsDecSep)[1];
end;

function TEpiXMLSettings.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
  ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);
  with TEpiXMLSettings(Result) do
  begin
    FDateSeparator     := Self.FDateSeparator;
    FDecimalSeparator  := Self.FDecimalSeparator;
    FFormatSettings    := Self.FFormatSettings;
    FMissingString     := Self.FMissingString;
    FScrambled         := Self.FScrambled;
    FTimeSeparator     := Self.FTimeSeparator;
  end;
end;

{ TEpiProjectSettings }

procedure TEpiProjectSettings.SetShowFieldBorders(const AValue: Boolean);
var
  Val: Boolean;
begin
  if FShowFieldBorders = AValue then exit;
  Val := FShowFieldBorders;
  FShowFieldBorders := AValue;
  DoChange(eegProjectSettings, Word(epceFieldBorder), @Val);
end;

procedure TEpiProjectSettings.SetBackupInterval(const AValue: Integer);
var
  Val: LongInt;
begin
  if FBackupInterval = AValue then exit;
  Val := FBackupInterval;
  FBackupInterval := AValue;
  DoChange(eegProjectSettings, Word(epceBackupInterval), @Val);
end;

procedure TEpiProjectSettings.SetAutoIncStartValue(const AValue: EpiInteger);
var
  Val: EpiInteger;
begin
  if FAutoIncStartValue = AValue then exit;
  Val := FAutoIncStartValue;
  FAutoIncStartValue := AValue;
  DoChange(eegProjectSettings, Word(epceAutoIncStart), @Val);
end;

procedure TEpiProjectSettings.SetBackupOnShutdown(const AValue: Boolean);
var
  Val: Boolean;
begin
  if FBackupOnShutdown = AValue then exit;
  Val := FBackupOnShutdown;
  FBackupOnShutdown := AValue;
  DoChange(eegProjectSettings, Word(epceBackupShutdown), @Val);
end;

procedure TEpiProjectSettings.SetShowFieldNames(const AValue: Boolean);
var
  Val: Boolean;
begin
  if FShowFieldNames = AValue then exit;
  Val := FShowFieldNames;
  FShowFieldNames := AValue;
  DoChange(eegProjectSettings, Word(epceFieldName), @Val);
end;

function TEpiProjectSettings.SaveAttributesToXml: string;
begin
  Result := inherited SaveAttributesToXml +
    SaveAttr(rsAutoIncStart,        AutoIncStartValue) +
    SaveAttr(rsTimedBackupInterval, BackupInterval) +
    SaveAttr(rsBackupOnShutdown,    BackupOnShutdown) +
    SaveAttr(rsShowFieldNames,      ShowFieldNames) +
    SaveAttr(rsShowFieldBorders,    ShowFieldBorders);
end;

function TEpiProjectSettings.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);

  SaveDomAttr(Result, rsAutoIncStart,        AutoIncStartValue);
  SaveDomAttr(Result, rsTimedBackupInterval, BackupInterval);
  SaveDomAttr(Result, rsBackupOnShutdown,    BackupOnShutdown);
  SaveDomAttr(Result, rsShowFieldNames,      ShowFieldNames);
  SaveDomAttr(Result, rsShowFieldBorders,    ShowFieldBorders);
end;

constructor TEpiProjectSettings.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);

  FShowFieldBorders := true;
  FShowFieldNames   := false;
  FBackupInterval   := 10;
  FAutoIncStartValue := 1;
end;

destructor TEpiProjectSettings.Destroy;
begin
  inherited Destroy;
end;

function TEpiProjectSettings.XMLName: string;
begin
  Result := rsProjectSettings;
end;

function TEpiProjectSettings.SaveToXml(Content: String; Lvl: integer): string;
begin
  Result := inherited SaveToXml(Content, Lvl);
end;

procedure TEpiProjectSettings.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
var
  I: Integer;
  B: Boolean;
begin
  AutoIncStartValue := LoadAttrInt(Root, rsAutoIncStart, AutoIncStartValue, false);
  BackupInterval    := LoadAttrInt(Root, rsTimedBackupInterval, BackupInterval, false);
  BackupOnShutdown  := LoadAttrBool(Root,rsBackupOnShutdown, BackupOnShutdown, false);
  ShowFieldNames    := LoadAttrBool(Root, rsShowFieldNames, ShowFieldNames, false);
  ShowFieldBorders  := LoadAttrBool(Root, rsShowFieldBorders, ShowFieldBorders, false);
end;

function TEpiProjectSettings.ScrambleXml: boolean;
begin
//TODO  Result := TEpiDocument(RootOwner).XMLSettings.Scrambled;
end;

function TEpiProjectSettings.DoClone(AOwner: TEpiCustomBase;
  Dest: TEpiCustomBase; ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);
  with TEpiProjectSettings(Result) do
  begin
    FAutoIncStartValue := Self.FAutoIncStartValue;
    FBackupInterval    := Self.FBackupInterval;
    FBackupOnShutdown  := Self.FBackupOnShutdown;
    FShowFieldBorders  := Self.FShowFieldBorders;
    FShowFieldNames    := Self.FShowFieldNames;
  end;
end;

end.

