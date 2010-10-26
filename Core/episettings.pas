unit episettings;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epicustombase, DOM;

type

  { TEpiXMLSettings }

  // esce = Epi Setting Change Event
  TEpiSettingChangeEvent = (
    esceVersion, esceDateSep, esceTimeSep, esceDecSep, esceMissing, esceScramble
  );

  TEpiXMLSettings = class(TEpiCustomBase)
  private
    FDateSeparator: string;
    FDecimalSeparator: string;
    FFormatSettings: TFormatSettings;
    FMissingString: string;
    FScrambled: boolean;
    FTimeSeparator: string;
    FVersion: integer;
    procedure SetDateSeparator(const AValue: string);
    procedure SetDecimalSeparator(const AValue: string);
    procedure SetMissingString(const AValue: string);
    procedure SetScrambled(const AValue: boolean);
    procedure SetTimeSeparator(const AValue: string);
    procedure SetVersion(const AValue: integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    XMLName: string; override;
    function    SaveToXml(Content: String; Lvl: integer): string; override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    property    Version: integer read FVersion write SetVersion;
    property    DateSeparator: string read FDateSeparator write SetDateSeparator;
    property    TimeSeparator: string read FTimeSeparator write SetTimeSeparator;
    property    DecimalSeparator: string read FDecimalSeparator write SetDecimalSeparator;
    property    MissingString: string read FMissingString write SetMissingString;
    property    Scrambled: boolean read FScrambled write SetScrambled;
    property    FormatSettings: TFormatSettings read FFormatSettings;
  end;

  { TEpiProjectSettings }

  TEpiProjectSettingChangeEvent = (
    epceFieldName, epceFieldBorder, epceBackupInterval, epceBackupShutdown
  );

  TEpiProjectSettings = class(TEpiCustomBase)
  private
    FBackupInterval: Integer;
    FBackupOnShutdown: Boolean;
    FShowFieldBorders: Boolean;
    FShowFieldNames: Boolean;
    procedure   SetBackupInterval(const AValue: Integer);
    procedure   SetBackupOnShutdown(const AValue: Boolean);
    procedure   SetShowFieldBorders(const AValue: Boolean);
    procedure   SetShowFieldNames(const AValue: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    XMLName: string; override;
    function    SaveToXml(Content: String; Lvl: integer): string; override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    function    ScrambleXml: boolean; override;
    property    ShowFieldNames: Boolean read FShowFieldNames write SetShowFieldNames;
    property    ShowFieldBorders: Boolean read FShowFieldBorders write SetShowFieldBorders;
    property    BackupInterval: Integer read FBackupInterval write SetBackupInterval;
    property    BackupOnShutdown: Boolean read FBackupOnShutdown write SetBackupOnShutdown;
  end;

implementation

uses
  epidocument;

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

procedure TEpiXMLSettings.SetMissingString(const AValue: string);
var
  Val: String;
begin
  if FMissingString = AValue then exit;
  Val := FMissingString;
  FMissingString := AValue;
  DoChange(eegXMLSetting, Word(esceMissing), @Val);
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

procedure TEpiXMLSettings.SetVersion(const AValue: integer);
var
  Val: LongInt;
begin
  if FVersion = AValue then exit;
  Val := FVersion;
  FVersion := AValue;
  DoChange(eegXMLSetting, Word(esceVersion), @Val);
end;

constructor TEpiXMLSettings.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Version := 0;
  Scrambled := false;

//  DefaultFormatSettings.ShortTimeFormat := 'HH:NN';
//  DefaultFormatSettings.LongTimeFormat := 'HH:NN:SS';
  FFormatSettings := DefaultFormatSettings;
  FFormatSettings.ShortDateFormat := 'YYYY/MM/DD HH:NN:SS';
  FFormatSettings.LongDateFormat  := 'YYYY/MM/DD HH:NN:SS';

  DateSeparator := '/';
  TimeSeparator := '.';
  DecimalSeparator := ',';
  MissingString := '.';
end;

destructor TEpiXMLSettings.Destroy;
begin
  inherited Destroy;
end;

function TEpiXMLSettings.XMLName: string;
begin
  Result := rsSettings;
end;

function TEpiXMLSettings.SaveToXml(Content: String; Lvl: integer): string;
begin
  Result :=
    SaveNode(Lvl + 1, rsVersion,    Version) +
    SaveNode(Lvl + 1, rsScrambled,  Scrambled) +
    SaveNode(Lvl + 1, rsDateSep,    DateSeparator) +
    SaveNode(Lvl + 1, rsTimeSep,    TimeSeparator) +
    SaveNode(Lvl + 1, rsDecSep,     DecimalSeparator) +
    SaveNode(Lvl + 1, rsMissingStr, MissingString);
  result := inherited SaveToXml(Result, Lvl);
end;

procedure TEpiXMLSettings.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMNode;
begin
  // Root = <Settings>

  // Version: so far we only got ver. 0
  Version          := LoadNodeInt(Root, rsVersion);
  Scrambled        := LoadNodeBool(Root, rsScrambled);
  DateSeparator    := LoadNodeString(Root, rsDateSep)[1];
  TimeSeparator    := LoadNodeString(Root, rsTimeSep)[1];
  DecimalSeparator := LoadNodeString(Root, rsDecSep)[1];
  MissingString    := LoadNodeString(Root, rsMissingStr);
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

constructor TEpiProjectSettings.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FShowFieldBorders := true;
  FShowFieldNames   := true;
  FBackupInterval   := 10;
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
  Result :=
    SaveNode(Lvl + 1, rsTimedBackupInterval, BackupInterval) +
    SaveNode(Lvl + 1, rsBackupOnShutdown,    BackupOnShutdown) +
    SaveNode(Lvl + 1, rsShowFieldNames,      ShowFieldNames) +
    SaveNode(Lvl + 1, rsShowFieldBorders,    ShowFieldBorders);
  Result := inherited SaveToXml(Result, Lvl);
end;

procedure TEpiProjectSettings.LoadFromXml(Root: TDOMNode);
begin
  BackupInterval   := LoadNodeInt(Root, rsTimedBackupInterval);
  BackupOnShutdown := LoadNodeBool(Root,rsBackupOnShutdown);
  ShowFieldNames   := LoadNodeBool(Root, rsShowFieldNames);
  ShowFieldBorders := LoadNodeBool(Root, rsShowFieldBorders);
end;

function TEpiProjectSettings.ScrambleXml: boolean;
begin
  Result := TEpiDocument(RootOwner).XMLSettings.Scrambled;
end;

end.

