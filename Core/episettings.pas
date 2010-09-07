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
    constructor Create(AOwner: TEpiCustomBase); override;
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
  end;

  { TEpiProjectSettings }

  TEpiProjectSettings = class(TEpiCustomBase)
  private
    FShowFieldBorders: Boolean;
    FShowFieldNames: Boolean;
    procedure   SetShowFieldBorders(const AValue: Boolean);
    procedure   SetShowFieldNames(const AValue: Boolean);
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor  Destroy; override;
    function    XMLName: string; override;
    function    SaveToXml(Content: String; Lvl: integer): string; override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    function    ScrambleXml: boolean; override;
    property    ShowFieldNames: Boolean read FShowFieldNames write SetShowFieldNames;
    property    ShowFieldBorders: Boolean read FShowFieldBorders write SetShowFieldBorders;
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
  DefaultFormatSettings.DateSeparator := FDateSeparator[1];
  DoChange(eegSetting, Word(esceDateSep), @Val);
end;

procedure TEpiXMLSettings.SetDecimalSeparator(const AValue: string);
var
  Val: String;
begin
  if FDecimalSeparator = AValue then exit;
  Val := FDecimalSeparator;
  FDecimalSeparator := AValue;
  DefaultFormatSettings.DecimalSeparator := FDecimalSeparator[1];
  DoChange(eegSetting, Word(esceDecSep), @Val);
end;

procedure TEpiXMLSettings.SetMissingString(const AValue: string);
var
  Val: String;
begin
  if FMissingString = AValue then exit;
  Val := FMissingString;
  FMissingString := AValue;
  DoChange(eegSetting, Word(esceMissing), @Val);
end;

procedure TEpiXMLSettings.SetScrambled(const AValue: boolean);
var
  Val: Boolean;
begin
  if FScrambled = AValue then exit;
  Val := FScrambled;
  FScrambled := AValue;
  DoChange(eegSetting, Word(esceScramble), @Val);
end;

procedure TEpiXMLSettings.SetTimeSeparator(const AValue: string);
var
  Val: String;
begin
  if FTimeSeparator = AValue then exit;
  Val := FTimeSeparator;
  FTimeSeparator := AValue;
  DefaultFormatSettings.TimeSeparator := TimeSeparator[1];
  DoChange(eegSetting, Word(esceTimeSep), @Val);
end;

procedure TEpiXMLSettings.SetVersion(const AValue: integer);
var
  Val: LongInt;
begin
  if FVersion = AValue then exit;
  Val := FVersion;
  FVersion := AValue;
  DoChange(eegSetting, Word(esceVersion), @Val);
end;

constructor TEpiXMLSettings.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  Version := 0;
  Scrambled := false;

  DefaultFormatSettings.ShortTimeFormat := 'HH:NN';
  DefaultFormatSettings.LongTimeFormat := 'HH:NN:SS';

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
begin
  if FShowFieldBorders = AValue then exit;
  FShowFieldBorders := AValue;
end;

procedure TEpiProjectSettings.SetShowFieldNames(const AValue: Boolean);
begin
  if FShowFieldNames = AValue then exit;
  FShowFieldNames := AValue;
end;

constructor TEpiProjectSettings.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);

  FShowFieldBorders := true;
  FShowFieldNames   := true;
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
    SaveNode(Lvl + 1, rsShowFieldNames,   ShowFieldNames) +
    SaveNode(Lvl + 1, rsShowFieldBorders, ShowFieldBorders);
  Result := inherited SaveToXml(Result, Lvl);
end;

procedure TEpiProjectSettings.LoadFromXml(Root: TDOMNode);
begin
  ShowFieldNames   := LoadNodeBool(Root, rsShowFieldNames);
  ShowFieldBorders := LoadNodeBool(Root, rsShowFieldBorders);
end;

function TEpiProjectSettings.ScrambleXml: boolean;
begin
  Result := TEpiDocument(RootOwner).XMLSettings.Scrambled;
end;

end.

