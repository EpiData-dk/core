unit episettings;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epicustombase, DOM;

type

  { TEpiSettings }

  // esce = Epi Setting Change Event
  TEpiSettingChangeEvent = (
    esceVersion, esceDateSep, esceTimeSep, esceDecSep, esceMissing, esceScramble
  );

  TEpiSettings = class(TEpiCustomBase)
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

implementation

{ TEpiSettings }

procedure TEpiSettings.SetDateSeparator(const AValue: string);
var
  Val: String;
begin
  if FDateSeparator = AValue then exit;
  Val := FDateSeparator;
  FDateSeparator := AValue;
  DefaultFormatSettings.DateSeparator := FDateSeparator[1];
  DoChange(eegSetting, Word(esceDateSep), @Val);
end;

procedure TEpiSettings.SetDecimalSeparator(const AValue: string);
var
  Val: String;
begin
  if FDecimalSeparator = AValue then exit;
  Val := FDecimalSeparator;
  FDecimalSeparator := AValue;
  DefaultFormatSettings.DecimalSeparator := FDecimalSeparator[1];
  DoChange(eegSetting, Word(esceDecSep), @Val);
end;

procedure TEpiSettings.SetMissingString(const AValue: string);
var
  Val: String;
begin
  if FMissingString = AValue then exit;
  Val := FMissingString;
  FMissingString := AValue;
  DoChange(eegSetting, Word(esceMissing), @Val);
end;

procedure TEpiSettings.SetScrambled(const AValue: boolean);
var
  Val: Boolean;
begin
  if FScrambled = AValue then exit;
  Val := FScrambled;
  FScrambled := AValue;
  DoChange(eegSetting, Word(esceScramble), @Val);
end;

procedure TEpiSettings.SetTimeSeparator(const AValue: string);
var
  Val: String;
begin
  if FTimeSeparator = AValue then exit;
  Val := FTimeSeparator;
  FTimeSeparator := AValue;
  DefaultFormatSettings.TimeSeparator := TimeSeparator[1];
  DoChange(eegSetting, Word(esceTimeSep), @Val);
end;

procedure TEpiSettings.SetVersion(const AValue: integer);
var
  Val: LongInt;
begin
  if FVersion = AValue then exit;
  Val := FVersion;
  FVersion := AValue;
  DoChange(eegSetting, Word(esceVersion), @Val);
end;

constructor TEpiSettings.Create(AOwner: TEpiCustomBase);
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

destructor TEpiSettings.Destroy;
begin
  inherited Destroy;
end;

function TEpiSettings.XMLName: string;
begin
  Result := rsSettings;
end;

function TEpiSettings.SaveToXml(Content: String; Lvl: integer): string;
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

procedure TEpiSettings.LoadFromXml(Root: TDOMNode);
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

end.

