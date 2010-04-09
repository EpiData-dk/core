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
    esceVersion, esceDateSep, esceDecSep, esceMissing, esceScramble
  );

  TEpiSettings = class(TEpiCustomBase)
  private
    FDateSeparator: string;
    FDecimalSeparator: string;
    FMissingString: string;
    FScrambled: boolean;
    FVersion: integer;
    procedure SetDateSeparator(const AValue: string);
    procedure SetDecimalSeparator(const AValue: string);
    procedure SetMissingString(const AValue: string);
    procedure SetScrambled(const AValue: boolean);
    procedure SetVersion(const AValue: integer);
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
    procedure  SaveToStream(St: TStream; Lvl: integer); override;
    procedure  LoadFromXml(Root: TDOMNode); override;
    property   Version: integer read FVersion write SetVersion;
    property   DateSeparator: string read FDateSeparator write SetDateSeparator;
    property   DecimalSeparator: string read FDecimalSeparator write SetDecimalSeparator;
    property   MissingString: string read FMissingString write SetMissingString;
    property   Scrambled: boolean read FScrambled write SetScrambled;
  end;

implementation

const
  rsVersion = 'Version';
  rsScrambled = 'Scrambled';
  rsDateSep = 'DateSeparator';
  rsDecSep = 'DecimalSeparator';
  rsMissingStr = 'MissingString';
  rsSettings = 'Settings';


{ TEpiSettings }

procedure TEpiSettings.SetDateSeparator(const AValue: string);
var
  Val: String;
begin
  if FDateSeparator = AValue then exit;
  Val := FDateSeparator;
  FDateSeparator := AValue;
  DoChange(Word(eegSetting), Word(esceDateSep), @Val);
end;

procedure TEpiSettings.SetDecimalSeparator(const AValue: string);
var
  Val: String;
begin
  if FDecimalSeparator = AValue then exit;
  Val := FDecimalSeparator;
  FDecimalSeparator := AValue;
  DoChange(Word(eegSetting), Word(esceDecSep), @Val);
end;

procedure TEpiSettings.SetMissingString(const AValue: string);
var
  Val: String;
begin
  if FMissingString = AValue then exit;
  Val := FMissingString;
  FMissingString := AValue;
  DoChange(Word(eegSetting), Word(esceMissing), @Val);
end;

procedure TEpiSettings.SetScrambled(const AValue: boolean);
var
  Val: Boolean;
begin
  if FScrambled = AValue then exit;
  Val := FScrambled;
  FScrambled := AValue;
  DoChange(Word(eegSetting), Word(esceScramble), @Val);
end;

procedure TEpiSettings.SetVersion(const AValue: integer);
var
  Val: LongInt;
begin
  if FVersion = AValue then exit;
  Val := FVersion;
  FVersion := AValue;
  DoChange(Word(eegSetting), Word(esceVersion), @Val);
end;

constructor TEpiSettings.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
end;

destructor TEpiSettings.Destroy;
begin
  inherited Destroy;
end;

procedure TEpiSettings.SaveToStream(St: TStream; Lvl: integer);
var
  S: String;
begin
  S :=
    SaveNode(Lvl + 1, rsVersion,    Version) +
    SaveNode(Lvl + 1, rsScrambled,  Scrambled) +
    SaveNode(Lvl + 1, rsDateSep,    DateSeparator) +
    SaveNode(Lvl + 1, rsDecSep,     DecimalSeparator) +
    SaveNode(Lvl + 1, rsMissingStr, MissingString);
  SaveStream(St, SaveNode(Lvl, rsSettings, S));
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
  DecimalSeparator := LoadNodeString(Root, rsDecSep)[1];
  MissingString    := LoadNodeString(Root, rsMissingStr);
end;

end.

