unit episettings;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epicustomclass, DOM;

type

  { TEpiSettings }

  TEpiSettings = class(TEpiCustomClass)
  strict private
    // Owner is TEpiDocument.
    FOwner: TObject;
  private
    FDateSeparator: string;
    FDecimalSeparator: string;
    // Clear Text master password for all scrambling.
    // -- although clear text here means a sequence of 16 random bytes.
    FMasterPassword: string;
    FMissingString: string;
    FScrambled: boolean;
    FVersion: integer;
    procedure SetDateSeparator(const AValue: string);
    procedure SetDecimalSeparator(const AValue: string);
    procedure SetMasterPassword(const AValue: string);
    procedure SetMissingString(const AValue: string);
    procedure SetScrambled(const AValue: boolean);
    procedure SetVersion(const AValue: integer);
  public
    constructor Create(AOwner: TObject);
    destructor Destroy; override;
    procedure  SaveToStream(St: TStream; Lvl: integer); override;
    procedure  LoadFromXml(Root: TDOMNode); override;
    property   Version: integer read FVersion write SetVersion;
    property   DateSeparator: string read FDateSeparator write SetDateSeparator;
    property   DecimalSeparator: string read FDecimalSeparator write SetDecimalSeparator;
    property   MissingString: string read FMissingString write SetMissingString;
    property   MasterPassword: string read FMasterPassword write SetMasterPassword;
    property   Scrambled: boolean read FScrambled write SetScrambled;
  end;

implementation

uses
  epidataglobals, epidatafile, DCPbase64;

{ TEpiSettings }

procedure TEpiSettings.SetDateSeparator(const AValue: string);
begin
  if FDateSeparator = AValue then exit;
  FDateSeparator := AValue;
end;

procedure TEpiSettings.SetDecimalSeparator(const AValue: string);
begin
  if FDecimalSeparator = AValue then exit;
  FDecimalSeparator := AValue;
end;

procedure TEpiSettings.SetMasterPassword(const AValue: string);
begin
  if FMasterPassword = AValue then exit;
  FMasterPassword := AValue;
end;

procedure TEpiSettings.SetMissingString(const AValue: string);
begin
  if FMissingString = AValue then exit;
  FMissingString := AValue;
end;

procedure TEpiSettings.SetScrambled(const AValue: boolean);
begin
  if FScrambled = AValue then exit;
  FScrambled := AValue;
end;

procedure TEpiSettings.SetVersion(const AValue: integer);
begin
  if FVersion = AValue then exit;
  FVersion := AValue;
end;

constructor TEpiSettings.Create(AOwner: TObject);
var
  Key: array[0..3] of LongInt;
  KeyByte: array[0..3*SizeOf(LongInt)] of Char absolute Key;
  i: Integer;
begin
  FOwner := AOwner;

  Randomize;

  {$IFNDEF EPI_RELEASE}
  // A little speedier and more secure (uses full spectre af possible byte combinations=
  for i := 0 to 3 do
    Key[i] := Random(maxLongint - 1) + 1;
  {$ELSE EPI_DEBUG}
  // A little slower and only uses a selection of printable chars.
  for i := 0 to SizeOf(KeyByte) - 1 do
    KeyByte[i] := Char(Random(90) + 33);
  {$ENDIF}

  MasterPassword := String(KeyByte);
//  MasterPassword := 'qwerty';
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
    Ins(Lvl) + '<Settings>' + LineEnding +
    Ins(Lvl + 1) + '<Version>' + IntToStr(Version) + '</Version>' + LineEnding +
    Ins(Lvl + 1) + '<Scrambled>' + BoolToStr(Scrambled, 'true', 'false') + '</Scrambled>' + LineEnding +
    Ins(Lvl + 1) + '<DateSeparator>' + EpiInternalFormatSettings.DateSeparator + '</DateSeparator>' + LineEnding +
    Ins(Lvl + 1) + '<DecimalSeparator>' + EpiInternalFormatSettings.DecimalSeparator + '</DecimalSeparator>' + LineEnding +
    Ins(Lvl + 1) + '<MissingString>' + StringToXml(TEpiStringField.DefaultMissing) + '</MissingString>' + LineEnding +
    Ins(Lvl) + '</Settings>' + LineEnding;
  St.Write(S[1], Length(S));
end;

procedure TEpiSettings.LoadFromXml(Root: TDOMNode);
var
  ElemNode: TDOMElement;
begin
  // Root = <Settings>

  // Version: so far we only got ver. 0
  ElemNode := TDOMElement(Root.FindNode('Version'));
  if not Assigned(ElemNode) then
    ReportXmlError(EPI_FILE_VERSION_ERROR, 0, 'No format version specified.', []);

  // Scrambled filed
  ElemNode := TDOMElement(Root.FindNode('Scrambled'));
  Scrambled := WideLowerCase(ElemNode.TextContent) = 'true';

  // Date Separator
  ElemNode := TDOMElement(Root.FindNode('DateSeparator'));
  if Assigned(ElemNode) then
    DateSeparator := UTF8Encode(ElemNode.TextContent)[1]
  else
    DateSeparator := EpiInternalFormatSettings.DateSeparator;

  // Decimal Separator
  ElemNode := TDOMElement(Root.FindNode('DecimalSeparator'));
  if Assigned(ElemNode) then
    DecimalSeparator := UTF8Encode(ElemNode.TextContent)[1]
  else
    DecimalSeparator := EpiInternalFormatSettings.DecimalSeparator;

  // Missing mark
  ElemNode := TDOMElement(Root.FindNode('MissingString'));
  if Assigned(ElemNode) then
    MissingString := UTF8Encode(ElemNode.TextContent)
  else
    MissingString := TEpiStringField.DefaultMissing;
end;

end.

