unit episettings;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epicustomclass, DOM;

type

  { TEpiSettings }

  TEpiSettings = class(TEpiCustomClass)
  private
    FDateSeparator: string;
    FDecimalSeparator: string;
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
    constructor Create;
    destructor Destroy; override;
    procedure  LoadFromXml(Root: TDOMNode);
    property   Version: integer read FVersion write SetVersion;
    property   DateSeparator: string read FDateSeparator write SetDateSeparator;
    property   DecimalSeparator: string read FDecimalSeparator write SetDecimalSeparator;
    property   MissingString: string read FMissingString write SetMissingString;
    property   MasterPassword: string read FMasterPassword write SetMasterPassword;
    property   Scrambled: boolean read FScrambled write SetScrambled;
  end;

implementation

uses
  epidataglobals, epidatafile;

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

constructor TEpiSettings.Create;
begin

end;

destructor TEpiSettings.Destroy;
begin
  inherited Destroy;
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

  // Scrambled filed
  ElemNode := TDOMElement(Root.FindNode('Scrambled'));
  Scrambled := Assigned(ElemNode);
end;

end.

