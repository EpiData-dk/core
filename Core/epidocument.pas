unit epidocument;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, epicustomclass, episettings, epiadmin, epistudy, epidatafile, epirelate,
  XMLRead, DOM, epidatatypes;

type

  { TEpiDocument }

  TEpiDocument = class(TEpiCustomClass)
  private
    FAdmin: TEpiAdmin;
    FDataFiles: TEpiDataFiles;
    FFileVersion: Integer;
    FRelates: TEpiRelates;
    FSettings: TEpiSettings;
    FStudy: TEpiStudy;
    function GetOnPassword: TRequestPasswordEvent;
    procedure SetAdmin(const AValue: TEpiAdmin);
    procedure SetDataFiles(const AValue: TEpiDataFiles);
    procedure SetFileVersion(const AValue: Integer);
    procedure SetOnPassword(const AValue: TRequestPasswordEvent);
    procedure SetRelates(const AValue: TEpiRelates);
    procedure SetSettings(const AValue: TEpiSettings);
    procedure SetStudy(const AValue: TEpiStudy);
    procedure WriteSettings(St: TStream; Lvl: integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure  LoadFromFile(const AFileName: string);
    procedure  LoadFromXml(Root: TDOMNode);
    procedure  SaveToStream(St: TStream; Lvl: integer = 0;
      IncludeHeader: boolean = true);
    Property   Settings: TEpiSettings read FSettings write SetSettings;
    Property   Admin: TEpiAdmin read FAdmin write SetAdmin;
    Property   Study: TEpiStudy read FStudy write SetStudy;
    Property   DataFiles: TEpiDataFiles read FDataFiles write SetDataFiles;
    Property   Relates: TEpiRelates read FRelates write SetRelates;
    Property   FileVersion: Integer read FFileVersion write SetFileVersion;
    property   OnPassword:  TRequestPasswordEvent read GetOnPassword write SetOnPassword;
  end;

implementation

uses
  epistringutils, epidataglobals;

{ TEpiDocument }

procedure TEpiDocument.SetAdmin(const AValue: TEpiAdmin);
begin
  if FAdmin = AValue then exit;
  FAdmin := AValue;
end;

function TEpiDocument.GetOnPassword: TRequestPasswordEvent;
begin
  result := Admin.OnPassword;
end;

procedure TEpiDocument.SetDataFiles(const AValue: TEpiDataFiles);
begin
  if FDataFiles = AValue then exit;
  FDataFiles := AValue;
end;

procedure TEpiDocument.SetFileVersion(const AValue: Integer);
begin
  if FFileVersion = AValue then exit;
  FFileVersion := AValue;
end;

procedure TEpiDocument.SetOnPassword(const AValue: TRequestPasswordEvent);
begin
  Admin.OnPassword := AValue;
end;

procedure TEpiDocument.SetRelates(const AValue: TEpiRelates);
begin
  if FRelates = AValue then exit;
  FRelates := AValue;
end;

procedure TEpiDocument.SetSettings(const AValue: TEpiSettings);
begin
  if FSettings = AValue then exit;
  FSettings := AValue;
end;

procedure TEpiDocument.SetStudy(const AValue: TEpiStudy);
begin
  if FStudy = AValue then exit;
  FStudy := AValue;
end;

procedure TEpiDocument.WriteSettings(St: TStream; Lvl: integer);
var
  TmpStr: String;
begin
  TmpStr :=
    Ins(Lvl) + '<Settings>' + LineEnding +
    Ins(Lvl + 1) + '<Version>' + IntToStr(FileVersion) + '</Version>' + LineEnding +
    Ins(Lvl + 1) + '<DateSeparator>' + EpiInternalFormatSettings.DateSeparator + '</DateSeparator>' + LineEnding +
    Ins(Lvl + 1) + '<DecimalSeparator>' + EpiInternalFormatSettings.DecimalSeparator + '</DecimalSeparator>' + LineEnding +
    Ins(Lvl + 1) + '<MissingString>' + StringToXml(TEpiStringField.DefaultMissing) + '</MissingMark>' + LineEnding +
    Ins(Lvl) + '</Settings>' + LineEnding;
  St.Write(TmpStr[1], Length(TmpStr));
end;

constructor TEpiDocument.Create;
begin
  FSettings  := TEpiSettings.Create;
  FAdmin     := TEpiAdmin.Create(Self);
  FDataFiles := TEpiDataFiles.Create;
  FRelates   := TEpiRelates.Create;
  FStudy     := TEpiStudy.Create;
end;

destructor TEpiDocument.Destroy;
begin
  FSettings.Free;
  FAdmin.Free;
  FDataFiles.Free;
  FRelates.Free;
  FStudy.Free;
  inherited Destroy;
end;

procedure TEpiDocument.LoadFromFile(const AFileName: string);
var
  RecXml: TXMLDocument;
  RootNode: TDOMElement;
begin
  ReadXMLFile(RecXml, AFileName);

  // **********************
  // Global <EpiData> structure
  // **********************
  RootNode := RecXml.DocumentElement;
  LoadFromXml(RootNode);
  RecXml.Free;
end;

procedure TEpiDocument.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMElement;
begin
  // Root = <EpiData>

  // First read settings!
  // - we need to catch if this is a scrambled file and other important settings
  // - such as version of the file.

  Node := TDOMElement(Root.FindNode('Settings'));
  Settings.LoadFromXml(Node);

  Node := TDOMElement(Root.FindNode('Settings'));
  Admin.LoadFromXml(Node, Settings);
end;

procedure TEpiDocument.SaveToStream(St: TStream; Lvl: integer;
  IncludeHeader: boolean);
var
  TmpStr: String;
begin

  // **********************
  // Global <EPIDATA> structure
  // **********************
  if IncludeHeader then
  begin
    TmpStr := '<?xml version="1.0" encoding="utf-8"?>' + LineEnding;
    St.Write(TmpStr[1], Length(TmpStr));
  end;

  TmpStr := Ins(Lvl) + '<EpiData>' + LineEnding;
  St.Write(TmpStr[1], Length(TmpStr));

  WriteSettings(St, Lvl + 1);
  Admin.SaveToStream(St, Lvl + 1);
  Study.SaveToStream(St, Lvl + 1);
  DataFiles.SaveToStream(St, Lvl + 1);
//  Relates.SaveToStream(St, Lvl + 1);

  TmpStr := Ins(Lvl) + '</EpiData>' + LineEnding;
  St.Write(TmpStr[1], Length(TmpStr));
end;

end.
