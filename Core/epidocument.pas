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
    FRelates: TEpiRelates;
    FSettings: TEpiSettings;
    FStudy: TEpiStudy;
    function GetOnPassword: TRequestPasswordEvent;
    procedure SetAdmin(const AValue: TEpiAdmin);
    procedure SetDataFiles(const AValue: TEpiDataFiles);
    procedure SetOnPassword(const AValue: TRequestPasswordEvent);
    procedure SetRelates(const AValue: TEpiRelates);
    procedure SetSettings(const AValue: TEpiSettings);
    procedure SetStudy(const AValue: TEpiStudy);
  public
    constructor Create;
    destructor Destroy; override;
    procedure  LoadFromFile(const AFileName: string);
    procedure  LoadFromStream(const St: TStream);
    procedure  LoadFromXml(Root: TDOMNode);
    procedure  SaveToStream(St: TStream; Lvl: integer = 0;
      IncludeHeader: boolean = true);
    Property   Settings: TEpiSettings read FSettings write SetSettings;
    Property   Admin: TEpiAdmin read FAdmin write SetAdmin;
    Property   Study: TEpiStudy read FStudy write SetStudy;
    Property   DataFiles: TEpiDataFiles read FDataFiles write SetDataFiles;
    Property   Relates: TEpiRelates read FRelates write SetRelates;
    property   OnPassword:  TRequestPasswordEvent read GetOnPassword write SetOnPassword;
  end;

implementation

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

constructor TEpiDocument.Create;
begin
  FSettings  := TEpiSettings.Create(Self);
  FAdmin     := TEpiAdmin.Create(Self);
  FDataFiles := TEpiDataFiles.Create(Self);
  FRelates   := TEpiRelates.Create(Self);
  FStudy     := TEpiStudy.Create(Self);
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
  St: TFileStream;
begin
  St := TFileStream.Create(AFileName, fmOpenRead);
  LoadFromStream(St);
  St.Free;
end;

procedure TEpiDocument.LoadFromStream(const St: TStream);
var
  RecXml: TXMLDocument;
  RootNode: TDOMElement;
begin
  ReadXMLFile(RecXml, St);

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

  Node := TDOMElement(Root.FindNode('Admin'));
  Admin.LoadFromXml(Node);

  Node := TDOMElement(Root.FindNode('Study'));
  Study.LoadFromXml(Node);

  Node := TDOMElement(Root.FindNode('DataFiles'));
  DataFiles.LoadFromXml(Node);

  Node := TDOMElement(Root.FindNode('Relates'));
  Relates.LoadFromXml(Node);
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

  Settings.SaveToStream(St, Lvl + 1);
  Admin.SaveToStream(St, Lvl + 1);
  Study.SaveToStream(St, Lvl + 1);
  DataFiles.SaveToStream(St, Lvl + 1);
  Relates.SaveToStream(St, Lvl + 1);

  TmpStr := Ins(Lvl) + '</EpiData>' + LineEnding;
  St.Write(TmpStr[1], Length(TmpStr));
end;

end.
