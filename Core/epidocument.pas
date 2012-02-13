unit epidocument;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, XMLRead, DOM,
  episettings, epiadmin, epidatafiles,
  epistudy, epirelations, epivaluelabels,
  epicustombase;

type

  { TEpiDocument }

  TEpiDocument = class(TEpiCustomBase)
  private
    FAdmin: TEpiAdmin;
    FLoading: boolean;
    FPassWord: string;
    FProjectSettings: TEpiProjectSettings;
    FValueLabelSets: TEpiValueLabelSets;
    FVersion: integer;
    FXMLSettings: TEpiXMLSettings;
    FStudy: TEpiStudy;
    FDataFiles: TEpiDataFiles;
    FRelations: TEpiRelations;
    function   GetOnPassword: TRequestPasswordEvent;
    procedure  SetOnPassword(const AValue: TRequestPasswordEvent);
  protected
    procedure  SetModified(const AValue: Boolean); override;
    function   SaveAttributesToXml: string; override;
  public
    constructor Create(Const LangCode: string);
    destructor Destroy; override;
    function   XMLName: string; override;
    procedure  LoadFromFile(const AFileName: string);
    procedure  LoadFromStream(const St: TStream);
    procedure  LoadFromXml(Root: TDOMNode); override;
    function   SaveToXml(Lvl: integer = 0;
      IncludeHeader: boolean = true): string;
    procedure  SaveToStream(Const St: TStream);
    procedure  SaveToFile(Const AFileName: string);
    Property   XMLSettings: TEpiXMLSettings read FXMLSettings;
    property   ProjectSettings: TEpiProjectSettings read FProjectSettings;
    Property   Admin: TEpiAdmin read FAdmin;
    Property   Study: TEpiStudy read FStudy;
    Property   ValueLabelSets: TEpiValueLabelSets read FValueLabelSets;
    Property   DataFiles: TEpiDataFiles read FDataFiles;
    Property   Relations: TEpiRelations read FRelations;
    property   OnPassword:  TRequestPasswordEvent read GetOnPassword write SetOnPassword;
    property   Loading: boolean read FLoading;
    Property   Version: integer read FVersion;
    // EpiData XML Version 2 perperties:
    property   PassWord: string read FPassWord write FPassWord;
  end;

implementation

uses
  epimiscutils;

{ TEpiDocument }

function TEpiDocument.GetOnPassword: TRequestPasswordEvent;
begin
  result := Admin.OnPassword;
end;

procedure TEpiDocument.SetOnPassword(const AValue: TRequestPasswordEvent);
begin
  Admin.OnPassword := AValue;
end;

procedure TEpiDocument.SetModified(const AValue: Boolean);
begin
  inherited SetModified(AValue);
end;

function TEpiDocument.SaveAttributesToXml: string;
begin
  Result :=
    inherited SaveAttributesToXml +
    SaveAttr('xmlns', 'http://www.epidata.dk/XML/1.0') +
    SaveAttr('xmlns:xsi', 'http://www.w3.org/2001/XMLSchema-instance') +
    SaveAttr('xsi:schemaLocation', 'http://www.epidata.dk/XML/1.0 http://www.epidata.dk/XML/1.0/epx.xsd') +
    SaveAttr(rsVersionAttr, Version) +
    SaveAttr('xml:lang', DefaultLang);

  // Version 2 Properties:
  if PassWord <> '' then
    Result += SaveAttr(rsPassword, StrToSHA1Base64(PassWord));
end;

constructor TEpiDocument.Create(const LangCode: string);
begin
  inherited Create(nil);
  FVersion         := EPI_XML_DATAFILE_VERSION;
  FXMLSettings     := TEpiXMLSettings.Create(Self);
  FProjectSettings := TEpiProjectSettings.Create(Self);
  FAdmin           := TEpiAdmin.Create(Self);
  FStudy           := TEpiStudy.Create(Self);
  FValueLabelSets  := TEpiValueLabelSets.Create(Self);
  FValueLabelSets.ItemOwner := true;
  FDataFiles       := TEpiDataFiles.Create(Self);
  FDataFiles.ItemOwner := true;
  FRelations       := TEpiRelations.Create(Self);

  RegisterClasses([XMLSettings, ProjectSettings, {Admin,} Study, ValueLabelSets, DataFiles, Relations]);

  SetLanguage(LangCode, true);
  // Needed to reset initial XMLSettings.
  Modified := false;
end;

destructor TEpiDocument.Destroy;
begin
  FRelations.Free;
  FDataFiles.Free;
  FStudy.Free;
  FAdmin.Free;
  FXMLSettings.Free;
  FProjectSettings.Free;
  FValueLabelSets.Free;
  inherited Destroy;
end;

function TEpiDocument.XMLName: string;
begin
  Result := rsEpiData;
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
  Node: TDOMNode;
  PW, Login, UserPW: String;
begin
  // Root = <EpiData>
  FLoading := true;

  // First read version no!
  FVersion := LoadAttrInt(Root, rsVersionAttr);
  // Then language!
  SetLanguage(LoadAttrString(Root, 'xml:lang'), true);
  // And last - file settings.
  LoadNode(Node, Root, rsSettings, true);
  XMLSettings.LoadFromXml(Node);

  // XML Version 2:
  if Version >= 2 then
  begin
    PW := LoadAttrString(Root, rsPassword, '', false);

    if (PW <> '') and (Assigned(OnPassword)) then
      OnPassword(Self, Login, UserPW);

    if (PW <> '') and (StrToSHA1Base64(UserPW) <> PW) then
      Raise EEpiBadPassword.Create('Incorrect Password');

    PassWord := UserPW;
  end;

  LoadNode(Node, Root, rsStudy, true);
  Study.LoadFromXml(Node);

  // TODO : Include in later versions.
//  LoadNode(Node, Root, rsAdmin, true);
//  Admin.LoadFromXml(Node);

  if LoadNode(Node, Root, rsProjectSettings, false) then
    ProjectSettings.LoadFromXml(Node);

  if LoadNode(Node, Root, rsValueLabelSets, false) then
    ValueLabelSets.LoadFromXml(Node);

  if LoadNode(Node, Root, rsDataFiles, false) then
    DataFiles.LoadFromXml(Node);

  if LoadNode(Node, Root, rsRelations, false) then
    Relations.LoadFromXml(Node);

  FLoading := false;
  Modified := false;
  FVersion := EPI_XML_DATAFILE_VERSION;
end;

function TEpiDocument.SaveToXml(Lvl: integer; IncludeHeader: boolean): string;
var
  Content: string;
begin
  if IncludeHeader then
    Result := '<?xml version="1.0" encoding="utf-8"?>' + LineEnding;

  // Inherited saves everything, since the the classes have been registered in Create.
  Result += inherited SaveToXml(Content, Lvl);
end;

procedure TEpiDocument.SaveToStream(const St: TStream);
var
  S: String;
begin
  S := SaveToXml(0);
  St.Write(S[1], Length(S));
end;

procedure TEpiDocument.SaveToFile(const AFileName: string);
var
  Fs: TFileStream;
begin
  Fs := TFileStream.Create(AFileName, fmCreate);
  SaveToStream(Fs);
  Fs.Free;
end;

end.
