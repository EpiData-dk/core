unit epidocument;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, XMLRead, DOM,
  episettings, epiadmin, epidatafiles,
  epistudy, epirelations,
  epicustombase;

type

  { TEpiDocument }

  TEpiDocument = class(TEpiCustomBase)
  private
    FAdmin: TEpiAdmin;
    FProjectSettings: TEpiProjectSettings;
    FXMLSettings: TEpiXMLSettings;
    FStudy: TEpiStudy;
    FDataFiles: TEpiDataFiles;
    FRelations: TEpiRelations;
    function   GetOnPassword: TRequestPasswordEvent;
    procedure  SetOnPassword(const AValue: TRequestPasswordEvent);
  protected
    procedure SetModified(const AValue: Boolean); override;
  public
    constructor Create(AOwner: TComponent; Const LangCode: string);
    constructor Create(AOwner: TComponent); override;
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
    Property   DataFiles: TEpiDataFiles read FDataFiles;
    Property   Relations: TEpiRelations read FRelations;
    property   OnPassword:  TRequestPasswordEvent read GetOnPassword write SetOnPassword;
  end;

implementation

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
//  State;
//  if Assigned(Study) then
//    Study.ModifiedDate := now;
end;

constructor TEpiDocument.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FXMLSettings     := TEpiXMLSettings.Create(Self);
  FProjectSettings := TEpiProjectSettings.Create(Self);
  FAdmin           := TEpiAdmin.Create(Self);
  FStudy           := TEpiStudy.Create(Self);
  FDataFiles       := TEpiDataFiles.Create(Self);
  FDataFiles.ItemOwner := true;
  FRelations       := TEpiRelations.Create(Self);

  RegisterClasses([XMLSettings, ProjectSettings, Admin, Study, DataFiles, Relations]);
end;

constructor TEpiDocument.Create(AOwner: TComponent; const LangCode: string);
begin
  Create(AOwner);

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
  Node: TDOMElement;
begin
  // Root = <EpiData>

  // First read XMLSettings!
  // - we need to catch if this is a scrambled file and other important XMLSettings
  // - such as version of the file, formatsettings, etc...
  LoadNode(Node, Root, rsSettings, true);
  XMLSettings.LoadFromXml(Node);

  if LoadNode(Node, Root, rsProjectSettings, false) then
    ProjectSettings.LoadFromXml(Node);

  LoadNode(Node, Root, rsAdmin, true);
  Admin.LoadFromXml(Node);

  LoadNode(Node, Root, rsStudy, true);
  Study.LoadFromXml(Node);

  if LoadNode(Node, Root, rsDataFiles, false) then
    DataFiles.LoadFromXml(Node);

  if LoadNode(Node, Root, rsRelations, false) then
    Relations.LoadFromXml(Node);

  Modified := false;
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
