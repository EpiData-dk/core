unit epidocument;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, XMLRead, DOM,
  episettings, epiadmin, epidatafiles,
  epistudy, //epirelate,
  epicustombase;

type

  { TEpiDocument }

  TEpiDocument = class(TEpiCustomBase)
  private
    FAdmin: TEpiAdmin;
    FDataFiles: TEpiDataFiles;
//    FRelates: TEpiRelates;
    FSettings: TEpiSettings;
    FStudy: TEpiStudy;
    function GetOnPassword: TRequestPasswordEvent;
    procedure SetOnPassword(const AValue: TRequestPasswordEvent);
  public
    constructor Create;
    destructor Destroy; override;
    procedure  LoadFromFile(const AFileName: string);
    procedure  LoadFromStream(const St: TStream);
    procedure  LoadFromXml(Root: TDOMNode); override;
    procedure  SaveToStream(St: TStream; Lvl: integer = 0;
      IncludeHeader: boolean = true);
    Property   Settings: TEpiSettings read FSettings;
    Property   Admin: TEpiAdmin read FAdmin;
    Property   Study: TEpiStudy read FStudy;
    Property   DataFiles: TEpiDataFiles read FDataFiles;
//    Property   Relates: TEpiRelates read FRelates;
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

constructor TEpiDocument.Create;
begin
  inherited Create(nil);
  FSettings  := TEpiSettings.Create(Self);
  FAdmin     := TEpiAdmin.Create(Self);
  FDataFiles := TEpiDataFiles.Create(Self);
//  FRelates   := TEpiRelates.Create(Self);
  FStudy     := TEpiStudy.Create(Self);
end;

destructor TEpiDocument.Destroy;
begin
  FSettings.Free;
  FAdmin.Free;
  FDataFiles.Free;
//  FRelates.Free;
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
  LoadNode(Node, Root, rsSettings, true);
  Settings.LoadFromXml(Node);

  LoadNode(Node, Root, rsAdmin, true);
  Admin.LoadFromXml(Node);

  LoadNode(Node, Root, rsStudy, true);
  Study.LoadFromXml(Node);

  if LoadNode(Node, Root, rsDataFiles, false) then
    DataFiles.LoadFromXml(Node);
 {
  LoadNode(Node, Root, rsRelates, true);
  Relates.LoadFromXml(Node);    }
end;

procedure TEpiDocument.SaveToStream(St: TStream; Lvl: integer;
  IncludeHeader: boolean);
var
  TmpStr: String;
begin
  if IncludeHeader then
  begin
    TmpStr := '<?xml version="1.0" encoding="utf-8"?>' + LineEnding;
    SaveStream(St, TmpStr);
  end;

  // **********************
  // Global <EpiData> structure
  // **********************
  SaveClasses(St, Lvl,
    [Settings, Admin, Study, DataFiles
//     Relates
     ],
     rsEpiData
  );
end;

end.
