unit epiexport_ddi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidocument, epidatafiles, epidatafilestypes, epivaluelabels,
  epieximtypes, epiexportsettings, DOM;

type

  { TEpiDDIExport }

  TEpiDDIExport = class
  private
    EpiDoc:     TEpiDocument;
    XMLDoc:     TXMLDocument;
    DDIInstance: TDOMElement;
    DDIStudyUnit:    TDOMElement;
    DDIUniverseRef:  TDOMElement;

    // Helper methods.
    procedure AddAttrNameSpace(Elem: TDOMElement; Const NameSpace: string);
    procedure AddAttrID(Elem: TDOMElement; Const Prefix: string);
    procedure AddAttrTranslation(Elem: TDOMElement);
    procedure AddAttrLang(Elem: TDOMElement; Const Lang: string);

    // Block to produce.
    procedure BuildCitations;
    procedure BuildAbstract;
    procedure BuildUniverseRef;
    procedure BuildFunding;
    procedure BuildPurpose;
    procedure BuildCoverage;
    procedure BuildConceptualComponent;
    procedure BuildDataCollection;
    procedure BuildLogicalProduct;
    procedure BuildPhysicalDataProduct;
  public
    constructor Create;
    destructor  Destroy; override;
    function    ExportDDI(Const Settings: TEpiDDIExportSetting): boolean;
  end;

implementation

uses
  XMLWrite;

const
  NSreuseable = 'ddi:reusable:3_1';
  NSstudy     = 'ddi:studyunit:3_1';
  NSconcept   = 'ddi:conceptualcomponent:3_1';
  // .... todo!

{ TEpiDDIExport }

procedure TEpiDDIExport.AddAttrNameSpace(Elem: TDOMElement;
  const NameSpace: string);
begin
  Elem.SetAttribute('xmlns', 'ddi:' + NameSpace + ':3_1');
end;

procedure TEpiDDIExport.AddAttrID(Elem: TDOMElement; const Prefix: string);
var
  GUID: TGUID;
  S: String;
begin
  CreateGUID(GUID);
  S := GUIDToString(GUID);
  S := Prefix + Copy(S, 2, Length(S) - 2);
  Elem.SetAttribute('id', S);
end;

procedure TEpiDDIExport.AddAttrTranslation(Elem: TDOMElement);
begin
  // TODO : Expand on Core translation working.
  Elem.SetAttribute('translated', 'false');
  Elem.SetAttribute('translatable', 'true');
end;

procedure TEpiDDIExport.AddAttrLang(Elem: TDOMElement; const Lang: string);
begin
  Elem.SetAttribute('xml:lang', Lang);
end;

procedure TEpiDDIExport.BuildCitations;
var
  Citation: TDOMElement;
  Elem: TDOMElement;
begin
  Citation := XMLDoc.CreateElementNS(NSreuseable, 'Citation');
  DDIStudyUnit.AppendChild(Citation);

  Elem := XMLDoc.CreateElementNS(NSreuseable, 'Title');
  AddAttrTranslation(Elem);
  AddAttrLang(Elem, EpiDoc.CurrentLang);
  Elem.TextContent := EpiDoc.Study.Title.Text;
  Citation.AppendChild(Elem);

  Elem := XMLDoc.CreateElementNS(NSreuseable, 'Creator');
  Elem.TextContent := EpiDoc.Study.Author;
  Citation.AppendChild(Elem);
end;

procedure TEpiDDIExport.BuildAbstract;
var
  Abstract: TDOMElement;
  Content: TDOMElement;
begin
  Abstract := XMLDoc.CreateElementNS(NSstudy, 'Abstract');
  DDIStudyUnit.AppendChild(Abstract);
  AddAttrID(Abstract, 'abst');

  Content := XMLDoc.CreateElementNS(NSreuseable, 'Content');
  Abstract.AppendChild(Content);
  AddAttrTranslation(Content);
  AddAttrLang(Content, EpiDoc.DefaultLang);
  Content.TextContent := EpiDoc.Study.AbstractText.Text;
end;

procedure TEpiDDIExport.BuildUniverseRef;
begin
  // Universe Ref cannot be built fully at this instance, but append it as child to StudyUnit
  // to get the correct order of things.
  DDIUniverseRef := XMLDoc.CreateElementNS(NSreuseable, 'UniverseReference');
  DDIStudyUnit.AppendChild(DDIUniverseRef);

  // UniverseReference element still need an "ID" element, but that is created later
  // when the actual UniverseScheme is created.
end;

procedure TEpiDDIExport.BuildFunding;
begin
  EpiDoc.Study.Funding;
  Funding := XMLDoc.CreateElementNS();
end;

procedure TEpiDDIExport.BuildPurpose;
begin

end;

procedure TEpiDDIExport.BuildCoverage;
begin

end;

procedure TEpiDDIExport.BuildConceptualComponent;
begin

end;

procedure TEpiDDIExport.BuildDataCollection;
begin

end;

procedure TEpiDDIExport.BuildLogicalProduct;
begin

end;

procedure TEpiDDIExport.BuildPhysicalDataProduct;
begin

end;

constructor TEpiDDIExport.Create;
begin

end;

destructor TEpiDDIExport.Destroy;
begin
  inherited Destroy;
end;

function TEpiDDIExport.ExportDDI(const Settings: TEpiDDIExportSetting): boolean;
var
  Elem: TDOMElement;
  RefIDElem: TDOMElement;
begin
  Settings.SanetyCheck;

  EpiDoc := Settings.Doc;

  XMLDoc := TXMLDocument.Create;
  DDIInstance := XMLDoc.CreateElement('DDIInstance');
  XMLDoc.AppendChild(DDIInstance);
  DDIInstance := XMLDoc.DocumentElement;

  {
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  xmlns="ddi:instance:3_1"
  xsi:schemaLocation="ddi:instance:3_1 http://www.ddialliance.org/sites/default/files/schema/ddi3.1/instance.xsd"
  id="2fbc5af9-939d-4b95-999a-2920c38c9dc3"
  version="1.0.0"
  versionDate="2012-05-30T13:27:58.364+02:00"
  agency="dk.dda"}

  AddAttrNameSpace(DDIInstance, 'instance');
  AddAttrID(DDIInstance, 'inst-');
  with DDIInstance do
  begin
    SetAttribute('xmlns:xsi', 'http://www.w3.org/2001/XMLSchema-instance');
    SetAttribute('xmlns', 'ddi:instance:3_1');
    SetAttribute('xsi:schemaLocation', 'ddi:instance:3_1 http://www.ddialliance.org/sites/default/files/schema/ddi3.1/instance.xsd');
    // Version NO!
    SetAttribute('version', '1.0.0');
    SetAttribute('versionDate', FormatDateTime('YYYY/MM/DD"T"HH:MM:SS"."ZZZ', Now));
    SetAttribute('agency', 'dk.dda');
  end;

  DDIStudyUnit := XMLDoc.CreateElementNS(NSstudy, 'StudyUnit');
  DDIStudyUnit.SetAttribute('id', 'epidata-test');
  DDIInstance.AppendChild(DDIStudyUnit);

  // Build the <StudyUnit>
  BuildCitations;
  BuildAbstract;
  BuildUniverseRef;
  BuildFunding;
  BuildPurpose;
  BuildCoverage;
  BuildConceptualComponent;
  BuildDataCollection;
  BuildLogicalProduct;
  BuildPhysicalDataProduct;

  WriteXML(XMLDoc, Settings.ExportFileName)
end;

end.

