unit epiexport_ddi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidocument, epidatafiles, epidatafilestypes, epivaluelabels,
  epieximtypes, epiexportsettings, DOM, epicustombase;

type

  { TEpiDDIExport }

  TEpiDDIExport = class
  private
    FSettings: TEpiDDIExportSetting;
    EpiDoc:     TEpiDocument;
    XMLDoc:     TXMLDocument;
    DDIInstance: TDOMElement;
    DDIStudyUnit:    TDOMElement;
    DDIUniverseRef:  TDOMElement;
    DDIFunding:      TDOMElement;
    DDISpatialCoverage: TDOMElement;

    // Helper methods.
    procedure AddAttrNameSpace(Elem: TDOMElement; Const NameSpace: string);
    procedure AddAttrID(Elem: TDOMElement; Const Prefix: string);
    procedure AddAttrTranslation(Elem: TDOMElement);
    procedure AddAttrLang(Elem: TDOMElement; Const Lang: string);

    procedure AppendContent(Root: TDOMElement; Const Text: string; Const ContentName: string = 'Content');
    function  AppendElem(Root: TDOMElement; Const NameSpace, NodeName: string;
      Const Text: String = ''): TDOMElement;

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
  NSdatacollection = 'ddi:datacollection:3_1';
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
  S := LowerCase(GUIDToString(GUID));
  S := Prefix + '-' + Copy(S, 2, Length(S) - 2);
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

procedure TEpiDDIExport.AppendContent(Root: TDOMElement; const Text: string;
  const ContentName: string);
var
  Content: TDOMElement;
begin
  Content := XMLDoc.CreateElementNS(NSreuseable, ContentName);
  Root.AppendChild(Content);
  AddAttrTranslation(Content);
//  AddAttrLang(Content, Text.DefaultLang);
  Content.TextContent := Text;
end;

function TEpiDDIExport.AppendElem(Root: TDOMElement; const NameSpace,
  NodeName: string; const Text: String): TDOMElement;
begin
  Result := XMLDoc.CreateElementNS(NameSpace, NodeName);
  Result.TextContent := Text;
  Root.AppendChild(Result);
end;

procedure TEpiDDIExport.BuildCitations;
var
  Citation: TDOMElement;
begin
  Citation := XMLDoc.CreateElementNS(NSreuseable, 'Citation');
  DDIStudyUnit.AppendChild(Citation);

  // Title MUST be present, so assume it is...
  AppendElem(Citation, NSreuseable, 'Title', FSettings.CitTitle);

  if FSettings.CitSubTitle <> '' then
    AppendElem(Citation, NSreuseable, 'SubTitle', FSettings.CitSubTitle);

  if FSettings.CitCreator <> '' then
    AppendElem(Citation, NSreuseable, 'Creator', FSettings.CitCreator);

  if FSettings.CitPublisher <> '' then
    AppendElem(Citation, NSreuseable, 'Publisher', FSettings.CitPublisher);

  if FSettings.CitCopyRight <> '' then
    AppendElem(Citation, NSreuseable, 'Copyright', FSettings.CitCopyRight);
end;

procedure TEpiDDIExport.BuildAbstract;
var
  Abstract: TDOMElement;
  Content: TDOMElement;
begin
  Abstract := XMLDoc.CreateElementNS(NSstudy, 'Abstract');
  DDIStudyUnit.AppendChild(Abstract);
  AddAttrID(Abstract, 'abst');

  AppendContent(Abstract, FSettings.AbstractText);
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
  if (FSettings.FundAgencyName = '') then exit;
  DDIFunding := XMLDoc.CreateElementNS(NSreuseable, 'FundingInformation');

  // Requires AgencyOrganizationReference, built later
end;

procedure TEpiDDIExport.BuildPurpose;
var
  Purpose: TDOMElement;
begin
  Purpose := XMLDoc.CreateElementNS(NSstudy, 'Purpose');
  AddAttrID(Purpose, 'purp');
  DDIStudyUnit.AppendChild(Purpose);

  AppendContent(Purpose, FSettings.Purpose);
end;

procedure TEpiDDIExport.BuildCoverage;
var
  Coverage: TDOMElement;
  CoverElem: TDOMElement;
  i: Integer;
  Elem: TDOMElement;
begin
  with FSettings do
    if (CoverSpatial = '') and
       (CoverTopKeyWords.Count = 0) and
       (CoverTopSubjects.Count = 0) and
       (CoverTmpStartDate = 0) then exit;

  Coverage := XMLDoc.CreateElementNS(NSreuseable, 'Coverage');
  DDIStudyUnit.AppendChild(Coverage);

  if (FSettings.CoverTopSubjects.Count > 0) or
     (FSettings.CoverTopKeyWords.Count > 0) then
  begin
    CoverElem := XMLDoc.CreateElementNS(NSreuseable, 'TopicalCoverage');
    Coverage.AppendChild(CoverElem);
    AddAttrID(CoverElem, 'topcov');

    for i := 0 to FSettings.CoverTopSubjects.Count - 1 do
      AppendElem(CoverElem, NSreuseable, 'Subject', FSettings.CoverTopSubjects[i]);

    for i := 0 to FSettings.CoverTopKeyWords.Count - 1 do
      AppendElem(CoverElem, NSreuseable, 'Keyword', FSettings.CoverTopKeyWords[i]);
  end;

  if FSettings.CoverSpatial <> '' then
  begin
    CoverElem := XMLDoc.CreateElementNS(NSreuseable, 'SpatialCoverage');
    DDISpatialCoverage := CoverElem;
    Coverage.AppendChild(CoverElem);
    AddAttrID(CoverElem, 'spacov');

    Elem := XMLDoc.CreateElementNS(NSreuseable, 'TopLevelReference');
    CoverElem.AppendChild(Elem);

    Elem := XMLDoc.CreateElementNS(NSreuseable, 'LowestLevelReference');
    CoverElem.AppendChild(Elem);
  end;

  if (FSettings.CoverTmpStartDate > 0) then
  begin
    CoverElem := XMLDoc.CreateElementNS(NSreuseable, 'TemporalCoverage');
    Coverage.AppendChild(CoverElem);
    AddAttrID(CoverElem, 'tmpcov');

    Elem := XMLDoc.CreateElementNS(NSreuseable, 'ReferenceDate');
    CoverElem.AppendChild(Elem);

    CoverElem := XMLDoc.CreateElementNS(NSreuseable, 'StartDate');
    CoverElem.TextContent := FormatDateTime('YYYY/MM/DD"T"HH:NN:SS', FSettings.CoverTmpStartDate);
    Elem.AppendChild(CoverElem);

    if FSettings.CoverTmpEndDate > 0 then
    begin
      CoverElem := XMLDoc.CreateElementNS(NSreuseable, 'EndDate');
      CoverElem.TextContent := FormatDateTime('YYYY/MM/DD"T"HH:NN:SS', FSettings.CoverTmpEndDate);
      Elem.AppendChild(CoverElem);
    end;
  end;
end;

procedure TEpiDDIExport.BuildConceptualComponent;
var
  ConceptualComponent: TDOMElement;
  UniS: TDOMElement;
  Uni: TDOMElement;
  GeoS: TDOMElement;
  GeoSch: TDOMElement;
  Geo: TDOMElement;
  OuterLevel: TDOMElement;
  Level: TDOMElement;
begin
  ConceptualComponent := XMLDoc.CreateElementNS(NSconcept, 'ConceptualComponent');
  AddAttrID(ConceptualComponent, 'coco');
  DDIStudyUnit.AppendChild(ConceptualComponent);

  // ***
  // Build Universe!
  UniS := XMLDoc.CreateElementNS(NSconcept, 'UniverseScheme');
  AddAttrID(UniS, 'unis');
  ConceptualComponent.AppendChild(UniS);

  Uni := XMLDoc.CreateElementNS(NSconcept, 'Universe');
  AddAttrID(Uni, 'univ');
  UniS.AppendChild(Uni);

  // A Universe MUST Exists.
  AppendElem(Uni, NSconcept, 'HumanReadable', FSettings.ConUniverse);

  // Create universe reference.
  AppendElem(DDIUniverseRef, NSreuseable, 'ID', Uni.GetAttribute('id'));
  // DONE
  // ****


  // ****
  // Build Geography
  if FSettings.CoverSpatial = '' then exit;
  GeoSch := XMLDoc.CreateElementNS(NSconcept, 'GeographicStructureScheme');
  AddAttrID(GeoSch, 'geostrs');
  ConceptualComponent.AppendChild(GeoSch);

  GeoS := XMLDoc.CreateElementNS(NSreuseable, 'GeographicStructure');
  AddAttrID(GeoS, 'geostr');
  GeoSch.AppendChild(GeoS);

  Geo := XMLDoc.CreateElementNS(NSreuseable, 'Geography');
  AddAttrID(Geo, 'geo');
  GeoS.AppendChild(Geo);

  Level := XMLDoc.CreateElementNS(NSreuseable, 'Level');
  Geo.AppendChild(Level);

  AppendContent(Level, FSettings.CoverSpatial, 'Name');

  // Build TopLevelReference & LowestLevelReference
  OuterLevel := TDOMElement(DDISpatialCoverage.FindNode('TopLevelReference'));
  Level := XMLDoc.CreateElementNS(NSreuseable, 'LevelReference');
  OuterLevel.AppendChild(Level);
  AppendElem(Level, NSreuseable, 'ID', Geo.GetAttribute('id'));
  AppendElem(OuterLevel, NSreuseable, 'LevelName', FSettings.CoverSpatial);

  OuterLevel := TDOMElement(DDISpatialCoverage.FindNode('LowestLevelReference'));
  Level := XMLDoc.CreateElementNS(NSreuseable, 'LevelReference');
  OuterLevel.AppendChild(Level);
  AppendElem(Level, NSreuseable, 'ID', Geo.GetAttribute('id'));
  AppendElem(OuterLevel, NSreuseable, 'LevelName', FSettings.CoverSpatial);
  // DONE
  // ****
end;

procedure TEpiDDIExport.BuildDataCollection;
var
  DataCollection: TDOMElement;
  QScheme: TDOMElement;
  QItem: TDOMElement;
  QText: TDOMElement;
  QLiteralText: TDOMElement;
  i: Integer;
  TextElem: TDOMElement;
  Domain: TDOMElement;
  S: String;
  j: Integer;
begin
  DataCollection := XMLDoc.CreateElementNS(NSdatacollection, 'DataCollection');
  AddAttrID(DataCollection, 'daco');
  DDIStudyUnit.AppendChild(DataCollection);

  QScheme := XMLDoc.CreateElementNS(NSdatacollection, 'QuestionScheme');
  AddAttrID(QScheme, 'ques');
  DataCollection.AppendChild(QScheme);

  for i := 0 to FSettings.Doc.DataFiles[0].Fields.Count - 1 do
  with FSettings.Doc.DataFiles[0].Field[i] do
  begin
    QItem := XMLDoc.CreateElementNS(NSdatacollection, 'QuestionItem');
    AddAttrID(QItem, 'quei');
    QScheme.AppendChild(QItem);


    QText := AppendElem(QItem, NSdatacollection, 'QuestionText', '');
    QLiteralText := AppendElem(QText, NSdatacollection, 'LiteralText', '');
    TextElem := AppendElem(QLiteralText, NSdatacollection, 'Text', Question.Text);

    case FieldType of
      ftBoolean:
        begin
          Domain := AppendElem(QItem, NSdatacollection, 'NumericDomain');
          Domain.SetAttribute('type', 'Short');
        end;
      ftInteger:
        begin
          Domain := AppendElem(QItem, NSdatacollection, 'NumericDomain');
          Domain.SetAttribute('type', 'Long');
        end;
      ftAutoInc:
        begin
          Domain := AppendElem(QItem, NSdatacollection, 'NumericDomain');
          Domain.SetAttribute('type', 'Incremental');
          Domain.SetAttribute('startValue', IntToStr(FSettings.Doc.ProjectSettings.AutoIncStartValue));
          Domain.SetAttribute('interval', '1');
        end;
      ftFloat:
        begin
          Domain := AppendElem(QItem, NSdatacollection, 'NumericDomain');
          Domain.SetAttribute('type', 'Float');
          Domain.SetAttribute('decimalPositions', IntToStr(Decimals));
        end;
      ftDMYDate, ftMDYDate, ftYMDDate,
      ftDMYAuto, ftMDYAuto, ftYMDAuto:
        begin
          Domain := AppendElem(QItem, NSdatacollection, 'DateTimeDomain');
          Domain.SetAttribute('type', 'Date')
        end;
      ftTime,
      ftTimeAuto:
        begin
          Domain := AppendElem(QItem, NSdatacollection, 'DateTimeDomain');
          Domain.SetAttribute('type', 'Time')
        end;
      ftString,
      ftUpperString:
        begin
          Domain := AppendElem(QItem, NSdatacollection, 'TextDomain');
        end;
    end;
    if Assigned(ValueLabelSet) and (ValueLabelSet.MissingCount > 0) then
    begin
      S := '';
      for j := 0 to ValueLabelSet.Count -1 do
        if ValueLabelSet[j].IsMissingValue then
          S += ValueLabelSet[j].ValueAsString + ' ';
      Domain.SetAttribute('missingValue', TrimRight(S));
    end;
    Domain.SetAttribute('blankIsMissingValue', 'true');
  end;
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
  FSettings := Settings;

  EpiDoc := Settings.Doc;

  XMLDoc := TXMLDocument.Create;
  DDIInstance := XMLDoc.CreateElementNS('ddi:instance:3_1', 'DDIInstance');
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

  AddAttrID(DDIInstance, 'inst');
  with DDIInstance do
  begin
    SetAttribute('xmlns:xsi', 'http://www.w3.org/2001/XMLSchema-instance');
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

