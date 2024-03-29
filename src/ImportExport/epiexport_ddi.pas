unit epiexport_ddi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidocument, epidatafiles, epidatafilestypes, epivaluelabels,
  epieximtypes, epiexportsettings, Laz2_DOM, epicustombase, fgl;

type
  { TEpiDDIExport }

  TEpiDDIExport = class
  private
    FSettings: TEpiDDIExportSetting;
    FCSVSettings: TEpiCSVExportSetting;

    FDFSettings: TEpiExportDatafileSettings;
    FDFCSVSettings: TEpiExportDatafileSettings;

    EpiDoc:     TEpiDocument;
    Datafile:   TEpiDataFile;

    XMLDoc:     TXMLDocument;
    DDIInstance: TDOMElement;
    DDIStudyUnit:    TDOMElement;
    DDIUniverseRef:  TDOMElement;
//    DDIFunding:      TDOMElement;
    DDISpatialCoverage: TDOMElement;
    DDILogicalProd:     TDOMElement;
    DDILogicalRec:      TDOMElement;
    DDIVarScheme:       TDOMElement;
    DDIRels:            TDOMElement;
    DDIRely:            TDOMElement;

    ValueLabelSetsGUIDs: TStringList;

    // Maps TEpiField -> QuestionItem
    QuieMap: TFPSMap;
    // Maps TEpiField -> QuestionConstruct
    QuecMap: TFPSMap;
    // Maps TEPiField -> VariableItem
    VarsMap: TFPSMap;
    // Maps TEpiSection -> Concept
    ConcMap: TFPSMap;

    // Helper methods.
    function  idFromMap(Const Map: TFPSMap; Const Item: TEpiCustomItem): string;
    procedure AddAttrNameSpace(Elem: TDOMElement; Const NameSpace: string);
    procedure AddAttrID(Elem: TDOMElement; Const Prefix: string);
    procedure AddAttrTranslation(Elem: TDOMElement);
    procedure AddAttrLang(Elem: TDOMElement; Const Lang: string);

    function  AppendElem(Root: TDOMElement; Const NameSpace, NodeName: string;
      Const Text: String = ''): TDOMElement;

    // Adds Lang
    function  AppendElemInternationalStringType(Root: TDOMElement;
      Const NameSpace, NodeName: string;
      Const Text: String): TDOMElement;

    // Adds ID
    function  AppendElemIdentifiableType(Root: TDOMElement;
      Const NameSpace, NodeName: string;
      Const Prefix: String = ''): TDOMElement;

    // Adds ID and Version
    function  AppendElemVersionableType(Root: TDOMElement;
      Const NameSpace, NodeName: string;
      Const Prefix: String = ''): TDOMElement;

    // Adds ID, Version, and Agency!
    function AppendElemMaintainableType(Root: TDOMElement;
      Const NameSpace, NodeName: string;
      Const Prefix: String = ''): TDOMElement;

    function AppendElemReferenceType(Root: TDOMElement;
      Const NameSpace, NodeName: string;
      Const ReferenceElem: TDOMElement): TDOMElement; overload;

    function AppendElemReferenceType(Root: TDOMElement;
      Const NameSpace, NodeName: string;
      Const ReferenceId: string): TDOMElement; overload;

    procedure ExportCSVFile;

    // Block to produce.
    procedure BuildCitations;
    procedure BuildAbstract;
    procedure BuildUniverseRef;
    procedure BuildFunding;
    procedure BuildPurpose;
    procedure BuildCoverage;
    procedure BuildUnitOfObs;
    procedure BuildKindOfData;
    procedure BuildDCElements(Const CitationElem: TDOMElement);
    procedure BuildConceptualComponent;

    procedure BuildDataCollection;
    // Datacollection helpers:
    procedure BuildQuestionScheme(DataCollection: TDOMElement);
    // helper: Returns MainSequence
    function  BuildControlConstructScheme(DataCollection: TDOMElement): TDomElement;
    // Helper: build interviewer instructions and references from QuestionConstruc to Instruction.
    procedure BuildInterviewerInstructionScheme(DataCollection: TDOMElement);

    procedure BuildLogicalProduct;
    // LogicalProductHelpers
    function  BuildVariableScheme: TDOMElement;
    procedure BuildCodeScheme(LogicalProduct: TDOMElement);

    procedure BuildPhysicalDataProduct;
    procedure BuildPhysicalInstance;
    procedure BuildArchive;
  public
    constructor Create;
    destructor  Destroy; override;
    function    ExportDDI(Const Settings: TEpiDDIExportSetting): boolean;
  end;

implementation

uses
  laz2_XMLWrite, epiexport, LazUTF8, epitools_filterinfo, epifields_helper;

const
  NSreuseable           = 'ddi:reusable:3_1';
  NSstudy               = 'ddi:studyunit:3_1';
  NSconcept             = 'ddi:conceptualcomponent:3_1';
  NSdatacollection      = 'ddi:datacollection:3_1';
  NSlogicalproduct      = 'ddi:logicalproduct:3_1';
  NSphysicaldataproduct = 'ddi:physicaldataproduct:3_1';
  NSphysicalinstance    = 'ddi:physicalinstance:3_1';
  NSarchive             = 'ddi:archive:3_1';
  NSdcelements          = 'ddi:dcelements:3_1';

{ TEpiDDIExport }

function CreateAttrID(Const Prefix: string): string;
var
  GUID: TGUID;
begin
  CreateGUID(GUID);
  Result := LowerCase(GUIDToString(GUID));
  Result := Copy(Result, 2, Length(Result) - 2);
  if Prefix <> '' then
    Result := Prefix + '-' + Result;
end;

function TEpiDDIExport.idFromMap(const Map: TFPSMap; const Item: TEpiCustomItem
  ): string;
begin
  Result := TDOMElement(Map.KeyData[@Item]^).GetAttribute('id')
end;

procedure TEpiDDIExport.AddAttrNameSpace(Elem: TDOMElement;
  const NameSpace: string);
begin
  Elem.SetAttribute('xmlns', 'ddi:' + NameSpace + ':3_1');
end;

procedure TEpiDDIExport.AddAttrID(Elem: TDOMElement; const Prefix: string);
begin
  Elem.SetAttribute('id', CreateAttrID(Prefix));
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

function TEpiDDIExport.AppendElem(Root: TDOMElement; const NameSpace,
  NodeName: string; const Text: String): TDOMElement;
begin
  Result := XMLDoc.CreateElementNS(NameSpace, NodeName);
  if Text <> '' then
    Result.TextContent := Text;
  if Assigned(Root) then
    Root.AppendChild(Result);
end;

function TEpiDDIExport.AppendElemInternationalStringType(Root: TDOMElement;
  const NameSpace, NodeName: string; const Text: String): TDOMElement;
begin
  Result := AppendElem(Root, NameSpace, NodeName, Text);
  AddAttrLang(Result, FSettings.ExportLang);
  Result.SetAttribute('translatable', 'true');
  Result.SetAttribute('translated',   'false');
end;

function TEpiDDIExport.AppendElemIdentifiableType(Root: TDOMElement;
  const NameSpace, NodeName: string; const Prefix: String): TDOMElement;
begin
  Result := AppendElem(Root, NameSpace, NodeName);
  AddAttrID(Result, Prefix);
end;

function TEpiDDIExport.AppendElemVersionableType(Root: TDOMElement;
  const NameSpace, NodeName: string; const Prefix: String): TDOMElement;
begin
  Result := AppendElemIdentifiableType(Root, NameSpace, NodeName, Prefix);
  Result.SetAttribute('version', FSettings.Version);
  Result.SetAttribute('versionDate', FormatDateTime('YYYY"-"MM"-"DD"T"HH":"MM":"SS"."ZZZ', Now));
end;

function TEpiDDIExport.AppendElemMaintainableType(Root: TDOMElement;
  const NameSpace, NodeName: string; const Prefix: String): TDOMElement;
begin
  Result := AppendElemVersionableType(Root, NameSpace, NodeName, Prefix);
  if EpiDoc.Study.Agency <> '' then
    Result.SetAttribute('agency', EpiDoc.Study.Agency)
  else
    Result.SetAttribute('agency', 'Unknown');
end;

function TEpiDDIExport.AppendElemReferenceType(Root: TDOMElement;
  const NameSpace, NodeName: string; const ReferenceElem: TDOMElement
  ): TDOMElement;
begin
  Result := AppendElemReferenceType(Root, NameSpace, NodeName,
              ReferenceElem.GetAttribute('id'));
end;

function TEpiDDIExport.AppendElemReferenceType(Root: TDOMElement;
  const NameSpace, NodeName: string; const ReferenceId: string): TDOMElement;
begin
  Result := AppendElem(Root, NameSpace, NodeName);
  AppendElem(Result, NSreuseable, 'ID', ReferenceId);
  if EpiDoc.Study.Agency <> '' then
    AppendElem(Result, NSreuseable, 'IdentifyingAgency', EpiDoc.Study.Agency)
  else
    AppendElem(Result, NSreuseable, 'IdentifyingAgency', 'Unknown');
  AppendElem(Result, NSreuseable, 'Version', FSettings.Version);
end;

procedure TEpiDDIExport.ExportCSVFile;
var
  CSVExporter: TEpiExport;
  TxtExportSetting: TEpiCSVExportSetting;
  i: Integer;
  NewDoc: TEpiDocument;
  NewDF: TEpiDataFile;
  NewList: TList;
  OldList: TList;
  TxtDfExportSetting: TEpiExportDatafileSettings;
begin
  TxtExportSetting := TEpiCSVExportSetting.Create;
  TxtExportSetting.Assign(FCSVSettings);
  TxtExportSetting.DatafileSettings.ClearAndFree;

  TxtDfExportSetting := TEpiExportDatafileSettings.Create;
  TxtDfExportSetting.Assign(FDFCSVSettings);
  TxtDfExportSetting.FromRecord     := 0;
  TxtDfExportSetting.ToRecord       := Datafile.Size - 1;

  TxtExportSetting.DatafileSettings.Add(TxtDfExportSetting);

{  if (FSettings.RenameVariablesPrefix <> '') and
     (TxtExportSetting.ExportFieldNames)
  then
  begin
    // Clone Document and rename variables;
    NewDoc := TEpiDocument(EpiDoc.Clone);
    NewDF  := NewDoc.DataFiles[TxtExportSetting.DataFileIndex];

    NewList := TList.Create;
    for i := 0 to TxtExportSetting.Fields.Count - 1 do
       NewList.Add(NewDf.ControlItems.GetItemByName(TEpiCustomItem(TxtExportSetting.Fields[i]).Name));

    // In case of existing field names with same as selected prefix.
    for i := 0 to NewDF.Fields.Count - 1 do
      NewDF.Field[i].Name := '@renamed' + IntToStr(i);

    for i := 0 to NewDF.Fields.Count - 1 do
      NewDF.Field[i].Name := FSettings.RenameVariablesPrefix + IntToStr(i+1);

    TxtExportSetting.Doc := NewDoc;
    OldList := TxtExportSetting.Fields;
    TxtExportSetting.Fields := NewList;
  end;           }

  CSVExporter := TEpiExport.Create;
  CSVExporter.Export(TxtExportSetting);

{  if (FSettings.RenameVariablesPrefix <> '') and
     (TxtExportSetting.ExportFieldNames)
  then
  begin
    TxtExportSetting.Doc := EpiDoc;
    TxtExportSetting.Fields := OldList;
    NewList.Free;
    NewDoc.Free;
  end;   }

  CSVExporter.Free;
  TxtExportSetting.Free;
end;

procedure TEpiDDIExport.BuildCitations;
var
  Citation: TDOMElement;
  Elem: TDOMElement;
begin
  Citation := AppendElem(DDIStudyUnit, NSreuseable, 'Citation');

  // Title MUST be present, so assume it is...
  AppendElemInternationalStringType(Citation, NSreuseable, 'Title', EpiDoc.Study.Title.Text);

  if EpiDoc.Study.Author <> '' then
    AppendElemInternationalStringType(Citation, NSreuseable, 'Creator', EpiDoc.Study.Author);

  if EpiDoc.Study.Publisher.Text <> '' then
    AppendElemInternationalStringType(Citation, NSreuseable, 'Publisher', EpiDoc.Study.Publisher.Text);

  AppendElem(Citation, NSreuseable, 'Language', EpiDoc.DefaultLang);

  if EpiDoc.Study.Identifier <> '' then
  begin
    Elem := AppendElemInternationalStringType(Citation, NSreuseable, 'InternationalIdentifier', EpiDoc.Study.Identifier);
    Elem.SetAttribute('type', 'Other');
  end;

  BuildDCElements(Citation);
end;

procedure TEpiDDIExport.BuildAbstract;
var
  Abstract: TDOMElement;
begin
  Abstract := AppendElemIdentifiableType(DDIStudyUnit, NSstudy, 'Abstract', 'abst');
  AppendElemInternationalStringType(Abstract, NSreuseable, 'Content', EpiDoc.Study.AbstractText.Text);
end;

procedure TEpiDDIExport.BuildUniverseRef;
begin
  // Universe Ref cannot be built fully at this instance, but append it as child to StudyUnit
  // to get the correct order of things.
  DDIUniverseRef := AppendElemReferenceType(DDIStudyUnit, NSreuseable, 'UniverseReference', '');
//  DDIUniverseRef := AppendElem(DDIStudyUnit, NSreuseable, 'UniverseReference');

  // UniverseReference element still need an "ID" element, but that is assigned later
  // when the actual UniverseScheme is created.
end;

procedure TEpiDDIExport.BuildFunding;
begin
  if (EpiDoc.Study.Funding.Text = '') then exit;
//  DDIFunding := AppendElem(DDIStudyUnit, NSreuseable, 'FundingInformation');

  // Requires AgencyOrganizationReference, built later
end;

procedure TEpiDDIExport.BuildPurpose;
var
  Purpose: TDOMElement;
begin
  Purpose := AppendElemIdentifiableType(DDIStudyUnit, NSstudy, 'Purpose', 'purp');

  AppendElemInternationalStringType(Purpose, NSreuseable, 'Content', EpiDoc.Study.Purpose.Text);
end;

procedure TEpiDDIExport.BuildCoverage;
var
  Coverage: TDOMElement;
  CoverElem: TDOMElement;
begin
  with EpiDoc.Study do
  begin
    if (GeographicalCoverage.Text = '') and
       (Keywords = '') and
       (true) then exit;

    Coverage := AppendElem(DDIStudyUnit, NSreuseable, 'Coverage');

    if (Keywords <> '') then
    begin
      CoverElem := AppendElemIdentifiableType(Coverage, NSreuseable, 'TopicalCoverage', 'topcov');
      AppendElemInternationalStringType(CoverElem, NSreuseable, 'Keyword', Keywords);
    end;

    if GeographicalCoverage.Text <> '' then
    begin
      DDISpatialCoverage := AppendElemIdentifiableType(Coverage, NSreuseable, 'SpatialCoverage', 'spacov');
      AppendElem(DDISpatialCoverage, NSreuseable, 'TopLevelReference');
      AppendElem(DDISpatialCoverage, NSreuseable, 'LowestLevelReference');
    end;
  end;
end;

procedure TEpiDDIExport.BuildUnitOfObs;
begin
  if EpiDoc.Study.UnitOfObservation.Text <> '' then
    AppendElemInternationalStringType(DDIStudyUnit, NSstudy, 'AnalysisUnitsCovered', EpiDoc.Study.UnitOfObservation.Text);
end;

procedure TEpiDDIExport.BuildKindOfData;
begin
  if EpiDoc.Study.Design.Text <> '' then
    AppendElem(DDIStudyUnit, NSstudy, 'KindOfData', EpiDoc.Study.Design.Text);
end;

procedure TEpiDDIExport.BuildDCElements(const CitationElem: TDOMElement);
var
  DCElem: TDOMElement;

  procedure BuildDCTag(Const DCTag, Content: string);
  var
    Elem: TDOMElement;
  begin
    Elem := AppendElem(DCElem, 'http://purl.org/dc/elements/1.1/', DCTag, Content);
    AddAttrLang(Elem, FSettings.ExportLang);
  end;

begin
  // Here we build all additional Dublin Core elements:
  // - Citation: Fungerer som ”udgivelser fra studiet”
  // - Rights:   right to use data
  // - Funding:  contributors
  DCElem := AppendElem(CitationElem, NSdcelements, 'DCElements');

  BuildDCTag('dc:identifier', EpiDoc.Study.Version);
  BuildDCTag('dc:source',     EpiDoc.Study.Citations.Text);
  BuildDCTag('dc:relation',   EpiDoc.Study.Funding.Text);
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
  Sections: TEpiSections;
  i: Integer;
  ConS: TDOMElement;
  Con: TDOMElement;
  Sec: TEpiSection;
begin
  ConceptualComponent := AppendElemMaintainableType(DDIStudyUnit, NSconcept, 'ConceptualComponent', 'coco');

  // ****
  // Build Concepts
  ConS := AppendElemMaintainableType(ConceptualComponent, NSconcept, 'ConceptScheme', 'cons');
  Con  := AppendElemVersionableType(Cons, NSconcept, 'Concept', 'conc');
  AppendElemInternationalStringType(Con, NSreuseable, 'Label', 'Project Concept');
  AppendElemInternationalStringType(Con, NSreuseable, 'Description', 'Main Concept');

  Sec := Datafile.MainSection;
  ConcMap.Add(@Sec, @Con);

  Sections := Datafile.Sections;
  for i := 0 to Sections.Count - 1 do
  begin
    Sec := Sections[i];
    if Sec = Datafile.MainSection then continue;

    Con  := AppendElemVersionableType(Cons, NSconcept, 'Concept', 'conc');
    AppendElemInternationalStringType(Con, NSreuseable, 'Label', 'Section Concept');
    AppendElemInternationalStringType(Con, NSreuseable, 'Description', Sec.Caption.Text);

    ConcMap.Add(@Sec, @Con);
  end;
  // DONE
  // ******


  // ***
  // Build Universe!
  UniS := AppendElemMaintainableType(ConceptualComponent, NSconcept, 'UniverseScheme', 'unis');
  Uni  := AppendElemVersionableType(UniS, NSconcept, 'Universe', 'univ');

  // A Universe MUST Exists.
  AppendElemInternationalStringType(Uni, NSconcept, 'HumanReadable', EpiDoc.Study.Population.Text);

  // Update universe reference.
  DDIUniverseRef.FindNode('ID').TextContent := Uni.GetAttribute('id');
  // DONE
  // ****


  // ****
  // Build Geography
  if EpiDoc.Study.GeographicalCoverage.Text = '' then exit;
  GeoSch := AppendElemMaintainableType(ConceptualComponent, NSconcept,   'GeographicStructureScheme', 'geostrs');
  GeoS   := AppendElemVersionableType( GeoSch,              NSreuseable, 'GeographicStructure',       'geostr');
  Geo    := AppendElemIdentifiableType(GeoS,                NSreuseable, 'Geography',                 'geo');

  Level  := AppendElem(Geo, NSreuseable, 'Level');
  AppendElem(Level, NSreuseable, 'Name', EpiDoc.Study.GeographicalCoverage.Text);

  // Build TopLevelReference & LowestLevelReference
  OuterLevel := TDOMElement(DDISpatialCoverage.FindNode('TopLevelReference'));
  AppendElemReferenceType(OuterLevel, NSreuseable, 'LevelReference', Geo);
  AppendElem(OuterLevel, NSreuseable, 'LevelName', EpiDoc.Study.GeographicalCoverage.Text);

  OuterLevel := TDOMElement(DDISpatialCoverage.FindNode('LowestLevelReference'));
  AppendElemReferenceType(OuterLevel, NSreuseable, 'LevelReference', Geo);
  AppendElem(OuterLevel, NSreuseable, 'LevelName', EpiDoc.Study.GeographicalCoverage.Text);
  // DONE
  // ****
end;

procedure TEpiDDIExport.BuildDataCollection;
var
  DataCollection: TDOMElement;
  Seq: TDOMElement;
  Elem: TDOMElement;
begin
  DataCollection := AppendElemMaintainableType(DDIStudyUnit, NSdatacollection, 'DataCollection', 'daco');

  if (EpiDoc.Study.DataCollectionStart <> MaxDateTime) then
  begin
    Elem := AppendElemIdentifiableType(DataCollection, NSdatacollection, 'CollectionEvent', 'coev');
    Elem := AppendElem(Elem, NSdatacollection, 'DataCollectionDate');
    AppendElem(Elem, NSreuseable, 'StartDate', FormatDateTime('YYYY/MM/DD', EpiDoc.Study.DataCollectionStart));
    AppendElem(Elem, NSreuseable, 'EndDate', FormatDateTime('YYYY/MM/DD', EpiDoc.Study.DataCollectionEnd));
  end;


  BuildQuestionScheme(DataCollection);
  Seq := BuildControlConstructScheme(DataCollection);
  BuildInterviewerInstructionScheme(DataCollection);


  Elem := AppendElemMaintainableType(DataCollection, NSdatacollection, 'Instrument', 'inst');
  AppendElemInternationalStringType(Elem, NSreuseable, 'Label', 'Instrument');
  AppendElemReferenceType(Elem, NSdatacollection, 'ControlConstructReference', Seq);
end;

procedure TEpiDDIExport.BuildQuestionScheme(DataCollection: TDOMElement);
var
  QScheme: TDOMElement;
  QItem: TDOMElement;
  QText: TDOMElement;
  QLiteralText: TDOMElement;
  i: Integer;
  Domain: TDOMElement;
  S: String;
  j: Integer;
  F: TEpiField;
begin
  QScheme := AppendElemMaintainableType(DataCollection, NSdatacollection, 'QuestionScheme', 'ques');
  AppendElemInternationalStringType(QScheme, NSreuseable, 'Label', 'QUES-' + Datafile.Name);
  AppendElemInternationalStringType(QScheme, NSreuseable, 'Description', Datafile.Notes.Text);

  // Build the list of GUID's for all our valuelabelsets.
  for i := 0 to EpiDoc.ValueLabelSets.Count - 1 do
    ValueLabelSetsGUIDs.AddObject(CreateAttrID('cods'), EpiDoc.ValueLabelSets[i]);

  // Build QuestionItem
  for F in Datafile.Fields do
  begin
    QItem := AppendElemVersionableType(QScheme, NSdatacollection, 'QuestionItem', 'quei');
    QuieMap.Add(@F, @QItem);

    AppendElemInternationalStringType(QItem, NSdatacollection, 'QuestionItemName', F.Name);
    QText := AppendElemInternationalStringType(QItem, NSdatacollection, 'QuestionText', '');
    QLiteralText := AppendElem(QText, NSdatacollection, 'LiteralText');

    if FSettings.SectionCaptionIsQText and
       (F.Section <> F.DataFile.MainSection)
    then
      AppendElem(QLiteralText, NSdatacollection, 'Text', F.Section.Caption.Text)
    else
      AppendElem(QLiteralText, NSdatacollection, 'Text', F.Question.Text);

    if Assigned(F.ValueLabelSet) then
    begin
      Domain := AppendElem(QItem, NSdatacollection, 'CodeDomain');
      j := ValueLabelSetsGUIDs.IndexOfObject(F.ValueLabelSet);
      AppendElemReferenceType(Domain, NSreuseable, 'CodeSchemeReference', ValueLabelSetsGUIDs[j]);
    end else
      case F.FieldType of
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
            Domain.SetAttribute('startValue', IntToStr(EpiDoc.ProjectSettings.AutoIncStartValue));
            Domain.SetAttribute('interval', '1');
          end;
        ftFloat:
          begin
            Domain := AppendElem(QItem, NSdatacollection, 'NumericDomain');
            Domain.SetAttribute('type', 'Float');
            Domain.SetAttribute('decimalPositions', IntToStr(F.Decimals));
          end;
        ftDMYDate, ftMDYDate, ftYMDDate,
        ftDMYAuto, ftMDYAuto, ftYMDAuto:
          begin
            Domain := AppendElem(QItem, NSdatacollection, 'DateTimeDomain');
            Domain.SetAttribute('type', 'Date');
            Domain.SetAttribute('format', F.FormatString());
            Domain.SetAttribute('blankIsMissingValue', 'true');
          end;
        ftTime,
        ftTimeAuto:
          begin
            Domain := AppendElem(QItem, NSdatacollection, 'DateTimeDomain');
            Domain.SetAttribute('type', 'Time');
            Domain.SetAttribute('format', F.FormatString());
            Domain.SetAttribute('blankIsMissingValue', 'true');
          end;
        ftString,
        ftUpperString:
          begin
            Domain := AppendElem(QItem, NSdatacollection, 'TextDomain');
            Domain.SetAttribute('maxLength', IntToStr(F.Length));
            Domain.SetAttribute('blankIsMissingValue', 'true');
          end;
        ftMemo:
          begin
            Domain := AppendElem(QItem, NSdatacollection, 'TextDomain');
            Domain.SetAttribute('maxLength', IntToStr(F.MaxByteLength));
            Domain.SetAttribute('blankIsMissingValue', 'true');
          end;
      end;

    // Missing Value
    if Assigned(F.ValueLabelSet) and (F.ValueLabelSet.MissingCount > 0) then
    begin
      BackupFormatSettings;
      DefaultFormatSettings.DecimalSeparator := '.';
      S := '';
      for i := 0 to F.ValueLabelSet.Count -1 do
        if F.ValueLabelSet[i].IsMissingValue then
          S += F.ValueLabelSet[i].ValueAsString + ' ';
      Domain.SetAttribute('missingValue', TrimRight(S));
      RestoreFormatSettings;
    end;

    // Only add blankIsMissingValue if the field actually contains data.
    for i := 0 to F.Size -1 do
      if F.IsMissing[i] then
      begin
        Domain.SetAttribute('blankIsMissingValue', 'true');
        Continue;
      end;

    // Add reference to Concept (via Section mapping)
    AppendElemReferenceType(QItem, NSdatacollection, 'ConceptReference', idFromMap(ConcMap, F.Section));
  end;
end;

function TEpiDDIExport.BuildControlConstructScheme(DataCollection: TDOMElement
  ): TDomElement;
var
  CCS: TDOMElement;
  MainSequence: TDOMElement;
  Elem: TDOMElement;
  DoneItem: TDOMElement;
  F: TEpiField;
  QCons: TDOMElement;
  H: TEpiHeading;

  procedure BuildSequence(Sequence: TDOMElement; FromIndex: integer);
  var
    QCons: TDOMElement;
    i: integer;
    NewSequence: TDOMElement;
    Idx: Integer;
    Elem: TDOMElement;
    ITE: TDOMElement;
    Jmp: TEpiJump;
    F: TEpiField;
    CustItem: TEpiCustomItem;
  begin
    while FromIndex < Datafile.ControlItems.Count do
    begin
      CustItem := Datafile.ControlItems[FromIndex];

      Inc(FromIndex);
      if CustItem is TEpiSection then continue;

      QCons := TDOMElement(QuecMap.KeyData[@CustItem]^);
      AppendElemReferenceType(Sequence, NSdatacollection, 'ControlConstructReference', QCons);

      if not (CustItem is TEpiField) then continue;
      F := TEpiField(CustItem);

      if Assigned(F.Jumps) and (F.Jumps.Count > 0) then
      begin
        for i := 0 to F.Jumps.Count - 1 do
        begin
          ITE := AppendElemVersionableType(CCS, NSdatacollection, 'IfThenElse', 'ifth');

          // Add a reference in the original sequence to this IfThenElse node:
          AppendElemReferenceType(Sequence, NSdatacollection, 'ControlConstructReference', ITE);

          Jmp := F.Jumps[i];

          // Build Inner nodes of IfThenElse:
          // - If Condition:
          Elem := AppendElem(ITE, NSdatacollection, 'IfCondition');
          AppendElem(Elem, NSreuseable, 'Code', F.Name + '==' + Jmp.JumpValueAsString).SetAttribute('programmingLanguage', 'dk.dda.inst-0.1');
          // - Source (Field) of If Condition
          AppendElemReferenceType(Elem, NSreuseable, 'SourceQuestionReference', QCons);


          // Handle special case with jtSaveRecord:
          if Jmp.JumpType = jtSaveRecord then
          begin
            AppendElemReferenceType(ITE, NSdatacollection, 'ThenConstructReference', DoneItem);
            Continue;
          end;

          // Construct new branching sequence for this jump value:
          NewSequence := AppendElemVersionableType(CCS, NSdatacollection, 'Sequence', 'seqc');
          AppendElem(NewSequence, NSreuseable, 'Label', 'Sequence: ' + F.Name + ' Jump=' + Jmp.JumpValueAsString);
          Case F.Jumps[i].JumpType of
            jtExitSection:
              begin
                for Idx := FromIndex to Datafile.ControlItems.Count -1 do
                  if ((Datafile.ControlItems[Idx] is TEpiField)   and (TEpiField(Datafile.ControlItems[Idx]).Section <> F.Section)) or
                     ((Datafile.ControlItems[Idx] is TEpiHeading) and (TEpiHeading(Datafile.ControlItems[Idx]).Section <> F.Section))
                     then break
              end;
            jtSkipNextField:
              begin
                Idx := FromIndex + 1;
                // Skip until we hit field.
                while (Idx < Datafile.ControlItems.Count) and (Datafile.ControlItems[Idx] is TEpiHeading) do inc(Idx);
                // Next idx must be after "Skip next Field" (which could be a heading or another thing);
                Inc(Idx);
              end;
            jtToField:
              Idx := Datafile.ControlItems.IndexOf(F.Jumps[i].JumpToField);
          end;
          AppendElemReferenceType(ITE, NSdatacollection, 'ThenConstructReference', NewSequence);

          BuildSequence(NewSequence, Idx);
        end;
        // Construct branching sequence for all other values (last else sequence)
        NewSequence := AppendElemVersionableType(CCS, NSdatacollection, 'Sequence', 'seqc');
        AppendElem(NewSequence, NSreuseable, 'Label', 'Sequence: ' + F.Name + ' Jump="other values"');
        AppendElemReferenceType(ITE, NSdatacollection, 'ElseConstructReference', NewSequence);
        BuildSequence(NewSequence, FromIndex);

        // Break the loop as the last Else will finish the sequence of fields.
        Break;
      end;
    end;
  end;

begin
  CCS := AppendElemMaintainableType(DataCollection, NSdatacollection, 'ControlConstructScheme', 'cocs');

  MainSequence := AppendElemVersionableType(CCS, NSdatacollection, 'Sequence', 'seqc');
  AppendElemInternationalStringType(MainSequence, NSreuseable, 'Label', 'Main Sequence');

  for F in Datafile.Fields do
  begin
    QCons := AppendElemVersionableType(CCS, NSdatacollection, 'QuestionConstruct', 'quec');
    AppendElemInternationalStringType(QCons, NSreuseable, 'Label', 'QUEC-' + F.Name);
    AppendElemReferenceType(QCons, NSdatacollection, 'QuestionReference', idFromMap(QuieMap, F));
    QuecMap.Add(@F, @QCons);
  end;

  for H in Datafile.Headings do
  begin
    QCons := AppendElemVersionableType(CCS, NSdatacollection, 'StatementItem', 'stai');
    AppendElemInternationalStringType(QCons, NSreuseable, 'Label', 'STAI-' + H.Name);
    Elem := AppendElemInternationalStringType(QCons, NSdatacollection, 'DisplayText', '');
    Elem := AppendElem(Elem, NSdatacollection, 'LiteralText');
    AppendElem(Elem, NSdatacollection, 'Text', H.Caption.Text);

    QuecMap.Add(@H, @QCons);
  end;

  DoneItem := AppendElemVersionableType(CCS, NSdatacollection, 'Sequence', 'seqc');
  AppendElemInternationalStringType(DoneItem, NSreuseable, 'Label', 'Empty Sequence');
  AppendElemInternationalStringType(DoneItem, NSreuseable, 'Description',
    'The empty sequence is designed to be the destination for ending a questionaire, based on a Jump with exit value = jtSaveRecord'
  );

  BuildSequence(MainSequence, 0);
  Result := MainSequence;
end;

procedure TEpiDDIExport.BuildInterviewerInstructionScheme(
  DataCollection: TDOMElement);
var
  F: TEpiField;
  InstSc: TDOMElement;
  Inst: TDOMElement;
  QCons: TDOMElement;
  Elem: TDOMElement;
begin
  InstSc := AppendElemMaintainableType(nil, NSdatacollection, 'InterviewerInstructionScheme', 'invs');

  for F in Datafile.Fields do
  begin
    if F.Notes.Text = '' then continue;

    Inst := AppendElemVersionableType(InstSc, NSdatacollection, 'Instruction',     'intv');
    AppendElemInternationalStringType(Inst,   NSdatacollection, 'InstructionName', 'INVS-' + F.Name);
    AppendElemInternationalStringType(Inst,   NSreuseable,      'Description',     'Instruction for Question:' + LineEnding + F.Question.Text);
    Elem := AppendElemInternationalStringType(Inst, NSdatacollection, 'InstructionText', '');
    Elem := AppendElem(Elem, NSdatacollection, 'LiteralText');
    AppendElemInternationalStringType(Elem, NSdatacollection, 'Text', F.Notes.Text);

    QCons := TDOMElement(QuecMap.KeyData[@F]^);
    Elem  := AppendElemReferenceType(QCons, NSdatacollection, 'InterviewerInstructionReference', Inst);
    QCons.InsertBefore(Elem, QCons.FindNode('QuestionReference'));
  end;

  if InstSc.HasChildNodes then
    DataCollection.AppendChild(InstSc)
  else
    InstSc.Free;
end;

procedure TEpiDDIExport.BuildLogicalProduct;
var
  Elem: TDOMElement;
begin
  DDILogicalProd := AppendElemMaintainableType(DDIStudyUnit, NSlogicalproduct, 'LogicalProduct', 'lopr');

  DDIVarScheme := BuildVariableScheme;

  Elem := AppendElemVersionableType(DDILogicalProd, NSlogicalproduct, 'DataRelationship', 'dars');
  DDILogicalRec := AppendElemIdentifiableType(Elem, NSlogicalproduct, 'LogicalRecord',    'lore');
  DDILogicalRec.SetAttribute('hasLocator', 'false');

  Elem := AppendElem(DDILogicalRec, NSlogicalproduct, 'VariablesInRecord');
  Elem.SetAttribute('allVariablesInLogicalProduct', 'true');
  AppendElemReferenceType(Elem, NSlogicalproduct, 'VariableSchemeReference', DDIVarScheme);

  BuildCodeScheme(DDILogicalProd);

  DDILogicalProd.AppendChild(DDIVarScheme);
end;

function TEpiDDIExport.BuildVariableScheme: TDOMElement;
var
  F: TEpiField;
  VarElem: TDOMElement;
  i: Integer;
  Elem: TDOMElement;
  ReprElem: TDOMElement;
  j: Integer;
  S: String;
  L: TList;
begin
  Result := AppendElemMaintainableType(nil, NSlogicalproduct, 'VariableScheme', 'vars');

  EpiTool_UpdateFilterInformation(Datafile);

  I := 0;
  for F in Datafile.Fields do
  begin
    L := TList(F.FindCustomData(EPITOOL_FILTER_CUSTDATA));
    S := '';

    // Create Filter Element.
    if (Assigned(L))
    then
      begin
        S := 'Filter: ' + TEpiToolFilterInfo(L[0]).Field.Name;
        for j := 1 to L.Count - 1 do
          if TEpiToolFilterInfo(L[j]).Field = TEpiToolFilterInfo(L[J-1]).Field then
            continue
          else
            S += ', ' + TEpiToolFilterInfo(L[j]).Field.Name;
      end;

    VarElem := AppendElemVersionableType(Result, NSlogicalproduct, 'Variable', 'vari');
    VarsMap.Add(@F, @VarElem);

    // DDA specific coding... remove when appropriate! :)
    if (FSettings.FilterTagIsUserId) and (S <> '') then
      begin
        Elem := AppendElem(VarElem, NSreuseable, 'UserID', S);
        Elem.SetAttribute('type', 'filter');
      end;

    if (FSettings.RenameVariablesPrefix <> '') and
       ((FSettings.RenameVariablesPrefix + IntToStr(i+1)) <> F.Name)  // only add UserID if there is a real change!
    then
      begin
        Elem := AppendElem(VarElem, NSreuseable, 'UserID', F.Name);
        Elem.SetAttribute('type', 'dk.dda.variable.orgvariablename');
        AppendElemInternationalStringType(VarElem, NSlogicalproduct, 'VariableName', FSettings.RenameVariablesPrefix + IntToStr(i+1));
      end
    else
      AppendElemInternationalStringType(VarElem, NSlogicalproduct, 'VariableName', F.Name);

    AppendElemInternationalStringType(VarElem, NSreuseable, 'Label', F.Question.Text);

    if (not FSettings.FilterTagIsUserId) and (S <> '') then
      AppendElemInternationalStringType(VarElem, NSreuseable, 'Description', S);

    AppendElemReferenceType(VarElem, NSlogicalproduct, 'ConceptReference',  idFromMap(ConcMap, F.Section));
    AppendElemReferenceType(VarElem, NSlogicalproduct, 'QuestionReference', idFromMap(QuieMap, F));

    Elem := AppendElem(VarElem, NSlogicalproduct, 'Representation');

    if Assigned(F.ValueLabelSet) then
    begin
      ReprElem := AppendElem(Elem, NSlogicalproduct, 'CodeRepresentation');
      j := ValueLabelSetsGUIDs.IndexOfObject(F.ValueLabelSet);
      AppendElemReferenceType(ReprElem, NSreuseable, 'CodeSchemeReference', ValueLabelSetsGUIDs[j]);
    end else
      case F.FieldType of
        ftBoolean:
          begin
            ReprElem := AppendElem(Elem, NSlogicalproduct, 'NumericRepresentation');
            ReprElem.SetAttribute('type', 'Short');
          end;
        ftInteger:
          begin
            ReprElem := AppendElem(Elem, NSlogicalproduct, 'NumericRepresentation');
            ReprElem.SetAttribute('type', 'Long');
          end;
        ftAutoInc:
          begin
            ReprElem := AppendElem(Elem, NSlogicalproduct, 'NumericRepresentation');
            ReprElem.SetAttribute('type', 'Incremental');
            ReprElem.SetAttribute('startValue', IntToStr(EpiDoc.ProjectSettings.AutoIncStartValue));
            ReprElem.SetAttribute('interval', '1');
          end;
        ftFloat:
          begin
            ReprElem := AppendElem(Elem, NSlogicalproduct, 'NumericRepresentation');
            ReprElem.SetAttribute('type', 'Float');
            ReprElem.SetAttribute('decimalPositions', IntToStr(F.Decimals));
          end;
        ftDMYDate, ftMDYDate, ftYMDDate,
        ftDMYAuto, ftMDYAuto, ftYMDAuto:
          begin
            ReprElem := AppendElem(Elem, NSlogicalproduct, 'DateTimeRepresentation');
            ReprElem.SetAttribute('type', 'Date');
            ReprElem.SetAttribute('format', F.FormatString());
            ReprElem.SetAttribute('blankIsMissingValue', 'true');
          end;
        ftTime,
        ftTimeAuto:
          begin
            ReprElem := AppendElem(Elem, NSlogicalproduct, 'DateTimeRepresentation');
            ReprElem.SetAttribute('type', 'Time');
            ReprElem.SetAttribute('format', F.FormatString());
            ReprElem.SetAttribute('blankIsMissingValue', 'true');
          end;
        ftString,
        ftUpperString:
          begin
            ReprElem := AppendElem(Elem, NSlogicalproduct, 'TextRepresentation');
            ReprElem.SetAttribute('maxLength', IntToStr(F.Length));
            ReprElem.SetAttribute('blankIsMissingValue', 'true');
          end;
        ftMemo:
          begin
            ReprElem := AppendElem(Elem, NSlogicalproduct, 'TextRepresentation');
            ReprElem.SetAttribute('maxLength', IntToStr(F.MaxByteLength));
            ReprElem.SetAttribute('blankIsMissingValue', 'true');
          end;
      end;

    // Missing Value
    if Assigned(F.ValueLabelSet) and (F.ValueLabelSet.MissingCount > 0) then
    begin
      BackupFormatSettings;
      DefaultFormatSettings.DecimalSeparator := '.';
      S := '';
      for j := 0 to F.ValueLabelSet.Count -1 do
        if F.ValueLabelSet[j].IsMissingValue then
          S += F.ValueLabelSet[j].ValueAsString + ' ';
      ReprElem.SetAttribute('missingValue', TrimRight(S));
      RestoreFormatSettings;
    end;

    // Only add blankIsMissingValue if the field actually contains data.
    for j := 0 to F.Size -1 do
      if F.IsMissing[j] then
      begin
        ReprElem.SetAttribute('blankIsMissingValue', 'true');
        Continue;
      end;

    Inc(i);
  end;
  EpiTool_RemoveFilterInformation(Datafile);
end;

procedure TEpiDDIExport.BuildCodeScheme(LogicalProduct: TDOMElement);
var
  VLSets: TEpiValueLabelSets;
  CatScheme: TDOMElement;
  CodScheme: TDOMElement;
  VSet: TEpiValueLabelSet;
  V: TEpiCustomValueLabel;
  Cat: TDOMElement;
  CodSchemeList: TList;
  i: Integer;
  j: Integer;
  Cod: TDOMElement;
begin
  VLSets := EpiDoc.ValueLabelSets;
  CodSchemeList := TList.Create;

  for i := 0 to VLSets.Count - 1 do
  begin
    VSet := VLSets[i];

    CatScheme := AppendElemMaintainableType(LogicalProduct, NSlogicalproduct, 'CategoryScheme', 'cats');
    CodScheme := AppendElemMaintainableType(nil,            NSlogicalproduct, 'CodeScheme',     'cods');
    CodSchemeList.Add(CodScheme);

    // Circumventing the id's created in Maintainable elemt...
    j := ValueLabelSetsGUIDs.IndexOfObject(VSet);
    CodScheme.SetAttribute('id', ValueLabelSetsGUIDs[j]);


    AppendElemInternationalStringType(CatScheme, NSreuseable, 'Label', VSet.Name);
    AppendElemInternationalStringType(CodScheme, NSreuseable, 'Label', VSet.Name);

    AppendElemReferenceType(CodScheme, NSlogicalproduct, 'CategorySchemeReference', CatScheme);

    BackupFormatSettings;
    FormatSettings.DecimalSeparator := '.';
    for j := 0 to VSet.Count - 1 do
    begin
      V := VSet[j];
      if V.IsMissingValue and FSettings.RemoveMissingVL then continue;

      Cat := AppendElemVersionableType(CatScheme, NSlogicalproduct, 'Category', 'cat');
      AppendElem(Cat, NSreuseable, 'Label', V.TheLabel.Text);

      Cod := AppendElem(CodScheme, NSlogicalproduct, 'Code');
      AppendElemReferenceType(Cod, NSlogicalproduct, 'CategoryReference', Cat);

      AppendElem(Cod, NSlogicalproduct, 'Value', V.ValueAsString);
    end;
    RestoreFormatSettings;
  end;

  for i := 0 to CodSchemeList.Count - 1 do
    LogicalProduct.AppendChild(TDOMElement(CodSchemeList[i]));
end;

procedure TEpiDDIExport.BuildPhysicalDataProduct;
var
  PhysDataProd: TDOMElement;
  PhysStruct: TDOMElement;
  Elem: TDOMElement;
  GrossRecStr: TDOMElement;
  PHRS: TDOMElement;
  DI: TDOMElement;
  F: TEpiField;
  i: Integer;
  S: String;
  CSVSettings: TEpiCSVExportSetting;
begin
  CSVSettings := TEpiCSVExportSetting(FSettings.AdditionalExportSettings);
  PhysDataProd := AppendElemMaintainableType(DDIStudyUnit, NSphysicaldataproduct, 'PhysicalDataProduct', 'phdp');

  // *********************
  // PhysicalStructureScheme
  // *********************
  Elem  := AppendElemMaintainableType(PhysDataProd, NSphysicaldataproduct, 'PhysicalStructureScheme', 'phss');
  PhysStruct := AppendElemVersionableType(Elem, NSphysicaldataproduct, 'PhysicalStructure',       'phst');

  AppendElemReferenceType(PhysStruct, NSphysicaldataproduct, 'LogicalProductReference', DDILogicalProd);

  Elem := AppendElem(PhysStruct, NSphysicaldataproduct, 'Format', 'Delimited file');
  Elem := AppendElem(PhysStruct, NSphysicaldataproduct, 'DefaultDelimiter', CSVSettings.FieldSeparator);
  Elem := AppendElem(PhysStruct, NSphysicaldataproduct, 'DefaultDecimalSeparator', CSVSettings.DecimalSeparator);

  GrossRecStr := AppendElemIdentifiableType(PhysStruct, NSphysicaldataproduct, 'GrossRecordStructure', 'grst');
  AppendElemReferenceType(GrossRecStr, NSphysicaldataproduct, 'LogicalRecordReference', DDILogicalRec);

  PHRS := AppendElemIdentifiableType(GrossRecStr, NSphysicaldataproduct, 'PhysicalRecordSegment', 'phrs');
  PHRS.SetAttribute('hasSegmentKey', 'false');
  PHRS.SetAttribute('segmentOrder', '1');
  // *********************

  // *********************
  // RecordLayoutScheme
  // *********************
  DDIRels := AppendElemMaintainableType(PhysDataProd, NSphysicaldataproduct, 'RecordLayoutScheme', 'rels');
  DDIRely := AppendElemVersionableType(DDIRels, NSphysicaldataproduct, 'RecordLayout', 'rely');

  Elem := AppendElemReferenceType(DDIRely, NSphysicaldataproduct, 'PhysicalStructureReference', PhysDataProd);
  AppendElem(Elem, NSphysicaldataproduct, 'PhysicalRecordSegmentUsed', PHRS.GetAttribute('id'));

  AppendElem(DDIRely, NSphysicaldataproduct, 'CharacterSet', 'UTF-8');
  AppendElem(DDIRely, NSphysicaldataproduct, 'ArrayBase', '1');
  AppendElemReferenceType(DDIRely, NSphysicaldataproduct, 'DefaultVariableSchemeReference', DDIVarScheme);

  I := 0;
  for F in Datafile.Fields do
  begin
    DI := AppendElem(DDIRely, NSphysicaldataproduct, 'DataItem');
    AppendElemReferenceType(DI, NSphysicaldataproduct, 'VariableReference', idFromMap(VarsMap, F));
    Elem := AppendElem(DI, NSphysicaldataproduct, 'PhysicalLocation');

    case F.FieldType of
      ftBoolean,
      ftInteger,
      ftAutoInc: S := 'integer';
      ftFloat:   S := 'real';
      ftDMYDate,
      ftMDYDate,
      ftYMDDate,
      ftDMYAuto,
      ftMDYAuto,
      ftYMDAuto,
      ftTime,
      ftTimeAuto,
      ftString,
      ftUpperString,
      ftMemo: S := 'string';
    end;
    AppendElem(Elem, NSphysicaldataproduct, 'StorageFormat', S);
    AppendElem(Elem, NSphysicaldataproduct, 'ArrayPosition', IntToStr(I + 1));
    AppendElem(Elem, NSphysicaldataproduct, 'Width', IntToStr(F.Length));
    if F.FieldType in FloatFieldTypes then
      AppendElem(Elem, NSphysicaldataproduct, 'DecimalPositions', IntToStr(F.Decimals));

    Inc(I);
  end;
end;

procedure TEpiDDIExport.BuildPhysicalInstance;
var
  PhysicalInst: TDOMElement;
  RecLayoutRef: TDOMElement;
  Elem: TDOMElement;
  Fn: String;
  GRFS: TDOMElement;
begin
  PhysicalInst := AppendElemMaintainableType(DDIStudyUnit, NSphysicalinstance, 'PhysicalInstance', 'phin');
  RecLayoutRef := AppendElemReferenceType(PhysicalInst, NSphysicalinstance, 'RecordLayoutReference', DDIRely);

  Elem := AppendElemIdentifiableType(PhysicalInst, NSphysicalinstance, 'DataFileIdentification', 'dafi');

  Fn := ExtractFileName(ChangeFileExt(FDFSettings.ExportFileName, '.csv'));
  AppendElem(Elem, NSphysicalinstance, 'URI', Fn);

  GRFS := AppendElemIdentifiableType(PhysicalInst, NSphysicalinstance, 'GrossFileStructure', 'grfs');
  Elem := AppendElemIdentifiableType(GRFS, NSphysicalinstance, 'CreationSoftware', 'crsw');
  AppendElem(Elem, NSreuseable, 'Name', FSettings.SoftwareName);
  AppendElem(Elem, NSreuseable, 'Version', FSettings.SoftwareVersion);
  AppendElem(Elem, NSreuseable, 'Description', 'website: <a href="http://www.epidata.dk">http://www.epidata.dk</a>');

  AppendElem(GRFS, NSphysicalinstance, 'OverallRecordCount', IntToStr(EpiDoc.DataFiles[0].Size));
end;

procedure TEpiDDIExport.BuildArchive;
var
  Archive: TDOMElement;
  OrgScheme: TDOMElement;
  Org: TDOMElement;
  Elem: TDOMElement;
begin
{  Archive := AppendElemMaintainableType(DDIStudyUnit, NSarchive, 'Archive', 'arch');
  Elem := AppendElem(Archive, NSarchive, 'ArchiveSpecific');
  AppendElemReferenceType(Elem, NSarchive, 'ArchiveOrganizationReference', 'Not_Filled');

  if (FSettings.FundAgencyName <> '') then
  begin
    OrgScheme := AppendElemMaintainableType(Archive,   NSarchive, 'OrganizationScheme', 'orgs');
    Org := AppendElemVersionableType(       OrgScheme, NSarchive, 'Organization',       'orga');
    AppendElemInternationalStringType(Org, NSarchive, 'OrganizationName', FSettings.FundAgencyName);
    if (FSettings.FundAgencyAddress <> '') then
    begin
      Elem := AppendElemIdentifiableType(Org, NSarchive, 'Location', 'loca');
      AppendElem(Elem, NSarchive, 'Address', FSettings.FundAgencyAddress);
    end;

    // Now we have the funding orgination, create the content of the FundingInfo element.
    AppendElemReferenceType(DDIFunding, NSreuseable, 'AgencyOrganizationReference', Org);
  end;   }
end;

constructor TEpiDDIExport.Create;
begin
  ValueLabelSetsGUIDs := TStringList.Create;
  QuieMap := TFPSMap.Create;
  QuecMap := TFPSMap.Create;
  VarsMap := TFPSMap.Create;
  ConcMap := TFPSMap.Create;
end;

destructor TEpiDDIExport.Destroy;
begin
  ConcMap.Free;
  VarsMap.Free;
  QuieMap.Free;
  QuecMap.Free;
  ValueLabelSetsGUIDs.Free;
  inherited Destroy;
end;

function TEpiDDIExport.ExportDDI(const Settings: TEpiDDIExportSetting): boolean;
var
  i: Integer;
begin
  result := false;

  FSettings := Settings;
  FCSVSettings := TEpiCSVExportSetting(FSettings.AdditionalExportSettings);
  EpiDoc := Settings.Doc;

  if (not Assigned(FCSVSettings)) or
     (not (FCSVSettings is TEpiCSVExportSetting))
  then
    Exit;

  try
    for i := 0 to Settings.DatafileSettings.Count - 1 do
    begin
      FDFSettings := Settings.DatafileSettings[i];
      FDFCSVSettings := FCSVSettings.DatafileSettings[i];

      Datafile    := EpiDoc.DataFiles.GetDataFileByName(FDFSettings.DatafileName);

      ExportCSVFile;

      XMLDoc := TXMLDocument.Create;
      DDIInstance := AppendElemMaintainableType(nil, 'ddi:instance:3_1', 'DDIInstance', 'inst');
      XMLDoc.AppendChild(DDIInstance);
      DDIInstance := XMLDoc.DocumentElement;

      with DDIInstance do
      begin
        SetAttribute('xmlns:xsi', 'http://www.w3.org/2001/XMLSchema-instance');
        SetAttribute('xsi:schemaLocation', 'ddi:instance:3_1 http://www.ddialliance.org/sites/default/files/schema/ddi3.1/instance.xsd');
      end;

      DDIStudyUnit := AppendElemMaintainableType(DDIInstance, NSstudy, 'StudyUnit', 'stud');

      // Build the <StudyUnit>
      BuildCitations;
      BuildAbstract;
      BuildUniverseRef;
    //  BuildFunding;
      BuildPurpose;
      BuildCoverage;
      BuildUnitOfObs;
      BuildKindOfData;

      BuildConceptualComponent;
      BuildDataCollection;
      BuildLogicalProduct;
      BuildPhysicalDataProduct;
      BuildPhysicalInstance;
    //  BuildArchive;

      WriteXML(XMLDoc, FDFSettings.ExportStream);
      FreeAndNil(XMLDoc);
    end;
    Result := true;
  finally
    FreeAndNil(XMLDoc);
  end;
end;

end.

