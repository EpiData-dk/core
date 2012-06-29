unit epiexport_ddi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidocument, epidatafiles, epidatafilestypes, epivaluelabels,
  epieximtypes, epiexportsettings, DOM, epicustombase, fgl;

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

    // Helper methods.
    procedure AddAttrNameSpace(Elem: TDOMElement; Const NameSpace: string);
    procedure AddAttrID(Elem: TDOMElement; Const Prefix: string);
    procedure AddAttrTranslation(Elem: TDOMElement);
    procedure AddAttrLang(Elem: TDOMElement; Const Lang: string);

    procedure AppendContent(Root: TDOMElement; Const Text: string; Const ContentName: string = 'Content');
    function  AppendElem(Root: TDOMElement; Const NameSpace, NodeName: string;
      Const Text: String = ''): TDOMElement;


    procedure ExportCSVFile;
    // Block to produce.
    procedure BuildCitations;
    procedure BuildAbstract;
    procedure BuildUniverseRef;
    procedure BuildFunding;
    procedure BuildPurpose;
    procedure BuildCoverage;
    procedure BuildConceptualComponent;

    procedure BuildDataCollection;
    // Datacollection helpers:
    procedure BuildQuestionScheme(DataCollection: TDOMElement);
    // helper: Returns MainSequence
    function BuildControlConstructScheme(DataCollection: TDOMElement): TDomElement;

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
  XMLWrite, epiexport, LazUTF8;

const
  NSreuseable = 'ddi:reusable:3_1';
  NSstudy     = 'ddi:studyunit:3_1';
  NSconcept   = 'ddi:conceptualcomponent:3_1';
  NSdatacollection = 'ddi:datacollection:3_1';
  NSlogicalproduct = 'ddi:logicalproduct:3_1';
  NSphysicaldataproduct = 'ddi:physicaldataproduct:3_1';
  NSphysicalinstance = 'ddi:physicalinstance:3_1';
  NSarchive          = 'ddi:archive:3_1';
  // .... todo!

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

procedure TEpiDDIExport.ExportCSVFile;
var
  CSVExporter: TEpiExport;
  TxtExportSetting: TEpiCSVExportSetting;
  i: Integer;
begin
  TxtExportSetting := TEpiCSVExportSetting.Create;
  TxtExportSetting.Assign(FSettings);
  TxtExportSetting.FieldSeparator := '!';
  TxtExportSetting.DecimalSeparator := '.';
  TxtExportSetting.DateSeparator  := '-';
  TxtExportSetting.TimeSeparator  := ':';
  TxtExportSetting.QuoteChar      := '';
  TxtExportSetting.FixedFormat    := true;
  TxtExportSetting.NewLine        := LineEnding;
  TxtExportSetting.Encoding       := eeUTF8;
  TxtExportSetting.ExportFieldNames := false;
  TxtExportSetting.ExportFileName := ChangeFileExt(FSettings.ExportFileName, '.csv');
  for i := 0 to FSettings.Doc.DataFiles[0].Fields.Count - 1 do
    TxtExportSetting.Fields.Add(FSettings.Doc.DataFiles[0].Field[i]);

  CSVExporter := TEpiExport.Create;
  CSVExporter.ExportCSV(TxtExportSetting);
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
  Seq: TDOMElement;
  Elem: TDOMElement;
begin
  DataCollection := XMLDoc.CreateElementNS(NSdatacollection, 'DataCollection');
  AddAttrID(DataCollection, 'daco');
  DDIStudyUnit.AppendChild(DataCollection);

  BuildQuestionScheme(DataCollection);
  Seq := BuildControlConstructScheme(DataCollection);

  Elem := AppendElem(DataCollection, NSdatacollection, 'Instrument');
  AddAttrID(Elem, 'inst');
  Elem := AppendElem(Elem, NSdatacollection, 'ControlConstructReference');
  AppendElem(Elem, NSreuseable, 'ID', Seq.GetAttribute('id'));
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
  Elem: TDOMElement;
  F: TEpiField;
begin
  QScheme := XMLDoc.CreateElementNS(NSdatacollection, 'QuestionScheme');
  AddAttrID(QScheme, 'ques');
  DataCollection.AppendChild(QScheme);

  // Build the list of GUID's for all our valuelabelsets.
  for i := 0 to FSettings.Doc.ValueLabelSets.Count - 1 do
    ValueLabelSetsGUIDs.AddObject(CreateAttrID('cods'), FSettings.Doc.ValueLabelSets[i]);

  for i := 0 to FSettings.Doc.DataFiles[0].Fields.Count - 1 do
  with FSettings.Doc.DataFiles[0].Field[i] do
  begin
    F := FSettings.Doc.DataFiles[0].Field[i];

    QItem := XMLDoc.CreateElementNS(NSdatacollection, 'QuestionItem');
    AddAttrID(QItem, 'quei');
    QScheme.AppendChild(QItem);
    QuieMap.Add(@F, @QItem);

    QText := AppendElem(QItem, NSdatacollection, 'QuestionText', '');
    QLiteralText := AppendElem(QText, NSdatacollection, 'LiteralText', '');
    Elem := AppendElem(QLiteralText, NSdatacollection, 'Text', Question.Text);

    if Assigned(ValueLabelSet) then
    begin
      Domain := AppendElem(QItem, NSdatacollection, 'CodeDomain');
      j := ValueLabelSetsGUIDs.IndexOfObject(ValueLabelSet);
      Elem := AppendElem(Domain, NSreuseable, 'CodeSchemeReference');
      AppendElem(Elem, NSreuseable, 'ID', ValueLabelSetsGUIDs[j]);
    end else
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
            Domain.SetAttribute('type', 'Date');
            Domain.SetAttribute('format', FormatString());
          end;
        ftTime,
        ftTimeAuto:
          begin
            Domain := AppendElem(QItem, NSdatacollection, 'DateTimeDomain');
            Domain.SetAttribute('type', 'Time');
            Domain.SetAttribute('format', FormatString());
          end;
        ftString,
        ftUpperString:
          begin
            Domain := AppendElem(QItem, NSdatacollection, 'TextDomain');
            Domain.SetAttribute('maxLength', IntToStr(Length));
          end;
      end;
    // Missing Value
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

function TEpiDDIExport.BuildControlConstructScheme(DataCollection: TDOMElement
  ): TDomElement;
var
  CCS: TDOMElement;
  i: Integer;
  MainSequence: TDOMElement;
  Elem: TDOMElement;
  DF: TEpiDataFile;
  DoneItem: TDOMElement;
  F: TEpiField;
  QCons: TDOMElement;

  procedure BuildSequence(Sequence: TDOMElement; FromIndex: integer);
  var
    QCons: TDOMElement;
    QConsRef: TDOMElement;
    i: integer;
    NewSequence: TDOMElement;
    Idx: Integer;
    Elem: TDOMElement;
    ITE: TDOMElement;
    Jmp: TEpiJump;
    F: TEpiField;
    TheITE: TDOMElement;
  begin
    while FromIndex < Df.Fields.Count do
    begin
      F := FSettings.Doc.DataFiles[0].Field[FromIndex];
      QConsRef := AppendElem(Sequence, NSdatacollection, 'ControlConstructReference');
      QCons := TDOmElement(QuecMap.KeyData[@F]^);
      AppendElem(QConsRef, NSreuseable, 'ID', QCons.GetAttribute('id'));
      Inc(FromIndex);

      if Assigned(F.Jumps) and (F.Jumps.Count > 0) then
      for i := 0 to F.Jumps.Count - 1 do
      begin
        Jmp := F.Jumps[i];

        // Build Main IfThenElse Node(s):
        if i = 0 then
        begin
          ITE := AppendElem(CCS, NSdatacollection, 'IfThenElse');
          TheITE := ITE;
          AddAttrID(ITE, 'ifth');
        end else begin
          ITE := AppendElem(TheITE, NSdatacollection, 'ElseIf');
        end;


        // Add a reference in the original sequence to this IfThenElse node:
        QConsRef := AppendElem(Sequence, NSdatacollection, 'ControlConstructReference');
        AppendElem(QConsRef, NSreuseable, 'ID', ITE.GetAttribute('id'));

        // Build Inner nodes of IfThenElse:
        // - If Condition:
        Elem := AppendElem(ITE, NSdatacollection, 'IfCondition');
        AppendElem(Elem, NSreuseable, 'Code', F.Name + '=' + Jmp.JumpValueAsString);
        // - Source (Field) of If Condition
        Elem := AppendElem(Elem, NSreuseable, 'SourceQuestionReference');
        AppendElem(Elem, NSreuseable, 'ID', QCons.GetAttribute('id'));


        // Handle special case with jtSaveRecord:
        if Jmp.JumpType = jtSaveRecord then
        begin
          Elem := AppendElem(ITE, NSdatacollection, 'ThenConstructReference');
          AppendElem(Elem, NSreuseable, 'ID', DoneItem.GetAttribute('id'));
          Continue;
        end;

        // Construct new branching sequence for this jump value:
        NewSequence := XMLDoc.CreateElementNS(NSdatacollection, 'Sequence');
        AppendElem(NewSequence, NSreuseable, 'Label', 'Sequence: ' + F.Name + ' Jump=' + Jmp.JumpValueAsString);
        AddAttrID(NewSequence, 'seqc');
        CCS.AppendChild(NewSequence);
        Case F.Jumps[i].JumpType of
          jtExitSection:
            begin
              for Idx := FromIndex to Df.Fields.Count -1 do
                if Df[Idx].Section <> F.Section then break;
            end;
          jtSkipNextField:
            Idx := Df.Fields.IndexOf(F) + 1;
          jtToField:
            Idx := Df.Fields.IndexOf(F.Jumps[i].JumpToField);
        end;
        Elem := AppendElem(ITE, NSdatacollection, 'ThenConstructReference');
        AppendElem(Elem, NSreuseable, 'ID', NewSequence.GetAttribute('id'));

        BuildSequence(NewSequence, Idx);
      end;
    end;
  end;

begin
  CCS := XMLDoc.CreateElementNS(NSdatacollection, 'ControlConstructScheme');
  AddAttrID(CCS, 'cocs');
  DataCollection.AppendChild(CCS);

  MainSequence := XMLDoc.CreateElementNS(NSdatacollection, 'Sequence');
  AppendElem(MainSequence, NSreuseable, 'Label', 'Main Sequence');
  AddAttrID(MainSequence, 'seqc');
  CCS.AppendChild(MainSequence);

  DF := FSettings.Doc.DataFiles[0];

  for i := 0 to Df.Fields.Count - 1 do
  begin
    F := Df.Field[i];

    QCons := XMLDoc.CreateElementNS(NSdatacollection, 'QuestionConstruct');
    AddAttrID(QCons, 'quec');
    Elem := AppendElem(QCons, NSdatacollection, 'QuestionReference');
    AppendElem(Elem, NSreuseable, 'ID', TDOMElement(QuieMap.KeyData[@F]^).GetAttribute('id'));

    CCS.AppendChild(QCons);
    QuecMap.Add(@F, @QCons);
  end;
  DoneItem := AppendElem(CCS, NSdatacollection, 'StatementItem');
  AddAttrID(DoneItem, 'stai');
  Elem := AppendElem(DoneItem, NSdatacollection, 'DisplayText');
  Elem := AppendElem(Elem, NSdatacollection, 'LiteralText');
  AppendElem(Elem, NSdatacollection, 'Text', 'End of Fields.');

  BuildSequence(MainSequence, 0);
  Result := MainSequence;
end;

procedure TEpiDDIExport.BuildLogicalProduct;
var
  Elem: TDOMElement;
begin
  DDILogicalProd := AppendElem(DDIStudyUnit, NSlogicalproduct, 'LogicalProduct');
  AddAttrID(DDILogicalProd, 'lopr');

  DDIVarScheme := BuildVariableScheme;

  Elem := AppendElem(DDILogicalProd, NSlogicalproduct, 'DataRelationship');
  AddAttrID(Elem, 'dars');
  DDILogicalRec := AppendElem(Elem, NSlogicalproduct, 'LogicalRecord');
  AddAttrID(DDILogicalRec, 'lore');
  DDILogicalRec.SetAttribute('hasLocator', 'false');
  Elem := AppendElem(DDILogicalRec, NSlogicalproduct, 'VariablesInRecord');
  Elem.SetAttribute('allVariablesInLogicalProduct', 'true');
  Elem := AppendElem(Elem, NSlogicalproduct, 'VariableSchemeReference');
  AppendElem(Elem, NSreuseable, 'ID', DDIVarScheme.GetAttribute('id'));

  BuildCodeScheme(DDILogicalProd);

  DDILogicalProd.AppendChild(DDIVarScheme);
end;

function TEpiDDIExport.BuildVariableScheme: TDOMElement;
var
  Df: TEpiDataFile;
  F: TEpiField;
  VarElem: TDOMElement;
  i: Integer;
  Elem: TDOMElement;
  ReprElem: TDOMElement;
  j: Integer;
  S: String;
begin
  Result := XMLDoc.CreateElementNS(NSlogicalproduct, 'VariableScheme');
  AddAttrID(Result, 'vars');

  Df := FSettings.Doc.DataFiles[0];
  for i := 0 to Df.Fields.Count -1 do
  begin
    F := Df.Field[i];

    VarElem := AppendElem(Result, NSlogicalproduct, 'Variable');
    VarsMap.Add(@F, @VarElem);

    AddAttrID(VarElem, 'vari');
    AppendElem(VarElem, NSlogicalproduct, 'VariableName', F.Name);
    AppendElem(VarElem, NSreuseable, 'Label', F.Question.Text);
    Elem := AppendElem(VarElem, NSlogicalproduct, 'QuestionReference');
    AppendElem(Elem, NSreuseable, 'ID', TDOMElement(QuieMap.KeyData[@F]^).GetAttribute('id'));

    Elem := AppendElem(VarElem, NSlogicalproduct, 'Representation');


    if Assigned(F.ValueLabelSet) then
    begin
      ReprElem := AppendElem(Elem, NSlogicalproduct, 'CodeRepresentation');
      j := ValueLabelSetsGUIDs.IndexOfObject(F.ValueLabelSet);
      Elem := AppendElem(ReprElem, NSreuseable, 'CodeSchemeReference');
      AppendElem(Elem, NSreuseable, 'ID', ValueLabelSetsGUIDs[j]);
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
            ReprElem.SetAttribute('startValue', IntToStr(FSettings.Doc.ProjectSettings.AutoIncStartValue));
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
          end;
        ftTime,
        ftTimeAuto:
          begin
            ReprElem := AppendElem(Elem, NSlogicalproduct, 'DateTimeRepresentation');
            ReprElem.SetAttribute('type', 'Time');
            ReprElem.SetAttribute('format', F.FormatString());
          end;
        ftString,
        ftUpperString:
          begin
            ReprElem := AppendElem(Elem, NSlogicalproduct, 'TextRepresentation');
            ReprElem.SetAttribute('maxLength', IntToStr(F.Length));
          end;
      end;
    // Missing Value
    if Assigned(F.ValueLabelSet) and (F.ValueLabelSet.MissingCount > 0) then
    begin
      S := '';
      for j := 0 to F.ValueLabelSet.Count -1 do
        if F.ValueLabelSet[j].IsMissingValue then
          S += F.ValueLabelSet[j].ValueAsString + ' ';
      ReprElem.SetAttribute('missingValue', TrimRight(S));
    end;
    ReprElem.SetAttribute('blankIsMissingValue', 'true');
  end;
end;

procedure TEpiDDIExport.BuildCodeScheme(LogicalProduct: TDOMElement);
var
  VLSets: TEpiValueLabelSets;
  CatScheme: TDOMElement;
  CodScheme: TDOMElement;
  VSet: TEpiValueLabelSet;
  V: TEpiCustomValueLabel;
  Cat: TDOMElement;
  Elem: TDOMElement;
  CodSchemeList: TList;
  i: Integer;
  j: Integer;
  Cod: TDOMElement;
begin
  VLSets := FSettings.Doc.ValueLabelSets;
  CodSchemeList := TList.Create;

  for i := 0 to VLSets.Count - 1 do
  begin
    VSet := VLSets[i];

    CatScheme := AppendElem(LogicalProduct, NSlogicalproduct, 'CategoryScheme');
    AddAttrID(CatScheme, 'cats');
    CodScheme := XMLDoc.CreateElementNS(NSlogicalproduct, 'CodeScheme');
    j := ValueLabelSetsGUIDs.IndexOfObject(VSet);
    CodScheme.SetAttribute('id', ValueLabelSetsGUIDs[j]);
    CodSchemeList.Add(CodScheme);

    AppendElem(CatScheme, NSreuseable, 'Label', VSet.Name);
    AppendElem(CodScheme, NSreuseable, 'Label', VSet.Name);

    Elem := AppendElem(CodScheme, NSlogicalproduct, 'CategorySchemeReference');
    AppendElem(Elem, NSreuseable, 'ID', CatScheme.GetAttribute('id'));

    for j := 0 to VSet.Count - 1 do
    begin
      V := VSet[j];

      Cat := AppendElem(CatScheme, NSlogicalproduct, 'Category');
      AddAttrID(Cat, 'cat');
      AppendElem(Cat, NSreuseable, 'Label', V.TheLabel.Text);

      Cod := AppendElem(CodScheme, NSlogicalproduct, 'Code');
      Elem := AppendElem(Cod, NSlogicalproduct, 'CategoryReference');
      AppendElem(Elem, NSreuseable, 'ID', Cat.GetAttribute('id'));

      AppendElem(Cod, NSlogicalproduct, 'Value', V.ValueAsString);
    end;
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
  RL: TDOMElement;
  PHRS: TDOMElement;
  Df: TEpiDataFile;
  DI: TDOMElement;
  F: TEpiField;
  StartPos: Integer;
  i: Integer;
  S: String;
begin
  PhysDataProd := AppendElem(DDIStudyUnit, NSphysicaldataproduct, 'PhysicalDataProduct');
  AddAttrID(PhysDataProd, 'phdp');
  // *********************
  // PhysicalStructureScheme
  // *********************
  Elem  := AppendElem(PhysDataProd, NSphysicaldataproduct, 'PhysicalStructureScheme');
  AddAttrID(Elem, 'phss');
  PhysStruct := AppendElem(Elem, NSphysicaldataproduct, 'PhysicalStructure');
  AddAttrID(PhysStruct, 'phst');

  Elem := AppendElem(PhysStruct, NSphysicaldataproduct, 'LogicalProductReference');
  AppendElem(Elem, NSreuseable, 'ID', DDILogicalProd.GetAttribute('id'));

  Elem := AppendElem(PhysStruct, NSphysicaldataproduct, 'Format', 'FIXED');
  Elem := AppendElem(PhysStruct, NSphysicaldataproduct, 'DefaultDecimalSeparator', '.');

  GrossRecStr := AppendElem(PhysStruct, NSphysicaldataproduct, 'GrossRecordStructure');
  AddAttrID(GrossRecStr, 'grst');
  Elem := AppendElem(GrossRecStr, NSphysicaldataproduct, 'LogicalRecordReference');
  AppendElem(Elem, NSreuseable, 'ID', DDILogicalRec.GetAttribute('id'));

  PHRS := AppendElem(GrossRecStr, NSphysicaldataproduct, 'PhysicalRecordSegment');
  AddAttrID(PHRS, 'phrs');
  PHRS.SetAttribute('hasSegmentKey', 'false');
  PHRS.SetAttribute('segmentOrder', '1');
  // *********************

  // *********************
  // RecordLayoutScheme
  // *********************
  DDIRels := AppendElem(PhysDataProd, NSphysicaldataproduct, 'RecordLayoutScheme');
  AddAttrID(DDIRels, 'rels');

  RL := AppendElem(DDIRels, NSphysicaldataproduct, 'RecordLayout');
  DDIRely := RL;
  AddAttrID(RL, 'rely');

  Elem := AppendElem(RL, NSphysicaldataproduct, 'PhysicalStructureReference');
  AppendElem(Elem, NSreuseable, 'ID', PhysDataProd.GetAttribute('id'));
  AppendElem(Elem, NSphysicaldataproduct, 'PhysicalRecordSegmentUsed', PHRS.GetAttribute('id'));

  AppendElem(RL, NSphysicaldataproduct, 'CharacterSet', 'UTF-8');
  AppendElem(RL, NSphysicaldataproduct, 'ArrayBase', '1');
  Elem := AppendElem(RL, NSphysicaldataproduct, 'DefaultVariableSchemeReference');
  AppendElem(Elem, NSreuseable, 'ID', DDIVarScheme.GetAttribute('id'));

  Df := FSettings.Doc.DataFiles[0];
  StartPos := 1;
  for i := 0 to Df.Fields.Count - 1 do
  begin
    F := DF.Field[i];

    DI := AppendElem(RL, NSphysicaldataproduct, 'DataItem');
    Elem := AppendElem(DI, NSphysicaldataproduct, 'VariableReference');
    AppendElem(Elem, NSreuseable, 'ID', TDOMElement(VarsMap.KeyData[@F]^).GetAttribute('id'));

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
      ftUpperString: S := 'string';
    end;
    AppendElem(Elem, NSphysicaldataproduct, 'StorageFormat', S);
    AppendElem(Elem, NSphysicaldataproduct, 'StartPosition', IntToStr(StartPos));
    AppendElem(Elem, NSphysicaldataproduct, 'Width', IntToStr(F.Length));
    if F.FieldType in FloatFieldTypes then
      AppendElem(Elem, NSphysicaldataproduct, 'DecimalPositions', IntToStr(F.Decimals));
    Inc(StartPos, F.Length);
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
  PhysicalInst := AppendElem(DDIStudyUnit, NSphysicalinstance, 'PhysicalInstance');
  AddAttrID(PhysicalInst, 'phin');
  RecLayoutRef := AppendElem(PhysicalInst, NSphysicalinstance, 'RecordLayoutReference');
  Elem := AppendElem(RecLayoutRef, NSreuseable, 'Scheme');
  AppendElem(Elem, NSreuseable, 'ID', DDIRels.GetAttribute('id'));

  AppendElem(RecLayoutRef, NSreuseable, 'ID', DDIRely.GetAttribute('id'));

  Elem := AppendElem(PhysicalInst, NSphysicalinstance, 'DataFileIdentification');
  AddAttrID(Elem, 'dafi');
  Fn := UTF8ToSys(FSettings.ExportFileName);
  Fn := ExtractFileName(ChangeFileExt(Fn, 'txt'));
  AppendElem(Elem, NSphysicalinstance, 'URI', Fn);

  GRFS := AppendElem(PhysicalInst, NSphysicalinstance, 'GrossFileStructure');
  AddAttrID(GRFS, 'grfs');
  Elem := AppendElem(GRFS, NSphysicalinstance, 'CreationSoftware');
  AddAttrID(Elem, 'crsw');
  AppendElem(Elem, NSreuseable, 'Name', 'EpiData');

  AppendElem(GRFS, NSphysicalinstance, 'CaseQuantity', IntToStr(FSettings.Doc.DataFiles[0].Size));
  AppendElem(GRFS, NSphysicalinstance, 'OverallRecordCount', IntToStr(FSettings.Doc.DataFiles[0].Size));
end;

procedure TEpiDDIExport.BuildArchive;
var
  Archive: TDOMElement;
  OrgScheme: TDOMElement;
  Org: TDOMElement;
  Elem: TDOMElement;
begin
{
<Archive xmlns="ddi:archive:3_1" id="5428ce11-5f2e-40c4-893e-e6439e536de0" version="1.0.0" versionDate="2012-05-30T13:28:54.496+02:00" agency="dk.dda">
  <VersionResponsibility xmlns="ddi:reusable:3_1">ddajvj</VersionResponsibility>
  <VersionRationale xmlns="ddi:reusable:3_1" translated="false" translatable="true" xml:lang="da">Version: 1.0.0, date: 2012-05-30T13:28:54.496+02:00</VersionRationale>
  <OrganizationScheme id="51ad1f62-30a7-4179-9169-71d7febbec48" version="1.0.0" versionDate="2012-05-30T13:28:54.504+02:00" agency="dk.dda">
    <Individual id="14627">
      <IndividualName>
        <First>Jørgen Møller Christiansen</First>
      </IndividualName>
      <DDIMaintenanceAgencyID registryID="dk.dda"/>
      <Location id="a96f0cdd-add8-4f51-8df9-d06bcb51577d">
        <Address>
          <Line>Center for Alternativ Samfundsanalyse</Line>
          <Line>Kigkurren 8M, st</Line>
          <City>KØBENHAVN S</City>
          <Postal>2300</Postal>
        </Address>
        <Country codeClass="ISO-3166">DK</Country>
        <Telephone>3389 0155</Telephone>
        <Email>jmc@casa-analyse.dk</Email>
      </Location>
    </Individual>
  </OrganizationScheme>
</Archive>
}
  Archive := AppendElem(DDIStudyUnit, NSarchive, 'Archive');
  AddAttrID(Archive, '');
  Elem := AppendElem(Archive, NSarchive, 'ArchiveSpecific');
  Elem := AppendElem(Elem, NSarchive, 'ArchiveOrganizationReference');
  AppendElem(Elem, NSreuseable, 'ID', 'NotFilled');

  if (FSettings.FundAgencyName <> '') then
  begin
    OrgScheme := AppendElem(Archive, NSarchive, 'OrganizationScheme');
    AddAttrID(OrgScheme, '');

    Org := AppendElem(OrgScheme, NSarchive, 'Organization');
    AddAttrID(Org, '');
    AppendElem(Org, NSarchive, 'OrganizationName', FSettings.FundAgencyName);
    if (FSettings.FundAgencyAddress <> '') then
    begin
      Elem := AppendElem(Org, NSarchive, 'Location');
      AddAttrID(Elem, '');
      AppendElem(Elem, NSarchive, 'Address', FSettings.FundAgencyAddress);
    end;

    // Now we have the funding orgination, create the content of the FundingInfo element.
    Elem := AppendElem(DDIFunding, NSreuseable, 'AgencyOrganizationReference');
    AppendElem(Elem, NSreuseable, 'ID', Org.GetAttribute('id'));
  end;
end;

constructor TEpiDDIExport.Create;
begin
  ValueLabelSetsGUIDs := TStringList.Create;
  QuieMap := TFPSMap.Create;
  QuecMap := TFPSMap.Create;
  VarsMap := TFPSMap.Create;
end;

destructor TEpiDDIExport.Destroy;
begin
  VarsMap.Free;
  QuieMap.Free;
  QuecMap.Free;
  ValueLabelSetsGUIDs.Free;
  inherited Destroy;
end;

function TEpiDDIExport.ExportDDI(const Settings: TEpiDDIExportSetting): boolean;
var
  Elem: TDOMElement;
  RefIDElem: TDOMElement;
  TxtExportSetting: TEpiCSVExportSetting;
begin
  Settings.SanetyCheck;
  FSettings := Settings;
  EpiDoc := Settings.Doc;

  ExportCSVFile;

  XMLDoc := TXMLDocument.Create;
  DDIInstance := XMLDoc.CreateElementNS('ddi:instance:3_1', 'DDIInstance');
  XMLDoc.AppendChild(DDIInstance);
  DDIInstance := XMLDoc.DocumentElement;

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
  BuildPhysicalInstance;
  BuildArchive;

  WriteXML(XMLDoc, Settings.ExportFileName)
end;

end.

