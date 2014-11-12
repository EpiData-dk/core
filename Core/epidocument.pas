unit epidocument;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, Laz2_DOM,
  episettings, epiadmin, epidatafiles,
  epistudy, epirelations, epivaluelabels,
  epicustombase, epidatafilestypes;

type

  TEpiDocumentChangeEvent = (edcePassword);

  TEpiProgressType =
    (
      eptInit,
      eptFinish,
      eptRecords
    );

  TEpiProgressEvent = procedure (
    Const Sender: TEpiCustomBase;
    ProgressType: TEpiProgressType;
    CurrentPos, MaxPos: Cardinal;
    var Canceled: Boolean) of object;

  TEpiDocumentLoadErrorEvent = procedure (
    Const Sender: TEpiCustomBase;
    ErrorType: Word;       // 0 = External ValueLabel file not found (Data = filename)
    Data: Pointer;
    out Continue: boolean
  ) of object;

  TEpiRelationListEx = class;

  { TEpiDocument }

  TEpiDocument = class(TEpiCustomBase)
  private
    FAdmin: TEpiAdmin;
    FCycleNo: Int64;
    FLoading: boolean;
    FOnLoadError: TEpiDocumentLoadErrorEvent;
    FOnProgress: TEpiProgressEvent;
    FPassWord: string;
    FProjectSettings: TEpiProjectSettings;
    FValueLabelSets: TEpiValueLabelSets;
    FVersion: integer;
    FXMLSettings: TEpiXMLSettings;
    FStudy: TEpiStudy;
    FDataFiles: TEpiDataFiles;
    FRelations: TEpiRelationListEx;
    function   GetOnPassword: TRequestPasswordEvent;
    procedure  SetOnPassword(const AValue: TRequestPasswordEvent);
    procedure  SetPassWord(AValue: string);
  protected
    procedure  SetModified(const AValue: Boolean); override;
    function   SaveAttributesToXml: string; override;
  public
    constructor Create(Const LangCode: string);
    destructor Destroy; override;
    function   XMLName: string; override;
    procedure  LoadFromFile(const AFileName: string);
    procedure  LoadFromStream(const St: TStream);
    procedure  LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
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
    Property   Relations: TEpiRelationListEx read FRelations;
    property   OnPassword: TRequestPasswordEvent read GetOnPassword write SetOnPassword;
    property   OnProgress: TEpiProgressEvent read FOnProgress write FOnProgress;
    property   OnLoadError: TEpiDocumentLoadErrorEvent read FOnLoadError write FOnLoadError;
    property   Loading: boolean read FLoading;
    Property   Version: integer read FVersion;
    // EpiData XML Version 2 perperties:
    property   PassWord: string read FPassWord write SetPassWord;

  { Cycle numbering }
  public
    procedure  IncCycleNo;
    property   CycleNo: Int64 read FCycleNo;

  { Cloning }
  protected
    function   DoCloneCreate(AOwner: TEpiCustomBase): TEpiCustomBase; override;
    function   DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
      ReferenceMap: TEpiReferenceMap): TEpiCustomBase; override;

  public
    function   SaveToXmlDocument: TXMLDocument;
  protected
    function   SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
  end;


  { TEpiRelationListEx }

  TEpiRelationListExCallBack = procedure(Const Relation: TEpiMasterRelation;
    Const Depth: Cardinal; Const Index: Cardinal; Var aContinue: boolean;
    Data: Pointer = nil) of object;

  TEpiRelationListEx = class(TEpiRelationList)
  public
    function GetOrderedDataFiles: TEpiDataFiles;
    procedure OrderedWalk(Const CallBackMethod: TEpiRelationListExCallBack;
      Data: Pointer = nil);
  public
    { Aux. methods }
    function IsMultiLeveled: boolean;     // Returns true if any top-level Master relation have a Detail relation.
  end;

implementation

uses
  epimiscutils, laz2_XMLRead, laz2_XMLWrite;

{ TEpiDocument }

function TEpiDocument.GetOnPassword: TRequestPasswordEvent;
begin
  result := Admin.OnPassword;
end;

procedure TEpiDocument.SetOnPassword(const AValue: TRequestPasswordEvent);
begin
  Admin.OnPassword := AValue;
end;

procedure TEpiDocument.SetPassWord(AValue: string);
var
  Val: String;
begin
  if FPassWord = AValue then Exit;
  Val := FPassWord;
  FPassWord := AValue;
  DoChange(eegDocument, Word(edcePassword), @Val);
end;

procedure TEpiDocument.SetModified(const AValue: Boolean);
begin
  inherited SetModified(AValue);
end;

function TEpiDocument.SaveAttributesToXml: string;
begin
  Result :=
    inherited SaveAttributesToXml +
    SaveAttr('xmlns', 'http://www.epidata.dk/XML/1.3') +
    SaveAttr('xmlns:xsi', 'http://www.w3.org/2001/XMLSchema-instance') +
    SaveAttr('xsi:schemaLocation', 'http://www.epidata.dk/XML/1.3 http://www.epidata.dk/XML/1.3/epx.xsd') +
    SaveAttr(rsVersionAttr, Version) +
    SaveAttr('xml:lang', DefaultLang);

  // Version 2 Properties:
  if PassWord <> '' then
    Result += SaveAttr(rsPassword, StrToSHA1Base64(PassWord));
  Result += SaveAttr(rsCycle, CycleNo)
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
  FRelations       := TEpiRelationListEx.Create(Self);
  FRelations.ItemOwner := true;
  FCycleNo         := 0;

  RegisterClasses([XMLSettings, ProjectSettings, {Admin,} Study, ValueLabelSets, DataFiles, Relations]);

  SetLanguage(LangCode, true);
  // Needed to reset initial XMLSettings.
  Modified := false;
end;

destructor TEpiDocument.Destroy;
begin
  FXMLSettings.Free;
  FProjectSettings.Free;
  FStudy.Free;
  FRelations.Free;
  FAdmin.Free;
  FDataFiles.Free;
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
  St := nil;
  try
    St := TFileStream.Create(AFileName, fmOpenRead);
    LoadFromStream(St);
  finally
    St.Free;
  end;
end;

procedure TEpiDocument.LoadFromStream(const St: TStream);
var
  RecXml: TXMLDocument;
  RootNode: TDOMElement;
  P: TDOMParser;
  Xin: TXMLInputSource;
  ReferenceMap: TEpiReferenceMap;
begin
  RecXml := nil;
  try
    ReadXMLFile(RecXml, St, [xrfPreserveWhiteSpace]);

    // **********************
    // Global <EpiData> structure
    // **********************
    RootNode := RecXml.DocumentElement;

    ReferenceMap := TEpiReferenceMap.Create;
    LoadFromXml(RootNode, ReferenceMap);
    ReferenceMap.FixupReferences;
  finally
    ReferenceMap.Free;
    RecXml.Free;
  end;
end;

procedure TEpiDocument.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
var
  Node: TDOMNode;
  PW, Login, UserPW: String;
  TmpVersion: EpiInteger;
  TmpBranch: EpiString;

begin
  // Root = <EpiData>
  FLoading := true;

  {$IFNDEF RELEASE}
  // Keep an eye in which branch we are loading from!
  TmpBranch := LoadAttrString(Root, rsBranchAttr, '', false);
  if (TmpBranch <> '') and
     (TmpBranch <> EPI_XML_BRANCH_STRING)
  then
    begin
      Raise EEpiBadVersion.CreateFmt(
        'Project has been created in another development branch!' + LineEnding +
        'Loading may not be possible - change branch name at own risk!' + LineEnding +
        'Program branch: %s' + LineEnding +
        'Project file branch: %s',
        [EPI_XML_BRANCH_STRING, TmpBranch]
      );
    end;
  {$ENDIF}

  // First read version no!
  TmpVersion := LoadAttrInt(Root, rsVersionAttr);
  if TmpVersion > EPI_XML_DATAFILE_VERSION then
    Raise EEpiBadVersion.CreateFmt(
      'Project has incorrect XML version!' + LineEnding +
      'Max supported XML Version: %d' + LineEnding +
      'Project XML Version: %d',
      [EPI_XML_DATAFILE_VERSION, TmpVersion]
      );
  FVersion := TmpVersion;

  // Then language!
  SetLanguage(LoadAttrString(Root, 'xml:lang'), true);
  // And last - file settings.
  LoadNode(Node, Root, rsSettings, true);
  XMLSettings.LoadFromXml(Node, ReferenceMap);

  // XML Version 2:
  if Version >= 2 then
  begin
    PW := LoadAttrString(Root, rsPassword, '', false);

    if (PW <> '') and (Assigned(OnPassword)) then
      OnPassword(Self, Login, UserPW);

    if (PW <> '') and (StrToSHA1Base64(UserPW) <> PW) then
      Raise EEpiBadPassword.Create('Incorrect Password');

    PassWord := UserPW;
    FCycleNo := LoadAttrInt(Root, rsCycle, CycleNo, false);
  end;

  LoadNode(Node, Root, rsStudy, true);
  Study.LoadFromXml(Node, ReferenceMap);

  // TODO : Include in later versions.
//  LoadNode(Node, Root, rsAdmin, true);
//  Admin.LoadFromXml(Node);

  if LoadNode(Node, Root, rsProjectSettings, false) then
    ProjectSettings.LoadFromXml(Node, ReferenceMap);

  if LoadNode(Node, Root, rsValueLabelSets, false) then
    ValueLabelSets.LoadFromXml(Node, ReferenceMap);

  if LoadNode(Node, Root, rsDataFiles, false) then
    DataFiles.LoadFromXml(Node, ReferenceMap);

  if Version <= 2 then
    // Version 2 only supported 1 DataFile and no relations,
    // hence this must be created to have a correct data container.
    Relations.NewMasterRelation.Datafile := DataFiles[0]
  else
    if LoadNode(Node, Root, rsRelations, false) then
      Relations.LoadFromXml(Node, ReferenceMap);

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
  FDoc: TXMLDocument;
begin
  {$IFDEF EPI_SAVE_STRING}
  S := SaveToXml(0);
  St.Write(S[1], Length(S));
  {$ELSE}
  FDoc := SaveToXmlDocument;
  WriteXMLFile(FDoc, St);
  FDoc.Free;
  {$ENDIF}
end;

procedure TEpiDocument.SaveToFile(const AFileName: string);
var
  Fs: TFileStream;
begin
  Fs := TFileStream.Create(AFileName, fmCreate);
  SaveToStream(Fs);
  Fs.Free;
end;

procedure TEpiDocument.IncCycleNo;
begin
  Inc(FCycleNo);
end;

function TEpiDocument.DoCloneCreate(AOwner: TEpiCustomBase): TEpiCustomBase;
begin
  Result := TEpiDocument.Create(Self.DefaultLang);
end;

function TEpiDocument.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
  ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);
  with TEpiDocument(Result) do
  begin
    FPassWord := Self.FPassWord;
    FCycleNo  := Self.FCycleNo;
  end;
end;

function TEpiDocument.SaveToXmlDocument: TXMLDocument;
begin
  result := TXMLDocument.Create;
  result.AppendChild(SaveToDom(Result));
end;

function TEpiDocument.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);

  SaveDomAttr(Result, 'xmlns', 'http://www.epidata.dk/XML/1.3');
  SaveDomAttr(Result, 'xmlns:xsi', 'http://www.w3.org/2001/XMLSchema-instance');
  SaveDomAttr(Result, 'xsi:schemaLocation', 'http://www.epidata.dk/XML/1.3 http://www.epidata.dk/XML/1.3/epx.xsd');
  SaveDomAttr(Result, rsVersionAttr, Version);
  SaveDomAttr(Result, 'xml:lang', DefaultLang);

  {$IFNDEF RELEASE}
  SaveDomAttr(Result, rsBranchAttr, EPI_XML_BRANCH_STRING);
  {$ENDIF}

  // Version 2 Properties:
  if PassWord <> '' then
    SaveDomAttr(Result, rsPassword, StrToSHA1Base64(PassWord));

  SaveDomAttr(Result, rsCycle, CycleNo);
end;

{ TEpiRelationListEx }

function TEpiRelationListEx.GetOrderedDataFiles: TEpiDataFiles;

  procedure BuildOrderedDataFiles(ARelation: TEpiMasterRelation);
  var
    i: integer;
  begin
    Result.AddItem(ARelation.Datafile);

    for i := 0 to ARelation.DetailRelations.Count - 1 do
      BuildOrderedDataFiles(ARelation.DetailRelation[i]);
  end;

var
  i: Integer;
begin
  Result := TEpiDataFiles.Create(nil);
  Result.ItemOwner := false;

  for i := 0 to Count - 1 do
    BuildOrderedDataFiles(MasterRelation[i]);
end;

procedure TEpiRelationListEx.OrderedWalk(
  const CallBackMethod: TEpiRelationListExCallBack; Data: Pointer);

var
  Depth: Cardinal;
  aContinue: Boolean;
  i: Integer;

  procedure RecurseMasterRelations(ARelation: TEpiMasterRelation; Idx: Cardinal);
  var
    i: Integer;
  begin
    CallBackMethod(ARelation, Depth, Idx, aContinue, Data);
    if not aContinue then exit;

    Inc(Depth);
    for i := 0 to ARelation.DetailRelations.Count - 1 do
    begin
      RecurseMasterRelations(ARelation.DetailRelations[i], i);
      if not aContinue then exit;
    end;
    Dec(Depth);

  end;

begin
  Depth := 0;
  aContinue := true;

  for i := 0 to Count - 1 do
    RecurseMasterRelations(MasterRelation[i], i);
end;

function TEpiRelationListEx.IsMultiLeveled: boolean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if MasterRelation[i].DetailRelations.Count > 0 then
      Exit(true);

  Result := false;
end;

end.
