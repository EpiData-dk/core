unit epidocument;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, Laz2_DOM,
  episettings, epiadmin, epidatafiles,
  epistudy, epidatafilerelations, epivaluelabels,
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
    FRelations: TEpiDatafileRelationList;
    function   GetOnPassword: TRequestPasswordEvent;
    procedure  SetOnPassword(const AValue: TRequestPasswordEvent);
    procedure  SetPassWord(AValue: string);
  protected
    procedure  SetModified(const AValue: Boolean); override;
  public
    constructor Create(Const LangCode: string);
    destructor Destroy; override;
    function   XMLName: string; override;
    procedure  LoadFromFile(const AFileName: string);
    procedure  LoadFromStream(const St: TStream);
    procedure  LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    procedure  SaveToStream(Const St: TStream);
    procedure  SaveToFile(Const AFileName: string);
    Property   XMLSettings: TEpiXMLSettings read FXMLSettings;
    property   ProjectSettings: TEpiProjectSettings read FProjectSettings;
    Property   Admin: TEpiAdmin read FAdmin;
    Property   Study: TEpiStudy read FStudy;
    Property   ValueLabelSets: TEpiValueLabelSets read FValueLabelSets;
    Property   DataFiles: TEpiDataFiles read FDataFiles;
    Property   Relations: TEpiDatafileRelationList read FRelations;
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

implementation

uses
  epimiscutils, laz2_XMLRead, laz2_XMLWrite,
  DCPrijndael, DCPsha256, DCPbase64;

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

constructor TEpiDocument.Create(const LangCode: string);
begin
  inherited Create(nil);
  FVersion         := EPI_XML_DATAFILE_VERSION;
  FAdmin           := TEpiAdmin.Create(Self);
  FXMLSettings     := TEpiXMLSettings.Create(Self);
  FProjectSettings := TEpiProjectSettings.Create(Self);
  FStudy           := TEpiStudy.Create(Self);
  FValueLabelSets  := TEpiValueLabelSets.Create(Self);
  FValueLabelSets.ItemOwner := true;
  FDataFiles       := TEpiDataFiles.Create(Self);
  FDataFiles.ItemOwner := true;
  FRelations       := TEpiDatafileRelationList.Create(Self);
  FRelations.ItemOwner := true;
  FCycleNo         := 0;

  RegisterClasses([Admin, XMLSettings, ProjectSettings, Study, ValueLabelSets, DataFiles, Relations]);

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
  DeCrypter: TDCP_rijndael;
  SS: TStringStream;
  MS: TMemoryStream;
  Res: TEpiRequestPasswordResponse;
  Count: Integer;

begin
  inherited LoadFromXml(Root, ReferenceMap);

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

  // Version 4:
  // Now check for User login;
  if (Version >= 4) and
     (LoadNode(Node, Root, rsUsers, false))
  then
    try
      SS := nil;
      MS := nil;
      DeCrypter := nil;

      // Preloading the basic user information also sends a
      // request for password from user.
      // Loading the rest of the user information (Name, etc.) is
      // done later.
      case Admin.Users.PreLoadFromXml(Node) of
        prSuccess:
          ;

        prFailed:
          raise EEpiBadPassword.Create('Incorrect Username/Password');

        prCanceled:
          raise EEpiPasswordCanceled.Create('');
      end;

    {     LoadNode(Node, Root, 'Crypt', true);

      SS := TStringStream.Create(Base64DecodeStr(Node.TextContent));
      MS := TMemoryStream.Create;

      DeCrypter := TDCP_rijndael.Create(nil);
      DeCrypter.InitStr(Admin.MasterPassword, TDCP_sha256);
      DeCrypter.DecryptStream(SS, MS, SS.Size);

      MS.Position := 0;
      ReadXMLFragment(Root, MS, [xrfPreserveWhiteSpace]);  }
    finally
      SS.Free;
      MS.Free;
      DeCrypter.Free;
    end;

  // XML Version 2:
  if (Version >= 2) then
  begin
    PW := LoadAttrString(Root, rsPassword, '', false);

    Count := 1;
    if (PW <> '') and (Assigned(OnPassword)) then
    repeat
      Res := OnPassword(Self, erpSinglePassword, Count, Login, UserPW);
      Inc(Count);
    until (StrToSHA1Base64(UserPW) = PW) or
          (Res in [rprStopOnFail, rprCanceled]);

    if (Res = rprCanceled) then
      Raise EEpiPasswordCanceled.Create('');

    if (PW <> '') and (StrToSHA1Base64(UserPW) <> PW) then
      Raise EEpiBadPassword.Create('Incorrect Password');

    PassWord := UserPW;
    FCycleNo := LoadAttrInt(Root, rsCycle, CycleNo, false);
  end;

  // Version 1:
  // And last - file settings.
  LoadNode(Node, Root, rsSettings, true);
  XMLSettings.LoadFromXml(Node, ReferenceMap);

  LoadNode(Node, Root, rsStudy, true);
  Study.LoadFromXml(Node, ReferenceMap);

  if LoadNode(Node, Root, rsProjectSettings, false) then
    ProjectSettings.LoadFromXml(Node, ReferenceMap);

  if LoadNode(Node, Root, rsValueLabelSets, false) then
    ValueLabelSets.LoadFromXml(Node, ReferenceMap);

  if LoadNode(Node, Root, rsDataFiles, false) then
    DataFiles.LoadFromXml(Node, ReferenceMap);

  if (Version <= 2) then
  begin
    // Version 2 only supported 1 DataFile and no relations,
    // hence this must be created to have a correct data container.
    if (DataFiles.Count > 0) then
      Relations.NewMasterRelation.Datafile := DataFiles[0]
  end else
  // Version 3:
    if LoadNode(Node, Root, rsRelations, (DataFiles.Count > 0)) then
      Relations.LoadFromXml(Node, ReferenceMap);

  // Version 4:
  if LoadNode(Node, Root, rsAdmin, false) then
    Admin.LoadFromXml(Node, ReferenceMap);

  FLoading := false;
  Modified := false;
  FVersion := EPI_XML_DATAFILE_VERSION;
end;

procedure TEpiDocument.SaveToStream(const St: TStream);
var
  FDoc: TXMLDocument;
begin
  FDoc := SaveToXmlDocument;
  WriteXMLFile(FDoc, St);
  FDoc.Free;
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
var
  Elem: TDOMNode;
  RootDoc: TDOMNode;
  EnCrypter: TDCP_rijndael;
  MSIn: TMemoryStream;
  MSOut: TMemoryStream;
  Node: TDOMNode;
  TmpNode: TDOMNode;
  S: String;
  CryptElem: TDOMElement;
  L: LongInt;
begin
  result := TXMLDocument.Create;
  result.AppendChild(SaveToDom(Result));

  if false{ (Admin.Users.Count > 0) }then
  begin
    RootDoc := Result.FirstChild;

    MSIn  := TMemoryStream.Create;
    MSOut := TMemoryStream.Create;

    for Elem in RootDoc do
    begin
      if Elem.NodeName = Admin.Users.XMLName then continue;
      WriteXML(Elem, MSIn);
    end;
    MSIn.Position := 0;

    Node := RootDoc.FirstChild;
    while Assigned(Node) do
    begin
      TmpNode := Node;
      Node := Node.NextSibling;

      if TmpNode.NodeName = Admin.Users.XMLName then continue;
      RootDoc.RemoveChild(TmpNode).Free;
    end;

    try
      EnCrypter := TDCP_rijndael.Create(nil);
      EnCrypter.InitStr(Admin.MasterPassword, TDCP_sha256);
      EnCrypter.EncryptStream(MSIn, MSOut, MSIn.Size);

      SetLength(S, (4 * MSOut.Size) div 3 + 10);
      L := Base64Encode(MSOut.Memory, @S[1], MSOut.Size);
      SetLength(S, L);

      CryptElem := Result.CreateElement('Crypt');
      CryptElem.AppendChild(Result.CreateTextNode(S));
      RootDoc.AppendChild(CryptElem);
    finally
      S := '';
      MSIn.Free;
      MSOut.Free;
      EnCrypter.Free;
    end;
  end;
end;

function TEpiDocument.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);

  SaveDomAttr(Result, 'xmlns', 'http://www.epidata.dk/XML/2.0');
  SaveDomAttr(Result, 'xmlns:xsi', 'http://www.w3.org/2001/XMLSchema-instance');
  SaveDomAttr(Result, 'xsi:schemaLocation', 'http://www.epidata.dk/XML/2.0 http://www.epidata.dk/XML/2.0/epx.xsd');
  SaveDomAttr(Result, rsVersionAttr, Version);
  SaveDomAttr(Result, 'xml:lang', DefaultLang);

  {$IFNDEF RELEASE}
  SaveDomAttr(Result, rsBranchAttr, EPI_XML_BRANCH_STRING);
  {$ENDIF}

  // Version 2 Properties:
  if PassWord <> '' then
    SaveDomAttr(Result, rsPassword, StrToSHA1Base64(PassWord));

  SaveDomAttr(Result, rsCycle, CycleNo);


  // Version 4:
  if Admin.Users.Count > 0 then
    Admin.Users.PreSaveToDom(RootDoc, Result);
end;

end.
