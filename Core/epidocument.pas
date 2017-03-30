unit epidocument;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, Laz2_DOM,
  episettings, epiadmin, epidatafiles,
  epistudy, epidatafilerelations, epivaluelabels,
  epicustombase, epidatafilestypes, epilogger;

type

  TEpiDocumentChangeEvent = (
    // Simple document password has changed.
    edcePassword,

    // Save events
    edce
  );

  TEpiProgressType =
    (
      eptInit,
      eptFinish,
      eptRecords
    );

  TEpiDocumentFlag = (
    edfLoginFailed                    // Set during load if at least one failed login attempt was detected.
  );
  TEpiDocumentFlags = set of TEpiDocumentFlag;

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
    FPassWord: string;
    FProjectSettings: TEpiProjectSettings;
    FValueLabelSets: TEpiValueLabelSets;
    FVersion: integer;
    FXMLSettings: TEpiXMLSettings;
    FStudy: TEpiStudy;
    FDataFiles: TEpiDataFiles;
    FRelations: TEpiDatafileRelationList;
    FLogger: TEpiLogger;
    FFailedLog: TEpiFailedLogger;
    function   GetOnPassword: TRequestPasswordEvent;
    procedure  SetOnPassword(const AValue: TRequestPasswordEvent);
    procedure  SetPassWord(AValue: string);

  // SecurityLog (XML v5)
  private
    procedure InitializeSecurityLog;
    procedure DeInitializeSecurityLog;

  protected
    procedure DoChange(const Initiator: TEpiCustomBase;
      EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer); override;
      overload;
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
    Property   Logger: TEpiLogger read FLogger;
    property   OnPassword: TRequestPasswordEvent read GetOnPassword write SetOnPassword;
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

  { Flags }
  private
    FFlags: TEpiDocumentFlags;
  protected
    property   Flags: TEpiDocumentFlags read FFlags;

  { Save }
  public
    function   SaveToXmlDocument: TXMLDocument;
  protected
    function   SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
  end;

implementation

uses
  epimiscutils, laz2_XMLRead, laz2_XMLWrite, epiglobals,
  DCPrijndael, DCPsha256, DCPbase64, episecuritylog;

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

procedure TEpiDocument.InitializeSecurityLog;
var
  DF: TEpiSecurityDatafile;
  VLSet: TEpiValueLabelSet;
  MR: TEpiMasterRelation;
  DataDF: TEpiSecurityDataEventLog;
  DR: TEpiDetailRelation;
  KeyDF: TEpiSecurityKeyFieldLog;
begin
  VLSet := TEpiValueLabelSet(ValueLabelSets.NewItem(TEpiSecurityValuelabelSet));
  VLSet.Name := EpiSecurityLogValuelLabelSetName;

  DF := TEpiSecurityDatafile(DataFiles.NewItem(TEpiSecurityDatafile));
  DF.Name := EpiSecurityLogDatafileName;
  DF.Caption.Text := EpiSecurityLogDatafileCaption;
  DF.LogType.ValueLabelSet := VLSet;

  MR := TEpiMasterRelation(Relations.NewItem(TEpiSecurityDatafileRelation));
  MR.Datafile := DF;
  MR.Name     := EpiSecurityLogRelationName;

  DataDF := TEpiSecurityDataEventLog(DataFiles.NewItem(TEpiSecurityDataEventLog));
  DataDF.Name := EpiSecurityLogDataEventName;
  DataDF.Caption.Text := EpiSecurityLogDataEventCaption;

  DR := TEpiDetailRelation(MR.DetailRelations.NewItem(TEpiSecurityDatafileDetailRelation));
  DR.Datafile := DataDF;
  DR.Name     := EpiSecurityLogDataRelationName;
  DR.MaxRecordCount := 0;

  KeyDF := TEpiSecurityKeyFieldLog(DataFiles.NewItem(TEpiSecurityKeyFieldLog));
  KeyDF.Name := EpiSecurityLogKeyDataName;
  KeyDF.Caption.Text := EpiSecurityLogKeyDataCaption;

  DR := TEpiDetailRelation(MR.DetailRelations.NewItem(TEpiSecurityDatafileDetailRelation));
  DR.Datafile := KeyDF;
  DR.Name     := EpiSecurityLogKeyDataRelationName;
  DR.MaxRecordCount := 0;

  FLogger := TEpiLogger.Create(Self, DataFiles);
end;

procedure TEpiDocument.DeInitializeSecurityLog;
begin

end;

procedure TEpiDocument.DoChange(const Initiator: TEpiCustomBase;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin

  case EventGroup of
    eegAdmin:
      case TEpiAdminChangeEventType(EventType) of
        // Admin has been initialize, now time to setup Security Datafile and Valuelabels.
        // Admin initialization is either done explicitly (Admin.InitAdmin) or right after
        // the preloading of users (in Admin.LoadCrypto)
        eaceAdminInitializing:
          InitializeSecurityLog;

        // Catch invalid login/password before passing them on in order to set
        // internal flag correctly.
        eaceAdminIncorrectUserName,
        eaceAdminIncorrectPassword:
          Include(FFlags, edfLoginFailed);
      end;
  end;

  inherited DoChange(Initiator, EventGroup, EventType, Data);
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
  FFailedLog       := TEpiFailedLogger.Create(Self);
  FCycleNo         := 0;
  FFlags           := [];

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
  FLogger.Free;
  inherited Destroy;
end;

function TEpiDocument.XMLName: string;
begin
  Result := rsEpiData;
end;

procedure TEpiDocument.LoadFromFile(const AFileName: string);
var
  St: TMemoryStream;
begin
  St := nil;
  try
    St := TMemoryStream.Create;
    St.LoadFromFile(AFileName);
    ST.Position := 0;
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

    if (Admin.Initialized) then
      DoChange(eegCustomBase, Word(ecceRequestSave), nil);
  finally
    ReferenceMap.Free;
    RecXml.Free;
  end;
end;

procedure TEpiDocument.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
var
  Node: TDOMNode;
  PW, Login, UserPW, S: String;
  TmpVersion: EpiInteger;
  TmpBranch: EpiString;
  DeCrypter: TDCP_rijndael;
  SS: TStringStream;
  MS: TMemoryStream;
  Res: TEpiRequestPasswordResponse;
  Count: Integer;
  Elem: TDOMElement;
  CRes: TEpiRequestPasswordResult;
begin
  inherited LoadFromXml(Root, ReferenceMap);

  // Root = <EpiData>
  FLoading := true;

//not yet...  DoChange(eegXMLProgress, Word(expeInit), nil);
  try

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

    // Now we need the separators, in order to load dates/times correctly.
    if (Version >= 4) then
    begin
      XMLSettings.DateSeparator    := LoadAttrString(Root, rsDateSep)[1];
      XMLSettings.TimeSeparator    := LoadAttrString(Root, rsTimeSep)[1];
      XMLSettings.DecimalSeparator := LoadAttrString(Root, rsDecSep)[1];
    end;

    // Then language!
    SetLanguage(LoadAttrString(Root, 'xml:lang'), true);

    // Third is cycle no:
    if (Version >= 2) then
      FCycleNo := LoadAttrInt(Root, rsCycle, CycleNo, false);

    // Version 4:
    //  - load External log
    if LoadNode(Node, Root, 'ExLog', false) then
    begin
      FFailedLog.LoadFromXml(Node, ReferenceMap);
      if FFailedLog.TooManyFailedLogins(EpiAdminLoginAttemps, EpiAdminLoginInterval) then
        raise EEpiTooManyFailedLogins.Create(rsTooManyFailedAttemps);
    end;

    // Version 4:
    //  - Now check for User login;
    if (Version >= 4) and
       (LoadNode(Node, Root, rsCrypto, false))
    then
      try
        SS := nil;
        MS := nil;
        DeCrypter := nil;

        // Preloading the basic user information also sends a
        // request for password from user.
        // Loading the rest of the user information (Name, etc.) is
        // done later.
        try
          CRes := Admin.LoadCrypto(Node, ReferenceMap, FFailedLog);
        finally
          if (edfLoginFailed in Flags) then
          begin
            Elem := FFailedLog.SaveToDom(Root.OwnerDocument);
            LoadNode(Node, Root, 'ExLog', false);
            Root.ReplaceChild(Elem, Node);

            DoChange(eegCustomBase, Word(ecceRequestSave), Root.OwnerDocument);
            Exclude(FFlags, edfLoginFailed);
          end;
        end;

        case CRes of
          prSuccess:  ;
          prFailed:   raise EEpiBadPassword.Create('Incorrect Username/Password');
          prCanceled: raise EEpiPasswordCanceled.Create('Login Canceled');
        end;

        {$IFNDEF EPI_ADMIN_NOCRYPT_LOAD}
        LoadNode(Node, Root, rsEncrypted, true);

        SS := TStringStream.Create(Base64DecodeStr(Node.TextContent));
        MS := TMemoryStream.Create;

        DeCrypter := TDCP_rijndael.Create(nil);
        DeCrypter.InitStr(Admin.MasterPassword, TDCP_sha256);
        DeCrypter.DecryptStream(SS, MS, SS.Size);

        MS.Position := 0;
        ReadXMLFragment(Root, MS, [xrfPreserveWhiteSpace]);
        {$ENDIF}
      finally
        SS.Free;
        MS.Free;
        DeCrypter.Free;
      end;

    // XML Version 2:
    if (Version >= 2) then
    begin
      if (Version <= 3) then
        PW := LoadAttrString(Root, rsCapitalPassword, '', false)
      else
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
    end;

    // Version 1-3:
    //  - from v4 separators are moved to <EpiData> tag
    if (Version <= 3) then
    begin
      LoadNode(Node, Root, rsSettings, true);
      XMLSettings.LoadFromXml(Node, ReferenceMap);
    end;

    // Version 4:
    if LoadNode(Node, Root, rsAdmin, false) then
      Admin.LoadFromXml(Node, ReferenceMap);

    // Version 1:
    LoadNode(Node, Root, rsStudy, true);
    Study.LoadFromXml(Node, ReferenceMap);

    // Version 1:
    if LoadNode(Node, Root, rsProjectSettings, false) then
      ProjectSettings.LoadFromXml(Node, ReferenceMap);

    // Version 1:
    if LoadNode(Node, Root, rsValueLabelSets, false) then
      ValueLabelSets.LoadFromXml(Node, ReferenceMap);

    // Version 1:
    if LoadNode(Node, Root, rsDataFiles, false) then
      DataFiles.LoadFromXml(Node, ReferenceMap);

    if (Version <= 2) then
      begin
        // Version 2 only supported 1 DataFile and no relations,
        // hence this must be created to have a correct data container.
        if (DataFiles.Count > 0) then
          Relations.NewMasterRelation.Datafile := DataFiles[0]
      end
    else
      // Version 3+:
      begin
        if (Version = 3) then
          S := rsRelations
        else  // Name change for Version 4+
          S := rsDataFileRelations;

        if LoadNode(Node, Root, S, (DataFiles.Count > 0)) then
          Relations.LoadFromXml(Node, ReferenceMap);
      end;

    if Assigned(Logger) then
    begin

      // Version 4 (only):
      if LoadNode(Node, Root, 'Log', false) then
        FLogger.LoadFromXml(Node, ReferenceMap);

      Logger.LoadExLog(FFailedLog);
    end;

    FLoading := false;
    Modified := false;
    FVersion := EPI_XML_DATAFILE_VERSION;
  finally
//not yet....    DoChange(eegXMLProgress, Word(expeDone), nil);
  end
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

  {$IFNDEF EPI_ADMIN_NOCRYPT_SAVE}
  if (Admin.Users.Count > 0) then
  begin
    RootDoc := Result.FirstChild;

    MSIn  := TMemoryStream.Create;
    MSOut := TMemoryStream.Create;

    for Elem in RootDoc do
    begin
      if (Elem.NodeName = rsCrypto) then continue;
      WriteXML(Elem, MSIn);
    end;
    MSIn.Position := 0;

    Node := RootDoc.FirstChild;
    while Assigned(Node) do
    begin
      TmpNode := Node;
      Node := Node.NextSibling;

      if (TmpNode.NodeName = rsCrypto) then continue;
      RootDoc.RemoveChild(TmpNode).Free;
    end;

    try
      EnCrypter := TDCP_rijndael.Create(nil);
      EnCrypter.InitStr(Admin.MasterPassword, TDCP_sha256);
      EnCrypter.EncryptStream(MSIn, MSOut, MSIn.Size);

      SetLength(S, (4 * MSOut.Size) div 3 + 10);
      L := Base64Encode(MSOut.Memory, @S[1], MSOut.Size);
      SetLength(S, L);

      CryptElem := Result.CreateElement(rsEncrypted);
      CryptElem.AppendChild(Result.CreateTextNode(S));
      RootDoc.AppendChild(CryptElem);
    finally
      S := '';
      MSIn.Free;
      MSOut.Free;
      EnCrypter.Free;
    end;
  end;
  {$ENDIF}
end;

function TEpiDocument.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);

  SaveDomAttr(Result, 'xmlns', 'http://www.epidata.dk/XML/2.1');
  SaveDomAttr(Result, 'xmlns:xsi', 'http://www.w3.org/2001/XMLSchema-instance');
  SaveDomAttr(Result, 'xsi:schemaLocation', 'http://www.epidata.dk/XML/2.1 http://www.epidata.dk/XML/2.1/epx.xsd');
  SaveDomAttr(Result, rsVersionAttr, Version);
  SaveDomAttr(Result, 'xml:lang', DefaultLang);
  SaveDomAttr(Result, rsDateSep, XMLSettings.DateSeparator);
  SaveDomAttr(Result, rsDecSep,  XMLSettings.DecimalSeparator);
  SaveDomAttr(Result, rsTimeSep, XMLSettings.TimeSeparator);

  {$IFNDEF RELEASE}
  SaveDomAttr(Result, rsBranchAttr, EPI_XML_BRANCH_STRING);
  {$ENDIF}

  // Version 2 Properties:
  if PassWord <> '' then
    SaveDomAttr(Result, rsPassword, StrToSHA1Base64(PassWord));

  SaveDomAttr(Result, rsCycle, CycleNo);

  // Version 4:
  if Admin.Users.Count > 0 then
    Result.InsertBefore(Admin.SaveCrypto(RootDoc), Result.FirstChild);
end;

end.
