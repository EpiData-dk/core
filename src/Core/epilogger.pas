unit epilogger;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidatafiles, epidatafilestypes, epicustombase, epitools_search,
  Laz2_DOM, DCPrijndael, episecuritylog, epidatafilerelations;

resourcestring
  rsTooManyFailedAttemps = 'Too many failed login attemps';

type
  TEpiLogEntry = (
    ltNone,              //  0 - The "empty" entry in the EnumField
    ltSuccessLogin,      //  1 - A succesfull login
    ltFailedLogin,       //  2 - An unsuccessfull login
    ltSearch,            //  3 - Search performed
    ltNewRecord,         //  4 - New Record added
    ltEditRecord,        //  5 - Edited an existing record
    ltViewRecord,        //  6 - Changed record no. in viewer
    ltPack,              //  7 - Packed datafiles
    ltAppend,            //  8 - Appended data to datafiles
    ltExport,            //  9 - Exported data (or part of) to uncontrolled file.
    ltClose,             // 10 - The document was closed
    ltNewPassword,       // 11 - The user password was changed/reset
    ltBlockedLogin,      // 12 - The access to the project was blocked
    ltExpiredLogin,      // 13 - The user account used for login was expired
    ltExpiredChange,     // 14 - The user accound setting for expired was changed
    ltSecurityLogExport, // 15 - The security logs were exported to another file
    ltRecordStatus       // 16 - The status of the observation changed
  );

const
 // EpiLogEntryDataFileSet = [ltSearch, ltNewRecord, ltEditRecord, ltViewRecord, ltPack, ltAppend];

  EpiLogEntryText: array[TEpiLogEntry] of string =
    (
      'None',
      'Successfull Login',
      'Failed Login',
      'Search',
      'New Observation',
      'Edit Observation',
      'View Observation',
      'Pack',
      'Append Data',
      'Export Data',
      'Closed Project',
      'New Password',
      'Blocked Login',
      'Account Expired',
      'Account Expire Changed',
      'Security Log Exported',
      'Observation Status Changed'
    );

type

  TEpiFailedLogger = class;

  { TEpiLogger }

  TEpiLogger = class(TEpiCustomBase)
  { Data Logging  }
  private type
    TCommitState = (csNone, csNewRecord, csEditRecord);
  private
    FCommitState: TCommitState;
    procedure StoreDataEvent(Field: TEpiField; Data: PEpiFieldDataEventRecord);

  private
    FSecurityLog: TEpiSecurityDatafile;
    FDataLog:     TEpiSecurityDataEventLog;
    FKeyLog:      TEpiSecurityKeyFieldLog;
    procedure DocumentHook(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
  public
    constructor Create(AOwner: TEpiCustomBase; DataFiles: TEpiDataFiles); virtual;
    property    SecurityLog: TEpiSecurityDatafile read FSecurityLog;
    property    DataLog: TEpiSecurityDataEventLog read FDataLog;
    property    KeyLog: TEpiSecurityKeyFieldLog read FKeyLog;

  { Misc. }
  private
    function    LogEntryFromNodeName(Const NodeName: DOMString): TEpiLogEntry;

  { Save/Load }
  private
    function    CreateCommonNode(RootDoc: TDOMDocument; LogIndex: Integer): TDOMElement;
    procedure   AddKeyFieldValues(RootNode: TDOMElement; LogIndex: Integer);
    procedure   AddEditFieldValues(RootNode: TDOMElement; LogIndex: Integer);
    procedure   GetEditFieldValues(RootNode: TDOMNode; LogIndex: Integer);
    procedure   AddSearchString(RootNode: TDOMElement; LogIndex: Integer);
    procedure   CreateExportNode(RootNode: TDOMElement; LogIndex: Integer);
    procedure   ReadExportNode(RootNode: TDOMNode; LogIndex: Integer);
  public
    function    XMLName: string; override;
    function    SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
    procedure   LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    procedure   LoadExLog(ExLogObject: TEpiFailedLogger);

  { Logging properties }
  private
    FSuccessLoginTime: TDateTime;
    FUserName: UTF8String;
    FDatafile: TEpiDataFile;
    FLogEvents: boolean;
    FFilename: UTF8string;
    procedure  SetUserName(AValue: UTF8String);
    procedure  SetDatafile(AValue: TEpiDataFile);
    function   DoNewLog(ALogType: TEpiLogEntry): Integer;  // Result = Index for new record.
    procedure  LogKeyValues(ParentId, RecordNo: integer);
  public
    // Datafile = the datafile the user is currently accessing
    property   Datafile: TEpiDataFile read FDatafile write SetDatafile;
    // Name of the user creating a log entry
    property   UserName: UTF8String read FUserName write SetUserName;
    // Property for activating logging
    property   LogEvents: boolean read FLogEvents write FLogEvents;
    // Should be set by the opening TEpiDocumentFile
    property    Filename: UTF8string read FFilename write FFilename;
  { Logging methods }
  private
    procedure  LogLoginSuccess();
    procedure  LogRecordNew();
    procedure  LogRecordEdit(RecordNo: Integer);
    procedure  LogRecordView(RecordNo: Integer);
    procedure  LogRecordStatus(Status: PEpiDataFileStatusRecord);
    procedure  LogPack();
    procedure  LogPassword();
    procedure  LogExpiredChange(NewExpiredDate: TDateTime);
  public
    procedure  LogSearch(Search: TEpiSearch);
    procedure  LogAppend();
    procedure  LogClose();
    procedure  LogExport(Settings: TObject);
    // Version 6:
    procedure  LogExportSecurityLog(const ExportFilename: UTF8String; StartID, EndID: Integer);
  end;

  { TEpiFailedLogger }

  TEpiFailedLogger = class(TEpiCustomBase)
  private
    FLogDataFile: TEpiDataFile;
    FUserNames:      TEpiField;       // Username for the log entry
    FTime:           TEpiField;       // Time of log entry
    FCycle:          TEpiField;       // Cycly no fo the log entry
    FAesKey:         TEpiField;       // The aes key for the line
    FLoginFailType:  TEpiField;       // Numbers for type of failed login:
                                      //   0 = username ,
                                      //   1 = password,
                                      //   2 = read from XML (the actual content of what wrong need to be decoded)
                                      //   3 = blocked login
    FEncryptedTxt:   TEpiField;       // If read from XML, store the encrypted TXT here.
    FHostName:       TEpiField;       //  -   "    -     , store the hostname here.
    FFileNameField:  TEpiField;       //  -   "    -     , store the filename here.
  private
    FEncrypter: TDCP_rijndael;
    FFilename: UTF8string;
    procedure DocumentHook(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    function    XMLName: string; override;
    function    SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
    procedure   LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    // Should be set by the opening TEpiDocumentFile
    property    Filename: UTF8string read FFilename write FFilename;
  public
    // Returns true if the same host has many attempts over a given interval.
    //  @Attemps = number of tries from same host before fail
    //  @Interval = number of second from first to last attempt.
    function TooManyFailedLogins(Attempts, Interval: Integer): boolean;
  end;

implementation

uses
  typinfo, epidocument, epiadmin, strutils, DCPsha512, DCPbase64, epimiscutils,
  dateutils, math, epiexportsettings, epiglobals;

const
  FAILEDLOG_FAILTYPE_INCORRECT_PW    = 0;
  FAILEDLOG_FAILTYPE_INCORRECT_LOGIN = 1;
  FAILEDLOG_FAILTYPE_AESKEY          = 2;
  FAILEDLOG_FAILTYPE_BLOCKED_LOGIN   = 3;
  FAILEDLOG_FAILTYPE_EXPIRED_USER    = 4;

type

  { TLogExportDatafile }

  TLogExportDatafile = class
  public
    Name: String;
    FromRecord:     integer;
    ToRecord:       integer;
    ExportFields:   TStringList;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TLogExportDocument }

  TLogExportDocument = class
  public
    ExportType: String;
    ExportDeleted: Boolean;
    ExportList: TList;
  public
    constructor Create;
    destructor Destroy; override;
  end;

{ TLogExportDatafile }

constructor TLogExportDatafile.Create;
begin
  ExportFields := TStringList.Create;
end;

destructor TLogExportDatafile.Destroy;
begin
  ExportFields.Free;
  inherited Destroy;
end;


constructor TLogExportDocument.Create;
begin
  ExportList := TList.Create;
end;

destructor TLogExportDocument.Destroy;
var
  Item: Pointer;
begin
  for Item in ExportList do
    TObject(Item).Free;

  inherited Destroy;
end;


function Doc(AValue: TEpiCustomBase): TEpiDocument;
begin
  Result := TEpiDocument(AValue.RootOwner);
end;

type
  TDataLogEntry = record
    DataFileName:   UTF8String;
    FieldName:      EpiString;
    OldStringValue: EpiString;
    NewStringValue: EpiString;
    case FieldType: TEpiFieldType of      // The senders Fieldtype (also accessible through the change event, but convient)
      ftBoolean:                          // Old Value as per field type
        (OldBoolValue: EpiBool;
         NewBoolValue: EpiBool);
      ftInteger,
      ftAutoInc:
        (OldIntValue: EpiInteger;
         NewIntValue: EpiInteger);
      ftFloat:
        (OldFloatValue: EpiFloat;
         NewFloatValue: EpiFloat);
      ftDMYDate,
      ftMDYDate,
      ftYMDDate,
      ftDMYAuto,
      ftMDYAuto,
      ftYMDAuto:
        (OldDateValue: EpiDate;
         NewDateValue: EpiDate);
      ftTime,
      ftTimeAuto:
        (OldTimeValue: EpiTime;
         NewTimeValue: EpiTime);
  end;
  PDataLogEntry = ^TDataLogEntry;

{ TLogExportDocument }

procedure TEpiLogger.StoreDataEvent(Field: TEpiField;
  Data: PEpiFieldDataEventRecord);
var
  Idx: Integer;
begin
  with Data^ do
  begin
    Idx := FDataLog.NewRecords();

    FDataLog.ID.AsInteger[Idx]          := FSecurityLog.ID.AsInteger[FSecurityLog.Size - 1];
    FDataLog.VariableName.AsString[Idx] := Field.Name;

    case Data^.FieldType of
      ftBoolean:
        begin
          FDataLog.BeforeValue.AsBoolean[Idx] := BoolValue;
          FDataLog.AfterValue.AsBoolean[Idx]  := Field.AsBoolean[Data^.Index];
//          OldBoolValue := BoolValue;
//          NewBoolValue := Field.AsBoolean[Data^.Index];
        end;

      ftInteger,
      ftAutoInc:
        begin
          FDataLog.BeforeValue.AsInteger[Idx] := IntValue;
          FDataLog.AfterValue.AsInteger[Idx]  := Field.AsInteger[Data^.Index];
//          OldIntValue := IntValue;
//          NewIntValue := Field.AsInteger[Data^.Index];
        end;

      ftFloat:
        begin
          FDataLog.BeforeValue.AsFloat[Idx] := FloatValue;
          FDataLog.AfterValue.AsFloat[Idx]  := Field.AsFloat[Data^.Index];
//          OldFloatValue := FloatValue;
//          NewFloatValue := Field.AsFloat[Data^.Index];
        end;

      ftDMYDate,
      ftMDYDate,
      ftYMDDate,
      ftDMYAuto,
      ftMDYAuto,
      ftYMDAuto:
        begin
          FDataLog.BeforeValue.AsDate[Idx] := DateValue;
          FDataLog.AfterValue.AsDate[Idx]  := Field.AsDate[Data^.Index];
//          OldDateValue := DateValue;
//          NewDateValue := Field.AsDate[Data^.Index];
        end;

      ftTime,
      ftTimeAuto:
        begin
          FDataLog.BeforeValue.AsTime[Idx] := TimeValue;
          FDataLog.AfterValue.AsTime[Idx]  := Field.AsTime[Data^.Index];
//          OldTimeValue := TimeValue;
//          NewTimeValue := Field.AsTime[Data^.Index];
        end;

      ftString,
      ftUpperString:
        begin
          FDataLog.BeforeValue.AsString[Idx] := StringValue^;
          FDataLog.AfterValue.AsString[Idx]  := Field.AsString[Data^.Index];
//          OldStringValue := StringValue^;
//          NewStringValue := Field.AsString[Data^.Index];
        end;
    end;
  end;
//  FDataLog.Add(NewData);
end;

procedure TEpiLogger.DocumentHook(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
var
  CurrentDF: TEpiDataFile;
begin
  if (not LogEvents) then
    Exit;

  case EventGroup of
    eegAdmin:
      case TEpiAdminChangeEventType(EventType) of
        eaceAdminLoginSuccessfull:       // Data: TEpiUser = the authenticated user.
          begin
            UserName := TEpiUser(Data).Login;
            LogLoginSuccess();
          end;

        eaceUserSetPassword:             // Initiator = TEpiUser.
          begin
            UserName := TEpiUser(Initiator).Login;
            LogPassword();
          end;

        eaceUserSetExpireDate:           // Initiator = TEpiUser.
          begin
            UserName := TEpiUser(Initiator).Login;
            LogExpiredChange(TEpiUser(Initiator).ExpireDate);
          end;

      else
        {
        eaceAdminIncorrectUserName: ;
        eaceAdminIncorrectPassword: ;
        eaceAdminIncorrectNewPassword: ;
        eaceUserSetFullName: ;
        eaceUserSetExpireDate: ;
        eaceUserSetLastLogin: ;
        eaceUserSetNotes: ;
        eaceGroupSetManageRights: ;
        eaceAdminResetting: ;
        }
        Exit;
      end;


    eegDataFiles:
      begin
        // Do not log events in the internal datafiles
        if (Initiator = FSecurityLog) or
           (Initiator = FDataLog) or
           (Initiator = FKeyLog)
        then
          Exit;

        case TEpiDataFileChangeEventType(EventType) of
          edcePack:
            LogPack();

          edceBeginCommit:
            begin
              // 0 = new record
              if PtrInt(Data) = 0 then
                FCommitState := csNewRecord
              else
                // 1 = edit record
                begin
                  FCommitState := csEditRecord;
                  // Create a new record in the security log, we need the index
                  DoNewLog(ltEditRecord);
                end;
            end;

          edceEndCommit:
            begin
              case FCommitState of
                csNone: ;

                csNewRecord:
                  LogRecordNew();

                csEditRecord:
                  LogRecordEdit(PtrInt(Data));
              end;
              FCommitState := csNone;
            end;

          edceLoadRecord:
            LogRecordView(PtrInt(Data));

          edceRecordStatus:
            begin
              CurrentDF := FDatafile;
              Datafile := TEpiDataFile(Initiator);
              if (not TEpiDocument(Sender).Loading) then
                LogRecordStatus(PEpiDataFileStatusRecord(Data));
              Datafile := CurrentDF;
            end;

        else
          {
          edceSize: ;
          edceStatusbarContentString: ;
          }
          Exit;
        end;
      end;

    eegFields:
      case TEpiFieldsChangeEventType(EventType) of
        efceData:
          begin
            if (FCommitState <> csEditRecord) then Exit;
            if (TEpiField(Initiator).DataFile <> FDatafile) then exit;
            StoreDataEvent(TEpiField(Initiator), Data);
          end;

      else
        {
        efceSetDecimal: ;
        efceSetLeft: ;
        efceSetLength: ;
        efceSetTop: ;
        efceSetSize: ;
        efceResetData: ;
        efceEntryMode: ;
        efceConfirmEntry: ;
        efceShowValueLabel: ;
        efceShowValueLabelNotes: ;
        efceRepeatValue: ;
        efceDefaultValue: ;
        efceValueLabelWriteTo: ;
        efceForcePickList: ;
        efceValueLabelSet: ;
        efceZeroFilled: ;
        }
        Exit;
      end;

   else {
    eegCustomBase: ;
    eegDocument: ;
    eegXMLSetting: ;
    eegProjectSettings: ;
    eegStudy: ;
    eegSections: ;
    eegHeading: ;
    eegRange: ;
    eegValueLabel: ;
    eegValueLabelSet: ;
    eegRelations: ;
    eegRights: ;
    }
    Exit;
  end;
end;

constructor TEpiLogger.Create(AOwner: TEpiCustomBase; DataFiles: TEpiDataFiles);
var
  RO: TEpiCustomBase;
begin
  inherited Create(AOwner);
  FCommitState := csNone;
  FLogEvents   := true;

  FSecurityLog := TEpiSecurityDatafile(DataFiles.GetDataFileByName(EpiSecurityLogDatafileName));
  FDataLog     := TEpiSecurityDataEventLog(Datafiles.GetDataFileByName(EpiSecurityLogDataEventName));
  FKeyLog      := TEpiSecurityKeyFieldLog(DataFiles.GetDataFileByName(EpiSecurityLogKeyDataName));

  RO := RootOwner;
  if not (RO is TEpiDocument) then
    Exit;

  RO.RegisterOnChangeHook(@DocumentHook, true);
end;

function TEpiLogger.LogEntryFromNodeName(const NodeName: DOMString
  ): TEpiLogEntry;
begin
  case NodeName of
    'LoginSuccess': result := ltSuccessLogin;
    'LoginFailed':  result := ltFailedLogin;
    'Search':       result := ltSearch;
    'NewRecord':    result := ltNewRecord;
    'EditRecord':   result := ltEditRecord;
    'ViewRecord':   result := ltViewRecord;
    'Pack':         result := ltPack;
    'Append':       result := ltAppend;
    'Closed':       result := ltClose;
    'Export':       result := ltExport;
    'Password':     result := ltNewPassword;
  else
    result := ltNone;
  end;
end;

function TEpiLogger.CreateCommonNode(RootDoc: TDOMDocument; LogIndex: Integer
  ): TDOMElement;
var
  S: String;
begin
 { case FLogDatafile.FType.AsEnum[LogIndex] of
    ltNone: ;
    ltSuccessLogin:
      S := 'LoginSuccess';
    ltFailedLogin:
      S := 'LoginFailed';
    ltSearch:
      S := 'Search';
    ltNewRecord:
      S := 'NewRecord';
    ltEditRecord:
      S := 'EditRecord';
    ltViewRecord:
      S := 'ViewRecord';
    ltPack:
      S := 'Pack';
    ltAppend:
      S := 'Append';
    ltExport:
      S := 'Export';
    ltClose:
      S := 'Closed';
    ltNewPassword:
      S := 'Password';
  else
    S := 'UnImplementedXmlTagInLogger';
  end;

  Result := RootDoc.CreateElement(S);

  SaveDomAttr(Result, 'time', FLogDatafile.FTime.AsDateTime[LogIndex]);
  if (FLogDatafile.FType.AsEnum[LogIndex] in EpiLogEntryDataFileSet) then
    SaveDomAttr(Result, rsDataFileRef, FLogDatafile.FDataFileNames.AsString[LogIndex]);
  SaveDomAttr(Result, 'username', FLogDatafile.FUserNames.AsString[LogIndex]);
  SaveDomAttr(Result, rsCycle, FLogDatafile.FCycle.AsString[LogIndex]);   }
end;

procedure TEpiLogger.AddKeyFieldValues(RootNode: TDOMElement; LogIndex: Integer
  );
begin
//  SaveTextContent(RootNode, 'Keys', FLogDatafile.FKeyFieldValues.AsString[LogIndex]);
end;

procedure TEpiLogger.AddEditFieldValues(RootNode: TDOMElement; LogIndex: Integer
  );
var
  AList: TList;
  Data: PDataLogEntry;
  Elem: TDOMElement;
  MissingStr: EpiString;
begin
{  AList := TList(PtrInt(FLogDatafile.FDataContent.AsInteger[LogIndex]));
  MissingStr := TEpiStringField.DefaultMissing;

  for Data in AList do
  with Data^ do
  begin
    Elem := RootNode.OwnerDocument.CreateElement('Change');
    SaveDomAttr(Elem, rsDataFileRef, DataFileName);
    SaveDomAttr(Elem, 'fieldRef', FieldName);
    SaveDomAttrEnum(Elem, rsType, FieldType, TypeInfo(TEpiFieldType));
    case FieldType of
      ftBoolean:
        begin
          if TEpiBoolField.CheckMissing(OldBoolValue) then
            SaveDomAttr(Elem, 'before', MissingStr)
          else
            SaveDomAttr(Elem, 'before', OldBoolValue);

          if TEpiBoolField.CheckMissing(NewBoolValue) then
            SaveDomAttr(Elem, 'after', MissingStr)
          else
            SaveDomAttr(Elem, 'after', NewBoolValue);
        end;

      ftInteger,
      ftAutoInc:
        begin
          if TEpiIntField.CheckMissing(OldIntValue) then
            SaveDomAttr(Elem, 'before', MissingStr)
          else
            SaveDomAttr(Elem, 'before', OldIntValue);

          if TEpiIntField.CheckMissing(NewIntValue) then
            SaveDomAttr(Elem, 'after', MissingStr)
          else
            SaveDomAttr(Elem, 'after', NewIntValue);
        end;

      ftFloat:
        begin
          if TEpiFloatField.CheckMissing(OldFloatValue) then
            SaveDomAttr(Elem, 'before', MissingStr)
          else
            SaveDomAttr(Elem, 'before', OldFloatValue);

          if TEpiFloatField.CheckMissing(NewFloatValue) then
            SaveDomAttr(Elem, 'after', MissingStr)
          else
            SaveDomAttr(Elem, 'after', NewFloatValue);
        end;

      ftDMYDate,
      ftMDYDate,
      ftYMDDate,
      ftDMYAuto,
      ftMDYAuto,
      ftYMDAuto:
        begin
          if TEpiDateField.CheckMissing(OldDateValue) then
            SaveDomAttr(Elem, 'before', MissingStr)
          else
            SaveDomAttr(Elem, 'before', OldDateValue);

          if TEpiDateField.CheckMissing(NewDateValue) then
            SaveDomAttr(Elem, 'after', MissingStr)
          else
            SaveDomAttr(Elem, 'after', NewDateValue);
        end;

      ftTime,
      ftTimeAuto:
        begin
          if TEpiDateTimeField.CheckMissing(OldTimeValue) then
            SaveDomAttr(Elem, 'before', MissingStr)
          else
            SaveDomAttr(Elem, 'before', OldTimeValue);

          if TEpiDateTimeField.CheckMissing(NewTimeValue) then
            SaveDomAttr(Elem, 'after', MissingStr)
          else
            SaveDomAttr(Elem, 'after', NewTimeValue);
        end;

      ftString,
      ftUpperString,
      ftMemo:
        begin
          if TEpiStringField.CheckMissing(OldStringValue) then
            SaveDomAttr(Elem, 'before', MissingStr)
          else
            SaveDomAttr(Elem, 'before', OldStringValue);

          if TEpiStringField.CheckMissing(NewStringValue) then
            SaveDomAttr(Elem, 'after', MissingStr)
          else
            SaveDomAttr(Elem, 'after', NewStringValue);
        end;
    end;
    RootNode.AppendChild(Elem);
  end; }
end;

procedure TEpiLogger.GetEditFieldValues(RootNode: TDOMNode; LogIndex: Integer);
var
  AList: TList;
  Data: PDataLogEntry;
  Node: TDOMNode;
  BeforeMissing, AfterMissing: Boolean;
begin
  // RootNode = <EditRecord ...>
  AList := TList.Create;

  Node := RootNode.FirstChild;
  while Assigned(Node) do
  begin
    while NodeIsWhiteSpace(Node) do
      Node := Node.NextSibling;
    if not Assigned(Node) then break;

    if (Node.NodeName <> 'Change') then
      begin
        Node := Node.NextSibling;
        Continue;
      end;

    Data := New(PDataLogEntry);
    with Data^ do
    begin
      DataFileName := LoadAttrString(Node, rsDataFileRef);
      FieldType := TEpiFieldType(LoadAttrEnum(Node, rsType, TypeInfo(TEpiFieldType)));
      FieldName := LoadAttrString(Node, 'fieldRef');

      BeforeMissing := (LoadAttrString(Node, 'before') = TEpiStringField.DefaultMissing);
      AfterMissing  := (LoadAttrString(Node, 'after')  = TEpiStringField.DefaultMissing);

      case FieldType of
        ftBoolean:
          begin
            if BeforeMissing then
              OldBoolValue := TEpiBoolField.DefaultMissing
            else
              OldBoolValue := LoadAttrInt(Node, 'before');

            if AfterMissing then
              NewBoolValue := TEpiBoolField.DefaultMissing
            else
              NewBoolValue := LoadAttrInt(Node, 'after', TEpiBoolField.DefaultMissing);
          end;

        ftInteger,
        ftAutoInc:
          begin
            if BeforeMissing then
              OldIntValue := TEpiIntField.DefaultMissing
            else
              OldIntValue := LoadAttrInt(Node, 'before');

            if AfterMissing then
              NewIntValue := TEpiIntField.DefaultMissing
            else
              NewIntValue := LoadAttrInt(Node, 'after');
          end;

        ftFloat:
          begin
            if BeforeMissing then
              OldFloatValue := TEpiFloatField.DefaultMissing
            else
              OldFloatValue := LoadAttrFloat(Node, 'before');

            if AfterMissing then
              NewFloatValue := TEpiFloatField.DefaultMissing
            else
              NewFloatValue := LoadAttrFloat(Node, 'after');
          end;

        ftDMYDate,
        ftMDYDate,
        ftYMDDate,
        ftDMYAuto,
        ftMDYAuto,
        ftYMDAuto:
          begin
            if BeforeMissing then
              OldDateValue := TEpiDateField.DefaultMissing
            else
              OldDateValue := LoadAttrInt(Node, 'before');

            if AfterMissing then
              NewDateValue := TEpiDateField.DefaultMissing
            else
              NewDateValue := LoadAttrInt(Node, 'after');
          end;

        ftTime,
        ftTimeAuto:
          begin
            if BeforeMissing then
              OldTimeValue := TEpiDateTimeField.DefaultMissing
            else
              OldTimeValue := LoadAttrDateTime(Node, 'before');

            if AfterMissing then
              NewTimeValue := TEpiDateTimeField.DefaultMissing
            else
              NewTimeValue := LoadAttrDateTime(Node, 'after');
          end;

        ftString,
        ftUpperString,
        ftMemo:
          begin
            if BeforeMissing then
              OldStringValue := TEpiStringField.DefaultMissing
            else
              OldStringValue := LoadAttrString(Node, 'before');

            if AfterMissing then
              NewStringValue := TEpiStringField.DefaultMissing
            else
              NewStringValue := LoadAttrString(Node, 'after');
          end;
      end; // Case
    end; // With Data^ do

    AList.Add(Data);
    Node := Node.NextSibling;
  end;
//  FLogDatafile.FDataContent.AsInteger[LogIndex] := PtrInt(AList);
end;

procedure TEpiLogger.AddSearchString(RootNode: TDOMElement; LogIndex: Integer);
begin
//  SaveTextContent(RootNode, 'SearchString', FLogDatafile.FLogContent.AsString[LogIndex]);
end;

procedure TEpiLogger.CreateExportNode(RootNode: TDOMElement; LogIndex: Integer);
var
  LogExportDoc: TLogExportDocument;
  LogExDF: TLogExportDatafile;
  Elem: TDOMElement;
  i: Integer;
begin
  // RootNode = <Export>
{  LogExportDoc := TLogExportDocument(PtrInt(FLogDatafile.FDataContent.AsInteger[LogIndex]));

  SaveDomAttr(RootNode, 'exportDeleted', LogExportDoc.ExportDeleted);
  SaveDomAttr(RootNode, rsType, LogExportDoc.ExportType);

  for i := 0 to LogExportDoc.ExportList.Count - 1 do
  begin
    LogExDF := TLogExportDatafile(LogExportDoc.ExportList[i]);

    Elem := RootNode.OwnerDocument.CreateElement('Dataform');
    SaveDomAttr(Elem, rsDataFileRef, LogExDF.Name);
    SaveDomAttr(Elem, 'fromRec', LogExDF.FromRecord);
    SaveDomAttr(Elem, 'toRec', LogExDF.ToRecord);
    SaveDomAttr(Elem, 'fieldsRefs', LogExDF.ExportFields.CommaText);

    RootNode.AppendChild(Elem);
  end;        }
end;

procedure TEpiLogger.ReadExportNode(RootNode: TDOMNode; LogIndex: Integer);
var
  Node: TDOMNode;
  LogExportDoc: TLogExportDocument;
  LogExDF: TLogExportDatafile;
begin
  // RootNode = <Export>
  LogExportDoc := TLogExportDocument.Create;

  LogExportDoc.ExportDeleted := LoadAttrBool(RootNode, 'exportDeleted');
  LogExportDoc.ExportType    := LoadAttrString(RootNode, rsType);

  Node := RootNode.FirstChild;
  while Assigned(Node) do
  begin
    while NodeIsWhiteSpace(Node) do
      Node := Node.NextSibling;
    if not Assigned(Node) then break;

    LogExDF := TLogExportDatafile.Create;

    LogExDF.Name         := LoadAttrString(Node, rsDataFileRef, LogExDF.Name);
    LogExDF.FromRecord   := LoadAttrInt(Node, 'fromRec');
    LogExDF.ToRecord     := LoadAttrInt(Node, 'toRec');
    LogExDF.ExportFields.CommaText := LoadAttrString(Node, 'fieldsRefs');

    LogExportDoc.ExportList.Add(LogExDF);

    Node := Node.NextSibling;
  end;

//  FLogDatafile.FDataContent.AsInteger[LogIndex] := PtrInt(LogExportDoc);
end;

function TEpiLogger.XMLName: string;
begin
  Result := 'Log';
end;

function TEpiLogger.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
var
  i: Integer;
  S: String;
  Elem: TDOMElement;
begin
{  Result := inherited SaveToDom(RootDoc);

  with FLogDatafile do
    for i := 0 to FType.Size - 1 do
    begin
      Elem := CreateCommonNode(RootDoc, I);

      case FType.AsEnum[I] of
        ltNone: ;
        ltSuccessLogin:
          SaveDomAttr(Elem, 'hostname', FLogContent.AsString[i]);
        ltFailedLogin:
          begin
            case FDataContent.AsInteger[I] of
              0: SaveDomAttr(Elem, 'type', 'password');
              1: SaveDomAttr(Elem, 'type', 'login');
              2: ; // used for external log only
              3: SaveDomAttr(Elem, 'type', 'blocked');
            end;

            SaveDomAttr(Elem, 'hostname', FLogContent.AsString[I]);
          end;
        ltSearch:
          AddSearchString(Elem, I);
        ltNewRecord:
          AddKeyFieldValues(Elem, I);
        ltEditRecord:
          begin
            AddKeyFieldValues(Elem, I);
            AddEditFieldValues(Elem, I);
          end;
        ltViewRecord:
          AddKeyFieldValues(Elem, I);
        ltPack: ;
        ltAppend: ;
        ltExport:
          CreateExportNode(Elem, I);
        ltClose:
          SaveDomAttr(Elem, 'lastEdited', FLogContent.AsString[I]);
        ltNewPassword: ;
      else
        SaveDomAttrEnum(Elem, 'NotImplementedLogEntry', FType.AsEnum[I], TypeInfo(TEpiLogEntry));
      end;
      Result.AppendChild(Elem);
    end;    }
end;

procedure TEpiLogger.LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap
  );
var
  Node: TDOMNode;
  Idx: Integer;
begin
  inherited LoadFromXml(Root, ReferenceMap);

  // Only XML v4 had log as a seperate instance. Hence if we are here we are loading
  // a v4 project and need to transfer data to the correct places.

  Node := Root.FirstChild;
  while Assigned(Node) do
  begin
    while NodeIsWhiteSpace(Node) do
      Node := Node.NextSibling;
    if not Assigned(Node) then break;

    FSecurityLog.NewRecords();
    Idx := FSecurityLog.Size - 1;

    with FSecurityLog do
    begin
      LogType.AsInteger[Idx]       := Integer(LogEntryFromNodeName(Node.NodeName));
      Date.AsDate[Idx]             := trunc(Self.LoadAttrDateTime(Node, 'time'));
      Time.AsTime[Idx]             := Frac(Self.LoadAttrDateTime(Node, 'time'));
      DataFileName.AsString[Idx]   := Self.LoadAttrString(Node, rsDataFileRef, '', false);
      UserName.AsString[Idx]       := Self.LoadAttrString(Node, 'username');
      Cycle.AsInteger[Idx]         := Self.LoadAttrInt(Node, rsCycle);

      case LogType.AsInteger[Idx] of
        Integer(ltNone): ;

        Integer(ltSuccessLogin):
          MachineName.AsString[Idx] := Self.LoadAttrString(Node, 'hostname');

        Integer(ltFailedLogin):
          begin
{TODO            case LoadAttrString(Node, 'type') of
              'password': DataContent.AsInteger[Idx] := 0;
              'login':    DataContent.AsInteger[Idx] := 1;
              'blocked':  DataContent.AsInteger[Idx] := 3;
            else
              //
            end;     }
            MachineName.AsString[Idx]     := Self.LoadAttrString(Node, 'hostname');
          end;

        Integer(ltSearch):
          LogContent.AsString[Idx]       := Self.LoadNodeString(Node, 'SearchString');

        Integer(ltNewRecord): ;
//TODO:          KeyFieldValues.AsString[Idx]   := Self.LoadNodeString(Node, 'Keys');

        Integer(ltEditRecord):
          begin
//TODO:            KeyFieldValues.AsString[Idx] := Self.LoadNodeString(Node, 'Keys');
            GetEditFieldValues(Node, Idx);
          end;

        Integer(ltViewRecord): ;
//TODO:          KeyFieldValues.AsString[Idx]   := Self.LoadNodeString(Node, 'Keys');

//        ltPack: ;
//        ltAppend: ;

        Integer(ltExport):
          ReadExportNode(Node, Idx);

        Integer(ltClose):
          LogContent.AsString[Idx]       := Self.LoadAttrString(Node, 'lastEdited');

//        ltNewPassword: ;
      end;
    end;

    Node := Node.NextSibling;
  end;
end;

procedure TEpiLogger.LoadExLog(ExLogObject: TEpiFailedLogger);
var
  Admin: TEpiAdmin;
  S: EpiString;
  Data: Pointer;
  Len: LongInt;
  Dummy, Idx, i: Integer;
  GuidPTR: PGuid;
  FDecrypter: TDCP_rijndael;
  EncryptSt, PlainTxtSt: TMemoryStream;
  B: Byte;
  D: TDateTime;
begin
  // ExLogRoot = <ExLog>
  Admin := Doc(self).Admin;

  FDecrypter := TDCP_rijndael.Create(nil);
  EncryptSt  := TMemoryStream.Create;
  PlainTxtSt := TMemoryStream.Create;

  for i := 0 to ExLogObject.FLogDataFile.Size -1 do
    begin

      // The content is encrypted and we need to decode it with the private RSA key.
      if ExLogObject.FLoginFailType.AsInteger[i] = 2 then
        begin
          S := ExLogObject.FAesKey.AsString[i];
          Data := GetMem(Length(S));
          Len  := Base64Decode(@S[1], Data, Length(S));
          Admin.RSA.Decrypt(Data, Len, GuidPTR, Dummy);
          Freemem(Data);

          // Prepare decryption of node content
          EncryptSt.Clear;
          PlainTxtSt.Clear;

          // Base 64 decode first to get raw data.
          S := ExLogObject.FEncryptedTxt.AsString[i];
          Data := GetMem(Length(S));
          Len  := Base64Decode(@S[1], Data, Length(S));
          EncryptSt.Write(Data^, Len);
          Freemem(Data);

          // Decrypt using AES key from above.
          EncryptSt.Position := 0;
          FDecrypter.Init(GuidPTR^, SizeOf(TGuid)*8, nil);
          FDecrypter.DecryptStream(EncryptSt, PlainTxtSt, EncryptSt.Size);
          PlainTxtSt.Position := 0;

          with FSecurityLog do
          begin
            B := PlainTxtSt.ReadByte;
            case B of
              FAILEDLOG_FAILTYPE_INCORRECT_PW,
              FAILEDLOG_FAILTYPE_INCORRECT_LOGIN:
                Idx := DoNewLog(ltFailedLogin);

              FAILEDLOG_FAILTYPE_AESKEY:
                ; // This should NOT happen

              FAILEDLOG_FAILTYPE_BLOCKED_LOGIN:
                Idx := DoNewLog(ltBlockedLogin);

              FAILEDLOG_FAILTYPE_EXPIRED_USER:
                Idx := DoNewLog(ltExpiredLogin);
            end;

            UserName.AsString[Idx]        := PlainTxtSt.ReadAnsiString;
            D := ScanDateTime('YYYY/MM/DD HH:NN:SS', PlainTxtSt.ReadAnsiString);
            Date.AsDateTime[Idx]          := D;
            Time.AsTime[Idx]              := TimeOf(D);
            Cycle.AsInteger[Idx]          := PlainTxtSt.ReadQWord;
            MachineName.AsString[Idx]     := PlainTxtSt.ReadAnsiString;
            Filename.AsString[Idx]        := PlainTxtSt.ReadAnsiString;
          end;

        end
      else
        // The project file was attempted opened but failed, however it was sucessfully loaded without
        // the document was freed. Hence data in the failed logger is not in an encrypted state and we
        // just need to copy over the data.
        begin
          case ExLogObject.FLoginFailType.AsInteger[i] of
            FAILEDLOG_FAILTYPE_INCORRECT_PW,
            FAILEDLOG_FAILTYPE_INCORRECT_LOGIN:
              Idx := DoNewLog(ltFailedLogin);

            FAILEDLOG_FAILTYPE_AESKEY:
              ; // This should NOT happen

            FAILEDLOG_FAILTYPE_BLOCKED_LOGIN:
              Idx := DoNewLog(ltBlockedLogin);

            FAILEDLOG_FAILTYPE_EXPIRED_USER:
              Idx := DoNewLog(ltExpiredLogin);
          end;

          FSecurityLog.UserName.AsString[Idx]     := ExLogObject.FUserNames.AsString[i];
          FSecurityLog.Date.AsDateTime[Idx]       := ExLogObject.FTime.AsDateTime[i];
          FSecurityLog.Time.AsDateTime[Idx]       := TimeOf(ExLogObject.FTime.AsDateTime[i]);
          FSecurityLog.Cycle.AsInteger[Idx]       := ExLogObject.FCycle.AsInteger[i];
          FSecurityLog.MachineName.AsString[Idx]  := ExLogObject.FHostName.AsString[i];
          FSecurityLog.Filename.AsString[Idx]     := ExLogObject.FFileNameField.AsString[i];
        end;
    end;

  if (not LogEvents) then
    Exit;

  Idx := DoNewLog(ltSuccessLogin);
  FSecurityLog.Date.AsDateTime[Idx] := FSuccessLoginTime;
  FSecurityLog.Time.AsDateTime[Idx] := FSuccessLoginTime;

  FDecrypter.Free;
  EncryptSt.Free;
  PlainTxtSt.Free;
end;

procedure TEpiLogger.SetUserName(AValue: UTF8String);
begin
  if FUserName = AValue then Exit;
  FUserName := AValue;
end;

procedure TEpiLogger.SetDatafile(AValue: TEpiDataFile);
begin
  if FDatafile = AValue then Exit;
  FDatafile := AValue;
end;

function TEpiLogger.DoNewLog(ALogType: TEpiLogEntry): Integer;
var
  ADoc: TEpiDocument;
begin
  Result := FSecurityLog.NewRecords();
  ADoc := Doc(Self.RootOwner);

  with FSecurityLog do
  begin
    UserName.AsString[Result]         := Self.UserName;
    Date.AsDateTime[Result]           := Now;
    Time.AsDateTime[Result]           := Now;
    Cycle.AsInteger[Result]           := ADoc.CycleNo;
    LogType.AsInteger[Result]         := Integer(ALogType);
    if Assigned(FDatafile) then
      DataFileName.AsString[Result]   := FDatafile.Name;
    Filename.AsString[Result]         := FFilename;
    MachineName.AsString[Result]      := GetHostNameWrapper;
  end;
end;

procedure TEpiLogger.LogKeyValues(ParentId, RecordNo: integer);
var
  F, KF: TEpiField;
  KeyIdx: Integer;
begin
  if (FDatafile.KeyFields.Count > 0) then
    for KF in FDatafile.KeyFields do
      begin
        KeyIdx := FKeyLog.NewRecords();

        FKeyLog.ID.AsInteger[KeyIdx]          := ParentID;
        FKeyLog.VariableName.AsString[KeyIdx] := KF.Name;
        FKeyLog.KeyValue.AsString[KeyIdx]     := KF.AsString[RecordNo];
      end
  else
    begin
      KeyIdx := FKeyLog.NewRecords();
      FKeyLog.ID.AsInteger[KeyIdx]          := ParentID;
      FKeyLog.VariableName.AsString[KeyIdx] := 'Obs. no';
      FKeyLog.KeyValue.AsInteger[KeyIdx]    := RecordNo + 1;
    end;
end;

procedure TEpiLogger.LogLoginSuccess();
begin
  // Just store the login time for now. If we were to add a record to the security log
  // at this point, all entries from the FailedLog would be out of order.
  // We add an entry into the securitylog once FailedLog is done loading.
  FSuccessLoginTime := now;
end;

procedure TEpiLogger.LogSearch(Search: TEpiSearch);
var
  Idx, i: Integer;
  S: String;
begin
  if (not LogEvents) then
    Exit;

  Idx := DoNewLog(ltSearch);

  if Assigned(Search) then
    begin
      S := DupeString('(', Search.ConditionCount - 2);

      for i := 0 to Search.ConditionCount - 1 do
        with Search.SearchCondiction[I] do
          begin
            if I > 0 then
              case BinOp of
                boAnd: S += ' And ';
                boOr:  S += ' Or ';
              end;
            S += '(' + Field.Name + ' ' + MatchCriteriaCaption[MatchCriteria] + ' ' + Text + ')';
            if (I > 0) and (I < Search.ConditionCount - 1) then
              S += ')';
          end;
    end
  else
    S := '*';

  with FSecurityLog do
  begin
    LogContent.AsString[Idx]    := S;
  end;
  DoChange(eegCustomBase, Word(ecceRequestSave), nil);
end;

procedure TEpiLogger.LogRecordNew();
var
  Idx, KeyIdx: Integer;
  KF: TEpiField;
begin
  Idx := DoNewLog(ltNewRecord);
  LogKeyValues(FSecurityLog.ID.AsInteger[Idx], FDatafile.Size - 1);
  DoChange(eegCustomBase, Word(ecceRequestSave), nil);
end;

procedure TEpiLogger.LogRecordEdit(RecordNo: Integer);
var
  Idx: Integer;
begin
  LogKeyValues(FSecurityLog.ID.AsInteger[FSecurityLog.Size - 1], RecordNo);
  DoChange(eegCustomBase, Word(ecceRequestSave), nil);
end;

procedure TEpiLogger.LogRecordView(RecordNo: Integer);
var
  Idx: Integer;
begin
  Idx := DoNewLog(ltViewRecord);
  LogKeyValues(FSecurityLog.ID.AsInteger[Idx], RecordNo);
  DoChange(eegCustomBase, Word(ecceRequestSave), nil);
end;

procedure TEpiLogger.LogRecordStatus(Status: PEpiDataFileStatusRecord);
var
  Idx: Integer;
begin
  Idx := DoNewLog(ltRecordStatus);
  LogKeyValues(FSecurityLog.ID.AsInteger[Idx], Status^.Index);
  case Status^.NewValue of
    rsNormal:   FSecurityLog.LogContent.AsString[Idx] := 'normal';
    rsVerified: FSecurityLog.LogContent.AsString[Idx] := 'verified';
    rsDeleted:  FSecurityLog.LogContent.AsString[Idx] := 'deleted';
  end;
  DoChange(eegCustomBase, Word(ecceRequestSave), nil);
end;

procedure TEpiLogger.LogPack();
begin
  DoNewLog(ltPack);
  DoChange(eegCustomBase, Word(ecceRequestSave), nil);
end;

procedure TEpiLogger.LogExportSecurityLog(const ExportFilename: UTF8String;
  StartID, EndID: Integer);
var
  Idx: Integer;
begin
  Idx := DoNewLog(ltSecurityLogExport);
  FSecurityLog.LogContent.AsString[Idx] := ExportFilename + Format(' (%d,%d)', [StartID, EndID]);
  DoChange(eegCustomBase, Word(ecceRequestSave), nil);
end;

procedure TEpiLogger.LogPassword();
begin
  DoNewLog(ltNewPassword);
  DoChange(eegCustomBase, Word(ecceRequestSave), nil);
end;

procedure TEpiLogger.LogExpiredChange(NewExpiredDate: TDateTime);
var
  Idx: Integer;
begin
  Idx := DoNewLog(ltExpiredChange);
  FSecurityLog.LogContent.AsString[Idx] := FormatDateTime('YYYY/MM/DD HH:NN:SS', NewExpiredDate);
  DoChange(eegCustomBase, Word(ecceRequestSave), nil);
end;

procedure TEpiLogger.LogAppend();
begin
  if (not LogEvents) then
    Exit;

  DoNewLog(ltAppend);
  DoChange(eegCustomBase, Word(ecceRequestSave), nil);
end;

procedure TEpiLogger.LogClose();
var
  Idx, i: Integer;
  LastEdit: TDateTime;
  ADoc: TEpiDocument;
begin
  if (not LogEvents) then
    Exit;

  Idx := DoNewLog(ltClose);

  ADoc := Doc(Self);
  LastEdit := ADoc.Study.ModifiedDate;
  for i := 0 to ADoc.DataFiles.Count -1 do
  with ADoc.DataFiles[i] do
    LastEdit := Max(LastEdit, Max(RecModifiedDate, StructureModifiedDate));

  FSecurityLog.LogContent.AsString[Idx] := FormatDateTime('YYYY/MM/DD HH:NN:SS', LastEdit);
  DoChange(eegCustomBase, Word(ecceRequestSave), nil);
end;

procedure TEpiLogger.LogExport(Settings: TObject);
var
  OrgSettings: TEpiExportSetting absolute Settings;
  Idx, i: Integer;
  LogExportDoc: TLogExportDocument;
  LogExportDF: TLogExportDatafile;
  DFSetting: TEpiExportDatafileSettings;
begin
  if (not LogEvents) then
    Exit;

  {Idx := DoNewLog(ltExport);

  LogExportDoc := TLogExportDocument.Create;
  LogExportDoc.ExportDeleted := OrgSettings.ExportDeleted;
  LogExportDoc.ExportType    := TEpiExportSettingClass(OrgSettings.ClassType).ClassName;

  for i := 0 to OrgSettings.DatafileSettings.Count - 1 do
  begin
    DFSetting := OrgSettings.DatafileSettings[i];
    LogExportDF := TLogExportDatafile.Create;
    LogExportDF.Name := DFSetting.DatafileName;
    LogExportDF.FromRecord := DFSetting.FromRecord;
    LogExportDF.ToRecord   := DFSetting.ToRecord;
    LogExportDF.ExportFields.Assign(DFSetting.ExportItems);

    LogExportDoc.ExportList.Add(LogExportDF);
  end;

  FLogDatafile.FDataContent.AsInteger[Idx] := PtrInt(LogExportDoc);}
end;

{ TEpiFailedLogger }

procedure TEpiFailedLogger.DocumentHook(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
var
  Idx: Integer;
  AEvent: TEpiAdminChangeEventType;
begin
  if (EventGroup <> eegAdmin) then exit;

  AEvent := TEpiAdminChangeEventType(EventType);
  if AEvent in
       [
         eaceAdminUserExpired,            // Data: TEpiUser = the expired user
         eaceAdminIncorrectUserName,      // Data: string   = the incorrect login name
         eaceAdminIncorrectPassword       // Data: string   = the incorrect login name
       ]
  then
    begin
      FLogDataFile.NewRecords();
      Idx := FLogDataFile.Size - 1;

      if (AEvent = eaceAdminUserExpired) then
        FUserNames.AsString[Idx] := TEpiUser(Data).Login
      else
        FUserNames.AsString[Idx] := PUTF8String(Data)^;
      FTime.AsDateTime[Idx]    := Now;
      FCycle.AsInteger[Idx]    := Doc(Self).CycleNo;
      FAesKey.AsString[Idx]    := '';
      Case AEvent of
        eaceAdminIncorrectPassword: FLoginFailType.AsInteger[Idx] := FAILEDLOG_FAILTYPE_INCORRECT_PW;
        eaceAdminIncorrectUserName: FLoginFailType.AsInteger[Idx] := FAILEDLOG_FAILTYPE_INCORRECT_LOGIN;
        eaceAdminUserExpired:       FLoginFailType.AsInteger[Idx] := FAILEDLOG_FAILTYPE_EXPIRED_USER;
      end;
      FEncryptedTxt.AsString[Idx] := '';
      FHostName.AsString[Idx]     := GetHostNameWrapper;
      FFileNameField.AsString[Idx] := Filename;
    end;


  if AEvent in
    [eaceAdminBlockedLogin]
  then
    begin
      FLogDataFile.NewRecords();
      Idx := FLogDataFile.Size - 1;

      FUserNames.AsString[Idx] := '';
      FTime.AsDateTime[Idx]    := Now;
      FCycle.AsInteger[Idx]    := Doc(Self).CycleNo;
      FAesKey.AsString[Idx]    := '';
      FLoginFailType.AsInteger[Idx] := FAILEDLOG_FAILTYPE_BLOCKED_LOGIN;
      FEncryptedTxt.AsString[Idx]   := '';
      FHostName.AsString[Idx]       := GetHostNameWrapper;
      FFileNameField.AsString[Idx]  := Filename;
    end;
end;

constructor TEpiFailedLogger.Create(AOwner: TEpiCustomBase);
var
  RO: TEpiCustomBase;
begin
  inherited Create(AOwner);

  RO := RootOwner;
  if not (RO is TEpiDocument) then
    Exit;

  FEncrypter := TDCP_rijndael.Create(nil);

  FLogDataFile   := TEpiDataFile.Create(nil);
  FUserNames     := FLogDataFile.NewField(ftString);
  FTime          := FLogDataFile.NewField(ftTime);
  FCycle         := FLogDataFile.NewField(ftInteger);
  FAesKey        := FLogDataFile.NewField(ftString);
  FLoginFailType := FLogDataFile.NewField(ftInteger);
  FEncryptedTxt  := FLogDataFile.NewField(ftString);
  FHostName      := FLogDataFile.NewField(ftString);
  FFileNameField := FLogDataFile.NewField(ftString);

  Doc(Self).RegisterOnChangeHook(@DocumentHook, true);
end;

function TEpiFailedLogger.XMLName: string;
begin
  Result := 'ExLog';
end;

function TEpiFailedLogger.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
var
  Elem: TDOMElement;
  i, OLen: Integer;
  GUID: TGUID;
  Data: PByte;
  B64Len: LongInt;
  AesKey, S: String;
  PlainTxtMs, EncryptMs: TMemoryStream;
  ATime: Double;
  BTime: array[0..7] of byte absolute ATime;
begin
  Result := inherited SaveToDom(RootDoc);

  CreateGUID(GUID);
  FEncrypter.Init(GUID, SizeOf(TGuid)*8, nil);
  Doc(Self).Admin.RSA.Encrypt(@GUID, SizeOf(TGuid), Data, OLen);
  SetLength(AesKey, OLen * 2);
  B64Len := Base64Encode(Data, @AesKey[1], OLen);
  SetLength(AesKey, B64Len);

  PlainTxtMs := TMemoryStream.Create;
  EncryptMs  := TMemoryStream.Create;

  for i := 0 to FLogDataFile.Size - 1 do
  begin
    Elem := RootDoc.CreateElement('LoginFailed');

    SaveDomAttr(Elem, 'time',     FTime.AsDateTime[i]);
    SaveDomAttr(Elem, 'hostname', FHostName.AsString[i]);

    if (FLoginFailType.AsInteger[i] = FAILEDLOG_FAILTYPE_AESKEY) then
      begin
        SaveDomAttr(Elem, 'aesKey', FAesKey.AsString[i]);
        Elem.TextContent := FEncryptedTxt.AsString[i];
      end
    else
      begin
        SaveDomAttr(Elem, 'aesKey', AesKey);

        PlainTxtMs.Clear;
        PlainTxtMs.WriteByte(FLoginFailType.AsInteger[i]);
        PlainTxtMs.WriteAnsiString(FUserNames.AsString[i]);
        PlainTxtMs.WriteAnsiString(FormatDateTime('YYYY/MM/DD HH:NN:SS', FTime.AsDateTime[i]));
        PlainTxtMs.WriteQWord(FCycle.AsInteger[i]);
        PlainTxtMs.WriteAnsiString(FHostName.AsString[i]);
        PlainTxtMs.WriteAnsiString(FFileNameField.AsString[i]);

        PlainTxtMs.Position := 0;
        EncryptMs.Clear;
        FEncrypter.Reset;
        FEncrypter.EncryptStream(PlainTxtMs, EncryptMs, PlainTxtMs.Size);

        SetLength(S, EncryptMs.Size * 2);
        B64Len := Base64Encode(EncryptMs.Memory, @S[1], EncryptMs.Size);
        SetLength(S, B64Len);

        Elem.TextContent := S;
    end;

    Result.AppendChild(Elem);
  end;

  PlainTxtMs.Free;
  EncryptMs.Free;
end;

procedure TEpiFailedLogger.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
var
  Node: TDOMNode;
  Idx: Integer;
begin
  // Root = <ExLog>
  inherited LoadFromXml(Root, ReferenceMap);

  Node := Root.FirstChild;
  while Assigned(Node) do
  begin
    // hack to skip whitespace nodes.
    while NodeIsWhiteSpace(Node) do
      Node := Node.NextSibling;
    if not Assigned(Node) then break;

    CheckNode(Node, 'LoginFailed');

    FLogDataFile.NewRecords();
    Idx := FLogDataFile.Size -1;

    FUserNames.AsString[Idx]      := '';
    FTime.AsTime[Idx]             := LoadAttrDateTime(Node, 'time');
    FCycle.AsInteger[Idx]         := -1;
    FAesKey.AsString[Idx]         := LoadAttrString(Node, 'aesKey');
    FLoginFailType.AsInteger[Idx] := FAILEDLOG_FAILTYPE_AESKEY;
    FEncryptedTxt.AsString[Idx]   := Node.TextContent;
    FHostName.AsString[Idx]       := LoadAttrString(Node, 'hostname');

    Node := Node.NextSibling;
  end;
end;

function TEpiFailedLogger.TooManyFailedLogins(Attempts, Interval: Integer
  ): boolean;
var
  AHostNames: TStringList;
  MyHostName: UTF8String;
  ATime: TDateTime;
  Counter, i: Integer;
begin
  Result := false;

  if FLogDataFile.Size = 0 then Exit;

  MyHostName := GetHostNameWrapper;
  Counter := 0;
  ATime := Now - (Interval / (60 * 60 * 24));
  for i := FLogDataFile.Size - 1 downto 0 do
  begin
    if (MyHostName <> FHostName.AsString[I]) then Continue;
    if (FTime.AsDateTime[I] >= ATime) then Inc(Counter);

    if (Counter >= Attempts) then
      Exit(True);
  end;
end;

end.

