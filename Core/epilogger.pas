unit epilogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidatafiles, epidatafilestypes, epicustombase, epitools_search,
  Laz2_DOM;

type
  TEpiLogEntry = (
    ltNone,            // The "empty" entry in the EnumField
    ltSuccessLogin,    // A succesfull login
    ltFailedLogin,     // An unsuccessfull login
    ltSearch,          // Search performed
    ltNewRecord,       // New Record added
    ltEditRecord,      // Edited an existing record
    ltViewRecord,      // Changed record no. in viewer
    ltPack,            // Packed datafiles
    ltAppend           // Appended data to datafiles
  );


  { TEpiEnumField }

  TEpiEnumField = class(TEpiField)
  private
    FData: array of TEpiLogEntry;
  protected
    function GetAsEnum(const Index: Integer): TEpiLogEntry; virtual;
    procedure SetAsEnum(const Index: Integer; AValue: TEpiLogEntry); virtual;
    procedure SetCapacity(AValue: Integer); override;
    procedure SetAsInteger(const index: Integer; const AValue: EpiInteger); override;
    procedure SetAsString(const index: Integer; const AValue: EpiString); override;
    function GetCapacity: Integer; override;
    function GetAsInteger(const index: Integer): EpiInteger; override;
    function GetAsString(const index: Integer): EpiString; override;
    procedure SetIsMissing(const index: Integer; const AValue: boolean); override;
    procedure SetHasDefaultValue(const AValue: boolean); override;
    procedure SetAsValue(const index: Integer; const AValue: EpiVariant); override;
    procedure SetAsTime(const index: Integer; const AValue: EpiTime); override;
    procedure SetAsFloat(const index: Integer; const AValue: EpiFloat); override;
    procedure SetAsDateTime(const index: Integer; const AValue: EpiDateTime); override;
    procedure SetAsDate(const index: Integer; const AValue: EpiDate); override;
    procedure SetAsBoolean(const index: Integer; const AValue: EpiBool); override;
    procedure MovePackData(const SrcIdx, DstIdx, Count: integer); override;
    function GetIsMissing(const index: Integer): boolean; override;
    function GetHasDefaultValue: boolean; override;
    function GetAsValue(const index: Integer): EpiVariant; override;
    function GetAsTime(const index: Integer): EpiTime; override;
    function GetAsFloat(const index: Integer): EpiFloat; override;
    function GetAsDateTime(const index: Integer): EpiDateTime; override;
    function GetAsDate(const index: Integer): EpiDate; override;
    function GetAsBoolean(const index: Integer): EpiBool; override;
    procedure DoSetDefaultValueAsString(const AValue: string); override;
    function DoGetDefaultValueAsString: string; override;
    function DoCompare(i, j: integer): integer; override;
  public
    procedure Exchange(i, j: integer); override;
    procedure ResetDefaultValue; override;
    procedure ResetData; override;
    function FormatString(const FillSpace: boolean = false): string; override;
    property AsEnum[Const Index: Integer]: TEpiLogEntry read GetAsEnum write SetAsEnum;
  public
    constructor Create(AOwner: TEpiCustomBase; AFieldType: TEpiFieldType); override;
    class function DefaultMissing: TEpiLogEntry;
  end;

  { TEpiLog }

  TEpiLog = class(TEpiDataFile)
  private
    FUserNames:      TEpiField;
    FTime:           TEpiField;
    FCycle:          TEpiField;
    FType:           TEpiEnumField;
    FDataFileNames:  TEpiField;
    FLogContent:     TEpiField;
  public
    constructor Create(AOwner: TEpiCustomBase; const aSize: integer = 0);
  end;

  { TEpiLogger }

  TEpiLogger = class(TEpiCustomBase)
  { Data Logging  }
  private type
    TCommitState = (csNone, csNewRecord, csEditRecord);
  private
    FCommitState: TCommitState;
    FDataLog: TStrings;
    procedure ClearDataLog;

  private
    FLogDatafile: TEpiLog;
    procedure DocumentHook(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    function    XMLName: string; override;
    function    SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
    procedure   LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;

  { Logging properties }
  private
    FUserName: string;
    FDatafile: TEpiDataFile;
    procedure SetUserName(AValue: string);
    procedure SetDatafile(AValue: TEpiDataFile);
    function  DoNewLog(LogType: TEpiLogEntry): Integer;  // Result = Index for new record.
    function  GetKeyValues: EpiString;
  public
    property   Datafile: TEpiDataFile read FDatafile write SetDatafile;
    property   UserName: string read FUserName write SetUserName;

  { Logging methods }
  private
    procedure  LogLoginSuccess();
    procedure  LogLoginFail();
    procedure  LogSearch(Search: TEpiSearch);
    procedure  LogRecordNew();
    procedure  LogRecordEdit(EditedFields: TEpiFields);
    procedure  LogRecordView(RecordNo: Integer);
    procedure  LogPack();
  public
    procedure  LogAppend();
  end;

implementation

uses
  typinfo, epidocument, epiadmin, strutils;

procedure TEpiLogger.ClearDataLog;
var
  PData: PEpiFieldDataEventRecord;
begin
  for i := 0 to FDataLog.Count - 1 do
    begin
      PData := PEpiFieldDataEventRecord(FDataLog.Objects[I]);
      Dispose(PData);
    end;
  FDataLog.Clear;
end;

procedure TEpiLogger.DocumentHook(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
var
  PData: PEpiFieldDataEventRecord;
begin
  case EventGroup of
    eegAdmin:
      case TEpiAdminChangeEventType(EventType) of
        eaceAdminLoginSuccessfull:       // Data: TEpiUser = the authenticated user.
          begin
            UserName := TEpiUser(Data).Login;
            LogLoginSuccess();
          end;

        eaceAdminIncorrectUserName,      // Data: string   = the incorrect login name
        eaceAdminIncorrectPassword:      // Data: string   = the incorrect login name
          begin
            UserName := string(Data);
            LogLoginFail();
          end;

        eaceAdminIncorrectNewPassword:   // Data: TEpiUser = the authenticated user.
          ;

      else
        {
        eaceUserSetFullName: ;
        eaceUserSetPassword: ;
        eaceUserSetExpireDate: ;
        eaceUserSetLastLogin: ;
        eaceUserSetNotes: ;
        eaceGroupSetManageRights: ;
        eaceAdminResetting: ;
        }
        Exit;
      end;


    eegDataFiles:
      case TEpiDataFileChangeEventType(EventType) of
        edcePack:
          LogPack();

        edceBeginCommit:
          begin
            if PtrInt(Data) = 0 then
              FCommitState := csNewRecord
            else
              begin
                ClearDataLog;
                FCommitState := csEditRecord;
              end;
          end;

        edceEndCommit:
          begin
            case FCommitState of
              csNone: ;

              csNewRecord:
                LogRecordNew();

              csEditRecord:
                LogRecordEdit(nil);
            end;

            FCommitState := csNone;
          end

      else
        {
        edceSize: ;
        edceRecordStatus: ;
        edceStatusbarContentString: ;
        }
        Exit;
      end;

    eegFields:
      case TEpiFieldsChangeEventType(EventType) of
        efceData:
          begin
            if (FCommitState <> csEditRecord) then Exit;

            PData := PEpiFieldDataEventRecord(Data);
            FDataLog.AddObject(TEpiField(Initiator).Name, );
            // TODO : Store data changes during a commit fase.
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

constructor TEpiLogger.Create(AOwner: TEpiCustomBase);
var
  RO: TEpiCustomBase;
begin
  inherited Create(AOwner);
  FCommitState := csNone;
  FDataLog := TStringList.Create;

  RO := RootOwner;
  if not (RO is TEpiDocument) then
    Exit;

  RO.RegisterOnChangeHook(@DocumentHook, true);
end;

function TEpiLogger.XMLName: string;
begin
  Result := inherited XMLName;
end;

function TEpiLogger.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);
end;

procedure TEpiLogger.LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap
  );
begin
  inherited LoadFromXml(Root, ReferenceMap);
end;

procedure TEpiLogger.SetUserName(AValue: string);
begin
  if FUserName = AValue then Exit;
  FUserName := AValue;
end;

procedure TEpiLogger.SetDatafile(AValue: TEpiDataFile);
begin
  if FDatafile = AValue then Exit;
  FDatafile := AValue;
end;

function TEpiLogger.DoNewLog(LogType: TEpiLogEntry): Integer;
var
  Doc: TEpiDocument;
begin
  FLogDatafile.NewRecords();
  Result := FLogDatafile.Size - 1;
  Doc := TEpiDocument(RootOwner);

  with FLogDatafile do
  begin
    FUserNames.AsString[Result]        := UserName;
    FTime.AsDateTime[Result]           := Now;
    FCycle.AsInteger[Result]           := Doc.CycleNo;
    FType.AsEnum[Result]               := LogType;
  end;
end;

function TEpiLogger.GetKeyValues: EpiString;
var
  F: TEpiField;
  Idx: Integer;
begin
  Idx := FDatafile.Size - 1;

  Result := '';
  for F in FDatafile.KeyFields do
    Result += F.Name + '=' + F.AsString[Idx] + ',';
end;

procedure TEpiLogger.LogLoginSuccess;
begin
  DoNewLog(ltSuccessLogin);
end;

procedure TEpiLogger.LogLoginFail;
begin
  DoNewLog(ltFailedLogin);
  // TODO: Force a save...
end;

procedure TEpiLogger.LogSearch(Search: TEpiSearch);
var
  Idx, i: Integer;
  S: String;
begin
  Idx := DoNewLog(ltSearch);

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

  with FLogDatafile do
  begin
    FDataFileNames.AsString[Idx] := FDatafile.Name;
    FLogContent.AsString[Idx]    := S;
  end;
end;

procedure TEpiLogger.LogRecordNew;
var
  Idx: Integer;
begin
  Idx := DoNewLog(ltNewRecord);

  with FLogDatafile do
  begin
    FDataFileNames.AsString[Idx] := FDatafile.Name;
    FLogContent.AsString[Idx]    := GetKeyValues;
  end;
end;

procedure TEpiLogger.LogRecordEdit(EditedFields: TEpiFields);
var
  Idx: Integer;
begin
  Idx := DoNewLog(ltNewRecord);

  with FLogDatafile do
  begin
    FDataFileNames.AsString[Idx] := FDatafile.Name;
    FLogContent.AsString[Idx]    := GetKeyValues;
  end;
end;

procedure TEpiLogger.LogRecordView(RecordNo: Integer);
begin

end;

procedure TEpiLogger.LogPack;
begin

end;

procedure TEpiLogger.LogAppend;
begin

end;

{ TEpiEnumField }

function TEpiEnumField.GetAsEnum(const Index: Integer): TEpiLogEntry;
begin
  CheckIndex(Index);
  Result := FData[Index];
end;

procedure TEpiEnumField.SetAsEnum(const Index: Integer; AValue: TEpiLogEntry);
begin
  CheckIndex(Index);
  FData[Index] := AValue;

end;

function TEpiEnumField.GetAsString(const index: Integer): EpiString;
begin
  Result := GetEnumName(TypeInfo(TEpiLogEntry), AsInteger[Index]);
end;

function TEpiEnumField.DoCompare(i, j: integer): integer;
begin
  result := 0;
end;

function TEpiEnumField.DoGetDefaultValueAsString: string;
begin
  result := '';
end;

procedure TEpiEnumField.DoSetDefaultValueAsString(const AValue: string);
begin
  //
end;

function TEpiEnumField.GetAsBoolean(const index: Integer): EpiBool;
begin

end;

function TEpiEnumField.GetAsDate(const index: Integer): EpiDate;
begin

end;

function TEpiEnumField.GetAsDateTime(const index: Integer): EpiDateTime;
begin

end;

function TEpiEnumField.GetAsFloat(const index: Integer): EpiFloat;
begin

end;

function TEpiEnumField.GetAsTime(const index: Integer): EpiTime;
begin

end;

function TEpiEnumField.GetAsValue(const index: Integer): EpiVariant;
begin
  result := 0;
end;

function TEpiEnumField.GetHasDefaultValue: boolean;
begin
  result := false;
end;

function TEpiEnumField.GetIsMissing(const index: Integer): boolean;
begin
  result := (AsEnum[Index] = ltNone);
end;

procedure TEpiEnumField.MovePackData(const SrcIdx, DstIdx, Count: integer);
begin

end;

procedure TEpiEnumField.SetAsBoolean(const index: Integer; const AValue: EpiBool
  );
begin

end;

procedure TEpiEnumField.SetAsDate(const index: Integer; const AValue: EpiDate);
begin

end;

procedure TEpiEnumField.SetAsDateTime(const index: Integer;
  const AValue: EpiDateTime);
begin

end;

procedure TEpiEnumField.SetAsFloat(const index: Integer; const AValue: EpiFloat
  );
begin

end;

procedure TEpiEnumField.SetAsTime(const index: Integer; const AValue: EpiTime);
begin

end;

procedure TEpiEnumField.SetAsValue(const index: Integer;
  const AValue: EpiVariant);
begin

end;

procedure TEpiEnumField.SetHasDefaultValue(const AValue: boolean);
begin

end;

procedure TEpiEnumField.SetIsMissing(const index: Integer; const AValue: boolean
  );
begin

end;

function TEpiEnumField.GetCapacity: Integer;
begin
  result := System.Length(FData);
end;

function TEpiEnumField.GetAsInteger(const index: Integer): EpiInteger;
begin
  Result := EpiInteger(AsEnum[Index]);
end;

procedure TEpiEnumField.SetAsString(const index: Integer;
  const AValue: EpiString);
begin
  SetAsInteger(Index, GetEnumValue(TypeInfo(TEpiLogEntry), AValue));
end;

procedure TEpiEnumField.SetCapacity(AValue: Integer);
var
  i: LongInt;
begin
  if AValue = Capacity then exit;
  System.SetLength(FData, AValue);
  for i := Capacity to AValue-1 do
    FData[i] := DefaultMissing;
  FCapacity := AValue;
end;

procedure TEpiEnumField.SetAsInteger(const index: Integer;
  const AValue: EpiInteger);
begin
  SetAsEnum(Index, TEpiLogEntry(AValue));
end;

procedure TEpiEnumField.Exchange(i, j: integer);
var
  Tmp: TEpiLogEntry;
begin
  Tmp := AsEnum[I];
  AsEnum[J] := AsEnum[I];
  AsEnum[I] := Tmp;
end;

function TEpiEnumField.FormatString(const FillSpace: boolean): string;
begin

end;

procedure TEpiEnumField.ResetData;
begin
  FillByte(FData[0], Length, 0);
end;

procedure TEpiEnumField.ResetDefaultValue;
begin

end;

constructor TEpiEnumField.Create(AOwner: TEpiCustomBase;
  AFieldType: TEpiFieldType);
begin
  inherited Create(AOwner, AFieldType);
end;

class function TEpiEnumField.DefaultMissing: TEpiLogEntry;
begin
  result := ltNone;
end;

{ TEpiLog }

constructor TEpiLog.Create(AOwner: TEpiCustomBase; const aSize: integer);
begin
  inherited Create(AOwner, aSize);

  FUserNames := Fields.NewField(ftString);
  FTime      := Fields.NewField(ftTime);
  FCycle     := Fields.NewField(ftInteger);
  FType      := TEpiEnumField.Create(nil, ftBoolean);
  MainSection.Fields.AddItem(FType);
end;

end.

