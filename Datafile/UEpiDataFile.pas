unit UEpiDataFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, UValueLabels, Rijndael, UCheckFileCmds, UEpiLog,
  UCheckFileTypes, Graphics, UDataFileTypes, UEpiDataGlobals;

type

  TEpiField = class;
  TEpiFields = class;
  TEpiDataFile = class;

  { TEpiCheckField }

  TEpiCheckField = class(TObject)
  private
    FMustEntert:       Boolean;
    FNoEnter:          Boolean;
    FTopOfScreen:      Boolean;
    FTopOfScreenLines: Integer;
    FDoRepeat:         boolean;
    FConfirm:          boolean;
    FBeforeCmds:       TChkCommands;
    FAfterCmds:        TChkCommands;
    FMin:              string;
    FMax:              string;
    FLegal:            string;
    FMissingValues:    TMissingValues;
    FDefaultValue:     string;
    FAutoFields:       string;
    FAutoList:         Boolean;
    FJumps:            string;
    FJumpResetChar:    Char;
    FValueLabel:       TValueLabelSet;
    FShowValueLabel:   Boolean;
    FValueLabelIsFieldRef: Boolean;
    FTypeType:         TTypeType;
    FTypeText:         string;
    FTypeColor:        TColor;
    FHasGlobalDefaultVal: Boolean;
    FFieldScope:       TFieldScope;
    FFieldComments:    TStrings;
    function           GetMissingValue(Index: integer): string;
    procedure          SetMissingValue(Index: integer; Value: string);
    function           GetAutoSearch(): Boolean;
    procedure          InternalReset();
  protected

  public
    Constructor Create();
    Destructor  Destroy(); override;
    procedure   Reset();
    procedure   Clone(Var Dest: TEpiCheckField);
    Property    ValueLabel:   TValueLabelSet read FValueLabel write FValueLabel;
    Property    ValueLabelIsFieldRef: Boolean read FValueLabelIsFieldRef write FValueLabelIsFieldRef;
    property    MustEnter:    boolean read FMustEntert write FMustEntert;
    property    NoEnter:      boolean read FNoEnter write FNoEnter;
    property    TopOfScreen:  Boolean read FTopOfScreen write FTopOfScreen;
    property    TopOfScreenLines: Integer read FTopOfScreenLines write FTopOfScreenLines;
    property    DoRepeat:     boolean read FDoRepeat write FDoRepeat;
    property    Confirm:      boolean read FConfirm write FConfirm;
    property    BeforeCmds:   TChkCommands read FBeforeCmds;
    property    AfterCmds:    TChkCommands read FAfterCmds;
    property    Min:          string read FMin write FMin;
    property    Max:          string read FMax write FMax;
    property    Legal:        string read FLegal write FLegal;
    property    MissingValues[Index: integer]: string read GetMissingValue write SetMissingValue;
    property    DefaultValue: string read FDefaultValue write FDefaultValue;
    property    AutoFields:   string read FAutoFields write FAutoFields;
    property    AutoSearch:   Boolean read GetAutoSearch;
    property    AutoList:     boolean read FAutoList write FAutoList;
    property    Jumps:        string read FJumps write FJumps;
    property    JumpResetChar: Char read FJumpResetChar write FJumpResetChar;
    property    ShowValueLabel: Boolean read FShowValueLabel write FShowValueLabel;
    property    TypeType:     TTypeType read FTypeType write FTypeType;
    property    TypeText:     string read FTypeText write FTypeText;
    property    TypeColour:   TColor read FTypeColor write FTypeColor;
    property    HasGlobalDefaultVal: Boolean read FHasGlobalDefaultVal write FHasGlobalDefaultVal;
    property    FieldScope:   TFieldScope read FFieldScope write FFieldScope;
    property    FieldComments: TStrings read FFieldComments;
  end;

  { TEpiCheckFile }

  TEpiCheckFile = class(TObject)
  private
    FValueLabelSets:       TValueLabelSets;
    FTopComments:          TStringList;       //Commentlines in the top of the checkfile
    FBeforeFileCmds:       TChkCommands;      //Commands to be run when file is opened
    FAfterFileCmds:        TChkCommands;      //Commands to be run when file is closed
    FBeforeRecordCmds:     TChkCommands;      //Commands to be run before current record changes
    FAfterRecordCmds:      TChkCommands;      //Commands to be run when changing current record
    FRecodeCmds:           TChkCommands;      //Commands to be run during Recode Datafile
    FAssertList:           TStringList;       //used only to store Asserts for checkfilemode
    FConfirm:              Boolean;           //If true then a field is not left automatically when filled out
    FAutoSave:             Boolean;           //IF true then user is not asked "Save record to disk?"
    FGlobalMissingValues:  TMissingValues;
    FGlobalDefaultValue:   string;            //Global default value defined by DEFAULTVALUE ALL X or DEFAULTVALUE field-field, field X
    FGlobalTypeCom:        Boolean;           //Show that all fields has a Type Comment Fieldname
    FGlobalTypeComColor:   TColor;
    FMissingAction:        TMissingAction;
    FShowLastRecord:       Boolean;           //if set, then last record is shown when datafile is opened; if false (default) then
    FFieldHighlightAct:    Boolean;           //highlight active field
    FFieldHighlightCol:    TColor;            //color af highlight of active field
    FBackupList:           TStringList;       //List of files to backup
    FErrorInFile:          Boolean;
    FHasCheckFile:         Boolean;
    FFileName:             string;
    FDefines:              TEpiFields;       
    function    GetGlobMissing(Index: Integer): string;
    procedure   SetGlobMissing(Index: Integer; const Value: string);
    procedure   InternalReset();
  protected

  public
    Constructor Create();
    Destructor  Destroy(); override;
    procedure   Reset();
    function    DefineExists(Const aName: string): Boolean;
    function    DefineByName(Const aName: string): TEpiField;
    procedure   AddDefine(Field: TEpiField);
    Property    ValueLabels:      TValueLabelSets read FValueLabelSets;
    Property    BeforeFileCmds:   TChkCommands read FBeforeFileCmds write FBeforeFileCmds;        //Commands to be run when file is opened
    Property    AfterFileCmds:    TChkCommands read FAfterFileCmds write FAfterFileCmds;          //Commands to be run when file is closed
    Property    BeforeRecordCmds: TChkCommands read FBeforeRecordCmds write FBeforeRecordCmds;    //Commands to be run before current record changes
    Property    AfterRecordCmds:  TChkCommands read FAfterRecordCmds write FAfterRecordCmds;      //Commands to be run when changing current record
    Property    RecodeCmds:       TChkCommands read FRecodeCmds write FRecodeCmds;                //Commands to be run during Recode Datafile
    Property    Confirm:          Boolean read FConfirm write FConfirm;
    Property    Autosave:         Boolean read FAutoSave write FAutoSave;
    Property    GlobalDefaultVal: string read FGlobalDefaultValue write FGlobalDefaultValue;
    Property    GlobalMissingVal[Index: Integer]: string read GetGlobMissing write SetGlobMissing;
    Property    MissingAction:    TMissingAction read FMissingAction write FMissingAction;
    Property    GlobalTypeCom:    Boolean read FGlobalTypeCom write FGlobalTypeCom;
    Property    GlobalTypeComColor: TColor read FGlobalTypeComColor write FGlobalTypeComColor;
    Property    ShowLastRecord:   Boolean read FShowLastRecord write FShowLastRecord;
    Property    FieldHighlightAct: Boolean read FFieldHighlightAct write FFieldHighlightAct;
    Property    FieldHighlightCol: TColor read FFieldHighlightCol write FFieldHighlightCol;
    Property    BackupList:       TStringList read FBackupList;
    Property    TopComments:      TStringList read FTopComments;
    Property    AssertList:       TStringList read FAssertList;
    Property    ErrorInFile:      Boolean read FErrorInFile write FErrorInFile;
    Property    HasCheckFile:     Boolean read FHasCheckFile write FHasCheckFile;
    Property    FileName:         string read FFileName write FFileName;
  end;

  { TEpiField }

  TEpiField = class(TObject)
  private
    FData:         string;
    FOwner:        TEpiFields;
    FDataFile:     TEpiDataFile;
    FFieldNo:      Cardinal;        
    FDisplayChar:  Char;
    FFieldName:    string;
    FQuestX:       Cardinal;
    FQuestY:       Cardinal;
    FQuestColor:   Cardinal;
    FFieldX:       Cardinal;
    FFieldY:       Cardinal;
    FFieldColor:   Cardinal;
    FFieldType:    TFieldType;
    FFieldLength:  Cardinal;
    FCryptLength:  Cardinal;
    FNumDecimals:  Cardinal;
    FQuestion:     string;
    FVariableLabel: string;
    FCheckField:   TEpiCheckField;
    function       GetFieldTypeName(): string;
    function       GetData(): string;
    procedure      SetData(aData: string);
    function       GetValueLabel(): TValueLabelSet;
    function       GetValue: string;
    procedure      SetValue(const Value: string);
  protected

  public
    constructor Create(); virtual;
    destructor  Destroy(); override;
    procedure   Reset();
    procedure   Clone(Var Dest: TEpiField);
    property    AsData:      string read GetData write SetData;
    property    AsValue:     string read GetValue write SetValue;
    property    FieldNo:     Cardinal read FFieldNo write FFieldNo;
    property    DisplayChar: Char read FDisplayChar write FDisplayChar;
    property    FieldName:   string read FFieldName write FFieldName;
    property    QuestX:      Cardinal read FQuestX write FQuestX;
    property    QuestY:      Cardinal read FQuestY write FQuestY;
    property    QuestColor:  Cardinal read FQuestColor write FQuestColor;
    property    FieldX:      Cardinal read FFieldX write FFieldX;
    property    FieldY:      Cardinal read FFieldY write FFieldY;
    property    FieldColor:  Cardinal read FFieldColor write FFieldColor;
    property    FieldType:   TFieldType read FFieldType write FFieldType;
    property    FieldTypeName: string read GetFieldTypeName;
    property    FieldLength: Cardinal read FFieldLength write FFieldLength;
    property    CryptLength: Cardinal read FCryptLength write FCryptLength;
    property    NumDecimals: Cardinal read FNumDecimals write FNumDecimals;
    property    Question:    string read FQuestion write FQuestion;
    property    VariableLabel: string read FVariableLabel write FVariableLabel;
    property    CheckField:  TEpiCheckField read FCheckField write FCheckField;
    property    ValueLabelSet: TValueLabelSet read GetValueLabel;
    property    Owner:       TEpiFields read FOwner;
    property    DataFile:    TEpiDataFile read FDataFile write FDataFile;
  end;

  { TEpiFields }

  TEpiFields = class(TObject)
  private
    FOwned:     Boolean;
    FDataFile:  TEpiDataFile;
    FList:      TList;
    function    GetField(Index: Integer): TEpiField;
    function    GetCount(): Cardinal;
  protected
    procedure   Add(aField: TEpiField);
  public
    constructor Create(aOwner: TEpiDataFile); virtual;
    destructor  Destroy(); override;
    function    FieldByName(Const aFieldName: string): TEpiField;
    function    FieldExists(Const aFieldName: string): boolean;
    Property    Field[Index: integer]: TEpiField read GetField; default;
    Property    Count: Cardinal read GetCount;
    Property    Owned: Boolean read FOwned write FOwned;
  end;

  { TEpiIndexFile }

  TEpiIndexFile = class(TObject)
  private
    FFileName:       string;
    FIndexFields:    TList;
    FIndexUnique:    Byte;
    function         GetIndexField(Index: integer): TEpiField;
    procedure        SetIndexField(Index: Integer; aField: TEpiField);         
    function         GetIndexUnique(Index: integer): Boolean;
    procedure        SetIndexUnique(Index: Integer; aUnique: Boolean);         
    function         GetIndexCount(): Integer;
    procedure        InternalReset();
  protected
  public
    constructor Create(Const aFileName: string);
    destructor  Destroy; override;
    procedure   Reset();
    function    IndexNoByName(Const Name: string): Integer;
    function    IndexFieldByName(Const Name: string): TEpiField;
    function    IndexExists(Const Name: string): Boolean;
    property    FileName: string read FFileName write FFileName;
    property    IndexFields[Index: integer]: TEpiField read GetIndexField write SetIndexField;
    property    IndexUnique[Index: integer]: Boolean read GetIndexUnique write SetIndexUnique;
    property    IndexCount: Integer read GetIndexCount;
  end;

  { TEpiDataFile }

  TEpiDataFile = class(TObject)
  private
    FFileName:     string;                  // .REC filename.
    FDataStream:   TStream;                 // Stream (memory or file) reading .REC file.
    FFileLabel:    string;                  // Label of datafile.
    FFields:       TEpiFields;              // Container for all associated fields. (Owned)
    FDataFields:   TEpiFields;              // - holds list of data fields only. (Not Owned)
    FQuestFields:  TEpiFields;              // - holds list of question fields only. (Not Owned. FieldType = ftQuestion)
    FPassword:     string;                  // Datafile password (~kq:....:kq~)
    FFieldNaming:  TFieldNaming;            // Datafile fieldnaming convention - Firstword or Auto.
    FCheckFile:    TEpiCheckFile;           // Container for global/datafile level information of the .CHK file.
    FOptions:      TEpiDataFileOptions;     // Options set by Open(...)
    FOnProgress:   TProgressEvent;          // Callback event for updating progress
    FOnTranslate:  TTranslateEvent;         // Callback event for translation
    FOnPassword:   TRequestPasswordEvent;   // Callback event for password request
    FIndexFile:    TEpiIndexFile;
    FErrorText:    string;
    FErrorCode:    Cardinal;
    FCrypter:      TDCP_rijndael;
    FFileVersion:  Cardinal;
    FRecLength:    Cardinal;
    FFullrecLength: Cardinal;
    FOffset:       Cardinal;
    FRecordState:  TRecordState;
    FCurRecord:    Integer;
    FOrgDataType:  TDataFileType;
    function GetField(Index: integer): TEpiField;
    function GetIndexFile: TEpiIndexFile;
    function GetValueLabels: TValueLabelSets;
    procedure  InternalReset;
  protected
    function   InternalOpen: boolean;
    function   InternalSave: boolean;
    function   Lang(LangCode: Integer; Const LangText: string): string;
    function   RequestPassword(Const EncryptedString: string): boolean;
    Function   UpdateProgress(Percent: Integer; Msg: string): TProgressResult;
    Function   TextPos(var F: Textfile): Cardinal;
    function   GetNumFields: Cardinal;
    function   GetNumDataFields: Cardinal;
    function   GetNumRecords: Integer;
    function   GetEOFMarker: Boolean;
  public
    constructor Create(aOptions: TEpiDataFileOptions = []); virtual;
    destructor Destroy; override;
    function   Open(Const aFileName: string): boolean;
    function   Save(Const aFileName: string): boolean;
    procedure  SaveMemToFile(Const aFileName: string);
    procedure  Read(RecNumber: Integer);
    procedure  Write(RecNumber: Integer = NewRecord);
    procedure  Reset;
    function   FieldByName(Const aFieldName: string): TEpiField;
    function   FieldExists(Const aFieldName: string): boolean;
    procedure  AddField(AField: TEpiField);
    function   CreateUniqueFieldName(Const AText: string): string;
    property   Field[Index: integer]: TEpiField read GetField; default;
    property   Fields:      TEpiFields read FFields;
    property   DataFields:  TEpiFields read FDataFields;
    property   QuestFields: TEpiFields read FQuestFields;
    property   ValueLabels: TValueLabelSets read GetValueLabels;
    property   OnProgress:  TProgressEvent read FOnProgress write FOnProgress;
    property   OnPassword:  TRequestPasswordEvent read FOnPassword write FOnPassword;
    property   OnTranslate: TTranslateEvent read FOnTranslate write FOnTranslate;
    property   Options:     TEpiDataFileOptions read FOptions;
    property   FileName:    string read FFileName write FFileName;
    property   FileLabel:   string read FFileLabel write FFileLabel;
    property   Password:    string read FPassword write FPassword;
    property   FieldNaming: TFieldNaming read FFieldNaming write FFieldNaming;
    Property   NumFields:   Cardinal read GetNumFields;
    Property   NumDataFields: Cardinal read GetNumDataFields;
    Property   NumRecords:  Integer read GetNumRecords;
    Property   EOFMarker:   Boolean read GetEOFMarker;
    Property   CheckFile:   TEpiCheckFile read FCheckFile;
    Property   IndexFile:   TEpiIndexFile read GetIndexFile;
    Property   ErrorCode:   Cardinal read FErrorCode write FErrorCode;
    Property   ErrorText:   string read FErrorText write FErrorText;
    Property   FileVersion: Cardinal read FFileVersion write FFileVersion;
    Property   RecordState: TRecordState read FRecordState write FRecordState;
    Property   OrgDataType: TDataFileType read FOrgDataType write FOrgDataType;
  end;

implementation

uses
  SysUtils, UStringUtils, Base64, UEpiUtils,
  StrUtils, UCheckFileIO, Math;


{ TEpiCheckField }

function TEpiCheckField.GetMissingValue(Index: integer): string;
begin
  Result := FMissingValues[Index];
end;

procedure TEpiCheckField.SetMissingValue(Index: integer; Value: string);
begin
  FMissingValues[Index] := Value; 
end;

function TEpiCheckField.GetAutoSearch(): Boolean;
begin
  Result := AutoFields <> '';
end;

procedure TEpiCheckField.InternalReset();
begin
  if Assigned(FBeforeCmds)    then FreeAndNil(FBeforeCmds);
  if Assigned(FAfterCmds)     then FreeAndNil(FAfterCmds);
  if Assigned(FFieldComments) then FreeAndNil(FFieldComments);

  FMustEntert        := false;
  FNoEnter           := false;
  FTopOfScreen       := false;
  FTopOfScreenLines  := 0;
  FDoRepeat          := false;
  FConfirm           := false;
  FMin               := '';
  FMax               := '';
  FLegal             := '';
  FDefaultValue      := '';
  FAutoFields        := '';
  FAutoList          := false;
  FJumps             := '';
  FJumpResetChar     := #32;
  // Destruction is ALWAYS HANDLED BY TEpiCheckFile!!!
  FValueLabel        := nil;
  FShowValueLabel    := false;
  FValueLabelIsFieldRef  := false;
  FTypeType           := ttNone;
  FTypeText          := '';
  FTypeColor         := clBlue;
  FHasGlobalDefaultVal := false;
  FieldScope         := scNone;
end;

constructor TEpiCheckField.Create;
begin
  Reset();
end;

destructor TEpiCheckField.Destroy;
begin
  EpiLogger.IncIndent;
  EpiLogger.Add(ClassName, 'Destroy', 3);
  try
    InternalReset();
    inherited;
  finally
    EpiLogger.DecIndent;
  end;
end;

procedure TEpiCheckField.Clone(var Dest: TEpiCheckField);
var
  TmpCmd: TChkCommands;
  i: Integer;
begin
  if Not Assigned(Dest) then
    Dest := TEpiCheckField.Create();

  Dest.Reset();
  
  Dest.FMustEntert        := FMustEntert;
  Dest.FNoEnter           := FNoEnter;
  Dest.FTopOfScreen       := FTopOfScreen;
  Dest.FTopOfScreenLines  := FTopOfScreenLines;
  Dest.FDoRepeat          := FDoRepeat;
  Dest.FConfirm           := FConfirm;
  Dest.FMin               := FMin;
  Dest.FMax               := FMax;
  Dest.FLegal             := FLegal;
  Dest.FDefaultValue      := FDefaultValue;
  Dest.FAutoFields        := FAutoFields ;
  Dest.FAutoList          := FAutoList;
  Dest.FJumps             := FJumps;
  Dest.FJumpResetChar     := FJumpResetChar;
  Dest.FShowValueLabel    := FShowValueLabel;
  Dest.FValueLabelIsFieldRef := FValueLabelIsFieldRef;
  Dest.FTypeType          := FTypeType;
  Dest.FTypeText          := FTypeText;
  Dest.FTypeColor         := FTypeColor;
  Dest.FHasGlobalDefaultVal := FHasGlobalDefaultVal;
  Dest.FFieldScope        := FFieldScope;

  TmpCmd := Dest.FBeforeCmds;
  FBeforeCmds.Clone(TmpCmd);
  TmpCmd := Dest.FAfterCmds;
  FAfterCmds.Clone(TmpCmd);

  Dest.FFieldComments.Assign(FFieldComments);
  for i := 0 to 2 do
    Dest.FMissingValues[i] := FMissingValues[i];
end;

procedure TEpiCheckField.Reset();
begin
  InternalReset();

  FBeforeCmds := TChkCommands.Create;
  FAfterCmds  := TChkCommands.Create;
  FFieldComments := TStringList.Create();
end;

{ TEpiCheckFile }

constructor TEpiCheckFile.Create;
begin
  Reset();
end;

destructor TEpiCheckFile.Destroy;
begin
  EpiLogger.IncIndent;
  EpiLogger.Add(ClassName, 'Destroy', 3);
  try
    InternalReset();
    inherited;
  finally
    EpiLogger.DecIndent;
  end
end;

procedure TEpiCheckFile.Reset();
begin
  InternalReset();

  FValueLabelSets := TValueLabelSets.Create();
  FBackupList     := TStringList.Create;
  FTopComments    := TStringList.Create;
  FDefines        := TEpiFields.Create(nil);
  FDefines.Owned  := True;
end;

function TEpiCheckFile.DefineExists(const aName: string): Boolean;
begin
  result := FDefines.FieldExists(aName);
end;

function TEpiCheckFile.DefineByName(const aName: string): TEpiField;
begin
  result := FDefines.FieldByName(aName);
end;

procedure TEpiCheckFile.AddDefine(Field: TEpiField);
begin
  FDefines.Add(Field);
end;

function TEpiCheckFile.GetGlobMissing(Index: Integer): string;
begin
  Result := FGlobalMissingValues[Index];
end;

procedure TEpiCheckFile.SetGlobMissing(Index: Integer;
  const Value: string);
begin
  FGlobalMissingValues[Index] := Value;
end;

procedure TEpiCheckFile.InternalReset();
begin
  if Assigned(FValueLabelSets)   then FreeAndNil(FValueLabelSets);
  if Assigned(FTopComments)      then FreeAndNil(FTopComments);
  if Assigned(FBeforeFileCmds)   then FreeAndNil(FBeforeFileCmds);
  if Assigned(FAfterFileCmds)    then FreeAndNil(FAfterFileCmds);
  if Assigned(FBeforeRecordCmds) then FreeAndNil(FBeforeRecordCmds);
  if Assigned(FAfterRecordCmds)  then FreeAndNil(FAfterRecordCmds);
  if Assigned(FRecodeCmds)       then FreeAndNil(FRecodeCmds);
  if Assigned(FAssertList)       then FreeAndNil(FAssertList);
  if Assigned(FBackupList)       then FreeAndNil(FBackupList);
  if Assigned(FDefines)          then FreeAndNil(FDefines);

  FConfirm             := false;
  FAutoSave            := false;
  FGlobalDefaultValue  := '';
  FGlobalTypeCom       := false;
  FGlobalTypeComColor  := clBlue;
  FMissingAction       := maIgnoreMissing;
  FShowLastRecord      := false;
  FFieldHighlightAct   := false;
  FFieldHighlightCol   := clBlue;
  FErrorInFile         := false;
  FHasCheckFile        := false;
  FFileName            := '';
end;

{ TEpiField }

function TEpiField.GetFieldTypeName(): string;
begin
  if Assigned(Owner) then
    result := FieldTypeToFieldTypeName(FieldType, FOwner.FDataFile.OnTranslate)
  else
    result := FieldTypeToFieldTypeName(FieldType, nil);
end;

function TEpiField.GetData(): string;
begin
  Result := Trim(FData);
end;

procedure TEpiField.SetData(aData: string);
begin
  FData := aData;
end;

function TEpiField.GetValueLabel(): TValueLabelSet;
begin
  result := nil;
  if Assigned(CheckField) then
    result := CheckField.ValueLabel;
end;

constructor TEpiField.Create();
begin
  Reset();
end;

destructor TEpiField.Destroy;
begin
  EpiLogger.IncIndent;
  EpiLogger.Add(ClassName, 'Destroy', 3, 'Fieldname = ' + FieldName);
  try
    Reset();
    inherited;
  finally
    EpiLogger.DecIndent;
  end;
end;

function TEpiField.GetValue: string;
begin
  if Assigned(ValueLabelSet) then
    Result := ValueLabelSet.ValueLabel[AsData]
  else
    Result := AsData;
end;

procedure TEpiField.SetValue(const Value: string);
begin
  if Assigned(ValueLabelSet) then
    ValueLabelSet.AddValueLabelPair(AsData, Value);
end;

procedure TEpiField.Clone(var Dest: TEpiField);
var
  TmpCheckField: TEpiCheckField;
  I: Integer;  
  Ptr: Pointer;
begin
  if not Assigned(Dest) then
    Dest := TEpiField.Create();

  // Reset all values... but retain Datafile linkage.
  Ptr := Dest.DataFile;
  Dest.Reset();
  Dest.DataFile := TEpiDataFile(Ptr);

  // Copy Field related values:
  Dest.FDisplayChar := FDisplayChar;
  Dest.FFieldName   := FFieldName;
  Dest.FQuestX      := FQuestX;
  Dest.FQuestY      := FQuestY;
  Dest.FQuestColor  := FQuestColor;
  Dest.FFieldX      := FFieldX;     
  Dest.FFieldY      := FFieldY;
  Dest.FFieldColor  := FFieldColor;
  Dest.FFieldType   := FFieldType;
  Dest.FFieldLength := FFieldLength;
  Dest.FCryptLength := FCryptLength;
  Dest.FNumDecimals := FNumDecimals;
  Dest.FQuestion    := FQuestion;
  Dest.FVariableLabel := FVariableLabel;

  // Copy CheckFile if present:
  TmpCheckField := nil;
  if Assigned(CheckField) then
    CheckField.Clone(TmpCheckField);
  Dest.CheckField := TmpCheckField;

  // Set Valuelabels here since CheckField does not have Owner info.
  // Scenarios:
  // - 1: Dest field has a link to a Datafile. Then any potential valuelabels should be found
  //      through Dest.Datafile's valuelabelsets.
  // - 2: Dest has NO Datafile. This could be a temporary clone, etc.
  //      Copy the Valuelabelset, assign it to Dest field (and posibly also create
  //      the CheckField) and let and TEpiDataFile.AddField handle Valuelabels.
  //      This will reset label type (to vltLocal), since at present it is not known
  //      how the valuelabelset is related to anything else...
  if Assigned(ValueLabelSet) then
  begin
    if Assigned(Dest.DataFile) then
    begin
      Dest.CheckField.FValueLabel := Dest.DataFile.ValueLabels.ValueLabelSetByName(ValueLabelSet.Name)
    end else begin
      If not Assigned(Dest.CheckField) then
        Dest.CheckField := TEpiCheckField.Create();
      ValueLabelSet.Clone(Dest.CheckField.FValueLabel);
      Dest.ValueLabelSet.LabelType := vltLocal;
    end;
  end;

  // Index?
  I := Owner.FDataFile.IndexFile.IndexNoByName(FieldName);
  if (I > 0) and (Assigned(Dest.Owner)) then
  begin
   Dest.Owner.FDataFile.IndexFile.IndexFields[I] := Dest;
   Dest.Owner.FDataFile.IndexFile.IndexUnique[I] := Owner.FDataFile.IndexFile.IndexUnique[I];
  end;
end;

procedure TEpiField.Reset;
begin
  if Assigned(FCheckField) then FreeAndNil(FCheckField);

  FData          := '';
  FOwner         := nil;
  FDataFile      := nil;
  FFieldNo       := 0;
  FDisplayChar   := '_';
  FFieldName     := '';
  FQuestX        := 0;
  FQuestY        := 0;
  FQuestColor    := 0;
  FFieldX        := 0;
  FFieldY        := 0;
  FFieldColor    := 0;
  FFieldType     := ftInteger;
  FFieldLength   := 0;
  FCryptLength   := 0;
  FNumDecimals   := 0;
  FQuestion      := '';
  FVariableLabel := '';
end;

{ TEpiFields }

function TEpiFields.GetField(Index: integer): TEpiField;
begin
  result := TEpiField(FList[Index]);
end;

function TEpiFields.GetCount(): Cardinal;
begin
  result := FList.Count
end;

constructor TEpiFields.Create(aOwner: TEpiDataFile);
begin
  FList := TList.Create();
  FDataFile := aOwner;
end;

destructor TEpiFields.Destroy;
var
  F: TEpiField;
begin
  while FList.Count > 0 do
  begin
    if Owned then
    begin
      F := TEpiField(FList.Last);
      FreeAndNil(F);
    end;
    FList.Delete(FList.Count - 1);
  end;
  FreeAndNil(FList);
  inherited;
end;

function TEpiFields.FieldByName(Const aFieldName: string): TEpiField;
var
  i: integer;
begin
  result := nil;
  for i := 0 to FList.Count - 1 do
    if AnsiCompareText(TEpiField(FList[i]).FieldName, aFieldName) = 0 then
    begin
      result := TEpiField(FList[i]);
      exit;
    end
end;

function TEpiFields.FieldExists(Const aFieldName: string): boolean;
begin
  result := Assigned(FieldByName(aFieldName)); 
end;

procedure TEpiFields.Add(aField: TEpiField);
begin
  if Owned then
  begin
    aField.FOwner := self;
    aField.FDataFile := Self.FDataFile;
  end;
  FList.Add(aField);
end;


{ TEpiDataFile }

function TEpiDataFile.GetField(Index: Integer): TEpiField;
begin
  result := Fields[Index];
end;


function TEpiDataFile.GetIndexFile: TEpiIndexFile;
begin
  if not Assigned(FIndexFile) then
    FIndexFile := TEpiIndexFile.Create(ChangeFileExt(FileName, '.EIX'));
  result := FIndexFile;
end;

function TEpiDataFile.GetValueLabels: TValueLabelSets;
begin
  result := nil;
  if Assigned(CheckFile) then
    Result := CheckFile.ValueLabels;
end;



procedure TEpiDataFile.InternalReset;
begin
  if Assigned(FDataStream) then FreeAndNil(FDataStream);
  if Assigned(FFields) then FreeAndNil(FFields);
  if Assigned(FDataFields) then FreeAndNil(FDataFields);
  if Assigned(FQuestFields) then FreeAndNil(FQuestFields);
  if Assigned(FCheckFile) then FreeAndNil(FCheckFile);
  if Assigned(FIndexFile) then FreeAndNil(FIndexFile);
  if Assigned(FCrypter) then FreeAndNil(FCrypter);

  FFileName       := '';
  FFileLabel      := '';
  FPassword       := '';
  FFieldNaming    := fnAuto;
  FOptions        := [];
  FOnProgress     := nil;
  FOnTranslate    := nil;
  FOnPassword     := nil;
  FErrorText      := '';
  FErrorCode      := 0;
  FFileVersion    := 0;
  FRecLength      := 0;
  FFullrecLength  := 0;
  FOffset         := 0;
  FRecordState    := rsNormal;
  FCurRecord      := NewRecord;
  FOrgDataType    := dftEpiData;
end;

function TEpiDataFile.InternalOpen: boolean;
var
  // Misc:
  TempInt, I: integer;
  TxtFile: TextFile;
  EField: TEpiField;
  FieldNumberCounter: cardinal;
  ChkIO: TCheckFileIO;

  // Reading the textfile:
  TxtLine: string;
  HeaderLineCount: Integer;
  ValCode: Integer;
  CurrentLine: Integer;

  // Field lines:
  TmpFieldChar, Dummy: Char;
  TmpFieldType,
  TmpFieldColor, TmpQuestX, TmpQuestY, TmpLength,
  TmpFieldX, TmpFieldY, TmpQuestColor: Integer;
  TmpName: string[10];
  TmpQuestion, TmpStr: string;
begin
  EpiLogger.IncIndent;
  EpiLogger.Add(ClassName, 'InternalOpen', 3);
  result := false;

  OrgDataType := dftEpiData;

  AssignFile(TxtFile, Filename);
  {$I-}
  System.Reset(TxtFile);
  {$I+}
  if IOResult() > 0 then
  begin
    FErrorText := Format(Lang(20108,'Data file %s could not be opened.'),[Filename]) + #13 +
                         Lang(20208,'Please check if the file is in use and that the file name is legal.');
    FErrorCode := EPI_DATAFILE_FORMAT_ERROR;
    EpiLogger.AddError(ClassName, 'InternalOpen', ErrorText, 20108);
    Exit;
  end;

  try
    // --- Read "First Line" header ---
    ReadLn(TxtFile, TxtLine);

    // - Version number
    FFileVersion := 0;
    TempInt := Pos('~VQ:', AnsiUpperCase(TxtLine));
    if TempInt > 0 then
    begin
      Val(Copy(TxtLine, TempInt + 5, Pos(':VQ~', AnsiUpperCase(TxtLine))), FFileVersion, ValCode);
      if ValCode > 0 then
      begin
        FErrorText := Format(Lang(0, 'File version information invalid %s'), [Filename]);
        FErrorCode := EPI_FILE_VERSION_ERROR;
        EpiLogger.AddError(ClassName, 'InternalOpen', ErrorText, 0);
        CloseFile(TxtFile);
        Exit;
      end;
    end;

    // - Password
    TempInt := Pos('~KQ:', AnsiUpperCase(TxtLine));
    if TempInt > 0 then
    begin
      if not RequestPassword(Copy(TxtLine, TempInt + 4, Pos(':KQ~', AnsiUpperCase(TxtLine)) - (TempInt + 4))) then
      begin
        FErrorText := Lang(9020, 'Incorrect password entered');
        FErrorcode := EPI_INVALID_PASSWORD;
        EpiLogger.AddError(ClassName, 'InternalOpen', ErrorText, 9020);
        CloseFile(TxtFile);
        Exit;
      end;
    end;

    // - FileLabel
    if Pos('FILELABEL: ', AnsiUpperCase(TxtLine)) > 0 then
      FFileLabel := Copy(TxtLine, Pos('FILELABEL: ', AnsiUpperCase(TxtLine)) + Length('FILELABEL: ') , Length(TxtLine));

    // - Autonaming or Firstword
    if Pos(' VLAB', TxtLine) > 0 then
      FFieldNaming := fnFirstWord
    else
      FFieldNaming := fnAuto;

    // - Header lines:
    Val(Copy(TxtLine, 1, Pos(' ', TxtLine)-1), HeaderLineCount, ValCode);
    if ValCode > 0 then
    begin
      FErrorText := Format(Lang(20112, 'Incorrect format of datafile %s'), [Filename]);
      FErrorCode := EPI_DATAFILE_FORMAT_ERROR;
      EpiLogger.AddError(ClassName, 'InternalOpen', ErrorText, 20112);
      CloseFile(TxtFile);
      Exit;
    end;

    FieldNumberCounter := 1;
    // Read field defining header lines.
    for CurrentLine := 1 to HeaderLineCount do
    begin
      EpiLogger.Add(ClassName, 'InternalOpen', 3, 'Reading headerline no: ' + IntToStr(CurrentLine));
      if UpdateProgress((CurrentLine*100) DIV HeaderLineCount, lang(0,'Opening data file'))=prCancel then
      begin
        FErrorText := Lang(0, 'Cancelled by user');
        FErrorcode := EPI_USERCANCELLED;
        EpiLogger.AddError(ClassName, 'InternalOpen', ErrorText, 0);
        CloseFile(TxtFile);
        Exit;
      end;

      EField := TEpiField.Create();
      ReadLn(TxtFile,
             TmpFieldChar, TmpName, TmpQuestX, TmpQuestY,
             TmpQuestColor, TmpFieldX, TmpFieldY, TmpFieldType, TmpLength,
             TmpFieldColor, dummy, TmpQuestion);
      with EField do
      begin
        FieldNo     := CurrentLine - 1;
        DisplayChar := TmpFieldChar;
        QuestX      := TmpQuestX;
        QuestY      := TmpQuestY;
        QuestColor  := TmpQuestColor;
        FieldX      := TmpFieldX;
        FieldY      := TmpFieldY;
        FieldLength := TmpLength;
        FieldColor  := TmpFieldColor;
        Question    := StringReplace(TmpQuestion, '_', '-', [rfReplaceAll]);

        // Ensure valid variable name.
        if not CheckVariableName(TmpName, AlfaNumChars + [' ']) then
        repeat
          TmpName := 'V '+ IntToStr(FieldNumberCounter);
          INC(FieldNumberCounter);
        until not Fields.FieldExists(TmpName);
        FieldName := Trim(TmpName);

        // Field types.
        if TmpFieldType >= 100 then
        begin
          // Type > 100 => float field
          NumDecimals := TmpFieldType-100;
          Fieldtype := ftFloat;
        end else begin
          // Normal field type recognition.
          FieldType := ftInteger;
          NumDecimals := 0;
          WHILE TmpFieldType > ORD(FieldType) DO
            FieldType := Succ(FieldType);
        end;

        // This is not a data field, but a question field.
        if FieldLength = 0 then FieldType := ftQuestion;

        // Unsupported field are automatically converted to string (ftAlfa) fields.
        if (not (FieldType in SupportedFieldTypes)) or
           ((fieldtype in DateFieldTypes) and (FieldLength < 10)) then
          FieldType := ftAlfa;

        // Encrypted field store length in field color property.
        if FieldType = ftCrypt then
        begin
          if FieldColor > 111 then
            CryptLength := FieldColor-111
          else
            CryptLength := FieldColor;
        end;

        // Variable label handling.
        VariableLabel := Trim(Question);
        if (FieldNaming = fnFirstWord) and (trim(VariableLabel) <> '') THEN
        begin
          TmpStr := FirstWord(VariableLabel, MaxFieldNameLen);
          Delete(FVariableLabel, Pos(TmpStr, VariableLabel), System.Length(TmpStr));
          VariableLabel := trim(VariableLabel);
        END;
        IF FieldName <> TmpName THEN VariableLabel := TmpName + ' ' + VariableLabel;

        // Summerize field findings.
        FRecLength := FRecLength + FieldLength;
      end;  // With EField
      AddField(EField);
    end; // For CurrentLine
    FFullrecLength := FRecLength + (((FRecLength - 1) DIV MaxRecLineLength) + 1) * 3;
    FOffSet := TextPos(TxtFile);
    FDataStream.Position := FOffset;
    CloseFile(TxtFile); 

    if NumRecords = -1 then
    begin
      FErrorText := Format(Lang(20118, 'Error in datafile %s.~~One or more records are corrupted.'), [Filename]);
      FErrorCode := EPI_DATAFILE_FORMAT_ERROR;
      EpiLogger.AddError(ClassName, 'InternalOpen', ErrorText, 20118);
      Exit;
    end;

    result := true;

    if not (eoIgnoreChecks in FOptions) then
    begin
      try
        try
          ChkIO := TCheckFileIO.Create();
          ChkIO.OnTranslate := Self.OnTranslate;
          result := ChkIO.ReadCheckFile(ChangeFileExt(FileName, '.chk'), Self);
          if not Result then
          begin
            ErrorCode := EPI_CHECKFILE_ERROR;
            for i := 0 to ChkIO.ErrorLines.Count -1 do
              ErrorText := ErrorText + #13#10 + ChkIO.ErrorLines[i];
            EpiLogger.AddError(ClassName, 'InternalOpen', ErrorText, 0);
          end;
        except
          ErrorCode := EPI_CHECKFILE_ERROR;
          result := false;
        end;
      finally
        FreeAndNil(ChkIO);
      end
    end;
  finally
    EpiLogger.DecIndent;
  end;
end;

function TEpiDataFile.InternalSave: boolean;
var
  Crypt: boolean;
  i: integer;
  S, EncData: string;
  Stream: TFileStream;
  ChkIO: TCheckFileIO;
begin
  EpiLogger.IncIndent;
  EpiLogger.Add(Classname, 'InternalSave', 3);
  result := false;

  IF Fields.Count = 0 THEN
  BEGIN
    Raise Exception.Create('No fields defined');
    Exit;
  END;

  Stream := nil;
  ChkIO := nil;

  try
    // - Encryption required:
    Crypt := false;
    for i := 0 to Fields.Count -1 do
      if Fields[i].FieldType = ftCrypt then
        Crypt := true;

    IF Crypt and (Password = '') THEN
    BEGIN
      if Assigned(OnPassword) then OnPassword(self, rpCreate, FPassWord);
      if Password = '' then
        raise Exception.Create('A password is needed for data files with encrypted fields');
      FCrypter.InitStr(Password);  
    END;

    // - Header lines (and colour):
    S := IntToStr(NumFields) + ' 1 ';

    // - Autonaming or Firstword
    IF FieldNaming = fnFirstWord THEN
      S := S + 'VLAB ';

    // - Password
    IF Password <> '' THEN
    begin
      EncData := Trim(Password);
      FCrypter.EncryptCFB(EncData[1], EncData[1], Length(EncData));
      EncData := B64Encode(EncData);
      FCrypter.Reset;
      S := S + '~KQ:' + EncData + ':KQ~ ';
    end;

    // TODO : Version
{    S := S + '~VQ:' + IntToStr(FileVersion) + ':VQ~ ';   }

    // - FileLabel
    IF Trim(FileLabel) <> '' THEN
      S := S + 'Filelabel: ' + FileLabel;

    S := S + #13#10;
    FDataStream.Write(S[1], Length(S));
    FOffset := Length(S);

    //TODO: Validate QuestX, QuestY, FieldX, fieldY
    FOR i := 0 TO NumFields - 1 DO
    WITH Fields[i] DO
    BEGIN
      EpiLogger.Add(Classname, 'InternalSave', 3, 'Writing heading no. ' + IntToStr(i+1));
      // - Fieldchar
      IF (FieldType = ftInteger) OR (FieldType = ftFloat) OR
         (FieldType = ftIDNUM) THEN
        s := '#'
      ELSE
        s := '_';
      s := s + Format('%-10s', [FieldName]);  //Name of field (left justified)
      s := s + ' ';                           //Space required for some unknown reason
      s := s + Format('%4d', [QuestX]);       //Question X-position
      s := s + Format('%4d', [QuestY]);       //Question Y-position
      s := s + Format('%4s', ['30']);         //Question colorcode
      s := s + Format('%4d', [FieldX]);       //Entry X-position
      s := s + Format('%4d', [FieldY]);       //Entry Y-position

      //Write FieldType
      // 0 = Question without entryfield, i.e. text only
      // 100+Number of decimals = Floating point number
      // For all other: use the fieldtype-code (fieldtype)
      IF FieldType = ftQuestion THEN
        s := s + Format('%4s', ['0'])
      ELSE IF (FieldType = ftFloat) AND (NumDecimals>0) THEN
        s := s + Format('%4d', [100 + NumDecimals])
      ELSE
        s := s + Format('%4d', [ORD(fieldtype)]);

      //Write length of field - use 0 for text only
      IF FieldType = ftQuestion THEN
        s := s + Format('%4s', ['0'])
      ELSE BEGIN
        s := s + Format('%4d', [FieldLength]);
        FRecLength := FRecLength + FieldLength;
      END;

      //write entry colorcode - special use in encrypted fields (holds entrylength of field)
      IF FieldType <> ftCrypt THEN
        s := s + Format('%4s', ['112'])
      ELSE
        IF CryptLength < 15 THEN
          s := s + Format('%4d', [111 + CryptLength])
        ELSE
          s := s + Format('%4d', [CryptLength]);

      s := s + ' ';                      //Another unnescessary blank
      if Question = '' then
        Question := VariableLabel;
      s := s + Question;

      s := s + #13#10;
      FDataStream.Write(S[1], Length(S));
      FOffset := FOffset + Length(S);
    END; // End With Field...

    FFullrecLength := FRecLength + (((FRecLength - 1) DIV MaxRecLineLength) + 1) * 3;
    FCurRecord := NewRecord;

    if not (eoIgnoreChecks in FOptions) then
    begin
      CheckFile.FileName := ChangeFileExt(FileName, '.chk');
      Stream := TFileStream.Create(CheckFile.FileName, fmCreate);
      ChkIO := TCheckFileIO.Create();
      ChkIO.WriteCheckToStream(Stream, Self);
      if Stream.Size = 0 then
      begin
        FreeAndNil(Stream);
        DeleteFile(CheckFile.FileName);
        CheckFile.FileName := '';
      end;
    end;
  finally
    EpiLogger.DecIndent;
    if Assigned(Stream) then FreeAndNil(Stream);
    if Assigned(ChkIO) then FreeAndNil(ChkIO);
  end;
end;

function TEpiDataFile.Lang(LangCode: Integer; Const LangText: string): string;
begin
  Result := LangText;
  IF Assigned(FOnTranslate) THEN
    Result := FOnTranslate(langcode, Result)
end;

function TEpiDataFile.RequestPassword(Const EncryptedString: string): boolean;
var
  S: string;
begin
  result := false;
  if Trim(Password) = '' then
    if Assigned(FOnPassword) then
      FOnPassword(Self, rpOpen, FPassword);
  try
    S := B64Decode(EncryptedString);
    FCrypter.InitStr(Password);
    FCrypter.DecryptCFB(S[1], S[1], Length(S));
    FCrypter.Reset;
    Result := (AnsiCompareText(Password, S) = 0);
  except
    FErrorText := Lang(0, 'Fatal Error in decrypting password.');
    FErrorCode := EPI_INVALID_PASSWORD;
    EpiLogger.AddError(ClassName, 'RequestPassword', ErrorText, 0);
    Abort;
  end;
end;

function TEpiDataFile.UpdateProgress(Percent: Integer; Msg: string): TProgressResult;
begin
  Result := prNormal;
  if Assigned(FOnProgress) then
  Begin
    result := FOnProgress(Self, Percent, Msg);
  end;
end;

function TEpiDataFile.TextPos(var F: Textfile): Cardinal;
begin
  with TTextRec(F) do
  begin
    Result := FileSeek(Handle, 0, 1);
    if Mode = FMOutput then
      inc(Result, BufPos)
    else if BufEnd <> 0 then
      Dec(Result, BufEnd-BufPos);
  end;
end;

function TEpiDataFile.GetNumFields: Cardinal;
begin
  result := Fields.Count;
end;

function TEpiDataFile.GetNumDataFields: Cardinal;
begin
  result := DataFields.Count;
end;

function TEpiDataFile.GetNumRecords: Integer;
var
  buf: Array[0..1] of Char;
  TmpPos: Cardinal;
BEGIN
  Result := 0;
  if FDataStream.Size = FOffset then exit;

  TmpPos := FDataStream.Position;
  if EOFMarker then
    FDataStream.Position := FDataStream.Size - 3
  else
    FDataStream.Position := FDataStream.Size - 2;
  FDataStream.ReadBuffer(buf, 2);
  FDataStream.Position := TmpPos;

  Result := FDataStream.Size - FOffset;
  if (Buf[0] <> #13) or (Buf[1] <> #10) then inc(Result, 2);
  if EOFMarker then Dec(Result);

  if (Result mod FFullRecLength) <> 0 then
    Result := -1
  else
    Result := Result div FFullRecLength;
end;

function TEpiDataFile.GetEOFMarker: Boolean;
var
  TmpPos: cardinal;
  Buf: Char;
begin
  if not Assigned(FDataStream) then exit;

  TmpPos := FDataStream.Position;
  FDataStream.Position := FDataStream.Size - 1;
  FDataStream.ReadBuffer(Buf, 1);
  FDataStream.Position := TmpPos;

  result := (Buf = #26);
end;

constructor TEpiDataFile.Create(aOptions: TEpiDataFileOptions);
var
  p: pointer;
begin
  EpiLogger.IncIndent;
  EpiLogger.Add(ClassName, 'Create', 3);
  try
    Reset();

    FOptions := aOptions;

    if eoInMemory in Options then
      FDataStream := TMemoryStream.Create();

    FFieldNaming := fnFirstWord;
  finally
    EpiLogger.DecIndent;
  end;
end;

destructor TEpiDataFile.Destroy;
begin
  EpiLogger.IncIndent;
  EpiLogger.Add(ClassName, 'Destroy', 2, 'Filename = ' + FileName);
  try
    InternalReset();
    inherited Destroy;
  finally
    EpiLogger.DecIndent;
  end;
end;

function TEpiDataFile.Open(const aFileName: string): boolean;
var
  TmpStream: TFileStream;
  Ext: string;
begin
  EpiLogger.IncIndent;
  EpiLogger.Add(Classname, 'Open', 2, 'Filename = "' + aFilename + '"');
  try
    FFileName := aFileName;

    if eoInMemory in Options then
      TMemoryStream(FDataStream).LoadFromFile(FileName)
    else
      FDataStream := TFileStream.Create(FileName, fmOpenReadWrite or fmShareDenyWrite);

    Ext := ExtractFileExt(FileName);
    if AnsiUpperCase(Ext) = '.REC' then
      result := InternalOpen()
    else begin
      FErrorText := Format(Lang(0, 'Unsupported file type for direct reading: %s'), [Ext]);
      FErrorCode := EPI_OPEN_FILE_ERROR;
      result := False;
      EpiLogger.AddError(Classname, 'Open', ErrorText, 0);
    end;
  finally
    EpiLogger.DecIndent;
  end;
end;

function TEpiDataFile.Save(Const aFileName: string): boolean;
var
  Ext: string;
begin
  EpiLogger.IncIndent;
  EpiLogger.Add(Classname, 'Save', 2, 'Filename = "' + aFilename + '"');
  try
    FFileName := aFileName;

    if (Trim(FileName) <> '') and (not (eoInMemory in Options)) then
      FDataStream := TFileStream.Create(FileName, fmCreate);

    Result := InternalSave();
  finally
    EpiLogger.DecIndent;
  end;
end;

procedure TEpiDataFile.SaveMemToFile(Const aFileName: string);
begin
  if Trim(aFileName) = '' then exit;

  if not (FDataStream is TMemoryStream) then exit;

  TMemoryStream(FDataStream).SaveToFile(aFileName); 
end;

procedure TEpiDataFile.Read(RecNumber: Integer);
var
  BufPos, I: integer;
  CharBuf: Array of char;
  StrBuf, EncData: string;
  ok: boolean;
begin
  // Sanity checks:
  if (RecNumber < 1) or (RecNumber > NumRecords) then
  begin
    FErrorText := Lang(0, 'Index error in reading.');
    FErrorCode := EPI_READ_FILE_ERROR;
    EpiLogger.AddError(Classname, 'Read', ErrorText, 0);
    raise Exception.Create('Index error in reading.');
  end;

  if FDataStream.Position <> (FOffset + ((RecNumber - 1) * FFullRecLength)) then
    FDataStream.Position := FOffset + ((RecNumber - 1) * FFullRecLength);

  SetLength(CharBuf, FFullRecLength);
  I := FDataStream.Read(CharBuf[0], FFullRecLength);
  if I <> FFullRecLength then
  begin
    FErrorText := Lang(20464, 'Error reading record');
    FErrorCode := EPI_READ_FILE_ERROR;
    EpiLogger.AddError(Classname, 'Open', ErrorText, 20464);
    raise Exception.Create('Error reading record');
  end;

  RecordState := rsNormal;
  StrBuf := CharBuf[High(CharBuf) - 2];
  if StrBuf = '?' then
    RecordState := rsDeleted
  else if StrBuf = '^' then
    RecordState := rsVerified;

  StrBuf := StringReplace(string(CharBuf), EOLChars, '', [rfReplaceAll]);

  BufPos := 1;
  for i := 0 TO Fields.Count - 1 DO
  with Fields[i] do begin
    if FieldType = ftQuestion then continue;
    AsData := Copy(StrBuf, BufPos, FieldLength);
    AsData := Trim(AsData);
    IF (fieldtype = ftCrypt) AND (FPassword <> '') THEN
    begin
      EncData := B64Decode(AsData);
      FCrypter.DecryptCFB(EncData[1], EncData[1], Length(EncData));
      AsData := Trim(EncData);
      FCrypter.Reset;
    end;
    Inc(BufPos, FieldLength);
  end;

  FCurRecord := RecNumber;
end;

procedure TEpiDataFile.Write(RecNumber: Integer = NewRecord);
var
  Z, I: Integer;
  S, EncData: string;

begin
  // Sanity checks:
  if (RecNumber < 1) or (RecNumber > NumRecords) then
    RecNumber := NewRecord;

  if EOFMarker then Z := 1 else Z := 0;

  if RecNumber = NewRecord then
    FDataStream.Position := FDataStream.Size - Z
  else
    FDataStream.Position := FOffset + ((RecNumber - 1) * FFullRecLength);

  // TODO -O Torsten : Index on new record?.
  S := '';
  for i := 0 TO Fields.Count - 1 DO
  with Fields[i] do begin
    if FieldType = ftQuestion then continue;
    if FieldType = ftCrypt then
    begin
      EncData := Trim(AsData);
      FCrypter.InitStr(Password);
      FCrypter.EncryptCFB(EncData[1], EncData[1], Length(EncData));
      EncData := B64Encode(EncData);
      FCrypter.Reset;
      S := S + Format('%-*s', [FieldLength, EncData])
    end else if FieldType in [ftAlfa, ftUpperAlfa] then
      S := S + Format('%-*s', [FieldLength, AsData])
    else
      S := S + Format('%*s', [FieldLength, AsData]);
  end;
  Z := Length(S);
  if Z + 3 > MaxRecLineLength then
    for I := (Z div MaxRecLineLength) downto 1 do
      Insert(EOLchars, S, (MaxRecLineLength * I) + 1);

  case RecordState of
    rsNormal:   S := S + EOLchars;
    rsDeleted:  S := S + '?' + #13#10;
    rsVerified: S := S + '^' + #13#10;
  end;

  FDataStream.Write(S[1], Length(S));

  FCurRecord := RecNumber;
end;

procedure TEpiDataFile.Reset;
begin
  InternalReset();

  FFields       := TEpiFields.Create(Self);
  FFields.Owned := True;
  FDataFields   := TEpiFields.Create(Self);
  FQuestFields  := TEpiFields.Create(Self);
  FCheckFile    := TEpiCheckFile.Create();
  FCrypter      := TDCP_rijndael.Create(nil);
end;

function TEpiDataFile.FieldByName(Const aFieldName: string): TEpiField;
begin
  result := Fields.FieldByName(aFieldName);
end;

function TEpiDataFile.FieldExists(Const aFieldName: string): boolean;
begin
  result := Fields.FieldExists(aFieldName);
end;

procedure TEpiDataFile.AddField(AField: TEpiField);
begin
  Fields.Add(AField);
  if AField.FieldType = ftQuestion then
    QuestFields.Add(AField)
  else
    DataFields.Add(AField);

  // Handle Valuelabels in accordance with Field Cloning rule 2.
  if Assigned(AField.ValueLabelSet) then
  begin
    // This Valuelabelset is already present!
    if ValueLabels.ValueLabelSetByName(AField.ValueLabelSet.Name) =
       AField.ValueLabelSet then
      exit;
    ValueLabels.AddValueLabelSet(AField.ValueLabelSet);
  end;
end;

function TEpiDataFile.CreateUniqueFieldName(const AText: string): string;
var
  Number: Integer;
begin
  result := AText;

  // If fieldname is unique, do nothing.
  Number := 0;
  while FieldExists(result) do
  begin
    // not unique, find a new.
    if Length(Result) = MaxFieldNameLen then
      Result := Copy(Result, 1, MaxFieldNameLen - Length(IntToStr(Number))) + IntToStr(Number)
    else
      Result := Result + IntToStr(Number);
    Inc(Number);
  end;
end;

{ TEpiIndexFile }

function TEpiIndexFile.GetIndexField(Index: integer): TEpiField;
begin
  Result := nil;
  if (Index < 1) or (Index > MaxIndices) then
    exit;
  Result := TEpiField(FIndexFields[Index-1]);
end;

procedure TEpiIndexFile.SetIndexField(Index: Integer; aField: TEpiField);
begin
  if (Index < 1) or (Index > MaxIndices) then
    exit;
  FIndexFields[Index-1] := aField;
end;

function TEpiIndexFile.GetIndexUnique(Index: integer): Boolean;
begin
  // TODO -o Torsten : Implement using TBit!
  result := false;
  if (Index < 1) or (Index > MaxIndices) then
    exit;
  Result := (FIndexUnique and ($01 shl Index) > 0);
end;

procedure TEpiIndexFile.SetIndexUnique(Index: Integer; aUnique: Boolean);
begin
  if (Index < 1) or (Index > MaxIndices) then
    exit;
  FIndexUnique := FIndexUnique or (Integer(aUnique) shl Index);
end;


function TEpiIndexFile.GetIndexCount(): Integer;
var
  i: integer;
begin
  result := 0;
  for i := 1 to MaxIndices do
    if Assigned(IndexFields[i]) then Inc(Result);
end;

procedure TEpiIndexFile.InternalReset();
begin
  if Assigned(FIndexFields) then FreeAndNil(FIndexFields);
  FFileName := '';
  FIndexUnique := 0;
end;

constructor TEpiIndexFile.Create(const aFileName: string);
begin
  Reset();
  FFileName := aFileName;
end;

destructor TEpiIndexFile.Destroy;
begin
  InternalReset();
  inherited;
end;

procedure TEpiIndexFile.Reset();
var
  i: integer;
begin
  InternalReset();
  FIndexFields := TList.Create;
  for i := 1 to MaxIndices do
    FIndexFields.Add(nil);
end;

function TEpiIndexFile.IndexNoByName(Const Name: string): Integer;
var
  i: integer;
begin
  result := 0;
  for i := 1 to MaxIndices do
    if Assigned(IndexFields[i]) and
       (AnsiCompareText(IndexFields[i].FieldName, Name) = 0) then
      result := i;
end;

function TEpiIndexFile.IndexFieldByName(Const Name: string): TEpiField;
var
  i: integer;
begin
  result := nil;
  for i := 1 to MaxIndices do
    if Assigned(IndexFields[i]) and
       (AnsiCompareText(IndexFields[i].FieldName, Name) = 0) then
      result := IndexFields[i];
end;

function TEpiIndexFile.IndexExists(Const Name: string): Boolean;
begin
  result := Assigned(IndexFieldByName(Name));
end;

end.
