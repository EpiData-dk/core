unit UEpiDataFile;

interface

uses
  UEpiDataConstants, SysUtils, Classes, UeFields, UValueLabels, UEpiTypes, Graphics;

TYPE

  TEpiDataFileOption = (eoInMemory, eoIgnoreChecks, eoReadRelates, oeIgnoreIndex);
  TEpiDataFileOptions = set of TEpiDataFileOption;

  TRequestPasswordTypes=(rpOpen,rpCreate);
  TRequestPasswordEvent = procedure(Sender: TObject; requesttype:TRequestPasswordTypes; var password:String) of object;

  TErrorEvent = procedure(errorcode: Integer) of object;
  TTranslateEvent = function(langcode:Integer; text:widestring): widestring of object;

  TLeaveStyles=(lsEnter,lsBrowse,lsJumpFirst,lsJumpLast,lsChangeRec,lsNone);
  RecBuf=Array[0..20000] OF Char;
  PRecBuf=^RecBuf;

  TIndexFields=Array[1..MaxIndices] OF Integer;
  TIndexIsUnique=Array[1..MaxIndices] OF Boolean;
  TIndexFile=File of string[30];

  PRelateInfo=^TRelateInfo;
  TRelateInfo=Record
      RelFileNo:    Integer;
      RelFieldNo:   Integer;
      CmdInFieldNo: Integer;
      One2One:      Boolean;
      Next:         Pointer;
    END;

    //TODO: Det nedenstående flyttes til CheckObjUnit
    TMissingActions=(maIgnoreMissing,maRejectMissing);
    TDirections=(dirForward,dirBackward,dirFirst,dirLast,dirAbsolute);


//****************** TEpiDataFile ********************************

  TEpiDataFile = class(TObject)
  private
    { Private declarations }
    FRecFilename,FQESFilename,FCHKFilename,FIndexFilename: TFilename;
    //Datafile properties
    FDatFile:           TFilestream;
    FMemFile:           TMemorystream;
    FOffset:            Longint;        //pointer in datafile where data begins
    FRecLength:         Word;           //Length of one record in datafile incl. NewLine and terminator
    FShortRecLength:    Word;           //Length of one record excl. newline and terminator
    FHasEOFMarker:      Boolean;        //True if datafile has an EOF marker
    FNumRecords:        Integer;        //Total number of records in datafile
    FHasCheckFile:      Boolean;        //does the datafile have a checkfile?
    FRecBuf:            PRecBuf;        //Buffer for reading and writing to/from datafile
    FStoredInMemory:     Boolean;       //True if file is loaded into memory, false if file is on disc
    FFileLabel:         string;          //Label for datafile
    FFieldCheckSum:     Integer;
    FFileModified:      Boolean;        //True if data in file has been modified
    FFilesize:          LongInt;        //Size of datafile in bytes;

    {Field properties}
    FFieldList:         TeFields;          //List of eFields Records
    FFieldNames:        TStringList;    //List of fieldnames (excl. questions) - created only when needed
    FFieldNamesList:    TStringList;    //List of fieldnames+fieldlabel - created only when needed by dataformunit.FindField1Click
    FNumFields:         Integer;        //Number of fields in datafile incl. question-fields
    FNumDataFields:     Integer;        //Number of fields in datafile excl. question-fields
    FCurField:          Integer;        //Used in CheckFileMode to indicate current field
    FLeaveStyle:        TLeaveStyles;   //Indicates how a field is exited
    FCanExit:           Boolean;        //Flag to indicate if field can be exited
    FFocusedField:      Integer;        //Used in PeekApplyCheckFile to indicate current field

    {Record properties}
    FCurRecord:         Integer;        //Record number of current record
    FCurRecDeleted:     Boolean;        //Is current record marked as deleted?
    FCurRecModified:    Boolean;        //Has current record been modified?
    FDatafileModified:  Boolean;        //Has as record been saved in this session?
    FCurRecVerified:    Boolean;        //Is current record marked as verified (^)

    {Create fields properties}
    FEpiInfoFieldNaming:Boolean;        //Flag to indicate how fieldnames are created
    FUpdateFieldnameInQuestion: Boolean;
    FValueLabels:       TValueLabelSets;       //List of valueLabels (pLabelRecs) used
    FHasLongFieldNames: Boolean;        //Flag to indicate if 10-chars fieldnames occur

    {Index related vars}
    FIndexCount:        Byte;           //Number of used indices
    FIndex:             TMemoryStream;  //Index values sorted by rec-no.
    FSortIndex:         TMemoryStream;  //List of rec-numbers (integers) that points to Index (used to sort index)
    FIndexFields:       TIndexFields;   //Fieldnumber of fields with index
    FIndexIsUnique:     TIndexIsUnique; //TRUE means that index[n] is unique
    FIndexFile:         TIndexFile;     //IndexFile = File of Str30

    {Define vars}
    FHasDefined:        Boolean;        //True if checkfile contains define
    FDefList:           TStringList;    //List of defined variables - local for this instance of TEpiDataFile
    FGlobalDefList:     TStringList;    //List of global defined fields - common to mother + related files

    {Encrypt properties}
    FHasCrypt:          Boolean;        //Indicates that a encrypt fields exists in datafile    //&&
    FKey:               String;         //encrypt key

    {IDNumber properties}
    FIDNUMField:        Integer;        //does the datafile contain a IDNUM field?
    FFirstIDNumber:     LongInt;        //First IDNumber used in new datafiles
    FCurIDNumber:       Longint;        //Current IDNumber

    {Checkfile related properties}
    FChkTopComments:    TStringList;    //Commentlines in the top of the checkfile - used only in checkfilemode
    FHasRepeatField:    Boolean;        //If one or more fields have a Repeat check
    FBeforeFileCmds:    TList;          //Commands to be run when file is opened
    FAfterFileCmds:     TList;          //Commands to be run when file is closed
    FBeforeRecordCmds:  TList;          //Commands to be run before current record changes
    FAfterRecordCmds:   TList;          //Commands to be run when changing current record
    FRecodeCmds:        TList;          //Commands to be run during Recode Datafile
    FAssertList:        TStringList;    //used only to store Asserts for checkfilemode
    FConfirm:           Boolean;        //If true then a field is not let automatically when filled out
    FAutoSave:          Boolean;        //IF true then user is not asked "Save record to disk?"
    FGlobalMissingValues: TMissingValues;
    FGlobalDefaultValue:  string;    //Global default value defined by DEFAULTVALUE ALL X or DEFAULTVALUE field-field, field X
    FGlobalTypeCom:     Boolean;   //Show that all fields has a Type Comment Fieldname
    FGlobalTypeComColor: Integer;
    FHasIncludeCmd:     Boolean;
    FHasKeyUnique:      Boolean;        //True = one or more KEYs are KEY UNIQUE
    FErrorInCheckFile:  Boolean;        //True if checkfile exists, but it has errors
    FCheckWriter:       TObject;   //Object that handles writing check-files from internal data structure

    {Relatefile system properties}
    FIsRelateTop:       Boolean;          //True=this instance is the top of the relate hierarchi
    FTopEpiDataFile:    TObject;          //TObject(top TEpiDataFile)
    FIsRelateFile:      Boolean;          //True if this instance is a relate file, i.e. not top of hierarchi
    FRelateFiles:       TStringList;      //List of relate files; is only stored in FTopEpiDataFile;
    FRelateMothers:     TList;            //List of the relate files' mothers; stored only in FTopEpiDatafile;
    FRelateInfo:        PRelateInfo;      //Information on relates in the datafile
    FHasRelate:          Boolean;          //Indicates that the datafile contains at least one relate command

    {Type Statusbar properties}
    FTypeStatusBarField:Integer;        //Fieldno. of TYPE STATUSBAR field
    FTypeStatusBarText: ShortString;    //Prefix text in TYPE STATUSBAR
    FTypeStatusBarColor: TColor;        //Color to write Type StatusBar in

    {Error properties}
    FErrorCode:         Integer;
    FErrorText:         String;
    FonError:           TErrorEvent;

    {Events}
    FOnRequestPassword: TRequestPasswordEvent;
    FOnTranslate:       TTranslateEvent;

    {Colors}
    FQuestionText:      TColor;
    FQuestionBg:        TColor;
    FFieldText:         TColor;
    FFieldBg:           TColor;
    FBackGround:        TColor;

    {misc}
    FMissingAction:     TMissingActions;    //What to do with missing values
    FFieldHighlightAct: Boolean;        //highlight active field
    FFieldHighlightCol: TColor;         //color af highlight of active field
    FBackupList:        TStringList;    //List of files to backup
    FOpenOptions:       TEpiDataFileOptions;     //Used to store options given in Open-method
    FComLegalCounter:   Integer;          //Counter used to name value labels
    FShowLastRecord:    Boolean;        //if set, then last record is shown when datafile is opened; if false (default) then

    Function  TextPos(var F:Textfile):Longint;
    Procedure ResetEpiDataFile;
    Procedure DisposeFieldList(VAR AList:TList);
    //Procedure DisposeCommandList(VAR AList:TList);
    Function  GetFieldTypeNames(OrdOfFieldtype:Integer):String;
    Function  CountRecords:LongInt;
    Function  GetField(Index: Integer): TeField;
    Function  GetFieldByName(Fieldname: String): TeField;
    Function  GetFieldNumber(Fieldname: String): Integer;
    Function  GetDefFieldByName(Fieldname: string): TeField;
    Function  GetDefFieldNumber(Fieldname: string): Integer;
    Function  GetIndexFields(Index: Integer):Integer;
    Procedure SetIndexFields(Index: Integer; Value:Integer);
    Function  GetIndexIsUnique(Index: Integer):Boolean;
    Procedure SetIndexIsUnique(Index: Integer; Value:Boolean);
    Function  GetGlobalMissingValues(Index: Integer):string;
    Procedure SetGlobalMissingValues(Index: Integer; Value:string);
    Function  GetFileSize:LongInt;
    function  GetCheckLines:TStringList;
    function  GetQesLines: string;
  public
    CheckFileMode:Boolean;
    constructor Create;
    destructor  Destroy;  override;
    Function    Lang(langkode:Integer; CONST langtext:string):String;    //TODO: Private
    Function    Open(Const filename:String; OpenOptions:TEpiDataFileOptions):Boolean;
    //Methods related to creating new datafile and adding fields, record
    Function    SaveStructureToFile(filename: string; OverwriteExisting:boolean=false):boolean;
    procedure   SaveCheckFile;
    procedure   AddField(aField: TeField);
    //Read and write methods
    Procedure   Read(RecNum:LongInt);
    Procedure   Write(RecNum:LongInt);
    Function    ReadFromMem(AField:TeField; RecNo:LongInt; VAR RecIsDeleted:Boolean):String;   //TODO: Funktionalitet flyttet til Read
    //procedure   Next;
    //procedure   Prev;
    //procedure   First;
    //procedure   Last;
    //procedure   Append;
    //procedure   Post;
    //Checkfile related methods
    Function    LoadChecks:Boolean;      //TODO: Private
    Function    MakeIndexFile:Boolean;     //TODO: Private
    Function    ApplyIndex:Boolean;   //TODO: Private
    Function    ReadFromIndex(IndexNo,RecNo: Integer):string;   //TODO: Private
    Function    ReadCommonIndex(RecNo: Integer):String;   //TODO: Private
    Procedure   InitSortIndex;   //TODO: Private
    Procedure   DoSort(L,R:Integer);   //TODO: Private
    Function    ReadIndexNoFromSortIndex(SortPos: Integer):Integer;   //TODO: Private
    Procedure   WriteIndexNoToSortIndex(SortPos,num:Integer);   //TODO: Private
    Function    ReadCommonViaSortIndex(SortPos: Integer):String;   //TODO: Private
    Procedure   WriteToIndex(IndexNo,RecNo: Integer; s:string);   //TODO: Private
    Function    SearchIndex(IndexNo: Integer; SearchStr: string):LongInt;   //TODO: Private
    Function    SearchIndexFrom(IndexNo: Integer; SearchStr: string; RecNo:Integer; direction:TDirections):LongInt;   //TODO: Private
    Function    IndexHasDuplicates(IndexNo:Integer):Boolean;   //TODO: Private
    Procedure   DecryptIndex;   //TODO: Private
    Function    DoRebuildIndex: Boolean;   //TODO: Private måske? Kaldes fra Entry tools?
    PROCEDURE DestroyValueLabels(VAR ALabelList:TStringList);   //TODO: slettes

    //Properties
    Property Fields[Index: Integer]: TeField read GetField; default;
    Property FieldsByName[Fieldname: string]: TeField read GetFieldByName;
    Property FieldNumbers[Fieldname: string]: Integer read GetFieldNumber;
    Property DefFieldsByName[Fieldname: string]: TeField read GetDefFieldByName;
    Property DefFieldNumbers[Fieldname: string]: Integer read GetDefFieldNumber;
    Property StoredInMemory:Boolean read FStoredInMemory;
    Property NumRecords: LongInt read FNumRecords;
    Property HasCheckFile:Boolean read FHasCheckFile write FHasCheckFile;
    Property NumFields:Integer read FNumfields;
    Property NumDataFields:Integer Read FNumDataFields;
    Property Curfield:Integer read FCurField;
    Property LeaveStyle:TLeaveStyles read FLeaveStyle write FLeaveStyle;
    Property CanExit:Boolean read FCanExit write FCanExit;
    Property FocusedField:Integer read FFocusedField write FFocusedField;
    Property CurRecord:Integer read FCurRecord;   //write SetCurRecord
    Property CurRecDeleted:Boolean read FCurRecDeleted write FCurRecDeleted;
    Property CurRecVerified:boolean read FCurRecVerified write FCurRecVerified;
    Property CurRecModified:Boolean read FCurRecModified;
    Property DatafileModified:Boolean read FDatafileModified;
    Property RecFilename:TFilename read FRecFilename write FRecFilename;
    Property ChkFilename:TFilename read FChkFilename write FChkFilename;
    Property Indexfilename:TFilename read FIndexfilename write FIndexfilename;
    Property Filelabel:string read FFilelabel write FFilelabel;
    Property EpiInfoFieldNaming:Boolean read FEpiInfoFieldNaming write FEpiInfoFieldNaming;
    Property ErrorCode:Integer read FErrorCode write FErrorCode;
    Property ErrorText:String read FErrorText write FErrorText;
    Property OnRequestPassword: TRequestPasswordEvent read FOnRequestPassword write FOnRequestPassword;
    Property OnTranslate: TTranslateEvent read FOnTranslate write FOnTranslate;
    Property onError: TErrorEvent read FOnError write FOnError;
    Property HasIncludeCmd:Boolean read FHasIncludeCmd write FHasIncludeCmd;
    Property ChkTopComments:TStringList read FChkTopComments write FChkTopComments;
    Property BeforeFileCmds:TList read FBeforeFileCmds write FBeforeFileCmds;
    Property AfterFileCmds:TList read FAfterFileCmds write FAfterFileCmds;
    Property BeforeRecordCmds:TList read FBeforeRecordCmds write FBeforeRecordCmds;
    Property AfterRecordCmds:TList read FAfterRecordCmds write FAfterRecordCmds;
    Property RecodeCmds:TList read FRecodeCmds write FRecodeCmds;
    Property IndexCount:Byte read FIndexCount write FIndexCount;
    Property EIndex:TMemoryStream read FIndex write FIndex;
    Property SortIndex:TMemoryStream read FSortIndex write FSortIndex;
    Property IndexFields[Index:Integer]:Integer read GetIndexFields write SetIndexFields;
    Property IndexIsUnique[Index:Integer]:Boolean read GetIndexIsUnique write SetIndexIsUnique;
    Property IndexFile:TIndexFile read FIndexFile write FIndexFile;
    Property HasRepeatField:Boolean read FHasRepeatField write FHasRepeatField;
    Property ValueLabels:TValueLabelSets read FValueLabels write FValueLabels;
    Property AssertList:TStringList read FAssertList write FAssertList;
    Property TopEpiDataFile:TObject read FTopEpiDataFile write FTopEpiDataFile;
    Property DefList:TStringList read FDefList write FDefList;
    Property GlobalDefList:TStringList read FGlobalDefList write FGlobalDefList;
    Property Confirm:Boolean read FConfirm write FConfirm;
    Property Autosave:Boolean read FAutosave write FAutosave;
    Property GlobalMissingValues[Index: integer]:string read GetGlobalMissingValues write SetGlobalMissingValues;
    Property GlobalDefaultValue:string read FGlobalDefaultValue write FGlobalDefaultValue;
    Property MissingAction:TMissingActions read FMissingAction write FMissingAction;
    Property GlobalTypeCom:Boolean read FGlobalTypeCom write FGlobalTypeCom;
    Property GlobalTypeComColor:Integer read FGlobalTypeComColor write FGlobalTypeComColor;
    Property FieldHighlightAct: Boolean read FFieldHighlightAct write FFieldHighlightAct;
    Property FieldHighlightCol: TColor read FFieldHighlightCol write FFieldHighlightCol;
    Property BackupList: TStringList read FBackupList write FBackupList;
    Property IsRelateTop:Boolean read FIsRelateTop write FIsRelateTop;
    Property IsRelateFile:Boolean read FIsrelateFile write FIsrelateFile;
    Property RelateFiles: TStringList read FRelateFiles write FRelateFiles;
    Property RelateMothers: TList read FRelateMothers write FRelateMothers;
    Property RelateInfo: PRelateInfo read FRelateInfo write FRelateInfo;
    Property HasRelate:Boolean read FHasRelate write FHasRelate;
    Property QuestionText:TColor read FQuestionText write FQuestionText;
    Property QuestionBg:TColor read FQuestionBg write FQuestionBg;
    Property FieldText:TColor read FFieldText write FFieldText;
    Property FieldBg:TColor read FFieldBg write FFieldBg;
    Property BackGround:TColor read FBackGround write FBackGround;
    Property ComLegalCounter:Integer read FComLegalCounter write FComLegalCounter;
    Property TypeStatusBarField:Integer read FTypeStatusBarField write FTypeStatusBarField;
    Property TypeStatusBarText: ShortString read FTypeStatusBarText write FTypeStatusBarText;
    Property TypeStatusBarColor: TColor read FTypeStatusBarColor write FTypeStatusBarColor;
    Property DatafileSize:LongInt read GetFileSize;
    Property ErrorInCheckFile:Boolean read FErrorInCheckFile write FErrorInCheckFile;
    Property FieldtypeNames[OrdOfFieldtype:Integer]:String read GetFieldtypeNames;
    Property ShowLastRecord:boolean read FShowLastRecord write FShowLastRecord;
    Property password:string read FKey write FKey;
    Property RecordLength:word read FRecLength;
    Property CheckLines:TStringList read GetCheckLines;
    property QesLines: string read GetQesLines;
  published
    { Published declarations }
  end;


implementation



{************************************* TEpiDataFile *************************************}

procedure TEpiDataFile.AddField(aField: TeField);
begin

end;


function TEpiDataFile.ApplyIndex: Boolean;
begin

end;

function TEpiDataFile.CountRecords: LongInt;
begin

end;

constructor TEpiDataFile.Create;
begin

end;

procedure TEpiDataFile.DecryptIndex;
begin

end;

destructor TEpiDataFile.Destroy;
begin

  inherited;
end;

procedure TEpiDataFile.DestroyValueLabels(var ALabelList: TStringList);
begin

end;

procedure TEpiDataFile.DisposeFieldList(var AList: TList);
begin

end;

function TEpiDataFile.DoRebuildIndex: Boolean;
begin

end;

procedure TEpiDataFile.DoSort(L, R: Integer);
begin

end;


function TEpiDataFile.GetCheckLines: TStringList;
begin

end;

function TEpiDataFile.GetDefFieldByName(Fieldname: string): TeField;
begin

end;

function TEpiDataFile.GetDefFieldNumber(Fieldname: string): Integer;
begin

end;

function TEpiDataFile.GetField(Index: Integer): TeField;
begin

end;

function TEpiDataFile.GetFieldByName(Fieldname: String): TeField;
begin

end;

function TEpiDataFile.GetFieldNumber(Fieldname: String): Integer;
begin

end;

function TEpiDataFile.GetFieldTypeNames(OrdOfFieldtype: Integer): String;
begin

end;

function TEpiDataFile.GetFileSize: LongInt;
begin

end;

function TEpiDataFile.GetGlobalMissingValues(Index: Integer): string;
begin

end;

function TEpiDataFile.GetIndexFields(Index: Integer): Integer;
begin

end;

function TEpiDataFile.GetIndexIsUnique(Index: Integer): Boolean;
begin

end;

function TEpiDataFile.GetQesLines: string;
begin

end;

function TEpiDataFile.IndexHasDuplicates(IndexNo: Integer): Boolean;
begin

end;

procedure TEpiDataFile.InitSortIndex;
begin

end;

function TEpiDataFile.Lang(langkode: Integer;
  const langtext: string): String;
begin

end;

function TEpiDataFile.LoadChecks: Boolean;
begin

end;

function TEpiDataFile.MakeIndexFile: Boolean;
begin

end;


function TEpiDataFile.Open(const filename: String;
  OpenOptions: TEpiDataFileOptions): Boolean;
begin

end;


procedure TEpiDataFile.Read(RecNum: Integer);
begin

end;

function TEpiDataFile.ReadCommonIndex(RecNo: Integer): String;
begin

end;

function TEpiDataFile.ReadCommonViaSortIndex(SortPos: Integer): String;
begin

end;

function TEpiDataFile.ReadFromIndex(IndexNo, RecNo: Integer): string;
begin

end;

function TEpiDataFile.ReadFromMem(AField: TeField; RecNo: Integer;
  var RecIsDeleted: Boolean): String;
begin

end;

function TEpiDataFile.ReadIndexNoFromSortIndex(SortPos: Integer): Integer;
begin

end;

procedure TEpiDataFile.ResetEpiDataFile;
begin

end;

procedure TEpiDataFile.SaveCheckFile;
begin

end;

function TEpiDataFile.SaveStructureToFile(filename: string;
  OverwriteExisting: boolean): boolean;
begin

end;

function TEpiDataFile.SearchIndex(IndexNo: Integer;
  SearchStr: string): LongInt;
begin

end;

function TEpiDataFile.SearchIndexFrom(IndexNo: Integer; SearchStr: string;
  RecNo: Integer; direction: TDirections): LongInt;
begin

end;

procedure TEpiDataFile.SetGlobalMissingValues(Index: Integer;
  Value: string);
begin

end;

procedure TEpiDataFile.SetIndexFields(Index, Value: Integer);
begin

end;

procedure TEpiDataFile.SetIndexIsUnique(Index: Integer; Value: Boolean);
begin

end;

function TEpiDataFile.TextPos(var F: Textfile): Longint;
begin

end;

procedure TEpiDataFile.Write(RecNum: Integer);
begin

end;

procedure TEpiDataFile.WriteIndexNoToSortIndex(SortPos, num: Integer);
begin

end;

procedure TEpiDataFile.WriteToIndex(IndexNo, RecNo: Integer; s: string);
begin

end;

end.

