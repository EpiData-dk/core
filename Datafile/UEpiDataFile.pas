unit UEpiDataFile;

//Errorhåndtering
//onProgress

interface

uses
  Windows,UEpiDataConstants, SysUtils, Classes, UeFields, UValueLabels, UEpiTypes, Graphics;

TYPE

  TEpiDataFileOption = (eoInMemory, eoIgnoreChecks, eoReadRelates, oeIgnoreIndex);
  TEpiDataFileOptions = set of TEpiDataFileOption;

  TRequestPasswordTypes=(rpOpen,rpCreate);
  TRequestPasswordEvent = procedure(Sender: TObject; requesttype:TRequestPasswordTypes; var password:String) of object;

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
    Procedure DisposeFieldList(AList: TeFields);
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
    procedure   DestroyValueLabels(aValueLabelSet: TValueLabelSet);   //TODO: slettes
    function    GetCheckLines:TStringList;
    procedure   Error(errorcode:integer; errortext:string);

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

uses
  CheckObjUnit, UEpiUtils;


{************************************* TEpiDataFile *************************************}


procedure TEpiDataFile.Error(errorcode:integer; errortext:string);
begin
  FErrorCode:=errorcode;
  FErrorText:=errortext;
  Abort;
end;

procedure TEpiDataFile.AddField(aField: TeField);
begin
  if (NOT Assigned(FFieldList)) then FFieldList:=TeFields.create;
  if (aField.Fieldtype=ftInteger) and (aField.Length>9) then aField.Fieldtype:=ftFloat;
  if (aField.Fieldtype=ftQuestion) then aField.Length:=0;
  if (aField.Fieldtype=ftBoolean) then aField.Length:=1;
  if (aField.Fieldtype=ftCrypt) then
    begin
      aField.CryptEntryLength:=aField.Length;
      aField.Length:=GetEncodedLength(AField.CryptEntryLength);
    end;
  if (aField.Fieldtype=ftIDNUM) then FIDNUMField:=FFieldList.Count;
  FFieldList.Add(aField);
  IF aField.Fieldtype<>ftQuestion then INC(FNumDataFields);
  INC(FNumFields);
end;


function TEpiDataFile.ApplyIndex: Boolean;
begin

end;

function TEpiDataFile.CountRecords: LongInt;
begin

end;

constructor TEpiDataFile.Create;
var
  n:integer;
begin
  inherited create;
  FRecFilename:='';
  FQesFilename:='';
  FChkFilename:='';
  FIndexFilename:='';
  FDatFile:=NIL;
  FMemFile:=NIL;
  FStoredInMemory:=False;
  FFieldList:=NIL;
  FHasEOFMarker:=False;
  FNumRecords:=0;
  FHasCheckFile:=False;
  FHasIncludeCmd:=False;
  FFileModified:=False;
  FFieldNames:=NIL;
  FFieldNamesList:=NIL;
  FNumFields:=0;
  FCurField:=0;
  FCurRecord:=0;
  FCurRecModified:=False;
  FDatafileModified:=False;
  FEpiInfoFieldNaming:=False;
  FUpdateFieldnameInQuestion:=False;
  FValueLabels:=TValueLabelSets.Create;
  FHasLongFieldNames:=False;
  FGlobalDefList:=TStringList.Create;
  FGlobalDefaultValue:='';
  FIndex:=NIL;
  FSortIndex:=NIL;
  FOR n:=1 TO MaxIndices DO
    BEGIN
      FIndexFields[n]:=-1;
      FIndexIsUnique[n]:=False;
    END;
  FRecBuf:=NIL;
  FDefList:=NIL;
  FHasDefined:=False;
  FHasCrypt:=False;
  FKey:='';
  FFilelabel:='';
  FChkTopComments:=NIL;
  FHasRepeatField:=False;
  FBeforeFileCmds:=NIL;
  FAfterFileCmds:=NIL;
  FBeforeRecordCmds:=NIL;
  FAfterRecordCmds:=NIL;
  FRecodeCmds:=NIL;
  FAssertList:=NIL;
  FConfirm:=False;
  FAutoSave:=False;
  FGlobalMissingValues[0]:='';
  FGlobalMissingValues[1]:='';
  FGlobalMissingValues[2]:='';
  FGlobalTypeCom:=False;
  FGlobalTypeComColor:=clBlue;
  FIsRelateTop:=True;
  FIsRelateFile:=False;
  FOnRequestPassword:=NIL;
  FChkTopComments:=NIL;
  FTopEpiDataFile:=TObject(self);
  FGlobalTypeCom:=False;
  CheckFileMode:=False;
  FHasKeyUnique:=False;
  FCheckWriter:=NIL;
end;

procedure TEpiDataFile.DecryptIndex;
VAR
  AField: TeField;
  n,CurRec: Integer;
  s: string[30];
begin
  IF (NOT FHasCrypt) OR (FKey='') THEN Exit;
  FOR n:=1 TO FIndexCount DO
    BEGIN
      AField:=TeField(FFieldList[FIndexFields[n]]);  //  PeField(df^.FieldList.Items[df^.IndexFields[n]]);
      IF AField.Fieldtype=ftCrypt THEN
        BEGIN
          FOR CurRec:=1 TO FNumRecords DO
            BEGIN
              s:=ReadFromIndex(n,CurRec);
              s:=DecryptString(trim(s),FKey);
              s:=Format('%-30s',[s]);
              WriteToIndex(n,CurRec,s);
            END;  //for
        END;  //if ftCrypt
    END;  //for
end;

destructor TEpiDataFile.Destroy;
begin
  ResetEpiDataFile;
  inherited destroy;
end;

procedure TEpiDataFile.DestroyValueLabels(aValueLabelSet: TValueLabelSet);
begin
  if assigned(aValueLabelSet) then FreeAndNil(aValueLabelSet);
end;

procedure TEpiDataFile.DisposeFieldList(AList: TeFields);
begin
  if assigned(aList) then FreeAndNil(aList);
end;

function TEpiDataFile.DoRebuildIndex: Boolean;
VAR
  tmpStr:String;
  tmpBool:Boolean;
begin
  //Datafile must be open (with checkfile)
  FIndexFilename:=ChangeFileExt(FRECFilename,'.eix');

  IF FIndexCount=0 THEN
    BEGIN
      Result:=False;
      FErrorText:=Lang(21104,'No key fields found.')+#13#13+Lang(21102,'In order to build an index one or more fields need to have the command KEY in a checkfile.');
      FErrorCode:=epi_CREATE_FILE_ERROR;
      Exit;
    END;
  tmpBool:=MakeIndexfile;
  IF tmpBool THEN
    BEGIN
      Result:=True;
      Exit;
    END
  ELSE
    BEGIN
      FErrorText:=Format(Lang(21108,'Could not create index for %s'),[FRECFilename]);
      FErrorCode:=epi_CREATE_FILE_ERROR;
      Exit;
    END;
{
  ELSE
    BEGIN
      Screen.Cursor:=crDefault;
      ErrorMsg(Lang(21110)    //'Checkfile not found.~~In order to build an index one or more fields need to have the command KEY in a checkfile.'
        +#13#13+Lang(21102));   //'Rebuild Index terminates.')
      Exit;
    END;
}
end;

procedure TEpiDataFile.DoSort(L, R: Integer);
VAR
  P:String;
  I,J,n2,n3: Integer;
begin
  repeat
    I := L;
    J := R;
    P := ReadCommonViaSortIndex((L+R) shr 1);
    repeat
      while ReadCommonViaSortIndex(I) < P do
        INC(I);
      while ReadCommonViaSortIndex(J) > P do
        DEC(J);
      if I <= J then
      begin
        n2:=ReadIndexNoFromSortIndex(I);
        n3:=ReadIndexNoFromSortIndex(J);
        WriteIndexNoToSortIndex(I,n3);
        WriteIndexNoToSortIndex(J,n2);
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then DoSort(L, J);
    L := I;
  until I >= R;
end;


function TEpiDataFile.GetCheckLines: TStringList;
begin
  FCheckWriter:=TCheckWriter.create(self);
  try
    result:=TCheckWriter(FCheckWriter).CheckLines;
  finally
    FCheckWriter.Free;
  end;
end;

function TEpiDataFile.GetDefFieldByName(Fieldname: string): TeField;
VAR
  n:Integer;
  Found: Boolean;
  s: String;
  TopDF:TEpiDataFile;
begin
  Result:=NIL;
  n:=-1;
  Found:=False;
  AnsiUpperCase(trim(Fieldname));
  IF (FDefList<>NIL) AND (FDefList.Count<>0) THEN
    BEGIN
      REPEAT
        INC(n);
        IF s=AnsiUpperCase(trim(FdefList[n])) THEN Found:=True;
      UNTIL (n=FDefList.Count-1) or (Found);
      IF Found THEN Result:=TeField(FDefList.Objects[n]);
    END;
  {Search list of global vars}
  TopDF:=TEpiDataFile(FTopEpiDataFile);
  IF (result=NIL) AND (TopDF.GlobalDefList<>NIL) AND (TopDF.GlobalDefList.Count>0) THEN
    BEGIN
      n:=-1;
      Found:=False;
      REPEAT
        INC(n);
        IF s=AnsiUpperCase(trim(TopDF.GlobalDefList[n])) THEN Found:=True;
      UNTIL (n=TopDF.GlobalDefList.Count-1) OR (Found);
      IF Found THEN Result:=TeField(TopDF.GlobalDefList.Objects[n]);
    END;
end;

function TEpiDataFile.GetDefFieldNumber(Fieldname: string): Integer;
VAR
  n:Integer;
  Found: Boolean;
  s: String;
  TopDF:TEpiDataFile;
begin
  Result:=-1;
  n:=-1;
  Found:=False;
  AnsiUpperCase(trim(Fieldname));
  IF (FDefList<>NIL) AND (FDefList.Count<>0) THEN
    BEGIN
      REPEAT
        INC(n);
        IF s=AnsiUpperCase(trim(FdefList[n])) THEN Found:=True;
      UNTIL (n=FDefList.Count-1) or (Found);
      IF Found THEN Result:=n;
    END;
  {Search list of global vars}
  TopDF:=TEpiDataFile(FTopEpiDataFile);
  IF (result=-1) AND (TopDF.FGlobalDefList<>NIL) AND (TopDF.FGlobalDefList.Count>0) THEN
    BEGIN
      n:=-1;
      Found:=False;
      REPEAT
        INC(n);
        IF s=AnsiUpperCase(trim(TopDF.FGlobalDefList[n])) THEN Found:=True;
      UNTIL (n=TopDF.FGlobalDefList.Count-1) OR (Found);
      IF Found THEN Result:=n;
    END;
end;

function TEpiDataFile.GetField(Index: Integer): TeField;
begin
  Result:=NIL;
  IF NOT Assigned(FFieldList) THEN Exit;
  IF (Index<0) or (Index>FFieldList.Count-1) THEN Exit;
  //IF FCurRecord<0 THEN Exit;
  Result:=TeField(FFieldList.Items[Index]);
end;

function TEpiDataFile.GetFieldByName(Fieldname: String): TeField;
VAR
  n:Integer;
  tmpS:String;
begin
  Result:=NIL;
  IF FFieldList=NIL THEN Exit;
  IF FFieldList.Count=0 THEN Exit;
  tmpS:=trim(Fieldname);
  n:=-1;
  REPEAT
    INC(n);
    IF Comparetext(tmpS,TeField(FFieldList.items[n]).FieldName)=0
    THEN Result:=TeField(FFieldList.Items[n]);
  UNTIL (n=FFieldList.Count-1) OR (Result<>NIL);
end;

function TEpiDataFile.GetFieldNumber(Fieldname: String): Integer;
VAR
  n:Integer;
  tmpS:String;
begin
  Result:=-1;
  IF FFieldList=NIL THEN Exit;
  IF FFieldList.Count=0 THEN Exit;
  tmpS:=trim(Fieldname);
  n:=-1;
  REPEAT
    INC(n);
    //IF tmpS=AnsiUpperCase(trim(PeField(df^.FieldList.Items[n])^.Name))
    IF CompareText(tmpS,TeField(FFieldList.items[n]).FieldName)=0
    THEN Result:=n;
  UNTIL (n=FFieldList.Count-1) or (Result<>-1);
end;

function TEpiDataFile.GetFieldTypeNames(OrdOfFieldtype: Integer): String;
begin
  CASE OrdOfFieldtype OF
    0:  Result:=Lang(50100,'Numeric');
    1:  Result:=Lang(50101,'Text');
    2:  Result:=Lang(50102,'Date (mdy)');
    3:  Result:=Lang(50103,'Uppercase text');
    4:  Result:='Checkbox';
    5:  Result:=Lang(50105,'Boolean');
    6:  Result:=Lang(50100,'Numeric');
    7:  Result:='Phonenumber';
    8:  Result:='Time';
    9:  Result:='Local phonenumber';
    10: Result:=Lang(50110,'Today (mdy)');
    11: Result:=Lang(50111,'Date (dmy)');
    12: Result:=Lang(50112,'ID-number');
    15: Result:=Lang(50115,'Question');
    16: Result:=Lang(50116,'Today (dmy)');
    17: Result:=Lang(50117,'Soundex');
    18: Result:=Lang(50118,'Encryptfield');
    19: Result:=Lang(50119,'Date (ymd)');
    20: Result:=Lang(50120,'Today (ymd)');
  ELSE
    Result:='Unknown type';
  END;  //case
end;

function TEpiDataFile.GetFileSize: LongInt;
begin
  Result:=-1;
  IF ((FStoredInMemory) AND (Assigned(FMemFile))) THEN Result:=FMemFile.Size;
  IF ((NOT FStoredInMemory) AND (Assigned(FDatFile))) THEN Result:=FDatFile.Size;
end;

function TEpiDataFile.GetGlobalMissingValues(Index: Integer): string;
begin
  Result:=FGlobalMissingValues[Index];
end;

function TEpiDataFile.GetIndexFields(Index: Integer): Integer;
begin
  Result:=FIndexFields[Index];
end;

function TEpiDataFile.GetIndexIsUnique(Index: Integer): Boolean;
begin
  Result:=FIndexIsUnique[Index];
end;

function TEpiDataFile.GetQesLines: string;
VAR
  CurField,rN,Lx,Indent,PrevIndent,nc,qc:Integer;
  TempStr,tmpFieldStr,s,q,tmpName:String;
  QES: TStringList;
  InBracket: Boolean;
  aField: TeField;
begin
  TRY
    QES:=TStringList.Create;
    result:='';
    try
      if (not assigned(FFieldList)) or (FFieldList.Count=0) then exit;
      Indent:=0;
      PrevIndent:=0;
      FOR CurField:=0 TO FFieldList.Count-1 do
        BEGIN
          WITH TeField(FFieldList.Items[CurField]) DO
            BEGIN
              Indent:=0;
              IF QuestY<>QES.Count THEN PrevIndent:=0;
              q:=Question;
              tmpName:=trim(FieldName);
              IF (FUpdateFieldnameInQuestion) AND (Fieldtype<>ftQuestion) THEN
                BEGIN
                  IF trim(Question)<>'' THEN
                    BEGIN
                      IF FEpiInfoFieldNaming THEN
                        BEGIN
                          IF NOT (AnsiUpperCase(tmpName)=AnsiUpperCase(trim(q))) THEN
                            BEGIN
                              //test if fieldname can be made from question
                              nc:=1;
                              qc:=1;
                              WHILE (nc<=system.Length(tmpName)) AND (qc<system.Length(q)) DO
                                BEGIN
                                  IF UpCase(q[qc])=UpCase(tmpName[nc]) THEN INC(nc);
                                  INC(qc);
                                END;  //while
                              IF nc=system.Length(tmpName)+1 THEN
                                BEGIN
                                  //Fieldname can be made from question
                                  nc:=1;
                                  qc:=1;
                                  InBracket:=False;
                                  s:='';
                                  WHILE qc<=system.Length(q) DO
                                    BEGIN
                                      IF nc<=system.Length(tmpName)+1 THEN
                                        BEGIN
                                          IF UpCase(q[qc])=UpCase(tmpName[nc]) THEN
                                            BEGIN
                                              INC(nc);
                                              IF NOT InBracket THEN
                                                BEGIN
                                                  InBracket:=True;
                                                  s:=s+'{';
                                                  INC(Indent);
                                                END;
                                            END  //if fieldname letter found
                                          ELSE IF InBracket THEN
                                            BEGIN
                                              s:=s+'}';
                                              INC(Indent);
                                              InBracket:=False;
                                            END;
                                        END;  //if parts of fieldname is still missing
                                      s:=s+q[qc];
                                      INC(qc)
                                    END;  //while qc<=Length(q)
                                  IF InBracket THEN
                                    BEGIN
                                      s:=s+'}';
                                      INC(Indent);
                                    END;
                                  q:=s;
                                END   //if question contains fieldname
                              ELSE
                                BEGIN
                                  //question does not contain fieldname
                                  nc:=1;
                                  WHILE q[nc]=' ' DO INC(nc);
                                  Insert('{'+tmpName+'} ',q,nc);
                                  Indent:=system.Length(tmpName)+3;
                                END;
                            END;  //if question<>Name
                        END  //if EpiInfoFieldNaming
                      ELSE
                        BEGIN
                          //First word is used as fieldname
                          IF AnsiUpperCase(FirstWord(q))<>AnsiUpperCase(tmpName) THEN
                            BEGIN
                              nc:=1;
                              WHILE q[nc]=' ' DO INC(nc);
                              Insert(tmpName+' ',q,nc);
                              Indent:=system.Length(tmpName)+1;
                            END;
                        END;
                    END  //if there is a question
                  ELSE
                    BEGIN
                      IF q='' THEN
                        BEGIN
                          q:=tmpName+' ';
                          QuestY:=FieldY;
                          QuestX:=FieldX;
                        END
                      ELSE
                        BEGIN
                          nc:=1;
                          WHILE q[nc]=' ' DO INC(nc);
                          Insert(tmpName+' ',q,nc);
                        END;
                      Indent:=system.Length(tmpName)+1;
                    END;
                END;  //if InsertFieldname
              IF trim(q)<>'' THEN   //is there a question?
                BEGIN
                  {Get the nessary number of lines}
                  WHILE QuestY>QES.Count DO QES.Append('');
                  Lx:=QuestY-1;
                  tempStr:=QES[Lx];
                  {Get the nessaray number of chars in the line}
                  WHILE system.Length(tempStr) < QuestX-1+system.Length(q)+PrevIndent DO
                    tempStr:=tempStr+' ';
                  {put Question in tempStr}
                  FOR rN:=1 TO system.Length(q) DO
                    tempStr[QuestX-1+rN+PrevIndent]:=q[rN];
                  QES[Lx]:=tempStr;
                END;  //if trim(Question)<>''
              PrevIndent:=PrevIndent+Indent;
              IF (Length>0) THEN  //is there a field?
                BEGIN
                  WHILE FieldY>QES.Count DO QES.Append('');
                  Lx:=FieldY-1;
                  tmpFieldStr:='';
                  CASE Fieldtype of
                    ftInteger: tmpFieldStr:=cFill('#',Length);
                    ftAlfa: tmpFieldStr:=cFill('_',Length);
                    ftDate:BEGIN
                      CASE Length of
                        5: tmpFieldStr:='<mm/dd>';
                        8: tmpFieldStr:='<mm/dd/yy>';
                        10: tmpFieldStr:='<mm/dd/yyyy>';
                      ELSE tmpFieldStr:='<ERROR>';
                      END;  //case FLength
                      END;  //Case Fieldtype of ftDate
                    ftUpperAlfa: tmpFieldStr:='<A'+cFill(' ',Length-1)+'>';
                    ftCrypt:     tmpFieldStr:='<E'+cFill(' ',CryptEntryLength-1)+'>';   //&&
                    ftIDNUM: tmpFieldStr:='<IDNUM'+cFill(' ',Length-5)+'>';
                    ftBoolean: tmpFieldStr:='<Y>';
                    ftFloat: BEGIN
                      tmpFieldStr:=cFill('#',Length-1-NumDecimals);
                      IF NumDecimals=0 THEN tmpFieldStr:=tmpFieldStr+'#'
                      ELSE tmpFieldStr:=tmpFieldStr+'.'+cFill('#',NumDecimals);
                      END;   //Case Fieldtype of ftFloat
                    ftYMDToday: tmpFieldStr:='<TODAY-YMD>';     //&&
                    ftYMDDate:  tmpFieldStr:='<yyyy/mm/dd>';    //&&
                    ftToday: BEGIN
                      CASE Length of
                        5: tmpFieldStr:='<TODAY>';
                        8: tmpFieldStr:='<TODAY/YY>';
                        10: tmpFieldStr:='<TODAY-MDY>';
                      END;  //Case FLength
                      END;  //Case Fieldname of ftToday;
                    ftEuroDate: BEGIN
                      CASE Length of
                        5: tmpFieldStr:='<dd/mm>';
                        8: tmpFieldStr:='<dd/mm/yy>';
                        10: tmpFieldStr:='<dd/mm/yyyy>';
                      ELSE tmpFieldStr:='<ERROR>';
                      END;  //case FLength
                      END;  //Case Fieldname of ftEuroDate
                    ftEuroToday: IF Length=10 THEN tmpFieldStr:='<today-dmy>'
                                 ELSE tmpFieldStr:='<ERROR>';
                    ftSoundex: tmpFieldStr:='<S'+cFill(' ',Length-1)+'>';
                    ELSE  tmpFieldStr:='<ERROR>';
                  END;  //Case Fieldtype

                  IF true THEN
                    BEGIN
                      tempStr:=QES[Lx];
                      WHILE system.Length(tempStr) < FieldX-1+system.Length(tmpFieldStr)+PrevIndent DO
                        tempStr:=tempStr+' ';

                      FOR rN:=1 TO system.Length(tmpFieldStr) DO
                        tempStr[FieldX-1+rN+PrevIndent]:=tmpFieldStr[rN];

                      QES[Lx]:=Tempstr;

                    END;  //if legal field found
                END;  //is there a field?
            END;   //with TempField
        END;   //for CurField
      result:=QES.Text;
    finally
      qes.Free;
    end;
  EXCEPT
    raise exception.Create('Error during creation of Qes-lines');
    Result:='';
  END;  //try..except
end;

function TEpiDataFile.IndexHasDuplicates(IndexNo: Integer): Boolean;
VAR
  FirstRec,SecondRec:Integer;
  pSecondrec,pFirstRec:Pointer;
begin
  Result:=True;
  FirstRec:=0;
  WHILE (FirstRec<=FNumRecords-1) AND (Result) DO
    BEGIN
      INC(FirstRec);
      pFirstRec:=Pointer(LongInt(FIndex.Memory)+31+( (FirstRec-1)*(31*FIndexcount) ) + (31*(IndexNo-1))+1);
      SecondRec:=FirstRec;
      WHILE (SecondRec<=FNumRecords) AND (Result) DO
        BEGIN
          INC(SecondRec);
          pSecondrec:=Pointer(LongInt(FIndex.Memory)+31+( (SecondRec-1)*(31*FIndexcount) ) + (31*(IndexNo-1))+1);
          Result:=NOT CompareMem(pFirstRec,pSecondRec,30);
        END;  //while Secondrec
    END;  //while FirstRec
  IF NOT Result
  THEN FErrorText:=Format('%s'+#13+Lang(20126,'The field %s is KEY UNIQUE, but duplicate keys are found in records %d and %d'),
    [ExtractFilename(FRECFilename),TeField(FFieldList[IndexNo]).Fieldname,FirstRec,SecondRec]);
end;

procedure TEpiDataFile.InitSortIndex;
VAR
  n:Integer;
  pn:ARRAY[0..3] OF Byte Absolute n;
begin
  {Initialize}
  FSortIndex:=TMemoryStream.Create;
  FSortIndex.SetSize(FNumRecords*4);
  FSortIndex.Position:=0;
  FOR n:=1 TO FNumRecords DO
    FSortIndex.Write(pn,4);
  DoSort(1,FNumRecords);
end;

function TEpiDataFile.Lang(langkode: Integer;  const langtext: string): String;
var
  s:string;
begin
  s:='';
  IF Assigned(FOnTranslate) THEN
    BEGIN
      FOnTranslate(langkode, s);
      Result:=s;
    END
  ELSE Result:=langtext;
  if result='' then result:=langtext;
end;

function TEpiDataFile.LoadChecks: Boolean;
VAR
  CheckObj: TCheckObj;
  tmpChecks: TStringList;
  s:String;
begin
  Result:=True;
  IF FCHKFilename='' THEN FCHKFilename:=ChangeFileExt(FRECFilename,'.chk');
  IF NOT FileExists(FCHKFilename) THEN Exit;
  Result:=False;
  TRY
    tmpChecks:=TStringList.Create;
    TRY
      tmpChecks.LoadFromFile(FCHKFilename);
    EXCEPT
      tmpChecks.Free;
      FErrorText:=Format(Lang(20130,'Error reading the checkfile %s'),[FCHKFilename]);
      FErrorCode:=EPI_CHECKFILE_ERROR;
      Exit;
    END;  //try..Except
    s:=tmpChecks.Text;
    TRY
      CheckObj:=TCheckObj.Create;
      CheckObj.ChkFileMode:=CheckFileMode;
      if assigned(FOnTranslate) then CheckObj.OnTranslate:=FOnTranslate;
      try
        Result:=CheckObj.ApplyChecks(self,s);
      except
        result:=false;
      end;
      IF (NOT Result) THEN
        BEGIN
          FErrorText:=CheckObj.ErrorList;
          FErrorCode:=EPI_CHECKFILE_ERROR;
        END
      ELSE
        BEGIN
          FErrorCode:=0;
          FErrorText:='';
        END;
    Finally
      CheckObj.Free;
    END;
  FINALLY
    tmpChecks.Free;
  END;
end;

function TEpiDataFile.MakeIndexFile: Boolean;
VAR
  tmpS: string[30];
  s: String;
  n: Integer;
  CurRec: LongInt;
  //tmpRecFile: TextFile;
  AField: TeField;
  HasKeyUnique,ok,OldFStoredInMemory: Boolean;
begin
  IF (FStoredInMemory) AND (FMemFile=NIL) THEN Exit;
  IF (NOT FStoredInMemory) AND (FDatFile=NIL) THEN Exit;
  IF FIndexFilename='' THEN FIndexFilename:=ChangeFileExt(FRECFilename,'.eix');
  TRY
    AssignFile(FIndexFile,FIndexFilename);
    Rewrite(FIndexFile);
  EXCEPT
    FErrorText:=Lang(21112,'Index file could not be created');
    FErrorCode:=epi_CREATE_FILE_ERROR;
    Result:=False;
    Exit;
  END;  //try..Except

  OldFStoredInMemory:=FStoredInMemory;
  TRY
    IF (NOT FStoredInMemory) THEN
      BEGIN
        FreeAndNil(FDatFile);
        FMemFile:=TMemoryStream.Create;
        FMemFile.LoadFromFile(FRECFilename);
        FStoredInMemory:=True;
      END;
  EXCEPT
    FErrorText:=Format(Lang(20120,'Error reading the datafile %s.'),[FRECFilename]);
    FErrorCode:=epi_READ_FILE_ERROR;
    Result:=False;
    Exit;
  END;
  tmpS:=IntToStr(FIndexCount);
  FOR n:=1 TO FIndexCount DO
    tmpS:=tmpS+'@'+IntToStr(FIndexFields[n]);
  system.Write(FIndexFile,tmpS);
  IF FNumRecords>0 THEN
    BEGIN
      FOR CurRec:=1 TO FNumRecords DO
        BEGIN
          Self.Read(CurRec);           //eReadOnlyNextRecord(df,tmpRecFile);
           FOR n:=1 TO FIndexCount DO
             BEGIN
               AField:=Fields[FIndexFields[n]];   //  PeField(df^.FieldList.Items[df^.IndexFields[n]]);
               IF AField.Fieldtype=ftCrypt THEN s:=EncryptString(trim(Copy(AField.AsString,1,21)),FKey)
               ELSE s:=trim(Copy(AField.AsString,1,30));
               tmpS:=s;
               CASE AField.Fieldtype OF
                 ftInteger,ftFloat: tmpS:=FormatNumberToIndex(tmpS); //Format('%30s',[tmpS]);
                 ftDate,ftEuroDate,ftToday,ftEuroToday,ftYMDDate,ftYMDToday:   //&&
                   tmpS:=Format('%30s',[FloatToStr(mibStrToDate(tmpS,AField.Fieldtype))]);
               ELSE
                 tmpS:=Format('%-30s',[tmpS]);
               END;  //case
               system.Write(FIndexFile,tmpS);
             END;  //for n
        END;  //for CurRec
    END;  //if NumRecords>0
  CloseFile(FIndexFile);
  IF OldFStoredInMemory=False THEN
    BEGIN
      FStoredInMemory:=False;
      FreeAndNil(FMemFile);
      FDatfile:=TFileStream.Create(FRECFilename,fmOpenReadWrite OR fmShareExclusive);
    END;
  Result:=True;

  //Check KEY UNIQUE
  FOR n:=1 TO FIndexCount DO
    IF FIndexIsUnique[n] THEN FHasKeyUnique:=True;
  IF FHasKeyUnique THEN
    BEGIN
      TRY
        FIndex:=TMemoryStream.Create;
        FIndex.LoadFromFile(FIndexfilename);
        n:=1;
        ok:=True;
        WHILE (n<=FIndexCount) AND (ok) DO
          BEGIN
            IF FIndexIsUnique[n] THEN ok:=IndexHasDuplicates(n);
            INC(n);
          END;
        Result:=ok;
        IF NOT Result THEN ok:=Sysutils.DeleteFile(FIndexfilename);
      FINALLY
        FIndex.Free;
        FIndex:=NIL;
      END;  //try..Except
    END;  //if HasKeyUnique
end;


function TEpiDataFile.Open(const filename: String; OpenOptions: TEpiDataFileOptions): Boolean;
VAR
  TempResult,ok: Boolean;
  F: TextFile;
  eLine,OrigFieldname,TempStr,TempKey,PreEnteredKey: String;
  tmpName:string[10];
  TempInt,TempInt2,Curfield: Integer;
  eField: TeField;
  tmpFieldChar,Dummy: Char;
  QuestColor,FieldColor,ft,n,t,FieldNumberCounter: Integer;
  tmpFieldColor,tmpQuestX,tmpQuestY,tmpLength,tmpFieldX,tmpFieldY: integer;
  tmpQuestColor:integer;
  tmpQuestion,tmpVarlabel:string;
begin
  FieldNumberCounter:=1;
  PreEnteredKey:=FKey;
  ResetEpiDatafile;
  FValueLabels:=TValueLabelSets.Create;
  FGlobalDefList:=TStringList.Create;
  FOpenOptions:=OpenOptions;
  FRecfilename:=filename;
  FNumFields:=0;
  TempResult:=True;
  AssignFile(F,FRecFilename);
  {$I-}
  Reset(F);
  TempInt:=IOResult;
  {$I+}
  IF TempInt=0 THEN   //datafile could be opened
    BEGIN
      FOffset:=0;
      FFieldList:=TeFields.create;
      {Read first line i datafile - number of fields}
      ReadLn(F,eLine);
      eLine:=eLine+' ';
      TempStr:=COPY(eLine,1,POS(' ',eLine)-1);
      IF IsInteger(TempStr) THEN FNumFields:=StrToInt(TempStr)
      ELSE
        BEGIN
          CloseFile(F);
          FErrorText:=Format(Lang(20112,'Incorrect format of datafile %s'),[FRECfilename]);  //Incorrect format of datafile %s.
          FErrorCode:=epi_DATAFILE_FORMAT_ERROR;
          Result:=False;
          Exit;
        END;
      IF TempResult THEN   //Begin reading the field-info
        BEGIN
          TempInt:=pos('~kq:',eLine);    //&&
          IF TempInt>0 THEN
            BEGIN
              //Datafile contains a crypt-key
              TempInt2:=pos(':kq~',eLine);
              IF (TempInt2>0) AND (TempInt2>TempInt) THEN FKey:=copy(eLine,TempInt+4,TempInt2-TempInt-4);
            END;
          TempInt:=Pos('FILELABEL: ',AnsiUpperCase(eLine));
          IF TempInt<>0 THEN FFilelabel:=Copy(eLine,TempInt+Length('FILELABEL: '),Length(eLine));
          IF Pos(' VLAB',eLine)>0 THEN FEpiInfoFieldNaming:=False ELSE FEpiInfoFieldNaming:=True;
          FRecLength:=0;
          FIDNumField:=-1;

          FOR CurField:=1 to NumFields DO
            BEGIN
              //Eventuelt lægge et progress-event ind her: ProgressBar.Position:=CurField  /  pct = CurField/NumFields
              eField:=TeField.Create;
              TRY
                WITH eField DO
                  BEGIN
                    ReadLn(F,tmpFieldChar,tmpName,tmpQuestX,tmpQuestY,tmpQuestColor,tmpFieldX,tmpFieldY,
                           ft,tmpLength,tmpFieldColor,dummy,tmpQuestion);
                    FieldChar:=tmpFieldChar;
                    QuestX:=tmpQuestX;
                    QuestY:=tmpQuestY;
                    QuestColor:=tmpQuestColor;
                    FieldX:=tmpFieldX;
                    FieldY:=tmpFieldY;
                    Length:=tmpLength;
                    FieldColor:=tmpFieldColor;
                    Question:=tmpQuestion;

                    Fieldname:=tmpName;
                    OrigFieldname:=fieldname;
                    //['0'..'9','A'..'Z','a'..'z',' ']
                    ok:=true;
                    FOR n:=1 TO system.Length(Fieldname) do
                      if (NOT (Fieldname[n] in ['0'..'9','A'..'Z','a'..'z',' '])) THEN ok:=false;
                    IF (not ok) THEN
                      BEGIN
                        //Fieldname includes illegal chars
                        ok:=false;
                        repeat
                          Fieldname:='V'+IntToStr(FieldNumberCounter);
                          IF FFieldlist.Count>0 THEN
                            begin
                              ok:=true;
                              for t:=0 to FFieldList.Count-1 DO
                                if trim(ansilowercase(TeField(FFieldList.Items[t]).FieldName))=trim(ansilowercase(Fieldname)) then ok:=false;
                            end  //if
                          else ok:=true;
                          INC(FieldNumberCounter);
                        until ok;
                      END;
                    IF system.Length(FieldName)>8 THEN FHasLongFieldNames:=True;
                    tmpQuestion:=Question;
                    WHILE Pos('_',tmpQuestion)>0 DO tmpQuestion[Pos('_',tmpQuestion)]:='-';
                    Question:=tmpQuestion;
                    NumDecimals:=0;
                    IF ft>=100 THEN BEGIN
                      NumDecimals:=ft-100;
                      Fieldtype:=ftFloat;
                    END  //if ftFloat
                    ELSE BEGIN
                      fieldtype:=ftInteger;
                      NumDecimals:=0;
                      WHILE ft>ORD(fieldtype) DO fieldtype:=Succ(fieldtype);
                    END;
                    IF Length=0 THEN fieldtype:=ftQuestion;
{                    IF (fieldtype=ftPhoneNum) or (fieldtype=ftLocalNum) THEN fieldtype:=ftAlfa;

                    IF (fieldtype in [ftCheckBox,ftPhoneNum,ftTime,ftLocalNum,ftRes4,ftRes5]) THEN
                      BEGIN
                        FErrorText:=Format(Lang(20144,'Datafile %s contains a field of the type %s~This fieldtype is not supported by EpiData.'),[FRECFilename,FieldTypeNames[ORD(fieldtype)]]);   //20144=Datafile %s contains a field of the type %s~This fieldtype is not supported by EpiData.
                        FErrorCode:=epi_DATAFILE_FORMAT_ERROR;
                        CloseFile(F);
                        Result:=False;
                        Exit;
                      END;}
                    IF (NOT (fieldtype in SupportedFieldTypes)) or ((fieldtype in DateFieldTypes) and (Length<10))
                    then fieldtype:=ftAlfa;
                    CryptEntryLength:=0;   //&&
                    LengthInFile:=Length;
                    IF (fieldtype=ftCrypt) AND (FHasCrypt=false) THEN
                      BEGIN
                        FHasCrypt:=True;
                        //IF FieldColor>111 THEN FCryptEntryLength:=FieldColor-111 ELSE FCryptEntryLength:=FieldColor;
                        TempKey:='';
                        TRY
                          IF SysUtils.Trim(PreEnteredKey)<>'' then TempKey:=PreEnteredKey
                          ELSE IF Assigned(FOnRequestPassword) THEN FOnRequestPassword(self,rpOpen,TempKey);
                          //Tilføj eventuelt en rutine der checker password f.eks. 3 gange før afbrydelse
                          IF TempKey=DecryptString(FKey,TempKey) THEN FKey:=TempKey
                          ELSE
                            BEGIN
                              FErrorText:=Lang(9020,'Incorrect password entered');
                              FErrorcode:=epi_DATAFILE_FORMAT_ERROR;
                              Result:=False;
                              CloseFile(F);
                              Exit;
                            END;
                        EXCEPT
                          FErrorText:=Lang(9022,'Error encouted during decryption of password');
                          FErrorcode:=epi_DATAFILE_FORMAT_ERROR;
                          Result:=False;
                          CloseFile(F);
                          Exit;
                        END;  //try..except
                      END;   //if Crypt and HasCrypt=False
                    IF fieldtype=ftCrypt THEN         //MIB 19jan07
                      BEGIN
                        IF FieldColor>111 THEN CryptEntryLength:=FieldColor-111
                        ELSE CryptEntryLength:=FieldColor;
                      END;
                    StartPos:=FRecLength+(FRecLength DIV MaxRecLineLength)*3;
                    FRecLength:=FRecLength+Length;
                    VariableLabel:=trim(Question);
                    tmpVarLabel:=VariableLabel;
                    FieldNo:=Curfield-1;
                    IF NOT (EpiInfoFieldNaming) AND (trim(VariableLabel)<>'') THEN
                      BEGIN
                        TempStr:=FirstWord(tmpVarLabel);
                        Delete(tmpVarLabel,Pos(TempStr,tmpVarLabel),system.Length(TempStr));
                        tmpVarLabel:=trim(tmpVarLabel);
                      END;
                    IF FieldName<>OrigFieldname THEN tmpVarLabel:=OrigFieldname+' '+tmpVarLabel;
                    VariableLabel:=tmpVarLabel;
                    eField.ResetCheckProperties;
                    FFieldCheckSum:=FFieldCheckSum+ORD(eField.Fieldtype)+system.Length(eField.Question)+eField.Length;
                  END;  //with eField
                FFieldList.Add(eField);
              EXCEPT
                FErrorText:=Format(Lang(20116,'Error in the datafile %s.~~The field definition in line %d could not be read or interpreted.'),[FRECfilename,CurField+1]);
                FErrorCode:=epi_DATAFILE_FORMAT_ERROR;
                CloseFile(F);
                Result:=False;
                Exit;
              END;  //try..except
            END;  //for CurField
          FShortRecLength:=FRecLength;
          FRecLength:=FRecLength+(((FRecLength-1) DIV MaxRecLineLength)+1)*3;  //Add NewLine+LineFeed+Terminatorchar.
          FOffSet:=TextPos(F);

          GetMem(FRecBuf,FRecLength);
          CloseFile(F);

          IF (eoInMemory in OpenOptions) THEN
            BEGIN
              //store file in memory
              FStoredInMemory:=True;
              FMemFile:=TMemoryStream.Create;
              FMemFile.LoadFromFile(FRECFilename);
            END
          ELSE
            BEGIN
              FStoredInMemory:=False;
              FDatfile:=TFileStream.Create(FRECFilename,fmOpenReadWrite OR fmShareExclusive);
            END;
          FNumDataFields:=0;
          FOR CurField:=0 TO FFieldList.Count-1 DO
            BEGIN
              IF TeField(FFieldList.Items[CurField]).Fieldtype=ftIDNUM THEN FIDNUMField:=CurField;
              IF TeField(FFieldList.Items[CurField]).Fieldtype<>ftQuestion THEN INC(FNumDataFields);
            END;
          FNumRecords:=CountRecords;
          IF FNumRecords=-1 THEN
            BEGIN
              FErrorText:=Format(Lang(20118,'Error in datafile %s.~~One or more records are corrupted.'),[FRECFilename]);
              FErrorCode:=epi_DATAFILE_FORMAT_ERROR;
              Result:=False;
              Exit;
            END;
        END;  //if fieldinfo can be read
      FHasCheckFile:=FileExists(ChangeFileExt(FRECFilename,'.chk'));
      if FHasCheckFile then FCHKFilename:=ChangeFileExt(FRECFilename,'.chk');
    END //if datafile could be opened
  ELSE
    BEGIN
      FErrorText:=Format(Lang(20108,'Data file %s could not be opened.'),[FRECFilename])+#13+Lang(20208,'Please check if the file is in use and that the file name is legal.');
      FErrorCode:=epi_DATAFILE_FORMAT_ERROR;
      TempResult:=False;
    END;
  FCurRecord:=-1;
  Result:=TempResult;
  IF (Result) AND (NOT (eoIgnoreChecks in OpenOptions)) THEN Result:=LoadChecks;
end;


procedure TEpiDataFile.Read(RecNum: Integer);
VAR
  ABuf: PRecBuf;
  n, rdN, LineCharCount, BufCount: Integer;
  ss: String;
  ok: Boolean;
begin
  IF (RecNum<=FNumRecords) AND (RecNum>0) THEN
    BEGIN
      ABuf:=FRecBuf;
      IF FStoredInMemory THEN
        BEGIN
          FMemFile.Position:=FOffset+((RecNum-1)*FRecLength);
          n:=0;
          REPEAT
            INC(n);
            ok:=True;
            TRY
              FMemFile.ReadBuffer(ABuf^,FRecLength);
            EXCEPT
              ok:=False;
              IF n>=3 THEN raise Exception.Create(Lang(20464,'Error reading record'));  //20464=Error reading record
            END;  //try..except
          UNTIL ok;
        END
      ELSE
        BEGIN
          FDatFile.Position:=FOffset+((RecNum-1)*FRecLength);
          n:=0;
          REPEAT
            INC(n);
            ok:=True;
            TRY
              FDatFile.ReadBuffer(ABuf^,FRecLength);
            EXCEPT
              ok:=False;
              IF n>=3 THEN raise Exception.Create(Lang(20464,'Error reading record'));  //20464=Error reading record
            END;  //try..except
          UNTIL ok;
        END;
      IF n>=3 THEN raise Exception.Create(Lang(20464,'Error reading record'));    //20464=Error reading record
      LineCharCount:=MaxRecLineLength;
      BufCount:=0;
      FOR rdN:=0 TO FFieldList.Count-1 DO
        BEGIN
          WITH TeField(FFieldList.Items[rdN]) DO
            BEGIN
              IF fieldtype<>ftQuestion THEN
                BEGIN
                  AsString:=cFill(' ',Length);
                  FOR n:=1 TO Length DO
                    BEGIN
                      AsString:=ABuf^[BufCount];
                      INC(BufCount);
                      DEC(LineCharCount);
                      IF LineCharCount<=0 THEN
                        BEGIN
                          LineCharCount:=MaxRecLineLength;
                          INC(BufCount,3);
                        END;
                    END;  //for
                  AsString:=trim(AsString);
                  IF (fieldtype=ftCrypt) AND (FKey<>'') THEN
                    BEGIN
                      ss:=AsString;
                      ss:=DecryptString(ss,FKey);  //&&
                      AsString:=ss;
                    END;

                  //**************** Eventuel opdatering af Dataform ****************
                  {
                  IF Assigned(df^.DatForm) THEN
                    BEGIN
                      ChangeGoingOn:=True;
                      TEntryField(EntryField).Text:=FFieldText;
                      TEntryField(EntryField).Modified:=False;
                      ChangeGoingOn:=False;
                      IF (FTypeComments) OR (FTypeString) THEN FTypeField.Caption:='';
                    END;  //if assigned
                  }
                  //****************************************

                END;  //if not ftQuestion
            END;  //with
        END;  //for
      FCurRecModified:=False;
      FCurRecord:=RecNum;
      FCurRecDeleted:=(ABuf^[BufCount]='?');
      FCurRecVerified:=(ABuf^[BufCount]='^');
      {
      IF NOT NoUpDateCurRecEdit
      THEN TDataForm(df^.DatForm).UpdateCurRecEdit(RecNum, df^.NumRecords);
      }
    END;  //if RecNum<=NumRecords
end;

function TEpiDataFile.ReadCommonIndex(RecNo: Integer): String;
VAR
  pS:Array[0..310] of Char;
  n:Integer;
begin
  FillChar(pS,310,0);
  FIndex.Position:=31+((RecNo-1)*(31*FIndexCount));
  FIndex.Read(pS,31*FIndexCount);
  Result:=StrPas(pS);
  FOR n:=1 TO FIndexCount Do
    Delete(Result,((n-1)*31)+1,1);
end;

function TEpiDataFile.ReadCommonViaSortIndex(SortPos: Integer): String;
VAR
  n:Integer;
  tmpS:String;
begin
  {Returns the common indexvalue pointer to by SortIndex[Posi]}
  n:=ReadIndexNoFromSortIndex(SortPos);
  tmpS:=ReadCommonIndex(n);
  Result:=tmpS;
end;

function TEpiDataFile.ReadFromIndex(IndexNo, RecNo: Integer): string;
VAR
  tmpS:string[30];
  ptmpS:Array[0..30] of byte absolute tmpS;
begin
  FIndex.Position:=31+( (RecNo-1)*(31*FIndexCount) ) + ( 31*(IndexNo-1) );
  FIndex.Read(ptmpS,31);
  Result:=tmpS;
end;

function TEpiDataFile.ReadFromMem(AField: TeField; RecNo: Integer; var RecIsDeleted: Boolean): String;
VAR
  RecordPos:LongInt;
  CharPointer: ^CHAR;
  FieldT:PChar;
  FieldText:String;
begin
  New(CharPointer);
  TRY
    Result:='';
    IF (RecNo<1) OR (RecNo>FNumRecords) THEN Exit;
    IF AField=NIL THEN Exit;
    IF (FStoredInMemory) AND (NOT Assigned(FMemFile)) THEN Exit;
    IF (NOT FStoredInMemory) AND (NOT Assigned(FDatFile)) THEN Exit;

    RecordPos:=FOffset+((RecNo-1)*FRecLength);
    IF FStoredInMemory THEN
      BEGIN
        FMemFile.Position:=RecordPos+FRecLength-3;
        FMemFile.Read(CharPointer^,1);
      END
    ELSE
      BEGIN
        FDatFile.Position:=RecordPos+FRecLength-3;
        FDatFile.Read(CharPointer^,1);
      END;
    IF CharPointer^='?' THEN RecIsDeleted:=True ELSE RecIsDeleted:=False;
    IF AField.Fieldtype<>ftQuestion THEN
      BEGIN
        {Read value of field}
        FieldT:=PChar(cFill(#0,AField.Length+3));
        IF FStoredInMemory THEN
          BEGIN
            FMemFile.Position:=RecordPos+AField.StartPos;
            FMemFile.ReadBuffer(FieldT^,AField.Length);
          END
        ELSE
          BEGIN
            FDatFile.Position:=RecordPos+AField.StartPos;
            FDatFile.ReadBuffer(FieldT^,AField.Length);
          END;
        FieldText:=FieldT;
        IF Pos('!',FieldText)>0 THEN
          BEGIN
            IF FStoredInMemory THEN
              BEGIN
                FMemFile.Position:=RecordPos+AField.StartPos;
                FMemFile.ReadBuffer(FieldT^, AField.Length+3);
              END
            ELSE
              BEGIN
                FDatFile.Position:=RecordPos+AField.StartPos;
                FDatFile.ReadBuffer(FieldT^, AField.Length+3);
              END;
            FieldText:=FieldT;
            Delete(FieldText,Pos('!',FieldText),3);
          END;
        Result:=trim(FieldText);
      END;
  FINALLY
    Dispose(CharPointer);
  END;
end;

function TEpiDataFile.ReadIndexNoFromSortIndex(SortPos: Integer): Integer;
VAR
  n: Integer;
  pN: Array[0..3] of Byte Absolute n;
begin
  FSortIndex.Position:=(SortPos-1)*4;
  FSortIndex.Read(pN,4);
  Result:=n;
end;

procedure TEpiDataFile.ResetEpiDataFile;
var
  n:Integer;
begin
  IF Assigned(FFieldList) THEN FreeAndNil(FFieldList);
  IF Assigned(FValueLabels) THEN FreeAndNil(FValueLabels);
  IF FRecBuf<>NIL THEN
    BEGIN
      FreeMem(FRecBuf);
      FRecBuf:=NIL;
    END;
  FMemFile.Free;
  FMemFile:=NIL;
  FDatfile.Free;
  FDatfile:=NIL;
  {$I-}
  CloseFile(FIndexFile);
  n:=IOResult;
  {$I+}
  IF Assigned(FIndex) THEN FreeAndNil(FIndex);
  IF Assigned(FSortIndex) THEN FreeAndNil(FSortIndex);
  IF Assigned(FFieldNames) THEN FreeAndNil(FFieldNames);
  IF Assigned(FFieldNamesList) THEN FreeAndNil(FFieldNamesList);
  IF Assigned(FDefList) THEN
    BEGIN
      FOR n:=0 TO FDefList.Count-1 DO
        TeField(FDefList.Objects[n]).Free;
      FreeAndNil(FDefList);
    END;
  IF (FIsRelateTop) AND (FGlobalDefList<>NIL) THEN
    BEGIN
      FOR n:=0 TO FGlobalDefList.Count-1 DO
        TeField(FGlobalDefList.Objects[n]).Free;
      FreeAndNil(FGlobalDefList);
    END;
  IF Assigned(FBeforeFileCmds)   THEN DisposeCommandList(FBeforeFileCmds);
  IF Assigned(FAfterFileCmds)    THEN DisposeCommandList(FAfterFileCmds);
  IF Assigned(FBeforeRecordCmds) THEN DisposeCommandList(FBeforeRecordCmds);
  IF Assigned(FAfterRecordCmds)  THEN DisposeCommandList(FAfterRecordCmds);
  IF Assigned(FRecodeCmds)       THEN DisposeCommandList(FRecodeCmds);
//  IF Assigned(FLastCommands)     THEN DisposeCommandList(FLastCommands);
  IF Assigned(FAssertList)       THEN FAssertList.Free;
  IF Assigned(FBackupList)       THEN FBackupList.Free;
  IF Assigned(FChkTopComments) THEN FChkTopComments.Free;
  FChkTopComments:=NIL;
  FRecFilename:='';
  FQESFilename:='';
  FCHKFilename:='';
  FIndexFilename:='';
  FErrorCode:=0;
  FErrorText:='';
  FHasCrypt:=False;
  FHasIncludeCmd:=False;
  FFileModified:=False;
  FGlobalTypeCom:=False;
  FIsRelateFile:=False;
  FIsRelateTop:=True;
  FHasKeyUnique:=False;
end;

procedure TEpiDataFile.SaveCheckFile;
var
  CheckLines: TStringList;
begin
  if (FRECFilename='') then raise Exception.Create('Cannot save check file: No data file name found');
  if (FCHKFilename='') then FCHKFilename:=ChangeFileExt(FRECFilename,'.chk');
  TRY
    CheckLines:=GetCheckLines;
    if CheckLines.Count>0 then
      CheckLines.SaveToFile(FCHKFilename)
    else if FileExists(FCHKFilename) and ((FileGetAttr(FCHKFilename) and SysUtils.faReadOnly) = 0) then
      DeleteFile(FCHKFilename);
  EXCEPT
    raise Exception.Create('Error saving check file');
  end;
end;

function TEpiDataFile.SaveStructureToFile(filename: string; OverwriteExisting: boolean): boolean;
VAR
  TempResult:Boolean;
  N,TempInt,colorN:Integer;
  ff:ByteFile;
  s:string;
  aField: TeField;
begin
{
  IF (NOT Assigned(FFieldList)) OR (FFieldList.Count=0) THEN
    BEGIN
      raise Exception.Create('No fields defined');
      Result:=False;
      Exit;
    END;
  if (FRECFilename='') and (filename='') then
    begin
      raise Exception.Create('No data file name defined');
      result:=false;
      exit;
    end;
  FRECFilename:=filename;
  FRECFilename:=changeFileExt(FRECFilename,'.rec');
  if (fileexists(FRECFilename)) and (OverwriteExisting=false) then
    begin
      raise Exception.Create('Data file '+FRECFilename+' already exists');
      result:=false;
      exit;
    end;
  TempResult:=True;
  AssignFile(ff,FRECFilename);
  {$I-
  Rewrite(ff);
  TempInt:=IOResult;
  {$I+
  IF TempInt=0 THEN
    BEGIN
      //Check if datafile contains encrypt-field    //&&
      n:=0;
      FHasCrypt:=false;
      REPEAT
        IF  GetField(n).Fieldtype=ftCrypt then FHasCrypt:=true;
        INC(n);
      UNTIL (n=FFieldList.Count) or (FHasCrypt);
      IF FHasCrypt THEN
        BEGIN
          IF FKey='' THEN
            BEGIN
              s:='';
              if Assigned(FOnRequestPassword) then FOnRequestPassword(self,rpCreate,s);
              FKey:=s;
            END;  //if key already assigned
          if (FKey='') then raise Exception.Create('A password is needed for data files with encrypted fields');
        END  //if HasCrypt
      ELSE FKey:='';
      //Write No of fields + background colour + FileLabel
      peWrite(ff,IntToStr(FFieldList.Count)+' 1');
      IF NOT FEpiInfoFieldNaming THEN peWrite(ff,' VLAB');
      IF FKey<>'' THEN peWrite(ff,' ~kq:'+EncryptString(FKey,FKey)+':kq~');
      IF trim(FFileLabel)<>'' THEN peWrite(ff,' Filelabel: '+FFilelabel);
      peWrite(ff,chr(NewLine));
      peWrite(ff,chr(LineFeed));
      FRecLength:=0;
      FOR n:=0 TO FFieldList.Count-1 DO
        BEGIN
          aField:=GetField(n);
          WITH aField DO
            BEGIN
              //write fieldchar
              IF (fieldtype=ftInteger) OR (fieldtype=ftFloat) OR (fieldtype=ftIDNUM)
              THEN peWrite(ff,'#') ELSE peWrite(ff,'_');
              peWrite(ff,FormatStr(Name,10));   //Name of field
              peWrite(ff,' ');                   //Space required for some unknown reason
              peWrite(ff,FormatInt(QuestX,4));  //Question X-position
              peWrite(ff,FormatInt(QuestY,4));  //Question Y-position
              peWrite(ff,FormatInt(30,4));       //Question colorcode
              peWrite(ff,FormatInt(FFieldX,4));  //Entry X-position
              peWrite(ff,FormatInt(FFieldY,4));  //Entry Y-position
              //Write FieldType
              // 0=Question without entryfield, i.e. text only
              // 100+Number of decimals = Floating point number
              // For all other: use the fieldtype-code (fieldtype)
              IF fieldtype=ftQuestion THEN peWrite(ff,FormatInt(0,4))
                ELSE IF (fieldtype=ftFloat) AND (FNumDecimals>0) THEN peWrite(ff,FormatInt(100+fNumDecimals,4))
                  ELSE peWrite(ff,FormatInt(ORD(fieldtype),4));
              //Write length of field - use 0 for text only
              IF fieldtype=ftQuestion THEN peWrite(ff,FormatInt(0,4))
              ELSE
                BEGIN
                  peWrite(ff,FormatInt(FLength,4));
                  FRecLength:=FRecLength+FLength;
                END;
              //write entry colorcode - special use in encrypted fields (holds entrylength of field)
              IF fieldtype<>ftCrypt THEN colorN:=112   //&&
              ELSE
                BEGIN
                  IF FCryptEntryLength<15 THEN colorN:=111+FCryptEntryLength ELSE colorN:=FCryptEntryLength;
                END;  //else
              peWrite(ff,FormatInt(colorN,4));         //Entry colorcode
              peWrite(ff,' ');                      //Another unnescessary blank
              if FOriginalQuest='' then FOriginalQuest:=Question;
              peWrite(ff,FOriginalQuest);
              peWrite(ff,chr(NewLine));
              peWrite(ff,chr(LineFeed));
            END;  //with
        END;  //for n
      FOffset:=Filesize(ff);
      FCurRecModified:=False;
      FShortRecLength:=FRecLength;
      FRecLength:=FRecLength+((FRecLength DIV MaxRecLineLength)+1)*3;  //Add NewLine+LineFeed+Terminatorchar.
      FNumRecords:=0;
      FCurRecord:=NewRecord;
      FHasEOFMarker:=False;
      CloseFile(ff);
    END  //if TempInt=0
  ELSE TempResult:=False;
  Result:=TempResult;
  }
end;

function TEpiDataFile.SearchIndex(IndexNo: Integer; SearchStr: string): LongInt;
VAR
  Found:Boolean;
  tmpCurRec: LongInt;
begin
  Found:=False;
  tmpCurRec:=0;
  WHILE (tmpCurRec<FNumRecords) AND (NOT Found) DO
    BEGIN
      INC(tmpCurRec);
      Found:=(AnsiCompareText(SearchStr,ReadFromIndex(IndexNo,tmpCurRec))=0);
    END;
  IF Found THEN Result:=tmpCurRec ELSE Result:=-1;
end;

function TEpiDataFile.SearchIndexFrom(IndexNo: Integer; SearchStr: string; RecNo: Integer; direction: TDirections): LongInt;
VAR
  Found:Boolean;
  tmpCurRec,EndRec: LongInt;
begin
  Found:=False;
  tmpCurRec:=RecNo;
  CASE direction OF
    dirForward,dirFirst:  BEGIN  EndRec:=FNumRecords;  DEC(tmpCurRec);   END;
    dirBackward,dirLast:  BEGIN  EndRec:=1;               INC(tmpCurRec);   END;
    dirAbsolute:          BEGIN  EndRec:=RecNo;           INC(tmpCurRec);   END;
  END;
  WHILE (tmpCurRec<>EndRec) AND (NOT Found) DO
    BEGIN
      IF (direction=dirForward) OR (direction=dirFirst) THEN INC(tmpCurRec) ELSE DEC(tmpCurRec);
      Found:=(AnsiCompareText(SearchStr,trim(ReadFromIndex(IndexNo,tmpCurRec)))=0);
    END;  //while
  IF Found THEN Result:=tmpCurRec ELSE Result:=-1;
end;

procedure TEpiDataFile.SetGlobalMissingValues(Index: Integer; Value: string);
begin
  FGlobalMissingValues[Index]:=Value;
end;

procedure TEpiDataFile.SetIndexFields(Index, Value: Integer);
begin
  FIndexfields[Index]:=Value;
end;

procedure TEpiDataFile.SetIndexIsUnique(Index: Integer; Value: Boolean);
begin
  FIndexIsUnique[Index]:=Value;
end;

function TEpiDataFile.TextPos(var F: Textfile): Longint;
begin
  With TTextRec(F) DO
    BEGIN
      Result:=SetFilePointer(Handle,0,nil,FILE_CURRENT);
      IF Mode=FMOutput THEN INC(Result, BufPos)
      ELSE IF BufEnd<>0 THEN Dec(Result, BufEnd-BufPos);
    END;
end;

procedure TEpiDataFile.Write(RecNum: Integer);
VAR
  wrN,n,repcounter,ecode:Integer;
  TempS:String[80];
  s:string[30];
  eRecString,s2:String;
  ABuf: PRecBuf;
  BufCount,LineCharCount: Integer;
  ok:Boolean;
begin
{
  ABuf:=FRecBuf;
  IF RecNum=NewRecord THEN
    BEGIN
      IF FHasEOFMarker THEN
        BEGIN
          FHasEOFMarker:=False;
          IF FStoredInMemory THEN
            BEGIN
              FMemFile.SetSize(FMemFile.Size-1+FRecLength);
              FMemFile.Position:=FMemFile.Size-FRecLength;
            END
          ELSE FDatfile.Position:=FDatfile.Size-1;
        END    //if HasEOFMarker
      ELSE
        BEGIN
          IF FStoredInMemory THEN
            BEGIN
              FMemFile.SetSize(FMemFile.Size+FRecLength);
              FMemFile.Position:=FMemFile.Size-FRecLength;
            END
          ELSE FDatfile.Position:=FDatFile.Size;
        END;  //if not HasEOFMarker
      INC(FNumRecords);
      //Add empty record to indexfile and resize index

      //TODO: Add handling of IndexFiles
      IF (false) and (FIndexCount>0) THEN
        BEGIN
          repcounter:=0;
          REPEAT
            ok:=True;
            INC(repcounter);
            TRY
              s:='';
              Seek(FIndexFile,Filesize(FIndexFile));
              FOR n:=1 TO FIndexCount DO System.Write(FIndexfile,s);
            EXCEPT
              ok:=False;
              IF repcounter>=3 THEN
                //IF eDlg(Format(Lang(20460,'%d attempts of writing current record failed~~Retry?'),[repcounter]),     //20460=%d attempts of writing current record failed~~Retry?
                //mtWarning,[mbYes,mbNo],0)=mrNo THEN
                //  BEGIN
                //    ok:=True;
                //    repcounter:=-1;
                //  END;
                ok:=True;
                Raise EWriteError.Create(Lang(20462,'Current record not saved!'));
            END;  //try..except
          UNTIL ok;
          //IF repcounter=-1 THEN raise EWriteError.Create(Lang(20462));  //20462=Current record not saved!

          FIndex.SetSize(FIndex.Size+(FIndexCount*31));
          //If assigned(df^.SortIndex) THEN....
        END;
    END  //if NewRecord
  ELSE
    BEGIN
      IF StoredInMemory THEN FMemFile.position:=FOffSet+((RecNum-1)*FRecLength)
      ELSE FDatfile.position:=FOffset+((RecNum-1)*FRecLength);
    END;
  eRecString:='';
  BufCount:=0;
  LineCharCount:=MaxRecLineLength;
  FOR wrN:=0 TO FFieldList.Count-1 DO    //Iterate through all fields
    BEGIN
      WITH TeField(FFieldList.Items[wrN]) DO
        BEGIN
          IF (fieldtype in [ftToday,ftEuroToday,ftYMDToday]) THEN FFieldText:=mibDateToStr(now,fieldtype);
          //Add indices
          //TODO: Add handling of Index files
          IF (false) and (TeField(FFieldList.Items[wrN]).FIndex>0) THEN
            BEGIN
              IF RecNum=NewRecord THEN n:=FNumRecords ELSE n:=RecNum;
              IF fieldtype=ftCrypt THEN s:=Copy(FFieldText,1,21)
              ELSE s:=Copy(FFieldText,1,30);
              CASE fieldtype OF
                ftInteger,ftFloat: s:=FormatNumberToIndex(s);
                ftDate,ftEuroDate,ftToday,ftEuroToday,ftYMDDate,ftYMDToday:  //&&
                  s:=Format('%30s',[FloatToStr(mibStrToDate(s,fieldtype))]);
               ELSE
                 s:=Format('%-30s',[s]);
              END;  //case
              WriteToIndex(FIndex,n,s);
              //WriteToSortIndex ???
              //Write to indexfile
              repcounter:=0;
              ok:=True;
              REPEAT
                INC(repcounter);
                TRY
                  Seek(FIndexFile,((n-1)*FIndexCount)+TeField(FFieldList.Items[wrN]).FIndex);
                  IF fieldtype=ftCrypt THEN    //&&
                    BEGIN
                      s2:=s;
                      s:=EncryptString(trim(s2),FKey);
                    END;
                  System.Write(FIndexFile,s);   //&&
                EXCEPT
                  ok:=False;
                  IF repcounter>=3 THEN
                    //IF eDlg(Format(Lang(20460),[repcounter]),     //20460=%d attempts of writing current record failed~~Retry?
                    //mtWarning,[mbYes,mbNo],0)=mrNo THEN
                    //  BEGIN
                    //    ok:=True;
                    //    repcounter:=-1;
                    //  END;
                    ok:=True;
                    raise EWriteError.Create(Lang(20462,'Current record not saved!'));  //20462=Current record not saved!
                END;  //try..except
              UNTIL ok;
              //IF repcounter=-1 THEN raise EWriteError.Create(Lang(20462));  //20462=Current record not saved!
            END;
          //Make RecString
          TempS:=FFieldText;
          IF (fieldtype=ftCrypt) AND (FKey<>'') THEN TempS:=EncryptString(trim(TempS),FKey);
          IF ((fieldtype=ftInteger) or (fieldtype=ftFloat))
            AND (Trim(TempS)<>'') THEN
            BEGIN
              IF fieldtype=ftFloat THEN
                BEGIN
                  WHILE pos(',',TempS)<>0 DO TempS[Pos(',',TempS)]:='.';
                  TempS:=FormatFloating(TempS,FLength);
                END  //if ftFloat
              ELSE
                TempS:=FormatInt(strToInt(TempS),FLength);
            END   //if ftInteger or ftFloat
          ELSE IF fieldtype<>ftQuestion THEN TempS:=FormatStr(TempS,Flength);
          FOR n:=1 TO FLength DO
            BEGIN
              ABuf^[BufCount]:=TempS[n];
              DEC(LineCharCount);
              INC(BufCount);
              IF LinecharCount=0 THEN
                BEGIN
                  Move(EOLchars, ABuf^[BufCount], length(EOLChars));
                  INC(BufCount, sizeof(EOLchars));
                  LinecharCount:=MaxRecLineLength;
                END;
            END;
        END;  //with
    END;  //for wrN - iterate through fields
  IF (LineCharCount<>MaxRecLineLength)
  THEN Move(EOLchars, ABuf^[BufCount], sizeof(EOLchars));
  IF FCurRecDeleted THEN
    BEGIN
      WHILE ABuf^[BufCount]<>'!' DO Dec(BufCount);
      ABuf^[BufCount]:='?';
    END
  else if FCurRecVerified then
    begin
      WHILE ABuf^[BufCount]<>'!' DO Dec(BufCount);
      ABuf^[BufCount]:='^';
    END;
  repcounter:=0;
  REPEAT
    ok:=True;
    INC(repcounter);
    TRY
      IF StoredInMemory THEN FMemFile.WriteBuffer(ABuf^,FRecLength)
      ELSE FDatFile.WriteBuffer(ABuf^,FRecLength);
    EXCEPT
      ok:=False;
      IF repcounter>=3 THEN
        //IF eDlg(Format(Lang(20460),[repcounter]),     //20460=%d attempts of writing current record failed~~Retry?
        //mtWarning,[mbYes,mbNo],0)=mrNo THEN
        // BEGIN
        //    ok:=True;
        //    repcounter:=-1;
        //  END;
        ok:=True;
        raise EWriteError.Create(Lang(20462,'Current record not saved!'));  //20462=Current record not saved!
    END;
  UNTIL ok;
  //IF repcounter=-1 THEN raise EWriteError.Create(Lang(20462));  //20462=Current record not saved!
  FCurRecModified:=False;
  FFileModified:=True;
  IF (FIDNUMField<>-1) AND (RecNum=NewRecord) THEN INC(FCurIDNumber);
  }
end;

procedure TEpiDataFile.WriteIndexNoToSortIndex(SortPos, num: Integer);
VAR
  pNum:ARRAY[0..3] of byte absolute num;
begin
  FSortIndex.Position:=(SortPos-1)*4;
  FSortIndex.Write(pNum,4);
end;

procedure TEpiDataFile.WriteToIndex(IndexNo, RecNo: Integer; s: string);
VAR
  tmpS:string[30];
  ptmpS:Array[0..30] of byte absolute tmpS;
begin
  FIndex.Position:=31+( (RecNo-1)*(31*FIndexCount) ) + ( 31*(IndexNo-1) );
  tmpS:=s;
  FIndex.Write(ptmpS,31);
end;

end.

