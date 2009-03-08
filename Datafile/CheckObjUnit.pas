unit CheckObjUnit;

interface

USES
  {$IFNDEF FPC}windows,{$ENDIF}
  Controls, Messages, Forms,Dialogs,Graphics,SysUtils,Classes,
  UeFields, UEpiDataFile, UEpiDataConstants, UEpiTypes, UValueLabels;



type
  str30=string[30];
  nwTypess=(nwAny,nwSameLine,nwSameKeepQuotes,nwKeepSpaces);

  TParser = class(TObject)
  private
    FLines:       TStringList;
    FCurLin:      String;
    FCurLinIndex: Integer;
    FEndOfLines:  Boolean;
    function      FGetCurLinIndex:Integer;
  public
    Constructor   Create(inputlines:String);
    Destructor    Destroy;  override;
    Function      GetToken(nwType: nwTypess):string;
    Function      GetUpperToken(nwType: nwTypess):String;
    Function      GetLowerToken(nwType: nwTypess):string;
    Function      GetLineAndFlush:string;
    Function      GetWholeLine:String;
    Procedure     CommentCurLine;
    property      EndOfLines: Boolean read FEndOfLines write FEndOfLines;
    property      GetCurLinIndex: Integer read FGetCurLinIndex write FCurLinIndex;
  end;

  TExportTypes=(etTxt,etDBase,etXLS,etStata,etRecToQes,etListData,etSPSS,etSAS,etEpiData,etCodebook,etASP);
  TBeepTypes=(btWarning,btConfirmation,btStandard);
  TIndexFields=Array[1..MaxIndices] OF Integer;
  TIndexIsUnique=Array[1..MaxIndices] OF Boolean;
  TIndexFile=File of str30;
  TDirections=(dirForward,dirBackward,dirFirst,dirLast,dirAbsolute);
  TLastSelectFilestype=(sfNone,sfMakeDatafile,sfRevise,sfAssert,sfRecode,sfRec2Qes,sfValDup,
                        sfImportStata,sfMerge);

  //Types related to searching datafiles
  TSearchStyle=(ssEquals,ssBeginsWith,ssContains);
  TScopeStyle=(ssForward,ssBackWard,ssAll);

  TFindOperators = (opNone,opEq,opNEq,opGT,opLT,opBW,opEW,opCON);

  TCrites=Record
    Fieldno: Integer;
    Opr: TFindOperators;
    SearchText: String;
    SearchValue: Double;
  END;



  Commands=(cmdIF,cmdHelp,cmdHide,cmdUnhide,cmdClear,cmdGoTo,cmdComLegal,
            cmdExit,cmdDefine,cmdAutosave,cmdConfirm,cmdTypeString,
            cmdRelate,cmdIgnoreMissing,cmdWriteNote,cmdBackup,cmdBeep,cmdLoad,cmdExecute,
            cmdColor,cmdMissingAll,cmdQuit,cmdCopyToClipboard,cmdShowLastRecord,cmdDefaultAll,cmdLet,cmdComment,cmdLeaveField);      
            //NB! Insert new codes BEFORE cmdLet


                                                                                                             //cmdLeaveField is only used internally (in conn. with relate)
CONST
  CommandNames:Array[Commands] of String[16]=
    ('IF','HELP','HIDE','UNHIDE','CLEAR','GOTO','COMMENT','EXIT','DEFINE',
    'AUTOSAVE','CONFIRM','TYPE','RELATE','IGNOREMISSING','WRITENOTE','BACKUP',
    'BEEP','LOAD','EXECUTE','COLOR','MISSINGVALUE','QUIT','COPYTOCLIPBOARD',
    'SHOWLASTRECORD','DEFAULTVALUE',
    'LET','dummy','leavefield');

TYPE
  TChkCommands = class;
  TChangeFieldActions=(cfNext,cfPrev,cfFirst,cfLast,cfValidate);
  TLeaveStyles=(lsEnter,lsBrowse,lsJumpFirst,lsJumpLast,lsChangeRec,lsNone);
  PCmds=^TCmds;
  TCmds=RECORD
          Next: PCmds;
          CASE Command: Commands OF
            cmdIF:
               (IfExpr:          String[200];
                IfShowExpr:      String[200];
                IfCmds:          TChkCommands;
                ElseCmds:        TChkCommands);
            cmdHelp:
               (HelpString:      String[250];
                HelpType:        TMsgDlgType;
                HelpKeys:        String[10]);
            cmdHide,cmdUnHide,cmdClear,cmdGoto:
               (HideVarNumber:   Integer;
                HideVarName:     String[10]);
            cmdComLegal:
               (clVarNumber:     Integer;
                ValueLabelName:  String[40];
                ValueLabel:      TValueLabelSet;
                ValueLabelType:  TValueLabelSetType;
                ValueLabelUse:   string[50];
                ShowList:        Boolean);
            cmdTypeString:
               (tsVarNumber:     Integer;
                TypeText:        String[40];
                Typecolor:       TColor);
            cmdRelate:
               (RelField:        String[10];
                RelFileNo:       Integer;
                RelFileStr:      String[200];
                One2One:         Boolean);
            cmdLet:
               (VarName:         String[20];
                VarNumber:       Integer;
                VarIsField:      Boolean;
                CodedWithLET:    Boolean;
                LetExpr:         String[200]);
            cmdComment:
               (Comment:         String[200]);
            cmdDefine:
               (FName:           String[20];
                FeltType:        TFieldtypes;
                FLength:         Integer;
                FNumDecimals:    Byte;
                FScope:          TScopes);
            cmdWriteNote:
               (FNote:           String[200];
                ShowNotes:       Boolean);
            cmdCopyToClipboard:
               (CopyStr:         String[200]);
            cmdBackup:
               (DestLib:         String[200];
                zipit:           Boolean;
                encryptit:       Boolean;
                filename:        String[200];
                pw:              string[30];
                dateit:          Boolean);
            cmdBeep:
               (BeepType:        TBeepTypes);
            cmdLoad:
               (DLLName:         String[200]);
            cmdExecute:
               (ExecCmdLine:     String[255];
                ExecParams:      String[255];
                ExecHide:        Boolean;
                ExecWait:        Boolean);
            cmdLeaveField:
               (cLeaveStyle:     TLeaveStyles;
                IsLastField:     Boolean);
            cmdColor:
               (ColorCmd:        Byte;      //1=color question, 2=color data,  3=color background, 4=color fieldname
                TxtColor:        Byte;
                BgColor:         Byte;
                IsEpiInfoNo:     Boolean;
                CFieldno:        Byte);
          END;

  PAssert=^TAssert;
  TAssert=RECORD
      AssName: String[40];
      AssExpr: String[200];
      OrigExpr: String[200];
      ViolCount: Integer;
      Violaters: String;
    END;

  PDefVar=^TDefVar;
  TDefVar=RECORD
    FName:             String[16];
    Felttype:          TFieldtypes;
    FLength:           Integer;
    FNumDecimals:      Byte;
    FFieldText:        String;
    FScope:            TScopes;
    END;

  TChkCommands = class(TObject)
  private
    FList: TList;
    function GetCount:integer;
    function GetItem(index:integer):PCmds;
    Procedure DisposeCommandList(aList: TList);
  public
    constructor create;
    destructor  destroy; override;
    procedure   AddCommand(cmd: PCmds);
    procedure   Clone(dest: TChkCommands);
    function    NewCmd:PCmds;
    property    count:integer read GetCount;
    property    items[index:integer]:PCmds read GetItem; default;
    property    List:TList read FList;
  end;

  TCheckObj = class(TObject)
  private
    FParser:         TParser;
    FErrorList:      TStringList;
    FFieldNameList:  TStrings;
    LegList:         TStrings;
    FEndOfChk:       Boolean;
    FMultiLineError: Boolean;
    FTempResult:     Boolean;
    FCheckFileMode:  Boolean;
    df:              TEpiDataFile;
    FirstTopFlaw,SaveTopFlawsAsComments: Boolean;
    FirstFieldFlaw,SaveFieldFlawsAsComments: Boolean;
    CommentsAddedToCheckFile: Boolean;
    ReadingLabelLibrary: Boolean;
    InIfBlock:       Boolean;
    CurCommand:      String;
    tmpField:        TeField;
    FOnTranslate:    TTranslateEvent;
    Function  GetErrorList: String;
    Procedure ReportError(CONST ErrStr:String);
    Procedure RetrieveFieldBlock;
    Procedure RetrieveLabelBlock;
    Procedure RetrieveLabel;
    Procedure RetrieveAssertBlock;
    Procedure GetCommandList(var CmdList:TChkCommands);
    Procedure GetCommand(var CmdList:TChkCommands);
    Procedure AddTopComment;
    Procedure RetrieveFlawBlock;
    Procedure RetrieveRange;
    Procedure RetrieveLegals;
    Procedure RetrieveAutoJump;
    Procedure RetrieveJumps;
    Procedure RetrieveMissingValues;
    Procedure RetrieveDefaultValue;
    Procedure RetrieveAutosearch;
    function  RetrieveCommentLegal(AsCommand:Boolean; var ValueLabelType:TValueLabelSetType; var Show:boolean; var ValueLabelUse:string):TValueLabelSet;
    Procedure RetrieveType;
    Procedure RetrieveKeys;
    Procedure AddFieldFlawComment;
    Procedure HandleBooleanConditions(VAR s:String);
    Procedure AddFieldComment;
    Function  Translate(stringnumber:Integer; origstring:string):string;
  public
    Constructor Create;
    Destructor  Destroy; override;
    Function    ApplyChecks(ADatafile: TEpiDataFile; chkLines: String): Boolean;
    property    ErrorList: String read GetErrorList;
    property    MultiLineError: Boolean read FMultiLineError write FMultiLineError;
    property    EndOfChk: Boolean read FEndOfChk;
    property    ChkFileMode: Boolean read FCheckFileMode write FCheckFileMode;
    Property    OnTranslate: TTranslateEvent read FOnTranslate write FOnTranslate;
  end;

  TCheckWriter = class(TObject)
  private
    df: TEpiDataFile;
    FInitBlocks: TStringList;
    FLastFieldBlock: TStringList;
    FieldComments: TStringList;
    Function Label2Text(aValueLabelSet:TValueLabelSet; NumSpc:Byte):String;
    Procedure AddCommandList(sList:TStringList; CmdList:TChkCommands; Indent:Byte);
    procedure ChecksToStrings;
  public
    constructor create(EpiDataFile: TEpiDataFile);
    destructor  destroy;
    procedure   Rewrite;       //clears stringslists and create new check file lines
    procedure   SaveToFile(filename: string);
    Procedure   FieldBlockToStrings(FieldNo:Integer; Indent:Byte);
    property    FieldBlock:TStringList read FLastFieldBlock;        //Holds the checklines of the last fieldblock created by FieldBlockToStrings
    property    CheckLines:TStringList read FInitBlocks;            //Holds intire checkfile's lines after rewrite og create
  end;


implementation

USES
  UEpiUtils,
  {$IFNDEF FPC}
  UExtUDF,
  {$ENDIF}
  epiUDFTypes;

{$IFDEF FPC}
const
  COLOR_ENDCOLORS = 30;
{$ENDIF}


// ==============================  TParser =======================================

Constructor TParser.Create(inputlines:String);
BEGIN
  inherited create;
  FLines:=TStringList.Create;
  FLines.Text:=inputLines;
  FCurLin:='';
  FCurLinIndex:=-1;
  FEndOfLines:=False;
END;  //tparser.create

Destructor TParser.Destroy;
BEGIN
  FLines.Free;
  inherited destroy;
END;  //tparser.destroy

Function TParser.GetToken(nwType: nwTypess):string;
VAR
  n: Integer;
  Stop: Boolean;
BEGIN
  IF (trim(FCurLin)='') AND (nwType=nwAny) THEN
    BEGIN
      INC(FCurLinIndex);
      IF FCurLinIndex<FLines.Count
      THEN FCurLin:=Trim(FLines[FCurLinIndex])+' '
      ELSE FEndOfLines:=True;
    END;
  IF (trim(FCurLin)<>'') THEN
    BEGIN
      IF Copy(trim(FCurLin),1,1)='*' THEN
        BEGIN
          Result:=FCurLin;
          FCurLin:='';
        END
      ELSE
        BEGIN
          Result:=Copy(FCurLin,1,Pos(' ',FCurLin)-1);
          IF (Result[1]='"') THEN
            BEGIN
              IF (Result[Length(Result)]='"')  AND (Length(Result)>1) THEN
                BEGIN
                  {Only one word is found in quotationmarks}
                  IF NOT (nwType=nwSameKeepQuotes) THEN Result:=Copy(Result,2,Length(Result)-2);
                  Delete(FCurLin,1,Pos(' ',FCurlin));
                END
              ELSE
                BEGIN
                  {Multiple words found in quotationsmarks}
                  n:=1;
                  Result:='';
                  Stop:=False;
                  REPEAT
                    Result:=Result+FCurLin[n];
                    INC(n);
                    IF n>Length(FCurLin) THEN Stop:=True
                    ELSE IF FCurLin[n]='"' THEN Stop:=True;
                  UNTIL Stop;
                  Result:=Result+'"';
                  IF NOT (nwType=nwKeepSpaces) THEN Result:=trim(Result);
                  IF NOT (nwType=nwSameKeepQuotes) THEN
                    BEGIN
                      Delete(Result,1,1);
                      Delete(FCurLin,1,n);
                      IF Result[Length(Result)]='"' THEN Delete(Result,Length(Result),1);
                    END
                  ELSE Delete(FCurLin,1,n);
                END;
            END
          ELSE Delete(FCurLin,1,Pos(' ',FCurLin));
          FCurLin:=trim(FCurLin)+' ';
        END;
    END
  ELSE Result:='';
END;  //tparser.GetToken

Function TParser.GetUpperToken(nwType: nwTypess):string;
BEGIN
  Result:=AnsiUpperCase(GetToken(nwType));
END;

Function TParser.GetLowerToken(nwType: nwTypess):string;
BEGIN
  Result:=AnsiLowerCase(GetToken(nwType));
END;

Function TParser.GetLineAndFlush:String;
BEGIN
  Result:=FCurLin;
  FCurLin:='';
END;   //TParser.GetLineAndFlush

Function TParser.GetWholeLine:String;
BEGIN
  Result:=FLines[FCurLinIndex];
  FCurLin:='';
END;

Function TParser.FGetCurLinIndex: Integer;
BEGIN
  IF FEndOfLines THEN Result:=FLines.Count ELSE Result:=FCurLinIndex;
END;

Procedure TParser.CommentCurLine;
BEGIN
  FLines[FCurLinIndex]:='* '+FLines[FCurLinIndex];
END;


// ==============================  TCheckObj =======================================


Constructor TCheckObj.create;
BEGIN
  inherited create;
  FErrorList:=TStringList.Create;
  FParser:=NIL;
  FFieldNameList:=NIL;
  LegList:=TStringList.Create;
  FMultiLineError:=True;
  FCheckFileMode:=False;
  CommentsAddedToCheckFile:=False;
  ReadingLabelLibrary:=False;
  df:=NIL;
END;   //create

Destructor TCheckObj.Destroy;
begin
  FErrorList.Free;
  IF Assigned(FParser) THEN FParser.Free;
  IF Assigned(LegList) THEN LegList.Free;
  IF Assigned(FFieldNameList) THEN FFieldNameList.Free;
  Inherited Destroy;
end;   //destroy

Function TCheckObj.Translate(stringnumber:Integer; origstring:string):string;
begin
  IF Assigned(FOnTranslate) THEN Result:=FOnTranslate(stringnumber, origstring)
  ELSE Result:=origstring;
end;

Function TCheckObj.GetErrorList: String;
begin
  Result:=FErrorList.Text;
end;

Function TCheckObj.ApplyChecks(ADatafile: TEpiDataFile; chkLines: String): Boolean;
VAR
  aN,n,n2: Integer;
  aFound: Boolean;
  tmpString,NewChkLines,IncludeStrings: TStringList;
  s: string;
  tmpCommands: TChkCommands;
begin
  Result:=False;
  df:=ADatafile;
  FErrorList.Clear;
  IF FMultiLineError THEN
    BEGIN
      FErrorList.Add('');
      FErrorList.Add(Format(translate(22794,'The check-file %s contains the following errors:'),[df.CHKFilename])); 
      FErrorList.Add('');
      FErrorList.Add('');
    END;
  //Check if checkfile uses INCLUDE command
  IF pos('INCLUDE',AnsiUpperCase(chkLines))>0 THEN
    BEGIN
      //checkfile might contain INCLUDE command
      TRY
        tmpString:=TStringList.Create;
        IncludeStrings:=TStringList.Create;
        NewChkLines:=TStringList.Create;
        tmpString.Text:=chkLines;
        FOR n:=0 TO tmpString.Count-1 DO
          BEGIN
            IF copy(AnsiUpperCase(trim(tmpString[n])),1,8)='INCLUDE ' THEN
              BEGIN
                //Include command found
                df.HasIncludeCmd:=True;
                IF FCheckFileMode THEN
                  BEGIN
                    ReportError(translate(22868,'Checkfiles with INCLUDE commands cannot be revised with the Add/Revise function')); 
                    Result:=False;
                    Exit;
                  END;
                s:=trim(copy(trim(tmpString[n]),9,Length(tmpString[n])));
                //IF ExtractFileExt(s)='' THEN s:=ChangeFileExt(s,'.chk');
                IF (s[1]='"') AND (s[Length(s)]='"') THEN s:=Copy(s,2,Length(s)-2);
                IF NOT FileExists(s) THEN
                  BEGIN
                    ReportError(Format(translate(22870,'Includefile %s not found'),[s])); 
                    Result:=False;
                    Exit;
                  END
                ELSE
                  BEGIN
                    TRY
                      IncludeStrings.Clear;
                      IncludeStrings.LoadFromFile(s);
                      FOR n2:=0 TO IncludeStrings.Count-1 DO
                        NewChkLines.Append(IncludeStrings[n2])
                    EXCEPT
                      ReportError(Format(translate(22872,'Error reading includefile %s'),[s]));  
                      Result:=False;
                      Exit;
                    END;  //try..except
                  END;  //else
              END  //if include word found
            ELSE NewChkLines.Append(tmpString[n]);
          END;  //for
        chkLines:=NewChkLines.Text;
      FINALLY
        tmpString.Free;
        IncludeStrings.Free;
        NewChkLines.Free;
      END;  //try..finally
    END;  //if chklines has include
  IF Assigned(FParser) THEN FParser.Free;
  FParser:=TParser.Create(chkLines);
  FirstTopFlaw:=True;
  SaveTopFlawsAsComments:=False;
  FirstFieldFlaw:=True;
  SaveFieldFlawsAsComments:=False;
  CommentsAddedToCheckFile:=False;
  InIfBlock:=False;
  IF Assigned(FFieldNameList) THEN FFieldNameList.Free;
  FFieldNameList:=TStringList.Create;
  FOR aN:=0 TO df.NumFields-1 DO
    FFieldnameList.Add(AnsiUpperCase(df[aN].FieldName));
  FTempResult:=True;
  IF (FCheckFileMode) AND (Assigned(df.ChkTopComments)) THEN df.ChkTopComments.Append('* '+translate(22796,'Revised')+' '+  
    FormatDateTime('dd mmm yyyy hh":"nn',now));
  REPEAT    //Read top-level check commands
    aFound:=False;
    CurCommand:=FParser.GetUpperToken(nwAny);  

    {Legal commands outside fieldblock are
      Fieldname..End
      Comments (*)
      LabelBlock..End
      AssertBlock..End
      Before File..End
      After File..End
      Before Record..End
      After Record..End
    }

    df.FocusedField:=FFieldnameList.IndexOf(CurCommand);
    IF df.FocusedField>-1 THEN RetrieveFieldBlock
    ELSE IF CurCommand='LABELBLOCK' THEN RetrieveLabelBlock
    ELSE IF CurCommand='CONSISTENCYBLOCK' THEN RetrieveAssertBlock
    ELSE IF CurCommand='BEFORE' THEN
      BEGIN
        CurCommand:=FParser.GetUpperToken(nwSameLine);  
        IF CurCommand='FILE' THEN
          begin
            tmpCommands:=TChkCommands(df.BeforeFileCmds);
            GetCommandList(tmpCommands);
            df.BeforeFileCmds:=tmpCommands;
          end
        ELSE IF CurCommand='RECORD' THEN
          begin
            tmpCommands:=TChkCommands(df.BeforeRecordCmds);
            GetCommandList(tmpCommands);
            df.BeforeRecordCmds:=tmpCommands;
          end
        ELSE
          BEGIN
            ReportError(translate(22798,'Unknown command after BEFORE'));
            FTempResult:=False;
          END;
      END
    ELSE IF CurCommand='AFTER' THEN
      BEGIN
        CurCommand:=FParser.GetUpperToken(nwSameLine);  
        IF CurCommand='FILE' THEN
          begin
            tmpCommands:=TChkCommands(df.AfterFileCmds);
            GetCommandList(tmpCommands);
            df.AfterFileCmds:=tmpCommands;
          end
        ELSE IF CurCommand='RECORD' THEN
          begin
            tmpCommands:=TChkCommands(df.AfterRecordCmds);
            GetCommandList(tmpCommands);
            df.AfterRecordCmds:=tmpCommands;
          end
        ELSE
          BEGIN
            ReportError(translate(22800,'Unknown command after AFTER'));
            FTempResult:=False;
          END;
      END
    ELSE IF CurCommand='RECODEBLOCK' THEN
      begin
        tmpCommands:=TChkCommands(df.RecodeCmds);
        GetCommandList(tmpCommands);
        df.RecodeCmds:=tmpCommands;
      end
    ELSE IF CurCommand<>'' THEN
      BEGIN
        IF CurCommand[1]='*' THEN AddTopComment ELSE RetrieveFlawBlock;
      END;
  UNTIL FParser.EndOfLines;
  {Pack indexfield list}
  IF df.IndexCount>0 THEN
    BEGIN
      REPEAT
        FOR n:=1 TO MaxIndices-1 DO
          BEGIN
            IF df.IndexFields[n]=-1 THEN
              BEGIN
                FOR n2:=n+1 TO MaxIndices DO
                  BEGIN
                    IF df.Indexfields[n2]<>-1
                    THEN df[df.indexFields[n2]].Index:=df[df.indexFields[n2]].Index-1;
                    df.IndexFields[n2-1]:=df.IndexFields[n2];
                    df.IndexIsUnique[n2-1]:=df.IndexIsUnique[n2];
                    df.IndexFields[n2]:=-1;
                    df.IndexIsUnique[n2]:=False;
                  END;
              END;
          END;
      UNTIL df.IndexFields[1]<>-1;
    END;  //if indexCount>0
  Result:=FTempResult;
end;

Procedure TCheckObj.ReportError(CONST ErrStr:String);
VAR
  n:Integer;
BEGIN
  IF Assigned(Fparser) THEN n:=FParser.GetCurLinIndex+1 ELSE n:=0;
  {$IFNDEF epidat}
  IF FMultiLineError THEN
    BEGIN
      FErrorList.Append(Format(translate(22700,'%s in line %d:'),[ErrStr,n])); 
      IF Assigned(FParser) THEN FErrorList.Append(FParser.GetWholeLine);
      FErrorList.Append('');
    END
  ELSE FErrorList.Append(Format(translate(22702,'Line %d: %s'),[n,ErrStr]));
  {$ELSE}
  FErrorList.Append(Format(translate(22702,'Line %d: %s'),[n,ErrStr]));
  {$ENDIF}
END;  //procedure ReportError

Procedure TCheckObj.RetrieveFieldBlock;
VAR
  n:Integer;
  tmpBool: boolean;
  ValueLabelType: TValueLabelSetType;
  ValueLabelUse:  String;
  ValueLabelShow: Boolean;
  tmpCommands: TChkCommands;
BEGIN
  {Legal commands in fieldblocks are
    RANGE
    LEGAL
    COMMENT LEGAL
    MUSTENTER
    REPEAT
    JUMPS [RESET [x]]
    NOENTER
    IF
    LET eller et feltnavn
    AFTER ENTRY
    BEFORE ENTRY
    TYPE STATUSBAR "..."
    TYPE COMMENT
    KEY [UNIQUE] [n]
    ENTER
    CONFIRMFIELD
    TOPOFSCREEN
    MISSINGVALUE n [n [n]]
    DEFAULTVALUE x
    AUTOSEARCH [LIST] [SOUNDEX] FIELD1 [FIELD2 [FIELD3...]]
  }
  IF df[df.FocusedField].Fieldtype<>ftQuestion THEN
    BEGIN
      tmpField:=TeField.Create;
      tmpField.owner:=df;
      tmpField.FieldName:=df[df.FocusedField].FieldName;
      tmpField.Fieldtype:=df[df.FocusedField].Fieldtype;
      tmpField.Length:=df[df.FocusedField].Length;
      REPEAT
        CurCommand:=FParser.GetUpperToken(nwAny);  
        //n:=FFieldNameList.IndexOf(CurCommand);   HVORFOR ER DET TILFØJET??
        IF      CurCommand='RANGE'       THEN RetrieveRange
        ELSE IF CurCommand='LEGAL'       THEN RetrieveLegals
        ELSE IF CurCommand='MISSINGVALUE' THEN RetrieveMissingValues
        ELSE IF CurCommand='DEFAULTVALUE' THEN RetrieveDefaultValue
        ELSE IF CurCommand='AUTOSEARCH'   THEN RetrieveAutosearch
        ELSE IF CurCommand='MUSTENTER'   THEN tmpField.MustEnter:=True
        ELSE IF CurCommand='NOENTER'     THEN tmpField.NoEnter:=True
        ELSE IF CurCommand='TOPOFSCREEN' THEN
          BEGIN
            tmpField.TopOfScreen:=True;
            tmpField.TopOfScreenLines:=0;
            CurCommand:=FParser.GetToken(nwSameLine);  
            IF (CurCommand<>'') AND (IsInteger(CurCommand)) THEN tmpField.TopOfScreenLines:=StrToInt(CurCommand);
          END
        ELSE IF CurCommand='REPEAT'      THEN
          BEGIN
            tmpField.doRepeat:=True;
            df.HasRepeatField:=True;
          END
        ELSE IF (CurCommand='CODEFIELD') OR (CurCommand='CODES') THEN
          BEGIN
            ReportError(translate(22782,'CODEFIELD/CODES not supported. Please use TYPE COMMENT fieldname instead.')); 
            FTempResult:=False;
          END
        ELSE IF CurCommand='AUTOJUMP'     THEN RetrieveAutoJump
        ELSE IF CurCommand='JUMPS'        THEN RetrieveJumps
        ELSE IF CurCommand='COMMENT'      THEN
          begin
            tmpField.Valuelabel:=RetrieveCommentLegal(false,ValueLabelType,ValueLabelShow,ValueLabelUse);
            if tmpField.Valuelabel<>NIL then
              begin
                tmpField.ShowLegalPickList:=ValuelabelShow;
                tmpField.ValueLabelType:=ValueLabelType;
                tmpField.ValueLabelUse:=ValueLabelUse;
              end
          end
        ELSE IF CurCommand='TYPE'         THEN RetrieveType
        ELSE IF CurCommand='KEY'          THEN RetrieveKeys
        ELSE IF CurCommand='CONFIRMFIELD' THEN tmpField.Confirm:=True
        ELSE IF CurCommand='ENTER'        THEN FTempResult:=True
//          BEGIN
//            ReportError(translate(22784));  //'ENTER command not supported. Please use BEFORE/AFTER ENTRY instead.'
//            FTempResult:=False;
//          END
        ELSE IF CurCommand='BEFORE'       THEN
          BEGIN
            CurCommand:=FParser.GetUpperToken(nwSameLine);
            IF CurCommand='ENTRY' THEN
              begin
                tmpCommands:=TChkCommands(tmpField.BeforeCmds);
                GetCommandList(tmpCommands);
                tmpField.BeforeCmds:=tmpCommands;
              end
            ELSE
              BEGIN
                ReportError(translate(22786,'ENTRY expected'));
                FTempResult:=False;
              END;
          END
        ELSE IF CurCommand='AFTER' THEN
          BEGIN
            CurCommand:=Fparser.GetUpperToken(nwSameLine);
            IF CurCommand='ENTRY' THEN
              begin
                tmpCommands:=TChkCommands(tmpField.AfterCmds);
                GetCommandList(tmpCommands);
                tmpField.AfterCmds:=tmpCommands;
              end
            ELSE
              BEGIN
                ReportError(translate(22786,'ENTRY expected'));
                FTempResult:=False;
              END;
          END
        ELSE IF CurCommand<>'' THEN
          begin
            tmpCommands:=TChkCommands(tmpField.AfterCmds);
            GetCommand(tmpCommands);
            tmpField.AfterCmds:=tmpCommands;
          end;
{            BEGIN
            IF CurCommand[1]='*' THEN AddFieldComment
            ELSE IF CurCommand<>'END' THEN AddFieldFlawComment;
          END;}
      UNTIL (FParser.EndOfLines) OR (CurCommand='END');
      IF FTempResult THEN
        BEGIN
          WITH df[df.FocusedField] DO
            BEGIN
              IF AfterCmds<>NIL THEN TChkCommands(AfterCmds).Free;
              IF BeforeCmds<>NIL THEN TChkCommands(BeforeCmds).Free;
              Min:=tmpField.Min;
              Max:=tmpField.Max;
              Legal:=tmpField.Legal;
              ValueLabel:=tmpField.ValueLabel;
              ValueLabelType:=tmpField.ValueLabelType;
              ValueLabelUse:=tmpField.ValueLabelUse;
              MustEnter:=tmpField.MustEnter;
              doRepeat:=tmpField.doRepeat;
              DefaultValue:=tmpField.DefaultValue;
              Jumps:=tmpField.Jumps;
              JumpResetChar:=tmpField.JumpResetChar;
              RangeDefined:=tmpField.RangeDefined;
              FieldComments:=tmpField.FieldComments;
              NoEnter:=tmpField.NoEnter;
              Index:=tmpField.Index;
              IsTypeStatusBar:=tmpField.IsTypeStatusBar;
              TypeComments:=tmpField.TypeComments;
              TypeString:=tmpField.TypeString;
              TypeCommentField:=tmpField.TypeCommentField;
              TypeCommentFieldStr:=tmpField.TypeCommentFieldStr;
              TypeColor:=tmpField.TypeColor;
              Confirm:=tmpField.Confirm;
              TopOfScreen:=tmpField.TopOfScreen;
              TopOfScreenLines:=tmpField.TopOfScreenLines;
              ShowLegalPickList:=tmpField.ShowLegalPickList;
              PickListNoSelect:=tmpField.PickListNoSelect;
              AfterCmds:=tmpField.AfterCmds;
              BeforeCmds:=tmpField.BeforeCmds;
              IF NOT HasGlobalMissing THEN
                BEGIN
                  MissingValues[0]:=tmpField.MissingValues[0];
                  MissingValues[1]:=tmpField.MissingValues[1];
                  MissingValues[2]:=tmpField.MissingValues[2];
                END;
              Autosearch:=tmpField.Autosearch;
              AutoFields:=tmpField.AutoFields;
              AutoList:=tmpField.AutoList;
            END;  //with
        END  //if TempResult
      ELSE
        BEGIN
          IF tmpField.AfterCmds<>NIL THEN TChkCommands(tmpField.AfterCmds).Free;
          IF tmpField.BeforeCmds<>NIL THEN TChkCommands(tmpField.BeforeCmds).Free;
        END;  //if NOT TempResult
    END;
  df.FocusedField:=-1;
  CurCommand:='';
END;   //procedure TCheckObj.RetrieveFieldBlock

Procedure TCheckObj.RetrieveLabelBlock;
{Reads the LABELBLOCK..END block in the checkfile}
BEGIN
  REPEAT
    CurCommand:=FParser.GetUpperToken(nwAny);
    IF CurCommand='LABEL' THEN RetrieveLabel;
  UNTIL (CurCommand='END') OR (FParser.EndOfLines);
  CurCommand:='';
END;  //RetrieveLabelBlock


Procedure TCheckObj.RetrieveLabel;
  //Reads a LABEL..END block
VAR
  aValueLabelSet: TValueLabelSet;
  tmpLabelName:String[80];
  ok,StopRead,FirstLabel:Boolean;
  s,tmpValue,tmpLabel: String;
BEGIN
  aValueLabelSet:=TValueLabelSet.create;
  ok:=True;
  CurCommand:=AnsiLowerCase(FParser.GetToken(nwSameLine)); 
  IF trim(CurCommand)<>'' THEN
    BEGIN
      IF (df.ValueLabels.ValueLabelSetByName(CurCommand)=NIL)
      AND (df.ValueLabels.ValueLabelSetByName(CurCommand+'¤')=NIL) THEN
        BEGIN
          StopRead:=False;
          tmpLabelName:=trim(CurCommand);
          IF ReadingLabelLibrary THEN
            BEGIN
              IF Length(tmpLabelName)=40 THEN tmpLabelName[40]:='¤'
              ELSE tmpLabelName:=tmpLabelName+'¤';
            END;
          aValueLabelSet.Name:=tmpLabelName;
          REPEAT
            //Read value
            CurCommand:=FParser.GetToken(nwAny); 
            IF Trim(CurCommand)='' THEN
              BEGIN
                StopRead:=True;
                ok:=False;
              END;
            IF AnsiUpperCase(CurCommand)='END' THEN StopRead:=True
            ELSE IF trim(CurCommand)<>'' THEN
              BEGIN
                s:=trim(CurCommand);
                IF s[1]='*' THEN    
                  BEGIN
                    s:=trim(FParser.GetWholeLine);
                    IF NOT FCheckFileMode THEN Continue;
                    IF Length(s)>(30+80) THEN
                      BEGIN
                        ReportError(translate(22874,'Commented line is too long'));
                        StopRead:=True;
                        ok:=False;
                      END
                  END;
                IF s[1]='*' THEN
                  BEGIN
                    tmpValue:=Copy(s,1,30);
                    tmpLabel:='';
                    IF Length(s)>30 THEN tmpLabel:=Copy(s,31,Length(s));
                    aValueLabelSet.AddValueLabelPair(tmpValue,tmpLabel);
                  END  //if reading a commented-out label
                ELSE
                  BEGIN
                    IF Length(CurCommand)>30 THEN CurCommand:=Copy(CurCommand,1,30);
                    tmpValue:=CurCommand;
                    //Read text
                    CurCommand:=FParser.GetToken(nwSameLine);   
                    IF trim(CurCommand)='' THEN
                      BEGIN
                        StopRead:=True;
                        ok:=False;
                      END
                    ELSE
                      BEGIN
                        IF Length(CurCommand)>80 THEN CurCommand:=Copy(CurCommand,1,80);
                        WHILE pos('"',CurCommand)>0 DO Delete(CurCommand,Pos('"',CurCommand),1);
                        tmpLabel:=CurCommand;
                      END;
                    aValueLabelSet.AddValueLabelPair(tmpValue,tmpLabel);
                  END  //if reading a proper label
              END  //if line is not empty
            ELSE stopRead:=True;
          UNTIL StopRead;
        END  //if label name didn't exist
      ELSE ok:=False;
    END  //if label name was found
  ELSE ok:=False;
  IF ok THEN df.ValueLabels.AddValueLabelSet(aValueLabelSet)
  ELSE aValueLabelSet.Free;
  CurCommand:='';
END;  //TCheckObj.RetrieveLabel


Procedure TCheckObj.RetrieveAssertBlock;
{Reads the CONSISTENCYBLOCK..END block - and ignores it...}
BEGIN
  IF NOT Assigned(df.AssertList) THEN df.AssertList:=TStringList.Create;
  REPEAT
    CurCommand:=FParser.GetUpperToken(nwAny); 
    IF (CurCommand='CHECK') OR (CurCommand='REPORT') THEN
      BEGIN
        df.AssertList.Append(FParser.GetWholeLine);
      END;
  UNTIL (CurCommand='END') OR (FParser.EndOfLines);
  CurCommand:='';
END;  //TCheckObj.RetrieveAssertBlock


Procedure TCheckObj.GetCommandList(var CmdList:TChkCommands);
BEGIN
  REPEAT
    CurCommand:=FParser.GetToken(nwAny);
    IF AnsiUpperCase(CurCommand)<>'END' THEN GetCommand(CmdList);
  UNTIL (AnsiUpperCase(CurCommand)='END') OR (FParser.EndOfLines);
  CurCommand:='';
END;  //GetCommandList



Procedure TCheckObj.GetCommand(var CmdList:TChkCommands);
VAR
  cmd:Commands;
  tmpCmdRec:TCmds;
  SeenElse,ok,found,IsEpiInfo,ImplicitLet,glob_dub,IsDefField:Boolean;
  tmpCmdPtr:PCmds;
  n,n2,n3,fieldFrom,fieldTo:Integer;
  tmpStr:String[20];
  s1,s2:String[200];
  tmpS,tmpS2:String;   
  tmpDefVar: TeField;
  AInfo,BInfo: PRelateInfo;
  bb,bb2,bb3:byte;
  tmpTxtColor,tmpBgColor: TColor;
  tmpStr10: string;
  tmpList1,tmpList2: TStringList;
  NumValues: Integer;
  //MisValues: Array[0..2] of String[10];
  mv1, mv2, mv3: string;
  AField: TeField;
  TopDf: TEpiDataFile;
  ValueLabelType: TValueLabelSetType;
  ValueLabelUse:  String;
  ValueLabelShow: Boolean;
BEGIN
  {Legal commands are
    IF <boolean expr.> THEN  <cmds> [ELSE <cmds>] ENDIF
    HELP "..." [Type=Information|Warning|Confirmation|Error]
    HIDE [fieldname]
    UNHIDE [fieldname]
    CLEAR [fieldname]
    GOTO [fieldname]
    COMMENT LEGAL
    EXIT
    DEFINE
    AUTOSAVE
    CONFIRM [fieldname]
    IGNOREMISSING
    TYPE "kjkj"
    RELATE fieldname filename [1]
    BACKUP dest_library
    BACKGROUNDCOLOUR
    BEEP [WARNING|CONFIRMATION]
    QUIT
    COPYTOCLIPBOARD
    SHOWLASTRECORD
    [LET] Fieldname=expression
    * (Comments)
  }
  IF (AnsiUpperCase(CurCommand)='END')
  or (AnsiUpperCase(CurCommand)='ENDIF')
  or (AnsiUpperCase(CurCommand)='ELSE') or (CurCommand='') THEN Exit;
  ok:=True;
  tmpCmdRec.IfCmds:=NIL;
  tmpCmdRec.ElseCmds:=NIL;
  tmpCmdRec.ValueLabel:=NIL;
  
  IF CurCommand[1]='*' THEN
    BEGIN
      cmd:=cmdComment;
      tmpCmdRec.Command:=cmd;
    END
  ELSE
    BEGIN
      cmd:=cmdIF;
      WHILE (cmd<cmdLET) AND (CommandNames[Cmd]<>AnsiUpperCase(CurCommand)) DO cmd:=Succ(Cmd);
      IF (CommandNames[Cmd]<>AnsiUpperCase(CurCommand))
      or (AnsiUpperCase(CurCommand)='LET') THEN
        BEGIN
          {check if unknown CurCommand is implicit LET}
          s1:=trim(CurCommand+' '+FParser.GetLineAndFlush);
          IF AnsiUpperCase(Copy(s1,1,3))='LET' THEN
            BEGIN
              Delete(s1,1,3);
              ImplicitLET:=FALSE;
            END
          ELSE ImplicitLET:=True;
          {Rules for valid LET:
           1. A '=' is present
           2. A fieldname or var-name is present before '='
           3. A valid expression is found after '='}
          n:=1;
          s2:=s1;
          n:=pos('=',s2);
          IF n=0 THEN ok:=False
          ELSE IF n=1 THEN
            BEGIN
              ReportError(translate(22756,'Missing field- or variablename to the left of the equal-sign'));  
              ok:=False
            END
          ELSE
            BEGIN
              IsDefField:=False;
              tmpStr:=trim(Copy(s2,1,n-1));
              {Check if tmpStr contains a fieldname or variablename}
              n:=df.FieldNumbers[tmpStr];
              tmpDefVar:=NIL;
              IF n=-1 THEN
                BEGIN
                  n:=df.DefFieldNumbers[tmpStr];
                  IF n<>-1 THEN IsDefField:=True;
                END;
              IF n=-1 THEN
                BEGIN
                  {$IFNDEF analysis}
                  ReportError(Translate(22758,'Unknown field- or variablename to the left of the equal-sign'));
                  ok:=False;
                  {$ENDIF}
                END
            END; //if '=' found in a legal position
          IF ok THEN
            BEGIN
              WITH tmpCmdRec DO
                BEGIN
                  Command:=cmdLET;
                  CodedWithLET:=NOT ImplicitLET;
                  VarIsField:=IsDefField;
                  VarNumber:=n;
                  VarName:=tmpStr;
                  tmpS:=trim(Copy(s2,Pos('=',s2)+1,Length(s2)));
                  Double2SingleQuotes(tmpS);
//                    IF tmpS='.' THEN tmpS:='_M';
                  LetExpr:=tmpS;
                END;  //with
              cmd:=cmdLET;
              ImplicitLet:=True;
            END
          ELSE
            BEGIN  // Unknown command was not a LET
              AddFieldFlawComment;
              Exit;
            END;
        END
      ELSE tmpCmdRec.Command:=cmd;
    END;  //else

  ok:=true;
  CASE cmd OF
    cmdIF:
      BEGIN
        tmpCmdRec.IfExpr:='';
        tmpCmdRec.IfCmds:=NIL;
        tmpCmdRec.ElseCmds:=NIL;
        REPEAT
          CurCommand:=FParser.GetToken(nwSameKeepQuotes); 
          tmpCmdRec.IfExpr:=tmpCmdRec.IfExpr+' '+CurCommand;
        UNTIL (AnsiUpperCase(CurCommand)='THEN') or (CurCommand='');
        IF AnsiUpperCase(CurCommand)='THEN' THEN
          BEGIN
           tmpCmdRec.IfExpr:=tmpCmdRec.IfExpr+' ';
           Delete(tmpCmdRec.IfExpr,
           Pos(' THEN ',AnsiUpperCase(tmpCmdRec.IfExpr)),6);
          END
        ELSE
          BEGIN  //no THEN was found in same line as expression
            CurCommand:=FParser.GetToken(nwAny); 
            IF AnsiUpperCase(CurCommand)<>'THEN' THEN
              BEGIN
                ReportError(translate(22760,'No THEN found after IF'));  
                ok:=False;
              END;
          END;
        tmpS:=trim(tmpCmdRec.IfExpr);
        tmpCmdRec.IfShowExpr:=tmpS;
        HandleBooleanConditions(tmpS);
        Double2SingleQuotes(tmpS);
        //Assign If-expression
        tmpCmdRec.IfExpr:='('+trim(tmpS)+')';
        IF ok THEN
          BEGIN
            SeenElse:=False;
            REPEAT
              CurCommand:=FParser.GetToken(nwAny);  
              IF AnsiUpperCase(CurCommand)='ELSE' THEN
                BEGIN
                  SeenElse:=True;
                  CurCommand:='ELSE'
                END;
              IF SeenElse THEN GetCommand(TChkCommands(tmpCmdRec.ElseCmds))
              ELSE GetCommand(TChkCommands(tmpCmdRec.IfCmds));
            UNTIL (AnsiUpperCase(CurCommand)='ENDIF') OR (FParser.EndOfLines)
            OR (AnsiUpperCase(CurCommand)='END');
            IF (FParser.EndOfLines) AND (AnsiUpperCase(CurCommand)<>'ENDIF') THEN
              BEGIN
                ReportError(translate(22762,'IF..THEN command without ENDIF')); 
                ok:=False;
              END;
            IF AnsiUpperCase(CurCommand)='END' THEN
              BEGIN
                ReportError(translate(22764,'ENDIF expected but END found')); 
                ok:=False;
              END;
            CurCommand:='';
          END;
      END;
    cmdHelp:
      BEGIN
        CurCommand:=FParser.GetToken(nwSameLine); 
        REPEAT
          n:=pos('\n',CurCommand);
          IF n=0 THEN n:=pos('\N',CurCommand);
          IF n>0 THEN
            BEGIN
              CurCommand[n]:=' ';
              CurCommand[n+1]:=#13;
            END;
        UNTIL n=0;
        tmpCmdRec.HelpString:=CurCommand;
        tmpCmdRec.HelpType:=mtInformation;
        tmpCmdRec.HelpKeys:='';
        CurCommand:=FParser.GetUpperToken(nwSameLine); 
        IF CurCommand<>'' THEN
          BEGIN
            IF (Copy(CurCommand,1,6)='KEYS="')
            AND (Length(CurCommand)>7)
            AND (CurCommand[Length(CurCommand)]='"') THEN
              BEGIN
                tmpS:=Copy(CurCommand,7,Length(CurCommand)-7);
                tmpCmdRec.HelpKeys:=Copy(tmpS,1,10);
              END;
          END;
        IF AnsiUpperCase(Copy(CurCommand,1,4))<>'TYPE'
        THEN CurCommand:=FParser.GetUpperToken(nwSameLine);  
        tmpS:='';
        WHILE CurCommand<>'' DO
          BEGIN
            tmpS:=tmpS+CurCommand;
            CurCommand:=FParser.GetUpperToken(nwSameLine);  
          END;
        IF (tmpS='TYPE=ERROR') OR (tmpS='TYPE=E') THEN tmpCmdRec.HelpType:=mtError
        ELSE IF (tmpS='TYPE=WARNING') OR (tmpS='TYPE=W') THEN tmpCmdRec.HelpType:=mtWarning
        ELSE IF (tmpS='TYPE=CONFIRMATION') OR (tmpS='TYPE=C') THEN tmpCmdRec.HelpType:=mtConfirmation;
        CurCommand:='';
      END;  //case cmdHelp
    cmdHide,cmdUnhide,cmdClear,cmdGoto:
      BEGIN
        {Check if a fieldname exists after command}
        ok:=True;
        CurCommand:=FParser.GetUpperToken(nwSameLine); 
        IF CurCommand<>'' THEN
          BEGIN
            n:=-1;
            Found:=False;
            IF ( (CurCommand='WRITE') OR (CurCommand='WRITEREC') ) AND (cmd=cmdGoto) THEN
              BEGIN
                tmpCmdRec.HideVarName:='WRITE';
                tmpCmdRec.HideVarNumber:=-1;
              END
            ELSE
              BEGIN
                n:=df.FieldNumbers[CurCommand];
                Found:=(n<>-1);
                //WHILE (n<df^.FieldList.Count-1) AND (NOT Found) DO
                //  BEGIN
                //    INC(n);
                //    IF CurCommand=AnsiUpperCase(trim(PeField(df^.FieldList.Items[n])^.FName))
                //    THEN Found:=True;
                //  END;
                IF (NOT Found) AND (CurCommand='COMMENT') THEN
                  BEGIN
                    CurCommand:=FParser.GetUpperToken(nwSameLine);
                    IF CurCommand='LEGAL' THEN
                      BEGIN
                        Found:=True;
                        tmpCmdRec.HideVarName:='$$COMLEG';
                        tmpCmdRec.HideVarNumber:=df.FocusedField;
                      END;
                  END;
                IF NOT Found THEN
                  BEGIN
                    ok:=False;
                    ReportError(Translate(22708,'Unknow fieldname.'));
                  END
                ELSE IF tmpCmdRec.HideVarName<>'$$COMLEG' THEN
                  BEGIN
                    tmpCmdRec.HideVarName:=df[n].FieldName;
                    tmpCmdRec.HideVarNumber:=n;
                  END;  //else
              END;  //if not GOTO WRITE | GOTO END
          END  //if a word was found after Hide/Unhide
        ELSE
          BEGIN
            tmpCmdRec.HideVarNumber:=df.FocusedField;
            tmpCmdRec.HideVarName:=df[df.FocusedField].FieldName;
          END;
      END;  //case cmdHide or cmdUnhide
    cmdComLegal:
      BEGIN
        tmpCmdRec.ValueLabel:=NIL;
        //RetrieveCommentLegal(tmpCmdRec.ValueLabel,tmpCmdRec.CommentLegalRec,tmpCmdRec.ShowList,True);
        tmpCmdRec.Valuelabel:=RetrieveCommentLegal(true,ValueLabelType,ValueLabelShow,ValueLabelUse);
        if tmpCmdRec.Valuelabel<>NIL then
          begin
            tmpCmdRec.ShowList:=ValuelabelShow;
            tmpCmdRec.ValueLabelType:=ValueLabelType;
            tmpCmdRec.ValueLabelUse:=ValueLabelUse;
            tmpCmdRec.clVarNumber:=df.FocusedField;
          end
        else ok:=false;
      END;  //case cmdComLegal
    cmdComment:
      BEGIN
        IF Length(CurCommand)>200 THEN CurCommand:=Copy(CurCommand,1,200);
        tmpCmdRec.Comment:=CurCommand;
      END;
    cmdDefine:
      BEGIN
        //get variable name
        CurCommand:=FParser.GetToken(nwSameLine);  //  NextWord(nwSameLine);
        IF CurCommand='' THEN
          BEGIN
            ok:=False;
            ReportError(Translate(22766,'DEFINE without variablename'));
          END
        ELSE IF Length(CurCommand)>16 THEN
          BEGIN
            ok:=False;
            ReportError(Translate(22768,'Variablename can be only 16 characters in DEFINE'));
          END
        ELSE IF df.FieldNumbers[CurCommand]<>-1 THEN
          BEGIN
            ok:=False;
            ReportError(Translate(22770,'Dublicate name: The variablename is used by a entryfield'));
          END;
        {ELSE IF (GetDefField(CurCommand,df)<>NIL) AND (MultiLineError) THEN
          BEGIN
            ok:=False;
            ReportError(Lang(22772));  //'Dublicate name: The variablename is allready used'
          END;}
        IF ok THEN
          BEGIN
            tmpCmdRec.FName:=CurCommand;
            tmpCmdRec.FNumDecimals:=0;
            //Variable name passed all tests - now get the Fieldtype
            CurCommand:=FParser.GetUpperToken(nwSameLine);  // AnsiUpperCase(NextWord(nwSameLine));
            IF CurCommand='' THEN
              BEGIN
                ok:=False;
                ReportError(Translate(22774,'Fieldtype missing in DEFINE command'));
              END
            ELSE
              BEGIN
                tmpCmdRec.FLength:=Length(CurCommand);
                IF CurCommand[1]='#' THEN
                  BEGIN
                    n2:=0;
                    FOR n:=1 TO Length(CurCommand) DO
                      BEGIN
                        IF (CurCommand[n]<>'#') AND (CurCommand[n]<>'.') THEN ok:=False;
                        IF CurCommand[n]='.' THEN INC(n2);
                      END;
                    IF (NOT ok) OR (n2>1) THEN
                      BEGIN
                        ok:=False;
                        ReportError(Translate(22776,'Error in Fieldtype. Use # and maximum one . to define numeric'));
                      END
                    ELSE
                      BEGIN
                        IF (n2>0) OR (Length(CurCommand)>4) THEN tmpCmdRec.Felttype:=ftFloat
                        ELSE tmpCmdRec.Felttype:=ftInteger;
                        IF n2>0 THEN tmpCmdRec.FNumDecimals:=Length(CurCommand)-Pos('.',CurCommand)
                        ELSE tmpCmdRec.FNumDecimals:=0;
                      END;
                  END  //if numeric
                ELSE IF CurCommand[1]='_' THEN tmpCmdRec.Felttype:=ftAlfa
                ELSE IF CurCommand='<MM/DD/YYYY>' THEN tmpCmdRec.Felttype:=ftDate
                ELSE IF Copy(CurCommand,1,2)='<A' THEN
                  BEGIN
                    tmpCmdRec.Felttype:=ftUpperAlfa;
                    tmpCmdRec.FLength:=Length(CurCommand)-2;
                  END
                ELSE IF Copy(Curcommand,1,2)='<S' THEN
                  BEGIN
                    tmpCmdRec.Felttype:=ftSoundex;
                    tmpCmdRec.FLength:=Length(CurCommand)-2;
                  END
                ELSE IF CurCommand='<Y>' THEN tmpCmdRec.Felttype:=ftBoolean
                ELSE IF CurCommand='<DD/MM/YYYY>' THEN tmpCmdRec.Felttype:=ftEuroDate
                ELSE IF CurCommand='<YYYY/MM/DD>' THEN tmpCmdRec.Felttype:=ftYMDDate    //&&
                ELSE
                  BEGIN
                    //No legal Fieldtype found
                    ok:=False;
                    ReportError(Translate(22778,'Illegal Fieldtype in DEFINE command'));
                  END;
                IF ok THEN
                  BEGIN
                    CurCommand:=FParser.GetUpperToken(nwSameLine);  // AnsiUpperCase(NextWord(nwSameLine));
                    IF CurCommand='' THEN tmpCmdRec.FScope:=scLocal
                    ELSE IF CurCommand[1]='G' THEN tmpCmdRec.FScope:=scGlobal
                    ELSE IF CurCommand[1]='C' THEN tmpCmdRec.FScope:=scCumulative
                    ELSE
                      BEGIN
                        ok:=False;
                        ReportError(Translate(22780,'Illegal scope in DEFINE command. Use GLOBAL or CUMULATIVE'));
                      END;
                    IF ok THEN
                      BEGIN
                        //All data concerning the DEFINE is read
                        //Now check is DEF-name is allready used
                        //Ignore the DEF if DEF is global and a global def-field with the
                        //same Fieldtype exists
                        glob_dub:=False;
                        tmpDefVar:=df.DefFieldsByName[tmpCmdRec.FName];
                        IF tmpDefVar<>NIL THEN
                          BEGIN
                            //a DEF-var with same name exists
                            IF (tmpCmdRec.FScope<>scGlobal) OR (tmpDefVar.Scope<>scGlobal) AND (FMultiLineError) THEN
                              BEGIN
                                ok:=False;
                                ReportError(Translate(22772,'Dublicate name: The variablename is allready used'));
                              END;
                            IF (tmpCmdRec.FScope=scGlobal) AND (tmpDefVar.Scope=scGlobal) THEN
                              BEGIN
                                IF NOT ( (tmpCmdRec.Felttype=tmpDefVar.Fieldtype)       AND
                                     (tmpCmdRec.FLength=tmpDefVar.Length)             AND
                                     (tmpCmdRec.FNumDecimals=tmpDefVar.NumDecimals) ) THEN
                                  BEGIN
                                    ok:=False;
                                    ReportError(Translate(22773,'A global DEFINE with same fieldname but different Fieldtype or length is allready defined'));
                                  END;
                              END;
                          END;
                        IF (df.DefList=NIL) AND (tmpCmdRec.FScope<>scGlobal) THEN df.DefList:=TStringList.Create;
//                          tmpDefVar:=GetDefField(tmpCmdRec.FName,df);
//                          IF (NOT MultiLineError) AND (tmpDefVar<>NIL)
//                          THEN tmpDefVar:=PDefVar(df^.DefList.Objects[n])
                        n:=-1;
                        IF tmpDefVar=NIL THEN tmpDefVar:=TeField.Create ELSE n:=0;
                        tmpDefVar.FieldName:=        tmpCmdRec.FName;
                        tmpDefVar.Fieldtype:=     tmpCmdRec.Felttype;
                        tmpDefVar.Length:=      tmpCmdRec.FLength;
                        tmpDefVar.NumDecimals:= tmpCmdRec.FNumDecimals;
                        tmpDefVar.Scope:=       tmpCmdRec.FScope;
                        tmpDefVar.AsString:='';
                        IF n=-1 THEN
                          BEGIN
                            IF tmpCmdRec.FScope<>scGlobal
                            THEN df.DefList.AddObject(tmpCmdRec.FName,TObject(tmpDefVar))
                            ELSE TEpiDataFile(df.TopEpiDataFile).GlobalDefList.AddObject(tmpCmdRec.FName,TObject(tmpDefVar));
                          END;
                      END;
                  END;  //if ok - look for scope
              END;  //if Fieldtype was present
          END;  //if (variablename is) ok
      END;  //case cmdDefine.
    cmdAutosave: df.AutoSave:=True;
    cmdConfirm:  df.Confirm:=True;
    cmdDefaultAll:
      BEGIN
        //Syntax DEFAULTVALUE ALL|ALLSTRINGS|ALLSTRING|ALLNUMERIC x    eller
        //       DEFAULTVALUE field-field, field, field  X
        CurCommand:=FParser.GetUpperToken(nwSameLine);
        IF (CurCommand='ALL') OR (CurCommand='ALLSTRINGS') OR (CurCommand='ALLSTRING') or (CurCommand='ALLNUMERIC') THEN
          BEGIN
            tmpS:=CurCommand;
            CurCommand:=FParser.GetToken(nwSameLine);
            IF CurCommand='' THEN
              BEGIN
                ok:=false;
                ReportError('The default value must follow DEFAULTVALUE ALL');
              END
            ELSE
              BEGIN
                df.GlobalDefaultValue:=CurCommand;
                for n:=0 TO df.NumFields-1 DO
                  BEGIN
                    IF (df[n].Fieldtype in [ftInteger,ftAlfa,ftUpperAlfa,ftFloat,ftCrypt]) THEN
                      BEGIN
                        IF (tmpS='ALL') THEN df[n].HasGlobalDefaultValue:=true
                        ELSE IF (df[n].Fieldtype in [ftAlfa,ftUpperAlfa,ftCrypt]) AND ((tmpS='ALLSTRINGS') OR (tmpS='ALLSTRING')) THEN df[n].HasGlobalDefaultValue:=true
                        ELSE IF (df[n].Fieldtype in [ftInteger,ftFloat]) AND (tmpS='ALLNUMERIC') THEN df[n].HasGlobalDefaultValue:=true;
                      END;   //if relevant Fieldtype
                  END;  //for
              END
          END
        ELSE
          BEGIN
            //Syntax DEFAULTVALUE field-field, field, field X Y Z is used
            s1:=trim(FParser.GetWholeLine);
            s1:=copy(s1,14,length(s1));    //remove the word DEFAULTVALUE
            TRY
              tmpList1:=TStringList.Create;
              tmpList1.CommaText:=s1;
              IF (tmpList1.Count<2) THEN
                begin
                  ok:=false;
                  ReportError('DEFAULTVALUE must be followed by ALL or at least one fieldname and a default value');
                end
              else
                begin
                  tmpS:=tmpList1[tmpList1.count-1];
                  df.GlobalDefaultValue:=tmpS;
                end;
              IF ok THEN
                BEGIN
                  FOR n:=0 TO tmpList1.Count-2 DO
                    BEGIN
                      //Traverse the list of fields
                      IF pos('-',tmpList1[n])>0 THEN   //is element a field-interval?
                        BEGIN
                          s1:=copy(tmpList1[n],1,pos('-',tmpList1[n])-1);  //get interval start
                          s2:=copy(tmpList1[n],pos('-',tmpList1[n])+1,length(tmpList1[n]));  //get interval end
                          fieldFrom:=df.FieldNumbers[s1];
                          fieldTo:=df.FieldNumbers[s2];
                          IF (fieldFrom=-1) OR (fieldTo=-1) THEN
                            BEGIN
                              IF fieldFrom=-1 THEN ReportError(translate(22708,'Unknown field name')+' '+s1);  //22708=Unknown field name
                              IF fieldTo=-1 THEN ReportError(translate(22708,'Unknown field name')+' '+s2);
                              ok:=False;
                              break;
                            END
                          ELSE
                            BEGIN
                              FOR n2:=fieldFrom TO fieldTo DO
                                BEGIN
                                  AField:=df[n2];
                                  IF (AField.Fieldtype<>ftQuestion) THEN
                                    BEGIN
                                      AField.DefaultValue:=tmpS;
                                      AField.HasGlobalDefaultValue:=true;
                                    END;  //if not question field
                                END;  //for
                            END;  //if all fields are known
                        END  //if interval
                      ELSE
                        BEGIN    //element is a single fieldname
                          fieldFrom:=df.FieldNumbers[tmpList1[n]];
                          IF fieldFrom=-1 THEN
                            BEGIN
                              ReportError(translate(22708,'Unknown field name')+' '+tmpList1[n]);   //22708=Unknown field name
                              ok:=False;
                            END
                          ELSE
                            BEGIN
                              AField:=df[fieldFrom];
                              IF (AField.Fieldtype<>ftQuestion) THEN
                                BEGIN
                                  Afield.HasGlobalDefaultValue:=True;
                                  AField.DefaultValue:=tmpS;
                                END;  //if not question field
                            END;  //if field is found
                        END;  //if single fieldname
                    END;  //for
                END;  //if ok
            FINALLY
              tmpList1.Free;
            END;  //try..finally
          END;
      END;
    cmdMissingAll:
      BEGIN
        //Syntax MISSINGVALUE ALL x [x [x]]
        CurCommand:=FParser.GetUpperToken(nwSameLine);
        IF CurCommand='ALL' THEN
          BEGIN
            CurCommand:=FParser.GetToken(nwSameLine);
            IF CurCommand<>'' THEN
              BEGIN
                IF (NOT IsInteger(CurCommand)) THEN ok:=False
                ELSE df.GlobalMissingValues[0]:=CurCommand;
              END;
            CurCommand:=FParser.GetToken(nwSameLine);
            IF CurCommand<>'' THEN
              BEGIN
                IF (NOT IsInteger(CurCommand)) THEN ok:=False
                ELSE df.GlobalMissingValues[1]:=CurCommand;
              END;
            CurCommand:=FParser.GetToken(nwSameLine);
            IF CurCommand<>'' THEN
              BEGIN
                IF (NOT IsInteger(CurCommand)) THEN ok:=False
                ELSE df.GlobalMissingValues[2]:=CurCommand;
              END;
            IF (NOT ok) THEN ReportError(translate(22876,'Only numbers can be used as MISSINGVALUES ALL'));   //22876=Only numbers can be used as MISSINGVALUES ALL
          END
        ELSE
          BEGIN
            //Syntax MISSINGVALUE field-field, field, field X Y Z is used
            s1:=trim(FParser.GetWholeLine);
            s1:=copy(s1,14,length(s1));    //remove the word MISSINGVALUE
            TRY
              tmpList1:=TStringList.Create;
              tmpList1.CommaText:=s1;
              NumValues:=0;
              n:=tmpList1.Count-1;
              Found:=False;
              REPEAT
                IF IsInteger(tmpList1[n]) THEN INC(NumValues) ELSE Found:=True;
                DEC(n);
              UNTIL (Found=True) OR (n<0);
              IF (NumValues>3) OR (NumValues=0) THEN
                BEGIN
                  ReportError(translate(22878,'One to three MISSINGVALUEs can be defined'));   //22878=One to three MISSINGVALUEs can be defined
                  ok:=False;
                END;
              IF NumValues=tmpList1.Count THEN
                BEGIN
                  ReportError(translate(22880,'ALL or fieldnames must follow MISSINGVALUE'));     //'ALL or fieldnames must follow MISSINGVALUE'
                  ok:=False;
                END;
              IF ok THEN
                BEGIN
                  n2:=0;
                  CASE NumValues OF
                    1: mv1:=tmpList1[tmpList1.count-1];
                    2: BEGIN
                         mv1:=tmpList1[tmpList1.count-2];
                         mv2:=tmpList1[tmpList1.count-1];
                       END;
                    3: BEGIN
                         mv1:=tmpList1[tmpList1.Count-3];
                         mv2:=tmpList1[tmpList1.Count-2];
                         mv3:=tmpList1[tmpList1.Count-1];
                       END;
                  END;  //case
                  //FOR n:=tmpList1.Count-NumValues TO tmpList1.Count-1 DO
                  //  BEGIN
                  //    MisValues[n2]:=tmpList1[n];
                  //    INC(n2);
                  //  END;  //for
                  //Get the fieldnames / fieldintervals
                  FOR n:=0 TO tmpList1.Count-NumValues-1 DO
                    BEGIN
                      //Traverse the list of fields
                      IF pos('-',tmpList1[n])>0 THEN   //is element a field-interval?
                        BEGIN
                          s1:=copy(tmpList1[n],1,pos('-',tmpList1[n])-1);  //get interval start
                          s2:=copy(tmpList1[n],pos('-',tmpList1[n])+1,length(tmpList1[n]));  //get interval end
                          fieldFrom:=df.FieldNumbers[s1];
                          fieldTo:=df.FieldNumbers[s2];
                          IF (fieldFrom=-1) OR (fieldTo=-1) THEN
                            BEGIN
                              IF fieldFrom=-1 THEN ReportError(translate(22708,'Unknown field name')+' '+s1);  //22708=Unknown field name
                              IF fieldTo=-1 THEN ReportError(translate(22708,'Unknown field name')+' '+s2);
                              ok:=False;
                              break;
                            END
                          ELSE
                            BEGIN
                              FOR n2:=fieldFrom TO fieldTo DO
                                BEGIN
                                  AField:=df[n2];
                                  IF (AField.Fieldtype in [ftInteger,ftFloat]) THEN
                                    BEGIN
                                      Afield.HasGlobalMissing:=True;
                                      CASE NumValues OF
                                        1: AField.MissingValues[0]:=mv1;
                                        2: BEGIN
                                             AField.MissingValues[1]:=mv2;
                                             AField.MissingValues[0]:=mv1;
                                           END;
                                        3: BEGIN
                                             AField.MissingValues[2]:=mv3;
                                             AField.MissingValues[1]:=mv2;
                                             AField.MissingValues[0]:=mv1;
                                           END;
                                      END;  //case
                                      //FOR n3:=0 TO NumValues DO
                                      //  AField^.FMissingValues[n3]:=MisValues[n3];
                                    END;  //if numeric field
                                END;  //for
                            END;  //if all fields are known
                        END  //if interval
                      ELSE
                        BEGIN    //element is a single fieldname
                          fieldFrom:=df.FieldNumbers[tmpList1[n]];   //   GetFieldNumber(tmpList1[n],df);
                          IF fieldFrom=-1 THEN
                            BEGIN
                              ReportError(translate(22708,'Unknown field name')+' '+tmpList1[n]);   //22708=Unknown field name
                              ok:=False;
                            END
                          ELSE
                            BEGIN
                              AField:=df[fieldFrom];
                              IF (AField.Fieldtype in [ftInteger,ftFloat]) THEN
                                BEGIN
                                  //FOR n3:=0 TO NumValues DO
                                  //  AField^.FMissingValues[n3]:=MisValues[n3];
                                  Afield.HasGlobalMissing:=True;
                                  CASE NumValues OF
                                    1: AField.MissingValues[0]:=mv1;
                                    2: BEGIN
                                         AField.MissingValues[1]:=mv2;
                                         AField.MissingValues[0]:=mv1;
                                       END;
                                    3: BEGIN
                                         AField.MissingValues[2]:=mv3;
                                         AField.MissingValues[1]:=mv2;
                                         AField.MissingValues[0]:=mv1;
                                       END;
                                  END;  //case

                                END;  //if numeric field
                            END;  //if field is found
                        END;  //if single fieldname
                    END;  //for
                END;  //if ok
            FINALLY
              tmpList1.Free;
            END;  //try..finally
          END;
      END;  //case cmdMissingAll
    cmdIgnoreMissing: df.MissingAction:=maIgnoreMissing;
    cmdTypeString:
      BEGIN
        {  Syntax: TYPE "text" [colour]  }
        CurCommand:=FParser.GetToken(nwSameLine);  // NextWord(nwSameLine);
        IF AnsiUpperCase(CurCommand)='COMMENT' THEN
          BEGIN
            CurCommand:=FParser.GetUpperToken(nwSameLine);
            IF CurCommand='ALLFIELDS' THEN
              BEGIN
                df.GlobalTypeCom:=True;
                tmpCmdRec.TypeText:='¤¤typecommentlegalallfields¤¤';
                tmpCmdRec.tsVarNumber:=-1;
                CurCommand:=FParser.GetUpperToken(nwSameLine);   //Get the color
                IF CurCommand<>'' THEN
                  BEGIN
                    FOR n2:=0 TO 17 DO
                      IF CurCommand=ColorNames[n2] THEN df.GlobalTypeComColor:=n2;
                  END;
              END
            ELSE
              BEGIN
                ReportError(translate(22741,'Command not legal in IF, AFTER ENTRY, and BEFORE ENTRY blocks'));    //'Command not legal in IF, AFTER ENTRY, and BEFORE ENTRY blocks'
                ok:=False;
              END;
          END
        ELSE
          BEGIN
            IF AnsiUpperCase(CurCommand)='STATUSBAR' THEN
              BEGIN
                ReportError(translate(22741,'Command not legal in IF, AFTER ENTRY, and BEFORE ENTRY blocks'));    //'Command not legal in IF, AFTER ENTRY, and BEFORE ENTRY blocks'
                ok:=False;
              END
            ELSE IF CurCommand='' THEN
              BEGIN
                ReportError(translate(22746,'Text to TYPE is missing'));   //'Text to TYPE is missing'
                ok:=False;
              END
            ELSE
              BEGIN
                tmpCmdRec.tsVarNumber:=df.FocusedField;
                IF Length(CurCommand)>40 THEN tmpCmdRec.TypeText:=Copy(CurCommand,1,40)
                ELSE tmpCmdRec.TypeText:=CurCommand;
                //Get a colour - if present
                CurCommand:=FParser.GetToken(nwSameLine);  //  NextWord(nwSameLine);
                tmpCmdRec.TypeColor:=2;
                IF CurCommand<>'' THEN
                  BEGIN
                    tmpCmdRec.TypeColor:=-1;
                    FOR n:=0 TO 17 DO
                      IF AnsiUppercase(CurCommand)=ColorNames[n] THEN tmpCmdRec.TypeColor:=n;
                    IF tmpCmdRec.TypeColor=-1 THEN
                      BEGIN
                        ReportError(translate(22743,'Unknown colour'));   //'Unknown colour'
                        ok:=False;
                      END;
                    {Read rest of line - compatibility with Epi Info}
                    REPEAT
                      CurCommand:=FParser.GetUpperToken(nwSameLine);  //  AnsiUpperCase(NextWord(nwSameLine));
                      tmpS:=tmpS+CurCommand;
                    UNTIL CurCommand='';
                  END;  //if CurCommand<>''
                IF ok THEN tmpField.TypeString:=True;
              END;
          END;
      END;  //case cmdTypeString
    cmdBeep:
      BEGIN
        CurCommand:=FParser.GetUpperToken(nwSameLine);  //  AnsiUpperCase(NextWord(nwSameLine));
        tmpCmdRec.BeepType:=btStandard;
        IF CurCommand<>'' THEN
          BEGIN
            IF (CurCommand='WARNING') OR (CurCommand='W') THEN tmpCmdRec.BeepType:=btWarning
            ELSE IF (CurCommand='CONFIRMATION') OR (CurCommand='C') THEN tmpCmdRec.BeepType:=btConfirmation;
          END;
      END;  //cmdBeep
{    cmdLoad:
      BEGIN
        //Syntax: LOAD [path\]dllname[.dll]
        CurCommand:=FParser.GetToken(nwSameLine);  //  NextWord(nwSameLine);
        IF Length(Curcommand)>200 THEN CurCommand:=Copy(CurCommand,1,200);
        IF CurCommand='' THEN
          BEGIN
            ReportError(Format(translate(22126,'The file %s does not exist.'),[CurCommand]));   //22126=The file %s does not exist.
            ok:=False;
          END
        ELSE
          BEGIN
            IF ExtractFileExt(CurCommand)='' THEN CurCommand:=CurCommand+'.DLL';
            IF ExtractFilePath(CurCommand)='' THEN
              BEGIN
                //No path specified - search for file in rec-file's dir. and in EpiData.exe's dir
                tmpS:=ExtractFilePath(df.RECFilename)+CurCommand;
                IF (NOT FileExists(tmpS)) THEN tmpS:=ExtractFilePath(Application.ExeName)+CurCommand;
              END
            ELSE tmpS:=CurCommand;
            IF (NOT FileExists(tmpS)) THEN
              BEGIN
                ReportError(Format(translate(22126,'The file %s does not exist.'),[tmpS]));   //22126=The file %s does not exist.
                ok:=False;
              END;
          END;
        IF ok THEN
          BEGIN
            // DLL file found either by path or in REC-dir og EXE-dir
            // Now save the DLLname and call the function that loads the DLL
            // CHECK IF DLL IS ALREADY LOADED ?
            tmpCmdRec.DLLName:=tmpS;
            // salah entry point here!
            EpiLoadModule(tmpCmdRec.DLLName,df.ModuleInfo);
            // 1. Create UDF List Structure
            df^.UDFList:=TEpiExternalUDFList.Create;
            // 2. 'Interview module' - call fill....
            df^.UDFList.FillFromDLLHandle(df^.ModuleInfo);
          END;
      END;  }
    cmdWriteNote:
      BEGIN
        {Syntax: WRITENOTE "notetext" [SHOW]}
        CurCommand:=FParser.GetToken(nwSameLine);   //NextWord(nwSameLine);
        IF Length(CurCommand)>200 THEN CurCommand:=Copy(CurCommand,1,200);
        REPEAT
          n:=pos('\n',CurCommand);
          IF n=0 THEN n:=pos('\N',CurCommand);
          IF n>0 THEN
            BEGIN
              CurCommand[n]:=' ';
              CurCommand[n+1]:=#13;
            END;
        UNTIL n=0;
        tmpCmdRec.FNote:=CurCommand;
        CurCommand:=FParser.GetUpperToken(nwSameLine);  //  AnsiUpperCase(NextWord(nwSameLine));
        IF CurCommand='SHOW' THEN tmpCmdRec.ShowNotes:=True ELSE tmpCmdRec.ShowNotes:=False;
      END;
    cmdCopyToClipboard:
      BEGIN
        {Syntax: COPYTOCLIPBOARD "text @variable"}
        CurCommand:=FParser.GetToken(nwSameLine);
        IF CurCommand='' THEN
          BEGIN
            ReportError(translate(23028,'Invalid parameters'));  //23028=Invalid parameters
            OK:=False;
          END;
        tmpCmdRec.CopyStr:=Copy(CurCommand,1,200);
      END;
    cmdShowLastRecord:
      BEGIN
        df.ShowLastRecord:=True;
      END;
    cmdExecute:
      BEGIN
        {Syntax: EXECUTE "exe-file name"|* "Parameters"|* NOWAIT|WAIT [HIDE]   }
        {
        Execute bla.htm WAIT
        Execute opera bla.htm WAIT

        }
        CurCommand:=FParser.GetToken(nwSameLine);  //  NextWord(nwSameLine);
        IF CurCommand='' THEN
          BEGIN
            ReportError(translate(22854,'Exe-filename or document-filename is required'));  //22854=Exe-filename or document-filename is required
            OK:=False;
          END;
        tmpCmdRec.ExecCmdLine:=CurCommand;    //=InsertFieldContents(df,CurCommand);
        //Read next: can be parameters or NOWAIT|WAIT
        CurCommand:=FParser.GetToken(nwSameLine);  //  NextWord(nwSameLine);
        tmpCmdRec.ExecParams:='';
        IF (AnsiUpperCase(CurCommand)<>'WAIT') AND (AnsiUpperCase(CurCommand)<>'NOWAIT') THEN
          BEGIN
            //Assume CurCommand contains parameter(s)
            tmpCmdRec.ExecParams:=CurCommand;
            CurCommand:=FParser.GetToken(nwSameLine);  //  NextWord(nwSameLine);
          END;
        CurCommand:=AnsiUpperCase(CurCommand);
        IF (CurCommand<>'WAIT') AND (CurCommand<>'NOWAIT') THEN
          BEGIN
            ReportError(translate(22856,'WAIT or NOWAIT is required'));  //22856=WAIT or NOWAIT is required
            ok:=False;
          END
        ELSE tmpCmdRec.ExecWait:=(CurCommand='WAIT');

        IF ok THEN
          BEGIN
            CurCommand:=FParser.GetUpperToken(nwSameLine);  //  ANSIupperCase(NextWord(nwSameLine));
            tmpCmdRec.ExecHide:=(CurCommand='HIDE');
          END;
      END;
    cmdColor:
      BEGIN
        {Syntax: COLOR QUESTION colors
                 COLOR DATA colors
                 COLOR BACKGROUND color
                 COLOR fieldname datacolors questioncolors

                 Colors can be Epi Info color codes
                 or EpiData color words}

        CurCommand:=FParser.GetUpperToken(nwSameLine);  //  AnsiUpperCase(NextWord(nwSameLine));
        tmpCmdRec.TxtColor:=255;
        tmpCmdRec.BgColor:=255;
        IF CurCommand='QUESTION' THEN tmpCmdRec.ColorCmd:=1
        ELSE IF CurCommand='DATA' THEN tmpCmdRec.ColorCmd:=2
        ELSE IF CurCommand='BACKGROUND' THEN tmpCmdRec.ColorCmd:=3
        ELSE
          BEGIN
            //could be COLOR fieldname
            //will be added later
            ReportError(translate(22858,'Unknown COLOR command'));  //22858=Unknown COLOR command
            ok:=False;
          END;
        IF tmpCmdRec.ColorCmd=3 THEN
          BEGIN
            //command is BACKGROUND
            CurCommand:=FParser.GetUpperToken(nwSameLine);  // AnsiUpperCase(NextWord(nwSameLine));
            IF IsInteger(CurCommand) THEN
              BEGIN
                tmpCmdRec.IsEpiInfoNo:=True;
                n:=StrToInt(CurCommand);
                IF (n<0) OR (n>7) THEN
                  BEGIN
                    ReportError(translate(22860,'Illegal COLOR number'));   //22860=Illegal COLOR number
                    ok:=False;
                  END
                ELSE tmpCmdRec.BgColor:=n;
              END
            ELSE
              BEGIN
                tmpCmdRec.IsEpiInfoNo:=False;
                tmpCmdRec.BgColor:=255;
                FOR n:=0 TO 17 DO
                  IF CurCommand=ColorNames[n] THEN tmpCmdRec.BgColor:=n;
                IF tmpCmdRec.BgColor=255 THEN
                  BEGIN
                    ReportError(translate(22858,'Unknown COLOR command'));  //22858=Unknown COLOR command
                    ok:=False;
                  END;
              END;
          END
        ELSE
          BEGIN
            //read rest of line
            tmpS:='';
            REPEAT
              CurCommand:=FParser.GetUpperToken(nwSameLine);  //  AnsiUpperCase(NextWord(nwSameLine));
              tmpS:=tmpS+CurCommand+' ';
            UNTIL CurCommand='';
            IF GetColors(tmpS,bb,bb2,bb3,IsEpiInfo) THEN
              BEGIN
                tmpCmdRec.TxtColor:=bb;
                tmpCmdRec.BgColor:=bb2;
                tmpCmdRec.IsEpiInfoNo:=IsEpiInfo;
                IF bb3<>255 THEN
                  BEGIN
                    //highlightcolor specified
                    df.FieldHighlightAct:=True;
                    df.FieldHighlightCol:=ColorValues[bb3];
                  END;
              END
            ELSE
              BEGIN
                ReportError(translate(22862,'Unknown color in COLOR command'));  //22862=Unknown color in COLOR command
                ok:=False;
              END;
          END;
      END;
    cmdBackup:
      BEGIN
        {syntax: BACKUP "destination-library" [ZIP filename [date]]
         or      BACKUP "destination-library" [ENCRYPT filname password [date]] }
        IF (df.CheckFileMode) AND (cmdList<>df.AfterFileCmds) THEN
          BEGIN
            ReportError(translate(22864,'BACKUP command only legal in AFTER FILE blocks'));  //22864=BACKUP command only legal in AFTER FILE blocks
            ok:=False;
          END
        ELSE
          BEGIN
            CurCommand:=FParser.GetToken(nwSameLine);  //  NextWord(nwSameLine);
            IF CurCommand='' THEN
              BEGIN
                ReportError(translate(22866,'BACKUP command without destination directory'));  //22866=BACKUP command without destination directory
                ok:=False;
              END
            ELSE IF (df.BackupList=NIL) AND (NOT df.IsRelateFile) THEN
              BEGIN
                tmpCmdRec.zipit:=False;
                tmpCmdrec.encryptit:=False;
                tmpCmdRec.DestLib:=CurCommand;
                CurCommand:=FParser.GetUpperToken(nwSameLine);
                IF (CurCommand<>'ZIP') AND (CurCommand<>'ENCRYPT') THEN
                  BEGIN
                    df.BackupList:=TStringList.Create;
                    df.BackupList.Append(tmpCmdRec.DestLib);
                    df.BackupList.Append(df.RECFilename);
                  END
                ELSE
                  BEGIN
                    //ZIP or ENCRYPT added as parameters
                    IF CurCommand='ZIP' THEN
                      BEGIN
                        CurCommand:=FParser.GetToken(nwSameLine);   //get the filename
                        IF CurCommand='' THEN
                          BEGIN
                            ReportError(translate(22884,'Filename needed after ZIP'));   //'Filename needed after ZIP'
                            ok:=False;
                          END
                        ELSE
                          BEGIN
                            df.BackupList:=TStringList.Create;
                            df.BackupList.Append(tmpCmdRec.DestLib);
                            tmpCmdRec.zipit:=True;
                            tmpCmdrec.filename:=ExtractFilename(CurCommand);
                            CurCommand:=FParser.GetUpperToken(nwSameLine);   //get date parameter
                            IF CurCommand='DATE' THEN tmpCmdRec.dateit:=True;
                          END;
                      END
                    ELSE
                      BEGIN
                        //encrypt it
                        CurCommand:=FParser.GetToken(nwSameLine);   //get the filename
                        IF CurCommand='' THEN
                          BEGIN
                            ReportError(translate(22886,'Filename needed after ENCRYPT'));   //'Filename needed after ENCRYPT'
                            ok:=False;
                          END
                        ELSE
                          BEGIN
                            tmpCmdRec.encryptit:=True;
                            tmpCmdRec.filename:=ExtractFilename(CurCommand);
                            CurCommand:=FParser.GetToken(nwSameLine);   //get the password
                            IF CurCommand='' THEN
                              BEGIN
                                ReportError(translate(22888,'Password must follow ENCRYPT and filename'));  //'Password must follow ENCRYPT and filename'
                                ok:=False;
                              END
                            ELSE
                              BEGIN
                                df.BackupList:=TStringList.Create;
                                df.BackupList.Append(tmpCmdRec.DestLib);
                                tmpCmdRec.pw:=CurCommand;
                                CurCommand:=FParser.GetUpperToken(nwSameLine);  //get date parameter
                                IF CurCommand='DATE' THEN tmpCmdRec.dateit:=True;
                              END;
                          END;
                      END;
                  END;  //if zip or encrypt
              END;  //if ok to create backuplist
          END;  //if in afterfile
      END;  //end case cmdBackup
    cmdRelate:
      BEGIN
        //Syntax: RELATE fieldname filename [1]
        //Get fieldname
        CurCommand:=FParser.GetToken(nwSameLine);  //  NextWord(nwSameLine);
        IF CurCommand='' THEN
          BEGIN
            ReportError(Translate(22840,'Error in RELATE command'));   //
            ok:=False;
          END
        ELSE
          BEGIN
            n:=df.FieldNumbers[CurCommand];
            IF n=-1 THEN
              BEGIN
                ReportError(Translate(22708,'Unknown fieldname'));
                ok:=False;
              END
            ELSE
              BEGIN
                //Check if field is KEY UNIQUE
                IF n=df.FocusedField THEN
                  BEGIN
                    n2:=tmpField.Index;
                    IF n2=0 THEN ok:=FALSE
                    ELSE IF df.IndexIsUnique[n2]=False THEN ok:=False;
                  END
                ELSE
                  BEGIN
                    n2:=df[n].Index;
                    IF n2=0 THEN ok:=False
                    ELSE IF df.IndexIsUnique[n2]=False THEN ok:=False;
                  END;
                IF NOT ok THEN ReportError(Translate(22842,'RELATE field must be KEY UNIQUE'));
              END;
          END;
        IF ok THEN
          BEGIN
            //Get relatefile name
            tmpCmdRec.RelField:=df[n].FieldName;  //save fieldname
            n2:=n;  //save fieldnumber to use with relateinfo
            Curcommand:=FParser.GetToken(nwSameLine);  //  NextWord(nwSameLine);
            IF CurCommand='' THEN
              BEGIN
                ReportError(Translate(22840,'Error in RELATE command'));
                ok:=False;
              END
            ELSE
              BEGIN
                tmpS:=CurCommand;
                IF ExtractFileExt(tmpS)='' THEN tmpS:=tmpS+'.rec';
                IF ExtractFileExt(tmpS)<>'.REC' THEN ChangeFileExt(tmpS,'.rec');
                tmpS2:=GetCurrentDir;   //&&
                SetCurrentDir(ExtractFileDir(df.RECFilename));
                tmpS:=ExpandFilename(tmpS);              //&&
                SetCurrentDir(tmpS2);
                //IF ExtractFilePath(tmpS)='' THEN tmpS:=ExtractFilePath(df^.RECFilename)+tmpS;
                tmpS:=AnsiLowerCase(tmpS);
                IF (NOT FileExists(tmpS)) AND (FMultiLineError) THEN
                  BEGIN
                    ReportError(Format(Translate(22126,'The file %s does not exist.'),[tmpS]));
                    ok:=False;
                  END
                ELSE
                  BEGIN
                    TopDf:=TEpiDataFile(df.TopEpiDataFile);
                    IF NOT Assigned(TopDf.RelateFiles) THEN TopDf.RelateFiles:=TStringList.Create;
                    IF NOT Assigned(TopDf.RelateMothers) THEN TopDf.RelateMothers:=TList.Create;
                    n:=TopDf.RelateFiles.IndexOf(CurCommand);
                    IF n=-1 THEN
                      BEGIN
                        TopDf.RelateFiles.AddObject(tmpS,NIL);
                        TopDf.RelateMothers.Add(Pointer(df));
                      END
                    ELSE TopDf.RelateMothers.Items[n]:=Pointer(df);
                    tmpCmdRec.RelFileNo:=TopDf.RelateFiles.IndexOf(tmpS);
                    tmpCmdRec.RelFileStr:=CurCommand;
                    New(AInfo);
                    //Fill out relatefile information
                    AInfo^.RelFileNo:=tmpCmdRec.RelFileNo;
                    AInfo^.RelFieldNo:=n2;
                    AInfo^.CmdInFieldNo:=df.FocusedField;
                    AInfo^.Next:=NIL;
                    //Link relatefile information to chain of relatefile infos
                    IF df.RelateInfo=NIL THEN df.RelateInfo:=AInfo
                    ELSE
                      BEGIN
                        BInfo:=df.RelateInfo;
                        WHILE BInfo^.Next<>NIL DO BInfo:=BInfo^.Next;
                        BInfo^.Next:=AInfo;
                      END;
                    //df^.RelateInfo:=df^.RelateInfo+'"Relates to '+ExtractFilename(tmpS)+'","'+
                    //'via '+tmpCmdRec.RelField+'","",';
                    df.HasRelate:=True;
                  END;
              END;
          END;
        IF ok THEN
          BEGIN
            //Get One2One marker
            CurCommand:=FParser.GetToken(nwSameLine);  //  NextWord(nwSameLine);
            tmpCmdRec.One2One:=(CurCommand='1');
            AInfo^.One2One:=tmpCmdRec.One2One;
          END;
        CurCommand:='';
      END;   //case cmdRelate
  END;  //Case
  IF ok THEN
    BEGIN
      IF CmdList=NIL THEN CmdList:=TChkCommands.Create;
      tmpCmdPtr:=CmdList.NewCmd;
      tmpCmdPtr^:=tmpCmdRec;
      CmdList.AddCommand(tmpCmdPtr);
      IF (CmdList=df.BeforeFileCmds) AND (tmpCmdRec.Command=cmdColor) THEN
        BEGIN
          IF tmpCmdRec.IsEpiInfoNo THEN
            BEGIN
              IF tmpCmdRec.TxtColor<>255 THEN tmpTxtColor:=TextColors[tmpCmdRec.TxtColor] ELSE tmpTxtColor:=COLOR_ENDCOLORS;
              IF tmpCmdRec.BgColor<>255 THEN tmpBgColor:=BgColors[tmpCmdRec.BgColor] ELSE tmpBgColor:=COLOR_ENDCOLORS;
            END
          ELSE
            BEGIN
              IF tmpCmdRec.TxtColor<>255 THEN tmpTxtColor:=ColorValues[tmpCmdRec.TxtColor] ELSE tmpTxtColor:=COLOR_ENDCOLORS;
              IF tmpCmdRec.BgColor<>255 THEN tmpBgColor:=ColorValues[tmpCmdRec.BgColor] ELSE tmpBgColor:=COLOR_ENDCOLORS;
            END;
          CASE tmpCmdRec.ColorCmd OF
            1: BEGIN
                 IF tmpTxtColor<>COLOR_ENDCOLORS THEN df.QuestionText:=tmpTxtColor;
                 IF tmpBgColor<>COLOR_ENDCOLORS THEN df.QuestionBg:=tmpBgColor;
               END;
            2: BEGIN
                 IF tmpTxtColor<>COLOR_ENDCOLORS THEN df.FieldText:=tmpTxtColor;
                 IF tmpBgColor<>COLOR_ENDCOLORS THEN df.FieldBg:=tmpBgColor;
               END;
            3: IF tmpBgColor<>COLOR_ENDCOLORS THEN df.Background:=tmpBgColor;
          END;  //case
        END;  //if
    END
  ELSE
    BEGIN
      FTempResult:=False;
      if tmpCmdRec.ValueLabel<>NIL then tmpCmdRec.ValueLabel.Free;
      IF cmd=cmdIF THEN
        BEGIN
          IF tmpCmdRec.IfCmds<>NIL THEN TChkCommands(tmpCmdRec.IfCmds).Free;
          IF tmpCmdRec.ElseCmds<>NIL THEN TChkCommands(tmpCmdRec.ElseCmds).Free;
        END;
    END;
END;  //GetCommand


Procedure TCheckObj.AddTopComment;
VAR
  s: String;
BEGIN
  s:=FParser.GetWholeLine;
  IF (FCheckFileMode) AND (Assigned(df.ChkTopComments)) THEN df.ChkTopComments.Append(s);
END;  //Procedure AddTopComment


Procedure TCheckObj.RetrieveFlawBlock;
VAR
  s: String;
BEGIN
  IF NOT FCheckFileMode THEN FTempResult:=False;
  CommentsAddedToCheckFile:=True;
  s:=FParser.GetLineAndFlush;
  ReportError(translate(22732,'Unknown command in line'));  //'Unknown command in line'
  IF (FirstTopFlaw) AND (FCheckFileMode) AND (FMultiLineError) THEN
    BEGIN
      FirstTopFlaw:=False;
(*      {$IFNDEF epidat}
      IF eDlg(Format(translate(22788,'Unknown fieldname found in checkfile %s'),   //'Unknown fieldname found in checkfile %s'
         [ExtractFilename(df^.CHKFilename)])+#13#13+
         translate(22790,'Do you want to save the checks of the unknown fieldname~as commentlines in the checkfile?')+#13#13+  //'Do you want to save the checks of the unknown fieldname~as commentlines in the checkfile?'
         translate(22792,'If you choose No the checks of the unknown fieldname will~be deleted when the revised checks are saved.'),   //'If you choose No the checks of the unknown fieldname will~be deleted when the revised checks are saved.'
         mtWarning,[mbYes,mbNo],0)=mrYes THEN SaveTopFlawsAsComments:=True;
      Screen.Cursor:=crHourGlass;
      {$ENDIF}*)
    END;  //FirstTopFlaw
  REPEAT
    IF SaveTopFlawsAsComments THEN
      BEGIN
        FParser.CommentCurLine;
        AddTopComment;
      END;
    CurCommand:=FParser.GetToken(nwAny);  // NextWord(nwAny);
    s:=FParser.GetLineAndFlush;
  UNTIL (FParser.EndOfLines) or (AnsiUpperCase(CurCommand)='END');
  IF AnsiUpperCase(CurCommand)='END' THEN
    BEGIN
      FParser.CommentCurLine;
      AddTopComment;
    END;
END;  //procedure RetrieveFlawBlock


Procedure TCheckObj.RetrieveRange;
VAR
  tmpS:String;
  RangeResult:Boolean;
BEGIN
  RangeResult:=True;
  {Get minimum value}
  CurCommand:=FParser.GetUpperToken(nwSameLine);  // AnsiUpperCase(NextWord(nwSameLine));
  IF CurCommand='' THEN
    BEGIN
      {$IFNDEF epidat}
      ReportError(translate(22712,'RANGE command without mininum value'));   //'RANGE command without mininum value'
      {$ENDIF}
      RangeResult:=False;
    END
  ELSE tmpField.Min:=CurCommand;
  IF tmpField.Min='-INFINITY' THEN tmpField.Min:='';
  {Get maxinum value}
  CurCommand:=FParser.GetUpperToken(nwSameLine);  // AnsiUpperCase(NextWord(nwSameLine));
  IF CurCommand='' THEN
    BEGIN
      {$IFNDEF epidat}
      ReportError(translate(22714,'RANGE command without maximum value'));  //'RANGE command without maximum value'
      {$ENDIF}
      RangeResult:=False;
    END
  ELSE tmpField.Max:=CurCommand;
  IF tmpField.Max='INFINITY' THEN tmpField.Max:='';

  {Check if range values are compliant with Fieldtype}
  IF (tmpField.Min<>'') AND (NOT IsCompliant(tmpField.Min,tmpField.Fieldtype)) THEN
    BEGIN
      {$IFNDEF epidat}
      ReportError(translate(22716,'Minimum value is not compatible with this type of field'));  //'Minimum value is not compatible with this type of field'
      {$ENDIF}
      RangeResult:=False;
    END;
  IF (RangeResult) AND (tmpField.Max<>'') AND (NOT IsCompliant(tmpField.Max,tmpField.Fieldtype)) THEN
    BEGIN
      {$IFNDEF epidat}
      ReportError(translate(22718,'Maximum value is not compatible with this type of field'));  //'Maximum value is not compatible with this type of field'
      {$ENDIF}
      RangeResult:=False;
    END;

  IF RangeResult THEN
    BEGIN
      WITH tmpField DO
        BEGIN
          IF Min='' THEN tmpS:='-INF-' ELSE tmpS:=Min+'-';
          IF Max='' THEN tmpS:=tmpS+'INF' ELSE tmpS:=tmpS+Max;
          RangeDefined:=True;
          IF Legal='' THEN Legal:=tmpS
          ELSE Legal:=tmpS+','+Legal;
          Legal:=RemoveQuotes(Legal);
        END;
    END
  ELSE FTempResult:=False;
  CurCommand:='';
END;  //function RetrieveRange


Procedure TCheckObj.RetrieveLegals;
VAR
  StopGet,LegalResult,UsedUse,FirstLegalResult:Boolean;
  n: Integer;
  s: string;
BEGIN
  UsedUse:=False;
  StopGet:=False;
  LegalResult:=True;
  FirstLegalResult:=True;
  LegList.Clear;
  REPEAT   //until StopGet
    IF NOT FParser.EndOfLines THEN CurCommand:=FParser.GetToken(nwAny)  //  NextWord(nwAny)
    ELSE
      BEGIN   //EndOfChkFile found before END
        {$IFNDEF epidat}
        ReportError(translate(22704,'Missing END of LEGAL-block.'));   //'Missing END of LEGAL-block.'
        {$ENDIF}
        LegalResult:=False;
        StopGet:=True;
      END;
    IF AnsiUpperCase(CurCommand)='END' THEN StopGet:=True
    ELSE IF AnsiUpperCase(CurCommand)='USE' THEN
      BEGIN
        //LEGAL USE structure
        CurCommand:=AnsiLowerCase(FParser.GetToken(nwSameLine));  //AnsiLowerCase(NextWord(nwSameLine));
        s:='';
        {$IFNDEF epidat}
        IF CurCommand='' THEN s:=translate(22706,'LEGAL USE command without fieldname');  //'LEGAL USE command without fieldname'
        n:=FFieldNameList.IndexOf(AnsiUpperCase(CurCommand));
        IF n=-1 THEN s:=translate(22708,'Unknown fieldname');  //'Unknown fieldname'
        {$ELSE}
        IF CurCommand='' THEN s:='LEGAL USE command without fieldname';
        n:=FFieldNameList.IndexOf(AnsiUpperCase(CurCommand));
        IF n=-1 THEN s:='Unknown fieldname';       //22708=Unknown field name
        {$ENDIF}
        IF s<>'' THEN
          BEGIN
            ReportError(s);
            FTempResult:=False;
          END
        ELSE
          BEGIN    //Fieldname came after the USE command
            IF df[n].RangeDefined THEN
              BEGIN
                LegList.CommaText:=df[n].Legal;
                LegList.Delete(0);
                tmpField.Legal:=RemoveQuotes(LegList.Commatext);
              END
            ELSE tmpField.Legal:=df[n].Legal;
            UsedUse:=True;
            StopGet:=True;
          END;
      END  //the word USE was found
    ELSE IF CurCommand<>'' THEN
      BEGIN
        IF IsCompliant(CurCommand,df[df.FocusedField].Fieldtype)
        THEN LegList.Add(CurCommand)
        ELSE
          BEGIN
            {$IFNDEF epidat}
            ReportError(translate(22710,'Legal value is not compatible with this Fieldtype'));  //'Legal value is not compatible with this Fieldtype'
            {$ENDIF}
            LegalResult:=False;
          END;
      END;  //else
  UNTIL StopGet;

  IF LegalResult THEN
    BEGIN
      WITH tmpField DO
        BEGIN
          IF NOT UsedUse THEN
            BEGIN
              IF Legal='' THEN Legal:=LegList.CommaText
              ELSE Legal:=Legal+','+LegList.CommaText;
              Legal:=RemoveQuotes(Legal);
            END;
        END;  //with
    END
  ELSE FTempResult:=False;
  CurCommand:='';
END;  //function RetrieveLegals


Procedure TCheckObj.RetrieveAutoJump;
BEGIN
  CurCommand:=FParser.GetUpperToken(nwSameLine);   // AnsiUpperCase(NextWord(nwSameLine));
  IF CurCommand='' THEN
    BEGIN
      ReportError(translate(22728,'AUTOJUMP command without name of field to jump to'));  //'AUTOJUMP command without name of field to jump to'
      FTempResult:=False;
    END;

  IF (FFieldNameList.IndexOf(CurCommand)=-1) AND (CurCommand<>'END')
  AND (CurCommand<>'WRITE') AND (CurCommand<>'SKIPNEXTFIELD') THEN
    BEGIN
      ReportError(translate(22730,'Unknown fieldname in AUTOJUMP command'));  //'Unknown fieldname in AUTOJUMP command'
      FTempResult:=False;
    END
  ELSE tmpField.jumps:='AUTOJUMP '+CurCommand;
  CurCommand:='';
END;  //procedure RetrieveAutojump


Procedure TCheckObj.RetrieveJumps;
VAR
  JumpsResult,StopGet:Boolean;
  tmpS: String;
BEGIN
  StopGet:=False;
  JumpsResult:=True;
  LegList.Clear;
  REPEAT   //until StopGet
    tmpS:='';
    {Check if a RESET command exists after JUMPS}
    CurCommand:=FParser.GetToken(nwSameLine);   //  NextWord(nwSameLine);
    IF CurCommand<>'' THEN
      BEGIN
        IF AnsiUpperCase(CurCommand)<>'RESET' THEN
          BEGIN
            {$IFNDEF epidat}
            ReportError(Format(translate(22830,'RESET expected but %s found'),[CurCommand]));   //'RESET expected but %s found'
            {$ENDIF}
            Jumpsresult:=False;
            StopGet:=True;
          END
        ELSE
          BEGIN
            tmpField.JumpResetChar:=#32;
            CurCommand:=FParser.GetToken(nwSameLine);  //  NextWord(nwSameLine);
            IF Length(CurCommand)=1 THEN tmpField.JumpResetChar:=CurCommand[1];
          END;
      END;
    {Read value}
    IF NOT FParser.EndOfLines THEN CurCommand:=FParser.GetUpperToken(nwAny)  //AnsiUpperCase(NextWord(nwAny))
    ELSE
      BEGIN   //EndOfChkFile found before END
        {$IFNDEF epidat}
        ReportError(translate(22720,'Missing END of JUMPS-block'));  //'Missing END of JUMPS-block'
        {$ENDIF}
        JumpsResult:=False;
        StopGet:=True;
      END;
    IF CurCommand='END' THEN StopGet:=True
    ELSE IF CurCommand<>'' THEN
      BEGIN
        CASE tmpField.Fieldtype OF
          ftInteger,ftIDNUM: IF IsInteger(CurCommand)
                             THEN tmpS:=trim(CurCommand)+'>'
                             ELSE JumpsResult:=False;

          ftFloat:           IF IsFloat(CurCommand)
                             THEN tmpS:=trim(CurCommand)+'>'
                             ELSE JumpsResult:=False;

          ftYMDDate,                  //&&
          ftDate,ftEuroDate: BEGIN
                               tmpS:=CurCommand;
                               IF mibIsDate(tmpS,tmpField.Fieldtype)
                               THEN tmpS:=tmpS+'>'
                               ELSE JumpsResult:=False;
                             END;

          ftBoolean:         IF (Length(Curcommand)=1) AND (CurCommand[1] in BooleanChars)
                             THEN tmpS:=CurCommand+'>'
                             ELSE JumpsResult:=False;

          ftToDay,ftYMDToday,
          ftEuroToday:       JumpsResult:=False;
        ELSE
          tmpS:=trim(CurCommand)+'>';
        END;  //Case

        {$IFNDEF epidat}
        IF NOT JumpsResult THEN ReportError(translate(22722,'Illegal datatype'));  //'Illegal datatype'
        {$ENDIF}

        {Get name of field to jump to}
        IF NOT FParser.EndOfLines THEN CurCommand:=FParser.GetToken(nwSameLine)  // NextWord(nwSameLine)
        ELSE
          BEGIN   //EndOfChkFile found before END
            {$IFNDEF epidat}
            ReportError(translate(22724,'Jumps command without field to jump to'));  //'Jumps command without field to jump to'
            {$ENDIF}
            JumpsResult:=False;
            StopGet:=True;
          END;

        IF (JumpsResult) AND (FFieldNameList.IndexOf(CurCommand)=-1) THEN
          BEGIN
            IF AnsiLowerCase(CurCommand)='end' THEN CurCommand:='END'
            ELSE IF AnsiLowerCase(CurCommand)='write' THEN CurCommand:='WRITE'
            ELSE IF AnsiLowerCase(CurCommand)='skipnextfield' THEN Curcommand:='SKIPNEXTFIELD';
            IF (CurCommand<>'END') AND (CurCommand<>'WRITE') AND (CurCommand<>'SKIPNEXTFIELD') THEN
              BEGIN
                {$IFNDEF epidat}
                ReportError(translate(22726,'Unknown fieldname in JUMP block'));  //'Unknown fieldname in JUMP block'
                {$ENDIF}
                JumpsResult:=False;
                StopGet:=True;
              END;
          END;

        IF JumpsResult THEN
          BEGIN
            tmpS:=tmpS+CurCommand;
            LegList.Add(tmpS);
          END;
      END;  //else
  UNTIL StopGet;

  IF JumpsResult THEN tmpField.jumps:=RemoveQuotes(LegList.CommaText)
  ELSE FTempResult:=False;
  CurCommand:='';
END;  //Procedure RetrieveJumps

procedure TCheckObj.RetrieveMissingValues;
VAR
  s1,s2,s3: string;
BEGIN
  //Syntax:  MISSINGVALUE x [x [x]]  where x is str10
  s1:=FParser.GetToken(nwSameLine);
  s2:=FParser.GetToken(nwSameLine);
  s3:=FParser.GetToken(nwSameLine);
  IF (Length(s1)>tmpField.Length)
  OR (Length(s2)>tmpField.Length)
  OR (Length(s3)>tmpField.Length) THEN
    BEGIN
      FTempResult:=False;
      ReportError(translate(22852,'Value is too wide for field'));   //22852=Value is too wide for field
    END;
  IF FTempResult THEN
    BEGIN
      IF ((s1<>'') AND (NOT IsCompliant(s1,tmpField.Fieldtype)))
      OR ((s2<>'') AND (NOT IsCompliant(s2,tmpField.Fieldtype)))
      OR ((s3<>'') AND (NOT IsCompliant(s3,tmpField.Fieldtype))) THEN
        BEGIN
          FTempResult:=False;
          ReportError(translate(22710,'Value is not compatible with this Fieldtype'));  //'Value is not compatible with this Fieldtype');
        END;
    END;
  IF FTempResult THEN
    BEGIN
      tmpField.MissingValues[0]:=s1;
      tmpField.MissingValues[1]:=s2;
      tmpField.MissingValues[2]:=s3;
    END;
END;  //procedure TCheckObj.RetrieveMissingValues


procedure TCheckObj.RetrieveDefaultValue;
VAR
  s1,s2,s3: string;
BEGIN
  //Syntax:  DEFAULTVALUE x where x is string
  s1:=FParser.GetToken(nwSameLine);
  IF (length(s1)>tmpField.Length) THEN
    BEGIN
      FTempResult:=False;
      ReportError(translate(22852,'Value is too wide for field'));   //22852=Value is too wide for field
    END
  ELSE
    BEGIN
      IF ((s1<>'') AND (NOT IsCompliant(s1,tmpField.Fieldtype))) THEN
        BEGIN
          FTempResult:=False;
          ReportError(translate(22710,'Value is not compatible with this Fieldtype'));  //'Value is not compatible with this Fieldtype');
        END;
    END;
  IF FTempResult THEN tmpField.DefaultValue:=s1;
END;  //procedure TCheckObj.RetrieveDefaultValues


Procedure TCheckObj.RetrieveAutosearch;
VAR
  n:Integer;
BEGIN
  tmpField.AutoFields:='';
  CurCommand:=FParser.GetUpperToken(nwSameLine);
  IF (CurCommand='LIST') OR (CurCommand='SOUNDEX') THEN
    BEGIN
      IF CurCommand='LIST' THEN tmpField.AutoList:=True;
      CurCommand:=FParser.GetUpperToken(nwSameLine);
      IF (CurCommand='LIST') OR (CurCommand='SOUNDEX') THEN
        BEGIN
          IF CurCommand='LIST' THEN tmpField.AutoList:=True;
          CurCommand:=FParser.GetUpperToken(nwSameLine);
        END;  //if
    END;  //if
  REPEAT
    n:=df.FieldNumbers[CurCommand];
    IF n=-1 THEN
      BEGIN
        FTempResult:=False;
        ReportError(Translate(22708,'Unknown fieldname'));
        Exit;
      END
    ELSE tmpField.AutoFields:=tmpField.AutoFields+IntToStr(n)+',';
    CurCommand:=FParser.GetToken(nwSameLine);
  UNTIL (CurCommand='') or (FTempResult=False);
  IF tmpField.AutoFields[Length(tmpField.AutoFields)]=','
  THEN tmpField.AutoFields:=Copy(tmpField.AutoFields,1,Length(tmpField.AutoFields)-1);
  tmpField.Autosearch:=True;
END;  //procedure TCheckObj.RetrieveAutosearch


function TCheckObj.RetrieveCommentLegal(AsCommand:Boolean; var ValueLabelType:TValueLabelSetType; var Show:boolean; var ValueLabelUse:string):TValueLabelSet;

VAR
  s,s2,LabelName,tmpS2,peekErrors,tmpValue,tmpText: String;
  n,CurRec: Integer;
  ValueField,TextField: TeField;
  tmpValueLabelSet: TValueLabelSet;
  ok,StopRead:Boolean;
  ComLegDf: TEpiDatafile;
  F: TIndexFile;
  F2:TextFile;
  s30: str30;
  TooLong,NotCompatible:Boolean;
  tmpStrings: TStrings;
  ShowList: Boolean;
BEGIN
  {Four kinds of COMMENT LEGAL possible:
  1. COMMENT LEGAL
       1  ...
       2  ...
     END
     Name in ValueLabels has a $ in the end
     ValueLabelType=vltLocal

  2. COMMENT LEGAL USE labelname
     FValueLabel has has ¤ in the end
     ValueLabelType=vltLabelRef

  3. COMMENT LEGAL USE fieldname
     ValueLabelType=vltFieldRef

  4. COMMENT LEGAL datafilename
     ValueLabelType=vltFile

  }

  result:=TValueLabelSet.Create;

  Show:=False;
  ValueLabelUse:='';
  ValueLabelType:=vltLocal;
  CurCommand:=FParser.GetUpperToken(nwSameLine);
  IF CurCommand<>'LEGAL' THEN
    BEGIN
      ReportError(translate(22732,'Unknown command in line'));
      FTempResult:=False;
    END
  ELSE
    BEGIN
      CurCommand:=FParser.GetUpperToken(nwSameLine);
      IF (CurCommand='') OR (CurCommand='SHOW') THEN
        BEGIN
          //1. scenario: COMMENT LEGAL...END Structure
          IF CurCommand='SHOW' THEN Show:=True;
          StopRead:=False;
          ok:=True;
          REPEAT
            //Read value
            CurCommand:=FParser.GetToken(nwAny);
            IF AnsiUpperCase(CurCommand)='END' THEN StopRead:=True
            ELSE IF trim(CurCommand)<>'' THEN
              BEGIN
                s:=trim(CurCommand);
                IF s[1]='*' THEN
                  BEGIN
                    s:=trim(FParser.GetWholeLine);
                    IF NOT FCheckFileMode THEN Continue;
                    IF Length(s)>(30+80) THEN
                      BEGIN
                        ReportError(translate(22874,'Commented line is too long'));   //22874=Commented line is too long
                        StopRead:=True;
                        FTempResult:=False;
                      END
                    ELSE result.AddValueLabelPair(Copy(s,1,30),Copy(s,31,length(s)));
                  END
                ELSE IF Length(trim(CurCommand))>tmpField.Length THEN
                  BEGIN
                    StopRead:=True;
                    FTempResult:=False;
                    ReportError(translate(22852,'Value is too wide for field'));   //22852=Value is too wide for field
                  END
                ELSE IF IsCompliant(trim(CurCommand),tmpField.Fieldtype) THEN
                  BEGIN
                    IF Length(CurCommand)>30 THEN CurCommand:=Copy(CurCommand,1,30);
                    tmpValue:=trim(CurCommand);
                    //Read text
                    CurCommand:=FParser.GetToken(nwSameLine);  //  NextWord(nwSameLine);
                    IF trim(CurCommand)='' THEN
                      BEGIN
                        StopRead:=True;
                        ok:=False;
                      END
                    ELSE
                      BEGIN
                        IF Length(CurCommand)>80 THEN CurCommand:=Copy(CurCommand,1,80);
                        WHILE pos('"',CurCommand)>0 DO Delete(CurCommand,Pos('"',CurCommand),1);
                        tmpText:=CurCommand;
                      END;
                    result.AddValueLabelPair(tmpValue,tmpText);
                  END  //if value is compliant with Fieldtype
                ELSE
                  BEGIN
                    StopRead:=True;
                    FTempResult:=False;
                    ReportError(translate(22710,'Value is not compatible with this Fieldtype'));  //'Value is not compatible with this Fieldtype');
                  END;
              END  //if curCommand<>END and CurCommand<>''
            ELSE
              BEGIN
                StopRead:=True;
                FTempResult:=False;
                ReportError(translate(22734,'Unexpected end of COMMENT LEGAL'));  //'Unexpected end of COMMENT LEGAL'
              END;
          UNTIL StopRead;
          IF FTempResult THEN
            BEGIN
              IF AsCommand
              THEN s:='ComLegal'+IntToStr(df.ValueLabels.count)
              ELSE s:=Translate(22736,'labels in field')+' '+df[df.FocusedField].FieldName;
              s:=AnsiLowerCase(s);
              if df.ValueLabels.ValueLabelSetByName(s)<>NIL then df.ValueLabels.DeleteValueLabelSet(s);
              result.Name:=s;
              df.ValueLabels.AddValueLabelSet(result);
            END  //if ok
          ELSE
            begin
              result.Free;
              result:=NIL;
            end
        END  //if COMMENT LEGAL...END Structure
      ELSE IF CurCommand='USE' THEN
        BEGIN
          //COMMENT LEGAL USE structure
          CurCommand:=AnsiLowerCase(FParser.GetToken(nwSameLine));
          s:='';
          IF CurCommand='' THEN s:=' '+translate(22738,'COMMENT LEGAL USE command without labelname or fieldname');
          tmpValueLabelSet:=df.ValueLabels.ValueLabelSetByName(CurCommand);
          IF assigned(tmpValueLabelSet) THEN
            BEGIN
              //comment legal use references a labelname
              ValueLabelType:=vltLabelRef;
              ValueLabelUse:=CurCommand;
              Result:=tmpValueLabelSet;
            END
          ELSE
            BEGIN
              n:=FFieldNameList.IndexOf(AnsiUpperCase(CurCommand));
              if (n>-1) then
                begin
                  //comment legal use references a fieldname
                  ValueLabelType:=vltFieldRef;
                  ValueLabelUse:=CurCommand;
                  Result:=df.Fields[n].Valuelabel;
                end
              else
                begin
                  ReportError(' '+translate(22740,'Unknown labelname or fieldname'));
                  FTempResult:=false;
                end
            END;
          IF FTempResult THEN
            BEGIN
              s2:=CurCommand;
              CurCommand:=FParser.GetUpperToken(nwSameLine);
              IF CurCommand='SHOW' THEN Show:=True;
              CurCommand:=s2;

              //check is labels are compatible with current field
              TooLong:=False;
              NotCompatible:=False;
              n:=0;
              WHILE (n<result.count) AND (NOT TooLong) AND (NOT NotCompatible) DO
                BEGIN
                  tmpValue:=result.Values[n];
                  tmpText:=result.Labels[n];
                  IF tmpValue[1]<>'*' THEN
                    BEGIN
                      IF Length(trim(tmpValue))>tmpField.Length THEN TooLong:=True;
                      IF (NOT IsCompliant(trim(tmpValue),tmpField.Fieldtype)) THEN NotCompatible:=True;
                    END;
                  inc(n);
                END;
              IF NotCompatible THEN
                BEGIN
                  StopRead:=True;
                  FTempResult:=False;
                  result:=NIL;
                  ReportError(translate(22710,'Value is not compatible with this Fieldtype'));
                END
              ELSE IF TooLong THEN
                BEGIN
                  StopRead:=True;
                  FTempResult:=False;
                  Result:=NIL;
                  ReportError(translate(22852,'Value is too wide for field'));
                END  //if TooLong
            END;
        END  //the word USE was found
      ELSE
        BEGIN  //Not Comment legal..end and not comment legal use
          IF ExtractFileExt(Curcommand)='' THEN s:=CurCommand+'.rec' ELSE s:=CurCommand;
          ValueLabelUse:=s;
          ValueLabelType:=vltFile;
          tmpS2:=GetCurrentDir;
          SetCurrentDir(ExtractFileDir(df.RECFilename));
          s:=ExpandFilename(s);
          SetCurrentDir(tmpS2);

          IF FCheckFileMode THEN
            BEGIN
              //Don't test if file exists and don't apply index
              CurCommand:=FParser.GetUpperToken(nwSameLine);
              IF CurCommand='SHOW' THEN Show:=True;
              result.Name:=AnsiLowerCase('Labels from '+ExtractFileName(s));
            END
          ELSE
            BEGIN
              IF NOT FileExists(s) THEN
                BEGIN
                  ReportError(Format(translate(20110,'Datafile %s does not exist.'),[s]));
                  FTempResult:=False;
                END
              ELSE
                BEGIN
                  //Comment Legal datafilename structure found
                  CurCommand:=FParser.GetUpperToken(nwSameLine);
                  IF CurCommand='SHOW' THEN Show:=True;
                  TRY
                    ComLegDf:=TEpiDataFile.Create;
                    ComLegDf.OnRequestPassword:=df.OnRequestPassword;
                    FTempResult:=ComLegDf.Open(s,[eoInMemory,eoIgnoreRelates]);
                    ComLegDf.IndexFilename:=ChangeFileExt(s,'.eix');
                    ComLegDf.CHKFilename:=ChangefileExt(s,'.chk');
                    IF NOT FTempResult THEN ReportError(Format(Translate(20108,'Datafile %s could not be opened'),[s]));
                    IF (FTempResult) AND (ComLegDf.NumRecords=0) THEN
                      BEGIN
                        FTempResult:=False;
                        ReportError(Format(Translate(22334,'Datafile %s does not contain any records'),[s]));
                        Result:=NIL;
                      END;
                    IF (FTempResult) AND (NOT FileExists(ComLegDf.IndexFilename)) THEN
                      BEGIN
                        FTempResult:=(ComLegDf.HasCheckFile) AND (NOT ComLegDf.ErrorInCheckFile);
                        IF FTempResult THEN
                          BEGIN
                            IF NOT ComLegDf.DoRebuildIndex THEN
                              BEGIN
                                FTempResult:=False;
                                ReportError(Format(Translate(20122,'Indexfile not found for the datafile %s'),[s]));
                                Result:=NIL;
                              END;
                          END;
                      END;
                    IF FTempResult THEN
                      BEGIN
                        Labelname:=AnsiLowerCase('Labels from '+ExtractFileName(ComLegDf.RECFilename));
                        tmpValueLabelSet:=df.ValueLabels.ValueLabelSetByName(Labelname);
                        IF (assigned(tmpValueLabelSet)) THEN
                          BEGIN
                            //Labels are already loaded
                            result:=tmpValueLabelSet;
                          END
                        ELSE
                          BEGIN
                            //Applyindex, sort index and read records into PLabelRec
                            AssignFile(F,ComLegdf.IndexFilename);
                            Reset(F);
                            Read(F,s30);
                            CloseFile(F);
                            //Get number of index fields
                            s30:=s30+'@';
                            s2:=Copy(s30,1,Pos('@',s30)-1);
                            IF (Length(s2)>0) AND (IsInteger(s2))
                              THEN ComLegDf.IndexCount:=StrToInt(s2) ELSE FTempResult:=False;
                            IF (ComLegDf.IndexCount<2) OR (NOT FTempResult) THEN
                              BEGIN
                                FTempResult:=False;
                                ReportError(Format(Translate(22832,'Datafile %s must contain two KEY-fields'),[s]));
                                Result:=NIL;
                              END
                            ELSE
                              BEGIN
                                n:=0;
                                REPEAT
                                  INC(n);
                                  Delete(s30,1,Pos('@',s30));
                                  s2:=Copy(s30,1,Pos('@',s30)-1);
                                  IF (Length(s2)=0) OR (NOT IsInteger(s2)) THEN FTempResult:=False;
                                  IF FTempResult THEN ComLegDf.IndexFields[n]:=StrToInt(s2);
                                UNTIL (n=ComLegDf.IndexCount) or (NOT FTempResult);
                                IF FTempResult THEN
                                  BEGIN
                                    ValueField:=ComLegDf[ComLegDf.IndexFields[1]];
                                    TextField:= ComLegDf[ComLegDf.Indexfields[2]];
                                  END
                                ELSE ReportError(Format(Translate(20128,'Error reading index file %s')+#13+Translate(22834,'Rebuild index'),[ComLegDf.RECFilename]));
                              END;
                            IF FTempResult THEN
                              BEGIN
                                FTempResult:=ComLegDf.ApplyIndex;
                                IF NOT FTempResult THEN ReportError(Format(Translate(20128,'Error reading index file %s')+#13+Translate(22834,'Rebuild index'),[ComLegDf.RECFilename]))
                                ELSE
                                  BEGIN
                                    ComLegDf.InitSortIndex;
                                    FOR CurRec:=1 TO ComLegDf.NumRecords DO
                                      BEGIN
                                        ComLegDf.Read(ComLegDf.ReadIndexNoFromSortIndex(CurRec));
                                        result.AddValueLabelPair(Copy(ValueField.AsString,1,30),Copy(TextField.AsString,1,80));
                                      END;  //for CurRec
                                    df.ValueLabels.AddValueLabelSet(result);
                                  END;
                              END
                            ELSE result:=NIL;
                          END;  //if apply index
                      END;  //if indexfile could be opened
                    ComLegDf.Free;
                  EXCEPT
                    ReportError(Format(Translate(22836,'Datafile %s could not be applied as a comment legal.~This could be caused by low memory'),[s]));
                    {$I-}
                    CloseFile(F);
                    n:=IOResult;
                    {$I+}
                    FTempResult:=False;
                    CurCommand:='';
                    ComLegDf.Free;
                    Result:=NIL;
                    Exit;
                  END;  //try..except
                END;  //if Comment Legal Datafilename
            END;  //if not in checkfilemode
        END;   //if Not Comment legal..end and not comment legal use
    END;  //the word LEGAL was found
  CurCommand:='';
END;   //RetrieveCommentLegal


Procedure TCheckObj.RetrieveType;
VAR
  rN,nn:Integer;
  tmpS: string;
BEGIN
  {Handles TYPE COMMENT, TYPE COMMENT fieldname, TYPE STATUSBAR}
  CurCommand:=FParser.GetToken(nwSameLine);  //  NextWord(nwSameLine);
  IF CurCommand='' THEN
    BEGIN
      ReportError(translate(22744,'Illegal syntax in TYPE command'));  //'Illegal syntax in TYPE command'
      FTempResult:=False;
    END
  ELSE
    BEGIN
      IF AnsiUpperCase(CurCommand)='STATUSBAR' THEN
        BEGIN
          tmpField.IsTypeStatusBar:=True;
          CurCommand:=FParser.GetToken(nwSameLine);  //  NextWord(nwSameLine);
          df.TypeStatusBarText:=CurCommand;
          df.TypeStatusBarColor:=2;   //clBlue;
          CurCommand:=FParser.GetToken(nwSameLine); //   NextWord(nwSameLine);
          IF CurCommand<>'' THEN
            BEGIN
              CurCommand:=AnsiUpperCase(CurCommand);
              FOR rn:=0 TO 17 DO
                IF CurCommand=ColorNames[rn] THEN df.TypeStatusBarColor:=rn;
            END;  //if
          df.TypeStatusBarField:=df.FocusedField;
        END
      ELSE IF AnsiUpperCase(CurCommand)='COMMENT' THEN
        BEGIN
          {Syntaxes: TYPE COMMENT
                     TYPE COMMENT colour
                     TYPE COMMENT fieldname
                     TYPE COMMENT ALLFIELDS}
          tmpField.TypeComments:=True;
          tmpField.TypeColor:=2;   //clBlue
          {Next word can be either a fieldname or a colour}
          {if not a fieldname then next word is interpreted as a colour}
          CurCommand:=FParser.GetToken(nwSameLine);  //NextWord(nwSameLine);
          IF AnsiUpperCase(CurCommand)='ALLFIELDS' THEN
            BEGIN
              df.GlobalTypeCom:=True;
              tmpField.TypeCommentField:=-2;
              tmpField.TypeCommentFieldStr:='';
              tmpField.TypeComments:=False;
              CurCommand:=FParser.GetUpperToken(nwSameLine);   //Get the color
              IF CurCommand<>'' THEN
                BEGIN
                  FOR rn:=0 TO 17 DO
                    IF CurCommand=ColorNames[rn] THEN df.GlobalTypeComColor:=rn;
                END;
            END
          ELSE
            BEGIN
              //is type comment colour or fieldname
              rN:=df.FieldNumbers[CurCommand];
              IF rN<>-1 THEN
                BEGIN
                  {IF (PeField(df^.FieldList.Items[rN])^.Fieldtype<>ftAlfa)
                  AND (PeField(df^.FieldList.Items[rN])^.Fieldtype<>ftUpperAlfa) THEN
                    BEGIN
                      ReportError(translate(22838));   //'Can only TYPE COMMENTs to textfields'
                      TempResult:=False;
                    END;}
                  tmpField.TypeCommentField:=rN;
                  tmpField.TypeCommentFieldStr:=CurCommand;
                  tmpField.TypeComments:=False;
                  CurCommand:='';
                END;
              IF CurCommand<>'' THEN
                BEGIN
                  tmpField.Typecolor:=-1;
                  FOR rn:=0 TO 17 DO
                    IF AnsiUppercase(CurCommand)=ColorNames[rn] THEN tmpField.TypeColor:=rn;
    {                IF tmpField^.FTypeColor=-1 THEN
                    BEGIN
                      ReportError(translate(22745));    //'Unknown fieldname or colour'
                      TempResult:=False;
                    END;}
                END;  //if CurCommand<>''
              {Read rest of line - compatibility with Epi Info}
              REPEAT
                CurCommand:=FParser.GetUpperToken(nwSameLine);   //AnsiUpperCase(NextWord(nwSameLine));
                tmpS:=tmpS+CurCommand;
              UNTIL CurCommand='';
            END;
        END  //if Type Comment
      ELSE
        BEGIN
          ReportError(translate(22744,'Illegal syntax in TYPE command'));  //'Illegal syntax in TYPE command'
          FTempResult:=False;
        END;
    END;
  CurCommand:='';
END;   //RetrieveType;


Procedure TCheckObj.RetrieveKeys;
VAR
  Number,n: Integer;
  IsUnique,Found: Boolean;
BEGIN
  {Can be KEY
          KEY UNIQUE
          KEY n
          KEY UNIQUE n}
  IsUnique:=False;
  Number:=0;
  CurCommand:=FParser.GetUpperToken(nwSameLine);   //AnsiUpperCase(NextWord(nwSameLine));
  IF CurCommand='UNIQUE' THEN
    BEGIN
      IsUnique:=True;
      CurCommand:=FParser.GetToken(nwSameLine);   //NextWord(nwSameLine);
      IF CurCommand<>'' THEN
        BEGIN
          IF IsInteger(CurCommand) THEN Number:=StrToInt(CurCommand)
          ELSE
            BEGIN
              ReportError(translate(22747,'Illegal syntax in KEY UNIQUE command'));   //'Illegal syntax in KEY UNIQUE command'
              FTempResult:=False;
            END;
        END;
    END  //if Unique found
  ELSE IF IsInteger(CurCommand) THEN Number:=StrToInt(CurCommand)
  ELSE IF CurCommand<>'' THEN
    BEGIN
      ReportError(translate(22748,'Illegal syntax in KEY command'));  //'Illegal syntax in KEY command'
      FTempResult:=False;
    END;
  IF FTempResult THEN
    BEGIN
      {Test if Key number is already used ny FocusedField}
      IF (Number>0) AND (Number<=MaxIndices)
        THEN IF df.IndexFields[Number]=df.FocusedField THEN
          BEGIN
            tmpField.Index:=Number;
            Exit;
          END;
      {Test if FocusedField occupies a Index-slot}
      IF (Number=0) AND (df.IndexCount=MaxIndices) THEN
        BEGIN
          Found:=False;
          n:=0;
          REPEAT
            INC(n);
            IF df.IndexFields[n]=df.FocusedField THEN Found:=True;
          UNTIL (Found) OR (n=MaxIndices);
          IF Found THEN
            BEGIN
              df.IndexFields[n]:=-1;
              df.IndexCount:=df.IndexCount-1;
            END
          ELSE
            BEGIN
              ReportError(Format(translate(22750,'Only %d KEYs are permitted'),[MaxIndices]));   //'Only %d KEYs are permitted'
              FTempResult:=False;
            END;
        END;
      {Test if Number is within limit}
      IF (Number>MaxIndices) OR (Number<0) THEN
        BEGIN
          ReportError(Format(translate(22752,'Illegal KEY number. Only key numbers from 1 to %d are permitted'),[MaxIndices]));   //'Illegal KEY number. Only key numbers from 1 to %d are permitted'
          FTempResult:=False;
        END;
      IF (Number>=1) AND (Number<=MaxIndices) AND (df.IndexFields[Number]<>-1) THEN
        BEGIN
          ReportError(translate(22754,'KEY number already used'));   //'KEY number already used'
          FTempResult:=False;
        END;
    END;  //if tempResult
  IF FTempResult THEN
    BEGIN
      IF Number=0 THEN
        BEGIN  //Find a slot
          n:=1;
          REPEAT
            IF df.IndexFields[n]=-1 THEN Number:=n;
            INC(n);
          UNTIL (Number<>0) OR (n>MaxIndices);
          IF Number=0 THEN
            BEGIN
              Number:=1;
              df.IndexCount:=df.IndexCount-1;
            END;
        END;
      df.IndexCount:=df.IndexCount+1;
      tmpField.Index:=Number;
      df.IndexFields[Number]:=df.FocusedField;
      df.IndexIsUnique[Number]:=IsUnique;
    END;  //if TempResult
END;   //RetrieveKeys


Procedure TCheckObj.AddFieldFlawComment;
VAR
  s: string;
BEGIN
  IF NOT FCheckFileMode THEN FTempResult:=False;
  IF NOT FMultiLineError THEN FTempResult:=False;
  CommentsAddedToCheckFile:=True;
  s:=FParser.GetLineAndFlush;
  ReportError(translate(22733,'Unknown command'));   //'Unknown command'
(*  {$IFNDEF epidat}
  IF (FirstFieldFlaw) AND (FCheckFileMode) AND (FMultiLineError) THEN
    BEGIN
      FirstFieldFlaw:=False;
      IF eDlg(Format(translate(22820,'Unknown command found in fieldblock in checkfile %s'),    //Unknown command found in fieldblock in checkfile %s
         [ExtractFilename(df^.CHKFilename)])+#13#13+
         translate(22822,'Do you want to save unknown checkcommands found in fieldblocks')+#13+      //'Do you want to save unknown checkcommands found in fieldblocks'
         translate(22824,'as commentlines in the fieldblock?')+#13#13+   //'as commentlines in the fieldblock?'
         translate(22826,'If you choose No then unknown commands in fieldblocks will')+#13+      //'If you choose No then unknown commands in fieldblocks will'
         translate(22828,'be deleted when the revised checks are saved.'),         //'be deleted when the revised checks are saved.'
         mtWarning,[mbYes,mbNo],0)=mrYes THEN SaveFieldFlawsAsComments:=True;
      Screen.Cursor:=crHourGlass;
    END;  //FirstTopFlaw
  {$ENDIF}    *)
  IF SaveFieldFlawsAsComments THEN
    BEGIN
      FParser.CommentCurLine;
      AddFieldComment;
    END;
END;  //procedure AddFieldFlawComment


Procedure TCheckObj.HandleBooleanConditions(VAR s:String);
VAR
  ts,FieldS: String;
  Hn,Hn2: Integer;
  tmpFieldtype: TFieldtypes;
  HtmpDefVar: TeField;
  HFound: Boolean;

BEGIN
  REPEAT
    ts:='';
    IF pos('="Y"',s)>0 THEN ts:='="Y"';
    IF pos('= "Y"',s)>0 THEN ts:='= "Y"';
    IF pos('="N"',s)>0 THEN ts:='="N"';
    IF pos('= "N"',s)>0 THEN ts:='= "N"';
    IF ts<>'' THEN
      BEGIN
        Hn:=pos(ts,s);
        fieldS:='';
        //Get name of field that is assigned to
        Hn2:=Hn;
        REPEAT
          HFound:=False;
          DEC(Hn2);
          IF Hn2>0 THEN
            BEGIN
              IF (s[Hn2] in AlfaNumChars) OR (s[Hn2]=' ') THEN
                BEGIN
                  fieldS:=s[Hn2]+FieldS;
                  HFound:=True;
                END;
            END;
        UNTIL (not HFound) or (Hn2<1);
        HFound:=False;
        FieldS:=Trim(FieldS);
        IF FieldS<>'' THEN
          BEGIN
            //is FieldS a boolean field?
            tmpFieldtype:=ftInteger;
            Hn2:=df.FieldNumbers[FieldS];
            IF Hn2<>-1 THEN tmpFieldtype:=df[Hn2].Fieldtype
            ELSE
              BEGIN
                HtmpDefVar:=df.DefFieldsByName[FieldS];
                IF HtmpDefVar<>NIL THEN tmpFieldtype:=HtmpDefVar.Fieldtype;
              END;
            IF tmpFieldtype=ftBoolean THEN
              BEGIN
                //Found a boolean field that is testet against "Y" or "N"
                Delete(s,Hn,Length(ts));
                IF ts='="Y"'  THEN insert('=True',s,Hn);
                IF ts='= "Y"' THEN insert('= True',s,Hn);
                IF ts='="N"'  THEN insert('=False',s,Hn);
                IF ts='= "N"' THEN insert('= False',s,Hn);
                HFound:=True;
              END;  //if tmpFieldtype=ftBoolean
          END;  //if FieldS<>'
        IF NOT HFound THEN s[Hn]:=#254;
      END;  //if ts<>''
  UNTIL (ts='');
  WHILE Pos(Chr(254),s)>0 DO s[Pos(#254,s)]:='=';
END;  //procedure HandleBooleanConditions


Procedure TCheckObj.AddFieldComment;
VAR
  s: String;
  FieldComments: TStrings;
BEGIN
  s:=FParser.GetLineAndFlush;
  IF FCheckFileMode THEN
    BEGIN
      FieldComments:=TStringList.Create;
      FieldComments.Text:=tmpField.FieldComments;
      FieldComments.Append(FParser.GetWholeLine);
      tmpField.FieldComments:=FieldComments.Text;
      Fieldcomments.Free;
    END;
END;  //Procedure AddFieldComment

{
Function TCheckObj.Translate(stringnumber: Integer; origstring:string):string;
VAR
  s:String;
BEGIN
  s:='';
  IF Assigned(FOnTranslate) THEN
    BEGIN
      FOnTranslate(self,stringnumber, s);
      Result:=s;
    END
  ELSE Result:=origstring;
  if result='' then result:=origstring;
END;
}

//******************************* Misc. functions *************************

Function cFill(c:Char; Len:Integer):String;
VAR
  s:String;
  n:Integer;
BEGIN
  s:='';
  FOR n:=1 TO Len DO s:=s+c;
  Result:=s;
END;   //function Fill



//********************************* TCheckWriter **************************************

constructor TCheckWriter.create(EpiDataFile: TEpiDataFile);
begin
  inherited create;
  if EpiDataFile=NIL then raise Exception.create('No EpiData file');
  df:=EpiDataFile;
  FInitBlocks:=TStringList.Create;
  FLastFieldBlock:=TStringList.Create;
  FieldComments:=TStringList.Create;
  self.Rewrite;
end;

destructor TCheckWriter.destroy;
begin
  FInitBlocks.Free;
  FLastFieldBlock.Free;
  FieldComments.Free;
  inherited destroy;
end;

procedure TCheckWriter.Rewrite;
begin
  FInitBlocks.Clear;
  FLastFieldBlock.Clear;
  ChecksToStrings;
end;

procedure TCheckWriter.SaveToFile(filename: String);
begin
  if FInitBlocks.Count=0 then exit;
  try
    FInitBlocks.SaveToFile(filename);
  except
    raise Exception.Create('Error saving check file');
  end;
end;

procedure TCheckWriter.ChecksToStrings;
VAR
  sN,sN2:    Integer;
  tmpS:      String;
  LegalList: TStringList;
  AField:    TeField;
  sList:     TStringList;
  aValueLabelSet: TValueLabelSet;

  procedure LabelsInCommands(cmdList: TChkCommands);
  VAR
    n,w:Integer;
    Cmd:PCmds;
  BEGIN
    IF CmdList=NIL THEN Exit;
    IF CmdList.Count=0 THEN Exit;
    FOR n:=0 TO cmdList.Count-1 DO
      BEGIN
        Cmd:=CmdList.Items[n];
        Case cmd^.Command OF
          cmdIF:
            BEGIN
              IF cmd^.IfCmds<>NIL THEN LabelsInCommands(TChkCommands(cmd^.IfCmds));
              IF cmd^.ElseCmds<>NIL THEN LabelsInCommands(TChkCommands(cmd^.ElseCmds));
            END;
          cmdComLegal:
            BEGIN
              tmpS:=AnsiLowerCase(cmd^.ValueLabel.Name);
              aValueLabelSet:=df.ValueLabels.ValueLabelSetByName(tmpS);
              IF (aValueLabelSet<>NIL) AND (LegalList.IndexOf(tmpS)=-1)
              AND (cmd^.ValueLabelType=vltLabelRef)
              THEN LegalList.AddObject(tmpS,aValueLabelSet);
            END;
        END;  //case
      END;  //for
  END;  //procedure LabelsInCommands

BEGIN  //ChecksToStrings
  LegalList:=TStringList.Create;
  sList:=FInitBlocks;
  sList.Clear;
  try
    IF Assigned(df.ChkTopComments)
    THEN IF df.ChkTopComments.Count>0 THEN
      BEGIN
        sList.AddStrings(df.ChkTopComments);
        sList.Append('');
      END;
    {Write LabelBlock}

    {
     sList.append('Liste over elementer i labelblock:');
    for sN:=0 to df.ValueLabels.count-1 do
      begin
        sList.append('--'+df.ValueLabels.items[sN].Name+'--');
      end;
    sList.append('');

    sList.append('Liste over felter med valuelabels:');
    for sN:=0 TO df.NumFields-1 do
      begin
        aField:=df.Fields[sN];
        if (aField.Valuelabel<>NIL) then sList.append(aField.FieldName+' --'+aField.Valuelabel.Name+'--');
      end;
    sList.append('');
    }

    FOR sN:=0 TO df.NumFields-1 do
      BEGIN
        AField:=df.Fields[sN];
        if aField.Valuelabel<>NIL then
          begin
            tmpS:=AnsiLowerCase(AField.ValueLabel.Name);
            aValueLabelSet:=df.ValueLabels.ValueLabelSetByName(tmpS);
            IF (aValueLabelSet<>NIL) AND (LegalList.IndexOf(tmpS)=-1)
            AND (AField.ValueLabelType=vltLabelRef)
            THEN LegalList.AddObject(tmpS,aValueLabelSet);
          end;
        {Check if fields has commands that contains comment legals}
        IF AField.AfterCmds<>NIL THEN LabelsInCommands(TChkCommands(AField.AfterCmds));
        IF AField.BeforeCmds<>NIL THEN LabelsInCommands(TChkCommands(AField.BeforeCmds));
      END;  //for sN
    {Legallist now contains all used value labels}
    IF LegalList.Count>0 THEN
      BEGIN
        sList.Append('LABELBLOCK');
        FOR sN:=0 TO LegalList.Count-1 DO sList.Append(Label2Text(TValueLabelSet(LegalList.Objects[sN]),2));
        sList.Append('END');  //of labelblock
        sList.Append('');
      END;  //if LegalList.Count>0
    LegalList.Clear;
    {Write assertblock}
    IF Assigned(df.AssertList) THEN
      BEGIN
        sList.Append('CONSISTENCYBLOCK');
        sList.AddStrings(df.AssertList);
        sList.Append('END');
        sList.Append('');
      END;
    {Write recodeblock}
    IF df.RecodeCmds<>NIL THEN
      BEGIN
        sList.Append('RECODEBLOCK');
        AddCommandList(sList,TChkCommands(df.RecodeCmds),2);
        sList.Append('END');
        sList.Append('');
      END;
    {Write before/after file and before/after record}
    IF (df.BeforeFileCmds<>NIL) OR (df.GlobalMissingValues[0]<>'') THEN
      BEGIN
        sList.Append('BEFORE FILE');
        tmpS:='';
        IF df.GlobalMissingValues[0]<>'' THEN tmpS:=df.globalMissingValues[0];
        IF df.GlobalMissingValues[1]<>'' THEN tmpS:=tmpS+' '+df.GlobalMissingValues[1];
        IF df.GlobalMissingValues[2]<>'' THEN tmpS:=tmpS+' '+df.GlobalMissingValues[2];
        IF tmpS<>'' THEN sList.Append('  MISSINGVALUE ALL '+tmpS);
        AddCommandList(sList,TChkCommands(df.BeforeFileCmds),2);
        sList.Append('END');
        sList.Append('');
      END;
    IF df.AfterFileCmds<>NIL THEN
      BEGIN
        sList.Append('AFTER FILE');
        AddCommandList(sList,TChkCommands(df.AfterFileCmds),2);
        sList.Append('END');
        sList.Append('');
      END;
    IF df.BeforeRecordCmds<>NIL THEN
      BEGIN
        sList.Append('BEFORE RECORD');
        AddCommandList(sList,TChkCommands(df.BeforeRecordCmds),2);
        sList.Append('END');
        sList.Append('');
      END;
    IF df.AfterRecordCmds<>NIL THEN
      BEGIN
        sList.Append('AFTER RECORD');
        AddCommandList(sList,TChkCommands(df.AfterRecordCmds),2);
        sList.Append('END');
        sList.Append('');
      END;

    {Write field blocks}
    FOR sN:=0 TO df.NumFields-1 do
      begin
        FieldBlockToStrings(sN,0);
        sList.AddStrings(FLastFieldBlock);
      end;
  finally
    LegalList.Free;
  end;
end;

Procedure TCheckWriter.FieldBlockToStrings(FieldNo:Integer; Indent:Byte);
VAR
  LegalList: TStrings;
  tmpAutoList: TStringList;
  tmpS,IndStr: String;
  sN2,sN3:Integer;
  AField,AField2: TeField;
  sList: TStringList;
BEGIN
  FLastFieldBlock.Clear;
  sList:=FLastFieldBlock;
  IndStr:=cFill(' ',Indent);
  LegalList:=TStringList.Create;
  try
    AField:=df.Fields[FieldNo];
    WITH AField DO
    BEGIN
      Min:=Trim(Min);
      Max:=Trim(Max);
      IF HasCheckProperties THEN
        BEGIN
          sList.Add(IndStr+trim(FieldName));
          {Write fieldblock comments}
          IF FieldComments<>'' THEN
          BEGIN
            //FieldComments.Text:=FFieldComments;  //TODO
            //sList.AddStrings(FieldComments);
          END;

          {Write index key}
          IF Index>0 THEN
            BEGIN
              tmpS:=IndStr+'  KEY ';
              IF df.IndexIsUnique[Index] THEN tmpS:=tmpS+'UNIQUE ';
              tmpS:=tmpS+IntToStr(Index);
              sList.Add(tmpS);
            END;
          {Write autosearch}
          IF Autosearch THEN
            BEGIN
              tmpS:=IndStr+'  Autosearch ';
              IF AutoList THEN tmpS:=tmpS+' LIST ';
              TRY
                tmpAutoList:=TStringList.Create;
                tmpAutoList.CommaText:=AutoFields;
                FOR sN2:=0 TO tmpAutoList.count-1 DO
                  BEGIN
                    AField2:=df.Fields[StrToInt(tmpAutoList[sN2])];
                    tmpS:=tmpS+trim(AField2.FieldName)+' ';
                  END;
              FINALLY
                tmpAutoList.Free;
              END;
              sList.Add(tmpS);
            END;

          {Write NoEnter}
          IF NoEnter THEN sList.Add(IndStr+'  NOENTER');
          {Write TopOfScreen}
          IF TopOfScreen THEN
            BEGIN
              tmpS:=IndStr+'  TOPOFSCREEN';
              IF TopOfScreenLines>0 THEN tmpS:=tmpS+' '+IntToStr(TopOfScreenLines);
              sList.Add(IndStr+tmpS);
            END;
          {Write RANGE}
          tmpS:='';
          IF Min<>'' THEN tmpS:=Min+' '
          ELSE tmpS:='-INFINITY ';
          IF Max<>'' THEN tmpS:=tmpS+Max
          ELSE tmpS:=tmpS+'INFINITY';
          IF (Min<>'') OR (Max<>'')
          THEN sList.Add(IndStr+'  RANGE '+tmpS);
          {Write LEGAL block}
          IF Legal<>'' THEN
            BEGIN
              IF RangeDefined THEN sN3:=1 ELSE sN3:=0;
              LegalList.CommaText:=Legal;
              IF LegalList.Count>sN3 THEN
                BEGIN
                  sList.Add(IndStr+'  LEGAL');
                  LegalList.CommaText:=Legal;
                  FOR sN2:=sN3 TO LegalList.Count-1 DO
                    IF Pos(' ',LegalList[sN2])>0
                    THEN sList.Add(IndStr+'    "'+LegalList[sN2]+'"')
                    ELSE sList.Add(IndStr+'    '+LegalList[sN2]);
                  sList.Add(IndStr+'  END');
                END;
            END;
          {Write Comment Legal}
          IF ValueLabel<>NIL THEN
            BEGIN
              case ValueLabelType of
                vltLocal:
                  begin
                    LegalList.Clear;
                    LegalList.Text:=Label2Text(ValueLabel,Indent+2);
                    LegalList[0]:=IndStr+'  COMMENT LEGAL';
                    if ShowLegalPickList then LegalList[0]:=LegalList[0]+' SHOW';
                    sList.AddStrings(LegalList);
                  end;
                vltFieldRef:
                  begin
                    tmpS:=ValueLabelUse;
                    if ShowLegalPickList then tmpS:=tmpS+' SHOW';
                    sList.Add(Indstr+'  COMMENT LEGAL USE '+tmpS);
                  end;
                vltLabelRef:
                  begin
                    tmpS:=ValueLabelUse;
                    if ShowLegalPickList then tmpS:=tmpS+' SHOW';
                    sList.Add(Indstr+'  COMMENT LEGAL USE '+tmpS);
                  end;
                vltFile:
                  begin
                    tmpS:=ValueLabelUse;
                    if ShowLegalPickList then tmpS:=tmpS+' SHOW';
                    sList.Add(Indstr+'  COMMENT LEGAL '+tmpS);
                  end;
              end;  //case
            END;  //if valuelabel<>NIL

          {Write JUMPS block}
          IF jumps<>'' THEN
            BEGIN
              LegalList.CommaText:=jumps;
              IF LegalList[0]='AUTOJUMP'
              THEN sList.Add(IndStr+'  AUTOJUMP '+trim(LegalList[1]))
              ELSE
                BEGIN
                  tmpS:=IndStr+'  JUMPS';
                  IF JumpResetChar<>#0 THEN tmpS:=IndStr+'  JUMPS RESET';
                  IF (JumpResetChar<>#0) AND (JumpResetChar<>#32)
                  THEN tmpS:=tmpS+' "'+JumpResetChar+'"';
                  sList.Add(tmpS);
                  FOR sN2:=0 TO LegalList.Count-1 DO
                    BEGIN
                      tmpS:=LegalList[sN2];
                      tmpS[Pos('>',tmpS)]:=' ';
                      sList.Add(IndStr+'    '+tmpS);
                    END;
                  sList.Add(IndStr+'  END');
                END;
            END;
          {Write MUSTENTER, REPEAT}
          IF MustEnter THEN sList.Add(IndStr+'  MUSTENTER');
          IF doRepeat THEN sList.Add(IndStr+'  REPEAT');
          IF Confirm THEN sList.Add(IndStr+'  CONFIRMFIELD');

          {Write Missingvalues}
          tmpS:='';
          IF MissingValues[0]<>'' THEN tmpS:=MissingValues[0];
          IF MissingValues[1]<>'' THEN tmpS:=tmpS+' '+MissingValues[1];
          IF MissingValues[2]<>'' THEN tmpS:=tmpS+' '+MissingValues[2];
          IF tmpS<>'' THEN sList.Add(IndStr+'  MISSINGVALUE '+tmpS);

          {Write TYPE STATUSBAR}
          IF IsTypeStatusBar THEN
            BEGIN
              tmpS:=IndStr+'  TYPE STATUSBAR';
              IF df.TypeStatusBarText<>'' THEN tmpS:=tmpS+' "'+df.TypeStatusBarText+'"';
              IF df.TypeStatusBarColor<>2 THEN tmpS:=tmpS+' '+ColorNames[df.TypeStatusbarcolor];
              sList.Add(tmpS);
            END;

          {Write TYPE COMMENT}
          tmpS:=IndStr+'  TYPE COMMENT ';
          IF TypeComments THEN
            BEGIN
              IF TypeColor<>2 THEN tmpS:=tmpS+ColorNames[TypeColor];
              sList.Add(tmpS);
            END
          ELSE IF (df.GlobalTypeCom) AND (TypeCommentField=-2) THEN
            BEGIN
              tmpS:=tmpS + 'ALLFIELDS';
              IF df.GlobalTypeComColor<>0 THEN tmpS:=tmpS+' '+ColorNames[df.globalTypeComColor];
              sList.Add(tmpS);
            END
          ELSE IF (TypeCommentField<>-1) or (TypeCommentFieldStr<>'') THEN
            BEGIN
              if (TypeCommentField=-1) and (TypeCommentFieldStr<>'') then TypeCommentField:=df.FieldNumbers[TypeCommentFieldStr];
              tmpS:=tmpS+trim(df.Fields[TypeCommentField].FieldName);
              sList.Add(tmpS);
            END;

          {Write Before Entry commands}
          IF BeforeCmds<>NIL THEN
            BEGIN
              sList.Add(IndStr+'  BEFORE ENTRY');
              AddCommandList(sList,TChkCommands(BeforeCmds),Indent+4);
              sList.Add(IndStr+'  END');
            END;  //if Before commands

          {Write After Entry commands}
          IF AfterCmds<>NIL THEN
            BEGIN
              sList.Add(IndStr+'  AFTER ENTRY');
              AddCommandList(sList,TChkCommands(AfterCmds),Indent+4);
              sList.Add(IndStr+'  END');
            END;  //if After commands

          {End fieldblock}
          sList.Add(IndStr+'END');
          sList.Add('');
        END;  //if field has checks attached
    END;  //With
  finally
    LegalList.Free;
  end
END;  //procedure FieldBlockToStrings


Function TCheckWriter.Label2Text(aValueLabelSet:TValueLabelSet; NumSpc:Byte):String;
VAR
  s,aValue,aLabel:String;
  spc:String[100];
  n:integer;
BEGIN
  result:='';
  if (not assigned(aValueLabelSet)) then exit;
  spc:=cFill(' ',NumSpc);
  s:=spc+'LABEL '+aValueLabelSet.Name;
  IF s[Length(s)]='¤' THEN s:=Copy(s,1,Length(s)-1);
  for n:=0 to aValueLabelSet.count-1 do
    BEGIN
      aValue:=aValueLabelSet.Values[n];
      aLabel:=aValueLabelSet.Labels[n];
      IF aValue[1]='*' THEN
        BEGIN
          s:=s+#13#10+spc+'  '+aValue+aLabel;
        END
      ELSE
        BEGIN
          IF Pos(' ',aValue)>0
            THEN s:=s+#13#10+spc+'  "'+aValue+'"'
            ELSE s:=s+#13#10+spc+'  '+aValue;
          IF Pos(' ',aLabel)>0
            THEN s:=s+'  "'+aLabel+'"'
            ELSE s:=s+'  '+aLabel;
        END;
    END;  //for
  s:=s+#13#10+spc+'END';
  Result:=s;
END;  //Label2Text


Procedure TCheckWriter.AddCommandList(sList:TStringList; CmdList:TChkCommands; Indent:Byte);
VAR
  CmdCounter,n:Integer;
  Cmd:PCmds;
  tmpStr:String[250];
  IndStr:String[50];
  LabelList: TStrings;
  s,tmpFieldStr:String;
  aValueLabelSet: TValueLabelSet;
BEGIN
  IF CmdList=NIL THEN Exit;
  IF CmdList.Count=0 THEN Exit;
  IndStr:=cFill(' ',Indent);
  FOR CmdCounter:=0 TO CmdList.Count-1 DO
    BEGIN
      Cmd:=PCmds(CmdList.Items[CmdCounter]);
      CASE Cmd^.Command OF
        cmdIF:
          BEGIN
            s:=cmd^.IfShowExpr;
//            Single2DoubleQuotes(s);
{            REPEAT
              n:=Pos('_M',s);
              IF n>0 THEN
                BEGIN
                  s[n]:=' ';
                  s[n+1]:='.';
                END;
            UNTIL n=0;}
            tmpStr:=s;
{            IF (tmpStr[1]='(') AND (tmpStr[Length(tmpStr)]=')') THEN
              BEGIN
                tmpStr[1]:=' ';
                tmpStr[Length(tmpStr)]:=' ';
              END;}
            sList.Append(IndStr+'IF '+trim(tmpStr)+' THEN');
            AddCommandList(sList,cmd^.IfCmds,Indent+2);
            IF cmd^.ElseCmds<>NIL THEN
              BEGIN
                sList.Append(IndStr+'ELSE');
                AddCommandList(sList,cmd^.ElseCmds,Indent+2);
              END;
            sList.Append(IndStr+'ENDIF');
          END;  //case cmdIF
        cmdHelp:
          BEGIN
            tmpStr:='"'+cmd^.HelpString+'"';
            REPEAT
              n:=pos(#13,tmpStr);
              IF n>0 THEN
                BEGIN
                  tmpStr[n]:='n';
                  tmpStr[n-1]:='\';
                END;
            UNTIL n=0;
            IF trim(cmd^.HelpKeys)<>''
            THEN tmpStr:=tmpStr+' KEYS="'+trim(AnsiUpperCase(cmd^.HelpKeys))+'"';
            CASE cmd^.HelpType OF
              mtError: tmpStr:=tmpStr+' TYPE=ERROR';
              mtWarning: tmpStr:=tmpStr+' TYPE=WARNING';
              mtConfirmation: tmpStr:=tmpStr+' TYPE=CONFIRMATION';
            END;
            sList.Append(IndStr+'HELP '+tmpStr);
          END;  //case cmdHelp
        cmdWriteNote:
          BEGIN
            tmpStr:='"'+cmd^.FNote+'"';
            REPEAT
              n:=pos(#13,tmpStr);
              IF n>0 THEN
                BEGIN
                  tmpStr[n]:='n';
                  tmpStr[n-1]:='\';
                END;
            UNTIL n=0;
            IF cmd^.ShowNotes THEN tmpStr:=tmpStr+' SHOW';
            sList.Append(IndStr+'WRITENOTE '+tmpStr);
          END;  //case cmdWriteNote
        cmdCopyToClipboard:
          BEGIN
            sList.Append(IndStr+'COPYTOCLIPBOARD "'+cmd^.CopyStr+'"');
          END;
        cmdHide:
          BEGIN
            tmpStr:=IndStr+'HIDE';
            tmpStr:=tmpStr+' '+cmd^.HideVarName;
            sList.Append(tmpStr);
          END;  //case cmdHide
        cmdUnHide:
          BEGIN
            tmpStr:=IndStr+'UNHIDE';
            tmpStr:=tmpStr+' '+cmd^.HideVarName;
            sList.Append(tmpStr);
          END;  //case cmdUnHide
        cmdClear:
          BEGIN
            tmpStr:=IndStr+'CLEAR';
            IF cmd^.HideVarName='$$COMLEG' THEN tmpStr:=tmpStr+' COMMENT LEGAL'
            ELSE tmpStr:=tmpStr+' '+cmd^.HideVarName;
            sList.Append(tmpStr);
          END;  //case cmdClear
        cmdGoto: sList.Append(IndStr+'GOTO '+cmd^.HideVarName);
        cmdExit: sList.Append(IndStr+'EXIT');
        cmdQuit: sList.Append(IndStr+'QUIT');    //###
        cmdTypeString:
          BEGIN
            IF cmd^.TypeText='¤¤typecommentlegalallfields¤¤' THEN
              BEGIN
                tmpStr:=IndStr+'TYPE COMMENT ALLFIELDS';
                IF df.GlobalTypeComColor<>0 THEN tmpStr:=tmpStr+' '+ColorNames[df.globalTypeComColor];
              END
            ELSE
              BEGIN
                tmpStr:=IndStr+'TYPE "'+cmd^.TypeText+'"';
                IF cmd^.TypeColor<>2 THEN tmpStr:=tmpStr+' '+ColorNames[cmd^.TypeColor];
              END;
            sList.Append(tmpStr);
          END;
        cmdBackup:
          BEGIN
            sList.Append(IndStr+'BACKUP '+cmd^.DestLib);
          END;
        cmdLoad:
          BEGIN
            tmpStr:=cmd^.DLLName;
            IF pos(' ',tmpStr)>0 THEN tmpStr:='"'+tmpStr+'"';
            sList.Append(IndStr+'LOAD '+tmpStr);
          END;
        cmdExecute:
          BEGIN
            IF pos(' ',cmd^.ExecCmdLine)>0
            THEN tmpStr:='EXECUTE '+'"'+cmd^.ExecCmdLine+'"'
            ELSE tmpStr:='EXECUTE '+cmd^.ExecCmdLine;
            IF cmd^.ExecParams<>'' THEN
              BEGIN
                IF pos(' ',cmd^.ExecParams)>0
                THEN tmpStr:=tmpStr+' "'+cmd^.ExecParams+'"'
                ELSE tmpStr:=tmpStr+' '+cmd^.ExecParams;
              END;
            IF cmd^.ExecWait THEN tmpStr:=tmpStr+' WAIT' ELSE tmpStr:=tmpStr+' NOWAIT';
            IF cmd^.ExecHide THEN tmpStr:=tmpStr+' HIDE';
            sList.Append(IndStr+tmpStr);
          END;
        cmdBeep:
          BEGIN
            tmpStr:=IndStr+'BEEP';
            IF cmd^.Beeptype=btWarning THEN tmpStr:=tmpStr+' Warning';
            IF cmd^.BeepType=btConfirmation THEN tmpStr:=tmpStr+' Confirmation';
            sList.Append(tmpStr);
          END;
        cmdRelate:
          BEGIN
            tmpStr:=IndStr+'RELATE '+trim(cmd^.RelField)+' ';
            IF Pos(' ',cmd^.RelFileStr)>0 THEN tmpStr:=tmpStr+'"'+cmd^.RelFileStr+'"'
            ELSE tmpStr:=tmpStr+cmd^.RelFileStr;
            IF cmd^.One2One THEN tmpStr:=tmpStr+' 1';
            sList.Append(tmpStr);
          END;
        cmdComLegal:
          BEGIN
            IF cmd^.ValueLabel<>NIL THEN
              BEGIN
                case cmd^.ValueLabelType of
                  vltLocal:
                    begin
                      LabelList:=TStringList.Create;
                      try
                        LabelList.Text:=Label2Text(cmd^.ValueLabel,Indent+2);
                        LabelList[0]:=IndStr+'  COMMENT LEGAL';
                        if cmd^.ShowList then LabelList[0]:=LabelList[0]+' SHOW';
                        sList.AddStrings(LabelList);
                      finally
                        LabelList.Free;
                      end;
                    end;
                  vltFieldRef:
                    begin
                      tmpStr:=cmd^.ValueLabelUse;
                      if cmd^.ShowList then tmpStr:=tmpStr+' SHOW';
                      sList.Add(Indstr+'  COMMENT LEGAL USE '+tmpStr);
                    end;
                  vltLabelRef:
                    begin
                      tmpStr:=cmd^.ValueLabelUse;
                      if cmd^.ShowList then tmpStr:=tmpStr+' SHOW';
                      sList.Add(Indstr+'  COMMENT LEGAL USE '+tmpStr);
                    end;
                  vltFile:
                    begin
                      tmpStr:=cmd^.ValueLabelUse;
                      if cmd^.ShowList then tmpStr:=tmpStr+' SHOW';
                      sList.Add(Indstr+'  COMMENT LEGAL '+tmpStr);
                    end;
                end;  //case
              END;  //if valuelabel<>NIL

{            IF Cmd^.ValueLabelName<>'' THEN
              BEGIN
                tmpStr:=AnsiLowerCase(trim(Cmd^.ValueLabelName));
                IF tmpStr[Length(tmpStr)]='$' THEN
                  BEGIN  //write comment legal..end block
                    aValueLabelSet:=df.ValueLabels.ValueLabelSetByName(tmpStr);
                    IF aValueLabelSet<>NIL THEN
                      BEGIN
                        LabelList:=TStringList.Create;
                        LabelList.Text:=Label2Text(aValueLabelSet,Indent);
                        LabelList[0]:=IndStr+'COMMENT LEGAL';
                        IF Cmd^.ShowList THEN LabelList[0]:=LabelList[0]+' SHOW';
                        sList.Addstrings(LabelList);
                      END;
                  END
                ELSE
                  BEGIN  //write Comment Legal Use ...
                    IF Copy(tmpStr,1,12)='labels from ' THEN
                      BEGIN
                        Delete(tmpStr,1,12);
                        IF Cmd^.ShowList THEN tmpStr:=tmpStr+' SHOW';
                        sList.Add('  COMMENT LEGAL '+tmpStr);
                      END
                    ELSE
                      BEGIN
                        IF tmpStr[Length(tmpStr)]='¤' THEN tmpStr:=Copy(tmpStr,1,Length(tmpStr)-1);
                        IF df.ValueLabels.ValueLabelSetByName(cmd^.ValueLabelName)<>NIL THEN
                          BEGIN
                            IF Cmd^.ShowList THEN tmpStr:=tmpStr+' SHOW';
                            sList.Add(IndStr+'COMMENT LEGAL USE '+trim(tmpStr));
                          END;
                      END;
                  END;
              END;
              }
          END;  //case cmdComLegal
        cmdLet:
          BEGIN
            IF cmd^.CodedWithLet THEN tmpStr:='LET ' ELSE tmpStr:='';
            s:=trim(cmd^.LetExpr);
            Single2DoubleQuotes(s);
            REPEAT
              n:=Pos('_M',s);
              IF n>0 THEN
                BEGIN
                  s[n]:=' ';
                  s[n+1]:='.';
                END;
            UNTIL n=0;
            tmpStr:=tmpStr+cmd^.VarName+'='+s;
            sList.Append(IndStr+tmpStr);
          END;  //case cmdLet
        cmdDefine:
          BEGIN
            CASE cmd^.Felttype OF
              ftInteger: tmpFieldStr:=cFill('#',cmd^.FLength);
              ftAlfa: tmpFieldStr:=cFill('_',cmd^.FLength);
              ftDate: tmpFieldStr:='<MM/DD/YYYY>';
              ftYMDDate: tmpFieldStr:='<YYYY/MM/DD>';          //&&
              ftUpperAlfa: tmpFieldStr:='<A'+cFill('a',cmd^.FLength-1)+'>';
              ftSoundex: tmpFieldStr:='<S'+cFill('s',cmd^.FLength-1)+'>';
              ftBoolean: tmpFieldStr:='<Y>';
              ftFloat: BEGIN
                  tmpFieldStr:=cFill('#',cmd^.FLength-1-cmd^.FNumDecimals);
                  IF cmd^.FNumDecimals=0 THEN tmpFieldStr:=tmpFieldStr+'#'
                  ELSE tmpFieldStr:=tmpFieldStr+'.'+cFill('#',cmd^.FNumDecimals);
                END;   //Case Fieldtype of ftFloat
              ftEuroDate: tmpFieldStr:='<DD/MM/YYYY>';
            END;  //Case Fieldtype
            tmpStr:='DEFINE '+cmd^.FName+' '+tmpFieldStr;
            IF cmd^.FScope=scGlobal THEN tmpStr:=tmpStr+' GLOBAL';
            IF cmd^.FScope=scCumulative THEN tmpStr:=tmpStr+' CUMULATIVE';
            sList.Append(IndStr+tmpStr);
          END;  //case cmdDefine
        cmdAutoSave: sList.Append(IndStr+'AUTOSAVE');
        cmdConfirm:  sList.Append(IndStr+'CONFIRM');
        cmdIgnoreMissing: sList.Append(IndStr+'IGNOREMISSING');
        //cmdBgColour: sList.Append(IndStr+'BACKGROUNDCOLOUR '+ColorNames[cmd^.BgColour]);
        cmdColor:
          BEGIN
            tmpStr:=IndStr+'COLOR ';
            CASE cmd^.ColorCmd OF
              1: tmpStr:=tmpStr+'QUESTION ';
              2: tmpStr:=tmpStr+'DATA ';
              3: tmpStr:=tmpStr+'BACKGROUND ';
            END;  //case
            IF cmd^.IsEpiInfoNo THEN
              BEGIN
                IF cmd^.ColorCmd=3 THEN tmpStr:=tmpStr+IntToStr(cmd^.BgColor)
                ELSE
                  BEGIN
                    n:=(cmd^.BgColor SHL 4);
                    n:=n AND cmd^.TxtColor;
                    tmpStr:=tmpStr+IntToStr(n);
                  END;
              END
            ELSE
              BEGIN
                IF cmd^.ColorCmd=3 THEN tmpStr:=tmpStr+ColorNames[cmd^.BgColor]
                ELSE
                  BEGIN
                    tmpStr:=tmpStr+ColorNames[cmd^.txtcolor];
                    IF cmd^.BgColor<>255 THEN tmpStr:=tmpStr+' '+ColorNames[cmd^.bgcolor];
                  END;
              END;
            sList.Append(tmpstr);
          END;
        cmdComment: sList.Append(IndStr+cmd^.Comment);
        cmdLeaveField:
          BEGIN
            tmpStr:='cmdLeaveField -';
            IF cmd^.IsLastField THEN tmpStr:=tmpStr+'LastField';
            sList.Append(tmpStr);
          END;
      END;  //Case
    END;  //for
END;  //Procedure AddCommandList


{TChkCommands}
constructor TChkCommands.create;
begin
  inherited;
  FList:=TList.Create;
end;

destructor TChkCommands.destroy;
var
  n: integer;
  tmpCmdRec: TCmds;
begin
  DisposeCommandList(FList);
  inherited;
end;

Procedure TChkCommands.DisposeCommandList(aList: TList);
VAR
  n:Integer;
  tmpCmdRec:PCmds;
BEGIN
  FOR n:=0 TO AList.Count-1 DO
    BEGIN
      tmpCmdRec:=PCmds(AList.Items[n]);
      CASE tmpCmdRec^.Command OF
        cmdIF:
          BEGIN
            IF tmpCmdRec^.IfCmds<>NIL THEN DisposeCommandList(tmpCmdRec^.IfCmds.List);
            IF tmpCmdRec^.ElseCmds<>NIL THEN DisposeCommandList(tmpCmdRec^.ElseCmds.List);
          END;
      END;  //case
      Dispose(PCmds(Alist.Items[n]));
    END;  //for
  FreeAndNil(Alist);
END;  //procedure DisposeCommandList

procedure TChkCommands.AddCommand(cmd: PCmds);
begin
  FList.Add(cmd);
end;

function TChkCommands.GetCount:integer;
begin
  result:=FList.count;
end;

function TChkCommands.GetItem(index:integer):PCmds;
begin
  if (index>=0) and (index<FList.Count)
  then result:=PCmds(FList.Items[index])
  else result:=NIL;
end;

procedure TChkCommands.Clone(dest: TChkCommands);
var
  n: integer;
  aCmd,new: PCmds;
begin
  if (not assigned(dest)) then dest:=TChkCommands.create;
  for n:=0 TO FList.Count-1 do
    begin
      aCmd:=PCmds(FList.Items[n]);
      new:=NewCmd;
      new^:=aCmd^;
      new^.IfCmds:=NIL;
      new^.ElseCmds:=NIL;
      if aCmd^.IfCmds<>NIL then
        begin
          new^.IfCmds:=TChkCommands.create;
          aCmd^.IfCmds.Clone(new^.IfCmds);
        end;
      if aCmd^.ElseCmds<>NIL then
        begin
          new^.ElseCmds:=TChkCommands.create;
          aCmd^.ElseCmds.Clone(new^.ElseCmds);
        end;
      dest.AddCommand(new);
    end;
end;

function TChkCommands.NewCmd:PCmds;
begin
  new(result);
  result^.IfCmds:=NIL;
  result^.ElseCmds:=NIL;
end;


end.
