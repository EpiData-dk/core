unit UCheckFileIO;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, UCheckFileCmds, UDataFileTypes, UEpiDataFile, UValueLabels,
  UDebug;

type

  TNwType = (nwAny, nwSameLine, nwSameKeepQuotes, nwKeepSpaces);
  TCheckParser = class(TObject)
  private
    FLines:       TStringList;
    FCurLin:      String;
    FCurLinIndex: Integer;
    FEndOfLines:  Boolean;
    function      FGetCurLinIndex:Integer;
  public
    Constructor   Create();
    Destructor    Destroy;  override;
    Function      GetToken(nwType: TNwType):string;
    Function      GetUpperToken(nwType: TNwType):String;
    Function      GetLowerToken(nwType: TNwType):string;
    Function      GetLineAndFlush:string;
    Function      GetWholeLine:String;
    Procedure     CommentCurLine;
    Procedure     LoadLines(ChkLines: TStrings);
    property      EndOfLines: Boolean read FEndOfLines write FEndOfLines;
    property      GetCurLinIndex: Integer read FGetCurLinIndex write FCurLinIndex;
  end;

  TCheckFileIO = class(TObject)
  private
    FDf:          TEpiDatafile;
    FCheckLines:  TStrings;
    FOnTranslate: TTranslateEvent;
    FErrorLines:  TStrings;
    FParser:      TCheckParser;
    FIndentLvl:   Integer;
    function      InternalRead(): boolean;
    function      InternalWrite(): boolean;
    function      PreParse(): boolean;
    function      Lang(Const aCode: cardinal; const Msg: string): string;
    function      ReportError(const ErrStr: String): boolean;
    function      RetrieveFieldBlock(CurField: TEpiField): boolean;
    function      RetrieveRange(CurField: TEpiField): boolean;
    function      RetrieveLegals(CurField: TEpiField): boolean;
    function      RetrieveMissingValues(CurField: TEpiField): boolean;
    function      RetrieveDefaultValue(CurField: TEpiField): boolean;
    function      RetrieveAutosearch(CurField: TEpiField): boolean;
    function      RetrieveAutoJump(CurField: TEpiField): boolean;
    function      RetrieveJumps(CurField: TEpiField): boolean;
    function      RetrieveCommentLegal(CurField: TEpiField): boolean;
    function      RetrieveType(CurField: TEpiField): Boolean;
    function      RetrieveKeys(CurField: TEpiField): Boolean;
    function      GetCommandList(CmdList: TChkCommands; CurField: TEpiField): Boolean;
    function      IsPosibleLetCmd(CurCommand: string; TmpCmdPtr: PCmd): Boolean;
    function      GetCommand(CurCommand: String; CmdList: TChkCommands; CurField: TEpiField): Boolean;
    function      AddFieldComment(CurField: TEpiField): boolean;
    function      RetrieveLabelBlock(): Boolean;
    function      RetrieveLabel(): Boolean;
    function      RetrieveAssertBlock(): boolean;
    function      AddTopComment(): Boolean;
    function      RetrieveFlawBlock(): boolean;
    procedure     AddToCheckLines(Const S: String);
    procedure     AddStringsToCheckLines(Const Strings: TStrings);
    function      LabelToText(aValueLabelSet: TValueLabelSet): boolean;
    function      AddCommandList(CmdList: TChkCommands): boolean;
    procedure     FieldBlockToStrings(aField: TEpiField);
  protected

  public
    Constructor Create;
    Destructor  Destroy; override;
    function    ReadCheckFile(const aFileName: string; Df: TEpiDataFile): boolean;
    function    WriteCheckToFile(const aFileName: string; Df: TEpiDataFile): boolean;
    function    WriteCheckToStream(Stream: TStream; Df: TEpiDataFile): boolean;
    Property    CheckLines: TStrings read FCheckLines;
    Property    OnTranslate: TTranslateEvent read FOnTranslate write FOnTranslate;
    Property    ErrorLines:  TStrings read FErrorLines;
  end;

implementation

uses
  SysUtils, UCheckFileTypes, UEpiUtils, TypInfo,
  Graphics, UEpiDataConstants, Dialogs, UStringUtils,
  StrUtils;

{ TCheckParser }

Constructor TCheckParser.Create();
BEGIN
  inherited create;
  FLines := TStringList.Create;
  FCurLin := '';
  FCurLinIndex := -1;
  FEndOfLines := False;
END;  //tparser.create

Destructor TCheckParser.Destroy;
BEGIN
  FreeAndNil(FLines);
  inherited destroy;
END;

procedure TCheckParser.LoadLines(ChkLines: TStrings);
begin
  FLines.AddStrings(ChkLines);
end;

Function TCheckParser.GetToken(nwType: TNwType):string;
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

Function TCheckParser.GetUpperToken(nwType: TNwType):string;
BEGIN
  Result:=AnsiUpperCase(GetToken(nwType));
END;

Function TCheckParser.GetLowerToken(nwType: TNwType):string;
BEGIN
  Result:=AnsiLowerCase(GetToken(nwType));
END;

Function TCheckParser.GetLineAndFlush:String;
BEGIN
  Result:=FCurLin;
  FCurLin:='';
END;   //TParser.GetLineAndFlush

Function TCheckParser.GetWholeLine:String;
BEGIN
  Result:=FLines[FCurLinIndex];
  FCurLin:='';
END;

Function TCheckParser.FGetCurLinIndex: Integer;
BEGIN
  IF FEndOfLines THEN Result:=FLines.Count ELSE Result:=FCurLinIndex;
END;

Procedure TCheckParser.CommentCurLine;
BEGIN
  FLines[FCurLinIndex]:='* '+FLines[FCurLinIndex];
END;

{ TCheckFileIO }

function TCheckFileIO.InternalRead(): boolean;
var
  CurCommand: string;
  TmpCF: TEpiCheckFile;
  Res: Boolean;
begin
  Debugger.IncIndent;
  Debugger.Add(ClassName, 'InternalRead', 3);
  result := false;

  if not PreParse() then
    exit;

  try
    FParser := TCheckParser.Create();
    FParser.LoadLines(FCheckLines);
    TmpCF := FDf.CheckFile;

    result := true;
    REPEAT    //Read top-level check commands
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
      Res := true;
      CurCommand := FParser.GetUpperToken(nwAny);

      if FDf.Fields.FieldExists(CurCommand) then
        Res := RetrieveFieldBlock(FDf.Fields.FieldByName(CurCommand))
      ELSE IF CurCommand = 'LABELBLOCK' THEN Res := RetrieveLabelBlock
      ELSE IF CurCommand = 'CONSISTENCYBLOCK' THEN Res := RetrieveAssertBlock
      ELSE IF CurCommand = 'BEFORE' THEN
        BEGIN
          CurCommand := FParser.GetUpperToken(nwSameLine);  
          IF CurCommand = 'FILE' THEN
          Begin
            if not Assigned(TmpCF.BeforeFileCmds) then TmpCF.BeforeFileCmds := TChkCommands.Create();
            Res := GetCommandList(TmpCF.BeforeFileCmds, nil)
          end ELSE IF CurCommand='RECORD' THEN
          begin
            if not Assigned(TmpCF.BeforeRecordCmds) then TmpCF.BeforeRecordCmds := TChkCommands.Create();
            Res := GetCommandList(TmpCF.BeforeRecordCmds, nil)
          end ELSE
            Res := ReportError(Lang(22798,'Unknown command after BEFORE'));
        END
      ELSE IF CurCommand='AFTER' THEN
        BEGIN
          CurCommand := FParser.GetUpperToken(nwSameLine);
          IF CurCommand = 'FILE' THEN
          begin
            if not Assigned(TmpCF.AfterFileCmds) then TmpCF.AfterFileCmds := TChkCommands.Create();
            Res := GetCommandList(TmpCF.AfterFileCmds, nil)
          end ELSE IF CurCommand = 'RECORD' THEN
          begin
            if not Assigned(TmpCF.AfterRecordCmds) then TmpCF.AfterRecordCmds := TChkCommands.Create();
            Res := GetCommandList(TmpCF.AfterRecordCmds, nil)
          end ELSE
            Res := ReportError(Lang(22800,'Unknown command after AFTER'));
        END
      ELSE IF CurCommand = 'RECODEBLOCK' THEN
      begin
        if not Assigned(TmpCF.RecodeCmds) then TmpCF.RecodeCmds := TChkCommands.Create();
        Res := GetCommandList(TmpCF.RecodeCmds, nil)
      end ELSE IF CurCommand <> '' THEN
      BEGIN
        IF CurCommand[1] = '*' THEN
          Res := AddTopComment
        ELSE
          Res := RetrieveFlawBlock;
      END;
      Result := Result and Res;
    UNTIL FParser.EndOfLines;
  finally
    Debugger.DecIndent;
    if Assigned(FParser) then FreeAndNil(FParser);
  end;
end;

function TCheckFileIO.InternalWrite(): boolean;
var
  LocalCheckFile: TEpiCheckFile;
  S: String;
  i, n: integer;
BEGIN
  FCheckLines.Clear;
  FCheckLines.BeginUpdate;
  LocalCheckFile := FDf.CheckFile;
  FIndentLvl := 0;

  try
    {Top Comments}
    IF Assigned(LocalCheckFile.TopComments) and (LocalCheckFile.TopComments.Count > 0) THEN
    begin
      AddStringsToCheckLines(LocalCheckFile.TopComments);
      AddToCheckLines('');
    END;

    {Write LabelBlock}
    if Assigned(FDf.ValueLabels) and (FDf.ValueLabels.Count > 0) then
    begin
      AddToCheckLines('LABELBLOCK');
      Inc(FIndentLvl);
      n := 0;
      for i := 0 to FDf.ValueLabels.Count -1 do
      begin
        if FDf.ValueLabels[i].LabelType = vltGlobal then
        begin
          LabelToText(FDf.ValueLabels[i]);
          Inc(n);
        end;
      end;
      Dec(FIndentLvl);
      AddToCheckLines('END');
      AddToCheckLines('');
      if n = 0 then
        for i := 0 to 2 do
          FCheckLines.Delete(FCheckLines.Count-1);
    end;

    {Write assertblock}
    IF Assigned(LocalCheckFile.AssertList) and (LocalCheckFile.AssertList.Count > 0) THEN
    BEGIN
      AddToCheckLines('CONSISTENCYBLOCK');
      AddStringsToCheckLines(LocalCheckFile.AssertList);
      AddToCheckLines('END');
      AddToCheckLines('');
    END;

    {Write recodeblock}
    IF Assigned(LocalCheckFile.RecodeCmds) and (LocalCheckFile.RecodeCmds.Count > 0) THEN
    BEGIN
      AddToCheckLines('RECODEBLOCK');
      Inc(FIndentLvl);
      AddCommandList(LocalCheckFile.RecodeCmds);
      Dec(FIndentLvl);
      AddToCheckLines('END');
      AddToCheckLines('');
    END;

    {Write before/after file and before/after record}
    IF Assigned(LocalCheckFile.BeforeFileCmds) OR (LocalCheckFile.GlobalMissingVal[0]<>'') THEN
    BEGIN
      AddToCheckLines('BEFORE FILE');
      Inc(FIndentLvl);
      S:='';
      IF LocalCheckFile.GlobalMissingVal[0] <> '' THEN S := LocalCheckFile.GlobalMissingVal[0];
      IF LocalCheckFile.GlobalMissingVal[1]<>'' THEN S := S + LocalCheckFile.GlobalMissingVal[1];
      IF LocalCheckFile.GlobalMissingVal[2]<>'' THEN S := S + LocalCheckFile.GlobalMissingVal[2];
      IF S <> '' THEN
        AddToCheckLines('MISSINGVALUE ALL ' + S);
      AddCommandList(LocalCheckFile.BeforeFileCmds);
      Dec(FIndentLvl);
      AddToCheckLines('END');
      AddToCheckLines('');
    END;
    IF Assigned(LocalCheckFile.AfterFileCmds) THEN
    BEGIN
      AddToCheckLines('AFTER FILE');
      Inc(FIndentLvl);
      AddCommandList(LocalCheckFile.AfterFileCmds);
      Dec(FIndentLvl);
      AddToCheckLines('END');
      AddToCheckLines('');
    END;
    IF Assigned(LocalCheckFile.BeforeRecordCmds) THEN
    BEGIN
      AddToCheckLines('BEFORE RECORD');
      Inc(FIndentLvl);
      AddCommandList(LocalCheckFile.BeforeRecordCmds);
      Dec(FIndentLvl);
      AddToCheckLines('END');
      AddToCheckLines('');
    END;
    IF Assigned(LocalCheckFile.AfterRecordCmds) THEN
    BEGIN
      AddToCheckLines('AFTER RECORD');
      Inc(FIndentLvl);
      AddCommandList(LocalCheckFile.AfterRecordCmds);
      Dec(FIndentLvl);
      AddToCheckLines('END');
      AddToCheckLines('');
    END;

    {Write field blocks}
    FOR i := 0 TO FDf.NumFields - 1 do
      if Assigned(FDf[i].CheckField) then
        FieldBlockToStrings(FDf[i]);
  finally
    FCheckLines.EndUpdate;
  end;
end;

function TCheckFileIO.PreParse(): boolean;
var
  TmpLines: TStringList;
  i, j: integer;
  CurLine, fn: string;
begin
  result := false;
  
  // Include in check lines:
  TmpLines := TStringList.Create();

  // Disable all background update function (improves speed).
  TmpLines.BeginUpdate;
  FCheckLines.BeginUpdate;

  for i := FCheckLines.Count - 1 downto 0 do
  begin
    CurLine := FCheckLines[i];
    if Pos('INCLUDE ', AnsiUpperCase(CurLine)) > 0 then
    begin
      fn := Trim(Copy(CurLine, 9, Length(CurLine)));
      if (fn[1] = '"') and (fn[Length(fn)] = '"') then
        fn := Copy(fn, 2, Length(fn)-2);

      if not FileExists(fn) then
      begin
        ReportError(Format(Lang(22870, 'Includefile %s not found'), [fn]));
        Exit;
      end;

      try
        TmpLines.LoadFromFile(fn);
        for j := TmpLines.Count -1 downto 0 do
          FCheckLines.Insert(i, TmpLines[j]);
      except
        ReportError(Format(Lang(22872, 'Error reading includefile %s'), [fn]));
        Exit;
      end;
    end;
  end;

  TmpLines.EndUpdate;
  FCheckLines.EndUpdate;

  FreeAndNil(TmpLines);
  Result := true;
end;

function TCheckFileIO.Lang(Const aCode: Cardinal; const Msg: string): string;
begin
  result := Msg;
  if Assigned(FOnTranslate) then
    result := FOnTranslate(aCode, Msg);
end;

function TCheckFileIO.ReportError(const ErrStr: String): boolean;
var
  n:Integer;
begin
  if Assigned(Fparser) then
    n := FParser.GetCurLinIndex + 1
  else
    n := 0;

  FErrorLines.Append(Format(Lang(22700, '%s in line %d:'), [ErrStr, n]));
  if Assigned(FParser) then
    FErrorLines.Append(FParser.GetWholeLine);
  FErrorLines.Append('');
  result := false;
end;

function TCheckFileIO.RetrieveFieldBlock(CurField: TEpiField): boolean;
VAR
{  n:Integer;
  tmpBool: boolean;
  ValueLabelType: TValueLabelSetType;
  ValueLabelUse:  String;
  ValueLabelShow: Boolean;
  tmpCommands: TChkCommands;    }

  LocalCheck: TEpiCheckField;
  CurCommand: string;
  Res: Boolean;
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
  Result := true;
  Res := True;
  
  IF CurField.FieldType = ftQuestion THEN exit;
  If not Assigned(CurField.CheckField) then
    CurField.CheckField := TEpiCheckField.Create();

  LocalCheck := CurField.CheckField;

  REPEAT
    CurCommand := FParser.GetUpperToken(nwAny);  
    IF      CurCommand='RANGE'       THEN Res := RetrieveRange(CurField)
    ELSE IF CurCommand='LEGAL'       THEN Res := RetrieveLegals(CurField)
    ELSE IF CurCommand='MISSINGVALUE' THEN Res := RetrieveMissingValues(CurField)
    ELSE IF CurCommand='DEFAULTVALUE' THEN Res := RetrieveDefaultValue(CurField)
    ELSE IF CurCommand='AUTOSEARCH'   THEN Res := RetrieveAutosearch(CurField)            
    ELSE IF CurCommand='MUSTENTER'   THEN LocalCheck.MustEnter := True
    ELSE IF CurCommand='NOENTER'     THEN LocalCheck.NoEnter := True
    ELSE IF CurCommand='TOPOFSCREEN' THEN
    BEGIN
      LocalCheck.TopOfScreen := True;
      LocalCheck.TopOfScreenLines := 0;
      CurCommand := FParser.GetToken(nwSameLine);
      IF (CurCommand<>'') AND (IsInteger(CurCommand)) THEN
        LocalCheck.TopOfScreenLines := StrToInt(CurCommand);
    END
    ELSE IF CurCommand='REPEAT' THEN
      LocalCheck.DoRepeat := True
    ELSE IF (CurCommand='CODEFIELD') OR (CurCommand='CODES') THEN
      Res := ReportError(Lang(22782,'CODEFIELD/CODES not supported. Please use TYPE COMMENT fieldname instead.'))
    ELSE IF CurCommand='AUTOJUMP'     THEN Res := RetrieveAutoJump(CurField)
    ELSE IF CurCommand='JUMPS'        THEN Res := RetrieveJumps(CurField)
    ELSE IF CurCommand='COMMENT'      THEN Res := RetrieveCommentLegal(CurField)
    ELSE IF CurCommand='TYPE'         THEN Res := RetrieveType(CurField)
    ELSE IF CurCommand='KEY'          THEN Res := RetrieveKeys(CurField)
    ELSE IF CurCommand='CONFIRMFIELD' THEN LocalCheck.Confirm := True
    ELSE IF CurCommand='ENTER'        THEN Res := true
    ELSE IF CurCommand='BEFORE'       THEN
    BEGIN
      CurCommand := FParser.GetUpperToken(nwSameLine);
      IF CurCommand = 'ENTRY' THEN
        GetCommandList(LocalCheck.BeforeCmds, CurField)
      ELSE
        Result := ReportError(Lang(22786,'ENTRY expected'));
    END
    ELSE IF CurCommand='AFTER' THEN
    BEGIN
      CurCommand := Fparser.GetUpperToken(nwSameLine);
      IF CurCommand = 'ENTRY' THEN
        GetCommandList(LocalCheck.AfterCmds, CurField)
      ELSE
        Result := ReportError(Lang(22786,'ENTRY expected'));
    END
    ELSE IF (Length(CurCommand) > 0) and (CurCommand[1] = '*') then
      Res := AddFieldComment(CurField)
    else IF not ((CurCommand = 'END') or (CurCommand = '')) THEN
      GetCommand(CurCommand, LocalCheck.AfterCmds, CurField);
    Result := Result and Res;
  UNTIL (FParser.EndOfLines) OR (CurCommand='END');
END;

function TCheckFileIO.RetrieveRange(CurField: TEpiField): boolean;
VAR
  CurCommand: string;
  LocalChk: TEpiCheckField;
  tmpS:String;
BEGIN
  Result := true;
  LocalChk := CurField.CheckField;
  
  {Get minimum value}
  CurCommand := FParser.GetUpperToken(nwSameLine);
  IF CurCommand='' THEN
    Result := ReportError(Lang(22712, 'RANGE command without mininum value'))
  ELSE
    LocalChk.Min := CurCommand;

  {Get maxinum value}
  CurCommand := FParser.GetUpperToken(nwSameLine);
  IF CurCommand = '' THEN
    Result := ReportError(Lang(22714, 'RANGE command without maximum value'))
  ELSE
    LocalChk.Max := CurCommand;

  {Check if range values are compliant with Fieldtype}
  IF (LocalChk.Min <> '') AND (NOT IsCompliant(LocalChk.Min, CurField.FieldType)) THEN
    Result := ReportError(Lang(22716, 'Minimum value is not compatible with this type of field'));
  IF (LocalChk.Max <> '') AND (NOT IsCompliant(LocalChk.Max, CurField.FieldType)) THEN
    Result := ReportError(Lang(22718, 'Maximum value is not compatible with this type of field'));
END;  //function RetrieveRange

function TCheckFileIO.RetrieveLegals(CurField: TEpiField): boolean;
VAR
  CurCommand: string;
  LocalCheck: TEpiCheckField;
BEGIN
  Result := true;

  LocalCheck := CurField.CheckField;
  REPEAT
    IF NOT FParser.EndOfLines THEN
      CurCommand := FParser.GetToken(nwAny)
    ELSE
      Result := ReportError(Lang(22704, 'Missing END of LEGAL-block.'));

    IF AnsiUpperCase(CurCommand)='END' THEN
      Break
    ELSE IF AnsiUpperCase(CurCommand)='USE' THEN
    BEGIN
      //LEGAL USE structure
      CurCommand := FParser.GetLowerToken(nwSameLine);

      IF CurCommand = '' THEN
        Result := ReportError(Lang(22706, 'LEGAL USE command without fieldname'));
      if not FDf.FieldExists(CurCommand) then
        Result := ReportError(Lang(22708, 'Unknown fieldname'));

      if Result then
      begin  //Fieldname came after the USE command
        LocalCheck.Legal := FDf.FieldByName(CurCommand).CheckField.Legal;
        break;
      END;
    END ELSE IF CurCommand<>'' THEN
    BEGIN
      //LEGAL values
      IF IsCompliant(CurCommand, CurField.FieldType) THEN
        LocalCheck.Legal := LocalCheck.Legal  + ',' + CurCommand
      ELSE
        Result := ReportError(Lang(22710, 'Legal value is not compatible with this Fieldtype'));
    END;  //else
  UNTIL not Result;
  If LocalCheck.Legal[1] = ',' then
    LocalCheck.Legal := Copy(LocalCheck.Legal, 2, Length(LocalCheck.Legal));
END;

function TCheckFileIO.RetrieveMissingValues(CurField: TEpiField): boolean;
VAR
  s: string;
  i: integer;
  LocalCheck: TEpiCheckField;
BEGIN
  Result := true;
  LocalCheck := CurField.CheckField;

  //Syntax:  MISSINGVALUE x [x [x]]  where x is str10
  for i := 0 to 2 do
  begin
    s := FParser.GetToken(nwSameLine);
    IF (Length(s) > CurField.FieldLength) then
      Result := ReportError(Lang(22852, 'Value is too wide for field'));

    IF ((S <> '') AND (NOT IsCompliant(S, CurField.FieldType))) then
      Result := ReportError(Lang(22710, 'Value is not compatible with this Fieldtype'));

    if Result then
      LocalCheck.MissingValues[i] := s;
  end;
END;

function TCheckFileIO.RetrieveDefaultValue(CurField: TEpiField): boolean;
VAR
  s: string;
BEGIN
  Result := true;

  //Syntax:  DEFAULTVALUE x where x is string
  s := FParser.GetToken(nwSameLine);
  IF (length(s) > CurField.FieldLength) THEN
    result := ReportError(Lang(22852, 'Value is too wide for field'));

  IF (s <> '') AND (NOT IsCompliant(s, CurField.FieldType)) THEN
    result := ReportError(Lang(22710, 'Value is not compatible with this Fieldtype'));

  IF Result THEN
    CurField.CheckField.DefaultValue := s;
END;

function TCheckFileIO.RetrieveAutosearch(CurField: TEpiField): boolean;
VAR
  LocalCheck: TEpiCheckField;
  CurCommand: String;
BEGIN
  result := true;
  LocalCheck := CurField.CheckField;

  LocalCheck.AutoFields := '';
  CurCommand := FParser.GetUpperToken(nwSameLine);
  IF (CurCommand = 'LIST') OR (CurCommand = 'SOUNDEX') THEN
  BEGIN
    IF CurCommand = 'LIST' THEN LocalCheck.AutoList := True;
    CurCommand := FParser.GetUpperToken(nwSameLine);
    IF (CurCommand = 'LIST') OR (CurCommand = 'SOUNDEX') THEN
      BEGIN
        IF CurCommand = 'LIST' THEN LocalCheck.AutoList := True;
        CurCommand := FParser.GetUpperToken(nwSameLine);
      END;
  END;

  REPEAT
    if not FDf.FieldExists(CurCommand) then
      result := ReportError(Lang(22708, 'Unknown fieldname'))
    ELSE
      LocalCheck.AutoFields := LocalCheck.AutoFields + CurCommand + ',';

    CurCommand := FParser.GetToken(nwSameLine);
  UNTIL (CurCommand='') or (not Result);

  IF LocalCheck.AutoFields[Length(LocalCheck.AutoFields)] = ',' THEN
    LocalCheck.AutoFields := Copy(LocalCheck.AutoFields, 1, Length(LocalCheck.AutoFields) - 1);
end;

function TCheckFileIO.RetrieveAutoJump(CurField: TEpiField): boolean;
var
  CurCommand: String;
BEGIN
  Result := true;
  CurCommand:=FParser.GetUpperToken(nwSameLine);
  
  IF CurCommand='' THEN
    Result := ReportError(Lang(22728,'AUTOJUMP command without name of field to jump to'));

  if (not FDf.FieldExists(CurCommand)) And (CurCommand <> 'END')
  AND (CurCommand <> 'WRITE') AND (CurCommand <> 'SKIPNEXTFIELD') THEN
    Result := ReportError(Lang(22730,'Unknown fieldname in AUTOJUMP command'))
  ELSE
    CurField.CheckField.Jumps := 'AUTOJUMP,' + CurCommand;
END;  

function TCheckFileIO.RetrieveJumps(CurField: TEpiField): boolean;
VAR
  CurCommand, TmpS: String;
  LocalCheck: TEpiCheckField;
BEGIN
  Result := true;
  LocalCheck := CurField.CheckField;

  REPEAT
    {Check if a RESET command exists after JUMPS}
    CurCommand := FParser.GetToken(nwSameLine);
    IF CurCommand <> '' THEN
    BEGIN
      IF AnsiUpperCase(CurCommand) <> 'RESET' THEN
        Result := ReportError(Format(Lang(22830, 'RESET expected but %s found'), [CurCommand]))
      ELSE BEGIN
        LocalCheck.JumpResetChar := #32;
        CurCommand := FParser.GetToken(nwSameLine);
        IF Length(CurCommand) = 1 THEN
          LocalCheck.JumpResetChar := CurCommand[1];
      END;
    END;
    
    {Read value}
    IF NOT FParser.EndOfLines THEN
      CurCommand := FParser.GetUpperToken(nwAny)
    ELSE
      Result := ReportError(Lang(22720, 'Missing END of JUMPS-block'));

    IF CurCommand='END' THEN
      Break
    ELSE
      IF CurCommand <> '' THEN
      BEGIN
        Result := IsCompliant(CurCommand, CurField.FieldType);
        Result := Result and (not (CurField.FieldType in [ftToDay, ftYMDToday, ftEuroToday]));

        TmpS := trim(CurCommand) + '>';

        IF Not Result THEN ReportError(Lang(22722, 'Illegal datatype'));

        {Get name of field to jump to}
        IF NOT FParser.EndOfLines THEN
          CurCommand := FParser.GetToken(nwSameLine)  // NextWord(nwSameLine)
        ELSE
          Result := ReportError(Lang(22724,'Jumps command without field to jump to'));  //'Jumps command without field to jump to'

        if (Pos(AnsiUpperCase(CurCommand), 'END WRITE SKIPNEXTFIELD') = 0) and
           (not FDf.FieldExists(CurCommand)) then
          Result := ReportError(Lang(22726, 'Unknown fieldname in JUMP block'));

        IF Result THEN
          LocalCheck.Jumps := LocalCheck.Jumps + TmpS + CurCommand + ',';
      END;
  UNTIL (Not Result);
END;

function TCheckFileIO.RetrieveCommentLegal(CurField: TEpiField): boolean;
var
  TmpStr, CurCommand: String;
  LocalCheck: TEpiCheckField;
  LocalValueLabel: TValueLabelSet;
  ComLegDF: TEpiDataFile;
  ValueField, TextField: TEpiField;
  i: Integer;
  F: File of byte;
BEGIN
  {Four kinds of COMMENT LEGAL possible:
  1. COMMENT LEGAL
       1  ...
       2  ...
     END
     Name in ValueLabels has a $ in the end
     ValueLabelType=vltLocal

  2. COMMENT LEGAL USE labelname
     ValueLabelType=vltGlobal

  3. COMMENT LEGAL USE fieldname
     ValueLabelType=vltLocal
     Current field (or command) has ValueLabelIsFieldRef = true

  4. COMMENT LEGAL datafilename
     ValueLabelType=vltFile

  }
  Result := true;

  LocalCheck := CurField.CheckField;

  CurCommand := FParser.GetUpperToken(nwSameLine);
  IF CurCommand <> 'LEGAL' THEN
  BEGIN
    Result := ReportError(Lang(22732, 'Unknown command in line'));
    Exit;
  END;

  CurCommand := FParser.GetUpperToken(nwSameLine);
  IF (CurCommand = '') OR (CurCommand = 'SHOW') THEN
  BEGIN
    // 1. scenario: COMMENT LEGAL...END Structure
    IF CurCommand = 'SHOW' THEN LocalCheck.ShowValueLabel := True;

    LocalValueLabel := TValueLabelSet.Create;
    LocalValueLabel.LabelType := vltLocal;

    While True do
    Begin
      //Repeatedly read value
      CurCommand := FParser.GetToken(nwAny);

      IF AnsiUpperCase(CurCommand) = 'END' THEN
        Break;

      IF Trim(CurCommand) = '' THEN
      BEGIN
        Result := ReportError(Lang(22734, 'Unexpected end of COMMENT LEGAL'));
        Break;
      END;

      TmpStr := Trim(CurCommand);
      // Comment line?
      IF TmpStr[1] = '*' THEN
      BEGIN
        TmpStr := Trim(FParser.GetWholeLine);
        IF Length(TmpStr) > (30 + 80) THEN
        BEGIN
          Result := ReportError(Lang(22874, 'Commented line is too long'));
          Break;
        END ELSE BEGIN
          LocalValueLabel.AddValueLabelPair(Copy(TmpStr, 1, 30), Copy(TmpStr, 31, length(TmpStr)));
          Continue;
        END;
      END;

      // To long?
      IF Length(Trim(CurCommand)) > CurField.FieldLength THEN
      BEGIN
        Result := ReportError(Lang(22852, 'Value is too wide for field'));
        Break;
      END;

      // Compatible types?
      IF Not IsCompliant(Trim(CurCommand), CurField.FieldType) THEN
      BEGIN
        Result := ReportError(Lang(22710, 'Value is not compatible with this Fieldtype'));
        Break;
      END;

      // All seems ok!
      IF Length(CurCommand) > 30 THEN
        CurCommand := Copy(CurCommand, 1, 30);
      TmpStr := Trim(CurCommand);

      //Read text
      CurCommand := FParser.GetToken(nwSameLine);
      IF Trim(CurCommand) = '' THEN
        Break;

      IF Length(CurCommand) > 80 THEN
        CurCommand := Copy(CurCommand,1,80);
      WHILE pos('"', CurCommand) > 0 DO
        Delete(CurCommand, Pos('"', CurCommand), 1);

      LocalValueLabel.AddValueLabelPair(TmpStr, CurCommand);
    End;

    IF Result THEN
    BEGIN
      TmpStr := AnsiLowerCase(Lang(22736, 'labels in field') + ' ' + CurField.FieldName);
      if Assigned(FDf.ValueLabels.ValueLabelSetByName(TmpStr)) then
        FDf.ValueLabels.DeleteValueLabelSet(TmpStr);
      LocalValueLabel.Name := TmpStr;
      FDf.ValueLabels.AddValueLabelSet(LocalValueLabel);
    END ELSE
      FreeAndNil(LocalValueLabel);

    LocalCheck.ValueLabel := LocalValueLabel;  
    Exit;
    //if COMMENT LEGAL...END Structure
  END;

  IF CurCommand = 'USE' THEN
  BEGIN
    // 2./3. scenario: COMMENT LEGAL USE structure
    CurCommand := AnsiLowerCase(FParser.GetToken(nwSameLine));

    IF CurCommand = '' THEN
    Begin
      Result := ReportError(Lang(22738, 'COMMENT LEGAL USE command without labelname or fieldname'));
      Exit;
    end;

    // 2. Scenario: Is "Comment legal use" a references to a labelname?
    LocalValueLabel := FDf.ValueLabels.ValueLabelSetByName(CurCommand);

    // 3. Scenario: Reference to a fieldname?
    if (not Assigned(LocalValueLabel)) and FDf.FieldExists(CurCommand) then
      LocalValueLabel := FDf.FieldByName(CurCommand).ValueLabelSet;

    If not Assigned(LocalValueLabel) then
    begin
      Result := ReportError(Lang(22740, 'Unknown labelname or fieldname'));
      Exit;
    end;

    TmpStr := FParser.GetUpperToken(nwSameLine);
    IF TmpStr = 'SHOW' THEN
       LocalCheck.ShowValueLabel := True;

    // Check is labels are compatible with current field
    I := 0;
    While Result and (i < LocalValueLabel.Count) do
    begin
      if Length(LocalValueLabel.Values[i]) > CurField.FieldLength then
        Result := ReportError(Lang(22852, 'Value is too wide for field'));
      if (not IsCompliant(LocalValueLabel.Values[i], CurField.FieldType)) Then
        Result := ReportError(Lang(22710, 'Value is not compatible with this Fieldtype'));
      inc(i);
    end;

    LocalCheck.ValueLabel := LocalValueLabel;
    Exit;
    //the word USE was found
  END;

  // 4. Scenario: Reference to a filename
  IF ExtractFileExt(Curcommand) = '' THEN
    CurCommand := CurCommand + '.rec';

  TmpStr := GetCurrentDir;
  SetCurrentDir(ExtractFileDir(FDf.FileName));
  CurCommand := ExpandFilename(CurCommand);
  SetCurrentDir(TmpStr);

  IF NOT FileExists(CurCommand) THEN
  BEGIN
    Result := ReportError(Format(Lang(20110, 'Datafile %s does not exist.'), [CurCommand]));
    Exit;
  END;
  TmpStr := CurCommand;
  
  //Comment Legal datafilename structure found
  CurCommand := FParser.GetUpperToken(nwSameLine);
  IF CurCommand = 'SHOW' THEN
    LocalCheck.ShowValueLabel := True;
    
  TRY
    ComLegDf := TEpiDataFile.Create([eoInMemory, eoIgnoreRelates]);
    ComLegDF.OnPassword := FDf.OnPassword;
    IF NOT ComLegDf.Open(TmpStr) THEN
    begin
      Result := ReportError(Format(Lang(20108,'Datafile %s could not be opened'), [TmpStr]));
      Exit;
    end;

    IF ComLegDf.NumRecords = 0 THEN
    BEGIN
      Result := ReportError(Format(Lang(22334,'Datafile %s does not contain any records'), [TmpStr]));
      Exit;
    END;

    // Todo -o Torsten: Index - rebuild index.
{
    TmpStr := ChangeFileExt(TmpStr, '.eix');
    IF (NOT FileExists(ComLegDf.IndexFilename)) THEN
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
      END;     }

    CurCommand := AnsiLowerCase(ExtractFileName(ComLegDf.Filename));
    LocalValueLabel := FDf.ValueLabels.ValueLabelSetByName(CurCommand);

    // Labels already loaded:
    IF (assigned(LocalValueLabel)) THEN
    Begin
      LocalCheck.ValueLabel := LocalValueLabel;
      Exit;
    End;

    // Todo -o Torsten: Index
{    IF (ComLegDf.IndexCount < 2) THEN
    BEGIN
      Result := ReportError(Format(Lang(22832, 'Datafile %s must contain two KEY-fields'), [ComLegDf.Filename]));
      Result:=NIL;
    END    }

    LocalValueLabel := TValueLabelSet.Create;
    LocalValueLabel.Name := CurCommand;
    LocalValueLabel.LabelType := vltFile;

    ValueField := ComLegDf.IndexFile.IndexFields[1];
    TextField  := ComLegDf.IndexFile.Indexfields[2];

    FOR i := 1 TO ComLegDf.NumRecords DO
    BEGIN
      ComLegDf.Read(i);
      LocalValueLabel.AddValueLabelPair(Copy(ValueField.AsData, 1, 30), Copy(TextField.AsData, 1, 80));
    END;
    FDf.ValueLabels.AddValueLabelSet(LocalValueLabel);

    LocalCheck.ValueLabel := LocalValueLabel;

    FreeAndNil(ComLegDf);
  EXCEPT
    Result := ReportError(Format(Lang(22836, 'Datafile %s could not be applied as a comment legal.~This could be caused by low memory'), [ComLegDf.FileName]));
    FreeAndNil(ComLegDf);
  END;  //try..except   
end;

function TCheckFileIO.RetrieveType(CurField: TEpiField): Boolean;
VAR
  CurCommand: String;
  LocalCheck: TEpiCheckField;
  i: integer;
BEGIN
  {Handles TYPE COMMENT, TYPE COMMENT fieldname, TYPE STATUSBAR}
  Result := true;
  LocalCheck := CurField.CheckField;

  CurCommand := FParser.GetToken(nwSameLine);
  IF CurCommand = '' THEN
  BEGIN
    Result := ReportError(Lang(22744, 'Illegal syntax in TYPE command'));
    Exit;
  END;

  IF AnsiUpperCase(CurCommand) = 'STATUSBAR' THEN
  BEGIN
    // Syntax: TYPE STATUSBAR "<text>" [color]
    LocalCheck.TypeType := ttStatusBar;
    CurCommand := FParser.GetToken(nwSameLine);
    LocalCheck.TypeText := CurCommand;
    LocalCheck.TypeColour := clBlue;
    CurCommand := FParser.GetToken(nwSameLine);
    IF CurCommand <> '' THEN
    BEGIN
      CurCommand := AnsiUpperCase(CurCommand);
      FOR i := 0 TO High(ChkColorNames) DO
        IF CurCommand = ChkColorNames[i] THEN
          LocalCheck.TypeColour := ChkColorTypes[i];
    END;
    Exit;
  END;


  IF AnsiUpperCase(CurCommand)='COMMENT' THEN
  BEGIN
    {Syntaxes: TYPE COMMENT
               TYPE COMMENT colour
               TYPE COMMENT fieldname
               TYPE COMMENT ALLFIELDS}
    LocalCheck.TypeColour := clBlue;

    {Next word can be a fieldname, a colour or ALLFIELDS}
    {if not a fieldname then next word is interpreted as a colour}
    CurCommand := FParser.GetToken(nwSameLine);
    IF AnsiUpperCase(CurCommand) = 'ALLFIELDS' THEN
    BEGIN
      LocalCheck.TypeType := ttAllFields;
      LocalCheck.TypeText := '';
      CurCommand := FParser.GetToken(nwSameLine);
      IF CurCommand <> '' THEN
      BEGIN
        CurCommand := AnsiUpperCase(CurCommand);
        FOR i := 0 TO High(ChkColorNames) DO
          IF CurCommand = ChkColorNames[i] THEN
            LocalCheck.TypeColour := ChkColorTypes[i];
      END;
      Exit;
    END;

    //Is type comment fieldname?
    if FDf.FieldExists(CurCommand) then
    BEGIN
      LocalCheck.TypeType := ttField;
      LocalCheck.TypeText := CurCommand;
      Exit;
    END;

    //Is type comment colour?
    IF CurCommand <> '' THEN
    BEGIN
      CurCommand := AnsiUpperCase(CurCommand);
      FOR i := 0 TO High(ChkColorNames) DO
        IF CurCommand = ChkColorNames[i] THEN
          LocalCheck.TypeColour := ChkColorTypes[i];
    END;

    {Read rest of line - compatibility with Epi Info}
    REPEAT
      CurCommand := FParser.GetUpperToken(nwSameLine);
    UNTIL CurCommand='';
    Exit;
  END;  //if Type Comment

  Result := ReportError(Lang(22744, 'Illegal syntax in TYPE command'));
END;

function TCheckFileIO.RetrieveKeys(CurField: TEpiField): Boolean;
VAR
  Number, i: Integer;
  IsUnique, Found: Boolean;
  CurCommand: String;
  LocalCheck: TEpiCheckField;
  LocalIndex: TEpiIndexFile;
BEGIN
  {Can be KEY [n]
          KEY UNIQUE [n] }
  Result := true;
  
  Number := 0;
  LocalIndex := FDf.IndexFile;
  CurCommand := FParser.GetUpperToken(nwSameLine);

  // KEY UNIQUE
  IF CurCommand='UNIQUE' THEN
  BEGIN
    IsUnique := True;
    CurCommand := FParser.GetToken(nwSameLine);
    // KEY UNIQUE n
    IF CurCommand<>'' THEN
    BEGIN
      IF IsInteger(CurCommand) THEN
        Number := StrToInt(CurCommand)
      ELSE BEGIN
        Result := ReportError(Lang(22747,'Illegal syntax in KEY UNIQUE command'));   
        Exit;
      END;
    END;
  END

  // KEY [n]
  ELSE IF IsInteger(CurCommand) THEN
    Number := StrToInt(CurCommand)

  // KEY
  ELSE IF CurCommand <> '' THEN
  BEGIN
    Result := ReportError(Lang(22748,'Illegal syntax in KEY command'));
    Exit;
  END;

  {Test if Key number is already used by CurField}
  IF (Number > 0) AND (Number <= MaxIndices) THEN
    IF LocalIndex.IndexFields[Number] = CurField THEN
      Exit;

  {Test if CurField occupies an Index-slot}
  IF (Number = 0) AND (LocalIndex.IndexCount = MaxIndices) THEN
  BEGIN
    Found:=False;
    i := 0;
    REPEAT
      INC(i);
      IF LocalIndex.IndexFields[i] = CurField THEN Found:=True;
    UNTIL (Found) OR (i = MaxIndices);
    IF Found THEN
      LocalIndex.IndexFields[i] := Nil
    ELSE BEGIN
      Result := ReportError(Format(Lang(22750,'Only %d KEYs are permitted'), [MaxIndices]));
      Exit;
    END;
  END;

  {Test if Number is within limit}
  IF (Number > MaxIndices) OR (Number < 0) THEN
  BEGIN
    Result := ReportError(Format(Lang(22752, 'Illegal KEY number. Only key numbers from 1 to %d are permitted'), [MaxIndices]));
    Exit;
  END;

  IF (Number >= 1) AND (Number <= MaxIndices) AND
     (Assigned(LocalIndex.IndexFields[Number])) THEN
  BEGIN
    Result := ReportError(Lang(22754,'KEY number already used'));
    Exit;
  END;

  IF Number = 0 THEN
  BEGIN  //Find a slot
    i := 1;
    REPEAT
      IF not Assigned(LocalIndex.IndexFields[i]) THEN
        Number := i;
      INC(i);
    UNTIL (Number <> 0) OR (i > MaxIndices);
    IF Number = 0 THEN
      Number := 1;
  END;

  LocalIndex.IndexFields[Number] := CurField;
  LocalIndex.IndexUnique[Number] := IsUnique;
end;

function TCheckFileIO.GetCommandList(CmdList: TChkCommands; CurField: TEpiField): Boolean;
var
  CurCommand: String;
begin
  if not Assigned(CmdList) then exit;
  result := true; 
  REPEAT
    CurCommand := FParser.GetToken(nwAny);
    IF (AnsiUpperCase(CurCommand) <> 'END') and (Trim(CurCommand) <> '') THEN
      Result :=  Result and GetCommand(CurCommand, CmdList, CurField);
  UNTIL (AnsiUpperCase(CurCommand) = 'END') OR (FParser.EndOfLines);
end;

function TCheckFileIO.IsPosibleLetCmd(CurCommand: string; TmpCmdPtr: PCmd): Boolean;
var
  TmpStr: String;
  I, N: Integer;
begin
  Result := true;
  
  {check if unknown CurCommand is implicit LET}
  CurCommand := trim(CurCommand + ' ' + FParser.GetLineAndFlush);
  IF AnsiUpperCase(Copy(CurCommand, 1, 3)) = 'LET' THEN
    Delete(CurCommand, 1, 3);

  {Rules for valid LET:
   1. A '=' is present
   2. A fieldname or var-name is present before '='
   3. A valid expression is found after '='}
  n := pos('=', CurCommand);
  If n = 0 then
  Begin
    Result := ReportError(Lang(22733, 'Unknown command'));
    Exit;
  end;
  IF n = 1 THEN
  BEGIN
    Result := ReportError(Lang(22756, 'Missing field- or variablename to the left of the equal-sign'));
    Exit;
  END;

  {Check if tmpStr contains a fieldname or variablename}
  TmpStr := trim(Copy(CurCommand, 1, n-1));
  if (not FDf.FieldExists(TmpStr)) and
     (not FDf.CheckFile.DefineExists(TmpStr)) THEN
  BEGIN
    Result := ReportError(Lang(22758, 'Unknown field- or variablename to the left of the equal-sign'));
    Exit;
  END;

  WITH TmpCmdPtr^ DO
  BEGIN
    Command    := cmdLET;
    VarIsField := FDf.FieldExists(TmpStr);
    VarName    := TmpStr;
    LetExpr       := trim(Copy(CurCommand, Pos('=', CurCommand) + 1, Length(CurCommand)));
  END;
end;

function TCheckFileIO.GetCommand(CurCommand: String; CmdList: TChkCommands; CurField: TEpiField): Boolean;
VAR
  TmpPCmd: PCmd;
  TmpCmdRec: TCmd;
  TmpField: TEpiField;
  TmpList, ValList: TStrings;
  TmpStr: string;
  Dummy: Boolean;
  N, I, J: Integer;
  TmpColor: TColor;
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
  Result := true;

  IF (AnsiUpperCase(CurCommand) = 'END') or
     (AnsiUpperCase(CurCommand) = 'ENDIF') or
     (AnsiUpperCase(CurCommand) = 'ELSE') or
     (CurCommand = '') THEN Exit;



  TmpCmdRec.IfCmds     := NIL;
  TmpCmdRec.ElseCmds   := NIL;
  TmpCmdRec.ValueLabel := NIL;

  IF CurCommand[1] = '*' THEN
    TmpCmdRec.Command := cmdComment
  ELSE BEGIN
    TmpCmdRec.Command := cmdIF;
    WHILE (TmpCmdRec.Command < cmdLET) AND
          (ChkCmdNames[TmpCmdRec.Command] <> AnsiUpperCase(CurCommand)) DO
      TmpCmdRec.Command := Succ(TmpCmdRec.Command);
    IF (ChkCmdNames[TmpCmdRec.Command] <> AnsiUpperCase(CurCommand)) or
       (AnsiUpperCase(CurCommand) = 'LET') THEN
      Result := IsPosibleLetCmd(CurCommand, @TmpCmdRec);
    if not Result then
      Exit;
  END;  //else

  try
    CASE TmpCmdRec.Command OF
      cmdIF:
        BEGIN
          TmpCmdRec.IfExpr:='';
          TmpCmdRec.IfCmds := TChkCommands.Create();
          REPEAT
            CurCommand := FParser.GetToken(nwSameKeepQuotes);
            TmpCmdRec.IfExpr := TmpCmdRec.IfExpr + ' ' + CurCommand;
          UNTIL (AnsiUpperCase(CurCommand) = 'THEN') or (CurCommand = '');
          IF AnsiUpperCase(CurCommand) = 'THEN' THEN
          BEGIN
            TmpCmdRec.IfExpr := TmpCmdRec.IfExpr + ' ';
            Delete(TmpCmdRec.IfExpr, Pos(' THEN ', AnsiUpperCase(TmpCmdRec.IfExpr)), 6);
          END ELSE BEGIN
            // No THEN was found in same line as expression
            CurCommand := FParser.GetToken(nwAny);
            IF AnsiUpperCase(CurCommand) <> 'THEN' THEN
            begin
              Result := ReportError(Lang(22760,'No THEN found after IF'));
              Exit;
            end
          END;
          //Assign If-expression
          tmpCmdRec.IfExpr := '(' + trim(tmpCmdRec.IfExpr) + ')';
          Dummy := false;
          REPEAT
            CurCommand := FParser.GetToken(nwAny);
            IF AnsiUpperCase(CurCommand) = 'ELSE' THEN
            Begin
              Dummy := True;
              TmpCmdRec.ElseCmds := TChkCommands.Create();
            End;
            If Dummy then GetCommand(CurCommand, TmpCmdRec.ElseCmds, CurField)
            ELSE GetCommand(CurCommand, TmpCmdRec.IfCmds, CurField);
          UNTIL (AnsiUpperCase(CurCommand)='ENDIF') OR (FParser.EndOfLines) OR 
                (AnsiUpperCase(CurCommand)='END');
          IF (FParser.EndOfLines) AND (AnsiUpperCase(CurCommand)<>'ENDIF') THEN
          BEGIN
            Result := ReportError(Lang(22762, 'IF..THEN command without ENDIF'));
            Exit;
          END;
          IF AnsiUpperCase(CurCommand)='END' THEN
          BEGIN
            Result := ReportError(Lang(22764, 'ENDIF expected but END found'));
            Exit;
          END;
        END;
      cmdHelp:
        BEGIN
          CurCommand := FParser.GetToken(nwSameLine); 
          CurCommand := StringReplace(CurCommand, '\n', #13#10, [rfReplaceAll,rfIgnoreCase]);
        
          TmpCmdRec.HelpString := CurCommand;
          TmpCmdRec.HelpType   := mtInformation;
          TmpCmdRec.HelpKeys   := '';
          CurCommand := FParser.GetUpperToken(nwSameLine); 
          IF CurCommand <> '' THEN
          BEGIN
            IF (Copy(CurCommand, 1, 6) = 'KEYS="') AND (Length(CurCommand) > 7) AND
               (CurCommand[Length(CurCommand)] = '"') THEN
            BEGIN
              TmpStr := Copy(CurCommand, 7, Length(CurCommand)-7);
              TmpCmdRec.HelpKeys := Copy(TmpStr, 1, 10);
            END;
          END;
          IF AnsiUpperCase(Copy(CurCommand, 1, 4)) <> 'TYPE' THEN
            CurCommand := FParser.GetUpperToken(nwSameLine);
          TmpStr := '';
          WHILE CurCommand<>'' DO
          BEGIN
            TmpStr := TmpStr + CurCommand;
            CurCommand := FParser.GetUpperToken(nwSameLine);
          END;
          IF (TmpStr = 'TYPE=ERROR') OR (TmpStr = 'TYPE=E') THEN
            tmpCmdRec.HelpType := mtError
          ELSE
            IF (TmpStr = 'TYPE=WARNING') OR (TmpStr = 'TYPE=W') THEN
              tmpCmdRec.HelpType := mtWarning
          ELSE
            IF (TmpStr = 'TYPE=CONFIRMATION') OR (TmpStr = 'TYPE=C') THEN
              tmpCmdRec.HelpType := mtConfirmation;
        END;  //case cmdHelp
      cmdHide, cmdUnhide, cmdClear, cmdGoto:
        BEGIN
          {Check if a fieldname exists after command}
          CurCommand:=FParser.GetUpperToken(nwSameLine);

          // Case: (Un)Hide/Goto/Clear <empty> = current field
          IF CurCommand = '' THEN
            tmpCmdRec.HideVarName := CurField.FieldName

          // Case: GOTO [WRITE | WRITEREC]
          ELSE IF ((CurCommand = 'WRITE') OR (CurCommand = 'WRITEREC')) AND
             (TmpCmdRec.Command = cmdGoto) THEN
            tmpCmdRec.HideVarName := 'WRITE'

          // Case: (Un)Hide/Goto/Clear [COMMENT [LEGAL]]
          ELSE IF (NOT FDf.FieldExists(CurCommand)) AND (CurCommand = 'COMMENT') THEN
          BEGIN
            CurCommand := FParser.GetUpperToken(nwSameLine);
            IF CurCommand = 'LEGAL' THEN
              tmpCmdRec.HideVarName   := '$$COMLEG';
          END

          ELSE IF (NOT FDf.FieldExists(CurCommand)) THEN
          BEGIN
            Result := ReportError(Lang(22708, 'Unknow fieldname.'));
            Exit;
          END

          ELSE
            tmpCmdRec.HideVarName   := CurCommand;
        END;  //case cmdHide or cmdUnhide
      cmdComLegal:
        BEGIN
          TmpField := TEpiField.Create();
          Result := RetrieveCommentLegal(TmpField);
          if Result then
          begin
            TmpCmdRec.ShowList       := TmpField.CheckField.ShowValueLabel;
            TmpCmdRec.ValueLabelIsFieldRef := TmpField.CheckField.ValueLabelIsFieldRef;
            TmpCmdRec.ValueLabel     := TmpField.ValueLabelSet;
          end;
          FreeAndNil(TmpField);
          Exit;
        END;  //case cmdComLegal
      cmdComment:
        BEGIN
          IF Length(CurCommand) > 200 THEN CurCommand := Copy(CurCommand, 1, 200);
          tmpCmdRec.Comment:=CurCommand;
        END;
      cmdDefine:
        BEGIN
          //get variable name
          CurCommand := FParser.GetToken(nwSameLine);
          IF CurCommand='' THEN
          BEGIN
            Result := ReportError(Lang(22766,'DEFINE without variablename'));
            Exit;
          END;
          IF Length(CurCommand)>16 THEN
          BEGIN
            Result := ReportError(Lang(22768,'Variablename can be only 16 characters in DEFINE'));
            Exit;
          END;
          IF FDf.FieldExists(CurCommand) THEN
          BEGIN
            Result := ReportError(Lang(22770,'Dublicate name: The variablename is used by a entryfield'));
            Exit;
          END;

          tmpCmdRec.FName := CurCommand;
          tmpCmdRec.FNumDecimals := 0;

          //Variable name passed all tests - now get the Fieldtype
          CurCommand := FParser.GetUpperToken(nwSameLine);
          IF CurCommand='' THEN
          BEGIN
            Result := ReportError(Lang(22774,'Fieldtype missing in DEFINE command'));
            Exit;
          END;

          TmpCmdRec.FLength := Length(CurCommand);
          IF CurCommand[1]='#' THEN
          BEGIN
            n := 0;
            FOR i := 1 TO Length(CurCommand) DO
            BEGIN
              IF (CurCommand[i] <> '#') AND (CurCommand[i] <> '.') THEN result := False;
              IF CurCommand[i] = '.' THEN INC(n);
            END;
            IF (NOT Result) OR (n>1) THEN
            BEGIN
              Result := ReportError(Lang(22776, 'Error in Fieldtype. Use # and maximum one . to define numeric'));
              Exit;
            END ELSE BEGIN
              IF (n > 0) OR (Length(CurCommand) > 4) THEN
                tmpCmdRec.Felttype := ftFloat
              ELSE
                tmpCmdRec.Felttype := ftInteger;
              IF n > 0 THEN
                tmpCmdRec.FNumDecimals := Length(CurCommand) - Pos('.', CurCommand)
              ELSE
                tmpCmdRec.FNumDecimals := 0;
            END;
          END  //if numeric
          ELSE IF CurCommand[1] = '_' THEN tmpCmdRec.Felttype := ftAlfa
          ELSE IF CurCommand = '<MM/DD/YYYY>' THEN tmpCmdRec.Felttype := ftDate
          ELSE IF Copy(CurCommand, 1, 2) = '<A' THEN
            BEGIN
              tmpCmdRec.Felttype := ftUpperAlfa;
              tmpCmdRec.FLength := Length(CurCommand) - 2;
            END
          ELSE IF Copy(Curcommand, 1, 2) = '<S' THEN
            BEGIN
              tmpCmdRec.Felttype := ftSoundex;
              tmpCmdRec.FLength := Length(CurCommand)-2;
            END
          ELSE IF CurCommand = '<Y>' THEN tmpCmdRec.Felttype := ftBoolean
          ELSE IF CurCommand = '<DD/MM/YYYY>' THEN tmpCmdRec.Felttype := ftEuroDate
          ELSE IF CurCommand = '<YYYY/MM/DD>' THEN tmpCmdRec.Felttype := ftYMDDate
          ELSE BEGIN
            //No legal Fieldtype found
            Result := ReportError(Lang(22778, 'Illegal Fieldtype in DEFINE command'));
            Exit;
          END;

          CurCommand := FParser.GetUpperToken(nwSameLine);
          IF CurCommand = '' THEN tmpCmdRec.FScope := scLocal
          ELSE IF CurCommand[1] = 'G' THEN tmpCmdRec.FScope := scGlobal
          ELSE IF CurCommand[1] = 'C' THEN tmpCmdRec.FScope := scCumulative
          ELSE BEGIN
            Result := ReportError(Lang(22780, 'Illegal scope in DEFINE command. Use GLOBAL or CUMULATIVE'));
            Exit;
          END;

          // All data concerning the DEFINE is read
          // Now check is DEF-name is allready used
          // Ignore the DEF if DEF is global and a global def-field with the
          // same Fieldtype exists
          TmpField := FDf.CheckFile.DefineByName(tmpCmdRec.FName);
          IF Assigned(TmpField) THEN
          BEGIN
            //a DEF-var with same name exists
            IF (tmpCmdRec.FScope <> scGlobal) OR (TmpField.CheckField.FieldScope <> scGlobal) THEN
            BEGIN
              Result := ReportError(Lang(22772,'Dublicate name: The variablename is allready used'));
              Exit;
            END;

            IF (tmpCmdRec.FScope = scGlobal) AND (TmpField.CheckField.FieldScope = scGlobal) THEN
            BEGIN
              IF NOT ((tmpCmdRec.Felttype = TmpField.Fieldtype) AND
                      (tmpCmdRec.FLength = TmpField.FieldLength) AND
                      (tmpCmdRec.FNumDecimals = TmpField.NumDecimals)) THEN
              BEGIN
                Result := ReportError(Lang(22773,'A global DEFINE with same fieldname but different Fieldtype or length is allready defined'));
                Exit;
              END;
            END;
          END;
          IF not Assigned(TmpField) THEN
            TmpField := TEpiField.Create();
          TmpField.FieldName   := tmpCmdRec.FName;
          TmpField.Fieldtype   := tmpCmdRec.Felttype;
          TmpField.FieldLength := tmpCmdRec.FLength;
          TmpField.NumDecimals := tmpCmdRec.FNumDecimals;
          TmpField.AsData      := '';
          TmpField.CheckField  := TEpiCheckField.Create();
          TmpField.CheckField.FieldScope := tmpCmdRec.FScope;

          Fdf.CheckFile.AddDefine(TmpField);
        END;  //case cmdDefine.
      cmdAutosave: FDf.CheckFile.Autosave := True;
      cmdConfirm:  FDf.CheckFile.Confirm  := True;
      cmdDefaultAll:
        BEGIN
          //Syntax DEFAULTVALUE ALL|ALLSTRINGS|ALLSTRING|ALLNUMERIC x    eller
          //       DEFAULTVALUE field-field, field, field  X
          CurCommand := FParser.GetUpperToken(nwSameLine);
          IF (CurCommand = 'ALL') OR (CurCommand = 'ALLSTRINGS') OR
             (CurCommand = 'ALLSTRING') or (CurCommand = 'ALLNUMERIC') THEN
          BEGIN
            TmpStr := CurCommand;
            CurCommand := FParser.GetToken(nwSameLine);
            IF CurCommand='' THEN
            BEGIN
              Result := ReportError('The default value must follow DEFAULTVALUE ALL');
              Exit;
            END;

            FDf.CheckFile.GlobalDefaultVal := CurCommand;
            for n:=0 TO FDf.NumFields - 1 DO
            BEGIN
              IF (FDf[n].FieldType in [ftInteger, ftAlfa, ftUpperAlfa, ftFloat, ftCrypt]) THEN
              BEGIN
                IF (TmpStr = 'ALL') THEN FDf[n].CheckField.HasGlobalDefaultVal:=true
                ELSE
                  IF (FDf[n].Fieldtype in [ftAlfa, ftUpperAlfa, ftCrypt]) AND
                     ((TmpStr='ALLSTRINGS') OR (TmpStr='ALLSTRING')) THEN FDf[n].CheckField.HasGlobalDefaultVal:=true
                ELSE
                  IF (FDf[n].Fieldtype in [ftInteger, ftFloat]) AND
                     (TmpStr='ALLNUMERIC') THEN FDf[n].CheckField.HasGlobalDefaultVal:=true;
              END;   //if relevant Fieldtype
            END;  //for
            Exit;
          END;

          //Syntax DEFAULTVALUE field-field, field, field X Y Z is used
          TmpStr := trim(FParser.GetWholeLine);
          Delete(TmpStr, 1, 14); //remove the word DEFAULTVALUE
          TRY
            TmpList := TStringList.Create;
            TmpList.CommaText := TmpStr;
            IF (TmpList.Count<2) THEN
            begin
              Result := ReportError('DEFAULTVALUE must be followed by ALL or at least one fieldname and a default value');
              Exit;
            end;
            TmpStr := TmpList[TmpList.Count - 1];
            FDf.CheckFile.GlobalDefaultVal := TmpStr;

            FOR n := 0 TO TmpList.Count-2 DO
            BEGIN
              //Traverse the list of fields
              IF pos('-', TmpList[n]) > 0 THEN   //is element a field-interval?
              BEGIN
                TmpStr := copy(TmpList[n], 1, pos('-', TmpList[n]) - 1);  //get interval start
                CurCommand := copy(TmpList[n], pos('-', TmpList[n]) + 1, Length(TmpList[n]));  //get interval end
                IF not (FDf.FieldExists(TmpStr) and FDf.FieldExists(CurCommand)) THEN
                BEGIN
                  IF not (FDf.FieldExists(TmpStr)) THEN
                    Result := ReportError(Lang(22708,'Unknown field name') + ' ' + TmpStr);
                  IF not (FDf.FieldExists(CurCommand)) THEN
                    Result := ReportError(Lang(22708,'Unknown field name') + ' ' + CurCommand);
                  Exit;
                END;

                FOR i := FDf.FieldByName(TmpStr).FieldNo TO FDf.FieldByName(CurCommand).FieldNo DO
                BEGIN
                  TmpField := FDf[i];
                  IF (TmpField.FieldType <> ftQuestion) THEN
                  BEGIN
  //                  TmpField.DefaultValue := FDf.CheckFile.GlobalDefaultVal;
                    TmpField.CheckField.HasGlobalDefaultVal := true;
                  END;  //if not question field
                END;  //for
                //if interval
              END ELSE BEGIN
                //element is a single fieldname
                TmpField := FDf.FieldByName(TmpList[n]);
                IF not Assigned(TmpField) THEN
                BEGIN
                  Result := ReportError(Lang(22708, 'Unknown field name') + ' ' + TmpList[n]);
                  Exit;
                END;
                IF (TmpField.Fieldtype <> ftQuestion) THEN
                BEGIN
                  TmpField.CheckField.HasGlobalDefaultVal := True;
  //                TmpField.DefaultValue := TmpStr;
                END;  //if not question field
              END;  //if single fieldname
            END;  //for
          FINALLY
            FreeAndNil(TmpList);
          END;  //try..finally
        END;
      cmdMissingAll:
        BEGIN

          //Syntax MISSINGVALUE ALL x [x [x]]
          CurCommand := FParser.GetUpperToken(nwSameLine);
          IF CurCommand = 'ALL' THEN
          BEGIN
            for i := 0 to 2 do
            begin
              CurCommand := FParser.GetToken(nwSameLine);
              IF CurCommand <> '' THEN
              BEGIN
                IF (NOT IsInteger(CurCommand)) THEN
                begin
                  Result := ReportError(Lang(22876, 'Only numbers can be used as MISSINGVALUES ALL'));
                  Exit;
                end;
                FDf.CheckFile.GlobalMissingVal[i] := CurCommand;
              END;
            end;
            Exit;
          END;

          //Syntax MISSINGVALUE field-field, field, field X Y Z is used
          TmpStr := trim(FParser.GetWholeLine);
          Delete(TmpStr, 1, 14);    //remove the word MISSINGVALUE
          TRY       
            TmpList := TStringList.Create;
            TmpList.CommaText := TmpStr;
            n := 0;
            i := TmpList.Count-1;
            REPEAT
              IF IsInteger(TmpList[i]) THEN INC(n) ELSE Break;
              DEC(i);
            UNTIL (i < 0);
            IF (n > 3) OR (n = 0) THEN
            BEGIN
              Result := ReportError(Lang(22878,'One to three MISSINGVALUEs can be defined'));
              Exit;
            END;
            IF N = TmpList.Count THEN
            BEGIN
              Result := ReportError(Lang(22880,'ALL or fieldnames must follow MISSINGVALUE'));
              Exit;
            END;

            ValList := TStringList.Create;
            for i := TmpList.Count - (1 + N) to TmpList.Count - 1 do
              ValList.Add(TmpList[i]);

            FOR n := 0 TO TmpList.Count-2 DO
            BEGIN
              //Traverse the list of fields
              IF pos('-', TmpList[n]) > 0 THEN   //is element a field-interval?
              BEGIN
                TmpStr := copy(TmpList[n], 1, pos('-', TmpList[n]) - 1);  //get interval start
                CurCommand := copy(TmpList[n], pos('-', TmpList[n]) + 1, Length(TmpList[n]));  //get interval end
                IF not (FDf.FieldExists(TmpStr) and FDf.FieldExists(CurCommand)) THEN
                BEGIN
                  IF not (FDf.FieldExists(TmpStr)) THEN
                    Result := ReportError(Lang(22708,'Unknown field name') + ' ' + TmpStr);
                  IF not (FDf.FieldExists(CurCommand)) THEN
                    Result := ReportError(Lang(22708,'Unknown field name') + ' ' + CurCommand);
                  Exit;
                END;

                FOR i := FDf.FieldByName(TmpStr).FieldNo TO FDf.FieldByName(CurCommand).FieldNo DO
                BEGIN
                  TmpField := FDf[i];
                  IF (TmpField.FieldType <> ftQuestion) THEN
                    for j := 0 to ValList.Count-1 do
                      TmpField.CheckField.MissingValues[j] := ValList[j];
                END;  //for
                //if interval
              END ELSE BEGIN
                TmpField := FDf.FieldByName(TmpList[n]);
                IF not Assigned(TmpField) THEN
                BEGIN
                  Result := ReportError(Lang(22708, 'Unknown field name') + ' ' + TmpList[n]);
                  Exit;
                END;
                TmpField := FDf[i];
                IF (TmpField.FieldType <> ftQuestion) THEN
                  for j := 0 to ValList.Count-1 do
                    TmpField.CheckField.MissingValues[j] := ValList[j];
              end;
            END;  //for
          FINALLY
            FreeAndNil(ValList);
            FreeAndNil(TmpList);
          END;  //try..finally
        END;  //case cmdMissingAll
      cmdIgnoreMissing: FDf.CheckFile.MissingAction := maIgnoreMissing;
      cmdTypeString:
        BEGIN
          {  Syntax: TYPE "text" [colour]  }
          CurCommand := FParser.GetToken(nwSameLine);
          IF AnsiUpperCase(CurCommand)='COMMENT' THEN
          BEGIN
            CurCommand:=FParser.GetUpperToken(nwSameLine);
            IF CurCommand <> 'ALLFIELDS' THEN
            BEGIN
              Result := ReportError(Lang(22741, 'Command not legal in IF, AFTER ENTRY, and BEFORE ENTRY blocks'));
              Exit;
            END;

            FDf.CheckFile.GlobalTypeCom := True;
            TmpCmdRec.TypeText := '¤¤typecommentlegalallfields¤¤';
            TmpCmdRec.tsVarNumber := -1;
            CurCommand := FParser.GetUpperToken(nwSameLine);
            IF CurCommand<>'' THEN
            BEGIN
              FOR i := 0 TO High(ChkColorNames) DO
                IF CurCommand = ChkColorNames[i] THEN
                  FDf.CheckFile.GlobalTypeComColor := ChkColorTypes[i];
            END;
            Exit;
          END;

          IF AnsiUpperCase(CurCommand) = 'STATUSBAR' THEN
          BEGIN
            Result := ReportError(Lang(22741, 'Command not legal in IF, AFTER ENTRY, and BEFORE ENTRY blocks'));
            exit;
          END;

          IF CurCommand='' THEN
          BEGIN
            Result := ReportError(Lang(22746, 'Text to TYPE is missing'));
            Exit;
          END;

  //        tmpCmdRec.tsVarNumber:=df.FocusedField;
  //        IF Length(CurCommand)>40 THEN tmpCmdRec.TypeText:=Copy(CurCommand,1,40)
          {ELSE }
          tmpCmdRec.TypeText := CurCommand;

          //Get a colour - if present
          CurCommand := FParser.GetToken(nwSameLine);  //  NextWord(nwSameLine);
          tmpCmdRec.TypeColor := clBlue;
          IF CurCommand<>'' THEN
          BEGIN
            FOR i := 0 TO High(ChkColorNames) DO
              IF CurCommand = ChkColorNames[i] THEN
                tmpCmdRec.TypeColor := ChkColorTypes[i];

            {Read rest of line - compatibility with Epi Info}
            REPEAT
              CurCommand := FParser.GetUpperToken(nwSameLine);
            UNTIL CurCommand='';
          END;
        END;  //case cmdTypeString
      cmdBeep:
        BEGIN
          CurCommand := FParser.GetUpperToken(nwSameLine);
          tmpCmdRec.BeepType := btStandard;
          IF CurCommand<>'' THEN
          BEGIN
            IF (CurCommand = 'WARNING') OR (CurCommand = 'W') THEN
              tmpCmdRec.BeepType := btWarning
            ELSE IF (CurCommand = 'CONFIRMATION') OR (CurCommand = 'C') THEN
              tmpCmdRec.BeepType := btConfirmation;
          END;
        END;  //cmdBeep
        // TODO -o Torsten : Load DLL files.
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
          CurCommand := FParser.GetToken(nwSameLine);
          IF Length(CurCommand) > 200 THEN CurCommand := Copy(CurCommand, 1, 200);
          CurCommand := StringReplace(CurCommand, '\n', #13#10, [rfReplaceAll, rfIgnoreCase]);
          tmpCmdRec.FNote := CurCommand;
          CurCommand := FParser.GetUpperToken(nwSameLine);
          IF CurCommand='SHOW' THEN
            tmpCmdRec.ShowNotes := True
          ELSE
            tmpCmdRec.ShowNotes := False;
        END;
      cmdCopyToClipboard:
        BEGIN
          {Syntax: COPYTOCLIPBOARD "text @variable"}
          CurCommand := FParser.GetToken(nwSameLine);
          IF CurCommand='' THEN
          BEGIN
            Result := ReportError(Lang(23028, 'Invalid parameters'));
            Exit;
          END;
          tmpCmdRec.CopyStr := Copy(CurCommand, 1, 200);
        END;
      cmdShowLastRecord:
        BEGIN
          Fdf.CheckFile.ShowLastRecord := True;
        END;
      cmdExecute:
        BEGIN
          {Syntax: EXECUTE "exe-file name"|* "Parameters"|* NOWAIT|WAIT [HIDE]   }
          {
            Execute bla.htm WAIT
            Execute opera bla.htm WAIT
          }
          CurCommand := FParser.GetToken(nwSameLine);
          IF CurCommand='' THEN
          BEGIN
            Result := ReportError(Lang(22854,'Exe-filename or document-filename is required'));
            Exit;
          END;
          tmpCmdRec.ExecCmdLine := CurCommand;

          //Read next: can be parameters or NOWAIT|WAIT
          CurCommand := FParser.GetToken(nwSameLine);
          tmpCmdRec.ExecParams := '';
          IF (AnsiUpperCase(CurCommand)<>'WAIT') AND (AnsiUpperCase(CurCommand)<>'NOWAIT') THEN
          BEGIN
            //Assume CurCommand contains parameter(s)
            tmpCmdRec.ExecParams := CurCommand;
            CurCommand := FParser.GetToken(nwSameLine);
          END;

          CurCommand := AnsiUpperCase(CurCommand);
          IF (CurCommand<>'WAIT') AND (CurCommand<>'NOWAIT') THEN
          BEGIN
            Result := ReportError(Lang(22856, 'WAIT or NOWAIT is required'));
            Exit;
          END;

          tmpCmdRec.ExecWait := (CurCommand = 'WAIT');

          CurCommand := FParser.GetUpperToken(nwSameLine);
          tmpCmdRec.ExecHide := (CurCommand = 'HIDE');
        END;
      cmdColor:
        BEGIN
          {Syntax: COLOR QUESTION colors
                   COLOR DATA colors
                   COLOR BACKGROUND color
                   COLOR fieldname datacolors questioncolors

                   Colors can be Epi Info color codes
                   or EpiData color words}

          CurCommand := FParser.GetUpperToken(nwSameLine);
          tmpCmdRec.TxtColor := clBlue;
          tmpCmdRec.BgColor := clBlue;
          IF CurCommand='QUESTION' THEN tmpCmdRec.ColorCmd:=1
          ELSE IF CurCommand='DATA' THEN tmpCmdRec.ColorCmd:=2
          ELSE IF CurCommand='BACKGROUND' THEN tmpCmdRec.ColorCmd:=3
          ELSE
            BEGIN
              //could be COLOR fieldname
              //will be added later
              Result := ReportError(Lang(22858, 'Unknown COLOR command'));
              Exit;
            END;

          IF tmpCmdRec.ColorCmd=3 THEN
          BEGIN
            //command is BACKGROUND
            CurCommand := FParser.GetUpperToken(nwSameLine);
            IF IsInteger(CurCommand) THEN
            BEGIN
              tmpCmdRec.IsEpiInfoNo := True;
              n := StrToInt(CurCommand);
              IF (n<0) OR (n>7) THEN
              BEGIN
                Result := ReportError(Lang(22860, 'Illegal COLOR number'));   //22860=Illegal COLOR number
                Exit;
              END;
              tmpCmdRec.BgColor := ChkColorTypes[n];
              Exit;
            END;

            tmpCmdRec.IsEpiInfoNo := False;
            FOR i := 0 TO High(ChkColorNames) DO
              IF CurCommand = ChkColorNames[i] THEN
                tmpCmdRec.BgColor := ChkColorTypes[i];
          END;

          //read rest of line
          TmpStr := '';
          REPEAT
            CurCommand := FParser.GetUpperToken(nwSameLine);
            TmpStr := TmpStr + CurCommand + ' ';
          UNTIL CurCommand='';
          SplitString(TmpStr, TmpList);

          if (TmpList.Count < 1) or (TmpList.Count > 3) then
          BEGIN
            Result := ReportError(Lang(22858, 'Unknown COLOR command'));
            Exit;
          END;

          if IsInteger(TmpList[0]) then
          begin
            tmpCmdRec.IsEpiInfoNo := True;
            n := StrToInt(TmpList[0]);
            IF n > 255 THEN
            BEGIN
              Result := ReportError(Lang(22862, 'Unknown color in COLOR command'));
              Exit;
            END;
            n := n AND $7F;  //clear first bit which indicates flashing text in epi info
            tmpCmdRec.BgColor := ChkColorTypes[(n AND $F0) SHR 4];
            tmpCmdRec.TxtColor := ChkColorTypes[(n AND $0F)];
            Exit;
          end;

          For n := 0 to TmpList.Count do
          begin
            FOR i := 0 TO High(ChkColorNames) DO
              IF CurCommand = ChkColorNames[i] THEN
                TmpColor := ChkColorTypes[i];
            case n of
              0: TmpCmdRec.TxtColor := TmpColor;
              1: TmpCmdRec.BgColor  := TmpColor;
              2: Begin
                  FDf.CheckFile.FieldHighlightAct := True;
                  FDf.CheckFile.FieldHighlightCol := TmpColor;
                 End;
            end;
          end;
        END;
      cmdBackup:
        BEGIN
          {syntax: BACKUP "destination-library" [ZIP filename [date]]
           or      BACKUP "destination-library" [ENCRYPT filname password [date]] }
          IF (CmdList <> FDf.CheckFile.AfterFileCmds) THEN
          BEGIN
            Result := ReportError(Lang(22864, 'BACKUP command only legal in AFTER FILE blocks'));
            Exit;
          END;

          CurCommand := FParser.GetToken(nwSameLine);
          IF CurCommand='' THEN
          BEGIN
            Result := ReportError(Lang(22866, 'BACKUP command without destination directory'));
            Exit;
          END;

          // TODO -o Torsten : Relate!
  {        ELSE IF (df.BackupList=NIL) AND (NOT df.IsRelateFile) THEN
            BEGIN }
          tmpCmdRec.zipit     := False;
          tmpCmdrec.encryptit := False;
          tmpCmdRec.DestLib   := CurCommand;
          CurCommand          := FParser.GetUpperToken(nwSameLine);

          IF (CurCommand <> 'ZIP') AND (CurCommand <> 'ENCRYPT') THEN
          BEGIN
            FDf.CheckFile.BackupList.Append(tmpCmdRec.DestLib);
            FDf.CheckFile.BackupList.Append(FDf.Filename);
            Exit;
          END;

          //ZIP added as parameters ?
          IF CurCommand = 'ZIP' THEN
          BEGIN
            CurCommand := FParser.GetToken(nwSameLine);   //get the filename
            IF CurCommand='' THEN
            BEGIN
              Result := ReportError(Lang(22884, 'Filename needed after ZIP'));
              Exit;
            END;

            FDf.CheckFile.BackupList.Append(tmpCmdRec.DestLib);
            tmpCmdRec.zipit    := True;
            tmpCmdrec.filename := ExtractFilename(CurCommand);
            CurCommand         := FParser.GetUpperToken(nwSameLine);   //get date parameter
            IF CurCommand = 'DATE' THEN tmpCmdRec.dateit := True;
            Exit;
          END;

          //encrypt it
          CurCommand := FParser.GetToken(nwSameLine);   //get the filename
          IF CurCommand='' THEN
          BEGIN
            Result := ReportError(Lang(22884, 'Filename needed after ZIP'));
            Exit;
          END;

          tmpCmdRec.encryptit := True;
          tmpCmdRec.filename  := ExtractFilename(CurCommand);
          CurCommand          := FParser.GetToken(nwSameLine);   //get the password
          IF CurCommand='' THEN
          BEGIN
            Result := ReportError(Lang(22888, 'Password must follow ENCRYPT and filename'));
            Exit;
          END;

          FDf.CheckFile.BackupList.Append(tmpCmdRec.DestLib);
          tmpCmdRec.pw := CurCommand;
          CurCommand   := FParser.GetUpperToken(nwSameLine);  //get date parameter
          IF CurCommand = 'DATE' THEN tmpCmdRec.dateit := True;
        END;  //end case cmdBackup
      cmdRelate:
        BEGIN
          //Syntax: RELATE fieldname filename [1]
          //Get fieldname
          CurCommand:=FParser.GetToken(nwSameLine);
          IF CurCommand='' THEN
          BEGIN
            Result := ReportError(Lang(22840,'Error in RELATE command'));
            Exit;
          END;

          // Relate field exists?
          IF not FDf.FieldExists(CurCommand) THEN
          BEGIN
            Result := ReportError(Lang(22708,'Unknown fieldname'));
            Exit;
          END;

          TmpField := FDf.FieldByName(CurCommand);
          I := FDf.IndexFile.IndexNoByName(TmpField.FieldName);
          if not FDf.IndexFile.IndexUnique[I] then
          begin
            Result := ReportError(Lang(22842,'RELATE field must be KEY UNIQUE'));
            Exit;
          end;

          //Get relatefile name
          tmpCmdRec.RelField := TmpField.FieldName;  //save fieldname

          Curcommand:=FParser.GetToken(nwSameLine);  //  NextWord(nwSameLine);
          IF CurCommand='' THEN
          BEGIN
            Result := ReportError(Lang(22840,'Error in RELATE command'));
            Exit;
          END;

          IF ExtractFileExt(Curcommand) = '' THEN
            CurCommand := CurCommand + '.rec';

          TmpStr := GetCurrentDir;
          SetCurrentDir(ExtractFileDir(FDf.FileName));
          CurCommand := ExpandFilename(CurCommand);
          SetCurrentDir(TmpStr);

          IF (NOT FileExists(CurCommand)) THEN
          BEGIN
            Result := ReportError(Format(Lang(22126,'The file %s does not exist.'),[CurCommand]));
            Exit;
          END;

          tmpCmdRec.RelFileStr := ExtractFileName(CurCommand);

          // TODO -o Torsten : Relate!
(*          TopDf:=TEpiDataFile(df.TopEpiDataFile);
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
          df.HasRelate:=True;    *)


          //Get One2One marker
          CurCommand := FParser.GetToken(nwSameLine);
          tmpCmdRec.One2One := (CurCommand = '1');
//          AInfo^.One2One:=tmpCmdRec.One2One;
        END;   //case cmdRelate
    END;  //Case
  finally
    if Result then
    begin
      TmpPCmd := New(PCmd);
      TmpPCmd^ := TmpCmdRec;
      CmdList.AddCommand(TmpPCmd);
    end;
  end;
end;

function TCheckFileIO.AddFieldComment(CurField: TEpiField): boolean;
var
  S: String;
begin
  S := FParser.GetLineAndFlush;
  CurField.CheckField.FieldComments.Append(FParser.GetWholeLine);
  result := true;
end;

function TCheckFileIO.RetrieveLabelBlock(): Boolean;
var
  CurCommand: String;
  Res: Boolean;
BEGIN
  Result := true;
  {Reads the LABELBLOCK..END block in the checkfile}
  REPEAT
    CurCommand := FParser.GetUpperToken(nwAny);
    IF CurCommand = 'LABEL' THEN
      Result := RetrieveLabel() and Result;
  UNTIL (CurCommand = 'END') OR (FParser.EndOfLines);
END;  //RetrieveLabelBlock

function TCheckFileIO.RetrieveLabel(): Boolean;
//Reads a LABEL..END block
VAR
  aValueLabelSet: TValueLabelSet;
  CurCommand,
  TmpStr: String;
BEGIN
  Result := false;
  CurCommand := FParser.GetLowerToken(nwSameLine);

  IF trim(CurCommand) = '' THEN
  Begin
    Result := ReportError(Lang(0, 'Label name not specified'));
    Exit;
  end;

  If Assigned(FDf.ValueLabels.ValueLabelSetByName(CurCommand)) then
  Begin
    Result := ReportError(Format(Lang(0, 'Label name %d already exists'), [CurCommand]));
    Exit;
  end;

  aValueLabelSet := TValueLabelSet.create;
  aValueLabelSet.Name := trim(CurCommand);
  aValueLabelSet.LabelType := vltGlobal;

  While True do
  Begin
    //Read value
    CurCommand := FParser.GetToken(nwAny);

    IF Trim(CurCommand) = '' THEN
    Begin
      Result := ReportError(Lang(0, 'Missing label value or END'));
      Exit;
    end;

    IF AnsiUpperCase(CurCommand) = 'END' THEN
      Break;

    TmpStr := trim(CurCommand);
    IF TmpStr[1]='*' THEN
    BEGIN
      TmpStr := Trim(FParser.GetWholeLine);
      IF Length(TmpStr)>(30+80) THEN
      BEGIN
        Result := ReportError(Lang(22874,'Commented line is too long'));
        Exit;
      END;

      CurCommand := Copy(TmpStr,1,30);
      IF Length(TmpStr)>30 THEN
        TmpStr := Copy(TmpStr, 31, Length(TmpStr));
      aValueLabelSet.AddValueLabelPair(CurCommand, TmpStr);
      Continue;
    END;

    IF Length(CurCommand) > 30 THEN
      CurCommand := Copy(CurCommand,1,30);
    TmpStr := CurCommand;

    //Read text
    CurCommand := FParser.GetToken(nwSameLine);

    IF trim(CurCommand) = '' THEN
    begin
      Result := ReportError(Format(Lang(0, 'Labe for value %d does not exists'), [TmpStr]));
      Exit;
    end;
      
    IF Length(CurCommand) > 80 THEN
      CurCommand := Copy(CurCommand, 1, 80);
    CurCommand := StringReplace(CurCommand, '"', '', [rfReplaceAll]);

    aValueLabelSet.AddValueLabelPair(TmpStr, CurCommand);
  End;
  FDf.ValueLabels.AddValueLabelSet(aValueLabelSet);
  Result := true;
END;  //TCheckObj.RetrieveLabel

function TCheckFileIO.RetrieveAssertBlock(): boolean;
{Reads the CONSISTENCYBLOCK..END block - and ignores it...}
var
  CurCommand: String;
BEGIN
  Result := true;
  REPEAT
    CurCommand := FParser.GetUpperToken(nwAny);
    IF (CurCommand = 'CHECK') OR (CurCommand = 'REPORT') THEN
      FDf.CheckFile.AssertList.Append(FParser.GetWholeLine);
  UNTIL (CurCommand = 'END') OR (FParser.EndOfLines);
END;

function TCheckFileIO.AddTopComment(): boolean;
VAR
  s: String;
BEGIN
  s := FParser.GetWholeLine;
  IF Assigned(FDf.CheckFile.TopComments) THEN
    FDf.CheckFile.TopComments.Append(s);
  Result := true;
END;  //Procedure AddTopComment


function TCheckFileIO.RetrieveFlawBlock(): Boolean;
var
  CurCommand: String;
BEGIN
  Result := False;
  FParser.GetLineAndFlush;
  ReportError(Lang(22732, 'Unknown command in line'));
  REPEAT
    FParser.CommentCurLine;
    AddTopComment;

    CurCommand := FParser.GetToken(nwAny);  // NextWord(nwAny);
    FParser.GetLineAndFlush;
  UNTIL (FParser.EndOfLines) or (AnsiUpperCase(CurCommand)='END');

  IF AnsiUpperCase(CurCommand) = 'END' THEN
  BEGIN
    FParser.CommentCurLine;
    AddTopComment;
  END;
END;  //procedure RetrieveFlawBlock

procedure TCheckFileIO.AddToCheckLines(Const S: String);
var
  T: string;
begin
  T := DupeString(' ', FIndentLvl * 2);
  FCheckLines.Append(T + S);
end;

procedure TCheckFileIO.AddStringsToCheckLines(Const Strings: TStrings);
var
  T: String;
  I: Integer;
begin
  T := DupeString(' ', FIndentLvl * 2);
  for I := 0 to Strings.Count - 1 do
    FCheckLines.Append(T + Strings[I]);
end;

function TCheckFileIO.LabelToText(aValueLabelSet: TValueLabelSet): boolean;
var
  s: string;
  i: integer;
  
BEGIN
  result := false;

  if (not assigned(aValueLabelSet)) then exit;

  if aValueLabelSet.LabelType = vltGlobal then
    AddToCheckLines('LABEL ' + aValueLabelSet.Name);

//  IF s[Length(s)]='¤' THEN s:=Copy(s,1,Length(s)-1);

  Inc(FIndentLvl);
  for i := 0 to aValueLabelSet.count - 1 do
  BEGIN
    IF aValueLabelSet.Values[i][1] = '*' THEN
    BEGIN
      AddToCheckLines(aValueLabelSet.Values[i] + aValueLabelSet.Labels[i]);
      Continue;
    END;

    S := '';
    if Pos(' ', aValueLabelSet.Values[i]) > 0 then
      S := S + '"' + aValueLabelSet.Values[i] + '"'
    else
      S := S + aValueLabelSet.Values[i];

    if Pos(' ', aValueLabelSet.Labels[i]) > 0 then
      S := S + '  "' + aValueLabelSet.Labels[i] + '"'
    else
      S := S + '  ' + aValueLabelSet.Labels[i];

    AddToCheckLines(S);
  END;  //for
  Dec(FIndentLvl);
  if aValueLabelSet.LabelType = vltGlobal then
    AddToCheckLines('END');
end;


function TCheckFileIO.AddCommandList(CmdList: TChkCommands): Boolean;
VAR
  i, j, n: integer;
  s: string;
  LocalVltType: TValueLabelSetType;
  Cmd: PCmd;
BEGIN
  // Sanity check;
  IF CmdList=NIL THEN Exit;
  IF CmdList.Count=0 THEN Exit;

  FOR j := 0 TO CmdList.Count - 1 DO
  BEGIN
    Cmd := PCmd(CmdList[j]);
    CASE Cmd^.Command OF
      cmdIF:
        BEGIN
          AddToCheckLines('IF ' + trim(cmd^.IfExpr) + ' THEN');
          Inc(FIndentLvl);
          AddCommandList(Cmd^.IfCmds);
          Dec(FIndentLvl);
          IF cmd^.ElseCmds<>NIL THEN
          BEGIN
            AddToCheckLines('ELSE');
            Inc(FIndentLvl);
            AddCommandList(cmd^.ElseCmds);
            Dec(FIndentLvl);
          END;
          AddToCheckLines('ENDIF');
        END;  //case cmdIF
      cmdHelp:
        BEGIN
          S := '"' + cmd^.HelpString + '"';
          S := StringReplace(S, #13#10, '\n', [rfReplaceAll, rfIgnoreCase]);
          IF trim(cmd^.HelpKeys) <> '' THEN
            S := S + ' KEYS="' + trim(AnsiUpperCase(cmd^.HelpKeys)) + '"';
          CASE cmd^.HelpType OF
            mtError:        S := S + ' TYPE=ERROR';
            mtWarning:      S := S + ' TYPE=WARNING';
            mtConfirmation: S := S + ' TYPE=CONFIRMATION';
          END;
          AddToCheckLines('HELP ' + S);
        END;  //case cmdHelp
      cmdWriteNote:
        BEGIN
          S := '"' + cmd^.FNote + '"';
          S := StringReplace(S, #13#10, '\n', [rfReplaceAll, rfIgnoreCase]);
          IF cmd^.ShowNotes THEN S := S + ' SHOW';
          AddToCheckLines('WRITENOTE ' + S);
        END;  //case cmdWriteNote
      cmdCopyToClipboard:
        AddToCheckLines('COPYTOCLIPBOARD "' + cmd^.CopyStr + '"');
      cmdHide:
        AddToCheckLines('HIDE' + ' ' + cmd^.HideVarName);
      cmdUnHide:
        AddToCheckLines('UNHIDE' + ' ' + cmd^.HideVarName);
      cmdClear:
        BEGIN
          S := 'CLEAR';
          IF cmd^.HideVarName='$$COMLEG' THEN
            S := S + ' COMMENT LEGAL'
          ELSE
            S := S + ' ' + cmd^.HideVarName;
          AddToCheckLines(S);
        END;  //case cmdClear
      cmdGoto:
        AddToCheckLines('GOTO ' + cmd^.HideVarName);
      cmdExit:
        AddToCheckLines('EXIT');
      cmdQuit:
        AddToCheckLines('QUIT');
      cmdTypeString:
        BEGIN
          IF cmd^.TypeText = '¤¤typecommentlegalallfields¤¤' THEN
          BEGIN
            S := 'TYPE COMMENT ALLFIELDS';
            IF FDf.CheckFile.GlobalTypeComColor <> 0 THEN
              For i := 0 to High(ChkColorTypes) do
                if ChkColorTypes[i] = FDf.CheckFile.GlobalTypeComColor then
                  S := S + ' ' + ChkColorNames[i];
          END ELSE BEGIN
            S := 'TYPE "' + cmd^.TypeText + '"';
            IF cmd^.TypeColor <> 2 THEN
              For i := 0 to High(ChkColorTypes) do
                if ChkColorTypes[i] = cmd^.TypeColor then
                  S := S + ' ' + ChkColorNames[i];
          END;
          AddToCheckLines(S);
        END;
      cmdBackup:
        begin
          S := 'BACKUP ' + cmd^.DestLib;
          if Cmd^.zipit or Cmd^.encryptit then
          begin
            if Cmd^.encryptit then
              S := S + ' ENCRYPT ';
            if Cmd^.zipit then
              S := S + ' ZIP ';
            S := S + Cmd^.filename;
            if Cmd^.encryptit then
              S := S + ' ' + Cmd^.pw;
            if Cmd^.dateit then
              S := S + ' DATE';
          end;
          AddToCheckLines(S);
        end;
      cmdLoad:
        BEGIN
          S := cmd^.DLLName;
          IF pos(' ', S) > 0 THEN S := '"' + S + '"';
          AddToCheckLines('LOAD ' + S);
        END;
      cmdExecute:
        BEGIN
          IF pos(' ', cmd^.ExecCmdLine) > 0 THEN
            S := 'EXECUTE ' + '"' + cmd^.ExecCmdLine + '"'
          ELSE
            S := 'EXECUTE ' + cmd^.ExecCmdLine;
          IF cmd^.ExecParams <> '' THEN
          BEGIN
            IF pos(' ', cmd^.ExecParams) > 0 THEN
              S := S + ' "' + cmd^.ExecParams + '"'
            ELSE
              S := S + ' ' + cmd^.ExecParams;
          END;
          IF cmd^.ExecWait THEN
            S := S + ' WAIT'
          ELSE
            S := S + ' NOWAIT';
          IF cmd^.ExecHide THEN
            S := S + ' HIDE';
          AddToCheckLines(S);
        END;
      cmdBeep:
        BEGIN
          S := 'BEEP';
          IF cmd^.Beeptype = btWarning THEN S := S + ' Warning';
          IF cmd^.BeepType = btConfirmation THEN S := S + ' Confirmation';
          AddToCheckLines(S);
        END;
      cmdRelate:
        BEGIN
          S := 'RELATE ' + trim(cmd^.RelField) + ' ';
          IF Pos(' ',cmd^.RelFileStr) > 0 THEN
            S := S + '"' + cmd^.RelFileStr + '"'
          ELSE
            S := S + cmd^.RelFileStr;
          IF cmd^.One2One THEN
            S := S + ' 1';
          AddToCheckLines(S);
        END;
      cmdComLegal:
        BEGIN
          IF cmd^.ValueLabel <> NIL THEN
          BEGIN
            LocalVltType := cmd^.ValueLabel.LabelType;
            if (LocalVltType = vltLocal) and (cmd^.ValueLabelIsFieldRef) then
             LocalVltType := vltGlobal;
            case LocalVltType of
              vltLocal:
                begin
                  S := 'COMMENT LEGAL';
                  if cmd^.ShowList then S := S + ' SHOW';
                  AddToCheckLines(S);
                  Inc(FIndentLvl);
                  LabelToText(cmd^.ValueLabel);
                  Dec(FIndentLvl);
                end;
              vltGlobal, vltFile:
                begin
                  S := 'COMMENT LEGAL ';
                  if cmd^.ValueLabel.LabelType <> vltFile then
                    S := S + 'USE ';
                  S := S + cmd^.ValueLabelName;
                  if cmd^.ShowList then S := S + ' SHOW';
                  AddToCheckLines(S);
                end;
            end;  //case
          END;  //if valuelabel<>NIL
        END;  //case cmdComLegal
      cmdLet:
        BEGIN
          S := trim(cmd^.LetExpr);
          S := StringReplace(S, '_M', ' .', [rfReplaceAll, rfIgnoreCase]);
          S := cmd^.VarName + ' = ' + S;
          IF cmd^.CodedWithLet THEN
            S := 'LET ' + S;
          AddToCheckLines(S);
        END;  //case cmdLet
      cmdDefine:
        BEGIN
          CASE cmd^.Felttype OF
            ftInteger:   S := DupeString('#', cmd^.FLength);
            ftAlfa:      S := DupeString('_', cmd^.FLength);
            ftDate:      S := '<MM/DD/YYYY>';
            ftYMDDate:   S := '<YYYY/MM/DD>';          //&&
            ftUpperAlfa: S := '<A' + DupeString('A', cmd^.FLength-1) + '>';
            ftSoundex:   S := '<S' + DupeString('S', cmd^.FLength-1) + '>';
            ftBoolean:   S := '<Y>';
            ftFloat:     BEGIN
                           S := DupeString('#', cmd^.FLength - cmd^.FNumDecimals - 1);
                           IF cmd^.FNumDecimals = 0 THEN
                             S := S + '#'
                           ELSE
                             S := S + '.' + DupeString('#', cmd^.FNumDecimals);
                         END;   
            ftEuroDate:  S := '<DD/MM/YYYY>';
          END;
          S := 'DEFINE ' + cmd^.FName +'  ' + S;
          IF cmd^.FScope = scGlobal THEN S := S + ' GLOBAL';
          IF cmd^.FScope = scCumulative THEN S := S + ' CUMULATIVE';
          AddToCheckLines(S);
        END;  //case cmdDefine
      cmdAutoSave:
        AddToCheckLines('AUTOSAVE');
      cmdConfirm:
        AddToCheckLines('CONFIRM');
      cmdIgnoreMissing:
        AddToCheckLines('IGNOREMISSING');
      //cmdBgColour: sList.Append(IndStr+'BACKGROUNDCOLOUR '+ColorNames[cmd^.BgColour]);
      cmdColor:
        BEGIN
          S := 'COLOR ';
          CASE cmd^.ColorCmd OF
            1: S := S + 'QUESTION ';
            2: S := S + 'DATA ';
            3: S := S + 'BACKGROUND ';
          END;  //case
          IF cmd^.IsEpiInfoNo THEN
          BEGIN
            IF cmd^.ColorCmd = 3 THEN S := S + IntToStr(cmd^.BgColor)
            ELSE BEGIN
              n := (cmd^.BgColor SHL 4);
              n := n AND cmd^.TxtColor;
              S := S + IntToStr(n);
            END;
          END ELSE BEGIN
            IF cmd^.ColorCmd = 3 THEN
            begin
              For i := 0 to High(ChkColorTypes) do
                if ChkColorTypes[i] = cmd^.BgColor then
                  S := S + ' ' + ChkColorNames[i];
            end ELSE BEGIN
              For i := 0 to High(ChkColorTypes) do
                if ChkColorTypes[i] = cmd^.txtcolor then
                  S := S + ' ' + ChkColorNames[i];
              IF cmd^.BgColor <> 255 THEN
                For i := 0 to High(ChkColorTypes) do
                  if ChkColorTypes[i] = cmd^.bgcolor then
                    S := S + ' ' + ChkColorNames[i];
            END;
          END;
          AddToCheckLines(S);
        END;
      cmdComment:
        AddToCheckLines(cmd^.Comment);
      cmdLeaveField:
        BEGIN
          S := 'cmdLeaveField -';
          IF cmd^.IsLastField THEN
            S := S + 'LastField';
          AddToCheckLines(S);
        END;      
    END;  //Case
  END;  //for
END;  //Procedure AddCommandList

Procedure TCheckFileIO.FieldBlockToStrings(aField: TEpiField);
VAR
  S: String;
  TmpList: TStrings;
  LocalVltType: TValueLabelSetType;
  I: Integer;
BEGIN
  TmpList := nil;
  try
    WITH AField.CheckField DO
    BEGIN
      AddToCheckLines(trim(AField.FieldName));
      Inc(FIndentLvl);

      Min := Trim(Min);
      Max := Trim(Max);

      {Write fieldblock comments}
      IF FieldComments.Count > 0 THEN
      BEGIN
        AddStringsToCheckLines(FieldComments);
      END;              

      {Write index key}
      I := FDf.IndexFile.IndexNoByName(aField.FieldName);
      IF I > 0 THEN
      BEGIN
        S := 'KEY ';
        IF FDf.IndexFile.IndexUnique[I] THEN S := S + 'UNIQUE ';
        S := S + IntToStr(I);
        AddToCheckLines(S);
      END;  

      {Write autosearch}
      IF AutoSearch THEN
      BEGIN
        S := 'AUTOSEARCH ';
        IF AutoList THEN S := S + ' LIST ';
        TRY
          SplitString(AutoFields, TmpList, [',']);
          FOR I := 0 TO TmpList.Count - 1 DO
            S := S + TmpList[i] + ' ';
        FINALLY
          FreeAndNil(TmpList);
        END;
        AddToCheckLines(S);
      END;

      {Write NoEnter}
      IF NoEnter THEN
        AddToCheckLines('NOENTER');
        
      {Write TopOfScreen}
      IF TopOfScreen THEN
      BEGIN
        S := 'TOPOFSCREEN';
        IF TopOfScreenLines > 0 THEN
          S := S + ' ' + IntToStr(TopOfScreenLines);
        AddToCheckLines(S);
      END;

      {Write RANGE}
      S := Trim(Min + ' ' + Max);
      IF (S <> '') THEN
        AddToCheckLines('RANGE ' + S);
      
      {Write LEGAL block}
      IF Legal<>'' THEN
      BEGIN
        SplitString(Legal, TmpList, [',']);

        AddToCheckLines('LEGAL');
        Inc(FIndentLvl);
        For I := 0 to TmpList.Count -1 do
          if Pos(' ', TmpList[i]) > 0 then
            AddToCheckLines ('"' + TmpList[i] + '" ')
          else
            AddToCheckLines(TmpList[i]);
        Dec(FIndentLvl);
        AddToCheckLines('END');
      END;

      {Write Comment Legal}
      IF Assigned(ValueLabel) THEN
      BEGIN
        LocalVltType := ValueLabel.LabelType;
        S := 'COMMENT LEGAL ';
        if (LocalVltType = vltLocal) and (ValueLabelIsFieldRef) then
          LocalVltType := vltGlobal;
        case LocalVltType of
          vltLocal:
            begin
              if ShowValueLabel then S := S + 'SHOW';
              AddToCheckLines(S);
              Inc(FIndentLvl);
              LabelToText(ValueLabel);
              Dec(FIndentLvl);
              AddToCheckLines('END');
            end;
          vltGlobal, vltFile:
            begin
              if ValueLabel.LabelType <> vltFile then
                S := S + 'USE ';
              S := S + ValueLabel.Name;
              if ShowValueLabel then S := S + ' SHOW';
              AddToCheckLines(S);
            end;
        end;  //case
      end;

      {Write JUMPS block}
      IF Jumps <> '' THEN
      BEGIN
        SplitString(Jumps, TmpList, [',']);
        IF TmpList[0] = 'AUTOJUMP' THEN
          AddToCheckLines('AUTOJUMP ' + trim(TmpList[1]))
        ELSE BEGIN
          S := 'JUMPS';
          IF JumpResetChar <> #0 THEN
            S := S + ' RESET';
          IF (JumpResetChar <> #0) AND (JumpResetChar <> #32) THEN
            S := S + ' "' + JumpResetChar + '"';
          AddToCheckLines(S);
          Inc(FIndentLvl);
          FOR i := 0 TO TmpList.Count - 1 DO
            AddToCheckLines(StringReplace(TmpList[i], '>', ' ', [rfReplaceAll]));
          Dec(FIndentLvl);
          AddToCheckLines('END');
        END;
      END;

      {Write MUSTENTER, REPEAT}
      IF MustEnter THEN
        AddToCheckLines('MUSTENTER');
      IF DoRepeat THEN
        AddToCheckLines('REPEAT');
      IF Confirm THEN
        AddToCheckLines('CONFIRMFIELD');

      {Write Missingvalues}
      IF MissingValues[0]<>'' THEN
      Begin
        S := MissingValues[0];
        IF MissingValues[1] <> '' THEN S := S + ' ' + MissingValues[1];
        IF MissingValues[2] <> '' THEN S := S + ' ' + MissingValues[2];
        AddToCheckLines('MISSINGVALUE ' + S);
      end;

      Case TypeType of
        ttStatusBar:
          {Write TYPE STATUSBAR}
          begin
            S := 'TYPE STATUSBAR';
            IF TypeText <> '' THEN S := S + ' "' + TypeText + '"';
            IF TypeColour <> clBlue THEN
              For i := 0 to High(ChkColorTypes) do
                if ChkColorTypes[i] = TypeColour then
                  S := S + ' ' + ChkColorNames[i];
            AddToCheckLines(S);
          END;
        {Write TYPE COMMENT ALLFIELDS}
        ttAllFields:
          Begin
            S := 'TYPE COMMENT ALLFIELDS';
            IF TypeColour <> clBlue THEN
              For i := 0 to High(ChkColorTypes) do
                if ChkColorTypes[i] = TypeColour then
                  S := S + ' ' + ChkColorNames[i];
            AddToCheckLines(S);
          END;
        {Write TYPE COMMENT <fieldname>}
        ttField:
          AddToCheckLines('TYPE COMMENT ' + TypeText);
        {Write TYPE COMMENT <colour>}
        ttColour:
          Begin
            For i := 0 to High(ChkColorTypes) do
              if ChkColorTypes[i] = TypeColour then
                S := ChkColorNames[i];
            AddToCheckLines('TYPE COMMENT ' + S);
          End;
      end;

      {Write Before Entry commands}
      IF Assigned(BeforeCmds) and (BeforeCmds.Count > 0) THEN
      BEGIN
        AddToCheckLines('BEFORE ENTRY');
        Inc(FIndentLvl);
        AddCommandList(BeforeCmds);
        Dec(FIndentLvl);
        AddToCheckLines('END');
      END;  //if Before commands

      {Write After Entry commands}
      IF Assigned(AfterCmds) and (AfterCmds.Count > 0) THEN
      BEGIN
        AddToCheckLines('AFTER ENTRY');
        Inc(FIndentLvl);
        AddCommandList(AfterCmds);
        Dec(FIndentLvl);
        AddToCheckLines('END');
      END;  //if After commands

      {End fieldblock}
      Dec(FIndentLvl);
      AddToCheckLines('END');
      AddToCheckLines('');
    END;  //With
  finally
    If Assigned(TmpList) then
      FreeAndNil(TmpList);
  end
END;  //procedure FieldBlockToStrings

constructor TCheckFileIO.Create;
begin
  FCheckLines := TStringList.Create();
  FErrorLines := TStringList.Create();
end;

destructor TCheckFileIO.Destroy;
begin
  if Assigned(FCheckLines) then FreeAndNil(FCheckLines);
  if Assigned(FErrorLines) then FreeAndNil(FErrorLines);
  inherited;
end;

function TCheckFileIO.ReadCheckFile(const aFileName: string;
  Df: TEpiDataFile): boolean;
begin
  Debugger.IncIndent;
  Debugger.Add(ClassName, 'ReadCheckFile', 2, 'Check File = ' + aFileName);

  result := false;
  try
    if not Assigned(DF) then exit;
    if not Assigned(Df.CheckFile) then exit;

    // It's not a fault if no .CHK file exists.
    result := true;
    if not FileExists(aFileName) then exit;

    DF.CheckFile.HasCheckFile := true;
    DF.CheckFile.FileName     := aFileName;

    try
      FCheckLines.LoadFromFile(aFileName);
    except
      FreeAndNil(FCheckLines);
      FErrorLines.Add(Format(Lang(20130,'Error reading the checkfile %s'), [aFileName]));
      Exit;
    end;

    FDf := Df;
    result := InternalRead();
    FDf.CheckFile.ErrorInFile := not result;
  finally
    Debugger.DecIndent;
  end;
end;

function TCheckFileIO.WriteCheckToFile(const aFileName: string;
  Df: TEpiDataFile): boolean;
var
  FStream: TFileStream;
begin
  Result := false;
  try
    FStream := TFileStream.Create(aFilename, fmOpenWrite);
    Result := WriteCheckToStream(FStream, DF);
  finally
    FreeAndNil(FStream);
  end;
end;

function TCheckFileIO.WriteCheckToStream(Stream: TStream; Df: TEpiDataFile): boolean;
var
  i: integer;
begin
  Debugger.IncIndent;
  Debugger.Add(ClassName, 'WriteCheckToStream', 2, 'Streamtype = ' + Stream.ClassName);
  result := false;

  try
    // Sanity check:
    if Not Assigned(Stream) then exit;
    if Not Assigned(Df) then exit;

    FDf := Df;

    result := InternalWrite();

    for i := 0 to FCheckLines.Count -1 do
    begin
      Stream.Write(Pointer(FCheckLines[i])^, Length(FCheckLines[i]));
      Stream.Write(#13#10, 2);
    end;

    Stream.Position := 0;
  finally
    Debugger.DecIndent;
  end;
end;

end.
