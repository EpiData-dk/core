unit UCheckFileIO;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, UCheckFileCmds, UDataFileTypes, UEpiDataFile, UValueLabels,
  UEpiLog;

type

  TNwType = (nwAny, nwSameLine, nwSameKeepQuotes, nwKeepSpaces);
  TCheckParser = class(TObject)
  private
    FLines:       TStringList;
    FCurLin:      string;
    FCurLinIndex: Integer;
    FEndOfLines:  Boolean;
    function      FGetCurLinIndex:Integer;
  public
    Constructor   Create();
    Destructor    Destroy;  override;
    Function      GetToken(nwType: TNwType):string;
    Function      GetUpperToken(nwType: TNwType):string;
    Function      GetLowerToken(nwType: TNwType):string;
    Function      GetLineAndFlush:string;
    Function      GetWholeLine:string;
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
    function      ReportError(const ErrStr: string): boolean;
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
    function      IsPosibleLetCmd(CurCommand: string; var TmpCmd: TChkCommand): Boolean;
    function      GetCommand(CurCommand: string; CmdList: TChkCommands; CurField: TEpiField): Boolean;
    function      AddFieldComment(CurField: TEpiField): boolean;
    function      RetrieveLabelBlock(): Boolean;
    function      RetrieveLabel(): Boolean;
    function      RetrieveAssertBlock(): boolean;
    function      AddTopComment(): Boolean;
    function      RetrieveFlawBlock(): boolean;
    procedure     AddToCheckLines(Const S: string);
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
  UEpiDataGlobals, UStringUtils, StrUtils;

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

Function TCheckParser.GetLineAndFlush:string;
BEGIN
  Result:=FCurLin;
  FCurLin:='';
END;   //TParser.GetLineAndFlush

Function TCheckParser.GetWholeLine:string;
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
  TmpCF: TEpiDataFileProperties;
  Res: Boolean;
begin
  EpiLogger.IncIndent;
  EpiLogger.Add(ClassName, 'InternalRead', 3);
  result := false;

  if not PreParse() then
    exit;

  try
    FParser := TCheckParser.Create();
    FParser.LoadLines(FCheckLines);
    TmpCF := FDf.FileProperties;

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
    EpiLogger.DecIndent;
    if Assigned(FParser) then FreeAndNil(FParser);
  end;
end;

function TCheckFileIO.InternalWrite(): boolean;
var
  LocalCheckFile: TEpiDataFileProperties;
  S: string;
  i, n: integer;
BEGIN
  FCheckLines.Clear;
  FCheckLines.BeginUpdate;
  LocalCheckFile := FDf.FileProperties;
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
        if FDf.ValueLabels[i].LabelScope = vlsGlobal then
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
    with FDf[i] do
      if (HasFieldProperties) or (Assigned(ValueLabelSet)) or
         (DefaultValue <> '')  then
        FieldBlockToStrings(FDf[i]);
  finally
    FCheckLines.EndUpdate;
    Result := true;
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
    CurLine := Trim(FCheckLines[i]);
    if Length(CurLine) = 0 then continue;
    if CurLine[1] = '*' then continue;
    if Pos('INCLUDE ', AnsiUpperCase(CurLine)) > 0 then
    begin
      fn := Trim(Copy(CurLine, 9, Length(CurLine)));
      if (fn[1] = '"') and (fn[Length(fn)] = '"') then
        fn := Copy(fn, 2, Length(fn)-2);

      fn := ExtractFilePath(FDF.FileProperties.FileName) + fn;
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

function TCheckFileIO.ReportError(const ErrStr: string): boolean;
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
  ValueLabelType: TValueLabelSetScope;
  ValueLabelUse:  string;
  ValueLabelShow: Boolean;
  tmpCommands: TChkCommands;    }

  LocalCheck: TEpiFieldProperties;
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

  LocalCheck := CurField.FieldProperties;

  REPEAT
    CurCommand := FParser.GetUpperToken(nwAny);  
    IF      CurCommand='RANGE'       THEN Res := RetrieveRange(CurField)
    ELSE IF CurCommand='LEGAL'       THEN Res := RetrieveLegals(CurField)
    ELSE IF CurCommand='MISSINGVALUE' THEN Res := RetrieveMissingValues(CurField)
    ELSE IF CurCommand='DEFAULTVALUE' THEN Res := RetrieveDefaultValue(CurField)
    ELSE IF CurCommand='AUTOSEARCH'   THEN Res := RetrieveAutosearch(CurField)            
    ELSE IF CurCommand='MUSTENTER'   THEN LocalCheck.EntryType := entMust
    ELSE IF CurCommand='NOENTER'     THEN LocalCheck.EntryType := entNone
    ELSE IF CurCommand='TOPOFSCREEN' THEN
    BEGIN
      LocalCheck.TopOfScreen := 0;
      CurCommand := FParser.GetToken(nwSameLine);
      IF (CurCommand<>'') AND (IsInteger(CurCommand)) THEN
        LocalCheck.TopOfScreen := StrToInt(CurCommand);
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
  CurCommand, Range: string;
  LocalChk: TEpiFieldProperties;
BEGIN
  Result := true;
  LocalChk := CurField.FieldProperties;
  
  {Get minimum value}
  CurCommand := FParser.GetUpperToken(nwSameLine);
  IF CurCommand='' THEN
    Result := ReportError(Lang(22712, 'RANGE command without mininum value'))
  ELSE
    Range := CurCommand + '-';
  {Check if range values are compliant with Fieldtype}
  IF (CurCommand <> '') AND (NOT IsCompliant(CurCommand, CurField.FieldType)) THEN
    Result := ReportError(Lang(22716, 'Minimum value is not compatible with this type of field'));

  {Get maxinum value}
  CurCommand := FParser.GetUpperToken(nwSameLine);
  IF CurCommand = '' THEN
    Result := ReportError(Lang(22714, 'RANGE command without maximum value'))
  ELSE
    Range := Range + CurCommand;
  {Check if range values are compliant with Fieldtype}
  IF (CurCommand <> '') AND (NOT IsCompliant(CurCommand, CurField.FieldType)) THEN
    Result := ReportError(Lang(22718, 'Maximum value is not compatible with this type of field'));

  LocalChk.Ranges.Add(Range);
END;  //function RetrieveRange

function TCheckFileIO.RetrieveLegals(CurField: TEpiField): boolean;
VAR
  CurCommand: string;
  LocalCheck: TEpiFieldProperties;
BEGIN
  Result := true;

  LocalCheck := CurField.FieldProperties;
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
        // TODO : Copy from other field!!!
//        LocalCheck.Legal := FDf.FieldByName(CurCommand).CheckField.Legal;
        break;
      END;
    END ELSE IF CurCommand<>'' THEN
    BEGIN
      //LEGAL values
      IF IsCompliant(CurCommand, CurField.FieldType) THEN
        LocalCheck.Ranges.Add(CurCommand)
      ELSE
        Result := ReportError(Lang(22710, 'Legal value is not compatible with this Fieldtype'));
    END;  //else
  UNTIL not Result;
END;

function TCheckFileIO.RetrieveMissingValues(CurField: TEpiField): boolean;
VAR
  s: string;
  i: integer;
  LocalValueLabel: TValueLabelSet;
BEGIN
  Result := true;
  LocalValueLabel := CurField.ValueLabelSet;
  if not Assigned(CurField.ValueLabelSet) then
  begin
    LocalValueLabel := TValueLabelSet.Create(CurField.FieldType);
    LocalValueLabel.Name := CurField.FieldName + '_lbl';
    LocalValueLabel.LabelScope := vlsLocal;
    CurField.ValueLabelSet := LocalValueLabel;
    CurField.DataFile.ValueLabels.AddValueLabelSet(LocalValueLabel);
  end;

  //Syntax:  MISSINGVALUE x [x [x]]  where x is str10
  for i := 0 to 2 do
  begin
    s := FParser.GetToken(nwSameLine);

    if S = '' then continue;

    IF (Length(s) > CurField.FieldLength) then
      Result := ReportError(Lang(22852, 'Value is too wide for field'));

    IF ((S <> '') AND (NOT IsCompliant(S, CurField.FieldType))) then
      Result := ReportError(Lang(22710, 'Value is not compatible with this Fieldtype'));

    if Result then
      LocalValueLabel.AddValueLabelPair(S, '', True);
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
    CurField.DefaultValue := s;
END;

function TCheckFileIO.RetrieveAutosearch(CurField: TEpiField): boolean;
VAR
  LocalCheck: TEpiFieldProperties;
  CurCommand: string;
BEGIN
  result := true;
  LocalCheck := CurField.FieldProperties;

  // TODO : .CHK Autosearch to .RECXML Autosearch.

{  LocalCheck.AutoFields := '';
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
    LocalCheck.AutoFields := Copy(LocalCheck.AutoFields, 1, Length(LocalCheck.AutoFields) - 1);       }
end;

function TCheckFileIO.RetrieveAutoJump(CurField: TEpiField): boolean;
var
  CurCommand: string;
BEGIN
  Result := true;
  CurCommand:=FParser.GetUpperToken(nwSameLine);
  
  IF CurCommand='' THEN
    Result := ReportError(Lang(22728,'AUTOJUMP command without name of field to jump to'));

  if (not FDf.FieldExists(CurCommand)) And (CurCommand <> 'END')
  AND (CurCommand <> 'WRITE') AND (CurCommand <> 'SKIPNEXTFIELD') THEN
    Result := ReportError(Lang(22730,'Unknown fieldname in AUTOJUMP command'))
  ELSE
    CurField.FieldProperties.Jumps.AddObject('', TString.Create(CurCommand));// := 'AUTOJUMP,' + CurCommand;
END;  

function TCheckFileIO.RetrieveJumps(CurField: TEpiField): boolean;
VAR
  CurCommand, TmpS: string;
  LocalCheck: TEpiFieldProperties;
BEGIN
  Result := true;
  LocalCheck := CurField.FieldProperties;

  REPEAT
    {Check if a RESET command exists after JUMPS}
    CurCommand := FParser.GetToken(nwSameLine);
    IF CurCommand <> '' THEN
    BEGIN
      IF AnsiUpperCase(CurCommand) <> 'RESET' THEN
        Result := ReportError(Format(Lang(22830, 'RESET expected but %s found'), [CurCommand]))
      ELSE BEGIN
        LocalCheck.JumpResetValue := #32;
        CurCommand := FParser.GetToken(nwSameLine);
        IF Length(CurCommand) > 1 THEN
          Result := ReportError(Format(Lang(0, 'RESET character too long: %s'), [CurCommand]))
        ELSE
          LocalCheck.JumpResetValue := CurCommand;
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
        IF Not Result THEN ReportError(Lang(22722, 'Illegal datatype'));

        TmpS := Trim(CurCommand);

        {Get name of field to jump to}
        IF NOT FParser.EndOfLines THEN
          CurCommand := FParser.GetToken(nwSameLine)  // NextWord(nwSameLine)
        ELSE
          Result := ReportError(Lang(22724,'Jumps command without field to jump to'));  //'Jumps command without field to jump to'

        if (Pos(AnsiUpperCase(CurCommand), 'END WRITE SKIPNEXTFIELD') = 0) and
           (not FDf.FieldExists(CurCommand)) then
          Result := ReportError(Lang(22726, 'Unknown fieldname in JUMP block'));

        IF Result THEN
          LocalCheck.Jumps.AddObject(TmpS, TString.Create(CurCommand));
      END;
  UNTIL (Not Result);
END;

function TCheckFileIO.RetrieveCommentLegal(CurField: TEpiField): boolean;
var
  TmpStr, CurCommand: string;
  LocalValueLabel: TValueLabelSet;
  ComLegDF: TEpiDataFile;
  ValueField, TextField: TEpiField;
  i: Integer;

BEGIN
  {Four kinds of COMMENT LEGAL possible:
  1. COMMENT LEGAL
       1  ...
       2  ...
     END
     Name in ValueLabels has a $ in the end
     ValueLabelType=vlsLocal

  2. COMMENT LEGAL USE labelname
     ValueLabelType=vlsGlobal

  3. COMMENT LEGAL USE fieldname
     ValueLabelType=vlsLocal
     Current field (or command) has ValueLabelIsFieldRef = true

  4. COMMENT LEGAL datafilename
     ValueLabelType=vlsFile

  }
  Result := true;

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
    IF CurCommand = 'SHOW' THEN
      CurField.FieldProperties.ShowValueLabel := True;

    LocalValueLabel := TValueLabelSet.Create(CurField.FieldType);
    LocalValueLabel.LabelScope := vlsLocal;

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
      TmpStr := CurField.FieldName + '_lbl';
      if Assigned(FDf.ValueLabels.ValueLabelSetByName(TmpStr)) then
        FDf.ValueLabels.DeleteValueLabelSet(TmpStr);
      LocalValueLabel.Name := TmpStr;
      FDf.ValueLabels.AddValueLabelSet(LocalValueLabel);
    END ELSE
      FreeAndNil(LocalValueLabel);

    CurField.ValueLabelSet := LocalValueLabel;
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
       CurField.FieldProperties.ShowValueLabel := True;

    // Check is labels are compatible with current field
    // TODO : Compatability testing!!! (Eg. ftString and ftUpperAlfa could be compared!!!)
{    if not (LocalValueLabel.LabelType = CurField.FieldType) Then
    begin
      Result := ReportError(Lang(22710, 'Value is not compatible with this Fieldtype'));
      exit;
    end;       }

    // Check that if ValueLabel existed beforehand is is considered to be missingvalues.
    if Assigned(CurField.ValueLabelSet) then
    with CurField.ValueLabelSet do
    begin
      for i := 0 to Count -1 do
        LocalValueLabel.AddValueLabelPair(Values[i], Labels[i], MissingValues[i]);

      CurField.DataFile.ValueLabels.DeleteValueLabelSet(Name);
    end;

    CurField.ValueLabelSet := LocalValueLabel;
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
    CurField.FieldProperties.ShowValueLabel := True;
    
  TRY
    ComLegDf := TEpiDataFile.Create(0);
    ComLegDF.OnPassword := FDf.OnPassword;
    IF NOT ComLegDf.Open(TmpStr, [eoIgnoreRelates]) THEN
    begin
      Result := ReportError(Format(Lang(20108,'Datafile %s could not be opened'), [TmpStr]));
      Exit;
    end;

    IF ComLegDf.Size = 0 THEN
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
      CurField.ValueLabelSet := LocalValueLabel;
      Exit;
    End;

    // Todo -o Torsten: Index
    IF (ComLegDf.IndexFile.IndexCount < 2) THEN
    BEGIN
      Result := ReportError(Format(Lang(22832, 'Datafile %s must contain two KEY-fields'), [ComLegDf.Filename]));
    END;

    ValueField := ComLegDf.IndexFile.IndexFields[1];
    TextField  := ComLegDf.IndexFile.Indexfields[2];

    LocalValueLabel := TValueLabelSet.Create(ValueField.FieldType);
    LocalValueLabel.Name := CurCommand;
    LocalValueLabel.LabelScope := vlsFile;

    FOR i := 1 TO ComLegDf.Size DO
      LocalValueLabel.AddValueLabelPair(ValueField.AsValue[i], TextField.AsString[i]);

    FDf.ValueLabels.AddValueLabelSet(LocalValueLabel);

    CurField.ValueLabelSet := LocalValueLabel;

    FreeAndNil(ComLegDf);
  EXCEPT
    Result := ReportError(Format(Lang(22836, 'Datafile %s could not be applied as a comment legal.~This could be caused by low memory'), [ComLegDf.FileName]));
    FreeAndNil(ComLegDf);
  END;  //try..except   
end;

function TCheckFileIO.RetrieveType(CurField: TEpiField): Boolean;
VAR
  CurCommand: string;
  LocalCheck: TEpiFieldProperties;
  GlobalProperties: TEpiDataFileProperties;
  i, fc, color: integer;
BEGIN
  {Handles TYPE COMMENT, TYPE COMMENT fieldname, TYPE STATUSBAR}
  Result := true;
  LocalCheck := CurField.FieldProperties;

  CurCommand := FParser.GetToken(nwSameLine);
  IF CurCommand = '' THEN
  BEGIN
    Result := ReportError(Lang(22744, 'Illegal syntax in TYPE command'));
    Exit;
  END;

  IF AnsiUpperCase(CurCommand) = 'STATUSBAR' THEN
  BEGIN
    // Syntax: TYPE STATUSBAR "<text>" [color]
    GlobalProperties := CurField.DataFile.FileProperties;
    GlobalProperties.HasTypeStatusBar := true;
    GlobalProperties.TypeStatusBarField := CurField;

    CurCommand := FParser.GetToken(nwSameLine);
    GlobalProperties.TypeStatusBarText  := CurCommand;
    GlobalProperties.TypeStatusBarColor := ChkBaseColor;
    CurCommand := FParser.GetToken(nwSameLine);
    IF CurCommand <> '' THEN
    BEGIN
      CurCommand := AnsiUpperCase(CurCommand);
      FOR i := 0 TO High(ChkColorNames) DO
        IF CurCommand = ChkColorNames[i] THEN
          LocalCheck.TypeColour := i;
    END;
    Exit;
  END;


  IF AnsiUpperCase(CurCommand)='COMMENT' THEN
  BEGIN
    {Syntaxes: TYPE COMMENT
               TYPE COMMENT colour
               TYPE COMMENT fieldname
               TYPE COMMENT ALLFIELDS}
    LocalCheck.TypeColour := ChkBaseColor;
    LocalCheck.TypeType := ttComment;

    {Next word can be a fieldname, a colour or ALLFIELDS}
    {if not a fieldname then next word is interpreted as a colour}
    CurCommand := FParser.GetToken(nwSameLine);
    IF AnsiUpperCase(CurCommand) = 'ALLFIELDS' THEN
    BEGIN
      Color := ChkBaseColor;
      CurCommand := FParser.GetToken(nwSameLine);
      IF CurCommand <> '' THEN
      BEGIN
        CurCommand := AnsiUpperCase(CurCommand);
        FOR i := 0 TO High(ChkColorNames) DO
          IF CurCommand = ChkColorNames[i] THEN
            Color := i;
      END;

      fc := FDf.NumDataFields;
      for i := 0 to Fc -1 do
      with FDf.DataFields[i] do
      begin
        if not Assigned(ValueLabelSet) then continue;
        FieldProperties.TypeType   := ttComment;
        FieldProperties.TypeColour := color;
      end;
      Exit;
    END;

    //Is type comment fieldname?
    if FDf.FieldExists(CurCommand) then
    BEGIN
      LocalCheck.TypeType := ttField;
      LocalCheck.TypeField := FDf.FieldByName(CurCommand);
      Exit;
    END;

    //Is type comment colour?
    IF CurCommand <> '' THEN
    BEGIN
      CurCommand := AnsiUpperCase(CurCommand);
      FOR i := 0 TO High(ChkColorNames) DO
        IF CurCommand = ChkColorNames[i] THEN
          LocalCheck.TypeColour := i;
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
  CurCommand: string;
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
  CurCommand: string;
begin
  if not Assigned(CmdList) then exit;
  result := true; 
  REPEAT
    CurCommand := FParser.GetToken(nwAny);
    IF (AnsiUpperCase(CurCommand) <> 'END') and (Trim(CurCommand) <> '') THEN
      Result :=  Result and GetCommand(CurCommand, CmdList, CurField);
  UNTIL (AnsiUpperCase(CurCommand) = 'END') OR (FParser.EndOfLines);
end;

function TCheckFileIO.IsPosibleLetCmd(CurCommand: string; var TmpCmd: TChkCommand): Boolean;
var
  TmpStr: string;
  N: Integer;
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
     (not FDf.FileProperties.DefineExists(TmpStr)) THEN
  BEGIN
    Result := ReportError(Lang(22758, 'Unknown field- or variablename to the left of the equal-sign'));
    Exit;
  END;

  TmpCmd := TChkLet.Create();
  WITH TChkLet(TmpCmd) DO
  BEGIN
    VarIsField := FDf.FieldExists(TmpStr);
    VarName    := TmpStr;
    LetExpr    := trim(Copy(CurCommand, Pos('=', CurCommand) + 1, Length(CurCommand)));
  END;
end;

function TCheckFileIO.GetCommand(CurCommand: string; CmdList: TChkCommands; CurField: TEpiField): Boolean;
VAR
  TmpCmd: TChkCmdType;
  TmpChkCmd: TChkCommand;
  TmpField: TEpiField;
  TmpList, ValList: TStrings;
  TmpStr, TmpStr2: string;
  Dummy: Boolean;
  N, I, J: Integer;
  TmpColor: Byte;
  TmpValueLabel: TValueLabelSet;
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

  TmpChkCmd := nil;

  try
    IF CurCommand[1] = '*' THEN
      TmpCmd := cmdComment
    ELSE BEGIN
      TmpCmd := cmdIF;
      WHILE (TmpCmd < cmdLET) AND
            (ChkCmdNames[TmpCmd] <> AnsiUpperCase(CurCommand)) DO
        TmpCmd := Succ(TmpCmd);
      IF (ChkCmdNames[TmpCmd] <> AnsiUpperCase(CurCommand)) or
         (AnsiUpperCase(CurCommand) = 'LET') THEN
        Result := IsPosibleLetCmd(CurCommand, TmpChkCmd);
      if not Result then
        Exit;
    END;  //else

    CASE TmpCmd OF
      cmdIF:
        BEGIN
          TmpChkCmd := TChkIf.Create;
          REPEAT
            CurCommand := FParser.GetToken(nwSameKeepQuotes);
            TmpStr := TmpStr + ' ' + CurCommand;
          UNTIL (AnsiUpperCase(CurCommand) = 'THEN') or (CurCommand = '');
          IF AnsiUpperCase(CurCommand) = 'THEN' THEN
          BEGIN
            TmpStr := TmpStr + ' ';
            Delete(TmpStr, Pos(' THEN ', AnsiUpperCase(TmpStr)), 6);
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
          TChkIf(TmpChkCmd).Expr := '(' + trim(TmpStr) + ')';
          Dummy := false;
          REPEAT
            CurCommand := FParser.GetToken(nwAny);
            IF AnsiUpperCase(CurCommand) = 'ELSE' THEN
              Dummy := True;
            If Dummy then GetCommand(CurCommand, TChkIf(TmpChkCmd).ElseCmds, CurField)
            ELSE GetCommand(CurCommand, TChkIf(TmpChkCmd).IfCmds, CurField);
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

          TmpChkCmd := TChkHelp.Create;
          TChkHelp(TmpChkCmd).Text    := CurCommand;
          TChkHelp(TmpChkCmd).MsgType := 2; // mtInformation;
          TChkHelp(TmpChkCmd).Keys    := '';
          CurCommand := FParser.GetUpperToken(nwSameLine); 
          IF CurCommand <> '' THEN
          BEGIN
            IF (Copy(CurCommand, 1, 6) = 'KEYS="') AND (Length(CurCommand) > 7) AND
               (CurCommand[Length(CurCommand)] = '"') THEN
            BEGIN
              TmpStr := Copy(CurCommand, 7, Length(CurCommand)-7);
              TChkHelp(TmpChkCmd).Keys := Copy(TmpStr, 1, 10);
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
            TChkHelp(TmpChkCmd).MsgType := 1 // mtError
          ELSE
            IF (TmpStr = 'TYPE=WARNING') OR (TmpStr = 'TYPE=W') THEN
              TChkHelp(TmpChkCmd).MsgType := 0 // mtWarning
          ELSE
            IF (TmpStr = 'TYPE=CONFIRMATION') OR (TmpStr = 'TYPE=C') THEN
              TChkHelp(TmpChkCmd).MsgType := 3; // mtConfirmation;
        END;  //case cmdHelp
      cmdHide, cmdUnhide, cmdClear, cmdGoto:
        BEGIN
          {Check if a fieldname exists after command}
          CurCommand:=FParser.GetUpperToken(nwSameLine);

          TmpChkCmd := TChkFieldReferer.Create(TmpCmd);

          // Case: (Un)Hide/Goto/Clear <empty> = current field
          IF CurCommand = '' THEN
            TChkFieldReferer(TmpChkCmd).VarName := CurField.FieldName

          // Case: GOTO [WRITE | WRITEREC]
          ELSE IF ((CurCommand = 'WRITE') OR (CurCommand = 'WRITEREC')) AND
             (TChkFieldReferer(TmpChkCmd).CmdType = cmdGoto) THEN
            TChkFieldReferer(TmpChkCmd).VarName := 'WRITE'

          // Case: (Un)Hide/Goto/Clear [COMMENT [LEGAL]]
          ELSE IF (NOT FDf.FieldExists(CurCommand)) AND (CurCommand = 'COMMENT') THEN
          BEGIN
            CurCommand := FParser.GetUpperToken(nwSameLine);
            IF CurCommand = 'LEGAL' THEN
              TChkFieldReferer(TmpChkCmd).VarName := '$$COMLEG';
          END

          ELSE IF (NOT FDf.FieldExists(CurCommand)) THEN
          BEGIN
            Result := ReportError(Lang(22708, 'Unknow fieldname.'));
            Exit;
          END

          ELSE
            TChkFieldReferer(TmpChkCmd).VarName   := CurCommand;
        END;  //case cmdHide or cmdUnhide
      cmdComLegal:
        BEGIN
          TmpChkCmd := TChkComLegal.Create;
          TmpField := TEpiField.Create();
          if Assigned(CurField) then
            TmpField.FieldLength := CurField.FieldLength
          else
            TmpField.FieldLength := 10; // Just give a length, 10 is hopefully enough in most cases.
          Result := RetrieveCommentLegal(TmpField);
          if Result then
          begin
            TChkComLegal(TmpChkCmd).ShowList := false;
            if Assigned(TmpField.FieldProperties) then
              TChkComLegal(TmpChkCmd).ShowList := TmpField.FieldProperties.ShowValueLabel;
            TChkComLegal(TmpChkCmd).ValueLabelIsFieldRef := TmpField.ValueLabelIsFieldRef;
            TChkComLegal(TmpChkCmd).ValueLabel := TmpField.ValueLabelSet;
          end;
          FreeAndNil(TmpField);
          Exit;
        END;  //case cmdComLegal
      cmdComment:
        BEGIN
          TmpChkCmd := TChkComment.Create();
          IF Length(CurCommand) > 200 THEN CurCommand := Copy(CurCommand, 1, 200);
          TChkComment(TmpChkCmd).Comment := CurCommand;
        END;
      cmdDefine:
        BEGIN
          TmpChkCmd := TChkDefine.Create();
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

          TChkDefine(TmpChkCmd).FieldName := CurCommand;
          TChkDefine(TmpChkCmd).NumDecimals := 0;

          //Variable name passed all tests - now get the Fieldtype
          CurCommand := FParser.GetUpperToken(nwSameLine);
          IF CurCommand='' THEN
          BEGIN
            Result := ReportError(Lang(22774,'Fieldtype missing in DEFINE command'));
            Exit;
          END;

          // TODO -o Torsten : Define common platform for finding variable types
          //                   - combine with QES handling!
          TChkDefine(TmpChkCmd).Length := Length(CurCommand);
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
              IF (n > 0) OR (Length(CurCommand) > MaxIntegerLength) THEN
                TChkDefine(TmpChkCmd).FieldType := ftFloat
              ELSE
                TChkDefine(TmpChkCmd).FieldType := ftInteger;
              IF n > 0 THEN
                TChkDefine(TmpChkCmd).NumDecimals := Length(CurCommand) - Pos('.', CurCommand)
              ELSE
                TChkDefine(TmpChkCmd).NumDecimals := 0;
            END;
          END  //if numeric
          ELSE IF CurCommand[1] = '_' THEN TChkDefine(TmpChkCmd).FieldType := ftString
          ELSE IF CurCommand = '<MM/DD/YYYY>' THEN TChkDefine(TmpChkCmd).FieldType := ftDate
          ELSE IF Copy(CurCommand, 1, 2) = '<A' THEN
            BEGIN
              TChkDefine(TmpChkCmd).FieldType := ftUpperAlfa;
              TChkDefine(TmpChkCmd).Length := Length(CurCommand) - 2;
            END
          ELSE IF Copy(Curcommand, 1, 2) = '<S' THEN
            BEGIN
              TChkDefine(TmpChkCmd).FieldType := ftSoundex;
              TChkDefine(TmpChkCmd).Length := Length(CurCommand)-2;
            END
          ELSE IF CurCommand = '<Y>' THEN TChkDefine(TmpChkCmd).FieldType := ftBoolean
          ELSE IF CurCommand = '<DD/MM/YYYY>' THEN TChkDefine(TmpChkCmd).FieldType := ftEuroDate
          ELSE IF CurCommand = '<YYYY/MM/DD>' THEN TChkDefine(TmpChkCmd).FieldType := ftYMDDate
          ELSE BEGIN
            //No legal Fieldtype found
            Result := ReportError(Lang(22778, 'Illegal Fieldtype in DEFINE command'));
            Exit;
          END;

          CurCommand := FParser.GetUpperToken(nwSameLine);
          IF CurCommand = '' THEN TChkDefine(TmpChkCmd).Scope := scLocal
          ELSE IF CurCommand[1] = 'G' THEN TChkDefine(TmpChkCmd).Scope := scGlobal
          ELSE IF CurCommand[1] = 'C' THEN TChkDefine(TmpChkCmd).Scope := scCumulative
          ELSE BEGIN
            Result := ReportError(Lang(22780, 'Illegal scope in DEFINE command. Use GLOBAL or CUMULATIVE'));
            Exit;
          END;

          // All data concerning the DEFINE is read
          // Now check is DEF-name is allready used
          // Ignore the DEF if DEF is global and a global def-field with the
          // same Fieldtype exists
          TmpField := FDf.FileProperties.DefineByName(TChkDefine(TmpChkCmd).FieldName);
          IF Assigned(TmpField) THEN
          BEGIN
            //a DEF-var with same name exists
            IF (TChkDefine(TmpChkCmd).Scope <> scGlobal) OR (TmpField.FieldProperties.FieldScope <> scGlobal) THEN
            BEGIN
              Result := ReportError(Lang(22772,'Dublicate name: The variablename is allready used'));
              Exit;
            END;

            IF (TChkDefine(TmpChkCmd).Scope = scGlobal) AND (TmpField.FieldProperties.FieldScope = scGlobal) THEN
            BEGIN
              IF NOT ((TChkDefine(TmpChkCmd).FieldType = TmpField.Fieldtype) AND
                      (TChkDefine(TmpChkCmd).Length = TmpField.FieldLength) AND
                      (TChkDefine(TmpChkCmd).NumDecimals = TmpField.FieldDecimals)) THEN
              BEGIN
                Result := ReportError(Lang(22773,'A global DEFINE with same fieldname but different Fieldtype or length is allready defined'));
                Exit;
              END;
            END;
          END;
          IF not Assigned(TmpField) THEN
            TmpField := TEpiField.CreateField(TChkDefine(TmpChkCmd).FieldType, 1);
          TmpField.FieldName   := TChkDefine(TmpChkCmd).FieldName;
          TmpField.FieldLength := TChkDefine(TmpChkCmd).Length;
          TmpField.FieldDecimals := TChkDefine(TmpChkCmd).NumDecimals;
          TmpField.IsMissing[1] := true;
          TmpField.FieldProperties.FieldScope := TChkDefine(TmpChkCmd).Scope;
          Fdf.FileProperties.AddDefine(TmpField);
        END;  //case cmdDefine.
      cmdAutosave: FDf.FileProperties.Autosave := True;
      cmdConfirm:  FDf.FileProperties.Confirm  := True;
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

            for n:=0 TO FDf.NumFields - 1 DO
            BEGIN
              IF (FDf[n].FieldType in [ftInteger, ftString, ftUpperAlfa, ftFloat, ftCrypt]) THEN
              BEGIN
                IF (TmpStr = 'ALL') THEN
                  FDf[n].DefaultValue := CurCommand
                ELSE
                  IF (FDf[n].Fieldtype in [ftString, ftUpperAlfa, ftCrypt]) AND
                     ((TmpStr='ALLSTRINGS') OR (TmpStr='ALLSTRING')) THEN FDf[n].DefaultValue := CurCommand
                ELSE
                  IF (FDf[n].Fieldtype in [ftInteger, ftFloat]) AND
                     (TmpStr='ALLNUMERIC') THEN FDf[n].DefaultValue := CurCommand;
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
            TmpStr2 := TmpList[TmpList.Count - 1];

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

                FOR i := FDf.FieldIndex(TmpStr) TO FDf.FieldIndex(CurCommand) DO
                  IF (FDf[i].FieldType <> ftQuestion) THEN
                    FDf[i].DefaultValue := TmpStr2;
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
                  TmpField.DefaultValue := TmpStr2;
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
                FDf.FileProperties.GlobalMissingVal[i] := CurCommand;
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

            TmpValueLabel := TValueLabelSet.Create;
            TmpValueLabel.Name := '__missingall' + IntToStr(Random(1000));
            for i := 0 to ValList.Count - 1 do
              TmpValueLabel.AddValueLabelPair(StrToInt(ValList[i]), '', True);
            FDf.ValueLabels.AddValueLabelSet(TmpValueLabel);

            FOR n := 0 TO TmpList.Count-(ValList.Count + 1) DO
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

                FOR i := FDf.FieldIndex(TmpStr) TO FDf.FieldIndex(CurCommand) DO
                BEGIN
                  TmpField := FDf[i];
                  IF (TmpField.FieldType <> ftQuestion) THEN
                  begin
                    if Assigned(TmpField.ValueLabelSet) then
                    begin
                      for j := 0 to TmpValueLabel.Count - 1 do
                        TmpField.ValueLabelSet.AddValueLabelPair(TmpValueLabel.Values[0], '', true);
                    end else
                      TmpField.ValueLabelSet := TmpValueLabel;
                  end;
                END;  //for
                //if interval
              END ELSE BEGIN
                TmpField := FDf.FieldByName(TmpList[n]);
                IF not Assigned(TmpField) THEN
                BEGIN
                  Result := ReportError(Lang(22708, 'Unknown field name') + ' ' + TmpList[n]);
                  Exit;
                END;
                IF (TmpField.FieldType <> ftQuestion) THEN
                begin
                  if Assigned(TmpField.ValueLabelSet) then
                  begin
                    for j := 0 to TmpValueLabel.Count - 1 do
                      TmpField.ValueLabelSet.AddValueLabelPair(TmpValueLabel.Values[0], '', true);
                  end else
                    TmpField.ValueLabelSet := TmpValueLabel;
                end;
              end;
            END;  //for
          FINALLY
            FreeAndNil(ValList);
            FreeAndNil(TmpList);
          END;  //try..finally
        END;  //case cmdMissingAll
      cmdIgnoreMissing: FDf.FileProperties.MissingAction := maIgnoreMissing;
      cmdTypeString:
        BEGIN
          {  Syntax: TYPE "text" [colour]  }
          TmpChkCmd := TChkTypeStr.Create();
          CurCommand := FParser.GetToken(nwSameLine);
          IF AnsiUpperCase(CurCommand)='COMMENT' THEN
          BEGIN
            CurCommand:=FParser.GetUpperToken(nwSameLine);
            IF CurCommand <> 'ALLFIELDS' THEN
            BEGIN
              Result := ReportError(Lang(22741, 'Command not legal in IF, AFTER ENTRY, and BEFORE ENTRY blocks'));
              Exit;
            END;

            FDf.FileProperties.GlobalTypeCom := True;
            TChkTypeStr(TmpChkCmd).Text := '¤¤typecommentlegalallfields¤¤';
            TChkTypeStr(TmpChkCmd).VarNumber := -1;
            CurCommand := FParser.GetUpperToken(nwSameLine);
            IF CurCommand<>'' THEN
            BEGIN
              FOR i := 0 TO High(ChkColorNames) DO
                IF CurCommand = ChkColorNames[i] THEN
                  FDf.FileProperties.GlobalTypeComColor := i;
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
          TChkTypeStr(TmpChkCmd).Text := CurCommand;

          //Get a colour - if present
          CurCommand := FParser.GetToken(nwSameLine);  //  NextWord(nwSameLine);
          TChkTypeStr(TmpChkCmd).Color := 0;
          IF CurCommand<>'' THEN
          BEGIN
            FOR i := 0 TO High(ChkColorNames) DO
              IF CurCommand = ChkColorNames[i] THEN
                TChkTypeStr(TmpChkCmd).Color := i;

            {Read rest of line - compatibility with Epi Info}
            REPEAT
              CurCommand := FParser.GetUpperToken(nwSameLine);
            UNTIL CurCommand='';
          END;
        END;  //case cmdTypeString
      cmdBeep:
        BEGIN
          TmpChkCmd := TChkBeep.Create;
          CurCommand := FParser.GetUpperToken(nwSameLine);
          TChkBeep(TmpChkCmd).BeepType := btStandard;
          IF CurCommand<>'' THEN
          BEGIN
            IF (CurCommand = 'WARNING') OR (CurCommand = 'W') THEN
              TChkBeep(TmpChkCmd).BeepType := btWarning
            ELSE IF (CurCommand = 'CONFIRMATION') OR (CurCommand = 'C') THEN
              TChkBeep(TmpChkCmd).BeepType := btConfirmation;
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
          TmpChkCmd := TChkWriteNote.Create;
          CurCommand := FParser.GetToken(nwSameLine);
          IF Length(CurCommand) > 200 THEN CurCommand := Copy(CurCommand, 1, 200);
          CurCommand := StringReplace(CurCommand, '\n', #13#10, [rfReplaceAll, rfIgnoreCase]);
          TChkWriteNote(TmpChkCmd).Note := CurCommand;
          CurCommand := FParser.GetUpperToken(nwSameLine);
          IF CurCommand='SHOW' THEN
            TChkWriteNote(TmpChkCmd).ShowNotes := True
          ELSE
            TChkWriteNote(TmpChkCmd).ShowNotes := False;
        END;
      cmdCopyToClipboard:
        BEGIN
          {Syntax: COPYTOCLIPBOARD "text @variable"}
          TmpChkCmd := TChkClpBrd.Create;
          CurCommand := FParser.GetToken(nwSameLine);
          IF CurCommand='' THEN
          BEGIN
            Result := ReportError(Lang(23028, 'Invalid parameters'));
            Exit;
          END;
          TChkClpBrd(TmpChkCmd).Text := Copy(CurCommand, 1, 200);
        END;
      cmdShowLastRecord:
        BEGIN
          Fdf.FileProperties.ShowLastRecord := True;
        END;
      cmdExecute:
        BEGIN
          {Syntax: EXECUTE "exe-file name"|* "Parameters"|* NOWAIT|WAIT [HIDE]   }
          {
            Execute bla.htm WAIT
            Execute opera bla.htm WAIT
          }
          TmpChkCmd := TChkExec.Create;
          CurCommand := FParser.GetToken(nwSameLine);
          IF CurCommand='' THEN
          BEGIN
            Result := ReportError(Lang(22854,'Exe-filename or document-filename is required'));
            Exit;
          END;
          TChkExec(TmpChkCmd).CmdLine := CurCommand;

          //Read next: can be parameters or NOWAIT|WAIT
          CurCommand := FParser.GetToken(nwSameLine);
          TChkExec(TmpChkCmd).Params := '';
          IF (AnsiUpperCase(CurCommand)<>'WAIT') AND (AnsiUpperCase(CurCommand)<>'NOWAIT') THEN
          BEGIN
            //Assume CurCommand contains parameter(s)
            TChkExec(TmpChkCmd).Params := CurCommand;
            CurCommand := FParser.GetToken(nwSameLine);
          END;

          CurCommand := AnsiUpperCase(CurCommand);
          IF (CurCommand<>'WAIT') AND (CurCommand<>'NOWAIT') THEN
          BEGIN
            Result := ReportError(Lang(22856, 'WAIT or NOWAIT is required'));
            Exit;
          END;

          TChkExec(TmpChkCmd).Wait := (CurCommand = 'WAIT');

          CurCommand := FParser.GetUpperToken(nwSameLine);
          TChkExec(TmpChkCmd).Hide := (CurCommand = 'HIDE');
        END;
      cmdColor:
        BEGIN
          {Syntax: COLOR QUESTION colors
                   COLOR DATA colors
                   COLOR BACKGROUND color
                   COLOR fieldname datacolors questioncolors

                   Colors can be Epi Info color codes
                   or EpiData color words}
          TmpChkCmd := TChkColor.Create;
          CurCommand := FParser.GetUpperToken(nwSameLine);
          TChkColor(TmpChkCmd).TxtColor := 0;
          TChkColor(TmpChkCmd).BgColor := 0;
          IF CurCommand='QUESTION' THEN TChkColor(TmpChkCmd).ColorCmd:=1
          ELSE IF CurCommand='DATA' THEN TChkColor(TmpChkCmd).ColorCmd:=2
          ELSE IF CurCommand='BACKGROUND' THEN TChkColor(TmpChkCmd).ColorCmd:=3
          ELSE
            BEGIN
              //could be COLOR fieldname
              //will be added later
              Result := ReportError(Lang(22858, 'Unknown COLOR command'));
              Exit;
            END;

          IF TChkColor(TmpChkCmd).ColorCmd=3 THEN
          BEGIN
            //command is BACKGROUND
            CurCommand := FParser.GetUpperToken(nwSameLine);
            IF IsInteger(CurCommand) THEN
            BEGIN
              TChkColor(TmpChkCmd).IsEpiInfoNo := True;
              n := StrToInt(CurCommand);
              IF (n<0) OR (n>7) THEN
              BEGIN
                Result := ReportError(Lang(22860, 'Illegal COLOR number'));   //22860=Illegal COLOR number
                Exit;
              END;
              TChkColor(TmpChkCmd).BgColor := n;
              Exit;
            END;

            TChkColor(TmpChkCmd).IsEpiInfoNo := False;
            FOR i := 0 TO High(ChkColorNames) DO
              IF CurCommand = ChkColorNames[i] THEN
                TChkColor(TmpChkCmd).BgColor := i;
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
            TChkColor(TmpChkCmd).IsEpiInfoNo := True;
            n := StrToInt(TmpList[0]);
            IF n > 255 THEN
            BEGIN
              Result := ReportError(Lang(22862, 'Unknown color in COLOR command'));
              Exit;
            END;
            n := n AND $7F;  //clear first bit which indicates flashing text in epi info
            TChkColor(TmpChkCmd).BgColor := (n AND $F0) SHR 4;
            TChkColor(TmpChkCmd).TxtColor := (n AND $0F);
            Exit;
          end;

          For n := 0 to TmpList.Count do
          begin
            FOR i := 0 TO High(ChkColorNames) DO
              IF CurCommand = ChkColorNames[i] THEN
                TmpColor := i;
            case n of
              0: TChkColor(TmpChkCmd).TxtColor := TmpColor;
              1: TChkColor(TmpChkCmd).BgColor  := TmpColor;
              2: Begin
                  FDf.FileProperties.FieldHighlightAct := True;
                  FDf.FileProperties.FieldHighlightCol := TmpColor;
                 End;
            end;
          end;
        END;
      cmdBackup:
        BEGIN
          {syntax: BACKUP "destination-library" [ZIP filename [date]]
           or      BACKUP "destination-library" [ENCRYPT filname password [date]] }
          TmpChkCmd := TChkBackup.Create;
          IF (CmdList <> FDf.FileProperties.AfterFileCmds) THEN
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
          TChkBackup(TmpChkCmd).Zip     := False;
          TChkBackup(TmpChkCmd).Encrypt := False;
          TChkBackup(TmpChkCmd).DestLib := CurCommand;
          CurCommand                    := FParser.GetUpperToken(nwSameLine);

          IF (CurCommand <> 'ZIP') AND (CurCommand <> 'ENCRYPT') THEN
          BEGIN
            FDf.FileProperties.BackupList.Append(TChkBackup(TmpChkCmd).DestLib);
            FDf.FileProperties.BackupList.Append(FDf.Filename);
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

            FDf.FileProperties.BackupList.Append(TChkBackup(TmpChkCmd).DestLib);
            TChkBackup(TmpChkCmd).Zip    := True;
            TChkBackup(TmpChkCmd).Filename := ExtractFilename(CurCommand);
            CurCommand                     := FParser.GetUpperToken(nwSameLine);   //get date parameter
            IF CurCommand = 'DATE' THEN TChkBackup(TmpChkCmd).AddDate := True;
            Exit;
          END;

          //encrypt it
          CurCommand := FParser.GetToken(nwSameLine);   //get the filename
          IF CurCommand='' THEN
          BEGIN
            Result := ReportError(Lang(22884, 'Filename needed after ZIP'));
            Exit;
          END;

          TChkBackup(TmpChkCmd).Encrypt := True;
          TChkBackup(TmpChkCmd).Filename  := ExtractFilename(CurCommand);
          CurCommand                      := FParser.GetToken(nwSameLine);   //get the password
          IF CurCommand='' THEN
          BEGIN
            Result := ReportError(Lang(22888, 'Password must follow ENCRYPT and filename'));
            Exit;
          END;

          FDf.FileProperties.BackupList.Append(TChkBackup(TmpChkCmd).DestLib);
          TChkBackup(TmpChkCmd).Password := CurCommand;
          CurCommand                     := FParser.GetUpperToken(nwSameLine);  //get date parameter
          IF CurCommand = 'DATE' THEN TChkBackup(TmpChkCmd).AddDate := True;
        END;  //end case cmdBackup
      cmdRelate:
        BEGIN
          //Syntax: RELATE fieldname filename [1]
          //Get fieldname
          TmpChkCmd := TChkRelate.Create;
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
          TChkRelate(TmpChkCmd).RelField := TmpField.FieldName;  //save fieldname

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

          TChkRelate(TmpChkCmd).RelFileStr := ExtractFileName(CurCommand);

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
          TChkRelate(TmpChkCmd).One2One := (CurCommand = '1');
//          AInfo^.One2One:=tmpCmdRec.One2One;
        END;   //case cmdRelate
    else
      if not ((TmpCmd = cmdLet) and result) then
        TmpChkCmd := TChkOther.Create(TmpCmd);
    END;  //Case
  finally
    if Result then
      CmdList.AddCommand(TmpChkCmd)
    else
      if Assigned(TmpChkCmd) then FreeAndNil(TmpChkCmd);
  end;
end;

function TCheckFileIO.AddFieldComment(CurField: TEpiField): boolean;
var
  S: string;
begin
  S := FParser.GetLineAndFlush;
  CurField.FieldProperties.FieldComments.Append(FParser.GetWholeLine);
  result := true;
end;

function TCheckFileIO.RetrieveLabelBlock(): Boolean;
var
  CurCommand: string;
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
  TmpStr: string;
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

  aValueLabelSet := TValueLabelSet.Create;
  aValueLabelSet.Name := trim(CurCommand);
  aValueLabelSet.LabelScope := vlsGlobal;

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

    aValueLabelSet.LabelType := FindFieldType(TmpStr, aValueLabelSet.LabelType);
    // TODO : Change TmpStr to correct type of variant (now it's string.)
    aValueLabelSet.AddValueLabelPair(TmpStr, CurCommand);
  End;
  FDf.ValueLabels.AddValueLabelSet(aValueLabelSet);
  Result := true;
END;  //TCheckObj.RetrieveLabel

function TCheckFileIO.RetrieveAssertBlock(): boolean;
{Reads the CONSISTENCYBLOCK..END block - and ignores it...}
var
  CurCommand: string;
BEGIN
  Result := true;
  REPEAT
    CurCommand := FParser.GetUpperToken(nwAny);
    IF (CurCommand = 'CHECK') OR (CurCommand = 'REPORT') THEN
      FDf.FileProperties.AssertList.Append(FParser.GetWholeLine);
  UNTIL (CurCommand = 'END') OR (FParser.EndOfLines);
END;

function TCheckFileIO.AddTopComment(): boolean;
VAR
  s: string;
BEGIN
  s := FParser.GetWholeLine;
  IF Assigned(FDf.FileProperties.TopComments) THEN
    FDf.FileProperties.TopComments.Append(s);
  Result := true;
END;  //Procedure AddTopComment


function TCheckFileIO.RetrieveFlawBlock(): Boolean;
var
  CurCommand: string;
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

procedure TCheckFileIO.AddToCheckLines(Const S: string);
var
  T: string;
begin
  T := DupeString(' ', FIndentLvl * 2);
  FCheckLines.Append(T + S);
end;

procedure TCheckFileIO.AddStringsToCheckLines(Const Strings: TStrings);
var
  T: string;
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

  if aValueLabelSet.LabelScope = vlsGlobal then
    AddToCheckLines('LABEL ' + aValueLabelSet.Name);

  Inc(FIndentLvl);
  for i := 0 to aValueLabelSet.Count - 1do
  with aValueLabelSet do
  BEGIN
    if MissingValues[i] then continue;

    S := '';
    if Pos(' ', Values[i]) > 0 then
      S := S + '"' + Values[i] + '"'
    else
      S := S + Values[i];

    if Pos(' ', Labels[i]) > 0 then
      S := S + '  "' + Labels[i] + '"'
    else
      S := S + '  ' + Labels[i];
    AddToCheckLines(S);
  END;  //for
  Dec(FIndentLvl);
  if aValueLabelSet.LabelScope = vlsGlobal then
    AddToCheckLines('END');
end;


function TCheckFileIO.AddCommandList(CmdList: TChkCommands): Boolean;
VAR
  i, j, n: integer;
  s: string;
  LocalVltType: TValueLabelSetScope;
  Cmd: TChkCommand;
BEGIN
  // Sanity check;
  IF CmdList = NIL THEN Exit;
  IF CmdList.Count = 0 THEN Exit;

  FOR j := 0 TO CmdList.Count - 1 DO
  BEGIN
    Cmd := CmdList[j];
    CASE Cmd.CmdType OF
      cmdIF:
        BEGIN
          AddToCheckLines('IF ' + trim(TChkIf(cmd).Expr) + ' THEN');
          Inc(FIndentLvl);
          AddCommandList(TChkIf(cmd).IfCmds);
          Dec(FIndentLvl);
          IF TChkIf(cmd).ElseCmds.Count > 0 THEN
          BEGIN
            AddToCheckLines('ELSE');
            Inc(FIndentLvl);
            AddCommandList(TChkIf(cmd).ElseCmds);
            Dec(FIndentLvl);
          END;
          AddToCheckLines('ENDIF');
        END;  //case cmdIF
      cmdHelp:
        BEGIN
          S := '"' + TChkHelp(cmd).Text + '"';
          S := StringReplace(S, #13#10, '\n', [rfReplaceAll, rfIgnoreCase]);
          IF trim(TChkHelp(cmd).Keys) <> '' THEN
            S := S + ' KEYS="' + trim(AnsiUpperCase(TChkHelp(cmd).Keys)) + '"';
          CASE TChkHelp(cmd).MsgType OF
            1 {mtError}:        S := S + ' TYPE=ERROR';
            0 {mtWarning}:      S := S + ' TYPE=WARNING';
            3 {mtConfirmation}: S := S + ' TYPE=CONFIRMATION';
          END;
          AddToCheckLines('HELP ' + S);
        END;  //case cmdHelp
      cmdWriteNote:
        BEGIN
          S := '"' + TChkWriteNote(cmd).Note + '"';
          S := StringReplace(S, #13#10, '\n', [rfReplaceAll, rfIgnoreCase]);
          IF TChkWriteNote(cmd).ShowNotes THEN S := S + ' SHOW';
          AddToCheckLines('WRITENOTE ' + S);
        END;  //case cmdWriteNote
      cmdCopyToClipboard:
        AddToCheckLines('COPYTOCLIPBOARD "' + TChkClpBrd(cmd).Text + '"');
      cmdHide:
        AddToCheckLines('HIDE' + ' ' + TChkFieldReferer(cmd).VarName);
      cmdUnHide:
        AddToCheckLines('UNHIDE' + ' ' + TChkFieldReferer(cmd).VarName);
      cmdClear:
        BEGIN
          S := 'CLEAR';
          IF TChkFieldReferer(cmd).VarName='$$COMLEG' THEN
            S := S + ' COMMENT LEGAL'
          ELSE
            S := S + ' ' + TChkFieldReferer(cmd).VarName;
          AddToCheckLines(S);
        END;  //case cmdClear
      cmdGoto:
        AddToCheckLines('GOTO ' + TChkFieldReferer(cmd).VarName);
      cmdExit:
        AddToCheckLines('EXIT');
      cmdQuit:
        AddToCheckLines('QUIT');
      cmdTypeString:
        BEGIN
          IF TChkTypeStr(cmd).Text = '¤¤typecommentlegalallfields¤¤' THEN
          BEGIN
            S := 'TYPE COMMENT ALLFIELDS';
            IF FDf.FileProperties.GlobalTypeComColor <> 0 THEN
              For i := 0 to High(ChkColorNames) do
                if i = FDf.FileProperties.GlobalTypeComColor then
                  S := S + ' ' + ChkColorNames[i];
          END ELSE BEGIN
            S := 'TYPE "' + TChkTypeStr(cmd).Text + '"';
            IF TChkTypeStr(cmd).Color <> 2 THEN
              For i := 0 to High(ChkColorNames) do
                if i = TChkTypeStr(cmd).Color then
                  S := S + ' ' + ChkColorNames[i];
          END;
          AddToCheckLines(S);
        END;
      cmdBackup:
        begin
          S := 'BACKUP ' + TChkBackup(cmd).DestLib;
          if TChkBackup(cmd).Zip or TChkBackup(cmd).Encrypt then
          begin
            if TChkBackup(cmd).Encrypt then
              S := S + ' ENCRYPT ';
            if TChkBackup(cmd).Zip then
              S := S + ' ZIP ';
            S := S + TChkBackup(cmd).Filename;
            if TChkBackup(cmd).Encrypt then
              S := S + ' ' + TChkBackup(cmd).Password;
            if TChkBackup(cmd).AddDate then
              S := S + ' DATE';
          end;
          AddToCheckLines(S);
        end;
      cmdLoad:
        BEGIN
          S := TChkLoadLib(cmd).LibName;
          IF pos(' ', S) > 0 THEN S := '"' + S + '"';
          AddToCheckLines('LOAD ' + S);
        END;
      cmdExecute:
        BEGIN
          IF pos(' ', TChkExec(cmd).CmdLine) > 0 THEN
            S := 'EXECUTE ' + '"' + TChkExec(cmd).CmdLine + '"'
          ELSE
            S := 'EXECUTE ' + TChkExec(cmd).CmdLine;
          IF TChkExec(cmd).Params <> '' THEN
          BEGIN
            IF pos(' ', TChkExec(cmd).Params) > 0 THEN
              S := S + ' "' + TChkExec(cmd).Params + '"'
            ELSE
              S := S + ' ' + TChkExec(cmd).Params;
          END;
          IF TChkExec(cmd).Wait THEN
            S := S + ' WAIT'
          ELSE
            S := S + ' NOWAIT';
          IF TChkExec(cmd).Hide THEN
            S := S + ' HIDE';
          AddToCheckLines(S);
        END;
      cmdBeep:
        BEGIN
          S := 'BEEP';
          IF TChkBeep(cmd).Beeptype = btWarning THEN S := S + ' Warning';
          IF TChkBeep(cmd).BeepType = btConfirmation THEN S := S + ' Confirmation';
          AddToCheckLines(S);
        END;
      cmdRelate:
        BEGIN
          S := 'RELATE ' + trim(TChkRelate(cmd).RelField) + ' ';
          IF Pos(' ',TChkRelate(cmd).RelFileStr) > 0 THEN
            S := S + '"' + TChkRelate(cmd).RelFileStr + '"'
          ELSE
            S := S + TChkRelate(cmd).RelFileStr;
          IF TChkRelate(cmd).One2One THEN
            S := S + ' 1';
          AddToCheckLines(S);
        END;
      cmdComLegal:
        BEGIN
          IF Assigned(TChkComLegal(cmd).ValueLabel) THEN
          BEGIN
            LocalVltType := TChkComLegal(cmd).ValueLabel.LabelScope;
            if (LocalVltType = vlsLocal) and (TChkComLegal(cmd).ValueLabelIsFieldRef) then
             LocalVltType := vlsGlobal;
            case LocalVltType of
              vlsLocal:
                begin
                  S := 'COMMENT LEGAL';
                  if TChkComLegal(cmd).ShowList then S := S + ' SHOW';
                  AddToCheckLines(S);
                  // Indetation is done in LabelToText;
                  LabelToText(TChkComLegal(cmd).ValueLabel);
                  AddToCheckLines('END');
                end;
              vlsGlobal, vlsFile:
                begin
                  S := 'COMMENT LEGAL ';
                  if TChkComLegal(cmd).ValueLabel.LabelScope <> vlsFile then
                    S := S + 'USE ';
                  S := S + TChkComLegal(cmd).ValueLabel.Name;
                  if TChkComLegal(cmd).ShowList then S := S + ' SHOW';
                  AddToCheckLines(S);
                end;
            end;  //case
          END;  //if valuelabel<>NIL
        END;  //case cmdComLegal
      cmdLet:
        BEGIN
          S := trim(TChkLet(cmd).LetExpr);
          S := StringReplace(S, '_M', ' .', [rfReplaceAll, rfIgnoreCase]);
          S := TChkLet(cmd).VarName + ' = ' + S;
          IF TChkLet(cmd).CodedWithLet THEN
            S := 'LET ' + S;
          AddToCheckLines(S);
        END;  //case cmdLet
      cmdDefine:
        BEGIN
          CASE TChkDefine(cmd).FieldType OF
            ftInteger:   S := DupeString('#', TChkDefine(cmd).Length);
            ftString:      S := DupeString('_', TChkDefine(cmd).Length);
            ftDate:      S := '<MM/DD/YYYY>';
            ftYMDDate:   S := '<YYYY/MM/DD>';          //&&
            ftUpperAlfa: S := '<A' + DupeString('A', TChkDefine(cmd).Length-1) + '>';
            ftSoundex:   S := '<S' + DupeString('S', TChkDefine(cmd).Length-1) + '>';
            ftBoolean:   S := '<Y>';
            ftFloat:     BEGIN
                           S := DupeString('#', TChkDefine(cmd).Length - TChkDefine(cmd).NumDecimals - 1);
                           IF TChkDefine(cmd).NumDecimals = 0 THEN
                             S := S + '#'
                           ELSE
                             S := S + '.' + DupeString('#', TChkDefine(cmd).NumDecimals);
                         END;   
            ftEuroDate:  S := '<DD/MM/YYYY>';
          END;
          S := 'DEFINE ' + TChkDefine(cmd).FieldName +'  ' + S;
          IF TChkDefine(cmd).Scope = scGlobal THEN S := S + ' GLOBAL';
          IF TChkDefine(cmd).Scope = scCumulative THEN S := S + ' CUMULATIVE';
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
          CASE TChkColor(cmd).ColorCmd OF
            1: S := S + 'QUESTION ';
            2: S := S + 'DATA ';
            3: S := S + 'BACKGROUND ';
          END;  //case
          IF TChkColor(cmd).IsEpiInfoNo THEN
          BEGIN
            IF TChkColor(cmd).ColorCmd = 3 THEN S := S + IntToStr(TChkColor(cmd).BgColor)
            ELSE BEGIN
              n := (TChkColor(cmd).BgColor SHL 4);
              n := n AND TChkColor(cmd).TxtColor;
              S := S + IntToStr(n);
            END;
          END ELSE BEGIN
            IF TChkColor(cmd).ColorCmd = 3 THEN
              S := S + ' ' + ChkColorNames[TChkColor(cmd).BgColor]
            ELSE BEGIN
              S := S + ' ' + ChkColorNames[TChkColor(cmd).txtcolor];
              IF TChkColor(cmd).BgColor <> 255 THEN
                S := S + ' ' + ChkColorNames[TChkColor(cmd).bgcolor];
            END;
          END;
          AddToCheckLines(S);
        END;
      cmdComment:
        AddToCheckLines(TChkComment(cmd).Comment);
      cmdLeaveField:
        BEGIN
          S := 'cmdLeaveField -';
          IF TChkLeaveField(cmd).IsLastField THEN
            S := S + 'LastField';
          AddToCheckLines(S);
        END;      
    END;  //Case
  END;  //for
END;  //Procedure AddCommandList

Procedure TCheckFileIO.FieldBlockToStrings(aField: TEpiField);
VAR
  S: string;
  TmpList: TStrings;
  LocalVltType: TValueLabelSetScope;
  I, p: Integer;
BEGIN
  TmpList := nil;
  try
    WITH AField.FieldProperties DO
    BEGIN
      AddToCheckLines(trim(AField.FieldName));
      Inc(FIndentLvl);

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

      {TODO: Write autosearch}
{      IF AutoSearch THEN
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
      END;      }

      {Write TopOfScreen}
      IF TopOfScreen >= 0 THEN
      BEGIN
        S := 'TOPOFSCREEN';
        IF TopOfScreen > 0 THEN
          S := S + ' ' + IntToStr(TopOfScreen);
        AddToCheckLines(S);
      END;

      {Write RANGE}
      S := '';
      for i := 0 to Ranges.Count-1 do
      begin
        P := Pos('-', Ranges[i]);
        if P > 0 then
        begin
          S := Copy(Ranges[i], 1, P) + ' ' + Copy(Ranges[i], P+1, 50); // 50+ digits is not likely... :D
          Break;
        end;
      end;
      IF (S <> '') THEN
        AddToCheckLines('RANGE ' + S);
      
      {Write LEGAL block}
      S := '';
      P := 0;
      for i := 0 to Ranges.Count-1 do
      begin
        if Pos('-', Ranges[i]) > 0 then continue;
        if P = 0 then
        begin
          AddToCheckLines('LEGAL');
          Inc(FIndentLvl);
          P := 1;
        end;
        if Pos(' ', Ranges[i]) > 0 then
          AddToCheckLines ('"' + Ranges[i] + '" ')
        else
          AddToCheckLines(Ranges[i]);
      end;
      if P > 0 then
      begin
        Dec(FIndentLvl);
        AddToCheckLines('END');
      end;

      {Write Comment Legal}
      IF Assigned(aField.ValueLabelSet) THEN
      BEGIN
        LocalVltType := aField.ValueLabelSet.LabelScope;
        S := 'COMMENT LEGAL ';
        if (LocalVltType = vlsLocal) and (aField.ValueLabelIsFieldRef) then
          LocalVltType := vlsGlobal;
        case LocalVltType of
          vlsLocal:
            begin
              if ShowValueLabel then S := S + 'SHOW';
              AddToCheckLines(S);
              // Indentation is done in LabelToText
              LabelToText(aField.ValueLabelSet);
              AddToCheckLines('END');
            end;
          vlsGlobal, vlsFile:
            begin
              if aField.ValueLabelSet.LabelScope <> vlsFile then
                S := S + 'USE ';
              S := S + aField.ValueLabelSet.Name;
              if ShowValueLabel then S := S + ' SHOW';
              AddToCheckLines(S);
            end;
        end;  //case
      end;

      {Write JUMPS block}
      IF Jumps.Count > 0 THEN
      BEGIN
        S := 'JUMPS';
        IF JumpResetValue <> '' THEN
          S := S + ' RESET';
        IF (JumpResetValue <> '') AND (JumpResetValue <> #32) THEN
          S := S + ' "' + JumpResetValue[1] + '"';
        AddToCheckLines(S);
        Inc(FIndentLvl);
        FOR i := 0 TO Jumps.Count - 1 DO
          AddToCheckLines(Jumps[i] + ' ' + TString(Jumps.Objects[i]).Str);
        Dec(FIndentLvl);
        AddToCheckLines('END');
      END;

      {Write MUSTENTER, NOENTER, REPEAT, CONFIRMFIELD}
      Case EntryType of
        entMust: AddToCheckLines('MUSTENTER');
        entNone: AddToCheckLines('NOENTER');
      end;
      IF DoRepeat THEN
        AddToCheckLines('REPEAT');
      IF Confirm THEN
        AddToCheckLines('CONFIRMFIELD');

      {TODO : Write Missingvalues}
(*      IF MissingValues[0]<>'' THEN
      Begin
        S := MissingValues[0];
        IF MissingValues[1] <> '' THEN S := S + ' ' + MissingValues[1];
        IF MissingValues[2] <> '' THEN S := S + ' ' + MissingValues[2];
        AddToCheckLines('MISSINGVALUE ' + S);
      end; *)

      Case TypeType of
        {Write TYPE STATUSBAR}
{        ttStatusBar:
          begin
            S := 'TYPE STATUSBAR';
            IF TypeText <> '' THEN S := S + ' "' + TypeText + '"';
            IF TypeColour <> 0 THEN
              S := S + ' ' + ChkColorNames[TypeColour];
            AddToCheckLines(S);
          END;          }
        {Write TYPE COMMENT}
        ttComment:
          Begin
            S := '';
            if TypeColour <> ChkBaseColor then
              S := ' ' + ChkColorNames[TypeColour];
            AddToCheckLines('TYPE COMMENT' + S);
          End;
        {Write TYPE COMMENT ALLFIELDS}
{        ttAllFields:
          Begin
            S := 'TYPE COMMENT ALLFIELDS';
            IF TypeColour <> 0 THEN
              S := S + ' ' + ChkColorNames[TypeColour];
            AddToCheckLines(S);
          END;           }
        {Write TYPE COMMENT <fieldname>}
        ttField:
          AddToCheckLines('TYPE COMMENT ' + TypeField.FieldName);
        {Write TYPE COMMENT <colour>}
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
  EpiLogger.IncIndent;
  EpiLogger.Add(ClassName, 'ReadCheckFile', 2, 'Check File = ' + aFileName);

  result := false;
  try
    if not Assigned(DF) then exit;
    if not Assigned(Df.FileProperties) then exit;

    // It's not a fault if no .CHK file exists.
    result := true;
    if not FileExists(aFileName) then exit;

    DF.FileProperties.HasCheckFile := true;
    DF.FileProperties.FileName     := aFileName;

    try
      FCheckLines.LoadFromFile(aFileName);
      EpiUnknownStringsToUTF8(FCheckLines);
    except
      FreeAndNil(FCheckLines);
      EpiLogger.AddError(ClassName, 'ReadCheckFile', Format('Error reading the checkfile %s', [aFileName]),20130);
      FErrorLines.Add(Format(Lang(20130,'Error reading the checkfile %s'), [aFileName]));
      Exit;
    end;

    FDf := Df;
    result := InternalRead();
    FDf.FileProperties.ErrorInFile := not result;
  finally
    EpiLogger.DecIndent;
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
  EpiLogger.IncIndent;
  EpiLogger.Add(ClassName, 'WriteCheckToStream', 2, 'Streamtype = ' + Stream.ClassName);
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
    EpiLogger.DecIndent;
  end;
end;

end.
