{%MainUnit epidatafile.pas}

{ TEpiDataFiles }

function TEpiDataFiles.GetCount: Integer;
begin
  result := FList.Count;
end;

function TEpiDataFiles.GetDataFile(Index: integer): TEpiDataFile;
begin
  Result := TEpiDataFile(FList[Index]);
end;

function TEpiDataFiles.GetSettings: TEpiSettings;
begin
  result := TEpiDocument(Owner).Settings;
end;

constructor TEpiDataFiles.Create(AOwner: TObject);
begin
  inherited;
  FList := TFPList.Create;
end;

destructor TEpiDataFiles.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TEpiDataFiles.NewDatafile: TEpiDatafile;
begin
  result := TEpiDataFile.Create(Self);
  Add(Result);
end;

procedure TEpiDataFiles.Add(aDataFile: TEpiDataFile);
begin
  FList.Add(aDataFile);
end;

procedure TEpiDataFiles.Delete(aDataFile: TEpiDataFile);
begin
  FList.Remove(aDataFile);
end;

function TEpiDataFiles.DataFileById(const aId: string): TEpiDataFile;
var
  Idx: LongInt;
begin
  Result := nil;
  Idx := IndexOf(aId);
  if Idx >= 0 then
    Result := DataFile[Idx];
end;

function TEpiDataFiles.DataFileExists(const aId: string): boolean;
begin
  result := IndexOf(aId) >= 0;
end;

function TEpiDataFiles.IndexOf(const aId: string): integer;
begin
  for result := 0 to Count - 1 do
    if DataFile[result].Id = aId then exit;
  result := -1;
end;

procedure TEpiDataFiles.SaveToStream(St: TStream; Lvl: Integer);
var
  S: String;
  i: Integer;
  TempSt: TStream;
begin
  if Count = 0 then exit;

  S :=
    Ins(Lvl) + '<DataFiles>' + LineEnding;
  St.Write(S[1], Length(S));

  if Settings.Scrambled then
    TempSt := TStringStream.Create('')
  else
    TempSt := St;

  for i := 0 to Count -1 do
    DataFile[i].SaveToStream(TempSt, Lvl + 1);

  if Settings.Scrambled then
  begin
    S := EnScramble(TempSt) + LineEnding;
    St.Write(S[1], Length(S));
    TempSt.Free;
  end;

  S :=
    Ins(Lvl) + '</DataFiles>' + LineEnding;
  St.Write(S[1], Length(S));
end;

procedure TEpiDataFiles.LoadFromXml(Root: TDOMNode);
var
  NewRoot: TDOMNode;
  Node: TDOMNode;
  NewDF: TEpiDataFile;
begin
  // Root = <DataFiles>
  if not Assigned(Root) then exit;

  // If file is scrambles, then we first need to descramble (using master password)
  // and then read xml structure.
  if Settings.Scrambled then
    NewRoot := DeScramble(Root)
  else
    NewRoot := Root;

  Node := NewRoot.FirstChild;
  while Assigned(Node) do
  begin
    if Node.CompareName('DataFile') <> 0 then
      ReportXmlError(EPI_XML_TAG_MISSING, 0, '', []); // TODO : Errormessage

    NewDF := TEpiDataFile.Create(Self);
    NewDF.LoadFromXml(Node);
    Add(NewDF);

    Node := Node.NextSibling;
  end;

  if Settings.Scrambled then
    NewRoot.Free;
end;

{ TEpiDataFileProperties }

constructor TEpiDataFileProperties.Create;
begin
  Reset();
end;

destructor TEpiDataFileProperties.Destroy;
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

procedure TEpiDataFileProperties.Reset;
begin
  InternalReset;

  FDefines        := TEpiFields.Create(nil);
  FDefines.Owned  := True;
  FAutoFields     := TEpiFields.Create(nil);
  FAutoFields.Owned := false;
end;

function TEpiDataFileProperties.Clone: TEpiDataFileProperties;
var
  i: Integer;
begin
  Result := TEpiDataFileProperties.Create;

  // Clone basic:
  Result.FShowLastRecord     := FShowLastRecord;
  Result.FMissingAction      := FMissingAction;

  // Command structures.
  if Assigned(FBeforeFileCmds) then
    FBeforeFileCmds.Clone(Result.FBeforeFileCmds);
  if Assigned(FAfterFileCmds) then
    FAfterFileCmds.Clone(Result.FAfterFileCmds);
  if Assigned(FBeforeRecordCmds) then
    FBeforeRecordCmds.Clone(Result.FBeforeRecordCmds);
  if Assigned(FAfterRecordCmds) then
    FAfterRecordCmds.Clone(Result.FAfterRecordCmds);
  if Assigned(FRecodeCmds) then
    FRecodeCmds.Clone(Result.FRecodeCmds);

  // Other
  for i := 0 to MaxDefinedMissingValues do
    Result.GlobalMissingVal[i] := GlobalMissingVal[i];

  for i := 0 to FDefines.Count - 1 do
    Result.FDefines.Add(FDefines[i].Clone());

  for i := 0 to FAutoFields.Count - 1 do
    Result.FAutoFields.Add(FAutoFields[i]);
end;

function TEpiDataFileProperties.DefineExists(const aName: string): Boolean;
begin
  result := FDefines.FieldExists(aName);
end;

function TEpiDataFileProperties.DefineByName(const aName: string): TEpiField;
begin
  result := FDefines.FieldByName(aName);
end;

procedure TEpiDataFileProperties.AddDefine(Field: TEpiField);
begin
  FDefines.Add(Field);
end;

function TEpiDataFileProperties.GetGlobMissing(Index: Integer): string;
begin
  Result := FGlobalMissingValues[Index];
end;

procedure TEpiDataFileProperties.SetGlobMissing(Index: Integer;
  const Value: string);
begin
  FGlobalMissingValues[Index] := Value;
end;

procedure TEpiDataFileProperties.InternalReset;
begin
  if Assigned(FBeforeFileCmds)   then FreeAndNil(FBeforeFileCmds);
  if Assigned(FAfterFileCmds)    then FreeAndNil(FAfterFileCmds);
  if Assigned(FBeforeRecordCmds) then FreeAndNil(FBeforeRecordCmds);
  if Assigned(FAfterRecordCmds)  then FreeAndNil(FAfterRecordCmds);
  if Assigned(FRecodeCmds)       then FreeAndNil(FRecodeCmds);
  if Assigned(FDefines)          then FreeAndNil(FDefines);
  if Assigned(FAutoFields)       then FreeAndNil(FAutoFields);

  FMissingAction       := maIgnoreMissing;
  FShowLastRecord      := false;
  FErrorInFile         := false;
  FHasCheckFile        := false;
  FFileName            := '';
  FHasTypeStatusBar    := false;
  FTypeStatusBarColor  := EpiColourBase;
  FTypeStatusBarText   := '';
  FTypeStatusBarField  := nil;
end;

{ TEpiDataFile }

function TEpiDataFile.GetField(Index: Integer): TEpiField;
begin
  result := Fields[Index];
end;

function TEpiDataFile.GetDeleted(Index: integer): boolean;
begin
  result := FRecordStatus.AsInteger[Index] = Ord(rsDeleted);
end;

function TEpiDataFile.GetSize: Integer;
begin
  result := FRecordStatus.Size;
end;

function TEpiDataFile.GetTextLabel(Index: integer): TEpiTextLabel;
begin
  result := TextLabels[Index];
end;

function TEpiDataFile.GetTextLabelCount: Cardinal;
begin
  result := TextLabels.Count;
end;

function TEpiDataFile.GetVerified(Index: integer): boolean;
begin
  result := FRecordStatus.AsInteger[index] = Ord(rsVerified);
end;

procedure TEpiDataFile.InternalReset;
begin
  if Assigned(FFields) then FreeAndNil(FFields);
  if Assigned(FTextLabels) then FreeAndNil(FTextLabels);
  if Assigned(FScreenProperties) then FreeAndNil(FScreenProperties);
  if Assigned(FCheckFile) then FreeAndNil(FCheckFile);
  if Assigned(FRecordStatus) then FreeAndNil(FRecordStatus);
  if Assigned(FValueLabels) then FreeAndNil(FValueLabels);

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
  FDatafileType   := dftNone;
end;

procedure TEpiDataFile.SetDeleted(Index: integer; const AValue: boolean);
begin
  if AValue then
    FRecordStatus.AsInteger[Index] := ord(rsDeleted)
  else
    FRecordStatus.AsInteger[Index] := ord(rsNormal);
end;

procedure TEpiDataFile.SetFileLabel(const AValue: string);
var
  OldVal: String;
begin
  if AValue = FileLabel then exit;
  OldVal := FileLabel;
  FFileLabel := AValue;
  DoChange(dceLabel, @OldVal);
end;

procedure TEpiDataFile.SetFileName(const AValue: string);
var
  OldVal: String;
begin
  if AValue = FileName then exit;
  OldVal := FileName;
  FFileName := AValue;
  DoChange(dceName, @OldVal);
end;

procedure TEpiDataFile.SetSize(const AValue: Integer);
var
  i: Integer;
begin
  for i := 0 to FieldCount - 1 do
    Fields[i].Size := AValue;

  FRecordStatus.Size := AValue;
end;

procedure TEpiDataFile.SetStudy(const AValue: string);
var
  OldVal: String;
begin
  if AValue = Study then exit;
  OldVal := Study;
  FStudy := AValue;
  DoChange(dceStudy, @OldVal);
end;

procedure TEpiDataFile.SetVerified(Index: integer; const AValue: boolean);
begin
  if AValue then
    FRecordStatus.AsInteger[Index] := ord(rsVerified)
  else
    FRecordStatus.AsInteger[Index] := ord(rsNormal);
end;

procedure TEpiDataFile.SetId(const AValue: string);
begin
  if FId = AValue then exit;
  FId := AValue;
end;

function TEpiDataFile.InternalOpenOld: boolean;
var
  // Misc:
  TempInt, I, TotFieldLength: integer;
  TxtFile: TextFile;
  EField: TEpiField;
  FieldNumberCounter: cardinal;
  ChkIO: TCheckFileIO;
  CharBuf: Array of char;

  // Reading the textfile:
  TxtLine: string;
  HeaderLineCount: Integer;
  ValCode: Integer;
  CurrentLine: Integer;

  // Field lines:
  TmpFieldType: TFieldType;
  TmpFieldChar, Dummy: Char;
  TmpFieldTypeInt,
  TmpFieldColor, TmpQuestX, TmpQuestY, TmpLength,
  TmpFieldX, TmpFieldY, TmpQuestColor: Integer;
  TmpName: string[10];
  TmpLabel, TmpStr: string;
  CurRec: Integer;
  StrBuf: String;
  DataStream: TMemoryStream;
  BufPos: Integer;
  EncData: String;
  Stop: Boolean;
  ELabel: TEpiTextLabel;
begin
{  EpiLogger.IncIndent;
  EpiLogger.Add(ClassName, 'InternalOpenOld', 2, 'Filename = ' + Filename);
  result := false;

  DatafileType := dftEpiDataRec;

  try
    AssignFile(TxtFile, UTF8ToSys(Filename));
    {$I-}
    System.Reset(TxtFile);
    {$I+}
    if IOResult() > 0 then
    begin
      ErrorText := Format(Lang(20108,'Data file %s could not be opened.'),[Filename]) + #13 +
                           Lang(20208,'Please check if the file is in use and that the file name is legal.');
      ErrorCode := EPI_DATAFILE_FORMAT_ERROR;
      EpiLogger.AddError(ClassName, 'InternalOpenOld', ErrorText, 20108);
      Exit;
    end;
    // --- Read "First Line" header ---
    ReadLn(TxtFile, TxtLine);

    // - Password
    TempInt := Pos('~KQ:', AnsiUpperCase(TxtLine));
    if TempInt > 0 then
    begin
      if not RequestPassword(Copy(TxtLine, TempInt + 4, Pos(':KQ~', AnsiUpperCase(TxtLine)) - (TempInt + 4))) then
      begin
        ErrorText := Lang(9020, 'Incorrect password entered');
        Errorcode := EPI_INVALID_PASSWORD;
        EpiLogger.AddError(ClassName, 'InternalOpenOld', ErrorText, 9020);
        CloseFile(TxtFile);
        Exit;
      end;
    end;

    // - FileLabel
    if Pos('FILELABEL: ', AnsiUpperCase(TxtLine)) > 0 then
      FileLabel :=  EpiUnknownStrToUTF8(Copy(TxtLine, Pos('FILELABEL: ', AnsiUpperCase(TxtLine)) + Length('FILELABEL: ') , Length(TxtLine)));

    // - Autonaming or Firstword
    if Pos(' VLAB', TxtLine) > 0 then
      FieldNaming := fnFirstWord
    else
      FieldNaming := fnAuto;

    // - Header lines:
    Val(Copy(TxtLine, 1, Pos(' ', TxtLine)-1), HeaderLineCount, ValCode);
    if ValCode > 0 then
    begin
      ErrorText := Format(Lang(20112, 'Incorrect format of datafile %s'), [Filename]);
      ErrorCode := EPI_DATAFILE_FORMAT_ERROR;
      EpiLogger.AddError(ClassName, 'InternalOpenOld', ErrorText, 20112);
      CloseFile(TxtFile);
      Exit;
    end;

    FieldNumberCounter := 1;
    TotFieldLength := 0;
    // Read field defining header lines.
    for CurrentLine := 1 to HeaderLineCount do
    begin
      EpiLogger.Add(ClassName, 'InternalOpenOld', 3, 'Reading headerline no: ' + IntToStr(CurrentLine));
      if UpdateProgress((CurrentLine*100) DIV HeaderLineCount, lang(0,'Opening data file')) = prCancel then
      begin
        ErrorText := Lang(0, 'Cancelled by user');
        Errorcode := EPI_USERCANCELLED;
        EpiLogger.AddError(ClassName, 'InternalOpenOld', ErrorText, 0);
        CloseFile(TxtFile);
        Exit;
      end;

      ReadLn(TxtFile,
             TmpFieldChar, TmpName, TmpQuestX, TmpQuestY,
             TmpQuestColor, TmpFieldX, TmpFieldY, TmpFieldTypeInt, TmpLength,
             TmpFieldColor, dummy, TmpLabel);

      // Field types.
      if TmpFieldTypeInt >= 100 then
        // Type > 100 => float field
        TmpFieldType := ftFloat
      else begin
        // Normal field type recognition.
        TmpFieldType := ftInteger;
        WHILE TmpFieldTypeInt > ORD(TmpFieldType) DO
          TmpFieldType := Succ(TmpFieldType);
      end;

      // This is not a data field, but a question field.
      if TmpLength = 0 then TmpFieldType := ftQuestion;

      // Unsupported field are automatically converted to string (ftString) fields.
      if (not (TmpFieldType in SupportedFieldTypes)) or
         ((TmpFieldType in DateFieldTypes) and (TmpLength < 10)) then
        TmpFieldType := ftString;

      // Trim text information.
      TmpName := Trim(TmpName);
      TmpLabel := Trim(TmpLabel);

      if TmpFieldType = ftQuestion then
      begin
        ELabel := TEpiTextLabel.Create(Self.TextLabels);
        with ELabel do
        begin
          Id := TmpName;
          Text := TmpLabel;
          ScreenProp := ScreenProperties.DefaultScreenProperty;
          TextLeft   := TmpFieldX;
          TextTop    := TmpFieldY;
        end;
        AddTextLabel(ELabel);
        Continue;
      end;

      EField := TEpiField.CreateField(TmpFieldType);
      with EField do
      begin
        ScreenProps         := ScreenProperties.DefaultScreenProperty;
        FieldLeft           := TmpFieldX;
        FieldTop            := TmpFieldY;
        VarLabelScreenProps := ScreenProperties.DefaultScreenProperty;
        VarLabelLeft        := TmpQuestX;
        VarLabelTop         := TmpQuestY;

        FieldLength := TmpLength;
        FieldDecimals := 0;
        if TmpFieldTypeInt >= 100 then
          FieldDecimals := TmpFieldTypeInt - 100;
        VariableLabel := EpiUnknownStrToUTF8(StringReplace(TmpLabel, '_', '-', [rfReplaceAll]));

        // In old style .REC files, first word in label is the name of the field. Remove it.
        if Pos(TmpName, VariableLabel) > 0 then
          VariableLabel := Trim(Copy(VariableLabel, Length(TmpName)+1, Length(VariableLabel)));
        // Ensure valid variable name.
        FieldName := Trim(CreateUniqueFieldName(TmpName));
        // If the field name was invalid (not very likely) use it in variable label.
        IF FieldName <> TmpName THEN
          VariableLabel := TmpName + ' ' + VariableLabel;

        // Summerize field findings.
        TotFieldLength := TotFieldLength + FieldLength;
      end;  // With EField
      AddField(EField);
    end; // For CurrentLine

    // Position for reading and check for corruptness.
    TotFieldLength := TotFieldLength + (((TotFieldLength - 1) DIV MaxRecLineLength) + 1) * 3;
    TmpLength := TextPos(TxtFile);
    CloseFile(TxtFile);

    DataStream := TMemoryStream.Create;
    DataStream.LoadFromFile(UTF8ToSys(Filename));
    DataStream.Position := DataStream.Size;

    // Skip all lineendings / EOF chars.
    SetLength(CharBuf, 16);
    Stop := false;
    while DataStream.Position >= TmpLength do
    begin
      DataStream.Seek(-16, soCurrent);
      DataStream.Read(CharBuf[0], 16);

      i := 15;
      while i >= 0 do
      begin
        if (CharBuf[i] in ['!', '?', '^']) then
        begin
          Stop := true;
          break;
        end;
        Dec(i);
      end;
      if Stop then break;
      DataStream.Seek(-16, soCurrent);
    end;

    if DataStream.Position < TmpLength then
      TempInt := TmpLength  // This is an empty datafile!
    else
      TempInt := DataStream.Position - (16 - i) + 3; // + 3 is for "!#13#10" which all .REC file should end with??!?!?
    if ((TempInt - TmpLength) mod TotFieldLength) <> 0 then
    begin
      ErrorText := Format(Lang(20118, 'Error in datafile %s. One or more records are corrupted. Size: %d, Offset: %d, TotalLength: %d, i: %d'),
        [Filename, DataStream.Size, TmpLength, TotFieldLength, i]);
      ErrorCode := EPI_DATAFILE_FORMAT_ERROR;
      EpiLogger.AddError(ClassName, 'InternalOpenOld', ErrorText, 20118);
      Exit;
    end;

    TempInt := ((TempInt - TmpLength) div TotFieldLength);
    Size := TempInt;
    DataStream.Position := TmpLength;

    SetLength(CharBuf, TotFieldLength);
    For CurRec := 1 to TempInt do
    begin
      I := DataStream.Read(CharBuf[0], TotFieldLength);
      if (I <> TotFieldLength) then
      begin
        ErrorText := Lang(20464, 'Error reading record');
        ErrorCode := EPI_READ_FILE_ERROR;
        EpiLogger.AddError(Classname, 'InternalOpenOld', ErrorText, 20464);
        raise Exception.Create('Error reading record');
      end;

      StrBuf := CharBuf[High(CharBuf) - 2];
      if StrBuf = '?' then
        Deleted[CurRec] := true
      else if StrBuf = '^' then
        Verified[CurRec] := true;

      StrBuf := StringReplace(string(CharBuf), EOLChars, '', [rfReplaceAll]);
      BufPos := 1;
      for i := 0 TO FieldCount - 1 DO
      with Fields[i] do begin
        TmpStr := Trim(Copy(StrBuf, BufPos, FieldLength));
        if TmpStr = '' then
          TmpStr := TEpiStringField.DefaultMissing;
        IF (fieldtype = ftCrypt) AND (FPassword <> '') THEN
        begin
          EncData := Base64DecodeStr(TmpStr);
          FCrypter.DecryptCFBblock(EncData[1], EncData[1], Length(EncData));
          TmpStr := Trim(EncData);
          FCrypter.Reset;
        end;
        AsString[CurRec] := EpiUnknownStrToUTF8(TmpStr);
        Inc(BufPos, FieldLength);
      end;
    end;

    result := true;
    if not (eoIgnoreChecks in Options) then
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
            EpiLogger.AddError(ClassName, 'InternalOpenOld', ErrorText, 0);
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
    if Assigned(DataStream) then FreeAndNil(DataStream);
    EpiLogger.DecIndent;
  end;          }
end;

function TEpiDataFile.InternalSaveOld: boolean;
var
  Crypt: boolean;
  i: integer;
  S, EncData: string;
  Stream: TFileStream;
  ChkIO: TCheckFileIO;
  CurRec: Integer;
  T: String;
  Z: Integer;
  TmpStr: String;
  FieldNames: TStrings;
  Fmt: TFormatSettings;
begin
{  EpiLogger.IncIndent;
  EpiLogger.Add(Classname, 'InternalSaveOld', 3);
  result := false;

  IF Fields.Count = 0 THEN
  BEGIN
    Raise Exception.Create('No fields defined');
    Exit;
  END;

  Stream := TFileStream.Create(FileName, fmCreate);
  ChkIO := nil;

  try
    FieldNames := TStringList.Create;
    Fmt.DecimalSeparator := '.';

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
      FCrypter.InitStr(Password, TDCP_sha1);
    END;

    // - Header lines (and colour):
    S := IntToStr(FieldCount + TextLabelCount) + ' 1 ';

    // - Autonaming or Firstword
    IF FieldNaming = fnFirstWord THEN
      S := S + 'VLAB ';

    // - Password
    IF Password <> '' THEN
    begin
      EncData := Trim(Password);
      FCrypter.EncryptCFBblock(EncData[1], EncData[1], Length(EncData));
      EncData := Base64EncodeStr(EncData);
      FCrypter.Reset;
      S := S + '~KQ:' + EncData + ':KQ~ ';
    end;

    // - FileLabel
    IF Trim(FileLabel) <> '' THEN
      S := S + 'Filelabel: ' + EpiUtf8ToAnsi(FileLabel);

    S := S + #13#10;
    Stream.Write(S[1], Length(S));

    FOR i := 0 TO FieldCount - 1 DO
    WITH Fields[i] DO
    BEGIN
      EpiLogger.Add(TEpiDataFile.Classname, 'InternalSaveOld', 3, 'Writing heading no. ' + IntToStr(i+1));

      // - Fieldchar
      IF (FieldType = ftInteger) OR (FieldType = ftFloat) OR
         (FieldType = ftIDNUM) THEN
        s := '#'
      ELSE
        s := '_';

      // Since format is not UTF8 add EpiUtf8ToAnsi here else length will be f*cked
      TmpStr := CreateUniqueAnsiVariableName(FieldName, MaxFieldNameLen, FieldNames);
      s := s + Format('%-10s', [TmpStr]);     //Name of field (left justified)
      s := s + ' ';                           //Space required for some unknown reason
      s := s + Format('%4d', [VarLabelLeft]);       //Question X-position
      s := s + Format('%4d', [VarLabelTop]);       //Question Y-position
      s := s + Format('%4s', ['30']);         //Question colorcode
      s := s + Format('%4d', [FieldLeft]);       //Entry X-position
      s := s + Format('%4d', [FieldTop]);       //Entry Y-position

      //Write FieldType
      // 0 = Question without entryfield, i.e. text only
      // 100+Number of decimals = Floating point number
      // For all other: use the fieldtype-code (fieldtype)
      IF FieldType = ftQuestion THEN
        s := s + Format('%4s', ['0'])
      ELSE IF (FieldType = ftFloat) AND (FieldDecimals > 0) THEN
        s := s + Format('%4d', [100 + FieldDecimals])
      ELSE if (FieldType = ftInteger) and (FieldLength > MaxIntegerLength) then
        S := S + Format('%4d', [ORD(ftFloat)])
      ELSE
        s := s + Format('%4d', [ORD(fieldtype)]);

      //Write length of field - use 0 for text only
      IF FieldType = ftQuestion THEN
        s := s + Format('%4s', ['0'])
      ELSE BEGIN
        s := s + Format('%4d', [FieldLength]);
      END;

      //write entry colorcode - special use in encrypted fields (holds entrylength of field)
      IF FieldType <> ftCrypt THEN
        s := s + Format('%4s', ['112'])
      ELSE
        IF FieldLength < 15 THEN
          s := s + Format('%4d', [111 + FieldLength])
        ELSE
          s := s + Format('%4d', [FieldLength]);

      s := s + ' ';                      //Another unnescessary blank
      s := s + EpiUtf8ToAnsi(VariableLabel);

      s := s + #13#10;
      Stream.Write(S[1], Length(S));
    END; // End With Field...

    // ******************
    //    Write Data
    // ******************
    for CurRec := 1 to Size do
    begin
      S := '';
      for i := 0 TO FieldCount - 1 DO
      with Field[i] do begin
        if IsMissing[CurRec] then
          T := DupeString(' ', FieldLength)
        else if FieldType = ftCrypt then
        begin
          EncData := EpiUtf8ToAnsi(Trim(AsString[CurRec]));
          FCrypter.InitStr(Password, TDCP_sha1);
          FCrypter.EncryptCFBblock(EncData[1], EncData[1], Length(EncData));
          EncData := Base64EncodeStr(EncData);
          FCrypter.Reset;
          T := Format('%-*s', [FieldLength, EncData])
        end else if FieldType in [ftString, ftUpperAlfa] then
          T := Format('%-*s', [FieldLength, EpiUtf8ToAnsi(AsString[CurRec])])
        else if FieldType = ftFloat then
          T := Format('%*.*f', [FieldLength, FieldDecimals, AsFloat[CurRec]], Fmt)
        else
          T := Format('%*s', [FieldLength, EpiUtf8ToAnsi(AsString[CurRec])]);
        S := S + T;
      end;
      Z := Length(S);
      if Z + 3 > MaxRecLineLength then
        for I := (Z div MaxRecLineLength) downto 1 do
          Insert(EOLchars, S, (MaxRecLineLength * I) + 1);

      if Deleted[CurRec] then
        S := S + '?' + #13#10
      else if Verified[CurRec] then
        S := S + '^' + #13#10
      else
        S := S + EOLchars;

      Stream.Write(S[1], Length(S));
    end;

    result := true;

    if not (eoIgnoreChecks in FOptions) then
    begin
      if Assigned(Stream) then FreeAndNil(Stream);
      FileProperties.FileName := ChangeFileExt(FileName, '.chk');
      Stream := TFileStream.Create(FileProperties.FileName, fmCreate);
      ChkIO := TCheckFileIO.Create();
      result := ChkIO.WriteCheckToStream(Stream, Self);
      if Stream.Size = 0 then
      begin
        FreeAndNil(Stream);
        DeleteFile(FileProperties.FileName);
        FileProperties.FileName := '';
      end;
    end;
  finally
    EpiLogger.DecIndent;
    if Assigned(Stream) then FreeAndNil(Stream);
    if Assigned(ChkIO) then FreeAndNil(ChkIO);
  end;       }
end;

function TEpiDataFile.Lang(LangCode: Integer; Const LangText: string): string;
begin
  Result := LangText;
  IF Assigned(FOnTranslate) THEN
    Result := FOnTranslate(langcode, Result)
end;

function TEpiDataFile.UpdateProgress(Percent: Integer; Msg: string): TProgressResult;
begin
  Result := prNormal;
  if Assigned(FOnProgress) then
  Begin
    result := FOnProgress(Self, Percent, Msg);
  end;
end;

function TEpiDataFile.GetFieldCount: Cardinal;
begin
  result := Fields.Count;
end;

procedure TEpiDataFile.SaveRecords(St: TStream; Lvl: Integer);
var
  CurRec: Integer;
  CurField: Integer;
  S: String;
begin
  S := Ins(Lvl) + '<Records>' + LineEnding;
  St.Write(S[1], Length(S));

  for CurRec := 1 to Size do
  begin
    S := Ins(Lvl + 1) + '<Record';

    for CurField := 0 to FieldCount - 1 do
    with Fields[CurField] do
      S += ' ' + Id + '="' + StringToXml(AsString[CurRec]) + '"';

    if Verified[CurRec] then
      S += ' st="2"'
    else if Deleted[CurRec] then
      S += ' st="1"'
    else
      S += ' st="0"';
    S += '/>' + LineEnding;
    St.Write(S[1], Length(S));
  end;
  S := Ins(Lvl) + '</Records>' + LineEnding;
  St.Write(S[1], Length(S));
end;

procedure TEpiDataFile.LoadRecords(Root: TDOMNode);
var
  ElemNode: TDOMElement;
  TmpField: TEpiField;
  CurRec: Integer;
  i: Integer;
begin
  // Root = <Records>

  CurRec := 1;
  Size := Root.ChildNodes.Count;
  ElemNode := TDOMElement(Root.FirstChild);
  while Assigned(ElemNode) do
  begin
    if ElemNode.CompareName('Record') <> 0 then
      ReportXmlError(EPI_XML_TAG_MISSING, 0, '', []);

    for i := 0 to ElemNode.Attributes.Length - 1 do
    begin
      if ElemNode.Attributes[i].NodeName = 'st' then
      begin
        if ElemNode.Attributes[i].NodeValue = '1' then
          Deleted[CurRec] := true
        else if ElemNode.Attributes[i].NodeValue = '2' then
          Verified[CurRec] := true;
      end else begin
        TmpField := FieldById(UTF8Decode(ElemNode.Attributes[i].NodeName));
        TmpField.AsString[CurRec] := UTF8Encode(ElemNode.Attributes[i].NodeValue);
      end;
    end;
    inc(CurRec);
    ElemNode := TDOMElement(ElemNode.NextSibling);
  end;
end;

procedure TEpiDataFile.DoChange(Event: TEpiDataFileChangeEventType;
  OldValue: Pointer);
var
  i: Integer;
  func: TEpiDataFileChangeEvent;
begin
  if FUpdateCount > 0 then exit;

  for i := 0 to FOnChangeListCount - 1 do
    FOnChangeList[i](Self, Event, OldValue);
end;

constructor TEpiDataFile.Create(AOwner: TObject; ASize: Cardinal);
var
  p: pointer;
begin
  inherited Create(AOwner);
  EpiLogger.IncIndent;
  EpiLogger.Add(ClassName, 'Create', 3);

  try
    Reset;
    FFieldNaming := fnFirstWord;
    FRecordStatus.Size := ASize;
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

    ReAllocMem(FOnChangeList,0);
    FOnChangeListCount := 0;
    FUpdateCount := 0;

    inherited Destroy;
  finally
    EpiLogger.DecIndent;
  end;
end;

function TEpiDataFile.PrepareDataFile(FieldNames: TStrings): TEpiDataFile;
begin
  //
end;

function TEpiDataFile.Clone(CloneData: boolean): TEpiDataFile;
var
  i: Integer;
begin
  // Create DataFile!
  Result := TEpiDataFile.Create(nil, Size);

  // Clone Basic data:
  Result.FileLabel   := FileLabel;
  Result.FileVersion := FileVersion;
  Result.FieldNaming := FieldNaming;
  Result.Study       := Study;
  Result.Password    := Password;
  Result.FOptions    := Options;

  // Close Events!
  Result.OnPassword := OnPassword;
  Result.OnProgress := OnProgress;
  Result.OnTranslate := OnTranslate;

  // Clone status field.
  FreeAndNil(Result.FRecordStatus);
  Result.FRecordStatus := FRecordStatus.Clone(Result, CloneData);

  // Clone the check file!
  FreeAndNil(Result.FCheckFile);
  Result.FCheckFile := FileProperties.Clone;

  // Clone ValueLabels.
  Result.ValueLabels.Assign(ValueLabels);

  // Clone Fields.
  for i := 0 To Fields.Count -1 do
    Result.AddField(Fields[i].Clone(Result, CloneData));
end;

procedure TEpiDataFile.SaveToStream(St: TStream; Lvl: Integer);
var
  S: String;
begin
  S :=
    Ins(Lvl)     + '<DataFile id="' + Id + '">' + LineEnding +
    Ins(Lvl + 1) + '<FileLabel>' + FileLabel + '</FileLabel>' + LineEnding;
  St.Write(S[1], Length(S));

  ValueLabels.SaveToStream(St, Lvl + 1);
  ScreenProperties.SaveToStream(St, Lvl + 1);
  Fields.SaveToStream(St, Lvl + 1);
  TextLabels.SaveToStream(St, Lvl + 1);
  SaveRecords(St, Lvl + 1);

  S :=
    Ins(Lvl) + '</DataFile>' + LineEnding;
  St.Write(S[1], Length(S));
end;

procedure TEpiDataFile.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMNode;
begin
  // root = <DataFile>

  Id := TDOMElement(Root).AttribStrings['id'];

  Node := Root.FindNode('FileLabel');
  if Assigned(Node) then
    FileLabel := Node.TextContent;

  Node := Root.FindNode('ValueLabels');
  if Assigned(Node) then
    ValueLabels.LoadFromXml(Node);

  Node := Root.FindNode('ScreenInfo');
  if Assigned(Node) then
    ScreenProperties.LoadFromXml(Node);

  Node := Root.FindNode('Headings');
  if Assigned(Node) then
    TextLabels.LoadFromXml(Node);

  Node := Root.FindNode('Fields');
  if Assigned(Node) then
    Fields.LoadFromXml(Node);

  Node := Root.FindNode('Records');
  if Assigned(Node) then
    LoadRecords(Node);
end;

procedure TEpiDataFile.Reset;
begin
  InternalReset;

  FFields       := TEpiFields.Create(Self);
  FFields.Owned := true;

  FTextLabels   := TEpiTextLabels.Create(Self);
  FTextLabels.Owned := true;

  FScreenProperties := TEpiScreenProperties.Create(Self);
  FScreenProperties.Owned := true;
  FScreenProperties.ReportOnChange := true;

  FValueLabels  := TValueLabelSets.Create(Self);
  FCheckFile    := TEpiDataFileProperties.Create;
  FRecordStatus := TEpiIntField.Create(0, ftInteger);
end;

function TEpiDataFile.NewField(FieldType: TFieldType): TEpiField;
begin
  result := TEpiField.CreateField(FieldType, Size);
  AddField(Result);
end;

function TEpiDataFile.FieldByName(Const aFieldName: string): TEpiField;
begin
  result := Fields.FieldByName(aFieldName);
end;

function TEpiDataFile.FieldById(const aId: string): TEpiField;
begin
  result := Fields.FieldById(aId);
end;

function TEpiDataFile.FieldExists(Const aFieldName: string): boolean;
begin
  result := Fields.FieldExists(aFieldName);
end;

function TEpiDataFile.FieldIndex(const aFieldName: string): Integer;
begin
  result := Fields.IndexOf(aFieldName);
end;

procedure TEpiDataFile.AddField(AField: TEpiField);
begin
  Fields.Add(AField);

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

procedure TEpiDataFile.RemoveField(var AField: TEpiField; DoDestroy: boolean);
begin
  if not Assigned(AField) then exit;

  Fields.Delete(AField);

  if DoDestroy then
    FreeAndNil(AField);
end;

procedure TEpiDataFile.AddTextLabel(ATextLabel: TEpiTextLabel);
begin
  TextLabels.Add(ATextLabel);
end;

procedure TEpiDataFile.RemoveTextLabel(var ATextLabel: TEpiTextLabel;
  DoDestroy: boolean);
begin
  if not Assigned(ATextLabel) then exit;

  TextLabels.Delete(ATextLabel);
  if DoDestroy then
    FreeAndNil(ATextLabel);
end;

function TEpiDataFile.TextLabelById(const ATextLabelId: string): TEpiTextLabel;
begin
  result := TextLabels.TextLabelById(ATextLabelId);
end;

function TEpiDataFile.TextLabelExists(const ATextLabelId: string): boolean;
begin
  result := TextLabels.TextLabelExists(ATextLabelId);
end;

function TEpiDataFile.CreateUniqueFieldName(const AText: string): string;
var
  Number: Integer;
  TmpStr: String;
begin
  TmpStr := AText;
  Result := AText;

  // If fieldname is unique, do nothing.
  Number := 1;
  while FieldExists(result) do
  begin
    // not unique, find a new.
    Result := TmpStr + IntToStr(Number);
    Inc(Number);
  end;
end;

function TEpiDataFile.NewTextLabel: TEpiTextLabel;
begin
  result := TEpiTextLabel.Create(TextLabels);
  AddTextLabel(result);
end;

procedure TEpiDataFile.NewRecords(ACount: Integer);
var
  i: Integer;
begin
  FRecordStatus.NewRecords(ACount);
  for i := 0 to FieldCount - 1 do
    Field[i].NewRecords(ACount);
end;

procedure TEpiDataFile.SortFields(Cmp: TListSortCompare);
begin
  Fields.Sort(Cmp);
end;

function TEpiDataFile.DocumentDatafile: TStrings;
var
  TmpStr: String;
  i: Integer;
  j: Integer;
begin
  Result := TStringList.Create;

  with Result do
  try
    {Header}
    TmpStr := FileName;
    if TmpStr = '' then
      TmpStr := '(not saved yet)';
    Append(Format('Datafile: %s', [TmpStr]));
    Append(Format('Filelabel: %s', [FileLabel]));
    if (FileName <> '') and (FileExistsUTF8(FileName)) then
      Append('Last revision: ' +
        FormatDateTime('d. mmm yyyy t', FileDateToDateTime(FileAge(UTF8ToSys(Filename)))));
    Append(Format(
      'Number of Fields: %d' + LineEnding +
      'Number of text labels: %d' + LineEnding +
      'Number of Records: %d' + LineEnding,
      [FieldCount, TextLabelCount, Size]));
    Append('');

    {Variable Information}
    { - header}
    Append('Fields in datafile:');
    Append(DupeString('-',102));
    Append(Format(
      '%-3s %-10s %-20s %-15s %-6s %-8s %-20s',
      ['No','Name','Variable label','Fieldtype','Length','Decimals','Value labels']));
    Append(DupeString('-',102));

    for i := 0 to FieldCount - 1 do
    with Field[i] do
    begin
      TmpStr := '';
      if Assigned(ValueLabelSet) then
        TmpStr := ValueLabelSet.Name;

      Append(
        UTF8Encode(
          WideFormat(
            '%3d %-10.10s %-20.20s %-15s %6d %8d %-20.20s',
            [i+1, UTF8Decode(FieldName), UTF8Decode(VariableLabel),
             UTF8Decode(FieldTypeToFieldTypeName(FieldType, nil)),
             FieldLength, FieldDecimals, UTF8Decode(TmpStr)]
          )
        )
      );

      if Assigned(ValueLabelSet) then
      for j := 0 to ValueLabelSet.Count -1 do
      begin
        // Left adjust at index 67.
        TmpStr := String(ValueLabelSet.Values[j]) + ': ' + ValueLabelSet.Labels[j];
        if ValueLabelSet.MissingValues[j] then
          TmpStr += '(missing)';
        Append(WideFormat('%-67.67s %-20.20s', ['', UTF8Decode(TmpStr)]));
      end;
    end;
  finally
  end;
end;

procedure TEpiDataFile.BeginUpdate;
var
  i: Integer;
begin
  Inc(FUpdateCount);
  for i := 0 to FieldCount -1 do
    Field[i].BeginUpdate;
end;

procedure TEpiDataFile.EndUpdate;
var
  i: Integer;
begin
  Dec(FUpdateCount);
  for i := 0 to FieldCount -1 do
    Field[i].EndUpdate;

  if (FUpdateCount < 0) or (FUpdateCount > 0) then
  begin
    if (FUpdateCount < 0) then
      FUpdateCount := 0;
    exit;
  end;
  DoChange(dceUpdate, nil);
end;

procedure TEpiDataFile.RegisterOnChangeHook(Event: TEpiDataFileChangeEvent);
begin
  Inc(FOnChangeListCount);
  ReAllocMem(FOnChangeList, FOnChangeListCount * SizeOf(TEpiDataFileChangeEvent));
  FOnChangeList[FOnChangeListCount-1] := Event
end;

procedure TEpiDataFile.UnRegisterOnChangeHook(Event: TEpiDataFileChangeEvent);
var
  Idx: LongInt;
begin
  Idx := 0;
  while Idx <= FOnChangeListCount -1 do
  begin
    if FOnChangeList[Idx] = Event then
      break;
    Inc(Idx)
  end;
  if Idx = FOnChangeListCount then exit;

  dec(FOnChangeListCount);
  if FOnChangeListCount > Idx then
    System.Move(FOnChangeList[Idx+1],FOnChangeList[Idx],(FOnChangeListCount-Idx)*SizeOf(TEpiDataFileChangeEvent));
  ReAllocMem(FOnChangeList,FOnChangeListCount*SizeOf(TEpiDataFileChangeEvent));
end;

