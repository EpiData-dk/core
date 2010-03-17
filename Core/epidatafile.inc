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

constructor TEpiDataFiles.Create;
begin
  FList := TFPList.Create;
end;

destructor TEpiDataFiles.Destroy;
begin
  FList.Free;
  inherited Destroy;
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
begin
  S :=
    Ins(Lvl) + '<DataFiles>' + LineEnding;
  St.Write(S[1], Length(S));

  for i := 0 to Count -1 do
    DataFile[i].SaveToStream(St, Lvl + 1);

  S :=
    Ins(Lvl) + '</DataFiles>' + LineEnding;
  St.Write(S[1], Length(S));
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
  if Assigned(FCrypter) then FreeAndNil(FCrypter);
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

procedure TEpiDataFile.ReportXmlError(ErrCode: Integer; LangCode: integer; Msg: String; Args: array of const);
begin
  ErrorCode := ErrCode;
  ErrorText := Format(Lang(LangCode, Msg), Args);
  EpiLogger.AddError(ClassName, 'InternalOpen', ErrorText, LangCode);
  Abort;
end;

procedure TEpiDataFile.ReadSettings(RootNode: TDOMElement;
  FmtSettings: TFormatSettings);
var
  ElemNode: TDOMElement;
begin
  // Version: so far we only got ver. 0
  ElemNode := TDOMElement(RootNode.FindNode('Version'));
  if not Assigned(ElemNode) then
    ReportXmlError(EPI_FILE_VERSION_ERROR, 0,
      'No format version specified for file: %s', [FileName]);

  // Date Separator
  ElemNode := TDOMElement(RootNode.FindNode('DateSeparator'));
  if Assigned(ElemNode) then
    FmtSettings.DateSeparator := UTF8Encode(ElemNode.TextContent)[1]
  else
    FmtSettings.DateSeparator := EpiInternalFormatSettings.DateSeparator;

  // Decimal Separator
  ElemNode := TDOMElement(RootNode.FindNode('DecimalSeparator'));
  if Assigned(ElemNode) then
    FmtSettings.DecimalSeparator := UTF8Encode(ElemNode.TextContent)[1]
  else
    FmtSettings.DecimalSeparator := EpiInternalFormatSettings.DecimalSeparator;

  // Missing mark
  ElemNode := TDOMElement(RootNode.FindNode('MissingMark'));
  if Assigned(ElemNode) then
    FmtSettings.TimeAMString := UTF8Encode(ElemNode.TextContent)
  else
    FmtSettings.TimeAMString := TEpiStringField.DefaultMissing;

  // Password
  ElemNode := TDOMElement(RootNode.FindNode('Password'));
  if Assigned(ElemNode) and (not RequestPassword(UTF8Encode(ElemNode.TextContent))) then
    ReportXmlError(EPI_INVALID_PASSWORD, 9020,
      'Incorrect password entered', []);
end;

procedure TEpiDataFile.ReadMetaData(RootNode: TDOMElement;
  FmtSettings: TFormatSettings);
var
  ElemNode: TDOMElement;
  SubSectionNode: TDOMNode;
begin
  // Filelabel
  ElemNode := TDOMElement(RootNode.FindNode('FileLabel'));
  if Assigned(ElemNode) then
    FileLabel := UTF8Encode(ElemNode.TextContent);

  // Study
  ElemNode := TDOMElement(RootNode.FindNode('Study'));
  if Assigned(ElemNode) then
    Study := UTF8Encode(ElemNode.TextContent);

  ElemNode := TDOMElement(RootNode.FindNode('ValueLabels'));
  if Assigned(SubSectionNode) then
    ReadValueLabels(ElemNode, FmtSettings);

  // TODO : User section
end;

procedure TEpiDataFile.ReadValueLabels(RootNode: TDOMElement;
  LocalFmt: TFormatSettings);
var
  ElemNode: TDOMElement;
  SubElem: TDOMNode;
  LocalValueLabel: TValueLabelSet;
  TmpStr: String;
begin
  // ElemNode = "ValueLabel"
  ElemNode := TDOMElement(RootNode.FirstChild);
  while Assigned(ElemNode) do
  begin
    if UTF8Encode(ElemNode.NodeName) <> 'ValueLabel' then
      ReportXmlError(EPI_XML_UNKNOWN_TAG, 0,
        'Unknown TAG placed in ValueLabels section: %s', [UTF8Encode(ElemNode.NodeName)]);

    SubElem := ElemNode.FindNode('Type');
    if not Assigned(SubElem) then
    begin
      ReportXmlError(EPI_XML_TAG_MISSING, 0,
        'Valuelabel Type not specified.', []);
      Exit;
    end;
    LocalValueLabel := TValueLabelSet.Create(XmlNameToFieldType(UTF8Encode(SubElem.TextContent)));

    TmpStr := UTF8Encode(ElemNode.GetAttribute('Id'));
    if TmpStr = '' then
    begin
      ReportXmlError(EPI_XML_TAG_MISSING, 0,
        'Valuelabel Id not specified.', []);
      Exit;
    end;
    LocalValueLabel.Id := UTF8Encode(TmpStr);

    SubElem := ElemNode.FindNode('Name');
    if not Assigned(SubElem) then
      ReportXmlError(EPI_XML_TAG_MISSING, 0,
        'Valuelabel name not specified.', []);
    LocalValueLabel.Name := TmpStr;

    SubElem := ElemNode.FindNode('External');
    if Assigned(SubElem) then
      ReadExternalValueLabels(TDOMElement(SubElem), LocalValueLabel);

    SubElem := ElemNode.FindNode('Internal');
    if Assigned(SubElem) then
      ReadInternalValueLabels(TDOMElement(SubElem), LocalValueLabel, LocalFmt);

    if LocalValueLabel.Count = 0 then
      ReportXmlError(EPI_XML_TAG_MISSING, 0,
        'Internal or External section missing from ValueLabel tag.', []);

    ElemNode := TDOMElement(ElemNode.NextSibling);
  end;
end;

procedure TEpiDataFile.ReadExternalValueLabels(RootNode: TDOMElement;
  ValueLabelSet: TValueLabelSet);
var
  LocalDf: TEpiDataFile;
  Importer: TEpiImportExport;
  ValueField,
  LabelField: TEpiField;
  Idx: Integer;
  ElemNode: TDOMNode;
  TmpStr: String;
  i: Integer;
begin
  // EXTERNAL Value labels...
  // ------------------------
  ElemNode := RootNode.FindNode('File');
  if not Assigned(ElemNode) then
    ReportXmlError(EPI_XML_TAG_MISSING, 0,
      'External Valuelabel: File TAG not specified.', []);

  Importer := TEpiImportExport.Create;
  Importer.OnPassword := OnPassword;
  Importer.OnProgress := OnProgress;
  Importer.OnTranslate := OnTranslate;
  TmpStr := UTF8Encode(ElemNode.TextContent);
  if not Importer.Import(TmpStr, LocalDf, dftNone) then
    ReportXmlError(EPI_DATAFILE_NOT_OPEN, 0,
      'Datafile %s could not be opened', [TmpStr]);

  if LocalDf.FieldCount < 2 then
    ReportXmlError(EPI_DATAFILE_NOT_OPEN, 0,
      'Datafile %s must have at least 2 fields', [TmpStr]);

  if LocalDf.Size = 0 then
    ReportXmlError(EPI_DATAFILE_NOT_OPEN, 0,
      'Datafile %s does not contain any records', [TmpStr]);

  Idx := -1;
  ElemNode := RootNode.FindNode('ValueField');
  if Assigned(ElemNode) then
    ValueField := LocalDf.FieldByName(UTF8Decode(ElemNode.TextContent))
  else
    ValueField := LocalDf[++Idx];

  ElemNode := RootNode.FindNode('LabelField');
  if Assigned(ElemNode) then
    LabelField := LocalDf.FieldByName(UTF8Decode(ElemNode.TextContent))
  else
    LabelField := LocalDf[++Idx];

  // Check for same field.
  if (LabelField = ValueField) then
    // Case where the two tags are the same.
    if Idx = -1 then
      ReportXmlError(EPI_XML_ERROR, 0,
        'ValueField and LabelField must be different', [])
    else
    // Case where ValueField is defined, but not LabelField.
      LabelField := LocalDf[++Idx];

  for i := 1 to LocalDf.Size do
    ValueLabelSet.AddValueLabelPair(ValueField.AsValue[i], LabelField.AsString[i]);

  FreeAndNil(LocalDf);
  Importer.Free;
end;

procedure TEpiDataFile.ReadInternalValueLabels(RootNode: TDOMElement;
  ValueLabelSet: TValueLabelSet; LocalFmt: TFormatSettings);
var
  ElemNode: TDOMElement;
  Val: Variant;
  TmpStr: String;
begin
  // INTERNAL Value labels...
  // ------------------------
  ElemNode := TDOMElement(RootNode.FirstChild);
  while Assigned(ElemNode) do
  begin
    if UTF8Encode(ElemNode.NodeName) <> 'Set' then
      ReportXmlError(EPI_XML_UNKNOWN_TAG, 0,
        'Unknown TAG placed in Internal section: %s', [UTF8Encode(ElemNode.NodeName)]);

    TmpStr := UTF8Encode(ElemNode.GetAttribute('value'));
    if TmpStr = '' then
      ReportXmlError(EPI_XML_ATTR_MISSING, 0,
        'Attribute "value" is missing in valuelabel %s', [ValueLabelSet.Name]);

    Case ValueLabelSet.LabelType of
      ftInteger: Val := StrToInt(TmpStr);
      ftFloat:   Val := StrToFloat(TmpStr, LocalFmt);
      ftString,
      ftBoolean: Val := TmpStr;
    end;
    TmpStr := UTF8Encode(ElemNode.GetAttribute('label'));
    ValueLabelSet.AddValueLabelPair(Val, TmpStr, UTF8Encode(ElemNode.GetAttribute('missing')) = '1');

    ElemNode := TDOMElement(ElemNode.NextSibling);
  end;
end;

procedure TEpiDataFile.ReadScreen(RootNode: TDOMElement);
var
  ElemNode: TDOMNode;
begin
  ElemNode := RootNode.FindNode('Form');
  if Assigned(ElemNode) then
  begin
    // TODO : Form Colours
  end;

  ElemNode := RootNode.FindNode('Colours');
  if Assigned(ElemNode) then
    ReadScreenColours(TDOMElement(ElemNode));
end;

procedure TEpiDataFile.ReadScreenColours(RootNode: TDOMElement);
var
  ElemNode: TDOMElement;
  SubElem: TDOMNode;
  LocalScreenProp: TEpiScreenProperty;
  TmpStr: String;
begin
  ElemNode := TDOMElement(RootNode.FirstChild);
  while Assigned(ElemNode) do
  begin
    if UTF8Encode(ElemNode.NodeName) <> 'Colour' then
      ReportXmlError(EPI_XML_UNKNOWN_TAG, 0,
        'Unknown TAG placed in Colours section: %s', [UTF8Encode(ElemNode.NodeName)]);
    LocalScreenProp := TEpiScreenProperty.Create(ScreenProperties);

    TmpStr := UTF8Encode(ElemNode.GetAttribute('id'));
    if TmpStr = '' then
      ReportXmlError(EPI_XML_ATTR_MISSING, 0,
        'Colour attribute Id not specified.', []);
    LocalScreenProp.Id := UTF8Encode(TmpStr);

    SubElem := ElemNode.FindNode('Name');
    if not Assigned(SubElem) then
      ReportXmlError(EPI_XML_TAG_MISSING, 0,
        'Colour Name tag not specified.', []);
    LocalScreenProp.Name := UTF8Encode(SubElem.TextContent);

    SubElem := ElemNode.FindNode('ForeGround');
    if not Assigned(SubElem) then
      ReportXmlError(EPI_XML_TAG_MISSING, 0,
        'Colour ForeGround tag not specified.', []);
    LocalScreenProp.FgColour := Hex2Dec(SubElem.TextContent);

    SubElem := ElemNode.FindNode('BackGround');
    if not Assigned(SubElem) then
      ReportXmlError(EPI_XML_TAG_MISSING, 0,
        'Colour BackGround tag not specified.', []);
    LocalScreenProp.BgColour := Hex2Dec(SubElem.TextContent);

    SubElem := ElemNode.FindNode('HighLight');
    if not Assigned(SubElem) then
      ReportXmlError(EPI_XML_TAG_MISSING, 0,
        'Colour HighLight tag not specified.', []);
    LocalScreenProp.HlColour := Hex2Dec(SubElem.TextContent);

    ScreenProperties.Add(LocalScreenProp);

    ElemNode := TDOMElement(ElemNode.NextSibling);
  end;
end;

procedure TEpiDataFile.ReadTextLabels(RootNode: TDOMElement);
var
  ElemNode: TDOMElement;
  LocalTextLabel: TEpiTextLabel;
  TmpStr: String;
  SubElem: TDOMNode;
  LocalScreenProp: TEpiScreenProperty;
begin
  ElemNode := TDOMElement(RootNode.FirstChild);
  while Assigned(ElemNode) do
  begin
    if UTF8Encode(ElemNode.NodeName) <> 'TextLabel' then
      ReportXmlError(EPI_XML_UNKNOWN_TAG, 0,
        'Unknown TAG placed in TextLabels section: %s', [UTF8Encode(ElemNode.NodeName)]);
    LocalTextLabel := TEpiTextLabel.Create(TextLabels);

    TmpStr := UTF8Encode(ElemNode.GetAttribute('id'));
    if TmpStr = '' then
      ReportXmlError(EPI_XML_ATTR_MISSING, 0,
        'TextLabel attribute Id not specified.', []);
    LocalTextLabel.Id := UTF8Encode(TmpStr);

    SubElem := ElemNode.FindNode('ScreenRef');
    if Assigned(SubElem) then
    begin
      if not ScreenProperties.ScreenPropertyExists(UTF8Decode(SubElem.TextContent), LocalScreenProp) then
        ReportXmlError(EPI_XML_DESTINATION_MISSING, 0,
          'Textlabel ScreenRef %d does not exists.', [UTF8Decode(SubElem.TextContent)])
    end else
      LocalScreenProp := ScreenProperties.DefaultScreenProperty;
    LocalTextLabel.ScreenProp := LocalScreenProp;

    SubElem := ElemNode.FindNode('Text');
    if Assigned(SubElem) then
      LocalTextLabel.Text := UTF8Decode(SubElem.TextContent);

    TextLabels.Add(LocalTextLabel);

    ElemNode := TDOMElement(ElemNode.NextSibling);
  end;
end;

procedure TEpiDataFile.ReadFields(RootNode: TDOMElement);
var
  List: TStringList;
  ElemNode: TDOMElement;
  SubElem: TDOMElement;
  TmpField: TEpiField;
  LocalScreenProp: TEpiScreenProperty;
  LocalValueLabel: TValueLabelSet;
  SubSection: TDOMNode;
begin
  ElemNode := TDOMElement(RootNode.FirstChild);
  while Assigned(ElemNode) do
  begin
    if UTF8Encode(ElemNode.NodeName) <> 'Field' then
      ReportXmlError(EPI_XML_UNKNOWN_TAG, 0,
        'Unknown TAG placed in Fields section: %s', [UTF8Encode(ElemNode.NodeName)]);

    // *****************
    // Must exists tags:
    // *****************
    SubElem := TDOMElement(ElemNode.FindNode('Type'));
    if not Assigned(SubElem) then
      ReportXmlError(EPI_XML_TAG_MISSING, 0,
        'Field Type not specified.', []);
    TmpField := TEpiField.CreateField(XmlNameToFieldType(UTF8Encode(SubElem.TextContent)), 0);

    with TmpField do
    begin
      Id            := UTF8Encode(ElemNode.GetAttribute('id'));
      FieldName     := UTF8Encode(ElemNode.FindNode('Name').TextContent);
      FieldLength   := StrToInt(ElemNode.FindNode('Length').TextContent);
      FieldDecimals := StrToInt(ElemNode.FindNode('Decimals').TextContent);

      SubElem := TDOMElement(ElemNode.FindNode('ScreenRef'));
      if Assigned(SubElem) then
      begin
        if not ScreenProperties.ScreenPropertyExists(UTF8Decode(SubElem.TextContent), LocalScreenProp) then
          ReportXmlError(EPI_XML_DESTINATION_MISSING, 0,
            'Field ScreenRef %d does not exists.', [UTF8Decode(SubElem.TextContent)])
      end else
        LocalScreenProp := ScreenProperties.DefaultScreenProperty;
      ScreenProps := LocalScreenProp;

      // *****************
      // Optional:
      // *****************
      // - Variable label
      SubSection := ElemNode.FindNode('FieldLabel');
      if Assigned(SubSection) then
      begin
        SubElem := TDOMElement(SubSection.FindNode('ScreenRef'));
        if Assigned(SubElem) then
        begin
          if not ScreenProperties.ScreenPropertyExists(UTF8Decode(SubElem.TextContent), LocalScreenProp) then
            ReportXmlError(EPI_XML_DESTINATION_MISSING, 0,
              'FieldLabel ScreenRef %d does not exists.', [UTF8Decode(SubElem.TextContent)])
        end else
          LocalScreenProp := ScreenProperties.DefaultScreenProperty;
        VarLabelScreenProps := LocalScreenProp;

        SubElem := TDOMElement(ElemNode.FindNode('Text'));
        if Assigned(SubElem) then
          VariableLabel := UTF8Decode(SubElem.TextContent);
      end;

      // - Valuelabel
      SubElem :=  TDOMElement(ElemNode.FindNode('ValueLabelRef'));
      if Assigned(SubElem) then
      begin
        if not ValueLabels.ValueLabelSetExits(UTF8Encode(SubElem.TextContent), LocalValueLabel) then
          ReportXmlError(EPI_XML_DESTINATION_MISSING, 0,
            'Field ValueLabelRef %d does not exists.', [UTF8Decode(SubElem.TextContent)])
      end else
        LocalValueLabel := nil;
      ValueLabelSet := LocalValueLabel;

      // - Default value
      SubElem :=  TDOMElement(ElemNode.FindNode('DefaultValue'));
      if Assigned(SubElem) then
        DefaultValue := UTF8Encode(SubElem.TextContent);

      // Optional - requires FieldProperties.
      // - Confirm
      SubElem :=  TDOMElement(ElemNode.FindNode('Confirm'));
      if Assigned(SubElem) then
        FieldProperties.Confirm := true;
      // - Repeat
      SubElem :=  TDOMElement(ElemNode.FindNode('Repeat'));
      if Assigned(SubElem) then
        FieldProperties.DoRepeat := true;
      // - Enter
      SubElem :=  TDOMElement(ElemNode.FindNode('Enter'));
      if Assigned(SubElem) then
      begin
        if UTF8Encode(WideLowerCase(SubElem.TextContent)) = 'true' then
          FieldProperties.EntryType := entMust
        else
          FieldProperties.EntryType := entNone;
      end;
      // - Jumps
      SubSection := ElemNode.FindNode('Jumps');
      if Assigned(SubSection) then
      begin
        SubElem := TDOMElement(SubSection.FirstChild);
        while Assigned(SubElem) do
        begin
          FieldProperties.Jumps.AddObject(UTF8Encode(SubElem.GetAttribute('on')),
            TString.Create(UTF8Encode(SubElem.GetAttribute('to'))));
          SubElem := TDOMElement(SubElem.NextSibling);
        end;
        if SubSection.Attributes.Length > 0 then
          FieldProperties.JumpResetValue := UTF8Encode(SubSection.Attributes[0].TextContent);
      end;
      // - Range
      SubElem := TDOMElement(ElemNode.FindNode('Range'));
      if Assigned(SubElem) then
      begin
        List := nil;
        SplitString(UTF8Encode(SubElem.TextContent), List, [',']);
        FieldProperties.Ranges.Assign(List);
      end;
      // - Top of Screen (reposition to top of screen)
      SubElem := TDOMElement(ElemNode.FindNode('TopOfScreen'));
      if Assigned(SubElem) then
        FieldProperties.TopOfScreen := StrToInt(SubElem.TextContent);
      // - TypeComment (place a label next to edit field)
      SubElem := TDOMElement(ElemNode.FindNode('TypeComment'));
      if Assigned(SubElem) then
      begin
        if SubElem.GetAttribute('colour') <> '' then
        begin
          FieldProperties.TypeType := ttComment;
          FieldProperties.TypeColour := Hex2Dec(SubElem.GetAttribute('colour'));
        end else
        if SubElem.GetAttribute('field') <> '' then
        begin
          FieldProperties.TypeType := ttField;
          FieldProperties.TypeField := FieldByName(UTF8Encode(SubElem.GetAttribute('colour')));
        end;
      end;
    end;

    AddField(TmpField);
    ElemNode := TDOMElement(ElemNode.NextSibling);
  end;
end;

procedure TEpiDataFile.ReadRecords(RootNode: TDOMElement;
  FmtSettings: TFormatSettings);
var
  ElemNode: TDOMElement;
  CurRec: Integer;
  i: Integer;
  TmpField: TEpiField;
  TmpStr: String;
begin
  CurRec := 1;
  Size := RootNode.ChildNodes.Count;
  ElemNode := TDOMElement(RootNode.FirstChild);
  while Assigned(ElemNode) do
  begin
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
        TmpStr := UTF8Encode(ElemNode.Attributes[i].NodeValue);
        if TmpField.FieldType = ftCrypt then
        begin
          TmpStr := B64Decode(TmpStr);
          FCrypter.DecryptCFB(TmpStr[1], TmpStr[1], Length(TmpStr));
          TmpStr := Trim(TmpStr);
          FCrypter.Reset;
        end;
        TmpField.AsString[CurRec] := TmpStr;;
      end;
    end;
    inc(CurRec);
    ElemNode := TDOMElement(ElemNode.NextSibling);
  end;
end;

function TEpiDataFile.Ins(Lvl: integer): string;
begin
  result := DupeString(' ', Lvl * 2);
end;

procedure TEpiDataFile.SetId(const AValue: string);
begin
  if FId = AValue then exit;
  FId := AValue;
end;

procedure TEpiDataFile.WriteSettings(St: TStream);
var
  TmpStr: String;
  EncData: String;

  function RequirePassword: boolean;
  var
    i: Integer;
  begin
    result := true;
    for i := 0 to FieldCount -1 do
      if Fields[i].FieldType = ftCrypt then
        Exit;
    result := false;
  end;

begin
  TmpStr :=
    Ins(1) + '<Settings>' + LineEnding +
    Ins(2) + '<Version>' + IntToStr(FileVersion) + '</Version>' + LineEnding +
    Ins(2) + '<DateSeparator>' + EpiInternalFormatSettings.DateSeparator + '</DateSeparator>' + LineEnding +
    Ins(2) + '<DecimalSeparator>' + EpiInternalFormatSettings.DecimalSeparator + '</DecimalSeparator>' + LineEnding +
    Ins(2) + '<MissingMark>' +  StringToXml(TEpiStringField.DefaultMissing) + '</MissingMark>' + LineEnding;
  if RequirePassword then
  begin
    // TODO : What about UTF-8 encoding??
    if Assigned(OnPassword) then OnPassword(self, rpCreate, FPassWord);
    if Password = '' then
      raise Exception.Create('A password is needed for data files with encrypted fields');
    FCrypter.InitStr(Password);
    EncData := Trim(Password);
    FCrypter.EncryptCFB(EncData[1], EncData[1], Length(EncData));
    EncData := B64Encode(EncData);
    FCrypter.Reset;
    TmpStr := TmpStr +
      Ins(2) + '<Password>' + EncData + '</Password>' + LineEnding;
  end;
  TmpStr := TmpStr +
    Ins(1) + '</Settings>' + LineEnding;
  St.Write(TmpStr[1], Length(TmpStr));
end;

procedure TEpiDataFile.WriteMetaData(St: TStream);
var
  TmpStr: String;
begin
  TmpStr :=
    Ins(1) + '<MetaData>' + LineEnding;
  if FileLabel <> '' then
    TmpStr := TmpStr +
      Ins(2) + '<FileLabel>' + StringToXml(FileLabel) + '</FileLabel>' + LineEnding;
  if Study <> '' then
    TmpStr := TmpStr +
      Ins(2) + '<Study>' + StringToXml(Study) + '</Study>' + LineEnding;
  St.Write(TmpStr[1], Length(TmpStr));

  // TODO : <USERDEFINED>
  TmpStr :=
    Ins(1) + '</MetaData>' + LineEnding;
  St.Write(TmpStr[1], Length(TmpStr));
end;


function TEpiDataFile.InternalOpen: boolean;
var
  RecXml: TXMLDocument;
  RootNode: TDOMElement;
  ElemNode,
  SubElem: TDOMElement;
  PairNode: TDOMElement;
  SectionNode: TDOMNode;
  TmpStr: String;
  TmpFieldType: TFieldType;
  TmpField: TEpiField;
  CurRec: Integer;
  i: Integer;
  Idx: LongInt;
  LocalFmt: TFormatSettings;
  MissingStr: String;
  SubSectionNode: TDOMNode;
  LocalValueLabel: TValueLabelSet;
  LocalDf: TEpiDataFile;
  ValueField: TEpiField;
  TextField: TEpiField;
  Val: Variant;
  List: TStrings;
  TagList: TStringList;
begin
  EpiLogger.IncIndent;
  EpiLogger.Add(ClassName, 'InternalOpen', 2, 'Filename = ' + Filename);
  result := false;

  DatafileType := dftEpiDataXml;

  try
    ReadXMLFile(RecXml, FileName);

    // **********************
    // Global <EpiData> structure
    // **********************
    RootNode := RecXml.DocumentElement;

    // **********************
    // <Settings> Section
    // **********************
    SectionNode := RootNode.FindNode('Settings');
    if not Assigned(SectionNode) then
      ReportXmlError(EPI_XML_TAG_MISSING, 0,
        'Settings section missing in file: %s', [FileName]);
    ReadSettings(TDOMElement(SectionNode), LocalFmt);

    // **********************
    // <MetaData> Section
    // **********************
    SectionNode := RootNode.FindNode('MetaData');
    if Assigned(SectionNode) then
      ReadMetaData(TDOMElement(SectionNode), LocalFmt);

    // **********************
    // <Screen> Section
    // **********************
    SectionNode := RootNode.FindNode('Screen');
    if Assigned(SectionNode) then
      ReadScreen(TDOMElement(SectionNode));

    // **********************
    // <TextLabels> Section
    // **********************
    SectionNode := RootNode.FindNode('TextLabels');
    if Assigned(SectionNode) then
      ReadTextLabels(TDOMElement(SectionNode));

    // **********************
    // <Fields> Section
    // **********************
    SectionNode := RootNode.FindNode('Fields');
    if Assigned(SectionNode) then
      ReadFields(TDOMElement(SectionNode));

    // **********************
    // <RECORDS> Section
    // **********************
    SectionNode := RootNode.FindNode('Records');
    if Assigned(SectionNode) then
      ReadRecords(TDOMElement(SectionNode), LocalFmt);

  finally
    EpiLogger.DecIndent;
    if Assigned(RecXml) then FreeAndNil(RecXml);
  end;
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
  EpiLogger.IncIndent;
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
          EncData := B64Decode(TmpStr);
          FCrypter.DecryptCFB(EncData[1], EncData[1], Length(EncData));
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
  end;
end;

function TEpiDataFile.InternalSave: boolean;
var
  CurField: Integer;
  CurRec: Integer;
  TmpStr: String;
  DataStream: TFileStream;
  EncData: String;
  j: Integer;
  i: Integer;
  IdNo: Integer;
begin
  EpiLogger.IncIndent;
  EpiLogger.Add(Classname, 'InternalSave', 3);
  result := false;
  DataStream := nil;

  try
    DataStream := TFileStream.Create(FileName, fmCreate);
    DatafileType := dftEpiDataXml;
    UpdateProgress(0, Lang(0, 'Writing header.'));

    // **********************
    // Global <EPIDATA> structure
    // **********************
    TmpStr := '<?xml version="1.0" encoding="utf-8"?>' + LineEnding +
      '<EpiData>' + LineEnding;
    DataStream.Write(TmpStr[1], Length(TmpStr));


    // **********************
    // <SETTINGS> Section
    // **********************
    WriteSettings(DataStream);

    // **********************
    // <METADATA> Section
    // **********************
    WriteMetaData(DataStream);

    // **********************
    // <SCREEN> Section
    // **********************
    TmpStr := Ins(1) +
      '<Screen>' + LineEnding;
    // Form data:

    // Field


    // **********************
    // <FIELDS> Section
    // **********************
    TmpStr :=
      Ins(1) + '<Fields>' + LineEnding;
    DataStream.Write(TmpStr[1], Length(TmpStr));
    IdNo := 1;
    for CurField := 0 to Fields.Count - 1 do
    with Fields[CurField] do
    begin
      TmpStr :=
        Ins(2) + '<Field Id="F' + IntToStr(IdNo) + '">' + LineEnding;
      TmpStr := TmpStr +
        Ins(3) + '<Type>' + FieldTypeXmlNames[FieldType] + '</Type>' + LineEnding;
      // Must exists tags!
      TmpStr +=
        Ins(3) + '<Name>' + StringToXml(FieldName) + '</Name>' + LineEnding +
        Ins(3) + '<Length>' + IntToStr(FieldLength) + '</Length>' + LineEnding +
        Ins(3) + '<Decimals>' + IntToStr(FieldDecimals) + '</Decimals>' + LineEnding +
        Ins(3) + '<ScreenRef>' + StringToXml(ScreenProps.Id) + '</ScreenRef>' + LineEnding +

      // Optional, but we choose to write out of courtesy.
        Ins(3) + '<FieldLabel>' + LineEnding +
        Ins(4) + '<ScreenRef>' + StringToXml(VarLabelScreenProps.Id) + '</ScreenRef>' + LineEnding +
        Ins(4) + '<Text>' + StringToXml(VariableLabel) + '</Text>' + LineEnding +
        Ins(3) + '</FieldLabel>' + LineEnding;
      Inc(IdNo);

      // Optional:
      if Assigned(ValueLabelSet) then
        TmpStr := TmpStr +
          Ins(3) + '<ValueLabelRef>' + StringToXml(ValueLabelSet.Id) + '</ValueLabelRef>' + LineEnding;
      if DefaultValue <> '' then
        TmpStr := TmpStr +
          Ins(3) + '<DefaultValue>' + StringToXml(DefaultValue) + '</DefaultValue>' + LineEnding;

      if HasFieldProperties then
      With FieldProperties Do
      begin
        if Confirm then
          TmpStr := TmpStr + Ins(3) + '<Confirm/>' + LineEnding;
        if DoRepeat then
          TmpStr := TmpStr + Ins(3) + '<Repeat/>' + LineEnding;
        Case EntryType of
          entMust: TmpStr := TmpStr + Ins(3) + '<Enter val="TRUE"/>' + LineEnding;
          entNone: TmpStr := TmpStr + Ins(3) + '<Enter val="FALSE"/>' + LineEnding;
        end;
        if Jumps.Count > 0 then
        begin
          TmpStr := TmpStr + Ins(3) + '<Jumps>' + LineEnding;
          For i := 0 to Jumps.Count - 1 do
            TmpStr := TmpStr + Ins(4) + '<Jump on="' + Jumps[i] +
                      '" to="' + StringToXml(TString(Jumps.Objects[i]).Str) + '"/>' + LineEnding;
          TmpStr := TmpStr + Ins(3) + '</Jumps>' + LineEnding;
        end;
        if Ranges.Count > 0 then
        begin
          TmpStr := TmpStr + Ins(3) + '<Range>';
          for i := 0 to Ranges.Count - 1 do
            TmpStr := TmpStr + StringToXml(Ranges[i]);
          TmpStr := TmpStr + '</Range>' + LineEnding;
        end;
        if TopOfScreen >= 0 then
          TmpStr := TmpStr +
            Ins(3) + '<TopOfScreen>' + IntToStr(TopOfScreen) + '</TopOfScreen>' + LineEnding;
        Case TypeType of
          ttComment: TmpStr := TmpStr +
            Ins(3) + '<TypeComment colour="' + IntToHex(TypeColour, 6) + '"/>' + LineEnding;
          ttField: TmpStr := TmpStr +
            Ins(3) + '<TypeComment field="' + StringToXml(TypeField.FieldName) + '"/>' + LineEnding;
        end;
        // TODO : Keys!
        //      if DataFile.IndexFile.IndexExists();
      End;

      // End tag:
      TmpStr := TmpStr + Ins(2) + '</Field>' + LineEnding;
      DataStream.Write(TmpStr[1], Length(TmpStr));
    end;
    TmpStr := Ins(1) +
      '</Fields>' + LineEnding;
    DataStream.Write(TmpStr[1], Length(TmpStr));

    // **********************
    // <RECORDS> Section
    // **********************
    TmpStr :=
      Ins(1) + '<Records>' + LineEnding;
    DataStream.Write(TmpStr[1], Length(TmpStr));
    for CurRec := 1 to Size do
    begin
      UpdateProgress(Trunc((CurRec / Size) * 100), Lang(0, 'Writing records.'));
      TmpStr :=
        Ins(2) + '<REC';
      for CurField := 0 to FieldCount - 1 do
      with Fields[CurField] do
      begin
        if FieldType = ftQuestion then continue;
        TmpStr += ' F' + IntToStr(CurField + 1) + '="';
        Case FieldType of
          ftCrypt:
            begin
              EncData := AsString[CurRec];
              FCrypter.InitStr(Password);
              FCrypter.EncryptCFB(EncData[1], EncData[1], Length(EncData));
              TmpStr += B64Encode(EncData);
              FCrypter.Reset;
            end;
          ftString:
            TmpStr += StringToXml(AsString[CurRec]);
        else
          TmpStr += AsString[CurRec];
        end;
        TmpStr += '"';
      end;
      if Verified[CurRec] then
        TmpStr += ' st="2"'
      else if Deleted[CurRec] then
        TmpStr += ' st="1"';
      TmpStr += '/>' + LineEnding;
      DataStream.Write(TmpStr[1], Length(TmpStr));
    end;
    TmpStr := '  </Records>' + LineEnding;
    TmpStr := TmpStr +
      '</EpiData>';
    DataStream.Write(TmpStr[1], Length(TmpStr));
    UpdateProgress(100, Lang(0, 'Complete.'));
    Result := true;
  finally
    EpiLogger.DecIndent;
    if Assigned(DataStream) then FreeAndNil(DataStream);
  end;
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
  EpiLogger.IncIndent;
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
      FCrypter.InitStr(Password);
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
      FCrypter.EncryptCFB(EncData[1], EncData[1], Length(EncData));
      EncData := B64Encode(EncData);
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
          FCrypter.InitStr(Password);
          FCrypter.EncryptCFB(EncData[1], EncData[1], Length(EncData));
          EncData := B64Encode(EncData);
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

function TEpiDataFile.GetFieldCount: Cardinal;
begin
  result := Fields.Count;
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

constructor TEpiDataFile.Create(ASize: Cardinal = 0);
var
  p: pointer;
begin
  inherited Create;
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
  Result := TEpiDataFile.Create(Size);

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

function TEpiDataFile.Open(const aFileName: string; aOptions: TEpiDataFileOptions = []): boolean;
var
  TmpStream: TFileStream;
  Ext: string;
begin
  EpiLogger.IncIndent;
  EpiLogger.Add(Classname, 'Open', 2, 'Filename = "' + aFilename + '"');
  try
    FFileName := aFileName;
    FOptions := aOptions;

    Ext := ExtractFileExt(FileName);
    if AnsiUpperCase(Ext) = '.REC' then
      result := InternalOpenOld
    else if AnsiUpperCase(Ext) = '.RECXML' then
      result := InternalOpen
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

function TEpiDataFile.Save(Const aFileName: string; aOptions: TEpiDataFileOptions = []): boolean;
var
  Ext: string;
begin
  EpiLogger.IncIndent;
  EpiLogger.Add(Classname, 'Save', 2, 'Filename = "' + aFilename + '"');
  try
    FFileName := aFileName;
    FOptions := aOptions;

    if AnsiUpperCase(ExtractFileExt(FileName)) = '.RECXML' then
      result := InternalSave
    else
      Result := InternalSaveOld;
  finally
    EpiLogger.DecIndent;
  end;
end;

procedure TEpiDataFile.SaveToStream(St: TStream; Lvl: Integer);
var
  S: String;
begin
  S :=
    Ins(Lvl)     + '<DataFile id="' + Id + '">' + LineEnding +
    Ins(Lvl + 1) + '<FileLabel>' + FileLabel + '<FileLabel>' + LineEnding;
  St.Write(S[1], Length(S));

  ValueLabels.SaveToStream(St, Lvl + 1);
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

  FValueLabels  := TValueLabelSets.Create;
  FCheckFile    := TEpiDataFileProperties.Create;
  FCrypter      := TDCP_rijndael.Create(nil);
  FRecordStatus := TEpiIntField.Create(0, ftInteger);
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

