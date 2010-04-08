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

{      if Assigned(ValueLabelSet) then
      for j := 0 to ValueLabelSet.Count -1 do
      begin
        // Left adjust at index 67.
        TmpStr := String(ValueLabelSet.Values[j]) + ': ' + ValueLabelSet.Labels[j];
        if ValueLabelSet.MissingValues[j] then
          TmpStr += '(missing)';
        Append(WideFormat('%-67.67s %-20.20s', ['', UTF8Decode(TmpStr)]));
      end;       }
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

