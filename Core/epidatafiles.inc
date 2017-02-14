{%MainUnit epidatafiles.pas}

type

  { FAllItemsList }

  TAllItemsList = class (TEpiCustomControlItemList)
  public
    constructor Create(AOwner: TEpiCustomBase); override;
  end;

{ FAllItemsList }

constructor TAllItemsList.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
end;

{ TEpiDataFiles }

function TEpiDataFiles.GetDataFile(Index: integer): TEpiDataFile;
begin
  result := TEpiDataFile(Items[Index]);
end;

function TEpiDataFiles.GetSettings: TEpiXMLSettings;
begin
  result := TEpiDocument(Owner).XMLSettings;
end;

function TEpiDataFiles.XMLName: string;
begin
  Result := rsDataFiles;
end;

function TEpiDataFiles.Prefix: string;
begin
  Result := 'dataform_';
end;

function TEpiDataFiles.NewItemLoad(const AName: EpiString;
  AItemClass: TEpiCustomItemClass): TEpiCustomItem;
begin
  if AName = EpiSecurityLogDatafileName then
//    AItemClass := TEpiSecurityDatafile;
    result := GetDataFileByName(EpiSecurityLogDatafileName)
  else
    result := inherited NewItemLoad(AName, AItemClass);
end;

constructor TEpiDataFiles.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
end;

function TEpiDataFiles.ItemClass: TEpiCustomItemClass;
begin
  result := TEpiDataFile;
end;

function TEpiDataFiles.NewDataFile: TEpiDataFile;
begin
  Result := TEpiDataFile(NewItem(TEpiDataFile));
end;

function TEpiDataFiles.GetEnumerator: TEpiDataFilesEnumerator;
begin
  result := TEpiDataFilesEnumerator.Create(Self);
end;

function TEpiDataFiles.GetDataFileByName(const AName: string): TEpiDataFile;
begin
  result := TEpiDataFile(GetItemByName(AName));
end;

{ TEpiDataFile }

function TEpiDataFile.GetField(Index: integer): TEpiField;
begin
  result := TEpiField(Fields[Index]);
end;

function TEpiDataFile.GetSection(Index: integer): TEpiSection;
begin
  result := TEpiSection(Sections[Index]);
end;

function TEpiDataFile.GetHeading(Index: integer): TEpiHeading;
begin
  result := TEpiHeading(Headings[Index]);
end;

function TEpiDataFile.GetValueLabelSet(Index: integer): TEpiValueLabelSet;
begin
  result := ValueLabels[Index];
end;

function TEpiDataFile.GetDeleted(Index: integer): boolean;
begin
  result := FRecordStatus.AsInteger[Index] = Ord(rsDeleted);
end;

function TEpiDataFile.GetControlItem(Index: integer): TEpiCustomControlItem;
begin
  result := TEpiCustomControlItem(FAllControlItems[Index]);
end;

function TEpiDataFile.GetDeletedCount: Integer;
var
  FSize: Integer;
  i: Integer;
begin
  Result := 0;

  FSize := Size;
  for i := 0 to FSize - 1 do
    if Deleted[i] then inc(Result);
end;

function TEpiDataFile.GetValueLabelSets: TEpiValueLabelSets;
begin
  result := TEpiDocument(RootOwner).ValueLabelSets;
end;

function TEpiDataFile.GetVerified(Index: integer): boolean;
begin
  result := FRecordStatus.AsInteger[Index] = Ord(rsVerified);
end;

function TEpiDataFile.GetSize: Integer;
begin
  result := FRecordStatus.Size;
end;

function TEpiDataFile.GetVerifiedCount: Integer;
var
  FSize: Integer;
  i: Integer;
begin
  Result := 0;

  FSize := Size;
  for i := 0 to FSize - 1 do
    if Verified[i] then inc(Result);
end;

procedure TEpiDataFile.LocalEventHook(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGrp: TEpiEventGroup; EventType: Word;
  Data: pointer);
var
  F: TEpiField;
  H: TEpiHeading;
  i: Integer;
  S: TEpiSection;
  Item: TEpiCustomItem;
begin
  // Skip hook if we recieve messages we do not care about.
  if EventGrp <> eegCustomBase
  then
    Exit;

  if not (TEpiCustomChangeEventType(EventType) in [ecceAddItem, ecceDelItem])
  then
    Exit;

  // Skip event if initiated from AllControls.
  if (Initiator = FAllControlItems) or
     (Initiator = FAllFields) or
     (Initiator = FAllHeadings)
  then
    Exit;

  if (Initiator is TEpiFields) then
  begin
    F := TEpiField(Data);

    if TEpiCustomChangeEventType(EventType) = ecceDelItem
    then
      Fields.RemoveItem(F)
    else
      begin
        // Check that id is not in conflict with others.
        if not ValidateRename(F.Name) then
          F.Name := Fields.GetUniqueItemName(TEpiField);
        Fields.AddItem(F);
        F.Size := Size;
        F.FDataFile := Self;
      end
  end;

  if (Initiator is TEpiHeadings) then
  begin
    H := TEpiHeading(Data);

    if TEpiCustomChangeEventType(EventType) = ecceDelItem
    then
      Headings.RemoveItem(H)
    else
      begin
        // Check that id is not in conflict with others.
        if not ValidateRename(H.Name) then
          H.Name := Headings.GetUniqueItemName(TEpiHeading);
        H.FDataFile := Self;
        Headings.AddItem(TEpiHeading(Data));
      end
  end;

  if (Initiator is TEpiSections) then
  begin
    S := TEpiSection(Data);

    if TEpiCustomChangeEventType(EventType) = ecceDelItem
    then
      begin
        if (ebsDestroying in State) then exit;

        for i := 0 to S.Fields.Count - 1 do
          S.Fields.DoChange(eegCustomBase, Word(ecceDelItem), S.Fields[i]);
        S.Fields.OnValidateRename := nil;

        for i := 0 to S.Headings.Count - 1 do
          S.Headings.DoChange(eegCustomBase, Word(ecceDelItem), Heading[i]);
        S.Headings.OnValidateRename := nil;
      end
    else
      begin
        // Check that id is not in conflict with others.
        if not Self.ValidateRename(S.Name) then
          S.Name := Sections.GetUniqueItemName(TEpiSection);

        S.FDataFile := Self;
        S.Fields.OnValidateRename := @Self.ValidateRename;
        S.Fields.Sorted := true;
        for i := 0 to S.Fields.Count - 1 do
          S.Fields.DoChange(eegCustomBase, Word(ecceAddItem), S.Fields[i]);

        S.Headings.OnValidateRename := @Self.ValidateRename;
        S.Headings.Sorted := true;
        for i := 0 to S.Headings.Count - 1 do
          S.Headings.DoChange(eegCustomBase, Word(ecceAddItem), S.Heading[i]);
      end;
  end;

  if TObject(Data).InheritsFrom(TEpiCustomControlItem) then
    if TEpiCustomChangeEventType(EventType) = ecceDelItem
    then
      ControlItems.RemoveItem(TEpiCustomItem(Data))
    else
      ControlItems.AddItem(TEpiCustomItem(Data));
end;

procedure TEpiDataFile.LoadRecords(Root: TDOMNode);
var
  CurRec: Integer;
  Node: TDOMElement;
  i: Integer;
  TmpField: TEpiField;
  FieldList: TFPDataHashTable;
  DataList: TStringList;
  FN: String;

const
  DummyString = #243#176#80#80;  // Unicode private area: http://www.utf8-chartable.de/unicode-utf8-table.pl?start=983040

var
  DeCrypter: TDCP_rijndael;
  n: String;
  ProgressMethod: TEpiProgressEvent;
  NodeCount: LongWord;
  NodeNum: LongWord;
  L: Integer;
  PEnd: PChar;
  PCur: PChar;
  PStart: PChar;
  S: String;
  DocVersion: Integer;
  PLen: Integer;
  Data: String;
  Salt: String;
  EscapedQuotes: Boolean;

  function Dummyfy(Const S: string): string;
  begin
    result := StringReplace(S, '\,', DummyString, [rfReplaceAll]);
  end;

  function UnDummyfy(Const S: string): string;
  begin
    result := StringReplace(S, DummyString, DefaultFormatSettings.DecimalSeparator, [rfReplaceAll]);
  end;

  function LocalDeCrypt(Const S: string): string;
  begin
    if Assigned(DeCrypter) then
      Result := DeCrypter.DecryptString(S)
    else
      Result := S;
  end;

  function DoProgress(ProgressType: TEpiProgressType;
    Count, Max: Cardinal):boolean; inline;
  begin
    Result := false;
    if Assigned(ProgressMethod) then
      ProgressMethod(Self, ProgressType, Count, Max, Result);
  end;

begin
  // Root = <Records>
  FLoadingRecs := true;
  FRecModifiedDate := LoadAttrDateTime(Root, rsModifiedAttr, '', FRecModifiedDate, false);

  CurRec := 0;
  DeCrypter := nil;

  FieldList := TFPDataHashTable.CreateWith(Fields.Count, @RSHash);
  for i := 0 to Fields.Count -1 do
  begin
    n := Fields[i].Name;
    FieldList.Add(N, Fields[i]);
  end;

  DataList := TStringList.Create;
  DataList.Delimiter := ',';
  DataList.QuoteChar := '"';
  DataList.StrictDelimiter := true;

  DocVersion := TEpiDocument(RootOwner).Version;

  if (DocVersion >= 2) and (TEpiDocument(RootOwner).PassWord <> '') then
  begin
    DeCrypter := TDCP_rijndael.Create(nil);
    DeCrypter.InitStr(TEpiDocument(RootOwner).PassWord, TDCP_sha256);

    // With .epx v3 each <Records> section is uniquely salted, storing encrypted result
    // in the attribute rsEncryptedSalt;
    if (DocVersion >= 3) then
    begin
      Salt := LoadAttrString(Root, rsEncryptedSalt);
      // just run it through decryption, this will progress state of internal decryption vectors.
      DeCrypter.DecryptString(Salt);
    end;
  end;

  ProgressMethod := TEpiDocument(RootOwner).OnProgress;
  // div 2: Because all nodes have a single whitespace node after each "normal" node.
  NodeCount := (Root.ChildNodes.Count div 2);
  DoProgress(eptInit, 0, NodeCount);

  Node := TDOMElement(Root.FirstChild);
  BackupFormatSettings(TEpiDocument(RootOwner).XMLSettings.FormatSettings);

  PStart := nil;
  PLen   := 0;

  while Assigned(Node) do
  begin
    if DoProgress(eptRecords, CurRec, NodeCount) then
      HaltLoading;
    Inc(NodeNum);

    while NodeIsWhiteSpace(Node) do
      Node := TDOMElement(Node.NextSibling);
    if not Assigned(Node) then break;

    // Add space for new record.
    NewRecords(1);
    CheckNode(Node, rsRecord);

    case TEpiRecordState(LoadAttrEnum(Node, rsRecordStatus, TypeInfo(TEpiRecordState))) of
      rsNormal:   Verified[CurRec] := false;
      rsDeleted:  Deleted[CurRec] := true;
      rsVerified: Verified[CurRec] := true;
    end;

    if DocVersion <= 2 then
      begin
        DataList.DelimitedText := Dummyfy(LocalDeCrypt(Node.TextContent));
        for i := 0 to DataList.Count - 1 do
        begin
          DataList.GetNameValue(i, FN, S);
          TmpField := TEpiField(THTDataNode(FieldList.Find(FN)).Data);

          // Do not crash if reading bad entry/line.
          if not Assigned(TmpField) then
            continue;
          TmpField.AsString[CurRec] := UnDummyfy(S);
        end;
      end
    else
      begin
        S := LocalDeCrypt(Node.TextContent);
        L := Length(S);

        PStart := @S[1];
        PCur := PStart;

        while (PCur - PStart) < L do
        begin
          // Find field name (FN)
          PEnd := PCur;
          while PEnd^ <> '=' do Inc(PEnd);
          FN := Copy(S, (PCur - PStart) + 1, (PEnd - PCur));

          PCur := PEnd;
          Inc(PCur);

          if PCur^ = '"' then
            begin
              Inc(PCur);
              PEnd := PCur;

              // Detect 2xdouble quotes, which translates to a single double qoute: "" -> "
              EscapedQuotes := false;
              while true
              do
                begin
                  if (PEnd^ = '"') then
                    if ((Pend + 1)^ <> '"')
                     then
                       // A regular "Stop"
                       Break
                     else
                       begin
                         // 2x double quotes found! Step over next "
                         Inc(PEnd);
                         EscapedQuotes := true;
                       end;
                  Inc(PEnd);
                end;

{              while (PEnd^ <> '"')
              do
                Inc(PEnd);}

              Data := Copy(S, (PCur - PStart) + 1, (PEnd - PCur));

              if EscapedQuotes then
                Data := StringReplace(Data, '""', '"', [rfReplaceAll]);

              Inc(PEnd);
            end
          else
            begin
              PEnd := PCur;
              while (PEnd^ <> ';') and
                    (PEnd^ <> #0)
              do
                Inc(PEnd);

              Data := Copy(S, (PCur - PStart) + 1, (PEnd - PCur));
            end;

          Inc(PEnd);

          TmpField := TEpiField(THTDataNode(FieldList.Find(FN)).Data);
          TmpField.AsString[CurRec] := Data;

          PCur := PEnd;
        end;
      end;

    inc(CurRec);
    Node := TDOMElement(Node.NextSibling);
  end;

  DoProgress(eptFinish, NodeCount, NodeCount);

  if Assigned(DeCrypter) then FreeAndNil(DeCrypter);
  RestoreFormatSettings;

  FieldList.Free;
  DataList.Free;
  FLoadingRecs := false;
end;

procedure TEpiDataFile.LoadKeyFields(Root: TDOMNode);
var
  Node: TDOMNode;
  FieldId: String;
begin
  // Root = <KeyFields>

  Node := Root.FirstChild;
  while Assigned(Node) do
  begin
    while NodeIsWhiteSpace(Node) do
      Node := Node.NextSibling;
    if not Assigned(Node) then break;

    CheckNode(Node, 'Key');
    KeyFields.AddItem(Fields.FieldByName[LoadAttrString(Node, 'fieldRef')]);

    Node := Node.NextSibling;
  end;
end;

procedure TEpiDataFile.SetDeleted(Index: integer; const AValue: boolean);
var
  Val: PEpiDataFileStatusRecord;
begin
  if Ord(AValue) = FRecordStatus.AsInteger[Index] then exit;

  Val := New(PEpiDataFileStatusRecord);
  Val^.Index := Index;
  Val^.OldValue := TEpiRecordState(FRecordStatus.AsInteger[Index]);

  if AValue then
    FRecordStatus.AsInteger[Index] := Ord(rsDeleted)
  else
    FRecordStatus.AsInteger[Index] := Ord(rsNormal);

  Val^.NewValue := TEpiRecordState(FRecordStatus.AsInteger[Index]);

  DoChange(eegDataFiles, Word(edceRecordStatus), Val);
  Dispose(Val);
end;

procedure TEpiDataFile.SetSize(const AValue: Integer);
var
  i: Integer;
  Val: LongInt;
begin
  if AValue = Size then exit;

  Val := FRecordStatus.Size;
  FRecordStatus.Size := AValue;

  for i := Val to AValue - 1 do
    FRecordStatus.AsInteger[i] := Integer(rsNormal);

  for i := 0 to Fields.Count - 1 do
    Fields[i].Size := AValue;

  DoChange(eegDataFiles, Word(edceSize), @Val);
end;

procedure TEpiDataFile.SetVerified(Index: integer; const AValue: boolean);
var
  Val: PEpiDataFileStatusRecord;
begin
  if Ord(AValue) = FRecordStatus.AsInteger[Index] then exit;

  Val := New(PEpiDataFileStatusRecord);
  Val^.Index := Index;
  Val^.OldValue := TEpiRecordState(FRecordStatus.AsInteger[Index]);

  if AValue then
    FRecordStatus.AsInteger[Index] := Ord(rsVerified)
  else
    FRecordStatus.AsInteger[Index] := Ord(rsNormal);

  DoChange(eegDataFiles, Word(edceRecordStatus), Val);
  Dispose(Val);
end;

function TEpiDataFile.InternalCompareRecords(const SortList: TEpiFields;
  const Idx1, Idx2: Integer): integer;
var
  i: Integer;
begin
  for i := 0 to SortList.Count - 1 do
  begin
    result := SortList[i].Compare(Idx1, Idx2);
    if result <> 0 then exit;
  end;
end;

procedure TEpiDataFile.InternalSortRecords(const SortList: TEpiFields; L,
  R: Integer);

  procedure SortListExchangeRecords(Const Index1, Index2: integer);
  var
    i: integer;
  begin
    for i := 0 to SortList.Count - 1 do
      if SortList[i].FDataFile = Self then
        // This field has been sorted!
        Continue
      else
        // This field is not part of the ExchangeRecords
        SortList[i].Exchange(Index1, Index2);
  end;

var
   I, J, P: Integer;
begin
  I:=L;
  J:=R;
  P:=(L + R) shr 1;
  repeat
    while InternalCompareRecords(SortList, I,P) < 0 do Inc(I);
    while InternalCompareRecords(SortList, J,P) > 0 do Dec(J);
    if I <= J then
    begin
      ExchangeRecords(J,I);
      SortListExchangeRecords(J, I);
      if p=i then p:=j
      else if p=j then p:=i;
      Inc(I);
      Dec(J);
    end;
  until I>J;
  if L<J then InternalSortRecords(SortList, L,J);
  if I < R then InternalSortRecords(SortList, I, R);
end;

procedure TEpiDataFile.HaltLoading;
begin
  Raise EEpiDataFileHaltLoading.Create('Loading aborted by user!');
end;

procedure TEpiDataFile.SetVersion(const AValue: string);
begin
  if FVersion = AValue then exit;
  FVersion := AValue;
end;

procedure TEpiDataFile.SetStatusbarContentString(AValue: string);
var
  Val: String;
begin
  if FStatusbarContentString = AValue then Exit;
  Val := FStatusbarContentString;
  FStatusbarContentString := AValue;
  DoChange(eegDataFiles, Word(edceStatusbarContentString), @Val);
end;

procedure TEpiDataFile.DoControlChange(const Initiator: TEpiCustomBase;
  EventType: TEpiCustomChangeEventType; Data: TEpiCustomControlItem);
var
  F: TEpiField;
  H: TEpiHeading;
  S: TEpiSection;
  i: Integer;
begin
  // Skip event if initiated from AllControls.
  if (Initiator = FAllControlItems) or
     (Initiator = FAllFields) or
     (Initiator = FAllHeadings) or
     (Initiator = FKeyFields)
  then
    Exit;

  if (Initiator is TEpiFields) then
  begin
    F := TEpiField(Data);

    if EventType = ecceDelItem
    then
      begin
        // APPLIES TO FIELDS AND HEADING!
        // Make this test because:
        // 1) If F is being destroyed, then F will at this point have been
        //    removed from FAllFields due to the internal hook of TEpiCustomList.
        //    Hence we will get an error trying to remove it!
        // 2) If F is being removed/deleted, then F will at this point STILL be
        //    in the list of FAllFields. Hence we MUST remove it, otherwise our
        //    internal structures will be in an inconsistent state.
        if FAllFields.IndexOf(F) > -1 then
          FAllFields.RemoveItem(F)
      end
    else
      begin
        // Check that id is not in conflict with others.
        if not ValidateRename(F.Name) then
          F.Name := FAllFields.GetUniqueItemName(TEpiField);
        FAllFields.AddItem(F);
        F.Size := Size;
        F.FDataFile := Self;
      end;
  end;

  if (Initiator is TEpiHeadings) then
  begin
    if EventType = ecceDelItem
    then
      begin
        if FAllHeadings.IndexOf(Data) > -1 then
          FAllHeadings.RemoveItem(Data)
      end
    else
      begin
        // Check that id is not in conflict with others.
        if not ValidateRename(Data.Name) then
          Data.Name := FAllHeadings.GetUniqueItemName(TEpiHeading);
        TEpiHeading(Data).FDataFile := Self;
        FAllHeadings.AddItem(Data);
      end;
  end;

  if (Initiator is TEpiSections) then
  begin
    S := TEpiSection(Data);

    if EventType = ecceDelItem
    then
      begin
        if (ebsDestroying in S.State) then exit;

        for i := 0 to S.Fields.Count - 1 do
          S.Fields.DoChange(eegCustomBase, Word(ecceDelItem), S.Fields[i]);
        S.Fields.OnValidateRename := nil;

        for i := 0 to S.Headings.Count - 1 do
          S.Headings.DoChange(eegCustomBase, Word(ecceDelItem), S.Heading[i]);
        S.Headings.OnValidateRename := nil;
      end
    else
      begin
        // Check that id is not in conflict with others.
        // - but only check Fields/Headings since S is
        //   already inserted into Sections.
        if Assigned(Fields.FieldByName[S.Name]) or
           Assigned(Headings.GetHeadingByName(S.Name))
        then
          S.Name := Sections.GetUniqueItemName(TEpiSection);

        S.FDataFile := Self;
        S.Fields.OnValidateRename := @Self.ValidateRename;
        S.Fields.Sorted := true;
        for i := 0 to S.Fields.Count - 1 do
          S.Fields.DoChange(eegCustomBase, Word(ecceAddItem), S.Fields[i]);

        S.Headings.OnValidateRename := @Self.ValidateRename;
        S.Headings.Sorted := true;
        for i := 0 to S.Headings.Count - 1 do
          S.Headings.DoChange(eegCustomBase, Word(ecceAddItem), S.Heading[i]);
      end;
  end;

  if EventType = ecceDelItem
  then
    begin
      if FAllControlItems.IndexOf(Data) > -1 then
        FAllControlItems.RemoveItem(Data)
    end
  else
    FAllControlItems.AddItem(Data);
end;

procedure TEpiDataFile.DoModifiedChange(const Initiator: TEpiCustomBase;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  if (EventGroup = eegFields) and
     (TEpiFieldsChangeEventType(EventType) in [efceData, efceResetData])
  then
    FRecModified := true
  else
    if not
         ((EventGroup = eegCustomBase) and
          (TEpiCustomChangeEventType(EventType) = ecceUpdate))
    then
      FStructureModified := true;
end;

procedure TEpiDataFile.DoChange(const Initiator: TEpiCustomBase;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  inherited DoChange(Initiator, EventGroup, EventType, Data);

  // This check for housekeeping!
  if (EventGroup = eegCustomBase) and
     (TEpiCustomChangeEventType(EventType) in [ecceAddItem, ecceDelItem]) and
     (TObject(Data).InheritsFrom(TEpiCustomControlItem))
  then
    DoControlChange(Initiator,
                    TEpiCustomChangeEventType(EventType),
                    TEpiCustomControlItem(Data));

  DoModifiedChange(Initiator, EventGroup, EventType, Data);
end;

constructor TEpiDataFile.Create(AOwner: TEpiCustomBase);
begin
  Create(AOwner, 0);
end;

procedure TEpiDataFile.SetModified(const AValue: Boolean);
begin
  inherited SetModified(AValue);

  if (not AValue) then
  begin
    FStructureModified := AValue;
    FRecModified       := AValue;
  end;
end;

procedure TEpiDataFile.SaveKeyFieldsToDom(RootNode: TDOMElement);
var
  OD: TDOMDocument;
  KFsElem: TDOMElement;
  Elem: TDOMElement;
  i: Integer;
begin
  if KeyFields.Count = 0 then exit;

  OD := RootNode.OwnerDocument;
  KFsElem := OD.CreateElement('KeyFields');

  for i := 0 to KeyFields.Count -1 do
  begin
    Elem := OD.CreateElement('Key');
    SaveDomAttr(Elem, 'fieldRef', KeyFields[i].Name);
    KFsElem.AppendChild(Elem);
  end;

  RootNode.AppendChild(KFsElem);
end;

procedure TEpiDataFile.SaveRecordToDom(RootNode: TDOMElement);
var
  RecordsElem: TDOMElement;
  Edoc: TEpiDocument;
  ProgressMethod: TEpiProgressEvent;
  Sz: Integer;
  Fc: Integer;
  EnCrypter: TDCP_rijndael;
  CurRec: Integer;
  DocNode: TDOMDocument;
  RElem: TDOMElement;
  S: String;
  CurField: Integer;
  SaltInt: LongInt;
  SaltByte: array[0..3] of char absolute SaltInt;
  Salt: String;

  function LocalEncrypt(Const S: String): string;
  begin
    if Assigned(EnCrypter) then
      Result := EnCrypter.EncryptString(S)
    else
      Result := S;
  end;

  function DoProgress(ProgressType: TEpiProgressType;
    Count, Max: Cardinal):boolean; inline;
  begin
    Result := false;
    if Assigned(ProgressMethod) then
      ProgressMethod(Self, ProgressType, Count, Max, Result);
  end;

begin
  if FRecModified then
    FRecModifiedDate := Now;


  DocNode := RootNode.OwnerDocument;
  RecordsElem := DocNode.CreateElement(rsRecords);
  SaveDomAttr(RecordsElem, rsModifiedAttr, RecModifiedDate);

  RootNode.AppendChild(RecordsElem);

  Edoc := TEpiDocument(RootOwner);
  ProgressMethod := Edoc.OnProgress;

  BackupFormatSettings(Edoc.XMLSettings.FormatSettings);
  Sz := Size;
  Fc := Fields.Count;

  EnCrypter := nil;
  SaltInt := (Random(maxLongint - 1) + 1) or $80000000;  // Must have highest bit set.
  Salt := String(SaltByte);

  if (Edoc.Version >= 2) and (Edoc.PassWord <> '') then
  begin
    EnCrypter := TDCP_rijndael.Create(nil);
    EnCrypter.InitStr(Edoc.PassWord, TDCP_sha256);
    Salt := EnCrypter.EncryptString(Salt);
  end else
    Salt := '';

  SaveDomAttr(RecordsElem, rsEncryptedSalt, Salt);

  DoProgress(eptInit, 0, Sz);
  for CurRec := 0 to Sz - 1 do
  begin
    DoProgress(eptRecords, CurRec, Sz);
    RElem := DocNode.CreateElement(rsRecord);
    RecordsElem.AppendChild(RElem);
    SaveDomAttrEnum(RElem, rsRecordStatus, FRecordStatus.AsInteger[CurRec], TypeInfo(TEpiRecordState));

    S := '';
    for CurField := 0 to Fc - 1 do
    with Fields[CurField] do
      if not IsMissing[CurRec] then
        if FieldType in StringFieldTypes then
          S += Name + '="' + StringReplace(AsString[CurRec], '"', '""', [rfReplaceAll]) + '";'
        else
          S += Name + '=' + AsString[CurRec] + ';';
    Delete(S, Length(S), 1);
    RElem.TextContent := LocalEncrypt(S);
  end;
  RestoreFormatSettings;
  DoProgress(eptFinish, Sz, Sz);

  EnCrypter.Free;
end;

function TEpiDataFile.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);

  if FStructureModified then
    FStructureModifiedDate := Now;

  SaveDomAttr(Result, rsCreatedAttr, Created);
  SaveDomAttr(Result, rsModifiedAttr, StructureModifiedDate);
  SaveDomAttr(Result, rsVersionAttr, Version);
  if EpiSaveMode = esmField then
    SaveDomAttr(Result, 'recordCount', Size);
  SaveDomAttrEnum(Result, rsAfterRecordState, AfterRecordState, TypeInfo(TEpiDataFileAfterRecordState));

  SaveKeyFieldsToDom(Result);

  // Version 4:
  if (StatusbarContentString <> '') then
    SaveTextContent(Result, rsStatusbarContentString, FStatusbarContentString);

  if (EpiSaveMode = esmDatafile) then
    SaveRecordToDom(Result);
end;

function ControlItemListSort(Item1, Item2: Pointer): integer;
var
  C1: TEpiCustomControlItem;
  C2: TEpiCustomControlItem;
  S1: TEpiSection;
  S2: TEpiSection;
  M: TEpiSection;

  function Compare(CI1, CI2: TEpiCustomControlItem): integer;
  begin
    result := CI1.Top - CI2.Top;
    if result <> 0 then exit;

    result := CI1.Left - CI2.Left;
    if result <> 0 then exit;

    // if both top and left are the same and not same pointer.
    // The two ControlItems must be placed on top of each other.
    // Just do a pointer diff...
    result := Pointer(CI1) - Pointer(CI2);
  end;

begin
  if Item1 = Item2 then exit(0);  // Same pointer = same object!

  C1 := TEpiCustomControlItem(Item1);
  C2 := TEpiCustomControlItem(Item2);
  if (C1 is TEpiSection) then
    S1 := TEpiSection(C1)
  else
    S1 := TEpiSection(C1.Owner.Owner);
  if (C2 is TEpiSection) then
    S2 := TEpiSection(C2)
  else
    S2 := TEpiSection(C2.Owner.Owner);
  M  := S1.DataFile.MainSection;

  // Same section! Regular compare
  if S1 = S2 then
  begin
    // Could be a compare between a Control IN a section
    if (C1 = S1) then
      Exit(-1);
    if (C2 = S2) then
      Exit(1);

    // Else
    // C1 := C1
    // C2 := C2
  end else

  // One section is main
  if (S1 = M) or (S2 = M) then
  begin
    if S1 = M then C2 := S2;
    if S2 = M then C1 := S1;
  end else

  // Two different sections - none is main.
  begin
    C1 := S1;
    C2 := S2;
  end;

  Result := Compare(C1, C2);
end;

constructor TEpiDataFile.Create(AOwner: TEpiCustomBase; const ASize: integer);
begin
  inherited Create(AOwner);

  // Caption
  FCaption := TEpiTranslatedTextWrapper.Create(Self, rsCaption, rsText);
  FNotes := TEpiTranslatedTextWrapper.Create(Self, rsNotes, rsText);

  FGroupRights := TEpiGroupRights.Create(Self);
  FGroupRights.ItemOwner := true;

  FVersion := '';
  FCreated := Now;
  FRecModifiedDate := Now;
  FStructureModifiedDate := Now;

  // Sections
  FSections := TEpiSections.Create(Self);
  FSections.ItemOwner := true;
  FSections.OnValidateRename := @ValidateRename;
  FSections.Sorted := true;

  // The "all" containers.
  FAllFields := TEpiFields.Create(Self);
  FAllFields.OnSort := @ControlItemListSort;
  FAllFields.Sorted := true;
  FAllFields.Name := '@ALLFIELDS';
  FAllHeadings := TEpiHeadings.Create(Self);
  FAllHeadings.OnSort := @ControlItemListSort;
  FAllHeadings.Sorted := true;
  FAllHeadings.Name := '@ALLHEADINGS';
  FAllControlItems := TAllItemsList.Create(Self);
  FAllControlItems.OnSort := @ControlItemListSort;
  FAllControlItems.Sorted := true;
  FAllControlItems.Name := '@ALLCONTROLS';

  // The special main section...
  FMainSection := NewSection;
  FMainSection.Name := 'MAIN';

  // Special field to hold status of current record.
  FRecordStatus := TEpiField.CreateField(Self, ftInteger);
  FRecordStatus.FDataFile := Self;
  FRecordStatus.Size := ASize;

  // Container for Key Fields.
  FKeyFields := TEpiFields.Create(Self);
  FKeyFields.Name := '@KEYFIELDS';

  // Relates
  FRelates   := TEpiRelates.Create(Self);

  RegisterClasses([Caption, Notes, Sections, Relates, GroupRights]);
end;

destructor TEpiDataFile.Destroy;
begin
  FGroupRights.Free;
  FCaption.Free;
  FNotes.Free;
  FVersion := '';

  FSections.Free;

  FRecordStatus.Free;
  FAllFields.Free;
  FAllHeadings.Free;
  FAllControlItems.Free;
  FKeyFields.Free;

  inherited Destroy;
end;

function TEpiDataFile.ValidateRename(const NewName: string): boolean;
begin
  result :=
    (not Fields.ItemExistsByName(NewName))  and
    (not Headings.ItemExistsByName(NewName)) and
    (not Sections.ItemExistsByName(NewName));
end;

function TEpiDataFile.XMLName: string;
begin
  Result := rsDataFile;
end;

procedure TEpiDataFile.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
var
  Node: TDOMNode;
  Jump: TEpiJump;
  i: Integer;
  AField: TEpiField;
  T1: TDateTime;
  T2: TDateTime;
begin
  // Since the main section is autocreated we remove it during load.
  Sections.RemoveItem(MainSection);
  MainSection.Free;

  // Now start loading.
  inherited LoadFromXml(Root, ReferenceMap);

  // If no name present, TEpiTranslatedText will take care of it.
  Caption.LoadFromXml(Root, ReferenceMap);
  FVersion := LoadAttrString(Root, rsVersion, FVersion, false);
  FCreated := LoadAttrDateTime(Root, rsCreatedAttr);

  // Version 2:
  FStructureModifiedDate := LoadAttrDateTime(Root, rsModifiedAttr, '', FStructureModifiedDate, false);

  if (EpiSaveMode = esmField) then
    Size := (LoadAttrInt(Root, 'recordCount'));

  FLoadingRecs := true;
  if LoadNode(Node, Root, rsSections, false) then
    Sections.LoadFromXml(Node, ReferenceMap);
  FLoadingRecs := false;

  // VERSION 2 PROPERTY
  if LoadNode(Node, Root, 'KeyFields', false) then
    LoadKeyFields(Node);

  if Sections.ItemExistsByName('MAIN') then
    FMainSection := Sections.GetSectionByName('MAIN');

  // Version 3:
  if (TEpiDocument(RootOwner).Version >= 3) then
  begin
    FAfterRecordState := TEpiDataFileAfterRecordState(LoadAttrEnum(Root, rsAfterRecordState, TypeInfo(TEpiDataFileAfterRecordState)));

    if LoadNode(Node, Root, rsRelates, false) then
      FRelates.LoadFromXml(Node, ReferenceMap);
  end;

  // Version 4:
  if LoadNode(Node, Root, rsGroupRights, false) then
    GroupRights.LoadFromXml(Node, ReferenceMap);

  FStatusbarContentString := LoadNodeString(Root, rsStatusbarContentString, FStatusbarContentString, false);

  {$IFNDEF RELEASE}
  T1 := Now;
  {$ENDIF}
  if (EpiSaveMode = esmDatafile) and
     LoadNode(Node, Root, rsRecords, false)
  then
    LoadRecords(Node);
  {$IFNDEF RELEASE}
  T2 := Now;
  if IsConsole then
    Writeln('Load Records: ', FormatDateTime('NN:SS:ZZZ', T2-T1));
  {$ENDIF}
end;

function TEpiDataFile.NewField(FieldType: TEpiFieldType): TEpiField;
begin
  result := MainSection.NewField(FieldType);
end;

function TEpiDataFile.NewSection: TEpiSection;
begin
  result := Sections.NewSection;
end;

function TEpiDataFile.NewHeading: TEpiHeading;
begin
  result := MainSection.NewHeading;
end;

function TEpiDataFile.NewRecords(const Count: Cardinal): integer;
const
  Field_Growth_Factor = 1.25;
var
  CurrentSize, CurrentCapacity: LongInt;
  NewSize, NewCapacity: Int64;
  i: Integer;
begin
  // Assuming most entries will contain at least 10 records, we start out
  // increasing capacity in chunks of 10.

  CurrentSize     := Size;
  CurrentCapacity := FRecordStatus.Capacity;
  NewSize         := CurrentSize + Count;
  NewCapacity     := CurrentCapacity;

  if (NewSize > CurrentCapacity) then
  begin
    if (NewSize < 50) then
      NewCapacity := ((NewSize div 10) * 10) + 10
    else
      NewCapacity := Trunc((NewSize-1) * Field_Growth_Factor);
  end;

  FRecordStatus.Capacity := NewCapacity;
  FRecordStatus.Size     := NewSize;
  for i := CurrentSize to NewSize - 1 do
    Verified[i] := false;

  for i := 0 to Fields.Count -1 do
  begin
    Field[i].Capacity    := NewCapacity;
    Field[i].Size        := NewSize;
  end;

  Result := CurrentSize;
end;

procedure TEpiDataFile.Pack;
var
  L: array of Integer;
  C: Integer;
  i, j: Integer;
  S: Integer;
  D: Integer;
begin
  C := 0;
  for i := 0 to Size - 1 do
    if Deleted[i] then
    begin
      Inc(C);
      SetLength(L, C);
      L[C-1] := I;
    end;
  if C = 0 then exit;

  Inc(C);
  SetLength(L, C);
  L[C-1] := Size;

  DoChange(eegDataFiles, word(edcePack), @L);
  for i := 1 to High(L) do
  begin
    S := L[i-1] + 1;          // Source Index
    D := L[i-1] - (i-1);      // Destination Index
    C := (L[i] - L[i-1]) - 1; // Count
    if C = 0 then continue;

    for j := 0 to Fields.Count - 1 do
      Field[j].MovePackData(S, D, C);
    FRecordStatus.MovePackData(S, D, C);
  end;
  Size := Size - (Length(L) - 1);
end;

procedure TEpiDataFile.ExchangeRecords(const Index1, Index2: Integer);
var
  i: Integer;
begin
  for i := 0 to Fields.Count - 1 do
    Fields[i].Exchange(Index1, Index2);
  FRecordStatus.Exchange(Index1, Index2);
end;

procedure TEpiDataFile.SortRecords(SortFields: TEpiFields);
begin
  if Size < 1 then exit;

  // Setting FLoadingRecs=true here, tell the fields NOT to send events during records exchange.;
  FLoadingRecs := true;
  InternalSortRecords(SortFields, 0, Size - 1);
  FLoadingRecs := false;
end;

procedure TEpiDataFile.SortRecords(SortField: TEpiField);
var
  SortFields: TEpiFields;
begin
  SortFields := TEpiFields.Create(nil);
  SortFields.AddItem(SortField);
  SortRecords(SortFields);
  SortFields.Free;
end;

procedure TEpiDataFile.BeginCommitRecord(NewRecord: boolean);
begin
  DoChange(eegDataFiles, Word(edceBeginCommit), Pointer(ifthen(NewRecord, 0, 1)));
end;

procedure TEpiDataFile.EndCommitRecord(RecordNo: Integer);
begin
  DoChange(eegDataFiles, Word(edceEndCommit), Pointer(RecordNo));
end;

procedure TEpiDataFile.LoadRecord(RecordNo: Integer);
begin
  DoChange(eegDataFiles, Word(edceLoadRecord), Pointer(RecordNo));
end;

procedure TEpiDataFile.BeginUpdate;
begin
  inherited BeginUpdate;
  Fields.BeginUpdate;
  Headings.BeginUpdate;
  ControlItems.BeginUpdate;
end;

procedure TEpiDataFile.EndUpdate;
begin
  ControlItems.EndUpdate;
  Headings.EndUpdate;
  Fields.EndUpdate;
  inherited EndUpdate;
end;

function TEpiDataFile.DoCloneCreate(AOwner: TEpiCustomBase): TEpiCustomBase;
begin
  Result := inherited DoCloneCreate(AOwner);
  with TEpiDataFile(Result) do
  begin
    Sections.RemoveItem(MainSection);
    MainSection.Free;
  end;
end;

function TEpiDataFile.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
  ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
var
  i: Integer;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);
  with TEpiDataFile(Result) do
  begin
    FVersion := Self.FVersion;
    FCreated := Self.FCreated;
    FRecModifiedDate := Self.FRecModifiedDate;
    FStructureModifiedDate := self.FStructureModifiedDate;
    Size := Self.Size;
    Fields.Sort;

    // Copy Record Status - start by deleting the one created by default.
    FRecordStatus.Free;
    FRecordStatus := TEpiField(Self.FRecordStatus.Clone(Result));
  end;

  // At this point all existing fields in this DataFile have been cloned:
  // hence we can do a lookup in result list for comparable names.

  // - Clone list of key-fields.
  for i := 0 to Self.KeyFields.Count - 1 do
    TEpiDataFile(Result).KeyFields.AddItem(TEpiDataFile(Result).Fields.FieldByName[KeyFields[i].Name]);
end;

{ TEpiDataFilesEnumeraton }

function TEpiDataFilesEnumerator.GetCurrent: TEpiDataFile;
begin
  Result := TEpiDataFile(inherited GetCurrent);
end;

