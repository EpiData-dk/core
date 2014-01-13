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

function TEpiDataFiles.ScrambleXml: boolean;
begin
  Result := Settings.Scrambled;
end;

function TEpiDataFiles.Prefix: string;
begin
  Result := 'datafile_id_';
end;

constructor TEpiDataFiles.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
end;

destructor TEpiDataFiles.Destroy;
begin
  inherited Destroy;
end;

function TEpiDataFiles.ItemClass: TEpiCustomItemClass;
begin
  result := TEpiDataFile;
end;

function TEpiDataFiles.SaveToXml(Content: String; Lvl: integer): string;
begin
  if Count = 0 then exit;
  Result := inherited SaveToXml(Content, Lvl);
end;

procedure TEpiDataFiles.LoadFromXml(Root: TDOMNode);
var
  NewRoot: TDOMNode;
  Node: TDOMNode;
  NDf: TEpiDataFile;
begin
  // Root = <DataFiles>
  if not Assigned(Root) then exit;

  // If file is scrambled, then we first need to descramble (using master password)
  // and then read xml structure.
  if Settings.Scrambled then
    NewRoot := DeCrypt(Root)
  else
    NewRoot := Root;

  // Inherited takes care of loading TEpiDataFile(s)
  inherited LoadFromXml(NewRoot);

  if Settings.Scrambled then
    NewRoot.Free;
end;

function TEpiDataFiles.NewDataFile: TEpiDataFile;
begin
  Result := TEpiDataFile(NewItem(TEpiDataFile));
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

{  case EventGrp of
    eegCustomBase:
      begin
        case TEpiCustomChangeEventType(EventType) of
          // On AddItem "data" is a pointer to the added item.
          ecceAddItem:
            begin
              // Here we make sure that fields added to any section is also added
              // to the global list of fields.
              if (Sender is TEpiFields) then
              begin
                F := TEpiField(Data);
                // Check that id is not in conflict with others.
                if not ValidateRename(F.Name) then
                  F.Name := Fields.GetUniqueItemName(TEpiField);
                Fields.AddItem(F);
                F.Size := Size;
                F.FDataFile := Self;
              end;
              // Same goes for headings
              if (Sender is TEpiHeadings) then
              begin
                H := TEpiHeading(Data);
                // Check that id is not in conflict with others.
                if not ValidateRename(H.Name) then
                  H.Name := Headings.GetUniqueItemName(TEpiHeading);
                Headings.AddItem(TEpiHeading(Data));
              end;

              // If more sections are created, we need to add a hook to this also.
              if (Sender is TEpiSections) then
              with TEpiSection(Data) do
              begin
                S := TEpiSection(Data);
                // Check that id is not in conflict with others.
                if not Self.ValidateRename(S.Name) then
                  S.Name := Sections.GetUniqueItemName(TEpiSection);

                FDataFile := Self;
                Fields.RegisterOnChangeHook(@LocalEventHook, true);
                Fields.OnValidateRename := @Self.ValidateRename;
                Fields.Sorted := true;
                for i := 0 to Fields.Count - 1 do
                  Fields.DoChange(eegCustomBase, Word(ecceAddItem), Fields[i]);

                Headings.RegisterOnChangeHook(@LocalEventHook, true);
                Headings.OnValidateRename := @Self.ValidateRename;
                Headings.Sorted := true;
                for i := 0 to Headings.Count - 1 do
                  Headings.DoChange(eegCustomBase, Word(ecceAddItem), Heading[i]);
              end;

              // Finally add the Item the list of ALL control items.
              ControlItems.AddItem(TEpiCustomControlItem(Data));
            end;

          // On DelIetem "data" is a pointer to the now deleted item.
          ecceDelItem:
            begin
              ControlItems.RemoveItem(TEpiCustomControlItem(Data));

              if (Sender is TEpiFields) then
                Fields.RemoveItem(TEpiField(Data));
              if (Sender is TEpiHeadings) then
                Headings.RemoveItem(TEpiHeading(Data));

              // If sections are removed, we need to delete the hook
              // in case the section lives on elsewhere.
              if (Sender is TEpiSections)  then
              with TEpiSection(Data) do
              begin
                if (ebsDestroying in State) then exit;

                for i := 0 to Fields.Count - 1 do
                  Fields.DoChange(eegCustomBase, Word(ecceDelItem), Fields[i]);
                Fields.UnRegisterOnChangeHook(@LocalEventHook);
                Fields.OnValidateRename := nil;

                for i := 0 to Headings.Count - 1 do
                  Headings.DoChange(eegCustomBase, Word(ecceDelItem), Heading[i]);
                Headings.UnRegisterOnChangeHook(@LocalEventHook);
                Headings.OnValidateRename := nil;
              end;
            end;
        end;
      end;
  end;     }
end;

function TEpiDataFile.SaveRecords(Lvl: integer): string;
var
  Sz: LongInt;
  Fc: LongInt;
  CurRec: Integer;
  CurField: Integer;
  S: String;
  Lst: TStringList;
  R: String;
  EnCrypter: TDCP_rijndael;
  Edoc: TEpiDocument;
  ProgressMethod: TEpiProgressEvent;

  function LocalEncrypt(S: String): string;
  begin
    if Assigned(EnCrypter) then
      S := EnCrypter.EncryptString(S);
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

  Lst := TStringList.Create;
  Lst.Add(Indent(Lvl) + '<' + rsRecords + SaveAttr(rsModifiedAttr, RecModifiedDate) + '>');

  Edoc := TEpiDocument(RootOwner);
  ProgressMethod := Edoc.OnProgress;

  BackupFormatSettings(Edoc.XMLSettings.FormatSettings);
  Sz := Size;
  Fc := Fields.Count;

  EnCrypter := nil;
  if (Edoc.Version >= 2) and (Edoc.PassWord <> '') then
  begin
    EnCrypter := TDCP_rijndael.Create(nil);
    EnCrypter.InitStr(Edoc.PassWord, TDCP_sha256);
  end;

  DoProgress(eptInit, 0, Sz);
  for CurRec := 0 to Sz - 1 do
  begin
    DoProgress(eptRecords, CurRec, Sz);
    R := Indent(Lvl + 1) + '<' + rsRecord + SaveAttrEnum(rsRecordStatus, FRecordStatus.AsInteger[CurRec], TypeInfo(TEpiRecordState)) + '>';

    S := '';
    for CurField := 0 to Fc - 1 do
    with Fields[CurField] do
      if not IsMissing[CurRec] then
        if FieldType in StringFieldTypes + FloatFieldTypes then
          S += Name + '=' + StringToXml(StringReplace(AsString[CurRec], ',', '\,', [rfReplaceAll])) + ','
        else
          S += Name + '=' + AsString[CurRec] + ',';
    Delete(S, Length(S), 1);

    Lst.Add(R + LocalEncrypt(S) + '</' + rsRecord + '>');
  end;
  RestoreFormatSettings;

  Lst.Add(Indent(Lvl) + '</' + rsRecords + '>');
  Result := Lst.Text;
  Lst.Free;

  DoProgress(eptFinish, Sz, Sz);
end;

procedure TEpiDataFile.LoadRecords(Root: TDOMNode);
var
  CurRec: Integer;
  Node: TDOMElement;
  i: Integer;
  TmpField: TEpiField;
  FieldList: TFPDataHashTable;
  NN: WideString;
  Collision: Boolean;
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
  PStr: PChar;
  L: Integer;
  PEnd: PChar;
  PCur: PChar;

  function Dummyfy(Const S: string): string;
  begin
    result := StringReplace(S, '\,', DummyString, [rfReplaceAll]);
  end;

  function UnDummyfy(Const S: string): string;
  begin
    result := StringReplace(S, DummyString, DecimalSeparator, [rfReplaceAll]);
  end;

  function LocalDeCrypt(S: string): string;
  begin
    if Assigned(DeCrypter) then
      S := DeCrypter.DecryptString(S);

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

  if (TEpiDocument(RootOwner).Version >= 2) and (TEpiDocument(RootOwner).PassWord <> '') then
  begin
    DeCrypter := TDCP_rijndael.Create(nil);
    DeCrypter.InitStr(TEpiDocument(RootOwner).PassWord, TDCP_sha256);
  end;

  ProgressMethod := TEpiDocument(RootOwner).OnProgress;
  // div 2: Because all nodes have a single whitespace node after each "normal" node.
  NodeCount := (Root.ChildNodes.Count div 2);
  DoProgress(eptInit, 0, NodeCount);


  Node := TDOMElement(Root.FirstChild);
  BackupFormatSettings(TEpiDocument(RootOwner).XMLSettings.FormatSettings);
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

    {$IFNDEF EPI_TEST_LOAD}
    DataList.DelimitedText := Dummyfy(LocalDeCrypt(UTF8Encode(Node.TextContent)));
    for i := 0 to DataList.Count - 1 do
    begin
      FN := DataList.Names[i];
      TmpField := TEpiField(THTDataNode(FieldList.Find(FN)).Data);

      // Do not crash if reading bad entry/line.
      if not Assigned(TmpField) then
        continue;
      TmpField.AsString[CurRec] := UnDummyfy(DataList.ValueFromIndex[i]);
    end;
    {$ELSE}
    FN := LocalDeCrypt(UTF8Encode(Node.TextContent))[1];
    PStr := @FN[1];
    PCur := PStr;
    L := Length(FN);

    while (PCur - PStr) < L do
    begin
      PEnd := PCur;
      while PEnd^ <> '=' do Inc(PEnd);
      PEnd^ := #0;
      FN := StrPas(PCur);

      PCur := PEnd;
      Inc(PCur);
      repeat
        if (PCur^ = #0) or
           (
            (PCur^ = ',') and
            ((PCur - 1)^ <> '\')
           )
        then
          break;

        Inc(PCur);
      until false;
    end;
    {$ENDIF EPI_TEST_LOAD}
    inc(CurRec);
    Node := TDOMElement(Node.NextSibling);
  end;
  DoProgress(eptFinish, NodeCount, NodeCount);

  if Assigned(DeCrypter) then FreeAndNil(DeCrypter);
  RestoreFormatSettings;
end;

function TEpiDataFile.SaveKeyFields(Lvl: integer): string;
var
  i: Integer;
begin
  Result := '';
  if KeyFields.Count = 0 then
    exit;

  Result := Indent(Lvl) + '<KeyFields>' + LineEnding;

  for i := 0 to KeyFields.Count -1 do
  begin
    Result += Indent(Lvl + 1) +
      '<Key' + SaveAttr('fieldRef', KeyFields[i].Name) + '/>' + LineEnding;
  end;
  Result += Indent(Lvl) + '</KeyFields>' + LineEnding;
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

    FieldId := LoadAttrString(Node, 'fieldRef');
    LinkMap.AddLink(ltKeyField, KeyFields, FieldId);

    Node := Node.NextSibling;
  end;
end;

procedure TEpiDataFile.SetDeleted(Index: integer; const AValue: boolean);
begin
  if AValue then
    FRecordStatus.AsInteger[Index] := Ord(rsDeleted)
  else
    FRecordStatus.AsInteger[Index] := Ord(rsNormal);
end;

procedure TEpiDataFile.SetSize(const AValue: Integer);
var
  i: Integer;
  Val: LongInt;
begin
  if AValue = Size then exit;

  Val := FRecordStatus.Size;
  FRecordStatus.Size := AValue;

  for i := 0 to Fields.Count - 1 do
    Fields[i].Size := AValue;

  DoChange(eegDataFiles, Word(edceSize), @Val);
end;

procedure TEpiDataFile.SetVerified(Index: integer; const AValue: boolean);
begin
  if AValue then
    FRecordStatus.AsInteger[Index] := Ord(rsVerified)
  else
    FRecordStatus.AsInteger[Index] := Ord(rsNormal);
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
     (TEpiFieldsChangeEventType(EventType) = efceData)
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

constructor TEpiDataFile.Create(AOwner: TEpiCustomBase; const Size: integer);
begin
  inherited Create(AOwner);

  // MASTER HOOK!
//  Self.RegisterOnChangeHook(@LocalEventHook, true);

  // Caption
  FCaption := TEpiTranslatedTextWrapper.Create(Self, rsCaption, rsText);
  FNotes := TEpiTranslatedText.Create(Self, rsNotes);
  FVersion := '';
  FCreated := Now;
  FRecModifiedDate := Now;
  FStructureModifiedDate := Now;

  // Sections
  FSections := TEpiSections.Create(Self);
  FSections.ItemOwner := true;
//  FSections.RegisterOnChangeHook(@LocalEventHook, true);
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
  FRecordStatus.Size := Size;

  // Container for Key Fields.
  FKeyFields := TEpiFields.Create(Self);
  FKeyFields.Name := '@KEYFIELDS';

  RegisterClasses([Caption, Notes, Sections]);
end;

destructor TEpiDataFile.Destroy;
begin
  FRecordStatus.Free;
  FSections.Free;
  FAllFields.Free;
  FAllHeadings.Free;
  FCaption.Free;
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

function TEpiDataFile.SaveToXml(Content: String; Lvl: integer): string;
begin
  Content +=
    SaveRecords(Lvl + 1) +
    SaveKeyFields(Lvl + 1);
  Result := inherited SaveToXml(Content, Lvl);
end;

function TEpiDataFile.SaveAttributesToXml: string;
begin
  if FStructureModified then
    FStructureModifiedDate := Now;

  Result := inherited SaveAttributesToXml +
    SaveAttr(rsCreatedAttr, Created) +
    SaveAttr(rsModifiedAttr, StructureModifiedDate);
    SaveAttr(rsVersionAttr, Version);
end;

procedure TEpiDataFile.LoadFromXml(Root: TDOMNode);
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
  inherited LoadFromXml(Root);

  // If no name present, TEpiTranslatedText will take care of it.
  Caption.LoadFromXml(Root);
  FVersion := LoadAttrString(Root, rsVersion, FVersion, false);
  FCreated := LoadAttrDateTime(Root, rsCreatedAttr);

  // Version 2:
  FStructureModifiedDate := LoadAttrDateTime(Root, rsModifiedAttr, '', FStructureModifiedDate, false);

  // VERSION 2 PROPERTY
  if LoadNode(Node, Root, 'KeyFields', false) then
    LoadKeyFields(Node);

  if LoadNode(Node, Root, rsSections, false) then
    Sections.LoadFromXml(Node);

  // Post Processing!
  for i := 0 to LinkMap.Count - 1 do
  begin
    AField := Fields.FieldByName[LinkMap.RefObjId[i]];
    Case LinkMap.LinkTypes[i] of
      ltJump:
        TEpiJump(LinkMap.Objects[i]).JumpToField := AField;
      ltValueLabelWrite:
        TEpiField(LinkMap.Objects[i]).ValueLabelWriteField := AField;
      ltCalcResField:
        TEpiCalculation(LinkMap.Objects[i]).ResultField := AField;
      ltCalcStartDate:
        TEpiTimeCalc(LinkMap.Objects[i]).StartDate := TEpiDateField(AField);
      ltCalcEndDate:
        TEpiTimeCalc(LinkMap.Objects[i]).EndDate := TEpiDateField(AField);
      ltCalcStartTime:
        TEpiTimeCalc(LinkMap.Objects[i]).StartTime := TEpiDateTimeField(AField);
      ltCalcEndTime:
        TEpiTimeCalc(LinkMap.Objects[i]).EndTime := TEpiDateTimeField(AField);
      ltCalcDayField:
        TEpiCombineDateCalc(LinkMap.Objects[i]).Day := TEpiIntField(AField);
      ltCalcMonthField:
        TEpiCombineDateCalc(LinkMap.Objects[i]).Month := TEpiIntField(AField);
      ltCalcYearField:
        TEpiCombineDateCalc(LinkMap.Objects[i]).Year := TEpiIntField(AField);
      ltCalcField1:
        TEpiCombineStringCalc(LinkMap.Objects[i]).Field1 := AField;
      ltCalcField2:
        TEpiCombineStringCalc(LinkMap.Objects[i]).Field2 := AField;
      ltCalcField3:
        TEpiCombineStringCalc(LinkMap.Objects[i]).Field3 := AField;
      ltCompare:
        TEpiComparison(LinkMap.Objects[i]).CompareField := AField;
      ltKeyField:
        TEpiFields(LinkMap.Objects[i]).AddItem(AField);
    end;
  end;
  LinkMap.Clear;

  if Sections.ItemExistsByName('MAIN') then
    FMainSection := Sections.GetSectionByName('MAIN');

  T1 := Now;
  if LoadNode(Node, Root, rsRecords, false) then
    LoadRecords(Node);
  T2 := Now;
  if IsConsole then
    Writeln('Load Records: ', FormatDateTime('NN:SS:ZZZ', T2-T1));
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

procedure TEpiDataFile.NewRecords(const Count: Cardinal);
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

  InternalSortRecords(SortFields, 0, Size - 1);
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

procedure TEpiDataFile.BeginUpdate;
begin
  inherited BeginUpdate;
  Fields.BeginUpdate;
  Headings.BeginUpdate;
end;

procedure TEpiDataFile.EndUpdate;
begin
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

function TEpiDataFile.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase
  ): TEpiCustomBase;
var
  i: Integer;
begin
  Result := inherited DoClone(AOwner, Dest);
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
    FRecordStatus := TEpiField(Self.FRecordStatus.Clone(Self));
  end;

  // At this point all existing fields in this DataFile have been cloned:
  // hence we can do a lookup in result list for comparable names.

  // - Clone list of key-fields.
  for i := 0 to Self.KeyFields.Count - 1 do
    TEpiDataFile(Result).KeyFields.AddItem(TEpiDataFile(Result).Fields.FieldByName[KeyFields[i].Name]);

  // - Update other references in each field
  for i := 0 to Fields.Count - 1 do
    Fields[i].PostCloneUpdate(TEpiDataFile(Result).Fields.FieldByName[Fields[i].Name]);
end;
