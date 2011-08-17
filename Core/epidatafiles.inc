{%MainUnit epidatafiles.pas}

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

constructor TEpiDataFiles.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
end;

destructor TEpiDataFiles.Destroy;
begin
  inherited Destroy;
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

  Node := NewRoot.FirstChild;
  while Assigned(Node) do
  begin
    CheckNode(Node, rsDataFile);

    NDf := NewDataFile;
    NDf.LoadFromXml(Node);

    Node := Node.NextSibling;
  end;

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

procedure TEpiDataFile.LocalEventHook(Sender: TObject;
  EventGrp: TEpiEventGroup; EventType: Word; Data: pointer);
var
  F: TEpiField;
  H: TEpiHeading;
begin
  case EventGrp of
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
                if Fields.ItemExistsById(F.Id) then
                  F.Id := Fields.GetUniqueItemId(TEpiField);
                Fields.AddItem(F);
                F.Size := Size;
                F.FDataFile := Self;
              end;
              // Same goes for headings
              if (Sender is TEpiHeadings) then
              begin
                H := TEpiHeading(Data);
                // Check that id is not in conflict with others.
                if Headings.ItemExistsById(H.Id) then
                  H.Id := Headings.GetUniqueItemId(TEpiHeading);
                Headings.AddItem(TEpiHeading(Data));
              end;

              // If more sections are created, we need to add a hook to this also.
              if (Sender is TEpiSections) then
              with TEpiSection(Data) do
              begin
                FDataFile := Self;
                Fields.RegisterOnChangeHook(@LocalEventHook, true);
                Fields.OnValidateRename := @Self.ValidateRename;
                Headings.RegisterOnChangeHook(@LocalEventHook, true);
                Headings.OnValidateRename := @Self.ValidateRename;
              end;
            end;

          // On DelIetem "data" is a pointer to the now deleted item.
          ecceDelItem:
            begin
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
                Fields.UnRegisterOnChangeHook(@LocalEventHook);
                Fields.OnValidateRename := nil;
                Headings.UnRegisterOnChangeHook(@LocalEventHook);
                Headings.OnValidateRename := nil;
              end;
            end;
        end;
      end;
  end;
end;

function TEpiDataFile.SaveRecords(Lvl: integer): string;
var
  Sz: LongInt;
  Fc: LongInt;
  CurRec: Integer;
  CurField: Integer;
begin
  Result :=
    Indent(Lvl) + '<' + rsRecords + '>' + LineEnding;

  BackupFormatSettings(TEpiDocument(RootOwner).XMLSettings.FormatSettings);
  Sz := Size;
  Fc := Fields.Count;
  for CurRec := 0 to Sz - 1 do
  begin
    Result += Indent(Lvl + 1) + '<' + rsRecord;

    for CurField := 0 to Fc - 1 do
    with Fields[CurField] do
      if not IsMissing[CurRec] then
        if FieldType in StringFieldTypes then
          Result += ' ' + Id + '="' + StringToXml(AsString[CurRec]) + '"'
        else
          Result += ' ' + Id + '="' + AsString[CurRec] + '"';
    if Verified[CurRec] then
      Result += ' st="2"'
    else if Deleted[CurRec] then
      Result += ' st="1"'
    else
      Result += ' st="0"';
    Result += '/>' + LineEnding;
  end;
  RestoreFormatSettings;

  Result +=
    Indent(Lvl) + '</' + rsRecords + '>' + LineEnding;
end;

{$DEFINE EPIHASH}
procedure TEpiDataFile.LoadRecords(Root: TDOMNode);
var
  CurRec: Integer;
  Node: TDOMElement;
  i: Integer;
  TmpField: TEpiField;
  {$IFDEF EPIHASH}
  FieldList: Array of TEpiField;
  Fc, P, Val, C: LongInt;
  {$ENDIF EPIHASH}
  NN: WideString;
  Collision: Boolean;

  {$IFDEF EPIHASH}
  function NextPrime(const AValue: integer): cardinal;
  const
    Primes:  array[1..240] of cardinal = (
        2,      3,      5,      7,     11,     13,     17,     19,     23,     29,
       31,     37,     41,     43,     47,     53,     59,     61,     67,     71,
       73,     79,     83,     89,     97,    101,    103,    107,    109,    113,
      127,    131,    137,    139,    149,    151,    157,    163,    167,    173,
      179,    181,    191,    193,    197,    199,    211,    223,    227,    229,
      233,    239,    241,    251,    257,    263,    269,    271,    277,    281,
      283,    293,    307,    311,    313,    317,    331,    337,    347,    349,
      353,    359,    367,    373,    379,    383,    389,    397,    401,    409,
      419,    421,    431,    433,    439,    443,    449,    457,    461,    463,
      467,    479,    487,    491,    499,    503,    509,    521,    523,    541,
      547,    557,    563,    569,    571,    577,    587,    593,    599,    601,
      607,    613,    617,    619,    631,    641,    643,    647,    653,    659,
      661,    673,    677,    683,    691,    701,    709,    719,    727,    733,
      739,    743,    751,    757,    761,    769,    773,    787,    797,    809,
      811,    821,    823,    827,    829,    839,    853,    857,    859,    863,
      877,    881,    883,    887,    907,    911,    919,    929,    937,    941,
      947,    953,    967,    971,    977,    983,    991,    997,   1009,   1013,
     1019,   1021,   1031,   1033,   1039,   1049,   1051,   1061,   1063,   1069,
     1087,   1091,   1093,   1097,   1103,   1109,   1117,   1123,   1129,   1151,
     1153,   1163,   1171,   1181,   1187,   1193,   1201,   1213,   1217,   1223,
     1229,   1231,   1237,   1249,   1259,   1277,   1279,   1283,   1289,   1291,
     1297,   1301,   1303,   1307,   1319,   1321,   1327,   1361,   1367,   1373,
     1381,   1399,   1409,   1423,   1427,   1429,   1433,   1439,   1447,   1451,
     1453,   1459,   1471,   1481,   1483,   1487,   1489,   1493,   1499,   1511
     );
  var
    i: integer;
    IsPrime: Boolean;
  begin
    // Lookup into the primes table.
    if AValue <= Primes[High(primes)] then
      for i := Low(Primes) to High(primes) do
        if AValue < Primes[i] then exit(Primes[i]);

    // Too high a value, search for a prime.
    Result := AValue;
    if Result mod 2 = 0 then inc(Result);
    // This limits max number of variables to 1511**2 = 2.283.121
    // - it should be enough.
    while result < (1511**2) do
    begin
      IsPrime := true;
      for i := Low(Primes) to High(Primes) do
      begin
        if (Primes[i]**2) > Result then break;
        if (Result mod Primes[i]) = 0 then
        begin
          IsPrime := false;
          break;
        end;
      end;
      if IsPrime then Exit;
      Inc(Result, 2);
    end;
    result := -1;
  end;

  Function Hash(S : string) : LongWord;
  Var
    thehash,g,I : LongWord;
  begin
     thehash:=0;
     For I:=1 to Length(S) do
       begin
       thehash:=thehash shl 4;
       inc(theHash,Ord(S[i]));
       g:=thehash and LongWord($f shl 28);
       if g<>0 then
         begin
         thehash:=thehash xor (g shr 24);
         thehash:=thehash xor g;
         end;
       end;
     If theHash=0 then
       Hash:=$ffffffff
     else
       Hash:=TheHash;
  end;
  {$ENDIF EPIHASH}

begin
  // Root = <Records>

  CurRec := 0;
  Size := Root.ChildNodes.Count;

{$IFDEF EPIHASH}
  // This improves speed by 10-15%.
  C := 0;
  while true do
  begin
    Collision := false;
    Fc := Fields.Count;
    P := NextPrime((Fc + C) ** 2);
    SetLength(FieldList, P);
    for i := 0 to Fc -1 do
    begin
      Val := (Hash(Fields[i].Id) mod P);
      // Collision detection (with Fc^2 size, the collision probability is < 0.00001)
      if Assigned(FieldList[val]) then
      begin
        // Collision detection!!! Try new Prime number.
        Inc(C);
        Collision := true;
        SetLength(FieldList,0);
        Break;
      end;
      FieldList[Val] := Fields[i];
    end;
    if not Collision then Break;
  end;
{$ENDIF EPIHASH}

  Node := TDOMElement(Root.FirstChild);
  BackupFormatSettings(TEpiDocument(RootOwner).XMLSettings.FormatSettings);
  while Assigned(Node) do
  begin
    CheckNode(Node, rsRecord);

    for i := 0 to Node.Attributes.Length - 1 do
    with Node.Attributes[i] do
    begin
      NN := NodeName;
      if NN = 'st' then
      begin
        if NodeValue = '1' then
          Deleted[CurRec] := true
        else if NodeValue = '2' then
          Verified[CurRec] := true;
      end else begin
        {$IFDEF EPIHASH}
        TmpField := FieldList[Hash(NN) mod P];
        {$ELSE}
        TmpField := TEpiField(Fields.GetItemById(NN));
        {$ENDIF EPIHASH}
        TmpField.AsString[CurRec] := UTF8Encode(NodeValue);
      end;
    end;
    inc(CurRec);
    Node := TDOMElement(Node.NextSibling);
  end;
  RestoreFormatSettings;
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

procedure TEpiDataFile.SetVersion(const AValue: string);
begin
  if FVersion = AValue then exit;
  FVersion := AValue;
end;

class function TEpiDataFile.IdString: string;
begin
  Result := 'datafile_id_';
end;

constructor TEpiDataFile.Create(AOwner: TEpiCustomBase);
begin
  Create(AOwner, 0);
end;

constructor TEpiDataFile.Create(AOwner: TEpiCustomBase; const Size: integer);
begin
  inherited Create(AOwner);

  // Caption
  FCaption := TEpiTranslatedTextWrapper.Create(Self, rsCaption, rsText);
  FVersion := '';
  FCreated := Now;

  // Sections
  FSections := TEpiSections.Create(Self);
  FSections.ItemOwner := true;
  FSections.RegisterOnChangeHook(@LocalEventHook, true);
  FSections.OnValidateRename := @ValidateRename;

  // The "all" containers.
  FAllFields := TEpiFields.Create(Self);
  FAllHeadings := TEpiHeadings.Create(Self);

  // The special main section...
  FMainSection := NewSection;
  FMainSection.Name := 'MAIN';
  FMainSection.Id := FMainSection.IdString + 'main';

  FRecordStatus := TEpiField.CreateField(Self, ftInteger);
  FRecordStatus.Size := Size;

  RegisterClasses([Caption, Sections]);
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
    SaveNode(Lvl + 1, rsVersion, Version) +
    SaveNode(Lvl + 1, rsCreated, Created) +
    SaveRecords(Lvl + 1);
  Result := inherited SaveToXml(Content, Lvl);
end;

procedure TEpiDataFile.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMNode;
  Jump: TEpiJump;
  i: Integer;
  AField: TEpiField;
begin
  // Since the main section is autocreated we remove it during load.
  Sections.RemoveItem(MainSection);
  MainSection.Free;

  // Now start loading.
  inherited LoadFromXml(Root);

  // If no name present, TEpiTranslatedText will take care of it.
  Caption.LoadFromXml(Root);
  FVersion := LoadNodeString(Root, rsVersion);
  FCreated := LoadNodeDateTime(Root, rsCreated);

  if LoadNode(Node, Root, rsSections, false) then
    Sections.LoadFromXml(Node);

  // Post Processing!
  for i := 0 to LinkMap.Count - 1 do
  begin
    AField := TEpiField(Fields.GetItemById(LinkMap.RefObjId[i]));
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
    end;
  end;
  LinkMap.Clear;

  if Sections.ItemExistsByName('MAIN') then
    FMainSection := TEpiSection(Sections.GetItemByName('MAIN'));

  if LoadNode(Node, Root, rsRecords, false) then
    LoadRecords(Node);
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

procedure TEpiDataFile.NewRecords(const Count: Cardinal = 1);
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
