
{%MainUnit epidatafiles.pas}

const
  NA_INT       = High(EpiInteger); //  $7FFFFFFFFFFFFFFF;
  NA_FLOAT     = MaxFloat;
  NA_DATE      = High(EpiDate);    // NA_INT;
  NA_TIME      = $FFFFFFFF;        //MaxDouble;
  NA_STRING    = '.';
  NA_BOOL      = $7F;

  JMP_ANY      = '*';

{ TEpiFields }

function TEpiFields.GetDataFile: TEpiDataFile;
var
  AOwner: TEpiCustomBase;
begin
  AOwner := Owner;
  while Assigned(AOwner) and (not (AOwner is TEpiDataFile)) do
    AOwner := AOwner.Owner;

  result := TEpiDataFile(AOwner);
end;

function TEpiFields.GetField(index: integer): TEpiField;
begin
  result := TEpiField(Items[index]);
end;

function TEpiFields.GetFieldByName(const AName: string): TEpiField;
begin
  result := TEpiField(GetItemByName(AName));
end;

function TEpiFields.Prefix: string;
begin
  Result := 'V';
end;

constructor TEpiFields.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
end;

destructor TEpiFields.Destroy;
begin
   inherited Destroy;
end;

function TEpiFields.XMLName: string;
begin
  Result := rsFields;
end;

procedure TEpiFields.LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap
  );
var
  Node: TDOMNode;
  NField: TEpiField;
  JumpNode: TDOMNode;
begin
  // We do NOT call inherited in this method,
  // since we have no way to call NewField during load of node!

  // Root = <Fields>
  Node := Root.FirstChild;
  while Assigned(Node) do
  begin
    // hack to skip whitespace nodes.
    while NodeIsWhiteSpace(Node) do
      Node := Node.NextSibling;
    if not Assigned(Node) then exit;

    CheckNode(Node, rsField);

    NField := NewField(TEpiFieldType(LoadAttrEnum(Node, rsType, TypeInfo(TEpiFieldType))));
    NField.LoadFromXml(Node, ReferenceMap);

    Node := Node.NextSibling;
  end;
end;

function TEpiFields.NewField(FieldType: TEpiFieldType): TEpiField;
var
  i: Integer;
begin
  result := TEpiField.CreateField(Self, FieldType);
  Result.Name := GetUniqueItemName(TEpiCustomItemClass(Result.ClassType));
  AddItem(Result);

  if (not TEpiDocument(RootOwner).Loading) and (FieldType = ftAutoInc) then
  begin
    for i := 0 to DataFile.Size - 1 do
      Result.AsInteger[i] := TEpiDocument(RootOwner).ProjectSettings.AutoIncStartValue + i;
  end;
end;

function TEpiFields.FieldExists(AField: TEpiField): boolean;
begin
  result := ItemExistsByName(AField.Name);
end;

function TEpiFields.GetEnumerator: TEpiFieldsEnumerator;
begin
  result := TEpiFieldsEnumerator.Create(Self);
end;

procedure TEpiFields.Assign(const AEpiCustomBase: TEpiCustomBase);
begin
  inherited Assign(AEpiCustomBase);
end;

{ TEpiFieldsEnumerator }

function TEpiFieldsEnumerator.GetCurrent: TEpiField;
begin
  result := TEpiField(inherited GetCurrent);
end;

{ TEpiField }

procedure TEpiField.SetDecimals(const AValue: Cardinal);
var
  Val: LongWord;
begin
  if Decimals = AValue then exit;
  Val := Decimals;
  FDecimals := AValue;
  DoChange(eegDataFiles, Word(efceSetDecimal), @Val);
end;

procedure TEpiField.SetDefaultValueAsString(const AValue: string);
begin
  if AValue = '' then
    HasDefaultValue := false
  else
    DoSetDefaultValueAsString(AValue);
end;

procedure TEpiField.SetConfirmEntry(const AValue: Boolean);
var
  Val: Boolean;
begin
  if FConfirmEntry = AValue then exit;
  Val := FConfirmEntry;
  FConfirmEntry := AValue;
  DoChange(eegFields, word(efceConfirmEntry), @Val);
end;

function TEpiField.GetDefaultValueAsString: string;
begin
  if HasDefaultValue then
    result := DoGetDefaultValueAsString
  else
    result := '';
end;

procedure TEpiField.SetEntryMode(const AValue: TEpiEntryMode);
var
  Val: TEpiEntryMode;
begin
  if FEntryMode = AValue then exit;
  Val := FEntryMode;
  FEntryMode := AValue;
  DoChange(eegFields, Word(efceEntryMode), @Val);
end;

procedure TEpiField.SetForcePickList(const AValue: Boolean);
var
  Val: Boolean;
begin
  if FForcePickList = AValue then exit;
  Val := FForcePickList;
  FForcePickList := AValue;
  DoChange(eegFields, word(efceForcePickList), @Val);
end;

function TEpiField.GetSettings: TEpiXMLSettings;
begin
  Result := nil;
  if RootOwner is TEpiDocument then
    Result := TEpiDocument(RootOwner).XMLSettings;
end;

function TEpiField.LoadingRecs: boolean;
begin
  Result := false;

  if Assigned(DataFile)
  then
    Result := DataFile.FLoadingRecs;
end;

procedure TEpiField.SetDataModified;
begin
  if Assigned(DataFile)
  then
    DataFile.FRecModified := true;
end;

procedure TEpiField.SetLength(const AValue: Cardinal);
var
  Val: LongWord;
begin
  if Length = AValue then exit;
  Val := Length;
  FLength := AValue;
  DoChange(eegFields, Word(efceSetLength), @Val);
end;

procedure TEpiField.SetRepeatValue(const AValue: boolean);
var
  Val: Boolean;
begin
  if FRepeatValue = AValue then exit;
  Val := FRepeatValue;
  FRepeatValue := AValue;
  DoChange(eegFields, word(efceRepeatValue), @Val);
end;

procedure TEpiField.LocalEventHook(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
begin
  if not
    ((EventGroup = eegCustomBase) and (EventType = word(ecceDestroy)))
  then
    Exit;

  if (Initiator = FValueLabelSet)
  then
    ValueLabelSet := nil;


  if (Initiator = FValueLabelWriteField)
  then
    ValueLabelWriteField := nil;
end;

procedure TEpiField.DoChange(const Initiator: TEpiCustomBase;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
var
  S: String;
begin
  inherited DoChange(Initiator, EventGroup, EventType, Data);

  if not (
      (EventGroup = eegCustomBase) and
      (TEpiCustomChangeEventType(EventType) = ecceReferenceDestroyed)
     )
  then
    Exit;

  if Initiator = FCalculation then
  begin
    FCalculation.Free;
    FCalculation := nil;
  end;

  if Initiator = FComparison then
  begin
    FComparison.Free;
    FComparison := nil;
  end;
end;

procedure TEpiField.SetShowValueLabel(const AValue: Boolean);
var
  Val: Boolean;
begin
  if FShowValueLabel = AValue then exit;
  Val := FShowValueLabel;
  FShowValueLabel := AValue;
  DoChange(eegFields, word(efceShowValueLabel), @Val);
end;

procedure TEpiField.SetValueLabelSet(const AValue: TEpiValueLabelSet);
var
  Val: TEpiValueLabelSet;
begin
  if FValueLabelSet = AValue then exit;

  if Assigned(FValueLabelSet) then
    FValueLabelSet.UnRegisterOnChangeHook(@LocalEventHook);

  Val := FValueLabelSet;
  FValueLabelSet := AValue;

  if Assigned(FValueLabelSet) then
    FValueLabelSet.RegisterOnChangeHook(@LocalEventHook, true);

  DoChange(eegFields, Word(efceValueLabelSet), Val);
end;

procedure TEpiField.SetValueLabelWriteField(const AValue: TEpiField);
var
  Val: TEpiField;
begin
  if FValueLabelWriteField = AValue then exit;

  if Assigned(FValueLabelWriteField) then
  begin
    // UnRegister our hook on the previous field set.
    FValueLabelWriteField.UnRegisterOnChangeHook(@LocalEventHook);
  end;

  Val := FValueLabelWriteField;
  FValueLabelWriteField := AValue;

  if Assigned(FValueLabelWriteField) then
    FValueLabelWriteField.RegisterOnChangeHook(@LocalEventHook, true);

  DoChange(eegFields, Word(efceValueLabelWriteTo), Val);
end;

function TEpiField.GetSection: TEpiSection;
var
  AOwner: TEpiCustomBase;
begin
  AOwner := Owner;
  while Assigned(AOwner) and (not (AOwner is TEpiSection)) do
    AOwner := AOwner.Owner;
  result := TEpiSection(AOwner);
end;

function TEpiField.GetIsMissingValue(const index: Integer): boolean;
begin
  result := false;
  if Assigned(ValueLabelSet) then
    result := ValueLabelSet.IsMissingValue[AsValue[Index]];
end;

function TEpiField.GetIsMaxMissingValue(const index: Integer): boolean;
begin
  result := false;
  if Assigned(ValueLabelSet) then
    result := ValueLabelSet.IsMaxMissingValue[AsValue[Index]];
end;

function TEpiField.GetIs2MaxMissingValue(const index: Integer): boolean;
begin
  result := false;
  if Assigned(ValueLabelSet) then
    result := ValueLabelSet.Is2ndMaxMissingValue[AsValue[Index]];
end;

procedure TEpiField.CheckIndex(const index: integer);
begin
  if (Index < 0) or (Index > Size - 1) then
    Raise Exception.CreateFmt('Index out of bounds: %d', [Index]);
end;

function TEpiField.GetSize: Integer;
begin
  result := FSize;
end;

procedure TEpiField.SetSize(const AValue: Integer);
begin
  if AValue = Size then exit;
  if AValue > Capacity then
    Capacity := AValue;
  FSize := AValue;
end;

function TEpiField.Compare(i, j: integer): integer;
begin
  if IsMissing[i] and IsMissing[j] then exit(0);         // Both missing, sort even.
  if IsMissing[i] and (not IsMissing[j]) then exit(1);   // A missing, sort A last
  if (not IsMissing[i]) and IsMissing[j] then exit(-1);  // B missing, sort A first
  result := DoCompare(i,j);                              // Compare normal values.
end;

procedure TEpiField.Assign(const AEpiCustomBase: TEpiCustomBase);
var
  OrgField: TEpiField absolute AEpiCustomBase;
  VLSet: TEpiValueLabelSet;
  RefMap: TEpiReferenceMap;
begin
  inherited Assign(AEpiCustomBase);

  BeginUpdate;

  Length := OrgField.Length;
  Decimals := OrgField.Decimals;
  EntryMode := OrgField.EntryMode;
  ConfirmEntry := OrgField.ConfirmEntry;
  ShowValueLabel := OrgField.ShowValueLabel;
  RepeatValue := OrgField.RepeatValue;
  if OrgField.HasDefaultValue then
    DefaultValueAsString := OrgField.DefaultValueAsString;
  ShowValueLabel := OrgField.ShowValueLabel;
  ForcePickList := OrgField.ForcePickList;
  ValueLabelSet := OrgField.ValueLabelSet;

  // Clone sub-objects, as they themselves are ItemOwners.
  // therefor if any prior items exists, they must
  // be cleared.
  // In order to Complete the Assignment, references must
  // be fixed, hence a Map is needed.
  RefMap := TEpiReferenceMap.Create;

  FreeAndNil(FRanges);
  if Assigned(OrgField.Ranges) then
    Ranges := TEpiRanges(OrgField.Ranges.Clone(Self, RefMap));

  FreeAndNil(FJumps);
  if Assigned(OrgField.Jumps) then
    Jumps := TEpiJumps(OrgField.Jumps.Clone(Self, RefMap));

  FreeAndNil(FComparison);
  if Assigned(OrgField.Comparison) then
    Comparison := TEpiComparison(OrgField.Comparison.Clone(Self, RefMap));

  FreeAndNil(FRelates);
  if Assigned(FRelates) then
    Relates := TEpiValueRelates(OrgField.Relates.Clone(Self, RefMap));

  if Assigned(DataFile) and
     Assigned(OrgField.DataFile) and
     (DataFile = OrgField.DataFile)
  then
    RefMap.FixupReferences;

  RefMap.Free;

  EndUpdate;
end;

procedure TEpiField.FixupReferences(EpiClassType: TEpiCustomBaseClass;
  ReferenceType: Byte; const ReferenceId: string);
begin
  if (EpiClassType = TEpiField) then
  begin

    case ReferenceType of
      0: // ValueLabels
       ValueLabelSet := DataFile.ValueLabels.GetValueLabelSetByName(ReferenceId);

      1: // ValueLabelWriteField;
       ValueLabelWriteField := DataFile.Fields.FieldByName[ReferenceId] ;
    end;

  end else
    inherited FixupReferences(EpiClassType, ReferenceType, ReferenceId);
end;

function TEpiField.DoCloneCreate(AOwner: TEpiCustomBase): TEpiCustomBase;
begin
  Result := CreateField(AOwner, FieldType);
end;

function TEpiField.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
  ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
var
  F: TEpiField;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);
  F := TEpiField(Result);

  F.FConfirmEntry        := FConfirmEntry;
  F.FDecimals            := FDecimals;
  F.FEntryMode           := FEntryMode;
  F.FLength              := FLength;
  F.FShowValueLabel      := FShowValueLabel;
  F.FRepeatValue         := FRepeatValue;
  F.HasDefaultValue      := HasDefaultValue;
  F.FForcePickList       := FForcePickList;

  // Hack to make Reference map work correctly on fixup-references
  if not Assigned(AOwner) then
    F.FDataFile := FDataFile;

  if Assigned(FRanges) then
    F.FRanges := TEpiRanges(FRanges.Clone(F, ReferenceMap));

  if Assigned(FValueLabelSet) then
    ReferenceMap.AddFixupReference(F, TEpiField, 0, FValueLabelSet.Name);

  if Assigned(ValueLabelWriteField) then
    ReferenceMap.AddFixupReference(F, TEpiField, 1, ValueLabelWriteField.Name);

  if Assigned(Jumps) then
    F.Jumps := TEpiJumps(Jumps.Clone(F, ReferenceMap));

  if Assigned(Calculation) then
    F.Calculation := TEpiCalculation(Calculation.Clone(F, ReferenceMap));

  if Assigned(Comparison) then
    F.Comparison := TEpiComparison(Comparison.Clone(F, ReferenceMap));

  if Assigned(Relates) then
    F.Relates := TEpiValueRelates(Relates.Clone(F, ReferenceMap));
end;

{
procedure TEpiField.PostCloneUpdate(Dest: TEpiField);
begin
  // PostCloneUpdate is called from TEpiDataFile, hence we know a datafile exists and all fields exist.

  if Assigned(ValueLabelWriteField) then
    Dest.FValueLabelWriteField := Dest.DataFile.Fields.FieldByName[FValueLabelWriteField.Name];

  if Assigned(Jumps) then
  begin
    if Assigned(Dest.FJumps) then Dest.FJumps.Free;
    Dest.FJumps := TEpiJumps(Jumps.Clone(Dest));
  end;

  if Assigned(Comparison) then
  begin
    if Assigned(Dest.Comparison) then Dest.Comparison.Free;
    Dest.FComparison := TEpiComparison(Comparison.Clone(Dest));
  end;

  if Assigned(Calculation) then
  begin
    if Assigned(Dest.Calculation) then Dest.Calculation.Free;
    Dest.FCalculation := TEpiCalculation(Calculation.Clone(Dest));
  end;
end;  }

constructor TEpiField.Create(AOwner: TEpiCustomBase; AFieldType: TEpiFieldType);
begin
  inherited Create(AOwner);
  FFieldType := AFieldType;
  FQuestion := TEpiTranslatedTextWrapper.Create(Self, rsQuestion, rsText);
  FNotes    := TEpiTranslatedTextWrapper.Create(Self, rsNotes, rsText);

  FConfirmEntry := false;
  FDecimals     := 0;
  FEntryMode    := emDefault;
  FLength       := 2;
  FShowValueLabel := false;
  FRepeatValue  := false;
  FForcePickList := false;
  HasDefaultValue := false;

  RegisterClasses([Notes, Question]);
end;

procedure TEpiField.LoadData(RootNode: TDOMNode);
var
  Node: TDOMNode;
  S: DOMString;
  Idx: Integer;
  Symbol: Char;
  Data: String;
begin
  // RootNode = <Data>
  Symbol := LineEnding{$IFDEF WINDOWS}[1]{$ENDIF};
  Node := RootNode.FirstChild;

  S := Node.TextContent;

  while (S <> '') do
  begin
    Idx := StrToInt(Copy2SymbDel(S, ':'));
    Data := Copy2SymbDel(S, Symbol);

    AsString[Idx] := Data;
  end;
end;

function TEpiField.SaveData(RootDoc: TDOMDocument): TDOMElement;
var
  S: String;
  i: Integer;
begin
  Result := RootDoc.CreateElement('Data');

  S := '';
  for i := 0 to Size - 1 do
    if IsMissing[i] then
      Continue
    else
      S += IntToStr(i) + ':' + AsString[i] + #13#10;
  Result.TextContent := S;
end;

function TEpiField.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);

  // Attributes
  SaveDomAttrEnum(Result, rsType, FieldType, TypeInfo(TEpiFieldType));
  SaveDomAttr(Result, rsLength, Length);
  SaveDomAttr(Result, rsDecimals, Decimals);
  SaveDomAttrEnum(Result, rsEntryMode, EntryMode, TypeInfo(TEpiEntryMode));

  if ConfirmEntry then
    SaveDomAttr(Result, rsConfirmEntry, ConfirmEntry);
  if RepeatValue then
    SaveDomAttr(Result, rsRepeatValue, RepeatValue);
  if ShowValueLabel then
    SaveDomAttr(Result, rsShowValueLabel, ShowValueLabel);

  // VERSION 2 Property
  if ForcePickList then
    SaveDomAttr(Result, rsForcePickList, ForcePickList);
  if HasDefaultValue then
    SaveDomAttr(Result, rsDefaultValue, DefaultValueAsString);
  if Assigned(ValueLabelSet) then
    SaveDomAttr(Result, rsValueLabelId, ValueLabelSet.Name);
  if Assigned(ValueLabelWriteField) then
    SaveDomAttr(Result, rsValueLabelWriteRef, ValueLabelWriteField.Name);


  // Tags
  if Assigned(Calculation) then
    Result.AppendChild(Calculation.SaveToDom(RootDoc));

  if Assigned(Comparison) then
    Result.AppendChild(Comparison.SaveToDom(RootDoc));

  if Assigned(Jumps) then
    Result.AppendChild(Jumps.SaveToDom(RootDoc));

  if Assigned(Ranges) then
    Result.AppendChild(Ranges.SaveToDom(RootDoc));

  // Version 3
  if Assigned(Relates) and
     (Relates.Count > 0)
  then
    Result.AppendChild(Relates.SaveToDom(RootDoc));

  if (EpiSaveMode = esmField) then
    Result.AppendChild(SaveData(RootDoc));
end;

class function TEpiField.CreateField(AOwner: TEpiCustomBase; AFieldType: TEpiFieldType): TEpiField;
var
  FC: TEpiFieldClass;
begin
  FC := FieldClassFromFieldType(AFieldType);
  if Assigned(FC) then
    Result := FC.Create(AOwner, AFieldType)
  else
    Result := nil;
end;

destructor TEpiField.Destroy;
begin
  // nil in order to remove LocalEventHook from other objects.
  ValueLabelSet := nil;
  ValueLabelWriteField := nil;

  FQuestion.Free;
  FNotes.Free;

  if Assigned(FJumps) then
    FJumps.Free;
  if Assigned(FRanges) then
    FRanges.Free;
  if Assigned(FCalculation) then
    FCalculation.Free;
  if Assigned(FComparison) then
    FComparison.Free;
  if Assigned(FRelates) then
    FRelates.Free;
  inherited Destroy;
end;

function TEpiField.XMLName: string;
begin
  Result := rsField;
end;

procedure TEpiField.LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap);
var
  Node: TDOMNode;
  Attr: TDOMAttr;
  VLFieldId: String;
begin
  // Root = <Field>
  inherited LoadFromXml(Root, ReferenceMap);

  Length   := LoadAttrInt(Root, rsLength);
  Decimals := LoadAttrInt(Root, rsDecimals);
  EntryMode := TEpiEntryMode(LoadAttrEnum(Root, rsEntryMode, TypeInfo(TEpiEntryMode)));

  ConfirmEntry := LoadAttrBool(Root, rsConfirmEntry, ConfirmEntry, false);
  RepeatValue := LoadAttrBool(Root, rsRepeatValue, RepeatValue, false);
  DefaultValueAsString := LoadAttrString(Root, rsDefaultValue, DefaultValueAsString, false);
  ShowValueLabel := LoadAttrBool(Root, rsShowValueLabel, ShowValueLabel, false);
  // VERSION 2 Property
  ForcePickList := LoadAttrBool(Root, rsForcePickList, ForcePickList, false);

  if LoadAttr(Attr, Root, rsValueLabelId, false) then
    ReferenceMap.AddFixupReference(Self, TEpiField, 0, LoadAttrString(Root, rsValueLabelId));
//    ValueLabelSet := DataFile.ValueLabels.GetValueLabelSetByName(LoadAttrString(Root, rsValueLabelId));

  if LoadAttr(Attr, Root, rsValueLabelWriteRef, false) then
    ReferenceMap.AddFixupReference(Self, TEpiField, 1, LoadAttrString(Root, rsValueLabelWriteRef));

{  begin
    VLFieldId := LoadAttrString(Root, rsValueLabelWriteRef);
    ValueLabelWriteField := DataFile.Fields.FieldByName[VLFieldId];
    // Since (possibly) all ValueLabelWriteRef to fields are "forward" declared then collect them into
    // a batch and process the list after all fields are loaded.
    if not Assigned(ValueLabelWriteField) then
      LinkMap.AddLink(ltValueLabelWrite, Self, VLFieldId);
  end;}

  // If not present this is handled within TEpiTranslatedText;
  FQuestion.LoadFromXml(Root, ReferenceMap);
  FNotes.LoadFromXml(Root, ReferenceMap);

  if LoadNode(Node, Root, rsRanges, false) then
  begin
    Ranges := TEpiRanges.Create(Self);
    Ranges.ItemOwner := true;
    Ranges.LoadFromXml(Node, ReferenceMap);
  end;

  if LoadNode(Node, Root, rsJumps, false) then
  begin
    Jumps := TEpiJumps.Create(Self);
    Jumps.ItemOwner := true;
    Jumps.LoadFromXml(Node, ReferenceMap);
  end;

  if LoadNode(Node, Root, rsCalculation, false) then
  begin
    Case TEpiCalcType(LoadAttrEnum(Node, rsType, TypeInfo(TEpiCalcType))) of
      ctTimeDiff:      Calculation := TEpiTimeCalc.Create(Self);
      ctCombineDate:   Calculation := TEpiCombineDateCalc.Create(Self);
      ctCombineString: Calculation := TEpiCombineStringCalc.Create(Self);
    end;
    Calculation.LoadFromXml(Node, ReferenceMap);
  end;

  if LoadNode(Node, Root, rsCompare, false) then
  begin
    Comparison := TEpiComparison.Create(Self);
    Comparison.LoadFromXml(Node, ReferenceMap);
  end;

  if LoadNode(Node, Root, rsValueRelates, false) then
  begin
    Relates := TEpiValueRelates.Create(Self);
    Relates.ItemOwner := true;
    Relates.LoadFromXml(Node, ReferenceMap);
  end;

  if (EpiSaveMode = esmField) and
     (LoadNode(Node, Root, 'Data', false))
  then
    LoadData(Node);
end;

{ TEpiIntField }

procedure TEpiIntField.SetDefaultValue(const AValue: EpiInteger);
var
  Val: EpiInteger;
begin
  if FDefaultValue = AValue then exit;
  Val := FDefaultValue;
  FDefaultValue := AValue;
  DoChange(eegFields, Word(efceDefaultValue), @Val);
end;

procedure TEpiIntField.SetZeroFilled(AValue: Boolean);
var
  Val: Boolean;
begin
  if FZeroFilled = AValue then Exit;
  Val := FZeroFilled;
  FZeroFilled := AValue;
  DoChange(eegFields, Word(efceZeroFilled), @Val);
end;

function TEpiIntField.GetAsBoolean(const index: Integer): EpiBool;
begin
  if IsMissing[Index] then
    result := TEpiBoolField.DefaultMissing
  else if AsInteger[index] > 0 then
    result := 1
  else
    result := 0;
end;

function TEpiIntField.GetAsDate(const index: Integer): EpiDate;
begin
  if IsMissing[Index] then
    result := TEpiDateField.DefaultMissing
  else
    result := AsInteger[Index];
end;

function TEpiIntField.GetAsDateTime(const index: Integer): EpiDateTime;
begin
  if IsMissing[Index] then
    result := TEpiDateTimeField.DefaultMissing
  else
    result := AsInteger[Index];
end;

function TEpiIntField.GetAsFloat(const index: Integer): EpiFloat;
begin
  if IsMissing[Index] then
    result := TEpiFloatField.DefaultMissing
  else
    result := AsInteger[Index];
end;

function TEpiIntField.GetAsInteger(const index: Integer): EpiInteger;
begin
  CheckIndex(Index);
  result := FData[Index];
end;

function TEpiIntField.GetAsString(const index: Integer): EpiString;
begin
  if IsMissing[Index] then
    result := TEpiStringField.DefaultMissing
  else
    result := Format(FormatString(false), [AsInteger[Index]]);
end;

function TEpiIntField.GetAsTime(const index: Integer): EpiTime;
begin
  Result := AsDateTime[index];
end;

function TEpiIntField.GetAsValue(const index: Integer): EpiVariant;
begin
  Result := AsInteger[Index];
end;

function TEpiIntField.DoGetDefaultValueAsString: string;
begin
  Result := IntToStr(DefaultValue);
end;

procedure TEpiIntField.DoSetDefaultValueAsString(const AValue: string);
var
  I: int64;
begin
  if TryStrToInt64(AValue, I) then
    DefaultValue := I
  else
    HasDefaultValue := false;
end;

function TEpiIntField.GetCapacity: Integer;
begin
  result := System.Length(FData);
end;

function TEpiIntField.GetHasDefaultValue: boolean;
begin
  Result := FDefaultValue <> DefaultMissing;
end;

function TEpiIntField.GetIsMissing(const index: Integer): boolean;
begin
  Result := AsInteger[Index] = DefaultMissing;
end;

procedure TEpiIntField.SetAsBoolean(const index: Integer; const AValue: EpiBool
  );
begin
  if TEpiBoolField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsInteger[Index] := AValue;
end;

procedure TEpiIntField.SetAsDate(const index: Integer; const AValue: EpiDate);
begin
  if TEpiDateField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsInteger[Index] := AValue;
end;

procedure TEpiIntField.SetAsDateTime(const index: Integer;
  const AValue: EpiDateTime);
begin
  if TEpiDateTimeField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsInteger[Index] := Trunc(AValue);
end;

procedure TEpiIntField.SetAsFloat(const index: Integer; const AValue: EpiFloat
  );
begin
  if TEpiFloatField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
  // TODO : TEpiIntField - Rounding function.
    AsInteger[Index] := trunc(AValue);
end;

procedure TEpiIntField.SetAsInteger(const index: Integer;
  const AValue: EpiInteger);
var
  Val: PEpiFieldDataEventRecord;
begin
  CheckIndex(Index);
  if FData[Index] = AValue then exit;

  if not LoadingRecs then
  begin;
    Val := New(PEpiFieldDataEventRecord);
    Val^.Index     := index;
    Val^.FieldType := FieldType;
    Val^.IntValue  := FData[Index];
  end;

  FData[Index] := AValue;

  if not LoadingRecs then
  begin
    DoChange(eegFields, word(efceData), Val);
    Dispose(Val);
  end;
end;

procedure TEpiIntField.SetAsString(const index: Integer; const AValue: EpiString
  );
begin
  if TEpiStringField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsInteger[index] := StrToInt64Def(AValue, DefaultMissing);
end;

procedure TEpiIntField.SetAsTime(const index: Integer; const AValue: EpiTime);
begin
  if TEpiDateTimeField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsInteger[Index] := Trunc(AValue);
end;

procedure TEpiIntField.SetAsValue(const index: Integer; const AValue: EpiVariant
  );
begin
  AsInteger[index] := AValue;
end;

procedure TEpiIntField.SetCapacity(AValue: Integer);
var
  i: LongInt;
begin
  if AValue = Capacity then exit;
  System.SetLength(FData, AValue);
  for i := FCapacity to AValue-1 do
    FData[i] := DefaultMissing;
  FCapacity := AValue;
end;

procedure TEpiIntField.SetIsMissing(const index: Integer; const AValue: boolean
  );
begin
  AsInteger[index] := DefaultMissing;
end;

procedure TEpiIntField.SetHasDefaultValue(const AValue: boolean);
begin
  if not AValue then FDefaultValue := DefaultMissing;
end;

constructor TEpiIntField.Create(AOwner: TEpiCustomBase;
  AFieldType: TEpiFieldType);
begin
  inherited Create(AOwner, AFieldType);
  if AFieldType = ftAutoInc then
    FLength := 6;
end;

procedure TEpiIntField.MovePackData(const SrcIdx, DstIdx, Count: integer);
begin
  Move(FData[SrcIdx], FData[DstIdx], Count * SizeOf(EpiInteger));
end;

class function TEpiIntField.CheckMissing(AValue: EpiInteger): boolean;
begin
  result := AValue = DefaultMissing;
end;

class function TEpiIntField.DefaultMissing: EpiInteger;
begin
  result := NA_INT;
end;

procedure TEpiIntField.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
begin
  inherited LoadFromXml(Root, ReferenceMap);

  ZeroFilled := LoadAttrBool(Root, rsZeroFilled, ZeroFilled, false);
end;

function TEpiIntField.DoCompare(i, j: integer): integer;
begin
  result := AsInteger[i] - AsInteger[j];
end;

function TEpiIntField.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
  ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);

  with TEpiIntField(Result) do
  begin
    Capacity := Self.Capacity;
    Move(Self.FData[0], FData[0], Self.Size * SizeOf(EpiInteger));
    FSize := Self.FSize;
    FDefaultValue := Self.FDefaultValue;
    FZeroFilled := Self.ZeroFilled;
  end;
end;

procedure TEpiIntField.Assign(const AEpiCustomBase: TEpiCustomBase);
begin
  inherited Assign(AEpiCustomBase);

  if not AEpiCustomBase.InheritsFrom(TEpiIntField) then exit;

  with TEpiIntField(AEpiCustomBase) do
  begin
    Self.Capacity      := Capacity;
    Self.FSize         := FSize;
    Self.FDefaultValue := FDefaultValue;
    Self.ZeroFilled    := FZeroFilled;
    Move(FData[0], Self.FData[0], Size * SizeOf(EpiInteger));
  end;
end;

function TEpiIntField.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);

  if ZeroFilled then
    SaveDomAttr(Result, rsZeroFilled, ZeroFilled);
end;

procedure TEpiIntField.Exchange(i, j: integer);
var
  TmpInt: EpiInteger;
begin
  TmpInt := AsInteger[i];
  AsInteger[i] := AsInteger[j];
  AsInteger[j] := TmpInt;
end;

function TEpiIntField.FormatString(const FillSpace: boolean): string;
begin
  if ZeroFilled then
    result := format('%%.%dd', [Length])
  else
  if FillSpace then
    result := format('%%%dd', [Length])
  else
    result := '%d';
end;

procedure TEpiIntField.ResetData;
begin
  FillQWord(FData[0], FSize, DefaultMissing);
  DoChange(eegFields, Word(efceResetData), nil);
end;

{ TEpiFloatField }

procedure TEpiFloatField.SetDefaultValue(const AValue: EpiFloat);
var
  Val: EpiFloat;
begin
  if FDefaultValue = AValue then exit;
  Val := FDefaultValue;
  FDefaultValue := AValue;
  DoChange(eegFields, Word(efceDefaultValue), @Val);
end;

procedure TEpiFloatField.DoSetDefaultValueAsString(const AValue: string);
var
  F: Extended;
begin
  if TryStrToFloat(AValue, F) then
    DefaultValue := F
  else
    HasDefaultValue := false;
end;

function TEpiFloatField.GetAsBoolean(const index: Integer): EpiBool;
begin
  if IsMissing[Index] then
    result := TEpiBoolField.DefaultMissing
  else if AsFloat[index] > 0 then
    result := 1
  else
    result := 0;
end;

function TEpiFloatField.GetAsDate(const index: Integer): EpiDate;
begin
  if IsMissing[Index] then
    result := TEpiDateField.DefaultMissing
  else
    result := AsInteger[Index];
end;

function TEpiFloatField.GetAsDateTime(const index: Integer): EpiDateTime;
begin
  if IsMissing[Index] then
    result := TEpiDateTimeField.DefaultMissing
  else
    Result := AsFloat[index];
end;

function TEpiFloatField.GetAsFloat(const index: Integer): EpiFloat;
begin
  CheckIndex(Index);
  Result := FData[index];
end;

function TEpiFloatField.GetAsInteger(const index: Integer): EpiInteger;
begin
  if IsMissing[Index] then
    result := TEpiIntField.DefaultMissing
  else   // TODO : Rounding function!!!
    result := Trunc(AsFloat[index]);
end;

function TEpiFloatField.GetAsString(const index: Integer): EpiString;
begin
  if IsMissing[Index] then
    Exit(TEpiStringField.DefaultMissing);
  result := FloatToStr(AsFloat[Index]);
end;

function TEpiFloatField.GetAsTime(const index: Integer): EpiTime;
begin
  if IsMissing[Index] then
    result := TEpiDateTimeField.DefaultMissing
  else
    Result := AsFloat[index];
end;

function TEpiFloatField.GetAsValue(const index: Integer): EpiVariant;
begin
  result := AsFloat[index];
end;

function TEpiFloatField.GetCapacity: Integer;
begin
  result := System.Length(FData);
end;

function TEpiFloatField.DoGetDefaultValueAsString: string;
begin
  Result := FloatToStr(DefaultValue);
end;

function TEpiFloatField.GetIsMissing(const index: Integer): boolean;
begin
  result := AsFloat[index] = DefaultMissing;
end;

function TEpiFloatField.GetHasDefaultValue: boolean;
begin
  Result := FDefaultValue <> DefaultMissing;
end;

procedure TEpiFloatField.SetAsBoolean(const index: Integer;
  const AValue: EpiBool);
begin
  if TEpiBoolField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsFloat[Index] := AValue;
end;

procedure TEpiFloatField.SetAsDate(const index: Integer; const AValue: EpiDate
  );
begin
  if TEpiDateField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsFloat[Index] := AValue;
end;

procedure TEpiFloatField.SetAsDateTime(const index: Integer;
  const AValue: EpiDateTime);
begin
  if TEpiDateTimeField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsFloat[Index] := AValue;
end;

procedure TEpiFloatField.SetAsFloat(const index: Integer; const AValue: EpiFloat
  );
var
  Val: PEpiFieldDataEventRecord;
begin
  CheckIndex(Index);
  if FData[Index] = AValue then exit;

  if not LoadingRecs then
  begin;
    Val := New(PEpiFieldDataEventRecord);
    Val^.Index     := index;
    Val^.FieldType := FieldType;
    Val^.FloatValue  := FData[Index];
  end;

  FData[index] := AValue;

  if not LoadingRecs then
  begin
    DoChange(eegFields, word(efceData), Val);
    Dispose(Val);
  end;
end;

procedure TEpiFloatField.SetAsInteger(const index: Integer;
  const AValue: EpiInteger);
begin
  if TEpiIntField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsFloat[Index] := AValue;
end;

procedure TEpiFloatField.SetAsString(const index: Integer;
  const AValue: EpiString);
begin
  if TEpiStringField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsFloat[Index] := StrToFloatDef(AValue, DefaultMissing);
end;

procedure TEpiFloatField.SetAsTime(const index: Integer; const AValue: EpiTime
  );
begin
  if TEpiDateTimeField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsFloat[Index] := AValue;
end;

procedure TEpiFloatField.SetAsValue(const index: Integer;
  const AValue: EpiVariant);
begin
  AsFloat[index] := AValue;
end;

procedure TEpiFloatField.SetIsMissing(const index: Integer;
  const AValue: boolean);
begin
  AsFloat[index] := DefaultMissing;
end;

procedure TEpiFloatField.SetCapacity(AValue: Integer);
var
  i: LongInt;
begin
  if AValue = Capacity then exit;
  System.SetLength(FData, AValue);
  for i := FCapacity to AValue-1 do
    FData[i] := DefaultMissing;
  FCapacity := AValue;
end;

procedure TEpiFloatField.SetHasDefaultValue(const AValue: boolean);
begin
  if not AValue then FDefaultValue := DefaultMissing;
end;

constructor TEpiFloatField.Create(AOwner: TEpiCustomBase;
  AFieldType: TEpiFieldType);
begin
  inherited Create(AOwner, AFieldType);
  FDecimals := 2;
end;

procedure TEpiFloatField.MovePackData(const SrcIdx, DstIdx, Count: integer);
begin
  Move(FData[SrcIdx], FData[DstIdx], Count * SizeOf(EpiFloat));
end;

class function TEpiFloatField.CheckMissing(AValue: EpiFloat): boolean;
begin
  result := AValue = DefaultMissing;
end;

class function TEpiFloatField.DefaultMissing: EpiFloat;
begin
  result := NA_FLOAT;
end;

function TEpiFloatField.DoCompare(i, j: integer): integer;
begin
  Result := CompareValue(AsFloat[i], AsFloat[j], 0.0);  // Sign(AsFloat[i] - AsFloat[j]);
end;

function TEpiFloatField.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
  ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);

  with TEpiFloatField(Result) do
  begin
    Capacity := Self.Capacity;
    Move(Self.FData[0], FData[0], Self.Size * SizeOf(EpiFloat));
    FSize := Self.FSize;
    FDefaultValue := Self.FDefaultValue;
  end;
end;

procedure TEpiFloatField.Exchange(i, j: integer);
var
  TmpFlt: EpiFloat;
begin
  TmpFlt := AsFloat[i];
  AsFloat[i] := AsFloat[j];
  AsFloat[j] := TmpFlt;
end;

function TEpiFloatField.FormatString(const FillSpace: boolean): string;
begin
  if FillSpace then
    result := format('%%%d.%df', [Length - Decimals - 1, Decimals])
  else
    result := format('%%.%df', [Decimals]);
end;

procedure TEpiFloatField.Assign(const AEpiCustomBase: TEpiCustomBase);
begin
  inherited Assign(AEpiCustomBase);

  if not AEpiCustomBase.InheritsFrom(TEpiFloatField) then exit;

  with TEpiFloatField(AEpiCustomBase) do
  begin
    Self.Capacity      := Capacity;
    Self.FSize         := FSize;
    Self.FDefaultValue := FDefaultValue;
    Move(FData[0], Self.FData[0], Size * SizeOf(EpiFloat));
  end;
end;

procedure TEpiFloatField.ResetData;
var
  i: Integer;
begin
//  FillDWord(FData[0], FSize, DefaultMissing);  Wrong type Extended vs. Q/DWord
  for i := 0 to FSize - 1 do
    FData[i] := DefaultMissing;
  DoChange(eegFields, Word(efceResetData), nil);
end;

{ TEpiBoolField }

procedure TEpiBoolField.SetDefaultValue(const AValue: EpiBool);
var
  Val: EpiBool;
begin
  if FDefaultValue = AValue then exit;
  Val := FDefaultValue;
  FDefaultValue := AValue;
  DoChange(eegFields, Word(efceDefaultValue), @Val);
end;

procedure TEpiBoolField.DoSetDefaultValueAsString(const AValue: string);
var
  I: integer;
begin
  if TryStrToInt(AValue, I) then
    DefaultValue := I
  else
    HasDefaultValue := false;
end;

function TEpiBoolField.GetAsBoolean(const index: Integer): EpiBool;
begin
  CheckIndex(Index);
  result := FData[index];
end;

function TEpiBoolField.GetAsDate(const index: Integer): EpiDate;
begin
  if IsMissing[Index] then
    result := TEpiDateField.DefaultMissing
  else
    result := AsBoolean[Index];
end;

function TEpiBoolField.GetAsDateTime(const index: Integer): EpiDateTime;
begin
  if IsMissing[Index] then
    result := TEpiDateTimeField.DefaultMissing
  else
    result := AsBoolean[Index];
end;

function TEpiBoolField.GetAsFloat(const index: Integer): EpiFloat;
begin
  if IsMissing[Index] then
    result := TEpiFloatField.DefaultMissing
  else
    result := AsBoolean[Index];
end;

function TEpiBoolField.GetAsInteger(const index: Integer): EpiInteger;
begin
  if IsMissing[Index] then
    result := TEpiIntField.DefaultMissing
  else
    result := AsBoolean[Index];
end;

function TEpiBoolField.GetAsString(const index: Integer): EpiString;
begin
  if IsMissing[Index] then
    Exit(TEpiStringField.DefaultMissing)
  else     // TODO: translation of boolean characters.
    result := BoolToStr(AsBoolean[index] <> 0, 'Y', 'N')
end;

function TEpiBoolField.GetAsTime(const index: Integer): EpiTime;
begin
  if IsMissing[Index] then
    result := TEpiDateTimeField.DefaultMissing
  else
    result := AsBoolean[Index];
end;

function TEpiBoolField.GetAsValue(const index: Integer): EpiVariant;
begin
  result := AsBoolean[index];
end;

function TEpiBoolField.GetCapacity: Integer;
begin
  result := System.Length(FData);
end;

function TEpiBoolField.DoGetDefaultValueAsString: string;
begin
  Result := IntToStr(DefaultValue);
end;

function TEpiBoolField.GetIsMissing(const index: Integer): boolean;
begin
  result := AsBoolean[index] = DefaultMissing;
end;

function TEpiBoolField.GetHasDefaultValue: boolean;
begin
  Result := FDefaultValue <> DefaultMissing;
end;

procedure TEpiBoolField.SetAsBoolean(const index: Integer; const AValue: EpiBool
  );
var
  Val: PEpiFieldDataEventRecord;
begin
  CheckIndex(Index);
  if FData[Index] = AValue then exit;

  if not LoadingRecs then
  begin;
    Val := New(PEpiFieldDataEventRecord);
    Val^.Index     := index;
    Val^.FieldType := FieldType;
    Val^.BoolValue  := FData[Index];
  end;

  FData[index] := AValue;

  if not LoadingRecs then
  begin
    DoChange(eegFields, word(efceData), Val);
    Dispose(Val);
  end;
end;

procedure TEpiBoolField.SetAsDate(const index: Integer; const AValue: EpiDate);
begin
  if TEpiDateField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else if AValue = 0 then
    AsBoolean[Index] := 0
  else
    AsBoolean[Index] := 1;
end;

procedure TEpiBoolField.SetAsDateTime(const index: Integer;
  const AValue: EpiDateTime);
begin
  if TEpiDateTimeField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else if AValue = 0 then
    AsBoolean[Index] := 0
  else
    AsBoolean[Index] := 1;
end;

procedure TEpiBoolField.SetAsFloat(const index: Integer; const AValue: EpiFloat
  );
begin
  if TEpiFloatField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else if AValue = 0 then
    AsBoolean[Index] := 0
  else
    AsBoolean[Index] := 1;
end;

procedure TEpiBoolField.SetAsInteger(const index: Integer;
  const AValue: EpiInteger);
begin
  if TEpiIntField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else if AValue = 0 then
    AsBoolean[Index] := 0
  else
    AsBoolean[Index] := 1;
end;

procedure TEpiBoolField.SetAsString(const index: Integer;
  const AValue: EpiString);
begin
  if TEpiStringField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else if System.Length(AValue) > 0 then
  begin
    if AValue[1] in BooleanYesChars then
      AsBoolean[Index] := 1
    else
      AsBoolean[Index] := 0;
  end else
    AsBoolean[Index] := 0;
end;

procedure TEpiBoolField.SetAsTime(const index: Integer; const AValue: EpiTime);
begin
  if TEpiDateTimeField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else if AValue = 0 then
    AsBoolean[Index] := 0
  else
    AsBoolean[Index] := 1;
end;

procedure TEpiBoolField.SetAsValue(const index: Integer;
  const AValue: EpiVariant);
begin
  AsBoolean[index] := AValue;
end;

procedure TEpiBoolField.SetIsMissing(const index: Integer; const AValue: boolean
  );
begin
  AsBoolean[index] := DefaultMissing;
end;

procedure TEpiBoolField.SetCapacity(AValue: Integer);
var
  i: LongInt;
begin
  if AValue = Capacity then exit;
  System.SetLength(FData, AValue);
  for i := FCapacity to AValue-1 do
    FData[i] := DefaultMissing;
  FCapacity := AValue;
end;

procedure TEpiBoolField.SetHasDefaultValue(const AValue: boolean);
begin
  if not AValue then FDefaultValue := DefaultMissing;
end;

constructor TEpiBoolField.Create(AOwner: TEpiCustomBase;
  AFieldType: TEpiFieldType);
begin
  inherited Create(AOwner, AFieldType);
  FLength := 1;
end;

procedure TEpiBoolField.MovePackData(const SrcIdx, DstIdx, Count: integer);
begin
  Move(FData[SrcIdx], FData[DstIdx], Count * SizeOf(EpiBool));
end;

class function TEpiBoolField.CheckMissing(AValue: EpiBool): boolean;
begin
  result := AValue = DefaultMissing;
end;

class function TEpiBoolField.DefaultMissing: EpiBool;
begin
  result := NA_BOOL;
end;

function TEpiBoolField.DoCompare(i, j: integer): integer;
begin
  Result := Sign(AsBoolean[i]) - Sign(AsBoolean[j]);
end;

function TEpiBoolField.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
  ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);

  with TEpiBoolField(Result) do
  begin
    Capacity := Self.Capacity;
    Move(Self.FData[0], FData[0], Self.Size * SizeOf(EpiBool));
    FSize := Self.FSize;
    FDefaultValue := Self.FDefaultValue;
  end;
end;

procedure TEpiBoolField.Exchange(i, j: integer);
var
  TmpBool: EpiBool;
begin
  TmpBool := AsBoolean[i];
  AsBoolean[i] := AsBoolean[j];
  AsBoolean[j] := TmpBool;
end;

procedure TEpiBoolField.Assign(const AEpiCustomBase: TEpiCustomBase);
begin
  inherited Assign(AEpiCustomBase);

  if not AEpiCustomBase.InheritsFrom(TEpiBoolField) then exit;

  with TEpiBoolField(AEpiCustomBase) do
  begin
    Self.Capacity      := Capacity;
    Self.FSize         := FSize;
    Self.FDefaultValue := FDefaultValue;
    Move(FData[0], Self.FData[0], Size * SizeOf(EpiBool));
  end;
end;

function TEpiBoolField.FormatString(const FillSpace: boolean): string;
begin
  Result := '%d';
end;

procedure TEpiBoolField.ResetData;
begin
  FillByte(FData[0], FSize, DefaultMissing);
  DoChange(eegFields, Word(efceResetData), nil);
end;

{ TEpiStringField }

procedure TEpiStringField.SetDefaultValue(const AValue: EpiString);
var
  Val: EpiString;
begin
  if FDefaultValue = AValue then exit;
  Val := FDefaultValue;
  FDefaultValue := AValue;
  DoChange(eegFields, Word(efceDefaultValue), @Val);
end;

function TEpiStringField.GetAsBoolean(const index: Integer): EpiBool;
begin
  if IsMissing[Index] then
    result := TEpiBoolField.DefaultMissing
  else if Trim(AsString[index]) <> '' then
    result := 1
  else
    result := 0;
end;

function TEpiStringField.GetAsDate(const index: Integer): EpiDate;
var
  Dummy: string;
begin
  if IsMissing[Index] or
     (not EpiStrToDateGuess(AsString[Index], Result, Dummy))
  then
    result := TEpiDateField.DefaultMissing;
//  else        // TODO : String To Date conversion.
//    result := 0; //Trunc(EpiDateToDateTime(AsString[index], ftDate, Length(AsString[index])));
end;

function TEpiStringField.GetAsDateTime(const index: Integer): EpiDateTime;
var
  Dummy: string;
begin
  if IsMissing[Index] or
     (not EpiStrToTimeGues(AsString[Index], Result, Dummy))
  then
    result := TEpiDateTimeField.DefaultMissing;
//  else        // TODO : String To Date conversion.
//    result := 0; //Trunc(EpiDateToDateTime(AsString[index], ftDate, Length(AsString[index])));
end;

function TEpiStringField.GetAsFloat(const index: Integer): EpiFloat;
begin
  if IsMissing[Index] then
    result := TEpiFloatField.DefaultMissing
  else
    result := StrToFloatDef(AsString[index], TEpiFloatField.DefaultMissing);
end;

function TEpiStringField.GetAsInteger(const index: Integer): EpiInteger;
begin
  if IsMissing[Index] then
    result := TEpiIntField.DefaultMissing
  else
    result := StrToIntDef(AsString[index], TEpiIntField.DefaultMissing);
end;

function TEpiStringField.GetAsString(const index: Integer): EpiString;
begin
  CheckIndex(Index);
  result := FData[index];
end;

function TEpiStringField.GetAsTime(const index: Integer): EpiTime;
var
  Dummy: string;
begin
  if IsMissing[Index] or
     (not EpiStrToTimeGues(AsString[Index], Result, Dummy))
  then
    result := TEpiDateTimeField.DefaultMissing;
//  else        // TODO : String To Time conversion.
//    result := 0; //Trunc(EpiDateToDateTime(AsString[index], ftDate, Length(AsString[index])));
end;

function TEpiStringField.GetAsValue(const index: Integer): EpiVariant;
begin
  result := AsString[index];
end;

function TEpiStringField.GetCapacity: Integer;
begin
  result := System.Length(FData);
end;

function TEpiStringField.DoGetDefaultValueAsString: string;
begin
  Result := DefaultValue;
end;

procedure TEpiStringField.DoSetDefaultValueAsString(const AValue: string);
begin
  if AValue <> '' then
    FDefaultValue := AValue
  else
    HasDefaultValue := false;
end;

function TEpiStringField.GetIsMissing(const index: Integer): boolean;
var
  S: String;
begin
  S := AsString[index];
  result := (S = DefaultMissing) or (S = '');
end;

function TEpiStringField.GetHasDefaultValue: boolean;
begin
  Result := FDefaultValue <> DefaultMissing;
end;

procedure TEpiStringField.SetAsBoolean(const index: Integer;
  const AValue: EpiBool);
begin
  if TEpiBoolField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else if AValue >= 1 then
    AsString[Index] := 'Y' // TODO : Tranlation for boolean characters.
  else
    AsString[Index] := 'N' // TODO : Tranlation for boolean characters.
end;

procedure TEpiStringField.SetAsDate(const index: Integer; const AValue: EpiDate
  );
begin
  if TEpiDateField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else                 // TODO : Date to String conversion.
    AsString[index] := DateToStr(TDateTime(AValue));
end;

procedure TEpiStringField.SetAsDateTime(const index: Integer;
  const AValue: EpiDateTime);
begin
  if TEpiDateTimeField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else                 // TODO : Date to String conversion.
    AsString[index] := DateToStr(AValue);
end;

procedure TEpiStringField.SetAsFloat(const index: Integer;
  const AValue: EpiFloat);
begin
  if TEpiFloatField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsString[index] := FloatToStr(AValue);
end;

procedure TEpiStringField.SetAsInteger(const index: Integer;
  const AValue: EpiInteger);
begin
  if TEpiIntField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsString[index] := IntToStr(AValue);
end;

procedure TEpiStringField.SetAsString(const index: Integer;
  const AValue: EpiString);
var
  Val: PEpiFieldDataEventRecord;
  OldString: EpiString;
  I: SizeInt;
begin
  CheckIndex(Index);
  if FData[Index] = AValue then exit;

  if not LoadingRecs then
  begin;
    OldString := FData[Index];
    Val := New(PEpiFieldDataEventRecord);
    Val^.Index     := index;
    Val^.FieldType := FieldType;
    Val^.StringValue  := @OldString;
  end;

  FData[index] := AValue;

  if not LoadingRecs then
  begin
    DoChange(eegFields, word(efceData), Val);
    I := StringRefCount(OldString);
    Dispose(Val);
  end;
  I := StringRefCount(OldString);
end;

procedure TEpiStringField.SetAsTime(const index: Integer; const AValue: EpiTime
  );
begin
  if TEpiDateTimeField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else                 // TODO : Date to String conversion.
    AsString[index] := DateToStr(AValue);
end;

procedure TEpiStringField.SetAsValue(const index: Integer;
  const AValue: EpiVariant);
begin
  AsString[index] := AValue;
end;

procedure TEpiStringField.SetIsMissing(const index: Integer;
  const AValue: boolean);
begin
  AsString[index] := DefaultMissing;
end;

procedure TEpiStringField.SetCapacity(AValue: Integer);
var
  i: Integer;
begin
  if AValue = Capacity then exit;
  System.SetLength(FData, AValue);
  for i := FCapacity to AValue-1 do
    FData[i] := DefaultMissing;
  FCapacity := AValue;
end;

procedure TEpiStringField.SetHasDefaultValue(const AValue: boolean);
begin
  if not AValue then FDefaultValue := DefaultMissing;
end;

constructor TEpiStringField.Create(AOwner: TEpiCustomBase;
  AFieldType: TEpiFieldType);
begin
  inherited Create(AOwner, AFieldType);
  FLength := 20;
end;

procedure TEpiStringField.MovePackData(const SrcIdx, DstIdx, Count: integer);
var
  i: LongInt;
begin
  // This is string - cannot use Move function, strings are ref. counted.
  for i := 0 to Count - 1 do
   FData[DstIdx + i] := FData[SrcIdx + i];
end;

class function TEpiStringField.CheckMissing(AValue: EpiString): boolean;
begin
  result := AValue = DefaultMissing;
end;

class function TEpiStringField.DefaultMissing: EpiString;
begin
  result := NA_STRING;
end;

function TEpiStringField.DoCompare(i, j: integer): integer;
begin
  Result := UTF8CompareStr(AsString[i], AsString[j]);
//  Result := AnsiCompareText(AsString[i], AsString[j]);
end;

function TEpiStringField.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
  ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
var
  i: Integer;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);

  with TEpiStringField(Result) do
  begin
    Capacity := Self.Capacity;
    for i := 0 to Self.Size - 1 do
      FData[i] := Self.FData[i];
    FSize := Self.FSize;
    FDefaultValue := Self.FDefaultValue;
  end;
end;

procedure TEpiStringField.Exchange(i, j: integer);
var
  TmpStr: string;
begin
  TmpStr := AsString[i];
  AsString[i] := AsString[j];
  AsString[j] := TmpStr;
end;

procedure TEpiStringField.Assign(const AEpiCustomBase: TEpiCustomBase);
var
  i: Integer;
begin
  inherited Assign(AEpiCustomBase);

  if not AEpiCustomBase.InheritsFrom(TEpiStringField) then exit;

  with TEpiStringField(AEpiCustomBase) do
  begin
    Self.Capacity      := Capacity;
    Self.FSize         := FSize;
    Self.FDefaultValue := FDefaultValue;
    for i := 0 to Self.Size - 1 do
      Self.FData[i] := FData[i];
  end;
end;

function TEpiStringField.FormatString(const FillSpace: boolean): string;
begin
  if FillSpace then
    result := '%' + IntToStr(Length) + 's'
  else
    Result := '%s';
end;

procedure TEpiStringField.ResetData;
var
  i: Integer;
begin
  for i := 0 to FSize - 1 do
    FData[i] := DefaultMissing;
  DoChange(eegFields, Word(efceResetData), nil);
end;

{ TEpiMemoField }

procedure TEpiMemoField.SetHeight(AValue: integer);
var
  Val: Integer;
begin
  if FHeight = AValue then Exit;
  Val := FHeight;
  FHeight := AValue;
  DoChange(eegFields, Word(efceSetHeight), @Val);
end;

procedure TEpiMemoField.SetWidth(AValue: integer);
var
  Val: Integer;
begin
  if FWidth = AValue then Exit;
  Val := FWidth;
  FWidth := AValue;
  DoChange(eegFields, Word(efceSetWidth), @Val);
end;

function TEpiMemoField.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);

  SaveDomAttr(Result, rsWidth, Width);
  SaveDomAttr(Result, rsHeight, Height);
end;

function TEpiMemoField.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
  ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);

  with TEpiMemoField(Result) do
  begin
    FWidth  := Self.FWidth;
    FHeight := Self.FHeight;
  end;
end;

procedure TEpiMemoField.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
begin
  inherited LoadFromXml(Root, ReferenceMap);

  Width := LoadAttrInt(Root, rsWidth);
  Height := LoadAttrInt(Root, rsHeight);
end;

procedure TEpiMemoField.Assign(const AEpiCustomBase: TEpiCustomBase);
begin
  inherited Assign(AEpiCustomBase);

  if not AEpiCustomBase.InheritsFrom(TEpiMemoField) then exit;

  with TEpiMemoField(AEpiCustomBase) do
  begin
    Self.FWidth  := FWidth;
    Self.FHeight := FHeight;
  end;
end;

function TEpiMemoField.FormatString(const FillSpace: boolean): string;
begin
  Result := '%s';
end;

{ TEpiCustomAutoField }

procedure TEpiCustomAutoField.SetAutoMode(const AValue: TEpiAutoUpdateMode);
begin
  if FAutoMode = AValue then exit;
  FAutoMode := AValue;
end;

function TEpiCustomAutoField.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);

  if FieldType in AutoUpdateFieldTypes then
    SaveDomAttrEnum(Result, rsAutoUpdateMode, AutoMode, TypeInfo(TEpiAutoUpdateMode));
end;

function TEpiCustomAutoField.DoClone(AOwner: TEpiCustomBase;
  Dest: TEpiCustomBase; ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);
  TEpiCustomAutoField(Result).FAutoMode := FAutoMode;
end;

procedure TEpiCustomAutoField.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
begin
  inherited LoadFromXml(Root, ReferenceMap);

  if FieldType in AutoUpdateFieldTypes then
    AutoMode := TEpiAutoUpdateMode(LoadAttrEnum(Root, rsAutoUpdateMode, TypeInfo(TEpiAutoUpdateMode)));
end;

procedure TEpiCustomAutoField.Assign(const AEpiCustomBase: TEpiCustomBase);
begin
  inherited Assign(AEpiCustomBase);

  AutoMode := TEpiCustomAutoField(AEpiCustomBase).AutoMode;
end;

{ TEpiDateField }

procedure TEpiDateField.SetDefaultValue(const AValue: EpiDate);
var
  Val: EpiDate;
begin
  if FDefaultValue = AValue then exit;
  Val := FDefaultValue;
  FDefaultValue := AValue;
  DoChange(eegFields, Word(efceDefaultValue), @Val);
end;

function TEpiDateField.GetAsBoolean(const index: Integer): EpiBool;
begin
  if IsMissing[Index] then
    result := TEpiBoolField.DefaultMissing
  else if AsDate[index] >= 1 then
    result := 1
  else
    result := 0;
end;

function TEpiDateField.GetAsDate(const index: Integer): EpiDate;
begin
  CheckIndex(Index);
  result := FData[index];
end;

function TEpiDateField.GetAsDateTime(const index: Integer): EpiDateTime;
begin
  if IsMissing[Index] then
    result := TEpiDateTimeField.DefaultMissing
  else
    Result := AsDate[Index];
end;

function TEpiDateField.GetAsFloat(const index: Integer): EpiFloat;
begin
  if IsMissing[Index] then
    result := TEpiFloatField.DefaultMissing
  else
    result := AsDate[index];
end;

function TEpiDateField.GetAsInteger(const index: Integer): EpiInteger;
begin
  if IsMissing[Index] then
    result := TEpiIntField.DefaultMissing
  else
    result := AsDate[index];
end;

function TEpiDateField.GetAsString(const index: Integer): EpiString;
var
  S: String;
begin
  if IsMissing[Index] then
    Exit(TEpiStringField.DefaultMissing);
  case FieldType of
    ftDMYDate, ftDMYAuto: S := 'DD/MM/YYYY';
    ftMDYDate, ftMDYAuto: S := 'MM/DD/YYYY';
    ftYMDDate, ftYMDAuto: S := 'YYYY/MM/DD';
  end;
  result := FormatDateTime(S, AsDate[index]);
end;

function TEpiDateField.GetAsTime(const index: Integer): EpiTime;
begin
  if IsMissing[Index] then
    result := TEpiDateTimeField.DefaultMissing
  else
    Result := AsDate[Index];
end;

function TEpiDateField.GetAsValue(const index: Integer): EpiVariant;
begin
  result := AsDate[index];
end;

function TEpiDateField.GetCapacity: Integer;
begin
  result := System.Length(FData);
end;

function TEpiDateField.DoGetDefaultValueAsString: string;
begin
  Result := FormatDateTime(FormatString, DefaultValue);
end;

procedure TEpiDateField.DoSetDefaultValueAsString(const AValue: string);
var
  D: EpiDate;
  S: string;
begin
  if EpiStrToDate(AValue, DateSeparator, FieldType, D, S) then
    DefaultValue := D
  else
    HasDefaultValue := false;
end;

function TEpiDateField.GetIsMissing(const index: Integer): boolean;
begin
  result := AsDate[index] = DefaultMissing;
end;

function TEpiDateField.GetHasDefaultValue: boolean;
begin
  Result := FDefaultValue <> DefaultMissing;
end;

class function TEpiDateField.CheckMissing(AValue: EpiDate): boolean;
begin
  result := AValue = DefaultMissing;
end;

class function TEpiDateField.DefaultMissing: EpiDate;
begin
  result := NA_DATE;
end;

function TEpiDateField.DoCompare(i, j: integer): integer;
begin
  Result := AsDate[i] - AsDate[j];
end;

function TEpiDateField.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
  ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);

  with TEpiDateField(Result) do
  begin
    Capacity := Self.Capacity;
    Move(Self.FData[0], FData[0], Self.Size * SizeOf(EpiDate));
    FSize := Self.FSize;
    FDefaultValue := Self.FDefaultValue;
  end;
end;

procedure TEpiDateField.Exchange(i, j: integer);
var
  TmpDate: EpiDate;
begin
  TmpDate := AsDate[i];
  AsDate[i] := AsDate[j];
  AsDate[j] := TmpDate;
end;

procedure TEpiDateField.Assign(const AEpiCustomBase: TEpiCustomBase);
begin
  inherited Assign(AEpiCustomBase);

  if not AEpiCustomBase.InheritsFrom(TEpiDateField) then exit;

  with TEpiDateField(AEpiCustomBase) do
  begin
    Self.Capacity      := Capacity;
    Self.FSize         := FSize;
    Self.FDefaultValue := FDefaultValue;
    Move(FData[0], Self.FData[0], Size * SizeOf(EpiDate));
  end;
end;

function TEpiDateField.FormatString(const FillSpace: boolean): string;
begin
  case FieldType of
    ftDMYDate, ftDMYAuto: result := 'DD/MM/YYYY';
    ftMDYDate, ftMDYAuto: result := 'MM/DD/YYYY';
    ftYMDDate, ftYMDAuto: result := 'YYYY/MM/DD';
  end;
end;

procedure TEpiDateField.ResetData;
begin
  FillDWord(FData[0], FSize, DefaultMissing);
  DoChange(eegFields, Word(efceResetData), nil);
end;

procedure TEpiDateField.SetAsBoolean(const index: Integer; const AValue: EpiBool
  );
begin
  if TEpiBoolField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsDate[index] := AValue;
end;

procedure TEpiDateField.SetAsDate(const index: Integer; const AValue: EpiDate);
var
  Val: PEpiFieldDataEventRecord;
begin
  CheckIndex(Index);
  if FData[Index] = AValue then exit;

  if not LoadingRecs then
  begin
    Val := New(PEpiFieldDataEventRecord);
    Val^.Index     := index;
    Val^.FieldType := FieldType;
    Val^.DateValue  := FData[Index];
  end;

  FData[index] := AValue;

  if not LoadingRecs then
  begin
    DoChange(eegFields, word(efceData), Val);
    Dispose(Val);
  end;
end;

procedure TEpiDateField.SetAsDateTime(const index: Integer;
  const AValue: EpiDateTime);
begin
  if TEpiDateTimeField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else                  // TODO : Define rounding function.
    AsDate[index] := Trunc(AValue);
end;

procedure TEpiDateField.SetAsFloat(const index: Integer; const AValue: EpiFloat
  );
begin
  if TEpiFloatField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else                  // TODO : Define rounding function.
    AsDate[index] := Trunc(AValue);
end;

procedure TEpiDateField.SetAsInteger(const index: Integer;
  const AValue: EpiInteger);
begin
  if TEpiIntField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsDate[index] := AValue;
end;

procedure TEpiDateField.SetAsString(const index: Integer;
  const AValue: EpiString);
var
  S: String;
begin
  if TEpiStringField.CheckMissing(AValue) then
  begin
    IsMissing[Index] := true;
    exit;
  end;
  case FieldType of
    ftDMYDate, ftDMYAuto: S := 'DD/MM/YYYY';
    ftMDYDate, ftMDYAuto: S := 'MM/DD/YYYY';
    ftYMDDate, ftYMDAuto: S := 'YYYY/MM/DD';
  end;
  DefaultFormatSettings.ShortDateFormat := S;
  AsDate[index] := Trunc(StrToDateDef(AValue, DefaultMissing));
end;

procedure TEpiDateField.SetAsTime(const index: Integer; const AValue: EpiTime);
begin
  if TEpiDateTimeField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else                  // TODO : Define rounding function.
    AsDate[index] := Trunc(AValue);
end;

procedure TEpiDateField.SetAsValue(const index: Integer;
  const AValue: EpiVariant);
begin
  AsDate[index] := AValue;
end;

procedure TEpiDateField.SetIsMissing(const index: Integer; const AValue: boolean
  );
begin
  AsDate[index] := DefaultMissing;
end;

procedure TEpiDateField.SetCapacity(AValue: Integer);
var
  i: LongInt;
begin
  if AValue = Capacity then exit;
  System.SetLength(FData, AValue);
  for i := FCapacity to AValue-1 do
    FData[i] := DefaultMissing;
  FCapacity := AValue;
end;

procedure TEpiDateField.SetHasDefaultValue(const AValue: boolean);
begin
  if not AValue then FDefaultValue := DefaultMissing;
end;

constructor TEpiDateField.Create(AOwner: TEpiCustomBase;
  AFieldType: TEpiFieldType);
begin
  inherited Create(AOwner, AFieldType);
  FLength := 10;
end;

procedure TEpiDateField.MovePackData(const SrcIdx, DstIdx, Count: integer);
begin
  Move(FData[SrcIdx], FData[DstIdx], Count * SizeOf(EpiDate));
end;

{ TEpiDateTimeField }

procedure TEpiDateTimeField.SetDefaultValue(const AValue: EpiTime);
var
  Val: EpiTime;
begin
  if FDefaultValue = AValue then exit;
  Val := FDefaultValue;
  FDefaultValue := AValue;
  DoChange(eegFields, Word(efceDefaultValue), @Val);
end;

function TEpiDateTimeField.GetAsBoolean(const index: Integer): EpiBool;
begin
  if IsMissing[Index] then
    result := TEpiBoolField.DefaultMissing
  else
    result := Sign(AsDateTime[index]);
end;

function TEpiDateTimeField.GetAsDate(const index: Integer): EpiDate;
begin
  if IsMissing[Index] then
    result := TEpiDateField.DefaultMissing
  else        // TODO : Rounding function
    result := Trunc(AsDateTime[Index]);
end;

function TEpiDateTimeField.GetAsDateTime(const index: Integer): EpiDateTime;
begin
  CheckIndex(Index);
  result := FData[index];
end;

function TEpiDateTimeField.GetAsFloat(const index: Integer): EpiFloat;
begin
  if IsMissing[Index] then
    result := TEpiFloatField.DefaultMissing
  else
    result := AsDateTime[Index];
end;

function TEpiDateTimeField.GetAsInteger(const index: Integer): EpiInteger;
begin
  if IsMissing[Index] then
    result := TEpiIntField.DefaultMissing
  else
    result := Trunc(AsDateTime[Index]);
end;

function TEpiDateTimeField.GetAsString(const index: Integer): EpiString;
begin
  if IsMissing[Index] then
    Exit(TEpiStringField.DefaultMissing);
  // Always format the Time according to HH:NN:SS -> we only support the 24h
  // time entry (no AM/PM)
  result := FormatDateTime(FormatString(), AsDateTime[Index]);
end;

function TEpiDateTimeField.GetAsTime(const index: Integer): EpiTime;
begin
  if IsMissing[Index] then
    result := DefaultMissing
  else
    result := frac(AsDateTime[Index]);
end;

function TEpiDateTimeField.GetAsValue(const index: Integer): EpiVariant;
begin
  Result := AsDateTime[index];
end;

function TEpiDateTimeField.GetCapacity: Integer;
begin
  Result := System.Length(FData);
end;

function TEpiDateTimeField.DoGetDefaultValueAsString: string;
begin
  Result := FormatDateTime(FormatString, DefaultValue);
end;

procedure TEpiDateTimeField.DoSetDefaultValueAsString(const AValue: string);
var
  T: EpiTime;
  S: string;
begin
  if EpiStrToTime(AValue, TimeSeparator, T, S) then
    DefaultValue := T
  else
    HasDefaultValue := false;
end;

function TEpiDateTimeField.GetIsMissing(const index: Integer): boolean;
begin
  Result := AsDateTime[index] = DefaultMissing;
end;

function TEpiDateTimeField.GetHasDefaultValue: boolean;
begin
  Result := FDefaultValue <> DefaultMissing;
end;

procedure TEpiDateTimeField.SetAsBoolean(const index: Integer;
  const AValue: EpiBool);
begin
  if TEpiBoolField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsDateTime[index] := AValue;
end;

procedure TEpiDateTimeField.SetAsDate(const index: Integer;
  const AValue: EpiDate);
begin
  if TEpiDateField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsDateTime[index] := AValue;
end;

procedure TEpiDateTimeField.SetAsDateTime(const index: Integer;
  const AValue: EpiDateTime);
var
  Val: PEpiFieldDataEventRecord;
begin
  CheckIndex(Index);
  if FData[Index] = AValue then exit;

  if not LoadingRecs then
  begin
    Val := New(PEpiFieldDataEventRecord);
    Val^.Index     := index;
    Val^.FieldType := FieldType;
    Val^.TimeValue  := FData[Index];
  end;

  FData[index] := AValue;

  if not LoadingRecs then
  begin
    DoChange(eegFields, word(efceData), Val);
    Dispose(Val);
  end;
end;

procedure TEpiDateTimeField.SetAsFloat(const index: Integer;
  const AValue: EpiFloat);
begin
  if TEpiFloatField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsDateTime[index] := AValue;
end;

procedure TEpiDateTimeField.SetAsInteger(const index: Integer;
  const AValue: EpiInteger);
begin
  if TEpiIntField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsDateTime[index] := AValue;
end;

procedure TEpiDateTimeField.SetAsString(const index: Integer;
  const AValue: EpiString);
begin
  if TEpiStringField.CheckMissing(AValue) then
    IsMissing[Index] := true
  else
    AsDateTime[index] := StrToTimeDef(AValue, DefaultMissing);
end;

procedure TEpiDateTimeField.SetAsTime(const index: Integer;
  const AValue: EpiTime);
begin
  AsDateTime[index] := AValue;
end;

procedure TEpiDateTimeField.SetAsValue(const index: Integer;
  const AValue: EpiVariant);
begin
  AsDateTime[index] := AValue;
end;

procedure TEpiDateTimeField.SetIsMissing(const index: Integer;
  const AValue: boolean);
begin
  AsDateTime[index] := DefaultMissing;
end;

procedure TEpiDateTimeField.SetCapacity(AValue: Integer);
var
  i: LongInt;
begin
  if AValue = Capacity then exit;
  System.SetLength(FData, AValue);
  for i := FCapacity to AValue-1 do
    FData[i] := DefaultMissing;
  FCapacity := AValue;
end;

procedure TEpiDateTimeField.SetHasDefaultValue(const AValue: boolean);
begin
  if not AValue then FDefaultValue := DefaultMissing;
end;

constructor TEpiDateTimeField.Create(AOwner: TEpiCustomBase;
  AFieldType: TEpiFieldType);
begin
  inherited Create(AOwner, AFieldType);
  FLength := 8;
end;

procedure TEpiDateTimeField.MovePackData(const SrcIdx, DstIdx, Count: integer);
begin
  Move(FData[SrcIdx], FData[DstIdx], Count * SizeOf(EpiTime));
end;

class function TEpiDateTimeField.CheckMissing(AValue: EpiDateTime): boolean;
begin
  result := AValue = DefaultMissing;
end;

class function TEpiDateTimeField.DefaultMissing: EpiDateTime;
begin
  result := NA_TIME;
end;

function TEpiDateTimeField.DoCompare(i, j: integer): integer;
begin
  Result := Sign(AsDateTime[i] - AsDateTime[j]);
end;

function TEpiDateTimeField.DoClone(AOwner: TEpiCustomBase;
  Dest: TEpiCustomBase; ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);

  with TEpiDateTimeField(Result) do
  begin
    Capacity := Self.Capacity;
    Move(Self.FData[0], FData[0], Self.Size * SizeOf(EpiDateTime));
    FSize := Self.FSize;
    FDefaultValue := Self.FDefaultValue;
  end;
end;

procedure TEpiDateTimeField.Exchange(i, j: integer);
var
  TmpDateTime: EpiDateTime;
begin
  TmpDateTime := AsDateTime[i];
  AsDateTime[i] := AsDateTime[j];
  AsDateTime[j] := TmpDateTime;
end;

procedure TEpiDateTimeField.Assign(const AEpiCustomBase: TEpiCustomBase);
begin
  inherited Assign(AEpiCustomBase);

  if not AEpiCustomBase.InheritsFrom(TEpiDateTimeField) then exit;

  with TEpiDateTimeField(AEpiCustomBase) do
  begin
    Self.Capacity      := Capacity;
    Self.FSize         := FSize;
    Self.FDefaultValue := FDefaultValue;
    Move(FData[0], Self.FData[0], Size * SizeOf(EpiDateTime));
  end;
end;

function TEpiDateTimeField.FormatString(const FillSpace: boolean): string;
begin
  result := 'HH:NN:SS';
end;

procedure TEpiDateTimeField.ResetData;
var
  i: Integer;
begin
  for i := 0 to FSize - 1 do
    FData[i] := DefaultMissing;
  DoChange(eegFields, Word(efceResetData), nil);
end;


