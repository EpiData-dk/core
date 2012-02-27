
{%MainUnit epidatafiles.pas}

const
  NA_INT       = High(EpiInteger); //  $7FFFFFFFFFFFFFFF;
  NA_FLOAT     = MaxFloat;
  NA_DATE      = NA_INT;
  NA_TIME      = $FFFFFFFF; //MaxDouble;
  NA_STRING    = '.';
  NA_BOOL      = $7F;

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

procedure TEpiFields.DoAssignList(const EpiCustomList: TEpiCustomList);
var
  OrgFields: TEpiFields absolute EpiCustomList;
  i: Integer;
  AField: TEpiField;
begin
  BeginUpdate;
  if OrgFields.Count > 0 then
  begin
    for i := 0 to OrgFields.Count - 1 do
    begin
      AField := NewField(OrgFields[i].FieldType);
      AField.Assign(OrgFields[i]);
    end;
  end;
  EndUpdate;
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

procedure TEpiFields.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMNode;
  NField: TEpiField;
  JumpNode: TDOMNode;
begin
  // Root = <Fields>
  Node := Root.FirstChild;
  while Assigned(Node) do
  begin
    CheckNode(Node, rsField);

    NField := NewField(TEpiFieldType(LoadAttrEnum(Node, rsType, TypeInfo(TEpiFieldType))));
    NField.LoadFromXml(Node);

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

procedure TEpiField.SetLength(const AValue: Cardinal);
var
  Val: LongWord;
begin
  if Length = AValue then exit;
  Val := Length;
  FLength := AValue;
  DoChange(eegDataFiles, Word(efceSetLength), @Val);
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

procedure TEpiField.LocalEventHook(Sender: TObject; EventGroup: TEpiEventGroup;
  EventType: Word; Data: Pointer);
begin
  if (Sender is TEpiValueLabelSet) or (Sender is TEpiField) then
  begin
    if (EventGroup = eegCustomBase) and (EventType = word(ecceDestroy)) then
      if Sender is TEpiValueLabelSet then
        FValueLabelSet := nil
      else
        FValueLabelWriteField := nil;
  end;

  if (Sender is TEpiValueLabelSets) or (Sender is TEpiFields) then
  begin
    if (EventGroup = eegCustomBase) and
       (EventType = word(ecceDelItem)) then
      if (Sender is TEpiValueLabelSets) and (FValueLabelSet = TEpiValueLabelSet(Data)) then
        FValueLabelSet := nil
      else
      if (Sender is TEpiFields) and (FValueLabelWriteField = TEpiField(Data)) then
        FValueLabelWriteField := nil;
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
begin
  if FValueLabelSet = AValue then exit;

  if Assigned(FValueLabelSet) then
  begin
    // UnRegister our hook on the previous valuelabel set.
    FValueLabelSet.UnRegisterOnChangeHook(@LocalEventHook);
    // also do the same with the parent.
    if Assigned(FValueLabelSet.Owner) then
      FValueLabelSet.Owner.UnRegisterOnChangeHook(@LocalEventHook);
  end;

  FValueLabelSet := AValue;

  if Assigned(FValueLabelSet) then
  begin
    FValueLabelSet.RegisterOnChangeHook(@LocalEventHook, true);
    if Assigned(FValueLabelSet.Owner) then
      FValueLabelSet.Owner.RegisterOnChangeHook(@LocalEventHook, true);
  end;
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
    // also do the same with the parent.
    if Assigned(FValueLabelWriteField.Owner) then
      FValueLabelWriteField.Owner.UnRegisterOnChangeHook(@LocalEventHook);
  end;

  Val := FValueLabelWriteField;
  FValueLabelWriteField := AValue;

  if Assigned(FValueLabelWriteField) then
  begin
    FValueLabelWriteField.RegisterOnChangeHook(@LocalEventHook, true);
    if Assigned(FValueLabelWriteField.Owner) then
      FValueLabelWriteField.Owner.RegisterOnChangeHook(@LocalEventHook, true);
  end;
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

procedure TEpiField.SetIsMissingValue(const index: Integer;
  const AValue: boolean);
begin
{  if Assigned(ValueLabelSet) then
  // ValueLabelSet.;
    ;          }
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
  if IsMissing[i] and IsMissing[j] then exit(0);        // Both missing, sort even.
  if IsMissing[i] and (not IsMissing[j]) then exit(1); // A missing, sort A last
  if (not IsMissing[i]) and IsMissing[j] then exit(-1);  // B missing, sort A first
  result := DoCompare(i,j);                             // Compare normal values.
end;

procedure TEpiField.Assign(const AEpiCustomBase: TEpiCustomBase);
var
  OrgField: TEpiField absolute AEpiCustomBase;
  VLSet: TEpiValueLabelSet;
begin
  inherited Assign(AEpiCustomBase);

  BeginUpdate;

  Length := OrgField.Length;
  EntryMode := OrgField.EntryMode;
  ConfirmEntry := OrgField.ConfirmEntry;
  ShowValueLabel := OrgField.ShowValueLabel;
  RepeatValue := OrgField.RepeatValue;
  if OrgField.HasDefaultValue then
    DefaultValueAsString := OrgField.DefaultValueAsString;
  ShowValueLabel := OrgField.ShowValueLabel;
  ForcePickList := OrgField.ForcePickList;

  // Valuelabels are NEVER copied - it's up to users manually to
  // copy/assign them correctly when assigning field to each other.
  ValueLabelSet := nil;

  if Assigned(OrgField.Ranges) then
  begin
    Ranges := TEpiRanges.Create(Self);
    Ranges.ItemOwner := true;
    Ranges.Assign(OrgField.Ranges);
  end;

  if Assigned(OrgField.Jumps) then
  begin
    Jumps := TEpiJumps.Create(Self);
    Jumps.ItemOwner := true;
    Jumps.Assign(OrgField.Jumps);
  end;

  if Assigned(OrgField.Comparison) then
  begin
    Comparison := TEpiComparison.Create(Self);
    Comparison.Assign(OrgField.Comparison);
  end;
  EndUpdate;
end;

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
  HasDefaultValue := false;

  RegisterClasses([Notes, Question]);
end;

class function TEpiField.CreateField(AOwner: TEpiCustomBase; AFieldType: TEpiFieldType): TEpiField;
begin
  case AFieldType of
    ftInteger, ftAutoInc:
      Result := TEpiIntField.Create(AOwner, AFieldType);

    ftDMYDate,  ftMDYDate,  ftYMDDate,
    ftDMYAuto, ftMDYAuto, ftYMDAuto:
      Result := TEpiDateField.Create(AOwner, AFieldType);

    ftTime, ftTimeAuto:
      result := TEpiDateTimeField.Create(AOwner, AFieldType);

    ftFloat:
      Result := TEpiFloatField.Create(AOwner, AFieldType);

    ftBoolean:
      Result := TEpiBoolField.Create(AOwner, AFieldType);

    ftString, ftUpperString:
      Result := TEpiStringField.Create(AOwner, AFieldType);
  else
    result := nil;
  end;
end;

destructor TEpiField.Destroy;
begin
  FQuestion.Free;
  if Assigned(FJumps) then
    FJumps.Free;
  if Assigned(FRanges) then
    FRanges.Free;
  inherited Destroy;
end;

function TEpiField.XMLName: string;
begin
  Result := rsField;
end;

function TEpiField.SaveToXml(Content: String; Lvl: integer): string;
begin
  if Assigned(Calculation) then
    Content += Calculation.SaveToXml('', Lvl + 1);
  if Assigned(Comparison) then
    Content += Comparison.SaveToXml('', Lvl + 1);
  if Assigned(Jumps) then
    Content += Jumps.SaveToXml('', Lvl + 1);
  if Assigned(Ranges) then
    Content += Ranges.SaveToXml('', Lvl + 1);

  Result := inherited SaveToXml(Content, Lvl);
end;

function TEpiField.SaveAttributesToXml: string;
begin
  Result :=
    inherited SaveAttributesToXml +
    SaveAttrEnum(rsType, Integer(FieldType), TypeInfo(TEpiFieldType)) +
    SaveAttr(rsLength, Length) +
    SaveAttr(rsDecimals, Decimals) +
    SaveAttrEnum(rsEntryMode, Integer(EntryMode), TypeInfo(TEpiEntryMode));
  if ConfirmEntry then
    Result += SaveAttr(rsConfirmEntry, ConfirmEntry);
  if RepeatValue then
    Result += SaveAttr(rsRepeatValue, RepeatValue);
  if ShowValueLabel then
    Result += SaveAttr(rsShowValueLabel, ShowValueLabel);
  if ForcePickList then
    Result += SaveAttr(rsForcePickList, ForcePickList);
  if HasDefaultValue then
    Result += SaveAttr(rsDefaultValue, DefaultValueAsString);
  if Assigned(ValueLabelSet) then
    Result += SaveAttr(rsValueLabelId, ValueLabelSet.Name);
  if Assigned(ValueLabelWriteField) then
    Result += SaveAttr(rsValueLabelWriteRef, ValueLabelWriteField.Name);
end;

procedure TEpiField.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMNode;
  Attr: TDOMAttr;
  VLFieldId: String;
begin
  // Root = <Field>
  inherited LoadFromXml(Root);
  Length   := LoadAttrInt(Root, rsLength);
  Decimals := LoadAttrInt(Root, rsDecimals);
  EntryMode := TEpiEntryMode(LoadAttrEnum(Root, rsEntryMode, TypeInfo(TEpiEntryMode)));

  ConfirmEntry := LoadAttrBool(Root, rsConfirmEntry, ConfirmEntry, false);
  RepeatValue := LoadAttrBool(Root, rsRepeatValue, RepeatValue, false);
  DefaultValueAsString := LoadAttrString(Root, rsDefaultValue, DefaultValueAsString, false);
  ShowValueLabel := LoadAttrBool(Root, rsShowValueLabel, ShowValueLabel, false);
  ForcePickList := LoadAttrBool(Root, rsForcePickList, ForcePickList, false);
  if LoadAttr(Attr, Root, rsValueLabelId, false) then
    ValueLabelSet := DataFile.ValueLabels.GetValueLabelSetByName(LoadAttrString(Root, rsValueLabelId));

  if LoadAttr(Attr, Root, rsValueLabelWriteRef, false) then
  begin
    VLFieldId := LoadAttrString(Root, rsValueLabelWriteRef);
    ValueLabelWriteField := DataFile.Fields.FieldByName[VLFieldId];
    // Since (possibly) all ValueLabelWriteRef to fields are "forward" declared then collect them into
    // a batch and process the list after all fields are loaded.
    if not Assigned(ValueLabelWriteField) then
      LinkMap.AddLink(ltValueLabelWrite, Self, VLFieldId);
  end;

  // If not present this is handled within TEpiTranslatedText;
  FQuestion.LoadFromXml(Root);
  FNotes.LoadFromXml(Root);

  if LoadNode(Node, Root, rsRanges, false) then
  begin
    Ranges := TEpiRanges.Create(Self);
    Ranges.ItemOwner := true;
    Ranges.LoadFromXml(Node);
  end;
  if LoadNode(Node, Root, rsJumps, false) then
  begin
    Jumps := TEpiJumps.Create(Self);
    Jumps.ItemOwner := true;
    Jumps.LoadFromXml(Node);
  end;
  if LoadNode(Node, Root, rsCalculation, false) then
  begin
    Case TEpiCalcType(LoadAttrEnum(Node, rsType, TypeInfo(TEpiCalcType))) of
      ctTimeDiff:      Calculation := TEpiTimeCalc.Create(Self);
      ctCombineDate:   Calculation := TEpiCombineDateCalc.Create(Self);
      ctCombineString: Calculation := TEpiCombineStringCalc.Create(Self);
    end;
    Calculation.LoadFromXml(Node);
  end;
  if LoadNode(Node, Root, rsCompare, false) then
  begin
    Comparison := TEpiComparison.Create(Self);
    Comparison.LoadFromXml(Node);
  end;
end;

{ TEpiIntField }

procedure TEpiIntField.SetDefaultValue(const AValue: EpiInteger);
var
  Val: LongInt;
begin
  if FDefaultValue = AValue then exit;
  Val := FDefaultValue;
  FDefaultValue := AValue;
  DoChange(eegFields, Word(efceDefaultValue), @Val);
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
    result := IntToStr(AsInteger[Index]);
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
  Val: EpiInteger;
begin
  CheckIndex(Index);
  Val := FData[index];
  FData[Index] := AValue;
  DoChange(eegFields, word(efceData), @Val);
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

function TEpiIntField.DoCompare(i, j: integer): integer;
begin
  result := AsInteger[i] - AsInteger[j];
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
  if FillSpace then
    result := format('%%%dd', [Length])
  else
    result := '%d';
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
  Val: EpiFloat;
begin
  CheckIndex(Index);
  Val := FData[Index];
  FData[index] := AValue;
  DoChange(eegFields, word(efceData), @Val);
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
  Result := Sign(AsFloat[i] - AsFloat[j]);
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
    result := format('%%%d.%df', [Length - Decimals - 1, Decimals]);
end;

procedure TEpiFloatField.Assign(const AEpiCustomBase: TEpiCustomBase);
begin
  inherited Assign(AEpiCustomBase);
  if AEpiCustomBase is TEpiFloatField then
    Decimals := TEpiFloatField(AEpiCustomBase).Decimals
  else
    Decimals := 1;
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
  Val: EpiBool;
begin
  CheckIndex(Index);
  Val := FData[index];
  FData[index] := AValue;
  DoChange(eegFields, Word(efceData), @Val);
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
    // TODO : Better recognition of boolean strings.
    if AValue[1] in ['Y'] then
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

procedure TEpiBoolField.Exchange(i, j: integer);
var
  TmpBool: EpiBool;
begin
  TmpBool := AsBoolean[i];
  AsBoolean[i] := AsBoolean[j];
  AsBoolean[j] := TmpBool;
end;

function TEpiBoolField.FormatString(const FillSpace: boolean): string;
begin
  Result := '%d';
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
begin
  if IsMissing[Index] then
    result := TEpiDateField.DefaultMissing
  else        // TODO : String To Date conversion.
    result := 0; //Trunc(EpiDateToDateTime(AsString[index], ftDate, Length(AsString[index])));
end;

function TEpiStringField.GetAsDateTime(const index: Integer): EpiDateTime;
begin
  if IsMissing[Index] then
    result := TEpiDateTimeField.DefaultMissing
  else        // TODO : String To Date conversion.
    result := 0; //Trunc(EpiDateToDateTime(AsString[index], ftDate, Length(AsString[index])));
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
begin
  if IsMissing[Index] then
    result := TEpiDateTimeField.DefaultMissing
  else        // TODO : String To Time conversion.
    result := 0; //Trunc(EpiDateToDateTime(AsString[index], ftDate, Length(AsString[index])));
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
begin
  result := AsString[index] = DefaultMissing;
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
  Val: EpiString;
begin
  CheckIndex(Index);
  Val := FData[index];
  FData[index] := AValue;
  DoChange(eegFields, word(efceData), @Val);
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
begin
  if AValue = Capacity then exit;
  System.SetLength(FData, AValue);
  // We do not want to initialize string, this is costly and should be avoided.
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
  // TODO: Use UTF8Compare[Text|Str]
  Result := CompareStr(AsString[i], AsString[j]);
end;

procedure TEpiStringField.Exchange(i, j: integer);
var
  TmpStr: string;
begin
  TmpStr := AsString[i];
  AsString[i] := AsString[j];
  AsString[j] := TmpStr;
end;

function TEpiStringField.FormatString(const FillSpace: boolean): string;
begin
  if FillSpace then
    result := '%' + IntToStr(Length) + 's'
  else
    Result := '%s';
end;

{ TEpiCustomAutoField }

procedure TEpiCustomAutoField.SetAutoMode(const AValue: TEpiAutoUpdateMode);
begin
  if FAutoMode = AValue then exit;
  FAutoMode := AValue;
end;

function TEpiCustomAutoField.SaveAttributesToXml: string;
begin
  if FieldType in AutoUpdateFieldTypes then
    Result := SaveAttrEnum(rsAutoUpdateMode, Integer(AutoMode), TypeInfo(TEpiAutoUpdateMode));
  Result :=
    inherited SaveAttributesToXml +
    Result;
end;

procedure TEpiCustomAutoField.LoadFromXml(Root: TDOMNode);
begin
  if FieldType in AutoUpdateFieldTypes then
    AutoMode := TEpiAutoUpdateMode(LoadAttrEnum(Root, rsAutoUpdateMode, TypeInfo(TEpiAutoUpdateMode)));
  inherited LoadFromXml(Root);
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

procedure TEpiDateField.Exchange(i, j: integer);
var
  TmpDate: EpiDate;
begin
  TmpDate := AsDate[i];
  AsDate[i] := AsDate[j];
  AsDate[j] := TmpDate;
end;

function TEpiDateField.FormatString(const FillSpace: boolean): string;
begin
  case FieldType of
    ftDMYDate, ftDMYAuto: result := 'DD/MM/YYYY';
    ftMDYDate, ftMDYAuto: result := 'MM/DD/YYYY';
    ftYMDDate, ftYMDAuto: result := 'YYYY/MM/DD';
  end;
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
  Val: EpiDate;
begin
  CheckIndex(Index);
  Val := FData[index];
  FData[index] := AValue;
  DoChange(eegFields, word(efceData), @Val);
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
  result := TimeToStr(AsDateTime[Index]);
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
  Val: EpiDateTime;
begin
  CheckIndex(index);
  Val := FData[Index];
  FData[Index] := AValue;
  DoChange(eegFields, word(efceData), @Val);
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

procedure TEpiDateTimeField.Exchange(i, j: integer);
var
  TmpDateTime: EpiDateTime;
begin
  TmpDateTime := AsDateTime[i];
  AsDateTime[i] := AsDateTime[j];
  AsDateTime[j] := TmpDateTime;
end;

function TEpiDateTimeField.FormatString(const FillSpace: boolean): string;
begin
  result := 'HH:NN:SS';
end;

