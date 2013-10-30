{%MainUnit epidatafiles.pas}

{ TEpiJumps }

function TEpiJumps.GetJump(index: integer): TEpiJump;
begin
  result := TEpiJump(Items[Index]);
end;

function TEpiJumps.GetJumpFromValue(const Value: string): TEpiJump;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to Count - 1 do
    if (Jump[i].JumpValueAsString = Value) or
       (Jump[i].JumpValueAsString = TEpiJump.DefaultOnAnyValue)
    then
      Exit(Jump[i]);
end;

function TEpiJumps.Prefix: string;
begin
  Result := 'jump_id_'
end;

constructor TEpiJumps.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
end;

destructor TEpiJumps.Destroy;
begin
  inherited Destroy;
end;

function TEpiJumps.XMLName: string;
begin
  Result := rsJumps;
end;

function TEpiJumps.ItemClass: TEpiCustomItemClass;
var
  Ft: TEpiFieldType;
begin
  Ft := TEpiField(Owner).FieldType;

  if Ft in AutoFieldTypes then
    Raise Exception.CreateFmt(
            'Field "%s" does not support jumps! (Auto Type)' + LineEnding +
            'Please Contact EpiData',
            [TEpiCustomItem(Owner).Name]);

  if Ft in BoolFieldTypes   then Result := TEpiBoolJump;
  if Ft in IntFieldTypes    then Result := TEpiIntJump;
  if Ft in FloatFieldTypes  then Result := TEpiFloatJump;
  if Ft in StringFieldTypes then Result := TEpiStringJump;
end;

function TEpiJumps.NewJump: TEpiJump;
begin
  Result := TEpiJump(NewItem());
end;

{ TEpiJump }

function GetDataFile(AJump: TEpiJump): TEpiDataFile;
var
  AParent: TEpiCustomBase;
begin
  AParent := AJump;
  repeat
    AParent := AParent.Owner;
  until AParent is TEpiDataFile;
  Result := TEpiDataFile(AParent);
end;

procedure TEpiJump.JumpToFieldChange(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
begin
  if (Initiator <> FJumpToField) then exit;
  if (EventGroup <> eegCustomBase) then exit;
  if TEpiCustomChangeEventType(EventType) <> ecceDestroy then exit;

  // Our reference field has been (or is being) destroyed,
  // change this jump to a SAVERECORD, with jrLeaveAsIs.

  FJumpToField := nil;
  FJumpType    := jtSaveRecord;
  FResetType   := jrLeaveAsIs;

  Initiator.UnRegisterOnChangeHook(@JumpToFieldChange);
  DoChange(eegCustomBase, Word(ecceReferenceDestroyed), Initiator);
end;

procedure TEpiJump.SetJumpToField(const AValue: TEpiField);
begin
  if FJumpToField = AValue then exit;

  if Assigned(FJumpToField) then
    FJumpToField.UnRegisterOnChangeHook(@JumpToFieldChange);

  FJumpToField := AValue;

  if Assigned(FJumpToField) then
    FJumpToField.RegisterOnChangeHook(@JumpToFieldChange, true);
end;

procedure TEpiJump.SetJumpType(const AValue: TEpiJumpType);
begin
  if FJumpType = AValue then exit;
  FJumpType := AValue;
end;

procedure TEpiJump.SetResetType(const AValue: TEpiJumpResetType);
begin
  if FResetType = AValue then exit;
  FResetType := AValue;
end;

constructor TEpiJump.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  FJumpType := jtToField;
  FResetType := jrLeaveAsIs;
  FJumpToField := nil;
end;

destructor TEpiJump.Destroy;
begin
  JumpToField := nil;
  inherited Destroy;
end;

function TEpiJump.WriteNameToXml: boolean;
begin
  Result := false;
end;

function TEpiJump.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase
  ): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest);

  with TEpiJump(Result) do
  begin
    FJumpType := Self.FJumpType;
    FResetType := Self.FResetType;

    if Assigned(Self.FJumpToField) then
    begin
      if (not Assigned(TEpiField(Owner.Owner).DataFile)) or
         (TEpiField(Owner.Owner).DataFile = (TEpiField(SElf.Owner.Owner).DataFile))
      then
        FJumpToField := Self.FJumpToField
      else
        FJumpToField := TEpiField(Owner.Owner).DataFile.Fields.FieldByName[Self.FJumpToField.Name];
    end;
  end;
end;

function TEpiJump.XMLName: string;
begin
  Result := rsJump;
end;

procedure TEpiJump.LoadFromXml(Root: TDOMNode);
var
  FieldId: String;
begin
  inherited LoadFromXml(Root);
  JumpType := TEpiJumpType(LoadAttrEnum(Root, rsJumpType, TypeInfo(TEpiJumpType)));
  ResetType := TEpiJumpResetType(LoadAttrEnum(Root, rsJumpReset, TypeInfo(TEpiJumpResetType)));
  if JumpType = jtToField then
  begin
    FieldId := LoadAttrString(Root, rsJumpTo);
    JumpToField := GetDataFile(Self).Fields.GetFieldByName(FieldId);

    // Since (possibly) all jumps to fields are "forward" declared then collect them into
    // a batch and process the list after all fields are loaded.
    if not assigned(JumpToField) then
      LinkMap.AddLink(ltJump, Self, FieldId);
  end;
end;

function TEpiJump.SaveAttributesToXml: string;
begin
  Result :=
    inherited SaveAttributesToXml +
    SaveAttr(rsJumpOn, GetJumpValueAsString) +
    SaveAttrEnum(rsJumpType, Integer(JumpType), TypeInfo(TEpiJumpType)) +
    SaveAttrEnum(rsJumpReset, Integer(ResetType), TypeInfo(TEpiJumpResetType));

  if JumpType = jtToField then
    Result := Result +
    SaveAttr(rsJumpTo, JumpToField.Name);
end;

procedure TEpiJump.Assign(const AEpiCustomBase: TEpiCustomBase);
var
  OrgJump: TEpiJump absolute AEpiCustomBase;
begin
  inherited Assign(AEpiCustomBase);

  BeginUpdate;
  JumpToField := OrgJump.JumpToField;
  JumpType    := OrgJump.JumpType;
  ResetType   := OrgJump.ResetType;
  EndUpdate;
end;

class function TEpiJump.DefaultOnAnyValue: string;
begin
  result := JMP_ANY;
end;

{ TEpiBoolJump }

procedure TEpiBoolJump.SetJumpValue(const AValue: EpiBool);
begin
  if FJumpValue = AValue then exit;
  FJumpValue := AValue;
end;

function TEpiBoolJump.GetJumpValueAsString: string;
begin
  if JumpValue = TEpiBoolField.DefaultMissing then
    result := DefaultOnAnyValue
  else
    Result := IntToStr(JumpValue);
end;

function TEpiBoolJump.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase
  ): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest);
  TEpiBoolJump(Result).FJumpValue := FJumpValue;
end;

procedure TEpiBoolJump.LoadFromXml(Root: TDOMNode);
var
  Attr: TDOMAttr;
  S: String;
begin
  inherited LoadFromXml(Root);
  S := LoadAttrString(Root, rsJumpOn);
  if S = DefaultOnAnyValue then
    JumpValue := TEpiBoolField.DefaultMissing
  else
    JumpValue := StrToInt(S);
end;

procedure TEpiBoolJump.Assign(const AEpiCustomBase: TEpiCustomBase);
begin
  inherited Assign(AEpiCustomBase);
  JumpValue := TEpiBoolJump(AEpiCustomBase).JumpValue;
end;

{ TEpiIntJump }

procedure TEpiIntJump.SetJumpValue(const AValue: EpiInteger);
begin
  if FJumpValue = AValue then exit;
  FJumpValue := AValue;
end;

function TEpiIntJump.GetJumpValueAsString: string;
begin
  if JumpValue = TEpiIntField.DefaultMissing then
    result := DefaultOnAnyValue
  else
    Result := IntToStr(JumpValue);
end;

function TEpiIntJump.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase
  ): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest);
  TEpiIntJump(Result).FJumpValue := FJumpValue;
end;

procedure TEpiIntJump.LoadFromXml(Root: TDOMNode);
var
  S: String;
begin
  inherited LoadFromXml(Root);
  S := LoadAttrString(Root, rsJumpOn);
  if S = DefaultOnAnyValue then
    JumpValue := TEpiIntField.DefaultMissing
  else
    JumpValue := StrToInt64(S);
end;

procedure TEpiIntJump.Assign(const AEpiCustomBase: TEpiCustomBase);
begin
  inherited Assign(AEpiCustomBase);
  JumpValue := TEpiIntJump(AEpiCustomBase).JumpValue;
end;

{ TEpiFloatJump }

procedure TEpiFloatJump.SetJumpValue(const AValue: EpiFloat);
begin
  if FJumpValue = AValue then exit;
  FJumpValue := AValue;
end;

function TEpiFloatJump.GetJumpValueAsString: string;
begin
  if JumpValue = TEpiFloatField.DefaultMissing then
    result := DefaultOnAnyValue
  else
    Result := FloatToStr(JumpValue);
end;

function TEpiFloatJump.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase
  ): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest);
  TEpiFloatJump(Result).FJumpValue := FJumpValue;
end;

procedure TEpiFloatJump.LoadFromXml(Root: TDOMNode);
var
  S: String;
begin
  inherited LoadFromXml(Root);
  S := LoadAttrString(Root, rsJumpOn);
  if S = DefaultOnAnyValue then
    JumpValue := TEpiFloatField.DefaultMissing
  else
    JumpValue := LoadAttrFloat(Root, rsJumpOn);
end;

procedure TEpiFloatJump.Assign(const AEpiCustomBase: TEpiCustomBase);
begin
  inherited Assign(AEpiCustomBase);
  JumpValue := TEpiFloatJump(AEpiCustomBase).JumpValue;
end;

{ TEpiStringJump }

procedure TEpiStringJump.SetJumpValue(const AValue: EpiString);
begin
  if FJumpValue = AValue then exit;
  FJumpValue := AValue;
end;

function TEpiStringJump.GetJumpValueAsString: string;
begin
  if JumpValue = TEpiStringField.DefaultMissing then
    result := DefaultOnAnyValue
  else
    Result := JumpValue;
end;

function TEpiStringJump.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase
  ): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest);
  TEpiStringJump(Result).FJumpValue := FJumpValue;
end;

procedure TEpiStringJump.LoadFromXml(Root: TDOMNode);
var
  Attr: TDOMAttr;
  S: String;
begin
  inherited LoadFromXml(Root);
  S := LoadAttrString(Root, rsJumpOn);
  if S = DefaultOnAnyValue then
    JumpValue := TEpiStringField.DefaultMissing
  else
    JumpValue := S;
end;

procedure TEpiStringJump.Assign(const AEpiCustomBase: TEpiCustomBase);
begin
  inherited Assign(AEpiCustomBase);
  JumpValue := TEpiStringJump(AEpiCustomBase).JumpValue;
end;

