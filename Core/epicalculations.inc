{%MainUnit epidatafiles.pas}


{ TEpiCalculation }

procedure TEpiCalculation.SetResultField(const AValue: TEpiField);
begin
  if FResultField = AValue then exit;
  RegisterFieldHook(AValue, FResultField);
  FResultField := AValue;
end;

procedure TEpiCalculation.RegisterFieldHook(NewField: TEpiField;
  OldField: TEpiField);
begin
  if Assigned(OldField) then
    OldField.UnRegisterOnChangeHook(@FieldHook);
  if Assigned(NewField) then
    NewField.RegisterOnChangeHook(@FieldHook, true);
end;

procedure TEpiCalculation.FieldHook(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
var
  Field: TEpiField absolute Sender;
begin
  if Sender <> FResultField then exit;

  if (EventGroup = eegCustomBase) and (EventType = Word(ecceDestroy)) then
    FResultField := nil;
end;

constructor TEpiCalculation.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  FResultField := nil;
end;

{ TEpiTimeCalc }

procedure TEpiTimeCalc.SetEndDate(const AValue: TEpiDateField);
begin
  if FEndDate = AValue then exit;
  RegisterFieldHook(AValue, FEndDate);
  FEndDate := AValue;
end;

procedure TEpiTimeCalc.SetEndTime(const AValue: TEpiDateTimeField);
begin
  if FEndTime = AValue then exit;
  RegisterFieldHook(AValue, FEndTime);
  FEndTime := AValue;
end;

procedure TEpiTimeCalc.SetStartDate(const AValue: TEpiDateField);
begin
  if FStartDate = AValue then exit;
  RegisterFieldHook(AValue, FStartDate);
  FStartDate := AValue;
end;

procedure TEpiTimeCalc.SetStartTime(const AValue: TEpiDateTimeField);
begin
  if FStartTime = AValue then exit;
  RegisterFieldHook(AValue, FStartTime);
  FStartTime := AValue;
end;

function TEpiTimeCalc.GetCalcType: TEpiCalcType;
begin
  Result := ctTimeDiff;
end;

procedure TEpiTimeCalc.FieldHook(Sender: TObject; EventGroup: TEpiEventGroup;
  EventType: Word; Data: Pointer);
begin
  inherited FieldHook(Sender, EventGroup, EventType, Data);

  if not ((EventGroup = eegCustomBase) and (EventType = Word(ecceDestroy))) then exit;

  if Sender = FStartDate then FStartDate := nil;
  if Sender = FEndDate   then FEndDate   := nil;
  if Sender = FStartTime then FStartTime := nil;
  if Sender = FEndTime   then FEndTime   := nil;
end;

{ TEpiCombineDateCalc }

procedure TEpiCombineDateCalc.SetDay(const AValue: TEpiIntField);
begin
  if FDay = AValue then exit;
  RegisterFieldHook(AValue, FDay);
  FDay := AValue;
end;

procedure TEpiCombineDateCalc.SetMonth(const AValue: TEpiIntField);
begin
  if FMonth = AValue then exit;
  RegisterFieldHook(AValue, FMonth);
  FMonth := AValue;
end;

procedure TEpiCombineDateCalc.SetYear(const AValue: TEpiIntField);
begin
  if FYear = AValue then exit;
  RegisterFieldHook(AValue, FYear);
  FYear := AValue;
end;

function TEpiCombineDateCalc.GetCalcType: TEpiCalcType;
begin
  Result := ctCombineDate;
end;

procedure TEpiCombineDateCalc.FieldHook(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  inherited FieldHook(Sender, EventGroup, EventType, Data);

  if not ((EventGroup = eegCustomBase) and (EventType = Word(ecceDestroy))) then exit;

  if Sender = FDay   then FDay   := nil;
  if Sender = FMonth then FMonth := nil;
  if Sender = FYear  then FYear  := nil;
end;

{ TEpiCombineStringCalc }

procedure TEpiCombineStringCalc.SetDelim1(const AValue: string);
begin
  if FDelim1 = AValue then exit;
  FDelim1 := AValue;
end;

procedure TEpiCombineStringCalc.SetDelim2(const AValue: string);
begin
  if FDelim2 = AValue then exit;
  FDelim2 := AValue;
end;

procedure TEpiCombineStringCalc.SetField1(const AValue: TEpiField);
begin
  if FField1 = AValue then exit;
  RegisterFieldHook(AValue, FField1);
  FField1 := AValue;
end;

procedure TEpiCombineStringCalc.SetField2(const AValue: TEpiField);
begin
  if FField2 = AValue then exit;
  RegisterFieldHook(AValue, FField2);
  FField2 := AValue;
end;

procedure TEpiCombineStringCalc.SetField3(const AValue: TEpiField);
begin
  if FField3 = AValue then exit;
  RegisterFieldHook(AValue, FField3);
  FField3 := AValue;
end;

function TEpiCombineStringCalc.GetCalcType: TEpiCalcType;
begin
  Result := ctCombineString;
end;

procedure TEpiCombineStringCalc.FieldHook(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  inherited FieldHook(Sender, EventGroup, EventType, Data);

  if not ((EventGroup = eegCustomBase) and (EventType = Word(ecceDestroy))) then exit;

  if Sender = FField1 then FField1 := nil;
  if Sender = FField2 then FField2 := nil;
  if Sender = FField3 then FField3 := nil;
end;

