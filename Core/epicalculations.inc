{%MainUnit epidatafiles.pas}


{ TEpiCalculation }

procedure TEpiCalculation.SetResultField(const AValue: TEpiField);
begin
  if FResultField = AValue then exit;
  RegisterFieldHook(AValue, FResultField);
  FResultField := AValue;
end;

function TEpiCalculation.GetOwnerField: TEpiField;
begin
  Result := nil;
  if Owner is TEpiField then
    result := TEpiField(Owner);
end;

procedure TEpiCalculation.RegisterFieldHook(NewField: TEpiField;
  OldField: TEpiField);
begin
  if Assigned(OldField) then
    OldField.UnRegisterOnChangeHook(@FieldHook);
  if Assigned(NewField) then
    NewField.RegisterOnChangeHook(@FieldHook, true);
end;

procedure TEpiCalculation.FieldHook(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
var
  Field: TEpiField absolute Sender;
begin
  if Initiator <> FResultField then exit;
  if (EventGroup <> eegCustomBase) then exit;
  if TEpiCustomChangeEventType(EventType) <> ecceDestroy then exit;

  FResultField := nil;
  Initiator.UnRegisterOnChangeHook(@FieldHook);
  DoChange(eegCustomBase, Word(ecceReferenceDestroyed), Initiator);
end;

function TEpiCalculation.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase
  ): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest);
  with TEpiCalculation(Result) do
  if (not Assigned(OwnerField.DataFile)) or
     (OwnerField.DataFile = Self.OwnerField.DataFile)
  then
    ResultField := Self.FResultField
  else
    ResultField := GetOwnerField.DataFile.Fields.FieldByName[Self.FResultField.Name];
end;

constructor TEpiCalculation.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  FResultField := nil;
end;

destructor TEpiCalculation.Destroy;
begin
  RegisterFieldHook(nil, FResultField);
  inherited Destroy;
end;

function TEpiCalculation.XMLName: string;
begin
  Result := rsCalculation;
end;

procedure TEpiCalculation.LoadFromXml(Root: TDOMNode);
var
  ResultFieldId: String;
begin
  inherited LoadFromXml(Root);
  ResultFieldId := LoadNodeString(Root, rsResultField);
  if Assigned(OwnerField) then
    ResultField := OwnerField.DataFile.Fields.FieldByName[ResultFieldId];

  if not Assigned(ResultField) then
    LinkMap.AddLink(ltCalcResField, Self, ResultFieldId);
end;

function TEpiCalculation.SaveToXml(Content: String; Lvl: integer): string;
begin
  Content :=
    SaveNode(Lvl + 1, rsResultField, ResultField.Name) +
    Content;
  Result := inherited SaveToXml(Content, Lvl);
end;

function TEpiCalculation.SaveAttributesToXml: string;
begin
  Result :=
    inherited SaveAttributesToXml +
    SaveAttrEnum(rsType, Integer(CalcType), TypeInfo(TEpiCalcType));
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

procedure TEpiTimeCalc.SetTimeCalcType(const AValue: TEpiTimeCalcType);
begin
  if FTimeCalcType = AValue then exit;
  FTimeCalcType := AValue;
end;

function TEpiTimeCalc.GetCalcType: TEpiCalcType;
begin
  Result := ctTimeDiff;
end;

procedure TEpiTimeCalc.FieldHook(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
begin
  inherited FieldHook(Sender, Initiator, EventGroup, EventType, Data);
  if (EventGroup <> eegCustomBase) then exit;
  if TEpiCustomChangeEventType(EventType) <> ecceDestroy then exit;

  if not (
    (Initiator = FStartDate) or
    (Initiator = FEndDate) or
    (Initiator = FStartTime) or
    (Initiator = FEndTime)
    )
  then
    Exit;

  if Initiator = FStartDate then FStartDate := nil;
  if Initiator = FEndDate   then FEndDate   := nil;
  if Initiator = FStartTime then FStartTime := nil;
  if Initiator = FEndTime   then FEndTime   := nil;
  Initiator.UnRegisterOnChangeHook(@FieldHook);
  DoChange(eegCustomBase, Word(ecceReferenceDestroyed), Initiator);
end;

function TEpiTimeCalc.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase
  ): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest);
  with TEpiTimeCalc(Result) do
  begin
    if (not Assigned(OwnerField.DataFile)) or
       (OwnerField.DataFile = Self.OwnerField.DataFile)
    then
    begin
      EndDate    := Self.FEndDate;
      EndTime    := Self.FEndTime;
      StartDate  := Self.FStartDate;
      StartTime  := Self.FStartTime;
    end else begin
      if Assigned(Self.FEndDate) then
        EndDate := TEpiDateField(GetOwnerField.DataFile.Fields.FieldByName[Self.FEndDate.Name]);
      if Assigned(Self.FEndTime) then
        EndTime := TEpiDateTimeField(GetOwnerField.DataFile.Fields.FieldByName[Self.FEndTime.Name]);
      if Assigned(Self.FStartDate) then
        StartDate := TEpiDateField(GetOwnerField.DataFile.Fields.FieldByName[Self.FStartDate.Name]);
      if Assigned(Self.FStartTime) then
        StartTime  := TEpiDateTimeField(GetOwnerField.DataFile.Fields.FieldByName[Self.FStartTime.Name]);
    end;
    FTimeCalcType := Self.FTimeCalcType;
  end;
end;

destructor TEpiTimeCalc.Destroy;
begin
  RegisterFieldHook(nil, FStartDate);
  RegisterFieldHook(nil, FStartTime);
  RegisterFieldHook(nil, FEndDate);
  RegisterFieldHook(nil, FEndTime);
  inherited Destroy;
end;

procedure TEpiTimeCalc.LoadFromXml(Root: TDOMNode);
var
  StartDateId: String;
  EndDateId: String;
  StartTimeId: String;
  EndTimeId: String;
  Node: TDOMNode;
begin
  inherited LoadFromXml(Root);

  TimeCalcType := TEpiTimeCalcType(LoadNodeInt(Root, rsTimeCalcType));

  if LoadNode(Node, Root, rsStartDate, false) then
  begin
    StartDateId := LoadNodeString(Root, rsStartDate);
    StartDate := TEpiDateField(OwnerField.DataFile.Fields.GetItemByName(StartDateId));
    if not Assigned(StartDate) then LinkMap.AddLink(ltCalcStartDate, Self, StartDateId);
  end;

  if LoadNode(Node, Root, rsEndDate, false) then
  begin
    EndDateId := LoadNodeString(Root, rsEndDate);
    EndDate := TEpiDateField(OwnerField.DataFile.Fields.GetItemByName(EndDateId));
    if not Assigned(EndDate) then LinkMap.AddLink(ltCalcEndDate, Self, EndDateId);
  end;

  if LoadNode(Node, Root, rsStartTime, false) then
  begin
    StartTimeId := LoadNodeString(Root, rsStartTime);
    StartTime := TEpiDateTimeField(OwnerField.DataFile.Fields.GetItemByName(StartTimeId));
    if not Assigned(StartTime) then LinkMap.AddLink(ltCalcStartTime, Self, StartTimeId);
  end;

  if LoadNode(Node, Root, rsEndTime, false) then
  begin
    EndTimeId := LoadNodeString(Root, rsEndTime);
    EndTime := TEpiDateTimeField(OwnerField.DataFile.Fields.GetItemByName(EndTimeId));
    if not Assigned(EndTime) then LinkMap.AddLink(ltCalcEndTime, Self, EndTimeId);
  end;
end;

function TEpiTimeCalc.SaveToXml(Content: String; Lvl: integer): string;
begin
  Content +=
    SaveNode(Lvl + 1, rsTimeCalcType, Integer(TimeCalcType));
  if Assigned(StartDate) then
    Content += SaveNode(Lvl + 1, rsStartDate, StartDate.Name);
  if Assigned(StartTime) then
    Content += SaveNode(Lvl + 1, rsStartTime, StartTime.Name);
  if Assigned(EndDate) then
    Content += SaveNode(Lvl + 1, rsEndDate,   EndDate.Name);
  if Assigned(EndTime) then
    Content += SaveNode(Lvl + 1, rsEndTime,   EndTime.Name);
  Result := inherited SaveToXml(Content, Lvl);
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

procedure TEpiCombineDateCalc.FieldHook(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
begin
  inherited FieldHook(Sender, Initiator, EventGroup, EventType, Data);
  if (EventGroup <> eegCustomBase) then exit;
  if TEpiCustomChangeEventType(EventType) <> ecceDestroy then exit;

  if not (
    (Initiator = FDay) or
    (Initiator = FMonth) or
    (Initiator = FYear)
    )
  then
    Exit;

  if Initiator = FDay   then FDay   := nil;
  if Initiator = FMonth then FMonth := nil;
  if Initiator = FYear  then FYear  := nil;

  Initiator.UnRegisterOnChangeHook(@FieldHook);
  DoChange(eegCustomBase, Word(ecceReferenceDestroyed), Initiator);
end;

function TEpiCombineDateCalc.DoClone(AOwner: TEpiCustomBase;
  Dest: TEpiCustomBase): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest);

  with TEpiCombineDateCalc(Result) do
  begin
    if (not Assigned(OwnerField.DataFile)) or
       (OwnerField.DataFile = Self.OwnerField.DataFile)
    then
    begin
      Day   := Self.FDay;
      Month := Self.FMonth;
      Year  := Self.FYear;
    end else begin
      Day   := TEpiIntField(GetOwnerField.DataFile.Fields.FieldByName[Self.FDay.Name]);
      Month := TEpiIntField(GetOwnerField.DataFile.Fields.FieldByName[Self.FMonth.Name]);
      Year  := TEpiIntField(GetOwnerField.DataFile.Fields.FieldByName[Self.FYear.Name]);
    end;
  end;
end;

destructor TEpiCombineDateCalc.Destroy;
begin
  RegisterFieldHook(nil, FYear);
  RegisterFieldHook(nil, FDay);
  RegisterFieldHook(nil, FMonth);
  inherited Destroy;
end;

procedure TEpiCombineDateCalc.LoadFromXml(Root: TDOMNode);
var
  DayId: String;
  MonthId: String;
  YearId: String;
begin
  inherited LoadFromXml(Root);

  DayId := LoadNodeString(Root, rsDayField);
  Day := TEpiIntField(OwnerField.DataFile.Fields.GetItemByName(DayId));
  if not Assigned(Day) then LinkMap.AddLink(ltCalcDayField, Self, DayId);

  MonthId := LoadNodeString(Root, rsMonthField);
  Month := TEpiIntField(OwnerField.DataFile.Fields.GetItemByName(MonthId));
  if not Assigned(Month) then LinkMap.AddLink(ltCalcMonthField, Self, MonthId);

  YearId := LoadNodeString(Root, rsYearField);
  Year := TEpiIntField(OwnerField.DataFile.Fields.GetItemByName(YearId));
  if not Assigned(Year) then LinkMap.AddLink(ltCalcYearField, Self, YearId);
end;

function TEpiCombineDateCalc.SaveToXml(Content: String; Lvl: integer): string;
begin
  Content +=
    SaveNode(Lvl + 1, rsDayField,   Day.Name) +
    SaveNode(Lvl + 1, rsMonthField, Month.Name) +
    SaveNode(Lvl + 1, rsYearField,  Year.Name);
  Result := inherited SaveToXml(Content, Lvl);
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

procedure TEpiCombineStringCalc.FieldHook(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
begin
  inherited FieldHook(Sender, Initiator, EventGroup, EventType, Data);
  if (EventGroup <> eegCustomBase) then exit;
  if TEpiCustomChangeEventType(EventType) <> ecceDestroy then exit;

  if not (
    (Initiator = FField1) or
    (Initiator = FField2) or
    (Initiator = FField3)
    )
  then
    Exit;

  if Initiator = FField1 then FField1 := nil;
  if Initiator = FField2 then FField2 := nil;
  if Initiator = FField3 then FField3 := nil;

  Initiator.UnRegisterOnChangeHook(@FieldHook);
  DoChange(eegCustomBase, Word(ecceReferenceDestroyed), Initiator);
end;

function TEpiCombineStringCalc.DoClone(AOwner: TEpiCustomBase;
  Dest: TEpiCustomBase): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest);

  with TEpiCombineStringCalc(Result) do
  begin
    if (not Assigned(OwnerField.DataFile)) or
       (OwnerField.DataFile = Self.OwnerField.DataFile)
    then
    begin
      if Assigned(Self.FField1) then
        Field1 := Self.FField1;
      if Assigned(Self.FField2) then
        Field2 := Self.FField2;
      if Assigned(Self.FField3) then
        Field3 := Self.FField3;
    end else begin
      if Assigned(Self.FField1) then
        Field1 := GetOwnerField.DataFile.Fields.FieldByName[Self.FField1.Name];
      if Assigned(Self.FField2) then
        Field2 := GetOwnerField.DataFile.Fields.FieldByName[Self.FField2.Name];
      if Assigned(Self.FField3) then
        Field3 := GetOwnerField.DataFile.Fields.FieldByName[Self.FField3.Name];
    end;

    FDelim1 := Self.FDelim1;
    FDelim2 := Self.FDelim2;
  end;
end;

destructor TEpiCombineStringCalc.Destroy;
begin
  RegisterFieldHook(nil, FField1);
  RegisterFieldHook(nil, FField2);
  RegisterFieldHook(nil, FField3);
  inherited Destroy;
end;

procedure TEpiCombineStringCalc.LoadFromXml(Root: TDOMNode);
var
  Field1Id: String;
  Field2Id: String;
  Field3Id: String;
  Node: TDOMNode;
begin
  inherited LoadFromXml(Root);

  if LoadNode(Node, Root, rsField1, false) then
  begin
    Field1Id := LoadNodeString(Root, rsField1);
    Field1 := OwnerField.DataFile.Fields.FieldByName[Field1Id];
    if not Assigned(Field1) then LinkMap.AddLink(ltCalcField1, Self, Field1Id);
  end;

  if LoadNode(Node, Root, rsField2, false) then
  begin
    Field2Id := LoadNodeString(Root, rsField2);
    Field2 := OwnerField.DataFile.Fields.FieldByName[Field2Id];
    if not Assigned(Field2) then LinkMap.AddLink(ltCalcField2, Self, Field2Id);
  end;

  if LoadNode(Node, Root, rsField3, false) then
  begin
    Field3Id := LoadNodeString(Root, rsField3);
    Field3 := OwnerField.DataFile.Fields.FieldByName[Field3Id];
    if not Assigned(Field3) then LinkMap.AddLink(ltCalcField3, Self, Field3Id);
  end;

  Delim1 := LoadNodeString(Root, rsDelim1);
  Delim2 := LoadNodeString(Root, rsDelim2);
end;

function TEpiCombineStringCalc.SaveToXml(Content: String; Lvl: integer
  ): string;
begin
  if Assigned(Field1) then
    Content += SaveNode(Lvl + 1, rsField1, Field1.Name);
  if Assigned(Field2) then
    Content += SaveNode(Lvl + 1, rsField2, Field2.Name);
  if Assigned(Field3) then
    Content += SaveNode(Lvl + 1, rsField3, Field3.Name);

  Content +=
    SaveNode(Lvl + 1, rsDelim1, Delim1) +
    SaveNode(Lvl + 1, rsDelim2, Delim2);
  Result := inherited SaveToXml(Content, Lvl);
end;

