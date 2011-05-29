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
    ResultField := TEpiField(OwnerField.DataFile.Fields.GetItemById(ResultFieldId));

  if not Assigned(ResultField) then
    LinkMap.AddLink(ltCalcResField, Self, ResultFieldId);
end;

function TEpiCalculation.SaveToXml(Content: String; Lvl: integer): string;
begin
  Content +=
    SaveNode(Lvl + 1, rsResultField, ResultField.Id);
  Result := inherited SaveToXml(Content, Lvl);
end;

function TEpiCalculation.SaveAttributesToXml: string;
begin
  Result :=
    ' type="' + IntToStr(Integer(CalcType)) + '"' +
    inherited SaveAttributesToXml;
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
    StartDate := TEpiDateField(OwnerField.DataFile.Fields.GetItemById(StartDateId));
    if not Assigned(StartDate) then LinkMap.AddLink(ltCalcStartDate, Self, StartDateId);
  end;

  if LoadNode(Node, Root, rsEndDate, false) then
  begin
    EndDateId := LoadNodeString(Root, rsEndDate);
    EndDate := TEpiDateField(OwnerField.DataFile.Fields.GetItemById(EndDateId));
    if not Assigned(EndDate) then LinkMap.AddLink(ltCalcEndDate, Self, EndDateId);
  end;

  if LoadNode(Node, Root, rsStartTime, false) then
  begin
    StartTimeId := LoadNodeString(Root, rsStartTime);
    StartTime := TEpiDateTimeField(OwnerField.DataFile.Fields.GetItemById(StartTimeId));
    if not Assigned(StartTime) then LinkMap.AddLink(ltCalcStartTime, Self, StartTimeId);
  end;

  if LoadNode(Node, Root, rsEndTime, false) then
  begin
    EndTimeId := LoadNodeString(Root, rsEndTime);
    EndTime := TEpiDateTimeField(OwnerField.DataFile.Fields.GetItemById(EndTimeId));
    if not Assigned(EndTime) then LinkMap.AddLink(ltCalcEndTime, Self, EndTimeId);
  end;
end;

function TEpiTimeCalc.SaveToXml(Content: String; Lvl: integer): string;
begin
  Content +=
    SaveNode(Lvl + 1, rsTimeCalcType, Integer(TimeCalcType));
  if Assigned(StartDate) then
    Content += SaveNode(Lvl + 1, rsStartDate, StartDate.Id);
  if Assigned(StartTime) then
    Content += SaveNode(Lvl + 1, rsStartTime, StartTime.Id);
  if Assigned(EndDate) then
    Content += SaveNode(Lvl + 1, rsEndDate,   EndDate.Id);
  if Assigned(EndTime) then
    Content += SaveNode(Lvl + 1, rsEndTime,   EndTime.Id);
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

procedure TEpiCombineDateCalc.FieldHook(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  inherited FieldHook(Sender, EventGroup, EventType, Data);

  if not ((EventGroup = eegCustomBase) and (EventType = Word(ecceDestroy))) then exit;

  if Sender = FDay   then FDay   := nil;
  if Sender = FMonth then FMonth := nil;
  if Sender = FYear  then FYear  := nil;
end;

procedure TEpiCombineDateCalc.LoadFromXml(Root: TDOMNode);
var
  DayId: String;
  MonthId: String;
  YearId: String;
begin
  inherited LoadFromXml(Root);

  DayId := LoadNodeString(Root, rsDayField);
  Day := TEpiIntField(OwnerField.DataFile.Fields.GetItemById(DayId));
  if not Assigned(Day) then LinkMap.AddLink(ltCalcDayField, Self, DayId);

  MonthId := LoadNodeString(Root, rsMonthField);
  Month := TEpiIntField(OwnerField.DataFile.Fields.GetItemById(MonthId));
  if not Assigned(Month) then LinkMap.AddLink(ltCalcMonthField, Self, MonthId);

  YearId := LoadNodeString(Root, rsYearField);
  Year := TEpiIntField(OwnerField.DataFile.Fields.GetItemById(YearId));
  if not Assigned(Year) then LinkMap.AddLink(ltCalcYearField, Self, YearId);
end;

function TEpiCombineDateCalc.SaveToXml(Content: String; Lvl: integer): string;
begin
  Content +=
    SaveNode(Lvl + 1, rsDayField,   Day.Id) +
    SaveNode(Lvl + 1, rsMonthField, Month.Id) +
    SaveNode(Lvl + 1, rsYearField,  Year.Id);
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

procedure TEpiCombineStringCalc.FieldHook(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  inherited FieldHook(Sender, EventGroup, EventType, Data);

  if not ((EventGroup = eegCustomBase) and (EventType = Word(ecceDestroy))) then exit;

  if Sender = FField1 then FField1 := nil;
  if Sender = FField2 then FField2 := nil;
  if Sender = FField3 then FField3 := nil;
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
    Field1 := TEpiField(OwnerField.DataFile.Fields.GetItemById(Field1Id));
    if not Assigned(Field1) then LinkMap.AddLink(ltCalcField1, Self, Field1Id);
  end;

  if LoadNode(Node, Root, rsField2, false) then
  begin
    Field2Id := LoadNodeString(Root, rsField2);
    Field2 := TEpiField(OwnerField.DataFile.Fields.GetItemById(Field2Id));
    if not Assigned(Field2) then LinkMap.AddLink(ltCalcField2, Self, Field2Id);
  end;

  if LoadNode(Node, Root, rsField3, false) then
  begin
    Field3Id := LoadNodeString(Root, rsField3);
    Field3 := TEpiField(OwnerField.DataFile.Fields.GetItemById(Field3Id));
    if not Assigned(Field3) then LinkMap.AddLink(ltCalcField3, Self, Field3Id);
  end;

  Delim1 := LoadNodeString(Root, rsDelim1);
  Delim2 := LoadNodeString(Root, rsDelim2);
end;

function TEpiCombineStringCalc.SaveToXml(Content: String; Lvl: integer
  ): string;
begin
  if Assigned(Field1) then
    Content += SaveNode(Lvl + 1, rsField1, Field1.Id);
  if Assigned(Field2) then
    Content += SaveNode(Lvl + 1, rsField2, Field2.Id);
  if Assigned(Field3) then
    Content += SaveNode(Lvl + 1, rsField3, Field3.Id);

  Content +=
    SaveNode(Lvl + 1, rsDelim1, Delim1) +
    SaveNode(Lvl + 1, rsDelim2, Delim2);
  Result := inherited SaveToXml(Content, Lvl);
end;
