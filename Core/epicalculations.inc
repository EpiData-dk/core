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

procedure TEpiCalculation.FixupReferences(EpiClassType: TEpiCustomBaseClass;
  ReferenceType: Byte; const ReferenceId: string);
begin
  if (EpiClassType = TEpiCalculation)
  then
    begin

      case ReferenceType of
        0: // ResultField
          if Assigned(GetOwnerField) then
            ResultField := GetOwnerField.DataFile.Fields.FieldByName[ReferenceId];
      end;

    end
  else
    inherited FixupReferences(EpiClassType, ReferenceType, ReferenceId);
end;

function TEpiCalculation.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
  ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);

  if Assigned(ResultField) then
    ReferenceMap.AddFixupReference(Result, TEpiCalculation, 0, ResultField.Name);

{  with TEpiCalculation(Result) do
  if (not Assigned(OwnerField.DataFile)) or
     (OwnerField.DataFile = Self.OwnerField.DataFile)
  then
    ResultField := Self.FResultField
  else
    ResultField := GetOwnerField.DataFile.Fields.FieldByName[Self.FResultField.Name]; }
end;

function TEpiCalculation.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);

  SaveDomAttrEnum(Result, rsType, CalcType, TypeInfo(TEpiCalcType));

  SaveTextContent(Result, rsResultField, ResultField.Name);
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

procedure TEpiCalculation.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
var
  ResultFieldId: String;
begin
  inherited LoadFromXml(Root, ReferenceMap);

  ReferenceMap.AddFixupReference(Self, TEpiCalculation, 0, LoadNodeString(Root, rsResultField));
{  ResultFieldId := LoadNodeString(Root, rsResultField);

  if Assigned(OwnerField) then
    ResultField := OwnerField.DataFile.Fields.FieldByName[ResultFieldId];

  if not Assigned(ResultField) then
    LinkMap.AddLink(ltCalcResField, Self, ResultFieldId);      }
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

procedure TEpiTimeCalc.FixupReferences(EpiClassType: TEpiCustomBaseClass;
  ReferenceType: Byte; const ReferenceId: string);
var
  F: TEpiField;
begin
  if (EpiClassType = TEpiTimeCalc)
  then
    begin
      if Not Assigned(GetOwnerField) then exit;
      F := GetOwnerField.DataFile.Fields.FieldByName[ReferenceId];

      case ReferenceType of
        0: // EndDate
          EndDate := TEpiDateField(F);

        1: // EndTime
          EndTime := TEpiDateTimeField(F);

        2: // StartDate
          StartDate := TEpiDateField(F);

        3: // StartTime
          StartTime := TEpiDateTimeField(F);

      end;
    end
  else
    inherited FixupReferences(EpiClassType, ReferenceType, ReferenceId);
end;

function TEpiTimeCalc.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
  ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);

  TEpiTimeCalc(Result).FTimeCalcType := Self.FTimeCalcType;

  if Assigned(EndDate) then
    ReferenceMap.AddFixupReference(Result, TEpiTimeCalc, 0, EndDate.Name);

  if Assigned(EndTime) then
    ReferenceMap.AddFixupReference(Result, TEpiTimeCalc, 1, EndTime.Name);

  if Assigned(StartDate) then
    ReferenceMap.AddFixupReference(Result, TEpiTimeCalc, 2, StartDate.Name);

  if Assigned(StartTime) then
    ReferenceMap.AddFixupReference(Result, TEpiTimeCalc, 3, StartTime.Name);


{  with TEpiTimeCalc(Result) do
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
  end;    }
end;

function TEpiTimeCalc.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);

  SaveTextContent(Result, rsTimeCalcType, Integer(TimeCalcType));

  if Assigned(StartDate) then
    SaveTextContent(Result, rsStartDate, StartDate.Name);
  if Assigned(StartTime) then
    SaveTextContent(Result, rsStartTime, StartTime.Name);
  if Assigned(EndDate) then
    SaveTextContent(Result, rsEndDate,   EndDate.Name);
  if Assigned(EndTime) then
    SaveTextContent(Result, rsEndTime,   EndTime.Name);
end;

destructor TEpiTimeCalc.Destroy;
begin
  RegisterFieldHook(nil, FStartDate);
  RegisterFieldHook(nil, FStartTime);
  RegisterFieldHook(nil, FEndDate);
  RegisterFieldHook(nil, FEndTime);
  inherited Destroy;
end;

procedure TEpiTimeCalc.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
var
  StartDateId: String;
  EndDateId: String;
  StartTimeId: String;
  EndTimeId: String;
  Node: TDOMNode;
begin
  inherited LoadFromXml(Root, ReferenceMap);

  TimeCalcType := TEpiTimeCalcType(LoadNodeInt(Root, rsTimeCalcType));

  if LoadNode(Node, Root, rsEndDate, false) then
    ReferenceMap.AddFixupReference(Self, TEpiTimeCalc, 0, LoadNodeString(Root, rsEndDate));

  if LoadNode(Node, Root, rsEndTime, false) then
    ReferenceMap.AddFixupReference(Self, TEpiTimeCalc, 1, LoadNodeString(Root, rsEndTime));

  if LoadNode(Node, Root, rsStartDate, false) then
    ReferenceMap.AddFixupReference(Self, TEpiTimeCalc, 2, LoadNodeString(Root, rsStartDate));

  if LoadNode(Node, Root, rsStartTime, false) then
    ReferenceMap.AddFixupReference(Self, TEpiTimeCalc, 3, LoadNodeString(Root, rsStartTime));

{
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
  end;}
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

procedure TEpiCombineDateCalc.FixupReferences(
  EpiClassType: TEpiCustomBaseClass; ReferenceType: Byte;
  const ReferenceId: string);
var
  F: TEpiIntField;
begin
  if (EpiClassType = TEpiCombineDateCalc)
  then
    begin
      if not Assigned(GetOwnerField) then Exit;
      F := TEpiIntField(GetOwnerField.DataFile.Fields.FieldByName[ReferenceId]);

      case ReferenceType of
        0: // Day
          Day := F;

        1: // Month
          Month := F;

        2: // Year
          Year := F;

      end;

    end
  else
    inherited FixupReferences(EpiClassType, ReferenceType, ReferenceId);
end;

function TEpiCombineDateCalc.DoClone(AOwner: TEpiCustomBase;
  Dest: TEpiCustomBase; ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);

  if Assigned(Day) then
    ReferenceMap.AddFixupReference(Result, TEpiCombineDateCalc, 0, Day.Name);

  if Assigned(Month) then
    ReferenceMap.AddFixupReference(Result, TEpiCombineDateCalc, 1, Month.Name);

  if Assigned(Year) then
    ReferenceMap.AddFixupReference(Result, TEpiCombineDateCalc, 2, Year.Name);

{  with TEpiCombineDateCalc(Result) do
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
  end;   }
end;

function TEpiCombineDateCalc.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);

  SaveTextContent(Result, rsDayField,   Day.Name);
  SaveTextContent(Result, rsMonthField, Month.Name);
  SaveTextContent(Result, rsYearField,  Year.Name);
end;

destructor TEpiCombineDateCalc.Destroy;
begin
  RegisterFieldHook(nil, FYear);
  RegisterFieldHook(nil, FDay);
  RegisterFieldHook(nil, FMonth);
  inherited Destroy;
end;

procedure TEpiCombineDateCalc.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
var
  DayId: String;
  MonthId: String;
  YearId: String;
begin
  inherited LoadFromXml(Root, ReferenceMap);

  ReferenceMap.AddFixupReference(Self, TEpiCombineDateCalc, 0, LoadNodeString(Root, rsDayField));
  ReferenceMap.AddFixupReference(Self, TEpiCombineDateCalc, 1, LoadNodeString(Root, rsMonthField));
  ReferenceMap.AddFixupReference(Self, TEpiCombineDateCalc, 2, LoadNodeString(Root, rsYearField));

{
  DayId := LoadNodeString(Root, rsDayField);
  Day := TEpiIntField(OwnerField.DataFile.Fields.GetItemByName(DayId));
  if not Assigned(Day) then LinkMap.AddLink(ltCalcDayField, Self, DayId);

  MonthId := LoadNodeString(Root, rsMonthField);
  Month := TEpiIntField(OwnerField.DataFile.Fields.GetItemByName(MonthId));
  if not Assigned(Month) then LinkMap.AddLink(ltCalcMonthField, Self, MonthId);

  YearId := LoadNodeString(Root, rsYearField);
  Year := TEpiIntField(OwnerField.DataFile.Fields.GetItemByName(YearId));
  if not Assigned(Year) then LinkMap.AddLink(ltCalcYearField, Self, YearId);   }
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

procedure TEpiCombineStringCalc.FixupReferences(
  EpiClassType: TEpiCustomBaseClass; ReferenceType: Byte;
  const ReferenceId: string);
var
  F: TEpiField;
begin
  if (EpiClassType = TEpiCombineStringCalc)
  then
    begin
      if not Assigned(GetOwnerField) then exit;
      F := GetOwnerField.DataFile.Fields.FieldByName[ReferenceId];

      case ReferenceType of
        0: // Field1
          Field1 := F;

        1: // Field2
          Field2 := F;

        2: // Field3
          Field3 := F;
      end;

    end
  else
    inherited FixupReferences(EpiClassType, ReferenceType, ReferenceId);
end;

function TEpiCombineStringCalc.DoClone(AOwner: TEpiCustomBase;
  Dest: TEpiCustomBase; ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);

  with TEpiCombineStringCalc(Result) do
  begin
    FDelim1 := Self.FDelim1;
    FDelim2 := Self.FDelim2;
  end;

  if Assigned(Field1) then
    ReferenceMap.AddFixupReference(Result, TEpiCombineStringCalc, 0, Field1.Name);

  if Assigned(Field2) then
    ReferenceMap.AddFixupReference(Result, TEpiCombineStringCalc, 1, Field2.Name);

  if Assigned(Field3) then
    ReferenceMap.AddFixupReference(Result, TEpiCombineStringCalc, 2, Field3.Name);

{  with TEpiCombineStringCalc(Result) do
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

  end;     }
end;

function TEpiCombineStringCalc.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);

  if Assigned(Field1) then
    SaveTextContent(Result, rsField1, Field1.Name);
  if Assigned(Field2) then
    SaveTextContent(Result, rsField2, Field2.Name);
  if Assigned(Field3) then
    SaveTextContent(Result, rsField3, Field3.Name);

  SaveTextContent(Result, rsDelim1, Delim1);
  SaveTextContent(Result, rsDelim2, Delim2);
end;

destructor TEpiCombineStringCalc.Destroy;
begin
  RegisterFieldHook(nil, FField1);
  RegisterFieldHook(nil, FField2);
  RegisterFieldHook(nil, FField3);
  inherited Destroy;
end;

procedure TEpiCombineStringCalc.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
var
  Field1Id: String;
  Field2Id: String;
  Field3Id: String;
  Node: TDOMNode;
begin
  inherited LoadFromXml(Root, ReferenceMap);

  if LoadNode(Node, Root, rsField1, false) then
    ReferenceMap.AddFixupReference(Self, TEpiCombineStringCalc, 0, LoadNodeString(Root, rsField1));

  if LoadNode(Node, Root, rsField2, false) then
    ReferenceMap.AddFixupReference(Self, TEpiCombineStringCalc, 1, LoadNodeString(Root, rsField2));

  if LoadNode(Node, Root, rsField3, false) then
    ReferenceMap.AddFixupReference(Self, TEpiCombineStringCalc, 2, LoadNodeString(Root, rsField3));

{  if LoadNode(Node, Root, rsField1, false) then
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
  end;                  }

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

