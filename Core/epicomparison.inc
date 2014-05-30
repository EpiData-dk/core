{%MainUnit epidatafiles.pas}

function TEpiComparison.GetOwnerField: TEpiField;
begin
  result := TEpiField(Owner);
end;

procedure TEpiComparison.SetCompareField(const AValue: TEpiField);
begin
  if FCompareField = AValue then exit;

  if Assigned(FCompareField) then
    FCompareField.UnRegisterOnChangeHook(@FieldHook);

  FCompareField := AValue;

  if Assigned(FCompareField) then
    FCompareField.RegisterOnChangeHook(@FieldHook, true);
end;

procedure TEpiComparison.SetCompareType(const AValue: TEpiComparisonType);
begin
  if FCompareType = AValue then exit;
  FCompareType := AValue;
end;

procedure TEpiComparison.FieldHook(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
var
  Field: TEpiField absolute Sender;
begin
  if (Initiator <> FCompareField) then exit;
  if (EventGroup <> eegCustomBase) then exit;
  if TEpiCustomChangeEventType(EventType) <> ecceDestroy then exit;

  FCompareField := nil;
  Initiator.UnRegisterOnChangeHook(@FieldHook);
  DoChange(eegCustomBase, Word(ecceReferenceDestroyed), Initiator);
end;

function TEpiComparison.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase
  ): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest);
  with TEpiComparison(Result) do
  begin
    if (not Assigned(OwnerField.DataFile)) or
       (OwnerField.DataFile = Self.OwnerField.DataFile)
    then
      CompareField := Self.FCompareField
    else
      CompareField := GetOwnerField.DataFile.Fields.FieldByName[Self.FCompareField.Name];
    FCompareType := Self.FCompareType;
  end;
end;

function TEpiComparison.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);

  SaveDomAttr(Result, rsCompareTo, CompareField.Name);
  SaveDomAttrEnum(Result, rsType, CompareType, TypeInfo(TEpiComparisonType));
end;

constructor TEpiComparison.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  FCompareField := nil;
  FCompareType := fcLT;
end;

destructor TEpiComparison.Destroy;
begin
  CompareField := nil;
  inherited Destroy;
end;

function TEpiComparison.XMLName: string;
begin
  Result := rsCompare;
end;

procedure TEpiComparison.LoadFromXml(Root: TDOMNode);
var
  CompareId: String;
begin
  inherited LoadFromXml(Root);
  CompareId := LoadAttrString(Root, rsCompareTo);
  if Assigned(OwnerField) then
    CompareField := OwnerField.DataFile.Fields.FieldByName[CompareId];
  if not Assigned(CompareField) then
    LinkMap.AddLink(ltCompare, Self, CompareId);

  CompareType := TEpiComparisonType(LoadAttrEnum(Root, rsType, TypeInfo(TEpiComparisonType)));
end;

function TEpiComparison.SaveToXml(Content: String; Lvl: integer): string;
begin
  Result := inherited SaveToXml(Content, Lvl);
end;

function TEpiComparison.SaveAttributesToXml: string;
begin
  Result := inherited SaveAttributesToXml +
    SaveAttr(rsCompareTo, CompareField.Name) +
    SaveAttrEnum(rsType, Integer(CompareType), TypeInfo(TEpiComparisonType));
end;

procedure TEpiComparison.Assign(const AEpiCustomBase: TEpiCustomBase);
var
  OrgComparison: TEpiComparison absolute AEpiCustomBase;
begin
  inherited Assign(AEpiCustomBase);
  FCompareField := OrgComparison.CompareField;
  FCompareType  := OrgComparison.CompareType;
end;

function ComparisonTypeToString(ct: TEpiComparisonType): string;
begin
  case ct of
    fcEq:  result := '=';
    fcNEq: result := '<>';
    fcLT:  result := '<';
    fcLEq: result := '<=';
    fcGEq: result := '>=';
    fcGT:  result := '>';
  end;
end;

