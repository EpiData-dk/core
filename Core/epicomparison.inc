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

procedure TEpiComparison.FieldHook(Sender: TObject; EventGroup: TEpiEventGroup;
  EventType: Word; Data: Pointer);
var
  Field: TEpiField absolute Sender;
begin
  if Sender <> FCompareField then exit;

  if (EventGroup = eegCustomBase) and (EventType = Word(ecceDestroy)) then
    FCompareField := nil;
end;

constructor TEpiComparison.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  FCompareField := nil;
  FCompareType := fcLT;
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
    CompareField := TEpiField(OwnerField.DataFile.Fields.GetItemById(CompareId));
  if not Assigned(CompareField) then
    LinkMap.AddLink(ltCompare, Self, CompareId);

  CompareType := TEpiComparisonType(LoadAttrInt(Root, rsCompareType));
end;

function TEpiComparison.SaveToXml(Content: String; Lvl: integer): string;
begin
  Result := inherited SaveToXml(Content, Lvl);
end;

function TEpiComparison.SaveAttributesToXml: string;
begin
  Result := inherited SaveAttributesToXml +
    SaveAttr(rsCompareTo, CompareField.Id) +
    SaveAttrEnum(rsCompareType, Integer(CompareType), TypeInfo(TEpiComparisonType));
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
    fcLT:  result := '<';
    fcLEq: result := '<=';
    fcGEq: result := '>=';
    fcGT:  result := '>';
  end;
end;

