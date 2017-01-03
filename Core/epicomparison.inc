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

procedure TEpiComparison.FixupReferences(EpiClassType: TEpiCustomBaseClass;
  ReferenceType: Byte; const ReferenceId: string);
begin
  if (EpiClassType = TEpiComparison)
  then
    begin
      if not Assigned(GetOwnerField) then exit;

      case ReferenceType of
        0: // CompareField
          CompareField := GetOwnerField.DataFile.Fields.FieldByName[ReferenceId];
      end;

    end
  else
    inherited FixupReferences(EpiClassType, ReferenceType, ReferenceId);
end;

function TEpiComparison.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
  ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);

  with TEpiComparison(Result) do
    FCompareType := Self.FCompareType;

  if Assigned(CompareField) then
    ReferenceMap.AddFixupReference(Result, TEpiComparison, 0, CompareField.Name);
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

procedure TEpiComparison.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
var
  CompareId: String;
begin
  inherited LoadFromXml(Root, ReferenceMap);

  CompareType := TEpiComparisonType(LoadAttrEnum(Root, rsType, TypeInfo(TEpiComparisonType)));
  ReferenceMap.AddFixupReference(Self, TEpiComparison, 0, LoadAttrString(Root, rsCompareTo));
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

