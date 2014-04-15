{%MainUnit epidatafiles.pas}

{ TEpiRelates }

function TEpiRelates.GetRelate(Index: Integer): TEpiRelate;
begin
  result := TEpiRelate(Items[Index]);
end;

function TEpiRelates.GetRelateFromValue(Value: EpiString): TEpiRelate;
var
  R: TEpiRelate;
  i: Integer;
begin
  result := nil;
  for i := 0 to Count - 1 do
  begin
    R := Relate[i];

    if (R.RelateValue = Value) or
       (R.RelateValue = TEpiJump.DefaultOnAnyValue)
    then
      Exit(R);
  end;
end;

procedure TEpiRelates.DoChange(const Initiator: TEpiCustomBase;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  inherited DoChange(Initiator, EventGroup, EventType, Data);

  // Not my child!
  if IndexOf(TEpiCustomItem(Initiator)) < 0  then exit;

  if (EventGroup <> eegCustomBase) then exit;
  if (EventType  <> Word(ecceReferenceDestroyed)) then exit;

  RemoveItem(TEpiCustomItem(Initiator));
  Initiator.Free;
end;

constructor TEpiRelates.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
end;

function TEpiRelates.XMLName: string;
begin
  Result := rsRelates;
end;

function TEpiRelates.ItemClass: TEpiCustomItemClass;
begin
  Result := TEpiRelate;
end;

function TEpiRelates.NewRelate: TEpiRelate;
begin
  result := TEpiRelate(NewItem());
end;

{ TEpiRelate }

procedure TEpiRelate.RelationHook(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
var
  i: Integer;
begin
  if (Initiator <> DetailRelation) then exit;
  if (EventGroup <> eegCustomBase) then exit;
  if (EventType <> Word(ecceDestroy)) then exit;

  Data := DetailRelation;
  DetailRelation := nil;
  DoChange(eegCustomBase, Word(ecceReferenceDestroyed), Data);
end;

procedure TEpiRelate.SetDetailRelation(AValue: TEpiCustomItem);
var
  Val: TEpiCustomItem;
begin
  if Assigned(AValue) and
     (not AValue.InheritsFrom(TEpiDetailRelation))
  then
    Raise Exception.Create('TEpiRelate.SetDetailRelation only acception object of type TEpiDetailRelation!');

  if FDetailRelation = AValue then Exit;
  UpdateRelationHook(FDetailRelation, AValue);

  Val := FDetailRelation;
  FDetailRelation := AValue;
//  DoChange(eegCustomBase, erceDetailRelation, @Val);
end;

procedure TEpiRelate.SetRelateValue(AValue: EpiString);
var
  Val: EpiString;
begin
  if FRelateValue = AValue then Exit;
  Val := FRelateValue;
  FRelateValue := AValue;
 // DoChange(eeg, erceRelateValue, @Val);
end;

procedure TEpiRelate.UpdateRelationHook(const Old, New: TEpiCustomItem);
begin
  if Assigned(Old) then
    Old.UnRegisterOnChangeHook(@RelationHook);

  if Assigned(New) then
    New.RegisterOnChangeHook(@RelationHook, true);
end;

procedure TEpiRelate.FixupReferences(EpiClassType: TEpiCustomBaseClass;
  ReferenceType: Byte; const ReferenceId: string);
var
  Obj: TEpiCustomBase;
  Doc: TEpiDocument;
begin
  if (EpiClassType = TEpiRelate)
  then
    begin
      Obj := RootOwner;
      if not (Obj is TEpiDocument) then exit;
      Doc := TEpiDocument(Obj);

      case ReferenceType of

        0: // DetailRelation
          DetailRelation := TEpiDetailRelation(Doc.Relations.GetItemByName(ReferenceId));
      end;

    end
  else
    inherited FixupReferences(EpiClassType, ReferenceType, ReferenceId);
end;

function TEpiRelate.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
  ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);

  TEpiRelate(Result).RelateValue := RelateValue;

  if Assigned(DetailRelation) then
    ReferenceMap.AddFixupReference(Result, TEpiRelate, 0, DetailRelation.Name);
end;

function TEpiRelate.WriteNameToXml: boolean;
begin
  Result := false;
end;

constructor TEpiRelate.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
end;

destructor TEpiRelate.Destroy;
begin
  UpdateRelationHook(DetailRelation, nil);
  inherited Destroy;
end;

procedure TEpiRelate.LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap
  );
var
  DetailRelationId: EpiString;
begin
  inherited LoadFromXml(Root, ReferenceMap);

  RelateValue := LoadAttrString(Root, rsRelateValue);
  ReferenceMap.AddFixupReference(Self, TEpiRelate, 0, LoadAttrString(Root, rsRelationRef));
end;

function TEpiRelate.SaveAttributesToXml: string;
begin
  Result := inherited SaveAttributesToXml +
    SaveAttr(rsRelationRef, DetailRelation.Name) +
    SaveAttr(rsRelateValue, RelateValue);
end;

function TEpiRelate.XMLName: string;
begin
  Result := rsRelate;
end;

procedure TEpiRelate.Assign(const AEpiCustomBase: TEpiCustomBase);
begin
  inherited Assign(AEpiCustomBase);
end;

