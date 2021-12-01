
{%MainUnit epidatafiles.pas}

{ TEpiValueRelates }

function TEpiValueRelates.GetRelate(Index: Integer): TEpiValueRelate;
begin
  result := TEpiValueRelate(inherited Relate[Index]);
end;

function TEpiValueRelates.GetRelateFromValue(Value: EpiString): TEpiValueRelate;
var
  R: TEpiValueRelate;
  i: Integer;
begin
  result := nil;
  for i := 0 to Count - 1 do
  begin
    R := Relate[i];

    if (R.RelateValue = Value) or
       (R.RelateValue = '*')
    then
      Exit(R);
  end;
end;

function TEpiValueRelates.ItemClass: TEpiCustomItemClass;
begin
  Result := TEpiValueRelate;
end;

function TEpiValueRelates.NewRelate: TEpiValueRelate;
begin
  result := TEpiValueRelate(inherited NewRelate);
end;

function TEpiValueRelates.XMLName: string;
begin
  Result := rsValueRelates;
end;

{ TEpiValueRelate }

procedure TEpiValueRelate.SetRelateValue(AValue: EpiString);
var
  Val: EpiString;
begin
  if FRelateValue = AValue then Exit;
  Val := FRelateValue;
  FRelateValue := AValue;
 // DoChange(eeg, erceRelateValue, @Val);
end;

function TEpiValueRelate.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
  ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);
  TEpiValueRelate(Result).RelateValue := RelateValue;
end;

function TEpiValueRelate.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);
  SaveDomAttr(Result, rsRelateValue, RelateValue);
end;

constructor TEpiValueRelate.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);

  FRelateValue := TEpiStringField.DefaultMissing;
end;

procedure TEpiValueRelate.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
begin
  inherited LoadFromXml(Root, ReferenceMap);
  RelateValue := LoadAttrString(Root, rsRelateValue);
end;

function TEpiValueRelate.XMLName: string;
begin
  Result := rsValueRelate;
end;

