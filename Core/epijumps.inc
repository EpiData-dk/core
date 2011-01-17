{%MainUnit epidatafiles.pas}

{ TEpiJumps }

function TEpiJumps.GetJump(index: integer): TEpiJump;
begin
  result := TEpiJump(Items[Index]);
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

procedure TEpiJumps.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMNode;
  NJump: TEpiJump;
begin
  // Root = <Jumps>
  inherited LoadFromXml(Root);

  Node := Root.FirstChild;
  while Assigned(Node) do
  begin
    CheckNode(Node, rsJump);

    NJump := NewJump;
    NJump.LoadFromXml(Node);

    Node := Node.NextSibling;
  end;
end;

function TEpiJumps.NewJump: TEpiJump;
var
  ItemClass: TEpiCustomItemClass;
begin
  case TEpiField(Owner).FieldType of
    ftBoolean: ItemClass := TEpiBoolJump;
    ftInteger: ItemClass := TEpiIntJump;
    ftFloat:   ItemClass := TEpiFloatJump;
    ftString:  ItemClass := TEpiStringJump;
  end;
  Result := TEpiJump(NewItem(ItemClass));
  Result.Id := '';
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

procedure TEpiJump.SetJumpToField(const AValue: TEpiField);
begin
  if FJumpToField = AValue then exit;
  FJumpToField := AValue;
end;

procedure TEpiJump.SetResetOnJump(const AValue: boolean);
begin
  if FResetOnJump = AValue then exit;
  FResetOnJump := AValue;
end;

constructor TEpiJump.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  FJumpToField := nil;
  FResetOnJump := false;
end;

class function TEpiJump.IdString: string;
begin
  Result := 'jump_id_';
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
  FieldId := LoadAttrString(Root, rsJumpTo);
  JumpToField := TEpiField(GetDataFile(Self).Fields.GetItemById(FieldId));
end;

function TEpiJump.SaveAttributesToXml: string;
begin
  Result := inherited SaveAttributesToXml +
    Format(' %s="%s" %s="%s"',
           [rsJumpOn, GetJumpValueAsString,
            rsJumpTo, JumpToField.Id]);
  if ResetOnJump then
    Result := Result + Format(' %s="%s"', [rsJumpReset, GetResetValueAsString]);
end;

{ TEpiBoolJump }

procedure TEpiBoolJump.SetJumpValue(const AValue: EpiBool);
begin
  if FJumpValue = AValue then exit;
  FJumpValue := AValue;
end;

procedure TEpiBoolJump.SetResetValue(const AValue: EpiBool);
begin
  if FResetValue = AValue then exit;
  FResetValue := AValue;
end;

function TEpiBoolJump.GetJumpValueAsString: string;
begin
  Result := IntToStr(JumpValue);
end;

function TEpiBoolJump.GetResetValueAsString: string;
begin
  Result := IntToStr(ResetValue);
end;

procedure TEpiBoolJump.LoadFromXml(Root: TDOMNode);
var
  Attr: TDOMAttr;
begin
  inherited LoadFromXml(Root);
  JumpValue := LoadAttrInt(Root, rsJumpReset);
  if LoadAttr(Attr, Root, rsJumpReset, false) then
  begin
    ResetOnJump := true;
    ResetValue  := LoadAttrInt(Root, rsJumpReset);
  end;
end;

{ TEpiIntJump }

procedure TEpiIntJump.SetJumpValue(const AValue: EpiInteger);
begin
  if FJumpValue = AValue then exit;
  FJumpValue := AValue;
end;

procedure TEpiIntJump.SetResetValue(const AValue: EpiInteger);
begin
  if FResetValue = AValue then exit;
  FResetValue := AValue;
end;

function TEpiIntJump.GetJumpValueAsString: string;
begin
  Result := IntToStr(JumpValue);
end;

function TEpiIntJump.GetResetValueAsString: string;
begin
  Result := IntToStr(ResetValue);
end;

procedure TEpiIntJump.LoadFromXml(Root: TDOMNode);
var
  Attr: TDOMAttr;
begin
  inherited LoadFromXml(Root);
  JumpValue := LoadAttrInt(Root, rsJumpOn);
  if LoadAttr(Attr, Root, rsJumpReset, false) then
  begin
    ResetOnJump := true;
    ResetValue  := LoadAttrInt(Root, rsJumpReset);
  end;
end;

{ TEpiFloatJump }

procedure TEpiFloatJump.SetJumpValue(const AValue: EpiFloat);
begin
  if FJumpValue = AValue then exit;
  FJumpValue := AValue;
end;

procedure TEpiFloatJump.SetResetValue(const AValue: EpiFloat);
begin
  if FResetValue = AValue then exit;
  FResetValue := AValue;
end;

function TEpiFloatJump.GetJumpValueAsString: string;
begin
  Result := FloatToStr(JumpValue);
end;

function TEpiFloatJump.GetResetValueAsString: string;
begin
  Result := FloatToStr(ResetValue);
end;

procedure TEpiFloatJump.LoadFromXml(Root: TDOMNode);
var
  Attr: TDOMAttr;
begin
  inherited LoadFromXml(Root);
  JumpValue := LoadAttrFloat(Root, rsJumpOn);
  if LoadAttr(Attr, Root, rsJumpReset, false) then
  begin
    ResetOnJump := true;
    ResetValue  := LoadAttrFloat(Root, rsJumpReset);
  end;
end;

{ TEpiStringJump }

procedure TEpiStringJump.SetJumpValue(const AValue: EpiString);
begin
  if FJumpValue = AValue then exit;
  FJumpValue := AValue;
end;

procedure TEpiStringJump.SetResetValue(const AValue: EpiString);
begin
  if FResetValue = AValue then exit;
  FResetValue := AValue;
end;

function TEpiStringJump.GetJumpValueAsString: string;
begin
  Result := JumpValue;
end;

function TEpiStringJump.GetResetValueAsString: string;
begin
  Result := ResetValue;
end;

procedure TEpiStringJump.LoadFromXml(Root: TDOMNode);
var
  Attr: TDOMAttr;
begin
  inherited LoadFromXml(Root);
  JumpValue := LoadAttrString(Root, rsJumpOn);
  if LoadAttr(Attr, Root, rsJumpReset, false) then
  begin
    ResetOnJump := true;
    ResetValue  := LoadAttrString(Root, rsJumpReset);
  end;
end;

