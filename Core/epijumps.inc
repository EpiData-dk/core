{%MainUnit epidatafiles.pas}

{ TEpiJumps }

function TEpiJumps.GetJump(index: integer): TEpiJump;
begin
  result := TEpiJump(Items[Index]);
end;

function TEpiJumps.GetJumpFromValue(const Value: string): TEpiJump;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to Count - 1 do
    if Jump[i].JumpValueAsString = Value then
      Exit(Jump[i]);
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
  Ft: TEpiFieldType;
begin
  Ft := TEpiField(Owner).FieldType;

  if Ft in AutoFieldTypes then
    Raise Exception.CreateFmt(
            'Field "%s" does not support jumps! (Auto Type)' + LineEnding +
            'Please Contact EpiData',
            [TEpiCustomNamedItem(Owner).Name]);

  if Ft in BoolFieldTypes   then ItemClass := TEpiBoolJump;
  if Ft in IntFieldTypes    then ItemClass := TEpiIntJump;
  if Ft in FloatFieldTypes  then ItemClass := TEpiFloatJump;
  if Ft in StringFieldTypes then ItemClass := TEpiStringJump;

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

procedure TEpiJump.SetJumpType(const AValue: TEpiJumpType);
begin
  if FJumpType = AValue then exit;
  FJumpType := AValue;
end;

procedure TEpiJump.SetResetType(const AValue: TEpiJumpResetType);
begin
  if FResetType = AValue then exit;
  FResetType := AValue;
end;

constructor TEpiJump.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  FJumpType := jtToField;
  FResetType := jrLeaveAsIs;
  FJumpToField := nil;
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
  JumpType := TEpiJumpType(LoadAttrInt(Root, rsJumpType));
  ResetType := TEpiJumpResetType(LoadAttrInt(Root, rsJumpReset));
  if JumpType = jtToField then
  begin
    FieldId := LoadAttrString(Root, rsJumpTo);
    JumpToField := TEpiField(GetDataFile(Self).Fields.GetItemById(FieldId));

    // Since (possibly) all jumps to fields are "forward" declared then collect them into
    // a batch and process the list after all fields are loaded.
    if not assigned(JumpToField) then
      LinkMap.AddLink(ltJump, Self, FieldId);
  end;
end;

function TEpiJump.SaveAttributesToXml: string;
begin
  Result := inherited SaveAttributesToXml +
    Format(' %s="%s" %s="%d" %s="%d"',
           [rsJumpOn, GetJumpValueAsString,
            rsJumpType, JumpType,
            rsJumpReset, ResetType]);
  if JumpType = jtToField then
    Result := Result + Format(' %s="%s"', [rsJumpTo, JumpToField.Id]);
end;

procedure TEpiJump.Assign(const AEpiCustomBase: TEpiCustomBase);
var
  OrgJump: TEpiJump absolute AEpiCustomBase;
begin
  inherited Assign(AEpiCustomBase);

  BeginUpdate;
  JumpToField := OrgJump.JumpToField;
  JumpType    := OrgJump.JumpType;
  ResetType   := OrgJump.ResetType;
  EndUpdate;
end;

{ TEpiBoolJump }

procedure TEpiBoolJump.SetJumpValue(const AValue: EpiBool);
begin
  if FJumpValue = AValue then exit;
  FJumpValue := AValue;
end;

function TEpiBoolJump.GetJumpValueAsString: string;
begin
  Result := IntToStr(JumpValue);
end;

procedure TEpiBoolJump.LoadFromXml(Root: TDOMNode);
var
  Attr: TDOMAttr;
begin
  inherited LoadFromXml(Root);
  JumpValue := LoadAttrInt(Root, rsJumpReset);
end;

procedure TEpiBoolJump.Assign(const AEpiCustomBase: TEpiCustomBase);
begin
  inherited Assign(AEpiCustomBase);
  JumpValue := TEpiBoolJump(AEpiCustomBase).JumpValue;
end;

{ TEpiIntJump }

procedure TEpiIntJump.SetJumpValue(const AValue: EpiInteger);
begin
  if FJumpValue = AValue then exit;
  FJumpValue := AValue;
end;

function TEpiIntJump.GetJumpValueAsString: string;
begin
  Result := IntToStr(JumpValue);
end;

procedure TEpiIntJump.LoadFromXml(Root: TDOMNode);
var
  Attr: TDOMAttr;
begin
  inherited LoadFromXml(Root);
  JumpValue := LoadAttrInt(Root, rsJumpOn);
end;

procedure TEpiIntJump.Assign(const AEpiCustomBase: TEpiCustomBase);
begin
  inherited Assign(AEpiCustomBase);
  JumpValue := TEpiIntJump(AEpiCustomBase).JumpValue;
end;

{ TEpiFloatJump }

procedure TEpiFloatJump.SetJumpValue(const AValue: EpiFloat);
begin
  if FJumpValue = AValue then exit;
  FJumpValue := AValue;
end;

function TEpiFloatJump.GetJumpValueAsString: string;
begin
  Result := FloatToStr(JumpValue);
end;

procedure TEpiFloatJump.LoadFromXml(Root: TDOMNode);
var
  Attr: TDOMAttr;
begin
  inherited LoadFromXml(Root);
  JumpValue := LoadAttrFloat(Root, rsJumpOn);
end;

procedure TEpiFloatJump.Assign(const AEpiCustomBase: TEpiCustomBase);
begin
  inherited Assign(AEpiCustomBase);
  JumpValue := TEpiFloatJump(AEpiCustomBase).JumpValue;
end;

{ TEpiStringJump }

procedure TEpiStringJump.SetJumpValue(const AValue: EpiString);
begin
  if FJumpValue = AValue then exit;
  FJumpValue := AValue;
end;

function TEpiStringJump.GetJumpValueAsString: string;
begin
  Result := JumpValue;
end;

procedure TEpiStringJump.LoadFromXml(Root: TDOMNode);
var
  Attr: TDOMAttr;
begin
  inherited LoadFromXml(Root);
  JumpValue := LoadAttrString(Root, rsJumpOn);
end;

procedure TEpiStringJump.Assign(const AEpiCustomBase: TEpiCustomBase);
begin
  inherited Assign(AEpiCustomBase);
  JumpValue := TEpiStringJump(AEpiCustomBase).JumpValue;
end;

