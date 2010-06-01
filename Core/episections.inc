
{%MainUnit epidatafiles.pas}

{ TEpiSections }

function TEpiSections.GetSection(Index: integer): TEpiSection;
begin
  result := TEpiSection(Items[Index]);
end;

constructor TEpiSections.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
end;

destructor TEpiSections.Destroy;
begin
  inherited Destroy;
end;

function TEpiSections.XMLName: string;
begin
  result := rsSections;
end;

procedure TEpiSections.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMNode;
  NSection: TEpiSection;
begin
  // Root = <Sections>
  if not Assigned(Root) then exit;

  Node := Root.FirstChild;
  while Assigned(Node) do
  begin
    CheckNode(Node, rsSection);

    NSection := NewSection;
    NSection.LoadFromXml(Node);

    Node := Node.NextSibling;
  end;
end;

function TEpiSections.NewSection: TEpiSection;
begin
  result := TEpiSection(NewItem(TEpiSection));
end;

{ TEpiSection }

function TEpiSection.GetField(Index: integer): TEpiField;
begin
  result := TEpiField(Fields[Index]);
end;

function TEpiSection.GetHeading(Index: integer): TEpiHeading;
begin
  result := TEpiHeading(Headings[Index]);
end;

procedure TEpiSection.SetHeight(const AValue: integer);
var
  Val: LongInt;
begin
  if FHeight = AValue then exit;
  Val := FHeight;
  FHeight := AValue;
  DoChange(eegSections, Word(esceHeight), @Val);
end;

procedure TEpiSection.SetWidth(const AValue: integer);
var
  Val: LongInt;
begin
  if FWidth = AValue then exit;
  Val := FWidth;
  FWidth := AValue;
  DoChange(eegSections, Word(esceWidth), @Val);
end;

class function TEpiSection.IdString: string;
begin
  Result := 'section_id_'
end;

constructor TEpiSection.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  FFields := TEpiFields.Create(self);
  FFields.ItemOwner := true;

  FHeadings := TEpiHeadings.Create(self);
  FHeadings.ItemOwner := true;

  FGroups := TEpiGroups.Create(Self);

  RegisterClasses([Fields, Headings]);
end;

destructor TEpiSection.Destroy;
begin
  FGroups.Free;
  FHeadings.Free;
  FFields.Free;
  inherited Destroy;
end;

function TEpiSection.XMLName: string;
begin
  Result := rsSection;
end;

function TEpiSection.SaveToXml(Content: String; Lvl: integer): string;
var
  i: Integer;
begin
  Content := '';
  For i := 0 to Groups.Count - 1 do
    Content += ',' + Groups[i].Id;
  if Length(Content) > 0 then
  begin
    Delete(Content, 1, 1);
    Content := SaveNode(Lvl + 1, rsGroupIdList, Content);
  end;
  Content :=
    SaveNode(Lvl + 1, rsWidth, Width) +
    SaveNode(Lvl + 1, rsHeight, Height) +
    Content;
  Result := inherited SaveToXml(Content, Lvl);
end;

procedure TEpiSection.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMNode;
  GroupList: TStrings;
  i: Integer;
begin
  // Root = <Section>
  inherited LoadFromXml(Root);

  Width := LoadNodeInt(Root, rsWidth);
  Height := LoadNodeInt(Root, rsHeight);

  GroupList := nil;
  if LoadNode(Node, Root, rsGroupIdList, false) then
  begin
    SplitString(LoadNodeString(Root, rsGroupIdList), GroupList, [',']);
    for i := 0 to GroupList.Count - 1 do
      Groups.AddItem(TEpiDocument(RootOwner).Admin.Groups.GetItemById(GroupList[i]));
  end;

  if LoadNode(Node, Root, rsFields, false) then
    Fields.LoadFromXml(Node);

  if LoadNode(Node, Root, rsHeadings, false) then
    Headings.LoadFromXml(Node);
end;

function TEpiSection.NewField(FieldType: TEpiFieldType): TEpiField;
begin
  result := Fields.NewField(FieldType);
end;

function TEpiSection.NewHeading: TEpiHeading;
begin
  Result := Headings.NewHeading;
end;
