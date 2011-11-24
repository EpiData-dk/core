
{%MainUnit epidatafiles.pas}

{ TEpiSections }

function TEpiSections.GetSection(Index: integer): TEpiSection;
begin
  result := TEpiSection(Items[Index]);
end;

function TEpiSections.Prefix: string;
begin
  Result := 'S';
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

function TEpiSections.GetSectionByName(const AName: string): TEpiSection;
begin
  result := TEpiSection(GetItemByName(AName));
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

constructor TEpiSection.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  FCaption := TEpiTranslatedTextWrapper.Create(Self, rsCaption, rsText);

  FFields := TEpiFields.Create(self);
  FFields.ItemOwner := true;

  FHeadings := TEpiHeadings.Create(self);
  FHeadings.ItemOwner := true;

  FGroups := TEpiGroups.Create(Self);

  RegisterClasses([Caption, Fields, Headings]);
end;

destructor TEpiSection.Destroy;
begin
  FGroups.Free;
  FHeadings.Free;
  FFields.Free;
  FCaption.Free;
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
{  Content := '';
  For i := 0 to Groups.Count - 1 do
    Content += ',' + Groups[i].Name;
  if Length(Content) > 0 then
  begin
    Delete(Content, 1, 1);
    Content := SaveNode(Lvl + 1, rsGroupIdList, Content);
  end;    }
  Result := inherited SaveToXml(Content, Lvl);
end;

function TEpiSection.SaveAttributesToXml: string;
begin
  Result :=
    inherited SaveAttributesToXml +
    SaveAttr(rsWidth, Width) +
    SaveAttr(rsHeight, Height);
end;

procedure TEpiSection.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMNode;
  GroupList: TStrings;
  i: Integer;
begin
  // Root = <Section>
  inherited LoadFromXml(Root);

  // If no name present, TEpiTranslatedText will take care of it.
  Caption.LoadFromXml(Root);

  Width := LoadAttrInt(Root, rsWidth);
  Height := LoadAttrInt(Root, rsHeight);

{  GroupList := nil;
  if LoadNode(Node, Root, rsGroupIdList, false) then
  begin
    SplitString(LoadNodeString(Root, rsGroupIdList), GroupList, [',']);
    for i := 0 to GroupList.Count - 1 do
      Groups.AddItem(TEpiDocument(RootOwner).Admin.Groups.GetItemById(GroupList[i]));
  end;    }

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

procedure TEpiSection.Assign(const AEpiCustomBase: TEpiCustomBase);
var
  OrgSection: TEpiSection absolute AEpiCustomBase;
begin
  BeginUpdate;
  inherited Assign(AEpiCustomBase);
//  FGroups.Assign(OrgSection.FGroups);
  Height := OrgSection.Height;
  Width  := OrgSection.Width;
  EndUpdate;
end;
