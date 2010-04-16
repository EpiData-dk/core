
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

class function TEpiSections.XMLName: string;
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
  result := TEpiSection.Create(Self);
  result.Id := 'section_id_' + IntToStr(count);
  AddItem(result);
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

constructor TEpiSection.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  FFields := TEpiFields.Create(self);
  FFields.ItemOwner := true;

  FHeadings := TEpiHeadings.Create(self);
  FHeadings.ItemOwner := true;
end;

destructor TEpiSection.Destroy;
begin
  FHeadings.Free;
  FFields.Free;
  inherited Destroy;
end;

class function TEpiSection.XMLName: string;
begin
  Result := rsSection;
end;

function TEpiSection.SaveToXml(Content: String; Lvl: integer): string;
begin
  Content :=
    Fields.SaveToXml('', Lvl + 1) +
    Headings.SaveToXml('', Lvl + 1);
  Result := inherited SaveToXml(Content, Lvl);
end;

procedure TEpiSection.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMNode;
begin
  // Root = <Section>
  if not Assigned(Root) then exit;

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


