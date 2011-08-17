{%MainUnit epidatafiles.pas}

{ TEpiHeadings }

function TEpiHeadings.NamePrefix: string;
begin
  Result := 'H';
end;

constructor TEpiHeadings.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
end;

destructor TEpiHeadings.Destroy;
begin
  inherited Destroy;
end;

function TEpiHeadings.XMLName: string;
begin
  Result := rsHeadings;
end;

procedure TEpiHeadings.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMNode;
  NHeading: TEpiHeading;
begin
  // Root = <Headings>
  if not Assigned(Root) then exit;

  Node := Root.FirstChild;
  while Assigned(Node) do
  begin
    CheckNode(Node, rsHeading);

    NHeading := NewHeading;
    NHeading.LoadFromXml(Node);

    Node := Node.NextSibling;
  end;
end;

function TEpiHeadings.NewHeading: TEpiHeading;
begin
  result := TEpiHeading(NewItem(TEpiHeading));
end;

function TEpiHeadings.GetHeadingByName(const AName: string): TEpiHeading;
begin
  result := TEpiHeading(GetItemByName(AName));
end;

{ TEpiHeading }

function TEpiHeading.GetSection: TEpiSection;
var
  AOwner: TEpiCustomBase;
begin
  AOwner := Owner;
  while Assigned(AOwner) and (not (AOwner is TEpiSection)) do
    AOwner := AOwner.Owner;
  result := TEpiSection(AOwner);
end;

constructor TEpiHeading.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  FCaption := TEpiTranslatedTextWrapper.Create(Self, rsCaption, rsText);
  RegisterClasses([FCaption]);
end;

destructor TEpiHeading.Destroy;
begin
  FCaption.Free;
  inherited Destroy;
end;

class function TEpiHeading.IdString: string;
begin
  Result := 'heading_id_';
end;

function TEpiHeading.XMLName: string;
begin
  Result := rsHeading;
end;

procedure TEpiHeading.LoadFromXml(Root: TDOMNode);
begin
  inherited LoadFromXml(Root);
  FCaption.LoadFromXml(Root);
end;



