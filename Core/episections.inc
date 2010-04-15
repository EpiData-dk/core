
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

procedure TEpiSections.SaveToStream(St: TStream; Lvl: integer);
begin
  inherited SaveToStream(St, Lvl);
end;

procedure TEpiSections.LoadFromXml(Root: TDOMNode);
begin
  inherited LoadFromXml(Root);
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
end;

destructor TEpiSection.Destroy;
begin
  inherited Destroy;
end;

procedure TEpiSection.SaveToStream(St: TStream; Lvl: integer);
begin
  inherited SaveToStream(St, Lvl);
end;

procedure TEpiSection.LoadFromXml(Root: TDOMNode);
begin
  inherited LoadFromXml(Root);
end;


