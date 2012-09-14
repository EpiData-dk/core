{%MainUnit epidatafiles.pas}

{ TEpiHeadings }

function TEpiHeadings.Prefix: string;
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

function TEpiHeadings.ItemClass: TEpiCustomItemClass;
begin
  result := TEpiHeading;
end;

function TEpiHeadings.NewHeading: TEpiHeading;
begin
  result := TEpiHeading(NewItem);
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

function TEpiHeading.XMLName: string;
begin
  Result := rsHeading;
end;

procedure TEpiHeading.LoadFromXml(Root: TDOMNode);
begin
  inherited LoadFromXml(Root);
  FCaption.LoadFromXml(Root);
end;



