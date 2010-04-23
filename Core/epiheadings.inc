{%MainUnit epidatafiles.pas}

{ TEpiHeadings }

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

function TEpiHeadings.NewHeading: TEpiHeading;
begin
  result := TEpiHeading.Create(Self);
  result.Id := GetUniqueItemId(TEpiHeading);
  AddItem(result);
end;

{ TEpiHeading }

constructor TEpiHeading.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  FCaption := TEpiTranslatedText.Create(Self, rsCaption);
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



