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

function TEpiHeadings.NewHeading: TEpiHeading;
begin
  result := TEpiHeading.Create(Self);
  result.Id := 'heading_id_' + IntToStr(Count);
  AddItem(result);
end;

{ TEpiHeading }

constructor TEpiHeading.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
end;

destructor TEpiHeading.Destroy;
begin
  inherited Destroy;
end;



