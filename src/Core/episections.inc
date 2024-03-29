
{%MainUnit epidatafiles.pas}

{ TEpiSections }

function TEpiSections.GetSection(Index: integer): TEpiSection;
begin
  result := TEpiSection(Items[Index]);
end;

function TEpiSections.Prefix: string;
begin
  Result := 'Section';
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

function TEpiSections.ItemClass: TEpiCustomItemClass;
begin
  result := TEpiSection;
end;

function TEpiSections.NewSection: TEpiSection;
begin
  result := TEpiSection(NewItem(TEpiSection));
end;

function TEpiSections.GetSectionByName(const AName: string): TEpiSection;
begin
  result := TEpiSection(GetItemByName(AName));
end;

function TEpiSections.GetUniqueItemName(AClass: TEpiCustomItemClass): string;
var
  i: Integer;
begin
  // Always start at 1, since number of sections is normally low
  // AND the MAIN sections interferes with "normal" naming of sections.
  i := 1;
  repeat
    result := DoPrefix + IntToStr(i);
    Inc(i);
  until ValidateRename(result, false);
end;

{ TEpiSection }

function TEpiSection.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);

  SaveDomAttr(Result, rsWidth, Width);
  SaveDomAttr(Result, rsHeight, Height);
end;

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
//  FFields.RegisterOnChangeHook(@ItemsHook, true);

  FHeadings := TEpiHeadings.Create(self);
  FHeadings.ItemOwner := true;
//  FHeadings.RegisterOnChangeHook(@ItemsHook, true);

  FAllControls := TAllItemsList.Create(self);
  FAllControls.ItemOwner := false;

  RegisterClasses([Caption, Fields, Headings]);
end;

destructor TEpiSection.Destroy;
begin
  FAllControls.Destroy;
//  FHeadings.UnRegisterOnChangeHook(@ItemsHook);
  FHeadings.Free;
//  FFields.UnRegisterOnChangeHook(@ItemsHook);
  FFields.Free;
  FCaption.Free;
  inherited Destroy;
end;

function TEpiSection.XMLName: string;
begin
  Result := rsSection;
end;

procedure TEpiSection.LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap
  );
var
  Node: TDOMNode;
  GroupList: TStrings;
  i: Integer;
begin
  // Root = <Section>
  inherited LoadFromXml(Root, ReferenceMap);

  // If no name present, TEpiTranslatedText will take care of it.
  Caption.LoadFromXml(Root, ReferenceMap);

  Width := LoadAttrInt(Root, rsWidth);
  Height := LoadAttrInt(Root, rsHeight);

  if LoadNode(Node, Root, rsFields, false) then
    Fields.LoadFromXml(Node, ReferenceMap);

  if LoadNode(Node, Root, rsHeadings, false) then
    Headings.LoadFromXml(Node, ReferenceMap);
end;

function TEpiSection.NewField(FieldType: TEpiFieldType): TEpiField;
begin
  result := Fields.NewField(FieldType);
end;

function TEpiSection.NewHeading: TEpiHeading;
begin
  Result := Headings.NewHeading;
end;

function TEpiSection.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
  ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);
  with TEpiSection(Result) do
  begin
    FWidth := Self.FWidth;
    FHeight := Self.FHeight;
  end;
end;
