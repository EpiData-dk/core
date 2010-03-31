{%MainUnit epidatafile.pas}

{ TEpiTextLabels }

function TEpiTextLabels.GetCount: Cardinal;
begin
  result := FList.Count;
end;

function TEpiTextLabels.GetTextLabel(Index: integer): TEpiTextLabel;
begin
  result := TEpiTextLabel(FList[Index]);
end;

constructor TEpiTextLabels.Create(AOwner: TEpiDataFile);
begin
  FList := TFPList.Create;
  FOwner := AOwner;
end;

destructor TEpiTextLabels.Destroy;
var
  T: TEpiTextLabel;
begin
  while FList.Count > 0 do
  begin
    if Owned then
    begin
      T := TEpiTextLabel(FList.Last);
      FreeAndNil(T);
    end;
    Flist.Delete(Flist.Count - 1);
  end;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TEpiTextLabels.SaveToStream(St: TStream; Lvl: integer);
var
  i: Integer;
  S: String;
begin
  if Count = 0 then exit;

  S :=
    Ins(LvL) + '<Headings>' + LineEnding;
  St.Write(S[1], Length(S));

  for i := 0 to Count - 1 do
    TextLabel[i].SaveToStream(St, Lvl + 1);

  S :=
    Ins(LvL) + '</Headings>' + LineEnding;
  St.Write(S[1], Length(S));
end;

procedure TEpiTextLabels.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMNode;
  NewTextLabel: TEpiTextLabel;
begin
  // Root = <Headings>
  Node := Root.FirstChild;
  while Assigned(Node) do
  begin
    if Node.CompareName('Heading') <> 0 then
      ReportXmlError(EPI_XML_TAG_MISSING, 0, '', []);

    NewTextLabel := TEpiTextLabel.Create(Self);
    NewTextLabel.LoadFromXml(Node);
    Add(NewTextLabel);

    Node := Node.NextSibling;
  end;
end;

procedure TEpiTextLabels.Add(aTextLabel: TEpiTextLabel);
begin
  FList.Add(aTextLabel);

  if Owned then
    aTextLabel.FOwner := Self;

//  if ReportOnChange and Assigned(FOwner) then
//    FOwner.DoChange(dceAddText, aTextLabel);
end;

procedure TEpiTextLabels.Delete(aTextLabel: TEpiTextLabel);
var
  Idx: LongInt;
begin
  Idx := IndexOf(aTextLabel.Id);
  FList.Delete(Idx);

  if Owned then
    aTextLabel.FOwner := nil;
//  if ReportOnChange and Assigned(FOwner) then
//    FOwner.DoChange(dceRemoveText, aTextLabel);
end;

function TEpiTextLabels.TextLabelById(const aTextLabelId: string): TEpiTextLabel;
var
  i: LongInt;
begin
  Result := nil;
  i := IndexOf(aTextLabelId);
  if i >= 0 then
    result := TEpiTextLabel(FList[i]);
end;

function TEpiTextLabels.TextLabelExists(const aTextLabelId: string): boolean;
begin
  result := Assigned(TextLabelById(aTextLabelId));
end;

function TEpiTextLabels.IndexOf(const aTextLabelId: string): integer;
begin
  for result := 0 to FList.Count - 1 do
    if CompareText(TEpiTextLabel(Flist[result]).Id, aTextLabelId) = 0 then
      exit;
  result := -1;
end;

{ TEpiTextLabel }

procedure TEpiTextLabel.SetId(const AValue: string);
var
  Val: String;
begin
  if FId = AValue then exit;
  Val := FId;
  FId := AValue;
  DoChange(tceId, @Val);
end;

procedure TEpiTextLabel.SetText(const AValue: string);
var
  Val: String;
begin
  if FText = AValue then exit;
  Val := FText;
  FText := AValue;
  DoChange(tceText, @Val);
end;

procedure TEpiTextLabel.SetTextLeft(const AValue: Integer);
var
  Val: LongInt;
begin
  if FTextLeft = AValue then exit;
  Val := FTextLeft;
  FTextLeft := AValue;
  DoChange(tceLeft, @Val);
end;

procedure TEpiTextLabel.SetTextTop(const AValue: Integer);
var
  Val: LongInt;
begin
  if FTextTop = AValue then exit;
  Val := FTextTop;
  FTextTop := AValue;
  DoChange(tceTop, @Val);
end;

constructor TEpiTextLabel.Create(AOwner: TEpiTextLabels);
begin
  FOwner := AOwner;
end;

destructor TEpiTextLabel.Destroy;
begin
  inherited Destroy;
end;

procedure TEpiTextLabel.SaveToStream(St: TStream; Lvl: integer);
var
  S: String;
begin
  S :=
    Ins(Lvl) +   '<Heading id="' + Id + '">' + LineEnding +
    Ins(Lvl + 1) + '<Text>' + StringToXml(Text)  + '</Text>' + LineEnding +
    Ins(Lvl + 1) + '<Top>' + IntToStr(TextTop)  + '</Top>' + LineEnding +
    Ins(Lvl + 1) + '<Left>' + IntToStr(TextLeft)  + '</Left>' + LineEnding +
    Ins(Lvl + 1) + '<ScreenId>' + StringToXml(ScreenProp.Id)  + '</ScreenId>' + LineEnding +
    Ins(Lvl) +   '</Heading>' + LineEnding;
  St.Write(S[1], Length(S));
end;

procedure TEpiTextLabel.LoadFromXml(Root: TDOMNode);
begin
  // Root = <Heading>
  Id       := TDOMElement(Root).AttribStrings['id'];

  Text     := UTF8Encode(Root.FindNode('Text').TextContent);
  TextTop  := StrToInt(Root.FindNode('Top').TextContent);
  TextLeft := StrToInt(Root.FindNode('Left').TextContent);
  ScreenProp := FOwner.FOwner.ScreenProperties.ScreenPropertyById(
                  UTF8Encode(Root.FindNode('ScreenId').TextContent));
end;

procedure TEpiTextLabel.DoChange(EventType: TEpiTextLabelChangeEventType;
  OldValue: Pointer);
var
  i: Integer;
begin
  if FUpdateCount > 0 then exit;

  for i := 0 to FOnChangeCount - 1 do
    FOnChange[i](Self, EventType, OldValue);
end;

procedure TEpiTextLabel.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TEpiTextLabel.EndUpdate;
begin
  Dec(FUpdateCount);

  if (FUpdateCount < 0) or (FUpdateCount > 0) then
  begin
    if (FUpdateCount < 0) then
      FUpdateCount := 0;
    exit;
  end;
  DoChange(tceUpdate, nil);
end;

procedure TEpiTextLabel.RegisterOnChangeHook(Event: TEpiTextLabelChangeEvent);
begin
  Inc(FOnChangeCount);
  ReAllocMem(FOnChange, FOnChangeCount * SizeOf(TEpiTextLabelChangeEvent));
  FOnChange[FOnChangeCount-1] := Event
end;

procedure TEpiTextLabel.UnRegisterOnChangeHook(Event: TEpiTextLabelChangeEvent
  );
var
  Idx: LongInt;
begin
  Idx := 0;
  while Idx <= FOnChangeCount -1 do
  begin
    if FOnChange[Idx] = Event then
      break;
    Inc(Idx)
  end;
  if Idx = FOnChangeCount then exit;

  dec(FOnChangeCount);
  if FOnChangeCount > Idx then
    System.Move(FOnChange[Idx+1], FOnChange[Idx], (FOnChangeCount-Idx)*SizeOf(TEpiFieldChangeEvent));
  ReAllocMem(FOnChange, FOnChangeCount*SizeOf(TEpiFieldChangeEvent));
end;

