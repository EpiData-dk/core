{%MainUnit epidatafiles.pas}

{ TEpiDataFiles }

function TEpiDataFiles.GetDataFile(Index: integer): TEpiDataFile;
begin
  result := TEpiDataFile(Items[Index]);
end;

function TEpiDataFiles.GetSettings: TEpiSettings;
begin
  result := TEpiDocument(Owner).Settings;
end;

constructor TEpiDataFiles.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
end;

destructor TEpiDataFiles.Destroy;
begin
  inherited Destroy;
end;

procedure TEpiDataFiles.SaveToStream(St: TStream; Lvl: integer);
begin
  if Count = 0 then exit;

  SaveList(St, Lvl, Self, rsEpiDataFiles, Settings.Scrambled);
end;

procedure TEpiDataFiles.LoadFromXml(Root: TDOMNode);
var
  NewRoot: TDOMNode;
  Node: TDOMNode;
  NDf: TEpiDataFile;
begin
  // Root = <DataFiles>
  if not Assigned(Root) then exit;

  // If file is scrambles, then we first need to descramble (using master password)
  // and then read xml structure.
  if Settings.Scrambled then
    NewRoot := DeCrypt(Root)
  else
    NewRoot := Root;

  Node := NewRoot.FirstChild;
  while Assigned(Node) do
  begin
    CheckNode(Node, rsEpiDataFile);

    NDf := NewDataFile;
    NDf.LoadFromXml(Node);

    Node := Node.NextSibling;
  end;

  if Settings.Scrambled then
    NewRoot.Free;
end;

function TEpiDataFiles.NewDataFile: TEpiDataFile;
begin
  Result := TEpiDataFile.Create(Self);
  Result.Id := 'datafile_id_' + IntToStr(Count);
  AddItem(Result);
end;

{ TEpiDataFile }

function TEpiDataFile.GetField(Index: integer): TEpiField;
begin
//  result := TEpiField(Fields[Index]);
end;

function TEpiDataFile.GetSection(Index: integer): TEpiSection;
begin
//  result := TEpiSection(Sections[Index]);
end;

function TEpiDataFile.GetHeading(Index: integer): TEpiHeading;
begin
//  result := TEpiHeading(Headings[Index]);
end;

function TEpiDataFile.GetValueLabelSet(Index: integer): TEpiValueLabelSet;
begin
  result := TEpiValueLabelSet(FValueLabels[Index]);
end;

constructor TEpiDataFile.Create(AOwner: TEpiCustomBase; const Size: integer);
begin
  inherited Create(AOwner);

  // Sections
  FSections := TEpiSections.Create(Self);
  FSections.ItemOwner := true;

  FMainSection := TEpiSection.Create(Self);

  // TODO : RecordStatus on DATAFILE!
//  FRecordStatus := TEpiIntField.Create(Self);
//  FRecordStatus.Size := Size;
end;

destructor TEpiDataFile.Destroy;
begin
  FMainSection.Free;
  FSections.Free;
  inherited Destroy;
end;

// TODO : SMARTEN UP ON SAVETOSTREAM IN TEPIDATAFILE!!!
procedure TEpiDataFile.SaveToStream(St: TStream; Lvl: integer);
var
  S: String;

  function Ins(Lvl: integer): string; inline;
  begin
    result := dupestring(' ', lvl);
  end;

begin
  S :=
    Ins(Lvl)     + '<DataFile id="' + Id + '">' + LineEnding +
    Ins(Lvl + 1) + '<Name>' + Name + '</Name>' + LineEnding;
  St.Write(S[1], Length(S));

//  ValueLabels.SaveToStream(St, Lvl + 1);
//  ScreenProperties.SaveToStream(St, Lvl + 1);
//  Fields.SaveToStream(St, Lvl + 1);
//  Headings.SaveToStream(St, Lvl + 1);
//  SaveRecords(St, Lvl + 1);

  S :=
    Ins(Lvl) + '</DataFile>' + LineEnding;
  St.Write(S[1], Length(S));
end;

procedure TEpiDataFile.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMNode;
begin
  Id := TDOMElement(Root).AttribStrings['id'];
  Name := LoadNodeString(Root, rsName);

  if LoadNode(Node, Root, rsValueLabels, false) then
    ValueLabels.LoadFromXml(Node);

//  if LoadNode(Node, Root, rsScreenInfo, false) then
//    ScreenInfo.LoadFromXml(Node);

//  if LoadNode(Node, Root, rsHeadings, false) then
//    Headings.LoadFromXml(Node);

//  if LoadNode(Node, Root, rsFields, false) then
//    Fields.LoadFromXml(Node);

//  if LoadNode(Node, Root, rsRecords, false) then
//  LoadRecords  ValueLabels.LoadFromXml(Node);
end;

function TEpiDataFile.NewField(FieldType: TEpiFieldType): TEpiField;
begin
//  result := Fields.NewField(FieldType);
end;

