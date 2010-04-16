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

class function TEpiDataFiles.XMLName: string;
begin
  Result := rsDataFiles;
end;

function TEpiDataFiles.ScrambleXml: boolean;
begin
  Result := Settings.Scrambled;
end;

function TEpiDataFiles.SaveToXml(Content: String; Lvl: integer): string;
begin
  if Count = 0 then exit;
  Result := inherited SaveToXml(Content, Lvl);
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
    CheckNode(Node, rsDataFile);

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
  result := TEpiField(Fields[Index]);
end;

function TEpiDataFile.GetSection(Index: integer): TEpiSection;
begin
  result := TEpiSection(Sections[Index]);
end;

function TEpiDataFile.GetHeading(Index: integer): TEpiHeading;
begin
  result := TEpiHeading(Headings[Index]);
end;

function TEpiDataFile.GetValueLabelSet(Index: integer): TEpiValueLabelSet;
begin
  result := TEpiValueLabelSet(FValueLabels[Index]);
end;

procedure TEpiDataFile.LocalEventHook(Sender: TObject;
  EventGrp: TEpiEventGroup; EventType: Word; Data: pointer);
begin
  case EventGrp of
    eegCustomBase:
      begin
        case TEpiCustomChangeEventType(EventType) of
          // On AddItem "data" is a pointer to the added item.
          ecceAddItem:
            begin
              // Here we make sure that fields added to any section is also added
              // to the global list of fields.
              if (Sender is TEpiFields) then
              begin
                Fields.AddItem(TEpiField(Data));
                TEpiField(Data).Size := Size;
              end;
              if (Sender is TEpiHeadings) then
                Headings.AddItem(TEpiHeading(Data));

              // If more sections are created, we need to add a hook to this also.
              if (Sender is TEpiSections) then
              with TEpiSection(Data) do
              begin
                Fields.RegisterOnChangeHook(@LocalEventHook, true);
                Headings.RegisterOnChangeHook(@LocalEventHook, true);
              end;
            end;

          // On DelIetem "data" is a pointer to the now deleted item.
          ecceDelItem:
            begin
              if (Sender is TEpiFields) then
                Fields.RemoveItem(TEpiField(Data));
              if (Sender is TEpiHeadings) then
                Headings.RemoveItem(TEpiHeading(Data));

              // If sections are removed, we need to delete the hook
              // in case the section lives on elsewhere.
              if (Sender is TEpiSections) then
              with TEpiSection(Data) do
              begin
                Fields.UnRegisterOnChangeHook(@LocalEventHook);
                Headings.UnRegisterOnChangeHook(@LocalEventHook);
              end;
            end;
        end;
      end;
  end;
end;

function TEpiDataFile.GetSize: Integer;
begin
  result := FRecordStatus.Size;
end;

procedure TEpiDataFile.SetSize(const AValue: Integer);
var
  i: Integer;
begin
  if AValue = Size then exit;

  FRecordStatus.Size := AValue;

  for i := 0 to Fields.Count - 1 do
    Fields[i].Size := AValue;
end;

constructor TEpiDataFile.Create(AOwner: TEpiCustomBase; const Size: integer);
begin
  inherited Create(AOwner);

  // The "all" containers.
  FAllFields := TEpiFields.Create(Self);
  FAllHeadings := TEpiHeadings.Create(Self);

  // Sections
  FSections := TEpiSections.Create(Self);
  FSections.ItemOwner := true;
  FSections.RegisterOnChangeHook(@LocalEventHook, true);
  // The special main section...
  FMainSection := NewSection;

  FRecordStatus := TEpiField.CreateField(Self, ftInteger);
  FRecordStatus.Size := Size;
end;

destructor TEpiDataFile.Destroy;
begin
  FRecordStatus.Free;
  FMainSection.Free;
  FSections.Free;
  FAllFields.Free;
  FAllHeadings.Free;
  inherited Destroy;
end;

class function TEpiDataFile.XMLName: string;
begin
  Result := rsDataFile;
end;

function TEpiDataFile.SaveToXml(Content: String; Lvl: integer): string;
begin
  Content :=
  //  ValueLabels.SaveToXml('', Lvl + 1);
  //  ScreenProperties.SaveToXml('', Lvl + 1);
    Sections.SaveToXml('', Lvl + 1) +
//    SaveRecord(Lvl + 1);
    '';
  Result := inherited SaveToXml(Content, Lvl);
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
  result := MainSection.NewField(FieldType);
end;

function TEpiDataFile.NewSection: TEpiSection;
begin
  result := Sections.NewSection;
end;

