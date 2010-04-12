{%MainUnit epidatafiles.pas}

{ TEpiDataFiles }

const
  rsEpiDataFiles = 'EpiDataFiles';
  rsEpiDataFile = 'EpiDataFile';

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

constructor TEpiDataFile.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
end;

destructor TEpiDataFile.Destroy;
begin
  inherited Destroy;
end;

