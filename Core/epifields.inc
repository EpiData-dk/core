
{%MainUnit epidatafiles.pas}

{ TEpiFields }

function TEpiFields.GetDataFile: TEpiDataFile;
begin
  result := TEpiDataFile(Owner);
end;

constructor TEpiFields.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
end;

destructor TEpiFields.Destroy;
begin
  inherited Destroy;
end;

procedure TEpiFields.SaveToStream(St: TStream; Lvl: integer);
begin
  SaveList(St, Lvl, Self, rsFields);
end;

procedure TEpiFields.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMNode;
  NField: TEpiField;
begin
  // Root = <Fields>
  Node := Root.FirstChild;
  while Assigned(Node) do
  begin
    CheckNode(Node, rsField);

    // TODO : Think about handling new fields...
    NField := DataFile.NewField(TEpiFieldType(LoadNodeInt(Node, rsType)));
    NField.LoadFromXml(Node);

    Node := Node.NextSibling;
  end;
end;

{ TEpiField }

constructor TEpiField.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
end;

destructor TEpiField.Destroy;
begin
  inherited Destroy;
end;

