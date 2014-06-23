unit epiv_dataform_treeview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, VirtualTrees, epicustombase,
  epidatafiles, epidatafilestypes;

type

  { TDataFormTreeViewFrame }

  TDataFormTreeViewFrame = class(TFrame)
    DataFileTree: TVirtualStringTree;
    ImageList1: TImageList;
    procedure DataFileTreeGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure DataFileTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure DataFileTreeInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  private
    FDataFile: TEpiDataFile;
    FShowFieldTypes: TEpiFieldTypes;
    FShowHeadings: Boolean;
    function GetCustomItemFromNode(Const Node: PVirtualNode): TEpiCustomControlItem;
    procedure PopulateTree(Const Datafile: TEpiDataFile);
    procedure SetDataFile(AValue: TEpiDataFile);
    procedure SetShowFieldTypes(AValue: TEpiFieldTypes);
    procedure SetShowHeadings(AValue: Boolean);
  public
    constructor Create(TheOwner: TComponent); override;
    function SelectedList: TList;
    procedure SelectAll;
    procedure SelectNone;
    property DataFile: TEpiDataFile read FDataFile write SetDataFile;
    property ShowHeadings: Boolean read FShowHeadings write SetShowHeadings;
    property ShowFieldTypes: TEpiFieldTypes read FShowFieldTypes write SetShowFieldTypes;
  end;

implementation

{$R *.lfm}

{ TDataFormTreeViewFrame }

procedure TDataFormTreeViewFrame.DataFileTreeGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  CI: TEpiCustomControlItem;
begin
  if Column = 1 then exit;

  CI := GetCustomItemFromNode(Node);

  if CI is TEpiHeading then
    ImageIndex := 8;
  if CI is TEpiSection then
    ImageIndex := 9;

  if CI is TEpiField then
  case TEpiField(CI).FieldType of
    ftBoolean:
      ImageIndex := 0;
    ftInteger,
    ftAutoInc:
      ImageIndex := 1;
    ftFloat:
      ImageIndex := 2;
    ftDMYDate,
    ftDMYAuto:
      ImageIndex := 3;
    ftMDYDate,
    ftMDYAuto:
      ImageIndex := 4;
    ftYMDDate,
    ftYMDAuto:
      ImageIndex := 5;
    ftTime,
    ftTimeAuto:
      ImageIndex := 6;
    ftString,
    ftUpperString:
      ImageIndex := 7;
  end;
end;

procedure TDataFormTreeViewFrame.DataFileTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  CI: TEpiCustomControlItem;
begin
  CI := GetCustomItemFromNode(Node);

  if Column = 0 then
    CellText := CI.Name
  else begin
    if CI is TEpiSection then
      CellText := TEpiSection(CI).Caption.Text;
    if CI is TEpiHeading then
      CellText := TEpiHeading(CI).Caption.Text;
    if CI is TEpiField then
      CellText := TEpiField(CI).Question.Text;
  end;
end;

procedure TDataFormTreeViewFrame.DataFileTreeInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  CI: TEpiCustomControlItem;
begin
  Sender.CheckType[Node]  := ctTriStateCheckBox;
  Sender.CheckState[Node] := csCheckedNormal;
  Sender.IsVisible[Node] := true;

  CI := GetCustomItemFromNode(Node);

  // Visibility for headings
  if (CI is TEpiHeading) then
  begin
    if (not ShowHeadings)
    then
      Sender.IsVisible[Node] := false;
  end else

  // Visibility for fields
  if (CI is TEpiField) then
  begin
    if (not (TEpiField(CI).FieldType in ShowFieldTypes))
    then
      Sender.IsVisible[Node] := false;
  end else

  // Visibility for sections
  if (CI is TEpiSection) then
  begin

  end;
end;

function TDataFormTreeViewFrame.GetCustomItemFromNode(const Node: PVirtualNode
  ): TEpiCustomControlItem;
begin
  result := TEpiCustomControlItem(DataFileTree.GetNodeData(Node)^);
end;

procedure TDataFormTreeViewFrame.PopulateTree(const Datafile: TEpiDataFile);
var
  MainNode: PVirtualNode;
  CI: TEpiCustomControlItem;
  CurrentNode: PVirtualNode;
  i: Integer;
begin
  if not Assigned(Datafile) then exit;

  DataFileTree.Clear;
  DataFileTree.BeginUpdate;
  DataFileTree.NodeDataSize := SizeOf(TEpiCustomControlItem);

  MainNode := DataFileTree.AddChild(nil, Datafile.MainSection);

  for i := 1 to Datafile.ControlItems.Count - 1 do
  begin
    CI := Datafile.ControlItem[i];

    if (CI.Owner.Owner = Datafile.MainSection) or
       (CI is TEpiSection)
    then
      CurrentNode := MainNode;

    if CI is TEpiSection then
      CurrentNode := DataFileTree.AddChild(CurrentNode, CI)
    else
      DataFileTree.AddChild(CurrentNode, CI);
  end;
  DataFileTree.EndUpdate;
  DataFileTree.ReinitChildren(MainNode, true);
  DataFileTree.ToggleNode(MainNode);
end;

procedure TDataFormTreeViewFrame.SetDataFile(AValue: TEpiDataFile);
begin
  if FDataFile = AValue then Exit;
  FDataFile := AValue;

  PopulateTree(FDataFile);
end;

procedure TDataFormTreeViewFrame.SetShowFieldTypes(AValue: TEpiFieldTypes);
begin
  if FShowFieldTypes = AValue then Exit;
  FShowFieldTypes := AValue;

  PopulateTree(FDataFile);
end;

procedure TDataFormTreeViewFrame.SetShowHeadings(AValue: Boolean);
begin
  if FShowHeadings = AValue then Exit;
  FShowHeadings := AValue;

  PopulateTree(FDataFile);
end;

constructor TDataFormTreeViewFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FDataFile := nil;
  FShowHeadings := true;
  FShowFieldTypes := AllFieldTypes;
end;

function TDataFormTreeViewFrame.SelectedList: TList;
var
  Node: PVirtualNode;
  CI: TEpiCustomControlItem;
begin
  Result := TList.Create;

  Node := DataFileTree.GetFirst();
  while Assigned(Node) do
  begin
    CI := GetCustomItemFromNode(Node);

    if (DataFileTree.CheckState[Node] in [csMixedNormal, csCheckedNormal]) and
       (DataFileTree.IsVisible[Node])
    then
      Result.Add(GetCustomItemFromNode(Node));
    Node := DataFileTree.GetNext(Node, true);
  end;
end;

procedure TDataFormTreeViewFrame.SelectAll;
begin
  DataFileTree.CheckState[DataFileTree.GetFirst()] := csCheckedNormal;
end;

procedure TDataFormTreeViewFrame.SelectNone;
begin
  DataFileTree.CheckState[DataFileTree.GetFirst()] := csUncheckedNormal;
end;

end.

