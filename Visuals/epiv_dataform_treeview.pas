unit epiv_dataform_treeview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, VirtualTrees, epicustombase,
  epidatafiles, epidatafilestypes;

type

  { TDataFormTreeViewFrame }

  TDataFormTreeViewFrame = class(TFrame)
  private
    VST: TVirtualStringTree;
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure VSTInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  private
    FDataFile: TEpiDataFile;
    FShowFieldTypes: TEpiFieldTypes;
    FShowHeadings: Boolean;
    function GetCustomItemFromNode(Const Node: PVirtualNode): TEpiCustomControlItem;
    function GetSelectedList: TList;
    procedure PopulateTree(Const Datafile: TEpiDataFile);
    procedure SetDataFile(AValue: TEpiDataFile);
    procedure SetSelectedList(AValue: TList);
    procedure SetShowFieldTypes(AValue: TEpiFieldTypes);
    procedure SetShowHeadings(AValue: Boolean);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure SelectAll;
    procedure SelectNone;
    procedure SelectKey;
    procedure SelectFieldTypes(Const FieldTypes: TEpiFieldTypes;
      DeSelect: Boolean);
    property DataFile: TEpiDataFile read FDataFile write SetDataFile;
    property SelectedList: TList read GetSelectedList write SetSelectedList;
    property ShowHeadings: Boolean read FShowHeadings write SetShowHeadings;
    property ShowFieldTypes: TEpiFieldTypes read FShowFieldTypes write SetShowFieldTypes;
  end;

implementation

{$R *.lfm}

uses
  epiv_datamodule;

{ TDataFormTreeViewFrame }

procedure TDataFormTreeViewFrame.VSTGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  CI: TEpiCustomControlItem;
begin
  if Column = 1 then exit;

  CI := GetCustomItemFromNode(Node);
  ImageIndex := DM.GetImageIndex(CI);
end;

procedure TDataFormTreeViewFrame.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  CI: TEpiCustomControlItem;
begin
  CI := GetCustomItemFromNode(Node);

  if Node^.Parent = VST.RootNode then
    CellText := DataFile.Caption.Text
  else
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

procedure TDataFormTreeViewFrame.VSTInitNode(Sender: TBaseVirtualTree; ParentNode,
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
  result := TEpiCustomControlItem(VST.GetNodeData(Node)^);
end;

function TDataFormTreeViewFrame.GetSelectedList: TList;
var
  Node: PVirtualNode;
begin
  Result := TList.Create;

  Node := VST.GetFirst();
  while Assigned(Node) do
  begin
    if (VST.CheckState[Node] in [csMixedNormal, csCheckedNormal]) and
       (VST.IsVisible[Node])
    then
      Result.Add(GetCustomItemFromNode(Node));
    Node := VST.GetNext(Node, true);
  end;
end;

procedure TDataFormTreeViewFrame.PopulateTree(const Datafile: TEpiDataFile);
var
  MainNode: PVirtualNode;
  CI: TEpiCustomControlItem;
  CurrentNode: PVirtualNode;
  i: Integer;
begin
  VST.Clear;

  if not Assigned(Datafile) then exit;

  VST.BeginUpdate;
  VST.NodeDataSize := SizeOf(TEpiCustomControlItem);

  MainNode := VST.AddChild(nil, Datafile.MainSection);

  for i := 1 to Datafile.ControlItems.Count - 1 do
  begin
    CI := Datafile.ControlItem[i];

    if (CI.Owner.Owner = Datafile.MainSection) or
       (CI is TEpiSection)
    then
      CurrentNode := MainNode;

    if CI is TEpiSection then
      CurrentNode := VST.AddChild(CurrentNode, CI)
    else
      VST.AddChild(CurrentNode, CI);
  end;
  VST.EndUpdate;
  VST.ReinitChildren(MainNode, true);
  VST.ToggleNode(MainNode);
end;

procedure TDataFormTreeViewFrame.SetDataFile(AValue: TEpiDataFile);
begin
  if FDataFile = AValue then Exit;
  FDataFile := AValue;

  PopulateTree(FDataFile);
end;

procedure TDataFormTreeViewFrame.SetSelectedList(AValue: TList);
var
  Node: PVirtualNode;
  CI: TEpiCustomControlItem;
begin
  if not Assigned(AValue) then Exit;

  Node := VST.GetFirst();
  while Assigned(Node) do
  begin
    CI := GetCustomItemFromNode(Node);

    if (AValue.IndexOf(CI) >= 0) then
      VST.CheckState[Node] := csCheckedNormal
    else
      VST.CheckState[Node] := csUncheckedNormal;

    Node := VST.GetNext(Node, true);
  end;
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


  VST := TVirtualStringTree.Create(self);
  with VST do
  begin
    BeginUpdate;

    with TreeOptions do
    begin
      AnimationOptions := [];
      AutoOptions      := [toAutoTristateTracking];
      MiscOptions      := [toCheckSupport, toFullRepaintOnResize,
                           toGridExtensions, toToggleOnDblClick, toWheelPanning];
      PaintOptions     := [toShowButtons, toShowDropmark, toShowRoot,
                           toShowTreeLines, toThemeAware, toUseBlendedImages];
      SelectionOptions := [];
      StringOptions    := [];
    end;

    with Header do
    begin
      Options := [hoAutoResize, hoColumnResize, hoDblClickResize, hoVisible,
                  hoFullRepaintOnResize];

      with Columns.Add do
      begin
        Text := 'Name';
        CheckBox   := true;
        CheckState := csCheckedNormal;
        CheckType  := ctTriStateCheckBox;
        Options    := [coAllowClick, coEnabled, coParentBidiMode,
                       coParentColor, coResizable, coShowDropMark, coVisible,
                       coSmartResize, coAllowFocus];
        Width      := 150;
      end;

      with Columns.Add do
      begin
        Text := 'Caption';
        CheckBox   := false;
        CheckState := csUncheckedNormal;
        CheckType  := ctCheckBox;
        Options    := [coAllowClick, coEnabled, coParentBidiMode,
                       coParentColor, coResizable, coShowDropMark, coVisible,
                       coSmartResize, coAllowFocus];
      end;

      MainColumn := 0;
      AutoSizeIndex := 1;
    end;

    OnInitNode      := @VSTInitNode;
    OnGetImageIndex := @VSTGetImageIndex;
    OnGetText       := @VSTGetText;

    Align := alClient;
    Parent := Self;
    EndUpdate;
  end;

  VST.Images := DM.Icons16;
end;

procedure TDataFormTreeViewFrame.SelectAll;
begin
  if VST.RootNodeCount = 0 then exit;
  VST.CheckState[VST.GetFirst()] := csCheckedNormal;
end;

procedure TDataFormTreeViewFrame.SelectNone;
begin
  if VST.RootNodeCount = 0 then exit;
  VST.CheckState[VST.GetFirst()] := csUncheckedNormal;
end;

procedure TDataFormTreeViewFrame.SelectKey;
var
  Node: PVirtualNode;
  CI: TEpiCustomControlItem;
begin
  if VST.RootNodeCount = 0 then exit;
  SelectNone;

  Node := VST.GetFirstChild(nil);
  while Assigned(Node) do
  begin
    CI := GetCustomItemFromNode(Node);

    if (CI.InheritsFrom(TEpiField)) and
       (DataFile.KeyFields.FieldExists(TEpiField(CI)))
    then
      VST.CheckState[Node] := csCheckedNormal;

    Node := VST.GetNext(Node, true);
  end;
end;

procedure TDataFormTreeViewFrame.SelectFieldTypes(
  const FieldTypes: TEpiFieldTypes; DeSelect: Boolean);
var
  Node: PVirtualNode;
  CI: TEpiCustomControlItem;
  Cs: TCheckState;
begin
  Node := VST.GetFirstChild(nil);

  if DeSelect then
    Cs := csUncheckedNormal
  else
    Cs := csCheckedNormal;

  while Assigned(Node) do
  begin
    CI := GetCustomItemFromNode(Node);

    if (CI.InheritsFrom(TEpiField)) and
       (TEpiField(CI).FieldType in FieldTypes)
    then
      VST.CheckState[Node] := Cs;

    Node := VST.GetNext(Node, true);
  end;
end;

end.

