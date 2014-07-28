unit epiv_projecttreeview_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, FileUtil, Forms, Controls, StdCtrls, ExtCtrls,
  VirtualTrees,
  {$IFDEF MSWINDOWS}
  ActiveX,
  {$ELSE}
  FakeActiveX,
  {$ENDIF}
  epidocument, epirelations, epidatafiles, epidatafilestypes;

type

  TEpiVProjectDisplayMode = (
    pdmSeperate,          // Display each document with distinct rootnodes
    pdmCommon             // Display only common dataform (structure AND name)
  );

  TEpiVProjectCheckType = (
    pctTriState,          // Allow individual selection of datafiles, normal tri-state functionality
    pctCascade            // Only allow cascading select -> selecting a Master automatically selects
                          //   details. Details cannot be de-selected.
  );


  TEpiVProjectTreeSelectDataFile = procedure(Const DataFile: TEpiDataFile) of object;
  TEpiVProjectTreeAllowSelectDataFile = procedure(Const OldDataFile,
    NewDataFile: TEpiDataFile; var Allowed: Boolean) of object;

  TEpiVProjectTreeError = procedure(Const Msg: String) of object;
  TEpiVProjectTreeGetHint = procedure(Const DataFile: TEpiDataFile;
    var HintText: string) of object;

  TEpiVProjectTreeRelationEvent = procedure(Const Relation: TEpiMasterRelation) of object;

  { TEpiVProjectTreeViewFrame }

  TEpiVProjectTreeViewFrame = class(TFrame)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Panel1: TPanel;
    VST: TVirtualStringTree;

    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure VSTChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTChecking(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var NewState: TCheckState; var Allowed: Boolean);
    procedure VSTDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure VSTDragDrop(Sender: TBaseVirtualTree; Source: TObject;
      DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
      const Pt: TPoint; var Effect: LongWord; Mode: TDropMode);
    procedure VSTDragOver(Sender: TBaseVirtualTree; Source: TObject;
      Shift: TShiftState; State: TDragState; const Pt: TPoint; Mode: TDropMode;
      var Effect: LongWord; var Accept: Boolean);
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure VSTFocusChanging(Sender: TBaseVirtualTree; OldNode,
      NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
      var Allowed: Boolean);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: String);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure VSTNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; const NewText: String);
    procedure VSTStartDrag(Sender: TObject; var DragObject: TDragObject);

  { Misc. }
  private
    FUpdatingTree: Boolean;
    FDocumentList: TList;
    function  AllRelationsAreEqual: boolean;
    function  DataFileFromNode(Const Node: PVirtualNode): TEpiDataFile;
    function  NodeFromDataFile(Const DataFile: TEpiDataFile): PVirtualNode;
    function  MasterRelationFromDataFile(Const Datafile: TEpiDataFile): TEpiMasterRelation;
    function  MasterRelationFromNode(Const Node: PVirtualNode): TEpiMasterRelation;
    function  NodeFromMasterRelation(Const MasterRelation: TEpiMasterRelation): PVirtualNode;
    procedure DoError(Const Msg: String);
    procedure UpdateCustomData(Const MasterRelation: TEpiMasterRelation;
      Const Node: PVirtualNode);
    procedure DoUpdateTree;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  { Structural }
  private
    procedure DoNewRelation(Const NewRelation: TEpiMasterRelation);
    procedure DoDeleteRelation(Const Relation: TEpiMasterRelation);
  public
    procedure AddDocument(Const Doc: TEpiDocument);
    procedure CreateRelatedDataFile(Const ParentDataFile: TEpiDataFile);
    procedure DeleteDataFile(DataFile: TEpiDataFile);

  { Options }
  private
    FAllowSelectProject: Boolean;
    FCheckType: TEpiVProjectCheckType;
    FDisplayMode: TEpiVProjectDisplayMode;
    FEditCaption: Boolean;
    FEditStructure: Boolean;
    FShowCheckBoxes: Boolean;
    FShowHint: boolean;
    procedure SetAllowSelectProject(AValue: Boolean);
    procedure SetCheckType(AValue: TEpiVProjectCheckType);
    procedure SetDisplayMode(AValue: TEpiVProjectDisplayMode);
    procedure SetEditCaption(AValue: Boolean);
    procedure SetEditStructure(AValue: Boolean);
    procedure SetShowCheckBoxes(AValue: Boolean);
    procedure SetShowHint(AValue: boolean);
  public
    property  AllowSelectProject: Boolean read FAllowSelectProject write SetAllowSelectProject;
    property  CheckType: TEpiVProjectCheckType read FCheckType write SetCheckType;
    property  DisplayMode: TEpiVProjectDisplayMode read FDisplayMode write SetDisplayMode;
    property  EditCaption: Boolean read FEditCaption write SetEditCaption;
    property  EditStructure: Boolean read FEditStructure write SetEditStructure;
    property  ShowCheckBoxes: Boolean read FShowCheckBoxes write SetShowCheckBoxes;
    property  ShowHint: boolean read FShowHint write SetShowHint;

  { Access properties }
  private
    function GetSelectedDataFile: TEpiDataFile;
  public
    property  SelectedDataFile: TEpiDataFile read GetSelectedDataFile;

  { Events }
  private
    FOnDataFileAllowSelect: TEpiVProjectTreeAllowSelectDataFile;
    FOnDataFileSelected: TEpiVProjectTreeSelectDataFile;
    FOnDelete: TEpiVProjectTreeRelationEvent;
    FOnError: TEpiVProjectTreeError;
    FOnGetHint: TEpiVProjectTreeGetHint;
    FOnNewRelation: TEpiVProjectTreeRelationEvent;
  public
    property  OnDataFileSelected: TEpiVProjectTreeSelectDataFile read FOnDataFileSelected write FOnDataFileSelected;
    property  OnDataFileAllowSelect: TEpiVProjectTreeAllowSelectDataFile read FOnDataFileAllowSelect write FOnDataFileAllowSelect;
    property  OnDelete: TEpiVProjectTreeRelationEvent read FOnDelete write FOnDelete;
    property  OnError: TEpiVProjectTreeError read FOnError write FOnError;
    property  OnNewRelation: TEpiVProjectTreeRelationEvent read FOnNewRelation write FOnNewRelation;
    property  OnGetHint: TEpiVProjectTreeGetHint read FOnGetHint write FOnGetHint;
  end;

implementation

{$R *.lfm}

uses
  Dialogs;

const
  PROJECTTREE_NODE_CUSTOMKEY     = 'PROJECTTREE_NODE_CUSTOMKEY';
  PROJECTTREE_RELATION_CUSTOMKEY = 'PROJECTTREE_RELATION_CUSTOMKEY';

type

  { TNodeDragObject }

  TNodeDragObject = class(TDragObjectEx)
  private
    FDragNode: PVirtualNode;
  public
    property DragNode: PVirtualNode read FDragNode write FDragNode;
  end;

{ TEpiVProjectTreeViewFrame }

procedure TEpiVProjectTreeViewFrame.VSTChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  CNode: PVirtualNode;
begin
  if CheckType = pctTriState then exit;

  CNode := Node^.FirstChild;
  while Assigned(CNode) do
  begin
    if Sender.CheckState[Node] in [csCheckedNormal, csMixedNormal] then
      Sender.CheckState[CNode] := csMixedNormal
    else
      Sender.CheckState[CNode] := csUncheckedNormal;
    CNode := CNode^.NextSibling;
  end;
end;

procedure TEpiVProjectTreeViewFrame.Button2Click(Sender: TObject);
begin
  CreateRelatedDataFile(SelectedDataFile);
end;

procedure TEpiVProjectTreeViewFrame.Button1Click(Sender: TObject);
begin
  //
end;

procedure TEpiVProjectTreeViewFrame.Button3Click(Sender: TObject);
begin
  DeleteDataFile(SelectedDataFile);
end;

procedure TEpiVProjectTreeViewFrame.VSTChecking(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
begin
  if CheckType = pctTriState then
  begin
    Allowed := true;
    Exit;
  end;

  if (Sender.CheckState[Node] = csMixedNormal) and
     (Sender.CheckState[Node^.Parent] in [csCheckedNormal, csMixedNormal])
  then
  begin
    Allowed := false;
    Exit;
  end;

  if (Node^.Parent <> nil) and (NewState = csUncheckedNormal) then
    Allowed := Sender.CheckState[Node^.Parent] = csUncheckedNormal;
end;

procedure TEpiVProjectTreeViewFrame.VSTDragAllowed(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := FEditStructure;
end;

procedure TEpiVProjectTreeViewFrame.VSTDragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; const Pt: TPoint;
  Mode: TDropMode; var Effect: LongWord; var Accept: Boolean);
var
  Node: PVirtualNode;
  DF: TEpiDataFile;
begin
  Node := Sender.DropTargetNode;
  if not Assigned(Node) then
    Panel1.Caption := 'No Drop Target'
  else
    begin
      DF := DataFileFromNode(Node);

      if Assigned(DF) then
        Panel1.Caption := 'Drop Target: ' + DF.Caption.Text
      else
        Panel1.Caption := 'Drop Target: Not a DF, must be DOC';
    end;


  Accept :=
    // Only allow to drag-drop within ourselves.
    (TNodeDragObject(Source).Control = Sender) and

    // We can not drop onto existing DF's, that would bust the key-field hierachy.
    (Mode in [dmAbove, dmBelow]) and

    // -same goes for out-of-parent experiences... ;)
    (TNodeDragObject(Source).DragNode^.Parent = Sender.DropTargetNode^.Parent)
    ;
end;

procedure TEpiVProjectTreeViewFrame.VSTDragDrop(Sender: TBaseVirtualTree;
  Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
  Shift: TShiftState; const Pt: TPoint; var Effect: LongWord; Mode: TDropMode);
var
  OldNode: PVirtualNode;
  MR: TEpiMasterRelation;
  MRList: TEpiRelationList;
  OldIndex: Integer;
  NewIndex: Integer;
begin
  OldNode := TNodeDragObject(Source).DragNode;

  MR := MasterRelationFromNode(OldNode);
  MRList := TEpiRelationList(MR.Owner);
  OldIndex := MRList.IndexOf(MR);

  MR := MasterRelationFromNode(Sender.DropTargetNode);
  NewIndex := MRList.IndexOf(MR);

  case Mode of
    dmAbove:
      begin
        if (NewIndex > OldIndex) then Dec(NewIndex);
        Sender.MoveTo(OldNode, Sender.DropTargetNode, amInsertBefore, false);
      end;
    dmBelow:
      begin
        if (NewIndex < OldIndex) then Inc(NewIndex);
        Sender.MoveTo(OldNode, Sender.DropTargetNode, amInsertAfter, false);
      end;
  end;
  MRList.Move(OldIndex, NewIndex);
end;

procedure TEpiVProjectTreeViewFrame.VSTGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: String);
var
  DF: TEpiDataFile;
begin
  DF := nil;

  if Node^.Parent <> VST.RootNode then
    DF := DataFileFromNode(Node);

  if Assigned(DF) then
    HintText := DF.Caption.Text;

  if Assigned(OnGetHint) then
    OnGetHint(DF, HintText);
end;

procedure TEpiVProjectTreeViewFrame.VSTFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  DF: TEpiDataFile;
begin
  if not Assigned(Node) then exit;

  if Node^.Parent = Sender.RootNode then
    begin

    end
  else
    begin
      DF := DataFileFromNode(Node);

      if Assigned(OnDataFileSelected) then
        OnDataFileSelected(DF);
    end;
end;

procedure TEpiVProjectTreeViewFrame.VSTFocusChanging(Sender: TBaseVirtualTree;
  OldNode, NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
  var Allowed: Boolean);
var
  OldDF: TEpiDataFile;
  NewDF: TEpiDataFile;
begin
  if not Assigned(NewNode) then
    begin
      Allowed := false;
      Exit;
    end;

  if (NewNode^.Parent = Sender.RootNode) and
     (not AllowSelectProject)
  then
    begin
      Allowed := false;
      Exit;
    end;

  if NewNode^.Parent = Sender.RootNode then
    begin

    end
  else
    begin
      OldDF := DataFileFromNode(OldNode);
      NewDF := DataFileFromNode(NewNode);

      if Assigned(OnDataFileAllowSelect) then
        OnDataFileAllowSelect(OldDF, NewDF, Allowed);
    end;
end;

procedure TEpiVProjectTreeViewFrame.VSTFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  DataFile: TEpiDataFile;
  Relation: TEpiMasterRelation;
begin
  if FUpdatingTree then exit;
  if not Assigned(Node) then exit;
  if Node^.Parent = VST.RootNode then exit;

  Relation := MasterRelationFromNode(Node);
  DoDeleteRelation(Relation);

  DataFile := DataFileFromNode(Node);
  DataFile.Free;
end;

procedure TEpiVProjectTreeViewFrame.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  Obj: TObject;
  Doc: TEpiDocument absolute Obj;
  MR:  TEpiMasterRelation absolute Obj;
begin
  if TextType <> ttNormal then exit;

  Obj := TObject(Sender.GetNodeData(Node)^);

  if Node^.Parent = Sender.RootNode then
    if Assigned(Obj) then
      CellText := Doc.Study.Title.Text
    else
      CellText := 'Incompatible relational structures!'
  else
    CellText := Mr.Datafile.Caption.Text;
end;

procedure TEpiVProjectTreeViewFrame.VSTNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; const NewText: String);
var
  DF: TEpiDataFile;
begin
  DF := DataFileFromNode(Node);

  if Assigned(DF) then
  begin
    if NewText = '' then
    begin
      DoError('A dataform caption cannot be empty!');
      Exit;
    end;

    DF.Caption.Text := NewText;
  end;
end;

procedure TEpiVProjectTreeViewFrame.VSTStartDrag(Sender: TObject;
  var DragObject: TDragObject);
var
  Pt: types.TPoint;
  Node: PVirtualNode;
begin
  Pt := Mouse.CursorPos;
  Pt := VST.ScreenToClient(Pt);

  Node := VST.GetNodeAt(Pt.X, Pt.Y);
  DragObject := TNodeDragObject.Create(VST);
  TNodeDragObject(DragObject).DragNode := Node;
end;

function TEpiVProjectTreeViewFrame.AllRelationsAreEqual: boolean;

  function CompareTreeStructure(Const RelationListA, RelationListB: TEpiRelationList): boolean;
  var
    i: Integer;
    MRA: TEpiMasterRelation;
    MRB: TEpiMasterRelation;
  begin
    result := (RelationListA.Count = RelationListB.Count);
    if not Result then exit;

    for i := 0 to RelationListA.Count - 1 do
    begin
      MRA := RelationListA.MasterRelation[i];
      MRB := RelationListB.MasterRelation[i];

      Result :=
        (MRA.Datafile.Name = MRB.Datafile.Name) and
        CompareTreeStructure(MRA.DetailRelations, MRB.DetailRelations);

      if not Result then exit;
    end;
  end;

var
  MainDoc: TEpiDocument;
  CompareDoc: TEpiDocument;
  i: Integer;
begin
  Result := true;

  if (FDocumentList.Count = 1)
  then
    Exit;

  MainDoc := TEpiDocument(FDocumentList[0]);

  for i := 1 to FDocumentList.Count - 1 do
  begin
    CompareDoc := TEpiDocument(FDocumentList[i]);

    Result := Result and
      CompareTreeStructure(MainDoc.Relations, CompareDoc.Relations);
  end;
end;

function TEpiVProjectTreeViewFrame.DataFileFromNode(const Node: PVirtualNode
  ): TEpiDataFile;
var
  Mr: TEpiMasterRelation;
begin
  Result := nil;

  Mr := MasterRelationFromNode(Node);
  if Assigned(Mr) then
    Result := Mr.Datafile;
end;

function TEpiVProjectTreeViewFrame.NodeFromDataFile(const DataFile: TEpiDataFile
  ): PVirtualNode;
begin
  result := nil;

  if Assigned(DataFile) then
    Result := PVirtualNode(DataFile.FindCustomData(PROJECTTREE_NODE_CUSTOMKEY));
end;

function TEpiVProjectTreeViewFrame.MasterRelationFromDataFile(
  const Datafile: TEpiDataFile): TEpiMasterRelation;
begin
  result := nil;

  if Assigned(DataFile) then
    result := TEpiMasterRelation(Datafile.FindCustomData(PROJECTTREE_RELATION_CUSTOMKEY));
end;

function TEpiVProjectTreeViewFrame.MasterRelationFromNode(const Node: PVirtualNode
  ): TEpiMasterRelation;
begin
  Result := nil;

  if (Node = nil) then exit;
  if (Node^.Parent = VST.RootNode) then exit;

  Result := TEpiMasterRelation(VST.GetNodeData(Node)^);
end;

function TEpiVProjectTreeViewFrame.NodeFromMasterRelation(
  const MasterRelation: TEpiMasterRelation): PVirtualNode;
begin
  result := nil;

  if Assigned(MasterRelation) then
    Result := PVirtualNode(MasterRelation.FindCustomData(PROJECTTREE_NODE_CUSTOMKEY));
end;

procedure TEpiVProjectTreeViewFrame.DoError(const Msg: String);
begin
  if Assigned(OnError) then
    OnError(Msg)
  else
    ShowMessage(Msg);
end;

procedure TEpiVProjectTreeViewFrame.UpdateCustomData(
  const MasterRelation: TEpiMasterRelation; const Node: PVirtualNode);
begin
  MasterRelation.AddCustomData(PROJECTTREE_NODE_CUSTOMKEY, TObject(Node));
  MasterRelation.Datafile.AddCustomData(PROJECTTREE_NODE_CUSTOMKEY, TObject(Node));

  MasterRelation.Datafile.AddCustomData(PROJECTTREE_RELATION_CUSTOMKEY, MasterRelation);
end;

procedure TEpiVProjectTreeViewFrame.DoUpdateTree;

  procedure BuildTreeRecursive(Const Parent: PVirtualNode; MR: TEpiMasterRelation);
  var
    i: Integer;
    Node: PVirtualNode;
  begin
    Node := VST.AddChild(Parent, MR);

    UpdateCustomData(MR, Node);

    if ShowCheckBoxes then
      VST.CheckType[Node] := ctTriStateCheckBox;

    for i := 0 to MR.DetailRelations.Count - 1 do
      BuildTreeRecursive(Node, MR.DetailRelation[i]);
  end;

  procedure BuildDocumentTree(Const Parent: PVirtualNode; Doc: TEpiDocument);
  var
    i: Integer;
    Node: PVirtualNode;
  begin
    Node := VST.AddChild(Parent, Doc);

    if ShowCheckBoxes then
      VST.CheckType[Node] := ctTriStateCheckBox;

    for i := 0 to Doc.Relations.Count - 1 do
      BuildTreeRecursive(Node, Doc.Relations.MasterRelation[i]);
  end;

var
  i: Integer;
begin
  if FDocumentList.Count = 0 then exit;

  FUpdatingTree := true;

  VST.BeginUpdate;
  VST.Clear;

  if DisplayMode = pdmSeperate then
    for i := 0 to FDocumentList.Count -1 do
      BuildDocumentTree(VST.RootNode, TEpiDocument(FDocumentList[i]))
  else if AllRelationsAreEqual then
    BuildDocumentTree(VST.RootNode, TEpiDocument(FDocumentList[0]))
  else
    VST.AddChild(VST.RootNode, nil);

  VST.EndUpdate;
  FUpdatingTree := false;
end;

constructor TEpiVProjectTreeViewFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  // Objects
  FDocumentList := TList.Create;
  FUpdatingTree := false;

  // Options
  FAllowSelectProject := false;
  FCheckType          := pctTriState;
  FDisplayMode        := pdmCommon;
  FEditCaption        := false;
  FEditStructure      := false;
  FShowCheckBoxes     := false;
  FShowHint           := false;

  with VST do
  begin
    NodeDataSize    := SizeOf(Pointer);

    TreeOptions.AutoOptions := TreeOptions.AutoOptions + [toAutoTristateTracking];

    OnGetText       := @VSTGetText;
    OnNewText       := @VSTNewText;

    OnChecking      := @VSTChecking;
    OnChecked       := @VSTChecked;

    OnFocusChanging := @VSTFocusChanging;
    OnFocusChanged  := @VSTFocusChanged;

    DragType        := dtVCL;
    OnDragAllowed   := @VSTDragAllowed;
    OnDragOver      := @VSTDragOver;
    OnDragDrop      := @VSTDragDrop;
    OnStartDrag     := @VSTStartDrag;

    OnGetHint       := @VSTGetHint;

    OnFreeNode      := @VSTFreeNode;
  end;
end;

destructor TEpiVProjectTreeViewFrame.Destroy;
begin
  FDocumentList.Free;
  inherited Destroy;
end;

procedure TEpiVProjectTreeViewFrame.DoNewRelation(
  const NewRelation: TEpiMasterRelation);
begin
  if Assigned(OnNewRelation) then
    OnNewRelation(NewRelation);
end;

procedure TEpiVProjectTreeViewFrame.DoDeleteRelation(
  const Relation: TEpiMasterRelation);
begin
  if Assigned(OnDelete) then
    OnDelete(Relation);
end;

procedure TEpiVProjectTreeViewFrame.AddDocument(const Doc: TEpiDocument);
begin
  FDocumentList.Add(Doc);
  DoUpdateTree;
end;

procedure TEpiVProjectTreeViewFrame.CreateRelatedDataFile(
  const ParentDataFile: TEpiDataFile);
var
  ParentNode: PVirtualNode;
  NewRelation: TEpiMasterRelation;
  NewDataFile: TEpiDataFile;
  i: Integer;
  Ft: TEpiFieldType;
  ParentKeyField: TEpiField;
  NewKeyField: TEpiField;
  Node: PVirtualNode;
begin
  if not EditStructure then exit;

  ParentNode := VST.RootNode;

  if Assigned(ParentDataFile) then
  begin
    ParentNode := NodeFromDataFile(ParentDataFile);
    NewRelation := MasterRelationFromDataFile(ParentDataFile).NewDetailRelation;
  end else
    // TODO: How to handle creating new datafile which is a true "master". Hence
    //       we need information on which tree we insert into!
    NewRelation := TEpiDocument(FDocumentList[0]).Relations.NewMasterRelation;

  NewDataFile := TEpiDataFiles(ParentDataFile.Owner).NewDataFile;

  NewRelation.Datafile := NewDataFile;

  for ParentKeyField in ParentDataFile.KeyFields do
  begin
    // In a related datafile, the "primary" keys cannot be autoinc - it would
    // screw up the numbering.
    Ft := ParentKeyField.FieldType;
    if Ft = ftAutoInc then Ft := ftInteger;

    NewKeyField := NewDataFile.NewField(Ft);
    NewKeyField.Assign(ParentKeyField);
    NewKeyField.EntryMode := emNoEnter;
    NewDataFile.KeyFields.AddItem(NewKeyField);
  end;

  // TODO : Husk checkbox state OG hvilket stadie checkboxen skal have!
  Node := VST.AddChild(ParentNode, NewRelation);

  DoNewRelation(NewRelation);
end;

procedure TEpiVProjectTreeViewFrame.DeleteDataFile(DataFile: TEpiDataFile);
var
  Node: PVirtualNode;
begin
  if not EditStructure then exit;
  if not Assigned(DataFile) then exit;

  Node := NodeFromDataFile(DataFile);
  VST.DeleteNode(Node);
end;

procedure TEpiVProjectTreeViewFrame.SetAllowSelectProject(AValue: Boolean);
begin
  if FAllowSelectProject = AValue then Exit;
  FAllowSelectProject := AValue;
end;

procedure TEpiVProjectTreeViewFrame.SetCheckType(AValue: TEpiVProjectCheckType);
begin
  if FCheckType = AValue then Exit;
  FCheckType := AValue;

  with VST.TreeOptions do
    case CheckType of
      pctTriState: AutoOptions := AutoOptions + [toAutoTristateTracking];
      pctCascade:  AutoOptions := AutoOptions - [toAutoTristateTracking];
  end;

  // UpdateTree ??
end;

procedure TEpiVProjectTreeViewFrame.SetDisplayMode(AValue: TEpiVProjectDisplayMode);
begin
  if FDisplayMode = AValue then Exit;
  FDisplayMode := AValue;
  DoUpdateTree;
end;

procedure TEpiVProjectTreeViewFrame.SetEditCaption(AValue: Boolean);
begin
  if FEditCaption = AValue then Exit;
  FEditCaption := AValue;

  with VST.TreeOptions do
    if FEditCaption then
      MiscOptions := MiscOptions + [toEditable]
    else
      MiscOptions := MiscOptions - [toEditable];
end;

procedure TEpiVProjectTreeViewFrame.SetEditStructure(AValue: Boolean);
begin
  if FEditStructure = AValue then Exit;
  FEditStructure := AValue;

  with VST do
    if FEditStructure then
      DragMode := dmAutomatic
    else
      DragMode := dmManual;
end;

procedure TEpiVProjectTreeViewFrame.SetShowCheckBoxes(AValue: Boolean);
var
  Node: PVirtualNode;
  Val: TCheckType;
begin
  if FShowCheckBoxes = AValue then Exit;
  FShowCheckBoxes := AValue;

  if ShowCheckBoxes then
    Val := ctTriStateCheckBox
  else
    Val := ctNone;

  Node := VST.GetFirst();
  while Assigned(Node) do
  begin
    VST.CheckType[Node] := Val;
    Node := VST.GetNext(Node, true);
  end;
end;

procedure TEpiVProjectTreeViewFrame.SetShowHint(AValue: boolean);
begin
  if FShowHint = AValue then Exit;
  FShowHint := AValue;

  VST.ShowHint := FShowHint;
end;

function TEpiVProjectTreeViewFrame.GetSelectedDataFile: TEpiDataFile;
begin
  result := DataFileFromNode(VST.FocusedNode);
end;

end.

