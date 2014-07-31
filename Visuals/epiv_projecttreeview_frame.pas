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
  epicustombase, epidocument, epirelations, epidatafiles, epidatafilestypes;

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


  TEpiVTreeNodeObjectType = (otDataFile, otProject);

  TEpiVTreeNodeSelected = procedure(
          Sender:  TObject;
    Const AObject: TEpiCustomBase;
          ObjectType: TEpiVTreeNodeObjectType
  ) of object;

  TEpiVTreeNodeSelecting = procedure(
          Sender:  TObject;
    Const OldObject,     NewObject: TEpiCustomBase;
          OldObjectType, NewObjectType: TEpiVTreeNodeObjectType;
    var   Allowed: Boolean
  ) of object;

  TEpiVProjectTreeError = procedure(Const Msg: String) of object;
  TEpiVProjectTreeGetHint = procedure(
          Sender: TObject;
    Const AObject: TEpiCustomBase;
          ObjectType: TEpiVTreeNodeObjectType;
    var HintText: string) of object;

  TEpiVProjectTreeRelationEvent = procedure(Const Relation: TEpiMasterRelation) of object;

  { TEpiVProjectTreeViewFrame }

  TEpiVProjectTreeViewFrame = class(TFrame)
    VST: TVirtualStringTree;
  private
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
    procedure UpdateCustomData(Const MasterRelation: TEpiMasterRelation;
      Const Node: PVirtualNode);
    procedure DoUpdateTree;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  { Structural }
  private
    function  GetDocuments(const Index: integer): TEpiDocument;
  public
    procedure AddDocument(Const Doc: TEpiDocument);
    procedure CreateRelatedDataFile(Const ParentDataFile: TEpiDataFile);
    procedure DeleteDataFile(DataFile: TEpiDataFile);
  public
    property  Documents[Const Index: integer]: TEpiDocument read GetDocuments;

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
    FOnDelete: TEpiVProjectTreeRelationEvent;
    FOnError: TEpiVProjectTreeError;
    FOnGetHint: TEpiVProjectTreeGetHint;
    FOnNewRelation: TEpiVProjectTreeRelationEvent;
    FOnTreeNodeSelected: TEpiVTreeNodeSelected;
    FOnTreeNodeSelecting: TEpiVTreeNodeSelecting;
  protected
    procedure DoDeleteRelation(Const Relation: TEpiMasterRelation); virtual;
    procedure DoError(Const Msg: String);
    procedure DoGetHint(Const AObject: TEpiCustomBase;
      ObjectType: TEpiVTreeNodeObjectType; var HintText: string); virtual;
    procedure DoNewRelation(Const NewRelation: TEpiMasterRelation); virtual;
    procedure DoTreeNodeSelected(Const AObject: TEpiCustomBase;
      ObjectType: TEpiVTreeNodeObjectType); virtual;
    procedure DoTreeNodeSelecting(Const OldObject, NewObject: TEpiCustomBase;
      OldObjectType, NewObjectType: TEpiVTreeNodeObjectType;
      var Allowed: Boolean); virtual;
  public
    property  OnDelete: TEpiVProjectTreeRelationEvent read FOnDelete write FOnDelete;
    property  OnError: TEpiVProjectTreeError read FOnError write FOnError;
    property  OnGetHint: TEpiVProjectTreeGetHint read FOnGetHint write FOnGetHint;
    property  OnNewRelation: TEpiVProjectTreeRelationEvent read FOnNewRelation write FOnNewRelation;
    property  OnTreeNodeSelected: TEpiVTreeNodeSelected read FOnTreeNodeSelected write FOnTreeNodeSelected;
    property  OnTreeNodeSelecting: TEpiVTreeNodeSelecting read FOnTreeNodeSelecting write FOnTreeNodeSelecting;
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
  O: TEpiCustomBase;
  Ot: TEpiVTreeNodeObjectType;
begin
  if Node^.Parent <> VST.RootNode then
  begin
    O := DataFileFromNode(Node);
    HintText := TEpiDataFile(O).Caption.Text;
    Ot := otDataFile;
  end else begin
    O := TEpiDocument(VST.GetNodeData(Node)^);
    HintText := TEpiDocument(O).Study.Title.Text;
    Ot := otProject;
  end;

  DoGetHint(O, Ot, HintText);
end;

procedure TEpiVProjectTreeViewFrame.VSTFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  O: TEpiCustomBase;
  OT: TEpiVTreeNodeObjectType;
begin
  if not Assigned(Node) then exit;

  if Node^.Parent = Sender.RootNode then
    begin
      O := TEpiDocument(VST.GetNodeData(Node)^);
      ot := otProject;
    end
  else
    begin
      O := DataFileFromNode(Node);
      OT := otDataFile;
    end;

  DoTreeNodeSelected(O, OT);
end;

procedure TEpiVProjectTreeViewFrame.VSTFocusChanging(Sender: TBaseVirtualTree;
  OldNode, NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
  var Allowed: Boolean);
var
  OldObject: TEpiCustomBase;
  NewObject: TEpiCustomBase;
  OldType: TEpiVTreeNodeObjectType;
  NewType: TEpiVTreeNodeObjectType;
begin
  if not Assigned(NewNode) then
    begin
      Allowed := false;
      Exit;
    end;

  OldObject := TEpiCustomBase(VST.GetNodeData(OldNode)^);
  if OldObject is TEpiDocument then
    OldType := otProject
  else
    OldType := otDataFile;

  NewObject := TEpiCustomBase(VST.GetNodeData(NewNode)^);
  if NewObject is TEpiDocument then
    NewType := otProject
  else
    NewType := otDataFile;

  if (NewType = otProject) and
     (not AllowSelectProject)
  then
    begin
      Allowed := false;
      Exit;
    end;

  DoTreeNodeSelecting(
    OldObject, NewObject,
    OldType,   NewType,
    Allowed);
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

function TEpiVProjectTreeViewFrame.GetDocuments(const Index: integer
  ): TEpiDocument;
begin
  Result := nil;

  if (Index < 0) or (Index >= FDocumentList.Count) then
    Exit;

  Result := TEpiDocument(FDocumentList[Index]);
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

  Node := VST.AddChild(ParentNode, NewRelation);
  VST.Expanded[ParentNode] := true;
  UpdateCustomData(NewRelation, Node);

  if ShowCheckBoxes then
  begin
    VST.CheckType[node] := ctTriStateCheckBox;

    case CheckType of
      pctTriState: ; // Do nothing - this is handled perfectly by the VST it-self.
      pctCascade:
        begin
          if VST.CheckState[ParentNode] in [csCheckedNormal, csMixedNormal] then
            VST.CheckState[Node] := csMixedNormal;
        end;
    end;
  end;

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
var
  Node: PVirtualNode;
begin
  if FCheckType = AValue then Exit;
  FCheckType := AValue;

  with VST.TreeOptions do
    case CheckType of
      pctTriState: AutoOptions := AutoOptions + [toAutoTristateTracking];
      pctCascade:  AutoOptions := AutoOptions - [toAutoTristateTracking];
  end;

  Node := VST.GetFirst();
  while Assigned(Node) do
  begin
    VST.CheckState[Node] := csUncheckedNormal;
    Node := VST.GetNext(Node, true);
  end;
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

procedure TEpiVProjectTreeViewFrame.DoDeleteRelation(
  const Relation: TEpiMasterRelation);
begin
  if Assigned(OnDelete) then
    OnDelete(Relation);
end;

procedure TEpiVProjectTreeViewFrame.DoError(const Msg: String);
begin
  if Assigned(OnError) then
    OnError(Msg)
  else
    ShowMessage(Msg);
end;

procedure TEpiVProjectTreeViewFrame.DoGetHint(const AObject: TEpiCustomBase;
  ObjectType: TEpiVTreeNodeObjectType; var HintText: string);
begin
  if Assigned(OnGetHint) then
    OnGetHint(Self, AObject, ObjectType, HintText);
end;

procedure TEpiVProjectTreeViewFrame.DoNewRelation(
  const NewRelation: TEpiMasterRelation);
begin
  if Assigned(OnNewRelation) then
    OnNewRelation(NewRelation);
end;

procedure TEpiVProjectTreeViewFrame.DoTreeNodeSelected(
  const AObject: TEpiCustomBase; ObjectType: TEpiVTreeNodeObjectType);
begin
  if Assigned(OnTreeNodeSelected) then
    OnTreeNodeSelected(Self, AObject, ObjectType);
end;

procedure TEpiVProjectTreeViewFrame.DoTreeNodeSelecting(const OldObject,
  NewObject: TEpiCustomBase; OldObjectType,
  NewObjectType: TEpiVTreeNodeObjectType; var Allowed: Boolean);
begin
  if Assigned(OnTreeNodeSelecting) then
    OnTreeNodeSelecting(Self, OldObject, NewObject, OldObjectType, NewObjectType, Allowed);
end;

end.

