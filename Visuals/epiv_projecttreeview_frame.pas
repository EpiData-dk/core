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
  epicustombase, epidocument, epirelations, epidatafiles, epidatafilestypes,
  Graphics;

type
  TEpiVProjectDisplayMode = (
    pdmSeperate,          // Display each document with distinct rootnodes
    pdmCommon             // Display only common dataform (structure AND name)
  );

  TEpiVProjectCheckType = (
    pctIndividual,        // Allow individual selection of datafiles
    pctTriState,          // Normal tri-state functionality
    pctCascade            // Only allow cascading select -> selecting a Master automatically selects
                          //   details. Details cannot be de-selected.
  );

  TEpiVTreeNodeObjectType = (
    otEmpty,              // No node has been selected
    otFake,               // The node is a fake "root", only possible if DisplayMode = pdmCommon and two project are not equal.
    otRelation,           // The node contains a TEpiMasterRelation
    otProject             // The node is a root node containing a project/document.
  );

  TEpiVTreeNodeChecked = procedure(
          Sender:  TObject;
    Const AObject: TEpiCustomBase;
          ObjectType: TEpiVTreeNodeObjectType;
          Checked: Boolean
  ) of object;

  TEpiVTreeNodeSelected = procedure(
          Sender:  TObject;
    Const AObject: TEpiCustomBase;
          ObjectType: TEpiVTreeNodeObjectType
  ) of object;

  TEpiVTreeNodeEditing = procedure(
          Sender:  TObject;
    Const AObject: TEpiCustomBase;
          ObjectType: TEpiVTreeNodeObjectType;
    var   Allowed: Boolean
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
    var   HintText: string) of object;

  TEpiVProjectTreeGetText = procedure(
          Sender: TObject;
    Const AObject: TEpiCustomBase;
          ObjectType: TEpiVTreeNodeObjectType;
    Const StaticText: boolean;
    var   NodeText: string) of object;

  TEpiVProjectTreeRelationEvent = procedure(Const Relation: TEpiMasterRelation) of object;

  { TEpiVProjectTreeViewFrame }

  TEpiVProjectTreeViewFrame = class(TFrame)
  private
    { VST & Events }
    VST: TVirtualStringTree;
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
    procedure VSTEditCancelled(Sender: TBaseVirtualTree; Column: TColumnIndex);
    procedure VSTEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure VSTEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
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
    procedure VSTPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure VSTStartDrag(Sender: TObject; var DragObject: TDragObject);

  { Document Hooks }
  private
    procedure DocumentHook(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
    procedure TitleChange(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
    procedure DataFileCaptionChange(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);

  { Misc. }
  private
    FUpdatingTree: Boolean;
    FDocumentList: TList;
    FFakeRoot: TEpiCustomItem;
    function  AllRelationsAreEqual: boolean;
    function  CustomBaseFromNode(Const Node: PVirtualNode): TEpiCustomBase;
    function  DataFileFromNode(Const Node: PVirtualNode): TEpiDataFile;
    function  DocumentCountInRange: boolean;
    procedure FocusNode(Const Node: PVirtualNode);
    function  MasterRelationFromNode(Const Node: PVirtualNode): TEpiMasterRelation;
    function  NodeFromCustomBase(Const AObject: TEpiCustomBase): PVirtualNode;
    function  NodeFromDataFile(Const DataFile: TEpiDataFile): PVirtualNode;
    function  NodeFromMasterRelation(Const MasterRelation: TEpiMasterRelation): PVirtualNode;
    procedure ObjectAndType(Const Node: PVirtualNode;
      out Obj: TEpiCustomBase; out ObjType: TEpiVTreeNodeObjectType);
    procedure UpdateCustomData(Const AObject: TEpiCustomBase;
      Const Node: PVirtualNode);
    procedure DoUpdateTree;
    procedure AddHooks(Doc: TEpiDocument);
    procedure RemoveHooks(Doc: TEpiDocument);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  { Structural }
  private
    FMaxDocumentCount: Integer;
    FMinDocumentCount: Integer;
    function GetDocumentCount: Integer;
    function  GetDocuments(const Index: integer): TEpiDocument;
    procedure SetMaxDocumentCount(AValue: Integer);
    procedure SetMinDocumentCount(AValue: Integer);
  public
    procedure AddDocument(Const Doc: TEpiDocument);
    procedure RemoveDocument(Const Doc: TEpiDocument);
    procedure CreateRelation(Const MasterRelation: TEpiMasterRelation);
    procedure DeleteRelation(Relation: TEpiMasterRelation);
  public
    property  DocumentCount: Integer read GetDocumentCount;
    property  Documents[Const Index: integer]: TEpiDocument read GetDocuments;
    // Document Min/Max-Count is only applied when DisplayMode = pdmCommon
    property  MaxDocumentCount: Integer read FMaxDocumentCount write SetMaxDocumentCount; // Set -1 for unlimited
    property  MinDocumentCount: Integer read FMinDocumentCount write SetMinDocumentCount; // Set -1 for unlimited

  { Options }
  private
    FAllowSelectProject: Boolean;
    FCheckType: TEpiVProjectCheckType;
    FDisplayMode: TEpiVProjectDisplayMode;
    FEditCaption: Boolean;
    FEditStructure: Boolean;
    FShowCheckBoxes: Boolean;
    FShowHint: boolean;
    FShowRecordCount: boolean;
    FShowProject: boolean;
  private
    procedure ResetCheckBoxes;
  private
    procedure SetAllowSelectProject(AValue: Boolean);
    procedure SetCheckType(AValue: TEpiVProjectCheckType);
    procedure SetDisplayMode(AValue: TEpiVProjectDisplayMode);
    procedure SetEditCaption(AValue: Boolean);
    procedure SetEditStructure(AValue: Boolean);
    procedure SetShowCheckBoxes(AValue: Boolean);
    procedure SetShowHint(AValue: boolean);
    procedure SetShowRecordCount(AValue: boolean);
    procedure SetShowProject(AValue: boolean);
  public
    property  AllowSelectProject: Boolean read FAllowSelectProject write SetAllowSelectProject;
    property  CheckType: TEpiVProjectCheckType read FCheckType write SetCheckType;
    property  DisplayMode: TEpiVProjectDisplayMode read FDisplayMode write SetDisplayMode;
    property  EditCaption: Boolean read FEditCaption write SetEditCaption;
    property  EditStructure: Boolean read FEditStructure write SetEditStructure;
    property  ShowCheckBoxes: Boolean read FShowCheckBoxes write SetShowCheckBoxes;
    property  ShowHint: boolean read FShowHint write SetShowHint;
    property  ShowRecordCount: boolean read FShowRecordCount write SetShowRecordCount;
    property  ShowProject: boolean read FShowProject write SetShowProject;

  { Option Methods }
  private
    function  GetCheckList: TList;
    procedure SetCheckList(AValue: TList);
  public
    procedure CheckAll;
    procedure CheckNone;
    property  CheckList: TList read GetCheckList write SetCheckList;

  { Access properties }
  private
    function GetSelectedObject: TEpiCustomBase;
    function GetSelectedObjectType: TEpiVTreeNodeObjectType;
    procedure SetSelectedObject(AValue: TEpiCustomBase);
  public
    property SelectedObject: TEpiCustomBase read GetSelectedObject write SetSelectedObject;
    property SelectedObjectType: TEpiVTreeNodeObjectType read GetSelectedObjectType;

  { Events }
  private
    FOnChecked: TEpiVTreeNodeChecked;
    FOnDelete: TEpiVProjectTreeRelationEvent;
    FOnEdited: TEpiVTreeNodeSelected;
    FOnEditing: TEpiVTreeNodeEditing;
    FOnError: TEpiVProjectTreeError;
    FOnGetHint: TEpiVProjectTreeGetHint;
    FOnGetText: TEpiVProjectTreeGetText;
    FOnNewRelation: TEpiVProjectTreeRelationEvent;
    FOnTreeNodeSelected: TEpiVTreeNodeSelected;
    FOnTreeNodeSelecting: TEpiVTreeNodeSelecting;
  protected
    procedure DoChecked(Const AObject: TEpiCustomBase;
      ObjectType: TEpiVTreeNodeObjectType; Checked: Boolean);
    procedure DoDeleteRelation(Const Relation: TEpiMasterRelation); virtual;
    procedure DoEdited(Const AObject: TEpiCustomBase;
      ObjectType: TEpiVTreeNodeObjectType);
    procedure DoEditing(Const AObject: TEpiCustomBase;
      ObjectType: TEpiVTreeNodeObjectType; var Allowed: Boolean);
    procedure DoError(Const Msg: String);
    procedure DoGetHint(Const AObject: TEpiCustomBase;
      ObjectType: TEpiVTreeNodeObjectType; var HintText: string); virtual;
    procedure DoGetText(Const AObject: TEpiCustomBase;
      ObjectType: TEpiVTreeNodeObjectType; Const StaticText: boolean;
      var NodeText: string); virtual;
    procedure DoNewRelation(Const NewRelation: TEpiMasterRelation); virtual;
    procedure DoTreeNodeSelected(Const AObject: TEpiCustomBase;
      ObjectType: TEpiVTreeNodeObjectType); virtual;
    procedure DoTreeNodeSelecting(Const OldObject, NewObject: TEpiCustomBase;
      OldObjectType, NewObjectType: TEpiVTreeNodeObjectType;
      var Allowed: Boolean); virtual;
  public
    property  OnChecked: TEpiVTreeNodeChecked read FOnChecked write FOnChecked;
    property  OnDelete: TEpiVProjectTreeRelationEvent read FOnDelete write FOnDelete;
    property  OnEdited: TEpiVTreeNodeSelected read FOnEdited write FOnEdited;
    property  OnEditing: TEpiVTreeNodeEditing read FOnEditing write FOnEditing;
    property  OnError: TEpiVProjectTreeError read FOnError write FOnError;
    property  OnGetHint: TEpiVProjectTreeGetHint read FOnGetHint write FOnGetHint;
    property  OnGetText: TEpiVProjectTreeGetText read FOnGetText write FOnGetText;
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

  { TFakeCustomItem }

  TFakeCustomItem = class(TEpiCustomItem)
  public
    constructor Create(AOwner: TEpiCustomBase); override;
  end;

{ TFakeCustomItem }

constructor TFakeCustomItem.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
end;

{ TEpiVProjectTreeViewFrame }

procedure TEpiVProjectTreeViewFrame.VSTChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  CNode: PVirtualNode;
  Obj: TEpiCustomBase;
  ObjT: TEpiVTreeNodeObjectType;
begin
  ObjectAndType(Node, Obj, ObjT);

  DoChecked(Obj, ObjT, VST.CheckState[Node] in [csCheckedNormal, csMixedNormal]);

  if (CheckType <> pctCascade) then exit;

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
  if (CheckType <> pctCascade) then
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
var
  O: TEpiCustomBase;
  OT: TEpiVTreeNodeObjectType;
begin
  ObjectAndType(Node, O, OT);

  Allowed :=
    (OT = otRelation) and
    (EditStructure);
end;

procedure TEpiVProjectTreeViewFrame.VSTDragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; const Pt: TPoint;
  Mode: TDropMode; var Effect: LongWord; var Accept: Boolean);
begin
  Accept :=
    // Only allow to drag-drop within ourselves.
    (TNodeDragObject(Source).Control = Sender) and

    // We can not drop onto existing DF's, that would bust the key-field hierachy.
    (Mode in [dmAbove, dmBelow]) and

    // -same goes for out-of-parent experiences... ;)
    (TNodeDragObject(Source).DragNode^.Parent = Sender.DropTargetNode^.Parent)
    ;
end;

procedure TEpiVProjectTreeViewFrame.VSTEditCancelled(Sender: TBaseVirtualTree;
  Column: TColumnIndex);
var
  AObject: TEpiCustomBase;
  ObjectType: TEpiVTreeNodeObjectType;
begin
  ObjectAndType(VST.FocusedNode, AObject, ObjectType);
  DoEdited(AObject, ObjectType);
end;

procedure TEpiVProjectTreeViewFrame.VSTEdited(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  AObject: TEpiCustomBase;
  ObjectType: TEpiVTreeNodeObjectType;
begin
  ObjectAndType(Node, AObject, ObjectType);
  DoEdited(AObject, ObjectType);
end;

procedure TEpiVProjectTreeViewFrame.VSTEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
var
  AObject: TEpiCustomBase;
  ObjectType: TEpiVTreeNodeObjectType;
begin
  ObjectAndType(Node, AObject, ObjectType);
  DoEditing(AObject, ObjectType, Allowed);
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
  ObjectAndType(Node, O, Ot);

  // Hinttext should default to the node text, hence call GetText.
  HintText := VST.Text[Node, Column];

  DoGetHint(O, Ot, HintText);
end;

procedure TEpiVProjectTreeViewFrame.VSTFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  O: TEpiCustomBase;
  OT: TEpiVTreeNodeObjectType;
begin
  if not Assigned(Node) then exit;

  ObjectAndType(Node, O, OT);
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
  ObjectAndType(OldNode, OldObject, OldType);
  ObjectAndType(NewNode, NewObject, NewType);

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
  // If the tree is updating (rebuilding itself) then do NOT delete
  // node data. This should only happen on an explicit deletion of
  // the node.
  if FUpdatingTree then
    Exit;

  if not Assigned(Node) then exit;
  if Node^.Parent = VST.RootNode then exit;

  Relation := MasterRelationFromNode(Node);
  DataFile := DataFileFromNode(Node);

  DoDeleteRelation(Relation);

  DataFile.Caption.UnRegisterOnChangeHook(@DataFileCaptionChange);
  DataFile.Free;
end;

procedure TEpiVProjectTreeViewFrame.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  Obj: TEpiCustomBase;
  Doc: TEpiDocument absolute Obj;
  MR:  TEpiMasterRelation absolute Obj;
  ObjType: TEpiVTreeNodeObjectType;
begin
  ObjectAndType(Node, Obj, ObjType);

  case TextType of
    ttNormal:
      case ObjType of
        otEmpty:
          CellText := '';

        otFake:
          if DocumentCountInRange then
            CellText := 'No Common Projects!'
          else
            CellTExt := 'Incorrect number of documents';

        otRelation:
          if Mr.Datafile.Caption.Text = '' then
            CellText := '(untitled)'
          else
            CellText := Mr.Datafile.Caption.Text;

        otProject:
          CellText := Doc.Study.Title.Text
      end;

    ttStatic:
      case ObjType of
        otEmpty:
          CellText := '';

        otFake:
          CellText := '';

        otRelation:
          CellText := Format('[%d]', [TEpiMasterRelation(Obj).Datafile.Size]);

        otProject:
          CellText := '';
      end;
  end;

  DoGetText(Obj, ObjType, TextType = ttStatic, CellText);
end;

procedure TEpiVProjectTreeViewFrame.VSTNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; const NewText: String);
var
  DF: TEpiDataFile;
  O: TEpiCustomBase;
  Ot: TEpiVTreeNodeObjectType;
  Doc: TEpiDocument;
begin
  ObjectAndType(Node, O, Ot);

  case ot of
    otEmpty:
      Exit;

    otFake:
      Exit;

    otRelation:
      begin
        DF := TEpiMasterRelation(O).Datafile;

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

    otProject:
      begin
        Doc := TEpiDocument(O);

        if Assigned(Doc) then
        begin
          if NewText = '' then
          begin
            DoError('Project Title cannot be empty!');
            Exit;
          end;

          Doc.Study.Title.Text := NewText;
        end;
      end;
  end;
end;

procedure TEpiVProjectTreeViewFrame.VSTPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  case TextType of
    ttNormal:
      TargetCanvas.Font.Color := clDefault;
    ttStatic:
      TargetCanvas.Font.Color := clBlue;
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

procedure TEpiVProjectTreeViewFrame.DocumentHook(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
begin
  if not (Initiator is TEpiDocument) then exit;
  if (EventGroup <> eegCustomBase) then exit;
  if (TEpiCustomChangeEventType(EventType) <> ecceDestroy) then exit;

  FDocumentList.Remove(Initiator);
  Initiator.UnRegisterOnChangeHook(@DocumentHook);

  DoUpdateTree;
end;

procedure TEpiVProjectTreeViewFrame.TitleChange(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
var
  Node: PVirtualNode;
begin
  if (EventGroup <> eegCustomBase) then exit;
  if (TEpiCustomChangeEventType(EventType) <> ecceText) then exit;

  Node := NodeFromCustomBase(Sender.RootOwner);

  if Assigned(Node) then
    VST.InvalidateNode(Node);
end;

procedure TEpiVProjectTreeViewFrame.DataFileCaptionChange(
  const Sender: TEpiCustomBase; const Initiator: TEpiCustomBase;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
var
  Node: PVirtualNode;
begin
  if (EventGroup <> eegCustomBase) then exit;
  if (TEpiCustomChangeEventType(EventType) <> ecceText) then exit;

  if (Sender is TEpiTranslatedText) then
    Node := NodeFromCustomBase(Sender.Owner);

  if Assigned(Node) then
    VST.InvalidateNode(Node);
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

function TEpiVProjectTreeViewFrame.CustomBaseFromNode(const Node: PVirtualNode
  ): TEpiCustomBase;
begin
  Result := nil;
  if (Node = nil) then exit;

  if Assigned(Node) then
    result := TEpiCustomBase(VST.GetNodeData(Node)^);
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

function TEpiVProjectTreeViewFrame.DocumentCountInRange: boolean;
begin
  result :=
    (FDocumentList.Count >= MinDocumentCount) and
    (FDocumentList.Count <= MaxDocumentCount);
end;

procedure TEpiVProjectTreeViewFrame.FocusNode(const Node: PVirtualNode);
begin
  VST.FocusedNode := Node;
  VST.Selected[Node] := true;
end;

function TEpiVProjectTreeViewFrame.MasterRelationFromNode(const Node: PVirtualNode
  ): TEpiMasterRelation;
var
  O: TEpiCustomBase;
  Ot: TEpiVTreeNodeObjectType;
begin
  Result := nil;

  ObjectAndType(Node, O, Ot);
  if Ot = otRelation then
    Result := TEpiMasterRelation(O);
end;

function TEpiVProjectTreeViewFrame.NodeFromCustomBase(
  const AObject: TEpiCustomBase): PVirtualNode;
begin
  Result := nil;

  if Assigned(AObject) then
    Result := PVirtualNode(AObject.FindCustomData(PROJECTTREE_NODE_CUSTOMKEY));
end;

function TEpiVProjectTreeViewFrame.NodeFromDataFile(const DataFile: TEpiDataFile
  ): PVirtualNode;
begin
  result := NodeFromCustomBase(DataFile);
end;

function TEpiVProjectTreeViewFrame.NodeFromMasterRelation(
  const MasterRelation: TEpiMasterRelation): PVirtualNode;
begin
  result := NodeFromCustomBase(MasterRelation);
end;

procedure TEpiVProjectTreeViewFrame.ObjectAndType(const Node: PVirtualNode; out
  Obj: TEpiCustomBase; out ObjType: TEpiVTreeNodeObjectType);
begin
  Obj := CustomBaseFromNode(Node);
  ObjType := otEmpty;

  if Obj = FFakeRoot then
    ObjType := otFake;

  if Obj is TEpiDocument then
    ObjType := otProject;

  if Obj is TEpiMasterRelation then
    ObjType := otRelation;
end;

procedure TEpiVProjectTreeViewFrame.UpdateCustomData(
  const AObject: TEpiCustomBase; const Node: PVirtualNode);
begin
  AObject.AddCustomData(PROJECTTREE_NODE_CUSTOMKEY, TObject(Node));
end;

procedure TEpiVProjectTreeViewFrame.DoUpdateTree;

  procedure BuildTreeRecursive(Const Parent: PVirtualNode; MR: TEpiMasterRelation);
  var
    i: Integer;
    Node: PVirtualNode;
  begin
    Node := VST.AddChild(Parent, MR);

    UpdateCustomData(MR, Node);

    for i := 0 to MR.DetailRelations.Count - 1 do
      BuildTreeRecursive(Node, MR.DetailRelation[i]);
  end;

  procedure BuildDocumentTree(Const Parent: PVirtualNode; Doc: TEpiDocument);
  var
    i: Integer;
    Node: PVirtualNode;
  begin
    if ShowProject then
    begin
      Node := VST.AddChild(Parent, DOC);

      UpdateCustomData(Doc, Node);
    end else
      Node := Parent;

    for i := 0 to Doc.Relations.Count - 1 do
      BuildTreeRecursive(Node, Doc.Relations.MasterRelation[i]);
  end;

var
  i: Integer;
  OldSelectedObject: TEpiCustomItem;
  Node: PVirtualNode;
begin
  FUpdatingTree := true;

  OldSelectedObject := nil;
  if Assigned(VST.FocusedNode) then
    OldSelectedObject := TEpiCustomItem(VST.GetNodeData(VST.FocusedNode)^);

  VST.BeginUpdate;
  VST.Clear;

  if FDocumentList.Count > 0 then
  begin
    if DisplayMode = pdmSeperate then
      for i := 0 to FDocumentList.Count -1 do
        BuildDocumentTree(VST.RootNode, TEpiDocument(FDocumentList[i]))
    else
    if AllRelationsAreEqual and
       DocumentCountInRange
    then
      BuildDocumentTree(VST.RootNode, TEpiDocument(FDocumentList[0]))
    else
      VST.AddChild(VST.RootNode, FFakeRoot);

    VST.FullExpand();


    if Assigned(OldSelectedObject) and
       Assigned(NodeFromCustomBase(OldSelectedObject))
    then
      FocusNode(NodeFromCustomBase(OldSelectedObject))
    else
      begin
        if (ShowProject and AllowSelectProject) or
           (not ShowProject)
        then
          FocusNode(VST.GetFirst())
        else
          FocusNode(VST.GetNext(VST.GetFirst()));
      end;
  end;
  ResetCheckBoxes;

  VST.EndUpdate;

  FUpdatingTree := false;
end;

procedure TEpiVProjectTreeViewFrame.AddHooks(Doc: TEpiDocument);
var
  DF: TEpiDataFile;
begin
  Doc.RegisterOnChangeHook(@DocumentHook, true);
  Doc.Study.Title.RegisterOnChangeHook(@TitleChange, true);

  for DF in Doc.DataFiles do
    DF.Caption.RegisterOnChangeHook(@DataFileCaptionChange, true);
end;

procedure TEpiVProjectTreeViewFrame.RemoveHooks(Doc: TEpiDocument);
var
  DF: TEpiDataFile;
begin
  for DF in Doc.DataFiles do
    DF.Caption.UnRegisterOnChangeHook(@DataFileCaptionChange);

  Doc.Study.Title.UnRegisterOnChangeHook(@TitleChange);
end;

constructor TEpiVProjectTreeViewFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  // Document properties:
  FMinDocumentCount := -1;
  FMaxDocumentCount := -1;

  // Objects
  FDocumentList := TList.Create;
  FUpdatingTree := false;
  FFakeRoot     := TFakeCustomItem.Create(nil);

  // Options
  FAllowSelectProject := true;
  FCheckType          := pctTriState;
  FDisplayMode        := pdmCommon;
  FEditCaption        := false;
  FEditStructure      := false;
  FShowCheckBoxes     := false;
  FShowHint           := false;
  FShowProject        := true;

  VST := TVirtualStringTree.Create(Self);
  with VST do
  begin
    NodeDataSize    := SizeOf(Pointer);

    with TreeOptions do
    begin
      AnimationOptions := [];
      AutoOptions      := [toAutoDropExpand, toAutoScrollOnExpand, toAutoDeleteMovedNodes, toAutoTristateTracking];
      MiscOptions      := [toCheckSupport, toFullRepaintOnResize, toInitOnSave, toWheelPanning, toEditOnDblClick];
      PaintOptions     := [toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages];
      SelectionOptions := [toFullRowSelect];
      StringOptions    := [toAutoAcceptEditChange];
    end;

    OnGetText       := @VSTGetText;
    OnNewText       := @VSTNewText;
    OnPaintText     := @VSTPaintText;
    OnEdited        := @VSTEdited;
    OnEditing       := @VSTEditing;
    OnEditCancelled := @VSTEditCancelled;

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

    Align           := alClient;
    Parent          := Self;
    EditDelay       := 0;
  end;
end;

destructor TEpiVProjectTreeViewFrame.Destroy;
var
  i: Integer;
begin
  for i := 0 to FDocumentList.Count -1 do
    RemoveHooks(TEpiDocument(FDocumentList[i]));

  FFakeRoot.Free;
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

function TEpiVProjectTreeViewFrame.GetDocumentCount: Integer;
begin
  result := FDocumentList.Count;
end;

procedure TEpiVProjectTreeViewFrame.SetMaxDocumentCount(AValue: Integer);
begin
  if FMaxDocumentCount = AValue then Exit;
  FMaxDocumentCount := AValue;

  DoUpdateTree;
end;

procedure TEpiVProjectTreeViewFrame.SetMinDocumentCount(AValue: Integer);
begin
  if FMinDocumentCount = AValue then Exit;
  FMinDocumentCount := AValue;

  DoUpdateTree;
end;

procedure TEpiVProjectTreeViewFrame.AddDocument(const Doc: TEpiDocument);
begin
  FDocumentList.Add(Doc);
  DoUpdateTree;
  AddHooks(Doc);
end;

procedure TEpiVProjectTreeViewFrame.RemoveDocument(const Doc: TEpiDocument);
begin
  RemoveHooks(Doc);
  FDocumentList.Remove(Doc);
  DoUpdateTree;
end;

procedure TEpiVProjectTreeViewFrame.CreateRelation(
  const MasterRelation: TEpiMasterRelation);
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

  if Assigned(MasterRelation) then
  begin
    ParentNode := NodeFromMasterRelation(MasterRelation);
    NewRelation := MasterRelation.NewDetailRelation;
    NewDataFile := TEpiDataFiles(MasterRelation.Datafile.Owner).NewDataFile;
  end else begin
    // TODO: How to handle creating new datafile which is a true "master". Hence
    //       we need information on which document we insert into!
    ParentNode  := NodeFromCustomBase(TEpiDocument(FDocumentList[0]));
    NewRelation := TEpiDocument(FDocumentList[0]).Relations.NewMasterRelation;
    NewDataFile := TEpiDocument(FDocumentList[0]).DataFiles.NewDataFile;
  end;

  NewDataFile.Caption.RegisterOnChangeHook(@DataFileCaptionChange, true);
  NewRelation.Datafile := NewDataFile;

  if Assigned(MasterRelation) then
    for ParentKeyField in MasterRelation.Datafile.KeyFields do
    begin
      // In a related datafile, the "primary" key cannot contain autoinc - it would
      // screw up the numbering.
      Ft := ParentKeyField.FieldType;
      if Ft = ftAutoInc then Ft := ftInteger;

      NewKeyField := NewDataFile.NewField(Ft);
      NewKeyField.Assign(ParentKeyField);
      NewKeyField.EntryMode := emNoEnter;
      NewDataFile.KeyFields.AddItem(NewKeyField);
    end;

  Node := VST.AddChild(ParentNode, NewRelation);
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

  // Expand node at the end, as this will cause an update/initialization of
  // internal node data.
  VST.Expanded[ParentNode] := true;
end;

procedure TEpiVProjectTreeViewFrame.DeleteRelation(Relation: TEpiMasterRelation
  );
var
  Node: PVirtualNode;
begin
  if not EditStructure then exit;
  if not Assigned(Relation) then exit;

  Node := NodeFromMasterRelation(Relation);
  VST.DeleteNode(Node);
end;

procedure TEpiVProjectTreeViewFrame.ResetCheckBoxes;
var
  Node: PVirtualNode;
  Val: TCheckType;
  Obj: TEpiCustomBase;
  ObjT: TEpiVTreeNodeObjectType;
begin
  if ShowCheckBoxes then
    if CheckType = pctIndividual then
      Val := ctCheckBox
    else
      Val := ctTriStateCheckBox
  else
    Val := ctNone;

  Node := VST.GetFirst();
  while Assigned(Node) do
  begin
    ObjectAndType(Node, Obj, ObjT);

    if (ObjT = otProject) and
       (not AllowSelectProject)
    then
      VST.CheckType[Node] := ctNone
    else
      VST.CheckType[Node] := Val;

    VST.CheckState[Node] := csUncheckedNormal;
    Node := VST.GetNext(Node, true);
  end;
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
      pctTriState:
        AutoOptions := AutoOptions + [toAutoTristateTracking];
      pctCascade,
      pctIndividual:
        AutoOptions := AutoOptions - [toAutoTristateTracking];
  end;

  ResetCheckBoxes;
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
begin
  if FShowCheckBoxes = AValue then Exit;
  FShowCheckBoxes := AValue;

  ResetCheckBoxes;
end;

procedure TEpiVProjectTreeViewFrame.SetShowHint(AValue: boolean);
begin
  if FShowHint = AValue then Exit;
  FShowHint := AValue;

  VST.ShowHint := FShowHint;
end;

procedure TEpiVProjectTreeViewFrame.SetShowRecordCount(AValue: boolean);
begin
  if FShowRecordCount = AValue then Exit;
  FShowRecordCount := AValue;

  with VST.TreeOptions do
    if FShowRecordCount then
      StringOptions := StringOptions + [toShowStaticText]
    else
      StringOptions := StringOptions - [toShowStaticText];

  VST.Invalidate;
end;

procedure TEpiVProjectTreeViewFrame.SetShowProject(AValue: boolean);
begin
  if FShowProject = AValue then Exit;
  FShowProject := AValue;

  DoUpdateTree;
end;

function TEpiVProjectTreeViewFrame.GetCheckList: TList;
var
  Node: PVirtualNode;
begin
  Node := VST.GetFirstChild(nil);

  Result := TList.Create;
  while Assigned(Node) do
  begin
    if VST.CheckState[Node] in [csCheckedNormal, csMixedNormal] then
      Result.Add(CustomBaseFromNode(Node));

    Node := VST.GetNext(Node, True);
  end;
end;

procedure TEpiVProjectTreeViewFrame.SetCheckList(AValue: TList);
var
  Node: PVirtualNode;
  CB: TEpiCustomBase;
begin
  Node := VST.GetFirstChild(nil);

  while Assigned(Node) do
  begin
    CB := CustomBaseFromNode(Node);
    if AValue.IndexOf(CB) >= 0 then
      VST.CheckState[Node] := csCheckedNormal;

    Node := VST.GetNext(Node, True);
  end;
end;

procedure TEpiVProjectTreeViewFrame.CheckAll;
var
  Node: PVirtualNode;
  Obj: TEpiCustomBase;
  ObjT: TEpiVTreeNodeObjectType;
  ChildNode: PVirtualNode;
begin
  if not ShowCheckBoxes then exit;

  Node := VST.GetFirstChild(Nil);
  case CheckType of
    pctIndividual:
      while Assigned(Node) do
      begin
        ObjectAndType(Node, Obj, ObjT);

        if (ObjT = otProject) AND
           (not AllowSelectProject)
        then
          VST.CheckState[Node] := csUncheckedNormal
        else
          VST.CheckState[Node] := csCheckedNormal;

        Node := VST.GetNext(Node, true);
      end;

    pctTriState,
    pctCascade:
      begin
        if (not ShowProject) or
           (AllowSelectProject and ShowProject)
        then
          begin
            VST.CheckState[Node] := csCheckedNormal;
            Exit;
          end;

        // NODE = Project Node
        while Assigned(Node) do
        begin
          VST.CheckState[Node] := csUncheckedNormal;
          ChildNode := VST.GetFirstChild(Node);

          // ChildNode = DataFile / MasterRelation
          while Assigned(ChildNode) do
          begin
            VST.CheckState[ChildNode] := csCheckedNormal;
            ChildNode := VST.GetNextSibling(ChildNode);
          end;

          Node := VST.GetNextSibling(Node);
        end;
      end;
  end;
end;

procedure TEpiVProjectTreeViewFrame.CheckNone;
var
  Node: PVirtualNode;
begin
  if not ShowCheckBoxes then exit;

  Node := VST.GetFirst(true);
  case CheckType of
    pctIndividual,
    pctTriState,
    pctCascade:
      while Assigned(Node) do
      begin
        VST.CheckState[Node] := csUncheckedNormal;
        Node := VST.GetNext(Node, true);
      end;
  end;
end;

function TEpiVProjectTreeViewFrame.GetSelectedObject: TEpiCustomBase;
begin
  result := CustomBaseFromNode(VST.FocusedNode);
end;

function TEpiVProjectTreeViewFrame.GetSelectedObjectType: TEpiVTreeNodeObjectType;
var
  O: TEpiCustomBase;
begin
  ObjectAndType(VST.FocusedNode, O, Result);
end;

procedure TEpiVProjectTreeViewFrame.SetSelectedObject(AValue: TEpiCustomBase);
var
  Node: PVirtualNode;
begin
  Node := NodeFromCustomBase(AValue);

  if not Assigned(Node) then exit;

  FocusNode(Node);
end;

procedure TEpiVProjectTreeViewFrame.DoChecked(const AObject: TEpiCustomBase;
  ObjectType: TEpiVTreeNodeObjectType; Checked: Boolean);
begin
  if Assigned(OnChecked) then
    OnChecked(Self, AObject, ObjectType, Checked);
end;

procedure TEpiVProjectTreeViewFrame.DoDeleteRelation(
  const Relation: TEpiMasterRelation);
begin
  if Assigned(OnDelete) then
    OnDelete(Relation);
end;

procedure TEpiVProjectTreeViewFrame.DoEdited(const AObject: TEpiCustomBase;
  ObjectType: TEpiVTreeNodeObjectType);
begin
  if Assigned(OnEdited) then
    OnEdited(Self, AObject, ObjectType);
end;

procedure TEpiVProjectTreeViewFrame.DoEditing(const AObject: TEpiCustomBase;
  ObjectType: TEpiVTreeNodeObjectType; var Allowed: Boolean);
begin
  if Assigned(OnEditing) then
    OnEditing(Self, AObject, ObjectType, Allowed);
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

procedure TEpiVProjectTreeViewFrame.DoGetText(const AObject: TEpiCustomBase;
  ObjectType: TEpiVTreeNodeObjectType; const StaticText: boolean;
  var NodeText: string);
begin
  if Assigned(OnGetText) then
    OnGetText(Self, AObject, ObjectType, StaticText, NodeText);
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

