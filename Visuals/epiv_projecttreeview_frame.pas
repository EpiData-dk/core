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
  epidocument, epirelations, epidatafiles;

type

  TProjectDisplayMode = (
    pdmSeperate,          // Display each document with distinct rootnodes
    pdmCommon             // Display only common dataform (structure AND name)
  );


  TProjectTreeSelectDataFile = procedure(Const DataFile: TEpiDataFile) of object;
  TProjectTreeAllowSelectDataFile = procedure(Const OldDataFile,
    NewDataFile: TEpiDataFile; var Allowed: Boolean) of object;

  TProjectTreeError = procedure(Const Msg: String) of object;
  TProjectTreeGetHint = procedure(Const DataFile: TEpiDataFile;
    var HintText: string) of object;

  { TProjectTreeViewFrame }

  TProjectTreeViewFrame = class(TFrame)
    Button1: TButton;
    Panel1: TPanel;
    VST: TVirtualStringTree;

  { VST Methods }
  // TODO: Make private when finish with implementation.
//  private
    procedure Button1Click(Sender: TObject);
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
    FDocumentList: TList;
    function  AllRelationsAreEqual: boolean;
    function  DataFileFromNode(Const Node: PVirtualNode): TEpiDataFile;
    function  MasterRelationFromNode(Const Node: PVirtualNode): TEpiMasterRelation;
    procedure DoError(Const Msg: String);
    procedure DoUpdateTree;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddDocument(Const Doc: TEpiDocument);

  { Options }
  private
    FAllowSelectProject: Boolean;
    FDisplayMode: TProjectDisplayMode;
    FEditCaption: Boolean;
    FEditStructure: Boolean;
    FShowCheckBoxes: Boolean;
    FShowHint: boolean;
    procedure SetAllowSelectProject(AValue: Boolean);
    procedure SetDisplayMode(AValue: TProjectDisplayMode);
    procedure SetEditCaption(AValue: Boolean);
    procedure SetEditStructure(AValue: Boolean);
    procedure SetShowCheckBoxes(AValue: Boolean);
    procedure SetShowHint(AValue: boolean);
  public
    property  AllowSelectProject: Boolean read FAllowSelectProject write SetAllowSelectProject;
    property  DisplayMode: TProjectDisplayMode read FDisplayMode write SetDisplayMode;
    property  EditCaption: Boolean read FEditCaption write SetEditCaption;
    property  EditStructure: Boolean read FEditStructure write SetEditStructure; unimplemented;
    property  ShowCheckBoxes: Boolean read FShowCheckBoxes write SetShowCheckBoxes;
    property  ShowHint: boolean read FShowHint write SetShowHint;

  { Access properties }
  private
    function GetSelectedDataFile: TEpiDataFile;
  public
    property  SelectedDataFile: TEpiDataFile read GetSelectedDataFile;

  { Events }
  private
    FOnDataFileAllowSelect: TProjectTreeAllowSelectDataFile;
    FOnDataFileSelected: TProjectTreeSelectDataFile;
    FOnError: TProjectTreeError;
    FOnGetHint: TProjectTreeGetHint;
  public
    property  OnDataFileSelected: TProjectTreeSelectDataFile read FOnDataFileSelected write FOnDataFileSelected;
    property  OnDataFileAllowSelect: TProjectTreeAllowSelectDataFile read FOnDataFileAllowSelect write FOnDataFileAllowSelect;
//    property  OnDelete:
//    property  OnEdited:
//    property  OnEditing:
//    property  OnEditingEnd:
    property  OnError: TProjectTreeError read FOnError write FOnError;
//    property  OnNew:
    property  OnGetHint: TProjectTreeGetHint read FOnGetHint write FOnGetHint;
  end;

implementation

{$R *.lfm}

uses
  Dialogs;

const
  PROJECTTREE_NODE_CUSTOMKEY = 'PROJECTTREE_NODE_CUSTOMKEY';

type

  { TNodeDragObject }

  TNodeDragObject = class(TDragObjectEx)
  private
    FDragNode: PVirtualNode;
  public
    property DragNode: PVirtualNode read FDragNode write FDragNode;
  end;

{ TProjectTreeViewFrame }

procedure TProjectTreeViewFrame.VSTChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  //
end;

procedure TProjectTreeViewFrame.Button1Click(Sender: TObject);
var
  Doc: TEpiDocument;
  Dlg: TSaveDialog;
begin
  Dlg := TSaveDialog.Create(self);
  Dlg.InitialDir := '/tmp';
  if Dlg.Execute then
  begin
    Doc := TEpiDocument(FDocumentList[0]);
    Doc.SaveToFile(Dlg.FileName);
  end;
  Dlg.Free;
end;

procedure TProjectTreeViewFrame.VSTChecking(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
begin
  //
end;

procedure TProjectTreeViewFrame.VSTDragAllowed(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := FEditStructure;
end;

procedure TProjectTreeViewFrame.VSTDragOver(Sender: TBaseVirtualTree;
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

procedure TProjectTreeViewFrame.VSTDragDrop(Sender: TBaseVirtualTree;
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

  case Mode of
    dmAbove:
      begin
        MR := MasterRelationFromNode(Sender.DropTargetNode);
        NewIndex := MRList.IndexOf(MR);
        Sender.MoveTo(OldNode, Sender.DropTargetNode, amInsertBefore, false);
      end;
    dmBelow:
      begin
        MR := MasterRelationFromNode(Sender.DropTargetNode);
        NewIndex := MRList.IndexOf(MR);
        Sender.MoveTo(OldNode, Sender.DropTargetNode, amInsertAfter, false);
      end;
  end;
  MRList.Move(OldIndex, NewIndex);
end;

procedure TProjectTreeViewFrame.VSTGetHint(Sender: TBaseVirtualTree;
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

procedure TProjectTreeViewFrame.VSTFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  DF: TEpiDataFile;
begin
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

procedure TProjectTreeViewFrame.VSTFocusChanging(Sender: TBaseVirtualTree;
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

procedure TProjectTreeViewFrame.VSTGetText(Sender: TBaseVirtualTree;
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

procedure TProjectTreeViewFrame.VSTNewText(Sender: TBaseVirtualTree;
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

procedure TProjectTreeViewFrame.VSTStartDrag(Sender: TObject;
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

function TProjectTreeViewFrame.AllRelationsAreEqual: boolean;

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

function TProjectTreeViewFrame.DataFileFromNode(const Node: PVirtualNode
  ): TEpiDataFile;
var
  Mr: TEpiMasterRelation;
begin
  Result := nil;

  Mr := MasterRelationFromNode(Node);
  if Assigned(Mr) then
    Result := Mr.Datafile;
end;

function TProjectTreeViewFrame.MasterRelationFromNode(const Node: PVirtualNode
  ): TEpiMasterRelation;
begin
  Result := nil;

  if (Node = nil) then exit;
  if (Node^.Parent = VST.RootNode) then exit;

  Result := TEpiMasterRelation(VST.GetNodeData(Node)^);
end;

procedure TProjectTreeViewFrame.DoError(const Msg: String);
begin
  if Assigned(OnError) then
    OnError(Msg)
  else
    ShowMessage(Msg);
end;

procedure TProjectTreeViewFrame.DoUpdateTree;

  procedure BuildTreeRecursive(Const Parent: PVirtualNode; MR: TEpiMasterRelation);
  var
    i: Integer;
    Node: PVirtualNode;
  begin
    Node := VST.AddChild(Parent, MR);
    MR.AddCustomData(PROJECTTREE_NODE_CUSTOMKEY, TObject(Node));

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
end;

constructor TProjectTreeViewFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  // Objects
  FDocumentList := TList.Create;

  // Options
  FAllowSelectProject := false;
  FDisplayMode        := pdmCommon;
  FEditCaption        := false;
  FEditStructure      := false;
  FShowCheckBoxes     := false;
  FShowHint           := false;

  with VST do
  begin
    NodeDataSize    := SizeOf(Pointer);

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
    OnStartDrag := @VSTStartDrag;

    OnGetHint       := @VSTGetHint;

  end;
end;

destructor TProjectTreeViewFrame.Destroy;
begin
  FDocumentList.Free;
  inherited Destroy;
end;

procedure TProjectTreeViewFrame.AddDocument(const Doc: TEpiDocument);
begin
  FDocumentList.Add(Doc);
  DoUpdateTree;
end;

procedure TProjectTreeViewFrame.SetAllowSelectProject(AValue: Boolean);
begin
  if FAllowSelectProject = AValue then Exit;
  FAllowSelectProject := AValue;
end;

procedure TProjectTreeViewFrame.SetDisplayMode(AValue: TProjectDisplayMode);
begin
  if FDisplayMode = AValue then Exit;
  FDisplayMode := AValue;
  DoUpdateTree;
end;

procedure TProjectTreeViewFrame.SetEditCaption(AValue: Boolean);
begin
  if FEditCaption = AValue then Exit;
  FEditCaption := AValue;

  with VST.TreeOptions do
    if FEditCaption then
      MiscOptions := MiscOptions + [toEditable]
    else
      MiscOptions := MiscOptions - [toEditable];
end;

procedure TProjectTreeViewFrame.SetEditStructure(AValue: Boolean);
begin
  if FEditStructure = AValue then Exit;
  FEditStructure := AValue;

  with VST do
    if FEditStructure then
      DragMode := dmAutomatic
    else
      DragMode := dmManual;
end;

procedure TProjectTreeViewFrame.SetShowCheckBoxes(AValue: Boolean);
begin
  if FShowCheckBoxes = AValue then Exit;
  FShowCheckBoxes := AValue;
  DoUpdateTree;
end;

procedure TProjectTreeViewFrame.SetShowHint(AValue: boolean);
begin
  if FShowHint = AValue then Exit;
  FShowHint := AValue;

  VST.ShowHint := FShowHint;
end;

function TProjectTreeViewFrame.GetSelectedDataFile: TEpiDataFile;
begin
  result := DataFileFromNode(VST.FocusedNode);
end;

end.

