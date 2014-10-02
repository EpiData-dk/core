unit epiv_field_list_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, Buttons, VirtualTrees,
  epidatafiles,
  {$IFDEF MSWINDOWS}
  ActiveX,
  {$ELSE}
  FakeActiveX,
  {$ENDIF}
  Graphics, ActnList;


type

  { TEpiVFieldList }

  TEpiVFieldList = class(TFrame)
    MoveDownAction: TAction;
    MoveUpAction: TAction;
    ActionList1: TActionList;
    MoveDownBtn: TSpeedButton;
    MoveUpBtn: TSpeedButton;
    GrandBtnPanel: TPanel;
    BtnPanel: TPanel;
    procedure MoveActionUpdate(Sender: TObject);
    procedure MoveDownActionExecute(Sender: TObject);
    procedure MoveUpActionExecute(Sender: TObject);
  private
    VST: TVirtualStringTree;
    procedure VSTDragDrop(Sender: TBaseVirtualTree; Source: TObject;
      DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
      const Pt: TPoint; var Effect: LongWord; Mode: TDropMode);
    procedure VSTDragOver(Sender: TBaseVirtualTree; Source: TObject;
      Shift: TShiftState; State: TDragState; const Pt: TPoint; Mode: TDropMode;
      var Effect: LongWord; var Accept: Boolean);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure VSTInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure VSTStartDrag(Sender: TObject; var DragObject: TDragObject);
  private
    function  FieldFromNodeData(Const Node: PVirtualNode): TEpiField;
    procedure LoadGlyphs;
    procedure UpdateDisplayFields;
  public
    constructor Create(TheOwner: TComponent); override;

  { Structural }
  private
    FDisplayFields: TEpiFields;
    function GetCheckedList: TEpiFields;
    procedure SetCheckedList(AValue: TEpiFields);
    procedure SetDisplayFields(AValue: TEpiFields);
  public
    property  CheckedList: TEpiFields read GetCheckedList write SetCheckedList;
    property  DisplayFields: TEpiFields read FDisplayFields write SetDisplayFields;

  { Helping properties }
  private
    function GetCheckedCount: Integer;
  public
    property  CheckedCount: Integer read GetCheckedCount;

  { Options }
  private
    FCheckBoxHeader: string;
    FDragAllowed: Boolean;
    FLocked: Boolean;
    FShowCheckBoxes: Boolean;
    FShowMoveButtons: Boolean;
    procedure SetCheckBoxHeader(AValue: string);
    procedure SetDragAllowed(AValue: Boolean);
    procedure SetLocked(AValue: Boolean);
    procedure SetShowCheckBoxes(AValue: Boolean);
    procedure SetShowMoveButtons(AValue: Boolean);
  public
    property  CheckBoxHeader: string read FCheckBoxHeader write SetCheckBoxHeader;     // Heading string for checkbox column
    property  DragAllowed: Boolean read FDragAllowed write SetDragAllowed;             // Allow drag-drop of fields in the list
    property  Locked: Boolean read FLocked write SetLocked;                            // Lock the list in its current state (applied checks are static, list items cannot be moved, etc.)
    property  ShowCheckBoxes: Boolean read FShowCheckBoxes write SetShowCheckBoxes;    // Show/no-show the checkbox column
    property  ShowMoveButtons: Boolean read FShowMoveButtons write SetShowMoveButtons; // Show/no-show the right panel with move-up/down buttons
  end;

implementation

{$R *.lfm}

uses
  epiv_datamodule, epidatafilestypes;

type

  { TNodeDragObject }

  TNodeDragObject = class(TDragObjectEx)
  private
    FDragNode: PVirtualNode;
  public
    property DragNode: PVirtualNode read FDragNode write FDragNode;
  end;


{ TEpiVFieldList }

procedure TEpiVFieldList.MoveDownActionExecute(Sender: TObject);
var
  TargetNode: PVirtualNode;
  TargetMode: TVTNodeAttachMode;
begin
  TargetNode := VST.FocusedNode^.NextSibling;
  TargetMode := amInsertAfter;

  if not Assigned(TargetNode) then Exit;
  VST.MoveTo(VST.FocusedNode, TargetNode, TargetMode, false);
end;

procedure TEpiVFieldList.MoveActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(VST.FocusedNode);
end;

procedure TEpiVFieldList.MoveUpActionExecute(Sender: TObject);
var
  TargetNode: PVirtualNode;
  TargetMode: TVTNodeAttachMode;
begin
  TargetNode := VST.FocusedNode^.PrevSibling;
  TargetMode := amInsertBefore;

  if not Assigned(TargetNode) then Exit;
  VST.MoveTo(VST.FocusedNode, TargetNode, TargetMode, false);
end;

procedure TEpiVFieldList.VSTDragDrop(Sender: TBaseVirtualTree; Source: TObject;
  DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
  const Pt: TPoint; var Effect: LongWord; Mode: TDropMode);
var
  OldNode: PVirtualNode;
begin
  OldNode := TNodeDragObject(Source).DragNode;
  Sender.MoveTo(OldNode, Sender.DropTargetNode, amInsertBefore, false);
end;

procedure TEpiVFieldList.VSTDragOver(Sender: TBaseVirtualTree; Source: TObject;
  Shift: TShiftState; State: TDragState; const Pt: TPoint; Mode: TDropMode;
  var Effect: LongWord; var Accept: Boolean);
begin
  Accept :=
    // Only allow to drag-drop within ourselves.
    (TNodeDragObject(Source).Control = Sender) and

    // We can not drop onto existing DF's, that would bust the key-field hierachy.
    (Mode = dmOnNode);
end;

procedure TEpiVFieldList.VSTGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  F: TEpiField;
begin
  if Column <> 1 then Exit;
  Ghosted := false;

  F := FieldFromNodeData(Node);
  ImageIndex := DM.GetImageIndex(F.FieldType);
end;

procedure TEpiVFieldList.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  F: TEpiField;
begin
  if TextType <> ttNormal then Exit;

  F := FieldFromNodeData(Node);

  case Column of
    0:  CellText := '';
    1:  CellText := F.Name;
    2:  CellText := F.Question.Text;
  end;
end;

procedure TEpiVFieldList.VSTInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  InitialStates := [];
  Pointer(VST.GetNodeData(Node)^) := FDisplayFields.Field[Node^.Index];

  VST.CheckType[Node] := ctCheckBox
end;

procedure TEpiVFieldList.VSTStartDrag(Sender: TObject;
  var DragObject: TDragObject);
var
  Pt: TPoint;
  Node: PVirtualNode;
begin
  Pt := Mouse.CursorPos;
  Pt := VST.ScreenToClient(Pt);

  Node := VST.GetNodeAt(Pt.X, Pt.Y);
  DragObject := TNodeDragObject.Create(VST);
  TNodeDragObject(DragObject).DragNode := Node;
end;

function TEpiVFieldList.FieldFromNodeData(const Node: PVirtualNode): TEpiField;
begin
  Result := TEpiField(VST.GetNodeData(Node)^);
end;

procedure TEpiVFieldList.LoadGlyphs;
begin
//  DM.Icons16.GetBitmap(35, MoveUpBtn.Glyph);
//  DM.Icons16.GetBitmap(36, MoveDownBtn.Glyph);
end;

procedure TEpiVFieldList.UpdateDisplayFields;
begin
  VST.RootNodeCount := FDisplayFields.Count;
  VST.ReinitChildren(nil, true);
  VST.Invalidate;
end;

constructor TEpiVFieldList.Create(TheOwner: TComponent);
var
  Col: TVirtualTreeColumn;
begin
  inherited Create(TheOwner);

  LoadGlyphs;

  FCheckBoxHeader  := '?';
  FDragAllowed     := true;
  FShowMoveButtons := true;
  FShowCheckBoxes  := true;

  VST := TVirtualStringTree.Create(Self);
  with VST do
  begin
    BeginUpdate;
    RootNodeCount := 0;

    TreeOptions.AnimationOptions := [];
    TreeOptions.AutoOptions      := [];
    TreeOptions.MiscOptions      := [toCheckSupport, toFullRepaintOnResize,
      toGridExtensions, toToggleOnDblClick, toWheelPanning, toFullRowDrag];
    TreeOptions.PaintOptions     := [toThemeAware, toUseBlendedImages];
    TreeOptions.SelectionOptions := [toExtendedFocus, toFullRowSelect];
    TreeOptions.StringOptions    := [];

    with Header do
    begin
      Options  := [hoAutoResize, hoColumnResize, hoDblClickResize, hoShowSortGlyphs, hoVisible];
      Height   := 22;

      with Columns.Add do
      begin
        CheckType := ctCheckBox;
        Options   := [coAllowClick, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus];
        Text      := FCheckBoxHeader;
        Spacing   := 50;
      end;

      with Columns.Add do
      begin
        CheckType := ctNone;
        Options   := [coAllowClick, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus];
        Text      := 'Name';
        Width     := 125;
      end;

      with Columns.Add do
      begin
        CheckType := ctNone;
        Options   := [coAllowClick, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus];
        Text      := 'Caption';
      end;

      MainColumn    := 0;
      AutoSizeIndex := 2;
    end;

    Images          := DM.Icons16;

    NodeDataSize    := SizeOf(Pointer);

    OnGetText       := @VSTGetText;
    OnGetImageIndex := @VSTGetImageIndex;
    OnInitNode      := @VSTInitNode;


    DragType        := dtVCL;
    DragKind        := dkDrag;
    DragMode        := dmAutomatic;
    OnStartDrag     := @VSTStartDrag;
    OnDragOver      := @VSTDragOver;
    OnDragDrop      := @VSTDragDrop;

    Align           := alClient;
    Parent          := Self;
    EndUpdate;
  end;
end;

function TEpiVFieldList.GetCheckedList: TEpiFields;
var
  Node: PVirtualNode;
begin
  Result := nil;
  if not ShowCheckBoxes then exit;

  Result := TEpiFields.Create(nil);

  Node := VST.GetFirstChild(nil);
  while Assigned(Node) do
  begin
    if (VST.CheckType[node] = ctCheckBox) and
       (VST.CheckState[Node] = csCheckedNormal)
    then
      Result.AddItem(FieldFromNodeData(Node));

    Node := VST.GetNext(Node, true);
  end;
end;

procedure TEpiVFieldList.SetCheckedList(AValue: TEpiFields);
var
  Node: PVirtualNode;
  F: TEpiField;
begin
  if not ShowCheckBoxes then exit;
  Node := VST.GetFirstChild(nil);

  while Assigned(Node) do
  begin
    F := FieldFromNodeData(Node);
    if AValue.FieldExists(F) then
      VST.CheckState[Node] := csCheckedNormal
    else
      VST.CheckState[Node] := csUnCheckedNormal;

    Node := VST.GetNext(Node, True);
  end;
end;

procedure TEpiVFieldList.SetDisplayFields(AValue: TEpiFields);
begin
  FDisplayFields := AValue;
  UpdateDisplayFields;
end;

function TEpiVFieldList.GetCheckedCount: Integer;
begin
  Result := VST.CheckedCount;
end;

procedure TEpiVFieldList.SetShowMoveButtons(AValue: Boolean);
begin
  if FShowMoveButtons = AValue then Exit;
  FShowMoveButtons := AValue;

  GrandBtnPanel.Visible := ShowMoveButtons;
end;

procedure TEpiVFieldList.SetShowCheckBoxes(AValue: Boolean);
var
  Node: PVirtualNode;
  Val: TCheckType;
begin
  if FShowCheckBoxes = AValue then Exit;
  FShowCheckBoxes := AValue;

  if ShowCheckBoxes then
    begin
      VST.Header.Columns[0].Options := VST.Header.Columns[0].Options + [coVisible];
      VST.Header.MainColumn := 0;
    end
  else
    begin
      VST.Header.Columns[0].Options := VST.Header.Columns[0].Options - [coVisible];
      VST.Header.MainColumn := 1;
    end;

  if ShowCheckBoxes then
    Val := ctCheckBox
  else
    Val := ctNone;

  Node := VST.GetFirstChild(nil);
  while Assigned(Node) do
  begin
    VST.CheckType[Node] := val;
    Node := VST.GetNext(Node, true);
  end;
end;

procedure TEpiVFieldList.SetCheckBoxHeader(AValue: string);
begin
  if FCheckBoxHeader = AValue then Exit;
  FCheckBoxHeader := AValue;

  VST.Header.Columns[0].Text := CheckBoxHeader;
end;

procedure TEpiVFieldList.SetDragAllowed(AValue: Boolean);
begin
  if FDragAllowed = AValue then Exit;
  FDragAllowed := AValue;

  if DragAllowed then
    begin
      VST.DragMode := dmAutomatic;
      VST.DragType := dtVCL;
    end
  else
    begin
      VST.DragMode := dmManual;
      VST.DragType := dtOLE;
    end;
end;

procedure TEpiVFieldList.SetLocked(AValue: Boolean);
begin
  if FLocked = AValue then Exit;
  FLocked := AValue;

  MoveDownAction.Enabled := not Locked;
  MoveUpAction.Enabled   := not Locked;

  VST.Enabled := not Locked;
end;

end.

