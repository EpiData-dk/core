unit epiv_dataset_viewer_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, FileUtil, Forms, Controls, Grids, ExtCtrls,
  StdCtrls, ActnList, VirtualTrees, epidatafiles, Graphics;

type

  TSelectRecordEvent = procedure(Sender: TObject; RecordNo: Integer;
    Const Field: TEpiField = nil) of object;

  { TDatasetViewerFrame }

  TDatasetViewerFrame = class(TFrame)
    ActionList1: TActionList;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Panel1: TPanel;
    ShowIndexOrAllFieldsAction: TAction;
    ShowValuesOrLabelsAction: TAction;
    SortByIndexAction: TAction;
    procedure ShowIndexOrAllFieldsActionExecute(Sender: TObject);
    procedure ShowIndexOrAllFieldsActionUpdate(Sender: TObject);
    procedure ShowValuesOrLabelsActionExecute(Sender: TObject);
    procedure SortByIndexActionExecute(Sender: TObject);
    procedure SortByIndexActionUpdate(Sender: TObject);
    procedure VLGBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
  private
    FCurrentDisplayField: TEpiFields;
    FDataFile: TEpiDataFile;
    FDisplayFields: TEpiFields;
    FKeyFields: TEpiFields;
    FOnSelectRecord: TSelectRecordEvent;
    FRecords: TBoundArray;
    FShowAllFields: boolean;
    FShowValueLabels: boolean;
    FSortOnIndex: boolean;
    procedure DoMouseClick(ADblClick: boolean);
    procedure DoSelectedRecord(RecordNo: Integer; Const Field: TEpiField);
    function GetKeyFields: TEpiFields;
    procedure SetDisplayFields(AValue: TEpiFields);
    procedure SetKeyFields(AValue: TEpiFields);
    procedure SetOnSelectRecord(AValue: TSelectRecordEvent);
    procedure AssignFields(Const ToFields, FromFields: TEpiFields);
    procedure UpdateFields;
    procedure UpdateGrid;
  private
    { Virtual String Tree }
    FVLG: TVirtualStringTree;
    procedure VLGClick(Sender: TObject);
    procedure VLGDoubleClick(Sender: TObject);
    procedure VLGInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure VLGGetNodeText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure VLGAfterGetMaxColumnWidth(Sender: TVTHeader;
      Column: TColumnIndex; var MaxWidth: Integer);
    procedure VLGCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure VLGHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
  public
    constructor Create(TheOwner: TComponent; Const DataFile: TEpiDataFile);
    destructor  Destroy; override;
    procedure   ShowRecords(const Records: TBoundArray);
    procedure   InitVisual;
    property    KeyFields: TEpiFields read GetKeyFields write SetKeyFields;
    property    DisplayFields: TEpiFields read FDisplayFields write SetDisplayFields;
    property    OnSelectRecord: TSelectRecordEvent read FOnSelectRecord write SetOnSelectRecord;
    property    VLG: TVirtualStringTree read FVLG;
  end;

implementation

uses
  epiglobals, math, LCLIntf,LCLType;

{$R *.lfm}

{ TDatasetViewerFrame }

procedure TDatasetViewerFrame.ShowValuesOrLabelsActionExecute(Sender: TObject);
begin
  FShowValueLabels := not FShowValueLabels;
  UpdateGrid;
end;

procedure TDatasetViewerFrame.ShowIndexOrAllFieldsActionExecute(Sender: TObject);
begin
  FShowAllFields := not FShowAllFields;

  if FShowAllFields or
    (not Assigned(FKeyFields))
  then
    AssignFields(FCurrentDisplayField, FDisplayFields)
  else
    AssignFields(FCurrentDisplayField, FKeyFields);

  UpdateFields;
  UpdateGrid;
end;

procedure TDatasetViewerFrame.ShowIndexOrAllFieldsActionUpdate(Sender: TObject);
begin
  ShowIndexOrAllFieldsAction.Enabled :=
    Assigned(FKeyFields) and (FKeyFields.Count > 0);
end;

procedure TDatasetViewerFrame.SortByIndexActionExecute(Sender: TObject);
begin
  FSortOnIndex := true;
  VLG.Sort(VLG.RootNode, -1, sdAscending);
end;

procedure TDatasetViewerFrame.SortByIndexActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (Assigned(FKeyFields)) and (FKeyFields.Count > 0);
end;

procedure TDatasetViewerFrame.VLGBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  if Column <> 0 then exit;
  if CellPaintMode <> cpmPaint then exit;

  DrawEdge(TargetCanvas.Handle, CellRect, BDR_RAISEDINNER, BF_RECT or BF_MIDDLE);
end;

procedure TDatasetViewerFrame.VLGClick(Sender: TObject);
begin
  DoMouseClick(false);
end;

procedure TDatasetViewerFrame.DoMouseClick(ADblClick: boolean);
var
  HitInfo: THitInfo;
  Node: PVirtualNode;
  RecNo: LongWord;
  P: TPoint;
  Field: TEpiField;
begin
  P := Mouse.CursorPos;
  P := VLG.ScreenToClient(P);

  VLG.GetHitTestInfoAt(P.X, P.Y, True, HitInfo);

  if (HitInfo.HitColumn <> 0) and
     (not ADblClick)
  then
    exit;

  if not (hiOnItem in HitInfo.HitPositions) then exit;

  Node := HitInfo.HitNode;
  RecNo := PCardinal(VLG.GetNodeData(Node))^;
  if HitInfo.HitColumn > 0 then
    Field := FDataFile.Field[HitInfo.HitColumn - 1]
  else
    Field := nil;

  DoSelectedRecord(RecNo, Field);
end;

procedure TDatasetViewerFrame.VLGDoubleClick(Sender: TObject);
var
  P: types.TPoint;
begin
  DoMouseClick(true);
end;

procedure TDatasetViewerFrame.VLGInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  if Assigned(FRecords) then
    PCardinal(Sender.GetNodeData(Node))^ := FRecords[Node^.Index]
  else
    PCardinal(Sender.GetNodeData(Node))^ := Node^.Index;
end;

procedure TDatasetViewerFrame.VLGHeaderClick(Sender: TVTHeader;
  HitInfo: TVTHeaderHitInfo);
begin
  with HitInfo do
  begin
    if Button <> mbLeft then Exit;
    if Shift <> [] then exit;
    FSortOnIndex := false;

    Screen.Cursor := crHourGlass;
    Application.ProcessMessages;

    VLG.Sort(VLG.RootNode, Column, sdAscending);

    Screen.Cursor := crDefault;
    Application.ProcessMessages;
  end;
end;

procedure TDatasetViewerFrame.VLGCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Idx1, Idx2: Cardinal;
  i: Integer;
begin
  Idx1 := PCardinal(Sender.GetNodeData(Node1))^;
  Idx2 := PCardinal(Sender.GetNodeData(Node2))^;
  if Column = 0 then
  begin
    Result := Idx1 - Idx2;
    Exit;
  end;

  if FSortOnIndex then
  begin
    for i := 0 to FKeyFields.Count - 1 do
    begin
      Result := FKeyFields[i].Compare(Idx1, Idx2);
      if Result <> 0 then exit;
    end;
  end
  else if Column > 0 then
    Result := FDataFile.Field[Column-1].Compare(Idx1, Idx2);
end;

procedure TDatasetViewerFrame.VLGAfterGetMaxColumnWidth(Sender: TVTHeader;
  Column: TColumnIndex; var MaxWidth: Integer);
var
  S: String;
  W: Integer;
begin
  S := VLG.Header.Columns[Column].Text;
  W := VLG.Canvas.GetTextWidth(S) + 12;
  MaxWidth := Max(MaxWidth, W);
end;

procedure TDatasetViewerFrame.VLGGetNodeText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  F: TEpiField;
  Idx: Cardinal;
begin
  if Column < 0 then exit;


  Idx := PCardinal(Sender.GetNodeData(Node))^;
  if Column = 0 then
  begin
    CellText := IntToSTr(Idx + 1);
    Exit;
  end;

  F := FDataFile.Field[Column-1];

  if (FShowValueLabels) and
     (Assigned(F.ValueLabelSet)) and
     (not F.IsMissing[Idx])
  then
    CellText := F.ValueLabelSet.ValueLabelString[F.AsValue[Idx]]
  else
    CellText := F.AsString[Idx];
end;

procedure TDatasetViewerFrame.DoSelectedRecord(RecordNo: Integer;
  const Field: TEpiField);
begin
  if assigned(FOnSelectRecord) then
    FOnSelectRecord(Self, RecordNo, Field);
end;

function TDatasetViewerFrame.GetKeyFields: TEpiFields;
begin
  result := FKeyFields;
end;

procedure TDatasetViewerFrame.SetDisplayFields(AValue: TEpiFields);
begin
  if Assigned(AValue) then
    FDisplayFields := AValue;

  AssignFields(FCurrentDisplayField, FDisplayFields);
  UpdateFields;
end;

procedure TDatasetViewerFrame.SetKeyFields(AValue: TEpiFields);
begin
  if Assigned(AValue) then
    FKeyFields := AValue
  else
    FKeyFields := FDataFile.KeyFields;
end;

procedure TDatasetViewerFrame.SetOnSelectRecord(AValue: TSelectRecordEvent);
begin
  if FOnSelectRecord = AValue then Exit;
  FOnSelectRecord := AValue;
end;

procedure TDatasetViewerFrame.AssignFields(const ToFields,
  FromFields: TEpiFields);
var
  i: integer;
begin
  ToFields.Clear;
  for i := 0 to FromFields.Count - 1 do
    ToFields.AddItem(FromFields[i]);
end;

procedure TDatasetViewerFrame.UpdateFields;
var
  i: Integer;
begin
  VLG.Header.Columns.BeginUpdate;
  for i := 0 to FDataFile.Fields.Count - 1 do
  begin
    if not FCurrentDisplayField.ItemExistsByName(FDataFile.Fields[i].Name) then
      VLG.Header.Columns[i+1].Options := VLG.Header.Columns[i+1].Options - [coVisible]
    else
      VLG.Header.Columns[i+1].Options := VLG.Header.Columns[i+1].Options + [coVisible];
  end;
  VLG.Header.Columns.EndUpdate;
  UpdateGrid;
end;

procedure TDatasetViewerFrame.UpdateGrid;
begin
  VLG.InvalidateToBottom(VLG.GetFirstVisible(nil, true));
  VLG.Header.AutoFitColumns(true, smaUseColumnOption);
end;

constructor TDatasetViewerFrame.Create(TheOwner: TComponent;
  const DataFile: TEpiDataFile);
var
  i: Integer;
begin
  inherited Create(TheOwner);
  FDataFile := DataFile;
  FKeyFields := FDataFile.KeyFields;
  FDisplayFields := FDataFile.Fields;
  FCurrentDisplayField := TEpiFields.Create(nil);
  AssignFields(FCurrentDisplayField, FDisplayFields);
  FShowValueLabels := false;
  FShowAllFields := true;
  FSortOnIndex := false;

  FVLG := TVirtualStringTree.Create(Self);
  with VLG do
  begin
    Align := alClient;

    Parent := Self;
    Color := clNone;
    WantTabs := true;
    TabStop := true;
    RootNodeCount := FDataFile.Size;

    // Events:
    NodeDataSize := SizeOf(Cardinal);
    OnInitNode := @VLGInitNode;
    OnGetText := @VLGGetNodeText;
    OnAfterGetMaxColumnWidth := @VLGAfterGetMaxColumnWidth;
    OnBeforeCellPaint := @VLGBeforeCellPaint;
    OnCompareNodes := @VLGCompareNodes;
    OnHeaderClick := @VLGHeaderClick;
    OnClick := @VLGClick;
    OnDblClick := @VLGDoubleClick;
  end;

  with VLG.TreeOptions do
  begin
    AnimationOptions := [];
    AutoOptions := [toAutoScroll];
    MiscOptions := [toGridExtensions, toWheelPanning];
    PaintOptions := [toShowHorzGridLines, toShowVertGridLines, toThemeAware];
    SelectionOptions := [toExtendedFocus, toRightClickSelect, toCenterScrollIntoView];
  end;

  VLG.BeginUpdate;
  with VLG.Header do
  begin
    Options := [hoColumnResize, hoDblClickResize, hoVisible];

    Columns.BeginUpdate;
    with Columns.Add do
    begin
      CaptionAlignment := taLeftJustify;
      Text := 'Record No:';
      Options := [coFixed, coSmartResize, coAllowClick, coEnabled, coParentBidiMode, coResizable, coVisible, coUseCaptionAlignment];
      Width := 50;
      Alignment := taRightJustify;
    end;

    for i := 0 to FDataFile.Fields.Count - 1 do
    with Columns.Add do
    begin
      CaptionAlignment := taLeftJustify;
      Text := FDataFile.Fields[i].Name;
      Options := [coSmartResize, coAllowClick, coEnabled, coParentBidiMode, coParentColor, coResizable, coVisible, coUseCaptionAlignment];
      Width := 100;
      Alignment := taRightJustify;
    end;
    Columns.EndUpdate;

    AutoSizeIndex := -1;
    Height := 25;
    MainColumn := 0;
  end;
  VLG.EndUpdate;
end;

destructor TDatasetViewerFrame.Destroy;
begin
  FCurrentDisplayField.Free;
  inherited Destroy;
end;

procedure TDatasetViewerFrame.ShowRecords(const Records: TBoundArray);
begin
  FRecords := Records;
  if Assigned(FRecords) then
    VLG.RootNodeCount := Length(FRecords)
  else
    VLG.RootNodeCount := FDataFile.Size;

  With VLG do
    ReinitChildren(RootNode, true);
  UpdateGrid;
end;

procedure TDatasetViewerFrame.InitVisual;
begin
  Screen.Cursor := crHourGlass;
  Application.ProcessMessages;

  VLG.RootNodeCount := FDataFile.Size;
  UpdateFields;
  VLG.Sort(VLG.RootNode, 0, sdAscending);

  Screen.Cursor := crDefault;
  Application.ProcessMessages;
end;

end.

