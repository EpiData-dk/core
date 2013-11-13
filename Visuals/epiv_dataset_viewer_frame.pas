unit epiv_dataset_viewer_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Grids, ExtCtrls, StdCtrls,
  ActnList, VirtualTrees, epidatafiles;

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
    procedure ListGridDblClick(Sender: TObject);
    procedure ListGridHeaderClick(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure ListGridPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure ShowIndexOrAllFieldsActionExecute(Sender: TObject);
    procedure ShowIndexOrAllFieldsActionUpdate(Sender: TObject);
    procedure ShowValuesOrLabelsActionExecute(Sender: TObject);
    procedure SortByIndexActionExecute(Sender: TObject);
    procedure SortByIndexActionUpdate(Sender: TObject);
  private
    FCurrentDisplayField: TEpiFields;
    FDataFile: TEpiDataFile;
    FDisplayFields: TEpiFields;
    FKeyFields: TEpiFields;
    FOnSelectRecord: TSelectRecordEvent;
    FRecords: TBoundArray;
    FShowAllFields: boolean;
    FShowValueLabels: boolean;
    FSortCol: integer;
    procedure DoSelectedRecord(RecordNo: Integer; Const Field: TEpiField);
    function GetKeyFields: TEpiFields;
    procedure SetDisplayFields(AValue: TEpiFields);
    procedure SetKeyFields(AValue: TEpiFields);
    procedure SetOnSelectRecord(AValue: TSelectRecordEvent);
    procedure AssignFields(Const ToFields, FromFields: TEpiFields);
    procedure UpdateFields;
    procedure UpdateGrid;
    procedure GridColumnSort(Sender: TObject; ACol, ARow, BCol,
      BRow: Integer; var Result: integer);
    procedure  GridIndexSort(Sender: TObject; ACol, ARow, BCol,
      BRow: Integer; var Result: integer);
  private
    { Virtual String Tree }
    FVLG: TVirtualStringTree;
    procedure VLGGetNodeText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure VLGAfterGetMaxColumnWidth(Sender: TVTHeader;
      Column: TColumnIndex; var MaxWidth: Integer);
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
  epiglobals, math, Graphics;

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
  UpdateFields;
  UpdateGrid;
end;

procedure TDatasetViewerFrame.ShowIndexOrAllFieldsActionUpdate(Sender: TObject);
begin
  ShowIndexOrAllFieldsAction.Enabled :=
    Assigned(FKeyFields) and (FKeyFields.Count > 0);
end;

procedure TDatasetViewerFrame.ListGridHeaderClick(Sender: TObject;
  IsColumn: Boolean; Index: Integer);
var
  SelectedRecordNo: Integer;
begin
  if Index < 0 then exit;

  if IsColumn then
  begin
    // If click on column -> then sort.

    if FSortCol = Index then exit;

    FSortCol := Index;
//    ListGrid.OnCompareCells := @GridColumnSort;
//    ListGrid.SortColRow(true, FSortCol);
  end else begin
    // If click on row -> then notify a "jump to record"

//    SelectedRecordNo := StrToInt(ListGrid.Cells[0, Index]) - 1;
    DoSelectedRecord(SelectedRecordNo, nil);
  end;
end;

procedure TDatasetViewerFrame.ListGridDblClick(Sender: TObject);
var
  P: TPoint;
  SelectedRecordNo: Integer;
  SelectedField: TEpiField;
begin
//  P := ListGrid.MouseToCell(ListGrid.ScreenToClient(Mouse.CursorPos));
  if P.Y <= 0 then exit;
  if P.X <= 0 then exit;

//  SelectedRecordNo := StrToInt(ListGrid.Cells[0, P.Y]) - 1;
  SelectedField := FCurrentDisplayField[P.X - 1];
  DoSelectedRecord(SelectedRecordNo, SelectedField);
end;

procedure TDatasetViewerFrame.ListGridPrepareCanvas(sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
begin
//  if (aRow = 0) and (aCol = FSortCol) then
//    ListGrid.Canvas.Brush.Color := clSkyBlue;
end;

procedure TDatasetViewerFrame.SortByIndexActionExecute(Sender: TObject);
begin
{  if FSortCol = -1 then exit;

  FSortCol := -1;
  ListGrid.OnCompareCells := @GridIndexSort;
  ListGrid.SortColRow(true, 0);   }
end;

procedure TDatasetViewerFrame.SortByIndexActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (Assigned(FKeyFields)) and (FKeyFields.Count > 0);
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
begin
  if Column < 0 then exit;

  if Column = 0 then
  begin
    CellText := IntToSTr(Node^.Index + 1);
    Exit;
  end;

  F := FCurrentDisplayField[Column-1];

  if (FShowValueLabels) and
     (Assigned(F.ValueLabelSet))
  then
    CellText := F.ValueLabelSet.ValueLabelString[F.AsValue[Node^.Index]]
  else
    CellText := F.AsString[Node^.Index];
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

{  with ListGrid do
    if Assigned(FOnSelectRecord) then
    begin
      HeaderHotZones  := HeaderHotZones + [gzFixedRows];
      HeaderPushZones := HeaderPushZones + [gzFixedRows];
    end else begin
      HeaderHotZones  := HeaderHotZones - [gzFixedRows];
      HeaderPushZones := HeaderPushZones - [gzFixedRows];
    end;  }
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
  VSTHeader: TVTHeader;
  F: TEpiField;
  i: Integer;
begin
  FVLG.BeginUpdate;
  with FVLG.Header do
  begin
    Options := [hoColumnResize, hoDblClickResize, hoVisible];

    Columns.BeginUpdate;
    Columns.Clear;
    with Columns.Add do
    begin
      Text := 'Record No:';
      Options := [coSmartResize, coAllowClick, coEnabled, coParentBidiMode, coParentColor, coResizable, coVisible];
      Width := 50;
      Alignment := taRightJustify;
    end;

    for i := 0 to FDisplayFields.Count - 1 do
    with Columns.Add do
    begin
      Text := FDisplayFields[i].Name;
      Options := [coSmartResize, coAllowClick, coEnabled, coParentBidiMode, coParentColor, coResizable, coVisible];
      Width := 100;
      Alignment := taRightJustify;
    end;
    Columns.EndUpdate;

    AutoSizeIndex := -1;
    Height := 25;
    MainColumn := 0;
  end;
  FVLG.EndUpdate;
end;

procedure TDatasetViewerFrame.UpdateGrid;
begin
  VLG.InvalidateToBottom(VLG.GetFirstVisible(nil, true));
  VLG.Header.AutoFitColumns(true, smaUseColumnOption);
end;

procedure TDatasetViewerFrame.GridColumnSort(Sender: TObject; ACol, ARow, BCol,
  BRow: Integer; var Result: integer);
begin
  if ACol <> BCol then exit;

//  ARow := StrToInt(ListGrid.Cells[0, ARow]) - 1;
//  BRow := StrToInt(ListGrid.Cells[0, BRow]) - 1;

  if ACol = 0 then
    result := ARow - BRow
  else
    result := FCurrentDisplayField[ACol - 1].Compare(ARow, BRow);
end;

procedure TDatasetViewerFrame.GridIndexSort(Sender: TObject; ACol, ARow, BCol,
  BRow: Integer; var Result: integer);
var
  i: Integer;
begin
  result := 0;
  if not FKeyFields.Count = 0 then exit;

//  ARow := StrToInt(ListGrid.Cells[0, ARow]) - 1;
//  BRow := StrToInt(ListGrid.Cells[0, BRow]) - 1;

  for i := 0 to FKeyFields.Count - 1 do
  begin
    result := FKeyFields[i].Compare(ARow, BRow);
    if result <> 0 then break;
  end;
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
  FSortCol := 0;

  FVLG := TVirtualStringTree.Create(Self);
  with FVLG do
  begin
    Align := alClient;

    Parent := Self;
    Color := clNone;
    WantTabs := true;
    TabStop := true;
    RootNodeCount := FDataFile.Size;

    // Events:
    OnGetText := @VLGGetNodeText;
    OnAfterGetMaxColumnWidth := @VLGAfterGetMaxColumnWidth;
  end;

  with FVLG.TreeOptions do
  begin
    AnimationOptions := [];
    AutoOptions := [toAutoScroll];
    MiscOptions := [toGridExtensions, toWheelPanning];
    PaintOptions := [toShowHorzGridLines, toShowVertGridLines, toThemeAware];
    SelectionOptions := [toExtendedFocus, toRightClickSelect, toCenterScrollIntoView];
  end;
end;

destructor TDatasetViewerFrame.Destroy;
begin
  FCurrentDisplayField.Free;
  inherited Destroy;
end;

procedure TDatasetViewerFrame.ShowRecords(const Records: TBoundArray);
begin
  FRecords := Records;
  VLG.RootNodeCount := Length(FRecords);
  UpdateGrid;
end;

procedure TDatasetViewerFrame.InitVisual;
begin
  UpdateFields;
  FVLG.RootNodeCount := FDataFile.Size;
  UpdateGrid;
end;

end.

