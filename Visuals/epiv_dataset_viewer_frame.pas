unit epiv_dataset_viewer_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Grids, ExtCtrls, StdCtrls,
  ActnList, epidatafiles;

type

  TSelectRecordEvent = procedure(Sender: TObject; RecordNo: Integer;
    Const Field: TEpiField = nil) of object;

  { TDatasetViewerFrame }

  TDatasetViewerFrame = class(TFrame)
    ActionList1: TActionList;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ListGrid: TStringGrid;
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
    procedure UpdateGrid;
    procedure GridColumnSort(Sender: TObject; ACol, ARow, BCol,
      BRow: Integer; var Result: integer);
    procedure  GridIndexSort(Sender: TObject; ACol, ARow, BCol,
      BRow: Integer; var Result: integer);
  public
    constructor Create(TheOwner: TComponent; Const DataFile: TEpiDataFile);
    destructor Destroy; override;
    procedure   ShowRecords(const Records: TBoundArray);
    property    KeyFields: TEpiFields read GetKeyFields write SetKeyFields;
    property    DisplayFields: TEpiFields read FDisplayFields write SetDisplayFields;
    property    OnSelectRecord: TSelectRecordEvent read FOnSelectRecord write SetOnSelectRecord;
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
  if Index <= 0 then exit;

  if IsColumn then
  begin
    // If click on column -> then sort.

    if FSortCol = Index then exit;

    FSortCol := Index;
    ListGrid.OnCompareCells := @GridColumnSort;
    ListGrid.SortColRow(true, FSortCol);
  end else begin
    // If click on row -> then notify a "jump to record"

    SelectedRecordNo := StrToInt(ListGrid.Cells[0, Index]) - 1;
    DoSelectedRecord(SelectedRecordNo, nil);
  end;
end;

procedure TDatasetViewerFrame.ListGridDblClick(Sender: TObject);
var
  P: TPoint;
  SelectedRecordNo: Integer;
  SelectedField: TEpiField;
begin
  P := ListGrid.MouseToCell(ListGrid.ScreenToClient(Mouse.CursorPos));
  if P.Y <= 0 then exit;
  if P.X <= 0 then exit;

  SelectedRecordNo := StrToInt(ListGrid.Cells[0, P.Y]) - 1;
  SelectedField := FCurrentDisplayField[P.X - 1];
  DoSelectedRecord(SelectedRecordNo, SelectedField);
end;

procedure TDatasetViewerFrame.ListGridPrepareCanvas(sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
begin
  if (aRow = 0) and (aCol = FSortCol) then
    ListGrid.Canvas.Brush.Color := clSkyBlue;
end;

procedure TDatasetViewerFrame.SortByIndexActionExecute(Sender: TObject);
begin
  if FSortCol = -1 then exit;

  FSortCol := -1;
  ListGrid.OnCompareCells := @GridIndexSort;
  ListGrid.SortColRow(true, 0);
end;

procedure TDatasetViewerFrame.SortByIndexActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (Assigned(FKeyFields)) and (FKeyFields.Count > 0);
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

  with ListGrid do
    if Assigned(FOnSelectRecord) then
    begin
      HeaderHotZones  := HeaderHotZones + [gzFixedRows];
      HeaderPushZones := HeaderPushZones + [gzFixedRows];
    end else begin
      HeaderHotZones  := HeaderHotZones - [gzFixedRows];
      HeaderPushZones := HeaderPushZones - [gzFixedRows];
    end;
end;

procedure TDatasetViewerFrame.UpdateGrid;
var
  i: Integer;
  j: Integer;
  F: TEpiField;

  procedure AssignFields(ToFields, FromFields: TEpiFields);
  var
    i: integer;
  begin
    for i := 0 to FromFields.Count - 1 do
      ToFields.AddItem(FromFields[i]);
  end;

begin
  FCurrentDisplayField.Clear;

  if FShowAllFields then
    AssignFields(FCurrentDisplayField, FDisplayFields)
  else begin
    AssignFields(FCurrentDisplayField, FKeyFields);
    F := FDataFile.Fields.FieldByName[EpiIndexIntegrityFieldName];
    if Assigned(F) then
      FCurrentDisplayField.AddItem(F);
  end;

  ListGrid.BeginUpdate;

  ListGrid.ColCount := FCurrentDisplayField.Count + 1;
  if Length(FRecords) > 0 then
    ListGrid.RowCount := Length(FRecords) + 1
  else
    ListGrid.RowCount := FDataFile.Size + 1;

  for i := 0 to FCurrentDisplayField.Count - 1 do
  with FCurrentDisplayField[i] do
    ListGrid.Cells[i+1, 0] := Name;

  if Length(FRecords) > 0 then
  begin
    for i := 0 to Length(FRecords) - 1 do
    begin
      ListGrid.Cells[0, i + 1] := IntToStr(FRecords[i] + 1);

      for j := 0 to FCurrentDisplayField.Count - 1 do
      with FCurrentDisplayField[j] do
        if (FShowValueLabels) and (Assigned(ValueLabelSet)) and (not IsMissing[FRecords[i]]) then
          ListGrid.Cells[j + 1, i + 1] := ValueLabelSet.ValueLabelString[AsValue[FRecords[i]]]
        else
          ListGrid.Cells[j + 1, i + 1] := AsString[FRecords[i]];
    end;
  end else begin
    for i := 0 to FDataFile.Size - 1 do
    begin
      ListGrid.Cells[0, i + 1] := IntToStr(i + 1);

      for j := 0 to FCurrentDisplayField.Count - 1 do
      with FCurrentDisplayField[j] do
        if (FShowValueLabels) and (Assigned(ValueLabelSet)) and (not IsMissing[i]) then
          ListGrid.Cells[j + 1, i + 1] := ValueLabelSet.ValueLabelString[AsValue[i]]
        else
          ListGrid.Cells[j + 1, i + 1] := AsString[i];
    end;
  end;
  if FSortCol > (ListGrid.ColCount - 1) then
    FSortCol := 0;
  ListGrid.SortColRow(true, Max(FSortCol,0));
  ListGrid.AutoSizeColumns;
  ListGrid.EndUpdate();
end;

procedure TDatasetViewerFrame.GridColumnSort(Sender: TObject; ACol, ARow, BCol,
  BRow: Integer; var Result: integer);
begin
  if ACol <> BCol then exit;

  ARow := StrToInt(ListGrid.Cells[0, ARow]) - 1;
  BRow := StrToInt(ListGrid.Cells[0, BRow]) - 1;

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

  ARow := StrToInt(ListGrid.Cells[0, ARow]) - 1;
  BRow := StrToInt(ListGrid.Cells[0, BRow]) - 1;

  for i := 0 to FKeyFields.Count - 1 do
  begin
    result := FKeyFields[i].Compare(ARow, BRow);
    if result <> 0 then break;
  end;
end;

constructor TDatasetViewerFrame.Create(TheOwner: TComponent;
  const DataFile: TEpiDataFile);
begin
  inherited Create(TheOwner);
  FDataFile := DataFile;
  FKeyFields := FDataFile.KeyFields;
  FDisplayFields := FDataFile.Fields;
  FCurrentDisplayField := TEpiFields.Create(nil);
  FShowValueLabels := false;
  FShowAllFields := true;
  FSortCol := 0;

  with ListGrid do
  begin
    Align := alClient;
    Cells[0,0] := 'Record No:';
    OnCompareCells := @GridColumnSort;
    UpdateGrid;
    Parent := Self;
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
  UpdateGrid;
end;

end.

