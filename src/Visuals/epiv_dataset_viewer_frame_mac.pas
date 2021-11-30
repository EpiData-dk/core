unit epiv_dataset_viewer_frame_mac;

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
    Button4: TButton;
    ListGrid: TStringGrid;
    Panel1: TPanel;
    ShowIndexOrAllFieldsAction: TAction;
    ShowValuesOrLabelsAction: TAction;
    SortByIndexAction: TAction;
    procedure Button4Click(Sender: TObject);
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
    FForwardIndex: TEpiField;
    FKeyFields: TEpiFields;
    FOnSelectRecord: TSelectRecordEvent;
    FRecords: TBoundArray;
    FReverseIndex: TEpiField;
    FShowAllFields: boolean;
    FShowValueLabels: boolean;
    FShowAllRecords: boolean;
    FSortCol: integer;
    procedure DoSelectedRecord(RecordNo: Integer; Const Field: TEpiField);
    function GetKeyFields: TEpiFields;
    procedure SetDatafile(AValue: TEpiDataFile);
    procedure SetDisplayFields(AValue: TEpiFields);
    procedure SetForwardIndex(AValue: TEpiField);
    procedure SetKeyFields(AValue: TEpiFields);
    procedure SetOnSelectRecord(AValue: TSelectRecordEvent);
    procedure SetReverseIndex(AValue: TEpiField);
    procedure UpdateGrid;
    procedure GridColumnSort(Sender: TObject; ACol, ARow, BCol,
      BRow: Integer; var Result: integer);
    procedure  GridIndexSort(Sender: TObject; ACol, ARow, BCol,
      BRow: Integer; var Result: integer);
  private
    FUpdateCount: Integer;
  public
    constructor Create(TheOwner: TComponent; Const DataFile: TEpiDataFile);
    destructor Destroy; override;
    procedure   ShowRecords(const Records: TBoundArray);
    procedure   BeginUpdate;
    procedure   EndUpdate;
    property    Datafile: TEpiDataFile read FDataFile write SetDatafile;
    property    KeyFields: TEpiFields read GetKeyFields write SetKeyFields;
    property    DisplayFields: TEpiFields read FDisplayFields write SetDisplayFields;
    property    OnSelectRecord: TSelectRecordEvent read FOnSelectRecord write SetOnSelectRecord;
    property    ForwardIndex: TEpiField read FForwardIndex write SetForwardIndex;
    property    ReverseIndex: TEpiField read FReverseIndex write SetReverseIndex;
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
  if Index < 0 then exit;

  if IsColumn then
  begin
    // If click on column -> then sort.

    if FSortCol = Index then exit;

    FSortCol := Index;
    ListGrid.OnCompareCells := @GridColumnSort;
    ListGrid.SortColRow(true, FSortCol);
  end else begin
    // If click on row -> then notify a "jump to record"

    SelectedRecordNo := PtrInt(ListGrid.Objects[0, Index]);
    if Assigned(FReverseIndex) then
      SelectedRecordNo := ReverseIndex.AsInteger[SelectedRecordNo];
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

  SelectedRecordNo := PtrInt(ListGrid.Objects[0, P.Y]);
  if Assigned(FReverseIndex) then
    SelectedRecordNo := ReverseIndex.AsInteger[SelectedRecordNo];

  SelectedField := FCurrentDisplayField[P.X - 1];
  DoSelectedRecord(SelectedRecordNo, SelectedField);
end;

procedure TDatasetViewerFrame.Button4Click(Sender: TObject);
begin
  if not Assigned(FReverseIndex) then exit;
  FShowAllRecords := not FShowAllRecords;
  UpdateGrid;
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

procedure TDatasetViewerFrame.SetDatafile(AValue: TEpiDataFile);
begin
  if FDataFile = AValue then Exit;
  FDataFile := AValue;
end;

procedure TDatasetViewerFrame.SetDisplayFields(AValue: TEpiFields);
begin
  if Assigned(AValue) then
    FDisplayFields := AValue;
end;

procedure TDatasetViewerFrame.SetForwardIndex(AValue: TEpiField);
begin
  if FForwardIndex = AValue then Exit;
  FForwardIndex := AValue;
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

procedure TDatasetViewerFrame.SetReverseIndex(AValue: TEpiField);
begin
  if FReverseIndex = AValue then Exit;
  FReverseIndex := AValue;
end;

procedure TDatasetViewerFrame.UpdateGrid;
var
  i: Integer;
  j: Integer;
  F: TEpiField;
  Idx: Integer;
  S: String;
  RC: Integer;

  procedure AssignFields(ToFields, FromFields: TEpiFields);
  var
    i: integer;
  begin
    for i := 0 to FromFields.Count - 1 do
      ToFields.AddItem(FromFields[i]);
  end;

begin
  if FUpdateCount > 0 then exit;

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
  RC := 0;

  if Length(FRecords) > 0 then
    RC := Length(FRecords)
  else if FShowAllRecords then
    RC := FDataFile.Size
  else
    RC := FForwardIndex.Size;

  ListGrid.RowCount := RC + 1;
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
    for i := 0 to RC - 1 do
    begin
      if Assigned(FForwardIndex) and (not FShowAllRecords) then
        Idx := FForwardIndex.AsInteger[i]
      else
        Idx := i;

      S := IntToStr(Idx + 1);
      if Assigned(FReverseIndex) and (FShowAllRecords) then
        if (FReverseIndex.IsMissing[Idx]) then
          S := '*'
        else
          S := IntToStr(FReverseIndex.AsInteger[Idx] + 1);

      ListGrid.Cells[0, i + 1] := S;
      ListGrid.Objects[0, i + 1] := TObject(PtrUInt(Idx));

      for j := 0 to FCurrentDisplayField.Count - 1 do
      with FCurrentDisplayField[j] do
        if (FShowValueLabels) and (Assigned(ValueLabelSet)) and (not IsMissing[Idx]) then
          ListGrid.Cells[j + 1, i + 1] := ValueLabelSet.ValueLabelString[AsValue[Idx]]
        else
          ListGrid.Cells[j + 1, i + 1] := AsString[Idx];
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
var
  AIdx: PtrInt;
  BIdx: PtrInt;
begin
  if ACol <> BCol then exit;

  AIdx := PtrInt(ListGrid.Objects[0, ARow]);
  BIdx := PtrInt(ListGrid.Objects[0, BRow]);

  if ACol = 0 then
    if Assigned(ReverseIndex) and (FShowAllRecords) then
      Result := ReverseIndex.Compare(AIdx, BIdx)
    else
      result := AIdx - BIdx
  else
    result := FCurrentDisplayField[ACol - 1].Compare(AIdx, BIdx);
end;

procedure TDatasetViewerFrame.GridIndexSort(Sender: TObject; ACol, ARow, BCol,
  BRow: Integer; var Result: integer);
var
  i: Integer;
begin
  result := 0;
  if not FKeyFields.Count = 0 then exit;

  ARow := PtrInt(ListGrid.Objects[0, ARow]);
  BRow := PtrInt(ListGrid.Objects[0, BRow]);

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
  FShowAllRecords := true;
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

procedure TDatasetViewerFrame.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TDatasetViewerFrame.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    UpdateGrid;
end;

end.

