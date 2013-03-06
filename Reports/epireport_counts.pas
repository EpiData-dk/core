unit epireport_counts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, epireport_base,
  epidatafiles, epireport_generator_base;

type

  { TEpiReportCounts }

  TEpiReportCounts = class(TEpiReportBase)
  private
    FDocuments: TStringList;
    FFieldList: TEpiFields;
    ResultDF: TEpiDataFile;
    ValueFieldList: TEpiFields;
    CountsFieldList: TEpiFields;
    procedure DoCounts(Const DF: TEpiDataFile; Const DFIndex: integer);
    procedure DoReport;
  public
    constructor Create(ReportGenerator: TEpiReportGeneratorBase);
       override;
    destructor Destroy; override;
    procedure DoSanityCheck; override;
    procedure   RunReport; override;
    property    Documents: TStringList read FDocuments write FDocuments;
    property    FieldList: TEpiFields read FFieldList write FFieldList;
  end;


implementation

uses
  epimiscutils, epidatafilestypes, epidocument, epidatafileutils;

resourcestring
  rsDocumentsNotAssigned = 'Documents not assigned to report';
  rsFieldlistNotAssigned = 'Fieldlist not assigned to report';


const
  COUNTS_CUSTDATA = 'counts_customdata_key';

var
  DFCount: integer;

type

  { TCountsList }

  TCountsList = class
  private
    FData: array of integer;
    function GetData(Index: integer): integer;
    function GetSize: integer;
    procedure SetData(Index: integer; AValue: integer);
  public
    constructor Create(Const Size: integer);
    destructor Destroy; override;
    property Data[Index: integer]: integer read GetData write SetData; default;
    property Size: integer read GetSize;
  end;

{ TCountsList }

function TCountsList.GetData(Index: integer): integer;
begin
  result := FData[Index];
end;

function TCountsList.GetSize: integer;
begin
  result := Length(FData);
end;

procedure TCountsList.SetData(Index: integer; AValue: integer);
begin
  FData[Index] := AValue;
end;

constructor TCountsList.Create(const Size: integer);
begin
  SetLength(FData, Size);
end;

destructor TCountsList.Destroy;
begin
  SetLength(FData, 0);
  inherited Destroy;
end;


{ TEpiReportCounts }

procedure TEpiReportCounts.DoCounts(const DF: TEpiDataFile;
  const DFIndex: integer);
var
  OrgSortingField: TEpiField;
  LocalValueFields: TEpiFields;
  F: TEpiField;
  i: Integer;
  Index: integer;
  j: Integer;


  function FindIndexInResultDF(Const DFIndex: integer;
    out Index: integer): boolean;
  var
    i, j: integer;
    CmpResult: TValueSign;
    F: TEpiField;
  begin
    for i := 0 to ResultDF.Size - 1 do
    begin
      Index := i;

      for j := 0 to LocalValueFields.Count - 1 do
      begin
        F := ValueFieldList.FieldByName[LocalValueFields[j].Name];
        CompareFieldRecords(CmpResult, F, LocalValueFields[j], i, DFIndex);
        if CmpResult <> 0 then break;
      end;

      if CmpResult = ZeroValue then Exit(true);
    end;
    Index := -1;
    Result := false;
  end;

begin
  // Create a field for restoring old sorting.
  OrgSortingField := DF.NewField(ftInteger);
  for i := 0 to OrgSortingField.Size - 1 do
    OrgSortingField.AsInteger[i] := i;

  // Fetch list of field in current DF with same names as in user specified
  // field list.
  LocalValueFields := TEpiFields.Create(nil);
  for i := 0 to FieldList.Count - 1 do
  begin
    F := DF.Fields.FieldByName[FieldList[i].Name];
    if Assigned(F) then
      LocalValueFields.AddItem(F);
  end;

  DF.SortRecords(LocalValueFields);

  for i := 0 to DF.Size - 1 do
  begin
    if (not FindIndexInResultDF(i, Index)) then
    begin
      ResultDF.NewRecords();
      Index := ResultDF.Size - 1;
      for j := 0 to LocalValueFields.Count - 1 do
      begin
        F := ValueFieldList.FieldByName[LocalValueFields[j].Name];
        if Assigned(F) then
          F.AsValue[ResultDf.Size - 1] := LocalValueFields[j].AsValue[i]
      end;
    end;

    if CountsFieldList[DFIndex].IsMissing[Index] then
      CountsFieldList[DFIndex].AsInteger[Index] := 1
    else
      CountsFieldList[DFIndex].AsInteger[Index] := CountsFieldList[DFIndex].AsInteger[Index] + 1;
  end;
  DF.SortRecords(OrgSortingField);
  FreeAndNil(OrgSortingField);
end;

procedure TEpiReportCounts.DoReport;
var
  S: String;
  i: Integer;
  j: Integer;
begin
  ResultDF.SortRecords(ValueFieldList);

  S := FieldList[0].Name;
  for i := 1 to FieldList.Count - 1 do
    S += ', ' + FieldList[i].Name;

  DoLineText('');
  DoLineText(IntToStr(ResultDF.Size) + ' different values found.');
  DoLineText('');

  DoTableHeader('Fields: ' + S, 1 + CountsFieldList.Count, 1 + ResultDF.Size);
  DoTableCell(0, 0, 'Value(s)');
  for i := 0 to CountsFieldList.Count - 1 do
    DoTableCell(1 + i, 0, CountsFieldList[i].Question.Text);


  for i := 0 to ResultDF.Size - 1 do
  begin
    S := ValueFieldList[0].AsString[i];
    for j := 1 to ValueFieldList.Count - 1 do
      S += ', ' + ValueFieldList[j].AsString[i];

    DoTableCell(0, 1 + i, S);

    for j := 0 to CountsFieldList.Count - 1 do
      DoTableCell(1 + j, 1 + i, CountsFieldList[j].AsString[i]);
  end;

  DoTableFooter('');
end;

constructor TEpiReportCounts.Create(ReportGenerator: TEpiReportGeneratorBase);
begin
  inherited Create(ReportGenerator);

  ResultDF := TEpiDataFile.Create(nil);
  ValueFieldList := TEpiFields.Create(nil);
  CountsFieldList := TEpiFields.Create(nil);
end;

destructor TEpiReportCounts.Destroy;
begin
  FreeAndNil(CountsFieldList);
  FreeAndNil(ValueFieldList);
  FreeAndNil(ResultDF);

  inherited Destroy;
end;

procedure TEpiReportCounts.DoSanityCheck;
begin
  inherited DoSanityCheck;

  if not Assigned(FDocuments) then
    DoError(EEpiReportBaseException, rsDocumentsNotAssigned);

  if not Assigned(FFieldList) then
    DoError(EEpiReportBaseException, rsFieldlistNotAssigned);
end;

procedure TEpiReportCounts.RunReport;
var
  i: Integer;
  F: TEpiField;
begin
  inherited RunReport;


  for i := 0 to FieldList.Count - 1 do
  with FieldList[i] do
  begin
    F := ResultDF.NewField(FieldType);
    F.Name := Name;
    F.Length := Length;
    F.Decimals := Decimals;
    ValueFieldList.AddItem(F);
  end;

  for i := 0 to Documents.Count - 1 do
  begin
    F := ResultDF.NewField(ftInteger);
    F.Name := 'File' + IntToStr(i + 1);
    F.Question.Text := 'File ' + IntToStr(i + 1);
    F.Length := 4;
    F.ResetData;
    CountsFieldList.AddItem(F);
  end;

  for i := 0 to Documents.Count - 1 do
    DoCounts(TEpiDocument(Documents.Objects[i]).DataFiles[0], i);

  DoReport;
end;

end.

