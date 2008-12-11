unit UValueLabels;

interface

uses
  Classes;
  
type

  TValueLabelSet = class;

  TValueLabelSets = class(TObject)
  private
    FList: TStringList;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure Clear();
    procedure Clone(var dest: TValueLabelSets);
    function ValueLabelSetByName(aName: string): TValueLabelSet;
    procedure AddValueLabelSet(aValueLabelSet: TValueLabelSet);
  end;

  TValueLabelSetType = (vltRef, vltGlobal, vltLocal);

  TValueLabelSet = class(TObject)
  private
    FData: TStringList;
    FName: String;
    FValueLabelSetType: TValueLabelSetType;
    function GetValue(const aLabel: string): string;
    procedure SetValue(const aLabel: string; const aValue: string);
    function GetLabel(const aValue: string): string;
    procedure SetLabel(const aValue: string; const aLabel: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddValueLabelPair(const aValue, aLabel: string);
    procedure Clone(var Dest: TValueLabelSet);
    procedure Clear();
    property Name: string read FName write FName;
    property ValueLabelSetType: TValueLabelSetType read FValueLabelSetType write FValueLabelSetType;
    property Value[const aLabel: string]: string read GetLabel write SetLabel;
    property vLabel[const aValue: string]: string read GetValue write SetValue;
  end;

implementation

uses
  UEpiUtils, SysUtils;

{ TValueLabelSets }

procedure TValueLabelSets.AddValueLabelSet(aValueLabelSet: TValueLabelSet);
begin
  FList.AddObject(aValueLabelSet.Name, aValueLabelSet);
end;

procedure TValueLabelSets.Clear;
var
  i: integer;
begin
  for i := 0 to FList.Count - 1 do
    TValueLabelSet(FList.Objects[i]).Destroy;
  FList.Capacity := 0;
end;

procedure TValueLabelSets.Clone(var dest: TValueLabelSets);
var
  i: integer;
  tmp: TValueLabelSet;
begin
  if not Assigned(Dest) then
    Dest := TValueLabelSets.Create();

  Dest.Clear;
  for i := 0 to FList.Count - 1 do
  begin
    TValueLabelSet(FList[i]).Clone(tmp);
    Dest.AddValueLabelSet(tmp);
  end;
end;

constructor TValueLabelSets.Create;
begin
  Flist := TStringList.Create;
end;

destructor TValueLabelSets.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

function TValueLabelSets.ValueLabelSetByName(
  aName: string): TValueLabelSet;
var
  idx: integer;
begin
  result := nil;
  if FList.Find(aName, idx) then
    result := TValueLabelSet(FList.Objects[idx]);
end;

{ TValueLabelSet }

procedure TValueLabelSet.AddValueLabelPair(const aValue, aLabel: string);
var
  idx: integer;
begin
  if FData.Find(aValue, idx) then
    TString(FData.Objects[idx]).Str := aLabel
  else
    FData.AddObject(aValue, TString.Create(aLabel));
end;

procedure TValueLabelSet.Clear;
var
  i: integer;
begin
  for i := 0 to FData.Count - 1 do
    TString(FData.Objects[i]).Destroy;
  FData.Capacity := 0;
end;

procedure TValueLabelSet.Clone(var Dest: TValueLabelSet);
var
  i: integer;
begin
  if not Assigned(Dest) then
    Dest := TValueLabelSet.Create();

  Dest.Clear;
  Dest.Name := Name;
  for i := 0 to FData.Count -1 do
    Dest.AddValueLabelPair(FData[i], TString(FData.Objects[i]).Str);
end;

constructor TValueLabelSet.Create;
begin
  FName := '';
  FValueLabelSetType := vltLocal;
  FData.Create;
  FData.Sorted := true;
  FData.CaseSensitive := false;
end;

destructor TValueLabelSet.Destroy;
begin
  Clear;
  FreeAndNil(FData);
  inherited;
end;

function TValueLabelSet.GetLabel(const aValue: string): string;
var
  idx: integer;
begin
  result := '';
  if FData.Find(aValue, idx) then
    result := TString(FData.Objects[idx]).Str;
end;

function TValueLabelSet.GetValue(const aLabel: string): string;
var
  i: integer;
begin
  result := '';
  for i := 0 to FData.Count - 1 do
    if AnsiCompareText(TString(FData.Objects[i]).Str, aLabel) <> 0 then
    begin
      result := FData[i];
      break;
    end;
end;

procedure TValueLabelSet.SetLabel(const aValue, aLabel: string);
var
  idx: integer;
begin
  if not FData.Find(aValue, idx) then
    Raise Exception.Create('Value not found');
  TString(FData.Objects[idx]).Str := aLabel;
end;

procedure TValueLabelSet.SetValue(const aLabel, aValue: string);
var
  i: integer;
begin
  for i := 0 to FData.Count - 1 do
    if AnsiCompareText(TString(FData.Objects[i]).Str, aLabel) <> 0 then
    begin
      FData[i] := aValue;
      break;
    end;
end;

end.
