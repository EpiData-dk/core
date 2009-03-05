unit UValueLabels;

interface

uses
  Classes;
  
type

  TValueLabelSet = class;

  TValueLabelSets = class(TObject)
  private
    FList: TStringList;
    function GetCount:integer;
    function GetItem(index:integer):TValueLabelSet;
  public
    constructor Create();
    destructor  Destroy(); override;
    procedure   Clear();
    procedure   Clone(var dest: TValueLabelSets);
    function    ValueLabelSetByName(aName: string): TValueLabelSet;
    procedure   AddValueLabelSet(aValueLabelSet: TValueLabelSet);
    procedure   DeleteValueLabelSet(name: string);
    property    count:integer read GetCount;
    property    items[index: integer]:TValueLabelSet read GetItem;
  end;

  TValueLabelSetType = (vltFieldRef, vltLabelRef, vltLocal, vltFile);

  TValueLabelSet = class(TObject)
  private
    FData: TStringList;
    FName: String;
    function  GetValue(const aLabel: string): string;
    procedure SetValue(const aLabel: string; const aValue: string);
    function  GetLabel(const aValue: string): string;
    procedure SetLabel(const aValue: string; const aLabel: string);
    function  GetCount:integer;
    function  GetValues(index:integer):string;
    procedure SetValues(index:integer;value:string);
    function  GetLabels(index:integer):string;
    procedure SetLabels(index:integer; alabel:string);
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   AddValueLabelPair(const aValue, aLabel: string);
    procedure   Clone(var Dest: TValueLabelSet);
    procedure   Clear();
    property    Name: string read FName write FName;
    property    Value[const aLabel: string]: string read GetValue write SetValue;
    property    ValueLabel[const aValue: string]: string read GetLabel write SetLabel;
    property    count:integer read GetCount;
    property    Values[index:integer]:string read GetValues write SetValues;
    property    Labels[index:integer]:string read GetLabels write SetLabels;
  end;

implementation

uses
  UEpiUtils, SysUtils;

{ TValueLabelSets }

procedure TValueLabelSets.AddValueLabelSet(aValueLabelSet: TValueLabelSet);
begin
  FList.AddObject(trim(aValueLabelSet.Name), aValueLabelSet);
end;

procedure TValueLabelSets.DeleteValueLabelSet(name:string);
var
  idx: integer;
begin
  idx:=FList.IndexOf(name);
  if idx>-1 then FList.Delete(idx);
end;

procedure TValueLabelSets.Clear;
var
  i: integer;
begin
  for i := 0 to FList.Count - 1 do
    TValueLabelSet(FList.Objects[i]).Destroy;
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
var
  n:integer;
begin
  clear;
  FreeAndNil(FList);
  inherited;
end;

function TValueLabelSets.ValueLabelSetByName(
  aName: string): TValueLabelSet;
var
  idx: integer;
  found: boolean;
begin
  result := nil;
  idx:=0;
  found:=false;
  while (idx<FList.Count) and (found=false) do
    begin
      if TValueLabelSet(FList.Objects[idx]).Name=aName then
        begin
          result:=TValueLabelSet(FList.Objects[idx]);
          found:=true;
        end;
      inc(idx);
    end;

//  if FList.Find(trim(aName), idx) then
//    result := TValueLabelSet(FList.Objects[idx]);
end;

function TValueLabelSets.GetCount:integer;
begin
  result:=FList.count;
end;

function TValueLabelSets.GetItem(index:integer):TValueLabelSet;
begin
  if (index>=0) and (index<FList.count)
  then result:=TValueLabelSet(FList.objects[index])
  else result:=NIL;
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
  FData:=TStringList.create;
  FData.Sorted := true;
  FData.CaseSensitive := false;
end;

destructor TValueLabelSet.Destroy;
begin
  Clear;
  //FreeAndNil(FData);
  FData.Free;
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

// TODO -O Torsten : Mangler Error håndtering
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

function TValueLabelSet.GetCount:integer;
begin
  result:=FData.Count;
end;

function TValueLabelSet.GetValues(index:integer):string;
begin
  if index<FData.Count then result:=FData[index] else result:='';
end;

procedure TValueLabelSet.SetValues(index:integer; value:string);
begin
  if index<FData.Count then FData[index]:=value;
end;

function TValueLabelSet.GetLabels(index:integer):string;
begin
  if index<FData.Count then result:=TString(FData.Objects[index]).Str else result:='';
end;

procedure TValueLabelSet.SetLabels(index:integer; alabel:string);
begin
  if index<FData.Count then TString(FData.Objects[index]).Str := aLabel;
end;

end.
