unit epirelate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidatafile;

type
  TEpiRelates = class;
  TEpiPrimaryKeys= class;
  TEpiPrimaryKey = class;
  TEpiRelate = class;

  { TEpiRelates }

  TEpiRelates = class
  private
    FRelateList: TFPList;
    FPrimaryKeys: TEpiPrimaryKeys;
    function GetRelate(Index: integer): TEpiRelate;
    procedure SetPrimaryKeys(const AValue: TEpiPrimaryKeys);
  public
    constructor Create;
    destructor  Destroy; override;
    Property    PrimaryKeys: TEpiPrimaryKeys read FPrimaryKeys write SetPrimaryKeys;
    Property    Relate[Index: integer]: TEpiRelate read GetRelate; default;
  end;

  { TEpiPrimaryKeys }

  TEpiPrimaryKeys = class
  private
    FList: TFPList;
    function GetCount: integer;
    function GetPrimaryKey(Index: integer): TEpiPrimaryKey;
  public
    constructor Create;
    destructor Destroy; override;
    Property   PrimaryKey[Index: integer]: TEpiPrimaryKey read GetPrimaryKey; default;
    Property   Count: integer read GetCount;
  end;

  TEpiPrimaryKey = class
  private
    FDataFile: TEpiDataFile;
    FFields: TEpiFields;
    procedure SetDataFile(const AValue: TEpiDataFile);
    procedure SetFields(const AValue: TEpiFields);
  public
    constructor Create;
    destructor  Destroy; override;
    Property    DataFile: TEpiDataFile read FDataFile write SetDataFile;
    Property    Fields: TEpiFields read FFields write SetFields;
  end;

  TEpiRelate = class
  private
    FDataFile: TEpiDataFile;
    FDestination: TEpiDataFile;
    FField: TEpiField;
    FRelateType: integer;
    FValue: string;
    procedure SetDataFile(const AValue: TEpiDataFile);
    procedure SetDestination(const AValue: TEpiDataFile);
    procedure SetField(const AValue: TEpiField);
    procedure SetRelateType(const AValue: integer);
    procedure SetValue(const AValue: string);
  public
    constructor Create;
    destructor  Destroy; override;
    Property    DataFile: TEpiDataFile read FDataFile write SetDataFile;
    Property    Field: TEpiField read FField write SetField;
    Property    Value: string read FValue write SetValue;
    Property    Destination: TEpiDataFile read FDestination write SetDestination;
    Property    RelateType: integer read FRelateType write SetRelateType;
  end;

implementation

{ TEpiRelates }

function TEpiRelates.GetRelate(Index: integer): TEpiRelate;
begin
  result := TEpiRelate(FRelateList[Index]);
end;

procedure TEpiRelates.SetPrimaryKeys(const AValue: TEpiPrimaryKeys);
begin
  if FPrimaryKeys = AValue then exit;
  FPrimaryKeys := AValue;
end;

constructor TEpiRelates.Create;
begin
  FRelateList := TFPList.Create;
end;

destructor TEpiRelates.Destroy;
begin
  FRelateList.Free;
  inherited Destroy;
end;

{ TEpiPrimaryKeys }

function TEpiPrimaryKeys.GetCount: integer;
begin
  Result := FList.Count;
end;

function TEpiPrimaryKeys.GetPrimaryKey(Index: integer): TEpiPrimaryKey;
begin
  Result := TEpiPrimaryKey(FList[Index]);
end;

constructor TEpiPrimaryKeys.Create;
begin
  FList := TFPList.Create;
end;

destructor TEpiPrimaryKeys.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

{ TEpiPrimaryKey }

procedure TEpiPrimaryKey.SetDataFile(const AValue: TEpiDataFile);
begin
  if FDataFile = AValue then exit;
  FDataFile := AValue;
end;

procedure TEpiPrimaryKey.SetFields(const AValue: TEpiFields);
begin
  if FFields = AValue then exit;
  FFields := AValue;
end;

constructor TEpiPrimaryKey.Create;
begin

end;

destructor TEpiPrimaryKey.Destroy;
begin
  inherited Destroy;
end;

{ TEpiRelate }

procedure TEpiRelate.SetDataFile(const AValue: TEpiDataFile);
begin
  if FDataFile = AValue then exit;
  FDataFile := AValue;
end;

procedure TEpiRelate.SetDestination(const AValue: TEpiDataFile);
begin
  if FDestination = AValue then exit;
  FDestination := AValue;
end;

procedure TEpiRelate.SetField(const AValue: TEpiField);
begin
  if FField = AValue then exit;
  FField := AValue;
end;

procedure TEpiRelate.SetRelateType(const AValue: integer);
begin
  if FRelateType = AValue then exit;
  FRelateType := AValue;
end;

procedure TEpiRelate.SetValue(const AValue: string);
begin
  if FValue = AValue then exit;
  FValue := AValue;
end;

constructor TEpiRelate.Create;
begin

end;

destructor TEpiRelate.Destroy;
begin
  inherited Destroy;
end;

end.

