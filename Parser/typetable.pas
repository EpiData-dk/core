unit typetable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs,
  epi_parser_types;

type

  { TTypeTable }

  TTypeTable = class(TObject)
  private
    FTable: TFPDataHashTable;
  public
    constructor Create;
    procedure AddVariable(Const VarName: string; Const VarType: TParserResultType);
    procedure Clear;
    function VariableExists(Const VarName: string): boolean;
    function VariableType(Const VarName: string): TParserResultType;
  end;

implementation

{ TTypeTable }

constructor TTypeTable.Create;
begin
  FTable := TFPDataHashTable.Create;
end;

procedure TTypeTable.AddVariable(const VarName: string;
  const VarType: TParserResultType);
begin
  FTable.Add(VarName, Pointer(PtrInt(VarType)));
end;

procedure TTypeTable.Clear;
begin
  FTable.Clear;
end;

function TTypeTable.VariableExists(const VarName: string): boolean;
begin
  result := Assigned(FTable.Find(VarName));
end;

function TTypeTable.VariableType(const VarName: string): TParserResultType;
var
  Node: THTDataNode;
begin
  result := rtUndefined;

  Node := THTDataNode(FTable.Find(VarName));
  if Assigned(Node) then
    result := TParserResultType(PtrInt(Node.Data));
end;

end.

