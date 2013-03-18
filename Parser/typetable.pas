unit typetable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs,
  parser_types;

type

  { TTypeTable }

  TTypeTable = class
  private
    FTable: TFPDataHashTable;
  public
    constructor Create;
    procedure AddVariable(Const VarName: string; Const VarType: TParserResultType);
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

