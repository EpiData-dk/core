unit UValueLabels;

interface

uses
  Classes;
  
type

  TValueLabelSet = class;

  TValueLabelSets = class(TObject)
  private
    FList: TList;
  public
    constructor Create();
    destructor Destroy(); override;
    function ValueLabelSetByName(aName: string): TValueLabelSet;
    procedure Clone(var dest: TValueLabelSets);
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

    property Value[const aLabel: string]: string read GetLabel write SetLabel;
    property vLabel[const aValue: string]: string read GetValue write SetValue;
  end;

implementation

uses
  UEpiUtils;

{ TValueLabelSets }

procedure TValueLabelSets.Clone(var dest: TValueLabelSets);
var
  i: integer;
begin
  if not Assigned(Dest) then
    Dest := TValueLabelSets.Create();

  for i := 0 to FList.Count - 1 do
  begin
//    TValueLabelSet(FList[i]).Clone();
  end;
end;

constructor TValueLabelSets.Create;
begin

end;

destructor TValueLabelSets.Destroy;
begin

  inherited;
end;

function TValueLabelSets.ValueLabelSetByName(
  aName: string): TValueLabelSet;
begin

end;

{ TValueLabelSet }

procedure TValueLabelSet.AddValueLabelPair(const aValue, aLabel: string);
begin

end;

procedure TValueLabelSet.Clone(var Dest: TValueLabelSet);
begin

end;

constructor TValueLabelSet.Create;
begin

end;

destructor TValueLabelSet.Destroy;
begin

  inherited;
end;

function TValueLabelSet.GetLabel(const aValue: string): string;
begin

end;

function TValueLabelSet.GetValue(const aLabel: string): string;
begin

end;

procedure TValueLabelSet.SetLabel(const aValue, aLabel: string);
begin

end;

procedure TValueLabelSet.SetValue(const aLabel, aValue: string);
begin

end;

end.
