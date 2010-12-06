unit epiranges;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epicustombase, epidatafilestypes, DOM;

type

  { TEpiRanges }

  TEpiRanges = class(TEpiCustomList)
  private
    function    GetFieldType: TEpiFieldType;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor  Destroy; override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    property    FieldType: TEpiFieldType read GetFieldType;
  end;

  { TEpiRange }

  TEpiRange = class(TEpiCustomItem)
  private
    function GetRanges: TEpiRanges;
    function GetSingle: boolean;
  protected
    function GetAsDate(const Start: boolean): EpiDate; virtual; abstract;
    function GetAsFloat(const Start: boolean): EpiFloat; virtual; abstract;
    function GetAsInteger(const Start: boolean): EpiInteger; virtual; abstract;
    function GetAsString(const Start: boolean): EpiString; virtual; abstract;
    function GetAsTime(const Start: boolean): EpiTime; virtual; abstract;
    procedure SetAsDate(const Start: boolean; const AValue: EpiDate); virtual; abstract;
    procedure SetAsFloat(const Start: boolean; const AValue: EpiFloat); virtual; abstract;
    procedure SetAsInteger(const Start: boolean; const AValue: EpiInteger); virtual; abstract;
    procedure SetAsTime(const Start: boolean; const AValue: EpiTime); virtual; abstract;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor  Destroy; override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    function    SaveToXml(Content: String; Lvl: integer): string; override;
    function    XMLName: string; override;
    property    AsInteger[const Start: boolean]: EpiInteger read GetAsInteger write SetAsInteger;
    property    AsFloat[const Start: boolean]: EpiFloat read GetAsFloat write SetAsFloat;
    property    AsDate[const Start: boolean]: EpiDate read GetAsDate write SetAsDate;
    property    AsTime[const Start: boolean]: EpiTime read GetAsTime write SetAsTime;
    property    AsString[const Start: boolean]: EpiString read GetAsString;
    property    Single: boolean read GetSingle;
    property    Ranges: TEpiRanges read GetRanges;
  end;

  { TEpiIntRange }

  TEpiIntRange = class(TEpiRange)
  private
    FStart:    EpiInteger;
    FEnd:      EpiInteger;
  protected
    function GetAsDate(const Start: boolean): EpiDate; inline;
    function GetAsFloat(const Start: boolean): EpiFloat; inline;
    function GetAsInteger(const Start: boolean): EpiInteger; inline;
    function GetAsString(const Start: boolean): EpiString; inline;
    function GetAsTime(const Start: boolean): EpiTime; inline;
    procedure SetAsDate(const Start: boolean; const AValue: EpiDate); inline;
    procedure SetAsFloat(const Start: boolean; const AValue: EpiFloat); inline;
    procedure SetAsInteger(const Start: boolean; const AValue: EpiInteger); inline;
    procedure SetAsTime(const Start: boolean; const AValue: EpiTime); inline;
  end;

  { TEpiFloatRange }

  TEpiFloatRange = class(TEpiRange)
  private
    FStart:    EpiFloat;
    FEnd:      EpiFloat;
  protected
    function GetAsDate(const Start: boolean): EpiDate; inline;
    function GetAsFloat(const Start: boolean): EpiFloat; inline;
    function GetAsInteger(const Start: boolean): EpiInteger; inline;
    function GetAsString(const Start: boolean): EpiString; inline;
    function GetAsTime(const Start: boolean): EpiTime; inline;
    procedure SetAsDate(const Start: boolean; const AValue: EpiDate); inline;
    procedure SetAsFloat(const Start: boolean; const AValue: EpiFloat); inline;
    procedure SetAsInteger(const Start: boolean; const AValue: EpiInteger); inline;
    procedure SetAsTime(const Start: boolean; const AValue: EpiTime); inline;
  end;

  { TEpiDateRange }

  TEpiDateRange = class(TEpiRange)
  private
    FStart:    EpiDate;
    FEnd:      EpiDate;
  protected
    function GetAsDate(const Start: boolean): EpiDate; inline;
    function GetAsFloat(const Start: boolean): EpiFloat; inline;
    function GetAsInteger(const Start: boolean): EpiInteger; inline;
    function GetAsString(const Start: boolean): EpiString; inline;
    function GetAsTime(const Start: boolean): EpiTime; inline;
    procedure SetAsDate(const Start: boolean; const AValue: EpiDate); inline;
    procedure SetAsFloat(const Start: boolean; const AValue: EpiFloat); inline;
    procedure SetAsInteger(const Start: boolean; const AValue: EpiInteger); inline;
    procedure SetAsTime(const Start: boolean; const AValue: EpiTime); inline;
  end;

  { TEpiTimeRange }

  TEpiTimeRange = class(TEpiRange)
  private
    FStart:    EpiTime;
    FEnd:      EpiTime;
  protected
    function GetAsDate(const Start: boolean): EpiDate; inline;
    function GetAsFloat(const Start: boolean): EpiFloat; inline;
    function GetAsInteger(const Start: boolean): EpiInteger; inline;
    function GetAsString(const Start: boolean): EpiString; inline;
    function GetAsTime(const Start: boolean): EpiTime; inline;
    procedure SetAsDate(const Start: boolean; const AValue: EpiDate); inline;
    procedure SetAsFloat(const Start: boolean; const AValue: EpiFloat); inline;
    procedure SetAsInteger(const Start: boolean; const AValue: EpiInteger); inline;
    procedure SetAsTime(const Start: boolean; const AValue: EpiTime); inline;
  end;

implementation

uses
  epidatafiles, math;

{ TEpiRanges }

function TEpiRanges.GetFieldType: TEpiFieldType;
begin
  result := TEpiField(Owner).FieldType;
end;

constructor TEpiRanges.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
end;

destructor TEpiRanges.Destroy;
begin
  inherited Destroy;
end;

procedure TEpiRanges.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMNode;
  ItemClass: TEpiCustomItemClass;
  NRange: TEpiRange;
begin
  // Root = <Ranges>
  Node := Root.FirstChild;

  case FieldType of
    ftInteger: ItemClass := TEpiIntRange;
    ftFloat:   ItemClass := TEpiFloatRange;
    ftDMYDate,
    ftMDYDate,
    ftYMDDate: ItemClass := TEpiDateRange;
    ftTime:    ItemClass := TEpiTimeRange;
  end;

  while Assigned(Node) do
  begin
    CheckNode(Node, rsRange);

    NRange := TEpiRange(NewItem(ItemClass));
    NRange.LoadFromXml(Node);

    Node := Node.NextSibling;
  end;
end;

{ TEpiTimeRange }

function TEpiTimeRange.GetAsDate(const Start: boolean): EpiDate;
begin
  result := trunc(AsTime[start]);
end;

function TEpiTimeRange.GetAsFloat(const Start: boolean): EpiFloat;
begin
  result := AsTime[start];
end;

function TEpiTimeRange.GetAsInteger(const Start: boolean): EpiInteger;
begin
  result := Trunc(AsTime[start]);
end;

function TEpiTimeRange.GetAsString(const Start: boolean): EpiString;
begin
  result := FormatDateTime('HH:MM:SS', AsTime[Start]);
end;

function TEpiTimeRange.GetAsTime(const Start: boolean): EpiTime;
begin
  if Start then
    result := FStart
  else
    result := FEnd;
end;

procedure TEpiTimeRange.SetAsDate(const Start: boolean; const AValue: EpiDate);
begin
  AsTime[start] := AValue;
end;

procedure TEpiTimeRange.SetAsFloat(const Start: boolean; const AValue: EpiFloat
  );
begin
  AsTime[start] := AValue;
end;

procedure TEpiTimeRange.SetAsInteger(const Start: boolean;
  const AValue: EpiInteger);
begin
  AsTime[start] := AValue;
end;

procedure TEpiTimeRange.SetAsTime(const Start: boolean; const AValue: EpiTime);
begin
  if Start then
    FStart := AValue
  else
    FEnd := AValue;
end;

{ TEpiDateRange }

function TEpiDateRange.GetAsDate(const Start: boolean): EpiDate;
begin
  if Start then
    result := FStart
  else
    result := FEnd;
end;

function TEpiDateRange.GetAsFloat(const Start: boolean): EpiFloat;
begin
  result := AsDate[start];
end;

function TEpiDateRange.GetAsInteger(const Start: boolean): EpiInteger;
begin
  result := AsDate[start];
end;

function TEpiDateRange.GetAsString(const Start: boolean): EpiString;
var
  S: String;
begin
  case TEpiField(Owner.Owner).FieldType of
    ftDMYDate: S := 'DD/MM/YYYY';
    ftMDYDate: S := 'MM/DD/YYYY';
    ftYMDDate: S := 'YYYY/MM/DD';
  end;
  result := FormatDateTime(S, AsDate[Start]);
end;

function TEpiDateRange.GetAsTime(const Start: boolean): EpiTime;
begin
  result := AsDate[Start];
end;

procedure TEpiDateRange.SetAsDate(const Start: boolean; const AValue: EpiDate);
begin
  if Start then
    FStart := AValue
  else
    FEnd := AValue;
end;

procedure TEpiDateRange.SetAsFloat(const Start: boolean; const AValue: EpiFloat
  );
begin
  AsDate[start] := Trunc(AValue);
end;

procedure TEpiDateRange.SetAsInteger(const Start: boolean;
  const AValue: EpiInteger);
begin
  AsDate[start] := AValue;
end;

procedure TEpiDateRange.SetAsTime(const Start: boolean; const AValue: EpiTime);
begin
  AsDate[start] := Trunc(AValue);
end;

{ TEpiFloatRange }

function TEpiFloatRange.GetAsDate(const Start: boolean): EpiDate;
begin
  result := Trunc(AsFloat[start]);
end;

function TEpiFloatRange.GetAsFloat(const Start: boolean): EpiFloat;
begin
  if Start then
    result := FStart
  else
    result := FEnd;
end;

function TEpiFloatRange.GetAsInteger(const Start: boolean): EpiInteger;
begin
  result := Trunc(AsFloat[start]);
end;

function TEpiFloatRange.GetAsString(const Start: boolean): EpiString;
begin
  result := Format(TEpiFloatField(Owner.Owner).FormatString, [AsFloat[Start]]);
end;

function TEpiFloatRange.GetAsTime(const Start: boolean): EpiTime;
begin
  result := AsFloat[Start];
end;

procedure TEpiFloatRange.SetAsDate(const Start: boolean; const AValue: EpiDate
  );
begin
  AsFloat[Start] := AValue;
end;

procedure TEpiFloatRange.SetAsFloat(const Start: boolean; const AValue: EpiFloat
  );
begin
  if Start then
    FStart := AValue
  else
    FEnd := AValue;
end;

procedure TEpiFloatRange.SetAsInteger(const Start: boolean;
  const AValue: EpiInteger);
begin
  AsFloat[Start] := AValue;
end;

procedure TEpiFloatRange.SetAsTime(const Start: boolean; const AValue: EpiTime
  );
begin
  AsFloat[Start] := AValue;
end;

{ TEpiRange }

function TEpiRange.GetRanges: TEpiRanges;
begin
  result := TEpiRanges(Owner);
end;

function TEpiRange.GetSingle: boolean;
begin
  case Ranges.FieldType of
    ftInteger,
    ftDMYDate,
    ftMDYDate,
    ftYMDDate: result := (AsInteger[true] - AsInteger[false]) = 0;
    ftFloat,
    ftTime:    result := (AsFloat[true] - AsFloat[false]) = 0;
  end;
end;

constructor TEpiRange.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
end;

destructor TEpiRange.Destroy;
begin
  inherited Destroy;
end;

procedure TEpiRange.LoadFromXml(Root: TDOMNode);
var
  S: String;
begin
  // Root = <Range>
  inherited LoadFromXml(Root);

  BackupFormatSettings;
  Case Ranges.FieldType of
    ftInteger: begin
                 AsInteger[true]  := LoadNodeInt(Root, rsStart);
                 AsInteger[false] := LoadNodeInt(Root, rsEnd);
               end;
    ftFloat:   begin
                 AsFloat[true]  := LoadNodeFloat(Root, rsStart);
                 AsFloat[false] := LoadNodeFloat(Root, rsEnd);
               end;
    ftDMYDate,
    ftMDYDate,
    ftYMDDate: begin
                 case TEpiField(Owner.Owner).FieldType of
                   ftDMYDate: S := 'DD/MM/YYYY';
                   ftMDYDate: S := 'MM/DD/YYYY';
                   ftYMDDate: S := 'YYYY/MM/DD';
                 end;
                 DefaultFormatSettings.ShortDateFormat := S;
                 AsDate[true] := Trunc(StrToDate(LoadNodeString(Root, rsStart)));
                 AsDate[false] := Trunc(StrToDate(LoadNodeString(Root, rsEnd)));
               end;
    ftTime:    begin
                 AsTime[true]  := LoadNodeDateTime(Root, rsStart);
                 AsTime[false] := LoadNodeDateTime(Root, rsEnd);
               end;
  end;
  RestoreFormatSettings;
end;

function TEpiRange.SaveToXml(Content: String; Lvl: integer): string;
begin
  Content :=
    SaveNode(Lvl + 1, rsStart, AsString[true]) +
    SaveNode(Lvl + 1, rsEnd, AsString[false]);
  Result := inherited SaveToXml(Content, Lvl);
end;

function TEpiRange.XMLName: string;
begin
  Result := inherited XMLName;
end;

{ TEpiIntRange }

function TEpiIntRange.GetAsDate(const Start: boolean): EpiDate;
begin
  result := AsInteger[start];
end;

function TEpiIntRange.GetAsFloat(const Start: boolean): EpiFloat;
begin
  result := AsInteger[start];
end;

function TEpiIntRange.GetAsInteger(const Start: boolean): EpiInteger;
begin
  if Start then
    Result := FStart
  else
    Result := FEnd;
end;

function TEpiIntRange.GetAsString(const Start: boolean): EpiString;
begin
  Result := IntToStr(AsInteger[Start]);
end;

function TEpiIntRange.GetAsTime(const Start: boolean): EpiTime;
begin
  result := AsInteger[start];
end;

procedure TEpiIntRange.SetAsDate(const Start: boolean; const AValue: EpiDate);
begin
  AsInteger[Start] := AValue;
end;

procedure TEpiIntRange.SetAsFloat(const Start: boolean; const AValue: EpiFloat
  );
begin
  AsInteger[Start] := Trunc(SimpleRoundTo(AValue, 0));
end;

procedure TEpiIntRange.SetAsInteger(const Start: boolean;
  const AValue: EpiInteger);
begin
  if Start then
    FStart := AValue
  else
    FEnd := AValue;
end;

procedure TEpiIntRange.SetAsTime(const Start: boolean; const AValue: EpiTime);
begin
  AsFloat[Start] := AValue;
end;

end.

