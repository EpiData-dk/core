unit epiranges;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epicustombase, epidatafilestypes, Laz2_DOM;

type
  TEpiRange = class;


  { TEpiRanges }

  TEpiRanges = class(TEpiCustomList)
  private
    function    GetFieldType: TEpiFieldType;
    function    GetRange(const index: integer): TEpiRange;
  protected
    function    Prefix: string; override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor  Destroy; override;
    function    ItemClass: TEpiCustomItemClass; override;
    function    XMLName: string; override;
    function    NewRange: TEpiRange;
    function    InRange(const AValue: EpiInteger): boolean; overload;
    function    InRange(const AValue: EpiFloat): boolean; overload;
    function    InRange(const AValue: EpiDate): boolean; overload;
    function    InRange(const AValue: EpiTime): boolean; overload;
    function    RangesToText: string;
    property    Range[const index: integer]: TEpiRange read GetRange; default;
    property    FieldType: TEpiFieldType read GetFieldType;
  end;

  { TEpiRange }

  TEpiRange = class(TEpiCustomItem)
  private
    function    GetRanges: TEpiRanges;
    function    GetSingle: boolean;
  protected
    function    WriteNameToXml: boolean; override;
    function    GetAsDate(const Start: boolean): EpiDate; virtual; abstract;
    function    GetAsFloat(const Start: boolean): EpiFloat; virtual; abstract;
    function    GetAsInteger(const Start: boolean): EpiInteger; virtual; abstract;
    function    GetAsString(const Start: boolean): EpiString; virtual; abstract;
    function    GetAsTime(const Start: boolean): EpiTime; virtual; abstract;
    procedure   SetAsDate(const Start: boolean; const AValue: EpiDate); virtual; abstract;
    procedure   SetAsFloat(const Start: boolean; const AValue: EpiFloat); virtual; abstract;
    procedure   SetAsInteger(const Start: boolean; const AValue: EpiInteger); virtual; abstract;
    procedure   SetAsTime(const Start: boolean; const AValue: EpiTime); virtual; abstract;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor  Destroy; override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    function    SaveAttributesToXml: string; override;
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
    function GetAsDate(const Start: boolean): EpiDate; override;
    function GetAsFloat(const Start: boolean): EpiFloat; override;
    function GetAsInteger(const Start: boolean): EpiInteger; override;
    function GetAsString(const Start: boolean): EpiString; override;
    function GetAsTime(const Start: boolean): EpiTime; override;
    procedure SetAsDate(const Start: boolean; const AValue: EpiDate); override;
    procedure SetAsFloat(const Start: boolean; const AValue: EpiFloat); override;
    procedure SetAsInteger(const Start: boolean; const AValue: EpiInteger); override;
    procedure SetAsTime(const Start: boolean; const AValue: EpiTime); override;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase =
       nil): TEpiCustomBase; override;
  public
    procedure Assign(const AEpiCustomBase: TEpiCustomBase); override;
  end;

  { TEpiFloatRange }

  TEpiFloatRange = class(TEpiRange)
  private
    FStart:    EpiFloat;
    FEnd:      EpiFloat;
  protected
    function GetAsDate(const Start: boolean): EpiDate; override;
    function GetAsFloat(const Start: boolean): EpiFloat; override;
    function GetAsInteger(const Start: boolean): EpiInteger; override;
    function GetAsString(const Start: boolean): EpiString; override;
    function GetAsTime(const Start: boolean): EpiTime; override;
    procedure SetAsDate(const Start: boolean; const AValue: EpiDate); override;
    procedure SetAsFloat(const Start: boolean; const AValue: EpiFloat); override;
    procedure SetAsInteger(const Start: boolean; const AValue: EpiInteger); override;
    procedure SetAsTime(const Start: boolean; const AValue: EpiTime); override;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase =
      nil): TEpiCustomBase; override;
  public
    procedure Assign(const AEpiCustomBase: TEpiCustomBase); override;
  end;

  { TEpiDateRange }

  TEpiDateRange = class(TEpiRange)
  private
    FStart:    EpiDate;
    FEnd:      EpiDate;
  protected
    function GetAsDate(const Start: boolean): EpiDate; override;
    function GetAsFloat(const Start: boolean): EpiFloat; override;
    function GetAsInteger(const Start: boolean): EpiInteger; override;
    function GetAsString(const Start: boolean): EpiString; override;
    function GetAsTime(const Start: boolean): EpiTime; override;
    procedure SetAsDate(const Start: boolean; const AValue: EpiDate); override;
    procedure SetAsFloat(const Start: boolean; const AValue: EpiFloat); override;
    procedure SetAsInteger(const Start: boolean; const AValue: EpiInteger); override;
    procedure SetAsTime(const Start: boolean; const AValue: EpiTime); override;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase =
       nil): TEpiCustomBase; override;
  public
    procedure Assign(const AEpiCustomBase: TEpiCustomBase); override;
  end;

  { TEpiTimeRange }

  TEpiTimeRange = class(TEpiRange)
  private
    FStart:    EpiTime;
    FEnd:      EpiTime;
  protected
    function GetAsDate(const Start: boolean): EpiDate; override;
    function GetAsFloat(const Start: boolean): EpiFloat; override;
    function GetAsInteger(const Start: boolean): EpiInteger; override;
    function GetAsString(const Start: boolean): EpiString; override;
    function GetAsTime(const Start: boolean): EpiTime; override;
    procedure SetAsDate(const Start: boolean; const AValue: EpiDate); override;
    procedure SetAsFloat(const Start: boolean; const AValue: EpiFloat); override;
    procedure SetAsInteger(const Start: boolean; const AValue: EpiInteger); override;
    procedure SetAsTime(const Start: boolean; const AValue: EpiTime); override;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase =
       nil): TEpiCustomBase; override;
  public
    procedure Assign(const AEpiCustomBase: TEpiCustomBase); override;
  end;

implementation

uses
  epidatafiles, math, epidocument;

{ TEpiRanges }

function TEpiRanges.GetFieldType: TEpiFieldType;
begin
  result := TEpiField(Owner).FieldType;
end;

function TEpiRanges.GetRange(const index: integer): TEpiRange;
begin
  Result := TEpiRange(Items[Index]);
end;

function TEpiRanges.Prefix: string;
begin
  Result := 'range_id_'
end;

constructor TEpiRanges.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
end;

destructor TEpiRanges.Destroy;
begin
  inherited Destroy;
end;

function TEpiRanges.ItemClass: TEpiCustomItemClass;
begin
  case FieldType of
    ftInteger: Result := TEpiIntRange;
    ftFloat:   Result := TEpiFloatRange;
    ftDMYDate,
    ftMDYDate,
    ftYMDDate: Result := TEpiDateRange;
    ftTime:    Result := TEpiTimeRange;
  end;
end;

function TEpiRanges.XMLName: string;
begin
  Result := rsRanges;
end;

function TEpiRanges.NewRange: TEpiRange;
begin
  Result := TEpiRange(NewItem);
end;

function TEpiRanges.InRange(const AValue: EpiInteger): boolean;
var
  i: Integer;
begin
  result := false;
  for i := 0 to Count - 1 do
  with TEpiRange(Items[i]) do
    if (AsInteger[true] <= AValue) and (AValue <= AsInteger[false]) then
      exit(true);
end;

function TEpiRanges.InRange(const AValue: EpiFloat): boolean;
var
  i: Integer;
begin
  result := false;
  for i := 0 to Count - 1 do
  with TEpiRange(Items[i]) do
    if (AsFloat[true] <= AValue) and (AValue <= AsFloat[false]) then
      exit(true);
end;

function TEpiRanges.InRange(const AValue: EpiDate): boolean;
var
  i: Integer;
begin
  result := false;
  for i := 0 to Count - 1 do
  with TEpiRange(Items[i]) do
    if (AsDate[true] <= AValue) and (AValue <= AsDate[false]) then
      exit(true);
end;

function TEpiRanges.InRange(const AValue: EpiTime): boolean;
var
  i: Integer;
begin
  result := false;
  for i := 0 to Count - 1 do
  with TEpiRange(Items[i]) do
    if (AsTime[true] <= AValue) and (AValue <= AsTime[false]) then
      exit(true);
end;

function TEpiRanges.RangesToText: string;
var
  i: integer;
  Field: TEpiField;
begin
  result := '';

  Field := TEpiField(Owner);
  for i := 0 to Count - 1 do
  with TEpiRange(Items[i]) do
  begin
    if FieldType = ftFloat then
      Result := Result + Format(Field.FormatString, [AsFloat[true]])
    else
      Result := Result + AsString[true];

    if not Single then
      if FieldType = ftFloat then
        Result := Result + '-' + Format(Field.FormatString, [AsFloat[false]])
      else
        Result := Result + '-' + AsString[false];
    Result := Result + '|';
  end;
  if Length(Result) > 0 then
    Delete(Result, Length(Result), 1);
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

function TEpiTimeRange.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase
  ): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest);
  with TEpiTimeRange(Result) do
  begin
    FStart := Self.FStart;
    FEnd := Self.FEnd;
  end;
end;

procedure TEpiTimeRange.Assign(const AEpiCustomBase: TEpiCustomBase);
var
  OrgRange: TEpiTimeRange absolute AEpiCustomBase;
begin
  inherited Assign(AEpiCustomBase);
  BeginUpdate;
  FStart := OrgRange.FStart;
  FEnd   := OrgRange.FEnd;
  EndUpdate;
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

function TEpiDateRange.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase
  ): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest);
  with TEpiDateRange(Result) do
  begin
    FStart := Self.FStart;
    FEnd := Self.FEnd;
  end;
end;

procedure TEpiDateRange.Assign(const AEpiCustomBase: TEpiCustomBase);
var
  OrgRange: TEpiDateRange absolute AEpiCustomBase;
begin
  inherited Assign(AEpiCustomBase);
  BeginUpdate;
  FStart := OrgRange.FStart;
  FEnd   := OrgRange.FEnd;
  EndUpdate;
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
  result := FloatToStr(AsFloat[Start]);
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

function TEpiFloatRange.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase
  ): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest);
  with TEpiFloatRange(Result) do
  begin
    FStart := Self.FStart;
    FEnd := Self.FEnd;
  end;
end;

procedure TEpiFloatRange.Assign(const AEpiCustomBase: TEpiCustomBase);
var
  OrgRange: TEpiFloatRange absolute AEpiCustomBase;
begin
  inherited Assign(AEpiCustomBase);
  BeginUpdate;
  FStart := OrgRange.FStart;
  FEnd   := OrgRange.FEnd;
  EndUpdate;
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

function TEpiRange.WriteNameToXml: boolean;
begin
  Result := false;
end;

function TEpiRange.SaveAttributesToXml: string;
begin
  BackupFormatSettings(TEpiDocument(RootOwner).XMLSettings.FormatSettings);
  Result := inherited SaveAttributesToXml +
    ' start="' + AsString[true] + '" end="' + AsString[false] + '"';
  RestoreFormatSettings;
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

  Case Ranges.FieldType of
    ftInteger: begin
                 AsInteger[true]  := LoadAttrInt(Root, rsStart);
                 AsInteger[false] := LoadAttrInt(Root, rsEnd);
               end;
    ftFloat:   begin
                 AsFloat[true]  := LoadAttrFloat(Root, rsStart);
                 AsFloat[false] := LoadAttrFloat(Root, rsEnd);
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
                 AsDate[true] := Trunc(LoadAttrDateTime(Root, rsStart, S));
                 AsDate[false] := Trunc(LoadAttrDateTime(Root, rsEnd, S));
               end;
    ftTime:    begin
                 ShortDateFormat := 'HH:NN:SS';
                 AsTime[true]  := LoadAttrDateTime(Root, rsStart, S);
                 AsTime[false] := LoadAttrDateTime(Root, rsEnd, S);
               end;
  end;
end;

function TEpiRange.XMLName: string;
begin
  Result := rsRange;
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

function TEpiIntRange.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase
  ): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest);
  with TEpiIntRange(Result) do
  begin
    FStart := Self.FStart;
    FEnd := Self.FEnd;
  end;
end;

procedure TEpiIntRange.Assign(const AEpiCustomBase: TEpiCustomBase);
var
  OrgRange: TEpiIntRange absolute AEpiCustomBase;
begin
  inherited Assign(AEpiCustomBase);
  BeginUpdate;
  FStart := OrgRange.FStart;
  FEnd   := OrgRange.FEnd;
  EndUpdate;
end;

end.

