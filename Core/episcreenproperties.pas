unit episcreenproperties;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, epicustomclass;

type
  TEpiScreenProperties = class;
  TEpiScreenProperty = class;

  { TEpiScreenProperty }

  TEpiScreenProperty = class(TEpiCustomClass)
  private
    FName: string;
    FId:       string;
    FFgColour: integer;
    FBgColour: integer;
    FHlColour: integer;
  public
    constructor Create(AOwner: TObject); override;
    destructor  Destroy; override;
    procedure   Clone(var Dest: TEpiScreenProperty);
    procedure   Reset;
    procedure   SaveToStream(St: TStream; Lvl: integer); override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    property    Id: string read FId write FId;
    property    Name: string read FName write FName;
    property    FgColour: integer read FFgColour write FFgColour;
    property    BgColour: integer read FBgColour write FBgColour;
    property    HlColour: integer read FHlColour write FHlColour;
  end;

  { TEpiScreenProperties }

  TEpiScreenProperties = class(TEpiCustomClass)
  private
    FOwned: Boolean;
//    FOwner:   TEpiDataFile;
    FList:    TFPList;
    FReportOnChange: Boolean;
    FDefaultScreenProperty: TEpiScreenProperty;
    function GetCount: Cardinal;
    function GetDefaultScreenProperty: TEpiScreenProperty;
    function GetScreenProperty(Index: integer): TEpiScreenProperty;
  public
    constructor Create(AOwner: TObject); virtual;
    destructor  Destroy; override;
    // TODO :   Clone!
    // TODO :   Reset!
    procedure   SaveToStream(St: TStream; Lvl: integer); override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    procedure   Add(aScreenProperty: TEpiScreenProperty);
    procedure   Delete(aScreenProperty: TEpiScreenProperty);
    function    ScreenPropertyById(Const aScreenPropertyId: string): TEpiScreenProperty;
    function    ScreenPropertyExists(Const aScreenPropertyId: string; var aScreenProperty: TEpiScreenProperty): boolean;
    function    IndexOf(Const aScreenPropertyId: string): integer;
    Property    DefaultScreenProperty: TEpiScreenProperty read GetDefaultScreenProperty;
    Property    ScreenProperty[Index: integer]: TEpiScreenProperty read GetScreenProperty; default;
    Property    Count: Cardinal read GetCount;
    Property    ReportOnChange: Boolean read FReportOnChange write FReportOnChange;
    Property    Owned: Boolean read FOwned write FOwned;
  end;


implementation

uses
  epidataglobals, strutils;

{ TEpiScreenProperty }

constructor TEpiScreenProperty.Create(AOwner: TObject);
begin
  inherited;
  Reset;
end;

destructor TEpiScreenProperty.Destroy;
begin
  Reset;
  inherited Destroy;
end;

procedure TEpiScreenProperty.Clone(var Dest: TEpiScreenProperty);
begin
  if not Assigned(Dest) then
    Dest := TEpiScreenProperty.Create(nil);

  Dest.FId       := FId;
  Dest.FName     := FName;
  Dest.FFgColour := FFgColour;
  Dest.FBgColour := FBgColour;
  Dest.FHlColour := FHlColour;
end;

procedure TEpiScreenProperty.Reset;
begin
  Id       := '';
  Name     := '';
  FgColour := EpiColourBase;
  BgColour := EpiColourBase;
  HlColour := EpiColourBase;
end;

procedure TEpiScreenProperty.SaveToStream(St: TStream; Lvl: integer);
var
  S: String;
begin
  S :=
    Ins(Lvl) +  '<Colour>' + LineEnding +
    Ins(Lvl + 1) + '<Name>' + StringToXml(Name) + '</Name>' + LineEnding +
    Ins(Lvl + 1) + '<ForeGround>' + hexStr(FgColour, 8) + '</ForeGround>' + LineEnding +
    Ins(Lvl + 1) + '<BackGround>' + hexStr(BgColour, 8) + '</BackGround>' + LineEnding +
    Ins(Lvl + 1) + '<HighLight>' + hexStr(HlColour, 8) + '</HighLight>' + LineEnding +
    Ins(Lvl) +  '</Colour>' + LineEnding;
  St.Write(S[1], Length(S));
end;

procedure TEpiScreenProperty.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMNode;
begin
  // Root = <Colour>
  Id := UTF8Encode(TDOMElement(Root).GetAttribute('id'));

  Node := Root.FindNode('Name');
  Name := UTF8Encode(Node.TextContent);

  Node := Root.FindNode('ForeGround');
  FgColour := Hex2Dec(Node.TextContent);

  Node := Root.FindNode('BackGround');
  BgColour := Hex2Dec(Node.TextContent);

  Node := Root.FindNode('HighLight');
  HlColour := Hex2Dec(Node.TextContent);
end;

{ TEpiScreenProperties }

function TEpiScreenProperties.GetCount: Cardinal;
begin
  Result := FList.Count;
end;

function TEpiScreenProperties.GetDefaultScreenProperty: TEpiScreenProperty;
begin
  if not Assigned(FDefaultScreenProperty) then
  begin
    FDefaultScreenProperty := TEpiScreenProperty.Create(Self);
    FDefaultScreenProperty.Id := '';
    FDefaultScreenProperty.BgColour := EpiColourBase;
    FDefaultScreenProperty.FgColour := EpiColourBase;
    FDefaultScreenProperty.HlColour := EpiColourBase;
  end;
  result := FDefaultScreenProperty;
end;

function TEpiScreenProperties.GetScreenProperty(Index: integer
  ): TEpiScreenProperty;
begin
  Result := TEpiScreenProperty(FList[Index]);
end;

constructor TEpiScreenProperties.Create(AOwner: TObject);
begin
  FList := TFPList.Create;
//  FOwner := AOwner;
  FReportOnChange := false;
  FDefaultScreenProperty := nil;
end;

destructor TEpiScreenProperties.Destroy;
var
  S: TEpiScreenProperty;
begin
  while FList.Count > 0 do
  begin
    if Owned then
    begin
      S := TEpiScreenProperty(FList.Last);
      FreeAndNil(S);
    end;
    Flist.Delete(Flist.Count - 1);
  end;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TEpiScreenProperties.SaveToStream(St: TStream; Lvl: integer);
var
  S: String;
  i: Integer;
begin
  S :=
    Ins(Lvl) +  '<ScreenInfo>' + LineEnding +
    Ins(Lvl + 1) + '<Form>' + LineEnding +
    Ins(Lvl + 2) +   'Not Implemented Yet' + LineEnding +
    Ins(Lvl + 1) + '</Form>' + LineEnding +
    Ins(Lvl + 1) + '<Colours>' + LineEnding;
  St.Write(S[1], Length(S));

  for i := 0 to Count - 1 do
    ScreenProperty[i].SaveToStream(St, Lvl + 2);

  S :=
    Ins(Lvl + 1) + '</Colours>' + LineEnding +
    Ins(Lvl) +  '</ScreenInfo>' + LineEnding;
  St.Write(S[1], Length(S));
end;

procedure TEpiScreenProperties.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMNode;
  NewScreenProperty: TEpiScreenProperty;
begin
  // Root = <ScreenInfo>
  Node := Root.FindNode('Form');
  if Assigned(Node) then
  begin
    // TODO : Form information.
  end;

  Node := Root.FindNode('Colours');
  if Assigned(Node) then
  begin
    // Node = <Colours>
    Node := Node.FirstChild;
    while Assigned(Node) do
    begin
      // Node = <Colour>
      if Node.CompareName('Colour') <> 0 then
        ReportXmlError(EPI_XML_TAG_MISSING, 0 , '', []);

      NewScreenProperty := TEpiScreenProperty.Create(Self);
      NewScreenProperty.LoadFromXml(Node);
      Add(NewScreenProperty);

      Node := Node.NextSibling;
    end;
  end;
end;

procedure TEpiScreenProperties.Add(aScreenProperty: TEpiScreenProperty);
begin
  FList.Add(aScreenProperty);

//  if ReportOnChange and Assigned(FOwner) then
//    FOwner.DoChange(dceAddScreenProp, nil);
end;

procedure TEpiScreenProperties.Delete(aScreenProperty: TEpiScreenProperty);
var
  Idx: LongInt;
begin
  Idx := IndexOf(aScreenProperty.Id);
  FList.Delete(Idx);

//  if ReportOnChange and Assigned(FOwner) then
//    FOwner.DoChange(dceRemoveScreenProp, aScreenProperty);
end;

function TEpiScreenProperties.ScreenPropertyById(const aScreenPropertyId: string
  ): TEpiScreenProperty;
var
  i: LongInt;
begin
  Result := nil;
  i := IndexOf(aScreenPropertyId);
  if i >= 0 then
    result := TEpiScreenProperty(FList[i]);
end;

function TEpiScreenProperties.ScreenPropertyExists(
  const aScreenPropertyId: string; var aScreenProperty: TEpiScreenProperty
  ): boolean;
begin
  aScreenProperty := ScreenPropertyById(aScreenPropertyId);
  result := Assigned(aScreenProperty);
end;

function TEpiScreenProperties.IndexOf(const aScreenPropertyId: string
  ): integer;
begin
  for result := 0 to FList.Count - 1 do
    if CompareText(TEpiScreenProperty(Flist[result]).Id, aScreenPropertyId) = 0 then
      exit;
  result := -1;
end;

end.

