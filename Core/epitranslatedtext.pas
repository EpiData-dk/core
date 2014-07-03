unit epitranslatedtext;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epicustombase, Laz2_DOM;

type
  { TEpiTranslatedText }

  TEpiTranslatedText = class(TEpiCustomBase)
  protected
    FTextList: TStringList;
    FCurrentText: String;
    FXMLName: string;
    procedure   SetCurrentText(const AValue: string);
    procedure   SetText(Const LangCode: string; Const AText: string);
    function    GetText(Const LangCode: string): string;
  protected
    procedure   Clear;
  public
    constructor Create(AOwner: TEpiCustomBase; Const aXMLName: string); virtual;
    destructor  Destroy; override;
    function    SaveToXml(Content: String; Lvl: integer): string; override;
    procedure   LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    procedure   Assign(const AEpiCustomBase: TEpiCustomBase); override;
    procedure   SetLanguage(Const LangCode: string;
      Const DefaultLanguage: boolean); override;
    function    XMLName: string; override;
    property    Text: string read FCurrentText write SetCurrentText;
    property    TextLang[LangCode: string]: string read GetText write SetText;
  { Cloning }
  protected
    function DoCloneCreate(AOwner: TEpiCustomBase): TEpiCustomBase; override;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
      ReferenceMap: TEpiReferenceMap): TEpiCustomBase; override;

  protected
    function SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
  end;

implementation

uses
  epistringutils;

{ TEpiTranslatedText }

procedure TEpiTranslatedText.SetCurrentText(const AValue: string);
var
  Val: String;
begin
  if FCurrentText = AValue then exit;
  Val := FCurrentText;
  SetText(CurrentLang, AValue);
  FCurrentText := AValue;
  DoChange(eegCustomBase, Word(ecceText), @Val);
end;

procedure TEpiTranslatedText.SetLanguage(const LangCode: string;
  const DefaultLanguage: boolean);
var
  Idx:  integer;
  Val: String;
begin
  // First seek "new" language
  if FTextList.Find(LangCode, Idx) then
  begin
    Val := FCurrentText;
    FCurrentText := TString(FTextList.Objects[Idx]).Str;
    DoChange(eegCustomBase, Word(ecceText), @Val);
  end
  // Fallback to default language
  else if (FTextList.Find(DefaultLang, Idx)) and (not DefaultLanguage) then
  begin
    Val := FCurrentText;
    FCurrentText := TString(FTextList.Objects[Idx]).Str;
    DoChange(eegCustomBase, Word(ecceText), @Val);
  end
  // If new default language does not exists create empty entry.
  else if DefaultLanguage then
  begin
    SetText(LangCode, '');
    FCurrentText := '';
  end;
  inherited SetLanguage(LangCode, DefaultLanguage);
end;

function TEpiTranslatedText.XMLName: string;
begin
  Result := FXMLName;
end;

procedure TEpiTranslatedText.Clear;
var
  i: Integer;
begin
  for i := FTextList.Count - 1 downto 0 do
    FTextList.Objects[i].Free;
  FTextList.Clear;
end;

constructor TEpiTranslatedText.Create(AOwner: TEpiCustomBase;
  const aXMLName: string);
begin
  inherited Create(AOwner);
  FTextList := TStringList.Create;
  FTextList.Sorted := true;
  FXMLName := aXMLName
end;

destructor TEpiTranslatedText.Destroy;
begin
  FXMLName := '';
  FCurrentText := '';

  Clear;

  FTextList.Free;
  inherited Destroy;
end;

function TEpiTranslatedText.SaveToXml(Content: String; Lvl: integer): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to FTextList.Count - 1 do
    if TString(FTextList.Objects[i]).Str <> '' then
      Result += Indent(Lvl) + '<' + XMLName + ' xml:lang="' + FTextList[i] + '">' +
                StringToXml(TString(FTextList.Objects[i]).Str) + '</' + XMLName + '>' + LineEnding;
end;

procedure TEpiTranslatedText.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
var
  ElemList: TDOMNodeList;
  LangCode: String;
  Val: String;
  i: Integer;
begin
  // Root = <Containing Section>
  { eg.
    <Group> - this is root.
      <Name xml:lang="en">Group A</Name>
      <Name xml:lang="dk">Gruppe A</Name>
      ...
    </Group>
  }

  ElemList := TDOMElement(Root).GetElementsByTagName(XMLName);
  for i := 0 to ElemList.Count -1 do
  begin
    // Ugly hack to prevent looking at nodes that is not directly below the root.
    if ElemList[i].ParentNode <> Root then continue;

    LangCode := TDOMElement(ElemList[i]).AttribStrings['xml:lang'];
    Val := ElemList[i].TextContent;
    if (LangCode = CurrentLang) or (LangCode = '') then
      SetCurrentText(Val)
    else
      SetText(LangCode, Val);
  end;
  ElemList.Free;
end;

procedure TEpiTranslatedText.Assign(const AEpiCustomBase: TEpiCustomBase);
var
  OrgText: TEpiTranslatedText absolute AEpiCustomBase;
  i: Integer;
begin
  inherited Assign(AEpiCustomBase);
  BeginUpdate;
  FXMLName := OrgText.FXMLName;
  FCurrentText := OrgText.FCurrentText;

  // Clear content before assign
  Clear;

  for i := 0 to OrgText.FTextList.Count - 1 do
    FTextList.AddObject(OrgText.FTextList[i], TString.Create(TString(OrgText.FTextList.Objects[i]).Str));
  EndUpdate;
end;

function TEpiTranslatedText.DoCloneCreate(AOwner: TEpiCustomBase
  ): TEpiCustomBase;
begin
  Result := TEpiTranslatedText.Create(AOwner, XMLName);
end;

function TEpiTranslatedText.DoClone(AOwner: TEpiCustomBase;
  Dest: TEpiCustomBase; ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
var
  i: Integer;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);

  with TEpiTranslatedText(Result) do
  begin
    Clear;

    for i := 0 to Self.FTextList.Count - 1 do
      FTextList.AddObject(Self.FTextList[i], TString.Create(TString(Self.FTextList.Objects[i]).Str));

    FCurrentText := Self.FCurrentText;
  end;
end;

function TEpiTranslatedText.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
var
  i: Integer;
  Elem: TDOMText;
begin
  Result := inherited SaveToDom(RootDoc);

  for i := 0 to FTextList.Count - 1 do
  begin
    if TString(FTextList.Objects[i]).Str <> '' then
    begin
      SaveDomAttr(Result, 'xml:lang', FTextList[i]);
      Result.TextContent := TString(FTextList.Objects[i]).Str;
    end;
  end;
end;

procedure TEpiTranslatedText.SetText( const LangCode: string;
  const AText: string);
var
  Idx: integer;
  Val: String;
begin
  Val := '';
  if FTextList.Find(LangCode, Idx) then
  begin
    Val := TString(FTextList.Objects[Idx]).Str;
    TString(FTextList.Objects[Idx]).Str := AText;
  end else
    FTextList.AddObject(LangCode, TString.Create(AText));
  DoChange(eegCustomBase, Word(ecceText), @Val);
end;

function TEpiTranslatedText.GetText(const LangCode: string): string;
var
  Idx: integer;
begin
  if FTextList.Find(LangCode, Idx) then
    Result := TString(FTextList.Objects[Idx]).Str
  else begin
    FTextList.Find(DefaultLang, Idx);
    Result := TString(FTextList.Objects[Idx]).Str;
  end;
end;

end.

