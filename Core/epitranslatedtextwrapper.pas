unit epitranslatedtextwrapper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz2_DOM, epicustombase, epitranslatedtext;

type

  { TEpiTranslatedTextWrapper }

  TEpiTranslatedTextWrapper = class(TEpiTranslatedText)
  private
    FNodeName: string;
  public
    constructor Create(AOwner: TEpiCustomBase; Const NodeName, TextName: string);
    function    SaveToXml(Content: String; Lvl: integer): string; override;
    procedure   LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
  { Cloning }
  protected
    function DoCloneCreate(AOwner: TEpiCustomBase): TEpiCustomBase; override;
  protected
    function SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
  end;


implementation

uses
  epistringutils;

{ TEpiTranslatedTextWrapper }

constructor TEpiTranslatedTextWrapper.Create(AOwner: TEpiCustomBase;
  const NodeName, TextName: string);
begin
  inherited Create(AOwner, TextName);
  FNodeName := NodeName;
end;

function TEpiTranslatedTextWrapper.SaveToXml(Content: String; Lvl: integer
  ): string;
begin
  Result := inherited SaveToXml(Content, Lvl + 1);

  if Result <> '' then
    Result :=
      Indent(Lvl) + '<' + FNodeName + '>' + LineEnding +
      Result +
      Indent(Lvl) + '</' + FNodeName + '>' + LineEnding;
end;

procedure TEpiTranslatedTextWrapper.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
var
  NRoot: TDOMNode;
begin
  // Root = Parent for FNodeName (since this is a wrapped object.
  if LoadNode(NRoot, Root, FNodeName, false) then
    inherited LoadFromXml(NRoot, ReferenceMap);
end;

function TEpiTranslatedTextWrapper.DoCloneCreate(AOwner: TEpiCustomBase
  ): TEpiCustomBase;
begin
  Result := TEpiTranslatedTextWrapper.Create(AOwner, FNodeName, XMLName);
end;

function TEpiTranslatedTextWrapper.SaveToDom(RootDoc: TDOMDocument
  ): TDOMElement;
var
  Elem: TDOMElement;
  i: Integer;
  ReturnNil: Boolean;
begin
  ReturnNil := true;
  for i := 0 to FTextList.Count -1 do
    if TString(FTextList.Objects[i]).Str <> '' then
    begin
      ReturnNil := false;
      Break;
    end;

  if ReturnNil then
    Exit(nil);

  Elem := inherited SaveToDom(RootDoc);

  Result := RootDoc.CreateElement(FNodeName);
  Result.AppendChild(Elem);
end;

end.

