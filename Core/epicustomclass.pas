unit epicustomclass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, DCPrijndael;

type

  { TEpiCustomClass }

{$static on}
  TEpiCustomClass = class(TObject)
  private
    FErrorCode: integer; static;
    FErrorText: string; static;
    AESCrypter: TDCP_rijndael;
    FOwner: TObject;
  protected
    Function   StringToXml(Const Src: String): string;
    Function   Ins(Level: integer): string;
    property   Owner: TObject read FOwner;
  public
    constructor Create(AOwner: TObject); virtual;
    destructor Destroy; override;
    procedure  SaveToStream(St: TStream; Lvl: integer); virtual;
    procedure  LoadFromXml(Root: TDOMNode); virtual;
    Procedure  ReportXmlError(ErrCode: Integer; LangCode: integer; Msg: String; Args: array of const);
    property   ErrorCode: integer read FErrorCode write FErrorCode;
    property   ErrorText: string read FErrorText write FErrorText;
  end;
{$static off}

implementation

uses
  epilog, strutils, DCPsha256, DCPsha1, DCPbase64;

{ TEpiCustomClass }

function TEpiCustomClass.StringToXml(const Src: String): string;
var
  i: Integer;
begin
  for i := 1 to Length(Src) do
  begin
    case Src[i] of
      '&':
        Result := Result  + '&amp;';
      '"':
        Result := Result  + '&quot;';
      '<':
        Result := Result  + '&lt;';
      '>':
        Result := Result  + '&gt;';
    else
      Result := Result  + Src[i];
    end;
  end;
end;

function TEpiCustomClass.Ins(Level: integer): string;
begin
  result := DupeString(' ', Level);
end;

constructor TEpiCustomClass.Create(AOwner: TObject);
begin
  FOwner := AOwner;
end;

destructor TEpiCustomClass.Destroy;
begin
  inherited Destroy;
end;

procedure TEpiCustomClass.SaveToStream(St: TStream; Lvl: integer);
var
  S: String;
begin
  S := Ins(LvL) + '<' + ClassName + '>Not Implemented Yet</' + ClassName + '>' + LineEnding;
  St.Write(S[1], Length(S));
end;

procedure TEpiCustomClass.LoadFromXml(Root: TDOMNode);
begin
    //
end;

procedure TEpiCustomClass.ReportXmlError(ErrCode: Integer;
  LangCode: integer; Msg: String; Args: array of const);
begin
  ErrorCode := ErrCode;
//  ErrorText := Format(Lang(LangCode, Msg), Args);
  EpiLogger.AddError(ClassName, 'InternalOpen', ErrorText, LangCode);
  Abort;
end;

end.

