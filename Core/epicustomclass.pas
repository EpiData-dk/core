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
  protected
    Function   StringToXml(Const Src: String): string;
    Function   Ins(Level: integer): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure  SaveToStream(St: TStream; Lvl: integer); virtual; abstract;
    procedure  LoadFromXml(Root: TDOMNode); virtual; abstract;
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

constructor TEpiCustomClass.Create;
begin

end;

destructor TEpiCustomClass.Destroy;
begin
  inherited Destroy;
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

