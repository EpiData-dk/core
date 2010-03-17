unit epicustomclass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM;

type

  { TEpiCustomClass }

{$static on}
  TEpiCustomClass = class(TObject)
  private
    FErrorCode: integer; static;
    FErrorText: string; static;
  public
    constructor Create;
    destructor Destroy; override;
    procedure  LoadFromXml(Root: TDOMNode); virtual; abstract;
    Procedure  ReportXmlError(ErrCode: Integer; LangCode: integer; Msg: String; Args: array of const);
    property   ErrorCode: integer read FErrorCode write FErrorCode;
    property   ErrorText: string read FErrorText write FErrorText;
  end;
{$static off}

implementation

uses
  epilog;

{ TEpiCustomClass }

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

