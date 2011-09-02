unit epiversionutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lNetSSL, lhttp, lHTTPUtil, lnet;

type

  TEpiVersionInfo = record
    VersionNo: Integer;
    MajorRev:  Integer;
    MinorRev:  Integer;
    BuildNo:   Integer;
  end;

  function GetCoreRevision: string;
  function GetCoreVersionInfo: string;
  function GetEpiVersionInfo(VersionInfo: TEpiVersionInfo): string;
  function CheckVersionOnline(Const ProgramName: String;
    out StableVersion: TEpiVersionInfo;
    out TestVersion: TEpiVersionInfo;
    out Response: string): boolean;

implementation

uses
  strutils;

{$IFDEF EPI_SHOWREVISION}
  {$I corerevision.inc}
{$ELSE}
  const RevisionStr = '(DEBUG)';
{$ENDIF}

const
  CoreVersion: TEpiVersionInfo = (
    VersionNo: 0;
    MajorRev:  8;
    MinorRev:  0;
    BuildNo:   0;
  );

  EpiDataURL = 'http://www.epidata.dk/version/checkversion.php';

function GetCoreRevision: string;
begin
  result := RevisionStr;
end;

function GetCoreVersionInfo: string;
begin
  result := GetEpiVersionInfo(CoreVersion);
end;

function GetEpiVersionInfo(VersionInfo: TEpiVersionInfo): string;
begin
  with VersionInfo do
  begin
    result := IntToStr(VersionNo);
    if MajorRev + MinorRev + BuildNo > 0 then
      result := result + '.' + IntToStr(MajorRev);
    if MinorRev + BuildNo > 0 then
      result := result + '.' + IntToStr(MinorRev);
    if BuildNo > 0 then
      result := result + '.' + IntToStr(BuildNo);
  end;
end;

{ TEpiVersionChecker }

type
  TEpiVersionChecker = class
  private
    FResponse: string;
    HTTPClient: TLHTTPClient;
    Session: TLSession;
    Done: boolean;
    ConnectOK: boolean;
    procedure HTTPClientDisconnect(aSocket: TLSocket);
    procedure HTTPClientDoneInput(ASocket: TLHTTPClientSocket);
    procedure HTTPClientError(const msg: string; aSocket: TLSocket);
    function  HTTPClientInput(ASocket: TLHTTPClientSocket; ABuffer: pchar;
      ASize: integer): integer;
    procedure HTTPClientProcessHeaders(ASocket: TLHTTPClientSocket);
  public
    function CheckVersionOnline(Const URL: string; var StableVersion: TEpiVersionInfo;
      var TestVersion: TEpiVersionInfo): boolean;
    property Response: string read FResponse;
  end;

procedure TEpiVersionChecker.HTTPClientDisconnect(aSocket: TLSocket);
begin
  Done := true;
end;

procedure TEpiVersionChecker.HTTPClientDoneInput(ASocket: TLHTTPClientSocket);
begin
  ASocket.Disconnect;
  ConnectOK := true;
end;

procedure TEpiVersionChecker.HTTPClientError(const msg: string;
  aSocket: TLSocket);
begin
  FResponse := msg;
  Done := true;
  ConnectOK := false;
end;

function TEpiVersionChecker.HTTPClientInput(ASocket: TLHTTPClientSocket;
  ABuffer: pchar; ASize: integer): integer;
begin
  SetLength(FResponse, ASize);
  Move(ABuffer^, FResponse[1], ASize);
  Result := aSize; // tell the http buffer we read it all
end;

procedure TEpiVersionChecker.HTTPClientProcessHeaders(
  ASocket: TLHTTPClientSocket);
begin
// Header response
end;

function TEpiVersionChecker.CheckVersionOnline(const URL: string;
  var StableVersion: TEpiVersionInfo; var TestVersion: TEpiVersionInfo
  ): boolean;
var
  Host: string;
  URI: string;
  Port: Word;
begin
  FResponse := '';
  ConnectOK := false;
  HttpClient := TLHTTPClient.Create(nil);
  Session    := TLSession.Create(HTTPClient);

  try
    DecomposeURL(URL, Host, URI, Port);
    HttpClient.Session := Session;
    HttpClient.Host := Host;
    HttpClient.Method := hmGet;
    HttpClient.Port := Port;
    HttpClient.URI := URI;
    HttpClient.Timeout := -1;
    HttpClient.OnDisconnect := @HTTPClientDisconnect;
    HttpClient.OnDoneInput := @HTTPClientDoneInput;
    HttpClient.OnError := @HTTPClientError;
    HttpClient.OnInput := @HTTPClientInput;
    HttpClient.OnProcessHeaders := @HTTPClientProcessHeaders;
    HttpClient.SendRequest;
    Done := false;

    while not Done do
      HttpClient.CallAction;
  finally
    HttpClient.Free;
    Result := ConnectOK;
  end;
end;

function CheckVersionOnline(const ProgramName: String;
  out StableVersion: TEpiVersionInfo; out TestVersion: TEpiVersionInfo;
  out Response: string): boolean;
var
  VersionChecker: TEpiVersionChecker;
  URL: String;
  List: TStringList;
begin
  result := false;
  try
    FillByte(StableVersion, SizeOf(TEpiVersionInfo), 0);
    FillByte(TestVersion, SizeOf(TEpiVersionInfo), 0);

    URL := EpiDataURL +
      '?program=' + LowerCase(ProgramName) +
      '&os=' + LowerCase({$I %FPCTARGETOS%}) +
      '&arch=' + LowerCase({$I %FPCTARGETCPU%});

    VersionChecker := TEpiVersionChecker.Create;
    Result := VersionChecker.CheckVersionOnline(URL, StableVersion, TestVersion);
    Response := VersionChecker.Response;

    if (Pos(LowerCase(Response), 'error') > 0) or (not result) then exit(false);

    List := TStringList.Create;
    List.DelimitedText := Response;

    try
      if List.Count >= 1 then
      With StableVersion do
      begin
        URL := List[0];
        VersionNo := StrToInt(Copy2SymbDel(URL, '.'));
        MajorRev  := StrToInt(Copy2SymbDel(URL, '.'));
        MinorRev  := StrToInt(Copy2SymbDel(URL, '.'));
        BuildNo   := StrToInt(URL);
      end;

      if List.Count >= 2 then
      With TestVersion do
      begin
        URL := List[1];
        VersionNo := StrToInt(Copy2SymbDel(URL, '.'));
        MajorRev  := StrToInt(Copy2SymbDel(URL, '.'));
        MinorRev  := StrToInt(Copy2SymbDel(URL, '.'));
        BuildNo   := StrToInt(URL);
      end;
    except
      Response := 'Invalid response from server:' + LineEnding +
        Response;
      Exit;
    end;
    Response := '';
    result := true;
  finally
    VersionChecker.Free;
  end;
end;

end.

