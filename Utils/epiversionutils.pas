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
    SpecialBuild: string;
  end;

  function GetCoreRevision: string;
  function GetCoreVersionInfo: string;
  function GetEpiVersionInfo(VersionInfo: TEpiVersionInfo): string; overload;
  function GetEpiVersionInfo(TheProgram: THandle): string; overload;
  function GetEpiVersion(TheProgram: THandle): TEpiVersionInfo; overload;
  function CheckVersionOnline(Const ProgramName: String;
    out StableVersion: TEpiVersionInfo;
    out TestVersion: TEpiVersionInfo;
    out Response: string): boolean;

implementation

uses
  strutils, vinfo;

  {$I epidatacore.revision.inc}

const
  CoreVersion: TEpiVersionInfo = (
  {$I epidatacore.version.inc}
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

function BuildVersionInfoString(Version, Major, Minor, Build: integer): string; overload;
begin
  result := IntToStr(Version) + '.' +
            IntToStr(Major)   + '.' +
            IntToStr(Minor)   + '.' +
            IntToStr(Build);
end;

function GetEpiVersionInfo(VersionInfo: TEpiVersionInfo): string;
begin
  with VersionInfo do
  begin
    Result := BuildVersionInfoString(VersionNo, MajorRev, MinorRev, BuildNo);
    if SpecialBuild <> '' then
      Result := Result + ' [' + SpecialBuild + ']';
  end;
end;

function GetEpiVersionInfo(TheProgram: THandle): string;
begin
  result := GetEpiVersionInfo(GetEpiVersion(TheProgram));
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

function GetEpiVersion(TheProgram: THandle): TEpiVersionInfo;
var
  Info: TVersionInfo;
  S: String;
begin
  Info := TVersionInfo.Create;
  Info.Load(TheProgram);
  with Result, Info.FixedInfo do
  begin
    VersionNo := FileVersion[0];
    MajorRev  := FileVersion[1];
    MinorRev  := FileVersion[2];
    BuildNo   := FileVersion[3];
  end;
  if Info.StringFileInfo.Count > 0 then
  try
    Result.SpecialBuild := Info.StringFileInfo.Items[0].Values['SpecialBuild'];
  except
    // Do nothing
  end;
  Info.Free;
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

