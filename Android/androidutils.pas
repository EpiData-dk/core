unit androidutils;

{$mode objfpc}{$H+}

interface

uses
  log;

procedure ALogDef(Const Txt: string);
procedure ALogVerb(Const Txt: string);
procedure ALogDbg(Const Txt: string);
procedure ALogInfo(Const Txt: string);
procedure ALogWarn(Const Txt: string);
procedure ALogErr(Const Txt: string);
procedure ALogFtl(Const Txt: string);
procedure ALogSlnc(Const Txt: string);

implementation

procedure _ALog(Prio: Longint; Const Txt: string);
begin
  __android_log_write(Prio, 'epidatacore', PChar(Txt));
end;

procedure ALogDef(const Txt: string);
begin
  _ALog(ANDROID_LOG_DEFAULT, Txt);
end;

procedure ALogVerb(const Txt: string);
begin
  _ALog(ANDROID_LOG_VERBOSE, Txt);
end;

procedure ALogDbg(const Txt: string);
begin
  _ALog(ANDROID_LOG_DEBUG, Txt);
end;

procedure ALogInfo(const Txt: string);
begin
  _ALog(ANDROID_LOG_INFO, Txt);
end;

procedure ALogWarn(const Txt: string);
begin
  _ALog(ANDROID_LOG_WARN, Txt);
end;

procedure ALogErr(const Txt: string);
begin
  _ALog(ANDROID_LOG_ERROR, Txt);
end;

procedure ALogFtl(const Txt: string);
begin
  _ALog(ANDROID_LOG_FATAL, Txt);
end;

procedure ALogSlnc(const Txt: string);
begin
  _ALog(ANDROID_LOG_SILENT, Txt);
end;

end.

