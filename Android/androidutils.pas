unit androidutils;

{$mode objfpc}{$H+}

interface

uses
  jni, log;

procedure ALogDef(Const Txt: string);
procedure ALogVerb(Const Txt: string);
procedure ALogDbg(Const Txt: string);
procedure ALogInfo(Const Txt: string);
procedure ALogWarn(Const Txt: string);
procedure ALogErr(Const Txt: string);
procedure ALogFtl(Const Txt: string);
procedure ALogSlnc(Const Txt: string);

function NewJNIString(Env: PJNIEnv; S: string): jstring;
function GetJNIString(Env: PJNIEnv; S: jstring): string;
function GetJNIStringNoRelease(Env: PJNIEnv; S: jstring; out Handle: pchar): string;
procedure ReleaseString(Env: PJNIEnv; S: jstring; Handle: pchar);

implementation

uses
  sysutils;

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

function NewJNIString(Env: PJNIEnv; S: string): jstring;
var
  P: PChar;
begin
  P := StrAlloc(Length(S) + 1);
  result := Env^^.NewStringUTF(Env, StrPCopy(P, S));
  StrDispose(P);
end;

function GetJNIString(Env: PJNIEnv; S: jstring): string;
var
  IsCopy: jboolean;
  P: PChar;
begin
  ALogInfo('GetJNIString (1)');
  P := Env^^.GetStringUTFChars(Env, S, IsCopy);
  ALogInfo('GetJNIString (2): P = ' + P);
  result := StrPas(P);
  ALogInfo('GetJNIString (3): Result = ' + result);
//  Env^^.ReleaseStringUTFChars(Env, S, P);
  ALogInfo('GetJNIString: 4');
end;

function GetJNIStringNoRelease(Env: PJNIEnv; S: jstring; out Handle: pchar): string;
var
  IsCopy: jboolean;
begin
  Handle := Env^^.GetStringUTFChars(Env, S, IsCopy);
  result := StrPas(Handle);
end;

procedure ReleaseString(Env: PJNIEnv; S: jstring; Handle: pchar);
begin
  Env^^.ReleaseStringUTFChars(Env, S, Handle);
end;

end.

