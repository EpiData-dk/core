unit EpiCustomBase_jni;

{$mode objfpc}{$H+}

interface

uses jni, sysutils, androidutils;

const
  EpiCustomBaseJNI = 'Java_dk_epidata_androidclient_core_EpiCustomBase_';

function EpiCustomBase_XMLName(Env: PJNIEnv; This: jobject; Self: jint): jstring; cdecl;
function EpiCustomBase_SaveToXml(Env: PJNIEnv; This: jobject; Self: jint; Content: jstring; Lvl: jint): jstring; cdecl;
procedure EpiCustomBase_LoadFromXml(Env: PJNIEnv; This: jobject; Self: jint; Root: jint); cdecl;
procedure EpiCustomBase_BeginUpdate(Env: PJNIEnv; This: jobject; Self: jint); cdecl;
procedure EpiCustomBase_EndUpdate(Env: PJNIEnv; This: jobject; Self: jint); cdecl;
procedure EpiCustomBase_RegisterOnChangeHook(Env: PJNIEnv; This: jobject; Self: jint; Event: jint; IgnoreUpdate: jboolean); cdecl;
procedure EpiCustomBase_UnRegisterOnChangeHook(Env: PJNIEnv; This: jobject; Self: jint; Event: jint); cdecl;
procedure EpiCustomBase_SetLanguage(Env: PJNIEnv; This: jobject; Self: jint; LangCode: jstring; DefaultLanguage: jboolean); cdecl;
function EpiCustomBase_GetCurrentLang(Env: PJNIEnv; This: jobject; Self: jint): jstring; cdecl;
function EpiCustomBase_GetDefaultLang(Env: PJNIEnv; This: jobject; Self: jint): jstring; cdecl;
procedure EpiCustomBase_BeforeDestruction(Env: PJNIEnv; This: jobject; Self: jint); cdecl;
procedure EpiCustomBase_Destroy(Env: PJNIEnv; This: jobject; Self: jint); cdecl;
procedure EpiCustomBase_Assign(Env: PJNIEnv; This: jobject; Self: jint; AEpiCustomBase: jint); cdecl;
function EpiCustomBase_GetOwner(Env: PJNIEnv; This: jobject; Self: jint): jint; cdecl;
function EpiCustomBase_GetRootOwner(Env: PJNIEnv; This: jobject; Self: jint): jint; cdecl;
function EpiCustomBase_GetState(Env: PJNIEnv; This: jobject; Self: jint): jint; cdecl;
function EpiCustomBase_GetModified(Env: PJNIEnv; This: jobject; Self: jint): jboolean; cdecl;
procedure EpiCustomBase_SetModified(Env: PJNIEnv; This: jobject; Self: jint; Modified: jboolean); cdecl;
function EpiCustomBase_GetOnModified(Env: PJNIEnv; This: jobject; Self: jint): jint; cdecl;
procedure EpiCustomBase_SetOnModified(Env: PJNIEnv; This: jobject; Self: jint; OnModified: jint); cdecl;
function EpiCustomBase_Clone(Env: PJNIEnv; This: jobject; Self: jint): jint; cdecl;

implementation

uses
  epicustombase, Laz2_DOM;

function EpiCustomBase_XMLName(Env: PJNIEnv; This: jobject; Self: jint): jstring; cdecl;
begin
  result := NewJNIString(Env, TEpiCustomBase(Self).XMLName());
end;

function EpiCustomBase_SaveToXml(Env: PJNIEnv; This: jobject; Self: jint; Content: jstring; Lvl: jint): jstring; cdecl;
begin
  result := NewJNIString(Env, TEpiCustomBase(Self).SaveToXml(GetJNIString(Env, Content),Lvl));
end;

procedure EpiCustomBase_LoadFromXml(Env: PJNIEnv; This: jobject; Self: jint;
  Root: jint); cdecl;
begin
  TEpiCustomBase(Self).LoadFromXml(TDOMNode(Root));
end;

procedure EpiCustomBase_BeginUpdate(Env: PJNIEnv; This: jobject; Self: jint); cdecl;
begin
  TEpiCustomBase(Self).BeginUpdate();
end;

procedure EpiCustomBase_EndUpdate(Env: PJNIEnv; This: jobject; Self: jint); cdecl;
begin
  TEpiCustomBase(Self).EndUpdate();
end;

procedure EpiCustomBase_RegisterOnChangeHook(Env: PJNIEnv; This: jobject;
  Self: jint; Event: jint; IgnoreUpdate: jboolean); cdecl;
begin
//  TEpiCustomBase(Self).RegisterOnChangeHook(TEpiChangeEvent(Event), Boolean(IgnoreUpdate));
end;

procedure EpiCustomBase_UnRegisterOnChangeHook(Env: PJNIEnv; This: jobject;
  Self: jint; Event: jint); cdecl;
begin
//  TEpiCustomBase(Self).UnRegisterOnChangeHook(TEpiChangeEvent(Event));
end;

procedure EpiCustomBase_SetLanguage(Env: PJNIEnv; This: jobject; Self: jint;
  LangCode: jstring; DefaultLanguage: jboolean); cdecl;
begin
  TEpiCustomBase(Self).SetLanguage(GetJNIString(Env, LangCode), Boolean(DefaultLanguage));
end;

function EpiCustomBase_GetCurrentLang(Env: PJNIEnv; This: jobject; Self: jint): jstring; cdecl;
begin
  result := NewJNIString(Env, TEpiCustomBase(Self).CurrentLang);
end;
function EpiCustomBase_GetDefaultLang(Env: PJNIEnv; This: jobject; Self: jint): jstring; cdecl;
begin
  result := NewJNIString(Env, TEpiCustomBase(Self).DefaultLang);
end;
procedure EpiCustomBase_BeforeDestruction(Env: PJNIEnv; This: jobject; Self: jint); cdecl;
begin
  TEpiCustomBase(Self).BeforeDestruction();
end;

procedure EpiCustomBase_Destroy(Env: PJNIEnv; This: jobject; Self: jint); cdecl;
begin
  TEpiCustomBase(Self).Destroy();
end;

procedure EpiCustomBase_Assign(Env: PJNIEnv; This: jobject; Self: jint;
  AEpiCustomBase: jint); cdecl;
begin
  TEpiCustomBase(Self).Assign(TEpiCustomBase(AEpiCustomBase));
end;

function EpiCustomBase_GetOwner(Env: PJNIEnv; This: jobject; Self: jint): jint;
  cdecl;
begin
  result := jint(TEpiCustomBase(Self).Owner);
end;
function EpiCustomBase_GetRootOwner(Env: PJNIEnv; This: jobject; Self: jint
  ): jint; cdecl;
begin
  result := jint(TEpiCustomBase(Self).RootOwner);
end;
function EpiCustomBase_GetState(Env: PJNIEnv; This: jobject; Self: jint): jint;
  cdecl;
begin
  result := jint(TEpiCustomBase(Self).State);
end;
function EpiCustomBase_GetModified(Env: PJNIEnv; This: jobject; Self: jint
  ): jboolean; cdecl;
begin
  result := jboolean(TEpiCustomBase(Self).Modified);
end;
procedure EpiCustomBase_SetModified(Env: PJNIEnv; This: jobject; Self: jint;
  Modified: jboolean); cdecl;
begin
  TEpiCustomBase(Self).Modified := Boolean(Modified);
end;
function EpiCustomBase_GetOnModified(Env: PJNIEnv; This: jobject; Self: jint
  ): jint; cdecl;
begin
//  result := TEpiCustomBase(Self).OnModified;
end;
procedure EpiCustomBase_SetOnModified(Env: PJNIEnv; This: jobject; Self: jint;
  OnModified: jint); cdecl;
begin
//  TEpiCustomBase(Self).OnModified := OnModified;
end;
function EpiCustomBase_Clone(Env: PJNIEnv; This: jobject; Self: jint): jint;
  cdecl;
begin
  result := jint(TEpiCustomBase(Self).Clone());
end;

end.

