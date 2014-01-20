unit EpiCustomItem_jni;

{$mode objfpc}{$H+}

interface

uses jni, sysutils, androidutils;

const
  EpiCustomItemJNI = 'Java_dk_epidata_androidclient_core_EpiCustomItem_';

procedure EpiCustomItem_Destroy(Env: PJNIEnv; This: jobject; Self: jint); cdecl;
procedure EpiCustomItem_LoadFromXml(Env: PJNIEnv; This: jobject; Self: jint; Root: jint); cdecl;
function EpiCustomItem_ValidateRename(Env: PJNIEnv; This: jobject; Self: jint; NewName: jstring; RenameOnSuccess: jboolean): jboolean; cdecl;
function EpiCustomItem_GetName(Env: PJNIEnv; This: jobject; Self: jint): jstring; cdecl;
procedure EpiCustomItem_SetName(Env: PJNIEnv; This: jobject; Self: jint; Name: jstring); cdecl;
procedure EpiCustomItem_AddCustomData(Env: PJNIEnv; This: jobject; Self: jint; Key: jstring; Obj: jint); cdecl;
function EpiCustomItem_FindCustomData(Env: PJNIEnv; This: jobject; Self: jint; Key: jstring): jint; cdecl;
function EpiCustomItem_RemoveCustomData(Env: PJNIEnv; This: jobject; Self: jint; Key: jstring): jint; cdecl;

implementation

uses
  epicustombase, Laz2_DOM;

procedure EpiCustomItem_Destroy(Env: PJNIEnv; This: jobject; Self: jint); cdecl;
begin
  TEpiCustomItem(Self).Destroy();
end;

procedure EpiCustomItem_LoadFromXml(Env: PJNIEnv; This: jobject; Self: jint;
  Root: jint); cdecl;
begin
  TEpiCustomItem(Self).LoadFromXml(TDOMNode( Root));
end;

function EpiCustomItem_ValidateRename(Env: PJNIEnv; This: jobject; Self: jint;
  NewName: jstring; RenameOnSuccess: jboolean): jboolean; cdecl;
begin
  result := jboolean(TEpiCustomItem(Self).ValidateRename(GetJNIString(Env, NewName), Boolean(RenameOnSuccess)));
end;

function EpiCustomItem_GetName(Env: PJNIEnv; This: jobject; Self: jint): jstring; cdecl;
begin
  result := NewJNIString(Env, TEpiCustomItem(Self).Name);
end;
procedure EpiCustomItem_SetName(Env: PJNIEnv; This: jobject; Self: jint; Name: jstring); cdecl;
begin
  TEpiCustomItem(Self).Name := GetJNIString(Env, Name);
end;
procedure EpiCustomItem_AddCustomData(Env: PJNIEnv; This: jobject; Self: jint;
  Key: jstring; Obj: jint); cdecl;
begin
  TEpiCustomItem(Self).AddCustomData(GetJNIString(Env, Key), TObject(Obj));
end;

function EpiCustomItem_FindCustomData(Env: PJNIEnv; This: jobject; Self: jint;
  Key: jstring): jint; cdecl;
begin
  result := jint(TEpiCustomItem(Self).FindCustomData(GetJNIString(Env, Key)));
end;

function EpiCustomItem_RemoveCustomData(Env: PJNIEnv; This: jobject;
  Self: jint; Key: jstring): jint; cdecl;
begin
  result := jint(TEpiCustomItem(Self).RemoveCustomData(GetJNIString(Env, Key)));
end;

end.

