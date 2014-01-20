unit EpiCustomItem_jni;

{$mode objfpc}{$H+}

interface

uses jni, sysutils, androidutils;

const
  EpiCustomItemJNI = 'Java_dk_epidata_androidclient_core_EpiCustomItem_'

procedure EpiCustomItem_Destroy(Env: PJNIEnv; This: jobject; Self: jint); cdecl;
procedure EpiCustomItem_LoadFromXml(Env: PJNIEnv; This: jobject; Self: jint; Root: TDOMNode); cdecl;
function EpiCustomItem_ValidateRename(Env: PJNIEnv; This: jobject; Self: jint; NewName: jstring; RenameOnSuccess: Boolean): Boolean; cdecl;
function EpiCustomItem_GetName(Env: PJNIEnv; This: jobject; Self: jint): jstring; cdecl;
procedure EpiCustomItem_SetName(Env: PJNIEnv; This: jobject; Self: jint; Name: jstring); cdecl;
procedure EpiCustomItem_AddCustomData(Env: PJNIEnv; This: jobject; Self: jint; Key: jstring; Obj: TObject); cdecl;
function EpiCustomItem_FindCustomData(Env: PJNIEnv; This: jobject; Self: jint; Key: jstring): TObject; cdecl;
function EpiCustomItem_RemoveCustomData(Env: PJNIEnv; This: jobject; Self: jint; Key: jstring): TObject; cdecl;

implementation

uses ;

procedure EpiCustomItem_Destroy(Env: PJNIEnv; This: jobject; Self: jint); cdecl;
begin
  TEpiCustomItem(Self).Destroy();
end;

procedure EpiCustomItem_LoadFromXml(Env: PJNIEnv; This: jobject; Self: jint; Root: TDOMNode); cdecl;
begin
  TEpiCustomItem(Self).LoadFromXml(Root);
end;

function EpiCustomItem_ValidateRename(Env: PJNIEnv; This: jobject; Self: jint; NewName: jstring; RenameOnSuccess: Boolean): Boolean; cdecl;
begin
  result := TEpiCustomItem(Self).ValidateRename(GetJNIString(NewName),RenameOnSuccess);
end;

function EpiCustomItem_GetName(Env: PJNIEnv; This: jobject; Self: jint): jstring; cdecl;
begin
  result := NewJNIString(Env, TEpiCustomItem(Self).Name);
end;
procedure EpiCustomItem_SetName(Env: PJNIEnv; This: jobject; Self: jint; Name: jstring); cdecl;
begin
  TEpiCustomItem(Self).Name := GetJNIString(Env, Name);
end;
procedure EpiCustomItem_AddCustomData(Env: PJNIEnv; This: jobject; Self: jint; Key: jstring; Obj: TObject); cdecl;
begin
  TEpiCustomItem(Self).AddCustomData(GetJNIString(Key),Obj);
end;

function EpiCustomItem_FindCustomData(Env: PJNIEnv; This: jobject; Self: jint; Key: jstring): TObject; cdecl;
begin
  result := TEpiCustomItem(Self).FindCustomData(GetJNIString(Key));
end;

function EpiCustomItem_RemoveCustomData(Env: PJNIEnv; This: jobject; Self: jint; Key: jstring): TObject; cdecl;
begin
  result := TEpiCustomItem(Self).RemoveCustomData(GetJNIString(Key));
end;

end.

