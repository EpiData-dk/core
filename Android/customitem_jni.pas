unit customitem_jni;

{$mode objfpc}{$H+}

interface

uses
  epicustombase, jni, sysutils;

const
  CustomItemJNI = 'Java_dk_epidata_androidclient_core_CustomItem_';

function CustomItem_ValidateRename(Env: PJNIEnv; This: jobject; Item: jint; NewName: jstring): jboolean; cdecl;
function CustomItem_GetName(Env: PJNIEnv; This: jobject; Item: jint): jstring; cdecl;
procedure CustomItem_SetName(Env: PJNIEnv; This: jobject; Item: jint; Name: jstring); cdecl;

implementation

uses
  androidutils;

function CustomItem_ValidateRename(Env: PJNIEnv; This: jobject; Item: jint;
  NewName: jstring): jboolean; cdecl;
begin
  result := jboolean(TEpiCustomItem(Item).ValidateRename(GetJNIString(Env, NewName), false));
end;

function CustomItem_GetName(Env: PJNIEnv; This: jobject; Item: jint): jstring;
  cdecl;
begin
  result := NewJNIString(Env, TEpiCustomItem(Item).Name);
end;

procedure CustomItem_SetName(Env: PJNIEnv; This: jobject; Item: jint;
  Name: jstring); cdecl;
begin
  TEpiCustomItem(Item).Name := GetJNIString(Env, Name);
end;

end.

