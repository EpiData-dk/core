unit EpiCustomControlItem_jni;

{$mode objfpc}{$H+}

interface

uses jni, sysutils, androidutils;

const
  EpiCustomControlItemJNI = 'Java_dk_epidata_androidclient_core_EpiCustomControlItem_';

function EpiCustomControlItem_GetLeft(Env: PJNIEnv; This: jobject; Self: jint): jint; cdecl;
procedure EpiCustomControlItem_SetLeft(Env: PJNIEnv; This: jobject; Self: jint; Left: jint); cdecl;
function EpiCustomControlItem_GetTop(Env: PJNIEnv; This: jobject; Self: jint): jint; cdecl;
procedure EpiCustomControlItem_SetTop(Env: PJNIEnv; This: jobject; Self: jint; Top: jint); cdecl;

implementation

uses
  epicustombase;

function EpiCustomControlItem_GetLeft(Env: PJNIEnv; This: jobject; Self: jint
  ): jint; cdecl;
begin
  result := TEpiCustomControlItem(Self).Left;
end;
procedure EpiCustomControlItem_SetLeft(Env: PJNIEnv; This: jobject; Self: jint;
  Left: jint); cdecl;
begin
  TEpiCustomControlItem(Self).Left := Left;
end;
function EpiCustomControlItem_GetTop(Env: PJNIEnv; This: jobject; Self: jint
  ): jint; cdecl;
begin
  result := TEpiCustomControlItem(Self).Top;
end;
procedure EpiCustomControlItem_SetTop(Env: PJNIEnv; This: jobject; Self: jint;
  Top: jint); cdecl;
begin
  TEpiCustomControlItem(Self).Top := Top;
end;
end.

