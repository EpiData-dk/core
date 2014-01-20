unit EpiCustomControlItemList_jni;

{$mode objfpc}{$H+}

interface

uses jni, sysutils, androidutils;

const
  EpiCustomControlItemListJNI = 'Java_dk_epidata_androidclient_core_EpiCustomControlItemList_';

procedure EpiCustomControlItemList_InsertItem(Env: PJNIEnv; This: jobject; Self: jint; Index: jint; Item: jint); cdecl;
function EpiCustomControlItemList_DeleteItem(Env: PJNIEnv; This: jobject; Self: jint; Index: jint): jint; cdecl;

implementation

uses
  epicustombase;

procedure EpiCustomControlItemList_InsertItem(Env: PJNIEnv; This: jobject;
  Self: jint; Index: jint; Item: jint); cdecl;
begin
  TEpiCustomControlItemList(Self).InsertItem(Index, TEpiCustomItem(Item));
end;

function EpiCustomControlItemList_DeleteItem(Env: PJNIEnv; This: jobject;
  Self: jint; Index: jint): jint; cdecl;
begin
  result := jint(TEpiCustomControlItemList(Self).DeleteItem(Index));
end;

end.

