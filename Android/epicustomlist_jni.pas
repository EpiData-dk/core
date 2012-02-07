unit epicustomlist_jni;

{$mode objfpc}{$H+}

interface

uses
  epicustombase, jni, sysutils;

const
  CustomListJNI = 'Java_dk_epidata_androidclient_core_CustomList_';

procedure CustomList_AddItem(Env: PJNIEnv; This: jobject; List: jint; Item: jint); cdecl;
procedure CustomList_InsertItem(Env: PJNIEnv; This: jobject; List: jint; Index: jint; Item: jint); cdecl;

implementation

procedure CustomList_AddItem(Env: PJNIEnv; This: jobject; List: jint; Item: jint
  ); cdecl;
begin
  TEpiCustomList(List).AddItem(TEpiCustomItem(Item));
end;

procedure CustomList_InsertItem(Env: PJNIEnv; This: jobject; List: jint;
  Index: jint; Item: jint); cdecl;
begin
  TEpiCustomList(List).InsertItem(Index, TEpiCustomItem(Item));
end;

end.

