unit EpiCustomList_jni;

{$mode objfpc}{$H+}

interface

uses jni, sysutils, androidutils;

const
  EpiCustomListJNI = 'Java_dk_epidata_androidclient_core_EpiCustomList_';

procedure EpiCustomList_Destroy(Env: PJNIEnv; This: jobject; Self: jint); cdecl;
function EpiCustomList_SaveToXml(Env: PJNIEnv; This: jobject; Self: jint; Content: jstring; Lvl: jint): jstring; cdecl;
procedure EpiCustomList_Clear(Env: PJNIEnv; This: jobject; Self: jint); cdecl;
procedure EpiCustomList_ClearAndFree(Env: PJNIEnv; This: jobject; Self: jint); cdecl;
function EpiCustomList_ItemClass(Env: PJNIEnv; This: jobject; Self: jint): jint; cdecl;
function EpiCustomList_NewItem(Env: PJNIEnv; This: jobject; Self: jint; AItemClass: jint): jint; cdecl;
procedure EpiCustomList_AddItem(Env: PJNIEnv; This: jobject; Self: jint; Item: jint); cdecl;
procedure EpiCustomList_InsertItem(Env: PJNIEnv; This: jobject; Self: jint; Index: jint; Item: jint); cdecl;
procedure EpiCustomList_RemoveItem(Env: PJNIEnv; This: jobject; Self: jint; Item: jint); cdecl;
function EpiCustomList_DeleteItem(Env: PJNIEnv; This: jobject; Self: jint; Index: jint): jint; cdecl;
function EpiCustomList_GetItemByName(Env: PJNIEnv; This: jobject; Self: jint; AName: jstring): jint; cdecl;
function EpiCustomList_ItemExistsByName(Env: PJNIEnv; This: jobject; Self: jint; AName: jstring): jboolean; cdecl;
function EpiCustomList_IndexOf(Env: PJNIEnv; This: jobject; Self: jint; Item: jint): jint; cdecl;
function EpiCustomList_GetCount(Env: PJNIEnv; This: jobject; Self: jint): jint; cdecl;
function EpiCustomList_GetItems(Env: PJNIEnv; This: jobject; Self: jint; Index: jint): jint; cdecl;
procedure EpiCustomList_SetItems(Env: PJNIEnv; This: jobject; Self: jint; Index: jint; Items: jint); cdecl;
function EpiCustomList_GetItemOwner(Env: PJNIEnv; This: jobject; Self: jint): jboolean; cdecl;
procedure EpiCustomList_SetItemOwner(Env: PJNIEnv; This: jobject; Self: jint; ItemOwner: jboolean); cdecl;
function EpiCustomList_GetUniqueItemName(Env: PJNIEnv; This: jobject; Self: jint; AClass: jint): jstring; cdecl;
function EpiCustomList_ValidateRename(Env: PJNIEnv; This: jobject; Self: jint; NewName: jstring; RenameOnSuccess: jboolean): jboolean; cdecl;
function EpiCustomList_GetOnValidateRename(Env: PJNIEnv; This: jobject; Self: jint): jint; cdecl;
procedure EpiCustomList_SetOnValidateRename(Env: PJNIEnv; This: jobject; Self: jint; OnValidateRename: jint); cdecl;
function EpiCustomList_GetOnGetPrefix(Env: PJNIEnv; This: jobject; Self: jint): jint; cdecl;
procedure EpiCustomList_SetOnGetPrefix(Env: PJNIEnv; This: jobject; Self: jint; OnGetPrefix: jint); cdecl;
function EpiCustomList_GetOnNewItemClass(Env: PJNIEnv; This: jobject; Self: jint): jint; cdecl;
procedure EpiCustomList_SetOnNewItemClass(Env: PJNIEnv; This: jobject; Self: jint; OnNewItemClass: jint); cdecl;
procedure EpiCustomList_BeginUpdate(Env: PJNIEnv; This: jobject; Self: jint); cdecl;
procedure EpiCustomList_EndUpdate(Env: PJNIEnv; This: jobject; Self: jint); cdecl;
procedure EpiCustomList_SetLanguage(Env: PJNIEnv; This: jobject; Self: jint; LangCode: jstring; DefaultLanguage: jboolean); cdecl;
procedure EpiCustomList_Sort(Env: PJNIEnv; This: jobject; Self: jint); cdecl;
function EpiCustomList_GetSorted(Env: PJNIEnv; This: jobject; Self: jint): jboolean; cdecl;
procedure EpiCustomList_SetSorted(Env: PJNIEnv; This: jobject; Self: jint; Sorted: jboolean); cdecl;
function EpiCustomList_GetOnSort(Env: PJNIEnv; This: jobject; Self: jint): jint; cdecl;
procedure EpiCustomList_SetOnSort(Env: PJNIEnv; This: jobject; Self: jint; OnSort: jint); cdecl;

implementation

uses
  epicustombase;

procedure EpiCustomList_Destroy(Env: PJNIEnv; This: jobject; Self: jint); cdecl;
begin
  TEpiCustomList(Self).Destroy();
end;

function EpiCustomList_SaveToXml(Env: PJNIEnv; This: jobject; Self: jint;
  Content: jstring; Lvl: jint): jstring; cdecl;
begin
  result := NewJNIString(Env, TEpiCustomList(Self).SaveToXml(GetJNIString(Env, Content), Lvl));
end;

procedure EpiCustomList_Clear(Env: PJNIEnv; This: jobject; Self: jint); cdecl;
begin
  TEpiCustomList(Self).Clear();
end;

procedure EpiCustomList_ClearAndFree(Env: PJNIEnv; This: jobject; Self: jint); cdecl;
begin
  TEpiCustomList(Self).ClearAndFree();
end;

function EpiCustomList_ItemClass(Env: PJNIEnv; This: jobject; Self: jint
  ): jint; cdecl;
begin
//  result := TEpiCustomList(Self).ItemClass();
end;

function EpiCustomList_NewItem(Env: PJNIEnv; This: jobject; Self: jint;
  AItemClass: jint): jint; cdecl;
begin
  result := jint(TEpiCustomList(Self).NewItem(TEpiCustomItemClass(AItemClass)));
end;

procedure EpiCustomList_AddItem(Env: PJNIEnv; This: jobject; Self: jint;
  Item: jint); cdecl;
begin
  TEpiCustomList(Self).AddItem(TEpiCustomItem(Item));
end;

procedure EpiCustomList_InsertItem(Env: PJNIEnv; This: jobject; Self: jint;
  Index: jint; Item: jint); cdecl;
begin
  TEpiCustomList(Self).InsertItem(Index, TEpiCustomItem(Item));
end;

procedure EpiCustomList_RemoveItem(Env: PJNIEnv; This: jobject; Self: jint;
  Item: jint); cdecl;
begin
  TEpiCustomList(Self).RemoveItem(TEpiCustomItem(Item));
end;

function EpiCustomList_DeleteItem(Env: PJNIEnv; This: jobject; Self: jint;
  Index: jint): jint; cdecl;
begin
  result := jint( TEpiCustomList(Self).DeleteItem(Index));
end;

function EpiCustomList_GetItemByName(Env: PJNIEnv; This: jobject; Self: jint;
  AName: jstring): jint; cdecl;
begin
  result := jint(TEpiCustomList(Self).GetItemByName(GetJNIString(Env, AName)));
end;

function EpiCustomList_ItemExistsByName(Env: PJNIEnv; This: jobject;
  Self: jint; AName: jstring): jboolean; cdecl;
begin
  result := jboolean( TEpiCustomList(Self).ItemExistsByName(GetJNIString(Env, AName)));
end;

function EpiCustomList_IndexOf(Env: PJNIEnv; This: jobject; Self: jint;
  Item: jint): jint; cdecl;
begin
  result := jint(TEpiCustomList(Self).IndexOf(TEpiCustomItem(Item)));
end;

function EpiCustomList_GetCount(Env: PJNIEnv; This: jobject; Self: jint): jint;
  cdecl;
begin
  result := TEpiCustomList(Self).Count;
end;
function EpiCustomList_GetItems(Env: PJNIEnv; This: jobject; Self: jint;
  Index: jint): jint; cdecl;
begin
  result := jint(TEpiCustomList(Self).Items[Index]);
end;
procedure EpiCustomList_SetItems(Env: PJNIEnv; This: jobject; Self: jint;
  Index: jint; Items: jint); cdecl;
begin
  TEpiCustomList(Self).Items[Index] := TEpiCustomItem(Items);
end;
function EpiCustomList_GetItemOwner(Env: PJNIEnv; This: jobject; Self: jint
  ): jboolean; cdecl;
begin
  result := jboolean( TEpiCustomList(Self).ItemOwner);
end;
procedure EpiCustomList_SetItemOwner(Env: PJNIEnv; This: jobject; Self: jint;
  ItemOwner: jboolean); cdecl;
begin
  TEpiCustomList(Self).ItemOwner := Boolean( ItemOwner);
end;
function EpiCustomList_GetUniqueItemName(Env: PJNIEnv; This: jobject;
  Self: jint; AClass: jint): jstring; cdecl;
begin
  result := NewJNIString(Env, TEpiCustomList(Self).GetUniqueItemName(TEpiCustomItemClass(AClass)));
end;

function EpiCustomList_ValidateRename(Env: PJNIEnv; This: jobject; Self: jint;
  NewName: jstring; RenameOnSuccess: jboolean): jboolean; cdecl;
begin
  result := jboolean( TEpiCustomList(Self).ValidateRename(GetJNIString(Env, NewName), Boolean(RenameOnSuccess)));
end;

function EpiCustomList_GetOnValidateRename(Env: PJNIEnv; This: jobject;
  Self: jint): jint; cdecl;
begin
//  result :=  TEpiCustomList(Self).OnValidateRename;
end;
procedure EpiCustomList_SetOnValidateRename(Env: PJNIEnv; This: jobject;
  Self: jint; OnValidateRename: jint); cdecl;
begin
//  TEpiCustomList(Self).OnValidateRename := OnValidateRename;
end;
function EpiCustomList_GetOnGetPrefix(Env: PJNIEnv; This: jobject; Self: jint
  ): jint; cdecl;
begin
//  result := TEpiCustomList(Self).OnGetPrefix;
end;
procedure EpiCustomList_SetOnGetPrefix(Env: PJNIEnv; This: jobject; Self: jint;
  OnGetPrefix: jint); cdecl;
begin
//  TEpiCustomList(Self).OnGetPrefix := OnGetPrefix;
end;
function EpiCustomList_GetOnNewItemClass(Env: PJNIEnv; This: jobject; Self: jint
  ): jint; cdecl;
begin
//  result := TEpiCustomList(Self).OnNewItemClass;
end;
procedure EpiCustomList_SetOnNewItemClass(Env: PJNIEnv; This: jobject;
  Self: jint; OnNewItemClass: jint); cdecl;
begin
//  TEpiCustomList(Self).OnNewItemClass := OnNewItemClass;
end;
procedure EpiCustomList_BeginUpdate(Env: PJNIEnv; This: jobject; Self: jint); cdecl;
begin
  TEpiCustomList(Self).BeginUpdate();
end;

procedure EpiCustomList_EndUpdate(Env: PJNIEnv; This: jobject; Self: jint); cdecl;
begin
  TEpiCustomList(Self).EndUpdate();
end;

procedure EpiCustomList_SetLanguage(Env: PJNIEnv; This: jobject; Self: jint;
  LangCode: jstring; DefaultLanguage: jboolean); cdecl;
begin
  TEpiCustomList(Self).SetLanguage(GetJNIString(Env, LangCode), Boolean(DefaultLanguage));
end;

procedure EpiCustomList_Sort(Env: PJNIEnv; This: jobject; Self: jint); cdecl;
begin
  TEpiCustomList(Self).Sort();
end;

function EpiCustomList_GetSorted(Env: PJNIEnv; This: jobject; Self: jint
  ): jboolean; cdecl;
begin
  result := jboolean( TEpiCustomList(Self).Sorted);
end;
procedure EpiCustomList_SetSorted(Env: PJNIEnv; This: jobject; Self: jint;
  Sorted: jboolean); cdecl;
begin
  TEpiCustomList(Self).Sorted := Boolean( Sorted);
end;
function EpiCustomList_GetOnSort(Env: PJNIEnv; This: jobject; Self: jint
  ): jint; cdecl;
begin
//  result := TEpiCustomList(Self).OnSort;
end;
procedure EpiCustomList_SetOnSort(Env: PJNIEnv; This: jobject; Self: jint;
  OnSort: jint); cdecl;
begin
 // TEpiCustomList(Self).OnSort := OnSort;
end;
end.

