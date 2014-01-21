unit EpiDocument_jni;

{$mode objfpc}{$H+}

interface

uses jni, sysutils, androidutils;

const
  EpiDocumentJNI = 'Java_dk_epidata_androidclient_core_EpiDocument_';

function EpiDocument_Create(Env: PJNIEnv; This: jobject; LangCode: jstring): jint; cdecl;
procedure EpiDocument_Destroy(Env: PJNIEnv; This: jobject; Self: jint); cdecl;
function EpiDocument_XMLName(Env: PJNIEnv; This: jobject; Self: jint): jstring; cdecl;
procedure EpiDocument_LoadFromFile(Env: PJNIEnv; This: jobject; Self: jint; AFileName: jstring); cdecl;
procedure EpiDocument_LoadFromStream(Env: PJNIEnv; This: jobject; Self: jint; St: jint); cdecl;
procedure EpiDocument_LoadFromXml(Env: PJNIEnv; This: jobject; Self: jint; Root: jint); cdecl;
function EpiDocument_SaveToXml(Env: PJNIEnv; This: jobject; Self: jint; Lvl: jint; IncludeHeader: jboolean): jstring; cdecl;
procedure EpiDocument_SaveToStream(Env: PJNIEnv; This: jobject; Self: jint; St: jint); cdecl;
procedure EpiDocument_SaveToFile(Env: PJNIEnv; This: jobject; Self: jint; AFileName: jstring); cdecl;
function EpiDocument_GetXMLSettings(Env: PJNIEnv; This: jobject; Self: jint): jint; cdecl;
function EpiDocument_GetProjectSettings(Env: PJNIEnv; This: jobject; Self: jint): jint; cdecl;
function EpiDocument_GetAdmin(Env: PJNIEnv; This: jobject; Self: jint): jint; cdecl;
function EpiDocument_GetStudy(Env: PJNIEnv; This: jobject; Self: jint): jint; cdecl;
function EpiDocument_GetValueLabelSets(Env: PJNIEnv; This: jobject; Self: jint): jint; cdecl;
function EpiDocument_GetDataFiles(Env: PJNIEnv; This: jobject; Self: jint): jint; cdecl;
function EpiDocument_GetRelations(Env: PJNIEnv; This: jobject; Self: jint): jint; cdecl;
function EpiDocument_GetOnPassword(Env: PJNIEnv; This: jobject; Self: jint): jint; cdecl;
procedure EpiDocument_SetOnPassword(Env: PJNIEnv; This: jobject; Self: jint; OnPassword: jint); cdecl;
function EpiDocument_GetOnProgress(Env: PJNIEnv; This: jobject; Self: jint): jint; cdecl;
procedure EpiDocument_SetOnProgress(Env: PJNIEnv; This: jobject; Self: jint; OnProgress: jint); cdecl;
function EpiDocument_GetLoading(Env: PJNIEnv; This: jobject; Self: jint): jboolean; cdecl;
function EpiDocument_GetVersion(Env: PJNIEnv; This: jobject; Self: jint): jint; cdecl;
function EpiDocument_GetPassWord(Env: PJNIEnv; This: jobject; Self: jint): jstring; cdecl;
procedure EpiDocument_SetPassWord(Env: PJNIEnv; This: jobject; Self: jint; PassWord: jstring); cdecl;
procedure EpiDocument_IncCycleNo(Env: PJNIEnv; This: jobject; Self: jint); cdecl;
function EpiDocument_GetCycleNo(Env: PJNIEnv; This: jobject; Self: jint): jlong; cdecl;

implementation

uses
  epidocument, Laz2_DOM, Classes;

function EpiDocument_Create(Env: PJNIEnv; This: jobject; LangCode: jstring): jint; cdecl;
begin
  result := jint(TEpiDocument.Create(GetJNIString(Env, LangCode)));
end;

procedure EpiDocument_Destroy(Env: PJNIEnv; This: jobject; Self: jint); cdecl;
begin
  TEpiDocument(Self).Destroy();
end;

function EpiDocument_XMLName(Env: PJNIEnv; This: jobject; Self: jint): jstring; cdecl;
begin
  result := NewJNIString(Env, TEpiDocument(Self).XMLName());
end;

procedure EpiDocument_LoadFromFile(Env: PJNIEnv; This: jobject; Self: jint; AFileName: jstring); cdecl;
begin
  TEpiDocument(Self).LoadFromFile(GetJNIString(Env, AFileName));
end;

procedure EpiDocument_LoadFromStream(Env: PJNIEnv; This: jobject; Self: jint;
  St: jint); cdecl;
begin
  TEpiDocument(Self).LoadFromStream(TStream(St));
end;

procedure EpiDocument_LoadFromXml(Env: PJNIEnv; This: jobject; Self: jint;
  Root: jint); cdecl;
begin
  TEpiDocument(Self).LoadFromXml(TDOMNode(Root));
end;

function EpiDocument_SaveToXml(Env: PJNIEnv; This: jobject; Self: jint;
  Lvl: jint; IncludeHeader: jboolean): jstring; cdecl;
begin
  result := NewJNIString(Env, TEpiDocument(Self).SaveToXml(Lvl, Boolean(IncludeHeader)));
end;

procedure EpiDocument_SaveToStream(Env: PJNIEnv; This: jobject; Self: jint;
  St: jint); cdecl;
begin
  TEpiDocument(Self).SaveToStream(TStream(St));
end;

procedure EpiDocument_SaveToFile(Env: PJNIEnv; This: jobject; Self: jint; AFileName: jstring); cdecl;
begin
  TEpiDocument(Self).SaveToFile(GetJNIString(Env, AFileName));
end;

function EpiDocument_GetXMLSettings(Env: PJNIEnv; This: jobject; Self: jint
  ): jint; cdecl;
begin
  result := jint(TEpiDocument(Self).XMLSettings);
end;
function EpiDocument_GetProjectSettings(Env: PJNIEnv; This: jobject; Self: jint
  ): jint; cdecl;
begin
  result := jint(TEpiDocument(Self).ProjectSettings);
end;
function EpiDocument_GetAdmin(Env: PJNIEnv; This: jobject; Self: jint): jint;
  cdecl;
begin
  result := jint(TEpiDocument(Self).Admin);
end;
function EpiDocument_GetStudy(Env: PJNIEnv; This: jobject; Self: jint): jint;
  cdecl;
begin
  result := jint(TEpiDocument(Self).Study);
end;
function EpiDocument_GetValueLabelSets(Env: PJNIEnv; This: jobject; Self: jint
  ): jint; cdecl;
begin
  result := jint(TEpiDocument(Self).ValueLabelSets);
end;
function EpiDocument_GetDataFiles(Env: PJNIEnv; This: jobject; Self: jint
  ): jint; cdecl;
begin
  result := jint(TEpiDocument(Self).DataFiles);
end;
function EpiDocument_GetRelations(Env: PJNIEnv; This: jobject; Self: jint
  ): jint; cdecl;
begin
  result := jint(TEpiDocument(Self).Relations);
end;
function EpiDocument_GetOnPassword(Env: PJNIEnv; This: jobject; Self: jint
  ): jint; cdecl;
begin
//  result := TEpiDocument(Self).OnPassword;
end;
procedure EpiDocument_SetOnPassword(Env: PJNIEnv; This: jobject; Self: jint;
  OnPassword: jint); cdecl;
begin
//  TEpiDocument(Self).OnPassword := OnPassword;
end;
function EpiDocument_GetOnProgress(Env: PJNIEnv; This: jobject; Self: jint
  ): jint; cdecl;
begin
//  result := TEpiDocument(Self).OnProgress;
end;
procedure EpiDocument_SetOnProgress(Env: PJNIEnv; This: jobject; Self: jint;
  OnProgress: jint); cdecl;
begin
//  TEpiDocument(Self).OnProgress := OnProgress;
end;
function EpiDocument_GetLoading(Env: PJNIEnv; This: jobject; Self: jint
  ): jboolean; cdecl;
begin
  result := jboolean(TEpiDocument(Self).Loading);
end;
function EpiDocument_GetVersion(Env: PJNIEnv; This: jobject; Self: jint): jint;
  cdecl;
begin
  result := TEpiDocument(Self).Version;
end;
function EpiDocument_GetPassWord(Env: PJNIEnv; This: jobject; Self: jint): jstring; cdecl;
begin
  result := NewJNIString(Env, TEpiDocument(Self).PassWord);
end;
procedure EpiDocument_SetPassWord(Env: PJNIEnv; This: jobject; Self: jint; PassWord: jstring); cdecl;
begin
  TEpiDocument(Self).PassWord := GetJNIString(Env, PassWord);
end;
procedure EpiDocument_IncCycleNo(Env: PJNIEnv; This: jobject; Self: jint); cdecl;
begin
  TEpiDocument(Self).IncCycleNo();
end;

function EpiDocument_GetCycleNo(Env: PJNIEnv; This: jobject; Self: jint
  ): jlong; cdecl;
begin
  result := TEpiDocument(Self).CycleNo;
end;
end.

