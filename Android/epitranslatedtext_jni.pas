unit EpiTranslatedText_jni;

{$mode objfpc}{$H+}

interface

uses jni, sysutils, androidutils;

const
  EpiTranslatedTextJNI = 'Java_dk_epidata_androidclient_core_EpiTranslatedText_';

function EpiTranslatedText_Create(Env: PJNIEnv; This: jobject; AOwner: jint; aXMLName: jstring): jint; cdecl;
procedure EpiTranslatedText_Destroy(Env: PJNIEnv; This: jobject; Self: jint); cdecl;
function EpiTranslatedText_SaveToXml(Env: PJNIEnv; This: jobject; Self: jint; Content: jstring; Lvl: Integer): jstring; cdecl;
procedure EpiTranslatedText_LoadFromXml(Env: PJNIEnv; This: jobject; Self: jint; Root: jint); cdecl;
procedure EpiTranslatedText_Assign(Env: PJNIEnv; This: jobject; Self: jint; AEpiCustomBase: jint); cdecl;
function EpiTranslatedText_GetText(Env: PJNIEnv; This: jobject; Self: jint): jstring; cdecl;
procedure EpiTranslatedText_SetText(Env: PJNIEnv; This: jobject; Self: jint; Text: jstring); cdecl;
function EpiTranslatedText_GetTextLang(Env: PJNIEnv; This: jobject; Self: jint; LangCode: jstring): jstring; cdecl;
procedure EpiTranslatedText_SetTextLang(Env: PJNIEnv; This: jobject; Self: jint; LangCode: jstring; TextLang: jstring); cdecl;

implementation

uses
  epicustombase, Laz2_DOM;

function EpiTranslatedText_Create(Env: PJNIEnv; This: jobject; AOwner: jint; aXMLName: jstring): jint; cdecl;
begin
  result := jint(TEpiTranslatedText.Create(TEpiCustomBase(AOwner), GetJNIString(Env, aXMLName)));
end;

procedure EpiTranslatedText_Destroy(Env: PJNIEnv; This: jobject; Self: jint); cdecl;
begin
  TEpiTranslatedText(Self).Destroy();
end;

function EpiTranslatedText_SaveToXml(Env: PJNIEnv; This: jobject; Self: jint; Content: jstring; Lvl: Integer): jstring; cdecl;
begin
  result := NewJNIString(Env, TEpiTranslatedText(Self).SaveToXml(GetJNIString(Env, Content),Lvl));
end;

procedure EpiTranslatedText_LoadFromXml(Env: PJNIEnv; This: jobject;
  Self: jint; Root: jint); cdecl;
begin
  TEpiTranslatedText(Self).LoadFromXml(TDOMNode(Root));
end;

procedure EpiTranslatedText_Assign(Env: PJNIEnv; This: jobject; Self: jint; AEpiCustomBase: jint); cdecl;
begin
  TEpiTranslatedText(Self).Assign(TEpiTranslatedText(AEpiCustomBase));
end;

function EpiTranslatedText_GetText(Env: PJNIEnv; This: jobject; Self: jint): jstring; cdecl;
begin
  result := NewJNIString(Env, TEpiTranslatedText(Self).Text);
end;
procedure EpiTranslatedText_SetText(Env: PJNIEnv; This: jobject; Self: jint; Text: jstring); cdecl;
begin
  TEpiTranslatedText(Self).Text := GetJNIString(Env, Text);
end;
function EpiTranslatedText_GetTextLang(Env: PJNIEnv; This: jobject; Self: jint; LangCode: jstring): jstring; cdecl;
begin
  result := NewJNIString(Env, TEpiTranslatedText(Self).TextLang[GetJNIString(Env, LangCode)]);
end;
procedure EpiTranslatedText_SetTextLang(Env: PJNIEnv; This: jobject; Self: jint; LangCode: jstring; TextLang: jstring); cdecl;
begin
  TEpiTranslatedText(Self).TextLang[GetJNIString(Env, LangCode)] := GetJNIString(Env, TextLang);
end;
end.

