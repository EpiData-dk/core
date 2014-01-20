unit EpiTranslatedTextWrapper_jni;

{$mode objfpc}{$H+}

interface

uses jni, sysutils, androidutils;

const
  EpiTranslatedTextWrapperJNI = 'Java_dk_epidata_androidclient_core_EpiTranslatedTextWrapper_';

function EpiTranslatedTextWrapper_Create(Env: PJNIEnv; This: jobject; AOwner: jint; NodeName: jstring; TextName: jstring): jint; cdecl;
function EpiTranslatedTextWrapper_SaveToXml(Env: PJNIEnv; This: jobject; Self: jint; Content: jstring; Lvl: jint): jstring; cdecl;
procedure EpiTranslatedTextWrapper_LoadFromXml(Env: PJNIEnv; This: jobject; Self: jint; Root: jint); cdecl;

implementation

uses
  epicustombase, Laz2_DOM;

function EpiTranslatedTextWrapper_Create(Env: PJNIEnv; This: jobject;
  AOwner: jint; NodeName: jstring; TextName: jstring): jint; cdecl;
begin
  result := jint(TEpiTranslatedTextWrapper.Create(TEpiCustomBase(AOwner), GetJNIString(Env, NodeName), GetJNIString(Env, TextName)));
end;

function EpiTranslatedTextWrapper_SaveToXml(Env: PJNIEnv; This: jobject;
  Self: jint; Content: jstring; Lvl: jint): jstring; cdecl;
begin
  result := NewJNIString(Env, TEpiTranslatedTextWrapper(Self).SaveToXml(GetJNIString(Env, Content), Lvl));
end;

procedure EpiTranslatedTextWrapper_LoadFromXml(Env: PJNIEnv; This: jobject;
  Self: jint; Root: jint); cdecl;
begin
  TEpiTranslatedTextWrapper(Self).LoadFromXml(TDOMNode(Root));
end;

end.

