unit EpiCustomValueLabel_jni;

{$mode objfpc}{$H+}

interface

uses jni, sysutils, androidutils;

const
  EpiCustomValueLabelJNI = 'Java_dk_epidata_androidclient_core_EpiCustomValueLabel_';

function EpiCustomValueLabel_Create(Env: PJNIEnv; This: jobject; AOwner: jint): jint; cdecl;
function EpiCustomValueLabel_XMLName(Env: PJNIEnv; This: jobject; Self: jint): jstring; cdecl;
function EpiCustomValueLabel_SaveToXml(Env: PJNIEnv; This: jobject; Self: jint; Content: jstring; Lvl: jint): jstring; cdecl;
procedure EpiCustomValueLabel_LoadFromXml(Env: PJNIEnv; This: jobject; Self: jint; Root: jint); cdecl;
procedure EpiCustomValueLabel_Assign(Env: PJNIEnv; This: jobject; Self: jint; AEpiCustomBase: jint); cdecl;
function EpiCustomValueLabel_GetOrder(Env: PJNIEnv; This: jobject; Self: jint): Integer; cdecl;
procedure EpiCustomValueLabel_SetOrder(Env: PJNIEnv; This: jobject; Self: jint; Order: Integer); cdecl;
function EpiCustomValueLabel_GetTheLabel(Env: PJNIEnv; This: jobject; Self: jint): jint; cdecl;
procedure EpiCustomValueLabel_SetTheLabel(Env: PJNIEnv; This: jobject; Self: jint; TheLabel: jint); cdecl;
function EpiCustomValueLabel_GetIsMissingValue(Env: PJNIEnv; This: jobject; Self: jint): jboolean; cdecl;
procedure EpiCustomValueLabel_SetIsMissingValue(Env: PJNIEnv; This: jobject; Self: jint; IsMissingValue: jboolean); cdecl;
function EpiCustomValueLabel_GetValueAsString(Env: PJNIEnv; This: jobject; Self: jint): jstring; cdecl;

implementation

uses
  epivaluelabels, Laz2_DOM, epicustombase;

function EpiCustomValueLabel_Create(Env: PJNIEnv; This: jobject; AOwner: jint
  ): jint; cdecl;
begin
  result := jint(TEpiCustomValueLabel.Create(TEpiCustomBase(AOwner)));
end;

function EpiCustomValueLabel_XMLName(Env: PJNIEnv; This: jobject; Self: jint): jstring; cdecl;
begin
  result := NewJNIString(Env, TEpiCustomValueLabel(Self).XMLName());
end;

function EpiCustomValueLabel_SaveToXml(Env: PJNIEnv; This: jobject; Self: jint;
  Content: jstring; Lvl: jint): jstring; cdecl;
begin
  result := NewJNIString(Env, TEpiCustomValueLabel(Self).SaveToXml(GetJNIString(Env, Content),Lvl));
end;

procedure EpiCustomValueLabel_LoadFromXml(Env: PJNIEnv; This: jobject;
  Self: jint; Root: jint); cdecl;
begin
  TEpiCustomValueLabel(Self).LoadFromXml(TDOMNode(Root));
end;

procedure EpiCustomValueLabel_Assign(Env: PJNIEnv; This: jobject; Self: jint;
  AEpiCustomBase: jint); cdecl;
begin
  TEpiCustomValueLabel(Self).Assign(TEpiCustomBase(AEpiCustomBase));
end;

function EpiCustomValueLabel_GetOrder(Env: PJNIEnv; This: jobject; Self: jint): Integer; cdecl;
begin
  result := TEpiCustomValueLabel(Self).Order;
end;
procedure EpiCustomValueLabel_SetOrder(Env: PJNIEnv; This: jobject; Self: jint; Order: Integer); cdecl;
begin
  TEpiCustomValueLabel(Self).Order := Order;
end;
function EpiCustomValueLabel_GetTheLabel(Env: PJNIEnv; This: jobject; Self: jint
  ): jint; cdecl;
begin
  result := jint(TEpiCustomValueLabel(Self).TheLabel);
end;
procedure EpiCustomValueLabel_SetTheLabel(Env: PJNIEnv; This: jobject;
  Self: jint; TheLabel: jint); cdecl;
begin
  TEpiCustomValueLabel(Self).TheLabel := TEpiTranslatedText(TheLabel);
end;
function EpiCustomValueLabel_GetIsMissingValue(Env: PJNIEnv; This: jobject;
  Self: jint): jboolean; cdecl;
begin
  result := jboolean(TEpiCustomValueLabel(Self).IsMissingValue);
end;
procedure EpiCustomValueLabel_SetIsMissingValue(Env: PJNIEnv; This: jobject;
  Self: jint; IsMissingValue: jboolean); cdecl;
begin
  TEpiCustomValueLabel(Self).IsMissingValue := Boolean(IsMissingValue);
end;
function EpiCustomValueLabel_GetValueAsString(Env: PJNIEnv; This: jobject; Self: jint): jstring; cdecl;
begin
  result := NewJNIString(Env, TEpiCustomValueLabel(Self).ValueAsString);
end;
end.

