unit epidocument_jni;

{$mode objfpc}{$H+}

interface

uses
  epidocument, jni, sysutils;

function Document_Create(Env: PJNIEnv; This: jobject): jint; cdecl;
function Document_CreateFromFile(Env: PJNIEnv; This: jobject; FileName: jstring): jint; cdecl;
procedure Document_Destroy(Env: PJNIEnv; This: jobject; Doc: jint); cdecl;
procedure Document_SaveToFile(Env: PJNIEnv; This: jobject; Doc: jint; FileName: jstring); cdecl;

implementation

uses
  androidutils;

function Document_Create(Env: PJNIEnv; This: jobject): jint; cdecl;
var
  Doc: TEpiDocument;
begin
  Doc := TEpiDocument.Create('en');
  ALogInfo('Doc = ' + IntToStr(jint(doc)));
  result := jint(doc);
end;

function Document_CreateFromFile(Env: PJNIEnv; This: jobject; FileName: jstring
  ): jint; cdecl;
var
  Doc: TEpiDocument;
  FN: String;
  IsCopy: jboolean;
begin
  Doc := TEpiDocument.Create('en');
  result := jint(doc);

  FN := String(Env^^.GetStringUTFChars(Env, FileName, IsCopy));
  Doc.LoadFromFile(FN);
end;

procedure Document_Destroy(Env: PJNIEnv; This: jobject; Doc: jint); cdecl;
begin
  TEpiDocument(Doc).Free;
end;

procedure Document_SaveToFile(Env: PJNIEnv; This: jobject; Doc: jint;
  FileName: jstring); cdecl;
var
  FN: String;
  IsCopy: jboolean;
begin
  ALogInfo('SaveToFile (doc = ' + IntToStr(Doc) + ')');
  FN := String(Env^^.GetStringUTFChars(Env, FileName, IsCopy));
  ALogInfo('FN = ' + FN);
  TEpiDocument(Doc).SaveToFile(FN);
  ALogInfo('Saved = ' + FN);
end;

end.

