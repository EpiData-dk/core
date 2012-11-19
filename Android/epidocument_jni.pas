unit epidocument_jni;

{$mode objfpc}{$H+}

interface

uses
  jni, sysutils;

const
  DocumentJNI = 'Java_dk_epidata_androidclient_core_Document_';


function Document_Create(Env: PJNIEnv; This: jobject): jint; cdecl;
function Document_CreateFromFile(Env: PJNIEnv; This: jobject; FileName: jstring): jint; cdecl;
procedure Document_Destroy(Env: PJNIEnv; This: jobject; Doc: jint); cdecl;
procedure Document_SaveToFile(Env: PJNIEnv; This: jobject; Doc: jint; FileName: jstring); cdecl;

implementation

uses
  epidocument, androidutils;

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
  ALogInfo('CreateFromFile (1): Doc = ' + IntToStr(jint(doc)));
  result := jint(doc);
  ALogInfo('CreateFromFile (2)');
  Doc.LoadFromFile(string(Env^^.GetStringUTFChars(Env, FileName, IsCopy)));
//  Doc.LoadFromFile(GetJNIString(Env, FileName));
  ALogInfo('CreateFromFile (3)');
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
  FN := GetJNIString(Env, FileName);
  ALogInfo('FN = ' + FN);
  TEpiDocument(Doc).SaveToFile(FN);
  ALogInfo('Saved = ' + FN);
end;

end.

