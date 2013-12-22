unit epidatafiles_jni;

{$mode objfpc}{$H+}

interface

uses
  jni, sysutils;

const
  EpiDataFilesJNI = 'Java_dk_epidata_androidclient_core_DataFiles_';

function DataFiles_GetDataFile(Env: PJNIEnv; This: jobject; DataFiles: jint; Index: jint): jint; cdecl;
function DataFiles_NewDataFile(Env: PJNIEnv; This: jobject; DataFiles: jint): jint; cdecl;

implementation

uses
  epidatafiles;

function DataFiles_GetDataFile(Env: PJNIEnv; This: jobject; DataFiles: jint;
  Index: jint): jint; cdecl;
begin
  result := Jint(TEpiDataFiles(DataFiles).DataFile[Index]);
end;

function DataFiles_NewDataFile(Env: PJNIEnv; This: jobject; DataFiles: jint
  ): jint; cdecl;
begin
  result := Jint(TEpiDataFiles(DataFiles).NewDataFile);
end;

end.

