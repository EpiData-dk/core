library jni_library;

{$mode objfpc}{$H+}

uses
  cmem, jni,
  // EpiData units
  androidutils, epidocument_jni;

{$ifdef ver2_5}
// This code is unnecessary in FPC 2.6+,
// it is required with the 2.5.1 snapshot.
procedure PASCALMAIN; external name 'PASCALMAIN';
procedure FPC_SHARED_LIB_START; [public, alias: 'FPC_SHARED_LIB_START'];
begin
   PASCALMAIN;
end;
{$endif}

function JNI_OnLoad(vm:PJavaVM; reserved:pointer):jint; cdecl;
begin
  result := JNI_VERSION_1_6;
end;

exports
  Document_Create name 'Java_dk_epidata_core_jni_Document_Create',
  Document_CreateFromFile name 'Java_dk_epidata_core_jni_Document_CreateFromFile',
  Document_SaveToFile name 'Java_dk_epidata_core_jni_Document_SaveToFile',
  Document_Destroy name 'Java_dk_epidata_core_jni_Docuement_Destroy',
  JNI_OnLoad;

end.

