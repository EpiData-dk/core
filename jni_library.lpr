library jni_library;

{$mode objfpc}{$H+}

uses
  cmem, jni,

  // EpiData units
  epicustomlist_jni, epidocument_jni, customitem_jni, epidatafiles_jni;

{$ifdef ver2_5}
// This code is unnecessary in FPC 2.6+,
// it is required with the 2.5.1 snapshot.
procedure PASCALMAIN; external name 'PASCALMAIN';
procedure FPC_SHARED_LIB_START; [public, alias: 'FPC_SHARED_LIB_START'];
begin
   PASCALMAIN;
end;
{$endif}

function JNI_OnLoad(vm:PJavaVM; reserved: pointer): jint; cdecl;
begin
  result := JNI_VERSION_1_6;
end;

exports
  // Default!
  JNI_OnLoad,

  // CustomBase:
  // - CUstomList
  CustomList_AddItem         name CustomListJNI + 'AddItem',
  CustomList_InsertItem      name CustomListJNI + 'InsertItem',

  // - CustomItem
  CustomItem_SetName         name CustomItemJNI + 'SetName',
  CustomItem_GetName         name CustomItemJNI + 'GetName',
  CustomItem_ValidateRename  name CustomItemJNI + 'ValidateRename',

  // Document:
  Document_Create            name DocumentJNI + 'Create',
  Document_CreateFromFile    name DocumentJNI + 'CreateFromFile',
  Document_SaveToFile        name DocumentJNI + 'SaveToFile',
  Document_Destroy           name DocumentJNI + 'Destroy',

  // DataFiles:
  // - Datafiles
  DataFiles_GetDataFile      name EpiDataFilesJNI + 'GetDataFile',
  DataFiles_NewDataFile      name EpiDataFilesJNI + 'NewDataFile';
end.

