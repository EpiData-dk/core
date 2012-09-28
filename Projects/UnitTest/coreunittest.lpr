program coreunittest;

{$mode objfpc}{$H+}

{.$DEFINE TEST_GUI}

uses
  {$IFDEF UNIX}
    {$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}
  cwstring, clocale,
  {$ENDIF}
  Classes,
  { you can add units after this }
  {$IFDEF TEST_GUI}
  GUITestRunner,
  {$ELSE}
  TextTestRunner,
  {$ENDIF}
  ut_epidocument, ut_epiprojectsettings, ut_epixmlsettings,
  ut_epiadmin,
  ut_epistudy, ut_epivaluelabels;

begin
  RunRegisteredTests;
end.

