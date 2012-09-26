program coreunittest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, epidatacore,
  { you can add units after this }
  GUITestRunner, TestFramework, TestFrameworkIfaces, ut_epicustombase;

var
  T: ITestProject;
begin
  coretestsuite.registerTest;

  RunRegisteredTests;
end.

