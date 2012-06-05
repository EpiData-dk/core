program epicentral;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, epidatacore,
  centralserverhandler
  { you can add units after this };

{$R *.res}

var
  Server: TServerHandler;
begin
  Server := TServerHandler.Create(nil);
  while Server.Active do ;
end.

