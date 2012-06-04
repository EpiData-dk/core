unit episervice_ipc; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpTimer, simpleipc, episervice_ipctypes;

type

  { TEpiIPCService }

  TEpiIPCService = class
  private
    IPCServer: TSimpleIPCServer;
    IPCClient: TSimpleIPCClient;
  public
    constructor Create(Const Name: string);
    function    IsFileOpen(Const FileName: string;
      var OpenProgram: string): boolean;
    procedure   NotifyProgram(Const ProgramName: string;
      Const Notification: integer);
  end;

implementation

type

  { TIPCCentral }

  TIPCCentral = class
  private
    IPS: TSimpleIPCServer;
    IPCs: TStringList;
    procedure CentralMessage(Sender: TObject);
    procedure DoConnectClient;
    procedure DoDisconnectClient;
  protected
    constructor Create;
  public
    class function StartCentral: TIPCCentral;
  end;

{ TIPCCentral }

procedure TIPCCentral.CentralMessage(Sender: TObject);
begin
  Case IPS.MsgType of
    EpiIPCMsgConnect:
      DoConnectClient;
    EpiIPCMsgDisconnect:
      DoDisconnectClient;
  end;
end;

procedure TIPCCentral.DoConnectClient;
var
  ProgName: String;
  Client: TSimpleIPCClient;
begin
  ProgName := IPS.StringMessage;
  Client   := TSimpleIPCClient.Create(nil);

  IPCs.AddObject(ProgName, Client);
  Client.ServerID := EpiSimpleIPCServerPrefix + ProgName;
  Client.Connect;
end;

procedure TIPCCentral.DoDisconnectClient;
var
  ProgName: String;
  Idx: Integer;
  Client: TSimpleIPCClient;
begin
  ProgName := IPS.StringMessage;
  Idx      := IPCs.IndexOf(ProgName);
  Client   := TSimpleIPCClient(IPCs.Objects[Idx]);
  IPCs.Delete(Idx);

  Client.Disconnect;
  Client.Free;
end;

constructor TIPCCentral.Create;
begin
  IPCs := TStringList.Create;

  IPS := TSimpleIPCServer.Create(nil);
  with IPS do
  begin
    ServerID := EpiSimpleIPCCentralName;
    Global := true;
    OnMessage  := @CentralMessage;
    StartServer;
  end;

  // Start timer....
end;

class function TIPCCentral.StartCentral: TIPCCentral;
var
  lIPC: TSimpleIPCClient;
begin
  Result := nil;
  lIPC := TSimpleIPCClient.Create(nil);
  lIPC.ServerID := EpiSimpleIPCCentralName;

  if not lIPC.ServerRunning then
    Result := TIPCCentral.Create;

  lIPC.Free;
end;


{ TEpiIPCService }

constructor TEpiIPCService.Create(const Name: string);
begin
  TIPCCentral.StartCentral;

  IPCServer := TSimpleIPCServer.Create(nil);
  IPCServer.ServerID := EpiSimpleIPCServerPrefix + Name;
  IPCServer.Global := true;
  IPCServer.StartServer;

  IPCClient := TSimpleIPCClient.Create(nil);
  IPCClient.ServerID := EpiSimpleIPCCentralName;
  IPCClient.Connect;
  IPCClient.SendStringMessage(EpiIPCMsgConnect, Name);

  // Start timer.....
end;

function TEpiIPCService.IsFileOpen(const FileName: string;
  var OpenProgram: string): boolean;
begin

end;

procedure TEpiIPCService.NotifyProgram(const ProgramName: string;
  const Notification: integer);
begin

end;

end.

