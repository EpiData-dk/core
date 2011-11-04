unit episervice_ipc; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpTimer, simpleipc, episervice_ipctypes;

type

  { TEpiIPC }

  TEpiIPC = class(TComponent)
  private
    Timer:      TFPTimer;
    IPCServer:  TSimpleIPCServer;
    IPCClient:  TSimpleIPCClient;
    procedure   TimerEvent(Sender: TObject);
    procedure   ServerEvent(Sender: TObject);
    procedure   SendMsg(Const MsgType: TMessageType; Const MsgText: string);
  public
    constructor Create(Const AppName: string; AOwner: TComponent);
    procedure   SendFileOpenMsg(Const FileName: string);
  end;

implementation

{ TEpiIPC }

procedure TEpiIPC.TimerEvent(Sender: TObject);
begin
   // TODO: Create list of sent events and loop through msg. with a timestamp larger than thresshold.
  IPCServer.PeekMessage(-1, true);
end;

procedure TEpiIPC.ServerEvent(Sender: TObject);
begin
  with IPCServer do
  case MsgType of
    epiIPC_Ack_Ok:
      // Do nothign since our (un)register request was completed. No further actions required.
      ;
    epiIPC_Ack_Fail:
      begin
        // MsgData contains out failed request.
        Case MsgData.ReadDWord of
          epiIPC_Req_Register:
            // TODO : Registration failed!
            ;
          epiIPC_Req_Unregister:
            // TODO : UnRegistration failed!
            ;
        end;
      end;
    epiIPC_Ack_FileNotOpen:
      // TODO : File not open reply
      ;
    epiIPC_Ack_FileIsOpen:
      // TODO : File is open reply
      ;
  end;
end;

procedure TEpiIPC.SendMsg(const MsgType: TMessageType; const MsgText: string);
begin
  // TODO: Create list of sent events and loop through msg. with a timestamp larger than thresshold.
  IPCClient.SendStringMessage(MsgType, MsgText);
end;

constructor TEpiIPC.Create(const AppName: string; AOwner: TComponent);
begin
  IPCServer := TSimpleIPCServer.Create(self);
  IPCServer.ServerID := AppName;
  IPCServer.Global := true;
  IPCServer.OnMessage := @ServerEvent;
  IPCServer.Active := true;

  Timer := TFPTimer.Create(self);
  Timer.Interval := 100;
  Timer.OnTimer := @TimerEvent;
  Timer.Enabled := true;

  IPCClient := TSimpleIPCClient.Create(self);
  IPCClient.ServerID := epiIPCCentralName;
  if not IPCClient.ServerRunning then
  begin
    // Central server not running - start it.
  end;
  IPCClient.Active := true;
  IPCClient.SendStringMessage(epiIPC_Req_Register, AppName);
end;

procedure TEpiIPC.SendFileOpenMsg(const FileName: string);
begin
  SendMsg(epiIPC_Req_IsFileOpen, FileName);
end;

end.

