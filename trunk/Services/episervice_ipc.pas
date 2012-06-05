unit episervice_ipc; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpTimer, simpleipc, episervice_ipctypes;

type

  { TEpiIPC }

  TEpiIPC = class(TComponent)
  private
    FOnRequest: TEpiIPCRequest;
    Timer:      TFPTimer;
    IPCServer:  TSimpleIPCServer;
    IPCClient:  TSimpleIPCClient;
    FAppName:   string;
    FWaitForMsg: boolean;
    FFileIsOpen: boolean;
    procedure   SetOnRequest(AValue: TEpiIPCRequest);
    procedure   TimerEvent(Sender: TObject);
    procedure   ServerEvent(Sender: TObject);
    procedure   SendMsg(Const MsgType: TMessageType; Const MsgText: string);
  public
    constructor Create(Const AppName: string; AOwner: TComponent);
    destructor  Destroy; override;
    function    IsFileOpenMsg(Const FileName: string): boolean;
    property    OnRequest: TEpiIPCRequest read FOnRequest write SetOnRequest;
  end;

implementation

type
  TMsgListEntry = record
    MsgType: TMessageType;
    MsgData: string;
  end;
  PMsgListEntry = ^TMsgListEntry;

{ TEpiIPC }

procedure TEpiIPC.TimerEvent(Sender: TObject);
begin
   // TODO: Create list of sent events and loop through msg. with a timestamp larger than thresshold.
  if not Timer.Enabled then exit;
  IPCServer.PeekMessage(0, true);
end;

procedure TEpiIPC.SetOnRequest(AValue: TEpiIPCRequest);
begin
  if FOnRequest = AValue then Exit;
  FOnRequest := AValue;
end;

procedure TEpiIPC.ServerEvent(Sender: TObject);
var
  Ack: TMessageType;
begin
  // Stop timer - we do not want to read messages while processing one.
//  Timer.Enabled := false;

  FWaitForMsg := false;
  with IPCServer do
  case MsgType of
    epiIPC_Req_IsFileOpen:
      begin
        if Assigned(FOnRequest) then
          FOnRequest(MsgType, StringMessage, Ack);

        IPCClient.Connect;
        IPCClient.SendStringMessage(Ack, StringMessage);
      end;
    epiIPC_Ack_FileNotOpen:
      FFileIsOpen := false;
    epiIPC_Ack_FileIsOpen:
      FFileIsOpen := true;
  end;

  // Start timer again.
//  Timer.StartTimer;
end;

procedure TEpiIPC.SendMsg(const MsgType: TMessageType; const MsgText: string);
var
  MsgEntry: PMsgListEntry;
  i: Integer;
begin
  if IPCClient.ServerRunning then
  begin
    IPCClient.Connect;
    IPCClient.SendStringMessage(MsgType, MsgText)
  end
  else
    FWaitForMsg := false;
end;

constructor TEpiIPC.Create(const AppName: string; AOwner: TComponent);
begin
  FAppName := AppName;

  IPCServer := TSimpleIPCServer.Create(self);
  IPCServer.ServerID := epiIPCprefix + AppName;
  IPCServer.Global := true;
  IPCServer.OnMessage := @ServerEvent;
  IPCServer.Active := true;

  Timer := TFPTimer.Create(self);
  Timer.Interval := 100;
  Timer.OnTimer := @TimerEvent;
  Timer.Enabled := true;

  IPCClient := TSimpleIPCClient.Create(self);
  IPCClient.ServerID := BoolToStr(AppName = epiIPCNames[0], epiIPCprefix + epiIPCNames[1], epiIPCprefix + epiIPCNames[0]);
end;

destructor TEpiIPC.Destroy;
begin
  inherited Destroy;
end;

function TEpiIPC.IsFileOpenMsg(const FileName: string): boolean;
begin
  FFileIsOpen := false;

  FWaitForMsg := true;
  SendMsg(epiIPC_Req_IsFileOpen, FileName);
  while FWaitForMsg do IPCServer.PeekMessage(50, true);

  Result := FFileIsOpen;
end;

end.

