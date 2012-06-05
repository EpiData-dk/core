unit centralserverhandler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, simpleipc, episervice_ipctypes;

type

  { TServerHandler }

  TServerHandler = class(TComponent)
  private
    ClientList: TStringList;
    Server: TSimpleIPCServer;
    function GetActive: boolean;
    procedure   ServerEvent(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    property    Active: boolean read GetActive;
  end;

implementation

{ TServerHandler }

function TServerHandler.GetActive: boolean;
begin
  result := true;
end;

procedure TServerHandler.ServerEvent(Sender: TObject);
var
  Client: TSimpleIPCClient;
  ClienName: String;
  Idx: Integer;
  Data: TStream;
begin
  with Server do
  case MsgType of
    epiIPC_Req_Register:
      begin
        if not Assigned(ClientList) then ClientList := TStringList.Create;
        ClienName := StringMessage;
        Data := TMemoryStream.Create;
        Data.WriteQWord(MsgType);
        Data.Position := 0;

        if ClientList.Find(ClienName, Idx) then
        begin
          TSimpleIPCClient(ClientList.Objects[Idx]).SendMessage(epiIPC_Ack_Fail, Data);
        end else begin
          Client := TSimpleIPCClient.Create(Self);
          Client.ServerID := StringMessage;
          Client.Active := true;
          ClientList.AddObject(Client.ServerID, Client);
          Client.SendMessage(epiIPC_Ack_Ok, Data);
        end;
      end;
    epiIPC_Req_Unregister:
      begin
        ClienName := StringMessage;
        Data := TMemoryStream.Create;
        Data.WriteQWord(MsgType);
        Data.Position := 0;

        if ClientList.Find(ClienName, Idx) then
        begin
          Client := TSimpleIPCClient(ClientList.Objects[Idx]);
          Client.SendMessage(epiIPC_Ack_Ok, Data);
          Client.Free;
          ClientList.Delete(Idx);
          if ClientList.Count = 0 then
            // TODO: Terminate := true;
            ;
        end else begin
          // Did not exists in list of clients - create a temp. client and send FAIL msg.
          Client := TSimpleIPCClient.Create(Self);
          Client.ServerID := StringMessage;
          Client.Active := true;
          Client.SendMessage(epiIPC_Ack_Fail, Data);
          Client.Free;
        end;

      end;
    epiIPC_Req_IsFileOpen:
      ;
  end;
  Data.Free;
end;

constructor TServerHandler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Server := TSimpleIPCServer.Create(self);
  Server.ServerID := epiIPCCentralName;
  Server.Global := true;
  Server.OnMessage := @ServerEvent;
  Server.Active := true;
end;

end.

