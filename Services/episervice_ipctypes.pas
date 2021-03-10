unit episervice_ipctypes;

{$mode objfpc}{$H+}

interface

uses
  simpleipc;

const
  epiIPCprefix = 'epidataipc_';
  epiIPCNames: array[0..1] of string = ('epidatamanager', 'epidataentryclient');

  // Request types
  // EpiData Inter-process messages: TMsgType = LongInt

  // - Dispatch msg. to all running programs if the file being opened is already in use.
  //   Msg is that full path of filename.
  epiIPC_Req_IsFileOpen = 0;
  // - last request msg. (should not be used/sent)
  epiIPC_Req_last       = epiIPC_Req_IsFileOpen;

  // Acknowledgements
  // - File(Not/Is)Open is valid reply for:
  //     IsFileOpen = The requested file is (NOT) open.
  epiIPC_Ack_FileNotOpen = epiIPC_Req_last + 1;
  epiIPC_Ack_FileIsOpen  = epiIPC_Ack_FileNotOpen + 1;

type
  TEpiIPCRequest = procedure(Const MsgType: TMessageType; Const Msg: string; out Ack: TMessageType) of object;

implementation

end.

