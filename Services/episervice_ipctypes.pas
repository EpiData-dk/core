unit episervice_ipctypes;

{$mode objfpc}{$H+}

interface

const
  epiIPCCentralName = 'epidatacentral';

  // Request types
  // EpiData Inter-process messages: TMsgType = LongInt

  // - Requests registration at the central server. Msg content is the name of program.
  epiIPC_Req_Register   = 0;
  // - Request unregistration at the central server. Msg content is the name of program.
  epiIPC_Req_Unregister = epiIPC_Req_Register + 1;
  // - Dispatch msg. to all running programs (through central) if the file being opened is already in use.
  //   Msg is that full path of filename.
  epiIPC_Req_IsFileOpen = epiIPC_Req_Unregister + 1;
  // - last request msg. (should not be used/sent)
  epiIPC_lastrequstmsg = epiIPC_Req_IsFileOpen;

  // Acknowledgements
  // - Ok is a valid reply for:
  //     Register, Unregister = the (un)registration was successfull. Msg content is value of request (epiIPC_Req_... )
  epiIPC_Ack_Ok          = epiIPC_lastrequstmsg + 1;
  // - Fail is a valid reply for:
  //     Register, Unregister = the (un)registration was NOT successfull. Msg content is value of request (epiIPC_Req_... )
  epiIPC_Ack_Fail        = epiIPC_Ack_Ok + 1;

  // - File(Not/Is)Open is valid reply for:
  //     IsFileOpen = The requested file is (NOT) open.
  epiIPC_Ack_FileNotOpen = epiIPC_Ack_Fail + 1;
  epiIPC_Ack_FileIsOpen  = epiIPC_Ack_FileNotOpen + 1;

implementation

end.

