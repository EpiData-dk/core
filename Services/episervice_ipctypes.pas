unit episervice_ipctypes;

{$mode objfpc}{$H+}

interface

uses
  simpleipc;

const
  EpiSimpleIPCServerPrefix = 'epiipc_';
  EpiSimpleIPCCentralName = EpiSimpleIPCServerPrefix + 'central';

  EpiIPCMsgConnect = 0;               // MsgData =  <programname>
  EpiIPCMsgDisconnect = 1;            // MsgData =  <programname>

implementation

end.

