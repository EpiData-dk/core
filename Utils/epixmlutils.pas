unit epixmlutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM;

  procedure InitScrambler(Password: string);
  function  DeScramble(Root: TDOMNode): TDOMNode; overload;
  function  DeScramble(S: string): string; overload;
  function  EnScramble(St: TStream): string; overload;
  function  EnScramble(S: string): string; overload;

implementation

uses
  DCPrijndael, DCPsha256, DCPbase64, XMLRead;

var
  AESCrypter: TDCP_rijndael = nil;
  Initialized: boolean = false;

procedure InitScrambler(Password: string);
begin
  // Use SHA256, since it produces a 256bit output key which makes the DCPcrypt
  // compatible with other AES/Rijndael libraries.

  if not Assigned(AESCrypter) then
    AESCrypter := TDCP_rijndael.Create(nil);
  AESCrypter.InitStr(Password, TDCP_sha256);
  Initialized := true;
end;

function DeScramble(Root: TDOMNode): TDOMNode;
var
  St: TStringStream;
  XMLDoc: TDOMDocumentFragment;
  s: String;
  Node: TDOMNode;
begin
  Result := nil;
  if not Initialized then exit;

  Node := Root.FirstChild;
  while Assigned(Node) do
  begin
    if Node.NodeType = TEXT_NODE then
      break;
    node := Node.NextSibling;
  end;
  AESCrypter.Reset;
  s := TDOMText(Node).Data;
  s := trim(s);
//  s := UTF8Encode(s);
  s := AESCrypter.DecryptString(s);
  St := TStringStream.Create(s);
  St.Position := 0;

  XMLDoc := Root.OwnerDocument.CreateDocumentFragment;
  ReadXMLFragment(XMLDoc, St);
  ST.Free;
  Result := XMLDoc;
end;

function DeScramble(S: string): string; overload;
begin
  Result := '';
  if not Initialized then exit;

  AESCrypter.Reset;
  Result := AESCrypter.DecryptString(S);
end;

function EnScramble(St: TStream): string;
var
  TmpSt: TStringStream;
begin
  Result := '';
  if not Initialized then exit;

  St.Position := 0;
  TmpSt := TStringStream.Create('');
  TmpSt.CopyFrom(St, St.Size);
  TmpSt.Position := 0;

  AESCrypter.Reset;
  Result := AESCrypter.EncryptString(TStringStream(TmpSt).DataString);
  TmpSt.Free;
end;

function EnScramble(S: string): string;
begin
  Result := '';
  if not Initialized then exit;

  AESCrypter.Reset;
  Result := AESCrypter.EncryptString(S);
end;

Finalization

begin
  if Assigned(AESCrypter) then
    AESCrypter.Free;
end;

end.

