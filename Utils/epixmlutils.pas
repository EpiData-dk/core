unit epixmlutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM;

  procedure InitScrambler(Password: string);
  function  DeScramble(Root: TDOMNode): TDOMNode;
  function  EnScramble(St: TStream): string;
  function  EnScramble(S: string): string;

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
begin
  Result := nil;
  if not Initialized then exit;

  AESCrypter.Reset;
  St := TStringStream.Create(AESCrypter.DecryptString(Root.TextContent));
  XMLDoc := Root.OwnerDocument.CreateDocumentFragment;
  ReadXMLFragment(XMLDoc, St);
  ST.Free;
  Result := XMLDoc;
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

