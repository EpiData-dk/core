unit epixmlutils;

{$codepage UTF8}
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

function Get4ByteSalt: Integer;
begin
  result := Random(maxLongint - 1) + 1;
end;

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

  // Extract the text content from this node (but not subnodes).
  Node := Root.FirstChild;
  while Assigned(Node) do
  begin
    if Node.NodeType = TEXT_NODE then
      break;
    node := Node.NextSibling;
  end;

  AESCrypter.Reset;
  s := AESCrypter.DecryptString(Trim(TDOMText(Node).Data));
  St := TStringStream.Create(s);
  // Shift 4 bytes to get rid of scrambling salt...
  St.Position := 4;

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
  Delete(Result, 1, 4);
end;

function EnScramble(St: TStream): string;
var
  TmpSt: TStringStream;
  Salt: Integer;
  SaltStr: Array[0..3] of Char absolute Salt;
begin
  Result := '';
  if not Initialized then exit;

  Salt := Get4ByteSalt;

  St.Position := 0;
  TmpSt := TStringStream.Create(String(SaltStr));
  TmpSt.Position := TmpSt.Size;
  TmpSt.CopyFrom(St, St.Size);
  TmpSt.Position := 0;

  AESCrypter.Reset;
  Result := AESCrypter.EncryptString(TStringStream(TmpSt).DataString);
  TmpSt.Free;
end;

function EnScramble(S: string): string;
var
  Salt: Integer;
  SaltStr: Array[0..3] of Char absolute Salt;
begin
  Result := '';
  if not Initialized then exit;

  Salt := Get4ByteSalt;

  AESCrypter.Reset;
  Result := AESCrypter.EncryptString(String(SaltStr) + S);
end;

initialization

begin
  Randomize;
end;

Finalization

begin
  if Assigned(AESCrypter) then
    AESCrypter.Free;
end;

end.

