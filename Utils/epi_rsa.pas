unit epi_rsa;

{$codepage UTf8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, OpenSSL, dynlibs, ctypes;

type

  { TEpiRSA }

  TEpiRSA = class
  private
    type TRSAKeyType = (rsaPub, rsaPriv);
  private
    procedure DoError(Const Msg: UTF8String);
  private
    FPrivateRSA: PRSA;
    FPublicRSA: PRSA;
    FPrivateKey: RawByteString;
    FPublicKey: RawByteString;
    procedure SetPrivateKey(AValue: RawByteString);
    procedure SetPublicKey(AValue: RawByteString);
    function PEMToRSA(PEM: RawByteString; KeyType: TRSAKeyType): PRSA;
    function CryptMsg(Const InMsg: RawByteString; out OutMsg: RawByteString; KeyType: TRSAKeyType): boolean;
    function CryptMsg(Const Input: Pointer; InLen: Integer; out Output: Pointer; out OutLen: Integer; KeyType: TRSAKeyType): boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure GenerateKeys(const BitSize: Integer = 2048);
    function Encrypt(Const OrigMsg: RawByteString; out EncMsg: RawByteString): boolean; overload;
    function Encrypt(Const Input: Pointer; InLen: Integer; out Output: Pointer; out OutLen: Integer): boolean; overload;
    function Decrypt(const EncMsg: RawByteString; out OrigMsg: RawByteString): boolean; overload;
    function Decrypt(Const Input: Pointer; InLen: Integer; out Output: Pointer; out OutLen: Integer): boolean; overload;
    property PrivateKey: RawByteString read FPrivateKey write SetPrivateKey;
    property PublicKey: RawByteString read FPublicKey write SetPublicKey;
  end;

implementation

type
  TPEM_write_bio_RSAPrivateKey = function(Pri: PBIO; KeyPair: PRSA; var1, var2, var3, var4, var5: pointer): integer; cdecl;
  TPEM_write_bio_RSAPublicKey  = function(Pri: PBIO; KeyPair: PRSA): integer; cdecl;
  TPEM_read_bio_RSAPrivateKey  = function(keybio: PBIO; rsa: PPRSA; Pass: Pointer; CallBack: Pointer): PRSA; cdecl;
  TPEM_read_bio_RSAPublicKey   = function(keybio: PBIO; rsa: PPRSA; Pass: Pointer; CallBack: Pointer): PRSA; cdecl;

// BN methods.
  TBN_new                      = function(): PBIGNUM; cdecl;
  TBN_free                     = procedure(ABn: PBIGNUM); cdecl;
  TBN_set_word                 = function(ABn: PBIGNUM; W: culong): cint; cdecl;



var
  PEM_write_bio_RSAPrivateKey: TPEM_write_bio_RSAPrivateKey;
  PEM_write_bio_RSAPublicKey:  TPEM_write_bio_RSAPublicKey;
  PEM_read_bio_RSAPublicKey:   TPEM_read_bio_RSAPublicKey;
  PEM_read_bio_RSAPrivateKey:  TPEM_read_bio_RSAPrivateKey;

  BN_new:                      TBN_new;
  BN_free:                     TBN_free;
  BN_set_word:                 TBN_set_word;


{ TEpiRSA }

constructor TEpiRSA.Create;
begin
  if (not InitSSLInterface) then exit;
  ERR_load_crypto_strings();

  PEM_write_bio_RSAPrivateKey := TPEM_write_bio_RSAPrivateKey(GetProcAddress(SSLUtilHandle, 'PEM_write_bio_RSAPrivateKey'));
  PEM_write_bio_RSAPublicKey  := TPEM_write_bio_RSAPublicKey(GetProcAddress(SSLUtilHandle,  'PEM_write_bio_RSAPublicKey'));
  PEM_read_bio_RSAPrivateKey  := TPEM_read_bio_RSAPrivateKey(GetProcAddress(SSLUtilHandle, 'PEM_read_bio_RSAPrivateKey'));
  PEM_read_bio_RSAPublicKey   := TPEM_read_bio_RSAPublicKey(GetProcAddress(SSLUtilHandle,  'PEM_read_bio_RSAPublicKey'));

  BN_new                      := TBN_new(GetProcAddress(SSLUtilHandle, 'BN_new'));
  BN_free                     := TBN_free(GetProcAddress(SSLUtilHandle, 'BN_free'));
  BN_set_word                 := TBN_set_word(GetProcAddress(SSLUtilHandle, 'BN_set_word'));
end;

destructor TEpiRSA.Destroy;
begin
  inherited Destroy;
end;

procedure TEpiRSA.GenerateKeys(const BitSize: Integer);
var
  BigNum:  PBIGNUM;
  KeyPair: PRSA;
  PriLen, PubLen: integer;
  PriBIO, PubBIO: PBIO;
  PriKey, PubKey: String;
begin
  // 1: Generate BigNum
  BigNum  := BN_new();
  if (BN_set_word(BigNum, 65537) <> 1) then
    DoError('BN_set_word: ');

  // 2: Generate the RSA KeyPair
  KeyPair := RSA_new();
  {$IF FPC_FULLVERSION < 30200}
  if (not Assigned(RSA_generate_key_ex(KeyPair, BitSize, BigNum, nil))) then
  {$ELSE}
  if (RSA_generate_key_ex(KeyPair, BitSize, BigNum, nil) <> 0) then
  {$ENDIF}
    DoError('RSA_generate_key_ex');

  // 3: Create bI/O's to output RSA KeyPair to.
  PriBIO     := BioNew(BioSMem);
  PubBIO     := BioNew(BioSMem);

  // 4: Translate internal RSA structure to PEM lines - within the bI/O structure.
  if (PEM_write_bio_RSAPrivateKey(PriBIO, keypair, nil, nil, nil, nil, nil) <> 1) then
    DoError('PEM_write_bio_RSAPrivateKey');

  if (PEM_write_bio_RSAPublicKey(PubBIO, keypair) <> 1) then
    DoError('PEM_write_bio_RSAPublicKey');

  // 5: Get lengths of PEM's
  Prilen := BioCtrlPending(PriBIO);
  Publen := BioCtrlPending(PubBIO);
  SetLength(PriKey, PriLen);
  SetLength(PubKey, PubLen);

  // 6: Write from bI/O to strings.
  if (BioRead(PriBIO, PriKey, PriLen) <= 0) then
    DoError('BioRead: ');
  if (BioRead(PubBIO, PubKey, PubLen) <= 0) then
    DoError('BioRead: ');

  // 7: Set value in internal structure
  FPrivateRSA := KeyPair;
  FPublicRSA  := KeyPair;
  FPrivateKey := PriKey;
  FPublicKey  := PubKey;

  // 8: Free SSL handles.
  BioFreeAll(PubBIO);
  BioFreeAll(PriBIO);
  BN_free(BigNum);
end;

procedure TEpiRSA.DoError(const Msg: UTF8String);
var
  Err: PChar;
  S: UTF8String;
begin
  Err := StrAlloc(200);
  Err_Error_String(ErrGetError(), err);
  S := Msg + UTF8String(err);
  StrDispose(Err);
  raise Exception.Create(S);
end;

procedure TEpiRSA.SetPrivateKey(AValue: RawByteString);
begin
  if FPrivateKey = AValue then Exit;

  RSA_free(FPrivateRSA);
  FPrivateRSA := PEMToRSA(AValue, rsaPriv);
  if Assigned(FPrivateRSA) then
    FPrivateKey := AValue;
end;

procedure TEpiRSA.SetPublicKey(AValue: RawByteString);
begin
  if FPublicKey = AValue then Exit;

  if Assigned(FPublicRSA) then
    RSA_free(FPublicRSA);

  FPublicRSA := PEMToRSA(AValue, rsaPub);
  if Assigned(FPublicRSA) then
    FPublicKey := AValue;
end;

function TEpiRSA.PEMToRSA(PEM: RawByteString; KeyType: TRSAKeyType): PRSA;
var
  KeyBIO: PBIO;
  TmpRsa: PRSA;
begin
  KeyBIO := nil;
  Result := nil;
  TmpRsa := nil;

  try
    KeyBIO := BIO_new_mem_buf(@Pem[1], -1);
    if KeyBIO = nil then
      DoError('Failed to create key PBIO: ');

    case KeyType of
      rsaPub:  Result := PEM_read_bio_RSAPublicKey(KeyBIO,  @TmpRsa, nil, nil);
      rsaPriv: Result := PEM_read_bio_RSAPrivateKey(KeyBIO, @TmpRsa, nil, nil);
    end;

    if Result = nil then
      DoError('Failed to create PRSA: ');

  finally
    BioFreeAll(KeyBIO);
  end;
end;

function TEpiRSA.CryptMsg(const InMsg: RawByteString; out
  OutMsg: RawByteString; KeyType: TRSAKeyType): boolean;
var
  RsaFunc: function (flen: cint; from_buf, to_buf: PByte; arsa: PRSA; padding: cint): cint;
  Key: PRSA;
  InLen, OLen: Integer;
  OMsg: PByte;
  S: UTF8String;
begin
  case KeyType of
    rsaPub:
      begin
        Key     := FPublicRSA;
        RsaFunc := @RSA_public_encrypt;
        S       := 'encrypt: ';
      end;
    rsaPriv:
      begin
        Key     := FPrivateRSA;
        RsaFunc := @RSA_private_decrypt;
        S       := 'decrypt: ';
      end;
  end;

  InLen := Length(InMsg);
  OMsg  := GetMem(RSA_size(Key));
  OLen  := RsaFunc(InLen, @InMsg[1], OMsg, Key, RSA_PKCS1_OAEP_PADDING);

  Result := (OLen >= 0);

  if (not Result) then
  begin
    DoError('Failed to ' + S);
  end else begin
    SetLength(OutMsg, OLen);
    StrMove(@OutMsg[1], PChar(OMSg), OLen);
  end;
  Freemem(OMsg);
end;

function TEpiRSA.CryptMsg(const Input: Pointer; InLen: Integer; out
  Output: Pointer; out OutLen: Integer; KeyType: TRSAKeyType): boolean;
var
  RsaFunc: function (flen: cint; from_buf, to_buf: PByte; arsa: PRSA; padding: cint): cint;
  Key: PRSA;
  S: UTF8String;
begin
  case KeyType of
    rsaPub:
      begin
        Key     := FPublicRSA;
        RsaFunc := @RSA_public_encrypt;
        S       := 'encrypt: ';
      end;
    rsaPriv:
      begin
        Key     := FPrivateRSA;
        RsaFunc := @RSA_private_decrypt;
        S       := 'decrypt: ';
      end;
  end;

  Output := GetMem(RSA_size(Key));
  OutLen := RsaFunc(InLen, Input, Output, Key, RSA_PKCS1_OAEP_PADDING);

  Result := (OutLen >= 0);

  if (not Result) then
    DoError('Failed to ' + S);
end;

function TEpiRSA.Encrypt(const OrigMsg: RawByteString; out EncMsg: RawByteString
  ): boolean;
begin
  result := CryptMsg(OrigMsg, EncMsg, rsaPub);
end;

function TEpiRSA.Encrypt(const Input: Pointer; InLen: Integer; out
  Output: Pointer; out OutLen: Integer): boolean;
begin
  result := CryptMsg(Input, InLen, Output, OutLen, rsaPub);
end;

function TEpiRSA.Decrypt(const EncMsg: RawByteString; out OrigMsg: RawByteString
  ): boolean;
begin
  result := CryptMsg(EncMsg, OrigMsg, rsaPriv);
end;

function TEpiRSA.Decrypt(const Input: Pointer; InLen: Integer; out
  Output: Pointer; out OutLen: Integer): boolean;
begin
  result := CryptMsg(Input, InLen, Output, OutLen, rsaPriv);
end;

end.

