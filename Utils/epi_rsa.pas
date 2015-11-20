unit epi_rsa;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, OpenSSL, dynlibs;

type

  { TEpiRSA }

  TEpiRSA = class
  private
    type TRSAKeyType = (rsaPub, rsaPriv);
  private
    FPrivateRSA: PRSA;
    FPublicRSA: PRSA;
    FPrivateKey: string;
    FPublicKey: string;
    procedure SetPrivateKey(AValue: string);
    procedure SetPublicKey(AValue: string);
    function PEMToRSA(PEM: String; KeyType: TRSAKeyType): PRSA;
  public
    constructor Create;
    destructor Destroy; override;
    function Encrypt(Const OrigMsg: String; out EncMsg: String): boolean; overload;
    function Decrypt(const EncMsg: String; var OrigMsg: String): boolean; overload;
    property PrivateKey: string read FPrivateKey write SetPrivateKey;
    property PublicKey: string read FPublicKey write SetPublicKey;
  end;

implementation



{ TEpiRSA }

constructor TEpiRSA.Create;
begin

end;

destructor TEpiRSA.Destroy;
begin
  inherited Destroy;
end;

procedure TEpiRSA.SetPrivateKey(AValue: string);
begin
  if FPrivateKey = AValue then Exit;
  FPrivateKey := AValue;


end;

procedure TEpiRSA.SetPublicKey(AValue: string);
begin
  if FPublicKey = AValue then Exit;
  FPublicKey := AValue;
end;

function TEpiRSA.PEMToRSA(PEM: String; KeyType: TRSAKeyType): PRSA;
begin

end;

function TEpiRSA.Encrypt(const OrigMsg: String; out EncMsg: String): boolean;
begin

end;

function TEpiRSA.Decrypt(const EncMsg: String; var OrigMsg: String): boolean;
begin

end;

end.

