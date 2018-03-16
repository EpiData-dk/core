unit epitools_archieve;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, zipper;

type

  { TEpiToolCompressor }

  TEpiToolCompressor = class
  private
    FFiles: TStrings;
    FOnCompressError: TNotifyEvent;
    FOnEncryptionError: TNotifyEvent;
    FPassword: UTF8String;
    FRootDir: UTF8String;
  public
    constructor Create;
    destructor Destroy; override;
    function CompressToFile(const Filename: UTF8String): boolean;
    function CompressToStream(ST: TStream): boolean;
    property RootDir: UTF8String read FRootDir write FRootDir;
    // Input files to compress (and encrypt)
    property Files: TStrings read FFiles;
    // Password used for encrypting file (if empty just zip)
    property Password: UTF8String read FPassword write FPassword;
    property OnCompressError: TNotifyEvent read FOnCompressError write FOnCompressError;
    property OnEncryptionError: TNotifyEvent read FOnEncryptionError write FOnEncryptionError;
  end;

  { TEpiToolDeCompressor }

  TEpiToolDeCompressor = class
  private
    FDestinationDir: UTF8String;
    FOnDecompressionError: TNotifyEvent;
    FOnDecryptionError: TNotifyEvent;
    FPassword: UTF8String;
    FDecompressStream: TStream;
    procedure UnzipCloseStream(Sender: TObject; var AStream: TStream);
    procedure UnzipOpenStream(Sender: TObject; var AStream: TStream);
  protected
    function InternalDecompress(ST: TStream): boolean;
    procedure DoDecryptionError();
  public
    constructor Create;
    destructor Destroy; override;
    function DecompressFromFile(Const Filename: UTF8String): boolean;
    function DecompressFromStream(ST: TStream): boolean;
    // Directory where all files are unzipped. Make sure the destination exists!
    property DestinationDir: UTF8String read FDestinationDir write FDestinationDir;
    property Password: UTF8String read FPassword write FPassword;
    property OnDecompressionError: TNotifyEvent read FOnDecompressionError write FOnDecompressionError;
    property OnDecryptionError: TNotifyEvent read FOnDecryptionError write FOnDecryptionError;
  end;

implementation

uses
  LazUTF8Classes, DCPrijndael, DCPsha512, LazFileUtils;

const
  EPITOOL_ARCHIVE_MAGIC = 'EPI1';

{ TEpiToolCompressor }

constructor TEpiToolCompressor.Create;
begin
  FFiles := TStringListUTF8.Create;
end;

destructor TEpiToolCompressor.Destroy;
begin
  FFiles.Clear;
  FFiles.Free;
  inherited Destroy;
end;

function TEpiToolCompressor.CompressToFile(const Filename: UTF8String): boolean;
var
  FS: TFileStreamUTF8;
begin
  FS := TFileStreamUTF8.Create(Filename, fmCreate);
  result := CompressToStream(FS);
  FS.Free;
end;

function TEpiToolCompressor.CompressToStream(ST: TStream): boolean;
var
  Zip: TZipper;
  InternalStream: TMemoryStream;
  Encrypter: TDCP_rijndael;
  S: String;
begin
  InternalStream := TMemoryStream.Create;

  Zip := TZipper.Create;
  Zip.InMemSize := 64 * 1024 * 1024;  // This allows for 64MB files to be compressed in memory;

  for S in Files do
    begin
      if FileIsInPath(S, RootDir) then
        Zip.Entries.AddFileEntry(S, ExtractRelativepath(RootDir, S))
      else
        Zip.Entries.AddFileEntry(S);
    end;

  Zip.SaveToStream(InternalStream);
  Zip.Free;

  InternalStream.Position := 0;

  if (Password <> '') then
    begin
      ST.Write(EPITOOL_ARCHIVE_MAGIC[1], Length(EPITOOL_ARCHIVE_MAGIC));

      Encrypter := TDCP_rijndael.Create(nil);
      Encrypter.InitStr(Password, TDCP_sha512);
      Encrypter.EncryptStream(InternalStream, ST, InternalStream.Size);

      Encrypter.Free;
    end
  else
    ST.CopyFrom(InternalStream, InternalStream.Size);

  InternalStream.Free;
end;

{ TEpiToolDeCompressor }

procedure TEpiToolDeCompressor.UnzipOpenStream(Sender: TObject;
  var AStream: TStream);
begin
  AStream := FDecompressStream;
end;

procedure TEpiToolDeCompressor.UnzipCloseStream(Sender: TObject;
  var AStream: TStream);
begin
  // Set AStream to nil, otherwise it free's the stream in the unzipper;
  AStream := nil;
end;

function TEpiToolDeCompressor.InternalDecompress(ST: TStream): boolean;
var
  UnZip: TUnZipper;
begin
  FDecompressStream := ST;

  UnZip := TUnZipper.Create;
  UnZip.OutputPath := DestinationDir;
  UnZip.OnOpenInputStream := @UnzipOpenStream;
  UnZip.OnCloseInputStream := @UnzipCloseStream;
  UnZip.UnZipAllFiles;

  Result := true;
end;

procedure TEpiToolDeCompressor.DoDecryptionError();
begin
  if Assigned(OnDecryptionError) then
    OnDecryptionError(Self);
end;

constructor TEpiToolDeCompressor.Create;
begin

end;

destructor TEpiToolDeCompressor.Destroy;
begin
  inherited Destroy;
end;

function TEpiToolDeCompressor.DecompressFromFile(const Filename: UTF8String
  ): boolean;
var
  FS: TFileStreamUTF8;
begin
  FS := TFileStreamUTF8.Create(Filename, fmOpenRead);
  Result := DecompressFromStream(FS);
  FS.Free;
end;

function TEpiToolDeCompressor.DecompressFromStream(ST: TStream): boolean;
var
  Buffer: Array[0..3] of Char;
  S: String;
  Decrypter: TDCP_rijndael;
  InternalStream: TMemoryStream;
begin
  ST.ReadBuffer(Buffer, SizeOf(Buffer));

  S := String(Buffer);
  if (S = 'PK'#03#04) then
    begin
      // Regular zip file
      if (Password <> '') then
        begin
          DoDecryptionError();
          Exit(false);
        end;

      Result := InternalDecompress(ST);
    end;

  if (S = EPITOOL_ARCHIVE_MAGIC) then
    begin
      // Encrypted EpiData (new format)
      if (Password = '') then
        begin
          DoDecryptionError();
          Exit(false);
        end;

      InternalStream := TMemoryStream.Create;

      Decrypter := TDCP_rijndael.Create(nil);
      Decrypter.InitStr(Password, TDCP_sha512);
      Decrypter.DecryptStream(ST, InternalStream, ST.Size - ST.Position);
      Decrypter.Free;

      Result := InternalDecompress(InternalStream);
      InternalStream.Free;
    end;

  // Should we implement a feature to import old .ZKY files?
end;

end.

