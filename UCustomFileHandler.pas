unit UCustomFileHandler;

interface

uses
  UEpiDataConstants;

type
  TEpiDataFileOption = (eoInMemory, eoIgnoreChecks, eoReadRelates, oeIgnoreIndex);
  TEpiDataFileOptions = set of TEpiDataFileOption;

  TFileHandlerType = (fhREC, fhDBF, fhSTATA, fhTXT, fhXLS, fhSAS, fhSPSS);

  TCustomFileHandler = class(TObject)
  private
    FEpiDataFile: TObject;
  protected
    function GetFileHandlerType(): TFileHandlerType; virtual; abstract;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function   Load(filename:string=''; aOptions:TEpiDataFileOptions=[]):boolean;  virtual; abstract;
    function   Read(RecordNum: integer): boolean; virtual; abstract;
    function   Write(RecordNum: integer): boolean; virtual; abstract;
    function   Commit: boolean; virtual; abstract;

    property   FileHandlerType: TFileHandlerType read GetFileHandlerType;
    property   EpiDataFile:TObject read FEpiDataFile write FEpiDataFile;
  end;

implementation

{*****************  TCustomFileHandler ************************* }

constructor TCustomFileHandler.Create();
begin
  inherited create;
end;

destructor TCustomFileHandler.Destroy();
begin
  inherited destroy;
end;

end.
