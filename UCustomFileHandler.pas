unit UCustomDataFileInfo;

interface

type

  TFileHandlerType = (fhREC, fhDBF, fhSTAT, fhTXT, fhXLS, fhSAS, fhSPSS);

  TCustomFileHandler = class(TObject)
  private
    //
  protected
    //
    function GetFileHandlerType(): TFileHandlerType; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    function Read(RecordNum: integer): boolean; abstract;
    function Write(RecordNum: integer): boolean; abstract;
    function Commit: boolean; abstract;

    property FileHandlerType: TFileHandlerType read GetFileHandlerType();
  end;

implementation

{*****************  TCustomFileHandler ************************* }

constructor TCustomFileHandler.Create();
begin
  //
end;

destructor TCustomFileHandler.Destroy();
begin
  inherited Create();
end;

end.
