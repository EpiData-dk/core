unit UEpiUtils;

interface

type
  TString = class(TObject)
  private
    fStr: String;
  public
    constructor Create(const AStr: String) ;
    property Str: String read FStr write FStr;
  end;

implementation

{ TString }

constructor TString.Create(const AStr: String) ;
begin
   inherited Create;
   FStr := AStr;
end;

end.
