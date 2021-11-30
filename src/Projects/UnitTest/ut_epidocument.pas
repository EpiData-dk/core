unit ut_epidocument;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestFramework, epidocument, epicustombase;

type

  { TUnitTest_EpiDocument }

  TUnitTest_EpiDocument = class(TTestCase)
  private
    FCalledPassword: boolean;
    FEpiDoc: TEpiDocument;
    procedure GetPassWord(Sender: TObject; var Login: string;
      var Password: string);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CloneDocument;
    procedure LoadFromFile;
    procedure LoadFromFileWithPassword;
    procedure SaveToXml;
    procedure SaveToFile;
  end;

implementation

uses
  FileUtil;


{ TUnitTest_EpiDocument }

procedure TUnitTest_EpiDocument.GetPassWord(Sender: TObject; var Login: string;
  var Password: string);
begin
  FCalledPassword := true;
  Login := '';
  Password := 'fakepass';
end;

procedure TUnitTest_EpiDocument.SetUp;
begin
  inherited SetUpOnce;

  FEpiDoc := TEpiDocument.Create('en');
  FCalledPassword := false;
end;

procedure TUnitTest_EpiDocument.TearDown;
begin
  FEpiDoc.Free;

  inherited TearDownOnce;
end;

procedure TUnitTest_EpiDocument.CloneDocument;
var
  Doc: TEpiCustomBase;
begin
  Doc := FEpiDoc.Clone;
  CheckNotNull(Doc);
  Doc.Free;
end;

procedure TUnitTest_EpiDocument.LoadFromFile;
begin
  FEpiDoc.LoadFromFile('sample.epx');
  CheckTrue(FEpiDoc.DataFiles[0].Fields.Count > 0, 'Sample.epx not loaded correctly!');
end;

procedure TUnitTest_EpiDocument.LoadFromFileWithPassword;
begin
  FEpiDoc.OnPassword := @GetPassWord;
  FEpiDoc.LoadFromFile('sample.encrypted.epx');
  CheckTrue(FCalledPassword, 'Did not call password function!');
  CheckTrue(FEpiDoc.DataFiles[0].Fields.Count > 0, 'Sample.encrypted.epx not loaded correctly!');
end;

procedure TUnitTest_EpiDocument.SaveToXml;
var
  S: String;
begin
  S := FEpiDoc.SaveToXml(0, true);
  CheckTrue(Length(S) > 20, 'FEpiDoc not saved correctly');
end;

procedure TUnitTest_EpiDocument.SaveToFile;
begin
  FEpiDoc.SaveToFile('sample.test.epx');
  CheckTrue(FileExistsUTF8('sample.test.epx'), 'File not save correctly');
end;

initialization
  RegisterTest(TUnitTest_EpiDocument.Suite);

end.

