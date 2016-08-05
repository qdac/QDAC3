program ipcdemo;

uses
  Vcl.Forms,
  main in 'main.pas' {Form3},
  qipc in 'qipc.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
