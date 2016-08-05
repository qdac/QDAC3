program namepipedemo;

uses
  Vcl.Forms,
  main in 'main.pas' {Form1},
  QNamePipe in '..\..\..\..\Source\QNamePipe.pas',
  QSimplePool in '..\..\..\..\Source\QSimplePool.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
