program simpexp;

uses
  Vcl.Forms,
  main in 'main.pas' {Form1},
  QMathExpr in 'QMathExpr.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
