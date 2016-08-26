program proxysvc;

uses
  Vcl.Forms,
  hostmain in 'hostmain.pas' {Form1},
  main in 'main.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
