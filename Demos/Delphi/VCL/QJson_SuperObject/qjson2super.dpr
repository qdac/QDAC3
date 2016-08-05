program qjson2super;

uses
  Forms,
  main in 'main.pas' {Form1},
  superobjecthelper in 'superobjecthelper.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
