program jsonview;

uses
  Forms,
  main in 'main.pas' {Form1},
  openurl in 'openurl.pas' {frmUrlInput};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
