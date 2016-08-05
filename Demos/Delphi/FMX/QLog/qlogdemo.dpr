program qlogdemo;

uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  main in 'main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
