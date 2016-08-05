program qlogmobile;

uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  main in 'main.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
