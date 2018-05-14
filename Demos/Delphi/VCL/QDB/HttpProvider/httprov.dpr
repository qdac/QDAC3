program httprov;

uses
  Vcl.Forms,
  main in 'main.pas' {Form2},
  qprov_http in '..\..\..\..\..\Source\qprov_http.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
