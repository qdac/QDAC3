program host;

uses
  Vcl.Forms,
  hostmain in 'hostmain.pas' {Form3},
  qplugins_qworker in 'qplugins_qworker.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
