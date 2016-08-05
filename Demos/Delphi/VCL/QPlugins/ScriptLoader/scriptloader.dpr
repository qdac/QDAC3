program scriptloader;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  qplugins in 'qplugins.pas',
  qplugins_mgr in 'qplugins_mgr.pas',
  scriptplugin in 'scriptplugin.pas' {Form2},
  qpluginsintf in 'qpluginsintf.pas',
  qplugins_pas_ldr in 'qplugins_pas_ldr.pas',
  directsvc in 'directsvc.pas',
  menu_demo in 'menu_demo.pas',
  rttisvc in 'rttisvc.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
