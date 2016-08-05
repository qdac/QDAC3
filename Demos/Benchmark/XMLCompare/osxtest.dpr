program osxtest;

uses
  FMX.Forms,
  osxmain in 'osxmain.pas' {Form1},
  qxml in 'qxml.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
