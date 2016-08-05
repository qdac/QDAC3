program menubuilder;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  qplugins in '..\kernel\qplugins.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
