program mrswtest;

uses
  Vcl.Forms,
  Unit5 in 'Unit5.pas' {Form5},
  QMrsw in 'QMrsw.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm5, Form5);
  Application.Run;
end.
