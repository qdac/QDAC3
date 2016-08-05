program fdconverter;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  qconverter_fdac in '..\..\..\..\..\Source\qconverter_fdac.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
