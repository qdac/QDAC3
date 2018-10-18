program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  QAMF3 in 'QAMF3.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:=True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
