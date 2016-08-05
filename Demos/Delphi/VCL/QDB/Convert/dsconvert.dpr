program dsconvert;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  qconverter_sql in '..\..\..\..\..\Source\qconverter_sql.pas',
  qconverter_cdsxml in '..\..\..\..\..\Source\qconverter_cdsxml.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
