program PgSQLDemo;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  qprov_pgsql in '..\..\..\..\..\Source\qprov_pgsql.pas',
  Login in 'Login.pas' {frmPgLogin};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
