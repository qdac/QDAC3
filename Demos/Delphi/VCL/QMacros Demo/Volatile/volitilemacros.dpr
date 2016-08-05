program volitilemacros;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
