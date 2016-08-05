program stateworker;

uses
  Vcl.Forms,
  main in 'main.pas' {frmMain},
  workpanel in 'workpanel.pas' {frmWork};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
