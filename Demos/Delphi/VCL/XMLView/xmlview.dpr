program xmlview;

uses
  Forms,
  main in 'main.pas' {frmMain},
  find in 'find.pas' {frmFind};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmFind, frmFind);
  Application.Run;
end.
