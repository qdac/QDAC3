program memstatics;

uses
  qmemstatics in '..\..\..\..\Source\qmemstatics.pas',
  Forms,
  dlgmemstatics in 'dlgmemstatics.pas' {frmMemStatics};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMemStatics, frmMemStatics);
  Application.Run;
end.
