program TestMenu;

uses
  Forms,
  frmMain in 'frmMain.pas' {MainForm},
  AboutFrm in 'AboutFrm.pas' {frmAbout},
  frmCertList in 'frmCertList.pas' {Form3};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
