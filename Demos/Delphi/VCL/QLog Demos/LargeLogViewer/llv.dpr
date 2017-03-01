program llv;

uses
  Vcl.Forms,
  main in 'main.pas' {frmLogViewer},
  logextract in 'logextract.pas' {frmLogExtractor},
  about in 'about.pas' {frmAbout},
  sftpbrowser in 'sftpbrowser.pas' {frmSftpBrowser};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmLogViewer, frmLogViewer);
  Application.Run;
end.
