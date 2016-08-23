program dockhost;

uses
  Vcl.Forms,
  hostmain in 'hostmain.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := false;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
