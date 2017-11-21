program FMXVTDBDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  main in 'main.pas' {frmMain},
  dbtreeview in 'dbtreeview.pas' {frmDBTree},
  millonodes in 'millonodes.pas' {frmMillioNodes},
  fmxvteditors in 'fmxvteditors.pas',
  inspector in 'inspector.pas' {frmObjInspector};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;

end.
