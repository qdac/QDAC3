program FMXVTDBDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  main in 'main.pas' {frmMain},
  dbtreeview in 'dbtreeview.pas' {frmDBTree},
  millonodes in 'millonodes.pas' {frmMillioNodes},
  inspector in 'inspector.pas' {frmObjInspector},
  multidrawer in 'multidrawer.pas' {frmCellMultiDrawer};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmCellMultiDrawer, frmCellMultiDrawer);
  Application.Run;

end.
