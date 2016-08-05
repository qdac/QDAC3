program taskplan;

uses
  Vcl.Forms,
  main in 'main.pas' {Form5},
  formdropdown in 'formdropdown.pas' {frmDropDownBase},
  taskprop in 'taskprop.pas' {frmTaskProps};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm5, Form5);
  Application.CreateForm(TfrmTaskProps, frmTaskProps);
  Application.Run;
end.
