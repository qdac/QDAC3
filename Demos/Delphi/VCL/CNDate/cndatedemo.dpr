program cndatedemo;

uses
  Vcl.Forms,
  main in 'main.pas' {Form5},
  cncalendar in 'cncalendar.pas' {frmCnCalendar};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm5, Form5);
  Application.Run;
end.
