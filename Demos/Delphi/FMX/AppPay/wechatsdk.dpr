program wechatsdk;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  weipay in 'weipay.pas' {frmWeiPay};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TfrmWeiPay, frmWeiPay);
  Application.Run;
end.
