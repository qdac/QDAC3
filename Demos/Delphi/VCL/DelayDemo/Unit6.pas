unit Unit6;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VclTee.TeeGDIPlus, Vcl.StdCtrls,
  Vcl.ExtCtrls, VclTee.TeEngine, VclTee.TeeProcs, VclTee.Chart, QWorker,
  VclTee.Series;

type
  TForm6 = class(TForm)
    Chart1: TChart;
    Panel1: TPanel;
    Button1: TButton;
    slCos: TLineSeries;
    slSin: TLineSeries;
    CheckBox1: TCheckBox;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FFlags: Integer;
    procedure DoSinJob(AJob: PQJob);
    procedure DoCosJob(AJob: PQJob);
  public
    { Public declarations }
  end;

var
  Form6: TForm6;

implementation

{$R *.dfm}

procedure TForm6.Button1Click(Sender: TObject);
begin
slCos.Clear;
slSin.Clear;
FFlags := 0;
Button1.Enabled:=False;
if CheckBox1.Checked then
  begin
  Chart1.LeftAxis.SetMinMax(-1, 1);
  Chart1.BottomAxis.SetMinMax(0, 10);
  end
else
  begin
  Chart1.LeftAxis.Automatic:=True;
  Chart1.BottomAxis.Automatic:=True;
  end;
Workers.Delay(DoSinJob, Q1MillSecond * 50, Pointer(0), True);
Workers.Delay(DoCosJob, Q1MillSecond * 50, Pointer(0), True);
end;

procedure TForm6.DoCosJob(AJob: PQJob);
var
  V: Double;
begin
V := Integer(AJob.Data) / 10;
slCos.AddXY(V, cos(V));
if Integer(AJob.Data) < 100 then
  Workers.Delay(DoCosJob, Q1MillSecond * 50,
    Pointer(Integer(AJob.Data) + 1), True)
else
  begin
  FFlags := FFlags or $01;
  if FFlags = $03 then
    Button1.Enabled := True;
  end;
end;

procedure TForm6.DoSinJob(AJob: PQJob);
var
  V: Double;
begin
V := Integer(AJob.Data) / 10;
slSin.AddXY(V, sin(V));
if Integer(AJob.Data) < 100 then
  Workers.Delay(DoSinJob, Q1MillSecond * 50,
    Pointer(Integer(AJob.Data) + 1), True)
else
  begin
  FFlags := FFlags or $02;
  if FFlags = $03 then
    Button1.Enabled := True;
  end;
end;

end.
