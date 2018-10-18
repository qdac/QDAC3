unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, QString,
  qtimetypes,
  dateutils, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    procedure DoTest(S: QStringW);
  protected
    procedure WndProc(var AMsg: TMessage); override;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  DoTest('0 0 12 * * ?');
  DoTest('0 15 10 ? * *');
  DoTest('0 15 10 * * ?');
  DoTest('0 15 10 * * ? *');
  DoTest('0 15 10 * * ? 2005');
  DoTest('0 * 14 * * ?');
  DoTest('0 0/5 14 * * ?');
  DoTest('0 0/5 14,18 * * ?');
  DoTest('0 0-5 14 * * ?');
  DoTest('0 10,44 14 ? 3 WED');
  DoTest('0 15 10 ? * MON-FRI');
  DoTest('0 15 10 15 * ?');
  DoTest('0 15 10 L * ?');
  DoTest('0 15 10 L-2 * ?');
  DoTest('0 15 10 ? * 6L');
  DoTest('0 15 10 ? * 6L');
  DoTest('0 15 10 ? * 6L 2002-2005');
  DoTest('0 15 10 ? * 6#3');
  DoTest('0 0 12 1/5 * ?');
  DoTest('0 11 11 11 11 ?');
  DoTest('0 0 12,22 ? * W');
  DoTest('0 0 12,23 LW * *');
  DoTest('0 0 12,23 L-2W');
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  S: String;
begin
  if InputQuery('cron 表达式', '请输入 cron 表达式：', S) then
  begin
    DoTest(S);
  end;
end;

procedure TForm1.DoTest(S: QStringW);
var
  AMask: TQPlanMask;
  ANextTime: TDateTime;
begin
  AMask.AsString := S;
  ANextTime := AMask.NextTime;
  if Trunc(ANextTime) = 0 then
    Memo1.Lines.Add('表达式(Exp):' + S + ' ==> ' + AMask.AsString + ' 该计划已错过最后执行时间(Mask plan is out of date)。')
  else
    Memo1.Lines.Add('表达式(Exp):' + S + ' ==> ' + AMask.AsString + ' 下次执行时间(Next time):' +
      FormatDateTime('yyyy-mm-dd hh:nn:ss', ANextTime) + ',' +
      FormatSettings.LongDayNames[DayOfWeek(ANextTime)]);
end;

procedure TForm1.WndProc(var AMsg: TMessage);
begin
  inherited;
  if AMsg.Msg = WM_TIMECHANGE then
  begin
    // 如果时间改变了，则需要重新计算最近的任务执行时间
  end;
end;

end.
