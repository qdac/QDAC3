unit Unit4;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm4 = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation
uses qtimetypes,dateutils;
{$R *.dfm}

procedure TForm4.Button1Click(Sender: TObject);
var
  ATime:TQTimeStamp;
begin
ATime:=Now;
Memo1.Lines.Add('直接赋DateTime值:'+ATime.AsString);
ATime.Year:=2011;
ATime.Month:=11;
ATime.Day:=11;
ATime.Hour:=11;
ATime.Minute:=11;
ATime.Second:=11;
ATime.MilliSecond:=111;
Memo1.Lines.Add('成员赋值:'+ATime.AsString);
ATime.Encode(2013,1,1);
Memo1.Lines.Add('Encode(2013,1,1):'+ATime.AsString);
ATime.Encode(11,12,33,11);
Memo1.Lines.Add('Encode(11,12,33,11):'+ATime.AsString);
ATime.Encode(2013,1,1,11,12,33,11);
Memo1.Lines.Add('Encode(2013,1,1,11,12,33,11):'+ATime.AsString);
end;

procedure TForm4.Button2Click(Sender: TObject);
var
  AInterval:TQInterval;
begin
//1.按成员赋值
AInterval.Clear;
AInterval.Year:=1;
AInterval.Month:=2;
AInterval.Day:=3;
AInterval.Hour:=4;
AInterval.Minute:=5;
AInterval.Second:=6;
AInterval.MilliSecond:=7;
Memo1.Lines.Add('按成员赋值(Year=1,Month=2,Day=3,Hour=4,Minute=5,Second=6,MilliSecond=7):'#13#10#9+AInterval.AsPgString);
//2.负数值
AInterval.Month:=-13;
AInterval.Hour:=-25;
Memo1.Lines.Add('修改月为-13,小时为-25:'#13#10#9+AInterval.AsPgString);
//3.使用Encode赋值
AInterval.Encode(8,9,10,11,12,13,14);
Memo1.Lines.Add('使用Encode(8,9,10,11,12,13,14)赋值:'#13#10#9+AInterval.AsPgString);
//4.使用字符串赋值
AInterval.AsPgString:='1 year 2 month 3 week 4 day 5 hour 6 minute 7 second 8 millisecond';
Memo1.Lines.Add('使用AsPgString属性赋值(1 year 2 month 3 week 4 day 5 hour 6 minute 7 second 8 millisecond):'#13#10#9+AInterval.AsPgString);
AInterval.AsOracleString:='Interval ''1-2'' year to month';
Memo1.Lines.Add('使用AsOracleString属性赋值(Interval ''1-2'' year to month):'#13#10#9+AInterval.AsPgString);
AInterval.AsOracleString:='Interval ''3 4:5:6.8'' day to second';
Memo1.Lines.Add('使用AsOracleString属性赋值(Interval ''3 4:5:6.8'' day to second):'#13#10#9+AInterval.AsPgString);
AInterval.AsSQLString:='1-2 3 4:5:6.789';
Memo1.Lines.Add('使用AsSQLString属性赋值(1-2 3 4:5:6.789):'#13#10#9+AInterval.AsPgString);
AInterval.AsISOString:='P9Y8M7DT5H4M3.2S';
Memo1.Lines.Add('使用AsISOString属性赋值(P9Y8M7DT5H4M3.2S):'#13#10#9+AInterval.AsPgString);
//5.使用类函数
AInterval:=TQInterval.DateTimeToInterval(Now);
Memo1.Lines.Add('DateTimeToInterval(Now):'#13#10#9+AInterval.AsPgString);
end;

procedure TForm4.Button3Click(Sender: TObject);
var
  ADate:TDateTime;
  AInterval:TQInterval;
begin
AInterval.Clear;
AInterval.Year:=2;
ADate:=Now+AInterval;
Memo1.Lines.Add('Now()+'+AInterval.AsString+'='+FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',ADate));
AInterval.Year:=-2;
ADate:=Now+AInterval;
Memo1.Lines.Add('Now()+'+AInterval.AsString+'='+FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',ADate));
ADate:=Now-AInterval;
Memo1.Lines.Add('Now()-'+AInterval.AsString+'='+FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',ADate));
AInterval.Year:=2;
ADate:=Now-AInterval;
Memo1.Lines.Add('Now()-'+AInterval.AsString+'='+FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',ADate));
AInterval:=AInterval-TQInterval.EncodeInterval(1,0);
Memo1.Lines.Add(AInterval.AsString+'-TQInterval.EncodeInterval(1,0)='+AInterval.AsPgString);
AInterval:=AInterval+TQInterval.EncodeInterval(1,0);
Memo1.Lines.Add(AInterval.AsString+'+TQInterval.EncodeInterval(1,0)='+AInterval.AsPgString);
AInterval.IncMonth(13);
Memo1.Lines.Add(AInterval.AsString+'->IncMonth(13)='+AInterval.AsPgString);
AInterval.IncDay(1).IncHour(2).IncMinute(3);
Memo1.Lines.Add(AInterval.AsString+'->IncDay(1)->IncHour(2)->IncMinute(3)='+AInterval.AsPgString);
end;

procedure TForm4.Button4Click(Sender: TObject);
var
  AInterval:TQInterval;
begin
AInterval.Encode(1,2,3,4,5,6,7);
Memo1.Lines.Add('PostgreSQL格式:'+AInterval.AsPgString);
Memo1.Lines.Add('SQL标准格式:'+AInterval.AsSQLString);
Memo1.Lines.Add('Oracle 格式:'+AInterval.AsOracleString);
Memo1.Lines.Add('ISO 8601/GBT7504格式:'+AInterval.AsISOString);
Memo1.Lines.Add('自定义格式:'+AInterval.Format('y年m月n日 h时n分s秒z毫秒',False));
end;

procedure TForm4.Button5Click(Sender: TObject);
var
  ATime:TQTimeStamp;
begin
ATime:=Today;
ATime.IncDay(50);
ShowMessage(ATime.AsString+',IncDay='+FormatDateTime('yyyy-mm-dd',IncDay(Now,50)));
ATime:=Today;
ATime.IncDay(-50);
ShowMessage(ATime.AsString+',IncDay='+FormatDateTime('yyyy-mm-dd',IncDay(Now,-50)));
ATime:=EncodeDate(2014,3,31);
ATime.IncMonth(-1);
ShowMessage(ATime.AsString+',IncMonth='+FormatDateTime('yyyy-mm-dd',IncMonth(EncodeDate(2014,3,31),-1)));
end;

end.
