unit vclmain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, qlog, StdCtrls;

type
  TForm2 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to 999 do
    PostLog(llHint, 'This is log,SeqNo: %d', [I + 1]);
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  CalcPerf('Button2Click');
  Sleep(1000);
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  SetDefaultLogFile(ExtractFilePath(Application.ExeName) + 'test.log',
    1024 * 1024, False, true).MaxLogHistories := 1; // 最大保留两个历史日志+1个当前日志文件
end;

end.
