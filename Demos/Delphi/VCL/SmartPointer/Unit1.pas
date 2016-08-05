unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls;

type
  TTestObject=class
  private
    FName:String;
    procedure SetName(const Value: String);
  public
    constructor Create;overload;
    destructor Destroy;override;
    property Name:String read FName write SetName;
  end;
  TForm1 = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses qstring;
{$R *.dfm}

{ TTestObject }

constructor TTestObject.Create;
begin
inherited;
Form1.Memo1.Lines.Add('构造函数已经被调用');
end;

destructor TTestObject.Destroy;
begin
Form1.Memo1.Lines.Add('析构函数已经被调用');
  inherited;
end;

procedure TTestObject.SetName(const Value: String);
begin
  FName := Value;
Form1.Memo1.Lines.Add('名称设置函数已经被调用');
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  ATest:TTestObject;
  I:Integer;
begin
Memo1.Lines.BeginUpdate;
for I := 0 to 999 do
  begin
  ATest:=TQPtr.Bind(TTestObject.Create).Get;;
  ATest.Name:='TestObject';
  end;
Memo1.Lines.EndUpdate;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
ReportMemoryLeaksOnShutdown:=True;
end;

end.
