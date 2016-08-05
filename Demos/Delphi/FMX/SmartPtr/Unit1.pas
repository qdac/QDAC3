unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Memo;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;
  TTestObject=class
  private
    FName:String;
    procedure SetName(const Value: String);
  public
    constructor Create;overload;
    destructor Destroy;override;
    property Name:String read FName write SetName;
  end;
var
  Form1: TForm1;

implementation
uses qstring;
{$R *.fmx}
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
begin
ATest:=TQPtr<TTestObject>.Bind(TTestObject.Create).Get;;
ATest.Name:='TestObject';
ShowMessage('对象已经被创建');
end;

end.
