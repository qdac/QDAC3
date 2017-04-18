unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
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
type
  TTestObject=class
    private
      Name:String;
    public
      constructor Create(AName:String);
  end;
//声明单例的实例，InitToNull 必需设置为nil
var
  TestObject:TQSingleton{$IFDEF UNICODE}<TTestObject>{$ENDIF}=(InitToNull:nil);

function GetTestObject:TTestObject;
begin
  Result:=TTestObject.Create('Jone smith');
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  Obj:TTestObject;
begin
{$IFDEF UNICODE}
  Obj:=TestObject.Instance(
  function:TTestObject
  begin
    Result:=TTestObject.Create('Jone smith');
  end
  );
{$ELSE}
  Obj:=TestObject.Instance(GetTestObject);
{$ENDIF}
 ShowMessage('Instance '+IntToHex(IntPtr(Obj),SizeOf(Pointer) shl 1)+'.Name='+Obj.Name);
end;

{ TTestObject }

constructor TTestObject.Create(AName: String);
begin
Name:=AName;
end;

{ TRunonceTask }

end.
