unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses qstring,qjson,qrbtree,superobjecthelper,superobject{$IFDEF UNICODE},rtti{$ENDIF};
{$R *.dfm}



procedure TForm1.Button1Click(Sender: TObject);
var
  S:String;
  AJson: TSuperObjectHelper;
  AJson2:TQJson;
  ASuper:ISuperObject;
  I: Integer;
  T,TS: Cardinal;
begin
  AJson := TSuperObjectHelper.Create;
  try
    T := GetTickCount;
    for I := 0 to 10000 * 10 do
    begin
      S:='abc' + IntToStr(i);
      AJson.S[S] := 'hello' + IntToStr(i * 10);
      AJson.S[S];
//      if  AJson.IndexOf(S)=-1 then
//        ShowMessage('Not found');
    end;
    T := GetTickCount - T;
    ASuper:=TSuperObject.Create();
    TS:=GetTickCount;
    for I := 0 to 10000 * 10 do
    begin
      S:='abc' + IntToStr(i);
      ASuper.S[S] := 'hello' + IntToStr(i * 10);
      ASuper.S[S];
    end;
    TS := GetTickCount - TS;
    ShowMessage('QJson添加100,000个结点用时:' + IntToStr(T) + 'ms,SuperObject='+IntToStr(TS)+'ms');
    AJson2:=TQJson.Create;
    T := GetTickCount;
    for I := 0 to 10000 * 10 do
    begin
      AJson2.Add('abc' + IntToStr(i)).AsString := 'hello' + IntToStr(i * 10);
    end;
    T := GetTickCount - T;
    AJson2.Free;
    ShowMessage('添加100,000个结点用时:' + IntToStr(T) + 'ms');
  finally
    AJson.Free;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  AJson,AObj:TSuperObjectHelper;
  Arr:TSuperArrayHelper;
begin
//AJson:=TQPtr<TSuperObjectHelper>.Bind(TSuperObjectHelper.Create).Get;
AJson := TSuperObjectHelper.Create;
AJson.S['String']:='这是一个字符串';
AJson.I['Integer']:=123;
AJson.D['Float']:=3.21;
AJson.B['Boolean']:=True;
//AObj:=TQPtr<TSuperObjectHelper>.Bind(TSuperObjectHelper.Create).Get;
AObj := TSuperObjectHelper.Create;
AObj.S['Name']:='子对象';
AJson.O['Object']:=AObj;
//Arr:=TQPtr<TSuperArrayHelper>.Bind(TSuperArrayHelper.Create).Get;
Arr := TSuperArrayHelper.Create;
Arr.I[0]:=100;
AJson.N['Array']:=Arr;
AJson.C['Currency']:=1.235;
AJson.S['Path.Name']:='按路径赋值的';
//混合
AJson.Add('QJsonNode').AsString:='这是直接用QJson方法添加的值';
ShowMessage(AJson.AsJson);
end;
end.
