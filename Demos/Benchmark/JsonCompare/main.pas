unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls,qstring,qjson;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    mmResult: TMemo;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Button10: TButton;
    chkCreate: TCheckBox;
    chkLoad: TCheckBox;
    chkSave: TCheckBox;
    chkTypes: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
  private
    { Private declarations }
    procedure CreateTest;
    procedure LoadTest;
    procedure SaveTest;
    procedure IOTest;
    procedure TypeTest;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
{$R *.dfm}
uses uLkJSON,superobject,yxdjson,UJSON,JSON;

function GetFileSize(AFileName:String):Int64;
var
  sr:TSearchRec;
  AHandle:Integer;
begin
AHandle:=FindFirst(AFileName,faAnyFile,sr);
if AHandle=0 then
  begin
  Result:=sr.Size;
  FindClose(sr);
  end
else
  Result:=0;
end;

procedure TForm1.Button10Click(Sender: TObject);
var
  T:Cardinal;
begin
Button10.Enabled:=False;
T:=GetTickCount;
mmResult.Lines.Add('测试开始……');
Caption:='测试创建10万结点速度...';
Update;
if chkCreate.Checked then
  CreateTest;
Caption:='测试加载速度...';
Update;
if chkLoad.Checked then
  LoadTest;
Caption:='测试保存速度...';
Update;
if chkSave.Checked then
  SaveTest;
Caption:='测试不同类型解析速度...';
Update;
if chkTypes.Checked then
  TypeTest;
Caption:='JSON Compare';
mmResult.Lines.Add('测试结束，共用时'+RollupTime((GetTickCount-T) div 1000));
Button10.Enabled:=True;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  AJson:TQJson;
  I:Integer;
  T:Cardinal;
begin
AJson:=TQJson.Create;
try
  T:=GetTickCount;
  for I := 0 to 100000 do
    AJson.Add('_'+IntToStr(I),Now);
  T:=GetTickCount-T;
  mmResult.Clear;
  mmResult.Lines.Add('添加100,000个结点用时:'+IntToStr(T)+'ms');
finally
  AJson.Free;
end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  AJson:TQJson;
  T:Cardinal;
  Speed:Cardinal;
  lkJson:TlkJSONbase;

begin
if OpenDialog1.Execute then
  begin
//  uJsonTest;
  AJson:=TQJson.Create;
  try
    T:=GetTickCount;
    AJson.LoadFromFile(OpenDialog1.FileName,QString.teUtf8);
    T:=GetTickCount-T;
    if T>0 then
      Speed:=(GetFileSize(OpenDialog1.FileName)*1000 div T)
    else
      Speed:=0;
    mmResult.Clear;
//    mmResult.Lines.Add('加载的JSON文件内容：');
//    mmResult.Lines.Add(AJson.Encode(True));
    mmResult.Lines.Add('QJson加载用时:'+IntToStr(T)+'ms，速度:'+RollupSize(Speed));
    T:=GetTickCount;
    lkJson:=TlkJSONstreamed.LoadFromFile(OpenDialog1.FileName);
    T:=GetTickCount-T;
    if T>0 then
      Speed:=(GetFileSize(OpenDialog1.FileName)*1000 div T)
    else
      Speed:=0;
    mmResult.Lines.Add('lkJson加载用时:'+IntToStr(T)+'ms，速度:'+RollupSize(Speed));
    lkJson.Free;
  finally
    AJson.Free;
  end;
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  AJson:TQJson;
  I:Integer;
  T1,T2:Cardinal;
  Speed:Cardinal;
begin
if SaveDialog1.Execute then
  begin
  AJson:=TQJson.Create;
  try
    mmResult.Clear;
    T1:=GetTickCount;
    with AJson.Add('Integers',qjson.jdtObject) do
      begin
      for I := 0 to 1000000 do
        Add('Node'+IntToStr(I)).AsInteger :=I;
      end;
    T1:=GetTickCount-T1;
    T2:=GetTickCount;
    AJson.SaveToFile(SaveDialog1.FileName,qstring.teAnsi,false);
    T2:=GetTickCount-T2;
    if T2>0 then
      Speed:=(GetFileSize(SaveDialog1.FileName)*1000 div T2)
    else
      Speed:=0;
    mmResult.Lines.Add('生成10万结点用时'+IntToStr(T1)+'ms,保存用时:'+IntToStr(T2)+'ms，速度：'+RollupSize(Speed));
  finally
    AJson.Free;
  end;
  end;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  AJson:TQJson;
begin
AJson:=TQJson.Create;
try
  AJson.Parse('{"results":[],"status":102,"msg":"IP\/SN\/SCODE\/REFERER Illegal:"}');
//  '{"name":"object_0","Id":1}');
  ShowMessage(AJson.Encode(True));
finally
  AJson.Free;
end;
end;

procedure TForm1.Button7Click(Sender: TObject);
var
  AStream:TMemoryStream;
  AJson:TQJson;
  S:QStringW;
  AEncode:qstring.TTextEncoding;
begin
AStream:=TMemoryStream.Create;
AJson:=TQJson.Create;
try
  AJson.DataType:=qjson.jdtObject;
  S:='{"record1":{"id":100,"name":"name1"}}'#13#10+
    '{"record2":{"id":200,"name":"name2"}}'#13#10+
    '{"record3":{"id":300,"name":"name3"}}'#13#10;
  //UCS2
  mmResult.Lines.Add('Unicode 16 LE编码:');
  AEncode:=qstring.teUnicode16LE;
  AStream.Size:=0;
  SaveTextW(AStream,S,False);
  AStream.Position:=0;
  AJson.Clear;
  AJson.ParseBlock(AStream,AEncode);
  mmResult.Lines.Add('第一次解析结果:'#13#10);
  mmResult.Lines.Add(AJson.AsJson);
  AJson.Clear;
  AJson.ParseBlock(AStream,AEncode);
  mmResult.Lines.Add(#13#10'第二次解析结果:'#13#10);
  mmResult.Lines.Add(AJson.AsJson);
  AJson.Clear;
  AJson.ParseBlock(AStream,AEncode);
  mmResult.Lines.Add(#13#10'第三次解析结果:');
  mmResult.Lines.Add(AJson.AsJson);
  //UTF-8
  mmResult.Lines.Add('UTF8编码:');
  AEncode:=qstring.teUtf8;
  AStream.Size:=0;
  SaveTextU(AStream,qstring.Utf8Encode(S),False);
  AStream.Position:=0;
  AJson.Clear;
  AJson.ParseBlock(AStream,AEncode);
  mmResult.Lines.Add(#13#10'第一次解析结果:'#13#10);
  mmResult.Lines.Add(AJson.AsJson);
  AJson.Clear;
  AJson.ParseBlock(AStream,AEncode);
  mmResult.Lines.Add(#13#10'第二次解析结果:'#13#10);
  mmResult.Lines.Add(AJson.AsJson);
  AJson.Clear;
  AJson.ParseBlock(AStream,AEncode);
  mmResult.Lines.Add(#13#10'第三次解析结果:');
  mmResult.Lines.Add(AJson.AsJson);
  //ANSI
  mmResult.Lines.Add(#13#10'ANSI编码:');
  AEncode:=qstring.teAnsi;
  AStream.Size:=0;
  SaveTextA(AStream,qstring.AnsiEncode(S));
  AStream.Position:=0;
  AJson.Clear;
  AJson.ParseBlock(AStream,AEncode);
  mmResult.Lines.Add('第一次解析结果:'#13#10);
  mmResult.Lines.Add(AJson.AsJson);
  AJson.Clear;
  AJson.ParseBlock(AStream,AEncode);
  mmResult.Lines.Add(#13#10'第二次解析结果:'#13#10);
  mmResult.Lines.Add(AJson.AsJson);
  AJson.Clear;
  AJson.ParseBlock(AStream,AEncode);
  mmResult.Lines.Add(#13#10'第三次解析结果:');
  mmResult.Lines.Add(AJson.AsJson);
  //UCS2BE
  mmResult.Lines.Add(#13#10'Unicode16BE编码:');
  AEncode:=qstring.teUnicode16BE;
  AStream.Size:=0;
  SaveTextWBE(AStream,S,False);
  AStream.Position:=0;
  AJson.Clear;
  AJson.ParseBlock(AStream,AEncode);
  mmResult.Lines.Add('第一次解析结果:'#13#10);
  mmResult.Lines.Add(AJson.AsJson);
  AJson.Clear;
  AJson.ParseBlock(AStream,AEncode);
  mmResult.Lines.Add(#13#10'第二次解析结果:'#13#10);
  mmResult.Lines.Add(AJson.AsJson);
  AJson.Clear;
  AJson.ParseBlock(AStream,AEncode);
  mmResult.Lines.Add(#13#10'第三次解析结果:');
  mmResult.Lines.Add(AJson.AsJson);
finally
  AStream.Free;
  AJson.Free;
end;
end;

procedure TForm1.Button8Click(Sender: TObject);
var
  AJson,AItem:TQJson;
begin
AJson:=TQJson.Create;
try
  //添加数组元素的N种方式演示
  // 1. 直接调用Add数组元素文本的方式
  AJson.Add('AddArrayText','["Item1",100,null,true,false,123.4]',jdtArray);//jdtArray如果省略会自动测试，如果明确知道，就不要让其判断增加开销
  // 2. 直接添加数组
  AJson.Add('AddArray',['Item1',100,Null,True,False,123.4]);
  // 3. 直接用VarArrayOf赋值
  AJson.Add('AsVariant').AsVariant:=VarArrayOf(['Item1',100,Null,True,False,123.4]);
  // 4. 直接用AsArray来赋给数组文件
  AJson.Add('AsArray').AsArray:='["Item1",100,null,true,false,123.4]';
  // 5. 手动逐个添加元素
  with AJson.Add('Manul') do
    begin
    DataType:=jdtArray;
    Add.AsString:='Item1';
    Add.AsInteger:=100;
    Add;
    Add.AsBoolean:=True;
    Add.AsBoolean:=False;
    Add.AsFloat:=123.4;
    end;
  // 添加对象数组和上面类型，只是子结点换成是对象就可以了
  AJson.Add('Object',[TQJson.Create.Add('Item1',100).Parent,TQJson.Create.Add('Item2',true).Parent]);
  mmResult.Lines.Add(AJson.AsJson);
finally
  FreeObject(AJson);
end;
end;

procedure TForm1.CreateTest;
var
  AJson:TQJson;
  lkJson:TlkJSONobject;
  lkItem:TlkJsonNumber;
  SuperObj:ISuperObject;
  YJson:JSONObject;
  FastJson:IJSONObject;
  TinyJson:TJSONobject;
  I:Integer;
  T1,T2,T3,T4,T5,T6:Cardinal;
begin
AJson:=TQJson.Create;
SuperObj:=TSuperObject.Create;
lkJson:=TlkJSONObject.Create;
YJson:=JsonObject.Create;
FastJson:=NewJSON;
TinyJson:=TJsonObject.Create();
try
  mmResult.Lines.Add('添加100,000个结点测试');
  T1:=GetTickCount;
  for I := 0 to 100000 do
    AJson.Add('_'+IntToStr(I),Now);
  T1:=GetTickCount-T1;
  T2:=GetTickCount;
  for I := 0 to 100000 do
    begin
    lkJson.Add('_'+IntToStr(I),TlkJsonNumber.Generate(Now));
    end;
  T2:=GetTickCount-T2;
  T3:=GetTickCount;
  for I := 0 to 100000 do
    SuperObj.D['_'+IntToStr(I)]:=Now;
  T3:=GetTickCount-T3;
  T4:=GetTickCount;
  for I := 0 to 100000 do
    YJson.put('_'+IntToStr(I),Now);
  T4:=GetTickCount-T4;
  T5:=GetTickCount;
  for I := 0 to 10000 do
    FastJson.AddChild('_'+IntToStr(I),FloatToStr(Now));
  T5:=GetTickCount-T5;
  T6:=GetTickCount;
  for I := 0 to 10000 do
    TinyJson.Add('_'+IntToStr(I),Now);
  T6:=GetTickCount-T6;
  mmResult.Lines.Add('  测试结果:QJson='+IntToStr(T1)+'ms,lkJson='+IntToStr(T2)+
    'ms,SuperObject='+IntToStr(T3)+'ms,YJson='+IntToStr(T4)+'ms,FastJson='+IntToStr(T5)+
    'ms,TinyJson='+IntToStr(T6)+'ms,'+
    'T1/T2='+FormatFloat('0.##',T1*1.0/T2)+',T1/T3='+
    FormatFloat('0.##',T1*1.0/T3)+',T1/T4='+FormatFloat('0.##',T1*1.0/T4)+',T1/T5='+FormatFloat('0.##',T1*1.0/T5)+
    ',T1/T6='+FormatFloat('0.##',T1*1.0/T6)
    );
finally
  AJson.Free;
  lkJson.Free;
  YJson.Free;
  TinyJson.Free;
end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
ReportMemoryLeaksOnShutdown:=True;
end;

procedure TForm1.IOTest;
begin

end;

procedure TForm1.LoadTest;
var
  I:Integer;
  auk:TlkJSONbase;
  AJson:TQJson;
  SuperObj:ISuperObject;
  YJson:JsonObject;
  FastJson:IJsonObject;
  TinyJson:TJsonArray;
  T1,T2,T3,T4,T5,T6:Cardinal;
const
  AFileName:String='Preferences.txt';
  procedure PreCache;
  var
    AStream:TMemoryStream;
  begin
  AStream:=TMemoryStream.Create;
  try
    AStream.LoadFromFile(AFileName);
  finally
    AStream.Free;
  end;
  end;
  procedure LoadTinyJson;
  var
    S:AnsiString;
  begin
  S:=LoadTextA(AFileName);
  TinyJson:=ParseJson(PAnsiChar(S));
  end;
begin
PreCache;
mmResult.Lines.Add('测试加载文件速度');
AJson:=TQJson.Create;
T1:=GetTickCount;
for I := 0 to 10 do
 AJson.LoadFromFile(AFileName);
T1:=GetTickCount-T1;
AJson.Free;
T2:=GetTickCount;
for I := 0 to 10 do
  begin
  auk:=TlkJSONstreamed.LoadFromFile(AFileName);
  auk.Free;
  end;
T2:=GetTickCount-T2;
T3:=GetTickCount;
for I := 0 to 10 do
  SuperObj:=TSuperObject.ParseFile(AFileName,false);
T3:=GetTickCount-T3;
YJson:=JsonObject.Create;
T4:=GetTickCount;
for I := 0 to 10 do
  YJson.LoadFromFile(AFileName);
T4:=GetTickCount-T4;
YJson.Free;
T5:=GetTickCount;
FastJson:=LoadJSON(AFileName);
T5:=GetTickCount-T5;
TinyJson:=TJsonObject.Create;
T6:=GetTickCount;
LoadTinyJson;
T6:=GetTickCount-T6;
TinyJson.Free;
mmResult.Lines.Add('  测试结果:QJson='+IntToStr(T1)+'ms,lkJson='+IntToStr(T2)+
    'ms,SuperObject='+IntToStr(T3)+'ms,YJson='+IntToStr(T4)+'ms,FastJson='+IntToStr(T5)+
    'ms,TinyJson='+IntToStr(T6)+'ms,'+
    'T1/T2='+FormatFloat('0.##',T1*1.0/T2)+',T1/T3='+
    FormatFloat('0.##',T1*1.0/T3)+',T1/T4='+FormatFloat('0.##',T1*1.0/T4)+
    ',T1/T5='+FormatFloat('0.##',T1*1.0/T5)+',T1/T6='+FormatFloat('0.##',T1*1.0/T6)
    );
end;

procedure TForm1.SaveTest;
var
  I:Integer;
  auk:TlkJSONbase;
  AJson:TQJson;
  YJson:JsonObject;
  SuperObj:ISuperObject;
  T1,T2,T3,T4:Cardinal;
const
  AFileName:String='Preferences.txt';
  ASaveFileName:String='saved.json';
  ACount:Integer=1;
begin
AJson:=TQJson.Create;
AJson.LoadFromFile(AFileName);
mmResult.Lines.Add('测试保存文件速度');
T1:=GetTickCount;
for I := 0 to ACount do
 AJson.SaveToFile(ASaveFileName,qstring.teUtf8,true);
T1:=GetTickCount-T1;
AJson.Free;
auk:=TlkJSONstreamed.LoadFromFile(AFileName);
T2:=GetTickCount;
for I := 0 to ACount do
  begin
  TlkJSONStreamed.SaveToFile(auk,ASaveFileName);
  end;
T2:=GetTickCount-T2;
auk.Free;
SuperObj:=TSuperObject.ParseFile(AFileName,false);
T3:=GetTickCount;
for I := 0 to ACount do
  SuperObj.SaveTo(ASaveFileName);
//  SuperObj:=TSuperObject.ParseFile(AFileName,false);
T3:=GetTickCount-T3;
YJson:=JsonObject.Create;
YJson.LoadFromFile(AFileName);
T4:=GetTickCount;
for I := 0 to ACount do
  YJson.SaveToFile(ASaveFileName,YxdJson.teUTF8,true);
T4:=GetTickCount-T4;
YJson.Free;
mmResult.Lines.Add('  测试结果:QJson='+IntToStr(T1)+'ms,lkJson='+IntToStr(T2)+
    'ms,SuperObject='+IntToStr(T3)+'ms,YJson='+IntToStr(T4)+'ms,T1/T2='+FormatFloat('0.##',T1*1.0/T2)+',T1/T3='+
    FormatFloat('0.##',T1*1.0/T3)+',T1/T4='+FormatFloat('0.##',T1*1.0/T4)
    );
end;

procedure TForm1.TypeTest;
var
  auk:TlkJSONbase;
  AJson:TQJson;
  SuperObj:ISuperObject;
  YJson:JsonObject;
  S:WideString;
  AStream:TMemoryStream;
  T1,T2,T3,T4:Cardinal;
  Ansi:AnsiString;
  procedure TestLongString;
  var
    I:Integer;
  begin
  SetLength(S,10*1024*1024);
  for I := 0 to Length(S)-1 do
    PWideChar(S)[I]:=WideChar(Ord('a')+random(26));
  AJson.Clear;
  AJson.Add('LongString',S,qjson.jdtString);
  S:=AJson.AsJson;
  T1:=GetTickCount;
  AJson.Parse(S);
  T1:=GetTickCount-T1;
  Ansi:=S;
  T2:=GetTickCount;
  auk:=TlkJson.ParseText(Ansi);
  T2:=GetTickCount-T2;
  auk.Free;
  T3:=GetTickCount;
  SuperObj:=TSuperObject.ParseString(PWideChar(S),False);
  T3:=GetTickCount-T3;
  T4:=GetTickCount;
  YJson.parse(S);
  T4:=GetTickCount-T4;
  mmResult.Lines.Add('  长字符串:QJson='+IntToStr(T1)+'ms,lkJson='+IntToStr(T2)+
    'ms,SuperObject='+IntToStr(T3)+'ms,YJson='+IntToStr(T4)+'ms,YJson='+IntToStr(T4)+', T1/T2='+FormatFloat('0.##',T1*1.0/T2)+',T1/T3='+
    FormatFloat('0.##',T1*1.0/T3)+',T1/T4='+FormatFloat('0.##',T1*1.0/T4)
    );
  end;
  function RandomString(ALen:Integer):QStringW;
  var
    I:Integer;
    p:PQCharW;
  begin
  SetLength(Result,ALen);
  p:=PWideChar(Result);
  for I := 0 to ALen-1 do
    p[I]:=WideChar(Ord('A')+56);
  end;
  procedure TestString;
  var
    I:Integer;
  begin
  for I := 0 to 100000 do
    AJson.Add('_'+IntToStr(I),RandomString(20),qjson.jdtString);
  S:=AJson.AsJson;
  T1:=GetTickCount;
  AJson.Parse(S);
  T1:=GetTickCount-T1;
  Ansi:=S;
  T2:=GetTickCount;
  auk:=TlkJson.ParseText(Ansi);
  T2:=GetTickCount-T2;
  auk.Free;
  T3:=GetTickCount;
  SuperObj:=TSuperObject.ParseString(PWideChar(S),False);
  T3:=GetTickCount-T3;
  T4:=GetTickCount;
  YJson.parse(S);
  T4:=GetTickCount-T4;
  mmResult.Lines.Add('  字符串:QJson='+IntToStr(T1)+'ms,lkJson='+IntToStr(T2)+
    'ms,SuperObject='+IntToStr(T3)+'ms,YJson='+IntToStr(T4)+'ms,T1/T2='+FormatFloat('0.##',T1*1.0/T2)+',T1/T3='+
    FormatFloat('0.##',T1*1.0/T3)+',T1/T4='+FormatFloat('0.##',T1*1.0/T4)
    );
  end;
  procedure TestInteger;
  var
    I:Integer;
  begin
  AJson.Clear;
  for I := 0 to 100000 do
    AJson.Add('_'+IntToStr(I),I);
  S:=AJson.AsJson;
  T1:=GetTickCount;
  AJson.Parse(S);
  T1:=GetTickCount-T1;
  Ansi:=S;
  T2:=GetTickCount;
  auk:=TlkJson.ParseText(Ansi);
  T2:=GetTickCount-T2;
  auk.Free;
  T3:=GetTickCount;
  SuperObj:=TSuperObject.ParseString(PWideChar(S),False);
  T3:=GetTickCount-T3;
  T4:=GetTickCount;
  YJson.parse(S);
  T4:=GetTickCount-T4;
  mmResult.Lines.Add('  整数:QJson='+IntToStr(T1)+'ms,lkJson='+IntToStr(T2)+
    'ms,SuperObject='+IntToStr(T3)+'ms,YJson='+IntToStr(T4)+'ms,T1/T2='+FormatFloat('0.##',T1*1.0/T2)+',T1/T3='+
    FormatFloat('0.##',T1*1.0/T3)+',T1/T4='+FormatFloat('0.##',T1*1.0/T4)
    );
  end;

  procedure TestNumeric;
  var
    I:Integer;
  begin
  AJson.Clear;
  for I := 0 to 100000 do
    AJson.Add('_'+IntToStr(I),I+random(10000)*0.0001);
  S:=AJson.AsJson;
  T1:=GetTickCount;
  AJson.Parse(S);
  T1:=GetTickCount-T1;
  Ansi:=S;
  T2:=GetTickCount;
  auk:=TlkJson.ParseText(Ansi);
  T2:=GetTickCount-T2;
  auk.Free;
  T3:=GetTickCount;
  SuperObj:=TSuperObject.ParseString(PWideChar(S),False);
  T3:=GetTickCount-T3;
  T4:=GetTickCount;
  YJson.parse(S);
  T4:=GetTickCount-T4;
  mmResult.Lines.Add('  浮点:QJson='+IntToStr(T1)+'ms,lkJson='+IntToStr(T2)+
    'ms,SuperObject='+IntToStr(T3)+'ms,YJson='+IntToStr(T4)+'ms,T1/T2='+FormatFloat('0.##',T1*1.0/T2)+',T1/T3='+
    FormatFloat('0.##',T1*1.0/T3)+',T1/T4='+FormatFloat('0.##',T1*1.0/T4)
    );
  end;

  procedure TestBoolean;
  var
    I:Integer;
  begin
  AJson.Clear;
  for I := 0 to 100000 do
    AJson.Add('_'+IntToStr(I),random(10)>5);
  S:=AJson.AsJson;
  T1:=GetTickCount;
  AJson.Parse(S);
  T1:=GetTickCount-T1;
  Ansi:=S;
  T2:=GetTickCount;
  auk:=TlkJson.ParseText(Ansi);
  T2:=GetTickCount-T2;
  auk.Free;
  T3:=GetTickCount;
  SuperObj:=TSuperObject.ParseString(PWideChar(S),False);
  T3:=GetTickCount-T3;
  T4:=GetTickCount;
  YJson.parse(S);
  T4:=GetTickCount-T4;
  mmResult.Lines.Add('  布尔:QJson='+IntToStr(T1)+'ms,lkJson='+IntToStr(T2)+
    'ms,SuperObject='+IntToStr(T3)+'ms,YJson='+IntToStr(T4)+'ms,T1/T2='+FormatFloat('0.##',T1*1.0/T2)+',T1/T3='+
    FormatFloat('0.##',T1*1.0/T3)+',T1/T4='+FormatFloat('0.##',T1*1.0/T4)
    );
  end;

  procedure TestNull;
  var
    I:Integer;
  begin
  AJson.Clear;
  for I := 0 to 100000 do
    AJson.Add('_'+IntToStr(I));
  S:=AJson.AsJson;
  T1:=GetTickCount;
  AJson.Parse(S);
  T1:=GetTickCount-T1;
  Ansi:=S;
  T2:=GetTickCount;
  auk:=TlkJson.ParseText(Ansi);
  T2:=GetTickCount-T2;
  auk.Free;
  T3:=GetTickCount;
  SuperObj:=TSuperObject.ParseString(PWideChar(S),False);
  T3:=GetTickCount-T3;
  T4:=GetTickCount;
  YJson.parse(S);
  T4:=GetTickCount-T4;
  mmResult.Lines.Add('  空值:QJson='+IntToStr(T1)+'ms,lkJson='+IntToStr(T2)+
    'ms,SuperObject='+IntToStr(T3)+'ms,YJson='+IntToStr(T4)+'ms,T1/T2='+FormatFloat('0.##',T1*1.0/T2)+',T1/T3='+
    FormatFloat('0.##',T1*1.0/T3)+',T1/T4='+FormatFloat('0.##',T1*1.0/T4)
    );
  end;

  procedure TestObject;
  var
    I:Integer;
  begin
  AJson.Clear;
  for I := 0 to 100000 do
    AJson.Add('_'+IntToStr(I),'{"name":"object_'+IntToStr(I)+'"}');
  S:=AJson.AsJson;
  T1:=GetTickCount;
  AJson.Parse(S);
  T1:=GetTickCount-T1;
  Ansi:=S;
  T2:=GetTickCount;
  auk:=TlkJson.ParseText(Ansi);
  T2:=GetTickCount-T2;
  auk.Free;
  T3:=GetTickCount;
  SuperObj:=TSuperObject.ParseString(PWideChar(S),False);
  T3:=GetTickCount-T3;
  T4:=GetTickCount;
  YJson.parse(S);
  T4:=GetTickCount-T4;
  mmResult.Lines.Add('  对象:QJson='+IntToStr(T1)+'ms,lkJson='+IntToStr(T2)+
    'ms,SuperObject='+IntToStr(T3)+'ms,YJson='+IntToStr(T4)+'ms,T1/T2='+FormatFloat('0.##',T1*1.0/T2)+',T1/T3='+
    FormatFloat('0.##',T1*1.0/T3)+',T1/T4='+FormatFloat('0.##',T1*1.0/T4)
    );
  end;

  procedure TestArray;
  var
    I:Integer;
  begin
  AJson.Clear;
  for I := 0 to 100000 do
    AJson.Add('_'+IntToStr(I),'["name",'+IntToStr(I)+']');
  S:=AJson.AsJson;
  T1:=GetTickCount;
  AJson.Parse(S);
  T1:=GetTickCount-T1;
  Ansi:=S;
  T2:=GetTickCount;
  auk:=TlkJson.ParseText(Ansi);
  T2:=GetTickCount-T2;
  auk.Free;
  T3:=GetTickCount;
  SuperObj:=TSuperObject.ParseString(PWideChar(S),False);
  T3:=GetTickCount-T3;
  T4:=GetTickCount;
  YJson.parse(S);
  T4:=GetTickCount-T4;
  mmResult.Lines.Add('  数组:QJson='+IntToStr(T1)+'ms,lkJson='+IntToStr(T2)+
    'ms,SuperObject='+IntToStr(T3)+'ms,YJson='+IntToStr(T4)+'ms,T1/T2='+FormatFloat('0.##',T1*1.0/T2)+',T1/T3='+
    FormatFloat('0.##',T1*1.0/T3)+',T1/T4='+FormatFloat('0.##',T1*1.0/T4)
    );
  end;

begin
mmResult.Lines.Add('测试不同类型数据解析速度');
AJson:=TQJson.Create;
AJson.DataType:=qjson.jdtObject;
YJson:=JsonObject.Create;
TestLongString;
TestString;
TestInteger;
TestNumeric;
TestBoolean;
TestNull;
TestObject;
TestArray;
AJson.Free;
YJson.Free;
end;

end.
