unit osxmain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Memo, Xml.xmldom, Xml.XMLIntf, Xml.adomxmldom, Xml.XMLDoc,
  System.Diagnostics;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    chkLoad: TCheckBox;
    chkSave: TCheckBox;
    chkIO: TCheckBox;
    chkCreate: TCheckBox;
    btnGo: TButton;
    mmResult: TMemo;
    lblItem: TLabel;
    procedure btnGoClick(Sender: TObject);
  private
    { Private declarations }
    procedure LoadTest;
    procedure SaveTest;
    procedure IOTest;
    procedure CreateTest;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses qxml,nativexml,oxmlcdom;
{$R *.fmx}

procedure TForm1.btnGoClick(Sender: TObject);
begin
if chkLoad.IsChecked then
  begin
  lblItem.Text:='测试加载XML文件速度……';
  Application.ProcessMessages;
  LoadTest;
  end;
if chkSave.IsChecked then
  begin
  lblItem.Text:='测试保存XML文件速度……';
  Application.ProcessMessages;
  SaveTest;
  end;
if chkIO.IsChecked then
  begin
  lblItem.Text:='测试加载+保存速度……';
  Application.ProcessMessages;
  IOTest;
  end;
if chkCreate.IsChecked then
  begin
  lblItem.Text:='测试创建结点速度……';
  Application.ProcessMessages;
  CreateTest;
  end;
lblItem.Text:='';
end;

procedure TForm1.CreateTest;
const
  ACount:Integer=100000;
var
  I:Integer;
  AQXML:TQXMLNode;
  ANXML:TNativeXML;
  AMSXML:xml.XMLDoc.TXMLDocument;
  AOXML:oxmlcdom.TXMLDocument;
  T0,T1,T2,T3,T4,T5:Integer;
  AMSNode:IXMLNode;
  AWatch:TStopWatch;
begin

AQXML:=TQXMLNode.Create;
ANXML:=TNativeXML.Create(nil);
AMSXML:=xml.XMLDoc.TXMLDocument.Create(nil);
AOXML:=OXMLCDom.TXMLDocument.Create(nil);
try
  mmResult.Lines.Add(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',Now)+' 测试添加'+IntToStr(ACount)+'个子结点');
  AMSXML.DOMVendor:=OpenXML4Factory;
  AMSXML.Active:=True;
  AWatch.StartNew;
  T0:=AWatch.GetTimeStamp;
  for I := 0 to ACount-1 do
    AQXML.Add('_'+IntToStr(I));
  T1:=AWatch.GetTimeStamp-T0;
  AWatch.StartNew;
  T0:=AWatch.GetTimeStamp;
  for I := 0 to ACount-1 do
    ANXML.NodeNew('_'+IntToStr(I));
  T2:=AWatch.GetTimeStamp-T0;
  T3:=-1;
  AWatch.StartNew;
  T0:=AWatch.GetTimeStamp;
  AMSNode:=AMSXML.CreateNode('Root');
  for I := 0 to ACount-2 do
    AMsNode.AddChild('_'+IntToStr(I));
  T4:=AWatch.GetTimeStamp-T0;
  AWatch.StartNew;
  T0:=AWatch.GetTimeStamp;
  for I := 0 to ACount-1 do
    AOXML.AddChild('_'+IntToStr(I));
  T5:=AWatch.GetTimeStamp-T0;
  mmResult.Lines.Add('  测试结果：'#13#10'  QXML='+IntToStr(T1 div 1000)+'ms,NativeXML='+IntToStr(T2 div 1000)+
    'ms,ALXML='+IntToStr(T3 div 1000)+'ms,OpenXML='+IntToStr(T4 div 1000)+'ms,OXML='+IntToStr(T5 div 1000)+'ms');
finally
  AQXML.Free;
  ANXML.Free;
  AMSXML.Free;
  AOXML.Free;
end;
end;

procedure TForm1.IOTest;
var
  AQXML:TQXMLNode;
  ANXML:TNativeXML;
  AMSXML:XML.XMLDoc.TXMLDocument;
  AOXML:oxmlcdom.TXMLDocument;
  I:Integer;
  T0,T1,T2,T3,T4,T5:Integer;
  AWatch:TStopWatch;
const
  ACount:Integer=10;
  AFileName:String='test.xml';
  ASaveFileName:String='testsave.xml';
begin
AQXML:=TQXMLNode.Create;
ANXML:=TNativeXML.Create(nil);
AMSXML:=XML.XMLDoc.TXMLDocument.Create(nil);
AOXML:=OXMLCDom.TXMLDocument.Create(nil);
try
  mmResult.Lines.Add(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',Now)+'-加载并保存文件10次...');
  AMSXML.DOMVendor:=OpenXML4Factory;
  AWatch.StartNew;
  T0:=AWatch.GetTimeStamp;
  for I := 0 to ACount-1 do
    begin
    AQXML.LoadFromFile(AFileName);
    AQXML.SaveToFile(ASaveFileName);
    end;
  T1:=AWatch.GetTimeStamp-T0;
  AWatch.StartNew;
  T0:=AWatch.GetTimeStamp;
  for I := 0 to ACount-1 do
    begin
    ANXML.LoadFromFile(AFileName);
    ANXML.SaveToFile(ASaveFileName);
    end;
  T2:=AWatch.GetTimeStamp-T0;
  AWatch.StartNew;
  T0:=AWatch.GetTimeStamp;
  T3:=-1;
  AWatch.StartNew;
  T0:=AWatch.GetTimeStamp;
  for I := 0 to ACount - 1 do
    begin
    AMSXML.LoadFromFile(AFileName);
    AMSXML.SaveToFile(ASaveFileName);
    end;
  T4:=AWatch.GetTimeStamp-T0;
  AWatch.StartNew;
  T0:=AWatch.GetTimeStamp;
  for I := 0 to ACount - 1 do
    begin
    AOXML.LoadFromFile(AFileName);
    AOXML.SaveToFile(ASaveFileName);
    end;
  T5:=AWatch.GetTimeStamp-T0;
  mmResult.Lines.Add('  测试结果'#13#10'  QXML='+IntToStr(T1 div 1000)+'ms,NativeXML='+IntToStr(T2 div 1000)+
    'ms,ALXML='+IntToStr(T3 div 1000)+'ms,OpenXML='+IntToStr(T4 div 1000)+'ms,OXML='+IntToStr(T5 div 1000)+'ms');
finally
  AQXML.Free;
  ANXML.Free;
  AMSXML.Free;
  AOXML.Free;
end;
end;

procedure TForm1.LoadTest;
var
  AQXML:TQXMLNode;
  ANXML:TNativeXML;
  AMSXML:Xml.XMLDoc.TXMLDocument;
  AOXML:oxmlcdom.TXMLDocument;
  I:Integer;
  T0,T1,T2,T3,T4,T5:Integer;
  AWatch:TStopWatch;
const
  ACount:Integer=10;
  AFileName:String='test.xml';
  procedure Precache;
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
begin
AQXML:=TQXMLNode.Create;
ANXML:=TNativeXML.Create(nil);
AMSXML:=Xml.XMLDoc.TXMLDocument.Create(nil);
AOXML:=OXMLCDom.TXMLDocument.Create(nil);
try
  mmResult.Lines.Add(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',Now)+'-重复加载文件 ['+AFileName+'] '+IntToStr(ACount)+' 次');
  Precache;
  AMSXML.DOMVendor:=OpenXML4Factory;
  AWatch.StartNew;
  T0:=AWatch.GetTimeStamp;
  for I := 0 to ACount-1 do
    begin
    AQXML.LoadFromFile(AFileName);
    end;
  T1:=AWatch.GetTimeStamp-T0;
  AWatch.StartNew;
  T0:=AWatch.GetTimeStamp;
  for I := 0 to ACount-1 do
    begin
    ANXML.LoadFromFile(AFileName);
    end;
  T2:=AWatch.GetTimeStamp-T0;
  AWatch.StartNew;
  T0:=AWatch.GetTimeStamp;
  T3:=-1;
  AWatch.StartNew;
  T0:=AWatch.GetTimeStamp;
  for I := 0 to ACount - 1 do
    begin
    AMSXML.LoadFromFile(AFileName);
    end;
  T4:=AWatch.GetTimeStamp-T0;
  AWatch.StartNew;
  T0:=AWatch.GetTimeStamp;
  for I := 0 to ACount - 1 do
    begin
    AOXML.LoadFromFile(AFileName);
    end;
  T5:=AWatch.GetTimeStamp-T0;
  mmResult.Lines.Add('  测试结果:'#13#10'  QXML='+IntToStr(T1 div 1000)+'ms,NativeXML='+IntToStr(T2  div 1000)+
    'ms,ALXML='+IntToStr(T3 div 1000)+'ms,OpenXML='+IntToStr(T4 div 1000)+'ms,OXML='+IntToStr(T5 div 1000)+'ms');
finally
  AQXML.Free;
  ANXML.Free;
  AMSXML.Free;
  AOXML.Free;
end;
end;

procedure TForm1.SaveTest;
var
  AQXML:TQXMLNode;
  ANXML:TNativeXML;
  AMSXML:Xml.XMLDoc.TXMLDocument;
  AOXML:oxmlcdom.TXMLDocument;
  I:Integer;
  T0,T1,T2,T3,T4,T5:Integer;
  AWatch:TStopWatch;
const
  ACount:Integer=10;
  AFileName:String='test.xml';
  ASaveFileName:String='testsave.xml';
begin
AQXML:=TQXMLNode.Create;
ANXML:=TNativeXML.Create(nil);
AMSXML:=Xml.XMLDoc.TXMLDocument.Create(nil);
AOXML:=oxmlcdom.TXMLDocument.Create(nil);
try
  mmResult.Lines.Add(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',Now)+'-重复保存文件 ['+AFileName+'] '+IntToStr(ACount)+' 次');
  AMSXML.DOMVendor:=OpenXML4Factory;
  AQXML.LoadFromFile(AFileName);
  AWatch.StartNew;
  T0:=AWatch.GetTimeStamp;
  for I := 0 to ACount-1 do
    begin
    AQXML.SaveToFile(ASaveFileName);
    end;
  T1:=AWatch.GetTimeStamp-T0;
  ANXML.LoadFromFile(AFileName);
  AWatch.StartNew;
  T0:=AWatch.GetTimeStamp;
  for I := 0 to ACount-1 do
    begin
    ANXML.SaveToFile(ASaveFileName);
    end;
  T2:=AWatch.GetTimeStamp-T0;
  T3:=-1;
  AMSXML.LoadFromFile(AFileName);
  AWatch.StartNew;
  T0:=AWatch.GetTimeStamp;
  for I := 0 to ACount - 1 do
    begin
    AMSXML.SaveToFile(ASaveFileName);
    end;
  T4:=AWatch.GetTimeStamp-T0;
  AOXML.LoadFromFile(AFileName);
  AWatch.StartNew;
  T0:=AWatch.GetTimeStamp;
  for I := 0 to ACount - 1 do
    begin
    AOXML.SaveToFile(ASaveFileName);
    end;
  T5:=AWatch.GetTimeStamp-T0;
  mmResult.Lines.Add('  测试结果:'#13#10'  QXML='+IntToStr(T1 div 1000)+'ms,NativeXML='+IntToStr(T2 div 1000)+
    'ms,ALXML='+IntToStr(T3 div 1000)+'ms,MSXML='+IntToStr(T4 div 1000)+'ms,OXML='+IntToStr(T5 div 1000)+'ms');
finally
  AQXML.Free;
  ANXML.Free;
  AMSXML.Free;
  AOXML.Free;
end;
end;

end.
