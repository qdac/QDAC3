unit testmain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls,qstring, qxml,StdCtrls, xmldom, XMLIntf, msxmldom, XMLDoc,
  ComCtrls;
{$M+}
type
  TfrmMain = class(TForm)
    Panel1: TPanel;
    mmResult: TMemo;
    lblItem: TLabel;
    chkLoad: TCheckBox;
    chkSave: TCheckBox;
    chkIO: TCheckBox;
    chkCreate: TCheckBox;
    Panel2: TPanel;
    Button5: TButton;
    procedure Button5Click(Sender: TObject);
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
  frmMain: TfrmMain;

implementation
uses nativexml,ALXmlDoc,oxmlcdom;
{$R *.dfm}

procedure TfrmMain.Button5Click(Sender: TObject);
begin
if chkLoad.Checked then
  begin
  lblItem.Caption:='测试加载XML文件速度……';
  Application.ProcessMessages;
  LoadTest;
  end;
if chkSave.Checked then
  begin
  lblItem.Caption:='测试保存XML文件速度……';
  Application.ProcessMessages;
  SaveTest;
  end;
if chkIO.Checked then
  begin
  lblItem.Caption:='测试加载+保存速度……';
  Application.ProcessMessages;
  IOTest;
  end;
if chkCreate.Checked then
  begin
  lblItem.Caption:='测试创建结点速度……';
  Application.ProcessMessages;
  CreateTest;
  end;
lblItem.Caption:='';
end;

procedure TfrmMain.CreateTest;
const
  ACount:Integer=100000;
var
  I:Integer;
  AQXML:TQXMLNode;
  ANXML:TNativeXML;
  AALXML:TALXMLDocument;
  AMSXML:XMLDoc.TXMLDocument;
  AOXML:oxmlcdom.TXMLDocument;
  T0,T1,T2,T3,T4,T5:Integer;
  AALNode:TALXMLNode;
  AMSNode:IXMLNode;
begin
AQXML:=TQXMLNode.Create;
ANXML:=TNativeXML.Create(nil);
AALXML:=TALXMLDocument.Create;
AMSXML:=XmlDoc.TXMLDocument.Create(nil);
AOXML:=OXMLCDom.TXMLDocument.Create(nil);
try
  mmResult.Lines.Add(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',Now)+' 测试添加'+IntToStr(ACount)+'个子结点');
  AALXML.Active:=True;
  AMSXML.Active:=True;
  T1:=GetTickCount;
  for I := 0 to ACount-1 do
    AQXML.Add('_'+IntToStr(I));
  T1:=GetTickCount-T1;
  T2:=GetTickCount;
  for I := 0 to ACount-1 do
    ANXML.NodeNew('_'+IntToStr(I));
  T2:=GetTickCount-T2;
  T3:=GetTickCount;
  AALNode:=AALXML.AddChild('Root');
  for I := 0 to ACount - 2 do
    AALNode.AddChild('_'+IntToStr(I));
  T3:=GetTickCount-T3;
  T4:=GetTickCount;
//  AMSNode:=AMSXML.CreateNode('Root');
//  for I := 0 to ACount-2 do
//    AMsNode.AddChild('_'+IntToStr(I));
//  T4:=AWatch.GetTimeStamp-T0;
//  T4:=GetTickCount-T4;
//  T5:=GetTickCount;
  T4:=-1;
  T5:=GetTickCount;
  for I := 0 to ACount-1 do
    AOXML.AddChild('_'+IntToStr(I));
  T5:=GetTickCount-T5;
  mmResult.Lines.Add('  测试结果：'#13#10'  QXML='+IntToStr(T1)+'ms,NativeXML='+IntToStr(T2)+
    'ms,ALXML='+IntToStr(T3)+'ms,MSXML='+IntToStr(T4)+'ms,OXML='+IntToStr(T5)+'ms,T1/T2='+
    FormatFloat('0.##',T1*1.0/T2)+',T1/T3='+FormatFloat('0.##',T1*1.0/T3)+',T1/T4='+
    FormatFloat('0.##',T1*1.0/T4)+',T1/T5='+FormatFloat('0.##',T1*1.0/T5));
finally
  AQXML.Free;
  AALXML.Free;
  ANXML.Free;
  AMSXML.Free;
  AOXML.Free;
end;
end;

procedure TfrmMain.IOTest;
var
  AQXML:TQXMLNode;
  ANXML:TNativeXML;
  AALXML:TALXMLDocument;
  AMSXML:XMLDoc.TXMLDocument;
  AOXML:oxmlcdom.TXMLDocument;
  I:Integer;
  T0,T1,T2,T3,T4,T5:Integer;
  AALNode:TALXMLNode;
const
  ACount:Integer=10;
  AFileName:String='test.xml';
  ASaveFileName:String='testsave.xml';
begin
AQXML:=TQXMLNode.Create;
ANXML:=TNativeXML.Create(nil);
AALXML:=TALXMLDocument.Create;
AMSXML:=XMLDoc.TXMLDocument.Create(nil);
AOXML:=OXMLCDom.TXMLDocument.Create(nil);
try
  mmResult.Lines.Add(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',Now)+'-加载并保存文件10次...');
  T1:=GetTickCount;
  for I := 0 to ACount-1 do
    begin
    AQXML.LoadFromFile(AFileName);
    AQXML.SaveToFile(ASaveFileName);
    end;
  T1:=GetTickCount-T1;
  T2:=GetTickCount;
  for I := 0 to ACount-1 do
    begin
    ANXML.LoadFromFile(AFileName);
    ANXML.SaveToFile(ASaveFileName);
    end;
  T2:=GetTickCount-T2;
  T3:=GetTickCount;
  for I := 0 to ACount - 1 do
    begin
    AALXML.LoadFromFile(AFileName);
    AALXML.SaveToFile(ASaveFileName);
    end;
  T3:=GetTickCount-T3;
  T4:=GetTickCount;
  for I := 0 to ACount - 1 do
    begin
    AMSXML.LoadFromFile(AFileName);
    AMSXML.SaveToFile(ASaveFileName);
    end;
  T4:=GetTickCount-T4;
  T5:=GetTickCount;
  for I := 0 to ACount - 1 do
    begin
    AOXML.LoadFromFile(AFileName);
    AOXML.SaveToFile(ASaveFileName);
    end;
  T5:=GetTickCount-T5;
  mmResult.Lines.Add('  测试结果'#13#10'  QXML='+IntToStr(T1)+'ms,NativeXML='+IntToStr(T2)+
    'ms,ALXML='+IntToStr(T3)+'ms,MSXML='+IntToStr(T4)+'ms,OXML='+IntToStr(T5)+'ms,T1/T2='+
    FormatFloat('0.##',T1*1.0/T2)+',T1/T3='+FormatFloat('0.##',T1*1.0/T3)+',T1/T4='+
    FormatFloat('0.##',T1*1.0/T4)+',T1/T5='+FormatFloat('0.##',T1*1.0/T5));
finally
  AQXML.Free;
  AALXML.Free;
  ANXML.Free;
  AMSXML.Free;
  AOXML.Free;
end;
end;

procedure TfrmMain.LoadTest;
var
  AQXML:TQXMLNode;
  ANXML:TNativeXML;
  AALXML:TALXMLDocument;
  AMSXML:XMLDoc.TXMLDocument;
  AOXML:oxmlcdom.TXMLDocument;
  I:Integer;
  T0,T1,T2,T3,T4,T5:Integer;
  AALNode:TALXMLNode;
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
AALXML:=TALXMLDocument.Create;
AMSXML:=XMLDoc.TXMLDocument.Create(nil);
AOXML:=OXMLCDom.TXMLDocument.Create(nil);
try
  mmResult.Lines.Add(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',Now)+'-重复加载文件 ['+AFileName+'] '+IntToStr(ACount)+' 次');
  Precache;
  T1:=GetTickCount;
  for I := 0 to ACount-1 do
    begin
    AQXML.LoadFromFile(AFileName);
    end;
  T1:=GetTickCount-T1;
  T2:=GetTickCount;
  for I := 0 to ACount-1 do
    begin
    ANXML.LoadFromFile(AFileName);
    end;
  T2:=GetTickCount-T2;
  T3:=GetTickCount;
  for I := 0 to ACount - 1 do
    begin
    AALXML.LoadFromFile(AFileName);
    end;
  T3:=GetTickCount-T3;
  T4:=GetTickCount;
  for I := 0 to ACount - 1 do
    begin
    AMSXML.LoadFromFile(AFileName);
    end;
  T4:=GetTickCount-T4;
  T5:=GetTickCount;
  for I := 0 to ACount - 1 do
    begin
    AOXML.LoadFromFile(AFileName);
    end;
  T5:=GetTickCount-T5;
  mmResult.Lines.Add('  测试结果:'#13#10'  QXML='+IntToStr(T1)+'ms,NativeXML='+IntToStr(T2)+
    'ms,ALXML='+IntToStr(T3)+'ms,MSXML='+IntToStr(T4)+'ms,OXML='+IntToStr(T5)+'ms,T1/T2='+
    FormatFloat('0.##',T1*1.0/T2)+',T1/T3='+FormatFloat('0.##',T1*1.0/T3)+',T1/T4='+
    FormatFloat('0.##',T1*1.0/T4)+',T1/T5='+FormatFloat('0.##',T1*1.0/T5));
finally
  AQXML.Free;
  AALXML.Free;
  ANXML.Free;
  AMSXML.Free;
  AOXML.Free;
end;
end;

procedure TfrmMain.SaveTest;
var
  AQXML:TQXMLNode;
  ANXML:TNativeXML;
  AALXML:TALXMLDocument;
  AMSXML:XMLDoc.TXMLDocument;
  AOXML:oxmlcdom.TXMLDocument;
  I:Integer;
  T0,T1,T2,T3,T4,T5:Integer;
  AALNode:TALXMLNode;
const
  ACount:Integer=10;
  AFileName:String='test.xml';
  ASaveFileName:String='testsave.xml';
begin
AQXML:=TQXMLNode.Create;
ANXML:=TNativeXML.Create(nil);
AALXML:=TALXMLDocument.Create;
AMSXML:=XMLDoc.TXMLDocument.Create(nil);
AOXML:=oxmlcdom.TXMLDocument.Create(nil);
try
  mmResult.Lines.Add(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',Now)+'-重复保存文件 ['+AFileName+'] '+IntToStr(ACount)+' 次');
  AQXML.LoadFromFile(AFileName);
  T1:=GetTickCount;
  for I := 0 to ACount-1 do
    begin
    AQXML.SaveToFile(ASaveFileName);
    end;
  T1:=GetTickCount-T1;
  ANXML.LoadFromFile(AFileName);
  T2:=GetTickCount;
  for I := 0 to ACount-1 do
    begin
    ANXML.SaveToFile(ASaveFileName);
    end;
  T2:=GetTickCount-T2;
  AALXML.LoadFromFile(AFileName);
  T3:=GetTickCount;
  for I := 0 to ACount - 1 do
    begin
    AALXML.SaveToFile(ASaveFileName);
    end;
  T3:=GetTickCount-T3;
  AMSXML.LoadFromFile(AFileName);
  T4:=GetTickCount;
  for I := 0 to ACount - 1 do
    begin
    AMSXML.SaveToFile(ASaveFileName);
    end;
  T4:=GetTickCount-T4;
  AOXML.LoadFromFile(AFileName);
  T5:=GetTickCount;
  for I := 0 to ACount - 1 do
    begin
    AOXML.SaveToFile(ASaveFileName);
    end;
  T5:=GetTickCount-T5;
  mmResult.Lines.Add('  测试结果:'#13#10'  QXML='+IntToStr(T1)+'ms,NativeXML='+IntToStr(T2)+
    'ms,ALXML='+IntToStr(T3)+'ms,MSXML='+IntToStr(T4)+'ms,OXML='+IntToStr(T5)+'ms,T1/T2='+
    FormatFloat('0.##',T1*1.0/T2)+',T1/T3='+FormatFloat('0.##',T1*1.0/T3)+',T1/T4='+
    FormatFloat('0.##',T1*1.0/T4)+',T1/T5='+FormatFloat('0.##',T1*1.0/T5));
finally
  AQXML.Free;
  AALXML.Free;
  ANXML.Free;
  AMSXML.Free;
  AOXML.Free;
end;

end;

end.
