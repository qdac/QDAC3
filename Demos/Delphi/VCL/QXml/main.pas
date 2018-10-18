unit main;

interface

uses
  Windows, Messages, SysUtils, types, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, qstring, qxml, rest.client;

type
  TForm2 = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    mmResult: TMemo;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Search: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Button15: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure SearchClick(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

type
  TRttiTestSubRecord = record
    Int64Val: Int64;
    UInt64Val: UInt64;
    UStr: String;
    AStr: AnsiString;
    SStr: ShortString;
    IntVal: Integer;
    // MethodVal: TNotifyEvent;
    SetVal: TBorderIcons;
    WordVal: Word;
    ByteVal: Byte;
    ObjVal: TObject;
    DtValue: TDateTime;
    CardinalVal: Cardinal;
    ShortVal: Smallint;
    CurrVal: Currency;
    EnumVal: TAlign;
    CharVal: Char;
    VarVal: Variant;
    ArrayVal: TBytes;
  end;

  TRttiUnionRecord = record
    case Integer of
      0:
        (iVal: Integer);
      1:
        (bVal: Boolean);
  end;

  TRttiTestRecord = record
    Name: QStringW;
    Id: Integer;
    SubRecord: TRttiTestSubRecord;
    UnionRecord: TRttiUnionRecord;
  end;

function GetFileSize(AFileName: String): Int64;
var
  sr: TSearchRec;
  AHandle: Integer;
begin
  AHandle := FindFirst(AFileName, faAnyFile, sr);
  if AHandle = 0 then
  begin
    Result := sr.Size;
    FindClose(sr);
  end
  else
    Result := 0;
end;

procedure TForm2.Button10Click(Sender: TObject);
var
  AXML: TQXMLNode;
begin
  AXML := TQXMLNode.Create;
  try
    AXML.Parse('<xml>' + '<ToUserName><![CDATA[toUser]]></ToUserName>' +
      '<FromUserName><![CDATA[fromUser]]></FromUserName>' +
      '<CreateTime>12345678</CreateTime>' +
      '<MsgType><![CDATA[text]]></MsgType>' +
      '<Content><![CDATA[content]]></Content>' + '</xml>');
    AXML.ItemByPath('xml\ToUserName').Items[0].Text := 'Jack Sony';
    mmResult.Clear;
    mmResult.Lines.Add('替换toUser的CDATA内容:');
    mmResult.Lines.Add(AXML.AsXML);
    {
      更改结点类型时，注意结点类型的内容不应包含目标类型中不合法的字符，否则会抛出异
      常导致转换失败，将一个结点类型转换为其它类型时，其文本将会保留，但属性和子结点
      都将丢失 }

    mmResult.Lines.Add('更改fromUser的类型为注释:');
    AXML.ItemByPath('xml\FromUserName').Items[0].NodeType := xntComment;
    mmResult.Lines.Add(AXML.AsXML);
    mmResult.Lines.Add('再次更改fromUser的类型为文本:');
    AXML.ItemByPath('xml\FromUserName').Items[0].NodeType := xntText;
    mmResult.Lines.Add(AXML.AsXML);
    mmResult.Lines.Add('再次更改fromUser的类型回CDATA:');
    AXML.ItemByPath('xml\FromUserName').Items[0].NodeType := xntCData;
    mmResult.Lines.Add(AXML.AsXML);
    mmResult.Lines.Add('更改整个xml结点的类型文本:');
    AXML.ItemByPath('xml').NodeType := xntText;
    mmResult.Lines.Add(AXML.AsXML);
  finally
    AXML.Free;
  end;
end;

procedure TForm2.Button11Click(Sender: TObject);
var
  AXML, AChild: TQXMLNode;
  Attr: TQXMLAttr;
begin
  AXML := TQXMLNode.Create;
  try
    AXML.Parse('<xml name="myname" value="myvalue">' +
      '<ToUserName><![CDATA[toUser]]></ToUserName>' +
      '<FromUserName><![CDATA[fromUser]]></FromUserName>' +
      '<CreateTime>12345678</CreateTime>' +
      '<MsgType><![CDATA[text]]></MsgType>' +
      '<Content><![CDATA[content]]></Content>' + '</xml>');
    mmResult.Clear;
    mmResult.Lines.Add('属性遍历');
    for Attr in AXML.ItemByName('xml').Attrs do
      mmResult.Lines.Add(Attr.Name + '=' + Attr.Value);
    mmResult.Lines.Add('结点遍历');
    for AChild in AXML.ItemByName('xml') do
      mmResult.Lines.Add(AChild.AsXML);
  finally
    AXML.Free;
  end;
end;

procedure TForm2.Button12Click(Sender: TObject);
var
  AXML, ANode, AEnNode, AMathNode: TQXMLNode;
begin
  AXML := TQXMLNode.Create;
  try
    ANode := AXML.AddNode('books');
    AEnNode := ANode.AddNode('book');
    AEnNode.Attrs.Add('name', '英语');
    with AEnNode.AddNode('levels') do
    begin
      AddNode('level').Attrs.Add('name', '一年级(上)');
      AddNode('level').Attrs.Add('name', '一年级(下)');
      AddNode('level').Attrs.Add('name', '二年级(上)');
      AddNode('level').Attrs.Add('name', '二年级(下)');
    end;
    AMathNode := ANode.AddNode('book');
    AMathNode.Attrs.Add('name', '数学');
    with AMathNode.AddNode('levels') do
    begin
      AddNode('level').Attrs.Add('name', '三年级(上)');
      AddNode('level').Attrs.Add('name', '三年级(下)');
      AddNode('level').Attrs.Add('name', '四年级(上)');
      AddNode('level').Attrs.Add('name', '四年级(下)');
    end;
    // 同级交换
    ShowMessage(AXML.AsXML);
    AMathNode.MoveTo(ANode, 0);
    ShowMessage(AXML.AsXML);
    AMathNode.MoveTo(ANode, 1);
    ShowMessage(AXML.AsXML);
    // 将数学的levels结点移动到上级结点
    AMathNode.Items[0].MoveTo(ANode, 0);
    ShowMessage(AXML.AsXML);
    // 移到下级
    ANode.Items[0].MoveTo(AEnNode, 1);
    ShowMessage(AXML.AsXML);
    // 移动到同级下
    AMathNode.MoveTo(AEnNode, 0);
    ShowMessage(AXML.AsXML);
    // 自己移动到自己的下级，发生异常
    try
      ANode.MoveTo(AMathNode, 0);
    except
      on E: Exception do
        ShowMessage('尝试将自己移动到自己的子结点下时失败：'#13#10 + E.Message);
    end;
  finally
    AXML.Free;
  end;
end;

procedure TForm2.Button13Click(Sender: TObject);
var
  AXML: TQSoapXML;
  AStream: TMemoryStream;
begin
  AXML := TQSoapXML.Create;
  try
    AXML.Header('m:SessionType', 'http://siebel.com/webservices')
      .AddText('Stateless');
    AXML.Body('CommandKey').AddText('100');
    AStream := TMemoryStream.Create;
    AXML.SaveToStream(AStream);
    mmResult.Clear;
    AStream.Position := 0;
    mmResult.Lines.Add(LoadTextW(AStream));
    FreeObject(AStream);
  finally
    AXML.Free;
  end;
end;

procedure TForm2.Button14Click(Sender: TObject);
var
  AXML, AChild: TQXMLNode;
  B: TBytes;
begin
  AXML := TQXMLNode.Create;
  AXML.ForcePath('a.b.c.d').AddText('OK');
  if AXML.HasChild('a.b.c.d', AChild) then
    mmResult.Text := AXML.AsXML + #13#10 + '子结点 a.b.c.d 存在，值为：' + AChild.AsXML
  else
    mmResult.Text := AXML.AsXML + #13#10 + '子结点 a.b.c.d 不存在';
  AXML.Free;
end;

procedure TForm2.Button15Click(Sender: TObject);
var
  AXML, ANode: TQXMLNode;
  I: Integer;
  T: Cardinal;
  AStream: TStringStream;
begin
  AXML := TQXML.Create;
  try
    AXML.AddComment('Hello,world');
    AXML.Add('Attrs').Attrs.Add('Type', 'test'' type');
    AXML.Add('Tag').AddText('Values');
    AXML.Add('Name').AddText('swish''s name');
    // with AXML.AddNode('xml') do
    // begin
    // AddText('  Hello , world  ');
    // Attrs.Add('Value',' %1 %2 ');
    // end;
    // AXML.Parse('<xml><aa>44</aa><bb/></xml>');
    ShowMessage(AXML.AsXML);
  finally
    FreeObject(AXML);
  end;

end;

procedure TForm2.Button1Click(Sender: TObject);
var
  AXML: TQXMLNode;
  I: Integer;
  T: Cardinal;
begin
  AXML := TQXMLNode.Create;
  try
    T := GetTickCount;
    for I := 0 to 100000 do
      AXML.Add('_' + IntToStr(I));
    T := GetTickCount - T;
    mmResult.Clear;
    mmResult.Lines.Add('添加100,000个结点用时:' + IntToStr(T) + 'ms');
  finally
    AXML.Free;
  end;
end;

procedure TForm2.Button2Click(Sender: TObject);
var
  AXML: TQXMLNode;
  TestRecord: TRttiTestRecord;
begin
  AXML := TQXMLNode.Create;
  try
    TestRecord.Id := 10001;
    TestRecord.Name := 'Complex Record';
    TestRecord.UnionRecord.iVal := 100;
    TestRecord.SubRecord.Int64Val := 1;
    TestRecord.SubRecord.UInt64Val := 2;
    TestRecord.SubRecord.UStr := 'Test String';
    TestRecord.SubRecord.IntVal := 3;
    // TestRecord.SubRecord.MethodVal:=Button2Click;
    TestRecord.SubRecord.SetVal :=
      [{$IFDEF UNICODE}TBorderIcon.{$ENDIF}biSystemMenu];
    TestRecord.SubRecord.WordVal := 4;
    TestRecord.SubRecord.ByteVal := 5;
    TestRecord.SubRecord.ObjVal := Button2;
    TestRecord.SubRecord.DtValue := Now;
    TestRecord.SubRecord.CardinalVal := 6;
    TestRecord.SubRecord.ShortVal := 7;
    TestRecord.SubRecord.CurrVal := 8.9;
    TestRecord.SubRecord.EnumVal := {$IFDEF UNICODE}TAlign.{$ENDIF}alTop;
    TestRecord.SubRecord.CharVal := 'A';
    TestRecord.SubRecord.VarVal :=
      VarArrayOf(['VariantArray', 1, 2.5, true, false]);
    SetLength(TestRecord.SubRecord.ArrayVal, 3);
    TestRecord.SubRecord.ArrayVal[0] := 100;
    TestRecord.SubRecord.ArrayVal[1] := 101;
    TestRecord.SubRecord.ArrayVal[2] := 102;
    with AXML.Add('FixedTypes', xntNode) do
    begin
      Add('This is Test', xntText);
      Add('This is Comment', xntComment);
      Add('This is CDATA', xntCData);
    end;
    with AXML.Add('RTTI') do
    begin
{$IFDEF UNICODE}
      Add('RTTIObject').FromRtti(Button2);
      Add('RTTIRecord').FromRecord(TestRecord);
{$ENDIF}
    end;
    with AXML.Add('Attrs') do
    begin
      Attrs.Add('AttrName', 'AttrValue');
    end;
    mmResult.Clear;
    mmResult.Lines.Add('添加测试结果:');
    mmResult.Lines.Add(AXML.Encode(true, '  '));
  finally
    AXML.Free;
  end;
end;

procedure TForm2.Button3Click(Sender: TObject);
var
  AXML: TQXMLNode;
  T, Speed: Cardinal;
begin
  if OpenDialog1.Execute then
  begin
    AXML := TQXMLNode.Create;
    try
      T := GetTickCount;
      AXML.LoadFromFile(OpenDialog1.FileName);
      T := GetTickCount - T;
      if T > 0 then
        Speed := GetFileSize(OpenDialog1.FileName) * 1000 div T
      else
        Speed := 0;
      mmResult.Lines.Add('QXML加载' + OpenDialog1.FileName + #13#10#9'用时:' +
        IntToStr(T) + 'ms，速度：' + RollupSize(Speed));
    finally
      AXML.Free;
    end;
  end;
end;

procedure TForm2.Button4Click(Sender: TObject);
var
  AXML: TQXMLNode;
  I: Integer;
  T, Speed: Cardinal;
begin
  if SaveDialog1.Execute then
  begin
    AXML := TQXMLNode.Create;
    try
      mmResult.Clear;
      with AXML.Add('String') do
      begin
        for I := 0 to 1000000 do
          AXML.Add('Node' + IntToStr(I)).Add(IntToStr(I), xntText);
      end;
      T := GetTickCount;
      AXML.SaveToFile(SaveDialog1.FileName, teUtf8, false);
      T := GetTickCount - T;
      if T > 0 then
        Speed := (GetFileSize(SaveDialog1.FileName) * 1000 div T)
      else
        Speed := 0;
      mmResult.Lines.Add('QXML保存' + SaveDialog1.FileName + #13#10#9'用时:' +
        IntToStr(T) + 'ms,速度：' + RollupSize(Speed));
    finally
      AXML.Free;
    end;
  end;
end;

procedure TForm2.Button5Click(Sender: TObject);
var
  AXML: TQXMLNode;
begin
  AXML := TQXMLNode.Create;
  try
    AXML.Parse(mmResult.Text);
    // AXML.Parse('<DATASET NAME="TM_ASC_BASICINFO">'#13#10 +
    // '<META IS_INDEPENDENT_LAW="INTEGER" ADDRESS="上海申银汽车服务有限公司上海普陀区中山北路2500号" SALES_LINE=""/>'#13#10
    // + '</DATASET>');
    mmResult.Lines.Add(AXML.Encode(true, '  '));
  finally
    AXML.Free;
  end;

end;

procedure TForm2.Button6Click(Sender: TObject);
var
  AXML: TQXMLNode;
begin
  AXML := TQXMLNode.Create;
  try
    AXML.Parse('<xml><node name="object_0" Id="1">' +
      'Normal Text with Escape in Here!&lt;&gt;' + '<!--Comment Here-->' +
      '<![CDATA[ CData Text]]>' + '</node></xml>');
    mmResult.Lines.Add('Search xml.node'#13#10#9 +
      AXML.ItemByPath('xml.node').Text);
  finally
    AXML.Free;
  end;
end;

procedure TForm2.Button7Click(Sender: TObject);
var
  ARec: TRttiTestSubRecord;
  AXML, ACopy: TQXMLNode;
begin
{$IFDEF UNICODE}
  ARec.Int64Val := 1;
  ARec.UInt64Val := 2;
  ARec.UStr := 'Test String';
  ARec.AStr := 'AnsiString';
  ARec.SStr := 'ShortString';
  ARec.IntVal := 3;
  // ARec.MethodVal:=Button2Click;
  ARec.SetVal := [{$IFDEF UNICODE}TBorderIcon.{$ENDIF}biSystemMenu];
  ARec.WordVal := 4;
  ARec.ByteVal := 5;
  ARec.ObjVal := Button2;
  ARec.DtValue := Now;
  ARec.CardinalVal := 6;
  ARec.ShortVal := 7;
  ARec.CurrVal := 8.9;
  ARec.EnumVal := {$IFDEF UNICODE}TAlign.{$ENDIF}alTop;
  ARec.CharVal := 'A';
  ARec.VarVal := 1; // VarArrayOf(['VariantArray', 1, 2.5, true, false]);
  SetLength(ARec.ArrayVal, 3);
  ARec.ArrayVal[0] := 100;
  ARec.ArrayVal[1] := 101;
  ARec.ArrayVal[2] := 102;
  AXML := TQXMLNode.Create;
  try
    AXML.Add('Record').FromRecord(ARec);
    ACopy := AXML.ItemByName('Record').Copy;
    // ACopy.AttrByPath('Int64Val', 'value').AsInt64 := 100;
    // ACopy.AttrByPath('UStr', 'value').AsString := 'UnicodeString-ByXML';
    // ACopy.AttrByPath('AStr', 'value').AsString := 'AnsiString-ByXML';
    // ACopy.AttrByPath('SStr', 'value').AsString := 'ShortString-ByXML';
    // ACopy.AttrByPath('EnumVal', 'value').AsString := 'alBottom';
    // ACopy.AttrByPath('SetVal', 'value').AsString := '[biHelp]';
    ACopy.ToRecord(ARec);
    ACopy.Free;
    AXML.Add('NewRecord').FromRecord(ARec);
    mmResult.Lines.Add(AXML.AsXML);
  finally
    AXML.Free;
  end;
{$ELSE}
    ShowMessage('本功能不受支持.');
{$ENDIF}
end;

procedure TForm2.Button8Click(Sender: TObject);
var
  AXML, AChild: TQXMLNode;
  // AExp:TBindingExpression;
begin

  { AXML:=TQXMLNode.Create;
    AChild:=AXML.Add('LiveBinding',xntText);
    AExp:=TCreateManagedBinding(
    [TCreateAssociationScope([Associate(AChild,'XML')])],
    'XML.Text',
    [TCreateAssociationScope([Associate(Button8,'Button')])],
    'Button.Caption',nil
    );
    AChild.Text:='哈哈哈';
    TNotify(AChild,'Text');
    AXML.DisposeOf; }
end;

procedure TForm2.Button9Click(Sender: TObject);
var
  AXML: TQXMLNode;
  AStream: TMemoryStream;
  S: QStringW;
  AEncoding: TTextEncoding;
begin
  AXML := TQXMLNode.Create;
  AStream := TMemoryStream.Create;
  try
    S := '<?xml version="1.0" encoding="UTF-8">' +
      '<xml><node name="object_0" Id="1">nodetext1</node></xml>' +
      '<xml><node name="object_1" Id="2">nodetext2</node></xml>' +
      '<!-- xml comment here -->' + '<!DOCTYPE xml>' +
      '<![CDATA[Hello,world]]>';
    // Unicode
    SaveTextW(AStream, S, false);
    AEncoding := teUnicode16LE;
    // UTF8
    // SaveTextU(AStream,QString.Utf8Encode(S),False);
    // AEncoding:=teUtf8;
    AStream.Position := 0;
    AXML.ParseBlock(AStream, AEncoding);
    mmResult.Lines.Add('第一次ParseBlock:');
    mmResult.Lines.Add(AXML.AsXML);
    AXML.ParseBlock(AStream, AEncoding);
    mmResult.Lines.Add('第二次ParseBlock:');
    mmResult.Lines.Add(AXML.AsXML);
    AXML.ParseBlock(AStream, AEncoding);
    mmResult.Lines.Add('第三次ParseBlock:');
    mmResult.Lines.Add(AXML.AsXML);
    AXML.ParseBlock(AStream, AEncoding);
    mmResult.Lines.Add('第四次ParseBlock:');
    mmResult.Lines.Add(AXML.AsXML);
  finally
    AXML.Free;
    AStream.Free;
  end;

end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := true;
end;

procedure TForm2.SearchClick(Sender: TObject);
var
  AXML: TQXMLNode;
begin
  AXML := TQXMLNode.Create;
  try
    AXML.Parse('<xml><node name="object_0" Id="1">nodetext</node></xml>');
    mmResult.Lines.Add('查找 xml.node'#13#10#9 + AXML.ItemByPath('xml.node')
      .Encode(true, '  '));
    mmResult.Lines.Add('查找 xml\node'#13#10#9 + AXML.ItemByPath('xml\node')
      .Encode(true, '  '));
    mmResult.Lines.Add('查找 xml/node'#13#10#9 + AXML.ItemByPath('xml/node')
      .Encode(true, '  '));
    mmResult.Lines.Add('查找 xml.node的name属性的值'#13#10#9 +
      AXML.AttrValueByPath('xml.node', 'name', ''));
  finally
    AXML.Free;
  end;
end;

end.
