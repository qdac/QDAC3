unit main;

interface

{$I 'qdac.inc'}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, NetEncoding,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, qstring, qjson;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    mmResult: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button9: TButton;
    Button13: TButton;
    Button14: TButton;
    Button15: TButton;
    Button16: TButton;
    Invoke: TButton;
    Button17: TButton;
    Button19: TButton;
    Button18: TButton;
    Button20: TButton;
    Button21: TButton;
    Button22: TButton;
    Button23: TButton;
    Button24: TButton;
    Button25: TButton;
    Button26: TButton;
    Button27: TButton;
    Button28: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure Button17Click(Sender: TObject);
    procedure Button19Click(Sender: TObject);
    procedure Button18Click(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure Button20Click(Sender: TObject);
    procedure Button21Click(Sender: TObject);
    procedure Button22Click(Sender: TObject);
    procedure Button23Click(Sender: TObject);
    procedure Button24Click(Sender: TObject);
    procedure Button25Click(Sender: TObject);
    procedure Button26Click(Sender: TObject);
    procedure Button27Click(Sender: TObject);
    procedure Button28Click(Sender: TObject);
  private
    { Private declarations }
    procedure DoCopyIf(ASender, AItem: TQJson; var Accept: Boolean;
      ATag: Pointer);
    procedure DoDeleteIf(ASender, AChild: TQJson; var Accept: Boolean;
      ATag: Pointer);
    procedure DoFindIf(ASender, AChild: TQJson; var Accept: Boolean;
      ATag: Pointer);
    procedure PrintRegexMatchResult(AItem: TQJson; ATag: Pointer);
  public
    { Public declarations }
    function Add(X, Y: Integer): Integer;
    function ObjectCall(AObject: TObject): String;
    procedure CharCall(s: PAnsiChar);
  end;

type
  TRttiTestSubRecord = record
    Int64Val: Int64;
    UInt64Val: UInt64;
    UStr: String;
    AStr: AnsiString;
    SStr: ShortString;
    IntVal: Integer;
    MethodVal: TNotifyEvent;
    SetVal: TBorderIcons;
    WordVal: Word;
    ByteVal: Byte;
    ObjVal: TObject;
    DtValue: TDateTime;
    tmValue: TTime;
    dValue: TDate;
    CardinalVal: Cardinal;
    ShortVal: Smallint;
    CurrVal: Currency;
    EnumVal: TAlign;
    CharVal: Char;
    VarVal: Variant;
    ArrayVal: TBytes;
{$IFDEF UNICODE}
    IntArray: TArray<Integer>;
{$ENDIF}
  end;

  TRttiUnionRecord = record
    case Integer of
      0:
        (iVal: Integer);
    // 1:(bVal:Boolean);
  end;

  TRttiTestRecord = record
    Name: QStringW;
    Id: Integer;
    SubRecord: TRttiTestSubRecord;
    UnionRecord: TRttiUnionRecord;
  end;

  // Test for user
  TTitleRecord = packed record
    Title: TFileName;
    Date: TDateTime;
  end;

  TTitleRecords = packed record
    Len: Integer;
    TitleRecord: array [0 .. 100] of TTitleRecord;
  end;

  TFixedRecordArray = array [0 .. 1] of TRttiUnionRecord;

  TRttiObjectRecord = record
    Obj: TStringList;
  end;

  TDeviceType = (dtSM3000, dtSM6000, dtSM6100, dtSM7000, dtSM8000);

  // 上位机所有命令
  TRCU_Cmd = record
    Id: string; // 命令ID 为了缩减命令，设备名称+.+INDEX
    DevcType: TDeviceType; // 设备类型，比如 SM-6000
    Rcu_Kind: Integer; // 配件类型
    Name: string; // 命令名称，比如继电器
    KEY_IDX: Integer; // 按键命令，如果是双个组合键，值大于255
    SHOW_IDX: Integer; // 显示顺序
    Cmds: TArray<TArray<Byte>>; // 命令字节,有可能是多个模式
    // 返回值处理
    ResultValue: string; // 返回值处理的公式，json表达式
    RCU_Type_ID: string; // 所属主机ID
    RCU_Type_Name: string; // 主机类型名称，比如 是 SM-6000
    // procedure Clear;
  end;

  // 场景信息，是一串组合的命令
  TSence = record
    Name: string; // 场景名称
    Cmds: TArray<string>; // TArray<TPlc_Cmd>;
  end;

  // 每个客房信息
  TRoom = record
    Hotel_ID: string; // 酒店ID
    Hotel_Code: string; // 酒店编码
    Room_ID: string; // 客房ID
    ROOM_Name: string; // 真实的客房名称
    // 客房编号，X区X栋X层X房 = X.X.X.X
    Room_Code: string; // 客房号 为了便于客户端调用，Room_Code做条件
    RCU_Type_ID: string; // 基于哪种设备
    RCU_Type_Name: string; // RCU名称
    RCU_HOST: string;
    RCU_Port: string;
    Cmds: TArray<TRCU_Cmd>; // 原始的命令信息
    // 客房里的设备信息以及设备名称
    // Cmd_Name_Ids:TNameValueRow;  // ID和名称对应 ，，保留命令原来的排序
    // 酒店客房里的场景信息，一个场景对应多条命令
    Sences: TArray<TSence>;
    // procedure Clear;
  end;

type
  TUserInfo = record
    openid: string;
    nickname: string; // 用户的昵称
    subscribe: Integer; // 用户是否订阅该公众号标识，值为0时，代表此用户没有关注该公众号，拉取不到其余信息。
    sex: Integer; // 用户的性别，值为1时是男性，值为2时是女性，值为0时是未知
    city: string; // 用户所在城市
    country: string; // 用户所在国家
    province: string; // 用户所在省份
    language: string; // 用户的语言，简体中文为zh_CN
    { 用户头像，最后一个数值代表正方形头像大小（有0、46、64、96、132数值可选，
      0代表640*640正方形头像），用户没有头像时该项为空。
      若用户更换头像，原有头像URL将失效。 }
    headimgurl: string;
    subscribe_time: TDateTime; // 用户关注时间，为时间戳。如果用户曾多次关注，则取最后关注时间
    unionid: string; // 只有在用户将公众号绑定到微信开放平台帐号后，才会出现该字段。
    remark: string; // 公众号运营者对粉丝的备注，公众号运营者可在微信公众平台用户管理界面对粉丝添加备注
    groupid: string; // 用户所在的分组ID（兼容旧的用户分组接口）
    tagid_list: string; // 用户被打上的标签ID列表
  end;

  TUserInfos = record
    user_info_list: TArray<TUserInfo>;
  end;

type
  Ta = record
    A: Integer;
    B: Integer;
  end;

  TTaArray = TArray<Ta>;

  TRe = record
    Id: Integer;
    Name: string;
    aryt: array of Ta; // TTaArray;
  end;

var
  Form1: TForm1;

implementation

uses typinfo{$IFDEF UNICODE}, rtti{$ENDIF};
{$R *.dfm}

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

function TForm1.Add(X, Y: Integer): Integer;
begin
  Result := X + Y;
end;

procedure TForm1.Button10Click(Sender: TObject);
var
  AJson, AItem: TQJson;
  s: String;
begin
  AJson := TQJson.Create;
  try
    AJson.Add('Item1', 0);
    AJson.Add('Item2', true);
    AJson.Add('Item3', 1.23);
    for AItem in AJson do
    begin
      s := s + AItem.Name + ' => ' + AItem.AsString + #13#10;
    end;
    mmResult.Lines.Add(s);
  finally
    AJson.Free;
  end;
end;

procedure TForm1.Button11Click(Sender: TObject);
var
  AJson: TQJson;
begin
  AJson := TQJson.Create;
  try
    // 强制路径访问，如果路径不存在，则会创建路径，路径分隔符可以是./\之一
    AJson.ForcePath('demo1.item[0].name').AsString := '1';
    AJson.ForcePath('demo1.item[1].name').AsString := '100';
    try
      ShowMessage('下面正常会抛出一个异常');
      AJson.ForcePath('demo1[0].item[1]').AsString := '200';
    except
      // 这个应该抛出异常，demo1是对象不是数组，所以是错的
    end;
    // 访问第6个元素，前5个元素会自动设置为null
    AJson.ForcePath('demo2[5]').AsInteger := 103;
    // 强制创建一个空数组对象，然后调用Add方法添加子成员，但其名称会被忽略，所以实际是demo3:[1.23]
    AJson.ForcePath('demo3[]').Add('Value', 1.23);
    // 下面的代码将生成"demo4":[{"Name":"demo4"}]的结果
    AJson.ForcePath('demo4[].Name').AsString := 'demo4';
    // 直接强制路径存在
    AJson.ForcePath('demo5[0]').AsString := 'demo5';
    mmResult.Text := AJson.AsJson;
  finally
    AJson.Free;
  end;
end;

procedure TForm1.Button12Click(Sender: TObject);
var
  AJson: TQJson;
  AList: TQJsonItemList;
begin
  AJson := TQJson.Create;
  try
    AJson.Parse('{' + '"object":{' + ' "name":"object_1",' + ' "subobj":{' +
      '   "name":"subobj_1"' + '   },' + ' "subarray":[1,3,4]' + ' },' +
      '"array":[100,200,300,{"name":"object"}]' + '}');
    mmResult.Lines.Add('[原始Json数据]');
    mmResult.Lines.Add(AJson.AsJson);
    mmResult.Lines.Add(#13#10 + '[查找结果]');
    AList := TQJsonItemList.Create;
    AJson.ItemByRegex('sub.+', AList, true);
    mmResult.Lines.Add('ItemByRegex找到' + IntToStr(AList.Count) + '个结点');
    AList.Free;
    mmResult.Lines.Add('ItemByPath(''object\subobj\name'')=' +
      AJson.ItemByPath('object\subobj\name').AsString);
    mmResult.Lines.Add('ItemByPath(''object\subarray[1]'')=' +
      AJson.ItemByPath('object\subarray[1]').AsString);
    mmResult.Lines.Add('ItemByPath(''array[1]'')=' +
      AJson.ItemByPath('array[1]').AsString);
    mmResult.Lines.Add('ItemByPath(''array[3].name'')=' +
      AJson.ItemByPath('array[3].name').AsString);
    mmResult.Lines.Add('ItemByPath(''object[0]'')=' +
      AJson.ItemByPath('object[0]').AsString);
    mmResult.Lines.Add('ItemByName(''array[3][0]'')=' +
      AJson.ItemByPath('array[3][0]').AsString);
  finally
    AJson.Free;
  end;
end;

procedure TForm1.Button13Click(Sender: TObject);
{$IFNDEF UNICODE}
begin
  ShowMessage('不支持的功能');
{$ELSE}
var
  AJson: TQJson;
  AValue: TValue;
begin
  AJson := TQJson.Create;
  try
    with AJson.Add('Add') do
    begin
      Add('X').AsInteger := 100;
      Add('Y').AsInteger := 200;
    end;
    AValue := AJson.ItemByName('Add').Invoke(Self);
    mmResult.Lines.Add(AJson.AsJson);
    mmResult.Lines.Add('.Invoke=' + IntToStr(AValue.AsInteger));
  finally
    AJson.Free;
  end;
{$ENDIF}
end;

procedure TForm1.Button15Click(Sender: TObject);
var
  AJson: TQJson;
  s: String;
begin
  AJson := TQJson.Create;
  try
    AJson.Add('Text').AsString := 'Hello,中国';
    ShowMessage(AJson.Encode(true, true));
    AJson.Parse(AJson.Encode(true, true));
    ShowMessage(AJson.AsJson);
  finally
    AJson.Free;
  end;
end;

procedure TForm1.Button16Click(Sender: TObject);
var
  AJson: TQJson;
  procedure DoTry(s: QStringW);
  begin
    if AJson.TryParse(s) then
      ShowMessage(AJson.AsString)
    else
      ShowMessage('解析失败'#13#10 + s);
  end;

begin
  AJson := TQJson.Create;
  try
    DoTry('{aaa}');
    DoTry('{"aaa":100}');
  finally
    AJson.Free;
  end;
end;

procedure TForm1.Button17Click(Sender: TObject);
var
  AJson, AChild: TQJson;
  B: TBytes;
begin
  AJson := TQJson.Create;
  AJson.ForcePath('a.b.c[].d').AsString := 'OK';
  if AJson.HasChild('a.b.c[0].d', AChild) then
    mmResult.Text := AJson.AsJson + #13#10 + '子结点 a.b.c[0].d 存在，值为：' +
      AChild.AsString
  else
    mmResult.Text := AJson.AsJson + #13#10 + '子结点 a.b.c[0].d 不存在';
  AJson.Free;
end;

procedure TForm1.Button18Click(Sender: TObject);
var
  AJson: TQJson;
  s: String;
  AStream: TMemoryStream;
  I: Integer;
begin
  AStream := TMemoryStream.Create;
  for I := 0 to 99 do
    AStream.Write(I, SizeOf(I));
  EncodeJsonBinaryAsBase64;
  AJson := TQJson.Create;
  AJson.Add('stream').ValueFromStream(AStream, 0);
  AJson.Add('code').AsInteger := 0;
  AJson.Add('message').AsString := '请使用客户端登录！';
  s := AJson.AsJson;
  mmResult.Clear;
  mmResult.Lines.Add('二进制使用 Base64 编码大小 ' + IntToStr(s.Length) + ' 个字符');
  mmResult.Lines.Add(s);
  EncodeJsonBinaryAsHex;
  AJson.ItemByName('stream').ValueFromStream(AStream, 0);
  s := AJson.AsJson;
  mmResult.Lines.Add('二进制使用16进制编码大小 ' + IntToStr(s.Length) + ' 个字符');
  mmResult.Lines.Add(s);
  AJson.Free;
  AStream.Free;
end;

procedure TForm1.Button19Click(Sender: TObject);
var
  AJson: TQJson;
  s: String;
begin
  // 下面的代码必需保证StrictJson为False，否则会由于不支持注释而出错
  AJson := TQJson.Create;
  s := '//This is demo'#13#10 + '{//Json Start'#13#10 +
    '"A"/*Name*/:/*Value Start*/"B"//Value End'#13#10 + '}'#13#10 +
    '//Json End'#13#10;
  AJson.Parse(s);
  mmResult.Text := '[原始内容]'#13#10 + s + #13#10'[解析结果]'#13#10 + AJson.AsJson;
  AJson.Free;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  AJson: TQJson;
  I: Integer;
  T: Cardinal;
begin
  AJson := TQJson.Create;
  try
    T := GetTickCount;
    for I := 0 to 100000 do
      AJson.Add('_' + IntToStr(I), Now);
    T := GetTickCount - T;
    mmResult.Clear;
    mmResult.Lines.Add('添加100,000个结点用时:' + IntToStr(T) + 'ms');
  finally
    AJson.Free;
  end;
end;

procedure TForm1.Button20Click(Sender: TObject);
var
  AJson: TQJson;
begin
  AJson := TQJson.Create;
  try
    AJson.Add('name').AsString := 'id_1';
    AJson.Add('id').AsInteger := 100;
    with AJson.Add('object') do
    begin
      Add('color2').AsInteger := 1536;
      Add('color1').AsInteger := 1234;
    end;
    AJson.Add('price').AsFloat := 12.45;
    if AJson.ContainsName('color1', true) then
      ShowMessage('Contains color1');
    if AJson.ContainsValue('1536', true, true) then
      ShowMessage('Contains 1536');
    // AJson.Sort(False,True,jdtUnknown,nil);
    // AJson.RevertOrder(True);
    ShowMessage(AJson.AsString);
  finally
    AJson.Free;
  end;
end;

procedure TForm1.Button21Click(Sender: TObject);
var
  AJson: TQJson;
begin
  AJson := TQJson.Create;
  try
    AJson.Parse
      ('//这是块注释'#13#10'{"id":1/*Object Id*/,"name":"objectname"//Object Name'#13#10',"array":[/*First*/1,2/*Second*/]}');
    ShowMessage(AJson.AsJson);
    AJson.CommentStyle := jcsBeforeName;
    ShowMessage(AJson.AsJson);
    AJson.CommentStyle := jcsAfterValue;
    ShowMessage(AJson.AsJson);
  finally
    FreeAndNil(AJson);
  end;

end;

procedure TForm1.Button22Click(Sender: TObject);
var
  AJson1, AJson2, AJson3: TQJson;
begin
  AJson1 := TQJson.Create;
  AJson2 := TQJson.Create;
  try
    AJson1.Parse('{a:100,b:20,c:"id"}');
    AJson2.Parse('{d:400,e:70,a:"from2"}');
    mmResult.Lines.Add('Json 1');
    mmResult.Lines.Add(AJson1.AsJson);
    mmResult.Lines.Add('Json 2');
    mmResult.Lines.Add(AJson2.AsJson);
    AJson3 := AJson1.Copy;
    AJson3.Merge(AJson2, jmmIgnore);
    mmResult.Lines.Add('Json1.Merge(Json2,jmmIgnore)');
    mmResult.Lines.Add(AJson3.AsJson);
    FreeAndNil(AJson3);
    AJson3 := AJson1.Copy;
    AJson3.Merge(AJson2, jmmAppend);
    mmResult.Lines.Add('Json1.Merge(Json2,jmmAppend)');
    mmResult.Lines.Add(AJson3.AsJson);
    FreeAndNil(AJson3);
    AJson3 := AJson1.Copy;
    AJson3.Merge(AJson2, jmmAsSource);
    mmResult.Lines.Add('Json1.Merge(Json2,jmmAsSource)');
    mmResult.Lines.Add(AJson3.AsJson);
    AJson3 := AJson1.Copy;
    AJson3.Merge(AJson2, jmmReplace);
    mmResult.Lines.Add('Json1.Merge(Json2,jmmReplace)');
    mmResult.Lines.Add(AJson3.AsJson);
    FreeAndNil(AJson3);
  finally
    FreeAndNil(AJson1);
    FreeAndNil(AJson2);
  end;
end;

procedure TForm1.Button23Click(Sender: TObject);
var
  AJson1, AJson2, AJson3: TQJson;
begin
  AJson1 := TQJson.Create;
  AJson2 := TQJson.Create;
  try
    AJson1.Parse('{a:100,b:20,c:"id"}');
    AJson2.Parse('{d:400,e:70,a:100}');
    mmResult.Lines.Add('Json 1');
    mmResult.Lines.Add(AJson1.AsJson);
    mmResult.Lines.Add('Json 2');
    mmResult.Lines.Add(AJson2.AsJson);
    AJson3 := AJson1.Intersect(AJson2);
    mmResult.Lines.Add('Result');
    mmResult.Lines.Add(AJson3.AsJson);
    FreeAndNil(AJson3);
  finally
    FreeAndNil(AJson1);
    FreeAndNil(AJson2);
  end;
end;

procedure TForm1.Button24Click(Sender: TObject);
var
  AJson1, AJson2, AJson3: TQJson;
begin
  AJson1 := TQJson.Create;
  AJson2 := TQJson.Create;
  try
    AJson1.Parse('{a:100,b:20,c:"id"}');
    AJson2.Parse('{d:400,e:70,a:100}');
    mmResult.Lines.Add('Json 1');
    mmResult.Lines.Add(AJson1.AsJson);
    mmResult.Lines.Add('Json 2');
    mmResult.Lines.Add(AJson2.AsJson);
    AJson3 := AJson1.Diff(AJson2);
    mmResult.Lines.Add('Result');
    mmResult.Lines.Add(AJson3.AsJson);
    FreeAndNil(AJson3);
  finally
    FreeAndNil(AJson1);
    FreeAndNil(AJson2);
  end;

end;

procedure TForm1.Button25Click(Sender: TObject);
var
  s: String;
begin
  mmResult.Text := JavaUnescape('a\402b', true);
end;

procedure TForm1.Button26Click(Sender: TObject);
var
  AJson: TQJson;
begin
  AJson := TQJson.Create;
  AJson.Parse('[1,2,true,false,''str'']');
  mmResult.Lines.Add('Index of Value 2=' + IntToStr(AJson.IndexOfValue(2)));
  mmResult.Lines.Add('Index of Value true=' +
    IntToStr(AJson.IndexOfValue(true, true)));
  mmResult.Lines.Add('Index of Value str=' +
    IntToStr(AJson.IndexOfValue('str')));
  FreeAndNil(AJson);
end;

procedure TForm1.Button27Click(Sender: TObject);
var
  J1, J2: TQJson;
begin
  J1 := TQJson.Create;
  J2 := TQJson.Create;
  try
    with J1.ForcePath('T1') do
    begin
      Add('T11').AsInteger := 100;
      Add('T12').AsString := 'T12';
    end;
    with J2.ForcePath('T1') do
    begin
      Add('T11').AsInteger := 200;
      Add('T22').AsString := 'T12';
    end;
    J1.Merge(J2, TQJsonMergeMethod.jmmReplace);
    ShowMessage(J1.AsJson);
  finally
    FreeAndNil(J1);
    FreeAndNil(J2);
  end;
end;

procedure TForm1.Button28Click(Sender: TObject);
var
  AJson, AItem: TQJson;
begin
  AJson := TQJson.Create;
  try
    AJson.Parse
      ('[{"code":"GTO","name":"国通"},{"code":"STO","name":"申通"},{"code":"YTO","name":"韵达"}]');
    mmResult.Lines.Add(AJson.AsJson);
    mmResult.Lines.Add('查找符合要求的结点:');
{$IFDEF UNICODE}
    mmResult.Lines.Add('ForEach 模式');
    AJson.Match('.+通', [jmsMatchValue, jmsNest]).ForEach(
      procedure(AItem: TQJson)
      begin
        mmResult.Lines.Add(AItem.Path + '=>' + AItem.AsString);
      end);
    mmResult.Lines.Add('For..In 模式');
    for AItem in AJson.Match('.+通', [jmsMatchValue, jmsNest]) do
    begin
      mmResult.Lines.Add(AItem.Path + '=>' + AItem.AsString);
    end

{$ELSE}
    AJson.Match('.+通', [jmsMatchValue, jmsNest])
      .ForEach(PrintRegexMatchResult, nil);
{$ENDIF}
  finally
    FreeAndNil(AJson);
  end;

end;

procedure TForm1.Button2Click(Sender: TObject);
var
  AJson: TQJson;
  TestRecord: TRttiTestRecord;
begin
  AJson := TQJson.Create;
  try
    TestRecord.Id := 10001;
    TestRecord.Name := 'Complex Record';
    TestRecord.UnionRecord.iVal := 100;
    TestRecord.SubRecord.Int64Val := 1;
    TestRecord.SubRecord.UInt64Val := 2;
    TestRecord.SubRecord.UStr := 'Test String';
    TestRecord.SubRecord.IntVal := 3;
    TestRecord.SubRecord.MethodVal := Button2Click;
    TestRecord.SubRecord.SetVal :=
      [{$IFDEF UNICODE}TBorderIcon.{$ENDIF}biSystemMenu];
    TestRecord.SubRecord.WordVal := 4;
    TestRecord.SubRecord.ByteVal := 5;
    TestRecord.SubRecord.ObjVal := Button2;
    TestRecord.SubRecord.DtValue := Now;
    TestRecord.SubRecord.tmValue := Time;
    TestRecord.SubRecord.dValue := Now;
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
    AJson.Add('IP', '192.168.1.1', jdtString);
    with AJson.Add('FixedTypes') do
    begin
      AddDateTime('DateTime', Now);
      Add('Integer', 1000);
      Add('Boolean', true);
      Add('Float', 1.23);
      Add('Array', [1, 'goods', true, 3.4]);
{$IFDEF UNICODE}
      Add('RTTIObject').FromRtti(Button2);
      Add('RTTIRecord').FromRecord(TestRecord);
{$ENDIF}
    end;
    with AJson.Add('AutoTypes') do
    begin
      Add('Integer', '-100', jdtUnknown);
      Add('Float', '-12.3', jdtUnknown);
      Add('Array', '[2,''goods'',true,4.5]', jdtUnknown);
      Add('Object', '{"Name":"Object_Name","Value":"Object_Value"}',
        jdtUnknown);
      Add('ForceArrayAsString', '[2,''goods'',true,4.5]', jdtString);
      Add('ForceObjectAsString',
        '{"Name":"Object_Name","Value":"Object_Value"}', jdtString);
    end;
    with AJson.Add('AsTypes') do
    begin
      Add('Integer').AsInteger := 123;
      Add('Float').AsFloat := 5.6;
      Add('Boolean').AsBoolean := false;
      Add('VarArray').AsVariant := VarArrayOf([9, 10, 11, 2]);
      Add('Array').AsArray := '[10,3,22,99]';
      Add('Object').AsObject := '{"Name":"Object_2","Value":"Value_2"}';
    end;
    mmResult.Clear;
    mmResult.Lines.Add('添加测试结果:');
    mmResult.Lines.Add(AJson.Encode(true));
  finally
    AJson.Free;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  AJson: TQJson;
  T: Cardinal;
  Speed: Cardinal;
  procedure PreCache;
  var
    AStream: TMemoryStream;
  begin
    AStream := TMemoryStream.Create;
    try
      AStream.LoadFromFile(OpenDialog1.FileName);
    finally
      AStream.Free;
    end;
  end;

begin
  if OpenDialog1.Execute then
  begin
    // uJsonTest;
    AJson := TQJson.Create;
    try
      T := GetTickCount;
      AJson.LoadFromFile(OpenDialog1.FileName);
      T := GetTickCount - T;
      if T > 0 then
        Speed := (GetFileSize(OpenDialog1.FileName) * 1000 div T)
      else
        Speed := 0;
      mmResult.Clear;
      // mmResult.Lines.Add('加载的JSON文件内容：');
      mmResult.Lines.Add(AJson.Encode(true));
      mmResult.Lines.Add('QJson加载用时:' + IntToStr(T) + 'ms，速度:' +
        RollupSize(Speed));
    finally
      AJson.Free;
    end;
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  AJson: TQJson;
  J: Integer;
  T1, T2: Cardinal;
  Speed: Cardinal;
begin
  if SaveDialog1.Execute then
  begin
    AJson := TQJson.Create;
    try
      mmResult.Clear;
      T1 := GetTickCount;
      with AJson.Add('Integers', jdtObject) do
      begin
        for J := 0 to 2000000 do
          Add('Node' + IntToStr(J)).AsInteger := J;
      end;
      T1 := GetTickCount - T1;
      T2 := GetTickCount;
      AJson.SaveToFile(SaveDialog1.FileName, teAnsi, false);
      T2 := GetTickCount - T2;
      if T2 > 0 then
        Speed := (GetFileSize(SaveDialog1.FileName) * 1000 div T2)
      else
        Speed := 0;
      mmResult.Lines.Add('生成200万结点用时' + IntToStr(T1) + 'ms,保存用时:' + IntToStr(T2)
        + 'ms，速度：' + RollupSize(Speed));
    finally
      AJson.Free;
    end;
  end;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  AJson: TQJson;
begin
  AJson := TQJson.Create;
  try
    AJson.Parse(mmResult.Text);
    ShowMessage(AJson.Encode(true));
  finally
    AJson.Free;
  end;
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  ARec: TRttiTestSubRecord;
  AJson, ACopy: TQJson;
begin
{$IFNDEF UNICODE}
  ShowMessage('本功能在当前IDE中不受支持.');
{$ELSE}
  ARec.Int64Val := 1;
  ARec.UInt64Val := 2;
  ARec.UStr := 'Test String';
  ARec.AStr := 'AnsiString';
  ARec.SStr := 'ShortString';
  ARec.IntVal := 3;
  ARec.MethodVal := Button2Click;
  ARec.SetVal := [{$IFDEF UNICODE}TBorderIcon.{$ENDIF}biSystemMenu];
  ARec.WordVal := 4;
  ARec.ByteVal := 5;
  ARec.ObjVal := Button2;
  ARec.DtValue := Now;
  ARec.tmValue := Time;
  ARec.dValue := Now;
  ARec.CardinalVal := 6;
  ARec.ShortVal := 7;
  ARec.CurrVal := 8.9;
  ARec.EnumVal := {$IFDEF UNICODE}TAlign.{$ENDIF}alTop;
  ARec.CharVal := 'A';
  ARec.VarVal := VarArrayOf(['VariantArray', 1, 2.5, true, false]);
  SetLength(ARec.ArrayVal, 3);
  ARec.ArrayVal[0] := 100;
  ARec.ArrayVal[1] := 101;
  ARec.ArrayVal[2] := 102;
  SetLength(ARec.IntArray, 2);
  ARec.IntArray[0] := 300;
  ARec.IntArray[1] := 200;
  AJson := TQJson.Create;
  try
{$IFDEF UNICODE}
    AJson.Add('Record').FromRecord(ARec);
    ACopy := AJson.ItemByName('Record').Copy;
    ACopy.ItemByName('Int64Val').AsInt64 := 100;
    ACopy.ItemByPath('UStr').AsString := 'UnicodeString-ByJson';
    ACopy.ItemByPath('AStr').AsString := 'AnsiString-ByJson';
    ACopy.ItemByPath('SStr').AsString := 'ShortString-ByJson';
    ACopy.ItemByPath('EnumVal').AsString := 'alBottom';
    ACopy.ItemByPath('SetVal').AsString := '[biHelp]';
    ACopy.ItemByPath('ArrayVal').AsJson := '[10,30,15]';
    ACopy.ItemByPath('VarVal').AsVariant :=
      VarArrayOf(['By Json', 3, 4, false, true]);
    ACopy.ToRecord<TRttiTestSubRecord>(ARec);
    ACopy.Free;
    AJson.Add('NewRecord').FromRecord(ARec);
{$ENDIF}
    mmResult.Lines.Add(AJson.AsJson);
  finally
    AJson.Free;
  end;
{$ENDIF}
end;

procedure TForm1.Button7Click(Sender: TObject);
var
  AStream: TMemoryStream;
  AJson: TQJson;
  s: QStringW;
  AEncode: TTextEncoding;
begin
  AStream := TMemoryStream.Create;
  AJson := TQJson.Create;
  try
    AJson.DataType := jdtObject;
    s := '{"record1":{"id":100,"name":"name1"}}'#13#10 +
      '{"record2":{"id":200,"name":"name2"}}'#13#10 +
      '{"record3":{"id":300,"name":"name3"}}'#13#10;
    // UCS2
    mmResult.Lines.Add('Unicode 16 LE编码:');
    AEncode := teUnicode16LE;
    AStream.Size := 0;
    SaveTextW(AStream, s, false);
    AStream.Position := 0;
    AJson.Clear;
    AJson.ParseBlock(AStream, AEncode);
    mmResult.Lines.Add('第一次解析结果:'#13#10);
    mmResult.Lines.Add(AJson.AsJson);
    AJson.Clear;
    AJson.ParseBlock(AStream, AEncode);
    mmResult.Lines.Add(#13#10'第二次解析结果:'#13#10);
    mmResult.Lines.Add(AJson.AsJson);
    AJson.Clear;
    AJson.ParseBlock(AStream, AEncode);
    mmResult.Lines.Add(#13#10'第三次解析结果:');
    mmResult.Lines.Add(AJson.AsJson);
    // UTF-8
    mmResult.Lines.Add('UTF8编码:');
    AEncode := teUtf8;
    AStream.Size := 0;
    SaveTextU(AStream, qstring.Utf8Encode(s), false);
    AStream.Position := 0;
    AJson.Clear;
    AJson.ParseBlock(AStream, AEncode);
    mmResult.Lines.Add(#13#10'第一次解析结果:'#13#10);
    mmResult.Lines.Add(AJson.AsJson);
    AJson.Clear;
    AJson.ParseBlock(AStream, AEncode);
    mmResult.Lines.Add(#13#10'第二次解析结果:'#13#10);
    mmResult.Lines.Add(AJson.AsJson);
    AJson.Clear;
    AJson.ParseBlock(AStream, AEncode);
    mmResult.Lines.Add(#13#10'第三次解析结果:');
    mmResult.Lines.Add(AJson.AsJson);
    // ANSI
    mmResult.Lines.Add(#13#10'ANSI编码:');
    AEncode := teAnsi;
    AStream.Size := 0;
    SaveTextA(AStream, qstring.AnsiEncode(s));
    AStream.Position := 0;
    AJson.Clear;
    AJson.ParseBlock(AStream, AEncode);
    mmResult.Lines.Add('第一次解析结果:'#13#10);
    mmResult.Lines.Add(AJson.AsJson);
    AJson.Clear;
    AJson.ParseBlock(AStream, AEncode);
    mmResult.Lines.Add(#13#10'第二次解析结果:'#13#10);
    mmResult.Lines.Add(AJson.AsJson);
    AJson.Clear;
    AJson.ParseBlock(AStream, AEncode);
    mmResult.Lines.Add(#13#10'第三次解析结果:');
    mmResult.Lines.Add(AJson.AsJson);
    // UCS2BE
    mmResult.Lines.Add(#13#10'Unicode16BE编码:');
    AEncode := teUnicode16BE;
    AStream.Size := 0;
    SaveTextWBE(AStream, s, false);
    AStream.Position := 0;
    AJson.Clear;
    AJson.ParseBlock(AStream, AEncode);
    mmResult.Lines.Add('第一次解析结果:'#13#10);
    mmResult.Lines.Add(AJson.AsJson);
    AJson.Clear;
    AJson.ParseBlock(AStream, AEncode);
    mmResult.Lines.Add(#13#10'第二次解析结果:'#13#10);
    mmResult.Lines.Add(AJson.AsJson);
    AJson.Clear;
    AJson.ParseBlock(AStream, AEncode);
    mmResult.Lines.Add(#13#10'第三次解析结果:');
    mmResult.Lines.Add(AJson.AsJson);
  finally
    AStream.Free;
    AJson.Free;
  end;
end;

procedure TForm1.Button8Click(Sender: TObject);
var
  AJson, AItem: TQJson;
  J: Integer;
  DynArray: array of Integer;
  RecordArray: array of TRttiUnionRecord;
begin
  AJson := TQJson.Create;
  try
    // 添加数组元素的N种方式演示
    // 1. 直接调用Add数组元素文本的方式
    AJson.Add('AddArrayText', '["Item1",100,null,true,false,123.4]', jdtArray);
    // jdtArray如果省略会自动测试，如果明确知道，就不要让其判断增加开销
    // 2. 直接添加数组
    AJson.Add('AddArray', ['Item1', 100, Null, true, false, 123.4]);
    // 3. 直接用VarArrayOf赋值
    AJson.Add('AsVariant').AsVariant :=
      VarArrayOf(['Item1', 100, Null, true, false, 123.4]);
    // 对于动态数组，由于
    SetLength(DynArray, 5);
    DynArray[0] := 100;
    DynArray[1] := 200;
    DynArray[2] := 300;
    DynArray[3] := 400;
    DynArray[4] := 500;
    AJson.Add('DynArray').AsVariant := DynArray;
{$IFDEF UNICODE}
    SetLength(RecordArray, 2);
    RecordArray[0].iVal := 1;
    RecordArray[1].iVal := 2;
    with AJson.Add('RecordArray', jdtArray) do
    begin
      for J := 0 to High(RecordArray) do
        Add.FromRecord(RecordArray[J]);
    end;
{$ENDIF}
    // AJson.Add('RecordArray').AsVariant:=RecordArray;
    // 4. 直接用AsArray来赋给数组文件
    AJson.Add('AsArray').AsArray := '["Item1",100,null,true,false,123.4]';
    // 5. 手动逐个添加元素
    with AJson.Add('Manul') do
    begin
      DataType := jdtArray;
      Add.AsString := 'Item1';
      Add.AsInteger := 100;
      Add;
      Add.AsBoolean := true;
      Add.AsBoolean := false;
      Add.AsFloat := 123.4;
    end;
    // 添加对象数组和上面类型，只是子结点换成是对象就可以了
    AJson.Add('Object', [TQJson.Create.Add('Item1', 100).Parent,
      TQJson.Create.Add('Item2', true).Parent]);
    mmResult.Lines.Add(AJson.AsJson);
    // 访问数组中的元素
    mmResult.Lines.Add('使用for in枚举数组Manul的元素值');
    J := 0;
    for AItem in AJson.ItemByName('Manul') do
    begin
      mmResult.Lines.Add('Manul[' + IntToStr(J) + ']=' + AItem.AsString);
      Inc(J);
    end;
    mmResult.Lines.Add('使用普通for循环枚举数组Manul的元素值');
    AItem := AJson.ItemByName('Manul');
    for J := 0 to AItem.Count - 1 do
      mmResult.Lines.Add('Manul[' + IntToStr(J) + ']=' + AItem[J].AsString);
  finally
    FreeObject(AJson);
  end;
end;

procedure TForm1.Button9Click(Sender: TObject);
var
  AJson, AItem: TQJson;
begin
  AJson := TQJson.Create;
  try
    AJson.Parse('{' + '"object":{' + ' "name":"object_1",' + ' "subobj":{' +
      '   "name":"subobj_1"' + '   },' + ' "subarray":[1,3,4]' + ' },' +
      '"array":[100,200,300,{"name":"object"}]' + '}');
{$IFDEF UNICODE}
    AItem := AJson.CopyIf(nil,
      procedure(ASender, AChild: TQJson; var Accept: Boolean; ATag: Pointer)
      begin
        Accept := (AChild.DataType <> jdtArray);
      end);
{$ELSE}
    AItem := AJson.CopyIf(nil, DoCopyIf);
{$ENDIF}
    mmResult.Lines.Add('CopyIf来复制除了数组类型外的所有结点');
    mmResult.Lines.Add(AItem.AsJson);
    mmResult.Lines.Add('FindIf来查找指定的结点');
{$IFDEF UNICODE}
    mmResult.Lines.Add(AItem.FindIf(nil, true,
      procedure(ASender, AChild: TQJson; var Accept: Boolean; ATag: Pointer)
      begin
        Accept := (AChild.Name = 'subobj');
      end).AsJson);
{$ELSE}
    mmResult.Lines.Add(AItem.FindIf(nil, true, DoFindIf).AsJson);
{$ENDIF}
    mmResult.Lines.Add('删除上面结果中的subobj结点');
{$IFDEF UNICODE}
    AItem.DeleteIf(nil, true,
      procedure(ASender, AChild: TQJson; var Accept: Boolean; ATag: Pointer)
      begin
        Accept := (AChild.Name = 'subobj');
      end);
{$ELSE}
    AItem.DeleteIf(nil, true, DoDeleteIf);
{$ENDIF}
    mmResult.Lines.Add(AItem.AsJson);
  finally
    FreeObject(AItem);
    FreeObject(AJson);
  end;
end;

procedure TForm1.CharCall(s: PAnsiChar);
begin
  ShowMessage(s);
end;

procedure TForm1.DoCopyIf(ASender, AItem: TQJson; var Accept: Boolean;
ATag: Pointer);
begin
  Accept := (AItem.DataType <> jdtArray);
end;

procedure TForm1.DoDeleteIf(ASender, AChild: TQJson; var Accept: Boolean;
ATag: Pointer);
begin
  Accept := (AChild.Name = 'subobj');
end;

procedure TForm1.DoFindIf(ASender, AChild: TQJson; var Accept: Boolean;
ATag: Pointer);
begin
  Accept := (AChild.Name = 'subobj');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := true;
end;

function TForm1.ObjectCall(AObject: TObject): String;
begin
  Result := AObject.ToString;
end;

procedure TForm1.Panel1Click(Sender: TObject);
var
  AJson: TQJson;
begin
  // StrictJson:=True;
  AJson := TQJson.Create;
  try
    AJson.Parse('{"value":1.0}');
    ShowMessage(AJson.IntByName('value', 0).toString);
  finally
    AJson.Free;
  end;
end;

procedure TForm1.PrintRegexMatchResult(AItem: TQJson; ATag: Pointer);
begin
  mmResult.Lines.Add(AItem.Path + '=>' + AItem.AsString);
end;

end.
