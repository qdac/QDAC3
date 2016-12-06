unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Data.DB, Datasnap.DBClient;

type
  TForm2 = class(TForm)
    Panel1: TPanel;
    mmResult: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    dlgSave: TSaveDialog;
    dlgOpen: TOpenDialog;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button8: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button9: TButton;
    Button13: TButton;
    Button14: TButton;
    Button15: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
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

uses qstring, qmsgpack, qjson, strutils;
{$R *.dfm}

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

  TSubRec = record
    I: Integer;
  end;

  TRec = record
    I: Integer;
    Arr: TArray<string>;
    Arr2: TArray<TSubRec>;
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
  AJson, AItem: TQMsgPack;
  S: String;
begin
  AJson := TQMsgPack.Create;
  try
    AJson.Add('Item1', 0);
    AJson.Add('Item2', true);
    AJson.Add('Item3', 1.23);
    for AItem in AJson do
    begin
      S := S + AItem.Name + ' => ' + AItem.AsString + #13#10;
    end;
    mmResult.Lines.Add(S);
  finally
    AJson.Free;
  end;
end;

procedure TForm2.Button11Click(Sender: TObject);
var
  AMsgPack: TQMsgPack;
  ABytes: TBytes;
begin
  AMsgPack := TQMsgPack.Create;
  try
    // 强制路径访问，如果路径不存在，则会创建路径，路径分隔符可以是./\之一
    AMsgPack.ForcePath('demo1.item[0].name').AsString := '1';
    AMsgPack.ForcePath('demo1.item[1].name').AsString := '100';
    try
      ShowMessage('下面正常会抛出一个异常');
      AMsgPack.ForcePath('demo1[0].item[1]').AsString := '200';
    except
      // 这个应该抛出异常，demo1是对象不是数组，所以是错的
    end;
    // 访问第6个元素，前5个元素会自动设置为null
    AMsgPack.ForcePath('demo2[5]').AsInteger := 103;
    // 强制创建一个空数组对象，然后调用Add方法添加子成员
    AMsgPack.ForcePath('demo3[]').Add('Value', 1.23);
    // 下面的代码将生成"demo4":[{"Name":"demo4"}]的结果
    AMsgPack.ForcePath('demo4[].Name').AsString := 'demo4';
    // 直接强制路径存在
    AMsgPack.ForcePath('demo5[0]').AsString := 'demo5';

    mmResult.Text := AMsgPack.AsString;
  finally
    AMsgPack.Free;
  end;
end;

procedure TForm2.Button12Click(Sender: TObject);
var
  AMsgPack: TQMsgPack;
  AList: TQMsgPackList;
begin
  AMsgPack := TQMsgPack.Create;
  try
    with AMsgPack.ForcePath('object') do
    begin
      Add('name', 'object_1');
      Add('subobj').Add('name', 'subobj_1');
      Add('subarray', mptArray).AsVariant := VarArrayOf([1, 3, 4]);
    end;
    with AMsgPack.Add('array', mptArray) do
    begin
      Add.AsInteger := 100;
      Add.AsInteger := 200;
      Add.AsInteger := 300;
      Add.Add('name', 'object');
    end;
    AList := TQMsgPackList.Create;
    AMsgPack.ItemByRegex('na.+', AList, true);
    mmResult.Lines.Add('ItemByRegex返回' + IntToStr(AList.Count) + '项结果');
    AList.Free;
    mmResult.Lines.Add('ItemByPath(''object\subobj\name'')=' +
      AMsgPack.ItemByPath('object\subobj\name').AsString);
    mmResult.Lines.Add('ItemByPath(''object\subarray[1]'')=' +
      AMsgPack.ItemByPath('object\subarray[1]').AsString);
    mmResult.Lines.Add('ItemByPath(''array[1]'')=' +
      AMsgPack.ItemByPath('array[1]').AsString);
    mmResult.Lines.Add('ItemByPath(''array[3].name'')=' +
      AMsgPack.ItemByPath('array[3].name').AsString);
    mmResult.Lines.Add('ItemByPath(''object[0]'')=' +
      AMsgPack.ItemByPath('object[0]').AsString);
  finally
    AMsgPack.Free;
  end;
end;

procedure TForm2.Button13Click(Sender: TObject);
var
  AMsgPack, AChild: TQMsgPack;
  B: TBytes;
begin
  AMsgPack := TQMsgPack.Create;
  AMsgPack.ForcePath('a.b.c[].d').AsString := 'OK';
  if AMsgPack.HasChild('a.b.c[0].d', AChild) then
    mmResult.Text := AMsgPack.AsJson + #13#10 + '子结点 a.b.c[0].d 存在，值为：' +
      AChild.AsString
  else
    mmResult.Text := AMsgPack.AsJson + #13#10 + '子结点 a.b.c[0].d 不存在';
  AMsgPack.Free;

end;

procedure TForm2.Button14Click(Sender: TObject);
var
  APack, AItem: TQMsgPack;
begin
  APack := TQMsgPack.Create;
  try
    with APack.Add('ID', mptArray) do
    begin
      Add.AsString := 'Name';
      Add.AsString := 'Group';
      Add.AsString := 'Readme';
    end;
    with APack.Items[0] do
    begin
      ShowMessage(Items[0].AsString);
    end;
    AItem := APack.Add;
    AItem.KeyAsInteger := 30;
    AItem.AsInteger := 100;
    ShowMessage(APack.AsJson);
  finally
    FreeObject(APack);
  end;
end;

procedure TForm2.Button15Click(Sender: TObject);
begin
  ShowMessage(IntToStr(RightPosW('http://www.baidu.com/', '//', true)));
end;

procedure TForm2.Button1Click(Sender: TObject);
var
  APack: TQMsgPack;
  T, Speed: Cardinal;
  AFileSize: Int64;
begin
  APack := TQMsgPack.Create;
  try
    if dlgOpen.Execute then
    begin
      T := GetTickCount;
      APack.LoadFromFile(dlgOpen.FileName);
      T := GetTickCount - T;
      AFileSize := GetFileSize(dlgOpen.FileName);
      if T > 0 then
        Speed := AFileSize * 1000 div T
      else
        Speed := 0;
      mmResult.Lines.Add(dlgOpen.FileName + ' - 大小' + RollupSize(AFileSize) +
        ', 用时:' + IntToStr(T) + 'ms，速度：' + RollupSize(Speed));
    end;
  finally
    APack.Free;
  end;
end;

procedure TForm2.Button2Click(Sender: TObject);
var
  APack: TQMsgPack;
  ABytes: TBytes;
  S: String;
  I: Integer;
  AStream: TMemoryStream;
begin
  APack := TQMsgPack.Create;
  try
    APack.Add('Age', 25);
    APack.Add('Name', 'Jone smith');
    with APack.Add('Colors', mptArray) do
    begin
      Add.AsString := 'Red';
      Add.AsString := 'Yellow';
      Add.AsString := 'Blue';
      Add.AsBoolean := False;
    end;
    with APack.Add('Arm') do
    begin
      Add('Name').AsString := 'Hande';
      Add('Fat').AsBoolean := False;
      Add('Length').AsInteger := 32;
    end;
    SetLength(ABytes, 10);
    for I := 0 to 9 do
      ABytes[I] := I + 1;
    APack.Add('Bytes', ABytes);
    AStream := TMemoryStream.Create;
    AStream.WriteBuffer(ABytes[0], 10);
    APack.Add('Stream').BytesFromStream(AStream, 0); // 直接从文件加载可以用BytesFromFile
    FreeObject(AStream);
    mmResult.Lines.Clear;
    S := APack.AsString;
    mmResult.Lines.Add('Text,Size(' + IntToStr(Length(S)) + 'B)');
    mmResult.Lines.Add(S);
    ABytes := APack.Encode;
    mmResult.Lines.Add('Binary,Size(' + IntToStr(Length(ABytes)) + 'B)');
    mmResult.Lines.Add(qstring.BinToHex(@ABytes[0], Length(ABytes)));
    mmResult.Lines.Add('[AsString]');
    mmResult.Lines.Add(APack.AsString);
    mmResult.Lines.Add('[AsJson]');
    mmResult.Lines.Add(APack.AsJson);
  finally
    APack.Free;
  end;
end;

procedure TForm2.Button3Click(Sender: TObject);
var
  AMsgPack: TQMsgPack;
  I, J: Integer;
  S: TBytes;
  str: String;
const
  BinarySizes: array [0 .. 8] of Integer = (2, 3, 4, 6, 8, 15, 16,
    65535, 65536);
begin
  AMsgPack := TQMsgPack.Create;
  try
    for I := 0 to 8 do
    begin
      SetLength(S, BinarySizes[I]);
      for J := 0 to BinarySizes[I] - 1 do
        S[J] := J mod 255;
      AMsgPack.Add('bin_' + IntToStr(BinarySizes[I]), S);
    end;
    // 浮点
    AMsgPack.Add('single', 1.23);
    AMsgPack.Add('double', 100.001);
    // 整数
    AMsgPack.Add('int_8_127', 127);
    AMsgPack.Add('int_8_n1', -1);
    AMsgPack.Add('int_8_n32', -32);
    AMsgPack.Add('int_8_n128', -128);
    AMsgPack.Add('int_8_255', 255);
    AMsgPack.Add('int_16_w2', 32767);
    AMsgPack.Add('int_16_n2', -32768);
    AMsgPack.Add('int_32_w4', 32768);
    AMsgPack.Add('int_32_n4', -32769);
    AMsgPack.Add('int_64_w2', Int64(MaxInt) + 1);
    AMsgPack.Add('int_64_n2', Int64(-MaxInt) - 10);
    // 布尔
    AMsgPack.Add('bool_true', true);
    AMsgPack.Add('bool_false', False);
    // NIL
    AMsgPack.Add('null');
    // 字符串
    AMsgPack.Add('str_short', '0123456789012345678901234567890'); // 31
    SetLength(str, 255);
    for I := 1 to 254 do
      str[I] := Char(Ord('a') + (I mod 32));
    AMsgPack.Add('str_b255', str);
    SetLength(str, 65535);
    for I := 1 to 65534 do
      str[I] := Char(Ord('a') + (I mod 32));
    AMsgPack.Add('str_b65535', str);
    SetLength(str, 65537);
    for I := 1 to 65536 do
      str[I] := Char(Ord('a') + (I mod 32));
    AMsgPack.Add('str_b65537', str);
    // Small array
    with AMsgPack.Add('array_I8', mptArray) do
    begin
      for I := 0 to 14 do
        Add.AsInteger := I;
    end;
    with AMsgPack.Add('array_U8', mptArray) do
    begin
      for I := 0 to 16 do
        Add.AsInteger := I;
    end;
    with AMsgPack.Add('array_I16', mptArray) do
    begin
      for I := 0 to 320 do
        Add.AsInteger := I;
    end;
    with AMsgPack.Add('array_I32', mptArray) do
    begin
      for I := 0 to 65536 do
        Add.AsInteger := I;
    end;
    with AMsgPack.Add('submap_I8') do
    begin
      for I := 0 to 14 do
        Add('item_' + IntToStr(I), I);
    end;
    with AMsgPack.Add('submap_U8') do
    begin
      for I := 0 to 16 do
        Add('item_' + IntToStr(I), I);
    end;
    with AMsgPack.Add('submap_I16') do
    begin
      for I := 0 to 320 do
        Add('item_' + IntToStr(I), I);
    end;
    with AMsgPack.Add('submap_I32') do
    begin
      for I := 0 to 65536 do
        Add('item_' + IntToStr(I), I);
    end;
    if not dlgSave.Execute then
      exit;
    AMsgPack.SaveToFile(dlgSave.FileName);
  finally
    AMsgPack.Free;
  end;
end;

procedure TForm2.Button4Click(Sender: TObject);
var
  AMsgPack: TQMsgPack;
  I: Integer;
  T: Cardinal;
begin
  AMsgPack := TQMsgPack.Create;
  try
    T := GetTickCount;
    for I := 0 to 100000 do
      AMsgPack.Add('_' + IntToStr(I), Now);
    T := GetTickCount - T;
    mmResult.Clear;
    mmResult.Lines.Add('添加100,000个结点用时:' + IntToStr(T) + 'ms');
  finally
    AMsgPack.Free;
  end;
end;

procedure TForm2.Button5Click(Sender: TObject);
var
  AJson: TQMsgPack;
  I: Integer;
  T1, T2: Cardinal;
  Speed: Cardinal;
begin
  if dlgSave.Execute then
  begin
    AJson := TQMsgPack.Create;
    try
      mmResult.Clear;
      T1 := GetTickCount;
      with AJson.Add('Integers') do
      begin
        for I := 0 to 2000000 do
          Add('Node' + IntToStr(I)).AsInteger := I;
      end;
      T1 := GetTickCount - T1;
      T2 := GetTickCount;
      AJson.SaveToFile(dlgSave.FileName);
      T2 := GetTickCount - T2;
      if T2 > 0 then
        Speed := (GetFileSize(dlgSave.FileName) * 1000 div T2)
      else
        Speed := 0;
      mmResult.Lines.Add('生成200万结点用时' + IntToStr(T1) + 'ms,保存用时:' + IntToStr(T2)
        + 'ms，速度：' + RollupSize(Speed));
    finally
      AJson.Free;
    end;
  end;
end;

function StrEqualText(Text: PAnsiChar; SearchText: PAnsiChar; MaxLen: Integer;
  IgnoreCase: Boolean): Boolean;
var
  I: Integer;
begin
  if IgnoreCase then
    Result := StrLIComp(Text, SearchText, MaxLen) = 0
  else
  begin
    Result := False;
    for I := 0 to MaxLen - 1 do
      if (Text[I] = #0) or (Text[I] <> SearchText[I]) then
        exit;
    Result := true;
  end;
end;

function FastStringReplace(const Text, SearchText, ReplaceText: AnsiString;
  ReplaceAll, IgnoreCase: Boolean): AnsiString;
var
  LenSearchText, LenReplaceText, LenText: Integer;
  Index, Len, StartIndex: Integer;
begin
  LenSearchText := Length(SearchText);
  LenReplaceText := Length(ReplaceText);
  LenText := Length(Text);
  if LenSearchText = 0 then
  begin
    Result := Text;
    exit;
  end;

  if ReplaceAll then
  begin
    if LenReplaceText - LenSearchText > 0 then
      SetLength(Result, LenText + (LenReplaceText - LenSearchText) *
        (LenText div LenSearchText))
    else
      SetLength(Result, LenText);
  end
  else
    SetLength(Result, LenText + (LenReplaceText - LenSearchText));

  Len := 0;
  StartIndex := 1;
  for Index := 1 to LenText do
  begin
    if StrEqualText(PAnsiChar(Text) + Index - 1, Pointer(SearchText),
      LenSearchText, IgnoreCase) then
    begin
      if Index > StartIndex then
      begin
        Move(Text[StartIndex], Result[Len + 1], Index - StartIndex);
        Inc(Len, Index - StartIndex);
      end;
      StartIndex := Index + LenSearchText;
      if LenReplaceText > 0 then
      begin
        Move(ReplaceText[1], Result[Len + 1], LenReplaceText);
        Inc(Len, LenReplaceText);
      end;

      if not ReplaceAll then
        Break;
    end;
  end;

  Index := LenText + 1;
  if Index > StartIndex then
  begin
    Move(Text[StartIndex], Result[Len + 1], Index - StartIndex);
    Inc(Len, Index - StartIndex);
  end;
  SetLength(Result, Len);
end;

procedure TForm2.Button6Click(Sender: TObject);
var
  ARec, ARec1: TRttiTestSubRecord;
  AMsgPack, ACopy, AItem: TQMsgPack;
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
  ARec.CardinalVal := 6;
  ARec.ShortVal := 7;
  ARec.CurrVal := 8.9;
  ARec.EnumVal := {$IFDEF UNICODE}TAlign.{$ENDIF}alTop;
  ARec.CharVal := 'A';
  ARec.VarVal := VarArrayOf(['VariantArray', 1, 2.5, true, False]);
  SetLength(ARec.ArrayVal, 3);
  ARec.ArrayVal[0] := 100;
  ARec.ArrayVal[1] := 101;
  ARec.ArrayVal[2] := 102;
  AMsgPack := TQMsgPack.Create;
  try
{$IFDEF UNICODE}
    AItem := AMsgPack.Add('Record');
    AItem.FromRecord(ARec);
    ACopy := AItem.Copy;
    ACopy.ItemByName('Int64Val').AsInt64 := 100;
    ACopy.ItemByPath('UStr').AsString := 'UnicodeString-ByJson';
    ACopy.ItemByPath('AStr').AsString := 'AnsiString-ByJson';
    ACopy.ItemByPath('SStr').AsString := 'ShortString-ByJson';
    ACopy.ItemByPath('EnumVal').AsString := 'alBottom';
    ACopy.ItemByPath('SetVal').AsString := '[biHelp]';
    ACopy.ItemByPath('ArrayVal').AsVariant := VarArrayOf([10, 30, 15]);
    // 或者
    // with ACopy.ItemByPath('ArrayVal') do
    // begin
    // Clear;
    // Add.AsInteger:=10;
    // Add.AsInteger:=30;
    // Add.AsInteger:=15;
    // end;
    // ACopy.ItemByPath('VarVal').AsVariant:=VarArrayOf(['By Json',3,4,false,true]);
    // ACopy.ToRecord<TRttiTestSubRecord>(ARec1);
    FreeObject(ACopy);
    AMsgPack.Add('NewRecord').FromRecord(ARec1);
{$ENDIF}
    mmResult.Lines.Add(AMsgPack.AsString);
  finally
    AMsgPack.Free;
  end;
{$ENDIF}
end;

procedure TForm2.Button7Click(Sender: TObject);
var
  AMsgPack: TQMsgPack;
  V: OleVariant;
  cds: TClientDataSet;
  AStream: TMemoryStream;
begin
  AMsgPack := TQMsgPack.Create;
  cds := TClientDataSet.Create(nil);
  try
    cds.FieldDefs.Add('Id', ftInteger);
    cds.FieldDefs.Add('Name', ftString, 20);
    cds.CreateDataSet;
    cds.Append;
    cds.FieldByName('Id').AsInteger := 1;
    cds.FieldByName('Name').AsString := 'QDAC';
    cds.Post;
    AMsgPack.AsVariant := cds.Data;
    mmResult.Lines.Add('转换为MsgPack的内容');
    mmResult.Lines.Add(AMsgPack.AsString);
    cds.Close;
    cds.Data := AMsgPack.AsVariant;
    AStream := TMemoryStream.Create;
    cds.SaveToStream(AStream, dfXML);
    mmResult.Lines.Add('从MsgPack中重新载入');
    AStream.Position := 0;
    mmResult.Lines.Add(LoadTextW(AStream));
    AStream.Free;
    cds.Free;
  finally
    FreeObject(AMsgPack);
  end;
end;

procedure TForm2.Button8Click(Sender: TObject);
var
  AMsgPack, AItem: TQMsgPack;
  I: Integer;
  DynArray: array of Integer;
begin
  AMsgPack := TQMsgPack.Create;
  try
    // 添加数组元素的N种方式演示
    // 1. 直接添加数组
    AMsgPack.Add('AddArray', ['Item1', 100, Null, true, False, 123.4]);
    // 2. 直接用VarArrayOf赋值
    AMsgPack.Add('AsVariant').AsVariant :=
      VarArrayOf(['Item1', 100, Null, true, False, 123.4]);
    // 动态数组
    SetLength(DynArray, 5);
    DynArray[0] := 100;
    DynArray[1] := 200;
    DynArray[2] := 300;
    DynArray[3] := 400;
    DynArray[4] := 500;
    AMsgPack.Add('DynArray').AsVariant := DynArray;
    // 5. 手动逐个添加元素
    with AMsgPack.Add('Manul') do
    begin
      DataType := mptArray;
      Add.AsString := 'Item1';
      Add.AsInteger := 100;
      Add;
      Add.AsBoolean := true;
      Add.AsBoolean := False;
      Add.AsFloat := 123.4;
    end;
    // 添加对象数组和上面类型，只是子结点换成是对象就可以了
    AMsgPack.Add('Object', [TQMsgPack.Create.Add('Item1', 100).Parent,
      TQMsgPack.Create.Add('Item2', true).Parent]);
    mmResult.Lines.Add(AMsgPack.AsString);
    // 访问数组中的元素
    mmResult.Lines.Add('使用for in枚举数组Manul的元素值');
    I := 0;
    for AItem in AMsgPack.ItemByName('Manul') do
    begin
      mmResult.Lines.Add('Manul[' + IntToStr(I) + ']=' + AItem.AsString);
      Inc(I);
    end;
    mmResult.Lines.Add('使用普通for循环枚举数组Manul的元素值');
    AItem := AMsgPack.ItemByName('Manul');
    for I := 0 to AItem.Count - 1 do
      mmResult.Lines.Add('Manul[' + IntToStr(I) + ']=' + AItem[I].AsString);
  finally
    FreeObject(AMsgPack);
  end;
end;

procedure TForm2.Button9Click(Sender: TObject);
var
  APack: TQMsgPack;
  AJson: TQJson;
  ABytes: TBytes;
  S: String;
  I: Integer;
  AStream: TMemoryStream;
begin
  APack := TQMsgPack.Create;
  try
    APack.Add('EmptyObject', mptMap);
    APack.Add('EmptyArray', mptArray);
    APack.Add('Age', 25);
    APack.Add('Name', 'Jone smith');
    with APack.Add('Colors', mptArray) do
    begin
      Add.AsString := 'Red';
      Add.AsString := 'Yellow';
      Add.AsString := 'Blue';
      Add.AsBoolean := False;
    end;
    with APack.Add('Arm') do
    begin
      Add('Name').AsString := 'Hande';
      Add('Fat').AsBoolean := False;
      Add('Length').AsInteger := 32;
    end;
    SetLength(ABytes, 10);
    for I := 0 to 9 do
      ABytes[I] := I + 1;
    APack.Add('Bytes', ABytes);
    AStream := TMemoryStream.Create;
    AStream.WriteBuffer(ABytes[0], 10);
    APack.Add('Stream').BytesFromStream(AStream, 0); // 直接从文件加载可以用BytesFromFile
    FreeObject(AStream);
    mmResult.Lines.Clear;
    mmResult.Lines.Add('【AsString】');
    mmResult.Lines.Add(APack.AsString);
    mmResult.Lines.Add('【AsJson】');
    S := APack.AsJson;
    mmResult.Lines.Add(S);
    AJson := TQJson.Create;
    AJson.Parse(S);
    mmResult.Lines.Add('【QJson格式化后结果】');
    mmResult.Lines.Add(AJson.AsJson);
    FreeObject(AJson);
  finally
    APack.Free;
  end;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := true;
end;

procedure TForm2.Panel1Click(Sender: TObject);
var
  AMsgPack, AItem: TQMsgPack;
  AStream: TMemoryStream;
begin
  AMsgPack := TQMsgPack.Create;
  try
    // with AMsgPack.Add('DATA') do
    // begin
    // Add('Type',1);
    // Add('Direction',-1);
    // end;
    // AItem:=TQMsgPack.Create;
    // AItem.AsMsgPack:=AMsgPack.AsMsgPack;
    AMsgPack.ForcePath('CMD').AsInteger := 2010;
    AMsgPack.ForcePath('From').AsInteger := 2;
    AMsgPack.ForcePath('Data.Trade.Type').AsInteger := -1;
    AMsgPack.ForcePath('Data.Trade.Direction').AsInteger := -1;
    AMsgPack.ForcePath('Data.Trade.Volum').AsInteger := -1;
    AStream := TMemoryStream.Create;
    AMsgPack.SaveToStream(AStream);
    AStream.SaveToFile('c:\test.mpk');
    AStream.Position := 0;

    AItem := TQMsgPack.Create;
    ShowMessage(BinToHex(AStream.Memory, AStream.Size));
    AItem.LoadFromStream(AStream);
    mmResult.Text := AItem.AsString;
    FreeObject(AItem);
    FreeObject(AStream);
    // ShowMessage(AMsgPack.AsString);
  finally
    FreeObject(AMsgPack);
  end;
end;

end.
