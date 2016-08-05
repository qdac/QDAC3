unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, System.Types,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, QRBTree, Generics.Collections,
  Vcl.StdCtrls, QString, QWorker, Vcl.ExtCtrls;

type

  TForm1 = class(TForm)
    mmResult: TMemo;
    Panel1: TPanel;
    Button3: TButton;
    Button1: TButton;
    Label1: TLabel;
    cbxPageSize: TComboBox;
    Button2: TButton;
    Button4: TButton;
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

  end;

var
  Form1: TForm1;

implementation

uses mydataset;
{$R *.dfm}

const
  TEST_COUNT      = 100000;
  TEST_GROUP_SIZE = 10000;

procedure TForm1.Button2Click(Sender: TObject);
var
  APaged: TQPagedList;
  I, AIdx: Integer;
  Values: array of Integer;
begin
  mmResult.Lines.Add(' 收缩测试');
  APaged := TQPagedList.Create;
  try
    randomize;
    SetLength(Values, TEST_COUNT);
    for I := 0 to TEST_COUNT - 1 do
      Values[I] := random(MaxInt);
    for I := 0 to TEST_COUNT - 1 do
    begin
      AIdx := random(APaged.Count);
      APaged.Insert(AIdx, Pointer(Values[I]));
    end;
    APaged.Clear;
    for I := 0 to TEST_COUNT - 1 do
    begin
      AIdx := random(APaged.Count);
      APaged.Insert(AIdx, Pointer(Values[I]));
    end;

    for I := 0 to TEST_COUNT - 1 do
      Values[I] := IntPtr(APaged[I]);
    mmResult.Lines.Add(' 收缩前页数:' + IntToStr(APaged.PageCount) + ',最大' +
      IntToStr(APaged.PageCount * APaged.PageSize));
    APaged.Pack;
    for I := 0 to TEST_COUNT - 1 do
      Assert(IntPtr(APaged[I]) = Values[I]);
    mmResult.Lines.Add(' 收缩后页数:' + IntToStr(APaged.PageCount) + ',最大' +
      IntToStr(APaged.PageCount * APaged.PageSize));
  finally
    FreeObject(APaged);
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  APageSize: Integer;
  procedure SortedInsert;
  var
    APaged: TQPagedList;
    AList: TList<Integer>;
    ARBTree: TQRbTree;
    T1, T2, T3: array [0 .. 9] of Int64;
    TT1, TT2, TT3: Int64;
    I, AIdx: Integer;
    Values: array of Integer;
    S: String;
  begin
    mmResult.Lines.Add('');
    mmResult.Lines.Add(' 即时排序插入测试');
    APaged := TQPagedList.Create(APageSize);
    AList := TList<Integer>.Create;
    ARBTree := TQRbTree.Create(RBDefaultComparor.IntComp);
    try
      randomize;
      APaged.Sort(
        procedure(p1, p2: Pointer; var Result: Integer)
        begin
          Result := IntPtr(p1) - IntPtr(p2);
        end);
      SetLength(Values, TEST_COUNT);
      for I := 0 to TEST_COUNT - 1 do
        Values[I] := random(MaxInt);
      T1[0] := GetTimeStamp;
      TT1 := T1[0];
      for I := 0 to TEST_COUNT - 1 do
      begin
        AList.BinarySearch(Values[I], AIdx);
        AList.Insert(AIdx, Values[I]);
        if ((I + 1) mod (TEST_COUNT div 10)) = 0 then
        begin
          T1[I div TEST_GROUP_SIZE] := GetTimeStamp - T1[I div TEST_GROUP_SIZE];
          if (I div TEST_GROUP_SIZE) < 9 then
            T1[I div TEST_GROUP_SIZE + 1] := GetTimeStamp;
        end;
      end;
      TT1 := GetTimeStamp - TT1;
      mmResult.Lines.Add('  TList 总计用时 ' + FormatFloat('0.0', TT1 / 10)
        + ' ms :');
      S := '    ';
      for I := 0 to 9 do
        S := S + FormatFloat('0.0', T1[I] / 10) + #9;
      mmResult.Lines.Add(S);
      TT2 := GetTimeStamp;
      T2[0] := TT2;
      for I := 0 to TEST_COUNT - 1 do
      begin
        APaged.Add(Pointer(Values[I]));
        if ((I + 1) mod (TEST_COUNT div 10)) = 0 then
        begin
          T2[I div TEST_GROUP_SIZE] := GetTimeStamp - T2[I div TEST_GROUP_SIZE];
          if (I div TEST_GROUP_SIZE) < 9 then
            T2[I div TEST_GROUP_SIZE + 1] := GetTimeStamp;
        end;
      end;
      TT2 := GetTimeStamp - TT2;
      mmResult.Lines.Add('  TQPagedList 总计用时 ' + FormatFloat('0.0', TT2 / 10)
        + ' ms :');
      S := '    ';
      for I := 0 to 9 do
        S := S + FormatFloat('0.0', T2[I] / 10) + #9;
      mmResult.Lines.Add(S);
      TT3 := GetTimeStamp;
      T3[0] := TT3;
      for I := 0 to TEST_COUNT - 1 do
      begin
        ARBTree.Insert(Pointer(Values[I]));
        if ((I + 1) mod (TEST_COUNT div 10)) = 0 then
        begin
          T3[I div TEST_GROUP_SIZE] := GetTimeStamp - T3[I div TEST_GROUP_SIZE];
          if (I div TEST_GROUP_SIZE) < 9 then
            T3[I div TEST_GROUP_SIZE + 1] := GetTimeStamp;
        end;
      end;
      TT3 := GetTimeStamp - TT3;
      mmResult.Lines.Add('  TQRBTree 总计用时 ' + FormatFloat('0.0', TT3 / 10)
        + ' ms :');
      S := '    ';
      for I := 0 to 9 do
        S := S + FormatFloat('0.0', T3[I] / 10) + #9;
      mmResult.Lines.Add(S);
      mmResult.Lines.Add('  即时排序插入测试完成:');
      S := '    TQRBTree 用时计为 1，则 TList 用时为 ' + FormatFloat('0.00', TT1 / TT3) +
        ' 倍，TQPagedList 用时为 ' + FormatFloat('0.00', TT2 / TT3) + ' 倍';
      mmResult.Lines.Add(S);
    finally
      FreeObject(APaged);
      FreeObject(AList);
      FreeObject(ARBTree);
    end;
  end;
  procedure InsertFirst;
  var
    APaged: TQPagedList;
    AList: TList<Integer>;
    T1, T2: array [0 .. 9] of Int64;
    TT1, TT2: Int64;
    I, AIdx: Integer;
    Values: array of Integer;
    S: String;
  begin
    mmResult.Lines.Add('');
    mmResult.Lines.Add(' 插入初始位置测试');
    APaged := TQPagedList.Create(APageSize);
    AList := TList<Integer>.Create;
    try
      randomize;
      SetLength(Values, TEST_COUNT);
      for I := 0 to TEST_COUNT - 1 do
        Values[I] := random(MaxInt);
      T1[0] := GetTimeStamp;
      TT1 := T1[0];
      AIdx := 0;
      for I := 0 to TEST_COUNT - 1 do
      begin
        AList.Insert(0, Values[I]);
        if ((I + 1) mod (TEST_COUNT div 10)) = 0 then
        begin
          T1[AIdx] := GetTimeStamp - T1[AIdx];
          Inc(AIdx);
          if AIdx <= 9 then
            T1[AIdx] := GetTimeStamp;
        end;
      end;
      TT1 := GetTimeStamp - TT1;
      mmResult.Lines.Add('  TList 总计用时 ' + FormatFloat('0.0', TT1 / 10)
        + ' ms :');
      S := '    ';
      for I := 0 to 9 do
        S := S + FormatFloat('0.0', T1[I] / 10) + #9;
      mmResult.Lines.Add(S);
      TT2 := GetTimeStamp;
      T2[0] := TT2;
      AIdx := 0;
      for I := 0 to TEST_COUNT - 1 do
      begin
        APaged.Insert(0, Pointer(Values[I]));
        if ((I + 1) mod (TEST_COUNT div 10)) = 0 then
        begin
          T2[AIdx] := GetTimeStamp - T2[AIdx];
          Inc(AIdx);
          if AIdx <= 9 then
            T2[AIdx] := GetTimeStamp;
        end;
      end;
      TT2 := GetTimeStamp - TT2;
      mmResult.Lines.Add('  TQPagedList 总计用时 ' + FormatFloat('0.0', TT2 / 10)
        + ' ms :');
      S := '    ';
      for I := 0 to 9 do
        S := S + FormatFloat('0.0', T2[I] / 10) + #9;
      mmResult.Lines.Add(S);
      mmResult.Lines.Add(' 插入初始位置测试完成:');
      mmResult.Lines.Add('    TQPagedList 用时为 1，则 TList 用时为 ' +
        FormatFloat('0.00', TT1 / TT2) + ' 倍');
    finally
      FreeObject(APaged);
      FreeObject(AList);
    end;
  end;
  procedure Append;
  var
    APaged: TQPagedList;
    AList: TList<Integer>;
    T1, T2: array [0 .. 9] of Int64;
    TT1, TT2: Int64;
    I, AIdx: Integer;
    Values: array of Integer;
    S: String;
  begin
    mmResult.Lines.Add('');
    mmResult.Lines.Add(' 追加数据测试');
    APaged := TQPagedList.Create(APageSize);
    AList := TList<Integer>.Create;
    try
      randomize;
      SetLength(Values, TEST_COUNT);
      for I := 0 to TEST_COUNT - 1 do
        Values[I] := random(MaxInt);
      T1[0] := GetTimeStamp;
      TT1 := T1[0];
      AIdx := 0;
      for I := 0 to TEST_COUNT - 1 do
      begin
        AList.Add(Values[I]);
        if ((I + 1) mod (TEST_COUNT div 10)) = 0 then
        begin
          T1[AIdx] := GetTimeStamp - T1[AIdx];
          Inc(AIdx);
          if AIdx <= 9 then
            T1[AIdx] := GetTimeStamp;
        end;
      end;
      TT1 := GetTimeStamp - TT1;
      mmResult.Lines.Add('  TList 总计用时 ' + FormatFloat('0.0', TT1 / 10)
        + ' ms :');
      S := '    ';
      for I := 0 to 9 do
        S := S + FormatFloat('0.0', T1[I] / 10) + #9;
      mmResult.Lines.Add(S);
      TT2 := GetTimeStamp;
      T2[0] := TT2;
      AIdx := 0;
      for I := 0 to TEST_COUNT - 1 do
      begin
        APaged.Add(Pointer(Values[I]));
        if ((I + 1) mod (TEST_COUNT div 10)) = 0 then
        begin
          T2[AIdx] := GetTimeStamp - T2[AIdx];
          Inc(AIdx);
          if AIdx <= 9 then
            T2[AIdx] := GetTimeStamp;
        end;
      end;
      TT2 := GetTimeStamp - TT2;
      mmResult.Lines.Add('  TQPagedList 总计用时 ' + FormatFloat('0.0', TT2 / 10)
        + ' ms :');
      S := '    ';
      for I := 0 to 9 do
        S := S + FormatFloat('0.0', T2[I] / 10) + #9;
      mmResult.Lines.Add(S);
      mmResult.Lines.Add(' 追加测试完成:');
      if TT2 <> 0 then
        mmResult.Lines.Add('    TQPagedList 用时为 1，则 TList 用时为 ' +
          FormatFloat('0.00', TT1 / TT2) + ' 倍')
      else
        mmResult.Lines.Add('    你的机器太快了，以至于 TQPagedList 用时连0.1ms都不到就完成了。');
    finally
      FreeObject(APaged);
      FreeObject(AList);
    end;
  end;
  procedure RandomInsert;
  var
    APaged: TQPagedList;
    AList: TList<Integer>;
    T1, T2: array [0 .. 9] of Int64;
    TT1, TT2: Int64;
    I, AIdx: Integer;
    Values: array of Integer;
    S: String;
  begin
    mmResult.Lines.Add('');
    mmResult.Lines.Add(' 随机插入测试');
    APaged := TQPagedList.Create(APageSize);
    AList := TList<Integer>.Create;
    try
      randomize;
      SetLength(Values, TEST_COUNT);
      for I := 0 to TEST_COUNT - 1 do
        Values[I] := random(MaxInt);
      T1[0] := GetTimeStamp;
      TT1 := T1[0];
      AIdx := 0;
      for I := 0 to TEST_COUNT - 1 do
      begin
        AList.Insert(random(AList.Count), Values[I]);
        if ((I + 1) mod (TEST_COUNT div 10)) = 0 then
        begin
          T1[AIdx] := GetTimeStamp - T1[AIdx];
          Inc(AIdx);
          if AIdx <= 9 then
            T1[AIdx] := GetTimeStamp;
        end;
      end;
      TT1 := GetTimeStamp - TT1;
      mmResult.Lines.Add('  TList 总计用时 ' + FormatFloat('0.0', TT1 / 10)
        + ' ms :');
      S := '    ';
      for I := 0 to 9 do
        S := S + FormatFloat('0.0', T1[I] / 10) + #9;
      mmResult.Lines.Add(S);
      TT2 := GetTimeStamp;
      T2[0] := TT2;
      AIdx := 0;
      for I := 0 to TEST_COUNT - 1 do
      begin
        APaged.Insert(random(AList.Count), Pointer(Values[I]));
        if ((I + 1) mod (TEST_COUNT div 10)) = 0 then
        begin
          T2[AIdx] := GetTimeStamp - T2[AIdx];
          Inc(AIdx);
          if AIdx <= 9 then
            T2[AIdx] := GetTimeStamp;
        end;
      end;
      TT2 := GetTimeStamp - TT2;
      mmResult.Lines.Add('  TQPagedList 总计用时 ' + FormatFloat('0.0', TT2 / 10)
        + ' ms :');
      S := '    ';
      for I := 0 to 9 do
        S := S + FormatFloat('0.0', T2[I] / 10) + #9;
      mmResult.Lines.Add(S);
      mmResult.Lines.Add(' 随机插入测试完成:');
      mmResult.Lines.Add('    TQPagedList 用时为 1，则 TList 用时为 ' +
        FormatFloat('0.00', TT1 / TT2) + ' 倍');
    finally
      FreeObject(APaged);
      FreeObject(AList);
    end;
  end;

  procedure DeleteFirst;
  var
    APaged: TQPagedList;
    AList: TList<Integer>;
    T1, T2: array [0 .. 9] of Int64;
    TT1, TT2: Int64;
    I, AIdx: Integer;
    Values: array of Integer;
    S: String;
  begin
    mmResult.Lines.Add('');
    mmResult.Lines.Add(' 删除首个元素测试');
    APaged := TQPagedList.Create(APageSize);
    AList := TList<Integer>.Create;
    try
      randomize;
      SetLength(Values, TEST_COUNT);
      for I := 0 to TEST_COUNT - 1 do
      begin
        Values[I] := random(MaxInt);
        AList.Add(Values[I]);
        APaged.Add(Pointer(Values[I]));
      end;
      T1[0] := GetTimeStamp;
      TT1 := T1[0];
      AIdx := 0;
      for I := 0 to TEST_COUNT - 1 do
      begin
        AList.Delete(0);
        if ((I + 1) mod (TEST_COUNT div 10)) = 0 then
        begin
          T1[AIdx] := GetTimeStamp - T1[AIdx];
          Inc(AIdx);
          if AIdx <= 9 then
            T1[AIdx] := GetTimeStamp;
        end;
      end;
      TT1 := GetTimeStamp - TT1;
      mmResult.Lines.Add('  TList 总计用时 ' + FormatFloat('0.0', TT1 / 10)
        + ' ms :');
      S := '    ';
      for I := 0 to 9 do
        S := S + FormatFloat('0.0', T1[I] / 10) + #9;
      mmResult.Lines.Add(S);
      TT2 := GetTimeStamp;
      T2[0] := TT2;
      AIdx := 0;
      for I := 0 to TEST_COUNT - 1 do
      begin
        APaged.Delete(0);
        if ((I + 1) mod (TEST_COUNT div 10)) = 0 then
        begin
          T2[AIdx] := GetTimeStamp - T2[AIdx];
          Inc(AIdx);
          if AIdx <= 9 then
            T2[AIdx] := GetTimeStamp;
        end;
      end;
      TT2 := GetTimeStamp - TT2;
      mmResult.Lines.Add('  TQPagedList 总计用时 ' + FormatFloat('0.0', TT2 / 10)
        + ' ms :');
      S := '    ';
      for I := 0 to 9 do
        S := S + FormatFloat('0.0', T2[I] / 10) + #9;
      mmResult.Lines.Add(S);
      mmResult.Lines.Add(' 删除首个元素测试完成:');
      mmResult.Lines.Add('    TQPagedList 用时为 1，则 TList 用时为 ' +
        FormatFloat('0.00', TT1 / TT2) + ' 倍');
    finally
      FreeObject(APaged);
      FreeObject(AList);
    end;
  end;
  procedure RandomDelete;
  var
    APaged: TQPagedList;
    AList: TList<Integer>;
    T1, T2: array [0 .. 9] of Int64;
    TT1, TT2: Int64;
    I, AIdx: Integer;
    Values: array of Integer;
    S: String;
  begin
    mmResult.Lines.Add('');
    mmResult.Lines.Add(' 删除随机元素测试');
    APaged := TQPagedList.Create(APageSize);
    AList := TList<Integer>.Create;
    try
      randomize;
      SetLength(Values, TEST_COUNT);
      for I := 0 to TEST_COUNT - 1 do
      begin
        Values[I] := random(MaxInt);
        AList.Add(Values[I]);
        APaged.Add(Pointer(Values[I]));
      end;
      T1[0] := GetTimeStamp;
      TT1 := T1[0];
      AIdx := 0;
      for I := 0 to TEST_COUNT - 1 do
      begin
        AList.Delete(random(AList.Count));
        if ((I + 1) mod (TEST_COUNT div 10)) = 0 then
        begin
          T1[AIdx] := GetTimeStamp - T1[AIdx];
          Inc(AIdx);
          if AIdx <= 9 then
            T1[AIdx] := GetTimeStamp;
        end;
      end;
      TT1 := GetTimeStamp - TT1;
      mmResult.Lines.Add('  TList 总计用时 ' + FormatFloat('0.0', TT1 / 10)
        + ' ms :');
      S := '    ';
      for I := 0 to 9 do
        S := S + FormatFloat('0.0', T1[I] / 10) + #9;
      mmResult.Lines.Add(S);
      TT2 := GetTimeStamp;
      T2[0] := TT2;
      AIdx := 0;
      for I := 0 to TEST_COUNT - 1 do
      begin
        APaged.Delete(random(AList.Count));
        if ((I + 1) mod (TEST_COUNT div 10)) = 0 then
        begin
          T2[AIdx] := GetTimeStamp - T2[AIdx];
          Inc(AIdx);
          if AIdx <= 9 then
            T2[AIdx] := GetTimeStamp;
        end;
      end;
      TT2 := GetTimeStamp - TT2;
      mmResult.Lines.Add('  TQPagedList 总计用时 ' + FormatFloat('0.0', TT2 / 10)
        + ' ms :');
      S := '    ';
      for I := 0 to 9 do
        S := S + FormatFloat('0.0', T2[I] / 10) + #9;
      mmResult.Lines.Add(S);
      mmResult.Lines.Add(' 删除随机元素测试完成:');
      mmResult.Lines.Add('    TQPagedList 用时为 1，则 TList 用时为 ' +
        FormatFloat('0.00', TT1 / TT2) + ' 倍');
    finally
      FreeObject(APaged);
      FreeObject(AList);
    end;
  end;
  procedure RandomAccess;
  var
    APaged: TQPagedList;
    AList: TList<Integer>;
    T1, T2: Int64;
    I: Integer;
    Values: array of Integer;
    AIndexes: array of Integer;
  begin
    mmResult.Lines.Add('');
    mmResult.Lines.Add(' 随机访问元素测试');
    APaged := TQPagedList.Create(APageSize);
    AList := TList<Integer>.Create;
    try
      randomize;
      SetLength(Values, TEST_COUNT);
      SetLength(AIndexes, TEST_COUNT);
      for I := 0 to TEST_COUNT - 1 do
      begin
        Values[I] := random(MaxInt);
        AIndexes[I] := random(TEST_COUNT);
        AList.Add(Values[I]);
        APaged.Add(Pointer(Values[I]));
      end;
      T1 := GetTimeStamp;
      for I := 0 to TEST_COUNT - 1 do
      begin
        AList[AIndexes[I]];
      end;
      T1 := GetTimeStamp - T1;
      mmResult.Lines.Add('  TList 总计用时 ' + FormatFloat('0.0', T1 / 10)
        + ' ms :');
      T2 := GetTimeStamp;
      for I := 0 to TEST_COUNT - 1 do
        APaged[AIndexes[I]];
      T2 := GetTimeStamp - T2;
      mmResult.Lines.Add('  TQPagedList 总计用时 ' + FormatFloat('0.0', T2 / 10)
        + ' ms :');
      mmResult.Lines.Add(' 随机访问元素测试完成:');
      mmResult.Lines.Add('    TQPagedList 用时为 1，则 TList 用时为 ' +
        FormatFloat('0.00', T1 / T2) + ' 倍');
    finally
      FreeObject(APaged);
      FreeObject(AList);
    end;
  end;

begin
  APageSize := 128 shl cbxPageSize.ItemIndex;
  mmResult.Lines.Clear;
  mmResult.Lines.Add('开始测试(页大小' + IntToStr(APageSize) + ')……');
  mmResult.Lines.Add('测试结果为操作完成用时，每10000条统计一次用时，单位为毫秒');
  InsertFirst;
  Append;
  RandomInsert;
  SortedInsert;
  DeleteFirst;
  RandomDelete;
  RandomAccess;
  mmResult.Lines.Add('测试完成。');
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  APL:TQPagedList;
  AList:TPointerList;
  I: Integer;
begin
APL:=TQPagedList.Create;
try
  SetLength(AList,2000);
  for I := 0 to 1999 do
    AList[I]:=Pointer(I);
  mmResult.Lines.Add('批量插入测试开始，插入到头部');
  APL.Insert(0,AList);
  mmResult.Lines.Add('全新插入后总数:'+IntToStr(APL.count)+',总页数:'+IntToStr(APL.PageCount));
  APL.Clear;
  APL.Insert(0,AList);
  mmResult.Lines.Add('Clear插入后总数:'+IntToStr(APL.count)+',总页数:'+IntToStr(APL.PageCount));
  APL.Clear;
  APL.Pack;
  mmResult.Lines.Add('清除并收缩后总数:'+IntToStr(APL.count)+',总页数:'+IntToStr(APL.PageCount));
  APL.Insert(0,AList);
  APL.Insert(MaxInt,AList);
  mmResult.Lines.Add('批量追加到尾部后总数:'+IntToStr(APL.count)+',总页数:'+IntToStr(APL.PageCount));
  //I:=random(APL.count);
  APL.Insert(200,AList);
  mmResult.Lines.Add('批量追加到中间后总数:'+IntToStr(APL.count)+',总页数:'+IntToStr(APL.PageCount));
finally
  FreeAndNil(APL);
end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  AList: TQPagedList;
  APList: TPointerList;
  I: Integer;
  J: Integer;
begin
  AList := TQPagedList.Create;
  try
    for J := 0 to 9 do
    begin
      AList.Clear;
      for I := 0 to 10000 do
        AList.Add(Pointer(I));
      for I := 0 to 10000 do
        Assert(IntPtr(AList[I]) = I);
      APList := AList.List;
      Assert(Length(APList) = AList.Count);
      for I := 0 to 10000 do
        Assert(IntPtr(APList[I]) = I);
    end;
    AList.Delete(30);
    AList.Insert(15, Pointer(33));
    AList.Insert(18, Pointer(34));
    while AList.Count > 0 do
      AList.Delete(AList.Count - 1);
    AList.Pack;
  finally
    FreeObject(AList);
  end;
end;

end.
