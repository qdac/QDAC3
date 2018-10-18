unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, qmacros, qstring, Vcl.StdCtrls,
  Vcl.ExtCtrls;

type
  TForm6 = class(TForm)
    Memo2: TMemo;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Button1: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Memo1: TMemo;
    Button2: TButton;
    chkSingleQuoter: TCheckBox;
    chkDoubleQuoter: TCheckBox;
    Button3: TButton;
    chkIgnoreCase: TCheckBox;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    chkEndBySpace: TCheckBox;
    Button8: TButton;
    chkEnableEscape: TCheckBox;
    chkIgnoreMissed: TCheckBox;
    chkParseParams: TCheckBox;
    Button9: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure chkIgnoreCaseClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure chkEndBySpaceClick(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
  private
    { Private declarations }
    FMacroMgr: TQMacroManager;
    procedure GetNow(AMacro: TQMacroItem; const AQuoter: QCharW);
    procedure DoGetMacroTag(AMacro: TQMacroItem; const AQuoter: QCharW);
    function ReplaceFlags: Integer;
    procedure DoPromptMacroValue(ASender: TQMacroManager; AName: QStringW;
      const AQuoter: QCharW; var AHandled: Boolean);
    procedure DoTestMacroNameEnd(ASender: TQMacroManager; p: PQCharW;
      var ALen: Integer; var AType: TQMacroCharType);
  public
    { Public declarations }
  end;

var
  Form6: TForm6;

implementation

{$R *.dfm}

procedure TForm6.Button1Click(Sender: TObject);
begin
  Memo2.Text := FMacroMgr.Replace(Memo1.Text, Edit1.Text, Edit2.Text,
    ReplaceFlags);
end;

procedure TForm6.Button2Click(Sender: TObject);
begin
  FMacroMgr.Push('Year', 'Happy New Year');
  Memo2.Text := '入栈新宏 Year 值为 "Happy New Year":';
  Memo2.Lines.Add(FMacroMgr.Replace(Memo1.Text, Edit1.Text, Edit2.Text,
    ReplaceFlags));
  FMacroMgr.Pop('Year');
  Memo2.Lines.Add('出栈恢复宏 Year 上一值:');
  Memo2.Lines.Add(FMacroMgr.Replace(Memo1.Text, Edit1.Text, Edit2.Text,
    ReplaceFlags));
end;

procedure TForm6.Button3Click(Sender: TObject);
var
  ASavePoint: Integer;
begin
  Memo2.Text := '设置保存点之前的值:';
  Memo2.Lines.Add('Year=' + FMacroMgr.MacroValue('Year'));
  ASavePoint := FMacroMgr.SavePoint;
  try
    FMacroMgr.Push('Year', '9999');
    Memo2.Lines.Add('设置保存点并修改Year的值后：');
    Memo2.Lines.Add('Year=' + FMacroMgr.MacroValue('Year'));
  finally
    FMacroMgr.Restore(ASavePoint);
    Memo2.Lines.Add('恢复保存点还原Year的值后：');
    Memo2.Lines.Add('Year=' + FMacroMgr.MacroValue('Year'));
  end;
end;

procedure TForm6.Button4Click(Sender: TObject);
var
  T1, T2, T3: Cardinal;
  S: String;
  AHandle: TQMacroComplied;
  AList: TStringList;
  I, ASavePoint: Integer;
  procedure DoTest;
  var
    I, J: Integer;
  begin
    Memo2.Lines.Add('测试用例:' + S);
    T1 := GetTickCount;
    for I := 0 to 99999 do
    begin
      for J := 0 to AList.Count - 1 do
        StringReplace(S, AList[J], '2014', [rfReplaceAll]);
    end;
    T1 := GetTickCount - T1;
    Memo2.Lines.Add('使用系统的StringReplace函数用时:' + IntToStr(T1) + 'ms(100%)');
    T2 := GetTickCount;
    for I := 0 to 99999 do
    begin
      for J := 0 to AList.Count - 1 do
        StringReplaceW(S, AList[J], '2014', [rfReplaceAll]);
    end;
    T2 := GetTickCount - T2;
    Memo2.Lines.Add('使用QString的StringReplaceW函数用时:' + IntToStr(T2) + 'ms(' +
      FormatFloat('0.00', T2 * 100 / T1) + ')');
    T3 := GetTickCount;
    AHandle := FMacroMgr.Complie(S, '%', '%', 0);
    for I := 0 to 99999 do
      FMacroMgr.Replace(AHandle);
    FreeObject(AHandle);
    T3 := GetTickCount - T3;
    Memo2.Lines.Add('使用QMacros的Replace函数用时:' + IntToStr(T3) + 'ms(' +
      FormatFloat('0.00', T3 * 100 / T1) + ')');
    T3 := GetTickCount;
    AHandle := FMacroMgr.Complie(S, '', '', 0);
    for I := 0 to 99999 do
      FMacroMgr.Replace(AHandle);
    FreeObject(AHandle);
    T3 := GetTickCount - T3;
    Memo2.Lines.Add('使用QMacros的Replace函数用时(无界定符):' + IntToStr(T3) + 'ms(' +
      FormatFloat('0.00', T3 * 100 / T1) + ')'#13#10);
  end;

begin
  AList := TStringList.Create;
  AList.Add('%Year%');
  S := StringReplicateW('%Year% ', 20);
  DoTest;
  AList.Strings[0] := '%Now%';
  S := StringReplicateW('%Now% ', 20);
  DoTest;
  S := StringReplicateW('%Year% %Month% %Now% ', 20);
  AList.Add('%Year%');
  AList.Add('%Month%');
  DoTest;
  AList.Clear;
  S := '';
  ASavePoint := FMacroMgr.SavePoint;
  for I := 0 to 19 do
  begin
    AList.Add('Macro' + IntToStr(I));
    FMacroMgr.Push('Macro' + IntToStr(I), DoGetMacroTag, mvStable, I);
    S := S + '%' + 'Macro' + IntToStr(I) + '% ';
  end;
  DoTest;
  T1 := GetTickCount;
  for I := 0 to 99999 do
    FMacroMgr.Replace(S, '%', '%');
  T1 := GetTickCount - T1;
  T2 := GetTickCount;
  for I := 0 to 99999 do
    FMacroMgr.Replace(S, '', '');
  T2 := GetTickCount - T2;
  Memo2.Lines.Add('无分隔符和有分隔符替换10万次相关时间为:' + IntToStr(T1) + '-' + IntToStr(T2) +
    '=' + IntToStr(Integer(T2) - Integer(T1)) + 'ms');
  FMacroMgr.Restore(ASavePoint);
  FreeObject(AList);
end;

procedure TForm6.Button5Click(Sender: TObject);
var
  AComplied: TQMacroComplied;
begin
  AComplied := FMacroMgr.Complie(Memo1.Text, Edit1.Text, Edit2.Text,
    ReplaceFlags);
  AComplied.SaveToFile('macro_complied.cache');
  AComplied.LoadFromFile('macro_complied.cache');
  Memo2.Text := FMacroMgr.Replace(AComplied);
  FreeObject(AComplied);
end;

procedure TForm6.Button6Click(Sender: TObject);
var
  sp: Integer;
begin
  sp := FMacroMgr.SavePoint;
  FMacroMgr.OnMacroMissed := DoPromptMacroValue;
  ShowMessage(FMacroMgr.Replace('%姓名%:'#13#10'欢迎您使用QMacros.', '%', '%'));
  FMacroMgr.OnMacroMissed := nil;
  FMacroMgr.Restore(sp);
end;

procedure TForm6.Button7Click(Sender: TObject);
var
  AComplied: TQMacroComplied;
  AList: TStringList;
  I, sp: Integer;
begin
  AComplied := FMacroMgr.Complie(Memo1.Text, Edit1.Text, Edit2.Text,
    MRF_DELAY_BINDING or ReplaceFlags);
  if AComplied <> nil then
  begin
    AList := TStringList.Create;
    Memo2.Lines.Add('当前内容中包括' + IntToStr(AComplied.EnumUsedMacros(AList))
      + '个宏定义:');
    Memo2.Lines.AddStrings(AList);
    sp := FMacroMgr.SavePoint;
    for I := 0 to AList.Count - 1 do
    begin
      FMacroMgr.Push(AList[I], '延迟绑定值' + IntToStr(I));
    end;
    Memo2.Lines.Add(AComplied.Replace);
    FMacroMgr.Restore(sp);
    FreeObject(AList);
    FreeObject(AComplied);
  end;
end;

procedure TForm6.Button8Click(Sender: TObject);
var
  AMgr: TQMacroManager;
begin
  Memo1.Text := 'Replace $1.Name to $2.Name';
  AMgr := TQMacroManager.Create;
  try
    AMgr.OnTestNameEnd := DoTestMacroNameEnd;
    AMgr.Push('1', '第一个');
    AMgr.Push('2', '第二个');
    Memo2.Text := AMgr.Replace(Memo1.Text, '$', '');
  finally
    FreeAndNil(AMgr);
  end;
end;

procedure TForm6.Button9Click(Sender: TObject);
begin
  FMacroMgr.Push('ParamMacro',
    procedure(AMacro: TQMacroItem; const AQuoter: QCharW)
    begin
      AMacro.Value.Value := AMacro.Params
        [AMacro.Params[0].AsInteger mod 2].AsString;
    end);
  Memo1.Lines.Text := 'this is %ParamMacro(100,"Value0")%' + SLineBreak +
    'this is %ParamMacro(101,"Value1")%';
  Memo2.Lines.Text := FMacroMgr.Replace(Memo1.Lines.Text, '%', '%',
    MRF_PARSE_PARAMS);
end;

procedure TForm6.chkEndBySpaceClick(Sender: TObject);
begin
  Edit2.Enabled := not chkEndBySpace.Checked;
end;

procedure TForm6.chkIgnoreCaseClick(Sender: TObject);
begin
  FMacroMgr.IgnoreCase := chkIgnoreCase.Checked;
  // 重新注册函数名
  FMacroMgr.Clear;
  FMacroMgr.Push('Year', '2014');
  FMacroMgr.Push('Month', '7');
  FMacroMgr.Push('Now', GetNow, mvStable);
end;

procedure TForm6.DoGetMacroTag(AMacro: TQMacroItem; const AQuoter: QCharW);
begin
  AMacro.Value.Value := '2014';
end;

procedure TForm6.DoPromptMacroValue(ASender: TQMacroManager; AName: QStringW;
const AQuoter: QCharW; var AHandled: Boolean);
var
  AValue: String;
begin
  if InputQuery('未知的宏定义', '宏 [' + AName + ']值：', AValue) then
  begin
    ASender.Push(AName, AValue);
    AHandled := True;
  end;
end;

procedure TForm6.DoTestMacroNameEnd(ASender: TQMacroManager; p: PQCharW;
var ALen: Integer; var AType: TQMacroCharType);
begin
  if (p^ < '0') or (p^ > '9') then
  begin
    AType := mctNameEnd;
    ALen := 0;
  end;
end;

procedure TForm6.FormCreate(Sender: TObject);
begin
  FMacroMgr := TQMacroManager.Create;
  FMacroMgr.Push('Year', '2014');
  FMacroMgr.Push('Month', '7');
  FMacroMgr.Push('Now', GetNow, mvStable);
end;

procedure TForm6.FormDestroy(Sender: TObject);
begin
  FreeObject(FMacroMgr);
end;

procedure TForm6.GetNow(AMacro: TQMacroItem; const AQuoter: QCharW);
begin
  AMacro.Value.Value := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
end;

procedure TForm6.Panel1Click(Sender: TObject);
begin
  FMacroMgr.Push('UnitName','TestUnit');
  FMacroMgr.Push('ClassName','TAccountEx');
  Memo1.Lines.Add(FMacroMgr.Replace(LoadTextW('H:\User\QDAC\temp\qmacrotest\模板.txt'),'[',']'));
end;

function TForm6.ReplaceFlags: Integer;
begin
  Result := 0;
  if chkSingleQuoter.Checked then
    Result := MRF_IN_SINGLE_QUOTER;
  if chkDoubleQuoter.Checked then
    Result := Result or MRF_IN_DBL_QUOTER;
  if chkEndBySpace.Checked then
    Result := Result or MRF_END_WITH_INVALID_CHAR;
  if chkParseParams.Checked then
    Result := Result or MRF_PARSE_PARAMS;
  if chkEnableEscape.Checked then
    Result := Result or MRF_ENABLE_ESCAPE;
  if chkIgnoreMissed.Checked then
    Result := Result or MRF_IGNORE_MISSED;
end;

end.
