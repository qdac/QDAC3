unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.OleCtrls, SHDocVw, qmacros, qstring, Data.DB, Data.Win.ADODB;

type
  TForm1 = class(TForm)
    WebBrowser1: TWebBrowser;
    Panel1: TPanel;
    Button1: TButton;
    adsData: TADODataSet;
    adsDataId: TIntegerField;
    adsDataName: TStringField;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
    procedure DoOddMacro(AMacro: TQMacroItem; const AQuoter: QCharW);
    procedure DoEvenMacro(AMacro: TQMacroItem; const AQuoter: QCharW);
    procedure DoPrintInfo(AMacro: TQMacroItem; const AQuoter: QCharW);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function GetHumanName: String;
const
  // 中国百家姓 (Chinese Last Names)
  FirstNames: array [0 .. 503] of String = ('赵', '钱', '孙', '李', '周', '吴', '郑',
    '王', '冯', '陈', '褚', '卫', '蒋', '沈', '韩', '杨', '朱', '秦', '尤', '许', '何', '吕',
    '施', '张', '孔', '曹', '严', '华', '金', '魏', '陶', '姜', '戚', '谢', '邹', '喻', '柏',
    '水', '窦', '章', '云', '苏', '潘', '葛', '奚', '范', '彭', '郎', '鲁', '韦', '昌', '马',
    '苗', '凤', '花', '方', '俞', '任', '袁', '柳', '酆', '鲍', '史', '唐', '费', '廉', '岑',
    '薛', '雷', '贺', '倪', '汤', '滕', '殷', '罗', '毕', '郝', '邬', '安', '常', '乐', '于',
    '时', '傅', '皮', '卞', '齐', '康', '伍', '余', '元', '卜', '顾', '孟', '平', '黄', '和',
    '穆', '萧', '尹', '姚', '邵', '堪', '汪', '祁', '毛', '禹', '狄', '米', '贝', '明', '臧',
    '计', '伏', '成', '戴', '谈', '宋', '茅', '庞', '熊', '纪', '舒', '屈', '项', '祝', '董',
    '粱', '杜', '阮', '蓝', '闵', '席', '季', '麻', '强', '贾', '路', '娄', '危', '江', '童',
    '颜', '郭', '梅', '盛', '林', '刁', '钟', '徐', '邱', '骆', '高', '夏', '蔡', '田', '樊',
    '胡', '凌', '霍', '虞', '万', '支', '柯', '昝', '管', '卢', '莫', '经', '房', '裘', '缪',
    '干', '解', '应', '宗', '丁', '宣', '贲', '邓', '郁', '单', '杭', '洪', '包', '诸', '左',
    '石', '崔', '吉', '钮', '龚', '程', '嵇', '邢', '滑', '裴', '陆', '荣', '翁', '荀', '羊',
    '於', '惠', '甄', '麴', '家', '封', '芮', '羿', '储', '靳', '汲', '邴', '糜', '松', '井',
    '段', '富', '巫', '乌', '焦', '巴', '弓', '牧', '隗', '山', '谷', '车', '侯', '宓', '蓬',
    '全', '郗', '班', '仰', '秋', '仲', '伊', '宫', '宁', '仇', '栾', '暴', '甘', '钭', '厉',
    '戎', '祖', '武', '符', '刘', '景', '詹', '束', '龙', '叶', '幸', '司', '韶', '郜', '黎',
    '蓟', '薄', '印', '宿', '白', '怀', '蒲', '邰', '从', '鄂', '索', '咸', '籍', '赖', '卓',
    '蔺', '屠', '蒙', '池', '乔', '阴', '欎', '胥', '能', '苍', '双', '闻', '莘', '党', '翟',
    '谭', '贡', '劳', '逄', '姬', '申', '扶', '堵', '冉', '宰', '郦', '雍', '舄', '璩', '桑',
    '桂', '濮', '牛', '寿', '通', '边', '扈', '燕', '冀', '郏', '浦', '尚', '农', '温', '别',
    '庄', '晏', '柴', '瞿', '阎', '充', '慕', '连', '茹', '习', '宦', '艾', '鱼', '容', '向',
    '古', '易', '慎', '戈', '廖', '庚', '终', '暨', '居', '衡', '步', '都', '耿', '满', '弘',
    '匡', '国', '文', '寇', '广', '禄', '阙', '东', '殴', '殳', '沃', '利', '蔚', '越', '夔',
    '隆', '师', '巩', '厍', '聂', '晁', '勾', '敖', '融', '冷', '訾', '辛', '阚', '那', '简',
    '饶', '空', '曾', '毋', '沙', '乜', '养', '鞠', '须', '丰', '巢', '关', '蒯', '相', '查',
    '後', '荆', '红', '游', '竺', '权', '逯', '盖', '益', '桓', '公', '万俟', '司马', '上官',
    '欧阳', '夏侯', '诸葛', '闻人', '东方', '赫连', '皇甫', '尉迟', '公羊', '澹台', '公冶', '宗政',
    '濮阳', '淳于', '单于', '太叔', '申屠', '公孙', '仲孙', '轩辕', '令狐', '钟离', '宇文', '长孙',
    '慕容', '鲜于', '闾丘', '司徒', '司空', '亓官', '司寇', '仉', '督', '子车', '颛孙', '端木', '巫马',
    '公西', '漆雕', '乐正', '壤驷', '公良', '拓拔', '夹谷', '宰父', '谷粱', '晋', '楚', '闫', '法',
    '汝', '鄢', '涂', '钦', '段干', '百里', '东郭', '南门', '呼延', '归', '海', '羊舌', '微生', '岳',
    '帅', '缑', '亢', '况', '后', '有', '琴', '梁丘', '左丘', '东门', '西门', '商', '牟', '佘',
    '佴', '伯', '赏', '南宫', '墨', '哈', '谯', '笪', '年', '爱', '阳', '佟', '第五',
    '言', '福');
var
  I, L: Integer;
begin
  L := 1 + Random(2);
  Result := '';
  for I := 0 to L - 1 do
    Result := Result + WideChar($4E00 + Random(20902));
  Result := FirstNames[Random(Length(FirstNames))] + Result;
end;

procedure TForm1.Button1Click(Sender: TObject);
const
  STableTemplate: QStringW = '<html><head><title><%Title%></title></head>' + //
    '<body>这是一个表格<table border="1"  cellspacing="0" cellpadding="0" bordercolor="#000000" style="BORDER-COLLAPSE: collapse">'
    + //
    '<th><tr><td>记录号</td><td>序号</td><td>编号</td><td>姓名</td></tr></th>' + //
    '%adsData.@Rows("<tr><td>%adsData.@RecNo%</td><td>%adsData.@Rows.@Index%<td>%Id%</td><td>%Name%</td></tr>","%","%")%'
    + //
    '</table></body>';
var
  AMacros: TQMacroManager;
  AHtmlFile, AHtmlText, ATag: String;
begin
  AMacros := TQMacroManager.Create;
  try
    AMacros.Push(adsData, '');
    AMacros.Push('Title', 'QMacros HTML Tag 替换');
    AHtmlText := AMacros.Replace(STableTemplate, '%', '%', MRF_PARSE_PARAMS);
    AHtmlFile := ExtractFilePath(Application.ExeName) + 'index.html';
    SaveTextW(AHtmlFile, AHtmlText);
    WebBrowser1.Navigate('file:///' + StringReplaceW(AHtmlFile, '\', '/',
      [rfReplaceAll]));
    AMacros.Pop(adsData, '');
  finally
    FreeAndNil(AMacros);
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
const
  STemplate: QStringW = '<html><head><title><%Title%></title></head>' + //
    '<body>条件替换示例<p>' + //
    '<%IsOdd("%Number% 是奇数")%><%IsEven("%Number% 是偶数")%>' + '</body>';
var
  AMacros: TQMacroManager;
  AHtmlFile: String;
begin
  AMacros := TQMacroManager.Create;
  try
    AMacros.Push('Title', 'QMacros HTML 模板示例');
    AMacros.Push('Number', IntToStr(Random(100)));
    AMacros.Push('IsOdd', DoOddMacro);
    AMacros.Push('IsEven', DoEvenMacro);
    AHtmlFile := ExtractFilePath(Application.ExeName) + 'index.html';
    SaveTextW(AHtmlFile, AMacros.Replace(STemplate, '<%', '%>',
      MRF_PARSE_PARAMS));
    WebBrowser1.Navigate('file:///' + StringReplaceW(AHtmlFile, '\', '/',
      [rfReplaceAll]));
  finally
    FreeAndNil(AMacros);
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
const
  STemplate: QStringW = '<html><head><title><%Title%></title></head>' + //
    '<body>多参数及转义示例<p>' + //
    '<%Info({"name":"QDAC","version":1.28,"copyright":"&copy;QDAC team\t2016"},3.0)%>'
    + '</body>';
var
  AMacros: TQMacroManager;
  AHtmlFile: String;
begin
  AMacros := TQMacroManager.Create;
  try
    AMacros.Push('Title', 'QMacros HTML 模板示例');
    AMacros.Push('Info', DoPrintInfo);
    AHtmlFile := ExtractFilePath(Application.ExeName) + 'index.html';
    SaveTextW(AHtmlFile, AMacros.Replace(STemplate, '<%', '%>',
      MRF_PARSE_PARAMS));
    WebBrowser1.Navigate('file:///' + StringReplaceW(AHtmlFile, '\', '/',
      [rfReplaceAll]));
  finally
    FreeAndNil(AMacros);
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
const
  STemplate: QStringW = '<html><head><title>字符串列表示例</title></head>' + //
    '<body>' + //
    '%List("<p>Welcome %List.Value%,you are %List.@Index% of our friends","%","%")%'
    + '</body>';
var
  AList: TStringList;
  AMacros: TQMacroManager;
  AHtmlFile: String;
  AIterator: IQMacroIterator ;
begin
  AList := TStringList.Create;
  AList.Text := 'Joson'#13#10'Mikelin';
  AMacros := TQMacroManager.Create;
  try
    AIterator := TQMacroStringsIterator.Create(AList);
    AMacros.Push('List', AIterator);
    AMacros.Push('List.Value',
      procedure(AMacro: TQMacroItem; const AQuoter: QCharW)
      begin
        AMacro.Value.Value := AList[AIterator.GetItemIndex];
      end);
    AHtmlFile := ExtractFilePath(Application.ExeName) + 'index.html';
    SaveTextW(AHtmlFile, AMacros.Replace(STemplate, '%', '%',
      MRF_PARSE_PARAMS));
    WebBrowser1.Navigate('file:///' + StringReplaceW(AHtmlFile, '\', '/',
      [rfReplaceAll]));
  finally
    FreeAndNil(AMacros);
    FreeAndNil(AList);
  end;
end;

procedure TForm1.DoEvenMacro(AMacro: TQMacroItem; const AQuoter: QCharW);
var
  AValue: Integer;
begin
  if TryStrToInt(AMacro.Owner.Values['Number'], AValue) and ((AValue mod 2) = 0)
  then
    AMacro.Value.Value := AMacro.Owner.Replace
      (AMacro.Params[0].AsString, '%', '%')
  else
    AMacro.Value.Value := '';
end;

procedure TForm1.DoOddMacro(AMacro: TQMacroItem; const AQuoter: QCharW);
var
  AValue: Integer;
begin
  if TryStrToInt(AMacro.Owner.Values['Number'], AValue) and ((AValue mod 2) <> 0)
  then
    AMacro.Value.Value := AMacro.Owner.Replace
      (AMacro.Params[0].AsString, '%', '%')
  else
    AMacro.Value.Value := '';
end;

procedure TForm1.DoPrintInfo(AMacro: TQMacroItem; const AQuoter: QCharW);
begin
  AMacro.Value.Value := 'QMacros ' + AMacro.Params[1].AsString + '<BR/>';
  with AMacro.Params[0] do
  begin
    AMacro.Value.Value := AMacro.Value.Value + '工程名称:' + ValueByName('name', '')
      + '<BR/>' + '版本号:' + ValueByName('version', '') + '<BR/>' + '版权：' +
      ValueByName('copyright', '');
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  adsData.CreateDataSet;
  adsData.DisableControls;
  try
    for I := 0 to 9 do
    begin
      adsData.Append;
      adsDataId.AsInteger := I;
      adsDataName.AsString := GetHumanName;
      adsData.Post;
    end;
  finally
    adsData.EnableControls;
  end;
  WebBrowser1.Navigate('about:blank');
end;

end.
