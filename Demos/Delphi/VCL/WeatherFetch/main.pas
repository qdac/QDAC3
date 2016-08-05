unit main;

interface

{ 本示例演示了一个稍微复杂的QWorker用例：
  1、并行作业使用了一个串行化的TQJobGroup来完成一个流程控制
  1.1、从百度天气使用For并行获取148个城市的天气信息，并写入到Memo中
  1.2、在主线程中显示操作所花费的时间
  2、作业执行结束，显示恢复按钮状态，允许下次操作
}
uses
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, QString, QWorker, wininet,
  ExtCtrls;

type
  TForm4 = class(TForm)
    Panel1: TPanel;
    Memo1: TMemo;
    Panel2: TPanel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    procedure DoFetchWeathers(AJob: PQJob);
    procedure FetchCityWeather(ALoopMgr: TQForJobs; AJob: PQJob;
      AIndex: NativeInt);
    procedure DoLogWeather(AJob: PQJob);
    procedure DoShowResult(AJob: PQJob);
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

uses qjson;
{$R *.dfm}

const
  Cities: array [0 .. 147] of String = ('长春市', '延边朝鲜族自治州', '四平市', '白城市', '天津市',
    '石家庄市', '唐山市', '秦皇岛市', '邯郸市', '邢台市', '保定市', '张家口市', '承德市', '沧州市', '廊坊市',
    '衡水市', '上海市', '太原市', '大同市', '阳泉市', '长治市', '晋城市', '朔州市', '晋中市', '运城市', '忻州市',
    '临汾市', '吕梁市', '齐齐哈尔市', '鸡西市', '鹤岗市', '双鸭山市', '大庆市', '伊春市', '佳木斯市', '七台河市',
    '牡丹江市', '黑河市', '绥化市', '大兴安岭地区', '上海市', '南昌市', '景德镇市', '萍乡市', '九江市', '新余市',
    '鹰潭市', '赣州市', '吉安市', '宜春市', '抚州市', '上饶市', '长沙市', '株洲市', '湘潭市', '衡阳市', '邵阳市',
    '岳阳市', '常德市', '张家界市,', '益阳市', '郴州市', '永州市', '怀化市', '娄底市', '湘西州', '广州市',
    '韶关市', '深圳市', '珠海市', '汕头市', '佛山市', '江门市', '湛江市', '茂名市', '肇庆市', '惠州市', '梅州市',
    '汕尾市', '河源市', '阳江市', '清远市', '东莞市', '中山市', '潮州市', '揭阳市', '云浮市', '南宁市', '柳州市',
    '桂林市', '梧州市', '北海市', '防城港市', '钦州市', '贵港市', '玉林市', '百色市', '贺州市', '河池市',
    '来宾市', '崇左市', '昆明市', '曲靖市', '玉溪市', '保山市', '昭通市', '丽江市', '普洱市', '临沧市', '楚雄州',
    '红河州', '文山州', '西双版纳州', '大理', '德宏', '怒江', '迪庆', '拉萨市', '昌都', '山南', '日喀则',
    '那曲', '阿里', '林芝', '西安市', '铜川市', '宝鸡市', '咸阳市', '渭南市', '延安市', '汉中市', '榆林市',
    '安康市', '商洛市', '杨凌区', '西宁市', '海东', '海北', '黄南', '海南', '果洛', '玉树', '海西',
    '呼和浩特市', '包头市', '乌海市', '赤峰市', '通辽市');

const
  BaiduAK: String = 'rZh9hrIOiQMzC7Cj3r6PYvcq';
  // 百度每天有5000次配额限制，建议更换为自己的AK，仅为演示用途

procedure TForm4.Button1Click(Sender: TObject);
var
  AGroup: TQJobGroup;
begin
Button1.Enabled := False;
Button1.Caption := '获取中...';
Memo1.Lines.Clear;
AGroup := TQJobGroup.Create(True);
AGroup.Prepare;
AGroup.Add(DoFetchWeathers, nil, False);
AGroup.Add(DoShowResult, Pointer(GetTickCount), True);
AGroup.Run();
AGroup.MsgWaitFor();
FreeObject(AGroup);
Button1.Caption := '开始';
Button1.Enabled := True;
end;

procedure TForm4.DoFetchWeathers(AJob: PQJob);
begin
//由于配置限制，为了方便更多的用户获取成功，修改限制为每次只获取10个城市
TQForJobs.For(0, 9, FetchCityWeather);
end;

procedure TForm4.DoLogWeather(AJob: PQJob);
begin
Memo1.Lines.Add(PString(AJob.Data)^);
Dispose(PString(AJob.Data));
end;

procedure TForm4.DoShowResult(AJob: PQJob);
begin
ShowMessage('获取天气数据完成，用时' + Rolluptime((GetTickCount - Cardinal(AJob.Data))
  div 1000));
end;

procedure TForm4.FetchCityWeather(ALoopMgr: TQForJobs; AJob: PQJob;
  AIndex: NativeInt);
var
  AConn, AHttp: HINTERNET;
  AReaded: Cardinal;
  S: TMemoryStream;
  AJson: TQJson;
  AWeather: PString;
  ABuf: array [0 .. 65535] of Byte;
  function EncodeUtf8Url(S: QStringW): QStringW;
  var
    U8: QStringA;
    p: PQCharA;
  begin
  U8 := QString.Utf8Encode(S);
  p := PQCharA(U8);
  SetLength(Result, 0);
  while p^ <> 0 do
    begin
    if p^ < 128 then
      begin
      Result := Result + Char(p^);
      Inc(p);
      end
    else
      begin
      while p^ >= 128 do
        begin
        Result := Result + '%' + IntToHex(p^, 2);
        Inc(p);
        end;
      end;
    end;
  end;

begin
AConn := InternetOpen(nil, INTERNET_OPEN_TYPE_DIRECT, nil, nil, 0);
if AConn <> nil then
  begin
  AHttp := InternetOpenUrl(AConn,
    PChar('http://api.map.baidu.com/telematics/v3/weather?location=' +
    EncodeUtf8Url(Cities[AIndex]) + '&output=json&ak=' + BaiduAK), nil,
    0, 0, 0);
  if AHttp <> nil then
    begin
    S := TMemoryStream.Create;
    try
      while InternetReadFile(AHttp, @ABuf[0], 65536, AReaded) and
        (AReaded > 0) do
        S.WriteBuffer(ABuf[0], AReaded);
      S.Position := 0;
      AJson := TQJson.Create;
      try
        if AJson.TryParse(LoadTextW(S)) then
          begin
          // 更新天气信息
          New(AWeather);
          AWeather^ := IntToStr(AIndex) + '-' + Cities[AIndex] + '天气:'#13#10 +
            AJson.AsJson;
          Workers.Post(DoLogWeather, AWeather, True);
          end;
      finally
        FreeObject(AJson);
      end;
    finally
      FreeObject(S);
      InternetCloseHandle(AHttp);
    end;
    end;
  InternetCloseHandle(AConn);
  end;
end;

end.
