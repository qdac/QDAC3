unit main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.ListBox;

type
  TForm1 = class(TForm)
    btnShortlog: TButton;
    btnLongLog: TButton;
    btnLoopLog: TButton;
    Label1: TLabel;
    cbxLogLevel: TComboBox;
    Label2: TLabel;
    chkMode: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure btnShortlogClick(Sender: TObject);
    procedure btnLongLogClick(Sender: TObject);
    procedure btnLoopLogClick(Sender: TObject);
    procedure chkModeChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses qstring, qlog,qcndate,ioutils{$IFDEF MSWINDOWS}, windows{$ENDIF}{$IFDEF ANDROID},
  Androidapi.ioutils{$ENDIF};
{$R *.fmx}

procedure TForm1.btnShortlogClick(Sender: TObject);
begin
  PostLog(TQLogLevel(cbxLogLevel.ItemIndex), '这是一条简短日志.');
end;

procedure TForm1.btnLongLogClick(Sender: TObject);
begin
  PostLog(TQLogLevel(cbxLogLevel.ItemIndex),
    '这是一条很长的中文日志，目标是超过syslog约定的1024个字符的限制。' +
    '从前有座山，山上有座庙，庙里老和尚在给小和尚念古诗，古诗的内容包括：'#13#10 +
    '天地玄黄 宇宙洪荒 日月盈昃 辰宿列张 寒来暑往 秋收冬藏'#13#10 +
    '闰馀成岁 律吕调阳 云腾致雨 露结为霜 金生丽水 玉出昆冈'#13#10 +
    '剑号巨阙 珠称夜光 果珍李柰 菜重芥姜 海咸河淡 鳞潜羽翔'#13#10 +
    '龙师火帝 鸟官人皇 始制文字 乃服衣裳 推位让国 有虞陶唐'#13#10 +
    '吊民伐罪 周发殷汤 坐朝问道 垂拱平章 爱育黎首 臣伏戎羌'#13#10 +
    '遐迩一体 率宾归王 鸣凤在竹 白驹食场 化被草木 赖及万方'#13#10 +
    '盖此身发 四大五常 恭惟鞠养 岂敢毁伤 女慕贞洁 男效才良'#13#10 +
    '知过必改 得能莫忘 罔谈彼短 靡恃己长 信使可复 器欲难量'#13#10 +
    '墨悲丝染 诗赞羔羊 景行维贤 克念作圣 德建名立 形端表正'#13#10 +
    '空谷传声 虚堂习听 祸因恶积 福缘善庆 尺璧非宝 寸阴是竞'#13#10 +
    '资父事君 曰严与敬 孝当竭力 忠则尽命 临深履薄 夙兴温①'#13#10 +
    '似兰斯馨 如松之盛 川流不息 渊澄取映 容止若思 言辞安定'#13#10 +
    '笃初诚美 慎终宜令 荣业所基 籍甚无竟 学优登仕 摄职从政'#13#10 +
    '存以甘棠 去而益咏 乐殊贵贱 礼别尊卑 上和下睦 夫唱妇随'#13#10 +
    '外受傅训 入奉母仪 诸姑伯叔 犹子比儿 孔怀兄弟 同气连枝'#13#10 +
    '交友投分 切磨箴规 仁慈隐恻 造次弗离 节义廉退 颠沛匪亏'#13#10 +
    '性静情逸 心动神疲 守真志满 逐物意移 坚持雅操 好爵自縻'#13#10 +
    '都邑华夏 东西二京 背邙面洛 浮渭据泾 宫殿盘郁 楼观飞惊'#13#10 +
    '图写禽兽 画彩仙灵 丙舍傍启 甲帐对楹 肆筵设席 鼓瑟吹笙'#13#10 +
    '升阶纳陛 弁转疑星 右通广内 左达承明 既集坟典 亦聚群英'#13#10 +
    '杜稿钟隶 漆书壁经 府罗将相 路侠槐卿 户封八县 家给千兵'#13#10 +
    '高冠陪辇 驱毂振缨 世禄侈富 车驾肥轻 策功茂实 勒碑刻铭'#13#10 +
    '②溪伊尹 佐时阿衡 奄宅曲阜 微旦孰营 桓公匡合 济弱扶倾'#13#10 +
    '绮回汉惠 说感武丁 俊③密勿 多士④宁 晋楚更霸 赵魏困横'#13#10 +
    '假途灭虢 践土会盟 何遵约法 韩弊烦刑 起翦颇牧 用军最精'#13#10 +
    '宣威沙漠 驰誉丹青 九州禹迹 百郡秦并 岳宗泰岱 禅主云亭'#13#10 +
    '雁门紫塞 鸡田赤城 昆池碣石 巨野洞庭 旷远绵邈 岩岫杳冥'#13#10 +
    '治本于农 务资稼穑 ⑤载南亩 我艺黍稷 税熟贡新 劝赏黜陟'#13#10 +
    '孟轲敦素 史鱼秉直 庶几中庸 劳谦谨敕 聆音察理 鉴貌辨色'#13#10 +
    '贻厥嘉猷 勉其祗植 省躬讥诫 宠增抗极 殆辱近耻 林皋幸即'#13#10 +
    '两疏见机 解组谁逼 索居闲处 沉默寂寥 求古寻论 散虑逍遥'#13#10 +
    '欣奏累遣 戚谢欢招 渠荷的历 园莽抽条 枇杷晚翠 梧桐蚤凋'#13#10 +
    '陈根委翳 落叶飘摇 游⑥独运 凌摩绛霄 耽读玩市 寓目囊箱'#13#10 +
    '易⑦攸畏 属耳垣墙 具膳餐饭 适口充肠 饱饫烹宰 饥厌糟糠'#13#10 +
    '亲戚故旧 老少异粮 妾御绩纺 侍巾帷房 纨扇圆⑧ 银烛炜煌'#13#10 +
    '昼眠夕寐 蓝笋象床 弦歌酒宴 接杯举觞 矫手顿足 悦豫且康'#13#10 +
    '嫡后嗣续 祭祀⑨尝 稽颡再拜 悚惧恐惶 笺牒简要 顾答审详'#13#10 +
    '骸垢想浴 执热愿凉 驴骡犊特 骇跃超骧 诛斩贼盗 捕获叛亡'#13#10 +
    '布射僚丸 嵇琴阮啸 恬笔伦纸 钧巧任钓 释纷利俗 ⑩皆佳妙'#13#10 +
    '毛施淑姿 工颦妍笑 年矢每催 曦晖朗曜 璇玑悬斡 晦魄环照'#13#10 +
    '指薪修祜 永绥吉劭 矩步引领 俯仰廊庙 束带矜庄 徘徊瞻眺'#13#10 + '孤陋寡闻 愚蒙等诮 谓语助者 焉哉乎也');
end;

procedure TForm1.btnLoopLogClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to 999 do
    btnLongLogClick(Sender);
  ShowMessage('日志投递完成。');
end;

procedure TForm1.chkModeChange(Sender: TObject);
begin
  if chkMode.IsChecked then
    Logs.Mode := lmAsyn
  else
    Logs.Mode := lmSync;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  AWriter: TQLogSocketWriter;
begin
{$IFNDEF MSWINDOWS}
  AWriter := TQLogSocketWriter.Create;
  AWriter.ServerHost := '192.168.0.2';
  AWriter.ServerPort := 514;
  // 如果要使用TCP协议，那么设置下面的属性为True即可，但QLogServer目前不支持TCP协议
  // 而且TCP协议一旦服务器不在线，可能会造成不必要的日志积压
  // AWriter.UseTcp:=True;
  Logs.Castor.AddWriter(AWriter);
{$IFDEF ANDROID}
  Logs.Castor.AddWriter(TQLogFileWriter.Create(GetExtSDDir + 'qlogdemo.log'));
  Label1.Text := '外置SDCard目录为：' + GetExtSDDir;
{$ENDIF}
{$ELSE}
  SetDefaultLogFile;
{$ENDIF}
  // 输出日志到控制台
  Logs.Castor.AddWriter(TQLogConsoleWriter.Create);
  // 输出日志到文件
//  SetDefaultLogFile({$IFDEF ANDROID}GetExtSDDir + '/' + Application.Title +
//    '.log'{$ELSE}''{$ENDIF}, 2 * 1024 * 1024, false);
  PostLog(llHint, 'Application Started.');
  ReportMemoryLeaksOnShutdown := True;
end;

end.
