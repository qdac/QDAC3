unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,RegularExpressionsCore,DateUtils,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, QString, QValue, QDB, Data.DB,
  Vcl.StdCtrls,
  Vcl.DBCtrls, Vcl.Grids, Vcl.DBGrids, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    DBGrid1: TDBGrid;
    DBMemo1: TDBMemo;
    Panel2: TPanel;
    DataSource1: TDataSource;
    Panel3: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel4: TPanel;
    Label1: TLabel;
    ComboBox1: TComboBox;
    DBNavigator1: TDBNavigator;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Panel2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
    FDataSet: TQDataSet;
    procedure DoCustomFilter(DataSet: TDataSet; var Accept: Boolean);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function GetHumanName: String;
const
  // ÖĞ¹ú°Ù¼ÒĞÕ (Chinese Last Names)
  FirstNames: array [0 .. 503] of String = ('ÕÔ', 'Ç®', 'Ëï', 'Àî', 'ÖÜ', 'Îâ', 'Ö£',
    'Íõ', '·ë', '³Â', 'ñÒ', 'ÎÀ', '½¯', 'Éò', 'º«', 'Ñî', 'Öì', 'ÇØ', 'ÓÈ', 'Ğí', 'ºÎ', 'ÂÀ',
    'Ê©', 'ÕÅ', '¿×', '²Ü', 'ÑÏ', '»ª', '½ğ', 'Îº', 'ÌÕ', '½ª', 'Æİ', 'Ğ»', '×Ş', 'Ó÷', '°Ø',
    'Ë®', 'ñ¼', 'ÕÂ', 'ÔÆ', 'ËÕ', 'ÅË', '¸ğ', 'ŞÉ', '·¶', 'Åí', 'ÀÉ', 'Â³', 'Î¤', '²ı', 'Âí',
    'Ãç', '·ï', '»¨', '·½', 'Óá', 'ÈÎ', 'Ô¬', 'Áø', 'Ûº', '±«', 'Ê·', 'ÌÆ', '·Ñ', 'Á®', 'á¯',
    'Ñ¦', 'À×', 'ºØ', 'Äß', 'ÌÀ', 'ëø', 'Òó', 'ÂŞ', '±Ï', 'ºÂ', 'Úù', '°²', '³£', 'ÀÖ', 'ÓÚ',
    'Ê±', '¸µ', 'Æ¤', '±å', 'Æë', '¿µ', 'Îé', 'Óà', 'Ôª', '²·', '¹Ë', 'ÃÏ', 'Æ½', '»Æ', 'ºÍ',
    'ÄÂ', 'Ïô', 'Òü', 'Ò¦', 'ÉÛ', '¿°', 'Íô', 'Æî', 'Ã«', 'Óí', 'µÒ', 'Ã×', '±´', 'Ã÷', 'ê°',
    '¼Æ', '·ü', '³É', '´÷', 'Ì¸', 'ËÎ', 'Ã©', 'ÅÓ', 'ĞÜ', '¼Í', 'Êæ', 'Çü', 'Ïî', '×£', '¶­',
    'Á»', '¶Å', 'Èî', 'À¶', 'ãÉ', 'Ï¯', '¼¾', 'Âé', 'Ç¿', '¼Ö', 'Â·', 'Â¦', 'Î£', '½­', 'Í¯',
    'ÑÕ', '¹ù', 'Ã·', 'Ê¢', 'ÁÖ', 'µó', 'ÖÓ', 'Ğì', 'Çñ', 'Âæ', '¸ß', 'ÏÄ', '²Ì', 'Ìï', '·®',
    'ºú', 'Áè', '»ô', 'Óİ', 'Íò', 'Ö§', '¿Â', 'êÃ', '¹Ü', 'Â¬', 'Äª', '¾­', '·¿', 'ôÃ', 'çÑ',
    '¸É', '½â', 'Ó¦', '×Ú', '¶¡', 'Ğû', 'êÚ', 'µË', 'Óô', 'µ¥', 'º¼', 'ºé', '°ü', 'Öî', '×ó',
    'Ê¯', '´Ş', '¼ª', 'Å¥', '¹¨', '³Ì', 'ïú', 'ĞÏ', '»¬', 'Åá', 'Â½', 'ÈÙ', 'ÎÌ', 'Ü÷', 'Ñò',
    'ì¶', '»İ', 'Õç', 'ôğ', '¼Ò', '·â', 'ÜÇ', 'ôà', '´¢', '½ù', '¼³', 'Úû', 'ÃÓ', 'ËÉ', '¾®',
    '¶Î', '¸»', 'Î×', 'ÎÚ', '½¹', '°Í', '¹­', 'ÄÁ', 'Úó', 'É½', '¹È', '³µ', 'ºî', 'åµ', 'Åî',
    'È«', 'Û­', '°à', 'Ñö', 'Çï', 'ÖÙ', 'ÒÁ', '¹¬', 'Äş', '³ğ', 'èï', '±©', '¸Ê', 'î×', 'À÷',
    'ÈÖ', '×æ', 'Îä', '·û', 'Áõ', '¾°', 'Õ²', 'Êø', 'Áú', 'Ò¶', 'ĞÒ', 'Ë¾', 'ÉØ', 'Û¬', 'Àè',
    '¼»', '±¡', 'Ó¡', 'ËŞ', '°×', '»³', 'ÆÑ', 'Û¢', '´Ó', '¶õ', 'Ë÷', 'ÏÌ', '¼®', 'Àµ', '×¿',
    'İş', 'ÍÀ', 'ÃÉ', '³Ø', 'ÇÇ', 'Òõ', '™ä', 'ñã', 'ÄÜ', '²Ô', 'Ë«', 'ÎÅ', 'İ·', 'µ³', 'µÔ',
    'Ì·', '¹±', 'ÀÍ', 'åÌ', '¼§', 'Éê', '·ö', '¶Â', 'È½', 'Ô×', 'Ûª', 'Óº', 'ôª', 'è³', 'É£',
    '¹ğ', 'å§', 'Å£', 'ÊÙ', 'Í¨', '±ß', 'ìè', 'Ñà', '¼½', 'Û£', 'ÆÖ', 'ÉĞ', 'Å©', 'ÎÂ', '±ğ',
    '×¯', 'êÌ', '²ñ', 'öÄ', 'ÑÖ', '³ä', 'Ä½', 'Á¬', 'Èã', 'Ï°', '»Â', '°¬', 'Óã', 'Èİ', 'Ïò',
    '¹Å', 'Ò×', 'É÷', '¸ê', 'ÁÎ', '¸ı', 'ÖÕ', 'ôß', '¾Ó', 'ºâ', '²½', '¶¼', '¹¢', 'Âú', 'ºë',
    '¿ï', '¹ú', 'ÎÄ', '¿Ü', '¹ã', 'Â»', 'ãÚ', '¶«', 'Å¹', 'ì¯', 'ÎÖ', 'Àû', 'Îµ', 'Ô½', 'Ùç',
    'Â¡', 'Ê¦', '¹®', 'ØÇ', 'Äô', 'êË', '¹´', '°½', 'ÈÚ', 'Àä', 'ö¤', 'ĞÁ', 'ãÛ', 'ÄÇ', '¼ò',
    'ÈÄ', '¿Õ', 'Ôø', 'Îã', 'É³', 'Ø¿', 'Ñø', '¾Ï', 'Ğë', '·á', '³²', '¹Ø', 'Øá', 'Ïà', '²é',
    'áá', '¾£', 'ºì', 'ÓÎ', 'óÃ', 'È¨', 'åÖ', '¸Ç', 'Òæ', '»¸', '¹«', 'ÍòÙ¹', 'Ë¾Âí', 'ÉÏ¹Ù',
    'Å·Ñô', 'ÏÄºî', 'Öî¸ğ', 'ÎÅÈË', '¶«·½', 'ºÕÁ¬', '»Ê¸¦', 'Î¾³Ù', '¹«Ñò', 'å£Ì¨', '¹«Ò±', '×ÚÕş',
    'å§Ñô', '´¾ÓÚ', 'µ¥ÓÚ', 'Ì«Êå', 'ÉêÍÀ', '¹«Ëï', 'ÖÙËï', 'ĞùÔ¯', 'Áîºü', 'ÖÓÀë', 'ÓîÎÄ', '³¤Ëï',
    'Ä½Èİ', 'ÏÊÓÚ', 'ãÌÇğ', 'Ë¾Í½', 'Ë¾¿Õ', 'ØÁ¹Ù', 'Ë¾¿Ü', 'Øë', '¶½', '×Ó³µ', 'ò§Ëï', '¶ËÄ¾', 'Î×Âí',
    '¹«Î÷', 'Æáµñ', 'ÀÖÕı', 'ÈÀæá', '¹«Á¼', 'ÍØ°Î', '¼Ğ¹È', 'Ô×¸¸', '¹ÈÁ»', '½ú', '³ş', 'ãÆ', '·¨',
    'Èê', 'Û³', 'Í¿', 'ÇÕ', '¶Î¸É', '°ÙÀï', '¶«¹ù', 'ÄÏÃÅ', 'ºôÑÓ', '¹é', 'º£', 'ÑòÉà', 'Î¢Éú', 'ÔÀ',
    'Ë§', 'çÃ', '¿º', '¿ö', 'ºó', 'ÓĞ', 'ÇÙ', 'ÁºÇğ', '×óÇğ', '¶«ÃÅ', 'Î÷ÃÅ', 'ÉÌ', 'Ä²', 'ÙÜ',
    'Ù¦', '²®', 'ÉÍ', 'ÄÏ¹¬', 'Ä«', '¹ş', 'ÚÛ', 'óÎ', 'Äê', '°®', 'Ñô', 'Ù¡', 'µÚÎå',
    'ÑÔ', '¸£');
var
  I, L: Integer;
begin
L := 1 + Random(2);
Result := '';
for I := 0 to L - 1 do
  Result := Result + WideChar($4E00 + Random(20902));
Result := FirstNames[Random(Length(FirstNames))] + Result;
end;

function CreateDemoDataSet(AOwner: TComponent; ARecordCount: Integer)
  : TQDataSet;
var
  AFdId, AFdAge, AFdBirthday,AFdName, AFdSex, AFdScale, AFdComment: TField;
  I: Integer;
begin
Result := TQDataSet.Create(AOwner);
Result.FieldDefs.Add('Id', ftString, 10);
Result.FieldDefs.Add('Age', ftInteger);
Result.FieldDefs.Add('Birthday',ftDateTime);
Result.FieldDefs.Add('Name', ftWideString, 30);
Result.FieldDefs.Add('Sex', ftBoolean);
Result.FieldDefs.Add('Scale', ftFloat);
Result.FieldDefs.Add('Comment', ftWideMemo);
Result.CreateDataSet;
Result.DisableControls;
try
  AFdId := Result.FieldByName('Id');
  AFdAge := Result.FieldByName('Age');
  AFdName := Result.FieldByName('Name');
  AFdSex := Result.FieldByName('Sex');
  AFdScale := Result.FieldByName('Scale');
  AFdBirthday:=Result.FieldByName('Birthday');
  AFdComment := Result.FieldByName('Comment');
  for I := 1 to ARecordCount - 1 do
    begin
    Result.Append;
    AFdId.AsString := 'CC_' + IntToStr(I);
    AFdName.AsString := GetHumanName;
    AFdBirthday.AsDateTime:=IncYear(IncDay(Now,-Random(50)),-10-Random(50));
    AFdAge.AsInteger := YearsBetween(Now,AFdBirthday.AsDateTime)+1;
    AFdSex.AsBoolean := Random(10) > 5;
    AFdScale.AsFloat := Random(1000) / 10;
    AFdComment.AsString := 'Comment for ' + AFdName.AsString;
    Result.Post;
    end;
  Result.Append;
  AFdId.AsString := 'CC_' + IntToStr(ARecordCount);
  Result.Post;
finally
  Result.EnableControls;
end;
end;



procedure TForm1.Button1Click(Sender: TObject);
var
  t: Cardinal;
begin
FDataSet.Filtered := false;
FDataSet.OnFilterRecord := nil;
FDataSet.Filter := ComboBox1.Text;
t := GetTickCount;
FDataSet.Filtered := Length(FDataSet.Filter) > 0;
t := GetTickCount - t;
Panel2.Caption := '¹ıÂËÓÃÊ± ' + IntToStr(t) + 'ms(·ûºÏÌõ¼ş¼ÇÂ¼Êı£º' +
  IntToStr(FDataSet.RecordCount) + ')';
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  t: Cardinal;
begin
FDataSet.Filtered := false;
FDataSet.Filter := '';
t := GetTickCount;
FDataSet.OnFilterRecord := DoCustomFilter;
FDataSet.Filtered := True;
t := GetTickCount - t;
Panel2.Caption := '¹ıÂËÓÃÊ± ' + IntToStr(t) + 'ms(·ûºÏÌõ¼ş¼ÇÂ¼Êı£º' +
  IntToStr(FDataSet.RecordCount) + ')';
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  dt: TQDataSet;
begin
  dt := TQDataSet.Create(nil);
  try
    dt.Clone(FDataSet);
    with dt do
    begin
      Filtered := false;
      OnFilterRecord := nil;
      Filter := ComboBox1.Text;
      Filtered := Length(Filter) > 0;
    end;
    FDataSet.DisableControls;
    dt.DisableControls;
    with dt do
    begin
      First;
      while not Eof do
      begin
        edit;
        FieldByName('Name').AsString := 'ÕÅÀÏ¶ş';
        Post;
        next;
      end;
    end;
  finally
    FDataSet.EnableControls;
    dt.EnableControls;
    dt.Free;
  end;

end;

procedure TForm1.DoCustomFilter(DataSet: TDataSet; var Accept: Boolean);
begin
Accept := DataSet.FieldByName('Age').AsInteger > 30;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
FDataSet := CreateDemoDataSet(Self, 10000);
DataSource1.DataSet := FDataSet;
end;

procedure TForm1.Panel2Click(Sender: TObject);
var
  S:QStringA;
begin
S:=QString.Utf8Encode('ĞÂÉú¶ùĞÕÃû');
ShowMessage(BinToHex(S));
end;

end.
