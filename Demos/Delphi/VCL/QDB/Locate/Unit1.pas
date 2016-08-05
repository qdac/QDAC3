unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,QString, QValue, QDB,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.StdCtrls, Vcl.DBCtrls,
  Vcl.Grids, Vcl.DBGrids, Vcl.ExtCtrls, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Comp.DataSet, FireDAC.Comp.Client;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    ComboBox1: TComboBox;
    DBGrid1: TDBGrid;
    DBMemo1: TDBMemo;
    Panel2: TPanel;
    DataSource1: TDataSource;
    ComboBox2: TComboBox;
    Label2: TLabel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    CheckBox3: TCheckBox;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    { Private declarations }
    FDataSet: TQDataSet;
    function GetFilterOptions: TFilterOptions;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function GetHumanName: String;
const
  // ÖÐ¹ú°Ù¼ÒÐÕ (Chinese Last Names)
  FirstNames: array [0 .. 503] of String = ('ÕÔ', 'Ç®', 'Ëï', 'Àî', 'ÖÜ', 'Îâ', 'Ö£',
    'Íõ', '·ë', '³Â', 'ñÒ', 'ÎÀ', '½¯', 'Éò', 'º«', 'Ñî', 'Öì', 'ÇØ', 'ÓÈ', 'Ðí', 'ºÎ', 'ÂÀ',
    'Ê©', 'ÕÅ', '¿×', '²Ü', 'ÑÏ', '»ª', '½ð', 'Îº', 'ÌÕ', '½ª', 'ÆÝ', 'Ð»', '×Þ', 'Ó÷', '°Ø',
    'Ë®', 'ñ¼', 'ÕÂ', 'ÔÆ', 'ËÕ', 'ÅË', '¸ð', 'ÞÉ', '·¶', 'Åí', 'ÀÉ', 'Â³', 'Î¤', '²ý', 'Âí',
    'Ãç', '·ï', '»¨', '·½', 'Óá', 'ÈÎ', 'Ô¬', 'Áø', 'Ûº', '±«', 'Ê·', 'ÌÆ', '·Ñ', 'Á®', 'á¯',
    'Ñ¦', 'À×', 'ºØ', 'Äß', 'ÌÀ', 'ëø', 'Òó', 'ÂÞ', '±Ï', 'ºÂ', 'Úù', '°²', '³£', 'ÀÖ', 'ÓÚ',
    'Ê±', '¸µ', 'Æ¤', '±å', 'Æë', '¿µ', 'Îé', 'Óà', 'Ôª', '²·', '¹Ë', 'ÃÏ', 'Æ½', '»Æ', 'ºÍ',
    'ÄÂ', 'Ïô', 'Òü', 'Ò¦', 'ÉÛ', '¿°', 'Íô', 'Æî', 'Ã«', 'Óí', 'µÒ', 'Ã×', '±´', 'Ã÷', 'ê°',
    '¼Æ', '·ü', '³É', '´÷', 'Ì¸', 'ËÎ', 'Ã©', 'ÅÓ', 'ÐÜ', '¼Í', 'Êæ', 'Çü', 'Ïî', '×£', '¶­',
    'Á»', '¶Å', 'Èî', 'À¶', 'ãÉ', 'Ï¯', '¼¾', 'Âé', 'Ç¿', '¼Ö', 'Â·', 'Â¦', 'Î£', '½­', 'Í¯',
    'ÑÕ', '¹ù', 'Ã·', 'Ê¢', 'ÁÖ', 'µó', 'ÖÓ', 'Ðì', 'Çñ', 'Âæ', '¸ß', 'ÏÄ', '²Ì', 'Ìï', '·®',
    'ºú', 'Áè', '»ô', 'ÓÝ', 'Íò', 'Ö§', '¿Â', 'êÃ', '¹Ü', 'Â¬', 'Äª', '¾­', '·¿', 'ôÃ', 'çÑ',
    '¸É', '½â', 'Ó¦', '×Ú', '¶¡', 'Ðû', 'êÚ', 'µË', 'Óô', 'µ¥', 'º¼', 'ºé', '°ü', 'Öî', '×ó',
    'Ê¯', '´Þ', '¼ª', 'Å¥', '¹¨', '³Ì', 'ïú', 'ÐÏ', '»¬', 'Åá', 'Â½', 'ÈÙ', 'ÎÌ', 'Ü÷', 'Ñò',
    'ì¶', '»Ý', 'Õç', 'ôð', '¼Ò', '·â', 'ÜÇ', 'ôà', '´¢', '½ù', '¼³', 'Úû', 'ÃÓ', 'ËÉ', '¾®',
    '¶Î', '¸»', 'Î×', 'ÎÚ', '½¹', '°Í', '¹­', 'ÄÁ', 'Úó', 'É½', '¹È', '³µ', 'ºî', 'åµ', 'Åî',
    'È«', 'Û­', '°à', 'Ñö', 'Çï', 'ÖÙ', 'ÒÁ', '¹¬', 'Äþ', '³ð', 'èï', '±©', '¸Ê', 'î×', 'À÷',
    'ÈÖ', '×æ', 'Îä', '·û', 'Áõ', '¾°', 'Õ²', 'Êø', 'Áú', 'Ò¶', 'ÐÒ', 'Ë¾', 'ÉØ', 'Û¬', 'Àè',
    '¼»', '±¡', 'Ó¡', 'ËÞ', '°×', '»³', 'ÆÑ', 'Û¢', '´Ó', '¶õ', 'Ë÷', 'ÏÌ', '¼®', 'Àµ', '×¿',
    'Ýþ', 'ÍÀ', 'ÃÉ', '³Ø', 'ÇÇ', 'Òõ', '™ä', 'ñã', 'ÄÜ', '²Ô', 'Ë«', 'ÎÅ', 'Ý·', 'µ³', 'µÔ',
    'Ì·', '¹±', 'ÀÍ', 'åÌ', '¼§', 'Éê', '·ö', '¶Â', 'È½', 'Ô×', 'Ûª', 'Óº', 'ôª', 'è³', 'É£',
    '¹ð', 'å§', 'Å£', 'ÊÙ', 'Í¨', '±ß', 'ìè', 'Ñà', '¼½', 'Û£', 'ÆÖ', 'ÉÐ', 'Å©', 'ÎÂ', '±ð',
    '×¯', 'êÌ', '²ñ', 'öÄ', 'ÑÖ', '³ä', 'Ä½', 'Á¬', 'Èã', 'Ï°', '»Â', '°¬', 'Óã', 'ÈÝ', 'Ïò',
    '¹Å', 'Ò×', 'É÷', '¸ê', 'ÁÎ', '¸ý', 'ÖÕ', 'ôß', '¾Ó', 'ºâ', '²½', '¶¼', '¹¢', 'Âú', 'ºë',
    '¿ï', '¹ú', 'ÎÄ', '¿Ü', '¹ã', 'Â»', 'ãÚ', '¶«', 'Å¹', 'ì¯', 'ÎÖ', 'Àû', 'Îµ', 'Ô½', 'Ùç',
    'Â¡', 'Ê¦', '¹®', 'ØÇ', 'Äô', 'êË', '¹´', '°½', 'ÈÚ', 'Àä', 'ö¤', 'ÐÁ', 'ãÛ', 'ÄÇ', '¼ò',
    'ÈÄ', '¿Õ', 'Ôø', 'Îã', 'É³', 'Ø¿', 'Ñø', '¾Ï', 'Ðë', '·á', '³²', '¹Ø', 'Øá', 'Ïà', '²é',
    'áá', '¾£', 'ºì', 'ÓÎ', 'óÃ', 'È¨', 'åÖ', '¸Ç', 'Òæ', '»¸', '¹«', 'ÍòÙ¹', 'Ë¾Âí', 'ÉÏ¹Ù',
    'Å·Ñô', 'ÏÄºî', 'Öî¸ð', 'ÎÅÈË', '¶«·½', 'ºÕÁ¬', '»Ê¸¦', 'Î¾³Ù', '¹«Ñò', 'å£Ì¨', '¹«Ò±', '×ÚÕþ',
    'å§Ñô', '´¾ÓÚ', 'µ¥ÓÚ', 'Ì«Êå', 'ÉêÍÀ', '¹«Ëï', 'ÖÙËï', 'ÐùÔ¯', 'Áîºü', 'ÖÓÀë', 'ÓîÎÄ', '³¤Ëï',
    'Ä½ÈÝ', 'ÏÊÓÚ', 'ãÌÇð', 'Ë¾Í½', 'Ë¾¿Õ', 'ØÁ¹Ù', 'Ë¾¿Ü', 'Øë', '¶½', '×Ó³µ', 'ò§Ëï', '¶ËÄ¾', 'Î×Âí',
    '¹«Î÷', 'Æáµñ', 'ÀÖÕý', 'ÈÀæá', '¹«Á¼', 'ÍØ°Î', '¼Ð¹È', 'Ô×¸¸', '¹ÈÁ»', '½ú', '³þ', 'ãÆ', '·¨',
    'Èê', 'Û³', 'Í¿', 'ÇÕ', '¶Î¸É', '°ÙÀï', '¶«¹ù', 'ÄÏÃÅ', 'ºôÑÓ', '¹é', 'º£', 'ÑòÉà', 'Î¢Éú', 'ÔÀ',
    'Ë§', 'çÃ', '¿º', '¿ö', 'ºó', 'ÓÐ', 'ÇÙ', 'ÁºÇð', '×óÇð', '¶«ÃÅ', 'Î÷ÃÅ', 'ÉÌ', 'Ä²', 'ÙÜ',
    'Ù¦', '²®', 'ÉÍ', 'ÄÏ¹¬', 'Ä«', '¹þ', 'ÚÛ', 'óÎ', 'Äê', '°®', 'Ñô', 'Ù¡', 'µÚÎå',
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
  AFdId, AFdAge, AFdName, AFdSex, AFdScale, AFdComment: TField;
  I: Integer;
begin
Result := TQDataSet.Create(AOwner);
Result.FieldDefs.Add('Id', ftString, 10);
Result.FieldDefs.Add('Age', ftInteger);
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
  AFdComment := Result.FieldByName('Comment');
  for I := 1 to ARecordCount do
    begin
    Result.Append;
    AFdId.AsString := 'CC_' + IntToStr(I);
    AFdName.AsString := GetHumanName;
    AFdAge.AsInteger := 10 + Random(50);
    AFdSex.AsBoolean := Random(10) > 5;
    AFdScale.AsFloat := Random(1000) / 10;
    AFdComment.AsString := 'Comment for ' + AFdName.AsString;
    Result.Post;
    end;
finally
  Result.EnableControls;
end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
if CheckBox3.Checked then
  begin
  if CheckBox1.Checked then
    FDataSet.Filter := ComboBox2.Text + ' like ''%' + StringReplaceW(ComboBox1.Text,'''','''''',[rfReplaceAll])+'%'''
  else
    FDataSet.Filter := ComboBox2.Text + ' = ' + QuotedStrW(ComboBox1.Text);
  FDataSet.FilterOptions := GetFilterOptions;
  FDataSet.FindNext;
  end
else
  FDataSet.LocateNext(ComboBox2.Text,ComboBox1.Text,[loCaseInsensitive, loPartialKey]);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
if CheckBox3.Checked then
  begin
  if CheckBox1.Checked then
    FDataSet.Filter := ComboBox2.Text + ' like ''%' + StringReplaceW(ComboBox1.Text,'''','''''',[rfReplaceAll])+'%'''
  else
    FDataSet.Filter := ComboBox2.Text + ' = ' + QuotedStrW(ComboBox1.Text);
  FDataSet.FilterOptions := GetFilterOptions;
  FDataSet.FindPrior;
  end
else
  FDataSet.LocatePrior(ComboBox2.Text,ComboBox1.Text,[loCaseInsensitive, loPartialKey]);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
if CheckBox3.Checked then
  begin
  if CheckBox1.Checked then
    FDataSet.Filter := ComboBox2.Text + ' like ''%' + StringReplaceW(ComboBox1.Text,'''','''''',[rfReplaceAll])+'%'''
  else
    FDataSet.Filter := ComboBox2.Text + ' = ' + QuotedStrW(ComboBox1.Text);
  FDataSet.FilterOptions := GetFilterOptions;
  FDataSet.FindFirst;
  end
else
  FDataSet.LocateFirst(ComboBox2.Text,ComboBox1.Text,[loCaseInsensitive, loPartialKey]);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
if CheckBox3.Checked then
  begin
  if CheckBox1.Checked then
    FDataSet.Filter := ComboBox2.Text + ' like ''%' + StringReplaceW(ComboBox1.Text,'''','''''',[rfReplaceAll])+'%'''
  else
    FDataSet.Filter := ComboBox2.Text + ' = ' + QuotedStrW(ComboBox1.Text);
  FDataSet.FilterOptions := GetFilterOptions;
  FDataSet.FindLast;
  end
else
  FDataSet.LocateLast(ComboBox2.Text,ComboBox1.Text,[loCaseInsensitive, loPartialKey]);
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
Application.MessageBox('Õâ½«¶¨Î»µ½µÚÒ»Ìõ Id °üº¬ CC£¬²¢ÇÒÄêÁäÎª 29 µÄ¼ÇÂ¼¡£','¶à×Ö¶Î¶¨Î»',MB_OK or MB_ICONINFORMATION);
FDataSet.LocateFirst('Id,Age',VarArrayOf(['CC',29]),[loPartialKey]);
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
if FDataSet.Exists('Id * "CC_2790%"',[]) then
  ShowMessage('Ö¸¶¨µÄ¼ÇÂ¼´æÔÚ(Found)');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
FDataSet := CreateDemoDataSet(Self, 10000);
FDataSet.ApplyChanges;
DataSource1.DataSet := FDataSet;
end;

function TForm1.GetFilterOptions: TFilterOptions;
begin
Result := [];
if not CheckBox1.Checked then
  Result := Result + [foNoPartialCompare];
if CheckBox2.Checked then
  Result := Result + [foCaseInsensitive];
end;

end.
