unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  QDB, DateUtils,
  Grids, DBGrids, DB;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    DBGrid1: TDBGrid;
    Panel5: TPanel;
    Panel6: TPanel;
    DBGrid2: TDBGrid;
    Splitter1: TSplitter;
    DataSource1: TDataSource;
    DataSource2: TDataSource;
    Button1: TButton;
    Bevel1: TBevel;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FSource, FDest: TQDataSet;
    procedure DoAfterScroll(ADataSet: TDataSet);
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
  AFdId, AFdAge, AFdBirthday, AFdName, AFdSex, AFdScale, AFdComment: TField;
  I: Integer;
begin
  Result := TQDataSet.Create(AOwner);
  Result.FieldDefs.Add('Id', ftString, 10);
  Result.FieldDefs.Add('Age', ftInteger);
  Result.FieldDefs.Add('Birthday', ftDateTime);
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
    AFdBirthday := Result.FieldByName('Birthday');
    AFdComment := Result.FieldByName('Comment');
    for I := 1 to ARecordCount do
    begin
      Result.Append;
      AFdId.AsString := 'CC_' + IntToStr(I);
      AFdName.AsString := GetHumanName;
      AFdBirthday.AsDateTime := IncYear(IncDay(Now, -Random(50)),
        -10 - Random(50));
      AFdAge.AsInteger := YearsBetween(Now, AFdBirthday.AsDateTime) + 1;
      AFdSex.AsBoolean := Random(10) > 5;
      AFdScale.AsFloat := Random(1000) / 10;
      AFdComment.AsString := 'Comment for ' + AFdName.AsString;
      Result.Cancel;
    end;
  finally
    Result.EnableControls;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FDest.Clone(FSource);
end;

procedure TForm1.DoAfterScroll(ADataSet: TDataSet);
begin
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FSource := CreateDemoDataSet(Self, 1);
  FSource.AfterScroll := DoAfterScroll;
  FDest := TQDataSet.Create(Self);
  DataSource1.DataSet := FSource;
  DataSource2.DataSet := FDest;
//  ReportMemoryLeaksOnShutDown := True;
end;

end.
