unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids, Vcl.DBGrids,
  Data.DB, Vcl.ExtCtrls, qdb, qvalue, Vcl.DBCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    Label1: TLabel;
    ComboBox1: TComboBox;
    Button1: TButton;
    DBMemo1: TDBMemo;
    Panel2: TPanel;
    Button2: TButton;
    DBNavigator1: TDBNavigator;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    FDataSet: TQDataSet;
    procedure DoSortBySpell(ADataSet: TQDataSet; ARecord1, ARecord2: TQRecord;
      var AResult: Integer);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function GetHumanName: String;
const
  // ÷–π˙∞Ÿº“–’ (Chinese Last Names)
  FirstNames: array [0 .. 503] of String = ('’‘', '«Æ', 'ÀÔ', '¿Ó', '÷‹', 'Œ‚', '÷£',
    'Õı', '∑Î', '≥¬', 'Ò“', 'Œ¿', 'ΩØ', '…Ú', '∫´', '—Ó', '÷Ï', '«ÿ', '”»', '–Ì', '∫Œ', '¬¿',
    ' ©', '’≈', 'ø◊', '≤‹', '—œ', 'ª™', 'Ω', 'Œ∫', 'Ã’', 'Ω™', '∆›', '–ª', '◊ﬁ', '”˜', '∞ÿ',
    'ÀÆ', 'Òº', '’¬', '‘∆', 'À’', '≈À', '∏', 'ﬁ…', '∑∂', '≈Ì', '¿…', '¬≥', 'Œ§', '≤˝', '¬Ì',
    '√Á', '∑Ô', 'ª®', '∑Ω', '”·', '»Œ', '‘¨', '¡¯', '€∫', '±´', ' ∑', 'Ã∆', '∑—', '¡Æ', '·Ø',
    '—¶', '¿◊', '∫ÿ', 'ƒﬂ', 'Ã¿', 'Î¯', '“Û', '¬ﬁ', '±œ', '∫¬', '⁄˘', '∞≤', '≥£', '¿÷', '”⁄',
    ' ±', '∏µ', '∆§', '±Â', '∆Î', 'øµ', 'ŒÈ', '”‡', '‘™', '≤∑', 'πÀ', '√œ', '∆Ω', 'ª∆', '∫Õ',
    'ƒ¬', 'œÙ', '“¸', '“¶', '…€', 'ø∞', 'ÕÙ', '∆Ó', '√´', '”Ì', 'µ“', '√◊', '±¥', '√˜', 'Í∞',
    'º∆', '∑¸', '≥…', '¥˜', 'Ã∏', 'ÀŒ', '√©', '≈”', '–‹', 'ºÕ', ' Ê', '«¸', 'œÓ', '◊£', '∂≠',
    '¡ª', '∂≈', '»Ó', '¿∂', '„…', 'œØ', 'ºæ', '¬È', '«ø', 'º÷', '¬∑', '¬¶', 'Œ£', 'Ω≠', 'ÕØ',
    '—’', 'π˘', '√∑', ' ¢', '¡÷', 'µÛ', '÷”', '–Ï', '«Ò', '¬Ê', '∏ﬂ', 'œƒ', '≤Ã', 'ÃÔ', '∑Æ',
    '∫˙', '¡Ë', 'ªÙ', '”›', 'ÕÚ', '÷ß', 'ø¬', 'Í√', 'π‹', '¬¨', 'ƒ™', 'æ≠', '∑ø', 'Ù√', 'Á—',
    '∏…', 'Ω‚', '”¶', '◊⁄', '∂°', '–˚', 'Í⁄', 'µÀ', '”Ù', 'µ•', '∫º', '∫È', '∞¸', '÷Ó', '◊Û',
    ' Ø', '¥ﬁ', 'º™', '≈•', 'π®', '≥Ã', 'Ô˙', '–œ', 'ª¨', '≈·', '¬Ω', '»Ÿ', 'ŒÃ', '‹˜', '—Ú',
    'Ï∂', 'ª›', '’Á', 'Ù', 'º“', '∑‚', '‹«', 'Ù‡', '¥¢', 'Ω˘', 'º≥', '⁄˚', '√”', 'À…', 'æÆ',
    '∂Œ', '∏ª', 'Œ◊', 'Œ⁄', 'Ωπ', '∞Õ', 'π≠', 'ƒ¡', '⁄Û', '…Ω', 'π»', '≥µ', '∫Ó', 'Âµ', '≈Ó',
    '»´', '€≠', '∞‡', '—ˆ', '«Ô', '÷Ÿ', '“¡', 'π¨', 'ƒ˛', '≥', 'ËÔ', '±©', '∏ ', 'Ó◊', '¿˜',
    '»÷', '◊Ê', 'Œ‰', '∑˚', '¡ı', 'æ∞', '’≤', ' ¯', '¡˙', '“∂', '–“', 'Àæ', '…ÿ', '€¨', '¿Ë',
    'ºª', '±°', '”°', 'Àﬁ', '∞◊', 'ª≥', '∆—', '€¢', '¥”', '∂ı', 'À˜', 'œÃ', 'ºÆ', '¿µ', '◊ø',
    '›˛', 'Õ¿', '√…', '≥ÿ', '««', '“ı', 'ô‰', 'Ò„', 'ƒ‹', '≤‘', 'À´', 'Œ≈', '›∑', 'µ≥', 'µ‘',
    'Ã∑', 'π±', '¿Õ', 'ÂÃ', 'ºß', '…Í', '∑ˆ', '∂¬', '»Ω', '‘◊', '€™', '”∫', 'Ù™', 'Ë≥', '…£',
    'π', 'Âß', '≈£', ' Ÿ', 'Õ®', '±ﬂ', 'ÏË', '—‡', 'ºΩ', '€£', '∆÷', '…–', '≈©', 'Œ¬', '±',
    '◊Ø', 'ÍÃ', '≤Ò', 'ˆƒ', '—÷', '≥‰', 'ƒΩ', '¡¨', '»„', 'œ∞', 'ª¬', '∞¨', '”„', '»›', 'œÚ',
    'π≈', '“◊', '…˜', '∏Í', '¡Œ', '∏˝', '÷’', 'Ùﬂ', 'æ”', '∫‚', '≤Ω', '∂º', 'π¢', '¬˙', '∫Î',
    'øÔ', 'π˙', 'Œƒ', 'ø‹', 'π„', '¬ª', '„⁄', '∂´', '≈π', 'ÏØ', 'Œ÷', '¿˚', 'Œµ', '‘Ω', 'ŸÁ',
    '¬°', ' ¶', 'πÆ', 'ÿ«', 'ƒÙ', 'ÍÀ', 'π¥', '∞Ω', '»⁄', '¿‰', 'ˆ§', '–¡', '„€', 'ƒ«', 'ºÚ',
    '»ƒ', 'ø’', '‘¯', 'Œ„', '…≥', 'ÿø', '—¯', 'æœ', '–Î', '∑·', '≥≤', 'πÿ', 'ÿ·', 'œ‡', '≤È',
    '··', 'æ£', '∫Ï', '”Œ', 'Û√', '»®', 'Â÷', '∏«', '“Ê', 'ª∏', 'π´', 'ÕÚŸπ', 'Àæ¬Ì', '…œπŸ',
    '≈∑—Ù', 'œƒ∫Ó', '÷Ó∏', 'Œ≈»À', '∂´∑Ω', '∫’¡¨', 'ª ∏¶', 'Œæ≥Ÿ', 'π´—Ú', 'Â£Ã®', 'π´“±', '◊⁄’˛',
    'Âß—Ù', '¥æ”⁄', 'µ•”⁄', 'Ã´ Â', '…ÍÕ¿', 'π´ÀÔ', '÷ŸÀÔ', '–˘‘Ø', '¡Ó∫¸', '÷”¿Î', '”ÓŒƒ', '≥§ÀÔ',
    'ƒΩ»›', 'œ ”⁄', '„Ã«', 'ÀæÕΩ', 'Àæø’', 'ÿ¡πŸ', 'Àæø‹', 'ÿÎ', '∂Ω', '◊”≥µ', 'ÚßÀÔ', '∂Àƒæ', 'Œ◊¬Ì',
    'π´Œ˜', '∆·µÒ', '¿÷’˝', '»¿Ê·', 'π´¡º', 'Õÿ∞Œ', 'º–π»', '‘◊∏∏', 'π»¡ª', 'Ω˙', '≥˛', '„∆', '∑®',
    '»Í', '€≥', 'Õø', '«’', '∂Œ∏…', '∞Ÿ¿Ô', '∂´π˘', 'ƒœ√≈', '∫Ù—”', 'πÈ', '∫£', '—Ú…‡', 'Œ¢…˙', '‘¿',
    'Àß', 'Á√', 'ø∫', 'øˆ', '∫Û', '”–', '«Ÿ', '¡∫«', '◊Û«', '∂´√≈', 'Œ˜√≈', '…Ã', 'ƒ≤', 'Ÿ‹',
    'Ÿ¶', '≤Æ', '…Õ', 'ƒœπ¨', 'ƒ´', 'π˛', '⁄€', 'ÛŒ', 'ƒÍ', '∞Æ', '—Ù', 'Ÿ°', 'µ⁄ŒÂ',
    '—‘', '∏£');
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
var
  T: Cardinal;
begin
FDataSet.OnCustomSort := nil; // ±£¥Ê≤ª « π”√∂®÷∆µƒ≈≈–ÚπÊ‘Ú
T := GetTickCount;
FDataSet.Sort := ComboBox1.Text;
Panel2.Caption := '≈≈–Ú”√ ±:' + IntToStr(GetTickCount - T) + 'ms(π≤' +
  IntToStr(FDataSet.RecordCount) + '––)';
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  T: Cardinal;
begin
if Assigned(FDataSet.OnCustomSort) then
  FDataSet.OnCustomSort := nil
else
  begin
  T := GetTickCount;
  FDataSet.OnCustomSort := DoSortBySpell;
  Panel2.Caption := '≈≈–Ú”√ ±:' + IntToStr(GetTickCount - T) + 'ms(π≤' +
    IntToStr(FDataSet.RecordCount) + '––)';
  end;
end;

procedure TForm1.DoSortBySpell(ADataSet: TQDataSet;
  ARecord1, ARecord2: TQRecord; var AResult: Integer);
begin
AResult := CompareStringW(
  // ∞¥∫∫”Ô∆¥“Ù≈≈–Ú
  MAKELCID(MAKELANGID(LANG_CHINESE, SUBLANG_CHINESE_SIMPLIFIED),
  SORT_CHINESE_PRCP), NORM_IGNORECASE or NORM_IGNOREWIDTH,
  PWideChar(ARecord1.Values[2].CurrentValue.AsString), -1,
  PWideChar(ARecord2.Values[2].CurrentValue.AsString), -1) - CSTR_EQUAL;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
FDataSet := CreateDemoDataSet(Self, 10000);
DataSource1.DataSet := FDataSet;
DBMemo1.DataField := 'Comment';
DBMemo1.DataSource := DataSource1;
end;

end.
