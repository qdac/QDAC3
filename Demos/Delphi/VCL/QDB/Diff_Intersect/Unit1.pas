unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Data.DB, Vcl.Grids,
  Vcl.DBGrids, Vcl.ExtCtrls,QDB,DateUtils;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Bevel1: TBevel;
    Button1: TButton;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Panel5: TPanel;
    Panel6: TPanel;
    DBGrid2: TDBGrid;
    Panel9: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    DBGrid3: TDBGrid;
    Panel3: TPanel;
    Panel4: TPanel;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    DataSource2: TDataSource;
    DataSource3: TDataSource;
    Button2: TButton;
    Label1: TLabel;
    Edit1: TEdit;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FSource1,FSource2,FDest:TQDataSet;
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
function CreateDemoDataSet(AOwner: TComponent; AStartId,ARecordCount: Integer)
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
    AFdId.AsString := 'CC_' + IntToStr(AStartId+I);
    AFdName.AsString := GetHumanName;
    AFdBirthday.AsDateTime := IncYear(IncDay(Now, -Random(50)),
      -10 - Random(50));
    AFdAge.AsInteger := YearsBetween(Now, AFdBirthday.AsDateTime) + 1;
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
FDest.Diff(FSource1,FSource2,Edit1.Text,CheckBox1.Checked,CheckBox2.Checked);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
FDest.Intersect(FSource1,FSource2,Edit1.Text);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  I: Integer;
  J: Integer;
begin
FSource1:=CreateDemoDataSet(Self,0,20);
FSource2:=CreateDemoDataSet(Self,20,20);
//≤Â»Î10Ãı÷ÿ∏¥º«¬º
FSource1.First;
for I := 0 to 9 do
  begin
  FSource2.Append;
  for J := 0 to FSource2.Fields.Count-1 do
    FSource2.Fields[J].Value:=FSource1.Fields[J].Value;
  FSource2.Post;
  FSource1.Next;
  end;
FSource1.First;
FSource2.First;
DataSource1.DataSet:=FSource1;
DataSource3.DataSet:=FSource2;
FDest:=TQDataSet.Create(Self);
FDest.Sort:='Id NASC';
DataSource2.DataSet:=FDest;
end;

end.
