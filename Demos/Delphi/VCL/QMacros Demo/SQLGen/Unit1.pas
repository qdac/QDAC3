unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, DateUtils, QString, QMacros, DB, ADODB,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Grids, Vcl.DBGrids;

type
  TForm1 = class(TForm)
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    Memo1: TMemo;
    Panel1: TPanel;
    Button1: TButton;
    edtExp: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FDataSet: TDataSet;
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
  : TADODataSet;
var
  AFdId, AFdAge, AFdBirthday, AFdName, AFdSex, AFdScale, AFdComment: TField;
  I: Integer;
begin
  Result := TADODataSet.Create(AOwner);
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
      Result.Post;
    end;
  finally
    Result.EnableControls;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  AMacros: TQMacroManager;
begin
  AMacros := TQMacroManager.Create;
  AMacros.BooleanAsInt := True;
  FDataSet.DisableControls;
  Memo1.Lines.BeginUpdate;
  try
    Memo1.Lines.Clear;
    AMacros.Push(FDataSet, '');
    AMacros.SetMacroMissed(
      procedure(ASender: TQMacroManager; AName: QStringW; const AQuoter: QCharW;
        var AHandled: Boolean)
      begin
        AHandled := StartWithW(PQCharW(AName), 'Repeat:', True);
        if AHandled then
          ASender.Push(AName,
            procedure(AMacro: TQMacroItem; const AQuoter: QCharW)
            var
              AReplace: TQMacroComplied;
              AHelper: TQStringCatHelperW;
            begin
              AReplace := AMacros.Complie(DequotedStrW(Copy(AMacro.Name, 8,
                MaxInt), '"'), '[', ']');
              if Assigned(AReplace) then
              begin
                AHelper := TQStringCatHelperW.Create;
                try
                  FDataSet.First;
                  while not FDataSet.Eof do
                  begin
                    AHelper.Cat(AReplace.Replace).Cat(SLineBreak);
                    FDataSet.Next;
                  end;
                  AMacro.Value.Value := AHelper.Value;
                finally
                  FreeAndNil(AHelper);
                  FreeAndNil(AReplace);
                end;
              end;
            end);
      end);
    Memo1.Lines.Text := AMacros.Replace(edtExp.Text, '[', ']');
  finally
    Memo1.Lines.EndUpdate;
    FreeAndNil(AMacros);
    FDataSet.EnableControls;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FDataSet := CreateDemoDataSet(Self, 100);
  DataSource1.DataSet := FDataSet;
end;

end.
