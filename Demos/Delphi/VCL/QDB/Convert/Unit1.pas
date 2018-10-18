unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, QString,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids, Vcl.DBGrids,
  Vcl.ExtCtrls, Db, QDB, qconverter_stds, qconverter_fdac, qconverter_csv,
  qconverter_adoxml,
  qsp_zlib, qaes,
  qsp_aes, zlib,
  DateUtils;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    DBGrid1: TDBGrid;
    GroupBox1: TGroupBox;
    chkSaveUnmodified: TCheckBox;
    chkSaveInserted: TCheckBox;
    chkSaveModified: TCheckBox;
    chkSaveDeleted: TCheckBox;
    chkSaveMeta: TCheckBox;
    DataSource1: TDataSource;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Panel2: TPanel;
    Button5: TButton;
    Button6: TButton;
    GroupBox2: TGroupBox;
    chkCompress: TCheckBox;
    chkEncrypt: TCheckBox;
    Button3: TButton;
    Button4: TButton;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    cbxKeyType: TComboBox;
    Label2: TLabel;
    Label3: TLabel;
    edtKey: TEdit;
    Label4: TLabel;
    edtInitVector: TEdit;
    Label5: TLabel;
    cbxCompressLevel: TComboBox;
    Label6: TLabel;
    cbxEncryptMode: TComboBox;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure GroupBox1Click(Sender: TObject);
    procedure GroupBox2Click(Sender: TObject);
  private
    { Private declarations }
    FDataSet: TQDataSet;
    FZLibProcessor: TQZlibStreamProcessor;
    FAESProcessor: TQAESStreamProcessor;
    procedure UpdateButtonStates;
    function CreateConverter(ATypeIndex: Integer): TQConverter;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses qconverter_sql;
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
      AFdBirthday.AsDateTime := IncYear(IncDay(Now, -Random(50)), -Random(5));
      AFdAge.AsInteger := YearsBetween(Now, AFdBirthday.AsDateTime) + 1;
      AFdSex.AsBoolean := Random(10) > 5;
      AFdScale.AsFloat := Random(1000) / 10;
      AFdComment.AsString := 'Comment for ' + AFdName.AsString;
      Result.Post;
    end;
    Result.ApplyChanges;
  finally
    Result.EnableControls;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  AConverter: TQConverter;
begin
  if OpenDialog1.Execute then
  begin
    AConverter := CreateConverter(OpenDialog1.FilterIndex);
    if AConverter <> nil then
    begin
      FDataSet.LoadFromFile(OpenDialog1.FileName, AConverter);
      UpdateButtonStates;
      FreeAndNil(AConverter);
    end;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  ARange: TQExportRanges;
  AConverter: TQConverter;
  T: DWORD;
begin
  ARange := [];
  if chkSaveMeta.Checked then
    ARange := [merMeta];
  if chkSaveInserted.Checked then
    ARange := ARange + [merInserted];
  if chkSaveUnmodified.Checked then
    ARange := ARange + [merUnmodified];
  if chkSaveDeleted.Checked then
    ARange := ARange + [merDeleted];
  if chkSaveModified.Checked then
    ARange := ARange + [merModified];
  if SaveDialog1.Execute then
  begin
    AConverter := CreateConverter(SaveDialog1.FilterIndex);
    if AConverter <> nil then
    begin
      AConverter.ExportRanges := ARange;
      T := GetTickCount;
      FDataSet.SaveToFile(SaveDialog1.FileName, AConverter);
      T := GetTickCount - T;
      ShowMessage('±£´æµ½Á÷ÖÐÓÃÊ±£º' + IntToStr(T) + 'ms');
      FreeAndNil(AConverter);
    end;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if Assigned(FDataSet) then
    FreeAndNil(FDataSet);
  FDataSet := CreateDemoDataSet(Self, 1000);
  DataSource1.DataSet := FDataSet;
  UpdateButtonStates;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  ATemp: TQDataSet;
  I: Integer;
begin
  if Assigned(FDataSet) then
    FDataSet.Active := False;
  FDataSet.AddDataSet(CreateDemoDataSet(FDataSet, 1000));
  ATemp := TQDataSet.Create(FDataSet);
  ATemp.FieldDefs.Add('Year', ftInteger);
  ATemp.FieldDefs.Add('Animal', ftWideString, 20);
  ATemp.FieldDefs.Add('Total', ftFloat);
  ATemp.CreateDataSet;
  for I := 2000 to 2014 do
  begin
    ATemp.Append;
    ATemp.Fields[0].AsInteger := I;
    ATemp.Fields[1].AsString := 'Mouse';
    ATemp.Fields[2].AsFloat := Random(10000) / 10;
    ATemp.Post;
  end;
  for I := 2000 to 2014 do
  begin
    ATemp.Append;
    ATemp.Fields[0].AsInteger := I;
    ATemp.Fields[1].AsString := 'Dog';
    ATemp.Fields[2].AsFloat := Random(10000) / 10;
    ATemp.Post;
  end;
  for I := 2000 to 2014 do
  begin
    ATemp.Append;
    ATemp.Fields[0].AsInteger := I;
    ATemp.Fields[1].AsString := 'Cat';
    ATemp.Fields[2].AsFloat := Random(10000) / 10;
    ATemp.Post;
  end;
  FDataSet.AddDataSet(ATemp);
  FDataSet.AddDataSet(CreateDemoDataSet(FDataSet, 10));
  DataSource1.DataSet := FDataSet;
  UpdateButtonStates;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  if FDataSet.ActiveRecordset > 0 then
    FDataSet.ActiveRecordset := FDataSet.ActiveRecordset - 1;
  UpdateButtonStates;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  if FDataSet.ActiveRecordset + 1 < FDataSet.RecordsetCount then
    FDataSet.ActiveRecordset := FDataSet.ActiveRecordset + 1;
  UpdateButtonStates;
end;

function TForm1.CreateConverter(ATypeIndex: Integer): TQConverter;
var
  I: Integer;
begin
  Result := nil;
  case ATypeIndex of
    1:
      Result := TQBinaryConverter.Create(nil);
    2:
      Result := TQMsgPackConverter.Create(nil);
    3:
      Result := TQJsonConverter.Create(nil);
    4:
      Result := TQFDBinaryConverter.Create(nil);
    5:
      Result := TQFDJsonConverter.Create(nil);
    6:
      Result := TQADOXMLConverter.Create(nil);
      //TQFDXMLConverter.Create(nil);
    7:
      Result := TQTextConverter.Create(nil);
    8:
      Result := TQMSSQLConverter.Create(nil);
    9:
      Result := TQPgSQLConverter.Create(nil);
    10:
      Result := TQMySQLConverter.Create(nil);
  end;
  if ATypeIndex in [8, 9, 10] then
  begin
    TQSQLConverter(Result).AllAsInsert := True;
    for I := 0 to FDataSet.FieldDefs.Count - 1 do
      (FDataSet.FieldDefs[I] as TQFieldDef).Table := 'asc';
  end;
  if chkCompress.Checked then
  begin
    if not Assigned(FZLibProcessor) then
      FZLibProcessor := TQZlibStreamProcessor.Create(Self);
    FZLibProcessor.CompressionLevel :=
      TZCompressionLevel(cbxCompressLevel.ItemIndex);
    Result.StreamProcessors.Add.Processor := FZLibProcessor;
  end;
  if chkEncrypt.Checked then
  begin
    if not Assigned(FAESProcessor) then
    begin
      FAESProcessor := TQAESStreamProcessor.Create(Self);
      // ÑÝÊ¾³ÌÐò£¬ËùÒÔÖ±½ÓÓ²ÉèÖÃÁË³õÊ¼ÏòÁ¿ºÍÃÜÂë
      FAESProcessor.InitVector := edtInitVector.Text;
      if cbxEncryptMode.ItemIndex = 0 then
        FAESProcessor.EncryptMode := emECB
      else
        FAESProcessor.EncryptMode := emCBC;
      FAESProcessor.KeyType := TQAESKeyType(cbxKeyType.ItemIndex);
      FAESProcessor.Password := edtKey.Text;
      FAESProcessor.EncryptMode := emCBC;
    end;
    Result.StreamProcessors.Add.Processor := FAESProcessor;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Button3Click(Sender);
end;

procedure TForm1.GroupBox1Click(Sender: TObject);
var
  ADataSet: TQDataSet;
begin
  ADataSet := TQDataSet.Create(nil);
  try
    ADataSet.LoadFromFile('E:\Tencent\QQ\Data\109867294\FileRecv\adoxml.xml',
      TQADOXMLConverter);
  finally
    FreeObject(ADataSet);
  end;
end;

procedure TForm1.GroupBox2Click(Sender: TObject);
var
  AConverter:TQJsonConverter;
begin
  AConverter:=TQJsonConverter.Create(nil);
  try
    AConverter.DataSet:=FDataSet;
    AConverter.ExportRanges:=[merMeta];//

    ShowMessage(AConverter.AsJson);
  finally
    FreeAndNil(AConverter);
  end;
end;

procedure TForm1.UpdateButtonStates;
begin
  Button5.Enabled := FDataSet.ActiveRecordset > 0;
  Button6.Enabled := FDataSet.ActiveRecordset + 1 < FDataSet.RecordsetCount;
end;

end.
