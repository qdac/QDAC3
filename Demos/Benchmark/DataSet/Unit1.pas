unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, DateUtils,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, DB, QString,
  QWorker, QDB, qconverter_stds, qsp_zlib,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  VirtualTable,
  DBAccess, Uni, adodb, adoint, kbmMemTable, kbmMemBinaryStreamFormat,
  FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, SQLMemMain, SQLMemComMain,
  Datasnap.DBClient, MemTableDataEh, MemTableEh, FireDAC.Stan.StorageBin;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    mmLog: TMemo;
    FDStanStorageBinLink1: TFDStanStorageBinLink;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FQdacDS: TQDataSet;
    FUnidacDS: TVirtualTable;
    FAdoDS: TADODataSet;
    FFdacDS: TFDMemTable;
    FKbmDS: TKbmMemTable;
    FCds: TClientDataSet;
    FEhLibDS: TMemTableEh;
    FSQLMemDS: TSQLMemTable;
    { Private declarations }
    procedure AddFields(ADataSet: TDataSet);
    procedure CloseDataSets;
    procedure Test_Insert;
    procedure Test_Append;
    procedure Test_Locate;
    procedure Test_Sort;
    procedure Test_Filter;
    procedure Test_Stream;
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
  AFdId, AFdAge, AFdBirthday, AFdName, AFdSex, AFdScale, AFdComment: TField;
  I: Integer;
begin
  Result := TQDataSet.Create(AOwner);

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

procedure TForm1.AddFields(ADataSet: TDataSet);
begin
  ADataSet.FieldDefs.Clear;
  ADataSet.FieldDefs.Add('Id', ftString, 10);
  ADataSet.FieldDefs.Add('Age', ftInteger);
  ADataSet.FieldDefs.Add('Birthday', ftDateTime);
  ADataSet.FieldDefs.Add('Name', ftWideString, 30);
  ADataSet.FieldDefs.Add('Sex', ftBoolean);
  ADataSet.FieldDefs.Add('Scale', ftFloat);
  ADataSet.FieldDefs.Add('Comment', ftWideMemo);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Test_Insert;
  Test_Append;
  Test_Locate;
  Test_Sort;
  Test_Filter;
  Test_Stream;
end;

procedure TForm1.CloseDataSets;
begin
  FQdacDS.Close;
  FUnidacDS.Close;
  FAdoDS.Close;
  FFdacDS.Close;
  FKbmDS.Close;
  FCds.Close;
  FEhLibDS.Close;
  FSQLMemDS.Close;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  V: Int64;
  B: array [0 .. 7] of Byte absolute V;
  S: String;
begin
  FQdacDS := TQDataSet.Create(Self);
  FUnidacDS := TVirtualTable.Create(Self);
  FAdoDS := TADODataSet.Create(Self);
  FFdacDS := TFDMemTable.Create(Self);
  FKbmDS := TKbmMemTable.Create(Self);
  FCds := TClientDataSet.Create(Self);
  FEhLibDS := TMemTableEh.Create(Self);
  FSQLMemDS := TSQLMemTable.Create(Self);
  V := 127;
  S := IntToStr(V) + '->' + BinToHex(@B[0], 8) + #13#10;
  V := 32767;
  S := S + IntToStr(V) + '->' + BinToHex(@B[0], 8) + #13#10;
  V := 100000;
  S := S + IntToStr(V) + '->' + BinToHex(@B[0], 8) + #13#10;
  V := Int64(MaxInt) + 1000;
  S := S + IntToStr(V) + '->' + BinToHex(@B[0], 8) + #13#10;
  ShowMessage(S);
end;

procedure TForm1.Test_Append;
var
  TQ, TS, TU, TA, TF, TK, TC, TE: array [0 .. 10] of Int64;
  ATotalTime: Int64;
  ANames: array of String;
  ABirthdays: array of TDateTime;
  S: String;
  procedure LogResult(ATimes: array of Int64);
  var
    I: Integer;
  begin
    S := '  √øÕÚÃı”√ ±(ms):';
    for I := 0 to 9 do
      S := S + FormatFloat('0.0', (ATimes[I + 1] - ATimes[I]) / 10) + ' ';
    mmLog.Lines.Add(S);
    mmLog.Lines.Add('  ◊‹º∆”√ ±(ms):' + FormatFloat('0.0',
      (ATimes[10] - ATimes[0]) / 10));
  end;
  procedure GenNames;
  var
    I: Integer;
  begin
    SetLength(ANames, 100000);
    SetLength(ABirthdays, 100000);
    for I := 0 to 99999 do
    begin
      ANames[I] := GetHumanName;
      ABirthdays[I] := IncYear(IncDay(Now, -Random(50)), -10 - Random(50));
    end;
  end;

  procedure DoTest(ADataSet: TDataSet; var ATimes: array of Int64);
  var
    AFdId, AFdAge, AFdBirthday, AFdName, AFdSex, AFdScale, AFdComment: TField;
    I: Integer;
  begin
    AFdId := ADataSet.FieldByName('Id');
    AFdAge := ADataSet.FieldByName('Age');
    AFdName := ADataSet.FieldByName('Name');
    AFdSex := ADataSet.FieldByName('Sex');
    AFdScale := ADataSet.FieldByName('Scale');
    AFdBirthday := ADataSet.FieldByName('Birthday');
    AFdComment := ADataSet.FieldByName('Comment');
    ATimes[0] := GetTimeStamp;
    ADataSet.DisableControls;
    try
      for I := 1 to 100000 do
      begin
        ADataSet.Append;
        AFdId.AsString := 'CC_' + IntToStr(I);
        AFdName.AsString := ANames[I - 1];
        AFdBirthday.AsDateTime := ABirthdays[I - 1];
        AFdAge.AsInteger := YearsBetween(Now, AFdBirthday.AsDateTime) + 1;
        AFdSex.AsBoolean := Random(10) > 5;
        AFdScale.AsFloat := Random(1000) / 10;
        AFdComment.AsString := 'Comment for ' + AFdName.AsString;
        if (I mod 10000) = 0 then
          ATimes[I div 10000] := GetTimeStamp;
        ADataSet.Post;
      end;
      LogResult(ATimes);
    finally
      ADataSet.EnableControls;
    end;
  end;

begin
  CloseDataSets;
  AddFields(FQdacDS);
  FQdacDS.CreateDataSet;
  AddFields(FUnidacDS);
  FUnidacDS.Open;
  AddFields(FAdoDS);
  FAdoDS.CreateDataSet;
  AddFields(FFdacDS);
  FFdacDS.CreateDataSet;
  AddFields(FKbmDS);
  FKbmDS.CreateTable;
  FKbmDS.Open;
  AddFields(FCds);
  FCds.CreateDataSet;
  AddFields(FEhLibDS);
  FEhLibDS.CreateDataSet;
  FSQLMemDS.EmptyTable;
  AddFields(FSQLMemDS);
  FSQLMemDS.Open;
  GenNames;
  mmLog.Lines.Add('°æ◊∑º”ÀŸ∂»≤‚ ‘°ø');
  mmLog.Lines.Add('≤‚ ‘ QDAC.TQDataSet...');
  DoTest(FQdacDS, TQ);
  mmLog.Lines.Add('≤‚ ‘ SQLMemTable...');
  DoTest(FSQLMemDS, TS);
  mmLog.Lines.Add('≤‚ ‘ UniDAC.TVirtualTable...');
  DoTest(FUnidacDS, TU);
  mmLog.Lines.Add('≤‚ ‘ ADO.TADODataSet...');
  DoTest(FAdoDS, TA);
  mmLog.Lines.Add('≤‚ ‘ FDAC.TFDMemTable...');
  FFdacDS.BeginBatch();
  DoTest(FFdacDS, TF);
  FFdacDS.EndBatch;
  mmLog.Lines.Add('≤‚ ‘ FDAC.TKBMMemTable...');
  DoTest(FKbmDS, TK);
  mmLog.Lines.Add('≤‚ ‘ DataSnap.TClientDataSet...');
  DoTest(FCds, TC);
  mmLog.Lines.Add('≤‚ ‘ EhLib.TMemTableEh...');
  DoTest(FEhLibDS, TE);
  ATotalTime := TQ[10] - TQ[0];
  mmLog.Lines.Add('QDAC vs SQLMemTable:' + FormatFloat('0.00',
    (TS[10] - TS[0]) / ATotalTime) + ' X');
  mmLog.Lines.Add('QDAC vs UniDAC:' + FormatFloat('0.00',
    (TU[10] - TU[0]) / ATotalTime) + ' X');
  mmLog.Lines.Add('QDAC vs ADO:' + FormatFloat('0.00',
    (TA[10] - TA[0]) / ATotalTime) + ' X');
  mmLog.Lines.Add('QDAC vs FDAC:' + FormatFloat('0.00',
    (TF[10] - TF[0]) / ATotalTime) + ' X');
  mmLog.Lines.Add('QDAC vs KBM:' + FormatFloat('0.00',
    (TK[10] - TK[0]) / ATotalTime) + ' X');
  mmLog.Lines.Add('QDAC vs DataSnap:' + FormatFloat('0.00',
    (TC[10] - TC[0]) / ATotalTime) + ' X');
  mmLog.Lines.Add('QDAC vs EhLib:' + FormatFloat('0.00',
    (TE[10] - TE[0]) / ATotalTime) + ' X');
end;

procedure TForm1.Test_Filter;
var
  TQ, TU, TS, TA, TF, TK, TC, TE: Int64;
  procedure DoFilter(var TS: Int64; ADataSet: TDataSet);
  begin
    TS := GetTimeStamp;
    ADataSet.Filter := 'Age>50';
    ADataSet.Filtered := True;
    mmLog.Lines.Add(' π˝¬À∫Ûº«¬º ˝£∫' + IntToStr(ADataSet.RecordCount));
    TS := GetTimeStamp - TS;
    ADataSet.Filtered := False;
  end;

begin
  mmLog.Lines.Add('°æ≈≈–Ú≤‚ ‘°ø');
  mmLog.Lines.Add('QDAC.Filter');
  DoFilter(TQ, FQdacDS);
  mmLog.Lines.Add('  ' + FormatFloat('0.#', TQ / 10) + 'ms');
  mmLog.Lines.Add('SQLMemTable.Filter');
  DoFilter(TS, FUnidacDS);
  mmLog.Lines.Add('  ' + FormatFloat('0.#', TS / 10) + 'ms');
  mmLog.Lines.Add('UniDAC.Filter');
  DoFilter(TU, FUnidacDS);
  mmLog.Lines.Add('  ' + FormatFloat('0.#', TU / 10) + 'ms');
  mmLog.Lines.Add('ADO.Filter');
  DoFilter(TA, FAdoDS);
  mmLog.Lines.Add('  ' + FormatFloat('0.#', TA / 10) + 'ms');
  mmLog.Lines.Add('FireDAC.Filter');
  DoFilter(TF, FFdacDS);
  mmLog.Lines.Add('  ' + FormatFloat('0.#', TF / 10) + 'ms');
  mmLog.Lines.Add('KBM.Filter');
  DoFilter(TK, FKbmDS);
  mmLog.Lines.Add('  ' + FormatFloat('0.#', TK / 10) + 'ms');
  mmLog.Lines.Add('DataSnap.Filter');
  DoFilter(TC, FCds);
  mmLog.Lines.Add('  ' + FormatFloat('0.#', TC / 10) + 'ms');
  mmLog.Lines.Add('EhLib.Filter');
  DoFilter(TE, FEhLibDS);
  mmLog.Lines.Add('  ' + FormatFloat('0.#', TE / 10) + 'ms');
  mmLog.Lines.Add('QDAC vs SQLMemTable:' + FormatFloat('0.00', TS / TQ) + ' X');
  mmLog.Lines.Add('QDAC vs ADO:' + FormatFloat('0.00', TA / TQ) + ' X');
  mmLog.Lines.Add('QDAC vs FDAC:' + FormatFloat('0.00', TF / TQ) + ' X');
  mmLog.Lines.Add('QDAC vs KBM:' + FormatFloat('0.00', TK / TQ) + ' X');
  mmLog.Lines.Add('QDAC vs DataSnap:' + FormatFloat('0.00', TC / TQ) + ' X');
  mmLog.Lines.Add('QDAC vs EhLib:' + FormatFloat('0.00', TE / TQ) + ' X');
end;

procedure TForm1.Test_Insert;
var
  TQ, TS, TU, TA, TF, TK, TC, TE: array [0 .. 10] of Int64;
  ATotalTime: Int64;
  ANames: array of String;
  ABirthdays: array of TDateTime;
  S: String;
  procedure LogResult(ATimes: array of Int64);
  var
    I: Integer;
  begin
    S := '  √øÕÚÃı”√ ±(ms):';
    for I := 0 to 9 do
      S := S + FormatFloat('0.0', (ATimes[I + 1] - ATimes[I]) / 10) + ' ';
    mmLog.Lines.Add(S);
    mmLog.Lines.Add('  ◊‹º∆”√ ±(ms):' + FormatFloat('0.0',
      (ATimes[10] - ATimes[0]) / 10));
  end;
  procedure GenNames;
  var
    I: Integer;
  begin
    SetLength(ANames, 100000);
    SetLength(ABirthdays, 100000);
    for I := 0 to 99999 do
    begin
      ANames[I] := GetHumanName;
      ABirthdays[I] := IncYear(IncDay(Now, -Random(50)), -10 - Random(50));
    end;
  end;

  procedure DoTest(ADataSet: TDataSet; var ATimes: array of Int64);
  var
    AFdId, AFdAge, AFdBirthday, AFdName, AFdSex, AFdScale, AFdComment: TField;
    I: Integer;
  begin
    AFdId := ADataSet.FieldByName('Id');
    AFdAge := ADataSet.FieldByName('Age');
    AFdName := ADataSet.FieldByName('Name');
    AFdSex := ADataSet.FieldByName('Sex');
    AFdScale := ADataSet.FieldByName('Scale');
    AFdBirthday := ADataSet.FieldByName('Birthday');
    AFdComment := ADataSet.FieldByName('Comment');
    ATimes[0] := GetTimeStamp;
    ADataSet.DisableControls;
    try
      for I := 1 to 100000 do
      begin
        ADataSet.Insert;
        AFdId.AsString := 'CC_' + IntToStr(I);
        AFdName.AsString := ANames[I - 1];
        AFdBirthday.AsDateTime := ABirthdays[I - 1];
        AFdAge.AsInteger := YearsBetween(Now, AFdBirthday.AsDateTime) + 1;
        AFdSex.AsBoolean := Random(10) > 5;
        AFdScale.AsFloat := Random(1000) / 10;
        AFdComment.AsString := 'Comment for ' + AFdName.AsString;
        if (I mod 10000) = 0 then
          ATimes[I div 10000] := GetTimeStamp;
        ADataSet.Post;
      end;
      LogResult(ATimes);
    finally
      ADataSet.EnableControls;
    end;
  end;

begin
  CloseDataSets;
  AddFields(FQdacDS);
  FQdacDS.CreateDataSet;
  AddFields(FUnidacDS);
  FUnidacDS.Open;
  AddFields(FAdoDS);
  FAdoDS.CreateDataSet;
  AddFields(FFdacDS);
  FFdacDS.CreateDataSet;
  AddFields(FKbmDS);
  FKbmDS.CreateTable;
  FKbmDS.Open;
  AddFields(FCds);
  FCds.CreateDataSet;
  AddFields(FEhLibDS);
  FEhLibDS.CreateDataSet;
  AddFields(FSQLMemDS);
  FSQLMemDS.Open;
  GenNames;
  mmLog.Lines.Add('°æ≤Â»ÎÀŸ∂»≤‚ ‘°ø');
  mmLog.Lines.Add('≤‚ ‘ QDAC.TQDataSet...');
  DoTest(FQdacDS, TQ);
  mmLog.Lines.Add('≤‚ ‘ SQLMemTable...');
  DoTest(FSQLMemDS, TS);
  mmLog.Lines.Add('≤‚ ‘ UniDAC.TQDataSet...');
  DoTest(FUnidacDS, TU);
  mmLog.Lines.Add('≤‚ ‘ ADO.TADODataSet...');
  DoTest(FAdoDS, TA);
  mmLog.Lines.Add('≤‚ ‘ FDAC.TFDMemTable...');
  FFdacDS.BeginBatch();
  DoTest(FFdacDS, TF);
  FFdacDS.EndBatch;
  mmLog.Lines.Add('≤‚ ‘ FDAC.TKBMMemTable...');
  DoTest(FKbmDS, TK);
  mmLog.Lines.Add('≤‚ ‘ DataSnap.TClientDataSet...');
  DoTest(FCds, TC);
  mmLog.Lines.Add('≤‚ ‘ EhLib.TMemTableEh...');
  DoTest(FEhLibDS, TE);
  ATotalTime := TQ[10] - TQ[0];
  mmLog.Lines.Add('QDAC vs SQLMemTable:' + FormatFloat('0.00',
    (TS[10] - TS[0]) / ATotalTime) + ' X');
  mmLog.Lines.Add('QDAC vs UniDAC:' + FormatFloat('0.00',
    (TU[10] - TU[0]) / ATotalTime) + ' X');
  mmLog.Lines.Add('QDAC vs ADO:' + FormatFloat('0.00',
    (TA[10] - TA[0]) / ATotalTime) + ' X');
  mmLog.Lines.Add('QDAC vs FDAC:' + FormatFloat('0.00',
    (TF[10] - TF[0]) / ATotalTime) + ' X');
  mmLog.Lines.Add('QDAC vs KBM:' + FormatFloat('0.00',
    (TK[10] - TK[0]) / ATotalTime) + ' X');
  mmLog.Lines.Add('QDAC vs DataSnap:' + FormatFloat('0.00',
    (TC[10] - TC[0]) / ATotalTime) + ' X');
  mmLog.Lines.Add('QDAC vs EhLib:' + FormatFloat('0.00',
    (TE[10] - TE[0]) / ATotalTime) + ' X');
end;

procedure TForm1.Test_Locate;
var
  AValue: array [0 .. 99] of String;
  I: Integer;
  TQ, TS, TU, TA, TF, TK, TC, TE: Int64;
  procedure DoLocate(var TS: Int64; ADS: TDataSet);
  var
    J: Integer;
  begin
    TS := GetTimeStamp;
    try
      for J := 0 to 99 do
      begin
        if not ADS.Locate('Id', AValue[J], []) then
          mmLog.Lines.Add(AValue[J] + ' ∂®Œª ß∞‹');
      end;
    except
      on E: Exception do
        mmLog.Lines.Add(' ' + E.Message)
    end;
    TS := GetTimeStamp - TS;
  end;

begin
  mmLog.Lines.Add('°æ∂®Œª≤‚ ‘°ø');
  for I := 0 to 99 do
    AValue[I] := 'CC_' + IntToStr(Random(100000));
  mmLog.Lines.Add('QDAC.Locate');
  DoLocate(TQ, FQdacDS);
  mmLog.Lines.Add('  ' + FormatFloat('0.#', TQ / 10) + 'ms');
  mmLog.Lines.Add('SQLMemTable.Locate');
  DoLocate(TS, FQdacDS);
  mmLog.Lines.Add('  ' + FormatFloat('0.#', TS / 10) + 'ms');
  mmLog.Lines.Add('UniDAC.Locate');
  DoLocate(TU, FUnidacDS);
  mmLog.Lines.Add('  ' + FormatFloat('0.#', TU / 10) + 'ms');
  mmLog.Lines.Add('ADO.Locate');
  DoLocate(TA, FAdoDS);
  mmLog.Lines.Add('  ' + FormatFloat('0.#', TA / 10) + 'ms');
  mmLog.Lines.Add('FireDAC.Locate');
  DoLocate(TF, FFdacDS);
  mmLog.Lines.Add('  ' + FormatFloat('0.#', TF / 10) + 'ms');
  mmLog.Lines.Add('KBM.Locate');
  DoLocate(TK, FKbmDS);
  mmLog.Lines.Add('  ' + FormatFloat('0.#', TK / 10) + 'ms');
  mmLog.Lines.Add('DataSnap.Locate');
  DoLocate(TC, FCds);
  mmLog.Lines.Add('  ' + FormatFloat('0.#', TC / 10) + 'ms');
  mmLog.Lines.Add('EhLib.Locate');
  DoLocate(TE, FEhLibDS);
  mmLog.Lines.Add('  ' + FormatFloat('0.#', TE / 10) + 'ms');
  mmLog.Lines.Add('QDAC vs SQLMemTable:' + FormatFloat('0.00', TS / TQ) + ' X');
  mmLog.Lines.Add('QDAC vs ADO:' + FormatFloat('0.00', TA / TQ) + ' X');
  mmLog.Lines.Add('QDAC vs FDAC:' + FormatFloat('0.00', TF / TQ) + ' X');
  mmLog.Lines.Add('QDAC vs KBM:' + FormatFloat('0.00', TK / TQ) + ' X');
  mmLog.Lines.Add('QDAC vs DataSnap:' + FormatFloat('0.00', TC / TQ) + ' X');
  mmLog.Lines.Add('QDAC vs EhLib:' + FormatFloat('0.00', TE / TQ) + ' X');
end;

procedure TForm1.Test_Sort;
var
  TQ, TS, TU, TA, TF, TK, TC, TE: Int64;
begin
  mmLog.Lines.Add('°æ≈≈–Ú≤‚ ‘°ø');
  mmLog.Lines.Add('QDAC.Sort');
  TQ := GetTimeStamp;
  FQdacDS.Sort := 'Age';
  TQ := GetTimeStamp - TQ;
  mmLog.Lines.Add('  ' + FormatFloat('0.#', TQ / 10) + 'ms');
  mmLog.Lines.Add('SQLMemTable.Sort');
  TS := GetTimeStamp;
  // FSQLMemDS.IndexDefs.Add('by_age','Age',[]);
  // FSQLMemDS.IndexName:='Age';
  TS := GetTimeStamp - TS;
  mmLog.Lines.Add('  ' + FormatFloat('0.#', TS / 10) + 'ms');
  mmLog.Lines.Add('UniDAC.IndexFieldDefs');
  TU := GetTimeStamp;
  FUnidacDS.IndexFieldNames := 'Age';
  TU := GetTimeStamp - TU;
  mmLog.Lines.Add('  ' + FormatFloat('0.#', TU / 10) + 'ms');
  mmLog.Lines.Add('ADO.Sort');
  TA := GetTimeStamp;
  FAdoDS.Sort := 'Age';
  TA := GetTimeStamp - TA;
  mmLog.Lines.Add('  ' + FormatFloat('0.#', TA / 10) + 'ms');
  mmLog.Lines.Add('FireDAC.IndexName');
  TF := GetTimeStamp;
  with FFdacDS.Indexes.Add do
  begin
    Name := 'by_age';
    Fields := 'Age';
    Active := True;
  end;
  FFdacDS.IndexName := 'by_age';
  TF := GetTimeStamp - TF;
  mmLog.Lines.Add('  ' + FormatFloat('0.#', TF / 10) + 'ms');
  mmLog.Lines.Add('KBM.SortFields');
  TK := GetTimeStamp;
  FKbmDS.SortFields := 'Age';
  TK := GetTimeStamp - TK;
  mmLog.Lines.Add('  ' + FormatFloat('0.#', TK / 10) + 'ms');
  mmLog.Lines.Add('DataSnap.IndexName');
  TC := GetTimeStamp;
  FCds.IndexDefs.Add('by_age', 'Age', []);
  FCds.IndexName := 'by_age';
  TC := GetTimeStamp - TC;
  mmLog.Lines.Add('  ' + FormatFloat('0.#', TC / 10) + 'ms');
  mmLog.Lines.Add('EhLib.SortOrder');
  TE := GetTimeStamp;
  FEhLibDS.SortOrder := 'Age';
  TE := GetTimeStamp - TE;
  mmLog.Lines.Add('  ' + FormatFloat('0.#', TE / 10) + 'ms');
  mmLog.Lines.Add('QDAC vs ADO:' + FormatFloat('0.00', TA / TQ) + ' X');
  mmLog.Lines.Add('QDAC vs FDAC:' + FormatFloat('0.00', TF / TQ) + ' X');
  mmLog.Lines.Add('QDAC vs KBM:' + FormatFloat('0.00', TK / TQ) + ' X');
  mmLog.Lines.Add('QDAC vs DataSnap:' + FormatFloat('0.00', TC / TQ) + ' X');
  mmLog.Lines.Add('QDAC vs EhLib:' + FormatFloat('0.00', TE / TQ) + ' X');

end;

procedure TForm1.Test_Stream;
var
  AStream: TMemoryStream;
  TQ, TS, TU, TA, TF, TK, TC, TE: Int64;
  SQ, SS, SU, SA, SF, SK, SC, SE: Int64;
  ATotalTime: Int64;
  procedure QDACSaveCompressed;
  var
    AConverter: TQBinaryConverter;
    SQC, TQC: Int64;

  begin
    AStream.Size := 0;
    TQC := GetTimeStamp;
    AConverter := TQBinaryConverter.Create(nil);
    try
      AConverter.ExportRanges := merAll;
      AConverter.StreamProcessors.Add.Processor :=
        TQZlibStreamProcssor.Create(nil);
      FQdacDS.SaveToStream(AStream, AConverter);
      TQC := GetTimeStamp - TQC;
      SQC := AStream.Size;
      mmLog.Lines.Add('QDAC.Compressed ' + FormatFloat('0.#', TQC / 10) +
        'ms , ' + RollupSize(SQC));
    finally
      FreeObject(AConverter.StreamProcessors[0].Processor);
      FreeObject(AConverter);
    end;
  end;

begin
  mmLog.Lines.Add('°æ±£¥ÊµΩ¡˜ÀŸ∂»≤‚ ‘°ø');
  AStream := TMemoryStream.Create;
  AStream.Size := 0;
  TQ := GetTimeStamp;
  FQdacDS.SaveToStream(AStream, TQBinaryConverter, merAll);
  TQ := GetTimeStamp - TQ;
  SQ := AStream.Size;
  mmLog.Lines.Add('QDAC ' + FormatFloat('0.#', TQ / 10) + 'ms , ' +
    RollupSize(SQ));
  QDACSaveCompressed;
  AStream.Size := 0;
  TS := GetTimeStamp;
  FSQLMemDS.Close;
  FSQLMemDS.SaveAllTablesToStream(AStream, TCompressionAlgorithm.caZLIB);
  TS := GetTimeStamp - TS;
  SS := AStream.Size;
  mmLog.Lines.Add('SQLMemTable ' + FormatFloat('0.#', TS / 10) + 'ms , ' +
    RollupSize(SS));
  AStream.Size := 0;
  TU := GetTimeStamp;
  try
    FUnidacDS.SaveToXML(AStream);
  except
    on E: Exception do
      mmLog.Lines.Add('UniDAC:' + E.Message);
  end;
  TU := GetTimeStamp - TU;
  SU := AStream.Size;
  mmLog.Lines.Add('UniDAC ' + FormatFloat('0.#', TU / 10) + 'ms , ' +
    RollupSize(SU));
  AStream.Size := 0;
  TA := GetTimeStamp;
  FAdoDS.Recordset.Save(TStreamAdapter.Create(AStream) as IUnknown,
    adPersistADTG);
  TA := GetTimeStamp - TA;
  SA := AStream.Size;
  mmLog.Lines.Add('ADO ' + FormatFloat('0.#', TA / 10) + 'ms , ' +
    RollupSize(SA));
  AStream.Size := 0;
  TF := GetTimeStamp;
  try
    FFdacDS.SaveToStream(AStream);
  except
    on E: Exception do
      mmLog.Lines.Add('FireDAC:' + E.Message);
  end;
  TF := GetTimeStamp - TF;
  SF := AStream.Size;
  mmLog.Lines.Add('FireDAC ' + FormatFloat('0.#', TF / 10) + 'ms , ' +
    RollupSize(SF));
  AStream.Size := 0;
  if FKbmDS.DefaultFormat = nil then
    FKbmDS.DefaultFormat := TkbmBinaryStreamFormat.Create(FKbmDS);
  TK := GetTimeStamp;
  FKbmDS.SaveToStream(AStream);
  TK := GetTimeStamp - TK;
  SK := AStream.Size;
  mmLog.Lines.Add('KBM ' + FormatFloat('0.#', TK / 10) + 'ms , ' +
    RollupSize(SK));
  AStream.Size := 0;
  TC := GetTimeStamp;
  FCds.SaveToStream(AStream);
  TC := GetTimeStamp - TC;
  SC := AStream.Size;
  mmLog.Lines.Add('ClientDataSet ' + FormatFloat('0.#', TC / 10) + 'ms , ' +
    RollupSize(SC));
  AStream.Size := 0;
  TE := GetTimeStamp;
  try
    FEhLibDS.SaveToFile(ExtractFilePath(Application.ExeName) + 'ehlib_save.tmp',
      dfmBinaryEh);
  except
    on E: Exception do
      mmLog.Lines.Add('EhLib:' + E.Message);
  end;
  TE := GetTimeStamp - TE;
  AStream.LoadFromFile(ExtractFilePath(Application.ExeName) + 'ehlib_save.tmp');
  DeleteFile(ExtractFilePath(Application.ExeName) + 'ehlib_save.tmp');
  SE := AStream.Size;
  mmLog.Lines.Add('EhLib ' + FormatFloat('0.#', TE / 10) + 'ms , ' +
    RollupSize(SE));
  mmLog.Lines.Add('QDAC vs ADO:' + FormatFloat('0.00', TA / TQ) + ' X');
  mmLog.Lines.Add('QDAC vs FDAC:' + FormatFloat('0.00', TF / TQ) + ' X');
  mmLog.Lines.Add('QDAC vs KBM:' + FormatFloat('0.00', TK / TQ) + ' X');
  mmLog.Lines.Add('QDAC vs DataSnap:' + FormatFloat('0.00', TC / TQ) + ' X');
  mmLog.Lines.Add('QDAC vs EhLib:' + FormatFloat('0.00', TE / TQ) + ' X');
  AStream.Free;
end;

end.
