unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, System.Net.HttpClient,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, qstring,
  qamf3;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    mmSourceBytes: TMemo;
    Panel3: TPanel;
    mmResult: TMemo;
    Panel4: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Label1: TLabel;
    Panel5: TPanel;
    Label2: TLabel;
    Button4: TButton;
    chkRTMPC3: TCheckBox;
    Label3: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
    function HexView(ABytes: TBytes): String;
    function ErrorOffset(AOffset: Integer): String;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure DoParseRiotGamesBroadcastNotification(ASender: TQAMFNode;
  AClassName: QStringW; var p: PByte; const pe: PByte);
var
  ALen: Integer;
begin
  ALen := ExchangeByteOrder(PInteger(p)^);
  Inc(p, 4);
  if IntPtr(p) + ALen < IntPtr(pe) then
  begin
    ASender.AsString := qstring.Utf8Decode(PQCharA(p), ALen);
    Inc(p, ALen);
  end
  else
    Dec(p, 4);
end;

procedure DoParseRiotGamesClientSystemStatesNotification(ASender: TQAMFNode;
  AClassName: QStringW; var p: PByte; const pe: PByte);
var
  ps: PByte;
  ACount: Integer;
begin
  ACount := ExchangeByteOrder(PInteger(p)^);
  Inc(p, 4);
  ASender.AsString := qstring.Utf8Decode(PQCharA(p), ACount);
  Inc(p, ACount);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  ABytes: TBytes;
  AMF: TQAMF;
begin
  AMF := TQAMF.Create;
  try
    ABytes := HexToBin(DeleteCharW(mmSourceBytes.Text, ' '#9#10#13));
    if chkRTMPC3.Checked then
      ABytes := DecodeRTMPC3Block(@ABytes[0], Length(ABytes));
    mmResult.Lines.Add(HexView(ABytes));
    if AMF.TryParseAMF3Data(@ABytes[0], Length(ABytes)) then
    begin
      Label3.Caption := '解析成功';
      mmResult.Lines.Add(AMF.AsString);
    end
    else
    begin
      Label3.Caption := '解析失败，出错于第 ' + IntToStr(AMF.ErrorOffset) + '字节(' +
        ErrorOffset(AMF.ErrorOffset) + '):' + AMF.LastErrorMsg;
      mmResult.Lines.Add('解析数据失败，出错位置第 ' + ErrorOffset(AMF.ErrorOffset) + ': ' +
        AMF.LastErrorMsg);
      mmResult.Lines.Add('已解析的部分内容：');
      mmResult.Lines.Add(AMF.AsString);
    end;
  finally
    FreeAndNil(AMF);
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  ABytes: TBytes;
  AMF: TQAMF;
begin
  AMF := TQAMF.Create;
  try
    ABytes := HexToBin(DeleteCharW(mmSourceBytes.Text, ' '#9#10#13));
    if chkRTMPC3.Checked then
      ABytes := DecodeRTMPC3Block(@ABytes[0], Length(ABytes));
    mmResult.Lines.Add(HexView(ABytes));
    if AMF.TryParseAMF0Data(@ABytes[0], Length(ABytes)) then
    begin
      Label3.Caption := '解析成功';
      mmResult.Lines.Add(AMF.AsString);
    end
    else
    begin
      Label3.Caption := '解析失败，出错于第 ' + IntToStr(AMF.ErrorOffset) + '字节(' +
        ErrorOffset(AMF.ErrorOffset) + '):' + AMF.LastErrorMsg;
      mmResult.Lines.Add('解析数据失败，出错位置第 ' + ErrorOffset(AMF.ErrorOffset) + ': ' +
        AMF.LastErrorMsg);
      mmResult.Lines.Add('已解析的部分内容：');
      mmResult.Lines.Add(AMF.AsString);
    end;
  finally
    FreeAndNil(AMF);
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  ABytes: TBytes;
  AMF: TQAMF;
begin
  AMF := TQAMF.Create;
  try
    ABytes := HexToBin(DeleteCharW(mmSourceBytes.Text, ' '#9#10#13));
    if chkRTMPC3.Checked then
      ABytes := DecodeRTMPC3Block(@ABytes[0], Length(ABytes));
    mmResult.Lines.Add(HexView(ABytes));
    if AMF.TryParse(@ABytes[0], Length(ABytes)) then
    begin
      Label3.Caption := '解析成功';
      mmResult.Lines.Add(AMF.AsString);
    end
    else
    begin
      Label3.Caption := '解析失败，出错于第 ' + IntToStr(AMF.ErrorOffset) + '字节(' +
        ErrorOffset(AMF.ErrorOffset) + '):' + AMF.LastErrorMsg;
      mmResult.Lines.Add('解析数据失败，出错位置第 ' + ErrorOffset(AMF.ErrorOffset) + ': ' +
        AMF.LastErrorMsg);
      mmResult.Lines.Add('已解析的部分内容：');
      mmResult.Lines.Add(AMF.AsString);
    end;
  finally
    FreeAndNil(AMF);
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  ABytes: TBytes;
  AMF: TQAMF;
  ANode: TQAMFNode;
begin
  AMF := TQAMF.Create;
  try
    ABytes := HexToBin
      ('0A070744534BA202061B676E2D3430313738383433363205427537EF58A090000C21D8F575FFF91B00E80AED8C9B276F85EC020C214C0C3455D523B48411A87EF7BA756F7E00');
    mmResult.Lines.Add(HexView(ABytes));
    AMF.TryParseAMF3Data(@ABytes[0], Length(ABytes));
    mmResult.Lines.Add(AMF.AsString);
    if AMF.HasChild('messages[0]\messageId', ANode) then
    begin
      ShowMessage('HasChild ' + ANode.Name + ':' + SLineBreak + ANode.AsString);
    end;
    ShowMessage('ValueByPath(''messages[0]\messageId'',''''):' + SLineBreak +
      AMF.ValueByPath('messages[0]\messageId', ''));
  finally
    FreeAndNil(AMF);
  end;
end;

function TForm1.ErrorOffset(AOffset: Integer): String;
begin
  Result := ' ' + IntToHex(AOffset div 16, 4) + ' 行 ' + IntToHex(AOffset mod 16,
    2) + ' 列';
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  RegisterAMFExternalClass
    ('com.riotgames.platform.broadcast.BroadcastNotification',
    DoParseRiotGamesBroadcastNotification);
  RegisterAMFExternalClass
    ('com.riotgames.platform.systemstate.ClientSystemStatesNotification',
    DoParseRiotGamesClientSystemStatesNotification);
end;

function TForm1.HexView(ABytes: TBytes): String;
var
  C, R, L, ARows, ACols: Integer;
  T: String;
  B: Byte;
begin
  L := Length(ABytes);
  ARows := (L shr 4);
  if (L and $F) <> 0 then
    Inc(ARows);
  Result := '     ';
  for C := 0 to 15 do
    Result := Result + IntToHex(C, 2) + ' ';
  Result := Result + SLineBreak;
  for R := 0 to ARows - 1 do
  begin
    Result := Result + IntToHex(R, 4) + ' ';
    ACols := L - (R shl 4);
    if ACols > 16 then
      ACols := 16;
    SetLength(T, 0);
    for C := 0 to ACols - 1 do
    begin
      B := ABytes[R * 16 + C];
      Result := Result + IntToHex(B, 2) + ' ';
      if (B >= $20) and (B <= $7E) then
        T := T + AnsiChar(B)
      else
        T := T + '.';
    end;
    for C := ACols to 15 do
      Result := Result + '   ';
    Result := Result + T + SLineBreak;
  end;
end;

end.
