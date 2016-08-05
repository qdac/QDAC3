unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, System.Types, System.Hash,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
    procedure DoHashProgress(ADone, ATotal: Int64);
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses qdigest, IdGlobal, IdHashMessageDigest, qstring;
{$R *.dfm}

type
  PIdBytes = ^TIdBytes;

procedure TForm2.Button1Click(Sender: TObject);
var
  ADigest: TQMD5Digest;
  AMD5: THashMD5;
  AIndy: TIdHashMessageDigest5;
  ABytes: TIdBytes;
  procedure MD5Test(S: String);
  begin
    Memo1.Lines.Add('System:' + QuotedStr(S) + '=>' + AMD5.GetHashString(S));
    ADigest := MD5Hash(S);
    Memo1.Lines.Add('QDigest:' + QuotedStr(S) + '=>' + DigestToString(ADigest));
    ABytes := AIndy.HashString(S);
    Memo1.Lines.Add('Indy:' + QuotedStr(S) + '=>' +
      qstring.BinToHex(@ABytes[0], 16))
  end;
  procedure MD5TestFile(AFileName: String);
  var
    AStream: TBytesStream;
    ATemp: TBytes;
  begin
    AStream := TBytesStream.Create();
    AStream.LoadFromFile(AFileName);
    SetLength(ATemp, AStream.Size);
    Move(AStream.Bytes[0], ATemp[0], AStream.Size);
    SetLength(ATemp, AStream.Size);
    AMD5.Reset;
    AMD5.Update(ATemp);
    Memo1.Lines.Add('System:' + QuotedStr(AFileName) + '=>' +
      AMD5.HashAsString);
    AStream.Position := 0;
    ADigest := MD5Hash(AStream, nil);
    Memo1.Lines.Add('QDigest(nil):' + QuotedStr(AFileName) + '=>' +
      DigestToString(ADigest));

    ADigest := MD5Hash(AStream, DoHashProgress);
    Memo1.Lines.Add('QDigest:' + QuotedStr(AFileName) + '=>' +
      DigestToString(ADigest));
    AStream.Position := 0;
    Memo1.Lines.Add('Indy:' + QuotedStr(AFileName) + '=>' +
      AIndy.HashStreamAsHex(AStream));
    FreeAndNil(AStream);
  end;

begin
  AIndy := TIdHashMessageDigest5.Create;
  // MD5Test('');
  // MD5Test('a');
  // MD5Test('abc');
  // MD5Test('message digest');
  // MD5Test('abcdefghijklmnopqrstuvwxyz');
  MD5Test('id=333333&vm=0&DC=ATI Radeon 3000 Graphics&report=HDHVAQ');
  MD5Test('12345678901234567890123456789012345678901234567890123456789012345678901234567890');
  AIndy.Free;
  MD5TestFile(Application.ExeName);
  // MD5 ("a") = 0cc175b9c0f1b6a831c399e269772661
  // MD5 ("abc") = 900150983cd24fb0d6963f7d28e17f72
  // MD5 ("") = f96b697d7cb7938d525a2f31aaf161d0
  // MD5 ("") = c3fcd3d76192e4007dfb496cca67e13b
  // MD5 ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789") =
  // d174ab98d277d9f5a5611c2c9f419d9f
  // MD5 ("") = 57edf4a22be3c955ac49da2e2107b67a
end;

procedure TForm2.Button2Click(Sender: TObject);
var
  ADigest: TQSHADigest;
  procedure DoSHA(S: String);
  begin
    Memo1.Lines.Add(S);
    Memo1.Lines.Add('========');
    ADigest := SHA160Hash(S);
    Memo1.Lines.Add('SHA1=' + DigestToString(ADigest));
    ADigest := SHA256Hash(S);
    Memo1.Lines.Add('SHA256=' + DigestToString(ADigest));
    ADigest := SHA384Hash(S);
    Memo1.Lines.Add('SHA384=' + DigestToString(ADigest));
    ADigest := SHA512Hash(S);
    Memo1.Lines.Add('SHA512=' + DigestToString(ADigest));
  end;

begin
  DoSHA('');
  DoSHA('a');
  DoSHA('abc');
  DoSHA('message digest');
  DoSHA('abcdefghijklmnopqrstuvwxyz');
  DoSHA('12345678901234567890123456789012345678901234567890123456789012345678901234567890');
  Memo1.Lines.Add('SHAFile(256):' + DigestToString
    (SHA256File(Application.ExeName, DoHashProgress)));
  Memo1.Lines.Add('SHAFile(384):' + DigestToString
    (SHA384File(Application.ExeName, DoHashProgress)));
  Memo1.Lines.Add('SHAFile(512):' + DigestToString
    (SHA512File(Application.ExeName, DoHashProgress)));
end;

procedure TForm2.Button3Click(Sender: TObject);
var
  I: Integer;
  AMD5: TQMD5Digest;
begin
  Memo1.Lines.Add('MD5Hash")=' + DigestToString
    (MD5File('H:\BaiduYunDownload\Win7_X64_Nin1_20140316_ForMe.ISO',
    procedure(ADone, ATotal: Int64)
    begin
      Caption := FormatFloat('0.##', ADone * 100 / ATotal);
    end)));
end;

procedure TForm2.Button4Click(Sender: TObject);
var
  T: Cardinal;
begin
  if OpenDialog1.Execute then
  begin
    T := GetTickCount;
    Memo1.Lines.Add(DigestToString(SHA256File(OpenDialog1.FileName,
      DoHashProgress)));
    T := GetTickCount - T;
    Memo1.Lines.Add('SHA256 操作用时：' + IntToStr(T) + 'ms');

    T := GetTickCount;
    Memo1.Lines.Add(DigestToString(SHA512File(OpenDialog1.FileName,
      DoHashProgress)));
    T := GetTickCount - T;
    Memo1.Lines.Add('SHA512 操作用时：' + IntToStr(T) + 'ms');
  end;
end;

procedure TForm2.DoHashProgress(ADone, ATotal: Int64);
begin
  Caption := FormatFloat('0.##', ADone * 100 / ATotal);
end;

end.
