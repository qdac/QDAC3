unit Unit3;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, qaes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls;

type
  TForm3 = class(TForm)
    Panel1: TPanel;
    Memo1: TMemo;
    Memo2: TMemo;
    Splitter1: TSplitter;
    Button1: TButton;
    Button2: TButton;
    CBC: TButton;
    Button3: TButton;
    rgAlog: TRadioGroup;
    rgKeyLen: TRadioGroup;
    GroupBox1: TGroupBox;
    edtKey: TEdit;
    ProgressBar1: TProgressBar;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CBCClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
    procedure InitEncrypt(var AES: TQAES);
    procedure DoProgress(AProcessed, ATotal: Int64);
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses qstring, encddecd;
{$R *.dfm}

{ AES加密单元中，加密函数全部是叫AESEncrypt，解密的全是AESDecrypt，区别只是参数的不同
  例子只是演示了字符串加密解密，其它的依次类推就好。
}
procedure TForm3.Button1Click(Sender: TObject);
var
  AES: TQAES;
  ABytes: TBytes;
begin
  InitEncrypt(AES);
  AES.Encrypt(Memo1.Lines.Text, ABytes);
  Memo2.Text := qstring.BinToHex(ABytes);
  // 如果不喜欢用结构体的形式，可以直接用AESXXX函数，内部实际也是调用相应的函数实现
end;

procedure TForm3.Button2Click(Sender: TObject);
var
  ABytes: TBytes;
  AES: TQAES;
begin
  qstring.HexToBin(Memo2.Text, ABytes);
  InitEncrypt(AES);
  ShowMessage(AES.Decrypt(ABytes));
  // 如果不喜欢用结构体的形式，可以直接用AESXXX函数，内部实际也是调用相应的函数实现
end;

procedure TForm3.Button3Click(Sender: TObject);
var
  AStream, AEncrypt: TMemoryStream;
  I, V: Integer;
  AES: TQAES;
begin
  AStream := TMemoryStream.Create;
  AEncrypt := TMemoryStream.Create;
  try
    for I := 0 to 100 do
    begin
      AStream.Write(I, SizeOf(I));
    end;
    InitEncrypt(AES);
    AStream.Position := 0;
    AES.Encrypt(AStream, AEncrypt);
    Assert(AStream.Size < AEncrypt.Size);
    AStream.Size := 0;
    AEncrypt.Position := 0;
    AES.Decrypt(AEncrypt, AStream);
    AStream.Position := 0;
    for I := 0 to 100 do
    begin
      AStream.Read(V, SizeOf(V));
      if I <> V then
        DebugBreak;
    end;
  finally
    FreeAndNil(AStream);
    FreeAndNil(AEncrypt);
  end;
end;

procedure TForm3.CBCClick(Sender: TObject);
var
  AES: TQAES;
  ABytes: TBytes;
begin
  InitEncrypt(AES);
  AES.Encrypt(Memo1.Lines.Text, ABytes);
  Memo2.Lines.Add('原始密文:' + qstring.BinToHex(ABytes));
  Memo2.Lines.Add('解密后:' + AES.Decrypt(ABytes));
end;

procedure TForm3.DoProgress(AProcessed, ATotal: Int64);
begin
  if not ProgressBar1.Visible then
    ProgressBar1.Visible := true;
  if AProcessed < ATotal then
    ProgressBar1.Position := AProcessed * 100 div ATotal
  else
    ProgressBar1.Position := 100;
end;

procedure TForm3.InitEncrypt(var AES: TQAES);
var
  AKeyType: TQAESKeyType;
const
  AInitVector: TQAESBuffer = ($21, $20, $B9, $3B, $99, $86, $41, $A5, $87, $80,
    $54, $0B, $C9, $A8, $B4, $E7);
begin
  case rgKeyLen.ItemIndex of
    0:
      AKeyType := kt128;
    1:
      AKeyType := kt192;
    2:
      AKeyType := kt256;
  end;
  if rgAlog.ItemIndex = 0 then
    AES.AsECB(edtKey.Text, AKeyType)
  else
    AES.AsCBC(AInitVector, edtKey.Text, AKeyType);
  AES.SetProgressCallback(DoProgress);
end;

end.
