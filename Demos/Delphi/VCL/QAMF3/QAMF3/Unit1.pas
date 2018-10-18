unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, QAMF3, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  QAMF3Helpher: TQAMF3Helpher;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  buff: array of Byte;
  bufflen, I: Integer;
  hex: string;
  XXX: variant;
begin
  hex := Trim(Memo1.Lines.Text);
  bufflen := Length(hex) div 2;
  SetLength(buff, bufflen);
  for I := 0 to bufflen - 1 do
  begin
    buff[I] := StrToInt('$' + hex[1 + I * 2] + hex[2 + I * 2]);
  end;
  QAMF3Helpher.SetData(@buff[0], bufflen);
  XXX := QAMF3Helpher.ReadAMF3Variant;

  Memo2.Text := XXX;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  I: Integer;
  tmp: string;
  tmpAdd: DWORD;
begin
  QAMF3Helpher.BeforeWrite;
  QAMF3Helpher.WriteAMF3Variant('ÄãºÃ£¬QAMF3');
  tmp := '';
  tmpAdd := DWORD(QAMF3Helpher.Stream.Memory);
  for I := 0 to QAMF3Helpher.Stream.Size do
  begin
    tmp := tmp + inttohex(pbyte(tmpAdd + I)^, 2);
  end;
  Memo1.Lines.Text := tmp;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  I: Integer;
  tmp: string;
  tmpAdd: DWORD;
begin
  QAMF3Helpher.BeforeWrite;
  QAMF3Helpher.WriteAMF3Variant(22.0);
  tmp := '';
  tmpAdd := DWORD(QAMF3Helpher.Stream.Memory);
  for I := 0 to QAMF3Helpher.Stream.Size do
  begin
    tmp := tmp + inttohex(pbyte(tmpAdd + I)^, 2);
  end;
  Memo1.Lines.Text := tmp;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  I: Integer;
  tmp: string;
  tmpAdd: DWORD;
begin
  QAMF3Helpher.BeforeWrite;
  QAMF3Helpher.WriteAMF3Variant(Now());
  tmp := '';
  tmpAdd := DWORD(QAMF3Helpher.Stream.Memory);
  for I := 0 to QAMF3Helpher.Stream.Size do
  begin
    tmp := tmp + inttohex(pbyte(tmpAdd + I)^, 2);
  end;
  Memo1.Lines.Text := tmp;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  ioResponse: IAS3Object;
  ioResponseHeader: IAS3Object;
  I: Integer;
  tmp: string;
  tmpAdd: DWORD;
begin
  QAMF3Helpher.BeforeWrite;
  ioResponse := TAS3Object.Create;
  ioResponseHeader := TAS3Object.Create;
  ioResponse.SetAS3ClassName('TESTResponse');
  ioResponse.SetIsDynamic(false);
  ioResponse.SetIsExternalized(false);

  ioResponseHeader.SetAS3ClassName('TESTResponseHeader');
  ioResponseHeader.SetIsDynamic(false);
  ioResponseHeader.SetIsExternalized(false);
  ioResponseHeader.SetValue('StatusCode', 0);
  ioResponseHeader.SetValue('StatusText', '²âÊÔ×´Ì¬');
  ioResponseHeader.SetValue('InternalStatusCode', 255);
  ioResponse.SetValue('header', ioResponseHeader);
  QAMF3Helpher.WriteAMF3Type(amf3dtObject);
  QAMF3Helpher.WriteAMF3Object(ioResponse);

  tmp := '';
  tmpAdd := DWORD(QAMF3Helpher.Stream.Memory);
  for I := 0 to QAMF3Helpher.Stream.Size do
  begin
    tmp := tmp + inttohex(pbyte(tmpAdd + I)^, 2);
  end;
  Memo1.Lines.Text := tmp;
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  S: AnsiString;
begin
  //
  S := #$0a#$00#$00 +
    #$00#$01#$11#$0a#$0b#$0d#$6d#$79#$54#$79#$70#$65#$11#$61#$72#$72 +
    #$61#$79#$56#$61#$6c#$09#$07#$01#$04#$01#$04#$02#$06#$07#$65#$72 +
    #$74#$13#$73#$74#$72#$69#$6e#$67#$56#$61#$6c#$06#$07#$62#$6c#$61 +
    #$0d#$69#$6e#$74#$56#$61#$6c#$04#$02#$01;
  QAMF3Helpher.SetData(PAnsiChar(S),Length(S));
  QAMF3Helpher.ReadAMF0Type;
  QAMF3Helpher.ReadAMF0Array;
  //
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  QAMF3Helpher.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  QAMF3Helpher := TQAMF3Helpher.Create;
end;

end.
