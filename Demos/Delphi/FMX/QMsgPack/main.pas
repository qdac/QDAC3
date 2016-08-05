unit main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,qstring,qmsgpack,
  FMX.StdCtrls, FMX.Layouts, FMX.Memo;

type
  TForm2 = class(TForm)
    Button1: TButton;
    mmResult: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.Button1Click(Sender: TObject);
var
  APack:TQMsgPack;
  ABytes:TBytes;
  S:String;
  I: Integer;
  AStream:TMemoryStream;
begin
APack:=TQMsgPack.Create;
try
  APack.Add('Age',25);
  APack.Add('Name','Jone smith');
  with APack.Add('Colors',mptArray) do
    begin
    Add.AsString:='Red';
    Add.AsString:='Yellow';
    Add.AsString:='Blue';
    Add.AsBoolean:=False;
    end;
  with APack.Add('Arm') do
    begin
    Add.AsString:='Hande';
    Add('Fat').AsBoolean:=False;
    Add('Length').AsInteger:=32;
    end;
  SetLength(ABytes,10);
  for I := 0 to 9 do
    ABytes[I]:=I+1;
  APack.Add('Bytes',ABytes);
  AStream:=TMemoryStream.Create;
  AStream.WriteBuffer(ABytes[0],10);
  APack.Add('Stream').BytesFromStream(AStream,0);//直接从文件加载可以用BytesFromFile
  FreeObject(AStream);
  mmResult.Lines.Clear;
  S:=APack.AsString;
  mmResult.Lines.Add('Text,Size('+IntToStr(Length(S))+'B)');
  mmResult.Lines.Add(S);
  ABytes:=APack.Encode;
  mmResult.Lines.Add('Binary,Size('+IntToStr(Length(ABytes))+'B)');
  mmResult.Lines.Add(QString.BinToHex(@ABytes[0],Length(ABytes)));
finally
  FreeObject(APack);
end;
end;

end.
