unit hostmain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function IntToBinStr(v: Integer): String;
var
  b: array [0 .. 32] of Char;
  o: Integer;
const
  Chars: array [0 .. 1] of Char = ('0', '1');
begin
  o := 31;
  if v <> 0 then
  begin
    while v <> 0 do
    begin
      b[o] := Chars[v and $1];
      v := v shr 1;
      Dec(o);
    end;
    b[32] := #0;
    Result := PChar(@b[o + 1]);
  end
  else
    Result := '0';
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowMessage(IntToBinStr(100));
end;

end.
