unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses QString;
{$R *.dfm}
var
  AIgnoreSpace,AIgnoreCase:Boolean;
function NCmp(List: TStringList; Index1, Index2: Integer): Integer;
begin
{$IF RTLVersion<19}
Result := NaturalCompareW(PQCharW(QStringW(List[Index1])), PQCharW(QStringW(List[Index2])), AIgnoreCase,AIgnoreSpace);
{$ELSE}
Result := NaturalCompareW(PQCharW(List[Index1]), PQCharW(List[Index2]), AIgnoreCase,AIgnoreSpace);
{$IFEND}
//OutputDebugString(PChar(List[Index1]+' vs '+List[Index2]+' = '+IntToStr(Result)));
end;



procedure TForm1.Button1Click(Sender: TObject);
var
  AList: TStringList;
begin
AList := TStringList.Create;
AList.AddStrings(Memo1.Lines);
AIgnoreCase:=CheckBox1.Checked;
AIgnoreSpace:=CheckBox2.Checked;
AList.CustomSort(NCmp);
Memo2.Lines.Assign(AList);
FreeObject(AList);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  I:Integer;
begin
Memo1.Lines.BeginUpdate;
Memo1.Lines.Clear;
for I := 0 to 999 do
  begin
  Memo1.Lines.Add('A_'+IntToStr(random(1000)));
  end;
Memo1.Lines.EndUpdate;
end;

end.
