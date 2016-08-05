unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Spin, Data.DB,
  Vcl.Grids, Vcl.DBGrids, Vcl.ExtCtrls, QDB;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Button1: TButton;
    Button2: TButton;
    Button4: TButton;
    DBGrid1: TDBGrid;
    Panel2: TPanel;
    DataSource1: TDataSource;
    Label3: TLabel;
    SpinEdit1: TSpinEdit;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
    FDataSet: TQDataSet;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button2Click(Sender: TObject);
begin
// 做为演示，只支持四种类型
case ComboBox1.ItemIndex of
  0: // 整数
    FDataSet.FieldDefs.Add(ComboBox2.Text, ftInteger);
  1: // 字符串
    begin
    if SpinEdit1.Value = 0 then
      FDataSet.FieldDefs.Add(ComboBox2.Text, ftString, 10)
    else
      FDataSet.FieldDefs.Add(ComboBox2.Text, ftString, SpinEdit1.Value);
    end;
  2: // 浮点
    FDataSet.FieldDefs.Add(ComboBox2.Text, ftFloat);
  3: // 日期时间
    FDataSet.FieldDefs.Add(ComboBox2.Text, ftDateTime);
end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
FDataSet.FieldDefs.Delete(DBGrid1.SelectedField.FieldNo-1);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  I: Integer;
  ADate:TDateTime;
begin
FDataSet := TQDataSet.Create(Self);
FDataSet.FieldDefs.Add('Id', ftInteger);
FDataSet.CreateDataSet;
for I := 0 to 99 do
  begin
  FDataSet.Append;
  FDataSet.Fields[0].AsInteger:=I+1;
  FDataSet.Post;
  end;
DataSource1.DataSet := FDataSet;
end;

end.
