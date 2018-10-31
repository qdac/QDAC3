unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Spin, qlog;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    ListBox1: TListBox;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpinEdit2Change(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
  private
    { Private declarations }
    FCount: Integer;
    FMemoWriter, FListWriter: TQLogStringsWriter;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  while FCount < 10000 do
  begin
    PostLog(llHint, 'Test log %d', [FCount]);
    Inc(FCount);
    Application.ProcessMessages;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FMemoWriter := TQLogStringsWriter.Create;
  FMemoWriter.Items := Memo1.Lines;
  FMemoWriter.MaxItems := SpinEdit1.Value;
  FMemoWriter.LazyMode:=True;
  Logs.Castor.AddWriter(FMemoWriter);
  FListWriter := TQLogStringsWriter.Create;
  FListWriter.Items := ListBox1.Items;
  FListWriter.MaxItems := SpinEdit2.Value;
  Logs.Castor.AddWriter(FListWriter);
end;

procedure TForm1.SpinEdit1Change(Sender: TObject);
begin
  FMemoWriter.MaxItems := SpinEdit1.Value;
end;

procedure TForm1.SpinEdit2Change(Sender: TObject);
begin

  FListWriter.MaxItems := SpinEdit2.Value;
end;

end.
