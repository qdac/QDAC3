unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Data.DB, Vcl.Grids,
  Vcl.DBGrids, Vcl.ExtCtrls,QDB;

type
  TForm1 = class(TForm)
    Panel2: TPanel;
    Splitter1: TSplitter;
    Panel3: TPanel;
    Panel4: TPanel;
    DBGrid1: TDBGrid;
    Panel5: TPanel;
    Panel6: TPanel;
    DBGrid2: TDBGrid;
    Panel1: TPanel;
    Button1: TButton;
    DataSource2: TDataSource;
    DataSource1: TDataSource;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FSource, FDest: TQDataSet;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
FDest.CopyFrom(FSource,dcmCurrents,'');
FDest.Cross('Year','Animal','Total','Animal');
end;
{
Year Name Total
2000 Mouse 100.0
2001 Mouse 200.1
2002 Mouse 300.4
2003 Mouse 150.8
2004 Mouse 100.3
2000 Dog   50.2
2001 Dog   1002
==>
*     2000   2001   2002   2003   2004
Mouse 100.0  200.1  300.4  150.8  100.3
Dog   50.2   1002   NULL¡¡¡¡NULL¡¡NULL
}
procedure TForm1.FormCreate(Sender: TObject);
var
  I: Integer;
begin
FSource:=TQDataSet.Create(Self);
FSource.FieldDefs.Add('Year',ftInteger);
FSource.FieldDefs.Add('Animal',ftWideString,20);
FSource.FieldDefs.Add('Total',ftFloat);
FSource.CreateDataSet;
for I := 2000 to 2014 do
  begin
  FSource.Append;
  FSource.Fields[0].AsInteger:=I;
  FSource.Fields[1].AsString:='Mouse';
  FSource.Fields[2].AsFloat:=Random(10000)/10;
  FSource.Post;
  end;
for I := 2000 to 2014 do
  begin
  FSource.Append;
  FSource.Fields[0].AsInteger:=I;
  FSource.Fields[1].AsString:='Dog';
  FSource.Fields[2].AsFloat:=Random(10000)/10;
  FSource.Post;
  end;
for I := 2000 to 2014 do
  begin
  FSource.Append;
  FSource.Fields[0].AsInteger:=I;
  FSource.Fields[1].AsString:='Cat';
  FSource.Fields[2].AsFloat:=Random(10000)/10;
  FSource.Post;
  end;
FDest:=TQDataSet.Create(Self);
DataSource1.DataSet:=FSource;
DataSource2.DataSet:=FDest;
end;

end.
