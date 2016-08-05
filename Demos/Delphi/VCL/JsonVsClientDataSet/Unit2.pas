unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids, Vcl.DBGrids,
  Vcl.ExtCtrls, Data.DB, Datasnap.DBClient;

type
  TForm2 = class(TForm)
    ClientDataSet1: TClientDataSet;
    DataSource1: TDataSource;
    Panel1: TPanel;
    DBGrid1: TDBGrid;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    Button4: TButton;
    Button5: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation
uses qstring,qjson;
{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
var
  I: Integer;
begin
ClientDataSet1.Close;
ClientDataSet1.FieldDefs.Clear;
ClientDataSet1.FieldDefs.Add('Id',ftInteger);
ClientDataSet1.FieldDefs.Add('Name',ftString,255);
ClientDataSet1.CreateDataSet;
for I := 0 to 99 do
  begin
  ClientDataSet1.Append;
  ClientDataSet1.FieldByName('Id').AsInteger:=I;
  ClientDataSet1.FieldByName('Name').AsString:='Name_'+IntToStr(I);
  ClientDataSet1.Post;
  end;
end;

procedure TForm2.Button2Click(Sender: TObject);
var
  AJson:TQJson;
begin
AJson:=TQJson.Create;
try
  AJson.AsVariant:=ClientDataSet1.Data;
  if SaveDialog1.Execute then
    AJson.SaveToFile(SaveDialog1.FileName,teUtf8,true);
finally
  AJson.Free;
end;
end;

procedure TForm2.Button3Click(Sender: TObject);
var
  AJson:TQJson;
  function Json2Bytes:TBytes;
  var
    I: Integer;
  begin
  SetLength(Result,AJson.Count);
  for I := 0 to AJson.Count-1 do
    Result[I]:=AJson[I].AsInteger;
  end;
begin
AJson:=TQJson.Create;
try
  if OpenDialog1.Execute then
    begin
    AJson.LoadFromFile(OpenDialog1.FileName);
    ClientDataSet1.Close;
    ClientDataSet1.Data:=Json2Bytes;
    end;
finally
  AJson.Free;
end;
end;

end.
