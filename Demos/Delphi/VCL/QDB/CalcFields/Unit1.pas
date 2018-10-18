unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.Grids, Vcl.DBGrids, QDB;

type
  TForm1 = class(TForm)
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FDataSet: TQDataSet;
    procedure DoCalcFields(ADataSet:TDataSet);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses dateutils,adodb;
{$R *.dfm}

procedure TForm1.DoCalcFields(ADataSet: TDataSet);
begin
ADataSet.FieldByName('Age').AsInteger:=YearsBetween(Now,ADataSet.FieldByName('Birthday').AsDateTime);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  AField: TField;
  I: Integer;
begin
  FDataSet := TQDataSet.Create(Self);
  DataSource1.DataSet := FDataSet;
  FDataSet.FieldDefs.Add('Id', ftInteger);
  FDataSet.FieldDefs.Add('Name', ftWideString, 20);
  FDataSet.FieldDefs.Add('Birthday', ftDateTime);
  for I := 0 to FDataSet.FieldDefs.Count-1 do
    FDataSet.FieldDefs[I].CreateField(FDataSet);
  AField := TIntegerField.Create(FDataSet);
  AField.FieldName:='Age';
  AField.FieldKind := fkCalculated;
  AField.DataSet := FDataSet;
  FDataSet.OnCalcFields:=DoCalcFields;
  FDataSet.CreateDataSet;
  FDataSet.DisableControls;
  try
    for I := 0 to 9 do
    begin
      FDataSet.Append;
      FDataSet.FieldByName('Id').AsInteger := I;
      FDataSet.FieldByName('Name').AsString := 'User_' + IntToStr(I);
      FDataSet.FieldByName('Birthday').AsDateTime := IncDay(Now, -random(3000));
      FDataSet.Post;
    end;
  finally
    FDataSet.EnableControls;
  end;
end;

end.
