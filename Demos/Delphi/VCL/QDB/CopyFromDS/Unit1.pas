unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Data.Win.ADODB, QDB,
  Vcl.StdCtrls, Datasnap.Provider, Datasnap.DBClient, Vcl.Grids, Vcl.DBGrids,
  qconverter_sql;

type
  TForm1 = class(TForm)
    ADODataSet1: TADODataSet;
    Button1: TButton;
    ClientDataSet1: TClientDataSet;
    DataSetProvider1: TDataSetProvider;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    FDataSet: TQDataSet;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses qstring;
{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  ATime: Cardinal;
begin
  if not Assigned(FDataSet) then
  begin
    FDataSet := TQDataSet.Create(Self);
    DataSource1.DataSet := FDataSet;
  end;
  ATime := GetTickCount;
  ClientDataSet1.LoadFromFile
    ('D:\User\QDAC3\Demos\Delphi\VCL\QDB\CopyFromDS\222.xml');
  FDataSet.CopyFrom(ClientDataSet1);
  ShowMessage(IntToStr(FDataSet.RecordCount) + ' used ' +
    IntToStr(GetTickCount - ATime) + 'ms');
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  ADataSet: TADODataSet;
  ASQL: TQMSSQLConverter;
begin
  ASQL := TQMSSQLConverter.Create(nil);
  ADataSet := TADODataSet.Create(nil);
  try
    ADataSet.LoadFromFile('c:\temp\ttadopfADTG');
    // ADataSet.FieldDefs.Add('blob', ftBlob);
    // ADataSet.CreateDataSet;
    // ADataSet.Append;
    // ADataSet.Fields[0].AsBytes := HexToBin('0123456789ABCDEF');
    // ADataSet.Post;
    if not Assigned(FDataSet) then
    begin
      FDataSet := TQDataSet.Create(Self);
      DataSource1.DataSet := FDataSet;
    end;
    FDataSet.CopyFrom(ADataSet);
    (FDataSet.FieldDefs[0] as TQFieldDef).Table := 'test';
    ASQL.AllAsInsert := True;
    ASQL.DataSet := FDataSet;
    ShowMessage(ASQL.SQL);

  finally
    FreeAndNil(ADataSet);
    FreeAndNil(ASQL);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin

  // ClientDataSet1.Delete;
  // ClientDataSet1.Edit;
  // ClientDataSet1.FieldByName('LB').AsInteger := 1;
  // ClientDataSet1.Post;
  // ClientDataSet1.SaveToFile
  // ('D:\User\QDAC3\Demos\Delphi\VCL\QDB\CopyFromDS\333.xml');
end;

end.
