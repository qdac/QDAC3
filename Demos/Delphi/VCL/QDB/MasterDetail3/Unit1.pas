unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.Grids, Vcl.DBGrids, qdb,
  qprov_pgsql, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    DataSource1: TDataSource;
    DataSource2: TDataSource;
    Panel1: TPanel;
    Label1: TLabel;
    Panel2: TPanel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FProv: TQPgSQLProvider;
    FRequests, FHits: Integer;
    FMasterDataSet, FDetailDataSet: TQDataSet;
    procedure DoMasterChanged(ADataSet: TDataSet);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses qstring;
{$R *.dfm}

procedure TForm1.DoMasterChanged(ADataSet: TDataSet);
var
  S: QStringW;
  ASub: TQDataSet;
  I: Integer;
begin
  try
    Inc(FRequests);
    S := 'select * from pg_attribute where attrelid=' +
      FMasterDataSet.FieldByName('oid').AsString;
    for I := 0 to FDetailDataSet.RecordsetCount - 1 do
    begin
      if FDetailDataSet.Recordsets[I].CommandText = S then
      begin
        FDetailDataSet.ActiveRecordset := I;
        Inc(FHits);
        Exit;
      end;
    end;
    ASub := FProv.OpenDataSet(S);
    FDetailDataSet.AddDataSet(ASub);
    FDetailDataSet.ActiveRecordset := FDetailDataSet.RecordsetCount - 1;
  finally
    Panel2.Caption := '总计: ' + IntToStr(FRequests) + ' 次请求，缓存命中:' +
      IntToStr(FHits) + '次';
  end;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FProv := TQPgSQLProvider.Create(Self);
  FProv.ServerHost := 'www.qdac.cc';
  FProv.ServerPort := 15432;
  FProv.UserName := 'qdac';
  FProv.Password := 'Qdac.Demo';
  FProv.Database := 'QDAC_Demo';
  FProv.KeepAlive := False;
  if not FProv.Open then
    raise Exception.Create(FProv.LastErrorMsg);
  FMasterDataSet := TQDataSet.Create(Self);
  FProv.OpenDataSet(FMasterDataSet,
    'select oid,relname from pg_class where relkind=''r''');
  FDetailDataSet := TQDataSet.Create(Self);
  DataSource1.DataSet := FMasterDataSet;
  DataSource2.DataSet := FDetailDataSet;
  FDetailDataSet.OnMasterChanged := DoMasterChanged;
  FDetailDataSet.MasterSource := DataSource1;
end;

end.
