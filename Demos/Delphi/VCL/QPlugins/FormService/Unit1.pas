unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics,
  Controls, Forms, Dialogs, DB, Grids, DBGrids, qdb,
  qprov_pgsql,
  // qplugins所需要的单元
  qplugins_base, qplugins, qplugins_params, qplugins_formsvc,
  qplugins_vcl_formsvc,
  ExtCtrls, StdCtrls;

type
  TForm1 = class(TForm, IQCustomAction)
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
    function DoAction(AParams: IQParams; AResult: IQParams): Boolean;
    procedure DoOpenDataSet(AParams: IInterface);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses qstring;
{$R *.dfm}

function TForm1.DoAction(AParams, AResult: IQParams): Boolean;
begin
  //
end;

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

procedure TForm1.DoOpenDataSet(AParams: IInterface);
begin
  if not FProv.Open then
    raise Exception.Create(FProv.LastErrorMsg);
  FProv.OpenDataSet(FMasterDataSet,
    'select oid,relname from pg_class where relkind=''r''');

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
  FMasterDataSet := TQDataSet.Create(Self);
  FDetailDataSet := TQDataSet.Create(Self);
  DataSource1.DataSet := FMasterDataSet;
  DataSource2.DataSet := FDetailDataSet;
  FDetailDataSet.OnMasterChanged := DoMasterChanged;
  FDetailDataSet.MasterSource := DataSource1;
  PluginsManager.AsynCall(DoOpenDataSet, nil);
end;

initialization

RegisterFormService('/Services/Docks/Forms', 'QdbMasterDetail', TForm1, False);

finalization

UnregisterServices('/Services/Docks/Forms', ['QdbMasterDetail']);

end.
