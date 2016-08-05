unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, qdb, qvalue, qprov_pgsql, qworker, qlog,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, winsock, Vcl.StdCtrls, Vcl.Grids,
  Vcl.DBGrids, Data.DB, Vcl.ExtCtrls, qconverter_stds, Vcl.Samples.Spin,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Stan.Async, FireDAC.DApt, FireDAC.UI.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.Phys.PG, FireDAC.Phys.PGDef,
  FireDAC.Comp.Client, FireDAC.Comp.DataSet, FireDAC.VCLUI.Wait,
  FireDAC.Comp.UI, FireDAC.Stan.StorageBin, FireDAC.Stan.StorageJSON, MemDS,
  DBAccess, Uni, UniProvider, PostgreSQLUniProvider;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    DataSource1: TDataSource;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    btnConnect: TButton;
    Splitter1: TSplitter;
    Button4: TButton;
    Button5: TButton;
    Panel2: TPanel;
    DBGrid1: TDBGrid;
    Panel3: TPanel;
    Button6: TButton;
    Button7: TButton;
    Panel4: TPanel;
    Label2: TLabel;
    mmSQL: TMemo;
    btnApply: TButton;
    UniConnection1: TUniConnection;
    PostgreSQLUniProvider1: TPostgreSQLUniProvider;
    UniQuery1: TUniQuery;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
  private
    { Private declarations }
    FProv: TQPgSQLProvider;
    FDataSet: TQDataSet;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses qstring, Login;
{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  T1, T2: Int64;
  I: Integer;
  ASQL: String;
begin
  btnConnectClick(Sender);
  DataSource1.DataSet := nil;
  try
    ASQL := mmSQL.Text;
    T1 := GetTickCount;
    FDataSet.Close;
    FDataSet.Provider := FProv;
    FDataSet.CommandText := ASQL;
    if FProv.OpenDataSet(FDataSet, ASQL) then
      Memo1.Lines.Add('QDAC 打开成功，记录数：' + IntToStr(FDataSet.RecordCount))
    else
      Memo1.Lines.Add('QDAC 打开失败，错误信息：' + FProv.LastErrorMsg);
    T1 := GetTickCount - T1;
    Memo1.Lines.Add('QDAC 操作用时:' + IntToStr(T1) + 'ms(' +
      RollupTime(T1 div 1000, False) + ')');
  finally
    DataSource1.DataSet := FDataSet;
    for I := 0 to DBGrid1.Columns.Count - 1 do
      DBGrid1.Columns[I].Width := 120;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  AStream: TMemoryStream;
  T: Cardinal;
  S: String;
begin
  btnConnectClick(Sender);
  AStream := TMemoryStream.Create;
  try
    S := mmSQL.Text;
    T := GetTickCount;
    if FProv.OpenStream(AStream, S, TQJsonConverter) then
    begin
      Memo1.Lines.Add('QDAC 用时:' + IntToStr(GetTickCount - T) + 'ms');
    end;
  finally
    FreeObject(AStream);
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  T: Cardinal;
  ARows: Integer;
  I, AStop: Integer;
  S: String;
begin
  btnConnectClick(Sender);
  T := GetTickCount;
  if FProv.ExecuteCmd(mmSQL.Text) >= 0 then
    Memo1.Lines.Add('QDAC 执行脚本成功。')
  else
    Memo1.Lines.Add('QDAC 执行脚本失败：' + FProv.LastErrorMsg);
  T := GetTickCount - T;
  Memo1.Lines.Add('QDAC 操作用时:' + IntToStr(T) + 'ms');
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  ACmd: TQCommand;
  T: Cardinal;
begin
  UniQuery1.Active := False;
  T := GetTickCount;
  UniQuery1.Active := True;
  T := GetTickCount - T;
  Memo1.Lines.Add('UniDAC used time:' + IntToStr(T) + 'ms');
  btnConnectClick(Sender);
  T := GetTickCount;
  if FProv.Prepare(ACmd, 'select * from config where id<$1') then
  begin
    if FProv.OpenDataSet(FDataSet, ACmd, [100]) then
      Memo1.Lines.Add('OK,QDAC used time:' + IntToStr(GetTickCount - T) + 'ms')
    else
      Memo1.Lines.Add(FProv.LastErrorMsg);
    FProv.FreeCommand(ACmd);
  end
  else
    Memo1.Lines.Add(FProv.LastErrorMsg);
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  FProv.OpenDataSet(FDataSet,
    'select id,char_d from dbtypes order by id;' +
    'update dbtypes set char_d=''11123'' where id=999999;'+
    'select id,abstime_d from dbtypes order by id;');
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  if FDataSet.ActiveRecordset > 0 then
    FDataSet.ActiveRecordset := FDataSet.ActiveRecordset - 1;
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  if FDataSet.ActiveRecordset + 1 < FDataSet.RecordsetCount then
    FDataSet.ActiveRecordset := FDataSet.ActiveRecordset + 1;
end;

procedure TForm1.btnConnectClick(Sender: TObject);
begin
  if (not FProv.Connected) then
  begin
    FProv.KeepAlive := False;
    if PgLogin(FProv) then
    begin
      Memo1.Lines.Add('服务器已连接');
      Memo1.Lines.AddStrings(FProv.Params);
    end
    else
      Memo1.Lines.Add('服务器连接失败:' + FProv.LastErrorMsg);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutDown := True;
  FProv := TQPgSQLProvider.Create(Self);
  FDataSet := FProv.AcquireDataSet;
  FDataSet.BatchMode := False;
  DataSource1.DataSet := FDataSet;
  SetDefaultLogFile();
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FProv.ReleaseDataSet(FDataSet);
end;

procedure TForm1.btnApplyClick(Sender: TObject);
begin
  FDataSet.ApplyChanges;
end;

end.
