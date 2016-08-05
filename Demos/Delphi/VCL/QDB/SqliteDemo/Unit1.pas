unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Def,
  FireDAC.Phys.SQLiteWrapper, FireDAC.Stan.Intf, FireDAC.Phys,
  FireDAC.Phys.SQLite, qprov_sqlite, qdb, Data.DB, Vcl.Grids, Vcl.DBGrids,
  Vcl.ExtCtrls, Vcl.StdCtrls, FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteDef,
  qvalue;

type
  TForm1 = class(TForm)
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    Button1: TButton;
    Image1: TImage;
    Button2: TButton;
    Button3: TButton;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    FDSQLiteFunction1: TFDSQLiteFunction;
    Button4: TButton;
    Button5: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
    procedure DoExecute(Func: TQSqliteFunction; Params: array of TQValue;
      var result: TQValue);
  public
    { Public declarations }
    SQLite: TQSqliteProvider;
    Qry: TQDataSet;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  Qry.Edit;
  Qry.FieldByName('Name').AsString := '≤ªµ√œ–123123';
  TBlobField(Qry.FieldByName('Õº∆¨')).LoadFromFile('D:\QQÕº∆¨20150929104950.png');
  Qry.Post;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Qry.Append;
  Qry.FieldByName('ID').AsInteger := 10;
  Qry.FieldByName('Name').AsString := '≤‚ ‘∞°';
  TBlobField(Qry.FieldByName('Õº∆¨')).LoadFromFile('D:\QQÕº∆¨20150929104950.png');
  Qry.Post;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Qry.Delete;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  I: Integer;
begin
  if not SQLite.TableExists('test_multiset') then
  begin
    SQLite.ExecuteCmd('create table test_multiset(id integer,name char(10));');
    for I := 0 to 10 do
      SQLite.ExecuteCmd('insert into test_multiset(id,name) values (' +
        IntToStr(I) + ',''name_' + IntToStr(I) + ''');');
  end;
  if SQLite.ColumnExists('test_multiset', 'id') then
    ShowMessage('Id exists');
  if not SQLite.ColumnExists('test_multiset', 'key') then
    ShowMessage('Key not exists');
  if not SQLite.OpenDataSet(Qry, ';') then
    ShowMessage(SQLite.LastErrorMsg);
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  Qry.ActiveRecordset := Qry.ActiveRecordset + 1;
end;

procedure TForm1.DoExecute(Func: TQSqliteFunction; Params: array of TQValue;
  var result: TQValue);
var
  I, sm: Integer;
begin
  sm := 0;
  for I := Low(Params) to High(Params) do
    Inc(sm, Params[I].AsInteger);
  result.TypeNeeded(vdtInteger);
  result.AsInteger := sm;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SQLite := TQSqliteProvider.Create(self);
  with SQLite.Functions.Add do // ‘ˆº”“ª∏ˆSqlite∫Ø ˝
  begin
    FuncName := 'CalcSum';
    ArgCount := 3;
    OnExecute := DoExecute;
  end;
  SQLite.LibName := 'sqlite3.dll';
  SQLite.DbName := ExtractFilePath(Application.ExeName) + 'testDb.db3';
  SQLite.Open;
  Qry := TQDataSet.Create(self);
  Qry.Provider := SQLite;
  Qry.BatchMode := False;
  DataSource1.DataSet := Qry;
  // Qry.CommandText := 'select * from testDb';// where Name=''≤‚ ‘»À''';
  Qry.CommandText := 'select CalcSum(4,4,5)';
  SQLite.OpenDataSet(Qry, Qry.CommandText);
  // Qry.Open;
  // sqlite.ExecuteCmd(Qry.CommandText);

  // ShowMessage(Qry.FieldByName('rowId').AsString);
  // TBlobField(Qry.FieldByName('Õº∆¨')).SaveToFile('d:\1.png');
end;

end.
