unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Data.DB, Vcl.Grids, Vcl.DBGrids, qstring, qdb, qconverter_sql;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    PageControl1: TPageControl;
    tsMSSQL: TTabSheet;
    tsPgSQL: TTabSheet;
    tsMySQL: TTabSheet;
    mmMSSQL: TMemo;
    mmPgSQL: TMemo;
    mmMySQL: TMemo;
    DBGrid1: TDBGrid;
    Panel2: TPanel;
    Button1: TButton;
    DataSource1: TDataSource;
    Label1: TLabel;
    edtTableName: TEdit;
    chkAllAsInsert: TCheckBox;
    chkByTemplate: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FPgSQL: TQPgSQLConverter;
    FMSSQL: TQMSSQLConverter;
    FMySQL: TQMySQLConverter;
    FDataSet: TQDataSet;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  I: Integer;
begin
  if Length(edtTableName.Text) > 0 then
  begin
    for I := 0 to FDataSet.FieldDefs.Count - 1 do
      TQFieldDef(FDataSet.FieldDefs[I]).Table := edtTableName.Text;
    if chkByTemplate.Checked then
    begin
      FMSSQL.InsertTemplate := 'insert into ' + edtTableName.Text +
        ' (id,name) values ({id.new},{name.new});';
      FPgSQL.InsertTemplate := FMSSQL.InsertTemplate;
      FMySQL.InsertTemplate := FMSSQL.InsertTemplate;
      FMSSQL.DeleteTemplate := 'delete from ' + edtTableName.Text +
        ' where id={id.old}';
      FPgSQL.DeleteTemplate := FMSSQL.DeleteTemplate;
      FMySQL.DeleteTemplate := FMSSQL.DeleteTemplate;
      FMSSQL.UpdateTemplate := 'update ' + edtTableName.Text +
        ' set name={name.new} where id={id.old};';
      FPgSQL.UpdateTemplate := FMSSQL.UpdateTemplate;
      FMySQL.UpdateTemplate := FMSSQL.UpdateTemplate;
    end
    else
    begin
      FMSSQL.InsertTemplate := '';
      FPgSQL.InsertTemplate := '';
      FMySQL.InsertTemplate := '';
      FMSSQL.DeleteTemplate := '';
      FPgSQL.DeleteTemplate := '';
      FMySQL.DeleteTemplate := '';
      FMSSQL.UpdateTemplate := '';
      FPgSQL.UpdateTemplate := '';
      FMySQL.UpdateTemplate := '';
    end;
    FMSSQL.AllAsInsert := chkAllAsInsert.Checked;
    FPgSQL.AllAsInsert := chkAllAsInsert.Checked;
    FMySQL.AllAsInsert := chkAllAsInsert.Checked;
    mmMSSQL.Text := FMSSQL.SQL;
    mmPgSQL.Text := FPgSQL.SQL;
    mmMySQL.Text := FMySQL.SQL;
  end
  else
    ShowMessage('表名不能为空。');
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  AFdId, AFdName, AFdBirthday, AFdSex, AFdBin: TField;
  I: Integer;
  function NewBytes: TBytes;
  begin
    SetLength(Result, 16);
    PGuid(@Result[0])^ := NewId;
  end;

begin
  FDataSet := TQDataSet.Create(Self);
  FPgSQL := TQPgSQLConverter.Create(Self);
  FPgSQL.DataSet := FDataSet;
  FMSSQL := TQMSSQLConverter.Create(Self);
  FMSSQL.DataSet := FDataSet;
  FMySQL := TQMySQLConverter.Create(Self);
  FMySQL.DataSet := FDataSet;
  DataSource1.DataSet := FDataSet;
  FDataSet.FieldDefs.Add('id', ftInteger);
  FDataSet.FieldDefs.Add('name', ftWideString, 20);
  FDataSet.FieldDefs.Add('birthday', ftDateTime);
  FDataSet.FieldDefs.Add('sex', ftBoolean);
  FDataSet.FieldDefs.Add('bin', ftVarBytes, 16);
  for I := 0 to FDataSet.FieldDefs.Count - 1 do
    TQFieldDef(FDataSet.FieldDefs[I]).Table := 'testtable';
  FDataSet.CreateDataSet;
  AFdId := FDataSet.FieldByName('id');
  AFdName := FDataSet.FieldByName('name');
  AFdBirthday := FDataSet.FieldByName('birthday');
  AFdSex := FDataSet.FieldByName('sex');
  AFdBin := FDataSet.FieldByName('bin');
  FDataSet.DisableControls;
  try
    for I := 0 to 3 do
    begin
      FDataSet.Append;
      AFdId.AsInteger := I;
      AFdName.AsString := 'Name_' + IntToStr(I);
      AFdBirthday.AsDateTime := Now - random(3000);
      if random(100) > 40 then
        AFdSex.AsBoolean := true
      else
        AFdSex.AsBoolean := False;
      AFdBin.AsBytes := NewBytes;
      FDataSet.Post;
    end;
    // 将上面的记录标记原始数据
    FDataSet.ApplyChanges;
    // 删除第二条记录
    FDataSet.RecNo := 2;
    FDataSet.Delete;
    // 再插入2条记录
    for I := 4 to 5 do
    begin
      FDataSet.Append;
      AFdId.AsInteger := I;
      AFdName.AsString := 'Name_' + IntToStr(I);
      AFdBirthday.AsDateTime := Now - random(3000);
      if random(100) > 40 then
        AFdSex.AsBoolean := true
      else
        AFdSex.AsBoolean := False;
      AFdBin.AsBytes := NewBytes;
      FDataSet.Post;
    end;
    // 再修改第1条记录性别
    FDataSet.First;
    FDataSet.Edit;
    AFdSex.AsBoolean := not AFdSex.AsBoolean;
    FDataSet.Post;
  finally
    FDataSet.EnableControls;
  end;
end;

end.
