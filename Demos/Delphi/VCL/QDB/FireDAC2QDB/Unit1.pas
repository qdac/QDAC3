unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt, FireDAC.UI.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.Phys.PG,
  FireDAC.Phys.PGDef, FireDAC.VCLUI.Wait, FireDAC.Comp.UI, Data.DB,
  FireDAC.Comp.Client, FireDAC.Comp.DataSet, Vcl.StdCtrls,
  FireDAC.Stan.StorageBin, FireDAC.Stan.StorageJSON, qdb, qconverter_fdac,
  qtimetypes, jpeg, pngimage, gifimg,
  Vcl.Grids, Vcl.DBGrids, Vcl.ExtCtrls, FireDAC.Stan.StorageXML, Vcl.DBCtrls,
  Vcl.ExtDlgs, FireDAC.Phys.MSSQL, FireDAC.Phys.MSSQLDef, FireDAC.Phys.ODBCBase;

type
  TForm1 = class(TForm)
    FDQuery1: TFDQuery;
    fdPg: TFDConnection;
    FDPhysPgDriverLink1: TFDPhysPgDriverLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDStanStorageJSONLink1: TFDStanStorageJSONLink;
    FDStanStorageBinLink1: TFDStanStorageBinLink;
    DataSource1: TDataSource;
    Panel1: TPanel;
    Button3: TButton;
    Panel2: TPanel;
    DataSource2: TDataSource;
    Button1: TButton;
    Splitter1: TSplitter;
    Panel4: TPanel;
    Label1: TLabel;
    Panel3: TPanel;
    mmSQL: TMemo;
    Label2: TLabel;
    Button2: TButton;
    Button4: TButton;
    FDSchemaAdapter1: TFDSchemaAdapter;
    chkDeleted: TCheckBox;
    chkUnchange: TCheckBox;
    chkModified: TCheckBox;
    chkInserted: TCheckBox;
    cbxConverterType: TComboBox;
    Label3: TLabel;
    FDStanStorageXMLLink1: TFDStanStorageXMLLink;
    Button5: TButton;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    OpenPictureDialog1: TOpenPictureDialog;
    Panel5: TPanel;
    Image1: TImage;
    Image2: TImage;
    Memo1: TMemo;
    Memo2: TMemo;
    Button6: TButton;
    Button7: TButton;
    fdSQL: TFDConnection;
    OpenDialog1: TOpenDialog;
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure chkInsertedClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FDQuery1AfterScroll(DataSet: TDataSet);
    procedure Image1DblClick(Sender: TObject);
    procedure Image2DblClick(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
  private
    { Private declarations }
    FQDataSet: TQDataSet;
    procedure QDataSetNeeded;
    procedure QDataSetAfterScroll(DataSet: TDataSet);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses qstring, qvalue;
{$R *.dfm}

type
  TImageFormat = (ifUnknown, ifBitmap, ifJpeg, ifPng, ifGif, ifMetafile);

function DetectImageFormat(AStream: TStream): TImageFormat;
var
  AHeader: array [0 .. 7] of AnsiChar;
  APos: Int64;

  function IsJpeg: Boolean;
  begin
    Result := (AHeader[0] = #$FF) and (AHeader[1] = #$D8);
  end;
  function IsBitmap: Boolean;
  begin
    Result := (AHeader[0] = #$42) and (AHeader[1] = #$4D);
  end;
  function IsPng: Boolean;
  begin
    Result := (AHeader[0] = #$89) and (AHeader[1] = #$50) and
      (AHeader[2] = #$4E) and (AHeader[3] = #$47) and (AHeader[4] = #$0D) and
      (AHeader[5] = #$0A) and (AHeader[6] = #$1A) and (AHeader[7] = #$0A);
  end;
  function IsGif: Boolean;
  begin
    Result := (AHeader[0] = #$47) and (AHeader[1] = #$49) and
      (AHeader[2] = #$46) and (AHeader[3] = #$38) and
      (AHeader[4] in [#$37, #$39]) and (AHeader[5] = #$61);
  end;
  function IsMetafile: Boolean;
  begin
    Result := (AHeader[0] = #$01) and (AHeader[1] = #$00) and (AHeader[3] = #0)
      and ((AHeader[2] = #0) or ((AHeader[2] = #9) and (AHeader[4] = #0) and
      (AHeader[5] = #3)));
  end;

begin
  APos := AStream.Position;
  Result := ifUnknown;
  if AStream.Read(AHeader[0], 8) = 8 then
  begin
    if IsJpeg then
      Result := ifJpeg
    else if IsPng then
      Result := ifPng
    else if IsBitmap then
      Result := ifBitmap
    else if IsGif then
      Result := ifGif
    else if IsMetafile then
      Result := ifMetafile
  end;
  AStream.Position := APos;
end;

function LoadGraphic(AStream: TStream): TGraphic;
begin
  Result := nil;
  case DetectImageFormat(AStream) of
    ifJpeg:
      Result := TJPEGImage.Create;
    ifBitmap:
      Result := TBitmap.Create;
    ifPng:
      Result := TPNGImage.Create;
    ifGif:
      Result := TGifImage.Create;
    ifMetafile:
      Result := TMetafile.Create;
  end;
  if Assigned(Result) then
    Result.LoadFromStream(AStream);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
//  FDQuery1.Close;
//  FDQuery1.DatSManager.Tables.Clear;
//  FDQuery1.Open(mmSQL.Text);
//  Caption := IntToStr(FDQuery1.DatSManager.Tables.Count);
FDQuery1.LoadFromFile('D:\User\QDAC3\Temp\test\test.bin');
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  AStream: TMemoryStream;
  S: String;
begin
  FDQuery1.Close;
  FDQuery1.DatSManager.Clear;
  ShowMessage(' OK 将TQDataSet的内容转换到 TFDQuery');
  AStream := TMemoryStream.Create;
  try
    case cbxConverterType.ItemIndex of
      0:
        begin
          FQDataSet.SaveToStream(AStream, TQFDBinaryConverter, merAll);
          AStream.SaveToFile(ExtractFilePath(Application.ExeName) + 'test.bin');
          AStream.Position := 0;
          FDQuery1.LoadFromStream(AStream, sfBinary);
        end;
      1:
        begin
          FQDataSet.SaveToStream(AStream, TQFDJsonConverter, merAll);
          AStream.SaveToFile(ExtractFilePath(Application.ExeName) +
            'test.json');
          AStream.Position := 0;
          FDQuery1.LoadFromStream(AStream, sfJson);
        end;
      2:
        begin
          FQDataSet.SaveToStream(AStream, TQFDXMLConverter, merAll);
          AStream.SaveToFile(ExtractFilePath(Application.ExeName) + 'test.xml');
          AStream.Position := 0;
          FDQuery1.LoadFromStream(AStream, sfXML);
        end;
    end;
  finally
    FreeAndNil(AStream);
    Caption := IntToStr(FDQuery1.DatSManager.Tables.Count);
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  AStream: TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  try
    case cbxConverterType.ItemIndex of
      0:
        begin
          FDQuery1.SaveToStream(AStream, sfBinary);
          AStream.SaveToFile(ExtractFilePath(Application.ExeName) + 'test.bin');
        end;
      1:
        begin
          FDQuery1.SaveToStream(AStream, sfJson);
          AStream.SaveToFile(ExtractFilePath(Application.ExeName) +
            'test.json');
        end;
      2:
        begin
          FDQuery1.SaveToStream(AStream, sfXML);
          AStream.SaveToFile(ExtractFilePath(Application.ExeName) + 'test.xml');
        end;
    end;
    AStream.Position := 0;
    QDataSetNeeded;
    case cbxConverterType.ItemIndex of
      0:
        FQDataSet.LoadFromStream(AStream, TQFDBinaryConverter);
      1:
        FQDataSet.LoadFromStream(AStream, TQFDJsonConverter);
      2:
        FQDataSet.LoadFromStream(AStream, TQFDXMLConverter);
    end;

  finally
    FreeAndNil(AStream);
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  FDSchemaAdapter1.ApplyUpdates();
  // FDQuery1.CommitUpdates();
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  T: Cardinal;
begin
  FQDataSet.MarkStatus(usInserted);
//  QDataSetNeeded;
//  if OpenDialog1.Execute then
//    begin
//    T := GetTickCount;
//    FQDataSet.LoadFromFile(OpenDialog1.FileName,
//      TQFDBinaryConverter);
//    // FDQuery1.LoadFromFile('C:\Users\swish\Documents\aaa.fdbin',sfBinary);
//    T := GetTickCount - T;
//    ShowMessage(IntToStr(T) + 'ms');
//    end;
  // T:=GetTickCount;
  // FDQuery1.SaveToFile('C:\Users\swish\Documents\bbb.fdbin',sfBinary);
  // T:=GetTickCount-T;
  // ShowMessage(IntToStr(T)+'ms');
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  AField: TField;
  ABytes: TBytes;
begin
  AField := FQDataSet.FindField('bit_d');
  if Assigned(AField) then
  begin
    ABytes := HexToBin(Memo2.Lines.Text);
    FQDataSet.Edit;
    AField.AsBytes := ABytes;
    FQDataSet.Post;
  end;
end;

procedure TForm1.Button7Click(Sender: TObject);
var
  AField: TField;
  ABytes: TBytes;
begin
  AField := FDQuery1.FindField('bit_d');
  if Assigned(AField) then
  begin
    ABytes := HexToBin(Memo1.Lines.Text);
    FDQuery1.Edit;
    AField.AsBytes := ABytes;
    FDQuery1.Post;
  end;
end;

procedure TForm1.chkInsertedClick(Sender: TObject);
var
  AChanges: TFDUpdateRecordTypes;
begin
  if chkUnchange.Checked then
    AChanges := [rtUnmodified]
  else
    AChanges := [];
  if chkModified.Checked then
    AChanges := AChanges + [rtModified];
  if chkInserted.Checked then
    AChanges := AChanges + [rtInserted];
  if chkDeleted.Checked then
    AChanges := AChanges + [rtDeleted];
  FDQuery1.FilterChanges := AChanges;
end;

procedure TForm1.FDQuery1AfterScroll(DataSet: TDataSet);
var
  AGraphic: TGraphic;
  AStream: TStream;
  AField: TField;
  ABytes: TArray<Byte>;
begin
  AField := DataSet.FindField('bytes_d');
  if AField <> nil then
  begin
    AStream := DataSet.CreateBlobStream(AField, bmRead);
    try
      AGraphic := LoadGraphic(AStream);
      Image1.Picture.Assign(AGraphic);
    finally
      FreeAndNil(AStream);
      FreeAndNil(AGraphic);
    end;
  end;
  AField := DataSet.FindField('bit_d');
  if Assigned(AField) then
  begin
    ABytes := AField.AsBytes;
    Memo1.Lines.Text := BinToHex(ABytes);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  AMask: TQPlanMask;
begin
  // AMask.AsString:='* 0/1 * * *';
  // ShowMessage(AMask.AsString);
end;

procedure TForm1.Image1DblClick(Sender: TObject);
var
  AStream: TStream;
  AFSStream: TFileStream;
begin
  if OpenPictureDialog1.Execute then
  begin
    FDQuery1.Edit;
    AStream := FDQuery1.CreateBlobStream
      (FDQuery1.FieldByName('bytes_d'), bmWrite);
    AFSStream := TFileStream.Create(OpenPictureDialog1.FileName, fmOpenRead);
    AStream.CopyFrom(AFSStream, 0);
    FreeAndNil(AFSStream);
    FreeAndNil(AStream);
    FDQuery1.Post;
  end;
end;

procedure TForm1.Image2DblClick(Sender: TObject);
var
  AStream: TStream;
  AFSStream: TFileStream;
begin
  if OpenPictureDialog1.Execute then
  begin
    FQDataSet.Edit;
    AStream := FQDataSet.CreateBlobStream
      (FQDataSet.FieldByName('bytes_d'), bmWrite);
    AFSStream := TFileStream.Create(OpenPictureDialog1.FileName, fmOpenRead);
    AStream.CopyFrom(AFSStream, 0);
    FreeAndNil(AFSStream);
    FreeAndNil(AStream);
    FQDataSet.Post;
  end;
end;

procedure TForm1.QDataSetAfterScroll(DataSet: TDataSet);
var
  AGraphic: TGraphic;
  AStream: TStream;
  AField: TField;
  ABytes: TArray<Byte>;
begin
  AField := DataSet.FindField('bytes_d');
  if AField <> nil then
  begin
    AStream := DataSet.CreateBlobStream(AField, bmRead);
    try
      AGraphic := LoadGraphic(AStream);
      Image2.Picture.Assign(AGraphic);
    finally
      FreeAndNil(AStream);
      FreeAndNil(AGraphic);
    end;
  end;
  AField := DataSet.FindField('bit_d');
  if Assigned(AField) then
  begin
    ABytes := AField.AsBytes;
    Memo2.Lines.Text := BinToHex(ABytes);
  end;
end;

procedure TForm1.QDataSetNeeded;
begin
  if not Assigned(FQDataSet) then
  begin
    FQDataSet := TQDataSet.Create(Self);
    FQDataSet.AfterScroll := QDataSetAfterScroll;
    DataSource2.DataSet := FQDataSet;
  end;
end;

end.
