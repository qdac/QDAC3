unit valueeditor;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Samples.Spin, QString, QMsgPack;

type
  TfrmNodeValueEditor = class(TForm)
    rgDataType: TRadioGroup;
    Label2: TLabel;
    btnLoadFromFile: TButton;
    Button2: TButton;
    Button3: TButton;
    odFromFile: TOpenDialog;
    edtValue: TMemo;
    lblName: TLabel;
    edtName: TEdit;
    Label3: TLabel;
    seExtType: TSpinEdit;
    procedure rgDataTypeClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure btnLoadFromFileClick(Sender: TObject);
  private
    { Private declarations }
    FParentNode: TQMsgPack;
  public
    { Public declarations }
  end;

var
  frmNodeValueEditor: TfrmNodeValueEditor;
function AddNode(AParent: TQMsgPack):Boolean;
function EditNode(ANode: TQMsgPack):Boolean;

implementation

{$R *.dfm}

function AddNode(AParent: TQMsgPack):Boolean;
var
  F: TfrmNodeValueEditor;
  ANode: TQMsgPack;
  ADate: TDateTime;
begin
F := TfrmNodeValueEditor.Create(Application);
F.FParentNode := AParent;
if AParent.IsArray then
  begin
  F.lblName.Enabled:=False;
  F.edtName.Enabled:=False;
  end;
F.ShowModal;
Result:=False;
if F.ModalResult = mrOk then
  begin
  ANode := AParent.Add(F.edtName.Text);
  case F.rgDataType.ItemIndex of
    0:
      ANode.AsInt64 := StrToInt(F.edtValue.Text);
    1:
      ANode.ResetNull;
    2:
      ANode.AsBoolean := StrToBool(F.edtValue.Text);
    3, 4:
      ANode.AsFloat := StrToFloat(F.edtValue.Text);
    5:
      ANode.AsString := F.edtValue.Text;
    6:
      ANode.AsBytes := HexToBin(F.edtValue.Text);
    7:
      ANode.DataType := mptArray;
    8:
      ANode.DataType := mptMap;
    9:
      begin
      ANode.ExtType := F.seExtType.Value;
      ANode.AsExtBytes := HexToBin(F.edtValue.Text);
      end;
    10:
      if ParseDateTime(PWideChar(F.edtValue.Text), ADate) then
        ANode.AsDateTime := ADate
      else if ParseWebTime(PQCharW(F.edtValue.Text), ADate) then
        ANode.AsDateTime := ADate
      else if TryStrToDateTime(F.edtValue.Text, ADate) then
        ANode.AsDateTime := ADate;
  end;
  Result:=True;
  end;
FreeObject(F);
end;

function EditNode(ANode: TQMsgPack):Boolean;
var
  F: TfrmNodeValueEditor;
  ADate: TDateTime;
begin
F := TfrmNodeValueEditor.Create(Application);
F.FParentNode := ANode.Parent;
if ANode.Parent.IsArray then
  begin
  F.edtName.Text := '[' + IntToStr(ANode.ItemIndex) + ']';
  F.edtName.Enabled := False;
  end
else
  F.edtName.Text := ANode.Name;
F.rgDataType.ItemIndex := Integer(ANode.DataType) - 1;
if ANode.DataType in [mptArray, mptMap] then
  begin
  F.edtValue.Enabled := False;
  F.edtValue.Text := '<复合类型>';
  end
else
  F.edtValue.Text := ANode.AsString;
if ANode.DataType = mptExtended then
  begin
  F.seExtType.Value := ANode.ExtType;
  F.seExtType.Enabled := True;
  end
else
  F.seExtType.Enabled := False;
F.btnLoadFromFile.Enabled := ANode.DataType in [mptString, mptBinary,
  mptExtended];
F.ShowModal;
Result:=False;
if F.ModalResult = mrOk then
  begin
  if F.edtName.Enabled then
    ANode.Name := F.edtName.Text;
  case F.rgDataType.ItemIndex of
    0:
      ANode.AsInt64 := StrToInt(F.edtValue.Text);
    1:
      ANode.ResetNull;
    2:
      ANode.AsBoolean := StrToBool(F.edtValue.Text);
    3, 4:
      ANode.AsFloat := StrToFloat(F.edtValue.Text);
    5:
      ANode.AsString := F.edtValue.Text;
    6:
      ANode.AsBytes := HexToBin(F.edtValue.Text);
    7:
      ANode.DataType := mptArray;
    8:
      ANode.DataType := mptMap;
    9:
      begin
      ANode.ExtType := F.seExtType.Value;
      ANode.AsExtBytes := HexToBin(F.edtValue.Text);
      end;
    10:
      if ParseDateTime(PWideChar(F.edtValue.Text), ADate) then
        ANode.AsDateTime := ADate
      else if ParseWebTime(PQCharW(F.edtValue.Text), ADate) then
        ANode.AsDateTime := ADate
      else if TryStrToDateTime(F.edtValue.Text, ADate) then
        ANode.AsDateTime := ADate;
  end;
  Result:=True;
  end;
FreeObject(F);
end;

procedure TfrmNodeValueEditor.btnLoadFromFileClick(Sender: TObject);
var
  AStream:TMemoryStream;
begin
if odFromFile.Execute then
  begin
  if rgDataType.ItemIndex = 5 then
    edtValue.Text := LoadTextW(odFromFile.FileName)
  else
    begin
    AStream:=TMemoryStream.Create;
    try
      AStream.LoadFromFile(odFromFile.FileName);
      edtValue.Text := BinToHex(AStream.Memory,AStream.Size);
    finally
      FreeObject(AStream);
    end;
    end;
  end;
end;

procedure TfrmNodeValueEditor.Button2Click(Sender: TObject);
var
  I: Int64;
  F: Double;
  D: TDateTime;
  B: Boolean;
begin
edtName.Text := HtmlTrimText(edtName.Text);
if edtName.Enabled then
  begin
  if Length(edtName.Text) = 0 then
    begin
    if edtName.Enabled then
      begin
      Application.MessageBox('名称不应为空。”，请重新输入。', '错误', MB_OK or MB_ICONSTOP);
      Exit;
      end;
    end
  else
    begin
    if ContainsCharW(edtName.Text, '[]\/.') then
      begin
      Application.MessageBox('名称不应包含保留字符“[]\/.”，请重新输入。', '错误',
        MB_OK or MB_ICONSTOP);
      Exit;
      end;
    end;
  end;
case rgDataType.ItemIndex of
  0:
    if not TryStrToInt64(edtValue.Text, I) then
      begin
      Application.MessageBox('输入的值不是有效的整数，请重新输入。', '错误', MB_OK or MB_ICONSTOP);
      Exit;
      end;
  2:
    if not TryStrToBool(edtValue.Text, B) then
      begin
      Application.MessageBox('输入的值不是有效的布尔值，请重新输入。', '错误', MB_OK or MB_ICONSTOP);
      Exit;
      end;
  3, 4:
    if not TryStrToFloat(edtValue.Text, F) then
      begin
      Application.MessageBox('输入的值不是有效的浮点数，请重新输入。', '错误', MB_OK or MB_ICONSTOP);
      Exit;
      end;
  6:
    if (Length(edtValue.Text) > 0) and (Length(HexToBin(edtValue.Text)) = 0)
    then
      begin
      Application.MessageBox('输入的值不是有效的十六进制字节数据流，请重新输入。', '错误',
        MB_OK or MB_ICONSTOP);
      Exit;
      end;
  9:
    begin
    if (Length(edtValue.Text) > 0) and (Length(HexToBin(edtValue.Text)) = 0)
    then
      begin
      Application.MessageBox('输入的值不是有效的十六进制字节数据流，请重新输入。', '错误',
        MB_OK or MB_ICONSTOP);
      Exit;
      end;
    end;
  10:
    if not(ParseDateTime(PWideChar(edtValue.Text), D) or
      ParseWebTime(PQCharW(edtValue.Text), D) or
      TryStrToDateTime(edtValue.Text, D)) then
      begin
      Application.MessageBox('输入的值不是有效的日期时间类型，请重新输入。', '错误',
        MB_OK or MB_ICONSTOP);
      Exit;
      end;
end;
ModalResult := mrOk;
end;

procedure TfrmNodeValueEditor.rgDataTypeClick(Sender: TObject);
var
  I: Int64;
  F: Double;
  D: TDateTime;
  B: Boolean;
begin
edtValue.Enabled := not(rgDataType.ItemIndex in [1, 7, 8]);
btnLoadFromFile.Enabled := rgDataType.ItemIndex in [5, 6, 9];
seExtType.Enabled := (rgDataType.ItemIndex = 9);
case rgDataType.ItemIndex of
  0:
    if not TryStrToInt64(edtValue.Text, I) then
      edtValue.Text := '0';
  1:
    edtValue.Text := '<NULL>';
  2:
    if not TryStrToBool(edtValue.Text, B) then
      edtValue.Text := 'False';
  3, 4:
    if not TryStrToFloat(edtValue.Text, F) then
      edtValue.Text := '0';
  6:
    if (Length(edtValue.Text) > 0) and (Length(HexToBin(edtValue.Text)) = 0)
    then
      edtValue.Text := '';
  7, 8:
    edtValue.Text := '<复合类型>';
  9:
    begin
    if (Length(edtValue.Text) > 0) and (Length(HexToBin(edtValue.Text)) = 0)
    then
      edtValue.Text := '';
    end;
  10:
    if not(ParseDateTime(PWideChar(edtValue.Text), D) or
      ParseWebTime(PQCharW(edtValue.Text), D) or
      TryStrToDateTime(edtValue.Text, D)) then
      edtValue.Text := ''
end;
end;

end.
