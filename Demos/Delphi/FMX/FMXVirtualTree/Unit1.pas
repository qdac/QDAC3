unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.TextLayout,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects
  // Custom units
    , qdac_fmx_virtualtree, Math, System.Generics.Collections, FMX.ImgList,
  FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.Layouts, Data.DB, Data.Win.ADODB;

type
  TDBCellData = class(TQVTDefaultCellData)
  protected
    FField: TField;
  public
    constructor Create(AField: TField); overload;
  end;

  TForm1 = class(TForm)
    Layout1: TLayout;
    chkHorizLine: TCheckBox;
    chkTreeLine: TCheckBox;
    chkShowHeader: TCheckBox;
    chkNodeButton: TCheckBox;
    chkVertLine: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    chkDrawHover: TCheckBox;
    chkRowSelection: TCheckBox;
    chkColSelection: TCheckBox;
    chkColSizable: TCheckBox;
    chkRowSizable: TCheckBox;
    Label1: TLabel;
    adsData: TADODataSet;
    adsDataName: TStringField;
    adsDataPrice: TCurrencyField;
    adsDataSold: TFloatField;
    adsDataPackable: TBooleanField;
    adsDataStatus: TBooleanField;
    adsDataId: TIntegerField;
    adsDataParent: TIntegerField;
    procedure FormCreate(Sender: TObject);
    procedure chkTreeLineChange(Sender: TObject);
    procedure chkShowHeaderChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure chkRowSizableChange(Sender: TObject);
  private
    FTreeView: TQVirtualTreeView;
    FCellDatas: array of IVTCellData;
    procedure DoGetCellData(Sender: TQVirtualTreeView; ANode: TQVTNode;
      ACol: Integer; var AData: IVTCellData);
    procedure DoHoverChanged(Sender: TObject);
    procedure CreateDemoDataSet;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

type
  // TDBTreeAdapter = class
  // protected
  // FIdField, FParentField: TField;
  // public
  // constructor Create(AIdField, AParentField: TField); overload;
  //
  // end;

  TDBTextCellData = class(TDBCellData, IVTTextCellData)
  protected
    function GetText: String;
  end;

  TDBProgressCellData = class(TDBCellData, IVTTextCellData, IVTProgressCellData,
    IVTTextDrawable)
  protected
    FTextSettings: TTextSettings;
    function GetProgress: Single;
    function GetText: String;
    function GetTextSettings: TTextSettings;
  public
    constructor Create(AField: TField); overload;
    destructor Destroy; override;
  end;

  TDBCheckCellData = class(TDBCellData, IVTCheckCellData)
  protected
    function GetText: String;
    function GetCheckStates: TQVTCheckStates;
    procedure SetCheckStates(AStates: TQVTCheckStates);
    function GetFollowStates: Boolean;
    procedure SetFollowStates(const value: Boolean);
    function GetCheckLayout: TQVTLayout;
    procedure SetCheckLayout(value: TQVTLayout);
  public
    constructor Create(AField: TField); overload;
  end;

  TDBRadioCellData = class(TDBCellData, IVTRadioCellData)
  protected
    function GetText: String;
    function GetGroupId: Integer;
    procedure SetGroupId(const AId: Integer);
    function GetCheckStates: TQVTCheckStates;
    procedure SetCheckStates(AStates: TQVTCheckStates);
    function GetRadioLayout: TQVTLayout;
    procedure SetRadioLayout(value: TQVTLayout);
  public
    constructor Create(AField: TField); overload;
  end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FTreeView.VertScrollBar.Value := FTreeView.VertScrollBar.Value + 10;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FTreeView.VertScrollBar.Value := FTreeView.VertScrollBar.Value - 10;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  FTreeView.HorzScrollBar.Value := FTreeView.HorzScrollBar.Value + 10;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  FTreeView.HorzScrollBar.Value := FTreeView.HorzScrollBar.Value - 10;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  FTreeView.FocusNode := FTreeView.RootNode.GetFirstChild;
end;

procedure TForm1.chkRowSizableChange(Sender: TObject);
begin
  if chkRowSizable.IsChecked then
    FTreeView.Options := FTreeView.Options + [TQVTOption.toRowSizable]
  else
    FTreeView.Options := FTreeView.Options - [TQVTOption.toRowSizable];
  if chkColSizable.IsChecked then
    FTreeView.Header.Options := FTreeView.Header.Options +
      [TQVTHeaderOption.hoResizable]
  else
    FTreeView.Header.Options := FTreeView.Header.Options -
      [TQVTHeaderOption.hoResizable];
end;

procedure TForm1.chkShowHeaderChange(Sender: TObject);
begin
  if chkShowHeader.IsChecked then
    FTreeView.Header.Options := FTreeView.Header.Options +
      [TQVTHeaderOption.hoVisible]
  else
    FTreeView.Header.Options := FTreeView.Header.Options -
      [TQVTHeaderOption.hoVisible];
end;

procedure TForm1.chkTreeLineChange(Sender: TObject);
var
  AOptions: TQVTPaintOptions;
begin
  AOptions := [];
  if chkHorizLine.IsChecked then
    AOptions := AOptions + [TQVTPaintOption.poHorizLine];
  if chkVertLine.IsChecked then
    AOptions := AOptions + [TQVTPaintOption.poVertLine];
  if chkTreeLine.IsChecked then
    AOptions := AOptions + [TQVTPaintOption.poTreeLine];
  if chkNodeButton.IsChecked then
    AOptions := AOptions + [TQVTPaintOption.poNodeButton];
  if chkDrawHover.IsChecked then
  begin
    AOptions := AOptions + [TQVTPaintOption.poHover];
    FTreeView.Options := FTreeView.Options + [TQVTOption.toTestHover];
  end
  else
    FTreeView.Options := FTreeView.Options - [TQVTOption.toTestHover];
  if chkRowSelection.IsChecked then
    AOptions := AOptions + [TQVTPaintOption.poRowSelection];
  if chkColSelection.IsChecked then
    AOptions := AOptions + [TQVTPaintOption.poColSelection];
  FTreeView.PaintOptions := AOptions;
end;

procedure TForm1.CreateDemoDataSet;
const
  ANames: array [0 .. 9] of String = ('西红柿', '白菜', '菠菜', '猪肉', '草鱼', '桔子', '荔芝',
    '苹果', '香蕉', '西瓜');
var
  I, AId: Integer;
begin
  adsData.CreateDataSet;
  adsData.Append;
  adsDataId.AsInteger := 1;
  adsDataName.AsString := '蔬菜';
  adsData.Post;
  adsData.Append;
  adsDataId.AsInteger := 2;
  adsDataName.AsString := '水果';
  adsData.Post;
  adsData.Append;
  adsDataId.AsInteger := 3;
  adsDataName.AsString := '植物';
  adsDataParent.AsInteger := 1;
  adsData.Post;
  adsData.Append;
  adsDataId.AsInteger := 4;
  adsDataName.AsString := '动物';
  adsDataParent.AsInteger := 1;
  adsData.Post;
  AId := 5;
  // 植物:50
  for I := 0 to 49 do
  begin
    adsData.Append;
    adsDataId.AsInteger := AId;
    adsDataName.AsString := ANames[Random(4)];
    adsDataParent.AsInteger := 3;
    adsDataPrice.AsCurrency := 1 + Random(100) / 10;
    adsDataSold.AsFloat := Random(100);
    adsDataPackable.AsBoolean := Boolean(Random(2));
    adsDataStatus.AsBoolean := Boolean(Random(2));
    adsData.Post;
    Inc(AId);
  end;
  // 动物：50
  for I := 0 to 49 do
  begin
    adsData.Append;
    adsDataId.AsInteger := AId;
    adsDataName.AsString := ANames[3 + Random(3)];
    adsDataParent.AsInteger := 4;
    adsDataPrice.AsCurrency := 1 + Random(100) / 10;
    adsDataSold.AsFloat := Random(100);
    adsDataPackable.AsBoolean := Boolean(Random(2));
    adsDataStatus.AsBoolean := Boolean(Random(2));
    adsData.Post;
    Inc(AId);
  end;
  // 水果
  for I := 0 to 49 do
  begin
    adsData.Append;
    adsDataId.AsInteger := AId;
    adsDataName.AsString := ANames[5 + Random(6)];
    adsDataParent.AsInteger := 2;
    adsDataPrice.AsCurrency := 1 + Random(100) / 10;
    adsDataSold.AsFloat := Random(100);
    adsDataPackable.AsBoolean := Boolean(Random(2));
    adsDataStatus.AsBoolean := Boolean(Random(2));
    adsData.Post;
    Inc(AId);
  end;
end;

procedure TForm1.DoGetCellData(Sender: TQVirtualTreeView; ANode: TQVTNode;
  ACol: Integer; var AData: IVTCellData);
begin
  adsData.RecNo := ANode.RowIndex + 1;
  AData := FCellDatas[ACol];
  with AData as TDBCellData do
  begin
    Node := ANode;
    Column := FTreeView.Header.Columns[ACol];
  end;
end;

procedure TForm1.DoHoverChanged(Sender: TObject);
begin
  if FTreeview.HoverNode <> nil then
    Caption := '当前行 ' + IntToStr(FTreeView.HoverNode.RowIndex) + ',列 ' +
      IntToStr(FTreeView.HoverColumn)
  else
    Caption := '无热点';
end;


procedure TForm1.FormCreate(Sender: TObject);
var
  I: Integer;
  AField: TField;
  ADrawerType: TQVTDrawerType;
begin
  FTreeView := TQVirtualTreeView.Create(Self);
  FTreeView.Parent := Self;
  FTreeView.Align := TAlignLayout.Client;
  FTreeView.Padding.Rect := RectF(5, 5, 5, 5);
  CreateDemoDataSet;
  FTreeView.Header.Columns.BeginUpdate;
  try
    SetLength(FCellDatas, adsData.Fields.Count - 2);
    for I := 0 to adsData.Fields.Count - 1 do
    begin
      AField := adsData.Fields[I];
      if (AField <> adsDataId) and (AField <> adsDataParent) then
      begin
        if AField = adsDataSold then
        begin
          FCellDatas[FTreeView.Header.Columns.Count] :=
            TDBProgressCellData.Create(AField);
          ADrawerType := TQVTDrawerType.dtProgress;
        end
        else if AField = adsDataPackable then
        begin
          FCellDatas[FTreeView.Header.Columns.Count] :=
            TDBCheckCellData.Create(AField);
          ADrawerType := TQVTDrawerType.dtCheck;
        end
        else if AField = adsDataStatus then
        begin
          FCellDatas[FTreeView.Header.Columns.Count] :=
            TDBRadioCellData.Create(AField);
          ADrawerType := TQVTDrawerType.dtRadio;
        end
        else
          FCellDatas[FTreeView.Header.Columns.Count] :=
            TDBTextCellData.Create(AField);
        with FTreeView.Header.Columns.Add do
        begin
          Title.Text := adsData.Fields[I].DisplayLabel;
          Width := 120;
          MinWidth := 10;
          DrawerType := ADrawerType;
        end;
        if AField = adsDataName then
          FTreeView.Header.MasterColumn := FTreeView.Header.Columns.Count - 1;
      end;
    end;
  finally
    FTreeView.Header.Columns.EndUpdate;
  end;
  FTreeView.PaintOptions := [TQVTPaintOption.poHorizLine,
    TQVTPaintOption.poVertLine, TQVTPaintOption.poTreeLine,
    TQVTPaintOption.poNodeButton, TQVTPaintOption.poRowSelection];
  FTreeView.OnGetCellData := DoGetCellData;
  FTreeView.OnHoverChanged := DoHoverChanged;
  FTreeView.RootNodeCount := adsData.RecordCount;
  FTreeView.Margins.Rect := RectF(5, 5, 5, 5);
  // FTreeview.Scale.Point:=PointF(2,2);
end;

{ TDBCellData }

constructor TDBCellData.Create(AField: TField);
begin
  inherited Create(nil, -1);
  FField := AField;
end;

{ TDBTextCellData }

function TDBTextCellData.GetText: String;
begin
  Result := FField.AsString;
end;

{ TDBProgressCellData }

constructor TDBProgressCellData.Create(AField: TField);
begin
  inherited;
  FFill := TBrush.Create(TBrushKind.Solid, TAlphaColors.Blue);
  FStroke := TStrokeBrush.Create(TBrushKind.Solid, TAlphaColors.LtGray);
  FTextSettings := TTextSettings.Create(Self);
  FTextSettings.FontColor := TAlphaColors.Wheat;
  FTextSettings.HorzAlign := TTextAlign.Center;
end;

destructor TDBProgressCellData.Destroy;
begin
  FreeAndNil(FTextSettings);
  inherited;
end;

function TDBProgressCellData.GetProgress: Single;
begin
  if FField.IsNull then
    Result := 0
  else
    Result := FField.AsFloat;
end;

function TDBProgressCellData.GetText: String;
begin
  if FField.IsNull then
    Result := ''
  else
    Result := FormatFloat('0.00', FField.AsFloat) + '%';
end;

function TDBProgressCellData.GetTextSettings: TTextSettings;
begin
  Result := FTextSettings;
end;

{ TDBCheckCellData }

constructor TDBCheckCellData.Create(AField: TField);
begin
  inherited;
  FStroke := TStrokeBrush.Create(TBrushKind.Solid, TAlphaColors.Black);
end;

function TDBCheckCellData.GetCheckLayout: TQVTLayout;
begin
  Result := TQVTLayout.clLeftCenter;
end;

function TDBCheckCellData.GetCheckStates: TQVTCheckStates;
begin
  if FField.IsNull then
    Result := []
  else if FField.AsBoolean then
    Result := [TQVTCheckState.csChecked, TQVTCheckState.csEnabled]
  else
    Result := [TQVTCheckState.csEnabled];
end;

function TDBCheckCellData.GetFollowStates: Boolean;
begin
  Result := True;
end;

function TDBCheckCellData.GetText: String;
begin
  Result := '';
end;

procedure TDBCheckCellData.SetCheckLayout(value: TQVTLayout);
begin

end;

procedure TDBCheckCellData.SetCheckStates(AStates: TQVTCheckStates);
begin

end;

procedure TDBCheckCellData.SetFollowStates(const value: Boolean);
begin

end;

{ TDBRadioCellData }

constructor TDBRadioCellData.Create(AField: TField);
begin
  inherited;
  FStroke := TStrokeBrush.Create(TBrushKind.Solid, TAlphaColors.Black);
end;

function TDBRadioCellData.GetCheckStates: TQVTCheckStates;
begin
  if FField.IsNull then
    Result := []
  else if FField.AsBoolean then
    Result := [TQVTCheckState.csChecked, TQVTCheckState.csEnabled]
  else
    Result := [TQVTCheckState.csEnabled];
end;

function TDBRadioCellData.GetGroupId: Integer;
begin
  Result := 0;
end;

function TDBRadioCellData.GetRadioLayout: TQVTLayout;
begin
  Result := TQVTLayout.clLeftCenter;
end;

function TDBRadioCellData.GetText: String;
begin
  Result := '';
end;

procedure TDBRadioCellData.SetCheckStates(AStates: TQVTCheckStates);
begin

end;

procedure TDBRadioCellData.SetGroupId(const AId: Integer);
begin

end;

procedure TDBRadioCellData.SetRadioLayout(value: TQVTLayout);
begin

end;

end.
