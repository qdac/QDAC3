unit main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.Generics.Defaults, FMX.Layouts,
  System.Generics.Collections, FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics,
  FMX.Dialogs, qjson,
  qdac_fmx_virtualtree, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit;

type
  TAreaType = (atUnknown, atCountry, atProvince, atCity, atCounty, atTownship);

  TAreaItem = class(TInterfacedObject)
  private
    [unsafe]
    FParent: TAreaItem;
    FItems: TList<TAreaItem>;
    FName: String;
    FAreaType: TAreaType;
    FId: Integer;
    FParentId: Integer;
    function GetCount: Integer;
    function GetItems(const AIndex: Integer): TAreaItem;
    function GetNamePath: String;
  public
    constructor Create; overload;
    destructor Destroy; override;
    function Add(AItem: TAreaItem): Integer;
    procedure Clear;
    procedure FromJson(AJson: TQJson);
    procedure Sort(ANest: Boolean);
    property Id: Integer read FId write FId;
    property Name: String read FName write FName;
    property NamePath: String read GetNamePath;
    property AreaType: TAreaType read FAreaType write FAreaType;
    property Count: Integer read GetCount;
    property Items[const AIndex: Integer]: TAreaItem read GetItems; default;
  end;

  TForm3 = class(TForm)
    QVirtualTreeView1: TQVirtualTreeView;
    QVTCustomTextCell1: TQVTCustomTextCell;
    Label1: TLabel;
    Timer1: TTimer;
    Edit1: TEdit;
    QVTCustomTextCell2: TQVTCustomTextCell;
    chkDefaultStateChars: TCheckBox;
    procedure QVTCustomTextCell1GetText(ASender: TObject; var AText: string);
    procedure QVirtualTreeView1InitNode(Sender: TQVirtualTreeView;
      ANode: TQVTNode);
    procedure FormCreate(Sender: TObject);
    procedure QVirtualTreeView1FocusChanged(Sender: TQVirtualTreeView;
      ANode: TQVTNode; ACol: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure QVirtualTreeView1Paint(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
    procedure Timer1Timer(Sender: TObject);
    procedure QVirtualTreeView1SortmarkerChanged(Sender: TObject);
    procedure QVirtualTreeView1GetNodeBackground(Sender: TQVirtualTreeView;
      ANode: TQVTNode; AFill: TBrush; var AHandled: Boolean);
    procedure QVirtualTreeView1GetCellBackground(Sender: TQVirtualTreeView;
      ANode: TQVTNode; AColumn: Integer; AFill: TBrush; var AHandled: Boolean);
    procedure chkDefaultStateCharsChange(Sender: TObject);
  private
    { Private declarations }
    FRootArea: TAreaItem;
    FFps: Integer;
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses System.ioutils;
{$R *.fmx}

procedure TForm3.chkDefaultStateCharsChange(Sender: TObject);
begin
  if chkDefaultStateChars.IsChecked then
  begin
    TQVTHeaderDrawer.AscChar := '^';
    TQVTHeaderDrawer.DescChar := 'v';
  end
  else
  begin
    TQVTHeaderDrawer.AscChar := '↑';
    TQVTHeaderDrawer.DescChar := '↓';
  end;
  QVirtualTreeView1.Invalidate;
end;

procedure TForm3.FormCreate(Sender: TObject);
var
  AJson: TQJson;
  AFileName: String;
begin
  FRootArea := TAreaItem.Create;
  AJson := TQJson.Create;
  try
{$IFDEF MSWINDOWS}
    if FileExists('areas.json') then
      AFileName := 'areas.json'
    else if FileExists('..\..\areas.json') then
      AFileName := '..\..\areas.json';
    AJson.LoadFromFile(AFileName);
{$ELSE}
    AFileName := TPath.Combine(TPath.GetDocumentsPath, 'areas.json');
{$ENDIF}
    if FileExists(AFileName) then
      AJson.LoadFromFile(AFileName);
    Label1.Text := TPath.GetFullPath(AFileName);
    FRootArea.FromJson(AJson);
  finally
    FreeAndNil(AJson);
  end;
  QVirtualTreeView1.RootNodeCount := FRootArea.Count;
  QVirtualTreeView1.Options := QVirtualTreeView1.Options + [toMultiSelection];
end;

procedure TForm3.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FRootArea);
end;

procedure TForm3.QVirtualTreeView1FocusChanged(Sender: TQVirtualTreeView;
  ANode: TQVTNode; ACol: Integer);
var
  AData: TAreaItem;
begin
  if Assigned(ANode) then
  begin
    AData := TAreaItem(ANode.ExtByType(TAreaItem));
    Caption := 'Area tree-' + AData.NamePath;
  end
  else
    Caption := 'Area tree';
end;

procedure TForm3.QVirtualTreeView1GetCellBackground(Sender: TQVirtualTreeView;
  ANode: TQVTNode; AColumn: Integer; AFill: TBrush; var AHandled: Boolean);
var
  ADrawer: IQVTTextDrawable;
begin
  if ANode.Index = AColumn then // 1/1,2/2,3/3 颜色设置为红色背景，注意我们的颜色优先，所以会覆盖选择或热点颜色
    AFill.Color := TAlphaColors.Red
  else
  begin
    if Supports(ANode.CellData[AColumn], IQVTTextDrawable, ADrawer) then
    begin
      if ANode.Index = 4 then // 修改第五行的字体颜色
        ADrawer.TextSettings.FontColor := TAlphaColors.Greenyellow
      else
        ADrawer.TextSettings.FontColor := TAlphaColors.Black;
    end;
    AHandled := false;
  end;
end;

procedure TForm3.QVirtualTreeView1GetNodeBackground(Sender: TQVirtualTreeView;
  ANode: TQVTNode; AFill: TBrush; var AHandled: Boolean);
begin
  if ((ANode.Index mod 2) = 1) then
    AFill.Color := TAlphaColors.Yellowgreen
  else
    AHandled := false;
end;

procedure TForm3.QVirtualTreeView1InitNode(Sender: TQVirtualTreeView;
  ANode: TQVTNode);
var
  AItem, AParent: TAreaItem;
begin
  if ANode.Parent = Sender.RootNode then
    AParent := FRootArea
  else
    AParent := TAreaItem(ANode.Parent.ExtByType(TAreaItem));
  if Sender.Header.Columns[0].Title.SortMarker = smDesc then
    AItem := AParent[AParent.Count - ANode.Index - 1]
  else
    AItem := AParent[ANode.Index];
  ANode.Exts.Add(AItem);
  ANode.ChildCount := AItem.Count;
  // 如果这里不能确定有多少个子结点，则可以设置States包含 nsHasChildren状态，然后在 OnInitChildren 中再计算实际的ChildCount进行赋值
end;

procedure TForm3.QVirtualTreeView1Paint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin
  Inc(FFps);
end;

procedure TForm3.QVirtualTreeView1SortmarkerChanged(Sender: TObject);
begin
  // Todo:加入排序的代码
  case QVirtualTreeView1.Header.Columns[0].Title.SortMarker of
    smAsc, smDesc:
      begin
        FRootArea.Sort(true);
        QVirtualTreeView1.RootNode.ReinitChildren(true);
      end;
  end;
end;

procedure TForm3.QVTCustomTextCell1GetText(ASender: TObject; var AText: string);
var
  AData: TAreaItem;
begin
  if QVTCustomTextCell1.Node.ExtByType(TAreaItem, AData) then
  begin
    case QVTCustomTextCell1.Column of
      0: // Name
        AText := AData.Name;
      1: // Code
        AText := IntToStr(AData.Id);
      2: // Type
        case AData.AreaType of
          atCountry:
            AText := '国家';
          atProvince:
            AText := '省份';
          atCity:
            AText := '地市';
          atCounty:
            AText := '区县';
          atTownship:
            AText := '乡镇'
        else
          AText := '';
        end;
    end;
  end;
end;

procedure TForm3.Timer1Timer(Sender: TObject);
begin
  Label1.Text := 'FPS:' + IntToStr(FFps);
  FFps := 0;
end;

{ TAreaItem }

function TAreaItem.Add(AItem: TAreaItem): Integer;
begin
  AItem.FParent := Self;
  if not Assigned(FItems) then
    FItems := TList<TAreaItem>.Create(TComparer<TAreaItem>.Construct(
      function(const T1, T2: TAreaItem): Integer
      begin
        Result := T1.Id - T2.Id;
      end));
  Result := FItems.Add(AItem);
  AItem._AddRef;
end;

procedure TAreaItem.Clear;
var
  I: Integer;
begin
  if Assigned(FItems) then
  begin
    for I := 0 to FItems.Count - 1 do
      FItems[I]._Release;
    FItems.Clear;
  end;
end;

constructor TAreaItem.Create;
begin
  inherited;
end;

destructor TAreaItem.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TAreaItem.FromJson(AJson: TQJson);
var
  I: Integer;
  AItem: TAreaItem;
  ADict: TDictionary<Integer, TAreaItem>;
  APair: TPair<Integer, TAreaItem>;
  AChild: TQJson;
begin
  ADict := TDictionary<Integer, TAreaItem>.Create;
  try
    for I := 0 to AJson.Count - 1 do
    begin
      AChild := AJson[I];
      AItem := TAreaItem.Create;
      AItem.FId := AChild.IntByName('id', -1);
      AItem.FName := AChild.ValueByName('name', '');
      AItem.FAreaType := TAreaType(AChild.IntByName('type', 0));
      AItem.FParentId := AChild.IntByName('parent_id', 0);
      ADict.Add(AItem.FId, AItem);
    end;
    for APair in ADict do
    begin
      if APair.Value.FParentId = 0 then
        Add(APair.Value)
      else if ADict.TryGetValue(APair.Value.FParentId, AItem) then
        AItem.Add(APair.Value);
    end;
    Sort(true);
  finally
    FreeAndNil(ADict);
  end;
end;

function TAreaItem.GetCount: Integer;
begin
  if Assigned(FItems) then
    Result := FItems.Count
  else
    Result := 0;
end;

function TAreaItem.GetItems(const AIndex: Integer): TAreaItem;
begin
  Result := FItems[AIndex];
end;

function TAreaItem.GetNamePath: String;
begin
  if Assigned(FParent) then
    Result := FParent.NamePath
  else
    Result := '';
  if Length(Result) = 0 then
    Result := FName
  else
    Result := Result + '.' + FName;
end;

procedure TAreaItem.Sort(ANest: Boolean);
var
  I: Integer;
begin
  if Assigned(FItems) and (FItems.Count > 0) then
  begin
    FItems.Sort;
    for I := 0 to FItems.Count - 1 do
      FItems[I].Sort(ANest);
  end;
end;

end.
