unit qdac.htmlrender;

interface

uses classes, sysutils, types, uitypes, qdac.htmlparser, qstring;

type
  TQHtmlRender = class;
  TQHtmlStyle = class;
  {
    TQHtmlPosition
    hpStatic：
    对象遵循常规流。此时4个定位偏移属性不会被应用。
    hpRelative：
    对象遵循常规流，并且参照自身在常规流中的位置通过top，right，bottom，left这4个定位偏移属性进行偏移时不会影响常规流中的任何元素。
    hpAbsolute：
    对象脱离常规流，此时偏移属性参照的是离自身最近的定位祖先元素，如果没有定位的祖先元素，则一直回溯到body元素。盒子的偏移位置不影响常规流中的任何元素，其margin不与其他任何margin折叠。
    hpFixed：
    与absolute一致，但偏移定位是以窗口为参考。当出现滚动条时，对象不会随着滚动。
    hpCenter：
    与absolute一致，但偏移定位是以定位祖先元素的中心点为参考。盒子在其包含容器垂直水平居中。（CSS3）
    hpPage：
    与absolute一致。元素在分页媒体或者区域块内，元素的包含块始终是初始包含块，否则取决于每个absolute模式。（CSS3）
    hpSticky：
    对象在常态时遵循常规流。它就像是relative和fixed的合体，当在屏幕中时按常规流排版，当卷动到屏幕外时则表现如fixed。该属性的表现是现实中你见到的吸附效果。（CSS3）
  }
  TQHtmlPosition = (hpStatic, hpRelative, hpAbsolute, hpFixed, hpCenter, hpPage,
    hpSticky);
  TQHtmlStyle = class
  private
    FHtmlTag: TQHTMLTag;
    FParent: TQHtmlStyle;
    FItems: TStringList;
    FName: QStringW;
    FPosition: TQHtmlPosition;
    FZOrder: Integer;
    function GetCount: Integer;
    function GetItems(AIndex: Integer): TQHtmlStyle;
  public
    constructor Create(AStyleName: QStringW); virtual;
    destructor Destroy; override;
    function Add(const AStyleName: QStringW): TQHtmlStyle;
    procedure Delete(AIndex: Integer);
    procedure Clear;
    function Find(const AStyleName: QStringW): TQHtmlStyle;
    function CalcBounds(AMaxBounds: TRect; AStartPoint: TPoint): TRect;
      virtual; abstract;
    procedure Draw(AMaxBounds: TRect; AStartPoint: TPoint); virtual; abstract;
    property HtmlTag: TQHTMLTag read FHtmlTag write FHtmlTag;
    property Parent: TQHtmlStyle read FParent; // 父样式
    property Name: QStringW read FName;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TQHtmlStyle read GetItems;
  end;

  // HTML 元素的渲染是一个逐级渲染的体系
  TQHtmlRenderItem = class
  protected
    FHtmlTag: TQHTMLTag;
    FRender: TQHtmlStyle;
    FItems: TList;
  public
    constructor Create; overload;
    destructor Destroy; override;
    function Add(const AClassName: QStringW): TQHTMLTag;
    procedure Delete(AIndex: Integer);
    procedure Clear;
    procedure Invalidate; virtual;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TQHtmlRender read GetItems; default;
  end;

  TQHtmlRender = class
  protected
    FStyles: TStringList; // 定义的样式列表索引
    FRootTag: TQHTMLTag;
    function GetCount: Integer;
    function GetItems(AIndex: Integer): TQHtmlRender;
  public
    function Add(const AClassName: QStringW): TQHTMLTag;
    procedure Delete(AIndex: Integer);
    procedure Clear;
    procedure Invalidate; virtual;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TQHtmlRender read GetItems; default;
  end;

implementation

{ TQHtmlStyle }

function TQHtmlStyle.Add(const AStyleName: QStringW): TQHtmlStyle;
begin
  Result := Find(AStyleName);
  if not Assigned(Result) then
  begin
    Result := TQHtmlStyle.Create(AStyleName);
    FItems.AddObject(Result.Name, Result);
  end;
end;

procedure TQHtmlStyle.Clear;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    FreeObject(FItems.Objects[I]);
  FItems.Clear;
end;

constructor TQHtmlStyle.Create(AStyleName: QStringW);
begin
  inherited Create;
  FName := AStyleName;
  FItems := TStringList.Create;
  FItems.Sorted := True;
  FItems.Duplicates := dupIgnore;
end;

procedure TQHtmlStyle.Delete(AIndex: Integer);
begin
  FreeObject(Items[AIndex]);
  FItems.Delete(AIndex);
end;

destructor TQHtmlStyle.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited;
end;

function TQHtmlStyle.Find(const AStyleName: QStringW): TQHtmlStyle;
var
  AIndex: Integer;
begin
  if FItems.Find(AStyleName, AIndex) then
    Result := Items[AIndex]
  else
    Result := nil;
end;

function TQHtmlStyle.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TQHtmlStyle.GetItems(AIndex: Integer): TQHtmlStyle;
begin
  Result := FItems.Objects[AIndex] as TQHtmlStyle;
end;

end.
