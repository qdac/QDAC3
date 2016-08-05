unit simplehtml;

interface

uses classes, sysutils, qstring;

type
  TQHtmlTag = class;

  TQHtmlAttr = class
  private
    FName, FValue: QStringW;
    function GetAsInteger: Integer;
    procedure SetAsInteger(const Value: Integer);
    function GetAsInt64: Int64;
    procedure SetAsInt64(const Value: Int64);
    function GetAsFloat: Extended;
    procedure SetAsFloat(const Value: Extended);
    function GetAsBoolean: Boolean;
    procedure SetAsBoolean(const Value: Boolean);
    function GetAsDateTime: TDateTime;
    procedure SetAsDateTime(const Value: TDateTime);
    procedure SetName(const Value: QStringW);
  public
    /// <summary>属性名称</summary>
    property Name: QStringW read FName write SetName;
    /// <summary>属性值</summary>
    property Value: QStringW read FValue write FValue;
    /// <summary>尝试按整形值来访问值内容</summary>
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    /// <summary>尝试以64位整数形式来访问值内容</summary>
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    /// <summary>尝试以浮点数的形式来访问值内容</summary>
    property AsFloat: Extended read GetAsFloat write SetAsFloat;
    /// <summary>尝试以字符串的形式来访问值内容（等价于访问Value)
    property AsString: QStringW read FValue write FValue;
    /// <summary>尝试以布尔的形式访问值内容
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    /// <summary>尝试以日期时间类型访问内容
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
  end;

{$IFDEF QDAC_UNICODE}

  TQHtmlAttrList = TList<TQHtmlAttr>;
  TQHtmlTagList = TList<TQHtmlTag>;
{$ELSE}
  TQHtmlAttrList = TList;
  TQHtmlTagList = TList;
{$ENDIF !QDAC_UNICODE}
  TQHtmlAttrEnumerator = class;

  /// <summary>XML属性列表，内部使用</summary>
  TQHtmlAttrs = class
  private
    FItems: TQHtmlAttrList;
    FOwner: TQHtmlTag;
    function GetCount: Integer;
    function GetItems(AIndex: Integer): TQHtmlAttr;
  public
    /// <summary>创建一个XML属性列表</summary>
    /// <param name="AOwner">所隶属的XML结点</param>
    constructor Create(AOwner: TQHtmlTag); overload;
    /// <summary>复制一个XML属性的内容</summary>
    procedure Assign(ASource: TQHtmlAttrs);
    /// <summary>添加一个XML属性</summary>
    /// <param name="AName">要添加的XML属性名称</param>
    /// <returns>返回添加的XML属性对象
    function Add(const AName: QStringW): TQHtmlAttr; overload;
    /// <summary>添加一个XML属性的值</summary>
    /// <param name="AName">属性名称</param>
    /// <param name="AValue">属性值</param>
    /// <returns>返回添加的XML属性的索引</returns>
    /// <remarks>QHtml不检查是否重复，如果不确定是否已存在，在添加前，请用
    /// ItemByName或IndexByName来检查是否存在属性
    function Add(const AName, AValue: QStringW): Integer; overload;
    /// <summary>查找指定名称的属性</summary>
    /// <param name="AName">要查找的属性名称</param>
    /// <returns>返回找到的属性对象</returns>
    function ItemByName(const AName: QStringW): TQHtmlAttr;
    /// <summary>获取指定名称的属性的索引号</summary>
    /// <param name="AName">要查找的属性名称</param>
    /// <returns>返回找到的属性的索引，如果未找到，返回-1</returns>
    function IndexOfName(const AName: QStringW): Integer;
    /// <summary>获取指定名称的属性的值</summary>
    /// <param name="AName">属性名称</param>
    /// <param name="ADefVal">如果属性不存在，返回的默认值</param>
    /// <returns>返回找到的属性的值，如果未找到，返回ADefVal参数的值</returns>
    function ValueByName(const AName: QStringW; const ADefVal: QStringW = '')
      : QStringW;
    /// <summary>删除指定索引的属性值</summary>
    /// <param name="AIndex">属性索引</param>
    procedure Delete(AIndex: Integer); overload;
    /// <summary>删除指定名称的属性值</summary>
    /// <param name="AName">属性名称</param>
    /// <remarks>如果有重名的属性，只会删除第一个找到的属性</remarks>
    procedure Delete(AName: QStringW); overload;
    /// <summary>清除所有的属性</summary>
    procedure Clear;
    /// <summary>析构函数</summary>
    destructor Destroy; override;
    /// <summary>for..in支持函数</summary>
    function GetEnumerator: TQHtmlAttrEnumerator;
    /// <summary>属性个数</summary>
    property Count: Integer read GetCount;
    /// <summary>属性列表</summary>
    property Items[AIndex: Integer]: TQHtmlAttr read GetItems; default;
    /// <summary>属性所有者结点</summary>
    property Owner: TQHtmlTag read FOwner;
  end;

  TQHtmlAttrEnumerator = class
  private
    FIndex: Integer;
    FList: TQHtmlAttrs;
  public
    constructor Create(AList: TQHtmlAttrs);
    function GetCurrent: TQHtmlAttr; inline;
    function MoveNext: Boolean;
    property Current: TQHtmlAttr read GetCurrent;
  end;

  TQHtmlTagEnumerator = class
  private
    FIndex: Integer;
    FList: TQHtmlTag;
  public
    constructor Create(AList: TQHtmlTag);
    function GetCurrent: TQHtmlTag; inline;
    function MoveNext: Boolean;
    property Current: TQHtmlTag read GetCurrent;
  end;

  /// <summary>HTML结点类型<summary>
  /// <list>
  /// <item><term>httNode</term><description>普通结点</description></item>
  /// <item><term>httText</term><description>文本内容</description></item>
  /// <item><term>httComment</term><description>注释</description></item>
  /// </list>
  TQHtmlTagType = (httNode, httText, httComment);

  TQHtmlTag = class
  private
    FTagType: TQHtmlTagType;
    function GetAsHtml: QStringW;
    function GetAttrs: TQHtmlAttrs;
    function GetCapacity: Integer;
    function GetCount: Integer;
    function GetItemIndex: Integer;
    function GetItems(AIndex: Integer): TQHtmlTag;
    function GetName: QStringW;
    function GetPath: QStringW;
    function GetText: QStringW;
    procedure SetAsHtml(const Value: QStringW);
    procedure SetCapacity(const Value: Integer);
    procedure SetName(const Value: QStringW);
    procedure SetTagType(const Value: TQHtmlTagType);
    procedure SetText(const Value: QStringW);
  protected
    FAttrs: TQHtmlAttrs;
    FItems: TQHtmlTagList;
    FParent: TQHtmlTag;
    FName: QStringW;
    FNameHash: Integer; // 名称的哈希值
    FData: Pointer;
    function CreateTag: TQHtmlTag; virtual;
    procedure FreeTag(ATag: TQHtmlTag); virtual;
  public
    /// <summary>构造函数</summary>
    constructor Create; overload;
    /// <summary>析构函数</summary>
    destructor Destroy; override;
    /// <summary>值拷贝函数</summary>
    procedure Assign(ANode: TQHtmlTag);
    /// <summary>添加一个未命名结点</summary>
    /// <remarks>如果您添加了一个未命名结点，保存时，该结点层级将被忽略而直接保存子一级</remarks>
    function Add: TQHtmlTag; overload;
    /// <summary>添加一个结点、文本、注释或CData</summary>
    /// <param name="AName_Text">名称或内容，具体取决于AType参数</param>
    /// <returns>返回添加的结点实例</returns>
    function Add(const AName_Text: QStringW; AType: TQHtmlTagType = httNode)
      : TQHtmlTag; overload;
    /// <summary>添加一个结点</summary>
    /// <param name="AName">结点名称</param>
    /// <returns>返回添加的结点实例</returns>
    /// <remarks>等价于调用Add(AName,xntNode)</remarks>
    function AddNode(const AName: QStringW): TQHtmlTag; virtual;
    /// <summary>添加一个文本结点</summary>
    /// <param name="AText">要添加的文本内容</param>
    /// <returns>返回添加的结点实例</returns>
    /// <remarks>等价于调用Add(AText,xntText)</remarks>
    function AddText(const AText: QStringW): TQHtmlTag;
    /// <summary>添加一个注释</summary>
    /// <param name="AText">要添加的注释内容，不能包含--&gt;</param>
    /// <returns>返回添加的结点实例</returns>
    /// <remarks>等价于调用Add(AText,xntComment)</remarks>
    function AddComment(const AText: QStringW): TQHtmlTag;
    /// <summary>获取指定名称的第一个结点</summary>
    /// <param name="AName">结点名称</param>
    /// <returns>返回找到的结点，如果未找到，返回空(NULL/nil)</returns>
    /// <remarks>注意XML允许重名，因此，如果存在重名的结点，只会返回第一个结点</remarks>
    function ItemByName(const AName: QStringW): TQHtmlTag; overload;
    /// <summary>获取指定名称的结点到列表中</summary>
    /// <param name="AName">结点名称</param>
    /// <param name="AList">用于保存结点的列表对象</param>
    /// <param name="ANest">是否递归查找子结点</param>
    /// <returns>返回找到的结点数量，如果未找到，返回0</returns>
    function ItemByName(const AName: QStringW; AList: TQHtmlTagList;
      ANest: Boolean = False): Integer; overload;
    /// <summary>获取指定路径的JSON对象</summary>
    /// <param name="APath">路径，以"."或"/"或"\"分隔</param>
    /// <returns>返回找到的子结点，如果未找到返回NULL(nil)</returns>
    function ItemByPath(const APath: QStringW): TQHtmlTag;
    /// <summary>获取符合指定名称规则的结点到列表中</summary>
    /// <param name="ARegex">正则表达式</param>
    /// <param name="AList">用于保存结点的列表对象</param>
    /// <param name="ANest">是否递归查找子结点</param>
    /// <returns>返回找到的结点数量，如果未找到，返回0</returns>
    function ItemByRegex(const ARegex: QStringW; AList: TQHtmlTagList;
      ANest: Boolean = False): Integer; overload;
    /// <summary>获取指定路径结点的文本内容</summary>
    /// <param name="APath">路径，以"."或"/"或"\"分隔</param>
    /// <param name="ADefVal">若路径不存在，返回的默认值</param>
    /// <returns>如果找到结点，返回找到的结点的文本内容，否则返回ADefVal参数的值</returns>
    function TextByPath(const APath, ADefVal: QStringW): QStringW;
    /// <summary>获取指定路径下的子结点中属性匹配指定值的结点</summary>
    /// <param name="APath">路径，以"."或"/"或"\"分隔</param>
    /// <param name="AttrName">要检查的属性名称</param>
    /// <param name="AttrValue">要检查的属性值</param>
    /// <returns>如果找到结点，返回找到的结点，否则返回空(nil/NULL)</returns>
    function ItemWithAttrValue(const APath: QStringW;
      const AttrName, AttrValue: QStringW): TQHtmlTag;
    /// <summary>获取指定路径结点的指定属性</summary>
    /// <param name="APath">路径，以"."或"/"或"\"分隔</param>
    /// <param name="AttrName">属性名称</param>
    /// <returns>如果找到结点相应的属性，返回找到的属性，否则返回NULL/nil</returns>
    /// <remarks>
    function AttrByPath(const APath, AttrName: QStringW): TQHtmlAttr;
    /// <summary>获取指定路径结点的指定属性值</summary>
    /// <param name="APath">路径，以"."或"/"或"\"分隔</param>
    /// <param name="AttrName">属性名称</param>
    /// <param name="ADefVal">若路径不存在，返回的默认值</param>
    /// <returns>如果找到结点相应的属性，返回找到的属性的文本内容，否则返回ADefVal参数的值</returns>
    /// <remarks>
    function AttrValueByPath(const APath, AttrName, ADefVal: QStringW)
      : QStringW;

    /// <summary>强制一个路径存在,如果不存在,则依次创建需要的结点</summary>
    /// <param name="APath">要添加的结点路径</param>
    /// <returns>返回路径对应的对象</returns>
    function ForcePath(APath: QStringW): TQHtmlTag;
    /// <summary>编码为字符串</summary>
    /// <param name="ADoFormat">是否格式化字符串，以增加可读性</param>
    /// <param name="AIndent">ADoFormat参数为True时，缩进内容，默认为两个空格</param>
    /// <returns>返回编码后的字符串</returns>
    /// <remarks>AsXML等价于Encode(True,'  ')</remarks>
    function Encode(ADoFormat: Boolean; AIndent: QStringW = '  '): QStringW;
    /// <summary>拷贝生成一个新的实例</summary>
    /// <returns>返回新的拷贝实例</returns>
    /// <remarks>因为是拷贝，所以新旧对象之间的内容变更没有任何关系，更改任意一个
    /// 对象，不会对另外一个对象造成影响。
    /// </remarks>
    function Copy: TQHtmlTag;
    /// <summary>克隆生成一个新的实例</summary>
    /// <returns>返回新的拷贝实例</returns>
    /// <remarks>因为实际上执行的是拷贝，所以新旧对象之间的内容变更没有任何关系，
    /// 更改任意一个对象，不会对另外一个对象造成影响，但此行为将来并不保证，可能
    /// 会调整为引用，以便相互影响。
    /// </remarks>
    function Clone: TQHtmlTag;
    /// <summary>删除指定索引的结点</summary>
    /// <param name="AIndex">要删除的结点索引</param>
    /// <remarks>
    /// 如果指定索引的结点不存在，则抛出EOutRange异常
    /// </remarks>
    procedure Delete(AIndex: Integer); overload; virtual;
    /// <summary>删除指定名称的结点</summary>
    /// <param name="AName">要删除的结点名称</param>
    /// <param name="ADeleteAll">是否删除全部同名的结点</param>
    procedure Delete(AName: QStringW; ADeleteAll: Boolean = True); overload;
    /// <summary>查找指定名称的结点的索引</summary>
    /// <param name="AName">要查找的结点名称</param>
    /// <returns>返回索引值，未找到返回-1</returns>
    function IndexOf(const AName: QStringW): Integer; virtual;
    /// <summary>清除所有的结点</summary>
    procedure Clear; virtual;
    /// <summary>解析指定的XML字符串</summary>
    /// <param name="p">要解析的字符串</param>
    /// <param name="l">字符串长度，<=0认为是以\0(#0)结尾的C语言标准字符串</param>
    /// <remarks>如果l>=0，会检测p[l]是否为\0，如果不为\0，则会创建拷贝实例并解析拷贝实例</remarks>
    procedure Parse(p: PQCharW; len: Integer = -1); overload;
    /// <summary>解析指定的JSON字符串</summary>
    /// <param name='s'>要解析的JSON字符串</param>
    procedure Parse(const s: QStringW); overload;
    /// <summary>从指定的文件中加载当前对象</summary>
    /// <param name="AFileName">要加载的文件名</param>
    /// <param name="AEncoding">源文件编码，如果为teUnknown，则自动判断</param>
    procedure LoadFromFile(AFileName: QStringW;
      AEncoding: TTextEncoding = teUnknown);
    /// <summary>从流的当前位置开始加载JSON对象</summary>
    /// <param name="AStream">源数据流</param>
    /// <param name="AEncoding">源文件编码，如果为teUnknown，则自动判断</param>
    /// <remarks>流的当前位置到结束的长度必需大于2字节，否则无意义</remarks>
    procedure LoadFromStream(AStream: TStream;
      AEncoding: TTextEncoding = teUnknown);
    /// <summary>保存当前对象内容到文件中</summary>
    /// <param name="AFileName">文件名</param>
    /// <param name="AEncoding">编码格式</param>
    /// <param name="AWriteBOM">是否写入UTF-8的BOM</param>
    /// <remarks>注意当前结点的名称不会被写入</remarks>
    procedure SaveToFile(AFileName: QStringW; AEncoding: TTextEncoding = teUTF8;
      AWriteBom: Boolean = False);
    /// <summary>保存当前对象内容到流中</summary>
    /// <param name="AStream">目标流对象</param>
    /// <param name="AEncoding">编码格式</param>
    /// <param name="AWriteBom">是否写入BOM</param>
    /// <remarks>注意当前结点的名称不会被写入</remarks>
    procedure SaveToStream(AStream: TStream; AEncoding: TTextEncoding = teUTF8;
      AWriteBom: Boolean = False);
    /// <summary>重载TObject.ToString函数</summary>
    function ToString: string; {$IFDEF QDAC_UNICODE}override; {$ENDIF}
    /// <summary>for..in支持函数</summary>
    function GetEnumerator: TQHtmlTagEnumerator;
    /// <summary>子结点数量</<summary>summary>
    property Count: Integer read GetCount;
    /// <summary>子结点数组</summary>
    property Items[AIndex: Integer]: TQHtmlTag read GetItems; default;
    /// <summary>额外的附加数据成员，供用户关联附加内容</summary>
    property Data: Pointer read FData write FData;
    /// <summary>结点的路径，中间以"\"分隔</summary>
    property Path: QStringW read GetPath;
    /// <summary>结点在父结点上的索引，如果自己是根结点，则返回-1</summary>
    property ItemIndex: Integer read GetItemIndex;
    /// <summary>父结点</summary>
    property Parent: TQHtmlTag read FParent write FParent;
  published
    /// <summary>结点名称</summary>
    property Name: QStringW read GetName write SetName;
    /// <summary>结点的纯文本内容（不含注释，只包含Text和CDATA)</summary>
    property Text: QStringW read GetText write SetText;
    /// <summary>结点类型</summary>
    property TagType: TQHtmlTagType read FTagType write SetTagType;
    /// <summary>属性列表</summary>
    property Attrs: TQHtmlAttrs read GetAttrs;
    /// <summary>列表容量</summary>
    property Capacity: Integer read GetCapacity write SetCapacity;
    /// <summary>返回XML格式的数据</summary>
    property AsHtml: QStringW read GetAsHtml write SetAsHtml;
  end;

implementation

resourcestring
  SValueNotNumeric = '字符串 %s 不是有效的数值。';
  SValueNotBoolean = '字符串 %s 不是有效的布尔值。';
  SValueNotDateTime = '字符串 %s 不是有效的日期时间值。';
  SBadTagName = '指定的Html标记(Tag)名称无效。';

function ValidTagName(const s: QStringW): Boolean;
begin
  // Not check now
  Result := True;
end;
{ TQHtmlAttr }

function TQHtmlAttr.GetAsBoolean: Boolean;
begin
  if not TryStrToBool(FValue, Result) then
  begin
    try
      Result := (AsInt64 <> 0);
    except
      raise Exception.Create(SValueNotBoolean);
    end;
  end;
end;

function TQHtmlAttr.GetAsDateTime: TDateTime;
begin
  if not ParseDateTime(PQCharW(FValue), Result) then
    Result := GetAsFloat;
end;

function TQHtmlAttr.GetAsFloat: Extended;
var
  p: PQCharW;
begin
  p := PQCharW(FValue);
  if not ParseNumeric(p, Result) then
    raise Exception.CreateFmt(SValueNotNumeric, [FValue]);
end;

function TQHtmlAttr.GetAsInt64: Int64;
begin
  Result := Trunc(AsFloat);
end;

function TQHtmlAttr.GetAsInteger: Integer;
begin
  Result := AsInt64;
end;

procedure TQHtmlAttr.SetAsBoolean(const Value: Boolean);
begin
  FValue := BoolToStr(Value, True);
end;

procedure TQHtmlAttr.SetAsDateTime(const Value: TDateTime);
begin
  FValue := FloatToStr(Value);
end;

procedure TQHtmlAttr.SetAsFloat(const Value: Extended);
begin
  FValue := FloatToStr(Value);
end;

procedure TQHtmlAttr.SetAsInt64(const Value: Int64);
begin
  FValue := IntToStr(Value);
end;

procedure TQHtmlAttr.SetAsInteger(const Value: Integer);
begin
  SetAsInt64(Value);
end;

procedure TQHtmlAttr.SetName(const Value: QStringW);
begin
  if FName <> Value then
    FName := Value;
end;

{ TQHtmlAttrs }

function TQHtmlAttrs.Add(const AName, AValue: QStringW): Integer;
var
  Attr: TQHtmlAttr;
begin
  if ValidTagName(AName) then
  begin
    Attr := TQHtmlAttr.Create;
    Attr.FName := AName;
    Attr.FValue := AValue;
    Result := FItems.Add(Attr);
  end
  else
    raise Exception.Create(SBadTagName);
end;

function TQHtmlAttrs.Add(const AName: QStringW): TQHtmlAttr;
begin
  if ValidTagName(AName) then
  begin
    Result := TQHtmlAttr.Create;
    Result.FName := AName;
    FItems.Add(Result);
  end
  else
    raise Exception.Create(SBadTagName);
end;

procedure TQHtmlAttrs.Assign(ASource: TQHtmlAttrs);
var
  I: Integer;
  Attr, ASrc: TQHtmlAttr;
begin
  Clear;
  if (ASource <> nil) and (ASource.Count > 0) then
  begin
    for I := 0 to ASource.Count - 1 do
    begin
      ASrc := ASource[I];
      Attr := TQHtmlAttr.Create;
      Attr.FName := ASrc.FName;
      Attr.FValue := ASrc.FValue;
      FItems.Add(Attr);
    end;
  end;
end;

procedure TQHtmlAttrs.Clear;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    FreeObject(Items[I]);
  FItems.Clear;
end;

constructor TQHtmlAttrs.Create(AOwner: TQHtmlTag);
begin
  inherited Create;
  FOwner := AOwner;
  FItems := TQHtmlAttrList.Create;
end;

procedure TQHtmlAttrs.Delete(AName: QStringW);
var
  AIndex: Integer;
begin
  AIndex := IndexOfName(AName);
  if AIndex <> -1 then
    Delete(AIndex);
end;

procedure TQHtmlAttrs.Delete(AIndex: Integer);
begin
  FreeObject(Items[AIndex]);
  FItems.Delete(AIndex);
end;

destructor TQHtmlAttrs.Destroy;
begin
  Clear;
  FreeObject(FItems);
  inherited;
end;

function TQHtmlAttrs.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TQHtmlAttrs.GetEnumerator: TQHtmlAttrEnumerator;
begin
  Result := TQHtmlAttrEnumerator.Create(Self);
end;

function TQHtmlAttrs.GetItems(AIndex: Integer): TQHtmlAttr;
begin
  Result := FItems[AIndex];
end;

function TQHtmlAttrs.IndexOfName(const AName: QStringW): Integer;
var
  I, L: Integer;
  AItem: TQHtmlAttr;
begin
  Result := -1;
  L := Length(AName);
  for I := 0 to Count - 1 do
  begin
    AItem := Items[I];
    if Length(AItem.FName) = L then // Html名称不区分大小写
    begin
      if StartWithW(PQCharW(AItem.FName), PQCharW(AName), True) then
      begin
        Result := I;
        Break;
      end;
    end;
  end;
end;

function TQHtmlAttrs.ItemByName(const AName: QStringW): TQHtmlAttr;
var
  I: Integer;
begin
  Result := nil;
  I := IndexOfName(AName);
  if I <> -1 then
    Result := Items[I];
end;

function TQHtmlAttrs.ValueByName(const AName, ADefVal: QStringW): QStringW;
var
  I: Integer;
begin
  I := IndexOfName(AName);
  if I <> -1 then
    Result := Items[I].FValue
  else
    Result := ADefVal;
end;

{ TQHtmlAttrEnumerator }

constructor TQHtmlAttrEnumerator.Create(AList: TQHtmlAttrs);
begin
  FList := AList;
  FIndex := -1;
  inherited Create;
end;

function TQHtmlAttrEnumerator.GetCurrent: TQHtmlAttr;
begin
  Result := FList[FIndex];
end;

function TQHtmlAttrEnumerator.MoveNext: Boolean;
begin
  if FIndex < FList.Count - 1 then
  begin
    Inc(FIndex);
    Result := True;
  end
  else
    Result := False;
end;

{ TQHtmlTagEnumerator }

constructor TQHtmlTagEnumerator.Create(AList: TQHtmlTag);
begin
  inherited Create;
  FList := AList;
  FIndex := -1;
end;

function TQHtmlTagEnumerator.GetCurrent: TQHtmlTag;
begin
  Result := FList[FIndex];
end;

function TQHtmlTagEnumerator.MoveNext: Boolean;
begin
  if FIndex + 1 < FList.Count then
  begin
    Inc(FIndex);
    Result := True;
  end
  else
    Result := False;
end;

{ TQHtmlTag }

function TQHtmlTag.Add: TQHtmlTag;
begin
  Result := CreateTag;
  Result.FParent := Self;
  if not Assigned(FItems) then
    FItems := TQHtmlTagList.Create;
  FItems.Add(Result);
end;

function TQHtmlTag.Add(const AName_Text: QStringW; AType: TQHtmlTagType)
  : TQHtmlTag;
begin
  if AType = httNode then
    Result := AddNode(AName_Text)
  else
  begin
    Result := Add;
    Result.FTagType := AType;
    Result.Text := AName_Text;
  end;
end;

function TQHtmlTag.AddComment(const AText: QStringW): TQHtmlTag;
begin
  Result := Add(AText, httComment);
end;

function TQHtmlTag.AddNode(const AName: QStringW): TQHtmlTag;
begin
  if ValidTagName(AName) then
  begin
    Result := Add;
    Result.FTagType := httNode;
    Result.FName := AName;
  end;
end;

function TQHtmlTag.AddText(const AText: QStringW): TQHtmlTag;
begin
  Result := Add(AText, httText);
end;

procedure TQHtmlTag.Assign(ANode: TQHtmlTag);
var
  I: Integer;
begin
  FName := ANode.FName;
  FTagType := ANode.TagType;
  Clear;
  if Assigned(ANode.FAttrs) then
    Attrs.Assign(ANode.Attrs);
  for I := 0 to ANode.Count - 1 do
    Add.Assign(ANode.Items[I]);
end;

function TQHtmlTag.AttrByPath(const APath, AttrName: QStringW): TQHtmlAttr;
var
  ANode: TQHtmlTag;
begin
  ANode := ItemByPath(APath);
  if Assigned(ANode) then
    Result := ANode.Attrs.ItemByName(AttrName)
  else
    Result := nil;
end;

function TQHtmlTag.AttrValueByPath(const APath, AttrName, ADefVal: QStringW)
  : QStringW;
var
  Attr: TQHtmlAttr;
begin
  Attr := AttrByPath(APath, AttrName);
  if Assigned(Attr) then
    Result := Attr.Value
  else
    Result := ADefVal;
end;

procedure TQHtmlTag.Clear;
var
  I: Integer;
begin
  if Assigned(FItems) then
  begin
    for I := 0 to FItems.Count - 1 do
      FreeTag(Items[I]);
    FItems.Clear;
  end;
  if Assigned(FAttrs) then
    FAttrs.Clear;
end;

function TQHtmlTag.Clone: TQHtmlTag;
begin
  Result := Copy;
end;

function TQHtmlTag.Copy: TQHtmlTag;
begin
  Result := CreateTag;
  Result.Assign(Self);
end;

constructor TQHtmlTag.Create;
begin
  inherited;
end;

function TQHtmlTag.CreateTag: TQHtmlTag;
begin
  // if Assigned(OnQHtmlTagCreate) then
  // Result:=OnQHtmlTagCreate
  // else
  Result := TQHtmlTag.Create;
end;

procedure TQHtmlTag.Delete(AIndex: Integer);
begin
  if Assigned(FItems) then
  begin
    FreeTag(Items[AIndex]);
    FItems.Delete(AIndex);
  end;
end;

procedure TQHtmlTag.Delete(AName: QStringW; ADeleteAll: Boolean);
var
  I: Integer;
begin
  I := 0;
  while I < Count do
  begin
    if Items[I].FName = AName then
    begin
      Delete(I);
      if not ADeleteAll then
        Break;
    end
    else
      Inc(I);
  end;
end;

destructor TQHtmlTag.Destroy;
begin
  if Assigned(FItems) then
  begin
    Clear;
    FreeObject(FItems);
  end;
  if Assigned(FAttrs) then
    FreeObject(FAttrs);
  inherited;
end;

function TQHtmlTag.Encode(ADoFormat: Boolean; AIndent: QStringW): QStringW;
begin
end;

function TQHtmlTag.ForcePath(APath: QStringW): TQHtmlTag;
var
  AName: QStringW;
  p: PQCharW;
  AParent: TQHtmlTag;
const
  PathDelimiters: PWideChar = './\';
begin
  p := PQCharW(APath);
  AParent := Self;
  Result := Self;
  while p^ <> #0 do
  begin
    AName := DecodeTokenW(p, PathDelimiters, WideChar(0), True);
    Result := AParent.ItemByName(AName);
    if not Assigned(Result) then
      Result := AParent.Add(AName);
    AParent := Result;
  end;
end;

procedure TQHtmlTag.FreeTag(ATag: TQHtmlTag);
begin
  // if Assigned(FOnQHtmlFreeTag) then
  // FOnQHtmlFreeTag(ATag);
  // else
  FreeObject(ATag);
end;

function TQHtmlTag.GetAsHtml: QStringW;
begin
  Result := Encode(True);
end;

function TQHtmlTag.GetAttrs: TQHtmlAttrs;
begin
  if not Assigned(FAttrs) then
    FAttrs := TQHtmlAttrs.Create(Self);
  Result := FAttrs;
end;

function TQHtmlTag.GetCapacity: Integer;
begin
  if Assigned(FItems) then
    Result := FItems.Capacity
  else
    Result := 0;
end;

function TQHtmlTag.GetCount: Integer;
begin
  if Assigned(FItems) then
    Result := FItems.Count
  else
    Result := 0;
end;

function TQHtmlTag.GetEnumerator: TQHtmlTagEnumerator;
begin
  Result := TQHtmlTagEnumerator.Create(Self);
end;

function TQHtmlTag.GetItemIndex: Integer;
var
  I: Integer;
begin
  Result := -1;
  if Assigned(FParent) then
  begin
    for I := 0 to FParent.Count - 1 do
    begin
      if FParent.Items[I] = Self then
      begin
        Result := I;
        Break;
      end;
    end;
  end;
end;

function TQHtmlTag.GetItems(AIndex: Integer): TQHtmlTag;
begin
  Result := FItems[AIndex];
end;

function TQHtmlTag.GetName: QStringW;
begin
  if TagType = httNode then
    Result := FName
  else
    SetLength(Result, 0);
end;

function TQHtmlTag.GetPath: QStringW;
var
  AParent: TQHtmlTag;
begin
  Result := Name;
  AParent := FParent;
  while Assigned(AParent) do
  begin
    if Length(AParent.Name) > 0 then
      Result := AParent.Name + '\' + Result;
    AParent := AParent.FParent;
  end;
end;

function TQHtmlTag.GetText: QStringW;
var
  ABuilder: TQStringCatHelperW;
  procedure InternalGetText(ANode: TQHtmlTag);
  var
    I: Integer;
  begin
    if ANode.TagType = httNode then
    begin
      for I := 0 to ANode.Count - 1 do
        InternalGetText(ANode.Items[I]);
    end
    else // if ANode.NodeType<>xntComment then //注释不包含在Text中，文本或CDATA数据返回
      ABuilder.Cat(ANode.FName);
  end;

begin
  ABuilder := TQStringCatHelperW.Create;
  try
    InternalGetText(Self);
    Result := ABuilder.Value;
  finally
    ABuilder.Free;
  end;
end;

function TQHtmlTag.IndexOf(const AName: QStringW): Integer;
var
  AIndex: Integer;
begin
  AIndex := IndexOf(AName);
  if AIndex <> -1 then
    Result := Items[AIndex]
  else
    Result := nil;
end;

function TQHtmlTag.ItemByName(const AName: QStringW; AList: TQHtmlTagList;
  ANest: Boolean): Integer;
var
  ANode: TQHtmlTag;
  function InternalFind(AParent: TQHtmlTag): Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 0 to AParent.Count - 1 do
    begin
      ANode := AParent.Items[I];
      if ANode.Name = AName then
      begin
        AList.Add(ANode);
        Inc(Result);
      end;
      if ANest then
        Inc(Result, InternalFind(ANode));
    end;
  end;

begin
  Result := InternalFind(Self);
end;

function TQHtmlTag.ItemByName(const AName: QStringW): TQHtmlTag;
begin

end;

function TQHtmlTag.ItemByPath(const APath: QStringW): TQHtmlTag;
var
  AName: QStringW;
  pPath: PQCharW;
  AParent, AItem: TQHtmlTag;
const
  PathDelimiters: PWideChar = '/\.';
begin
  if Length(APath) > 0 then
  begin
    pPath := PQCharW(APath);
    AParent := Self;
    AItem := nil;
    while pPath^ <> #0 do
    begin
      AName := DecodeTokenW(pPath, PathDelimiters, WideChar(0), False);
      AItem := AParent.ItemByName(AName);
      if Assigned(AItem) then
        AParent := AItem
      else
        Break;
    end;
    if AParent = AItem then
      Result := AParent
    else
      Result := nil;
  end
  else
    Result := Self;
end;

function TQHtmlTag.ItemByRegex(const ARegex: QStringW; AList: TQHtmlTagList;
  ANest: Boolean): Integer;
begin

end;

function TQHtmlTag.ItemWithAttrValue(const APath, AttrName, AttrValue: QStringW)
  : TQHtmlTag;
var
  ANode: TQHtmlTag;
  I: Integer;
  Attr: TQHtmlAttr;
  AFound: Boolean;
begin
  Result := nil;
  ANode := ItemByPath(APath);
  if Assigned(ANode) then
  begin
    I := ANode.ItemIndex;
    while I < ANode.Parent.Count do
    begin
      ANode := ANode.Parent[I];
      Attr := ANode.Attrs.ItemByName(AttrName);
      if Attr <> nil then
      begin
        if Length(Attr.Value) = Length(AttrValue) then
        begin
          AFound := StartWithW(PQCharW(Attr.Value), PQCharW(AttrValue), True);
          if AFound then
          begin
            Result := ANode;
            Break;
          end;
        end;
      end;
      Inc(I);
    end;
  end;
end;

procedure TQHtmlTag.LoadFromFile(AFileName: QStringW; AEncoding: TTextEncoding);
var
  AStream: TFileStream;
begin
  AStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(AStream);
  finally
    FreeObject(AStream);
  end;
end;

procedure TQHtmlTag.LoadFromStream(AStream: TStream; AEncoding: TTextEncoding);
begin
  Parse(LoadTextW(AStream));
end;

procedure TQHtmlTag.Parse(const s: QStringW);
begin

end;

procedure TQHtmlTag.Parse(p: PQCharW; len: Integer);
begin

end;

procedure TQHtmlTag.SaveToFile(AFileName: QStringW; AEncoding: TTextEncoding;
  AWriteBom: Boolean);
begin

end;

procedure TQHtmlTag.SaveToStream(AStream: TStream; AEncoding: TTextEncoding;
  AWriteBom: Boolean);
begin

end;

procedure TQHtmlTag.SetAsHtml(const Value: QStringW);
begin

end;

procedure TQHtmlTag.SetCapacity(const Value: Integer);
begin

end;

procedure TQHtmlTag.SetName(const Value: QStringW);
begin

end;

procedure TQHtmlTag.SetTagType(const Value: TQHtmlTagType);
begin
  FTagType := Value;
end;

procedure TQHtmlTag.SetText(const Value: QStringW);
begin

end;

function TQHtmlTag.TextByPath(const APath, ADefVal: QStringW): QStringW;
begin

end;

function TQHtmlTag.ToString: string;
begin

end;

end.
