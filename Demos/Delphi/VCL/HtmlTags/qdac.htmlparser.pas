unit qdac.htmlparser;

interface

uses classes, sysutils, qstring, qvalue {$IFDEF UNICODE},
  Generics.Collections{$ENDIF};
{$I QDAC.INC}

{ 本单元是一个简单的HTML解析单元，不支持复杂的包含脚本的内容的解析，只是简单的解析
  HTML 的标签和属性
}
type
  TQHTMLTag = class;

  TQTagAttr = class
  private
    FValue: TQValue;
  protected
  public
    constructor Create; overload;
    destructor Destroy; override;
    property Value: TQValue read FValue write FValue;
  end;

  TQTagAttributes = class
  private
    FItems: TStringList;
    function GetCount: Integer;
    function GetItems(AIndex: Integer): TQTagAttr;
    function GetNames(AIndex: Integer): QStringW;
    function GetValues(AIndex: Integer): PQValue;
    property Items[AIndex: Integer]: TQTagAttr read GetItems;
  public
    constructor Create(AOwner: TQHTMLTag); overload;
    destructor Destroy; override;
    function Add(const AName: QStringW): PQValue;
    procedure Delete(AIndex: Integer);
    procedure Clear;
    function ValueByName(const AName: QStringW): PQValue;
    procedure BeginUpdate;
    procedure EndUpdate;
    property Count: Integer read GetCount;
    property Names[AIndex: Integer]: QStringW read GetNames;
    property Values[AIndex: Integer]: PQValue read GetValues;
  end;
{$IF RTLVersion>=21}

  TQTagItems = TList<TQHTMLTag>;
{$ELSE}
  TQTagItems = TList;
{$IFEND}

  TQHTMLTag = class
  private
    FName: QStringW;
    FInnerHtml: QStringW;
    FAttrs: TQTagAttributes;
    FItems: TQTagItems;
    FParent: TQHTMLTag;
    function GetCount: Integer;
    function GetItems(AIndex: Integer): TQHTMLTag;
    function GetAttrValue(AName: QStringW): PQValue;
    function GetInnerHtml: QStringW;
  public
    constructor Create; overload;
    destructor Destroy; override;
    function Add(const AName: QStringW): TQHTMLTag;
    procedure Delete(AIndex: Integer);
    procedure Clear;
    function TryParse(var s: PQCharW; l: Integer): Boolean;
    procedure Parse(s: QStringW);
    function IsChildOf(ATag: TQHTMLTag): Boolean;
    function IsParentOf(ATag: TQHTMLTag): Boolean;
    property Name: QStringW read FName write FName;
    property InnerHtml: QStringW read GetInnerHtml write FInnerHtml;
    property Attrs: TQTagAttributes read FAttrs;
    property Parent: TQHTMLTag read FParent;
    property Items[AIndex: Integer]: TQHTMLTag read GetItems; default;
    property Count: Integer read GetCount;
    property AttrValue[AName: QStringW]: PQValue read GetAttrValue;
  end;

implementation

resourcestring
  SBadHtmlText = '不受支持的 HTML 文本格式';
  { TQTagAttribute }

constructor TQTagAttr.Create;
begin
  FValue.TypeNeeded(vdtString); // 默认为字符串类型
end;

destructor TQTagAttr.Destroy;
begin
  FValue.Reset;
  inherited;
end;

{ TQTagAttributes }

function TQTagAttributes.Add(const AName: QStringW): PQValue;
var
  Attr: TQTagAttr;
begin
  Result := ValueByName(AName);
  if not Assigned(Result) then
  begin
    Attr := TQTagAttr.Create;
    FItems.AddObject(AName, Attr);
    Result := @Attr.FValue;
  end;
end;

procedure TQTagAttributes.BeginUpdate;
begin
  FItems.BeginUpdate;
end;

procedure TQTagAttributes.Clear;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    FreeObject(FItems.Objects[I]);
  FItems.Clear;
end;

constructor TQTagAttributes.Create(AOwner: TQHTMLTag);
begin
  inherited Create;
  FItems := TStringList.Create;
  FItems.Sorted := True;
  FItems.Duplicates := dupIgnore;
  FItems.CaseSensitive := False; // HTML 不区分大小写
end;

procedure TQTagAttributes.Delete(AIndex: Integer);
begin
  FreeObject(FItems.Objects[AIndex]);
  FItems.Delete(AIndex);
end;

destructor TQTagAttributes.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited;
end;

procedure TQTagAttributes.EndUpdate;
begin
  FItems.EndUpdate;
end;

function TQTagAttributes.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TQTagAttributes.GetItems(AIndex: Integer): TQTagAttr;
begin
  Result := FItems.Objects[AIndex] as TQTagAttr;
end;

function TQTagAttributes.GetNames(AIndex: Integer): QStringW;
begin
  Result := FItems[AIndex];
end;

function TQTagAttributes.GetValues(AIndex: Integer): PQValue;
begin
  Result := @Items[AIndex].FValue;
end;

function TQTagAttributes.ValueByName(const AName: QStringW): PQValue;
var
  AIndex: Integer;
begin
  if FItems.Find(AName, AIndex) then
    Result := Values[AIndex]
  else
    Result := nil;
end;

{ TQHTMLTag }

function TQHTMLTag.Add(const AName: QStringW): TQHTMLTag;
begin
  Result := TQHTMLTag.Create;
  Result.FParent := Self;
  Result.FName := AName;
  SetLength(FInnerHtml, 0);
  FItems.Add(Result);
end;

procedure TQHTMLTag.Clear;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    FreeObject(FItems[I]);
  FItems.Clear;
  SetLength(FInnerHtml, 0);
end;

constructor TQHTMLTag.Create;
begin
  inherited Create;
  FAttrs := TQTagAttributes.Create(Self);
  FItems := TQTagItems.Create;
end;

procedure TQHTMLTag.Delete(AIndex: Integer);
begin
  FreeObject(FItems[AIndex]);
  FItems.Delete(AIndex);
  SetLength(FInnerHtml, 0);
end;

destructor TQHTMLTag.Destroy;
begin
  FreeAndNil(FAttrs);
  Clear;
  inherited;
end;

function TQHTMLTag.GetAttrValue(AName: QStringW): PQValue;
begin
  Result := Attrs.ValueByName(AName);
  if not Assigned(Result) then // 如果没有指定值，则递归找父结点的值
  begin
    if Assigned(FParent) then
      Result := FParent.AttrValue[AName];
  end;
end;

function TQHTMLTag.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TQHTMLTag.GetInnerHtml: QStringW;
var
  AHelper: TQStringCatHelperW;
  procedure BuildTag(ATag: TQHTMLTag);
  const
    HtmlTagStart: QStringW = '<';
    HtmlTagEnd: QStringW = '>';
    HtmlTagClose: QStringW = '</';
    HtmlTagEndClose: QStringW = '/>';
    SpaceChar: QCharW = ' ';
    EqualChar: QCharW = '=';
    DblQuoter: QCharW = '"';
  var
    I: Integer;
  begin
    if Length(ATag.Name) > 0 then
    begin
      AHelper.Cat(HtmlTagStart).Cat(ATag.Name);
      if ATag.Attrs.Count > 0 then
      begin
        for I := 0 to ATag.Attrs.Count - 1 do
          AHelper.Cat(SpaceChar).Cat(ATag.Attrs.Names[I]).Cat(EqualChar)
            .Cat(DblQuoter).Cat(HtmlEscape(ATag.Attrs.Values[I].AsString))
            .Cat(DblQuoter);
      end;
      if (ATag.Count = 0) and (Length(ATag.FInnerHtml) = 0) then
      begin
        AHelper.Cat(HtmlTagEndClose).Cat(SLineBreak);
        Exit;
      end
      else
        AHelper.Cat(HtmlTagEnd).Cat(SLineBreak);
    end;
    if Length(ATag.FInnerHtml) > 0 then
      AHelper.Cat(ATag.FInnerHtml).Cat(SLineBreak)
    else
    begin
      for I := 0 to ATag.Count - 1 do
        BuildTag(ATag[I]);
    end;
    if Length(ATag.Name) > 0 then
      AHelper.Cat(HtmlTagClose).Cat(ATag.Name).Cat(HtmlTagEnd).Cat(SLineBreak);
  end;

begin
  if Length(FInnerHtml) = 0 then
  begin
    AHelper := TQStringCatHelperW.Create;
    try
      BuildTag(Self);
      AHelper.Back(Length(SLineBreak));
      FInnerHtml := AHelper.Value;
    finally
      FreeAndNil(AHelper);
    end;
  end;
  Result := FInnerHtml;
end;

function TQHTMLTag.GetItems(AIndex: Integer): TQHTMLTag;
begin
  Result := FItems[AIndex];
end;

function TQHTMLTag.IsChildOf(ATag: TQHTMLTag): Boolean;
var
  AParent: TQHTMLTag;
begin
  AParent := FParent;
  Result := False;
  while Assigned(AParent) do
  begin
    if AParent = ATag then
    begin
      Result := True;
      Break;
    end
    else
      AParent := AParent.Parent;
  end;
end;

function TQHTMLTag.IsParentOf(ATag: TQHTMLTag): Boolean;
begin
  Result := ATag.IsChildOf(Self);
end;

procedure TQHTMLTag.Parse(s: QStringW);
var
  p: PQCharW;
begin
  p := PQCharW(s);
  if not TryParse(p, Length(s)) then
    raise Exception.Create(SBadHtmlText);
end;

function TQHTMLTag.TryParse(var s: PQCharW; l: Integer): Boolean;
var
  AName, AValue, ATemp: QStringW;
  ps, pe: PQCharW;
  AChild: TQHTMLTag;
const
  HtmlTagStart: PWideChar = '<';
  HtmlTagClose: PWideChar = '>';
  HtmlAttrDelimiters: PWideChar = ' '#9#10#13'>';
  NullChar: WideChar = #0;
  SingleQuoter: WideChar = '''';
  DblQuoter: WideChar = '"';
  AttrValueDelimiter: PWideChar = '=';
  function DequotedValue(const V: QStringW): QStringW;
  var
    p: PQCharW;
  begin
    p := PQCharW(V);
    if p^ = SingleQuoter then
      Result := DequotedStrW(V, SingleQuoter)
    else if p^ = DblQuoter then
      Result := DequotedStrW(V, DblQuoter)
    else
      Result := V;
  end;

begin
  ps := s;
  pe := s;
  Inc(pe, l);
  Result := False;
  while s < pe do
  begin
    ATemp := DecodeTokenW(s, HtmlTagStart, NullChar, True, False);
    if Length(ATemp) > 0 then
      Add('').FInnerHtml := HtmlUnescape(ATemp);
    if s^ = '<' then
    begin
      Inc(s);
      if s^ = '/' then // ATagClose
      begin
        Inc(s);
        AName := DecodeTokenW(s, HtmlAttrDelimiters, NullChar, True, True);
        if StrCmpW(PQCharW(AName), PQCharW(FName), True) = 0 then
        begin
          Result := True;
          Exit;
        end
        else
          Break;
      end
      else
        AName := DecodeTokenW(s, HtmlAttrDelimiters, NullChar, True, False);
      AChild := Add(AName);
      while (s < pe) and (s^ <> HtmlTagClose^) do
      begin
        AName := HtmlUnescape(DequotedValue(DecodeTokenW(s, AttrValueDelimiter,
          NullChar, True, True)));
        AValue := HtmlUnescape(DequotedValue(DecodeTokenW(s, HtmlAttrDelimiters,
          NullChar, True, False)));
        AChild.Attrs.Add(AName).AsString := AValue;
      end;
      if s^ = HtmlTagClose^ then
      begin
        Inc(s);
        if not AChild.TryParse(s, l - ((IntPtr(s) - IntPtr(ps)) shr 1)) then
          Break;
      end
      else
        Break;
    end
    else
      Break;
  end;
  Result := s >= pe;
end;

end.
