/// <summary>
///   一个简单的通用进制转换单元，虽然做了一些效率优化，但肯定不要指望与专用的代码比拼效率。另外在2007及以前的版本，映射字符不要使用多字节字符。 <br />
///   用法： <br />1、调用 CreateBinaryConverter 创建转换器，参数传具体的数字到字符的映射，比如： <br />
///   十进制:CreateBinaryConverter('0123456789') 或
///   TQBinaryConverter.NewSimple('0123456789') <br />
///   十六进制：CreateBinaryConverter('0123456789ABCDEF') 或
///   TQBinaryConverter.NewSimple('0123456789ABCDEF') <br />
///   如果要在整数和字符串之间双向转换，注意保证传入的映射是排序的好的，否则不需要。 <br />2、调用实例的 ToString
///   将一个值转换为字符串，调用 FromString 从一个字符串转换为整数。注意不要加任体何前缀或后缀。
/// </summary>
unit qbinary_system;

interface

uses classes, sysutils, math;

type
  /// <summary>
  /// 进制转换接口，用来提供给用户进行相应的转换操作
  /// </summary>
  IQBinaryConverter = interface
    ['{20144EA6-2001-4E87-84F7-9D814B310701}']
    /// <summary>
    /// 将一个64位无符号整数转换为字符串，如果是有符号整数，可以强转后再转换
    /// </summary>
    /// <param name="AValue">
    /// 要转换的原始值
    /// </param>
    /// <example>
    /// AConverter.ToString(2000); <br />AConverter.ToString(UInt64(-2000));
    /// </example>
    function ToString(AValue: UInt64): String;
    /// <summary>
    ///   从一个有效的字符串中转换回原始的数值
    /// </summary>
    /// <param name="AValue">
    ///   字符串表达式
    /// </param>
    /// <returns>
    ///   返回转换结果
    /// </returns>
    /// <exception cref="EConvertError">
    ///   原始字符串为空或包含无效字符时触发
    /// </exception>
    function FromString(AValue: String): UInt64; overload;
    /// <summary>
    ///   从一个有效的字符串中转换回原始的数值
    /// </summary>
    /// <param name="AValue">
    ///   字符串表达式
    /// </param>
    /// <param name="ADefVal">
    ///   转换失败时返回的默认值
    /// </param>
    /// <returns>
    ///   返回转换结果
    /// </returns>
    function FromString(AValue: String; ADefVal: UInt64): UInt64; overload;

    /// <summary>
    ///   将一个值转换为对应的字符
    /// </summary>
    /// <param name="V">
    ///   原始值
    /// </param>
    /// <returns>
    ///   返回对应的字符
    /// </returns>
    /// <remarks>
    ///   如果参数值无效，则返回 EAssertError
    /// </remarks>
    function ToChar(V: Cardinal): Char;
    /// <summary>
    ///   将指定的字符转换为其对应的整数值
    /// </summary>
    /// <param name="C">
    ///   原始字符
    /// </param>
    /// <exception cref="EConvertError">
    ///   指定的字符不是有效的字符时抛出
    /// </exception>
    function FromChar(C: Char): Cardinal;
  end;

  /// <summary>
  ///   进制转换基类
  /// </summary>
  TQBinaryConverter = class(TInterfacedObject, IQBinaryConverter)
  protected
    /// <summary>
    ///   进制，必需大于等于2
    /// </summary>
    FSystem: Cardinal;
    function ToChar(V: Cardinal): Char; virtual; abstract;
    function FromChar(C: Char): Cardinal; virtual; abstract;
  public
    function ToString(AValue: UInt64): String;overload;
    function FromString(AValue: String): UInt64; overload;
    function FromString(AValue: String; ADefVal: UInt64): UInt64; overload;
    /// <summary>
    ///   创建一个新的简单进制转换器
    /// </summary>
    /// <param name="AMaps">
    ///   进制映射字符列表，在可能的情况下，应尽量使用按内码排序的字符序列，以为FromString/FromChar 提供更好的效率优化
    /// </param>
    class function NewSimple(const AMaps: String): IQBinaryConverter; overload;
    /// <summary>
    ///   创建一个新的简单进制转换器
    /// </summary>
    /// <param name="AMaps">
    ///   进制映射字符列表，在可能的情况下，应尽量使用按内码排序的字符序列，以为FromString/FromChar 提供更好的效率优化
    /// </param>
    class function NewSimple(const AMaps: array of Char): IQBinaryConverter; overload;
  end;

/// <summary>
///   TQBinaryConverter.CreateSimple 的全局函数版本
/// </summary>
/// <param name="AMaps">
///   进制映射字符列表，在可能的情况下，应尽量使用按内码排序的字符序列，以为FromString/FromChar 提供更好的效率优化
/// </param>
function CreateBinaryConverter(AMaps: String): IQBinaryConverter; overload;
/// <summary>
///   TQBinaryConverter.CreateSimple 的全局函数版本
/// </summary>
/// <param name="AMaps">
///   进制映射字符列表，在可能的情况下，应尽量使用按内码排序的字符序列，以为FromString/FromChar 提供更好的效率优化
/// </param>
function CreateBinaryConverter(const AMaps: array of Char): IQBinaryConverter; overload;

implementation

resourcestring
  SInvalidChar = '%s 不是一个有效的 %d 进制字符';
  SConvertSourceEmpty = '无法将一个空字符串转换为整数';

type
  TQSimpleBinaryConverter = class(TQBinaryConverter)
  protected
    FValueMaps: array of Char;
    FSorted: Boolean;
    function ToChar(V: Cardinal): Char; override;
    function FromChar(C: Char): Cardinal; override;
    procedure CheckSorted;
  public
    constructor Create(const AMaps: array of Char); overload;
    constructor Create(const AMaps: String); overload;
  end;

function CreateBinaryConverter(AMaps: String): IQBinaryConverter;
begin
  Result := TQSimpleBinaryConverter.Create(AMaps);
end;

function CreateBinaryConverter(const AMaps: array of Char): IQBinaryConverter;
begin
  Result := TQSimpleBinaryConverter.Create(AMaps);
end;

{ TBinaryConverter }

class function TQBinaryConverter.NewSimple(const AMaps: array of Char): IQBinaryConverter;
begin
  Result := TQSimpleBinaryConverter.Create(AMaps);
end;


class function TQBinaryConverter.NewSimple(const AMaps: String): IQBinaryConverter;
begin
  Result := TQSimpleBinaryConverter.Create(AMaps);
end;

function TQBinaryConverter.FromString(AValue: String): UInt64;
var
  p: PChar;
begin
  if Length(AValue) = 0 then
    raise EConvertError.Create(SConvertSourceEmpty);
  p := PChar(AValue);
  Result := 0;
  while p^ <> #0 do
  begin
    Result := Result * FSystem + FromChar(p^);
    Inc(p);
  end;
end;

function TQBinaryConverter.FromString(AValue: String; ADefVal: UInt64): UInt64;
begin
  try
    Result := FromString(AValue);
  except
    on E: EConvertError do
      Result := ADefVal;
  end;
end;

function TQBinaryConverter.ToString(AValue: UInt64): String;
var
  V, L, I: Cardinal;
  pd: PChar;
begin
  if AValue = 0 then
    Result := ToChar(0)
  else
  begin
    L := Trunc(LogN(FSystem, AValue)) + 1;
    SetLength(Result, L);
    pd := PChar(Result) + L - 1;
    I := 0;
    while AValue > 0 do
    begin
      V := AValue mod FSystem;
      pd^ := ToChar(V);
      Dec(pd);
      AValue := AValue div FSystem;
    end;
    if pd = PChar(Result) then
    begin
      Inc(pd);
      Result := pd;
    end;
  end;
end;

{ TSimpleBinaryConverter }

constructor TQSimpleBinaryConverter.Create(const AMaps: array of Char);
begin
  inherited Create;
  Assert(Length(AMaps) > 1);
  FSystem := Length(AMaps);
  SetLength(FValueMaps, Length(AMaps));
  Move(AMaps[0], FValueMaps[0], Length(AMaps) * SizeOf(Char));
  CheckSorted;
end;

procedure TQSimpleBinaryConverter.CheckSorted;
var
  I: Integer;
begin
  FSorted := true;
  for I := 1 to High(FValueMaps) do
  begin
    if FValueMaps[I] <= FValueMaps[I - 1] then
    begin
      FSorted := false;
      Break;
    end;
  end;
end;

constructor TQSimpleBinaryConverter.Create(const AMaps: String);
begin
  inherited Create;
  Assert(Length(AMaps) > 1);
  FSystem := Length(AMaps);
  SetLength(FValueMaps, Length(AMaps));
  Move(PChar(AMaps)^, FValueMaps[0], Length(AMaps) * SizeOf(Char));
  CheckSorted;
end;

function TQSimpleBinaryConverter.FromChar(C: Char): Cardinal;
var
  I, L, H, D: Integer;
begin
  if not FSorted then //不是排序的列表，只能挨个找了（当然仅限于此实现）
  begin
    for I := 0 to FSystem - 1 do
    begin
      if FValueMaps[I] = C then
        Exit(I);
    end;
  end
  else
  begin
    L := 0;
    H := FSystem - 1;
    //假设字符位于顺序区，这样直接就可以找到对应的项目，比如 16进制时，0-9 可以直接定位到
    I := (Ord(C) - Ord(FValueMaps[L])) * FSystem div (Ord(FValueMaps[H]) - Ord(FValueMaps[L]));
    if I > H then
      I := H;
    repeat
      D := Ord(FValueMaps[I]) - Ord(C);
      if D = 0 then
        Exit(Cardinal(I))
      else if D > 0 then
        H := I - 1
      else
        L := I + 1;
      I := (L + H) shr 1;
    until L > H;
  end;
  raise EConvertError.CreateFmt(SInvalidChar, [C, FSystem]);
end;

function TQSimpleBinaryConverter.ToChar(V: Cardinal): Char;
begin
  Assert(V < Length(FValueMaps));
  Result := FValueMaps[V];
end;

initialization

end.
