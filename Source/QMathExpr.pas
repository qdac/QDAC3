unit QMathExpr;

interface

uses classes, sysutils, variants, math;

{ QSimpleMathExpr 是一个简单的数学表达式解析和执行工具，注意本单元并不是为效率优化，
  而是试图以很少的代码来实现对四则运算及函数运算的支持(800行左右）。
  用法示例：
  var
  AExpr:TQMathExpression;
  begin
  AExpr:=TQMathExpression.Create;
  AExpr.Parse('1+2*3+Avg(1,4)');
  ShowMessage(VarToStr(AExpr.Value));
  FreeAndNil(AExpr);
  end;
  注册自定义函数的方法请参考Max/Min等函数的实现
}
const
  ENotMathExpr = -1;
  EVarNotFound = -2;
  EParamsTooFew = -3;
  EParamsTooMany = -4;
  EParamsMismatch = -5;
  EBracketNotClosed = -6;
  EBracketMismatch = -7;
  EParamMustGeZero = -8;
  ECalcError = -9;

type
  TQMathVar = class;
  TQMathExpr = class;
  TQMathExpression = class;
  TQMathFunction = function(const ALeft, ARight: Variant): Variant;
  TQMathValueEvent = procedure(Sender: TObject; AVar: TQMathVar);
  { 三种类型值，以便优化：固定不变，本次调用不变，易变 }
  TQMathVolatile = (mvImmutable, mvStable, mvVolatile);
  // 操作符的优先级，有些实际上没有用，只是做个标记
  TQMathOprPriority = (moNone, moGroupBegin, moList, moAddSub, moMulDivMod,
    moNeg, moGroupEnd);
  TQErrorHandler = (ehException, ehNull, ehAbort);
  TQMathParams = array of TQMathVar;

  TQMathVar = class
  protected
    FName: String;
    FValue: Variant;
{$IFDEF AUTOREFCOUNT}[unsafe]
{$ENDIF}
    FOwner: TQMathExpression;
{$IFDEF AUTOREFCOUNT}[unsafe]
{$ENDIF}
    FRefVar: TQMathVar;
    FParams: TQMathParams;
    FMinParams, FMaxParams: Integer;
    FPasscount, FSavePoint: Integer;
    FOnGetValue: TQMathValueEvent;
    FTag: Pointer;
    FVolatile: TQMathVolatile;
    function GetValue: Variant; virtual;
    procedure DoGetValue;
    property RefVar: TQMathVar read FRefVar;
  public
    constructor Create(AOwner: TQMathExpression); overload;
    destructor Destroy; override;
    function CheckParamCount: Integer; // 检查参数个数是否符合要求
    procedure Assign(const ASource: TQMathVar); virtual;
    procedure Clear; virtual;
    property Name: String read FName;
    property Volatile: TQMathVolatile read FVolatile;
    property Value: Variant read GetValue write FValue;
    property OnGetValue: TQMathValueEvent read FOnGetValue write FOnGetValue;
    property MinParams: Integer read FMinParams;
    property MaxParams: Integer read FMaxParams;
    property Params: TQMathParams read FParams;
    property Tag: Pointer read FTag write FTag;
  end;

  TQVarStackItem = record
    Expr: TQMathExpr;
    OpCode: Char;
  end;

  TQOpStackItem = record
    OpCode: Char;
    Pri: TQMathOprPriority;
  end;

  TQMathExpr = class(TQMathVar)
  protected
    FStacks: array of TQVarStackItem;
    function GetValue: Variant; override;
    procedure Parse(p: PChar);
  public
    destructor Destroy; override;
    procedure Clear; override;
  end;

  IQMathExpression = interface
    ['{AF0C22C3-05B5-43DA-8C55-25A633DBC7DE}']
    function Eval(const AExpr: String): Variant; overload;
    function Eval(const AExpr: String; var AResult: Variant): Boolean; overload;
    function Eval: Variant; overload;
    function Parse(const AExpr: String): Boolean;
    function Add(const AVarName: String; AVolatile: TQMathVolatile = mvVolatile)
      : TQMathVar; overload;
    function Add(const AFuncName: String; AMinParams, AMaxParams: Integer;
      ACallback: TQMathValueEvent; AVolatile: TQMathVolatile = mvVolatile)
      : TQMathVar; overload;
    procedure Delete(const AVarName: String); overload;
    procedure Delete(const AIndex: Integer); overload;
    procedure Clear;
    function Find(const AVarName: String): TQMathVar;
    procedure Reset;
    function GetUseStdDiv: Boolean;
    procedure SetUseStdDiv(const AValue: Boolean);
    function GetVarCount: Integer;
    function GetVars(const AIndex: Integer): TQMathVar;
    function GetNumIdentAsMultiply: Boolean;
    function SavePoint: Integer;
    procedure Restore(ASavePoint: Integer);
    procedure SetNumIdentAsMultiply(const AValue: Boolean);
    function GetErrorCode: Integer;
    function GetErrorMsg: String;
    function GetErrorHandler: TQErrorHandler;
    procedure SetErrorHandler(const AValue: TQErrorHandler);
    property UseStdDiv: Boolean read GetUseStdDiv write SetUseStdDiv;
    property NumIdentAsMultiply: Boolean read GetNumIdentAsMultiply
      write SetNumIdentAsMultiply;
    property Vars[const AIndex: Integer]: TQMathVar read GetVars;
    property VarCount: Integer read GetVarCount;
    property ErrorCode: Integer read GetErrorCode;
    property ErrorMsg: String read GetErrorMsg;
    property ErrorHandler: TQErrorHandler read GetErrorHandler
      write SetErrorHandler;
  end;

  EQMathExpr = class(Exception)

  end;

  TQMathExpression = class(TInterfacedObject, IQMathExpression)
  private
    FRoot: TQMathExpr;
    FVars: TStringList;
    FPasscount: Integer;
    FTag: Pointer;
    FUseStdDiv: Boolean;
    FNumIdentAsMultiply: Boolean;
    FSavePoint: Integer;
    FErrorCode: Integer;
    FErrorMsg: String;
    FErrorHandler: TQErrorHandler;
  private
    class function DoAdd(const ALeft, ARight: Variant): Variant; static;
    class function DoSub(const ALeft, ARight: Variant): Variant; static;
    class function DoMul(const ALeft, ARight: Variant): Variant; static;
    class function DoDiv(const ALeft, ARight: Variant): Variant; static;
    class function DoStdDiv(const ALeft, ARight: Variant): Variant; static;
    class function DoMod(const ALeft, ARight: Variant): Variant; static;
    // 默认提供的函数
    class procedure DoMax(Sender: TObject; AExpr: TQMathVar); static;
    class procedure DoMin(Sender: TObject; AExpr: TQMathVar); static;
    class procedure DoAvg(Sender: TObject; AExpr: TQMathVar); static;
    class procedure DoSum(Sender: TObject; AExpr: TQMathVar); static;
    class procedure DoStdDev(Sender: TObject; AExpr: TQMathVar); static;
    class procedure DoRand(Sender: TObject; AExpr: TQMathVar); static;
    class procedure DoPow(Sender: TObject; AExpr: TQMathVar); static;
    class procedure DoIIf(Sender: TObject; AExpr: TQMathVar); static;
    class procedure DoSin(Sender: TObject; AExpr: TQMathVar); static;
    class procedure DoCos(Sender: TObject; AExpr: TQMathVar); static;
    class procedure DoTan(Sender: TObject; AExpr: TQMathVar); static;
    class procedure DoAcrSin(Sender: TObject; AExpr: TQMathVar); static;
    class procedure DoArcCos(Sender: TObject; AExpr: TQMathVar); static;
    class procedure DoArcTan(Sender: TObject; AExpr: TQMathVar); static;
    class procedure DoArcTan2(Sender: TObject; AExpr: TQMathVar); static;
    class procedure DoAbs(Sender: TObject; AExpr: TQMathVar); static;
    class procedure DoTrunc(Sender: TObject; AExpr: TQMathVar); static;
    class procedure DoFrac(Sender: TObject; AExpr: TQMathVar); static;
    class procedure DoRound(Sender: TObject; AExpr: TQMathVar); static;
    class procedure DoCeil(Sender: TObject; AExpr: TQMathVar); static;
    class procedure DoFloor(Sender: TObject; AExpr: TQMathVar); static;
    class procedure DoLog(Sender: TObject; AExpr: TQMathVar); static;
    class procedure DoLn(Sender: TObject; AExpr: TQMathVar); static;
    class procedure DoSqrt(Sender: TObject; AExpr: TQMathVar); static;
    class procedure DoFactorial(Sender: TObject; AExpr: TQMathVar); static;
    class procedure DoExp(Sender: TObject; AExpr: TQMathVar); static;
    function GetUseStdDiv: Boolean;
    procedure SetUseStdDiv(const AValue: Boolean);
    function GetNumIdentAsMultiply: Boolean;
    procedure SetNumIdentAsMultiply(const AValue: Boolean);
    function GetVarCount: Integer;
    function GetVars(const AIndex: Integer): TQMathVar;
    function GetErrorCode: Integer;
    function GetErrorMsg: String;
    procedure SetLastError(const ACode: Integer; const AMsg: String);
    function GetErrorHandler: TQErrorHandler;
    procedure SetErrorHandler(const AValue: TQErrorHandler);
  public
    constructor Create;
    destructor Destroy; override;
    function Eval(const AExpr: String; var AResult: Variant): Boolean; overload;
    function Eval(const AExpr: String): Variant; overload;
    function Eval: Variant; overload;
    function Parse(const AExpr: String): Boolean;
    function Add(const AVarName: String; AVolatile: TQMathVolatile = mvVolatile)
      : TQMathVar; overload;
    function Add(const AFuncName: String; AMinParams, AMaxParams: Integer;
      ACallback: TQMathValueEvent; AVolatile: TQMathVolatile = mvVolatile)
      : TQMathVar; overload;
    procedure Delete(const AVarName: String); overload;
    procedure Delete(const AIndex: Integer); overload;
    procedure Clear;
    function Find(const AVarName: String): TQMathVar;
    procedure Reset;
    function SavePoint: Integer;
    procedure Restore(ASavePoint: Integer);
    property UseStdDiv: Boolean read FUseStdDiv write FUseStdDiv;
    property NumIdentAsMultiply: Boolean read FNumIdentAsMultiply
      write FNumIdentAsMultiply;
    property Vars[const AIndex: Integer]: TQMathVar read GetVars;
    property VarCount: Integer read GetVarCount;
    property Tag: Pointer read FTag write FTag;
  end;

implementation

resourcestring
  SNotQMathExpr = '表达式 %s 解析失败';
  SVarNotFound = '变量 %s 的定义未发现';
  SParamsTooFew = '函数 %s 至少需要 %d 个参数';
  SParamsTooMany = '函数 %s 至多可以有 %d 个参数';
  SParamsMismatch = '函数 %s 需要 %d 个参数';
  SBracketNotClosed = '括号数量不匹配，缺少右括号';
  SBracketMismatch = '左右括号数量不匹配';
  SParamMustGEZero = '参数必需大于等于0';

type

  TQMathFunctionItem = record
    Method: TQMathFunction;
    Op: Char;
    Pri: TQMathOprPriority;
  end;

const
  // %&'()*+,-./
  MathFunctions: array ['%' .. '/'] of TQMathFunctionItem = ( //
    // %
    (Method: TQMathExpression.DoMod; Op: '%'; Pri: moMulDivMod),
    // &
    (Method: nil; Op: #0; Pri: moNone),
    // '
    (Method: nil; Op: #0; Pri: moNone),
    // (
    (Method: nil; Op: '('; Pri: moGroupBegin),
    // )
    (Method: nil; Op: ')'; Pri: moGroupEnd),
    // *
    (Method: TQMathExpression.DoMul; Op: '*'; Pri: moMulDivMod),
    // +
    (Method: TQMathExpression.DoAdd; Op: '+'; Pri: moAddSub),
    // ,
    (Method: nil; Op: ','; Pri: moList),
    // -
    (Method: TQMathExpression.DoSub; Op: '-'; Pri: moAddSub),
    // .
    (Method: nil; Op: #0; Pri: moNone),
    // /
    (Method: TQMathExpression.DoDiv; Op: '/'; Pri: moMulDivMod) //
    );
  { TQMathExpression }

function TQMathExpression.Add(const AVarName: String; AVolatile: TQMathVolatile)
  : TQMathVar;
var
  AIdx: Integer;
begin
  if FVars.Find(AVarName, AIdx) then
    Result := TQMathVar(FVars.Objects[AIdx])
  else
  begin
    Result := TQMathVar.Create(Self);
    Result.FName := AVarName;
    Result.FVolatile := AVolatile;
    Result.Value := Unassigned;
    FVars.InsertObject(AIdx, AVarName, Result);
  end;
end;

function TQMathExpression.Add(const AFuncName: String;
  AMinParams, AMaxParams: Integer; ACallback: TQMathValueEvent;
  AVolatile: TQMathVolatile): TQMathVar;
var
  AIdx: Integer;
  AExist: TObject;
begin
  if AVolatile = mvImmutable then // 常量函数不需要任务参数，如果需要那肯定是你设计错了
  begin
    Result := Add(AFuncName, AVolatile);
    if Assigned(ACallback) then
      ACallback(Self, Result);
  end
  else if FVars.Find(AFuncName, AIdx) then
  begin
    AExist := FVars.Objects[AIdx];
    Result := TQMathVar(AExist);
  end
  else
  begin
    Result := TQMathVar.Create(Self);
    Result.FName := AFuncName;
    Result.FVolatile := AVolatile;
    Result.Value := Unassigned;
    Result.FMinParams := AMinParams;
    Result.FMaxParams := AMaxParams;
    Result.FOnGetValue := ACallback;
    FVars.InsertObject(AIdx, AFuncName, Result);
  end;
end;

procedure TQMathExpression.Clear;
var
  I: Integer;
begin
  for I := 0 to FVars.Count - 1 do
    FVars.Objects[I].{$IFDEF NEXTGEN}DisposeOf{$ELSE}Free{$ENDIF};
  FVars.Clear;
  FRoot.Clear;
end;

constructor TQMathExpression.Create;
  procedure RegisterFunctions;
  const
    E: Double = 2.71828182845905;
  begin
    Add('MAX', 1, MaxInt, DoMax); // 求最大值
    Add('MIN', 1, MaxInt, DoMin); // 求最小值
    Add('AVG', 1, MaxInt, DoAvg); // 求平均值
    Add('SUM', 1, MaxInt, DoSum); // 求和
    Add('STDDEV', 2, MaxInt, DoStdDev); // 求标准方差
    Add('RAND', 0, 1, DoRand); // 随机数
    Add('POW', 2, 2, DoPow); // 幂运算
    Add('IIF', 3, 3, DoIIf); // 条件判断
    Add('SIN', 1, 1, DoSin); // 正弦
    Add('COS', 1, 1, DoCos); // 余弦
    Add('TAN', 1, 1, DoTan); // 正切
    Add('ASIN', 1, 1, DoAcrSin); // 反正弦
    Add('ACOS', 1, 1, DoArcCos); // 反余弦
    Add('ATAN', 1, 1, DoArcTan); // 反正切
    Add('ATAN2', 2, 2, DoArcTan2); // 返回从 x 轴到点 (x,y) 的角度（介于 -PI/2 与 PI/2 弧度之间）
    Add('ABS', 1, 1, DoAbs); // 绝对值
    Add('TRUNC', 1, 1, DoTrunc); // 取整
    Add('FRAC', 1, 1, DoFrac); // 取小数
    Add('ROUND', 1, 2, DoRound); // 四舍五入
    Add('CEIL', 1, 1, DoCeil); // 上取整
    Add('FLOOR', 1, 1, DoFloor); // 下取整
    Add('LOG', 1, 2, DoLog); // Log(x)/Log(base,x)
    Add('LN', 1, 1, DoLn); // 自然对数
    Add('SQRT', 1, 1, DoSqrt); // 求平方根，等价于Pow(x,0.5)
    Add('FACT', 1, 1, DoFactorial); // 求阶乘
    Add('EXP', 1, 1, DoExp); // 求E的指数 ，等价于Pow(e,x)
    // 常量
    Add('PI', mvImmutable).Value := PI; // π值，3.1415926...
    Add('E', mvImmutable).Value := E; // 自然对数常量
  end;

begin
  inherited;
  FVars := TStringList.Create;
  FVars.CaseSensitive := false;
  FRoot := TQMathExpr.Create(Self);
  FUseStdDiv := True;
  RegisterFunctions;
end;

procedure TQMathExpression.Delete(const AVarName: String);
var
  AIdx: Integer;
begin
  if FVars.Find(AVarName, AIdx) then
    Delete(AIdx);
end;

procedure TQMathExpression.Delete(const AIndex: Integer);
begin
  FVars.Objects[AIndex].{$IFDEF NEXTGEN}DisposeOf{$ELSE}Free{$ENDIF};
  FVars.Delete(AIndex);
end;

destructor TQMathExpression.Destroy;
begin
  Clear;
  FreeAndNil(FVars);
  FreeAndNil(FRoot);
  inherited;
end;

class procedure TQMathExpression.DoAbs(Sender: TObject; AExpr: TQMathVar);
begin
  AExpr.Value := abs(AExpr.Params[0].Value);
end;

class procedure TQMathExpression.DoAcrSin(Sender: TObject; AExpr: TQMathVar);
begin
  AExpr.Value := arcsin(AExpr.Params[0].Value);
end;

class function TQMathExpression.DoAdd(const ALeft, ARight: Variant): Variant;
begin
  Result := ALeft + ARight;
end;

class procedure TQMathExpression.DoArcCos(Sender: TObject; AExpr: TQMathVar);
begin
  AExpr.Value := arccos(AExpr.Params[0].Value);
end;

class procedure TQMathExpression.DoArcTan(Sender: TObject; AExpr: TQMathVar);
begin
  AExpr.Value := arctan(AExpr.Params[0].Value);
end;

class procedure TQMathExpression.DoArcTan2(Sender: TObject; AExpr: TQMathVar);
begin
  AExpr.Value := arctan2(AExpr.Params[0].Value, AExpr.Params[1].Value);
end;

class function TQMathExpression.DoDiv(const ALeft, ARight: Variant): Variant;
begin
  if (VarType(ALeft) in [varSingle, varDouble]) or
    (VarType(ARight) in [varSingle, varDouble]) then
    Result := ALeft / ARight
  else
    Result := ALeft div ARight;
end;

class procedure TQMathExpression.DoExp(Sender: TObject; AExpr: TQMathVar);
begin
  AExpr.Value := Exp(AExpr.Params[0].Value);
end;

class procedure TQMathExpression.DoFactorial(Sender: TObject; AExpr: TQMathVar);
var
  V, R: Int64;
begin
  V := AExpr.Params[0].Value;
  if V < 0 then
    AExpr.FOwner.SetLastError(EParamMustGeZero, SParamMustGEZero);
  if V = 0 then
    R := 1
  else
  begin
    R := V;
    while V > 1 do
    begin
      Dec(V);
      R := R * V;
    end;
  end;
  AExpr.Value := R;
end;

class procedure TQMathExpression.DoFloor(Sender: TObject; AExpr: TQMathVar);
begin
  AExpr.Value := Floor(AExpr.Params[0].Value);
end;

class procedure TQMathExpression.DoFrac(Sender: TObject; AExpr: TQMathVar);
begin
  AExpr.Value := Frac(AExpr.Params[0].Value);
end;

class procedure TQMathExpression.DoIIf(Sender: TObject; AExpr: TQMathVar);
begin
  if AExpr.Params[0].Value <> 0 then
    AExpr.Value := AExpr.Params[1].Value
  else
    AExpr.Value := AExpr.Params[2].Value;
end;

class procedure TQMathExpression.DoLn(Sender: TObject; AExpr: TQMathVar);
begin
  AExpr.Value := Ln(AExpr.Params[0].Value);
end;

class procedure TQMathExpression.DoLog(Sender: TObject; AExpr: TQMathVar);
begin
  if Length(AExpr.Params) = 2 then
    AExpr.Value := LogN(AExpr.Params[0].Value, AExpr.Params[1].Value)
  else
    AExpr.Value := Log10(AExpr.Params[0].Value);
end;

class function TQMathExpression.DoMul(const ALeft, ARight: Variant): Variant;
begin
  Result := ALeft * ARight;
end;

class procedure TQMathExpression.DoPow(Sender: TObject; AExpr: TQMathVar);
begin
  AExpr.Value := Power(AExpr.Params[0].Value, AExpr.Params[1].Value);
end;

class procedure TQMathExpression.DoRand(Sender: TObject; AExpr: TQMathVar);
begin
  if Length(AExpr.Params) = 1 then
    AExpr.Value := Random(AExpr.Params[0].Value)
  else
    AExpr.Value := Random(MaxInt);
end;

class procedure TQMathExpression.DoRound(Sender: TObject; AExpr: TQMathVar);
begin
  if Length(AExpr.Params) = 2 then
    AExpr.Value := SimpleRoundTo(AExpr.Params[0].Value, AExpr.Params[1].Value)
  else
    AExpr.Value := SimpleRoundTo(AExpr.Params[0].Value, 0);
end;

class procedure TQMathExpression.DoSin(Sender: TObject; AExpr: TQMathVar);
begin
  AExpr.Value := sin(AExpr.Params[0].Value);
end;

class procedure TQMathExpression.DoSqrt(Sender: TObject; AExpr: TQMathVar);
begin
  AExpr.Value := sqrt(AExpr.Params[0].Value);
end;

class procedure TQMathExpression.DoStdDev(Sender: TObject; AExpr: TQMathVar);
var
  AValues: array of Double;
  I: Integer;
begin
  SetLength(AValues, Length(AExpr.Params));
  for I := 0 to High(AExpr.Params) do
    AValues[I] := AExpr.Params[I].Value;
  AExpr.Value := StdDev(AValues);
end;

class function TQMathExpression.DoStdDiv(const ALeft, ARight: Variant): Variant;
begin
  Result := ALeft / Double(ARight);
end;

class function TQMathExpression.DoSub(const ALeft, ARight: Variant): Variant;
begin
  Result := ALeft - ARight;
end;

function TQMathExpression.Eval: Variant;
begin
  Inc(FPasscount);
  Result := FRoot.Value;
end;

function TQMathExpression.Find(const AVarName: String): TQMathVar;
var
  AIdx: Integer;
begin
  if FVars.Find(AVarName, AIdx) then
    Result := FVars.Objects[AIdx] as TQMathVar
  else
    Result := nil;
end;

function TQMathExpression.GetErrorCode: Integer;
begin
  Result := FErrorCode;
end;

function TQMathExpression.GetErrorHandler: TQErrorHandler;
begin
  Result := FErrorHandler;
end;

function TQMathExpression.GetErrorMsg: String;
begin
  Result := FErrorMsg;
end;

function TQMathExpression.GetNumIdentAsMultiply: Boolean;
begin
  Result := FNumIdentAsMultiply;
end;

function TQMathExpression.GetUseStdDiv: Boolean;
begin
  Result := FUseStdDiv;
end;

function TQMathExpression.GetVarCount: Integer;
begin
  Result := FVars.Count;
end;

function TQMathExpression.GetVars(const AIndex: Integer): TQMathVar;
begin
  Result := TQMathVar(FVars.Objects[AIndex]);
end;

function TQMathExpression.Eval(const AExpr: String): Variant;
begin
  try
    FRoot.Parse(PChar(AExpr));
    Result := Eval;
  except
    on E: EAbort do
    begin
      if FErrorHandler = ehNull then
        Result := Null
      else if FErrorHandler = ehAbort then
        begin
        E.Message:=FErrorMsg;
        raise
        end
      else
        raise EQMathExpr.Create(FErrorMsg);
    end;
  end;
end;

function TQMathExpression.Parse(const AExpr: String): Boolean;
begin
  FErrorCode := 0;
  SetLength(FErrorMsg, 0);
  try
    FRoot.Parse(PChar(AExpr));
  except
    on E: EAbort do
    begin

    end;
  end;
  Result := FErrorCode = 0;
end;

procedure TQMathExpression.Reset;
begin
  FRoot.Clear;
end;

procedure TQMathExpression.Restore(ASavePoint: Integer);
var
  I: Integer;
  AVar: TQMathVar;
begin
  if (ASavePoint < 1) or (ASavePoint > FSavePoint) then
    // 0保留给系统内部预定义的变量，不支持通过保存点清理，但可以通过Delete/Clear删除
    Exit;
  I := 0;
  while I < FVars.Count do
  begin
    AVar := TQMathVar(FVars.Objects[I]);
    if AVar.FSavePoint >= ASavePoint then
    begin
      FVars.Delete(I);
      FreeAndNil(AVar);
    end
    else
      Inc(I);
  end;
  FSavePoint := ASavePoint;
end;

function TQMathExpression.SavePoint: Integer;
begin
  Inc(FSavePoint);
  Result := FSavePoint;
end;

procedure TQMathExpression.SetErrorHandler(const AValue: TQErrorHandler);
begin
  FErrorHandler := AValue;
end;

procedure TQMathExpression.SetLastError(const ACode: Integer;
  const AMsg: String);
begin
  FErrorCode := ACode;
  FErrorMsg := AMsg;
  if ACode <> 0 then
  begin
    if FErrorHandler in [ehAbort, ehNull] then
      Abort
    else
      raise EQMathExpr.Create(AMsg);
  end;
end;

procedure TQMathExpression.SetNumIdentAsMultiply(const AValue: Boolean);
begin
  FNumIdentAsMultiply := AValue;
end;

procedure TQMathExpression.SetUseStdDiv(const AValue: Boolean);
begin
  FUseStdDiv := AValue;
end;

class procedure TQMathExpression.DoAvg(Sender: TObject; AExpr: TQMathVar);
var
  I: Integer;
  AResult: Variant;
begin
  AResult := 0;
  for I := 0 to High(AExpr.Params) do
    AResult := AResult + AExpr.Params[I].Value;
  AExpr.Value := AResult / Length(AExpr.Params);
end;

class procedure TQMathExpression.DoCeil(Sender: TObject; AExpr: TQMathVar);
begin
  AExpr.Value := Ceil(AExpr.Params[0].Value);
end;

class procedure TQMathExpression.DoCos(Sender: TObject; AExpr: TQMathVar);
begin
  AExpr.Value := cos(AExpr.Params[0].Value);
end;

class procedure TQMathExpression.DoMax(Sender: TObject; AExpr: TQMathVar);
var
  I: Integer;
  AValue, AResult: Variant;
begin
  AResult := AExpr.Params[0].Value;
  for I := 1 to High(AExpr.Params) do
  begin
    AValue := AExpr.Params[I].Value;
    if AValue > AResult then
      AResult := AValue;
  end;
  AExpr.Value := AResult;
end;

class procedure TQMathExpression.DoMin(Sender: TObject; AExpr: TQMathVar);
var
  I: Integer;
  AValue, AResult: Variant;
begin
  AResult := AExpr.Params[0].Value;
  for I := 1 to High(AExpr.Params) do
  begin
    AValue := AExpr.Params[I].Value;
    if AValue < AResult then
      AResult := AValue;
  end;
  AExpr.Value := AResult;
end;

class function TQMathExpression.DoMod(const ALeft, ARight: Variant): Variant;
begin
  Result := ALeft mod ARight;
end;

class procedure TQMathExpression.DoSum(Sender: TObject; AExpr: TQMathVar);
var
  I: Integer;
  AResult: Variant;
begin
  AResult := 0;
  for I := 0 to High(AExpr.Params) do
    AResult := AResult + AExpr.Params[I].Value;
  AExpr.Value := AResult;
end;

class procedure TQMathExpression.DoTan(Sender: TObject; AExpr: TQMathVar);
begin
  AExpr.Value := tan(AExpr.Params[0].Value);
end;

class procedure TQMathExpression.DoTrunc(Sender: TObject; AExpr: TQMathVar);
begin
  AExpr.Value := Trunc(AExpr.Params[0].Value);
end;

function TQMathExpression.Eval(const AExpr: String;
  var AResult: Variant): Boolean;
begin
  try
    FRoot.Parse(PChar(AExpr));
    AResult := Eval;
    Result := True;
  except
    on E: Exception do
    begin
      Result := false;
      if not(E is EAbort) then
      begin
        FErrorCode := ECalcError;
        FErrorMsg := E.Message;
      end;
    end;
  end;
end;

{ TQMathVar }

procedure TQMathVar.Assign(const ASource: TQMathVar);
begin
  Clear;
  FName := ASource.Name;
  FValue := ASource.FValue;
  FPasscount := ASource.FPasscount;
  FOnGetValue := ASource.OnGetValue;
  FVolatile := ASource.Volatile;
  FMinParams := ASource.FMinParams;
  FMaxParams := ASource.FMaxParams;
  FTag := ASource.FTag;
end;

function TQMathVar.CheckParamCount: Integer;
begin
  Result := Length(FParams);
  if Result < MinParams then
  begin
    if MinParams = MaxParams then
      FOwner.SetLastError(EParamsMismatch, Format(SParamsMismatch,
        [Name, MinParams]))
    else
      FOwner.SetLastError(EParamsTooFew, Format(SParamsTooFew,
        [Name, MinParams]));
  end
  else if Result > MaxParams then
  begin
    if MinParams = MaxParams then
      FOwner.SetLastError(EParamsMismatch, Format(SParamsMismatch,
        [Name, MinParams]))
    else
      FOwner.SetLastError(EParamsTooMany, Format(SParamsTooMany,
        [Name, MaxParams]));
  end;
end;

procedure TQMathVar.Clear;
var
  I: Integer;
begin
  FValue := Unassigned;
  for I := 0 to High(FParams) do
    FreeAndNil(FParams[I]);
  SetLength(FParams, 0);
end;

constructor TQMathVar.Create(AOwner: TQMathExpression);
begin
  inherited Create;
  FOwner := AOwner;
  FMaxParams := 0;
  FSavePoint := AOwner.FSavePoint;
end;

destructor TQMathVar.Destroy;
begin
  Clear;
  inherited;
end;

procedure TQMathVar.DoGetValue;
begin
  if Assigned(OnGetValue) then
    OnGetValue(FOwner, TQMathVar(Self));
end;

function TQMathVar.GetValue: Variant;
var
  AParams: TQMathParams;
begin
  case FVolatile of
    mvStable: // Stable 在一次计算中值是不变的，所以用引用的原始变量调用一次
      begin
        if Assigned(FRefVar) then
        begin
          if FRefVar.FPasscount <> FOwner.FPasscount then
          begin
            FRefVar.FPasscount := FOwner.FPasscount;
            AParams := FRefVar.Params;
            FRefVar.FParams := FParams;
            try
              FRefVar.DoGetValue;
            finally
              FRefVar.FParams := AParams;
            end;
          end;
          Result := FRefVar.Value;
        end
        else
        begin
          if FPasscount <> FOwner.FPasscount then
          begin
            FPasscount := FOwner.FPasscount;
            DoGetValue;
          end;
          Result := FValue;
        end;
      end;
    mvVolatile:
      begin
        DoGetValue;
        Result := FValue;
      end
  else
    Result := FValue;
  end;

end;

{ TQMathExpr }
procedure TQMathExpr.Clear;
var
  I: Integer;
begin
  inherited;
  for I := 0 to High(FStacks) do
  begin
    if Assigned(FStacks[I].Expr) then
      FreeAndNil(FStacks[I].Expr);
  end;
  SetLength(FStacks, 0);
end;

destructor TQMathExpr.Destroy;
begin
  Clear;
  inherited;
end;

function TQMathExpr.GetValue: Variant;
var
  AStacks: array of Variant;
  ATop, I: Integer;
begin
  if Length(FStacks) > 0 then
  begin
    ATop := -1;
    SetLength(AStacks, Length(FStacks));
    for I := 0 to High(FStacks) do
    begin
      with FStacks[I] do
      begin
        if OpCode <> #0 then
        begin
          if (OpCode = '/') and FOwner.UseStdDiv then
          begin
            AStacks[ATop - 1] := FOwner.DoStdDiv(AStacks[ATop - 1],
              AStacks[ATop]);
            Dec(ATop);
          end
          else if OpCode = '!' then // 负值
            AStacks[ATop] := -AStacks[ATop]
          else
          begin
            AStacks[ATop - 1] := MathFunctions[OpCode].Method(AStacks[ATop - 1],
              AStacks[ATop]);
            Dec(ATop);
          end;
        end
        else
        begin
          Inc(ATop);
          AStacks[ATop] := Expr.Value;
        end;
      end;
    end;
    Result := AStacks[0];
  end
  else
    Result := inherited;
end;

procedure TQMathExpr.Parse(p: PChar);
var
  ps, os: PChar;
  ALastExpr: TQMathExpr;
  AExpectStackSize: Integer;
  AVarTop: Integer;
  AOprStacks: array of TQOpStackItem;
  AOprTop: Integer;
const
  SpaceChars: PChar = #9#10#13#32;

  function CharIn(c: Char; ASet: PChar): Boolean;
  begin
    Result := false;
    while ASet^ <> #0 do
    begin
      Result := (c = ASet^);
      if Result then
        Break;
      Inc(ASet);
    end;
  end;

  procedure DetectToken(AVar: TQMathVar; AToken: String);
  var
    pt, pe: PChar;
    vInt: Int64;
    vFloat: Double;
    ATemp: TQMathVar;
  begin
    pt := PChar(AToken);
    pe := pt + Length(AToken) - 1;
    while (pe >= pt) and CharIn(pe^, SpaceChars) do
      Dec(pe);
    SetLength(AToken, pe - pt + 1);
    pt := PChar(AToken);
    if (pt^ = '"') or (pt^ = '''') then
    begin
      AVar.FVolatile := TQMathVolatile.mvImmutable;
      AVar.Value := AnsiExtractQuotedStr(pt, pt^);
    end
    else if TryStrToInt64(AToken, vInt) then
    begin
      AVar.FVolatile := TQMathVolatile.mvImmutable;
      AVar.Value := vInt;
    end
    else if TryStrToFloat(AToken, vFloat) then
    begin
      AVar.FVolatile := TQMathVolatile.mvImmutable;
      AVar.Value := vFloat;
    end
    else if Length(AToken) > 0 then
    begin
      ATemp := FOwner.Find(AToken);
      if not Assigned(ATemp) then
        FOwner.SetLastError(EVarNotFound, Format(SVarNotFound, [AToken]));
      AVar.Assign(ATemp);
      AVar.FRefVar := ATemp;
    end
    else
    begin
      AVar.FVolatile := TQMathVolatile.mvVolatile;
      AVar.Value := Unassigned;
    end;
  end;

  function SkipSpace(var p: PChar): PChar;
  begin
    while CharIn(p^, SpaceChars) do
      Inc(p);
    Result := p;
  end;

  procedure PushVar(AExpr: TQMathExpr; AOpCode: Char);
  begin
    Inc(AVarTop);
    if AVarTop = Length(FStacks) then
      SetLength(FStacks, Length(FStacks) + 32);
    FStacks[AVarTop].Expr := AExpr;
    FStacks[AVarTop].OpCode := AOpCode;
  end;

  procedure PushOp(AOpCode: Char; APri: TQMathOprPriority);
  begin
    Inc(AOprTop);
    if AOprTop = Length(AOprStacks) then // 动态调整栈大小，每次调整32
      SetLength(AOprStacks, Length(AOprStacks) + 32);
    AOprStacks[AOprTop].OpCode := AOpCode;
    AOprStacks[AOprTop].Pri := APri;
    case AOpCode of
      '(':
        ;
      '!':
        Inc(AExpectStackSize, 2)
    else
      Inc(AExpectStackSize, 3);
    end;
  end;

  procedure NextChar(var p: PChar);
  var
    q: Char;
  begin
    if (p^ = '''') or (p^ = '"') then
    begin
      q := p^;
      Inc(p);
      while p^ <> #0 do
      begin
        if p^ = q then
        begin
          Inc(p);
          if p^ <> q then
            Break;
        end
        else
          Inc(p);
      end;
    end
    else
      Inc(p);
  end;
  function DecodeBracketsText(var p: PChar): String;
  var
    ABrackets: Integer;
    ps: PChar;
  begin
    ps := p + 1;
    ABrackets := 1;
    p := ps;
    while p^ <> #0 do
    begin
      if p^ = '(' then
        Inc(ABrackets)
      else if p^ = ')' then
      begin
        Dec(ABrackets);
        if ABrackets = 0 then
          Break;
      end;
      NextChar(p);
    end;
    Result := Copy(ps, 0, p - ps);
    Inc(p);
  end;
  function DecodeParam(var p: PChar): String;
  var
    ps: PChar;
    ABrackets: Integer;
  begin
    ps := p;
    ABrackets := 0;
    while p^ <> #0 do
    begin
      if p^ = '(' then
        Inc(ABrackets)
      else if p^ = ')' then
        Dec(ABrackets)
      else if (p^ = ',') and (ABrackets = 0) then
        Break;
      NextChar(p);
    end;
    Result := Copy(ps, 0, p - ps);
    if p^ = ',' then
      Inc(p);
  end;

  function NextIsOperator: Boolean;
  begin
    Result := (p^ >= '%') and (p^ <= '/') and (MathFunctions[p^].Pri <> moNone);
  end;
  procedure DecodeParams(AExpr: TQMathExpr);
  var
    AParams, AParam: String;
    I: Integer;
    pp: PChar;
  begin
    AParams := DecodeBracketsText(p);
    pp := PChar(AParams);
    if AExpr.MaxParams <> MaxInt then // 有最大参数限制？
      SetLength(AExpr.FParams, AExpr.MaxParams)
    else
      SetLength(AExpr.FParams, AExpr.MinParams);
    I := 0;
    while pp^ <> #0 do
    begin
      AParam := DecodeParam(pp);
      if I <= AExpr.MaxParams then
      begin
        if I = Length(AExpr.FParams) then
          SetLength(AExpr.FParams, Length(AExpr.FParams) + 32);
        AExpr.FParams[I] := TQMathExpr.Create(FOwner);
      end
      else
        FOwner.SetLastError(EParamsTooMany, Format(SParamsTooMany,
          [AExpr.Name, AExpr.MaxParams]));
      TQMathExpr(AExpr.FParams[I]).Parse(PChar(AParam));
      Inc(I);
    end;
    SetLength(AExpr.FParams, I);
    AExpr.CheckParamCount;
  end;
  procedure ProcessOpCode;
  begin
    if (p^ = '+') or (p^ = '-') then
    begin
      if (AOprTop = 0) and (AVarTop = -1) then
      begin
        if p^ = '-' then
          PushOp('!', moNeg);
        Inc(p);
        ps := SkipSpace(p);
        Exit;
      end;
    end;
    with MathFunctions[p^] do
    begin
      if p^ = ')' then
      begin
        while AOprStacks[AOprTop].Pri > moGroupBegin do
        begin
          PushVar(nil, AOprStacks[AOprTop].OpCode);
          Dec(AOprTop);
        end;
        Dec(AOprTop);
      end
      else if (Pri > AOprStacks[AOprTop].Pri) or (p^ = '(') then
      begin
        PushOp(p^, Pri);
      end
      else
      begin
        while AOprStacks[AOprTop].Pri >= Pri do
        begin
          PushVar(nil, AOprStacks[AOprTop].OpCode);
          Dec(AOprTop);
        end;
        PushOp(p^, Pri);
      end;
      Inc(p);
      ps := SkipSpace(p);
      // 一个运算符后面跟+-肯定是正负号，直接处理
      if (Op <> ')') and ((p^ = '+') or (p^ = '-')) then
      begin
        if p^ = '-' then
        begin
          PushOp('!', moNeg);
        end;
        Inc(p);
        ps := SkipSpace(p);
        if NextIsOperator and (MathFunctions[p^].Op <> '(') then
          FOwner.SetLastError(ENotMathExpr, Format(SNotQMathExpr, [os]));
      end;
    end;
  end;

  procedure ValidStacks;
  var
    ATop, I: Integer;
  begin
    if Length(FStacks) > 0 then
    begin
      ATop := -1;
      for I := 0 to High(FStacks) do
      begin
        with FStacks[I] do
        begin
          if OpCode <> #0 then
          begin
            if OpCode <> '!' then
              Dec(ATop);
          end
          else
            Inc(ATop);
        end;
      end;
      if ATop <> 0 then
        FOwner.SetLastError(ENotMathExpr, Format(SNotQMathExpr, [os]));
    end;
  end;

  procedure DirectParse;
  begin
    Clear;
    SetLength(FStacks, 32);
    SetLength(AOprStacks, 32);
    AVarTop := -1;
    AOprTop := 0;
    ps := SkipSpace(p);
    while p^ <> #0 do
    begin
      if NextIsOperator then
      begin
        if ps < p then
        begin
          ALastExpr := TQMathExpr.Create(FOwner);
          PushVar(ALastExpr, #0);
          DetectToken(ALastExpr, Copy(ps, 0, p - ps));
          if (ALastExpr.Volatile <> mvImmutable) and (p^ = '(') then
          begin
            DecodeParams(ALastExpr);
            ps := SkipSpace(p);
            continue;
          end;
        end;
        ProcessOpCode;
      end
      else
        Inc(p);
    end;
    if p > ps then
    begin
      ALastExpr := TQMathExpr.Create(FOwner);
      PushVar(ALastExpr, #0);
      DetectToken(ALastExpr, Copy(ps, 0, p - ps));
    end;
    if AOprTop < 0 then
      FOwner.SetLastError(EBracketMismatch, SBracketMismatch);
    while AOprTop > 0 do
    begin
      PushVar(nil, AOprStacks[AOprTop].OpCode);
      Dec(AOprTop);
    end;
    SetLength(FStacks, AVarTop + 1);
    ValidStacks;
  end;

  function NextIsNumChar: Boolean;
  begin
    if p^ = '.' then
    begin
      Result := (p[1] >= '0') and (p[1] <= '9');
    end
    else
      Result := (p^ >= '0') and (p^ <= '9');
  end;
  function NextIsIdentChar: Boolean;
  begin
    Result := ((p^ >= 'A') and (p^ <= 'Z')) or ((p^ >= 'a') and (p^ <= 'a')) or
      (p^ = '_') or {$IFDEF UNICODE}(p^ > #$7F) {$ELSE} (p^ < 0){$ENDIF};
  end;
  procedure DoParse;
  var
    ATemp: String;
    L: Integer;
    pd: PChar;
  begin
    if FOwner.NumIdentAsMultiply then
    // 如果要支持2x这种解析为2*x，那么对表达式进行预处理成正常的表达式，然后再解析
    begin
      ps := p;
      L := 0;
      while p^ <> #0 do
      begin
        if NextIsNumChar then
        begin
          repeat
            Inc(p);
          until (p^ = #0) or (not NextIsNumChar);
          if (p^ <> #0) and NextIsIdentChar then
            Inc(L);
        end
        else
          Inc(p);
      end;
      if L > 0 then
      begin
        SetLength(ATemp, L + (p - ps));
        p := ps;
        pd := PChar(ATemp);
        ps := pd;
        while p^ <> #0 do
        begin
          if NextIsNumChar then
          begin
            repeat
              pd^ := p^;
              Inc(p);
              Inc(pd);
            until (p^ = #0) or (not NextIsNumChar);
            if (p^ <> #0) and NextIsIdentChar then
            begin
              pd^ := '*';
              Inc(pd);
            end;
          end
          else
          begin
            pd^ := p^;
            Inc(p);
            Inc(pd);
          end;
        end;
      end;
      p := ps;
    end;
    DirectParse;
  end;

begin
  os := p;
  AExpectStackSize := 0;
  DoParse;
end;

end.
