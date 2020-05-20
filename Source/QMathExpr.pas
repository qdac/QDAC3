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
  2020-1-14
  =========
  * 修改实现，以便能够在多线程中同时调用一个表达式（注意自定义的函数必需自行保证线程安全）
  2020-1-13                                            。
  =========
  + 增加对 ==、or、and、shl、shr、<<、>> 运算符的支持
  2020-1-12
  =========
  + 增加逻辑运算符(与&&,或||,非!)和位运算符(位于&,位或|,位非~,位异或^)的支持
  * 字符串与其它类型相加的结果，结果类型改为字符串
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
  IQMathExpression = interface;
  PQEvalData = ^TQEvalData;

  TQEvalData = record
    Expression: TQMathExpression;
    PassCount: Integer;
    Params: Pointer;
    Next: PQEvalData;
  end;

  TQMathFunction = function(const ALeft, ARight: Variant): Variant;
  TQMathValueEvent = procedure(Sender: TObject; AVar: TQMathVar;
    const ACallParams: PQEvalData; var AResult: Variant) of object;
  TQMathValueEventG = procedure(Sender: TObject; AVar: TQMathVar;
    const ACallParams: PQEvalData; var AResult: Variant);
  TQMathValueEventA = reference to procedure(Sender: TObject; AVar: TQMathVar;
    const ACallParams: PQEvalData; var AResult: Variant);
  TQMathExpressVarLookupEvent = procedure(Sender: IQMathExpression;
    const AVarName: String; var AVar: TQMathVar) of object;
  TQMathExpressVarLookupEventA = reference to procedure
    (Sender: IQMathExpression; const AVarName: String; var AVar: TQMathVar);
  TQMathExpressVarLookupEventG = procedure(Sender: IQMathExpression;
    const AVarName: String; var AVar: TQMathVar);
  { 三种类型值，以便优化：固定不变，本次调用不变，易变 }
  TQMathVolatile = (mvImmutable, mvStable, mvVolatile);
  // 操作符的优先级，有些实际上没有用，只是做个标记
  TQMathOprPriority = (moNone, moGroupBegin, moLogistic, moCompare, moBit,
    moList, moAddSub, moMulDivMod, moNeg, moNot, moGroupEnd);
  TQErrorHandler = (ehException, ehNull, ehAbort);
  TQMathParams = array of TQMathVar;

  TQMathVar = class
  private

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
    function GetValueEx(const ACallParams: PQEvalData): Variant; virtual;
    procedure SetValue(const Value: Variant); virtual;
    function DoGetValue(const ACallParams: PQEvalData): Variant;
    procedure SetValueEx(const ACallParams: PQEvalData; const Value: Variant);
    function GetCurrentEvalParams: PQEvalData;
    property RefVar: TQMathVar read FRefVar;
  public
    constructor Create(AOwner: TQMathExpression); overload;
    destructor Destroy; override;
    function CheckParamCount: Integer; // 检查参数个数是否符合要求
    procedure Assign(const ASource: TQMathVar); virtual;
    procedure Clear; virtual;
    property Name: String read FName;
    property Volatile: TQMathVolatile read FVolatile;
    property Value: Variant read GetValue write SetValue;
    property ValueEx[const ACallParams: PQEvalData]: Variant read GetValueEx
      write SetValueEx;
    property OnGetValue: TQMathValueEvent read FOnGetValue write FOnGetValue;
    property MinParams: Integer read FMinParams;
    property MaxParams: Integer read FMaxParams;
    property Params: TQMathParams read FParams;
    property Tag: Pointer read FTag write FTag;
    property Owner: TQMathExpression read FOwner;
    property CurrentEvalParams: PQEvalData read GetCurrentEvalParams;
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
    FPasscount: Integer;
    function GetValueEx(const ACallParams: PQEvalData): Variant; override;
    procedure Parse(p: PChar);
  public
    constructor Create(AOwner: TQMathExpression; AName: String;
      const AValue: Variant); overload;
    destructor Destroy; override;
    procedure Clear; override;
  end;

  IQMathExpression = interface
    ['{AF0C22C3-05B5-43DA-8C55-25A633DBC7DE}']
    function Eval(const AExpr: String; AParams: Pointer = nil)
      : Variant; overload;
    function Eval(const AExpr: String; var AResult: Variant;
      AParams: Pointer = nil): Boolean; overload;
    function Eval(const AParams: Pointer = nil): Variant; overload;
    function Parse(const AExpr: String): Boolean;
    function Add(const AVarName: String; const AValue: Variant)
      : TQMathVar; overload;
    function Add(const AVarName: String; AVolatile: TQMathVolatile = mvVolatile)
      : TQMathVar; overload;
    function Add(const AFuncName: String; AMinParams, AMaxParams: Integer;
      ACallback: TQMathValueEvent; AVolatile: TQMathVolatile = mvVolatile)
      : TQMathVar; overload;
    function Add(const AFuncName: String; AMinParams, AMaxParams: Integer;
      ACallback: TQMathValueEventG; AVolatile: TQMathVolatile = mvVolatile)
      : TQMathVar; overload;
    function Add(const AFuncName: String; AMinParams, AMaxParams: Integer;
      ACallback: TQMathValueEventA; AVolatile: TQMathVolatile = mvVolatile)
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
    function GetOnLookupMissed: TQMathExpressVarLookupEvent;
    procedure SetOnLookupMissed(const AValue: TQMathExpressVarLookupEvent);
    function GetOnLookupMissedA: TQMathExpressVarLookupEventA;
    procedure SetOnLookupMissedA(const AValue: TQMathExpressVarLookupEventA);
    function GetOnLookupMissedG: TQMathExpressVarLookupEventG;
    procedure SetOnLookupMissedG(const AValue: TQMathExpressVarLookupEventG);

    function GetTag: Pointer;
    procedure SetTag(const ATag: Pointer);

    property UseStdDiv: Boolean read GetUseStdDiv write SetUseStdDiv;
    property NumIdentAsMultiply: Boolean read GetNumIdentAsMultiply
      write SetNumIdentAsMultiply;
    property Vars[const AIndex: Integer]: TQMathVar read GetVars;
    property VarCount: Integer read GetVarCount;
    property Tag: Pointer read GetTag write SetTag;
    property ErrorCode: Integer read GetErrorCode;
    property ErrorMsg: String read GetErrorMsg;
    property ErrorHandler: TQErrorHandler read GetErrorHandler
      write SetErrorHandler;
    property OnLookupMissed: TQMathExpressVarLookupEvent read GetOnLookupMissed
      write SetOnLookupMissed;
    property OnLookupMissedA: TQMathExpressVarLookupEventA
      read GetOnLookupMissedA write SetOnLookupMissedA;
    property OnLookupMissedG: TQMathExpressVarLookupEventG
      read GetOnLookupMissedG write SetOnLookupMissedG;
  end;

  EQMathExpr = class(Exception)

  end;

  TQMathExpression = class(TInterfacedObject, IQMathExpression)
  private
    FRoot: TQMathExpr;
    FVars: TStringList;
    FTag: Pointer;
    FPasscount: Integer;
    FUseStdDiv: Boolean;
    FNumIdentAsMultiply: Boolean;
    FSavePoint: Integer;
    FErrorCode: Integer;
    FErrorMsg: String;
    FErrorHandler: TQErrorHandler;
    FOnVarLookup: TQMathExpressVarLookupEvent;
  private
    class function DoAdd(const ALeft, ARight: Variant): Variant; static;
    class function DoSub(const ALeft, ARight: Variant): Variant; static;
    class function DoMul(const ALeft, ARight: Variant): Variant; static;
    class function DoDiv(const ALeft, ARight: Variant): Variant; static;
    class function DoStdDiv(const ALeft, ARight: Variant): Variant; static;
    class function DoMod(const ALeft, ARight: Variant): Variant; static;
    class function DoGreatThan(const ALeft, ARight: Variant): Variant; static;
    class function DoGE(const ALeft, ARight: Variant): Variant; static;
    class function DoEqual(const ALeft, ARight: Variant): Variant; static;
    class function DoNotEqual(const ALeft, ARight: Variant): Variant; static;
    class function DoLessThan(const ALeft, ARight: Variant): Variant; static;
    class function DoLE(const ALeft, ARight: Variant): Variant; static;
    // 逻辑运算
    class function DoNot(const ALeft, ARight: Variant): Variant; static;
    class function DoAnd(const ALeft, ARight: Variant): Variant; static;
    class function DoOr(const ALeft, ARight: Variant): Variant; static;
    // 位运算
    class function DoXor(const ALeft, ARight: Variant): Variant; static;
    class function DoBitAnd(const ALeft, ARight: Variant): Variant; static;
    class function DoBitOr(const ALeft, ARight: Variant): Variant; static;
    class function DoBitNot(const ALeft, ARight: Variant): Variant; static;
    class function DoBitLeftShift(const ALeft, ARight: Variant)
      : Variant; static;
    class function DoBitRightShift(const ALeft, ARight: Variant)
      : Variant; static;
    class function DoSmartAnd(const ALeft, ARight: Variant): Variant; static;
    class function DoSmartOr(const ALeft, ARight: Variant): Variant; static;

    // 默认提供的函数
    class procedure DoMax(Sender: TObject; AExpr: TQMathVar;
      const ACallParams: PQEvalData; var AResult: Variant); static;
    class procedure DoMin(Sender: TObject; AExpr: TQMathVar;
      const ACallParams: PQEvalData; var AResult: Variant); static;
    class procedure DoAvg(Sender: TObject; AExpr: TQMathVar;
      const ACallParams: PQEvalData; var AResult: Variant); static;
    class procedure DoSum(Sender: TObject; AExpr: TQMathVar;
      const ACallParams: PQEvalData; var AResult: Variant); static;
    class procedure DoStdDev(Sender: TObject; AExpr: TQMathVar;
      const ACallParams: PQEvalData; var AResult: Variant); static;
    class procedure DoRand(Sender: TObject; AExpr: TQMathVar;
      const ACallParams: PQEvalData; var AResult: Variant); static;
    class procedure DoPow(Sender: TObject; AExpr: TQMathVar;
      const ACallParams: PQEvalData; var AResult: Variant); static;
    class procedure DoIIf(Sender: TObject; AExpr: TQMathVar;
      const ACallParams: PQEvalData; var AResult: Variant); static;
    class procedure DoSin(Sender: TObject; AExpr: TQMathVar;
      const ACallParams: PQEvalData; var AResult: Variant); static;
    class procedure DoCos(Sender: TObject; AExpr: TQMathVar;
      const ACallParams: PQEvalData; var AResult: Variant); static;
    class procedure DoTan(Sender: TObject; AExpr: TQMathVar;
      const ACallParams: PQEvalData; var AResult: Variant); static;
    class procedure DoAcrSin(Sender: TObject; AExpr: TQMathVar;
      const ACallParams: PQEvalData; var AResult: Variant); static;
    class procedure DoArcCos(Sender: TObject; AExpr: TQMathVar;
      const ACallParams: PQEvalData; var AResult: Variant); static;
    class procedure DoArcTan(Sender: TObject; AExpr: TQMathVar;
      const ACallParams: PQEvalData; var AResult: Variant); static;
    class procedure DoArcTan2(Sender: TObject; AExpr: TQMathVar;
      const ACallParams: PQEvalData; var AResult: Variant); static;
    class procedure DoAbs(Sender: TObject; AExpr: TQMathVar;
      const ACallParams: PQEvalData; var AResult: Variant); static;
    class procedure DoTrunc(Sender: TObject; AExpr: TQMathVar;
      const ACallParams: PQEvalData; var AResult: Variant); static;
    class procedure DoFrac(Sender: TObject; AExpr: TQMathVar;
      const ACallParams: PQEvalData; var AResult: Variant); static;
    class procedure DoRound(Sender: TObject; AExpr: TQMathVar;
      const ACallParams: PQEvalData; var AResult: Variant); static;
    class procedure DoCeil(Sender: TObject; AExpr: TQMathVar;
      const ACallParams: PQEvalData; var AResult: Variant); static;
    class procedure DoFloor(Sender: TObject; AExpr: TQMathVar;
      const ACallParams: PQEvalData; var AResult: Variant); static;
    class procedure DoLog(Sender: TObject; AExpr: TQMathVar;
      const ACallParams: PQEvalData; var AResult: Variant); static;
    class procedure DoLn(Sender: TObject; AExpr: TQMathVar;
      const ACallParams: PQEvalData; var AResult: Variant); static;
    class procedure DoSqrt(Sender: TObject; AExpr: TQMathVar;
      const ACallParams: PQEvalData; var AResult: Variant); static;
    class procedure DoFactorial(Sender: TObject; AExpr: TQMathVar;
      const ACallParams: PQEvalData; var AResult: Variant); static;
    class procedure DoExp(Sender: TObject; AExpr: TQMathVar;
      const ACallParams: PQEvalData; var AResult: Variant); static;
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
    function GetOnLookupMissed: TQMathExpressVarLookupEvent;
    procedure SetOnLookupMissed(const AValue: TQMathExpressVarLookupEvent);
    function GetTag: Pointer;
    procedure SetTag(const ATag: Pointer);
    function GetOnLookupMissedA: TQMathExpressVarLookupEventA;
    procedure SetOnLookupMissedA(const AValue: TQMathExpressVarLookupEventA);
    function GetOnLookupMissedG: TQMathExpressVarLookupEventG;
    procedure SetOnLookupMissedG(const AValue: TQMathExpressVarLookupEventG);
    function DoLookupVar(const AVarName: String): TQMathVar;
  public
    constructor Create;
    destructor Destroy; override;
    function Eval(const AExpr: String; AParams: Pointer = nil)
      : Variant; overload;
    function Eval(const AExpr: String; var AResult: Variant;
      AParams: Pointer = nil): Boolean; overload;
    function Eval(const AParams: Pointer = nil): Variant; overload;
    function Parse(const AExpr: String): Boolean;
    function Add(const AVarName: String; const AValue: Variant)
      : TQMathVar; overload;
    function Add(const AVarName: String; AVolatile: TQMathVolatile = mvVolatile)
      : TQMathVar; overload;
    function Add(const AFuncName: String; AMinParams, AMaxParams: Integer;
      ACallback: TQMathValueEvent; AVolatile: TQMathVolatile = mvVolatile)
      : TQMathVar; overload;
    function Add(const AFuncName: String; AMinParams, AMaxParams: Integer;
      ACallback: TQMathValueEventG; AVolatile: TQMathVolatile = mvVolatile)
      : TQMathVar; overload;
    function Add(const AFuncName: String; AMinParams, AMaxParams: Integer;
      ACallback: TQMathValueEventA; AVolatile: TQMathVolatile = mvVolatile)
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

threadvar ThreadEvalData: PQEvalData;

const
  // %&'()*+,-./
  MathFunctions: array ['!' .. '>'] of TQMathFunctionItem = ( //
    // !
    (Method: TQMathExpression.DoNot; Op: '!'; Pri: moNot),
    // "
    (Method: nil; Op: #0; Pri: moNone),
    // #
    (Method: nil; Op: #0; Pri: moNone),
    // $
    (Method: nil; Op: #0; Pri: moNone),
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
    (Method: TQMathExpression.DoDiv; Op: '/'; Pri: moMulDivMod), (Method: nil;
    Op: #0; Pri: moNone), // 0
    (Method: nil; Op: #0; Pri: moNone), // 1
    (Method: nil; Op: #0; Pri: moNone), // 2
    (Method: nil; Op: #0; Pri: moNone), // 3
    (Method: nil; Op: #0; Pri: moNone), // 4
    (Method: nil; Op: #0; Pri: moNone), // 5
    (Method: nil; Op: #0; Pri: moNone), // 6
    (Method: nil; Op: #0; Pri: moNone), // 7
    (Method: nil; Op: #0; Pri: moNone), // 8
    (Method: nil; Op: #0; Pri: moNone), // 9
    (Method: nil; Op: #0; Pri: moNone), // :
    (Method: nil; Op: #0; Pri: moNone), // ;
    (Method: nil; Op: '<'; Pri: moCompare), // <
    (Method: nil; Op: '='; Pri: moCompare), // ==
    (Method: nil; Op: '>'; Pri: moCompare) // >
    );

type
  TProcA = reference to procedure;
  TProcG = procedure;
  TProcE = procedure of object;

function CopyMethod(const AMethod: TMethod): TMethod;
begin
  Result.Code := nil;
  Result.Data := nil;
  if AMethod.Data = Pointer(-1) then
  begin
    TProcA(Result.Code) := TProcA(AMethod.Code);
    Result.Data := Pointer(-1);
  end
  else if AMethod.Data = nil then
  begin
    Result.Data := nil;
    Result.Code := AMethod.Code
  end
  else
    TProcE(Result) := TProcE(AMethod);
end;

procedure ClearMethod(var AMethod: TMethod);
begin
  if AMethod.Data = Pointer(-1) then
    TProcA(AMethod.Code) := nil
  else if AMethod.Data = nil then
    TProcG(AMethod.Code) := nil
  else
    TProcE(AMethod) := nil;
end;
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
      ACallback(Self, Result, nil, Result.FValue);
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

function TQMathExpression.Add(const AFuncName: String;
  AMinParams, AMaxParams: Integer; ACallback: TQMathValueEventA;
  AVolatile: TQMathVolatile): TQMathVar;
var
  AEvent: TQMathValueEvent;
  AMethod: TMethod absolute AEvent;
begin
  AMethod.Data := Pointer(-1);
  AMethod.Code := nil;
  TQMathValueEventA(AMethod.Code) := ACallback;
  Result := Add(AFuncName, AMinParams, AMaxParams, AEvent, AVolatile);
end;

function TQMathExpression.Add(const AFuncName: String;
  AMinParams, AMaxParams: Integer; ACallback: TQMathValueEventG;
  AVolatile: TQMathVolatile): TQMathVar;
var
  AEvent: TQMathValueEvent;
  AMethod: TMethod absolute AEvent;
begin
  AMethod.Data := nil;
  AMethod.Code := nil;
  TQMathValueEventG(AMethod.Code) := ACallback;
  Result := Add(AFuncName, AMinParams, AMaxParams, AEvent, AVolatile);
end;

function TQMathExpression.Add(const AVarName: String; const AValue: Variant)
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
    Result.FVolatile := mvImmutable;
    FVars.InsertObject(AIdx, AVarName, Result);
  end;
  Result.Value := AValue;
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
  ClearMethod(TMethod(FOnVarLookup));
  Clear;
  FreeAndNil(FVars);
  FreeAndNil(FRoot);
  inherited;
end;

class procedure TQMathExpression.DoAbs(Sender: TObject; AExpr: TQMathVar;
  const ACallParams: PQEvalData; var AResult: Variant);
begin
  AResult := abs(AExpr.Params[0].GetValueEx(ACallParams));
end;

class procedure TQMathExpression.DoAcrSin(Sender: TObject; AExpr: TQMathVar;
  const ACallParams: PQEvalData; var AResult: Variant);
begin
  AResult := arcsin(AExpr.Params[0].GetValueEx(ACallParams));
end;

class function TQMathExpression.DoAdd(const ALeft, ARight: Variant): Variant;
begin
  if VarIsStr(ALeft) or VarIsStr(ARight) then
    Result := VarToStr(ALeft) + VarToStr(ARight)
  else
    Result := ALeft + ARight;
end;

class function TQMathExpression.DoAnd(const ALeft, ARight: Variant): Variant;
begin
  Result := (ALeft <> 0) and (ARight <> 0);
end;

class procedure TQMathExpression.DoArcCos(Sender: TObject; AExpr: TQMathVar;
  const ACallParams: PQEvalData; var AResult: Variant);
begin
  AResult := arccos(AExpr.Params[0].GetValueEx(ACallParams));
end;

class procedure TQMathExpression.DoArcTan(Sender: TObject; AExpr: TQMathVar;
  const ACallParams: PQEvalData; var AResult: Variant);
begin
  AResult := arctan(AExpr.Params[0].GetValueEx(ACallParams));
end;

class procedure TQMathExpression.DoArcTan2(Sender: TObject; AExpr: TQMathVar;
  const ACallParams: PQEvalData; var AResult: Variant);
begin
  AResult := arctan2(AExpr.Params[0].GetValueEx(ACallParams),
    AExpr.Params[1].GetValueEx(ACallParams));
end;

class function TQMathExpression.DoDiv(const ALeft, ARight: Variant): Variant;
begin
  if (VarType(ALeft) in [varSingle, varDouble]) or
    (VarType(ARight) in [varSingle, varDouble]) then
    Result := ALeft / ARight
  else
    Result := ALeft div ARight;
end;

class function TQMathExpression.DoEqual(const ALeft, ARight: Variant): Variant;
begin
  Result := ALeft = ARight;
end;

class procedure TQMathExpression.DoExp(Sender: TObject; AExpr: TQMathVar;
  const ACallParams: PQEvalData; var AResult: Variant);
begin
  AResult := Exp(AExpr.Params[0].GetValueEx(ACallParams));
end;

class procedure TQMathExpression.DoFactorial(Sender: TObject; AExpr: TQMathVar;
  const ACallParams: PQEvalData; var AResult: Variant);
var
  V, R: Int64;
begin
  V := AExpr.Params[0].GetValueEx(ACallParams);
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
  AResult := R;
end;

class procedure TQMathExpression.DoFloor(Sender: TObject; AExpr: TQMathVar;
  const ACallParams: PQEvalData; var AResult: Variant);
begin
  AResult := Floor(AExpr.Params[0].GetValueEx(ACallParams));
end;

class procedure TQMathExpression.DoFrac(Sender: TObject; AExpr: TQMathVar;
  const ACallParams: PQEvalData; var AResult: Variant);
begin
  AResult := Frac(AExpr.Params[0].GetValueEx(ACallParams));
end;

class function TQMathExpression.DoGE(const ALeft, ARight: Variant): Variant;
begin
  Result := ALeft >= ARight;
end;

class function TQMathExpression.DoGreatThan(const ALeft,
  ARight: Variant): Variant;
begin
  Result := ALeft > ARight;
end;

class procedure TQMathExpression.DoIIf(Sender: TObject; AExpr: TQMathVar;
  const ACallParams: PQEvalData; var AResult: Variant);
begin
  if AExpr.Params[0].Value <> 0 then
    AResult := AExpr.Params[1].GetValueEx(ACallParams)
  else
    AResult := AExpr.Params[2].GetValueEx(ACallParams);
end;

class function TQMathExpression.DoLE(const ALeft, ARight: Variant): Variant;
begin
  Result := ALeft <= ARight;
end;

class function TQMathExpression.DoLessThan(const ALeft,
  ARight: Variant): Variant;
begin
  Result := ALeft < ARight;
end;

class procedure TQMathExpression.DoLn(Sender: TObject; AExpr: TQMathVar;
  const ACallParams: PQEvalData; var AResult: Variant);
begin
  AResult := Ln(AExpr.Params[0].GetValueEx(ACallParams));
end;

class procedure TQMathExpression.DoLog(Sender: TObject; AExpr: TQMathVar;
  const ACallParams: PQEvalData; var AResult: Variant);
begin
  if Length(AExpr.Params) = 2 then
    AResult := LogN(AExpr.Params[0].GetValueEx(ACallParams),
      AExpr.Params[1].GetValueEx(ACallParams))
  else
    AResult := Log10(AExpr.Params[0].GetValueEx(ACallParams));
end;

function TQMathExpression.DoLookupVar(const AVarName: String): TQMathVar;
begin
  Result := nil;
  if Assigned(FOnVarLookup) then
  begin
    case IntPtr(TMethod(FOnVarLookup).Data) of
      0:
        TQMathExpressVarLookupEventG(TMethod(FOnVarLookup).Code)
          (Self, AVarName, Result);
      -1:
        TQMathExpressVarLookupEventA(TMethod(FOnVarLookup).Code)
          (Self, AVarName, Result)
    else
      FOnVarLookup(Self, AVarName, Result);
    end;
  end;
end;

class function TQMathExpression.DoMul(const ALeft, ARight: Variant): Variant;
begin
  Result := ALeft * ARight;
end;

class function TQMathExpression.DoNot(const ALeft, ARight: Variant): Variant;
begin
  Result := ARight <> 0;
end;

class function TQMathExpression.DoNotEqual(const ALeft,
  ARight: Variant): Variant;
begin
  Result := ALeft <> ARight;
end;

class function TQMathExpression.DoOr(const ALeft, ARight: Variant): Variant;
begin
  Result := (ALeft <> 0) or (ARight <> 0);
end;

class procedure TQMathExpression.DoPow(Sender: TObject; AExpr: TQMathVar;
  const ACallParams: PQEvalData; var AResult: Variant);
begin
  AResult := Power(AExpr.Params[0].GetValueEx(ACallParams),
    AExpr.Params[1].GetValueEx(ACallParams));
end;

class procedure TQMathExpression.DoRand(Sender: TObject; AExpr: TQMathVar;
  const ACallParams: PQEvalData; var AResult: Variant);
begin
  if Length(AExpr.Params) = 1 then
    AResult := Random(AExpr.Params[0].GetValueEx(ACallParams))
  else
    AResult := Random(MaxInt);
end;

class procedure TQMathExpression.DoRound(Sender: TObject; AExpr: TQMathVar;
  const ACallParams: PQEvalData; var AResult: Variant);
begin
  if Length(AExpr.Params) = 2 then
    AResult := SimpleRoundTo(AExpr.Params[0].GetValueEx(ACallParams),
      AExpr.Params[1].GetValueEx(ACallParams))
  else
    AResult := SimpleRoundTo(AExpr.Params[0].GetValueEx(ACallParams), 0);
end;

class procedure TQMathExpression.DoSin(Sender: TObject; AExpr: TQMathVar;
  const ACallParams: PQEvalData; var AResult: Variant);
begin
  AResult := sin(AExpr.Params[0].GetValueEx(ACallParams));
end;

class function TQMathExpression.DoSmartAnd(const ALeft,
  ARight: Variant): Variant;
begin
  if VarIsType(ALeft, varBoolean) and VarIsType(ARight, varBoolean) then
    Result := (ALeft = True) and (ARight = True)
  else
    Result := ALeft and ARight;
end;

class function TQMathExpression.DoSmartOr(const ALeft, ARight: Variant)
  : Variant;
begin
  if VarIsType(ALeft, varBoolean) and VarIsType(ARight, varBoolean) then
    Result := (ALeft = True) or (ARight = True)
  else
    Result := ALeft or ARight;
end;

class procedure TQMathExpression.DoSqrt(Sender: TObject; AExpr: TQMathVar;
  const ACallParams: PQEvalData; var AResult: Variant);
begin
  AResult := sqrt(AExpr.Params[0].GetValueEx(ACallParams));
end;

class procedure TQMathExpression.DoStdDev(Sender: TObject; AExpr: TQMathVar;
  const ACallParams: PQEvalData; var AResult: Variant);
var
  AValues: array of Double;
  I: Integer;
begin
  SetLength(AValues, Length(AExpr.Params));
  for I := 0 to High(AExpr.Params) do
    AValues[I] := AExpr.Params[I].GetValueEx(ACallParams);
  AResult := StdDev(AValues);
end;

class function TQMathExpression.DoStdDiv(const ALeft, ARight: Variant): Variant;
begin
  Result := ALeft / Double(ARight);
end;

class function TQMathExpression.DoSub(const ALeft, ARight: Variant): Variant;
begin
  Result := ALeft - ARight;
end;

function TQMathExpression.Eval(const AParams: Pointer): Variant;
var
  ACallParams: TQEvalData;
  procedure RemoveLink;
  var
    AItem, APrior: PQEvalData;
  begin
    AItem := ThreadEvalData;
    APrior := nil;
    while Assigned(AItem) do
    begin
      if AItem = @ACallParams then
      begin
        if Assigned(APrior) then
          APrior.Next := AItem.Next
        else
          ThreadEvalData := AItem.Next;
        Break;
      end;
      APrior := AItem;
      AItem := AItem.Next;
    end;
  end;

begin
  ACallParams.PassCount := AtomicIncrement(FPasscount);
  ACallParams.Params := AParams;
  ACallParams.Expression := Self;
  ACallParams.Next := ThreadEvalData;
  ThreadEvalData := @ACallParams;
  Result := FRoot.GetValueEx(@ACallParams);
  RemoveLink;
end;

function TQMathExpression.Find(const AVarName: String): TQMathVar;
var
  AIdx: Integer;
begin
  if FVars.Find(AVarName, AIdx) then
    Result := FVars.Objects[AIdx] as TQMathVar
  else
    Result := DoLookupVar(AVarName);
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

function TQMathExpression.GetOnLookupMissed: TQMathExpressVarLookupEvent;
begin
  Result := FOnVarLookup;
end;

function TQMathExpression.GetOnLookupMissedA: TQMathExpressVarLookupEventA;
begin
  if TMethod(FOnVarLookup).Data = Pointer(-1) then
    Result := TQMathExpressVarLookupEventA(TMethod(FOnVarLookup).Code)
  else
    Result := nil;
end;

function TQMathExpression.GetOnLookupMissedG: TQMathExpressVarLookupEventG;
begin
  if TMethod(FOnVarLookup).Data = nil then
    Result := TQMathExpressVarLookupEventG(TMethod(FOnVarLookup).Code)
  else
    Result := nil;
end;

function TQMathExpression.GetTag: Pointer;
begin
  Result := FTag;
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

function TQMathExpression.Eval(const AExpr: String; AParams: Pointer): Variant;
begin
  try
    FRoot.Parse(PChar(AExpr));
    Result := Eval(AParams);
  except
    on E: EAbort do
    begin
      if FErrorHandler = ehNull then
        Result := Null
      else if FErrorHandler = ehAbort then
      begin
        E.Message := FErrorMsg;
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

procedure TQMathExpression.SetOnLookupMissed(const AValue
  : TQMathExpressVarLookupEvent);
begin
  FOnVarLookup := AValue;
end;

procedure TQMathExpression.SetOnLookupMissedA(const AValue
  : TQMathExpressVarLookupEventA);
begin
  ClearMethod(TMethod(FOnVarLookup));
  TQMathExpressVarLookupEventA(TMethod(FOnVarLookup).Code) := AValue;
  Pointer(TMethod(FOnVarLookup).Data) := Pointer(-1);
end;

procedure TQMathExpression.SetOnLookupMissedG(const AValue
  : TQMathExpressVarLookupEventG);
begin
  ClearMethod(TMethod(FOnVarLookup));
  TQMathExpressVarLookupEventG(TMethod(FOnVarLookup).Code) := AValue;
end;

procedure TQMathExpression.SetTag(const ATag: Pointer);
begin
  FTag := ATag;
end;

procedure TQMathExpression.SetUseStdDiv(const AValue: Boolean);
begin
  FUseStdDiv := AValue;
end;

class procedure TQMathExpression.DoAvg(Sender: TObject; AExpr: TQMathVar;
  const ACallParams: PQEvalData; var AResult: Variant);
var
  I: Integer;
begin
  AResult := 0;
  for I := 0 to High(AExpr.Params) do
    AResult := AResult + AExpr.Params[I].GetValueEx(ACallParams);
  AResult := AResult / Length(AExpr.Params);
end;

class function TQMathExpression.DoBitAnd(const ALeft, ARight: Variant): Variant;
var
  VL, VR: Int64;
begin
  VL := ALeft;
  VR := ARight;
  Result := VL and VR;
end;

class function TQMathExpression.DoBitLeftShift(const ALeft,
  ARight: Variant): Variant;
begin
  Result := ALeft shl ARight;
end;

class function TQMathExpression.DoBitNot(const ALeft, ARight: Variant): Variant;
var
  VR: Int64;
begin
  VR := ARight;
  Result := not VR;
end;

class function TQMathExpression.DoBitOr(const ALeft, ARight: Variant): Variant;
var
  VL, VR: Int64;
begin
  VL := ALeft;
  VR := ARight;
  Result := VL or VR;
end;

class function TQMathExpression.DoBitRightShift(const ALeft,
  ARight: Variant): Variant;
begin
  Result := ALeft shr ARight;
end;

class procedure TQMathExpression.DoCeil(Sender: TObject; AExpr: TQMathVar;
  const ACallParams: PQEvalData; var AResult: Variant);
begin
  AResult := Ceil(AExpr.Params[0].Value);
end;

class procedure TQMathExpression.DoCos(Sender: TObject; AExpr: TQMathVar;
  const ACallParams: PQEvalData; var AResult: Variant);
begin
  AResult := cos(AExpr.Params[0].Value);
end;

class procedure TQMathExpression.DoMax(Sender: TObject; AExpr: TQMathVar;
  const ACallParams: PQEvalData; var AResult: Variant);
var
  I: Integer;
  AValue: Variant;
begin
  AResult := AExpr.Params[0].Value;
  for I := 1 to High(AExpr.Params) do
  begin
    AValue := AExpr.Params[I].Value;
    if AValue > AResult then
      AResult := AValue;
  end;
end;

class procedure TQMathExpression.DoMin(Sender: TObject; AExpr: TQMathVar;
  const ACallParams: PQEvalData; var AResult: Variant);
var
  I: Integer;
  AValue: Variant;
begin
  AResult := AExpr.Params[0].Value;
  for I := 1 to High(AExpr.Params) do
  begin
    AValue := AExpr.Params[I].GetValueEx(ACallParams);
    if AValue < AResult then
      AResult := AValue;
  end;
end;

class function TQMathExpression.DoMod(const ALeft, ARight: Variant): Variant;
begin
  Result := ALeft mod ARight;
end;

class procedure TQMathExpression.DoSum(Sender: TObject; AExpr: TQMathVar;
  const ACallParams: PQEvalData; var AResult: Variant);
var
  I: Integer;
begin
  AResult := 0;
  for I := 0 to High(AExpr.Params) do
    AResult := AResult + AExpr.Params[I].GetValueEx(ACallParams);
end;

class procedure TQMathExpression.DoTan(Sender: TObject; AExpr: TQMathVar;
  const ACallParams: PQEvalData; var AResult: Variant);
begin
  AResult := tan(AExpr.Params[0].GetValueEx(ACallParams));
end;

class procedure TQMathExpression.DoTrunc(Sender: TObject; AExpr: TQMathVar;
  const ACallParams: PQEvalData; var AResult: Variant);
begin
  AResult := Trunc(AExpr.Params[0].GetValueEx(ACallParams));
end;

class function TQMathExpression.DoXor(const ALeft, ARight: Variant): Variant;
var
  VL, VR: Int64;
begin
  VL := ALeft;
  VR := ARight;
  Result := VL xor VR;
end;

function TQMathExpression.Eval(const AExpr: String; var AResult: Variant;
  AParams: Pointer): Boolean;
begin
  try
    FRoot.Parse(PChar(AExpr));
    AResult := Eval(AParams);
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
  FOnGetValue := TQMathValueEvent(CopyMethod(TMethod(ASource.OnGetValue)));
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
  ClearMethod(TMethod(FOnGetValue));
  Clear;
  inherited;
end;

function TQMathVar.DoGetValue(const ACallParams: PQEvalData): Variant;
begin
  Result := Unassigned;
  if Assigned(FOnGetValue) then
  begin
    case IntPtr(TMethod(FOnGetValue).Data) of
      0:
        TQMathValueEventG(TMethod(FOnGetValue).Code)
          (FOwner, Self, ACallParams, Result);
      -1:
        TQMathValueEventA(TMethod(FOnGetValue).Code)
          (FOwner, Self, ACallParams, Result)
    else
      FOnGetValue(FOwner, Self, ACallParams, Result);
    end;
  end;
end;

function TQMathVar.GetCurrentEvalParams: PQEvalData;
begin
  Result := ThreadEvalData;
  while Assigned(Result) do
  begin
    if Result.Expression = Owner then
      Break
    else
      Result := Result.Next;
  end;
end;

function TQMathVar.GetValue: Variant;
begin
  Result := GetValueEx(CurrentEvalParams);
end;

function TQMathVar.GetValueEx(const ACallParams: PQEvalData): Variant;
var
  AParams: TQMathParams;
begin
  case FVolatile of
    mvStable: // Stable 在一次计算中值是不变的，所以用引用的原始变量调用一次
      begin
        if FPasscount <> ACallParams.PassCount then
        begin
          FPasscount := ACallParams.PassCount;
          if Assigned(FRefVar) then
          begin
            AParams := FRefVar.Params;
            FRefVar.FParams := FParams;
            try
              Result := FRefVar.DoGetValue(ACallParams);
            finally
              FRefVar.FParams := AParams;
            end;
          end
          else
            Result := DoGetValue(ACallParams);
          // 缓存末次计算的值
          FValue := Result;
        end
        else
          Result := FValue;
      end;
    mvVolatile:
      begin
        Result := DoGetValue(ACallParams);
      end
  else
    Result := FValue;
  end;
end;

procedure TQMathVar.SetValue(const Value: Variant);
begin
  FValue := Value;
end;

procedure TQMathVar.SetValueEx(const ACallParams: PQEvalData;
  const Value: Variant);
begin
  FValue := Value;
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

constructor TQMathExpr.Create(AOwner: TQMathExpression; AName: String;
  const AValue: Variant);
begin
  inherited Create(AOwner);
  FName := AName;
  FValue := AValue;
  FVolatile := mvImmutable;
end;

destructor TQMathExpr.Destroy;
begin
  Clear;
  inherited;
end;

function TQMathExpr.GetValueEx(const ACallParams: PQEvalData): Variant;
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
          else if OpCode = '#' then // 负值
            AStacks[ATop] := -AStacks[ATop]
          else
          begin
            case OpCode of
              'G': // >=
                AStacks[ATop - 1] := TQMathExpression.DoGE(AStacks[ATop - 1],
                  AStacks[ATop]);
              'g': // >
                AStacks[ATop - 1] := TQMathExpression.DoGreatThan
                  (AStacks[ATop - 1], AStacks[ATop]);
              '=':
                AStacks[ATop - 1] := TQMathExpression.DoEqual(AStacks[ATop - 1],
                  AStacks[ATop]);
              'L': // <=
                AStacks[ATop - 1] := TQMathExpression.DoLE(AStacks[ATop - 1],
                  AStacks[ATop]);
              'l': // <
                AStacks[ATop - 1] := TQMathExpression.DoLessThan
                  (AStacks[ATop - 1], AStacks[ATop]);
              'n': // <>
                AStacks[ATop - 1] := TQMathExpression.DoNotEqual
                  (AStacks[ATop - 1], AStacks[ATop]);
              '!': // 单目操作
                AStacks[ATop] := TQMathExpression.DoNot(Null, AStacks[ATop]);
              '|': // 或
                AStacks[ATop - 1] := (AStacks[ATop - 1] <> 0) or
                  (AStacks[ATop] <> 0);
              '\': // 位或
                AStacks[ATop - 1] := TQMathExpression.DoBitOr(AStacks[ATop - 1],
                  AStacks[ATop]);
              '&': // 与
                AStacks[ATop - 1] := (AStacks[ATop - 1] <> 0) and
                  (AStacks[ATop] <> 0);
              '@': // 位与
                AStacks[ATop - 1] := TQMathExpression.DoBitAnd
                  (AStacks[ATop - 1], AStacks[ATop]);
              '~':
                AStacks[ATop] := TQMathExpression.DoBitNot(Null, AStacks[ATop]);
              '^':
                AStacks[ATop - 1] := TQMathExpression.DoXor(AStacks[ATop - 1],
                  AStacks[ATop]);
              ':': // 或，逻辑或智能检测，用于支持 or
                AStacks[ATop - 1] := TQMathExpression.DoSmartOr
                  (AStacks[ATop - 1], AStacks[ATop]);
              '$': // 与，逻辑与智能检查，用于支持 and
                AStacks[ATop - 1] := TQMathExpression.DoSmartAnd
                  (AStacks[ATop - 1], AStacks[ATop]);
              '<': // 左移位操作符
                AStacks[ATop - 1] := TQMathExpression.DoBitLeftShift
                  (AStacks[ATop - 1], AStacks[ATop]);
              '>': // 右移位操作符
                AStacks[ATop - 1] := TQMathExpression.DoBitRightShift
                  (AStacks[ATop - 1], AStacks[ATop])
            else
              AStacks[ATop - 1] := MathFunctions[OpCode]
                .Method(AStacks[ATop - 1], AStacks[ATop]);
            end;
            Dec(ATop);
          end;
        end
        else
        begin
          Inc(ATop);
          AStacks[ATop] := Expr.GetValueEx(ACallParams);
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
function ProcessOpToken(const AToken: String): Boolean; forward;
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

  function DetectToken(AToken: String): TQMathExpr;
  var
    pt, pe: PChar;
    AValue: TVarData;
    ATemp: TQMathVar;
  begin
    pt := PChar(AToken);
    pe := pt + Length(AToken) - 1;
    while (pe >= pt) and CharIn(pe^, SpaceChars) do
      Dec(pe);
    SetLength(AToken, pe - pt + 1);
    pt := PChar(AToken);
    AValue.VInt64 := 0;
    if (pt^ = '"') or (pt^ = '''') then
      Result := TQMathExpr.Create(FOwner, AToken, AnsiExtractQuotedStr(pt, pt^))
    else if TryStrToInt64(AToken, AValue.VInt64) then
      Result := TQMathExpr.Create(FOwner, AToken, AValue.VInt64)
    else if TryStrToFloat(AToken, AValue.VDouble) then
      Result := TQMathExpr.Create(FOwner, AToken, AValue.VDouble)
    else if TryStrToBool(AToken, PBoolean(@AValue.VBoolean)^) then
      Result := TQMathExpr.Create(FOwner, AToken, AValue.VBoolean)
    else if Length(AToken) > 0 then
    begin
      if not ProcessOpToken(AToken) then
      begin
        ATemp := FOwner.Find(AToken);
        if not Assigned(ATemp) then
          FOwner.SetLastError(EVarNotFound, Format(SVarNotFound, [AToken]));
        Result := TQMathExpr.Create(FOwner);
        Result.Assign(ATemp);
        Result.FRefVar := ATemp;
      end
      else
        Result := nil;
    end
    else
      Result := TQMathExpr.Create(FOwner, AToken, Unassigned)
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
      '!', '#', '~':
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
    Result := ((p^ >= MathFunctions[Low(MathFunctions)].Op) and
      (p^ <= MathFunctions[High(MathFunctions)].Op) and
      (MathFunctions[p^].Pri <> moNone)) or CharIn(p^, '|&~^');
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
  function ProcessOpToken(const AToken: String): Boolean;
  begin
    Result := True;
    if CompareText(AToken, 'or') = 0 then
      PushOp(':', moBit) // 智能检查，这里按位运算的优先级处理
    else if CompareText(AToken, 'xor') = 0 then
      PushOp('^', moBit)
    else if CompareText(AToken, 'and') = 0 then
      PushOp('$', moBit)
    else if CompareText(AToken, 'shl') = 0 then
      PushOp('<', moBit)
    else if CompareText(AToken, 'shr') = 0 then
      PushOp('>', moBit)
    else
      Result := false;
  end;
  procedure ProcessOpCode;
  var
    AOp: Char;
    APri: TQMathOprPriority;
  begin
    if (p^ = '+') or (p^ = '-') then
    begin
      if (AOprTop = 0) and (AVarTop = -1) then
      begin
        if p^ = '-' then
          PushOp('#', moNeg);
        Inc(p);
        ps := SkipSpace(p);
        Exit;
      end;
    end;
    if p^ = '|' then
    begin
      Inc(p);
      if p^ = '|' then
      begin
        PushOp('|', moLogistic);
        Inc(p);
      end
      else // 位或
        PushOp('\', moBit);
      ps := SkipSpace(p);
      Exit;
    end
    else if p^ = '&' then
    begin
      Inc(p);
      if p^ = '&' then
      begin
        PushOp('&', moLogistic);
        Inc(p);
      end
      else
        PushOp('@', moBit);
      ps := SkipSpace(p);
      Exit;
    end
    else if p^ = '^' then // 位异或
    begin
      Inc(p);
      PushOp('^', moBit);
      ps := SkipSpace(p);
      Exit;
    end
    else if p^ = '~' then
    begin
      Inc(p);
      PushOp('~', moBit);
      ps := SkipSpace(p);
      Exit;
    end;
    with MathFunctions[p^] do
    begin
      APri := Pri;
      if Pri = moCompare then // 比较操作符
      begin
        if p^ = '<' then
        begin
          Inc(p);
          if p^ = '=' then
          begin
            AOp := 'L';
            Inc(p);
          end
          else if p^ = '>' then
          begin
            AOp := 'n';
            Inc(p);
          end
          else if p^ = '<' then
          begin
            APri := moBit;
            AOp := '<';
            Inc(p);
          end
          else
            AOp := 'l';
        end
        else if p^ = '>' then
        begin
          Inc(p);
          if p^ = '=' then
          begin
            AOp := 'G';
            Inc(p);
          end
          else if p^ = '>' then
          begin
            AOp := '>';
            Inc(p);
            APri := moBit;
          end
          else
            AOp := 'g';
        end
        else
        begin
          AOp := '=';
          Inc(p);
          // 将 = 和 == 都解释成等于
          if p^ = '=' then
            Inc(p);
        end;
      end
      else if p^ = '!' then
      begin
        AOp := p^;
        Inc(p);
        if p^ = '=' then
        begin
          AOp := 'n';
          Inc(p);
        end;
      end
      else
      begin
        AOp := p^;
        Inc(p);
      end;
      if AOp = ')' then
      begin
        while AOprStacks[AOprTop].Pri > moGroupBegin do
        begin
          PushVar(nil, AOprStacks[AOprTop].OpCode);
          Dec(AOprTop);
        end;
        Dec(AOprTop);
      end
      else if (APri > AOprStacks[AOprTop].Pri) or (AOp = '(') then
      begin
        PushOp(AOp, APri);
      end
      else
      begin
        while AOprStacks[AOprTop].Pri >= APri do
        begin
          PushVar(nil, AOprStacks[AOprTop].OpCode);
          Dec(AOprTop);
        end;
        PushOp(AOp, APri);
      end;
      ps := SkipSpace(p);
      // 一个运算符后面跟+-肯定是正负号，直接处理
      if (Op <> ')') and ((p^ = '+') or (p^ = '-')) then
      begin
        if p^ = '-' then
        begin
          PushOp('#', moNeg);
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
            if (OpCode <> '!') and (OpCode <> '#') then
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
  procedure SkipText(var p: PChar);
  var
    ps: PChar;
  begin
    if CharIn(p^, '''"') then
    begin
      ps := p;
      Inc(p);
      while p^ <> #0 do
      begin
        if p^ = ps^ then
        begin
          Inc(p);
          if p^ = ps^ then
            Inc(p)
          else
            Break;
        end;
        Inc(p);
      end;
    end;
  end;
  procedure DirectParse;
  var
    AToken: String;
    AExpr: TQMathExpr;
  begin
    Clear;
    SetLength(FStacks, 32);
    SetLength(AOprStacks, 32);
    AVarTop := -1;
    AOprTop := 0;
    ps := SkipSpace(p);
    SkipText(p);
    while p^ <> #0 do
    begin
      if NextIsOperator then
      begin
        if ps < p then
        begin
          AToken := Copy(ps, 0, p - ps);
          AExpr := DetectToken(AToken);
          if Assigned(AExpr) then
          begin
            ALastExpr := AExpr;
            PushVar(ALastExpr, #0);
            if (ALastExpr.Volatile <> mvImmutable) and (p^ = '(') then
            begin
              DecodeParams(ALastExpr);
              ps := SkipSpace(p);
              SkipText(p);
              continue;
            end;
          end
          else
          begin
            ps := SkipSpace(p);
            SkipText(p);
            continue;
          end;
        end;
        ProcessOpCode;
      end
      else if CharIn(p^, ' '#9#10#13) then
      begin
        AToken := Copy(ps, 0, p - ps);
        AExpr := DetectToken(AToken);
        if Assigned(AExpr) then
        begin
          ALastExpr := AExpr;
          PushVar(ALastExpr, #0);
        end;
        ps := SkipSpace(p);
        SkipText(p);
      end
      else
        Inc(p);
    end;
    if p > ps then
    begin
      AToken := Copy(ps, 0, p - ps);
      AExpr := DetectToken(AToken);
      if Assigned(AExpr) then
      begin
        ALastExpr := AExpr;
        PushVar(ALastExpr, #0);
      end;
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
      (p^ = '_') or
{$IFDEF UNICODE}(p^ > #$7F) {$ELSE} (p^ < 0){$ENDIF};
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
