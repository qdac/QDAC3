unit qdac_ani;

interface

uses Classes, Sysutils, Math, Rtti, UiTypes, TypInfo;

type
  TQAnimationType = (&In, &Out, InOut);
  TQInterpolationType = (Linear, Quadratic, Cubic, Quartic, Quintic, Sinusoidal, Exponential, Circular, Elastic, Back, Bounce);
  TQAnimationEvent = (Starting, Started, Stopping, Stopped, NextLoop, Disabled, Enabled, Paused, Resumed, GotoTime, EnterDelay,
    ExitDelay);

  IQAnimation = interface
    ['{799E4AB7-E28A-4A8E-9E2F-DEEAE5753B3F}']
    function GetType: TQAnimationType;
    procedure SetType(const AType: TQAnimationType);
    function GetInterpolation: TQInterpolationType;
    procedure SetInterpolation(const AType: TQInterpolationType);
    function GetDuration: Single;
    procedure SetDuration(const AValue: Single);
    procedure Start;
    procedure Stop;
    procedure Pause;
    procedure Resume;
    function GetPaused: Boolean;
    function GetInverse: Boolean;
    procedure SetInverse(const AValue: Boolean);
    function GetNormalizedTime: Single;
    function GetCurrentTime: Single;
    procedure SetCurrentTime(const AValue: Single);
    function GetData: Pointer;
    procedure SetData(const AData: Pointer);
    function GetEnabled: Boolean;
    procedure SetEnabled(AValue: Boolean);
    function GetLoop: Boolean;
    procedure SetLoop(const AValue: Boolean);
    function GetLoopCount: Integer;
    function GetMaxLoops: Integer;
    procedure SetMaxLoops(AValue: Integer);
    function GetAutoReverse: Boolean;
    procedure SetAutoReverse(AValue: Boolean);
    function GetDelayTime: Single;
    procedure SetDelayTime(const AValue: Single);
    function GetEscapedTime: Single;
    procedure SetEscapedTime(const AValue: Single);
    function GetLastEvent: TQAnimationEvent;
    property AnimationType: TQAnimationType read GetType write SetType;
    property Duration: Single read GetDuration write SetDuration;
    property Interpolation: TQInterpolationType read GetInterpolation write SetInterpolation;
    property Paused: Boolean read GetPaused;
    property Inverse: Boolean read GetInverse write SetInverse;
    property AutoReverse: Boolean read GetAutoReverse write SetAutoReverse;
    property Loop: Boolean read GetLoop write SetLoop;
    property NormalizedTime: Single read GetNormalizedTime;
    property CurrentTime: Single read GetCurrentTime write SetCurrentTime;
    property EscapedTime: Single read GetEscapedTime write SetEscapedTime;
    property Data: Pointer read GetData write SetData;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property LoopCount: Integer read GetLoopCount;
    property MaxLoops: Integer read GetMaxLoops write SetMaxLoops;
    property DelayTime: Single read GetDelayTime write SetDelayTime;
    property LastEvent: TQAnimationEvent read GetLastEvent;
  end;

  IQPropertyAnimation = interface(IQAnimation)
    ['{D1DE41AD-7919-4555-A8B9-5D899433F6FD}']
    function GetPropertyPath: String;
    procedure SetPropertyPath(APath: String);
    function GetProperty: TRttiProperty;
    function GetPropertyOwner: TObject;
    function GetPropertyInstance: Pointer;
    property Path: String read GetPropertyPath write SetPropertyPath;
    property Prop: TRttiProperty read GetProperty;
    property Owner: TObject read GetPropertyOwner;
    property PropInstance: Pointer read GetPropertyInstance;
  end;

  IQFloatAnimation = interface(IQAnimation)
    ['{ECA5CF1C-6A15-4EC7-8BEF-1B33BEC9EC60}']
    function GetMinVal: Double;
    function GetMaxVal: Double;
    procedure SetMinVal(const AVal: Double);
    procedure SetMaxVal(const AVal: Double);
    function GetCurrentVal: Double;
    property MinVal: Double read GetMinVal write SetMinVal;
    property MaxVal: Double read GetMaxVal write SetMaxVal;
    property CurrentVal: Double read GetCurrentVal;
  end;

  IQBoolAnimation = interface(IQAnimation)
    ['{5D0C1161-B1A5-4144-9D47-DAA063834CBE}']
    function GetCurrentVal: Boolean;
    property CurrentVal: Boolean read GetCurrentVal;
  end;

  IQIntAnimation = interface(IQAnimation)
    ['{9B7F06AF-E18D-4077-AD9C-3DD5F48F7946}']
    function GetMinVal: Int64;
    function GetMaxVal: Int64;
    procedure SetMinVal(const AVal: Int64);
    procedure SetMaxVal(const AVal: Int64);
    function GetCurrentVal: Int64;
    property MinVal: Int64 read GetMinVal write SetMinVal;
    property MaxVal: Int64 read GetMaxVal write SetMaxVal;
    property CurrentVal: Int64 read GetCurrentVal;
  end;

  IQColorAnimation = interface(IQAnimation)
    ['{511BB9E9-BB40-49E5-9346-719C52B62FAD}']
    function GetMinVal: TColor;
    function GetMaxVal: TColor;
    procedure SetMinVal(const AVal: TColor);
    procedure SetMaxVal(const AVal: TColor);
    function GetCurrentVal: TColor;
    property MinVal: TColor read GetMinVal write SetMinVal;
    property MaxVal: TColor read GetMaxVal write SetMaxVal;
    property CurrentVal: TColor read GetCurrentVal;
  end;

  IQAlphaColorAnimation = interface(IQAnimation)
    ['{3B8F8325-A465-4C49-BEC2-299D795E0754}']
    function GetMinVal: TAlphaColor;
    function GetMaxVal: TAlphaColor;
    procedure SetMinVal(const AVal: TAlphaColor);
    procedure SetMaxVal(const AVal: TAlphaColor);
    function GetCurrentVal: TAlphaColor;
    property MinVal: TAlphaColor read GetMinVal write SetMinVal;
    property MaxVal: TAlphaColor read GetMaxVal write SetMaxVal;
    property CurrentVal: TAlphaColor read GetCurrentVal;
  end;

  TQAnimationNotifyEvent = procedure(Sender: IQAnimation; AEvent: TQAnimationEvent) of object;
  TQAnimationNotifyCallback = reference to procedure(Sender: IQAnimation; AEvent: TQAnimationEvent);

  TQAniHost = class(TComponent)
  private
    FAnimation: IQPropertyAnimation;
  public
    class function BindAnimation(AOwner: TComponent; Animation: IQPropertyAnimation): TQAniHost;
    class function AnimationHost(AOwner: TComponent; ACreateIfMissed: Boolean): TQAniHost;
    class procedure ClearAnimation(AOwner: TComponent);
    property Animation: IQPropertyAnimation read FAnimation write FAnimation;
  end;

  // 最简单的用户自定义处理动画
function CreateAnimation(ACallback: TQAnimationNotifyCallback): IQAnimation; overload;
function CreateAnimation(ACallback: TQAnimationNotifyEvent): IQAnimation; overload;
// 浮点动画处理
function CreateFloatAnimation(ACallback: TQAnimationNotifyCallback): IQFloatAnimation; overload;
function CreateFloatAnimation(ACallback: TQAnimationNotifyEvent): IQFloatAnimation; overload;
// 整数动画处理
function CreateIntAnimation(ACallback: TQAnimationNotifyCallback): IQIntAnimation; overload;
function CreateIntAnimation(ACallback: TQAnimationNotifyEvent): IQIntAnimation; overload;
// 布尔动画处理
function CreateBooleanAnimation(ACallback: TQAnimationNotifyCallback): IQBoolAnimation; overload;
function CreateBooleanAnimation(ACallback: TQAnimationNotifyEvent): IQBoolAnimation; overload;
// 颜色动画处理
function CreateColorAnimation(ACallback: TQAnimationNotifyCallback): IQColorAnimation; overload;
function CreateColorAnimation(ACallback: TQAnimationNotifyEvent): IQColorAnimation; overload;
function CreateAlphaColorAnimation(ACallback: TQAnimationNotifyCallback): IQAlphaColorAnimation; overload;
function CreateAlphaColorAnimation(ACallback: TQAnimationNotifyEvent): IQAlphaColorAnimation; overload;

// 用户自定义控制特定属性的动画，具体的项目的值由用户自行决定
function CreatePropertyAnimation(AObject: TObject; APropPath: String; ACallback: TQAnimationNotifyCallback)
  : IQPropertyAnimation; overload;
function CreatePropertyAnimation(AObject: TObject; APropPath: String; ACallback: TQAnimationNotifyEvent)
  : IQPropertyAnimation; overload;

function CreateIntPropertyAnimation(AObject: TObject; APropPath: String; ACallback: TQAnimationNotifyCallback)
  : IQIntAnimation; overload;
function CreateIntPropertyAnimation(AObject: TObject; APropPath: String; ACallback: TQAnimationNotifyEvent)
  : IQIntAnimation; overload;

function CreateFloatPropertyAnimation(AObject: TObject; APropPath: String; ACallback: TQAnimationNotifyCallback)
  : IQFloatAnimation; overload;
function CreateFloatPropertyAnimation(AObject: TObject; APropPath: String; ACallback: TQAnimationNotifyEvent)
  : IQFloatAnimation; overload;

function CreateColorPropertyAnimation(AObject: TObject; APropPath: String; ACallback: TQAnimationNotifyCallback)
  : IQColorAnimation; overload;
function CreateColorPropertyAnimation(AObject: TObject; APropPath: String; ACallback: TQAnimationNotifyEvent)
  : IQColorAnimation; overload;

function CreateAlphaColorPropertyAnimation(AObject: TObject; APropPath: String; ACallback: TQAnimationNotifyCallback)
  : IQAlphaColorAnimation; overload;
function CreateAlphaColorPropertyAnimation(AObject: TObject; APropPath: String; ACallback: TQAnimationNotifyEvent)
  : IQAlphaColorAnimation; overload;

implementation

uses System.Generics.Collections, System.Generics.Defaults;
{$LEGACYIFEND ON }

const
  AF_INVERSE = 1;
  AF_AUTO_REVERSE = 2;
  AF_LOOP = 4;
  AF_STARTED = 8;
  AF_STOPPED = 16;
  AF_INVERSING = 32;

type
  TBaseAnimation = class;

  TAnimationTimer = class
  private
    class var FCurrentTimer: TAnimationTimer;
    function GetEnabled: Boolean;
    function GetInterval: Integer;
    procedure SetEnabled(const Value: Boolean);
    procedure SetInterval(const Value: Integer);
  protected
    FAnimations: TList<TBaseAnimation>;
    FTime: Cardinal;
    FTimer: TComponent;
    FRttiContext: TRttiContext;
    FIntervalProp: TRttiProperty;
    FEnabledProp: TRttiProperty;
    procedure DoTimer(Sender: TObject);
  public
    class constructor Create;
    class destructor Destory;
    constructor Create; overload;
    destructor Destroy; override;
    procedure Add(const ASource: TBaseAnimation);
    procedure Remove(const ASource: TBaseAnimation);
    property Interval: Integer read GetInterval write SetInterval;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    class property Current: TAnimationTimer read FCurrentTimer;
  end;

  TBaseAnimation = class(TInterfacedObject, IQAnimation)
  private
    FDisableCount: Integer;
    FPauseCount: Integer;
    FTime: Single;
    FEscapedTime, FLoopEscapedTime, FDelayTime: Single;
    FDuration: Single;
    FData: Pointer;
    FMaxLoops: Integer; // <0 为永不停止
    FLoopCount: Integer;
    FFlags: Integer; // AF_XXX
    FType: TQAnimationType;
    FInterpolation: TQInterpolationType;
    FOnAnimationEvent: TQAnimationNotifyEvent;
    FLastEvent: TQAnimationEvent;
  protected
    function GetType: TQAnimationType;
    procedure SetType(const AType: TQAnimationType);
    function GetInterpolation: TQInterpolationType;
    procedure SetInterpolation(const AType: TQInterpolationType);
    function GetDuration: Single;
    procedure SetDuration(const AValue: Single);
    procedure Start;
    procedure Stop;
    procedure Pause;
    procedure Resume;
    function GetPaused: Boolean;
    function GetInverse: Boolean;
    procedure SetInverse(const AValue: Boolean);
    function GetNormalizedTime: Single;
    function GetCurrentTime: Single;
    procedure SetCurrentTime(const AValue: Single);
    function GetData: Pointer;
    procedure SetData(const AData: Pointer);
    function GetEnabled: Boolean;
    procedure SetEnabled(AValue: Boolean);
    function GetLoop: Boolean;
    procedure SetLoop(const AValue: Boolean);
    function GetLoopCount: Integer;
    function GetMaxLoops: Integer;
    procedure SetMaxLoops(AValue: Integer);
    function GetAutoReverse: Boolean;
    procedure SetAutoReverse(AValue: Boolean);
    procedure PushAnimation;
    procedure PopAnimation;
    function GetDelayTime: Single;
    procedure SetDelayTime(const AValue: Single);
    function GetEscapedTime: Single;
    procedure SetEscapedTime(const AValue: Single);
    procedure ProcessEvent(AEvent: TQAnimationEvent); virtual;
    procedure ProcessAnimation(const ADeltaTime: Single);
    function GetInversing: Boolean;
    procedure SetInversion(const Value: Boolean);
    function GetLastEvent: TQAnimationEvent;
  public
    constructor Create; overload;
    constructor Create(ACallback: TQAnimationNotifyCallback); overload;
    constructor Create(AEvent: TQAnimationNotifyEvent); overload;
    destructor Destroy; override;
    property OnAnimationEvent: TQAnimationNotifyEvent read FOnAnimationEvent write FOnAnimationEvent;
    property AnimationType: TQAnimationType read FType write FType;
    property Duration: Single read FDuration write SetDuration;
    property Interpolation: TQInterpolationType read FInterpolation write FInterpolation;
    property Paused: Boolean read GetPaused;
    property Inverse: Boolean read GetInverse write SetInverse;
    property AutoReverse: Boolean read GetAutoReverse write SetAutoReverse;
    property Inversing: Boolean read GetInversing write SetInversion;
    property Loop: Boolean read GetLoop write SetLoop;
    property NormalizedTime: Single read GetNormalizedTime;
    property CurrentTime: Single read FTime write SetCurrentTime;
    property EscapedTime: Single read FEscapedTime write SetEscapedTime;
    property Data: Pointer read FData write FData;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property LoopCount: Integer read FLoopCount;
    property MaxLoops: Integer read FMaxLoops write SetMaxLoops;
    property DelayTime: Single read FDelayTime write FDelayTime;
  end;

  TRangeAnimation<T> = class(TBaseAnimation)
  protected
    FMinVal, FMaxVal: T;

    function GetMinVal: T;
    function GetMaxVal: T;
    procedure SetMinVal(const AVal: T);
    procedure SetMaxVal(const AVal: T);
    function GetCurrentVal: T; virtual; abstract;
    procedure ProcessEvent(AEvent: TQAnimationEvent); override;
  public
    property MinVal: T read FMinVal write FMinVal;
    property MaxVal: T read FMaxVal write FMaxVal;
    property CurrentVal: T read GetCurrentVal;
  end;

  TFloatAnimation = class(TRangeAnimation<Double>, IQFloatAnimation)
  protected
    function GetCurrentVal: Double; override;
  end;

  TIntAnimation = class(TRangeAnimation<Int64>, IQIntAnimation)
  protected
    function GetCurrentVal: Int64; override;
  end;

  TBooleanAnimation = class(TRangeAnimation<Boolean>, IQBoolAnimation)
  protected
    FValue: Boolean;
    function GetCurrentVal: Boolean; override;
    procedure ProcessEvent(AEvent: TQAnimationEvent); override;
  end;

  TColorAnimation = class(TRangeAnimation<TColor>, IQColorAnimation)
  protected
    function GetCurrentVal: TColor; override;
  end;

  TAlphaColorAnimation = class(TRangeAnimation<TAlphaColor>, IQAlphaColorAnimation)
    function GetCurrentVal: TAlphaColor; override;
  end;

  TPropertyAnimation = class(TBaseAnimation, IQPropertyAnimation)
  protected
    FPath: String;
    FProp: TRttiProperty;
    FOwner: TObject;
    FPropInstance: Pointer;

    function GetPropertyPath: String;
    procedure SetPropertyPath(APath: String);
    function GetProperty: TRttiProperty;
    function GetPropertyOwner: TObject;
    function GetPropertyInstance: Pointer;
  public
    constructor Create(AOwner: TObject; APath: String; ACallback: TQAnimationNotifyCallback); overload;
    constructor Create(AOwner: TObject; APath: String; ACallback: TQAnimationNotifyEvent); overload;
  end;

  TRangePropertyAnimation<T> = class(TPropertyAnimation)
  protected
    FMinVal, FMaxVal: T;

    function GetMinVal: T;
    function GetMaxVal: T;
    procedure SetMinVal(const AVal: T);
    procedure SetMaxVal(const AVal: T);
    function GetCurrentVal: T; virtual; abstract;
    procedure SetPropValue(const AValue: T); virtual; abstract;
    procedure ProcessEvent(AEvent: TQAnimationEvent); override;
  public
    property MinVal: T read FMinVal write FMinVal;
    property MaxVal: T read FMaxVal write FMaxVal;
    property CurrentVal: T read GetCurrentVal;
  end;

  TFloatPropertyAnimation = class(TRangePropertyAnimation<Double>, IQFloatAnimation)
  protected
    function GetCurrentVal: Double; override;
    procedure SetPropValue(const AValue: Double); override;
  public
  end;

  TIntPropertyAnimation = class(TRangePropertyAnimation<Int64>, IQIntAnimation)
  protected
    function GetCurrentVal: Int64; override;
    procedure SetPropValue(const AValue: Int64); override;
  public

  end;

  TColorPropertyAnimation = class(TRangePropertyAnimation<TColor>, IQColorAnimation)
  protected
    function GetCurrentVal: TColor; override;
    procedure SetPropValue(const AValue: TColor); override;
  public
  end;

  TAlphaColorPropertyAnimation = class(TRangePropertyAnimation<TAlphaColor>, IQAlphaColorAnimation)
  protected
    function GetCurrentVal: TAlphaColor; override;
    procedure SetPropValue(const AValue: TAlphaColor); override;
  public
  end;

function ConvertCallback(ACallback: TQAnimationNotifyCallback): TQAnimationNotifyEvent;
var
  AMethod: TMethod absolute Result;
begin
  Result := nil;
  AMethod.Data := Pointer(-1);
  TQAnimationNotifyCallback(AMethod.Code) := ACallback;
end;

function CreateAnimation(ACallback: TQAnimationNotifyCallback): IQAnimation;
begin
  Result := TBaseAnimation.Create(ACallback);
end;

function CreateAnimation(ACallback: TQAnimationNotifyEvent): IQAnimation;
begin
  Result := TBaseAnimation.Create(ACallback);
end;

function CreateFloatAnimation(ACallback: TQAnimationNotifyCallback): IQFloatAnimation;
begin
  Result := TFloatAnimation.Create(ACallback);
end;

function CreateFloatAnimation(ACallback: TQAnimationNotifyEvent): IQFloatAnimation;
begin
  Result := TFloatAnimation.Create(ACallback);
end;

// 整数动画处理
function CreateIntAnimation(ACallback: TQAnimationNotifyCallback): IQIntAnimation;
begin
  Result := TIntAnimation.Create(ACallback);
end;

function CreateIntAnimation(ACallback: TQAnimationNotifyEvent): IQIntAnimation;
begin
  Result := TIntAnimation.Create(ACallback);
end;

function CreateBooleanAnimation(ACallback: TQAnimationNotifyCallback): IQBoolAnimation;
begin
  Result := TBooleanAnimation.Create(ACallback);
end;

function CreateBooleanAnimation(ACallback: TQAnimationNotifyEvent): IQBoolAnimation;
begin
  Result := TBooleanAnimation.Create(ACallback);
end;

// 颜色动画处理
function CreateColorAnimation(ACallback: TQAnimationNotifyCallback): IQColorAnimation;
begin
  Result := TColorAnimation.Create(ACallback);
end;

function CreateColorAnimation(ACallback: TQAnimationNotifyEvent): IQColorAnimation;
begin
  Result := TColorAnimation.Create(ACallback);
end;

function CreateAlphaColorAnimation(ACallback: TQAnimationNotifyCallback): IQAlphaColorAnimation;
begin
  Result := TAlphaColorAnimation.Create(ACallback);
end;

function CreateAlphaColorAnimation(ACallback: TQAnimationNotifyEvent): IQAlphaColorAnimation;
begin
  Result := TAlphaColorAnimation.Create(ACallback);
end;

function CreatePropertyAnimation(AObject: TObject; APropPath: String; ACallback: TQAnimationNotifyCallback)
  : IQPropertyAnimation;
begin
  Result := TPropertyAnimation.Create(AObject, APropPath, ACallback);
end;

function CreatePropertyAnimation(AObject: TObject; APropPath: String; ACallback: TQAnimationNotifyEvent): IQPropertyAnimation;
begin
  Result := TPropertyAnimation.Create(AObject, APropPath, ACallback);
end;

function CreateIntPropertyAnimation(AObject: TObject; APropPath: String; ACallback: TQAnimationNotifyCallback): IQIntAnimation;
begin
  Result := TIntPropertyAnimation.Create(AObject, APropPath, ACallback);
end;

function CreateIntPropertyAnimation(AObject: TObject; APropPath: String; ACallback: TQAnimationNotifyEvent): IQIntAnimation;
begin
  Result := TIntPropertyAnimation.Create(AObject, APropPath, ACallback);
end;

function CreateFloatPropertyAnimation(AObject: TObject; APropPath: String; ACallback: TQAnimationNotifyCallback)
  : IQFloatAnimation;
begin
  Result := TFloatPropertyAnimation.Create(AObject, APropPath, ACallback);
end;

function CreateFloatPropertyAnimation(AObject: TObject; APropPath: String; ACallback: TQAnimationNotifyEvent): IQFloatAnimation;
begin
  Result := TFloatPropertyAnimation.Create(AObject, APropPath, ACallback);
end;

function CreateColorPropertyAnimation(AObject: TObject; APropPath: String; ACallback: TQAnimationNotifyCallback)
  : IQColorAnimation;
begin
  Result := TColorPropertyAnimation.Create(AObject, APropPath, ACallback);
end;

function CreateColorPropertyAnimation(AObject: TObject; APropPath: String; ACallback: TQAnimationNotifyEvent): IQColorAnimation;
begin
  Result := TColorPropertyAnimation.Create(AObject, APropPath, ACallback);
end;

function CreateAlphaColorPropertyAnimation(AObject: TObject; APropPath: String; ACallback: TQAnimationNotifyCallback)
  : IQAlphaColorAnimation;
begin
  Result := TAlphaColorPropertyAnimation.Create(AObject, APropPath, ACallback);
end;

function CreateAlphaColorPropertyAnimation(AObject: TObject; APropPath: String; ACallback: TQAnimationNotifyEvent)
  : IQAlphaColorAnimation;
begin
  Result := TAlphaColorPropertyAnimation.Create(AObject, APropPath, ACallback);
end;

function GetNormalizeColor(AStart, AStop: TColor; ATime: Single): TColor; overload;
var
  AStartColor: TColorRec absolute AStart;
  AStopColor: TColorRec absolute AStop;
  AResultColor: TColorRec absolute Result;
begin
  AResultColor.R := Trunc(AStartColor.R + (AStopColor.R - AStartColor.R) * ATime);
  AResultColor.G := Trunc(AStartColor.G + (AStopColor.G - AStartColor.G) * ATime);
  AResultColor.B := Trunc(AStartColor.B + (AStopColor.B - AStartColor.B) * ATime);
  AResultColor.A := Trunc(AStartColor.A + (AStopColor.A - AStartColor.A) * ATime);
end;

function GetNormalizeColor(AStart, AStop: TAlphaColor; ATime: Single): TAlphaColor; overload;
var
  AStartColor: TAlphaColorRec absolute AStart;
  AStopColor: TAlphaColorRec absolute AStop;
  AResultColor: TAlphaColorRec absolute Result;
begin
  AResultColor.R := Trunc(AStartColor.R + (AStopColor.R - AStartColor.R) * ATime);
  AResultColor.G := Trunc(AStartColor.G + (AStopColor.G - AStartColor.G) * ATime);
  AResultColor.B := Trunc(AStartColor.B + (AStopColor.B - AStartColor.B) * ATime);
  AResultColor.A := Trunc(AStartColor.A + (AStopColor.A - AStartColor.A) * ATime);
end;

function InterpolateBack(T, B, C, D, S: Single; AType: TQAnimationType): Single;
begin
  case AType of
    TQAnimationType.In:
      begin
        if S = 0 then
          S := 1.70158;
        T := T / D;
        Result := C * T * T * ((S + 1) * T - S) + B;
      end;
    TQAnimationType.Out:
      begin
        if S = 0 then
          S := 1.70158;
        T := T / D - 1;
        Result := C * (T * T * ((S + 1) * T + S) + 1) + B;
      end;
    TQAnimationType.InOut:
      begin
        if S = 0 then
          S := 1.70158;
        T := T / (D / 2);
        if T < 1 then
        begin
          S := S * 1.525;
          Result := C / 2 * (T * T * ((S + 1) * T - S)) + B;
        end
        else
        begin
          T := T - 2;
          S := S * 1.525;
          Result := C / 2 * (T * T * ((S + 1) * T + S) + 2) + B;
        end;
      end;
  else
    Result := 0;
  end;
end;

function InterpolateBounce(T, B, C, D: Single; AType: TQAnimationType): Single;
  function _EaseOut(T, B, C, D: Single): Single;
  begin
    T := T / D;
    if T < 1 / 2.75 then
    begin
      Result := C * (7.5625 * T * T) + B;
    end
    else if T < 2 / 2.72 then
    begin
      T := T - (1.5 / 2.75);
      Result := C * (7.5625 * T * T + 0.75) + B;
    end
    else if T < 2.5 / 2.75 then
    begin
      T := T - (2.25 / 2.75);
      Result := C * (7.5625 * T * T + 0.9375) + B;
    end
    else
    begin
      T := T - (2.625 / 2.75);
      Result := C * (7.5625 * T * T + 0.984375) + B;
    end;
  end;
  function _EaseIn(T, B, C, D: Single): Single;
  begin
    Result := C - _EaseOut(D - T, 0, C, D) + B;
  end;

begin
  case AType of
    TQAnimationType.In:
      begin
        Result := _EaseIn(T, B, C, D);
      end;
    TQAnimationType.Out:
      begin
        Result := _EaseOut(T, B, C, D);
      end;
    TQAnimationType.InOut:
      begin
        if T < D / 2 then
          Result := _EaseIn(T * 2, 0, C, D) * 0.5 + B
        else
          Result := _EaseOut(T * 2 - D, 0, C, D) * 0.5 + C * 0.5 + B;
      end;
  else
    Result := 0;
  end;
end;

function InterpolateCirc(T, B, C, D: Single; AType: TQAnimationType): Single;
begin
  case AType of
    TQAnimationType.In:
      begin
        T := T / D;
        Result := -C * (Sqrt(1 - T * T) - 1) + B;
      end;
    TQAnimationType.Out:
      begin
        T := T / D - 1;
        Result := C * Sqrt(1 - T * T) + B;
      end;
    TQAnimationType.InOut:
      begin
        T := T / (D / 2);
        if T < 1 then
          Result := -C / 2 * (Sqrt(1 - T * T) - 1) + B
        else
        begin
          T := T - 2;
          Result := C / 2 * (Sqrt(1 - T * T) + 1) + B;
        end;
      end;
  else
    Result := 0;
  end;
end;

function InterpolateCubic(T, B, C, D: Single; AType: TQAnimationType): Single;
begin
  case AType of
    TQAnimationType.In:
      begin
        T := T / D;
        Result := C * T * T * T + B;
      end;
    TQAnimationType.Out:
      begin
        T := T / D - 1;
        Result := C * (T * T * T + 1) + B;
      end;
    TQAnimationType.InOut:
      begin
        T := T / (D / 2);
        if T < 1 then
          Result := C / 2 * T * T * T + B
        else
        begin
          T := T - 2;
          Result := C / 2 * (T * T * T + 2) + B;
        end;
      end;
  else
    Result := 0;
  end;
end;

function InterpolateElastic(T, B, C, D, A, P: Single; AType: TQAnimationType): Single;
var
  S: Single;
begin
  case AType of
    TQAnimationType.In:
      begin
        if T = 0 then
        begin
          Result := B;
          Exit;
        end;
        T := T / D;
        if T = 1 then
        begin
          Result := B + C;
          Exit;
        end;
        if P = 0 then
          P := D * 0.3;
        if (A = 0) or (A < Abs(C)) then
        begin
          A := C;
          S := P / 4;
        end
        else
        begin
          S := P / (2 * Pi) * ArcSin((C / A));
        end;
        T := T - 1;
        Result := -(A * Power(2, (10 * T)) * Sin((T * D - S) * (2 * Pi) / P)) + B;
      end;
    TQAnimationType.Out:
      begin
        if T = 0 then
        begin
          Result := B;
          Exit;
        end;
        T := T / D;
        if T = 1 then
        begin
          Result := B + C;
          Exit;
        end;
        if P = 0 then
          P := D * 0.3;
        if (A = 0) or (A < Abs(C)) then
        begin
          A := C;
          S := P / 4;
        end
        else
        begin
          S := P / (2 * Pi) * ArcSin((C / A));
        end;
        Result := A * Power(2, (-10 * T)) * Sin((T * D - S) * (2 * Pi) / P) + C + B;
      end;
    TQAnimationType.InOut:
      begin
        if T = 0 then
        begin
          Result := B;
          Exit;
        end;
        T := T / (D / 2);
        if T = 2 then
        begin
          Result := B + C;
          Exit;
        end;
        if P = 0 then
          P := D * (0.3 * 1.5);
        if (A = 0) or (A < Abs(C)) then
        begin
          A := C;
          S := P / 4;
        end
        else
        begin
          S := P / (2 * Pi) * ArcSin((C / A));
        end;

        if T < 1 then
        begin
          T := T - 1;
          Result := -0.5 * (A * Power(2, (10 * T)) * Sin((T * D - S) * (2 * Pi) / P)) + B;
        end
        else
        begin
          T := T - 1;
          Result := A * Power(2, (-10 * T)) * Sin((T * D - S) * (2 * Pi) / P) * 0.5 + C + B;
        end;
      end;
  else
    Result := 0;
  end;
end;

function InterpolateExpo(T, B, C, D: Single; AType: TQAnimationType): Single;
begin
  case AType of
    TQAnimationType.In:
      begin
        If T = 0 Then
          Result := B
        else
          Result := C * Power(2, (10 * (T / D - 1))) + B;
      end;
    TQAnimationType.Out:
      begin
        If T = D then
          Result := B + C
        else
          Result := C * (-Power(2, (-10 * T / D)) + 1) + B;
      end;
    TQAnimationType.InOut:
      begin
        if T = 0 then
        begin
          Result := B;
          Exit;
        end;
        if T = D then
        begin
          Result := B + C;
          Exit;
        end;
        T := T / (D / 2);
        if T < 1 then
          Result := C / 2 * Power(2, (10 * (T - 1))) + B
        else
        begin
          T := T - 1;
          Result := C / 2 * (-Power(2, (-10 * T)) + 2) + B;
        end;
      end;
  else
    Result := 0;
  end;
end;

function InterpolateLinear(T, B, C, D: Single): Single;
begin
  Result := C * T / D + B;
end;

function InterpolateQuad(T, B, C, D: Single; AType: TQAnimationType): Single;
begin
  case AType of
    TQAnimationType.In:
      begin
        T := T / D;
        Result := C * T * T + B;
      end;
    TQAnimationType.Out:
      begin
        T := T / D;
        Result := -C * T * (T - 2) + B;
      end;
    TQAnimationType.InOut:
      begin
        T := T / (D / 2);

        if T < 1 then
          Result := C / 2 * T * T + B
        else
        begin
          T := T - 1;
          Result := -C / 2 * (T * (T - 2) - 1) + B;
        end;
      end;
  else
    Result := 0;
  end;
end;

function InterpolateQuart(T, B, C, D: Single; AType: TQAnimationType): Single;
begin
  case AType of
    TQAnimationType.In:
      begin
        T := T / D;
        Result := C * T * T * T * T + B;
      end;
    TQAnimationType.Out:
      begin
        T := T / D - 1;
        Result := -C * (T * T * T * T - 1) + B;
      end;
    TQAnimationType.InOut:
      begin
        T := T / (D / 2);
        if T < 1 then
          Result := C / 2 * T * T * T * T + B
        else
        begin
          T := T - 2;
          Result := -C / 2 * (T * T * T * T - 2) + B;
        end;
      end;
  else
    Result := 0;
  end;
end;

function InterpolateQuint(T, B, C, D: Single; AType: TQAnimationType): Single;
begin
  case AType of
    TQAnimationType.In:
      begin
        T := T / D;
        Result := C * T * T * T * T * T + B;
      end;
    TQAnimationType.Out:
      begin
        T := T / D - 1;
        Result := C * (T * T * T * T * T + 1) + B;
      end;
    TQAnimationType.InOut:
      begin
        T := T / (D / 2);
        if T < 1 then
          Result := C / 2 * T * T * T * T * T + B
        else
        begin
          T := T - 2;
          Result := C / 2 * (T * T * T * T * T + 2) + B;
        end;
      end;
  else
    Result := 0;
  end;
end;

function InterpolateSine(T, B, C, D: Single; AType: TQAnimationType): Single;
begin
  case AType of
    TQAnimationType.In:
      begin
        Result := -C * Cos(T / D * (Pi / 2)) + C + B;
      end;
    TQAnimationType.Out:
      begin
        Result := C * Sin(T / D * (Pi / 2)) + B;
      end;
    TQAnimationType.InOut:
      begin
        Result := -C / 2 * (Cos(Pi * T / D) - 1) + B;
      end;
  else
    Result := 0;
  end;
end;

{ TBaseAnimation }

constructor TBaseAnimation.Create;
begin
  inherited;
  FDuration := 0.5;
end;

constructor TBaseAnimation.Create(AEvent: TQAnimationNotifyEvent);
begin
  inherited Create;
  FDuration := 0.5;
  FOnAnimationEvent := AEvent;
end;

constructor TBaseAnimation.Create(ACallback: TQAnimationNotifyCallback);
begin
  inherited Create;
  FDuration := 0.5;
  FOnAnimationEvent := ConvertCallback(ACallback);
end;

destructor TBaseAnimation.Destroy;
begin
  PopAnimation;
  with TMethod(FOnAnimationEvent) do
  begin
    if Data = Pointer(-1) then
    begin
      TQAnimationNotifyCallback(Code) := nil;
      Data := nil;
    end;
  end;
  inherited;
end;

function TBaseAnimation.GetAutoReverse: Boolean;
begin
  Result := (FFlags and AF_AUTO_REVERSE) <> 0;
end;

function TBaseAnimation.GetCurrentTime: Single;
begin
  Result := FTime;
end;

function TBaseAnimation.GetData: Pointer;
begin
  Result := FData;
end;

function TBaseAnimation.GetDelayTime: Single;
begin
  Result := FDelayTime;
end;

function TBaseAnimation.GetDuration: Single;
begin
  Result := FDuration;
end;

function TBaseAnimation.GetEnabled: Boolean;
begin
  Result := FDisableCount = 0;
end;

function TBaseAnimation.GetEscapedTime: Single;
begin
  Result := FEscapedTime;
end;

function TBaseAnimation.GetInterpolation: TQInterpolationType;
begin
  Result := FInterpolation;
end;

function TBaseAnimation.GetInverse: Boolean;
begin
  Result := (FFlags and AF_INVERSE) <> 0;
end;

function TBaseAnimation.GetInversing: Boolean;
begin
  Result := (FFlags and AF_INVERSING) <> 0;
end;

function TBaseAnimation.GetLastEvent: TQAnimationEvent;
begin
  Result := FLastEvent;
end;

function TBaseAnimation.GetLoop: Boolean;
begin
  Result := (FFlags and AF_LOOP) <> 0;
end;

function TBaseAnimation.GetLoopCount: Integer;
begin
  Result := FLoopCount;
end;

function TBaseAnimation.GetMaxLoops: Integer;
begin
  Result := FMaxLoops;
end;

function TBaseAnimation.GetNormalizedTime: Single;
begin
  Result := 0;
  if FDuration > 0 then
  begin
    case FInterpolation of
      TQInterpolationType.Linear:
        Result := InterpolateLinear(FTime, 0, 1, FDuration);
      TQInterpolationType.Quadratic:
        Result := InterpolateQuad(FTime, 0, 1, FDuration, FType);
      TQInterpolationType.Cubic:
        Result := InterpolateCubic(FTime, 0, 1, FDuration, FType);
      TQInterpolationType.Quartic:
        Result := InterpolateQuart(FTime, 0, 1, FDuration, FType);
      TQInterpolationType.Quintic:
        Result := InterpolateQuint(FTime, 0, 1, FDuration, FType);
      TQInterpolationType.Sinusoidal:
        Result := InterpolateSine(FTime, 0, 1, FDuration, FType);
      TQInterpolationType.Exponential:
        Result := InterpolateExpo(FTime, 0, 1, FDuration, FType);
      TQInterpolationType.Circular:
        Result := InterpolateCirc(FTime, 0, 1, FDuration, FType);
      TQInterpolationType.Elastic:
        Result := InterpolateElastic(FTime, 0, 1, FDuration, 0, 0, FType);
      TQInterpolationType.Back:
        Result := InterpolateBack(FTime, 0, 1, FDuration, 0, FType);
      TQInterpolationType.Bounce:
        Result := InterpolateBounce(FTime, 0, 1, FDuration, FType);
    end;
  end;
end;

function TBaseAnimation.GetPaused: Boolean;
begin
  Result := FPauseCount > 0;
end;

function TBaseAnimation.GetType: TQAnimationType;
begin
  Result := FType;
end;

procedure TBaseAnimation.Pause;
begin
  Inc(FPauseCount);
  ProcessEvent(TQAnimationEvent.Paused);
  PopAnimation;
end;

procedure TBaseAnimation.PopAnimation;
begin
  if Assigned(TAnimationTimer.Current) then
    TAnimationTimer.Current.Remove(Self);
end;

procedure TBaseAnimation.ProcessAnimation(const ADeltaTime: Single);
begin
  FEscapedTime := FEscapedTime + ADeltaTime;
  FLoopEscapedTime := FLoopEscapedTime + ADeltaTime;
  if FLoopEscapedTime >= DelayTime then
  begin
    if Inversing then
      CurrentTime := CurrentTime - ADeltaTime
    else
      CurrentTime := CurrentTime + ADeltaTime;
  end;
end;

procedure TBaseAnimation.ProcessEvent(AEvent: TQAnimationEvent);
begin
  FLastEvent := AEvent;
  if Assigned(FOnAnimationEvent) then
  begin
    if TMethod(FOnAnimationEvent).Data = Pointer(-1) then
      TQAnimationNotifyCallback(TMethod(FOnAnimationEvent).Code)(Self, AEvent)
    else
      FOnAnimationEvent(Self, AEvent);
  end;
end;

procedure TBaseAnimation.PushAnimation;
begin
  if (FPauseCount = 0) and (FDisableCount = 0) and ((FFlags and AF_STARTED) <> 0) and Assigned(TAnimationTimer.Current) then
    TAnimationTimer.Current.Add(Self);
end;

procedure TBaseAnimation.Resume;
begin
  Dec(FPauseCount);
  if (FPauseCount = 0) and (FDisableCount = 0) then
  begin
    ProcessEvent(Resumed);
    PushAnimation;
  end;
end;

procedure TBaseAnimation.SetAutoReverse(AValue: Boolean);
begin
  if AValue then
    FFlags := FFlags or AF_AUTO_REVERSE
  else
    FFlags := FFlags and (not AF_AUTO_REVERSE);
end;

procedure TBaseAnimation.SetCurrentTime(const AValue: Single);
  procedure DoNextLoop();
  begin
    Inc(FLoopCount);
    ProcessEvent(NextLoop);
    if (FMaxLoops >= 0) and (FLoopCount >= FMaxLoops) then
      Stop;
  end;
  procedure InternalGotoTime(ATime: Single);
  begin
    FTime := ATime;
    ProcessEvent(GotoTime);
  end;

begin
  if AValue >= FDuration then
  begin
    InternalGotoTime(FDuration);
    FLoopEscapedTime := 0;
    if Loop then
    begin
      if AutoReverse then // 自动翻转
      begin
        if Inverse then
          DoNextLoop;
        Inversing := true
      end
      else
      begin
        DoNextLoop;
        FTime := 0;
      end;
    end
    else if not Inverse then
      DoNextLoop;
    if FDelayTime > 0 then
      ProcessEvent(EnterDelay);
  end
  else if AValue <= 0 then
  begin
    InternalGotoTime(0);
    FLoopEscapedTime := 0;
    if Loop then
    begin
      if AutoReverse then
      begin
        Inversing := false;
        if not Inverse then
          DoNextLoop;
      end
      else
      begin
        DoNextLoop;
        FTime := FDuration
      end;
    end
    else if Inverse then
      DoNextLoop;
    if FDelayTime > 0 then
      ProcessEvent(EnterDelay);
  end
  else
  begin
    if FLastEvent = EnterDelay then
      ProcessEvent(ExitDelay);
    InternalGotoTime(AValue);
  end;
end;

procedure TBaseAnimation.SetData(const AData: Pointer);
begin
  FData := AData;
end;

procedure TBaseAnimation.SetDelayTime(const AValue: Single);
begin
  FDelayTime := AValue;
end;

procedure TBaseAnimation.SetDuration(const AValue: Single);
begin
  if AValue > 0 then
    FDuration := AValue
  else
    FDuration := 0;
end;

procedure TBaseAnimation.SetEnabled(AValue: Boolean);
begin
  if AValue then
    Dec(FDisableCount)
  else
    Inc(FDisableCount);
  if (FPauseCount = 0) and (FDisableCount = 0) then
  begin
    ProcessEvent(TQAnimationEvent.Enabled);
    PushAnimation;
  end
  else if FDisableCount = 1 then
  begin
    ProcessEvent(Disabled);
    PopAnimation;
  end;
end;

procedure TBaseAnimation.SetEscapedTime(const AValue: Single);
begin
  FEscapedTime := AValue;
end;

procedure TBaseAnimation.SetInterpolation(const AType: TQInterpolationType);
begin
  FInterpolation := AType;
end;

procedure TBaseAnimation.SetInverse(const AValue: Boolean);
begin
  if AValue then
    FFlags := FFlags and AF_INVERSE
  else
    FFlags := FFlags and (not AF_INVERSE);
end;

procedure TBaseAnimation.SetInversion(const Value: Boolean);
begin
  if Value then
    FFlags := FFlags or AF_INVERSING
  else
    FFlags := FFlags and (not AF_INVERSING);
end;

procedure TBaseAnimation.SetLoop(const AValue: Boolean);
begin
  if AValue then
  begin
    FFlags := FFlags or AF_LOOP;
    if FMaxLoops = 0 then
      FMaxLoops := -1;
  end
  else
    FFlags := FFlags and (not AF_LOOP);
end;

procedure TBaseAnimation.SetMaxLoops(AValue: Integer);
begin
  if AValue >= 0 then
    FMaxLoops := AValue
  else
    FMaxLoops := -1;
end;

procedure TBaseAnimation.SetType(const AType: TQAnimationType);
begin
  FType := AType;
end;

procedure TBaseAnimation.Start;
begin
  if (FFlags and AF_STARTED) = 0 then
  begin
    ProcessEvent(Starting);
    FFlags := (FFlags or AF_STARTED) and (not AF_STOPPED);
    if Inverse then
      FFlags := FFlags or AF_INVERSING;
    FLoopEscapedTime := 0;
    FEscapedTime := 0;
    PushAnimation;
    ProcessEvent(Started);
    if FDelayTime > 0 then
      ProcessEvent(EnterDelay);
  end;
end;

procedure TBaseAnimation.Stop;
begin
  ProcessEvent(Stopping);
  FFlags := (FFlags or AF_STOPPED) and (not AF_STARTED);
  PopAnimation;
  ProcessEvent(Stopped);
end;

{ TAnimationTimer }

procedure TAnimationTimer.Add(const ASource: TBaseAnimation);
begin
  FAnimations.Add(ASource);
  Enabled := true;
  FTime := TThread.GetTickCount;
end;

constructor TAnimationTimer.Create;
var
  ATimerClass: TRttiType;
  AOnTimerProp: TRttiProperty;
  AEvent: TNotifyEvent;
  AMethod: TMethod absolute AEvent;
  V: TValue;
const
  VclTimerName =
{$IF RTLVersion>22}'Vcl.ExtCtrls.TTimer'{$ELSE}'ExtCtrls.TTimer'{$IFEND};
  FmxTimerName = 'FMX.Types.TTimer';
begin
  inherited;
  FRttiContext := TRttiContext.Create;
  ATimerClass := FRttiContext.FindType(VclTimerName); // 查找 VCL 版的TTimer
  if not Assigned(ATimerClass) then // FMX?
    ATimerClass := FRttiContext.FindType(FmxTimerName);
  assert(ATimerClass <> nil);
  FTimer := TComponentClass(ATimerClass.Handle.TypeData.ClassType).Create(nil);
  FIntervalProp := ATimerClass.GetProperty('Interval');
  FEnabledProp := ATimerClass.GetProperty('Enabled');
  AEvent := DoTimer;
  TValue.Make(@AMethod, TypeInfo(TNotifyEvent), V);
  AOnTimerProp := ATimerClass.GetProperty('OnTimer');
  AOnTimerProp.SetValue(FTimer, V);
  Enabled := false;
  Interval := 16; // 1000/60=16.666...
  FAnimations := TList<TBaseAnimation>.Create;
end;

class constructor TAnimationTimer.Create;
begin
  FCurrentTimer := TAnimationTimer.Create;
end;

class destructor TAnimationTimer.Destory;
begin
  FreeAndNil(FCurrentTimer);
end;

destructor TAnimationTimer.Destroy;
begin
  FreeAndNil(FTimer);
  FreeAndNil(FAnimations);
  inherited;
end;

procedure TAnimationTimer.DoTimer(Sender: TObject);
var
  I: Integer;
  ANewTime: Cardinal;
  AItems: TArray<TBaseAnimation>;
  ADelta: Single;
begin
  ANewTime := TThread.GetTickCount;
  ADelta := (ANewTime - FTime) / 1000;
  FTime := ANewTime;
  AItems := FAnimations.ToArray;
  for I := 0 to High(AItems) do
    AItems[I].ProcessAnimation(ADelta);
end;

function TAnimationTimer.GetEnabled: Boolean;
begin
  Result := FEnabledProp.GetValue(FTimer).AsBoolean;
end;

function TAnimationTimer.GetInterval: Integer;
begin
  Result := FIntervalProp.GetValue(FTimer).AsInteger;
end;

procedure TAnimationTimer.Remove(const ASource: TBaseAnimation);
begin
  FAnimations.Remove(ASource);
  if FAnimations.Count = 0 then
    Enabled := false;
end;

procedure TAnimationTimer.SetEnabled(const Value: Boolean);
begin
  FEnabledProp.SetValue(FTimer, Value);
end;

procedure TAnimationTimer.SetInterval(const Value: Integer);
begin
  FIntervalProp.SetValue(FTimer, Value);
end;

{ TPropertyAnimation }

constructor TPropertyAnimation.Create(AOwner: TObject; APath: String; ACallback: TQAnimationNotifyEvent);
begin
  inherited Create(ACallback);
  FPath := APath;
  FOwner := AOwner;
end;

constructor TPropertyAnimation.Create(AOwner: TObject; APath: String; ACallback: TQAnimationNotifyCallback);
begin
  inherited Create(ACallback);
  FPath := APath;
  FOwner := AOwner;
end;

function TPropertyAnimation.GetProperty: TRttiProperty;
var
  P: PChar;
  function NextToken: String;
  var
    ps: PChar;
  begin
    ps := P;
    while (P^ <> #0) and (P^ <> '.') do
      Inc(P);
    Result := Copy(ps, 0, P - ps);
    if P^ = '.' then
      Inc(P);
  end;
  function GetRttiProp: TRttiProperty;
  var
    AContext: TRttiContext;
    AType: TRttiType;
    P: PChar;
    AInst: Pointer;
  begin
    AContext := TRttiContext.Create;
    P := PChar(FPath);
    AInst := FOwner;
    AType := AContext.GetType(FOwner.ClassType);
    Result := nil;
    FPropInstance := nil;
    while Assigned(AType) and (P^ <> #0) do
    begin
      Result := AType.GetProperty(NextToken);
      if not Assigned(Result) then
        Break
      else
      begin
        // 类？接口/记录暂不支持
        case Result.PropertyType.TypeKind of
          tkClass:
            begin
              AInst := Result.GetValue(AInst).AsObject;
              AType := AContext.GetType(TObject(AInst).ClassType);
            end;
          tkInteger, tkChar, tkEnumeration, tkFloat, tkString, tkSet, tkMethod, tkWChar, tkLString, tkWString, tkInt64,
            tkUString:
            begin
              if Result.IsReadable and Result.IsWritable then
              begin
                if P^ <> #0 then
                  Result := nil
                else
                  FPropInstance := AInst;
              end;
            end;
        end;
      end;
    end;
  end;

begin
  if Assigned(FOwner) and (not Assigned(FProp)) then
    FProp := GetRttiProp;
  Result := FProp;
end;

function TPropertyAnimation.GetPropertyInstance: Pointer;
begin
  Result := FPropInstance;
end;

function TPropertyAnimation.GetPropertyOwner: TObject;
begin
  Result := FOwner;
end;

function TPropertyAnimation.GetPropertyPath: String;
begin
  Result := FPath;
end;

procedure TPropertyAnimation.SetPropertyPath(APath: String);
begin
  if FPath <> APath then
  begin
    FProp := nil;
    FPath := APath;
  end;
end;

{ TFloatPropertyAnimation }

function TFloatPropertyAnimation.GetCurrentVal: Double;
begin
  Result := FMinVal + NormalizedTime * (FMaxVal - FMinVal);
end;

procedure TFloatPropertyAnimation.SetPropValue(const AValue: Double);
var
  AProp: TRttiProperty;
begin
  AProp := GetProperty;
  if Assigned(AProp) then
    AProp.SetValue(FPropInstance, AValue);
end;

{ TIntPropertyAnimation }

function TIntPropertyAnimation.GetCurrentVal: Int64;
begin
  Result := Trunc(FMinVal + NormalizedTime * (FMaxVal - FMinVal));
end;

procedure TIntPropertyAnimation.SetPropValue(const AValue: Int64);
var
  AProp: TRttiProperty;
begin
  AProp := GetProperty;
  if Assigned(AProp) then
    AProp.SetValue(FPropInstance, AValue);
end;

{ TFloatAnimation }

function TFloatAnimation.GetCurrentVal: Double;
begin
  Result := FMinVal + NormalizedTime * (FMaxVal - FMinVal);
end;

{ TIntAnimation }

function TIntAnimation.GetCurrentVal: Int64;
begin
  Result := Trunc(FMinVal + NormalizedTime * (FMaxVal - FMinVal));
end;

{ TRangeAnimation<T> }

function TRangeAnimation<T>.GetMaxVal: T;
begin
  Result := FMaxVal;
end;

function TRangeAnimation<T>.GetMinVal: T;
begin
  Result := FMinVal;
end;

procedure TRangeAnimation<T>.ProcessEvent(AEvent: TQAnimationEvent);
var
  V: T;
begin
  if AEvent = TQAnimationEvent.Starting then
  begin
    if TComparer<T>.Default.Compare(FMinVal, FMaxVal) > 0 then
    begin
      V := FMinVal;
      FMinVal := FMaxVal;
      FMaxVal := V;
    end;
  end;
  inherited;
end;

procedure TRangeAnimation<T>.SetMaxVal(const AVal: T);
begin
  FMaxVal := AVal;
end;

procedure TRangeAnimation<T>.SetMinVal(const AVal: T);
begin
  FMinVal := AVal;
end;

{ TRangePropertyAnimation<T> }

function TRangePropertyAnimation<T>.GetMaxVal: T;
begin
  Result := FMaxVal;
end;

function TRangePropertyAnimation<T>.GetMinVal: T;
begin
  Result := FMinVal;
end;

procedure TRangePropertyAnimation<T>.ProcessEvent(AEvent: TQAnimationEvent);
var
  V: T;
begin
  if AEvent = TQAnimationEvent.GotoTime then
    SetPropValue(GetCurrentVal)
  else if AEvent = TQAnimationEvent.Starting then
  begin
    if TComparer<T>.Default.Compare(FMinVal, FMaxVal) > 0 then
    begin
      V := FMinVal;
      FMinVal := FMaxVal;
      FMaxVal := V;
    end;
  end;
  inherited;
end;

procedure TRangePropertyAnimation<T>.SetMaxVal(const AVal: T);
begin
  FMaxVal := AVal;
end;

procedure TRangePropertyAnimation<T>.SetMinVal(const AVal: T);
begin
  FMinVal := AVal;
end;

{ TColorAnimation }

function TColorAnimation.GetCurrentVal: TColor;
begin
  Result := GetNormalizeColor(FMinVal, FMaxVal, NormalizedTime);
end;

{ TAlphaColorAnimation }

function TAlphaColorAnimation.GetCurrentVal: TAlphaColor;
begin
  Result := GetNormalizeColor(FMinVal, FMaxVal, NormalizedTime);
end;

{ TColorPropertyAnimation }

function TColorPropertyAnimation.GetCurrentVal: TColor;
begin
  Result := GetNormalizeColor(FMinVal, FMaxVal, NormalizedTime);
end;

procedure TColorPropertyAnimation.SetPropValue(const AValue: TColor);
var
  AProp: TRttiProperty;
begin
  AProp := GetProperty;
  if Assigned(AProp) then
    AProp.SetValue(FPropInstance, AValue);
end;

{ TAlphaColorPropertyAnimation }

function TAlphaColorPropertyAnimation.GetCurrentVal: TAlphaColor;
begin
  Result := GetNormalizeColor(FMinVal, FMaxVal, NormalizedTime);
end;

procedure TAlphaColorPropertyAnimation.SetPropValue(const AValue: TAlphaColor);
var
  AProp: TRttiProperty;
begin
  AProp := GetProperty;
  if Assigned(AProp) then
    AProp.SetValue(FPropInstance, AValue);
end;

{ TQAniHost }

class function TQAniHost.AnimationHost(AOwner: TComponent; ACreateIfMissed: Boolean): TQAniHost;
var
  I: Integer;
begin
  for I := 0 to AOwner.ComponentCount - 1 do
  begin
    if (AOwner.Components[I] is TQAniHost) then
      Exit(TQAniHost(AOwner.Components[I]));
  end;
  if ACreateIfMissed then
    Result := TQAniHost.Create(AOwner)
  else
    Result := nil;
end;

class function TQAniHost.BindAnimation(AOwner: TComponent; Animation: IQPropertyAnimation): TQAniHost;
var
  I: Integer;
begin
  for I := 0 to AOwner.ComponentCount - 1 do
  begin
    if (AOwner.Components[I] is TQAniHost) then
    begin
      if TQAniHost(AOwner.Components[I]).Animation = Animation then
        Exit(TQAniHost(AOwner.Components[I]));
    end;
  end;
  Result := TQAniHost.Create(AOwner);
  Result.Animation := Animation;
end;

class procedure TQAniHost.ClearAnimation(AOwner: TComponent);
var
  I, C: Integer;
  AList: TArray<TComponent>;
begin
  SetLength(AList, AOwner.ComponentCount);
  C := 0;
  for I := 0 to AOwner.ComponentCount - 1 do
  begin
    if (AOwner.Components[I] is TQAniHost) then
    begin
      AList[C] := AOwner.Components[I];
      Inc(C);
    end;
  end;
  for I := 0 to C - 1 do
    FreeAndNil(AList[C]);
end;

{ TBooleanAnimation }

function TBooleanAnimation.GetCurrentVal: Boolean;
begin
  Result := FValue;
end;

procedure TBooleanAnimation.ProcessEvent(AEvent: TQAnimationEvent);
begin
  if AEvent = TQAnimationEvent.Starting then
  begin
    FMinVal := false;
    FMaxVal := true;
    FValue := false;
  end
  else if AEvent = TQAnimationEvent.NextLoop then
    FValue := not FValue;
  if AEvent <> TQAnimationEvent.GotoTime then
    inherited;
end;

end.
