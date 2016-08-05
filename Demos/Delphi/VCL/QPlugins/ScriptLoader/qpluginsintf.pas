unit qpluginsintf;

interface

type
  IQPluginsManager = interface;

  TQShortVersion = packed record
    case Integer of
      0:
        (Major, Minor, Release, Build: Byte);
      1:
        (Value: Integer);
  end;

  TQPluginVersion = packed record
    Version: TQShortVersion;
    Company: array [0 .. 63] of WideChar;
    Name: array [0 .. 63] of WideChar;
    Description: array [0 .. 255] of WideChar;
    FileName: array [0 .. 255] of WideChar;
  end;

  // 插件版本信息
  IQPluginVersion = interface
    ['{4AD82500-4148-45D1-B0F8-F6B6FB8B7F1C}']
    function GetVersion(var AVerInfo: TQPluginVersion): Boolean; stdcall;
  end;

  // 数据传递的参数信息

  IQParams = interface;
  TQParamType = (ptUnknown, ptInt8, ptUInt8, ptInt16, ptUInt16, ptInt32,
    ptUInt32, ptInt64, ptUInt64,
    // Integer Types
    ptFloat4, ptFloat8, // Float types
    ptDateTime, ptInterval, // DateTime types
    ptAnsiString, ptUtf8String, ptUnicodeString, // String types
    ptBoolean, // Boolean
    ptGuid, // Guid
    ptBytes, // Binary
    ptPointer, // untyped pointer
    ptInterface, // Interface
    ptStream, // 流
    ptArray // Array
    );

  IQStream = interface
    ['{BCFD2F69-CCB8-4E0B-9FE9-A7D58797D1B8}']
    function Read(pv: Pointer; cb: Cardinal): Cardinal; stdcall;
    function Write(pv: Pointer; cb: Cardinal): Cardinal; stdcall;
    function Seek(AOffset: Int64; AFrom: Byte): Int64; stdcall;
    procedure SetSize(ANewSize: UInt64); stdcall;
    function CopyFrom(AStream: IQStream; ACount: Int64): Int64; stdcall;
  end;

  IQParam = interface
    ['{DCD4D1D2-A7A2-4307-A850-3270B57420D7}']
    function GetName: PWideChar; stdcall;
    function GetAsInteger: Integer; stdcall;
    procedure SetAsInteger(const AValue: Integer); stdcall;
    function GetAsInt64: Int64; stdcall;
    procedure SetAsInt64(const AValue: Int64); stdcall;
    function GetAsBoolean: Boolean; stdcall;
    procedure SetAsBoolean(const AValue: Boolean); stdcall;
    function GetAsSingle: Single; stdcall;
    procedure SetAsSingle(const AValue: Single); stdcall;
    function GetAsFloat: Double; stdcall;
    procedure SetAsFloat(const AValue: Double); stdcall;
    function GetAsString(ABuf: PWideChar; ABufLen: Integer): Integer; stdcall;
    procedure SetAsString(const AValue: PWideChar); stdcall;
    function GetAsGuid: TGuid; stdcall;
    procedure SetAsGuid(const Value: TGuid); stdcall;
    function GetAsBytes(ABuf: PByte; ABufLen: Integer): Integer; stdcall;
    procedure SetAsBytes(ABuf: PByte; ABufLen: Integer); stdcall;
    function GetAsInterface: IInterface; stdcall;
    procedure SetAsInterface(AIntf: IInterface); stdcall;
    function GetIsNull: Boolean; stdcall;
    procedure SetNull; stdcall;
    function GetAsArray: IQParams; stdcall;
    function GetAsStream: IQStream; stdcall;
    function GetAsPointer: Pointer; stdcall;
    procedure SetAsPointer(AValue: Pointer); stdcall;
    function GetParent: IQParams; stdcall;
    function GetSize: Integer; stdcall;
    function GetType: TQParamType; stdcall;
    procedure SetSize(ASize: Integer); stdcall;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsSingle: Single read GetAsSingle write SetAsSingle;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsGuid: TGuid read GetAsGuid write SetAsGuid;
    property IsNull: Boolean read GetIsNull;
    property AsArray: IQParams read GetAsArray;
    property AsPointer: Pointer read GetAsPointer write SetAsPointer;
    property AsInterface: IInterface read GetAsInterface write SetAsInterface;
    property Parent: IQParams read GetParent;
    property ParamType: TQParamType read GetType;
    property Size: Integer read GetSize;
  end;

  IQParams = interface
    ['{1AD2A534-D0C1-4D24-A4A5-A5E1B6E21AF6}']
    function GetItems(AIndex: Integer): IQParam; stdcall;
    function GetCount: Integer; stdcall;
    function ParamByName(const AName: PWideChar): IQParam; stdcall;
    function ParamByPath(APath: PWideChar): IQParam; stdcall;
    function Add(const AName: PWideChar; AParamType: TQParamType)
      : IQParam; stdcall;
    procedure Delete(AIndex: Integer); stdcall;
    procedure Clear; stdcall;
    procedure SaveToStream(AStream: IQStream); stdcall;
    procedure LoadFromStream(AStream: IQStream); stdcall;
    procedure SaveToFile(const AFileName: PWideChar); stdcall;
    procedure LoadFromFile(const AFileName: PWideChar); stdcall;
    property Items[AIndex: Integer]: IQParam read GetItems; default;
    property Count: Integer read GetCount;
  end;

  IQPluginConfigure = interface
    ['{E1100ABC-4AEA-4DFA-AB6D-EF89CE818F4B}']
    procedure Save(AName: PWideChar; AParams: IQParams); stdcall;
    function Load(AName: PWideChar): IQParams; stdcall;
  end;

  IQPluginServices = interface;
  IQPluginsLoader = interface;

  IQPluginService = interface
    ['{E53FAAD2-5C85-4E15-BEF7-13821DE07190}']
    function Execute(const AParams:IQParams;AResult:IQParams): Boolean; stdcall;
    function NewInstance: IQPluginService; stdcall;
    function GetParent: IQPluginServices; stdcall;
    procedure SetParent(AParent: IQPluginServices); stdcall;
    function GetName: PWideChar; stdcall;
    function GetAttributes: IQParams; stdcall;
    function SaveStates(AParams: IQParams): Boolean; stdcall;
    function LoadStates(AParams: IQParams): Boolean; stdcall;
    function Terminate(AWait: Boolean): Boolean; stdcall;
    function GetLoader: IQPluginsLoader; stdcall;
    function GetLastError: Integer; stdcall;
    function GetLastErrorMsg: PWideChar; stdcall;
    function GetId: TGuid; stdcall;
    property Id: TGuid read GetId;
    property Parent: IQPluginServices read GetParent write SetParent;
    property Name: PWideChar read GetName;
    property Attributes: IQParams read GetAttributes;
    property Loader: IQPluginsLoader read GetLoader;
    property LastError: Integer read GetLastError;
    property LastErrorMsg: PWideChar read GetLastErrorMsg;
  end;

  // 插件服务管理中心,Loader 本身就是一个服务
  IQPluginServices = interface
    ['{7325DF17-BC83-4163-BB72-0AE0208352ED}']
    function GetItems(AIndex: Integer): IQPluginService; stdcall;
    function GetCount: Integer; stdcall;
    function ByPath(APath: PWideChar): IQPluginService; stdcall;
    function ById(const AId: TGuid): IQPluginService; stdcall;
    function Add(AItem: IQPluginService): Integer; stdcall;
    function IndexOf(AItem: IQPluginService): Integer; stdcall;
    procedure Delete(AIndex: Integer); stdcall;
    procedure Remove(AItem: IQPluginService); stdcall;
    procedure Clear; stdcall;
    procedure Lock; stdcall;
    procedure Unlock; stdcall;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: IQPluginService read GetItems; default;
  end;

  IQNotify = interface
    ['{00C7F80F-44BF-4E60-AA58-5992B2B71754}']
    procedure Notify(const AId: Cardinal; AParams: IQParams;
      var AFireNext: Boolean);stdcall;
  end;

  IQNotifyManager = interface(IQPluginServices)
    ['{037DCCD1-6877-4917-A315-120CD3E403F4}']
    procedure Subscribe(ANotifyId: Cardinal; AHandler: IQNotify);stdcall;
    procedure Unsubscribe(ANotifyId: Cardinal; AHandler: IQNotify);stdcall;
    function IdByName(const AName:PWideChar):Cardinal;stdcall;
    function NameOfId(const AId:Cardinal):PWideChar;stdcall;
    procedure Send(AId: Cardinal; AParams: IQParams);stdcall;
    procedure Post(AId: Cardinal; AParams: IQParams);stdcall;
    function GetCount:Integer;stdcall;
    function GetId(const AIndex:Integer):Cardinal;stdcall;
    function GetName(const AIndex:Integer):PWideChar;stdcall;
    property Count:Integer read GetCount;
//    property Id[AIndex:Integer]:Cardinal read GetId;
//    property Name[AIndex:Integer]:PWideChar read GetName;
  end;

  TQPluginRouteRule = packed record
    ByPath: Boolean;
    case Integer of
      0:
        (SourcePath, TargetPath: PWideChar);
      1:
        (SourceId, TargetId: TGuid);
  end;

  PQPluginRouteRule = ^TQPluginRouteRule;

  // 插件路由管理器，允许用户对支持同一接口的不同插件或其不同版本间进行路由
  IQPluginsRouter = interface
    ['{EBC6F48A-6C7C-46EE-924B-000E871763F3}']
    function AddMap(const ASourcePath, ATargetPath: PWideChar):Boolean; stdcall;
    function AddId(const ASourceId, ATargetId: TGuid):Boolean; stdcall;
    procedure Delete(AIndex: Integer); stdcall;
    procedure Clear; stdcall;
    function ByPath(APath: PWideChar): IQPluginService; stdcall;
    function ById(const AId: TGuid): IQPluginService; stdcall;
    function GetCount: Integer; stdcall;
    function GetItems(AIndex: Integer): PQPluginRouteRule; stdcall;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: PQPluginRouteRule read GetItems; default;
  end;

  // 加载器
  IQPluginsLoader = interface(IQPluginService)
    ['{EB6863DB-08BE-43CF-9788-AFB9A8F42C4D}']
    function GetVersion(var AVersion: TQPluginVersion): Boolean;
  end;

  IQPluginsManager = interface(IQPluginServices)
    ['{BDE6247B-87AD-4105-BDC9-1EA345A9E4B0}']
    function GetLoaders: IQPluginServices; stdcall;
    function GetRouters: IQPluginServices; stdcall;
    function GetServices: IQPluginServices; stdcall;
    function RequireService(APath: PWideChar): IQPluginService; stdcall;
    procedure Start; stdcall;
    function Stop: Boolean; stdcall;
    function Replace(ANewManager: IQPluginsManager): Boolean; stdcall;
    property Services: IQPluginServices read GetServices;
    property Router: IQPluginServices read GetRouters;
    property Loaders: IQPluginServices read GetLoaders;
  end;

implementation

end.
