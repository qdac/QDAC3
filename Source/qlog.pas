unit qlog;

interface

{$I 'qdac.inc'}
// QLOG_CREATE_GLOBAL指定是否创建默认的QLogs全局变量，如果定义，则创建默认的QLogs对象
{$DEFINE QLOG_CREATE_GLOBAL}

uses classes, SysUtils, Types, qstring, SyncObjs{$IFDEF UNICODE}, ZLib{$ENDIF}
{$IFDEF ANDROID}
    , Androidapi.Log
{$ENDIF}
{$IFDEF IOS}
    , iOSapi.Foundation, Macapi.Helpers, Macapi.ObjectiveC
{$ENDIF}
{$IFDEF POSIX}
    , Posix.Base, Posix.Stdio, Posix.Pthread, Posix.UniStd, IOUtils,
  Posix.NetDB, Posix.SysSocket, Posix.Fcntl,
  Posix.NetinetIn, Posix.arpainet, Posix.SysSelect, Posix.Systime
{$ELSE}
    , Windows, winsock, TlHelp32
{$ENDIF};
{
  本源码来自QDAC项目，版权归swish(QQ:109867294)所有。
  (1)、使用许可及限制
  您可以自由复制、分发、修改本源码，但您的修改应该反馈给作者，并允许作者在必要时，
  合并到本项目中以供使用，合并后的源码同样遵循QDAC版权声明限制。
  您的产品的关于中，应包含以下的版本声明:
  本产品使用的日志记录器来自QDAC项目中的QLog，版权归作者所有。
  (2)、技术支持
  有技术问题，您可以加入QDAC官方QQ群250530692共同探讨。
  (3)、赞助
  您可以自由使用本源码而不需要支付任何费用。如果您觉得本源码对您有帮助，您可以赞
  助本项目（非强制），以使作者不为生活所迫，有更多的精力为您呈现更好的作品：
  赞助方式：
  支付宝： guansonghuan@sina.com 姓名：管耸寰
  建设银行：
  户名：管耸寰
  账号：4367 4209 4324 0179 731
  开户行：建设银行长春团风储蓄所
}

(*
  QLog日志记录单元
  【注意】
  XE6自带的zlib支持了gzwrite等函数，而Delphi 2007自带的zlib不支持，因此，QLog在
  Delphi 2007编译的程序上运行时，请携带zlib1.dll，而XE6编译的版本不需要。
  【QLog是什么】
  本单元用于提供一个高性能的日志记录单元，使用简单的接口完成日志记录工作。对上层
  隐藏内部实现逻辑：
  1、外部正常只需要调用PostLog函数(C++ Builder用户还可以调用AddLog)就可以完成日
  志的记录.
  2、子类可以继承实现各种输入(TQLogWriter)和日志读取接口(TQLogReader)，但目前版
  本只实现了TQLogFileWriter和TQLogSocketWriter。
  本单元类的集成和继承关系如下：
  集成：
  TQLog <- TQLogCastor <- TQLogWriter
  继承：
  TQLog
  TQLogWriter->TQLogFileWriter
  +->TQLogSocketWriter
  TQLogReader
  本单元各个类的作用：
  TQLog : 日志缓存管理单元，用于缓存需要记录的日志
  TQLogCastor : 日志后台写入线程，负责将日志实际格式化并调用各个TQLogWriter实例完成写入
  TQLogWriter : 实际的日志写入对象
  TQLogReader : 日志读取对象，用于日志的定位和获取
  TQLogFileWriter : 日志文件写入对象
  TQLogSocketWriter : 日志文件syslog支持对象，可以发送日志到syslog服务器或其它监听程序
*)
{ 修订日志
  =========
  2017.12.29
  ==========
  * 修正了与 Syslog 协议的兼容性（优先级处理格式问题）
  2016.5.17
  =========
  * 修改了 SetDefaultLogFile 的代码，移除了不必要的锁定，并修改返回默认文件日志写入对象
  + TQLogFileWriter 增加 MaxLogHistories 属性来指定最多允许保留的历史日志文件数量（青春建议）
  2016.3.13
  =========
  * 修正了同时设置 OneFilePerDay 和 RenameHistory 时，启动时会备份原来的文件的问题
  2015.8.26
  =========
  * 修正了TQSocketLogWriter的一个构造函数初始化错误（感谢研究报告）
  2015.7.16
  =========
  * 修正了每天创建一个文件时，出现创建空历史文件的问题（感谢青春报告）
  2015.7.15
  =========
  + 应群里朋友的要求，新增每天一个日志文件的模式

  2015.5.22
  ==========
  * TQLogFileWriter.HandleNeeded 加入对路径的检查，以保证目标目录存在 （感谢松鼠的松建议）

  2015.5.14
  ==========
  + SetDefaultLogFile 函数来设置默认的日志记录文件名及配置
  * 移除 QLOG_CREATE_DEFAULT_WRITER 和 RENAME_HISTORY 条件编译选项（改成通过SetDefaultLogFile来支持）
  + TQLogConsoleWriter 增加对 Android 的 LogCat、iOS 的 NSLog 及 MacOS 控制台的支持
  + TQLog 增加 Mode 设置，来确认日志是同步还是异步输出

  2014.11.11
  ==========
  * 修正了使用QLog时，程序无法多起的问题（麦子仲肥报告）

  2014.11.6
  =========
  * 修改程序启动时日志的行为，默认启动时，如果重命的日志文件已经存在，则重命名并压缩已经
  存在的日志文件，而不再在清空内容重写。

  2014.11.5
  ==========
  * 修订了跨平台编译时，sockaddr/sockaddr_in参数类型定义冲突的问题

  2014.10.23
  ==========
  + TQLogSocketWriter加入TCP协议选项，通过修改UseTCP属性来控制，默认使用UDP

  2014.8.2
  =========
  * 修正了CreateItem时如果日志内容为空时，访问越界的问题

  2014.6.26
  =========
  * 加入HPPEMIT默认链接本单元(麦子仲肥 建议)

  2014.6.21
  =========
  * 修正了2010中由于TThreadId没定义造成无法编译的问题
}
{$M+}
{$HPPEMIT '#pragma link "qlog"'}

type
  { 日志记录模式
    lmSync : 同步模式，等待日志写入完成
    lmAsyn : 异步模式，不等待日志写入完成
  }
  TQLogMode = (lmSync, lmAsyn);
  {
    C++:
    由于$HPPEMIT指令的命令被强制放在了#include之后，其它代码之前，因此由于TLogLevel
    未声明，造成无法编译，因此使用模板和宏来处理下，以便滞后编译
  }
{$HPPEMIT '#include <stdio.h>'}
{$HPPEMIT 'template<class TLogLevel> void PostLog(TLogLevel ALevel,const wchar_t *format,...)'}
{$HPPEMIT '{'}
{$HPPEMIT 'int ASize;'}
{$HPPEMIT 'QStringW AMsg;'}
{$HPPEMIT 'va_list args;'}
{$HPPEMIT 'va_start(args, format);}
{$HPPEMIT 'ASize=vsnwprintf(NULL, 0, format, args);}
{$HPPEMIT 'AMsg.SetLength(ASize);'}
{$IFDEF UNICODE}
{$HPPEMIT 'vsnwprintf(AMsg.c_str(), ASize+1, format, args);'}
{$ELSE}
{$HPPEMIT 'vsnwprintf(AMsg.c_bstr(),ASize+1,format,args);'}
{$ENDIF}
{$HPPEMIT 'PostLog(ALevel,AMsg);'}
{$HPPEMIT 'va_end(args);'}
(*$HPPEMIT '}'*)
{$HPPEMIT '#define AddLog PostLog<TQLogLevel>'}
  {
    //兼容下Linux的Syslog日志级别定义
    0       Emergency: system is unusable
    1       Alert: action must be taken immediately
    2       Critical: critical conditions
    3       Error: error conditions
    4       Warning: warning conditions
    5       Notice: normal but significant condition
    6       Informational: informational messages
    7       Debug: debug-level messages
  }

  TQLogLevel = (llEmergency, llAlert, llFatal, llError, llWarning, llHint, llMessage, llDebug);
  TQLogLevels = set of TQLogLevel;
  TQLog = class;
  TQLogCastor = class;
  TQLogWriter = class;
  TQLogReader = class;
{$IF RTLVersion<22}
  TThreadId = LongWord;
{$IFEND}
  // 日志记录条目
  PQLogItem = ^TQLogItem;

  TQLogItem = record
    Next, Prior: PQLogItem;
    ThreadId: TThreadId;
    TimeStamp: TDateTime;
    Level: TQLogLevel;
    TagLen: Integer;
    MsgLen: Integer;
    Text: array [0 .. 0] of WideChar;
    function GetTag: String;
    function GetMessageText: String;
    property Tag: String read GetTag;
    property MessageText: String read GetMessageText;
  end;

  TQLogList = record
    case Integer of
      0:
        (Value: Int64;);
      1:
        (First: PQLogItem;
          Last: PQLogItem;
        );
      2:
        (FirstVal: Integer;
          LastVal: Integer;
        );
  end;

  // 日志缓存
  TQLog = class
  private
  protected
    FList: TQLogList;
    FCastor: TQLogCastor;
    FInFree: Boolean;
    FCount: Integer;
    FFlushed: Integer;
    FFlags: Integer;
    FCS: TCriticalSection;
    FMode: TQLogMode;
    FSyncEvent: TEvent;
    FAcceptLevels: TQLogLevels;
    FEnabled: Boolean;
    procedure SetMode(const Value: TQLogMode);
    procedure Lock;
    procedure Unlock;
    function Pop: PQLogItem;
    function CreateCastor: TQLogCastor; virtual;
    function GetCastor: TQLogCastor;
    procedure WaitLogWrote;
    procedure BeginWrite;
    procedure EndWrite;
  public
    constructor Create; overload;
    destructor Destroy; override;
    procedure Post(ALevel: TQLogLevel; const AMsg, ATag: QStringW); overload;
    procedure Post(ALevel: TQLogLevel; const AFormat: QStringW; Args: array of const; const ATag: QStringW); overload;
    property Mode: TQLogMode read FMode write SetMode;
    property Castor: TQLogCastor read GetCastor;
    property Count: Integer read FCount;
    property Enabled: Boolean read FEnabled write FEnabled;
    property Flushed: Integer read FFlushed;
    property AcceptLevels: TQLogLevels read FAcceptLevels write FAcceptLevels;
  end;

  TQLogItemAcceptEvent = procedure(Sender: TQLogWriter; AItem: PQLogItem; var Accept: Boolean) of object;

  // 日志写入对象
  TQLogWriter = class
  protected
    FCastor: TQLogCastor;
    FAcceptLevels: TQLogLevels;
    FLazyList: TQLogList;
    FOnAccept: TQLogItemAcceptEvent;
    FTag: IntPtr;
    FLazyMode: Boolean;
    FEnabled: Boolean;
    FAcceptTags: QStringW;
    procedure BeginWrite; virtual;
    procedure EndWrite; virtual;
    procedure LazyWrite; virtual;
    procedure SetLazyMode(const Value: Boolean);
    function Accept(AItem: PQLogItem): Boolean; virtual;
    function ItemToText(AItem: PQLogItem): String; virtual;
    property LazyMode: Boolean read FLazyMode write SetLazyMode;
  public
    constructor Create; overload;
    destructor Destroy; override;
    procedure HandleNeeded; virtual;
    function WriteItem(AItem: PQLogItem): Boolean; virtual;
    property AcceptLevels: TQLogLevels read FAcceptLevels write FAcceptLevels;
    property Enabled: Boolean read FEnabled write FEnabled;
    property OnAccept: TQLogItemAcceptEvent read FOnAccept write FOnAccept;
    property Tag: IntPtr read FTag write FTag;
    property AcceptTags: QStringW read FAcceptTags write FAcceptTags;
  end;

  // 日志读取对象
  TQLogReader = class
  protected
    FItemIndex: Int64;
    function GetCount: Int64; virtual;
    procedure SetItemIndex(const Value: Int64);
  public
    constructor Create; overload;
    destructor Destroy; override;
    function ReadItem(var AMsg: QStringW; ALevel: TQLogLevel): Boolean; virtual;
    function First: Boolean;
    function Last: Boolean;
    function Prior: Boolean;
    function Next: Boolean;
    function MoveTo(AIndex: Int64): Boolean; virtual;
    property Count: Int64 read GetCount;
    property ItemIndex: Int64 read FItemIndex write SetItemIndex;
  end;

  PLogWriterItem = ^TLogWriterItem;

  TLogWriterItem = record
    Next, Prior: PLogWriterItem;
    Writer: TQLogWriter;
  end;

  // 日志广播对象，用于将日志传送到相应的对象，如本地系统文件或者是远程日志对象
  TQLogCastor = class(TThread)
  private
    FLazyInterval: Cardinal;
  protected
    FLastError: Cardinal;
    FLastErrorMsg: QStringW;
    FNotifyHandle: TEvent;
    FCS: TCriticalSection;
    FOwner: TQLog;
    FWriters: PLogWriterItem;
    FActiveWriter: PLogWriterItem;
    FActiveLog: PQLogItem;
    procedure Execute; override;
    procedure LogAdded;
    function WaitForLog: Boolean; virtual;
    function FetchNext: PQLogItem; virtual;
    function FirstWriter: PLogWriterItem;
    function NextWriter: PLogWriterItem;
    procedure ClearWriters;
{$IFNDEF UNICODE}
    function GetFinished: Boolean;
{$ENDIF}
  published
  public
    constructor Create(AOwner: TQLog); overload;
    destructor Destroy; override;
    procedure SetLastError(ACode: Cardinal; const AMsg: QStringW);
    procedure AddWriter(AWriter: TQLogWriter);
    procedure RemoveWriter(AWriter: TQLogWriter);
    property ActiveLog: PQLogItem read FActiveLog;
    property LazyInterval: Cardinal read FLazyInterval write FLazyInterval;
{$IFNDEF UNICODE}
    property Finished: Boolean read GetFinished;
{$ENDIF}
  end;

  TQLogFileCreateMode = (lcmReplace, lcmRename, lcmAppend);
  TQLogFileStream = TFileStream;

  TQLogFileWriter = class(TQLogWriter)
  private
    FCreateMode: TQLogFileCreateMode;
    FMaxLogHistories: Integer;
    procedure SetMaxLogHistories(const Value: Integer);
  protected
    // 索引和日志文件句柄
    FLogHandle: TFileStream;
    FFileName: QStringW;
    FPosition: Int64;
    FLastTime: TDateTime;
    FLastThreadId: Cardinal;
    FLastTimeStamp, FLastThread: QStringW;
    FBuilder: TQStringCatHelperW;
    FReplaceMode: TQLogFileCreateMode;
    FMaxSize: Int64;
    FOneFilePerDay: Boolean;
    function FlushBuffer: Boolean;
    function CompressLog(ALogFileName: QStringW): Boolean;
    procedure RenameHistory;
    procedure DeleteHistories;
    procedure LazyWrite; override;
  public
    /// 创建一个日志文件
    /// <param name="AFileName">文件名</param>
    /// <param name="AWithIndex">是否同时创建索引文件</param>
    /// <remarks>
    /// 创建索引文件有利于检索日志时快速定位日志的起始位置，也可以不创建索引日志。
    /// 如果不创建索引，则定位到某一特定日志时，将需要更多的IO操作
    constructor Create(const AFileName: QStringW; AWithIndex: Boolean = False); overload;
    constructor Create; overload;
    destructor Destroy; override;
    function WriteItem(AItem: PQLogItem): Boolean; override;
    procedure EndWrite; override;
    procedure HandleNeeded; override;
    property FileName: QStringW read FFileName;
    property MaxSize: Int64 read FMaxSize write FMaxSize;
    property CreateMode: TQLogFileCreateMode read FCreateMode write FCreateMode;
    property OneFilePerDay: Boolean read FOneFilePerDay write FOneFilePerDay;
    property MaxLogHistories: Integer read FMaxLogHistories write SetMaxLogHistories;
  end;

  TQLogConsoleWriter = class(TQLogWriter)
  public
    constructor Create; overload;
{$IFDEF MSWINDOWS}
    constructor Create(AUseDebugConsole: Boolean); overload;
{$ENDIF}
    function WriteItem(AItem: PQLogItem): Boolean; override;
    procedure HandleNeeded; override;
{$IFDEF MSWINDOWS}
  private
    FUseDebugConsole: Boolean;
  public
    property UseDebugConsole: Boolean read FUseDebugConsole write FUseDebugConsole;
{$ENDIF}
  end;

  // Linux syslog服务写入
  TQLogSocketWriter = class(TQLogWriter)
  private
    FServerPort: Word;
    FServerHost: String;
    FSocket: THandle;
    FReaderAddr: sockaddr_in;
    FBuilder: TQStringCatHelperW;
    FTextEncoding: TTextEncoding;
    FUseTCP: Boolean;
    FLastConnectTryTime: TDateTime;
    procedure SetTextEncoding(const Value: TTextEncoding);
    function ConnectNeeded: Boolean;
    function LookupServer: Boolean;
  public
    constructor Create; overload;
    constructor Create(const AHost: String; APort: Word; AUseTcp: Boolean); overload;
    destructor Destroy; override;
    function WriteItem(AItem: PQLogItem): Boolean; override;
    procedure HandleNeeded; override;
    property ServerHost: String read FServerHost write FServerHost;
    property ServerPort: Word read FServerPort write FServerPort;
    property TextEncoding: TTextEncoding read FTextEncoding write SetTextEncoding;
    property UseTCP: Boolean read FUseTCP write FUseTCP;
  end;

  TQLogStringsWriter = class(TQLogWriter)
  protected
    FMaxItems: Integer;
    FItems: TStrings;
    FBuffered: Integer;
    FLastFlush: Cardinal;
    FFlushRefCount: Integer;
    FLocker: TCriticalSection;
    procedure BeginWrite; override;
    procedure EndWrite; override;
    procedure DoItemsUpdated;
    procedure LazyWrite; override;
    procedure FlushLogs;
  public
    constructor Create;
    destructor Destroy; override;
    function WriteItem(AItem: PQLogItem): Boolean; override;
    procedure HandleNeeded; override;
    property MaxItems: Integer read FMaxItems write FMaxItems;
    property Items: TStrings read FItems write FItems;
    property LazyMode;
  end;

  IPerfCounter = interface
    ['{FD288B18-F6C8-4EF7-B66B-38267D1A80FA}']
    procedure MarkEscape(const ATag: String);
  end;

procedure PostLog(ALevel: TQLogLevel; const AMsg: QStringW; const ATag: QStringW = ''); overload;
procedure PostLog(ALevel: TQLogLevel; const fmt: PWideChar; Args: array of const; const ATag: QStringW = ''); overload;
function CalcPerf(const ATag: QStringW; const ALogToConsole: Boolean = true): IPerfCounter;
{$IFDEF POSIX}
function GetCurrentProcessId: Integer;
{$ENDIF}
{$IFDEF ANDROID}
function GetExtSDDir: String;
{$ENDIF}
function SetDefaultLogFile(const AFileName: QStringW = ''; AMaxSize: Int64 = 2097152; // 2MB
  ARenameHistory: Boolean = true; AOneFilePerDay: Boolean = False): TQLogFileWriter;

const
  ELOG_WRITE_FAILURE = $80000001;

var
  Logs: TQLog;
  PerfTagStartFormat, PerfTagStopFormat: QStringW;

implementation

resourcestring
  SLogSeekError = '无法定位到第%d条日志记录';
  SHandleNeeded = '需要的日志写入对象句柄无法创建。';
  SCantCreateLogFile = '无法创建指定的日志文件 "%s"。';
  SCantCreateCastor = '无法创建日志广播对象。';
  SUnsupportSysLogEncoding = 'Syslog只支持Ansi和Utf8编码两种格式。';
  SZlibDLLMissed = 'zlib1.dll未找到，不支持压缩分卷日志。';
  SPerfTagStartFormat = '[%s] 开始执行';
  SPerfTagStopFormat = '[%s] 执行完成，用时 %d ms';

const
  SItemBreak: array [0 .. 2] of WideChar = (#$3000, #13, #10);
  LogLevelText: array [llEmergency .. llDebug] of QStringW = ('[EMG]', '[ALERT]', '[FATAL]', '[ERROR]', '[WARN]', '[HINT]',
    '[MSG]', '[DEBUG]');

var
  DefaultLogWriter: TQLogFileWriter;

type
  TQLogCompressThread = class(TThread)
  protected
    FLogFileName: QStringW;
    procedure Execute; override;
  public
    constructor Create(ALogFileName: QStringW); overload;
  end;

  TQPerfCounter = class(TInterfacedObject, IPerfCounter)
  private
    FTag: QStringW;
    FStartTick, FLastTick: Cardinal;
    FLogToConsole: Boolean;
  public
    constructor Create(const ATag: QStringW; ALogToConsole: Boolean); overload;
    destructor Destroy; override;
    procedure MarkEscape(const ATag: String);
  end;

{$IF RTLVersion<26}

  gzFile = Pointer;
  z_off_t = Longint;
  _gzopen = function(path: PAnsiChar; Mode: PAnsiChar): gzFile; cdecl;
  // _gzseek = function(file_: gzFile; offset: z_off_t; flush: Integer)
  // : z_off_t; cdecl;
  // _gztell = function(file_: gzFile): z_off_t; cdecl;
  _gzwrite = function(file_: gzFile; const buf; len: Cardinal): Integer; cdecl;
  _gzclose = function(file_: gzFile): Integer; cdecl;

var
  gzopen: _gzopen;
  // gzseek: _gzseek;
  // gztell: _gztell;
  gzwrite: _gzwrite;
  gzclose: _gzclose;
  zlibhandle: THandle;
{$IFEND <XE5UP}
{$IFDEF POSIX}

function GetCurrentProcessId: Integer;
begin
  Result := getpid;
end;
{$ENDIF}

function CalcPerf(const ATag: QStringW; const ALogToConsole: Boolean): IPerfCounter;
begin
  Result := TQPerfCounter.Create(ATag, ALogToConsole);
end;

function FormatSyslogTime(ATimeStamp: TDateTime): QStringW;
var
  Y, M, D: Word;
const
  LinuxMonth: array [0 .. 11] of QStringW = ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct',
    'Nov', 'Dec');
begin
  DecodeDate(ATimeStamp, Y, M, D);
  // Aug 24 05:34:00 CST 1987
  Result := LinuxMonth[M - 1];
  if D < 10 then
    Result := Result + '  ' + IntToStr(D)
  else
    Result := Result + ' ' + IntToStr(D);
  Result := Result + ' ' + FormatDateTime('hh:nn:ss', ATimeStamp);
end;

function HostName: QStringW;
var
  AName: QStringA;
begin
  AName.Length := 64;
  gethostname(Pointer(PQCharA(AName)), 64);
  Result := DeleteCharW(qstring.AnsiDecode(PQCharA(AName), -1), ' '#9#10#13);
end;

{$IFDEF ANDROID}

function GetExtSDDir: String;
var
  AList: TStringDynArray;
  S: String;
  I, J, ALastNo, ANo: Integer;
const
  ExtSDCardNames: array [0 .. 7] of String = ('/mnt/ext_sdcard', '/mnt/extsd', '/mnt/ext_card', '/mnt/external_sd',
    '/mnt/ext_sd', '/mnt/external', '/mnt/extSdCard', '/mnt/externalSdCard');
begin
  Result := '';
  AList := TDirectory.GetDirectories('/mnt');
  ALastNo := 0;
  for I := 0 to High(AList) do
  begin
    S := AList[I];
    for J := 0 to High(ExtSDCardNames) do
    begin
      if S = ExtSDCardNames[J] then
      begin
        Result := S + '/';
        Exit;
      end
    end;
    if StartWithW(PWideChar(S), '/mnt/sdcard', False) then
    begin
      if TryStrToInt(RightStrW(AList[I], Length(AList[I]) - 11, False), ANo) then
      begin
        if ANo > ALastNo then
        begin
          ALastNo := ANo;
          Result := AList[I] + '/';
        end;
      end;
    end;
  end;
end;
{$ENDIF}

function SetDefaultLogFile(const AFileName: QStringW; AMaxSize: Int64; ARenameHistory: Boolean; AOneFilePerDay: Boolean)
  : TQLogFileWriter;
var
  AWriter: TQLogFileWriter;
begin
  if DefaultLogWriter = nil then
  begin
    if Length(AFileName) > 0 then
      AWriter := TQLogFileWriter.Create(AFileName)
    else
      AWriter := TQLogFileWriter.Create;
    AWriter.MaxSize := AMaxSize;
    if ARenameHistory then
      AWriter.CreateMode := lcmRename
    else
      AWriter.CreateMode := lcmAppend;
    AWriter.OneFilePerDay := AOneFilePerDay;
    if AtomicCmpExchange(Pointer(DefaultLogWriter), Pointer(AWriter), nil) <> nil then
      FreeAndNil(AWriter)
    else
      Logs.Castor.AddWriter(DefaultLogWriter);
  end;
  Result := DefaultLogWriter;
end;

procedure PostLog(ALevel: TQLogLevel; const AMsg, ATag: QStringW);
begin
  Logs.Post(ALevel, AMsg, ATag);
end;

procedure PostLog(ALevel: TQLogLevel; const fmt: PWideChar; Args: array of const; const ATag: QStringW);
begin
  Logs.Post(ALevel, fmt, Args, ATag);
end;

function CreateItemBuffer(ALevel: TQLogLevel; AMsgLen, ATagLen: Integer): PQLogItem;
begin
  AMsgLen := AMsgLen shl 1;
  ATagLen := ATagLen shl 1;
  GetMem(Result, SizeOf(TQLogItem) + AMsgLen + ATagLen);
  Result.Next := nil;
  Result.ThreadId := GetCurrentThreadId;
  Result.TimeStamp := Now;
  Result.Level := ALevel;
  Result.MsgLen := AMsgLen;
  Result.TagLen := ATagLen;
end;

function CreateItem(const AMsg, ATag: QStringW; ALevel: TQLogLevel): PQLogItem;
begin
  Result := CreateItemBuffer(ALevel, Length(AMsg), Length(ATag));
  if (Result.MsgLen + Result.TagLen) > 0 then
  begin
    Move(PQCharW(AMsg)^, Result.Text[0], Result.MsgLen);
    if Result.TagLen > 0 then
      Move(PQCharW(ATag)^, Result.Text[Result.MsgLen shr 1], Result.TagLen);
  end;
end;

function CopyItem(const ASource: PQLogItem): PQLogItem; // inline;
var
  ALen: Integer;
begin
  ALen := SizeOf(TQLogItem) + ASource.MsgLen + ASource.TagLen;
  GetMem(Result, ALen);
  Move(ASource^, Result^, ALen);
  Result.Next := nil;
  Result.Prior := nil;
end;

procedure FreeItem(AItem: PQLogItem);
begin
  FreeMem(AItem);
end;

// TQLogReader
constructor TQLogReader.Create;
begin

end;

destructor TQLogReader.Destroy;
begin

  inherited;
end;

function TQLogReader.First: Boolean;
begin
  Result := MoveTo(0);
end;

function TQLogReader.GetCount: Int64;
begin
  Result := 0;
end;

function TQLogReader.Last: Boolean;
begin
  Result := MoveTo(Count - 1);
end;

function TQLogReader.MoveTo(AIndex: Int64): Boolean;
begin
  Result := False;
end;

function TQLogReader.Next: Boolean;
begin
  Result := MoveTo(FItemIndex + 1);
end;

function TQLogReader.Prior: Boolean;
begin
  Result := MoveTo(FItemIndex - 1);
end;

function TQLogReader.ReadItem(var AMsg: QStringW; ALevel: TQLogLevel): Boolean;
begin
  Result := False;
end;

procedure TQLogReader.SetItemIndex(const Value: Int64);
begin
  if FItemIndex <> Value then
  begin
    if not MoveTo(Value) then
      raise EXCEPTIOn.Create(Format(SLogSeekError, [Value]));
  end;
end;

// TQLogWriter
function TQLogWriter.Accept(AItem: PQLogItem): Boolean;
var
  pTags, pTag: PWideChar;
  ATag: QStringW;
begin
  Result := Enabled and (AItem.Level in AcceptLevels);
  if Result and (Length(FAcceptTags) > 0) then
  begin
    Result := (AItem.TagLen > 0);
    if Result then
    begin
      pTags := PWideChar(FAcceptTags);
      ATag := AItem.Tag;
      pTag := StrIStrW(pTags, PWideChar(ATag));
      if Assigned(pTag) then
      begin
        if pTag <> pTags then
        begin
          Dec(pTag);
          if pTag^ = ',' then
          begin
            Inc(pTag, Length(ATag) + 1);
            Result := (pTag^ = ',') or (pTag^ = #0);
          end
          else
            Result := False;
        end
        else
        begin
          Inc(pTag, Length(ATag));
          Result := (pTag^ = ',') or (pTag^ = #0);
        end;
      end;
    end;
  end;
  if Assigned(OnAccept) then
    OnAccept(Self, AItem, Result);
end;

procedure TQLogWriter.BeginWrite;
begin

end;

constructor TQLogWriter.Create;
begin
  inherited;
  FEnabled := true;
  // 默认记录所有日志
  FAcceptLevels := [llEmergency, llAlert, llFatal, llError, llWarning, llHint, llMessage, llDebug];
end;

destructor TQLogWriter.Destroy;
begin
  inherited;
end;

procedure TQLogWriter.EndWrite;
begin

end;

procedure TQLogWriter.HandleNeeded;
begin
  raise EXCEPTIOn.Create(SHandleNeeded);
end;

function TQLogWriter.ItemToText(AItem: PQLogItem): String;
begin
  Result := '[' + IntToStr(AItem.ThreadId) + ']' + FormatDateTime('hh:nn:ss.zz', AItem.TimeStamp) + ' ' +
    LogLevelText[AItem.Level] + ':' + StrDupX(@AItem.Text[0], AItem.MsgLen shr 1);
end;

procedure TQLogWriter.LazyWrite;
begin
  // 啥也不干，反正我也不知道该干啥
end;

procedure TQLogWriter.SetLazyMode(const Value: Boolean);
begin
  if FLazyMode <> Value then
  begin
    FLazyMode := Value;
    if Value then // 如果是懒人模式，则延迟100ms写入
      Logs.Castor.LazyInterval := 100;
  end;
end;

function TQLogWriter.WriteItem(AItem: PQLogItem): Boolean;
begin
  Result := False;
end;

{ TQLogFile }

constructor TQLogFileWriter.Create(const AFileName: QStringW; AWithIndex: Boolean);
begin
  inherited Create;
  FFileName := AFileName;
  FLogHandle := nil;
  FBuilder := TQStringCatHelperW.Create;
{$IF RTLVersion>=31}
  // Berlin 以后的版本强制启用懒汉模式
  LazyMode := true;
{$IFEND}
end;

function TQLogFileWriter.CompressLog(ALogFileName: QStringW): Boolean;
begin
  if MaxLogHistories > 0 then
    DeleteHistories;
{$IF RTLVersion<26}
  if not Assigned(gzopen) then
    Result := False
  else
{$IFEND <XE5}
  begin
    Result := true;
    if FileExists(ALogFileName) then // 日志文件没有被DeleteHistory清除
      TQLogCompressThread.Create(ALogFileName);
  end;
end;

constructor TQLogFileWriter.Create;
var
  APath: QStringW;
begin
  inherited Create;
  FBuilder := TQStringCatHelperW.Create;
  FFileName := ParamStr(0);
{$IFDEF MSWINDOWS}
  APath := ExtractFilePath(FFileName) + 'Logs\';;
  ForceDirectories(APath);
  FFileName := APath + DeleteRightW(ExtractFileName(FFileName), ExtractFileExt(FFileName), true, 1) + '.log';
{$ELSE}
  APath := TPath.GetSharedDocumentsPath;
  if Length(APath) = 0 then
    APath := ExtractFilePath(FFileName) + TPath.DirectorySeparatorChar + 'Logs/';
  ForceDirectories(APath);
  FFileName := ExtractFileName(ParamStr(0));
  FFileName := APath + DeleteRightW(FFileName, ExtractFileExt(FFileName), true, 1) + '.log';
{$ENDIF}
{$IF RTLVersion>=31}
  // Berlin 以后的版本强制启用懒汉模式
  LazyMode := true;
{$IFEND}
end;

procedure TQLogFileWriter.DeleteHistories;
var
  sr: TSearchRec;
  AList: TStringList;
  APath, AFileName, ASearchedFile: QStringW;
  I, ANeedDeletes: Integer;
begin
  APath := ExtractFilePath(FFileName);
  if FindFirst(APath + '*.*', faAnyFile, sr) = 0 then
  begin
    AFileName := ExtractFileName(FFileName);
    AFileName := LeftStrW(AFileName, Length(AFileName) - Length(ExtractFileExt(AFileName)), False);
    AList := TStringList.Create;
    try
      // 查找所有的日志文件并按文件名排序，由于文件名是按日期自动排序的，所以理论上不需要比较时间
      AList.Sorted := true;
      repeat
        ASearchedFile := sr.Name;
        if ((sr.Attr and faDirectory) = 0) and (strcmpW(PQCharW(ASearchedFile), PQCharW(AFileName), true) <> 0) and
          StartWithW(PQCharW(ASearchedFile), PQCharW(AFileName), true) and
          (EndWithW(ASearchedFile, '.log', true) or EndWithW(ASearchedFile, '.gz', true)) then
          AList.Add(sr.Name);
      until FindNext(sr) <> 0;
      SysUtils.FindClose(sr);
      // 不包含当前日志文件，所以要减1
      if AList.Count > MaxLogHistories then
      begin
        ANeedDeletes := AList.Count - MaxLogHistories - 1;
        for I := 0 to ANeedDeletes do
          SysUtils.DeleteFile(APath + AList[I]);
      end;
    finally
      FreeAndNil(AList);
    end;
  end;
end;

destructor TQLogFileWriter.Destroy;
begin
  FreeObject(FBuilder);
  FreeObject(FLogHandle);
  inherited;
end;

procedure TQLogFileWriter.EndWrite;
begin
  inherited;
  FlushBuffer;
end;

function TQLogFileWriter.FlushBuffer: Boolean;
var
  AWriteBytes: Cardinal;
  ps: PByte;
  l: Integer;
begin
  Result := true;
  l := FBuilder.Position shl 1;
  if l > 0 then
  begin
    ps := PByte(FBuilder.Start);
    repeat
      AWriteBytes := FLogHandle.Write(ps^, l);
      if AWriteBytes = 0 then
      begin
        FCastor.SetLastError(ELOG_WRITE_FAILURE, SysErrorMessage(GetLastError));
        DebugOut('无法写入日志数据：%s', [FCastor.FLastErrorMsg]);
        Result := False;
        Break
      end
      else
      begin
        Dec(l, AWriteBytes);
        Inc(ps, AWriteBytes);
        Inc(FPosition, AWriteBytes);
      end;
    until l = 0;
    FBuilder.Position := 0;
  end;
end;

procedure TQLogFileWriter.HandleNeeded;
var
  ALogFileName, AExt: QStringW;
  AIndex: Cardinal;
  ACreateMode: TQLogFileCreateMode;
const
  UTF16BOM: Word = $FEFF;
  function CanAccess(AFileName: QStringW): Boolean;
  var
    AHandle: THandle;
  begin
    AHandle := FileOpen(AFileName, fmOpenReadWrite);
    if AHandle = THandle(-1) then
      Result := False
    else
    begin
      Result := true;
      FileClose(AHandle);
    end;
  end;
  procedure NextFileName;
  begin
    FFileName := ALogFileName + '_' + IntToStr(GetCurrentProcessId) + '_' + IntToStr(AIndex) + AExt;
    Inc(AIndex);
  end;
  procedure CheckPath;
  var
    ADir: String;
  begin
    ADir := ExtractFilePath(ExpandFileName(FFileName));
    if not ForceDirectories(ADir) then
      raise EXCEPTIOn.CreateFmt(SCantCreateLogFile, [FFileName]);
  end;
  function DayChanged: Boolean;
  var
    AFileDate: TDateTime;
  begin
    FileAge(FFileName, AFileDate);
    Result := Trunc(AFileDate) <> Trunc(Now);
  end;

begin
  CheckPath;
  if (CreateMode = lcmRename) and FileExists(FFileName) and CanAccess(FFileName) and (not OneFilePerDay) then
    RenameHistory
  else
  begin
    if not Assigned(FLogHandle) then
    begin
      AIndex := 1;
      AExt := ExtractFileExt(FFileName);
      ACreateMode := CreateMode;
      if OneFilePerDay then // 如果要求每天一个日志，则强制替换为追加模式
        ACreateMode := lcmAppend;
      ALogFileName := Copy(FFileName, 1, Length(FFileName) - Length(AExt));
      repeat
        try
          case ACreateMode of
            lcmReplace, lcmRename:
              begin
                if (not FileExists(FFileName)) or CanAccess(FFileName) then
                begin
                  FLogHandle := TQLogFileStream.Create(FFileName, fmCreate);
                  FLogHandle.WriteBuffer(UTF16BOM, SizeOf(UTF16BOM));
                  // 好吧，创建的禁止他人读，我创建再打开还不行嘛
                  FreeObject(FLogHandle);
                  FLogHandle := TQLogFileStream.Create(FFileName, fmOpenWrite or fmShareDenyWrite);
                end
                else
                  NextFileName;
              end;
            lcmAppend:
              begin
                if FileExists(FFileName) and CanAccess(FFileName) then
                begin
                  if OneFilePerDay and DayChanged then
                  // 如果是每天一个日志文件，则检查文件最后写入日期
                  begin
                    RenameHistory;
                    ACreateMode := lcmReplace;
                  end
                  else
                  begin
                    FLogHandle := TQLogFileStream.Create(FFileName, fmOpenWrite or fmShareDenyWrite);
                  end;
                end
                else
                  ACreateMode := lcmReplace;
                FLastTime := 0;
              end;
          end;
        except
          NextFileName;
        end;
      until Assigned(FLogHandle) or (AIndex = 100);
      if not Assigned(FLogHandle) then
        raise EXCEPTIOn.CreateFmt(SCantCreateLogFile, [FFileName]);
      FLogHandle.Seek(0, soEnd);
      FPosition := FLogHandle.Position;
    end;
  end;
end;

procedure TQLogFileWriter.LazyWrite;
begin
  inherited;
  FlushBuffer;
end;

procedure TQLogFileWriter.RenameHistory;
var
  ALogFileName, AOldName, ATimeStamp, AExt: QStringW;
begin
  if Assigned(FLogHandle) then
  begin
    AOldName := FLogHandle.FileName;
    FreeObject(FLogHandle);
    FLogHandle := nil;
  end
  else
    AOldName := FFileName;
  if FileExists(AOldName) then
  begin
    if SizeOfFile(AOldName) <= 2 then
      SysUtils.DeleteFile(AOldName)
    else
    begin
      AExt := ExtractFileExt(AOldName);
      ATimeStamp := FormatDateTime('yyyymmddhhnnsszzz', Now);
      ALogFileName := StrDupX(PQCharW(AOldName), Length(AOldName) - Length(AExt)) + '_' + ATimeStamp + AExt;
      if RenameFile(FFileName, ALogFileName) then
      begin
        // 创建线程压缩日志文件
        if not CompressLog(ALogFileName) then
        begin
          HandleNeeded;
          PostLog(llWarning, SZlibDLLMissed);
          Exit;
        end;
      end;
    end;
    HandleNeeded;
  end;
end;

procedure TQLogFileWriter.SetMaxLogHistories(const Value: Integer);
begin
  if FMaxLogHistories <> Value then
  begin
    FMaxLogHistories := Value;
    DeleteHistories;
  end;
end;

function TQLogFileWriter.WriteItem(AItem: PQLogItem): Boolean;
  procedure TimeChangeCheck;
  var
    ADayChanged: Boolean;
  begin
    if FLastTime <> AItem.TimeStamp then
    begin
      if Trunc(FLastTime) <> Trunc(AItem.TimeStamp) then
        ADayChanged := true
      else
        ADayChanged := False;
      if ADayChanged then
      begin
        if OneFilePerDay and (FLastTime > 0) then
          RenameHistory;
        FBuilder.Cat(FormatDateTime('[yyyy-mm-dd]', AItem.TimeStamp)).Cat(SLineBreak);
      end;
      FLastTime := AItem.TimeStamp;
    end;
    FLastTimeStamp := FormatDateTime('[hh:nn:ss.zzz]', FLastTime);
  end;

begin
  Result := False;
  if FLogHandle <> nil then
  begin
    Result := true;
    TimeChangeCheck;
    if FLastThreadId <> AItem.ThreadId then
    begin
      FLastThreadId := AItem.ThreadId;
      FLastThread := '[' + IntToStr(FLastThreadId) + ']';
    end;
    FBuilder.Cat(FLastTimeStamp).Cat(FLastThread).Cat(LogLevelText[AItem.Level]).Cat(':')
      .Cat(@AItem.Text[0], AItem.MsgLen shr 1);
    FBuilder.Cat(SLineBreak);
    if (MaxSize > 0) and (FPosition + FBuilder.Position >= MaxSize) then
    begin
      FlushBuffer;
      // 超过单个日志文件大小限制，将现在的日志文件重命名并压缩保存
      RenameHistory;
    end;
  end;
end;

{ TQLogCastor }
// 后添加的始终在前面
procedure TQLogCastor.AddWriter(AWriter: TQLogWriter);
var
  AItem: PLogWriterItem;
begin
  AWriter.HandleNeeded;
  New(AItem);
  AItem.Prior := nil;
  AItem.Writer := AWriter;
  AWriter.FCastor := Self;
  FCS.Enter;
  AItem.Next := FWriters;
  if Assigned(FWriters) then
    FWriters.Prior := AItem;
  FWriters := AItem;
  FCS.Leave;
end;

procedure TQLogCastor.ClearWriters;
var
  AItem, ANext: PLogWriterItem;
begin
  FCS.Enter;
  AItem := FWriters;
  FWriters := nil;
  FCS.Leave;
  try
    while Assigned(AItem) do
    begin
      ANext := AItem.Next;
      FreeAndNil(AItem.Writer);
      Dispose(AItem);
      AItem := ANext;
    end;
  finally
    FCS.Leave;
  end;
end;

constructor TQLogCastor.Create(AOwner: TQLog);
begin
  inherited Create(true);
  FCS := TCriticalSection.Create;
  FNotifyHandle := TEvent.Create(nil, False, False, '');
  FOwner := AOwner;
  Suspended := False;
end;

destructor TQLogCastor.Destroy;
begin
{$IFDEF NEXTGEN}
  FNotifyHandle.DisposeOf;
  FCS.DisposeOf;
{$ELSE}
  FNotifyHandle.Free;
  FCS.Free;
{$ENDIF}
  inherited;
end;

procedure TQLogCastor.Execute;
var
  APrior: PQLogItem;
  procedure WriteItem;
  begin
    FirstWriter;
    while Assigned(FActiveWriter) do
    begin
      if FActiveWriter.Writer.Accept(ActiveLog) then
      begin
        if not FActiveWriter.Writer.WriteItem(ActiveLog) then
        begin
          if FLastError = ELOG_WRITE_FAILURE then
          begin
            // Write Error handle
          end;
        end;
      end;
      NextWriter;
    end;
  end;

begin
  while not Terminated do
  begin
    if WaitForLog and (FOwner.Count - FOwner.Flushed > 0) then
    begin
      FOwner.BeginWrite;
      try
        FActiveLog := FetchNext;
        // 开始写入日志
        while FActiveLog <> nil do
        begin
          WriteItem;
          APrior := FActiveLog;
          FActiveLog := APrior.Next;
          FreeItem(APrior);
          Inc(FOwner.FFlushed);
        end;
      finally
        FOwner.EndWrite;
      end;
    end;
  end;
  ClearWriters;
end;

function TQLogCastor.FetchNext: PQLogItem;
begin
  Result := FOwner.Pop;
end;

function TQLogCastor.FirstWriter: PLogWriterItem;
begin
  FActiveWriter := FWriters;
  Result := FActiveWriter;
end;

{$IFNDEF UNICODE}

function TQLogCastor.GetFinished: Boolean;
  function WinThreadExists: Boolean;
  var
    ASnapshot: THandle;
    AEntry: TThreadEntry32;
    AProcessId: DWORD;
  begin
    Result := False;
    ASnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
    if ASnapshot = INVALID_HANDLE_VALUE then
      Exit;
    try
      AEntry.dwSize := SizeOf(TThreadEntry32);
      if Thread32First(ASnapshot, AEntry) then
      begin
        AProcessId := GetCurrentProcessId;
        repeat
          if ((AEntry.th32OwnerProcessID = AProcessId) or (AProcessId = $FFFFFFFF)) and (AEntry.th32ThreadID = ThreadId) then
          begin
            Result := true;
            Break;
          end;
        until not Thread32Next(ASnapshot, AEntry);
      end;
    finally
      CloseHandle(ASnapshot);
    end;
  end;

begin
  Result := PBoolean(IntPtr(@ReturnValue) + SizeOf(Integer))^;
  if not Result then
    Result := not WinThreadExists;
end;
{$ENDIF}

procedure TQLogCastor.LogAdded;
begin
  FNotifyHandle.SetEvent;
end;

function TQLogCastor.NextWriter: PLogWriterItem;
begin
  FCS.Enter;
  if Assigned(FActiveWriter) then
    FActiveWriter := FActiveWriter.Next
  else
    FActiveWriter := FWriters;
  Result := FActiveWriter;
  FCS.Leave;
end;

procedure TQLogCastor.RemoveWriter(AWriter: TQLogWriter);
var
  AItem: PLogWriterItem;
begin
  repeat
    FCS.Enter;
    try
      if not Assigned(FActiveWriter) or (FActiveWriter.Writer <> AWriter) then
      begin
        AItem := FWriters;
        while Assigned(AItem) do
        begin
          if AItem.Writer = AWriter then
          begin
            if AItem = FWriters then
              FWriters := AItem.Next;
            if Assigned(AItem.Prior) then
              AItem.Prior.Next := AItem.Next;
            if Assigned(AItem.Next) then
              AItem.Next.Prior := AItem.Prior;
            Break;
          end;
        end;
        Break;
      end
    finally
      FCS.Leave;
      Yield;
    end;
  until 1 > 2;
  if Assigned(AItem) then
    Dispose(AItem);
end;

procedure TQLogCastor.SetLastError(ACode: Cardinal; const AMsg: QStringW);
begin
  FLastError := ACode;
  FLastErrorMsg := AMsg;
end;

function TQLogCastor.WaitForLog: Boolean;
var
  AResult: TWaitResult;
  AInterval: Cardinal;
begin
  if LazyInterval > 0 then
  begin
    AInterval := LazyInterval;
    repeat
      AResult := FNotifyHandle.WaitFor(AInterval);
      if AResult = wrTimeout then
      begin
        // AInterval := INFINITE;
        FirstWriter;
        while Assigned(FActiveWriter) do
        begin
          FActiveWriter.Writer.LazyWrite;
          NextWriter;
        end;
      end;
    until (AResult = wrSignaled) or Terminated;
    Result := AResult = wrSignaled;
  end
  else
    Result := (FNotifyHandle.WaitFor(INFINITE) = wrSignaled);
end;

{ TQLog }

function TQLog.Pop: PQLogItem;
begin
  Lock;
  Result := FList.First;
  FList.Value := 0;
  Unlock;
end;

procedure TQLog.Post(ALevel: TQLogLevel; const AFormat: QStringW; Args: array of const; const ATag: QStringW);
begin
  if Enabled and (not FInFree) and (ALevel in AcceptLevels) then
  begin
{$IFDEF NEXTGEN}
    Post(ALevel, Format(AFormat, Args), ATag);
{$ELSE}
    Post(ALevel, WideFormat(AFormat, Args), ATag);
{$ENDIF}
  end;
end;

procedure TQLog.SetMode(const Value: TQLogMode);
begin
  if FMode <> Value then
  begin
    FMode := Value;
    if Value = lmSync then
      WaitLogWrote;
  end;
end;

procedure TQLog.Post(ALevel: TQLogLevel; const AMsg, ATag: QStringW);
var
  AItem: PQLogItem;
begin
  if Enabled and (not FInFree) and (ALevel in AcceptLevels) then
  begin
    AItem := CreateItem(AMsg, ATag, ALevel);
    AItem.Next := nil;
    // 使用临界锁定版
    Lock();
    Inc(FCount);
    if FList.FirstVal = 0 then
    begin
      FList.First := AItem;
      FList.Last := AItem;
    end
    else
    begin
      FList.Last.Next := AItem;
      FList.Last := AItem;
    end;
    Unlock;
    FCastor.LogAdded;
    WaitLogWrote;
  end;
end;

procedure TQLog.BeginWrite;
var
  AWriter: PLogWriterItem;
begin
  AWriter := FCastor.FirstWriter;
  while Assigned(AWriter) do
  begin
    AWriter.Writer.BeginWrite;
    AWriter := FCastor.NextWriter;
  end;
end;

constructor TQLog.Create;
begin
  inherited;
  FList.Value := 0;
  FInFree := False;
  FCastor := TQLogCastor.Create(Self);
  FCS := TCriticalSection.Create;
  FSyncEvent := TEvent.Create(nil, true, False, '');
  FMode := lmAsyn;
  FEnabled := true;
  // 默认输出全部日志
  FAcceptLevels := [llEmergency, llAlert, llFatal, llError, llWarning, llHint, llMessage, llDebug];
end;

function TQLog.CreateCastor: TQLogCastor;
begin
  Result := TQLogCastor.Create(Self);
end;

destructor TQLog.Destroy;
begin
  FInFree := true;
  // 等待日志全部写入完成
  while Assigned(FList.First) do
    Sleep(10);
  FCastor.Terminate;
  FCastor.FNotifyHandle.SetEvent;
  while not FCastor.Finished do
    Sleep(10);
  FreeObject(FCastor);
  FreeObject(FCS);
  FreeObject(FSyncEvent);
  inherited;
end;

procedure TQLog.EndWrite;
var
  AWriter: PLogWriterItem;
begin
  FSyncEvent.SetEvent;
  AWriter := FCastor.FirstWriter;
  while Assigned(AWriter) do
  begin
    AWriter.Writer.EndWrite;
    AWriter := FCastor.NextWriter;
  end;
end;

function TQLog.GetCastor: TQLogCastor;
begin
  if FCastor = nil then
    FCastor := CreateCastor;
  Result := FCastor;
end;

procedure TQLog.Lock;
begin
  FCS.Enter;
end;

procedure TQLog.Unlock;
begin
  FCS.Leave;
end;

procedure TQLog.WaitLogWrote;
begin
  while (Mode = lmSync) and (Count > Flushed) do
    FSyncEvent.WaitFor(50);
end;

{ TQLogConsoleWriter }

constructor TQLogConsoleWriter.Create;
begin
  inherited;
{$IFDEF MSWINDOWS}
  UseDebugConsole := true;
{$ENDIF}
end;
{$IFDEF MSWINDOWS}

constructor TQLogConsoleWriter.Create(AUseDebugConsole: Boolean);
begin
  inherited Create;
  UseDebugConsole := AUseDebugConsole;
end;
{$ENDIF}

procedure TQLogConsoleWriter.HandleNeeded;
begin
  // Nothing Needed
end;

function TQLogConsoleWriter.WriteItem(AItem: PQLogItem): Boolean;
var
  S: QStringW;
begin
  S := ItemToText(AItem);
{$IFDEF MSWINDOWS}
  S := LogLevelText[AItem.Level] + ' ' + S;
  if UseDebugConsole then
    OutputDebugStringW(PWideChar(S))
  else
    WriteLn(S);
{$ENDIF}
{$IFDEF ANDROID}
  case AItem.Level of
    llEmergency:
      __android_log_write(ANDROID_LOG_WARN, 'emerg', Pointer(PQCharA(qstring.Utf8Encode(S))));
    llAlert:
      __android_log_write(ANDROID_LOG_WARN, 'alert', Pointer(PQCharA(qstring.Utf8Encode(S))));
    llFatal:
      __android_log_write(ANDROID_LOG_FATAL, 'fatal', Pointer(PQCharA(qstring.Utf8Encode(S))));
    llError:
      __android_log_write(ANDROID_LOG_ERROR, 'error', Pointer(PQCharA(qstring.Utf8Encode(S))));
    llWarning:
      __android_log_write(ANDROID_LOG_WARN, 'warn', Pointer(PQCharA(qstring.Utf8Encode(S))));
    llHint, llMessage:
      __android_log_write(ANDROID_LOG_INFO, 'info', Pointer(PQCharA(qstring.Utf8Encode(S))));
    llDebug:
      __android_log_write(ANDROID_LOG_DEBUG, 'debug', Pointer(PQCharA(qstring.Utf8Encode(S))));
  end;
{$ENDIF}
{$IFDEF MACOS}
{$IFDEF IOS}
  NSLog(((StrToNSStr(S)) as ILocalObject).GetObjectID);
{$ELSE}
  WriteLn(S);
{$ENDIF}
{$ENDIF}
  Result := true;
end;

{ TQLogSocketWriter }

function TQLogSocketWriter.ConnectNeeded: Boolean;
begin
  if UseTCP then
  begin
    if FLastConnectTryTime >= 0 then
    begin
      Result := False;
      if (Now - FLastConnectTryTime) > 120 / 86400 then
      // 失败时间间隔大于2分钟，重试
      begin
        if connect(FSocket,
{$IFNDEF MSWINDOWS}sockaddr({$ENDIF}FReaderAddr{$IFNDEF MSWINDOWS}){$ENDIF}, SizeOf(sockaddr_in)) = 0 then
        begin
          Result := true;
          FLastConnectTryTime := -1;
        end
        else
          FLastConnectTryTime := Now;
      end;
    end
    else
      Result := true;
  end
  else
    Result := true;
end;

constructor TQLogSocketWriter.Create;
begin
  inherited;
  FServerPort := 514;
  // Syslog端口
  FTextEncoding := teUtf8;
  FBuilder := TQStringCatHelperW.Create(1024);
  // Syslog默认不超过1024字节
end;

constructor TQLogSocketWriter.Create(const AHost: String; APort: Word; AUseTcp: Boolean);
begin
  inherited Create;
  FTextEncoding := teUtf8;
  FBuilder := TQStringCatHelperW.Create(1024); // Syslog默认不超过1024字节
  FServerPort := APort;
  FServerHost := AHost;
  UseTCP := AUseTcp;
end;

destructor TQLogSocketWriter.Destroy;
begin
  FreeObject(FBuilder);
  if FSocket <> THandle(-1) then
  begin
{$IFDEF MSWINDOWS}
    closesocket(FSocket);
    WSACleanup;
{$ELSE}
    __close(FSocket);
{$ENDIF}
  end;
  inherited;
end;

procedure TQLogSocketWriter.HandleNeeded;
var
  ALastUseTcp: Boolean;
{$IFDEF MSWINDOWS}
  AData: WSAData;
{$ENDIF}
begin
  if Length(ServerHost) > 0 then
  begin
{$IFDEF MSWINDOWS}
    if WSAStartup(MakeWord(1, 1), AData) <> 0 then
      RaiseLastOSError(WSAGetLastError);
{$ENDIF}
    FReaderAddr.sin_family := AF_INET;
    FReaderAddr.sin_port := htons(ServerPort);
    FReaderAddr.sin_addr.s_addr := inet_addr(Pointer(PQCharA(qstring.AnsiEncode(ServerHost))));
    PInt64(@FReaderAddr.sin_zero[0])^ := 0;
    if UseTCP then
      FSocket := socket(AF_INET, SOCK_STREAM, 6)
    else
      FSocket := socket(AF_INET, SOCK_DGRAM, 17);
    if FSocket = THandle(-1) then
      RaiseLastOSError;
  end
  else
  begin
    // 查找QSyslogServer
    ALastUseTcp := UseTCP;
    UseTCP := False;
    ServerHost := '255.255.255.255';
    HandleNeeded;
    LookupServer;
    if FServerHost <> '255.255.255.255' then
    begin
      if ALastUseTcp then
      begin
        UseTCP := ALastUseTcp;
{$IFDEF MSWINDOWS}
        closesocket(FSocket);
{$ELSE}
        __close(FSocket);
{$ENDIF}
        FSocket := 0;
        HandleNeeded;
      end;
    end;
  end;
end;

function TQLogSocketWriter.LookupServer: Boolean;
var
  AMsg: QStringA;
  I, ALen, sr: Integer;
  l:
{$IFDEF MSWINDOWS}Integer{$ELSE} Cardinal{$ENDIF};
  AHost: sockaddr_in;
  ABuf: array [0 .. 4095] of Byte;
  tv: TIMEVAL;
  fdread:
{$IFDEF MSWINDOWS}TFdSet{$ELSE}FD_SET{$ENDIF};
begin
  AMsg := qstring.Utf8Encode('<9>' + FormatSyslogTime(Now) + ' ' + HostName + ' ~Who~Is~QLog~SysD~Server~');
  // 广播查找谁是QLogServer
  I := 1;
  setsockopt(FSocket, SOL_SOCKET, SO_BROADCAST,
{$IFDEF MSWINDOWS}PAnsiChar(@I){$ELSE}I{$ENDIF}, SizeOf(I));
  AHost.sin_family := AF_INET;
  AHost.sin_port := htons(ServerPort);
  AHost.sin_addr.s_addr := Longint(INADDR_BROADCAST);
  PInt64(@AHost.sin_zero[0])^ := 0;
  tv.tv_sec := 0;
  tv.tv_usec := 500 * 1000; // 50ms
  FD_ZERO(fdread);
{$IFDEF MSWINDOWS}
  FD_SET(FSocket, fdread);
{$ELSE}
  _FD_SET(FSocket, fdread);
{$ENDIF}
  Result := False;
  repeat
    sendto(FSocket, PQCharA(AMsg)^, AMsg.Length, 0, PSockAddr(@AHost)^, SizeOf(sockaddr_in));
    sr := select(FSocket + 1, @fdread, nil, nil, @tv);
    if sr > 0 then
    begin
      l := SizeOf(sockaddr_in);
      ALen := recvfrom(FSocket, ABuf[0], 4096, MSG_PEEK, PSockAddr(@FReaderAddr)^, l);
      if ALen <> -1 then
      begin
        recvfrom(FSocket, ABuf[0], ALen, 0, PSockAddr(@FReaderAddr)^, l);
        if qstring.Utf8Decode(@ABuf[0], ALen) = '~I~am~QLog~SysD~Server~' then
        begin
          FServerHost := String(inet_ntoa(FReaderAddr.sin_addr));
          Break;
        end;
      end;
    end;
    Inc(I);
  until I > 3;

end;

procedure TQLogSocketWriter.SetTextEncoding(const Value: TTextEncoding);
begin
  if Value in [teAnsi, teUtf8] then
  begin
    if FTextEncoding <> Value then
      FTextEncoding := Value;
  end
  else
    raise EXCEPTIOn(SUnsupportSysLogEncoding);
end;

function TQLogSocketWriter.WriteItem(AItem: PQLogItem): Boolean;
var
  p: PQCharA;
  ASize, ALen: Integer;
  APri: QStringW;
  AHeader, AText: QStringA;
  ABuf: array [0 .. 1023] of Byte;
  procedure CalcPri;
  begin
    APri := '<' + IntToStr(8 + Integer(FCastor.ActiveLog.Level)) + '> ';
    {
      0       Emergency: system is unusable
      1       Alert: action must be taken immediately
      2       Critical: critical conditions
      3       Error: error conditions
      4       Warning: warning conditions
      5       Notice: normal but significant condition
      6       Informational: informational messages
      7       Debug: debug-level messages
    }
  end;

  function CopyText: Integer;
  var
    ps, pd: PQCharA;
    ACharSize: Integer;
  begin
    if ALen + AHeader.Length < 1024 then
    begin
      Move(p^, ABuf[AHeader.Length], ALen);
      Inc(p, ALen);
      Result := AHeader.Length + ALen;
      ALen := 0;
    end
    else
    begin
      pd := @ABuf[AHeader.Length];
      ps := @ABuf[0];
      Result := AHeader.Length;
      while p^ <> 0 do
      begin
        if TextEncoding = teAnsi then
          ACharSize := CharSizeA(p)
        else
          ACharSize := CharSizeU(p);
        if (IntPtr(pd) - IntPtr(ps)) + ACharSize <= 1024 then
        begin
          while ACharSize > 0 do
          begin
            pd^ := p^;
            Inc(p);
            Inc(pd);
            Dec(ACharSize);
          end;
        end
        else
        begin
          Result := IntPtr(pd) - IntPtr(ps);
          Dec(ALen, Result - AHeader.Length);
          Break;
        end;
      end;
    end;
  end;

begin
  if FSocket = 0 then
  begin
    Result := False;
    Exit;
  end;
  if not ConnectNeeded then
  begin
    Result := False;
    Exit;
  end;
  Result := true;
  FBuilder.Position := 0;
  CalcPri;
  FBuilder.Cat(APri);
  FBuilder.Cat(FormatSyslogTime(AItem.TimeStamp)).Cat(' ');
  FBuilder.Cat(HostName).Cat(' ');
  FBuilder.Cat('[').Cat(IntToStr(AItem.ThreadId)).Cat(']');
  FBuilder.Cat(LogLevelText[AItem.Level]);
  AHeader := qstring.Utf8Encode(FBuilder.Value);
  if TextEncoding = teAnsi then
    AText := qstring.AnsiEncode(@AItem.Text[0], AItem.MsgLen shr 1)
  else
    AText := qstring.Utf8Encode(@AItem.Text[0], AItem.MsgLen shr 1);
  p := PQCharA(AText);
  ALen := AText.Length;
  repeat
    Move(PQCharA(AHeader)^, ABuf[0], AHeader.Length);
    ASize := CopyText;
    sendto(FSocket, ABuf[0], ASize, 0, PSockAddr(@FReaderAddr)^, SizeOf(sockaddr_in));
  until ALen <= 0;
end;

{ TQLogCompressThread }

constructor TQLogCompressThread.Create(ALogFileName: QStringW);
begin
  FLogFileName := ALogFileName;
  inherited Create(true);
  FreeOnTerminate := true;
  Suspended := False;
end;

procedure TQLogCompressThread.Execute;
const
{$IFDEF NEXTGEN}
  AMode: MarshaledAString = 'wb';
{$ELSE}
  AMode: PAnsiChar = 'wb';
{$ENDIF}
  procedure DoCompress(AFileName: QStringW);
  var
    AFile: gzFile;
    ABuf: array [0 .. 65535] of Byte;
    AStream: TFileStream;
    AReaded: Integer;
  begin
    AStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
    try
      AFile := gzopen(Pointer(PQCharA(AnsiEncode(AFileName + '.gz'))), AMode);
      if AFile <> nil then
      begin
        repeat
          AReaded := AStream.Read(ABuf[0], 65536);
          if AReaded > 0 then
            gzwrite(AFile, ABuf[0], AReaded);
        until AReaded = 0;
        gzclose(AFile);
      end;
    finally
      AStream.Free;
      SysUtils.DeleteFile(AFileName);
    end;
  end;

begin
  if Length(FLogFileName) > 0 then
    DoCompress(FLogFileName);
end;

{ TQPerfCounter }

constructor TQPerfCounter.Create(const ATag: QStringW; ALogToConsole: Boolean);
begin
  inherited Create;
  FTag := ATag;
  FLogToConsole := ALogToConsole;
  FStartTick :=
{$IF RTLVersion>=23}TThread.{$IFEND} GetTickCount;
  FLastTick := FStartTick;
  if FLogToConsole then
    DebugOut(PQCharW(PerfTagStartFormat), [ATag])
  else
    PostLog(llDebug, PQCharW(PerfTagStartFormat), [ATag]);
end;

destructor TQPerfCounter.Destroy;
begin
  if FLogToConsole then
    DebugOut(PQCharW(PerfTagStopFormat), [FTag,
{$IF RTLVersion>=23}TThread.{$IFEND}GetTickCount - FStartTick])
  else
    PostLog(llDebug, PQCharW(PerfTagStopFormat), [FTag,
{$IF RTLVersion>=23}TThread.{$IFEND}GetTickCount - FStartTick]);
  inherited;
end;

procedure TQPerfCounter.MarkEscape(const ATag: String);
var
  ATick: Cardinal;
begin
  ATick := {$IF RTLVersion>=23}TThread.{$IFEND} GetTickCount;
  if FLogToConsole then
    DebugOut(PQCharW('  ' + PerfTagStopFormat), [ATag, ATick - FLastTick])
  else
    PostLog(llDebug, PQCharW('  ' + PerfTagStopFormat), [ATag, ATick - FLastTick]);
  FLastTick := ATick;
end;

{ TQLogStringsWriter }

procedure TQLogStringsWriter.BeginWrite;
begin

end;

constructor TQLogStringsWriter.Create;
begin
  inherited;
  FLocker := TCriticalSection.Create;
end;

destructor TQLogStringsWriter.Destroy;
begin
  FreeAndNil(FLocker);
  inherited;
end;

procedure TQLogStringsWriter.DoItemsUpdated;
begin
  inherited;
  LazyWrite;
end;

procedure TQLogStringsWriter.EndWrite;
begin
  inherited;
  LazyWrite;
end;

procedure TQLogStringsWriter.FlushLogs;
var
  ADelta, ABuffered: Integer;
  AFirst, AItem: PQLogItem;
  // T: Cardinal;
  ATemp: TStrings;
  I: Integer;
begin
  if Assigned(FItems) and (FBuffered > 0) then
  begin
    // T := GetTickCount;
    FLocker.Enter;
    AFirst := FLazyList.First;
    FLazyList.First := nil;
    FLazyList.Last := nil;
    ABuffered := FBuffered;
    FBuffered := 0;
    FLocker.Leave;
    if FItems is TStringList then
      ATemp := FItems
    else // TListStrings一类的封装在大量记录时，如果直接操作，效率渣渣，不如用一个临时变量中转优化
      ATemp := TStringList.Create;
    ATemp.BeginUpdate;
    try
      if FMaxItems > 0 then
        ATemp.Capacity := FMaxItems // 不要在后台线程中修改它，这里假设不会
      else
        ATemp.Capacity := FItems.Count + ABuffered;
      if FMaxItems > 0 then
      begin
        while ABuffered > FMaxItems do
        begin
          AItem := AFirst;
          AFirst := AFirst.Next;
          Dispose(AItem);
          Dec(ABuffered);
        end;
        if ABuffered = FMaxItems then
          ATemp.Clear;
        // 计算要删除的数量
        ADelta := FItems.Count + ABuffered - FMaxItems;
        if ADelta < 0 then
          ADelta := 0;
        if ADelta >= 0 then // 复制要保留的项目
        begin
          if ATemp <> FItems then
          begin
            for I := ADelta to FItems.Count - 1 do
              ATemp.Add(FItems[I]);
          end
          else // 目标就是 TStringsList 类型，则直接移动数据
          begin
            for I := ADelta to FItems.Count - 1 do
              ATemp.Exchange(I, I-ADelta);
            while ADelta > 0 do
              begin
              FItems.Delete(FItems.Count - 1);
              Dec(ADelta);
              end;
          end;
        end;
      end;
      while Assigned(AFirst) do
      begin
        ATemp.Add(ItemToText(AFirst));
        AItem := AFirst.Next;
        Dispose(AFirst);
        AFirst := AItem;
      end;
    finally
      if ATemp <> FItems then
      begin
        if FMaxItems > 0 then
          // 一次性赋值
          FItems.Text := ATemp.Text
        else
          FItems.AddStrings(ATemp);
        FreeAndNil(ATemp);
      end
      else
        ATemp.EndUpdate;
    end;
    AtomicDecrement(FFlushRefCount);
    // {$IFDEF DEBUG}
    // DebugOut('Flush log used time %d ms',
    // [{$IF RTLVersion>=23}TThread.{$IFEND}GetTickCount - T]);
    // {$ENDIF}
  end
  // {$IFDEF DEBUG}
  // else
  // DebugOut('No log need flush');
  // {$ENDIF}
end;

procedure TQLogStringsWriter.HandleNeeded;
begin
  // 不需要
end;

procedure TQLogStringsWriter.LazyWrite;
var
  T: Cardinal;
  procedure DoFlush;
  begin
    FLastFlush := T;
    if AtomicIncrement(FFlushRefCount) = 1 then
      TThread.Queue(nil, FlushLogs)
    else
      AtomicDecrement(FFlushRefCount);
  end;

begin
  T := {$IF RTLVersion>=23}TThread.{$IFEND} GetTickCount;
  if FBuffered > 0 then
  begin
    if LazyMode then
    begin
      if T - FLastFlush >= Logs.Castor.LazyInterval then // 大于日志刷新间隔
        DoFlush;
    end
    else
      DoFlush;
  end;
end;

function TQLogStringsWriter.WriteItem(AItem: PQLogItem): Boolean;
var
  ACopy, AFirst: PQLogItem;
begin
  ACopy := CopyItem(AItem);
  AFirst := nil;
  FLocker.Enter;
  try
    if (MaxItems > 0) and (FBuffered = MaxItems) then
    begin
      AFirst := FLazyList.First;
      FLazyList.First := FLazyList.First.Next;
    end
    else
      Inc(FBuffered);
    if not Assigned(FLazyList.Last) then
      FLazyList.First := ACopy;
    ACopy.Prior := FLazyList.Last;
    if Assigned(FLazyList.Last) then
      FLazyList.Last.Next := ACopy;
    FLazyList.Last := ACopy;
  finally
    FLocker.Leave;
    if Assigned(AFirst) then
      FreeItem(AFirst);
  end;
  Result := true;
end;

{ TQLogItem }

function TQLogItem.GetMessageText: String;
begin
  SetLength(Result, MsgLen shr 1);
  if MsgLen > 0 then
    Move(Text[0], PQCharW(Result)^, MsgLen);
end;

function TQLogItem.GetTag: String;
begin
  SetLength(Result, TagLen shr 1);
  if TagLen > 0 then
    Move(Text[MsgLen shr 1], PQCharW(Result)^, TagLen);
end;

initialization

PerfTagStartFormat := SPerfTagStartFormat;
PerfTagStopFormat := SPerfTagStopFormat;
{$IFDEF QLOG_CREATE_GLOBAL}
Logs := TQLog.Create;
{$ELSE}
Logs := nil;
{$ENDIF}
{$IF RTLVersion<26}
zlibhandle := LoadLibrary('zlib1.dll');
if zlibhandle <> 0 then
begin
  gzopen := GetProcAddress(zlibhandle, 'gzopen');
  // gzseek := GetProcAddress(zlibhandle, 'gzseek');
  // gztell := GetProcAddress(zlibhandle, 'gztell');
  gzwrite := GetProcAddress(zlibhandle, 'gzwrite');
  gzclose := GetProcAddress(zlibhandle, 'gzclose');
end
else
begin
  gzopen := nil;
  // gzseek := nil;
  // gztell := nil;
  gzwrite := nil;
  gzclose := nil;
end;
{$IFEND <XE5}

finalization

{$IFDEF QLOG_CREATE_GLOBAL}
  FreeObject(Logs);
Logs := nil;
{$ENDIF}
{$IF RTLVersion<26}
if zlibhandle <> 0 then
  FreeLibrary(zlibhandle);
{$IFEND <XE5}

end.
