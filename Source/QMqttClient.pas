/// <summary>
/// <para>
/// MQTT 消息队列客户端实现： <br />使用方法： <br />1、创建一个 TQMQTTMessageClient
/// 实例，也可以直接用全局的 DefaultMQTTClient 实例。 <br />2、调用 RegisterDispatch
/// 关联主题与处理函数之间的关系。 <br />3、调用 Subscribe
/// 函数添加订阅，注意如果网络没有连接，则不会实际注册，在网络连接后会自动注册。 <br />4、调用 Start 启动客户端。 <br />
/// 5、要发布主题，调用 Publish 方法发布 <br />6、停止连接可以调用 Stop 方法。
/// </para>
/// <para>
/// 还是老规矩，开源免费，但需要申明版权，并不承担由于使用本代码引起的任何后果。由于使用匿名函数，所以不兼容2007
/// </para>
/// </summary>
unit QMqttClient;

interface

{ 要自动记录日志，请启用此选项，本单元发送的日志都带有QMQTT标记以便在写入时可以独立写入到一个特定的日志文件中 }
{ .$DEFINE LogMqttContent }

{$I qdac.inc}
{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}

uses classes, sysutils, qstring, qworker,
  netencoding, generics.collections, syncobjs, variants,
  RegularExpressionsCore{$IFDEF POSIX}
    , Posix.Base, Posix.Stdio, Posix.Pthread, Posix.UniStd, IOUtils,
  Posix.NetDB, Posix.SysSocket, Posix.Fcntl, Posix.StrOpts, Posix.Errno,
  Posix.NetinetIn, Posix.arpainet, Posix.SysSelect, Posix.Systime
{$ELSE}
    , windows, messages, winsock, TlHelp32
{$ENDIF}{$IFDEF LogMqttContent},
  qlog{$ENDIF}, qdac_ssl;

const
  /// <summary>
  /// 连接失败
  /// </summary>
  MQERR_CONNECT = 0;
  /// <summary>
  /// 协议版本错误
  /// </summary>
  MQERR_PROTOCOL_VERSION = 1;
  /// <summary>
  /// 客户端ID被拒绝
  /// </summary>
  MQERR_CLIENT_ID_REJECT = 2;
  /// <summary>
  /// 服务器不可用
  /// </summary>
  MQERR_SERVER_UNUSED = 3;
  /// <summary>
  /// 认证失败
  /// </summary>
  MQERR_BAD_AUTH = 4;
  /// <summary>
  /// 无权访问
  /// </summary>
  MQERR_NO_AUTH = 5;
  /// <summary>
  /// 订阅失败
  /// </summary>
  MQERR_SUBSCRIBE_FAILED = 20;
  /// <summary>
  /// 发布失败
  /// </summary>
  MQERR_PUBLISH_FAILED = 21;

  // 属性ID定义
  MP_PAYLOAD_FORMAT = 1;
  MP_MESSAGE_EXPIRE = 2;
  MP_CONTENT_TYPE = 3;
  MP_RESPONSE_TOPIC = 8;
  MP_DATA = 9;
  MP_ID = 11;
  MP_SESSION_EXPIRE = 17;
  MP_CLIENT_ID = 18;
  MP_KEEP_ALIVE = 19;
  MP_AUTH_METHOD = 21;
  MP_AUTH_DATA = 22;
  MP_NEED_PROBLEM_INFO = 23;
  MP_WILL_DELAY = 24;
  MP_NEED_RESPONSE_INFO = 24;
  MP_RESPONSE_INFO = 26;
  MP_REASON = 31;
  MP_RECV_MAX = 33;
  MP_ALIAS_MAX = 34;
  MP_TOPIC_ALIAS = 35;
  MP_MAX_QOS = 36;
  MP_HAS_RETAIN = 37;
  MP_USER_PROP = 38;
  MP_MAX_PACKAGE_SIZE = 39;
  MP_HAS_WILDCARD_SUBSCRIBES = 40;
  MP_HAS_SUBSCRIBE_ID = 41;
  MP_HAS_SHARE_SUBSCRIBES = 42;

type

  /// <summary>
  /// 控制类型，客户端只使用部分，具体参考 MQTT 协议标准
  /// </summary>
  TQMQTTControlType = (
    /// <summary>
    /// 保留未用
    /// </summary>
    ctReserved,
    /// <summary>
    /// 客户端-&gt;服务器：发送连接到服务器命令
    /// </summary>
    ctConnect,
    /// <summary>
    /// 服务器-&gt;客户端：连接结果确认
    /// </summary>
    ctConnectAck,
    /// <summary>
    /// 发送者-&gt;接收者：发布一个新消息
    /// </summary>
    ctPublish,
    /// <summary>
    /// 接收者&lt;-&gt;发送者：发送收到新消息确认
    /// </summary>
    ctPublishAck,
    /// <summary>
    /// 接收者-&gt;发布者：收到发布
    /// </summary>
    ctPublishRecv,
    /// <summary>
    /// 发送者-&gt;接收者：发布释放
    /// </summary>
    ctPublishRelease,
    /// <summary>
    /// 接收者-&gt;发送者：发布完成
    /// </summary>
    ctPublishDone,
    /// <summary>
    /// 客户端-&gt;服务器：订阅一组主题，注意 QoS 级别可能会降级为服务器支持的级别
    /// </summary>
    ctSubscribe,
    /// <summary>
    /// 服务器-&gt;客户端：订阅结果通知
    /// </summary>
    ctSubscribeAck,
    /// <summary>
    /// 客户端-&gt;服务器：取消订阅
    /// </summary>
    ctUnsubscribe,
    /// <summary>
    /// 服务器-&gt;客户端：取消订阅结果确认
    /// </summary>
    ctUnsubscribeAck,
    /// <summary>
    /// 客户端-&gt;服务器：发送保活指令
    /// </summary>
    ctPing,
    /// <summary>
    /// 服务器-&gt;客户端：保活指令回复
    /// </summary>
    ctPingResp,
    /// <summary>
    /// 客户端-&gt;服务器：断开连接
    /// </summary>
    ctDisconnect);
  /// <summary>
  /// MQTT 协议的标志位
  /// </summary>
  TQMQTTFlag = (
    /// <summary>
    /// Retain
    /// 标志位用于在发布消息时，确认消息是否保留。保留的消息会被保存并被新的同类消息替换，一旦有新订阅者接入，这类消息将发送给新的订阅者。具体参考
    /// MQTT 协议3.3.1。
    /// </summary>
    mfRetain,
    /// <summary>
    /// 重发标志
    /// </summary>
    mfDup);
  /// <summary>
  /// MQTT 服务质量等级
  /// </summary>
  TQMQTTQoSLevel = (
    /// <summary>
    /// 最多完成分发一次，接收端如果在接收过程中出错，也不再重发
    /// </summary>
    qlMax1,
    /// <summary>
    /// 最少完成分发一次，对于大部分需要保证接收到的消息来说，这个级别足够
    /// </summary>
    qlAtLeast1,
    /// <summary>
    /// 保证完成且只进行一次分发
    /// </summary>
    qlOnly1);
  TQMQTTFlags = set of TQMQTTFlag;

  PQMQTTMessage = ^TQMQTTMessage;

  /// <summary>
  /// 内部消息流转状态
  /// </summary>
  TQMQTMessageState = (msSending, msSent, msRecving, msRecved, msDispatching,
    msDispatched, msNeedAck, msNeedWait, msWaiting);
  TQMQTMessageStates = set of TQMQTMessageState;
  TQMQTTMessageClient = class;

  // 主题派发全部放到主线程，上层不需要考虑线程安全
  /// <summary>
  /// 订阅结果通知条目定义
  /// </summary>
  TQMQTTSubscribeResult = record
    /// <summary>
    /// 主题名称
    /// </summary>
    Topic: String;
    /// <summary>
    /// 错误代码
    /// </summary>
    ErrorCode: Byte;
    /// <summary>
    /// 服务器返回的最终实际的 QoS 级别
    /// </summary>
    Qos: TQMQTTQoSLevel;
    function GetSuccess: Boolean;
    property Success: Boolean read GetSuccess;
  end;

  TQMQTTSubscribeResults = array of TQMQTTSubscribeResult;
  PQMQTTSubscribeResults = ^TQMQTTSubscribeResults;

  /// <summary>
  /// 订阅结果通知事件
  /// </summary>
  TQMQTTTopicSubscribeResultNotify = procedure(ASender: TQMQTTMessageClient;
    const AResults: TQMQTTSubscribeResults) of object;
  /// <summary>
  /// 取消订阅结果通知事件
  /// </summary>
  TQMQTTTopicUnsubscribeEvent = procedure(ASender: TQMQTTMessageClient;
    const ATopic: String) of object;
  /// <summary>
  /// 消息派发事件，ATopic 指明了被派发的消息主题，当然您也可以从 AReq 参数中取其 TopicName 属性的值。这里 ATopic
  /// 是从 AReq.TopicName 缓存的值。
  /// </summary>
  TQMQTTTopicDispatchEvent = procedure(ASender: TQMQTTMessageClient;
    const ATopic: String; const AReq: PQMQTTMessage) of object;
  TQMQTTTopicDispatchEventG = procedure(ASender: TQMQTTMessageClient;
    const ATopic: String; const AReq: PQMQTTMessage);
  TQMQTTTopicDispatchEventA = reference to procedure
    (ASender: TQMQTTMessageClient; const ATopic: String;
    const AReq: PQMQTTMessage);
  /// <summary>
  /// 系统出错时的触发事件
  /// </summary>
  TQMQTTErrorEvent = procedure(ASender: TQMQTTMessageClient;
    const AErrorCode: Integer; const AErrorMsg: String) of object;
  /// <summary>
  /// 常规通知事件
  /// </summary>
  TQMQTTNotifyEvent = procedure(ASender: TQMQTTMessageClient) of object;
  /// <summary>
  /// 消息通知事件
  /// </summary>
  TQMQTTMessageNotifyEvent = procedure(const AMessage: PQMQTTMessage) of object;

  /// <summary>
  /// MQTT 交互消息实现
  /// </summary>
  TQMQTTMessage = record
  private
    FData: TBytes;
    FCurrent: PByte;
    FVarHeader: PByte;
    FStates: TQMQTMessageStates;
    FClient: TQMQTTMessageClient;
    FAfterSent: TQMQTTMessageNotifyEvent;
    FRecvTime, FSentTime, FSentTimes, FSize: Cardinal;
    // 用于发送队列，内部使用
    FNext: PQMQTTMessage;
    FWaitEvent: TEvent;
    function GetControlType: TQMQTTControlType;
    procedure SetControlType(const AType: TQMQTTControlType);
    function GetHeaderFlags(const Index: Integer): Boolean;
    procedure SetHeaderFlags(const Index: Integer; const Value: Boolean);
    function GetPayloadSize: Cardinal;
    procedure SetPayloadSize(Value: Cardinal);
    procedure EncodeInt(var ABuf: PByte; V: Cardinal);
    procedure EncodeInt64(var ABuf: PByte; V: UInt64);
    function DecodeInt(var ABuf: PByte; AMaxCount: Integer;
      var AResult: Cardinal): Boolean;
    function DecodeInt64(var ABuf: PByte; AMaxCount: Integer;
      var AResult: Int64): Boolean;
    function GetBof: PByte;
    function GetEof: PByte;
    function GetPosition: Integer;
    procedure SetQosLevel(ALevel: TQMQTTQoSLevel);
    function GetQoSLevel: TQMQTTQoSLevel;
    procedure SetPosition(const Value: Integer);
    function GetByte(const AIndex: Integer): Byte;
    procedure SetByte(const AIndex: Integer; const Value: Byte);
    function GetVarHeaderOffset: Integer;
    function GetRemainSize: Integer;
    function GetTopicText: String;
    function GetTopicId: Word;
    function GetTopicContent: TBytes;
    function GetCapacity: Cardinal;
    procedure SetCapacity(const Value: Cardinal);
    function GetTopicName: String;
    function ReloadSize(AKnownBytes: Integer): Boolean;
    function GetTopicContentSize: Integer;
    function GetTopicOriginContent: PByte;
    function GetPackageId: Word;
    property Next: PQMQTTMessage read FNext write FNext;
  public
    class function IntEncodedSize(const V: Cardinal): Byte; static;
    /// <summary>
    /// 复制一个新的消息出来，需要自行调用 Dispose 释放
    /// </summary>
    function Copy: PQMQTTMessage;
    /// <summary>
    /// 创建并初始化一个新的消息实例
    /// </summary>
    class function Create(AClient: TQMQTTMessageClient): PQMQTTMessage; static;
    /// <summary>
    /// 向当前位置写入一个字符串
    /// </summary>
    function Cat(const S: QStringW; AWriteZeroLen: Boolean = false)
      : PQMQTTMessage; overload;
    /// <summary>
    /// 向当前位置写入一个字符串
    /// </summary>
    function Cat(const S: QStringA; AWriteZeroLen: Boolean = false)
      : PQMQTTMessage; overload;
    /// <summary>
    /// 向当前位置写入指定的数据
    /// </summary>
    function Cat(const ABuf: Pointer; const ALen: Cardinal)
      : PQMQTTMessage; overload;
    /// <summary>
    /// 向当前位置写入指定的数据
    /// </summary>
    function Cat(const ABytes: TBytes): PQMQTTMessage; overload;
    /// <summary>
    /// 向当前位置写入一个8位无符号整数
    /// </summary>
    function Cat(const V: Byte): PQMQTTMessage; overload;
    /// <summary>
    /// 向当前位置写入一个16位无符号整数
    /// </summary>
    /// <param name="V">
    /// 要写入的数值
    /// </param>
    /// <param name="AEncode">
    /// 是否对其进行编码（具体编码规则参考 EncodeInt 和 EncodeInt64 的实现）
    /// </param>
    function Cat(const V: Word; AEncode: Boolean = false)
      : PQMQTTMessage; overload;
    /// <summary>
    /// 向当前位置写入一个32位无符号整数
    /// </summary>
    /// <param name="V">
    /// 要写入的数值
    /// </param>
    /// <param name="AEncode">
    /// 是否对其进行编码（具体编码规则参考 EncodeInt 和 EncodeInt64 的实现）
    /// </param>
    function Cat(const V: Cardinal; AEncode: Boolean = false)
      : PQMQTTMessage; overload;

    /// <summary>
    /// 向当前位置写入一个64位无符号整数
    /// </summary>
    /// <param name="V">
    /// 要写入的数值
    /// </param>
    /// <param name="AEncode">
    /// 是否对其进行编码（具体编码规则参考 EncodeInt 和 EncodeInt64 的实现）
    /// </param>
    function Cat(const V: UInt64; AEncode: Boolean = false)
      : PQMQTTMessage; overload;
    /// <summary>
    /// 向当前位置写入一个8位整数
    /// </summary>
    function Cat(const V: Shortint): PQMQTTMessage; overload;
    /// <summary>
    /// 向当前位置写入一个16位整数
    /// </summary>
    /// <param name="V">
    /// 要写入的数值
    /// </param>
    /// <param name="AEncode">
    /// 是否对其进行编码（具体编码规则参考 EncodeInt 和 EncodeInt64 的实现）
    /// </param>
    function Cat(const V: Smallint; AEncode: Boolean = false)
      : PQMQTTMessage; overload;
    /// <summary>
    /// 向当前位置写入一个32位整数
    /// </summary>
    /// <param name="V">
    /// 要写入的数值
    /// </param>
    /// <param name="AEncode">
    /// 是否对其进行编码（具体编码规则参考 EncodeInt 和 EncodeInt64 的实现）
    /// </param>
    function Cat(const V: Integer; AEncode: Boolean = false)
      : PQMQTTMessage; overload;
    /// <summary>
    /// 向当前位置写入一个64位整数
    /// </summary>
    /// <param name="V">
    /// 要写入的数值
    /// </param>
    /// <param name="AEncode">
    /// 是否对其进行编码（具体编码规则参考 EncodeInt 和 EncodeInt64 的实现）
    /// </param>
    function Cat(const V: Int64; AEncode: Boolean = false)
      : PQMQTTMessage; overload;
    /// <summary>
    /// 向当前位置写入一个32位浮点数
    /// </summary>
    /// <param name="V">
    /// 要写入的数值
    /// </param>
    /// <param name="AEncode">
    /// 是否对其进行编码（具体编码规则参考 EncodeInt 和 EncodeInt64 的实现）
    /// </param>
    function Cat(const V: Single; AEncode: Boolean = false)
      : PQMQTTMessage; overload;
    /// <summary>
    /// 向当前位置写入一个64位浮点数
    /// </summary>
    /// <param name="V">
    /// 要写入的数值
    /// </param>
    /// <param name="AEncode">
    /// 是否对其进行编码（具体编码规则参考 EncodeInt 和 EncodeInt64 的实现）
    /// </param>
    function Cat(const V: Double; AEncode: Boolean = false)
      : PQMQTTMessage; overload;

    /// <summary>
    /// 从当前位置读取下一个字节的值（8位无符号整数）
    /// </summary>
    function NextByte: Byte;
    /// <summary>
    /// 从当前位置读取下一个16位无符号整数
    /// </summary>
    /// <param name="AIsEncoded">
    /// 是否是编码过的数值
    /// </param>
    function NextWord(AIsEncoded: Boolean): Word;

    /// <summary>
    /// 读取下一个32位无符号整数
    /// </summary>
    /// <param name="AIsEncoded">
    /// 从当前位置是否是编码过的数值
    /// </param>
    function NextDWord(AIsEncoded: Boolean = false): Cardinal;
    /// <summary>
    /// 读取下一个64位无符号整数
    /// </summary>
    /// <param name="AIsEncoded">
    /// 是否是编码过的数值
    /// </param>
    function NextUInt64(AIsEncoded: Boolean = false): UInt64;
    /// <summary>
    /// 从当前位置读取下一个8位整数
    /// </summary>
    function NextTinyInt: Shortint;
    /// <summary>
    /// 从当前位置读取下一个16位整数
    /// </summary>
    /// <param name="AIsEncoded">
    /// 是否是编码过的数值
    /// </param>
    function NextSmallInt(AIsEncoded: Boolean = false): Smallint;
    /// <summary>
    /// 从当前位置读取下一个32位整数
    /// </summary>
    /// <param name="AIsEncoded">
    /// 是否是编码过的数值
    /// </param>
    function NextInt(AIsEncoded: Boolean = false): Integer;

    /// <summary>
    /// 从当前位置读取下一个64位整数
    /// </summary>
    /// <param name="AIsEncoded">
    /// 是否是编码过的数值
    /// </param>
    function NextInt64(AIsEncoded: Boolean = false): Int64;
    /// <summary>
    /// 从当前位置读取下一个32位浮点数
    /// </summary>
    /// <param name="AIsEncoded">
    /// 是否是编码过的数值
    /// </param>
    function NextSingle(AIsEncoded: Boolean = false): Single;
    /// <summary>
    /// 从当前位置读取下一个64位浮点数
    /// </summary>
    /// <param name="AIsEncoded">
    /// 是否是编码过的数值
    /// </param>
    function NextFloat(AIsEncoded: Boolean = false): Double;
    /// <summary>
    /// 从当前位置读取指定长度的内容
    /// </summary>
    function NextBytes(ASize: Integer): TBytes; overload;
    /// <summary>
    /// 从当前位置读取指定长度的内容
    /// </summary>
    function NextBytes(ABuf: Pointer; ALen: Integer): Integer; overload;
    /// <summary>
    /// 从当前位置读取一个字符串
    /// </summary>
    function NextString: String;

    /// <summary>
    /// 向前或向后移动指定的位置。如果调整后的位置小于Bof，则设置为Bof，如果大于Eof，则设置为Eof的值。
    /// </summary>
    function MoveBy(const ADelta: Integer): PQMQTTMessage; inline;
    procedure MoveToVarHeader;
    /// <summary>
    /// 控制命令类型
    /// </summary>
    property ControlType: TQMQTTControlType read GetControlType
      write SetControlType;
    /// <summary>
    /// 以十六进制视图方式来显示整个消息内容（用于记录日志分析）
    /// </summary>
    function ContentAsHexText: String;
    /// <summary>
    /// 保留标志
    /// </summary>
    property IsRetain: Boolean index 0 read GetHeaderFlags write SetHeaderFlags;
    /// <summary>
    /// 重发标志
    /// </summary>
    property IsDup: Boolean index 1 read GetHeaderFlags write SetHeaderFlags;
    /// <summary>
    /// 服务质量级别
    /// </summary>
    property QosLevel: TQMQTTQoSLevel read GetQoSLevel write SetQosLevel;
    /// <summary>
    /// 载荷大小
    /// </summary>
    property PayloadSize: Cardinal read GetPayloadSize write SetPayloadSize;
    /// <summary>
    /// 内容开始地址
    /// </summary>
    property Bof: PByte read GetBof;
    /// <summary>
    /// 内容 结束地址
    /// </summary>
    property Eof: PByte read GetEof;
    /// <summary>
    /// 内容当前地址
    /// </summary>
    property Current: PByte read FCurrent;
    /// <summary>
    /// 可变头部的起始位置，是固定头+剩余长度后的起始位置，从这个地址开始是根据类型不同填充的
    /// </summary>
    property VarHeader: PByte read FVarHeader;
    /// <summary>
    /// 可变头部的偏移量
    /// </summary>
    property VarHeaderOffset: Integer read GetVarHeaderOffset;
    /// <summary>
    /// 当前位置
    /// </summary>
    property Position: Integer read GetPosition write SetPosition;
    /// <summary>
    /// 整个消息内容长度（注意它小于等于 Capacity 属性的值）
    /// </summary>
    property Size: Cardinal read FSize;
    /// <summary>
    /// 剩余大小，等于 Size-Position
    /// </summary>
    property RemainSize: Integer read GetRemainSize;
    /// <summary>
    /// 内容中特定字节的值
    /// </summary>
    property Bytes[const AIndex: Integer]: Byte read GetByte
      write SetByte; default;
    /// <summary>
    /// 当前状态
    /// </summary>
    property States: TQMQTMessageStates read FStates write FStates;
    /// <summary>
    /// 消息关联的 TQMQTTMessageClient 实例
    /// </summary>
    property Client: TQMQTTMessageClient read FClient write FClient;
    /// <summary>
    /// 接收或发送的消息的PackageId，仅对部分类型的消息有效，无效的类型返回 0
    /// </summary>
    property TopicId: Word read GetTopicId;
    /// <summary>
    /// 消息主题，仅在类型为 ctPublish 时有意义
    /// </summary>
    property TopicName: String read GetTopicName;
    /// <summary>
    /// 消息体文本内容，注意请自行确认消息是文本格式的内容，否则可能出现异常（无法将内容当成 UTF-8 编码的字符串来处理）
    /// </summary>
    property TopicText: String read GetTopicText;
    /// <summary>
    /// 消息体的二进制内容
    /// </summary>
    property TopicContent: TBytes read GetTopicContent;
    /// <summary>
    /// 接收到的消息内容总大小（注意和 Size 的区别），它不包含消息的其它部分所占用的空间
    /// </summary>
    property TopicContentSize: Integer read GetTopicContentSize;
    /// <summary>
    /// 消息体原始内容地址
    /// </summary>
    property TopicOriginContent: PByte read GetTopicOriginContent;
    /// <summary>
    /// 容量大小
    /// </summary>
    ///
    property Capacity: Cardinal read GetCapacity write SetCapacity;
    property RecvTime: Cardinal read FRecvTime;
    property SentTime: Cardinal read FSentTime;
    property SentTimes: Cardinal read FSentTimes;
    property PackageId: Word read GetPackageId;
  end;

  /// <summary>
  /// 主题匹配模式
  /// </summary>
  TTopicMatchType = (
    /// <summary>
    /// 完整匹配（注意消息主题区分大小写）
    /// </summary>
    mtFull,
    /// <summary>
    /// 模式匹配，支持通配符（$,#+）
    /// </summary>
    mtPattern,
    /// <summary>
    /// 正则表达式匹配
    /// </summary>
    mtRegex);

  /// <summary>
  /// 消息订阅请求内部记录使用，用于记录用户申请订阅的消息主题
  /// </summary>
  TQMQTTSubscribeItem = record
    Topic: String;
    Qos: TQMQTTQoSLevel;
  end;

  TQMQTTProtocolVersion = (pv3_1_1 = 4, pv5_0 = 5);
  TQMQTT5AuthMode = (amNone);

  TQMQTTPropId = (piFormat = 1, piMsgTimeout = 2, piContentType = 3,
    piRespTopic = 8, piRelData = 9, piIdentDef = 11, piSessionTimeout = 17,
    piClientId = 18, piKeepAlive = 19, piAuthMode = 21, piAuthData = 22,
    piErrorData = 23, piWillDelay = 24, piRequestResp = 25, piRequst = 26,
    piServerRef = 28, piReason = 31, piMaxRecv = 33, piMaxTopicLen = 34,
    piTopicAlias = 35, piMaxQoS = 36, piUserProp = 38, piMaxPackageSize = 39,
    piAcceptPatten = 40, piAcceptTopicId = 41, piAcceptShareTopic = 42);
  TQMQTT5PropDataType = (ptUnknown, ptByte, ptWord, ptInt, ptVarInt, ptString,
    ptBinary);

  TQMQTT5PropType = record
  private
    FName: String;
    FId: Byte;
    FDataType: TQMQTT5PropDataType;
    function GetDataSize: Integer;
  public
    property Id: Byte read FId;
    property DataType: TQMQTT5PropDataType read FDataType;
    property DataSize: Integer read GetDataSize;
    property Name: String read FName;
  end;

  PQMQTT5PropType = ^TQMQTT5PropType;

  TQMQTT5Prop = record
    case Integer of
      0:
        (AsByte: Byte);
      1:
        (AsWord: Word);
      2:
        (AsInteger: Cardinal);
      3:
        (AsString: PQStringA; IsSet: Boolean);
      4:
        (AsBytes: ^TBytes);
  end;

  EMQTTError = class(Exception)

  end;

  EMQTTAbortError = class(EAbort)

  end;

  PQMQTT5Prop = ^TQMQTT5Prop;

  TQMQTT5Props = class
  private
    function GetPropTypes(const APropId: Byte): PQMQTT5PropType;
    function GetAsInt(const APropId: Byte): Cardinal;
    function GetAsString(const APropId: Byte): String;
    function GetAsVariant(const APropId: Byte): Variant;
    procedure SetAsInt(const APropId: Byte; const Value: Cardinal);
    procedure SetAsString(const APropId: Byte; const Value: String);
    procedure SetAsVariant(const APropId: Byte; const Value: Variant);
    function GetIsSet(const APropId: Byte): Boolean;
    function GetDataSize(const APropId: Byte): Integer;
    function GetPayloadSize: Integer;
    function GetMinMaxId(const Index: Integer): Byte;
  protected
    FItems: TArray<TQMQTT5Prop>;
  public
    constructor Create;
    destructor Destroy; override;
    function Copy: TQMQTT5Props;
    procedure ReadProps(const AMessage: PQMQTTMessage);
    procedure WriteProps(AMessage: PQMQTTMessage);
    procedure Replace(AProps: TQMQTT5Props);
    property PropTypes[const APropId: Byte]: PQMQTT5PropType read GetPropTypes;
    property Values[const APropId: Byte]: Variant read GetAsVariant
      write SetAsVariant;
    property AsInt[const APropId: Byte]: Cardinal read GetAsInt write SetAsInt;
    property AsString[const APropId: Byte]: String read GetAsString
      write SetAsString;
    property IsSet[const APropId: Byte]: Boolean read GetIsSet;
    property DataSize[const APropId: Byte]: Integer read GetDataSize;
    property PayloadSize: Integer read GetPayloadSize;
    property MinId: Byte index 0 read GetMinMaxId;
    property MaxId: Byte index 1 read GetMinMaxId;
  end;

  TQMQTTClientState = (qcsConnecting, qcsReconnecting, qcsRunning, qcsStopping);
  TQMQTTClientStates = set of TQMQTTClientState;

  PQMQTTPublishPendingItem = ^TQMQTTPublishPendingItem;

  TQMQTTPublishPendingItem = record
    Topic: String;
    Content: TBytes;
    Qos: TQMQTTQoSLevel;
    Props: TQMQTT5Props;
    PushTime: Cardinal;
    Prior, Next: PQMQTTPublishPendingItem; // forms.pas
  end;

  TQMQTTPublishPendings = record
    First, Last: PQMQTTPublishPendingItem;
  end;

  TQMQTTPublishOption = (poCacheUnpublish, poCleanOnDisconnect);
  TQMQTTPublishOptions = set of TQMQTTPublishOption;

  /// <summary>
  /// QMQTT 消息客户端实现，注意目前版本并没有实现消息的重发（主要是作者懒）
  /// </summary>
  TQMQTTMessageClient = class(TComponent)
  private

  protected
    // 事件列表
    FBeforeConnect: TQMQTTNotifyEvent;
    FAfterDispatch: TQMQTTTopicDispatchEvent;
    FAfterConnected: TQMQTTNotifyEvent;
    FBeforeDispatch: TQMQTTTopicDispatchEvent;
    FBeforePublish: TQMQTTTopicDispatchEvent;
    FAfterSubscribed: TQMQTTTopicSubscribeResultNotify;
    FAfterUnsubscribed: TQMQTTTopicUnsubscribeEvent;
    FAfterPublished: TQMQTTTopicDispatchEvent;
    FBeforeSubscribe: TQMQTTTopicDispatchEvent;
    FBeforeUnsubscribe: TQMQTTTopicDispatchEvent;
    FBeforeSend: TQMQTTMessageNotifyEvent;
    FAfterSent: TQMQTTMessageNotifyEvent;
    FOnRecvTopic: TQMQTTTopicDispatchEvent;
    FAfterDisconnected: TQMQTTNotifyEvent;
    //
    FSSL: IQSSLItem;
    FConnectionTimeout, FReconnectInterval, FMaxTopicAckTimeout: Cardinal;
    FWaitAckTopics: Integer;
    FServerHost, FUserName, FPassword, FClientId, FWillTopic: String;
    FWillMessage: TBytes;
    FRecvThread, FSendThread: TThread;
    FThreadCount: Integer;
    FSocket: THandle;
    FOnError: TQMQTTErrorEvent;
    FPackageId: Integer;
    FWaitAcks: TArray<PQMQTTMessage>;
    FPublishPendings: TQMQTTPublishPendings;
    FSubscribes: TStringList;
    FTopicHandlers: TStringList;
    FNotifyEvent: TEvent;
    FConnectJob: IntPtr;
    FLastIoTick, FPublishTTL: Cardinal;
    FPingStarted, FPingTime: Cardinal;
    FReconnectTimes, FReconnectTime: Cardinal;
    FLastConnectTime: Cardinal;
    FSentTopics: Cardinal;
    FRecvTopics: Cardinal;
    FConnectProps: TQMQTT5Props;
    FStates: TQMQTTClientStates;
    FPublishOptions: TQMQTTPublishOptions;
    FLocker: TCriticalSection;
    FPeekInterval: Word;
    FServerPort: Word;

    FProtocolVersion: TQMQTTProtocolVersion;
    FQoSLevel: TQMQTTQoSLevel;
    FIsRetain: Boolean;
    FUseSSL: Boolean;
    FCleanLastSession: Boolean;
    FConnected: Boolean;
    FEventInThread: Boolean;
{$IFDEF LogMqttContent}
    FLogPackageContent: Boolean;
{$ENDIF}
    procedure RecreateSocket;
    procedure DoConnect;
    procedure DoMQTTConnect(AJob: PQJob);
    procedure DoDispatch(var AReq: TQMQTTMessage);
    procedure DispatchTopic(AMsg: PQMQTTMessage);
    procedure DispatchProps(AMsg: PQMQTTMessage);
    procedure InvokeTopicHandlers(const ATopic: String; AMsg: PQMQTTMessage);
    procedure DoError(AErrorCode: Integer);
    procedure DoBeforeConnect;
    procedure DoAfterConnected;
    procedure DoConnectFailed;
    procedure DoBeforePublish(ATopic: String; AMsg: PQMQTTMessage);
    procedure DoAfterSubcribed(AResult: PQMQTTSubscribeResults);
    procedure DoAfterUnsubscribed(ASource: PQMQTTMessage);
    procedure ValidClientId;
    procedure SetWillMessage(const AValue: TBytes);
    procedure DoBeforeSend(const AReq: PQMQTTMessage);
    procedure DoAfterSent(const AReq: PQMQTTMessage);
    function DoSend(AReq: PQMQTTMessage): Boolean;
    procedure DoRecv;
    function GetClientId: String;
    procedure Lock; inline;
    procedure Unlock; inline;
    procedure ClearWaitAcks;
    procedure ClearHandlers;
    function PopWaitAck(APackageId: Word): PQMQTTMessage;
    function DNSLookupV4(const AHost: QStringW): Cardinal; overload;
    procedure DoFreeAfterSent(const AMessage: PQMQTTMessage);
    procedure DoTopicPublished(const AMessage: PQMQTTMessage);
    procedure Disconnect;
    procedure DoAfterDisconnected;
    procedure DoPing;
    procedure FreeMessage(AMsg: PQMQTTMessage);
    function AcquirePackageId(AReq: PQMQTTMessage; AIsWaitAck: Boolean): Word;
    procedure Queue(ACallback: TThreadProcedure; AIsForce: Boolean = false);
    procedure DoQueueCallback(AJob: PQJob);
    procedure DoCloseSocket;
    function GetIsRunning: Boolean;
    function GetConnectProps: TQMQTT5Props;
    function GetSSLManager: TQSSLManager;
    procedure DoTimer(AJob: PQJob);
    procedure ReconnectNeeded;
    procedure DoCleanup;
    procedure CheckPublished;
    procedure CleanPublishPendings;
    procedure SubscribePendings;
    procedure RemoveSubscribePendings(const ATopics: array of String);
    function CompareTopic(const ATopic1, ATopic2: String): Integer;
    procedure PublishFirstPending;
  public
    /// <summary>
    /// 构造函数
    /// </summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>
    /// 析构前调用
    /// </summary>
    procedure BeforeDestruction; override;

    /// <summary>
    /// 析构函数
    /// </summary>
    destructor Destroy; override;
    /// <summary>
    /// 启动服务
    /// </summary>
    procedure Start;
    /// <summary>
    /// 停止服务
    /// </summary>
    procedure Stop;
    /// <summary>
    /// 订阅服务
    /// </summary>
    /// <param name="ATopics">
    /// 主题列表，支持模式匹配
    /// </param>
    /// <param name="AQoS">
    /// 服务质量要求
    /// </param>
    /// <param name="AProps">
    /// 额外属性设置（仅5.0版协议有效）
    /// </param>
    /// <remarks>
    /// 1、 服务质量要求并不合最终的服务质量要求一致，服务器不支持该级别时，可能降级。 <br />
    /// 2、如果尚未连接到服务器，则对应的请求会在服务连接完成后尝试自动订阅（先买票后上车和先上车后买票的区别）。
    /// </remarks>
    procedure Subscribe(const ATopics: array of String;
      const AQoS: TQMQTTQoSLevel; AProps: TQMQTT5Props = nil);
    /// <summary>
    /// 取消指定的主题订阅
    /// </summary>
    /// <param name="ATopics">
    /// 要取消的主题列表
    /// </param>
    /// <remarks>
    /// 这个请求只能在连接完成后才可以，暂时不支持离线取消
    /// </remarks>
    procedure Unsubscribe(const ATopics: array of String);
    /// <summary>
    /// 清除所有的主题订阅
    /// </summary>

    procedure ClearSubscribes;
    /// <summary>
    /// 发布一个消息
    /// </summary>
    /// <param name="ATopic">
    /// 消息主题，注意不能使用任何通配符
    /// </param>
    /// <param name="AContent">
    /// 消息内容
    /// </param>
    /// <param name="AQoSLevel">
    /// 服务质量要求
    /// </param>
    procedure Publish(const ATopic, AContent: String;
      AQoSLevel: TQMQTTQoSLevel); overload;
    /// <summary>
    /// 发布一个消息
    /// </summary>
    /// <param name="ATopic">
    /// 消息主题，注意不能使用任何通配符
    /// </param>
    /// <param name="AContent">
    /// 消息内容
    /// </param>
    /// <param name="AQoSLevel">
    /// 服务质量要求
    /// </param>
    procedure Publish(const ATopic: String; AContent: TBytes;
      AQoSLevel: TQMQTTQoSLevel); overload;
    /// <summary>
    /// 发布一个消息
    /// </summary>
    /// <param name="ATopic">
    /// 消息主题，注意不能使用任何通配符
    /// </param>
    /// <param name="AContent">
    /// 消息内容
    /// </param>
    /// <param name="AQoSLevel">
    /// 服务质量要求
    /// </param>
    procedure Publish(const ATopic: String; const AContent; ALen: Cardinal;
      AQoSLevel: TQMQTTQoSLevel); overload;

    /// <summary>
    /// 注册一个消息派发处理过程
    /// </summary>
    /// <param name="ATopic">
    /// 要关联的主题，格式与 AType 参数的类型要求保持一致
    /// </param>
    /// <param name="AHandler">
    /// 消息处理函数
    /// </param>
    /// <param name="AType">
    /// 消息主题类型
    /// </param>
    procedure RegisterDispatch(const ATopic: String;
      AHandler: TQMQTTTopicDispatchEvent;
      AType: TTopicMatchType = mtFull); overload;
    procedure RegisterDispatch(const ATopic: String;
      AHandler: TQMQTTTopicDispatchEventG;
      AType: TTopicMatchType = mtFull); overload;
    procedure RegisterDispatch(const ATopic: String;
      AHandler: TQMQTTTopicDispatchEventA;
      AType: TTopicMatchType = mtFull); overload;

    /// <summary>
    /// 移除一个消息派发函数注册
    /// </summary>
    procedure UnregisterDispatch(AHandler: TQMQTTTopicDispatchEvent); overload;
    property EventInThread: Boolean read FEventInThread write FEventInThread;
    /// <summary>
    /// 服务器IP或域名
    /// </summary>
    property ServerHost: String read FServerHost write FServerHost;
    /// <summary>
    /// 服务器端口号，默认1883
    /// </summary>
    property ServerPort: Word read FServerPort write FServerPort;
    /// <summary>
    /// 用户名
    /// </summary>
    property UserName: String read FUserName write FUserName;
    /// <summary>
    /// 密码
    /// </summary>
    property Password: String read FPassword write FPassword;
    /// <summary>
    /// 客户ID，如果不指定会随机生成。当然最好你自己保留好这个ID。
    /// </summary>
    property ClientId: String read GetClientId write FClientId;
    /// <summary>
    /// 连接超时,单位为秒
    /// </summary>
    property ConnectionTimeout: Cardinal read FConnectionTimeout
      write FConnectionTimeout;
    /// <summary>
    /// 默认服务质量要求
    /// </summary>
    property QosLevel: TQMQTTQoSLevel read FQoSLevel write FQoSLevel;
    /// <summary>
    /// 遗言的主题
    /// </summary>
    property WillTopic: String read FWillTopic write FWillTopic;
    /// <summary>
    /// 遗言的内容，如果如果是字符串，请使用 UTF 8 编码
    /// </summary>
    property WillMessage: TBytes read FWillMessage write SetWillMessage;
    /// <summary>
    /// 默认保留标志
    /// </summary>
    property IsRetain: Boolean read FIsRetain write FIsRetain;
    /// <summary>
    /// 是否连接时清除上次会话信息
    /// </summary>
    property CleanLastSession: Boolean read FCleanLastSession
      write FCleanLastSession;
    /// <summary>
    /// 保活间隔，单位为秒
    /// </summary>
    property PeekInterval: Word read FPeekInterval write FPeekInterval;
    /// <summary>
    /// 重连间隔，单位为秒
    /// </summary>
    property ReconnectInterval: Cardinal read FReconnectInterval
      write FReconnectInterval;
    property MaxTopicAckTimeout: Cardinal read FMaxTopicAckTimeout
      write FMaxTopicAckTimeout;
    /// <summary>
    /// 客户端是否已经成功连接到服务器
    /// </summary>
    property IsRunning: Boolean read GetIsRunning;
    property Connected: Boolean read FConnected;
    /// <summary>
    /// 接收到的消息数量
    /// </summary>
    property RecvTopics: Cardinal read FRecvTopics;
    /// <summary>
    /// 发送的消息数量
    /// </summary>
    property SentTopics: Cardinal read FSentTopics;
{$IFDEF LogMqttContent}
    property LogPackageContent: Boolean read FLogPackageContent
      write FLogPackageContent;
{$ENDIF}
    property UseSSL: Boolean read FUseSSL write FUseSSL;
    property SSLManager: TQSSLManager read GetSSLManager;

    // MQTT 5.0 Added
    property ProtocolVersion: TQMQTTProtocolVersion read FProtocolVersion
      write FProtocolVersion;
    property ConnectProps: TQMQTT5Props read GetConnectProps;
    property PublishOptions: TQMQTTPublishOptions read FPublishOptions
      write FPublishOptions;
    property PublishTTL: Cardinal read FPublishTTL write FPublishTTL;
    /// <summary>
    /// 出错通知
    /// </summary>
    property OnError: TQMQTTErrorEvent read FOnError write FOnError;
    /// <summary>
    /// 连接前通知
    /// </summary>
    property BeforeConnect: TQMQTTNotifyEvent read FBeforeConnect
      write FBeforeConnect;
    /// <summary>
    /// 连接后通知
    /// </summary>
    property AfterConnected: TQMQTTNotifyEvent read FAfterConnected
      write FAfterConnected;
    /// <summary>
    /// 断开后通知
    /// </summary>
    property AfterDisconnected: TQMQTTNotifyEvent read FAfterDisconnected
      write FAfterDisconnected;
    /// <summary>
    /// 派发前通知
    /// </summary>
    property BeforeDispatch: TQMQTTTopicDispatchEvent read FBeforeDispatch
      write FBeforeDispatch;
    /// <summary>
    /// 派发后通知
    /// </summary>
    property AfterDispatch: TQMQTTTopicDispatchEvent read FAfterDispatch
      write FAfterDispatch;
    /// <summary>
    /// 发布前通知
    /// </summary>
    property BeforePublish: TQMQTTTopicDispatchEvent read FBeforePublish
      write FBeforePublish;
    /// <summary>
    /// 发布后通知
    /// </summary>
    property AfterPublished: TQMQTTTopicDispatchEvent read FAfterPublished
      write FAfterPublished;
    /// <summary>
    /// 订阅前通知
    /// </summary>
    property BeforeSubscribe: TQMQTTTopicDispatchEvent read FBeforeSubscribe
      write FBeforeSubscribe;
    /// <summary>
    /// 订阅后通知
    /// </summary>
    property AfterSubscribed: TQMQTTTopicSubscribeResultNotify
      read FAfterSubscribed write FAfterSubscribed;
    /// <summary>
    /// 取消订阅前通知
    /// </summary>
    property BeforeUnsubscribe: TQMQTTTopicDispatchEvent read FBeforeUnsubscribe
      write FBeforeUnsubscribe;
    /// <summary>
    /// 取消订阅后通知
    /// </summary>
    property AfterUnsubscribed: TQMQTTTopicUnsubscribeEvent
      read FAfterUnsubscribed write FAfterUnsubscribed;
    /// <summary>
    /// 发送数据前通知
    /// </summary>
    property BeforeSend: TQMQTTMessageNotifyEvent read FBeforeSend
      write FBeforeSend;
    /// <summary>
    /// 发送数据后通知
    /// </summary>
    property AfterSent: TQMQTTMessageNotifyEvent read FAfterSent
      write FAfterSent;
    /// <summary>
    /// 收到消息时通知
    /// </summary>
    property OnRecvTopic: TQMQTTTopicDispatchEvent read FOnRecvTopic
      write FOnRecvTopic;
  end;

  /// <summary>
  /// 默认的全局 MQTT 客户端实例
  /// </summary>
function DefaultMqttClient: TQMQTTMessageClient;

implementation

resourcestring
  SServerHostUnknown = 'MQTT 服务器地址未设置';
  SServerPortInvalid = '无效的服务器端口号';
  STooLargePayload = '载荷大小 %d 超出包限制';
  SClientNotRunning = '客户端未连接到服务器，不能订阅';
  SInitSSLFailed = '初始化 SSL 失败，请检查目录下是否存在 libssl-1_1.dll 和 libcrypto-1_1.dll';

const
  MQTT5PropTypes: array [0 .. 41] of TQMQTT5PropType = ( //
    (FName: 'PayloadFormat'; FId: 1; FDataType: ptByte), //
    (FName: 'MessageExpire'; FId: 2; FDataType: ptInt), //
    (FName: 'ContentType'; FId: 3; FDataType: ptString), //
    (FName: ''; FId: 4; FDataType: ptUnknown), //
    (FName: ''; FId: 5; FDataType: ptUnknown), //
    (FName: ''; FId: 6; FDataType: ptUnknown), //
    (FName: ''; FId: 7; FDataType: ptUnknown), //
    (FName: 'ResponseTopic'; FId: 8; FDataType: ptString), //
    (FName: 'Data'; FId: 9; FDataType: ptBinary), //
    (FName: ''; FId: 10; FDataType: ptUnknown), //
    (FName: 'Identifier'; FId: 11; FDataType: ptInt), //
    (FName: ''; FId: 12; FDataType: ptUnknown), //
    (FName: ''; FId: 13; FDataType: ptUnknown), //
    (FName: ''; FId: 14; FDataType: ptUnknown), //
    (FName: ''; FId: 15; FDataType: ptUnknown), //
    (FName: ''; FId: 16; FDataType: ptUnknown), //
    (FName: 'SessionExpire'; FId: 17; FDataType: ptInt), //
    (FName: 'AssignClientId'; FId: 18; FDataType: ptString), //
    (FName: 'KeepAlive'; FId: 19; FDataType: ptWord), //
    (FName: ''; FId: 20; FDataType: ptUnknown), //
    (FName: 'AuthMethod'; FId: 21; FDataType: ptString), //
    (FName: 'AuthData'; FId: 22; FDataType: ptBinary), //
    (FName: 'NeedProblemInfo'; FId: 23; FDataType: ptByte), //
    (FName: 'WillDelay'; FId: 24; FDataType: ptInt), //
    (FName: 'NeedRespInfo'; FId: 25; FDataType: ptByte), //
    (FName: 'ResponseInfo'; FId: 26; FDataType: ptString), //
    (FName: ''; FId: 27; FDataType: ptUnknown), //
    (FName: 'ServerRefer'; FId: 28; FDataType: ptString), //
    (FName: ''; FId: 29; FDataType: ptUnknown), //
    (FName: ''; FId: 30; FDataType: ptUnknown), //
    (FName: 'Reason'; FId: 31; FDataType: ptString), //
    (FName: ''; FId: 32; FDataType: ptUnknown), //
    (FName: 'RecvMax'; FId: 33; FDataType: ptWord), //
    (FName: 'AliasMax'; FId: 34; FDataType: ptWord), //
    (FName: 'TopicAlias'; FId: 35; FDataType: ptWord), //
    (FName: 'MaxQoS'; FId: 36; FDataType: ptByte), //
    (FName: 'HasRetain'; FId: 37; FDataType: ptByte), //
    (FName: 'UserProp'; FId: 38; FDataType: ptString), //
    (FName: 'MaxPkgSize'; FId: 39; FDataType: ptInt), //
    (FName: 'HasWildcardSubcribes'; FId: 40; FDataType: ptByte), //
    (FName: 'HasSubscribeId'; FId: 41; FDataType: ptByte), //
    (FName: 'HasShareSubscribes'; FId: 42; FDataType: ptByte) //
    );

type
  TQMQTTConnectHeader = packed record
    Protocol: array [0 .. 5] of Byte;
    Level: Byte; // Level
    Flags: Byte; // Flags
    Interval: Word; // Keep Alive
  end;

  PMQTTConnectHeader = ^TQMQTTConnectHeader;

  TSocketThread = class(TThread)
  protected
    [unsafe]
    FOwner: TQMQTTMessageClient;
  public
    constructor Create(AOwner: TQMQTTMessageClient); overload;
  end;

  TSocketRecvThread = class(TSocketThread)
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TQMQTTMessageClient); overload;
  end;

  TSocketSendThread = class(TSocketThread)
  protected
    FNotifyEvent: TEvent;
    FFirst, FLast: PQMQTTMessage;
    procedure Execute; override;
  public
    constructor Create(AOwner: TQMQTTMessageClient); overload;
    destructor Destroy; override;
    procedure Post(AMessage: PQMQTTMessage);
    function Send(AMessage: PQMQTTMessage;
      ATimeout: Cardinal = INFINITE): Boolean;
    procedure Clear;
  end;

  TTopicHandler = class
  protected
    FRegex: TPerlRegex;
    FTopic: String;
    FOnDispatch: TQMQTTTopicDispatchEvent;
    FNext: TTopicHandler;
    FMatchType: TTopicMatchType;
  public
    constructor Create(const ATopic: String; AHandler: TQMQTTTopicDispatchEvent;
      AMatchType: TTopicMatchType);
    destructor Destroy; override;
    function IsMatch(const ATopic: String): Boolean;
    property Topic: String read FTopic;
  end;

const
  RegexTopic = '@regex@';
  PatternTopic = '@topic@';

var
  _DefaultClient: TQMQTTMessageClient = nil;

function DefaultMqttClient: TQMQTTMessageClient;
var
  AClient: TQMQTTMessageClient;
begin
  if not Assigned(_DefaultClient) then
  begin
    AClient := TQMQTTMessageClient.Create(nil);
    if AtomicCmpExchange(Pointer(_DefaultClient), Pointer(AClient), nil) <> nil
    then
      FreeAndNil(AClient);
{$IFDEF AUTOREFCOUNT}
    AClient.__ObjAddRef;
{$ENDIF}
  end;
  Result := _DefaultClient;
end;
{ TMessageQueue }

function TQMQTTMessageClient.AcquirePackageId(AReq: PQMQTTMessage;
  AIsWaitAck: Boolean): Word;
var
  ALast: Word;
  ATryTimes: Integer;
begin
  ATryTimes := 0;
  repeat
    Lock;
    try
      ALast := FPackageId;
      repeat
        Result := Word(AtomicIncrement(FPackageId));
        if not Assigned(FWaitAcks[Result]) then
        begin
          if AIsWaitAck then
          begin
            FWaitAcks[Result] := AReq;
            Inc(FWaitAckTopics);
          end;
          Break;
        end;
      until Result = ALast;
    finally
      Unlock;
    end;
    if Result = ALast then
    begin
      Result := 0;
      // 给出时间来确空出可用的PackageId，如果重试超过3次仍不行，放弃
      // 这里给出较大的延时,是因为到这种情况下，明显待确认的已经有65536项，
      // 明显是太多了
      Sleep(50);
      Inc(ATryTimes);
    end;
  until (Result > 0) and (ATryTimes < 3);
  // 投递的太多，造成等待确认的包已经达到上限，那么放弃
  Assert(Result <> ALast);
end;

procedure TQMQTTMessageClient.BeforeDestruction;
begin
  inherited;
  Stop;
  CheckSynchronize;
end;

procedure TQMQTTMessageClient.CheckPublished;
var
  I: Integer;
  ATick, ATimeout: Cardinal;
begin
  if (FWaitAckTopics > 0) and Assigned(FSendThread) then
  begin
    ATick := {$IF RTLVersion>=23}TThread.{$IFEND}GetTickCount;
    ATimeout := MaxTopicAckTimeout * 1000;
    Lock;
    try
      for I := 0 to High(FWaitAcks) do
      begin
        if Assigned(FWaitAcks[I]) and (FWaitAcks[I].SentTime > 0) and
          (ATick - FWaitAcks[I].SentTime > ATimeout) then
          TSocketSendThread(FSendThread).Post(FWaitAcks[I]);
      end;
    finally
      Unlock;
    end;
  end;
end;

procedure TQMQTTMessageClient.CleanPublishPendings;
var
  AItem, ANext: PQMQTTPublishPendingItem;
begin
  if Assigned(FPublishPendings.First) then
  begin
    AItem := FPublishPendings.First;
    FPublishPendings.First := nil;
    FPublishPendings.Last := nil;
    while Assigned(AItem) do
    begin
      ANext := AItem.Next;
      if Assigned(AItem.Props) then
        FreeAndNil(AItem.Props);
      Dispose(AItem);
      AItem := ANext;
    end;
  end;
end;

procedure TQMQTTMessageClient.ClearHandlers;
var
  I: Integer;
  AHandler, ANext: TTopicHandler;
begin
  for I := 0 to FTopicHandlers.Count - 1 do
  begin
    AHandler := TTopicHandler(FTopicHandlers.Objects[I]);
    while Assigned(AHandler) do
    begin
      ANext := AHandler.FNext;
      FreeAndNil(AHandler);
      AHandler := ANext;
    end;
  end;
  FTopicHandlers.Clear;
end;

procedure TQMQTTMessageClient.ClearSubscribes;
var
  I: Integer;
  ATopics: array of String;
  AList: TStringList;
  AProps: TQMQTT5Props;
begin
  AList := TStringList.Create;
  try
    AList.Duplicates := dupIgnore;
    AList.Sorted := True;
    for I := 0 to FSubscribes.Count - 1 do
    begin
      AList.Add(ValueOfW(FSubscribes[I], '|'));
      AProps := TQMQTT5Props(AList.Objects[I]);
      if Assigned(AProps) then
        FreeAndNil(AProps);
    end;
    SetLength(ATopics, AList.Count);
    for I := 0 to AList.Count - 1 do
      ATopics[I] := AList[I];
    Unsubscribe(ATopics);
    FSubscribes.Clear;
  finally
    FreeAndNil(AList);
  end;
end;

procedure TQMQTTMessageClient.ClearWaitAcks;
var
  I, C: Integer;
  ATemp: TArray<PQMQTTMessage>;
begin
  SetLength(ATemp, 65536);
  C := 0;
  Lock;
  try
    for I := 0 to High(FWaitAcks) do
    begin
      if Assigned(FWaitAcks[I]) then
      begin
        ATemp[C] := FWaitAcks[I];
        Inc(C);
        FWaitAcks[I] := nil;
      end;
    end;
  finally
    Unlock;
  end;
  for I := 0 to C - 1 do
    FreeMessage(ATemp[I]);
end;

constructor TQMQTTMessageClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPeekInterval := 60; // 默认 Ping 时间间隔
  FConnectionTimeout := 15; // 默认连接超时
  FReconnectInterval := 30; // 默认重连间隔
  FMaxTopicAckTimeout := 15; // 对于QoS1/2消息，从发出到确认的最大间隔时间
  FPublishTTL := INFINITE;
  FStates := [];
  FNotifyEvent := TEvent.Create(nil, false, false, '');
  FTopicHandlers := TStringList.Create;
  FTopicHandlers.Sorted := True;
  FSubscribes := TStringList.Create;
  FSubscribes.Sorted := True;
  FSubscribes.Duplicates := dupIgnore;
  FProtocolVersion := pv3_1_1;
  FLocker := TCriticalSection.Create;
end;

destructor TQMQTTMessageClient.Destroy;
var
  I: Integer;
  AProps: TQMQTT5Props;
begin
  AtomicCmpExchange(Pointer(_DefaultClient), nil, Pointer(Self));
  DebugOut('Free MQTT client %x', [IntPtr(Self)]);
  FreeAndNil(FNotifyEvent);
  ClearHandlers;
  FreeAndNil(FTopicHandlers);
  for I := 0 to FSubscribes.Count - 1 do
  begin
    AProps := TQMQTT5Props(FSubscribes.Objects[I]);
    if Assigned(AProps) then
      FreeAndNil(AProps);
  end;
  CleanPublishPendings;
  FreeAndNil(FSubscribes);
  FreeAndNil(FConnectProps);
  FreeAndNil(FLocker);
  inherited;
end;

procedure TQMQTTMessageClient.Disconnect;
var
  AReq: PQMQTTMessage;
begin
  if Assigned(FSendThread) then
  begin
    AReq := TQMQTTMessage.Create(Self);
    AReq.PayloadSize := 0;
    AReq.ControlType := TQMQTTControlType.ctDisconnect;
    AReq.FAfterSent := DoFreeAfterSent;
    AReq.States := [msNeedWait];
    TSocketSendThread(FSendThread).Send(AReq, 1000);
  end;
end;

procedure TQMQTTMessageClient.InvokeTopicHandlers(const ATopic: String;
  AMsg: PQMQTTMessage);
var
  AIdx: Integer;
  procedure InvokeItem(AItem: TTopicHandler);
  begin
    while Assigned(AItem) do
    begin
      if Assigned(AItem.FOnDispatch) and AItem.IsMatch(ATopic) then
      begin
        case IntPtr(TMethod(AItem.FOnDispatch).Data) of
          0:
            TQMQTTTopicDispatchEventG(TMethod(AItem.FOnDispatch).Code)
              (Self, ATopic, AMsg);
          1:
            TQMQTTTopicDispatchEventA(TMethod(AItem.FOnDispatch).Code)
              (Self, ATopic, AMsg);
        else
          AItem.FOnDispatch(Self, ATopic, AMsg);
        end;
      end;
      AItem := AItem.FNext;
    end;
  end;

begin
  try
    if Assigned(FBeforeDispatch) then
      FBeforeDispatch(Self, ATopic, AMsg);
    if FTopicHandlers.Find(ATopic, AIdx) then
      InvokeItem(TTopicHandler(FTopicHandlers.Objects[AIdx]));
    if FTopicHandlers.Find(PatternTopic, AIdx) then
      InvokeItem(TTopicHandler(FTopicHandlers.Objects[AIdx]));
    if FTopicHandlers.Find(RegexTopic, AIdx) then
      InvokeItem(TTopicHandler(FTopicHandlers.Objects[AIdx]));
  finally
    if Assigned(FAfterDispatch) then
      FAfterDispatch(Self, ATopic, AMsg);
  end;
end;

procedure TQMQTTMessageClient.DispatchProps(AMsg: PQMQTTMessage);
var
  AProps: TQMQTT5Props;
begin
  if ProtocolVersion = pv5_0 then
  begin
    AProps := TQMQTT5Props.Create;
    try
      AProps.ReadProps(AMsg);
      ConnectProps.Replace(AProps);
    finally
      FreeAndNil(AProps);
    end;
  end;
end;

procedure TQMQTTMessageClient.DispatchTopic(AMsg: PQMQTTMessage);
var
  ACopy: PQMQTTMessage;
begin
  ACopy := AMsg.Copy;
  Queue(
    procedure
    var
      ATopic: String;
    begin
      try
        Inc(FRecvTopics);
        ATopic := ACopy.TopicName;
        if Assigned(FOnRecvTopic) then
          FOnRecvTopic(Self, ATopic, ACopy);
        InvokeTopicHandlers(ATopic, ACopy);
      finally
        FreeMessage(ACopy);
      end;
    end);
end;

function TQMQTTMessageClient.DNSLookupV4(const AHost: QStringW): Cardinal;
var
  Utf8Host: QStringA;
  AEntry: PHostEnt;
  function TryAsAddr(

    var AResult: Cardinal): Boolean;
  var
    p: PQCharW;
    I, V: Integer;
    B: array [0 .. 3] of Byte absolute AResult;
  begin
    p := PQCharW(AHost);
    V := 0;
    I := 0;
    while p^ <> #0 do
    begin
      if (p^ >= '0') and (p^ <= '9') then
        V := V * 10 + Ord(p^) - Ord('0')
      else if p^ = '.' then
      begin
        if V > 255 then
        begin
          Result := false;
          Exit;
        end;
        B[I] := Byte(V);
        Inc(I);
        V := 0;
      end
      else
      begin
        Result := false;
        Exit;
      end;
      Inc(p);
    end;
    Result := (p^ = #0) and (I = 3) and (V < 256);
    if Result then
      B[I] := V;
  end;

begin
  if not TryAsAddr(Result) then
  begin
    Result := 0;
    Utf8Host := qstring.Utf8Encode(AHost);
    AEntry := gethostbyname
      ({$IFDEF UNICODE}MarshaledAString{$ELSE}PAnsiChar{$ENDIF}(PQCharA(Utf8Host)));
    if Assigned(AEntry) then
    begin
      if AEntry.h_addrtype = AF_INET then
      begin
        if AEntry.h_addr_list^ <> nil then
          Result := PCardinal(AEntry.h_addr_list^)^;
      end;
    end;
  end;
end;

procedure TQMQTTMessageClient.DoAfterConnected;
begin
  Queue(
    procedure
    begin
      FConnected := True;
      FReconnectTimes := 0; // 清空重连计数
      FReconnectTime := 0;
      FPingTime := {$IF RTLVersion>=23}TThread.{$IFEND}GetTickCount;
      FStates := FStates - [qcsConnecting, qcsReconnecting];
      if Assigned(FAfterConnected) then
        FAfterConnected(Self);
      SubscribePendings;
      PublishFirstPending;
    end);
end;

procedure TQMQTTMessageClient.DoAfterSent(const AReq: PQMQTTMessage);
begin
  if Assigned(AReq.FAfterSent) or Assigned(FAfterSent) then
  begin
    Queue(
      procedure
      begin
        if Assigned(AReq.FAfterSent) then
          AReq.FAfterSent(AReq);
        if Assigned(FAfterSent) then
          FAfterSent(AReq);
      end);
  end;
end;

procedure TQMQTTMessageClient.DoAfterDisconnected;
begin
  if poCleanOnDisconnect in FPublishOptions then
    CleanPublishPendings;
  if Assigned(FAfterDisconnected) then
    FAfterDisconnected(Self);
end;

procedure TQMQTTMessageClient.DoAfterSubcribed(AResult: PQMQTTSubscribeResults);
begin
  if Assigned(FAfterSubscribed) then
  begin
    Queue(
      procedure
      begin
        try
          if Assigned(FAfterSubscribed) then
            FAfterSubscribed(Self, AResult^);
        finally
          Dispose(AResult);
        end;
      end);
  end
  else
    Dispose(AResult);
end;

procedure TQMQTTMessageClient.DoAfterUnsubscribed(ASource: PQMQTTMessage);
begin
  if Assigned(FAfterUnsubscribed) and Assigned(ASource) then
  begin
    Queue(
      procedure
      var
        ATopic: String;
      begin
        try
          if Assigned(FAfterUnsubscribed) then
          begin
            ASource.Position := ASource.VarHeaderOffset + 2;
            while ASource.Current < ASource.Eof do
            begin
              ATopic := ASource.NextString;
              FAfterUnsubscribed(Self, ATopic);
            end;
          end;
        finally
          FreeMessage(ASource);
        end;
      end);
  end
  else
    FreeMessage(ASource);
end;

procedure TQMQTTMessageClient.DoBeforeConnect;
begin
  if Assigned(FBeforeConnect) then
  begin
    Queue(
      procedure
      begin
        FStates := FStates + [qcsConnecting];
        if Assigned(FBeforeConnect) then
          FBeforeConnect(Self);
      end)
  end;
end;

procedure TQMQTTMessageClient.DoBeforePublish(ATopic: String;
AMsg: PQMQTTMessage);
var
  ATemp: PQMQTTMessage;
begin
  if Assigned(FBeforePublish) then
  begin
    ATemp := AMsg.Copy; // 已知：如果正在处理时程序溢出，内存可能泄露
    Queue(
      procedure
      begin
        try
          if Assigned(FBeforePublish) then
            FBeforePublish(Self, ATemp.TopicName, ATemp);
        finally
          Dispose(ATemp);
        end;
      end);
  end;
end;

procedure TQMQTTMessageClient.DoBeforeSend(const AReq: PQMQTTMessage);
begin
  if Assigned(FBeforeSend) then
  begin
  end;
end;

procedure TQMQTTMessageClient.DoCleanup;
begin
  FSSL := nil;
end;

procedure TQMQTTMessageClient.DoCloseSocket;
const
  SD_BOTH = 2;
begin
  if (FSocket <> 0) then
  begin
    if Assigned(FSSL) then
    begin
      FSSL := nil;
    end;
    Shutdown(FSocket, SD_BOTH);
{$IFDEF MSWINDOWS}
    closesocket(FSocket);
{$ELSE}
    __close(FSocket);
{$ENDIF}
    FSocket := 0;
    FPingStarted := 0;
    DoAfterDisconnected;
  end;
end;

procedure TQMQTTMessageClient.DoConnect;
var
  AReq: PQMQTTMessage;
  AUserName, APassword, AClientId, AWillTopic: QStringA;
  APayloadSize: Integer;
  AHeader: PMQTTConnectHeader;

const
  Protocol: array [0 .. 5] of Byte = (0, 4, Ord('M'), Ord('Q'), Ord('T'),
    Ord('T'));
begin
  FConnected := false;
  AReq := TQMQTTMessage.Create(Self);
  APayloadSize := SizeOf(TQMQTTConnectHeader);
  if Length(FUserName) > 0 then
  begin
    AUserName := qstring.Utf8Encode(FUserName);
    Inc(APayloadSize, 2 + AUserName.Length);
  end;
  if Length(FPassword) > 0 then
  begin
    APassword := qstring.Utf8Encode(FPassword);
    Inc(APayloadSize, 2 + APassword.Length);
  end;
  ValidClientId;
  AClientId := qstring.Utf8Encode(FClientId);
  Inc(APayloadSize, 2 + AClientId.Length);
  if (Length(FWillTopic) > 0) and (Length(FWillMessage) > 0) then
  begin
    AWillTopic := qstring.Utf8Encode(FWillTopic);
    Inc(APayloadSize, 4 + AWillTopic.Length + Length(FWillMessage));
  end;
  if (ProtocolVersion = pv5_0) then
  begin
    if Assigned(FConnectProps) then
      Inc(APayloadSize, FConnectProps.PayloadSize)
    else
      Inc(APayloadSize, 1);
  end;
  AReq.PayloadSize := APayloadSize;
  AReq.ControlType := ctConnect;
  AReq.IsRetain := IsRetain;
  AReq.QosLevel := QosLevel;
  AHeader := PMQTTConnectHeader(AReq.Current);
  Move(Protocol, AHeader^.Protocol, 6);
  with AHeader^ do
  begin
    Level := Ord(ProtocolVersion);
    // MQTT 3.1.1
    Flags := 0;
    if Length(FUserName) > 0 then
      Flags := Flags or $80;
    if Length(FPassword) > 0 then
      Flags := Flags or $40;
    if IsRetain then
      Flags := Flags or $20;
    Flags := Flags or (Ord(QosLevel) shl 3);
    if Length(FWillTopic) > 0 then
      Flags := Flags or $04;
    if CleanLastSession then
      Flags := Flags or $02;
    Interval := ExchangeByteOrder(FPeekInterval);
  end;
  AReq.MoveBy(SizeOf(TQMQTTConnectHeader));
  if (ProtocolVersion = pv5_0) then
  begin
    if Assigned(FConnectProps) then
      FConnectProps.WriteProps(AReq)
    else
      AReq.Cat(Byte(0));
  end;
  AReq.Cat(AClientId);
  if (Length(FWillTopic) > 0) and (Length(FWillMessage) > 0) then
    AReq.Cat(AWillTopic).Cat(Word(Length(FWillMessage))).Cat(FWillMessage);
  if Length(FUserName) > 0 then
    AReq.Cat(AUserName);
  if Length(FPassword) > 0 then
    AReq.Cat(APassword);
  AReq.FAfterSent := DoFreeAfterSent;
  if Assigned(FSendThread) then
    TSocketSendThread(FSendThread).Post(AReq);
end;

procedure TQMQTTMessageClient.DoConnectFailed;
begin
  FStates := FStates - [qcsConnecting];
end;

procedure TQMQTTMessageClient.DoDispatch(var AReq: TQMQTTMessage);

  procedure DispatchConnectAck;
  var
    AErrorCode: Byte;
  begin
    Assert(AReq.PayloadSize >= 2); // RemainSize=2
    if ProtocolVersion = pv5_0 then
    begin
      AReq.MoveToVarHeader;
      AReq.MoveBy(1);
      // 跳过标志位，只有一个会话是否是已经存在的标记，忽略掉(0x01)
      AErrorCode := AReq.NextByte;
      if AErrorCode = 0 then
      begin
        DispatchProps(@AReq);
        DoAfterConnected;
      end
      else
      begin
        DoConnectFailed;
        DoError(MQERR_CONNECT + AErrorCode);
      end
    end
    else
    begin
      // 错误代码
      case AReq[3] of
        0: //
          DoAfterConnected
      else
        begin
          DoError(MQERR_CONNECT + AReq[3]);
          DoConnectFailed;
          Queue(Stop, True);
        end;
      end;
    end;
  end;

  procedure DispatchSubscribeAck;
  var
    APackageId: Word;
    AAckPayload: Cardinal;
    ASource: PQMQTTMessage;
    AResults: PQMQTTSubscribeResults;
    AIdx: Integer;
    Ack: Byte;
  begin
    // 跳过固定报头
    AReq.Position := 1;
    // 载荷大小
    AAckPayload := AReq.NextDWord(True);
    // 消息ID
    APackageId := AReq.NextWord(false);
    // 属性(5.0+)
    DispatchProps(@AReq);
    ASource := PopWaitAck(APackageId);
    if Assigned(ASource) then
    begin
      New(AResults);
      try
        ASource.Position := ASource.VarHeaderOffset + 2;
        SetLength(AResults^, AAckPayload - 2); // 去掉PackageId，剩下的每个字节处理一个结果
        AIdx := 0;
        while ASource.Current < ASource.Eof do
        begin
          with AResults^[AIdx] do
          begin
            Topic := ASource.NextString;
            Ack := AReq.NextByte;
            if (Ack and $80) <> 0 then
            begin
              DoError(MQERR_SUBSCRIBE_FAILED);
              ErrorCode := Ack;
              Qos := TQMQTTQoSLevel(ASource.NextByte);
            end
            else
            begin
              Qos := TQMQTTQoSLevel(Ack);
              ErrorCode := 0;
              ASource.NextByte;
            end;
          end;
          Inc(AIdx);
        end;
        DoAfterSubcribed(AResults);
        AResults := nil;
      finally
        FreeMessage(ASource);
        if Assigned(AResults) then
          Dispose(AResults);
      end;
    end;
  end;

  procedure DispatchPublish;
  var
    Ack: PQMQTTMessage;
    APackageId, ATopicLen: Word;
    AQoSLevel: TQMQTTQoSLevel;
  begin
    DispatchTopic(@AReq);
    AReq.Position := AReq.VarHeaderOffset;
    ATopicLen := AReq.NextWord(false);
    AReq.MoveBy(ATopicLen);
    AQoSLevel := AReq.QosLevel;
    if AQoSLevel > qlMax1 then
    begin
      APackageId := AReq.NextWord(false);
      Ack := TQMQTTMessage.Create(Self);
      Ack.PayloadSize := 2;
      if AQoSLevel = qlAtLeast1 then
        Ack.ControlType := TQMQTTControlType.ctPublishAck
      else
        Ack.ControlType := TQMQTTControlType.ctPublishRecv;
      Ack.Cat(APackageId);
      Ack.FAfterSent := DoFreeAfterSent;
      TSocketSendThread(FSendThread).Post(Ack);
    end;
  end;

  procedure DoPublishAck;
  var
    APackageId: Word;
  begin
    AReq.Position := AReq.VarHeaderOffset;
    APackageId := AReq.NextWord(false);
    Queue(
      procedure
      begin
        DoTopicPublished(PopWaitAck(APackageId));
      end);
  end;

  procedure DoPublishRelease;
  var
    APackageId: Word;
    Ack: PQMQTTMessage;
  begin
    AReq.Position := AReq.VarHeaderOffset;
    APackageId := AReq.NextWord(false);
    Queue(
      procedure
      var
        ASource: PQMQTTMessage;
      begin
        ASource := PopWaitAck(APackageId);
        if Assigned(ASource) then
          FreeMessage(ASource);
      end);
    Ack := TQMQTTMessage.Create(Self);
    Ack.PayloadSize := 2;
    Ack.QosLevel := TQMQTTQoSLevel.qlAtLeast1;
    Ack.ControlType := TQMQTTControlType.ctPublishDone;
    Ack.Cat(APackageId);
    Ack.FAfterSent := DoFreeAfterSent;
    TSocketSendThread(FSendThread).Post(Ack);
  end;

  procedure DoPublishRecv;
  var
    Ack: PQMQTTMessage;
    APackageId: Word;
  begin
    Ack := TQMQTTMessage.Create(Self);
    AReq.Position := AReq.VarHeaderOffset;
    APackageId := AReq.NextWord(false);
    Ack.PayloadSize := 2;
    Ack.IsRetain := false;
    Ack.QosLevel := TQMQTTQoSLevel.qlAtLeast1;
    Ack.ControlType := TQMQTTControlType.ctPublishRelease;
    Ack.Position := AReq.VarHeaderOffset;
    Ack.Cat(APackageId);
    Ack.FAfterSent := DoFreeAfterSent;
    TSocketSendThread(FSendThread).Post(Ack);
  end;

  procedure DoUnsubscribeAck;
  var
    APackageId: Word;
  begin
    AReq.Position := AReq.VarHeaderOffset;
    APackageId := AReq.NextWord(false);
    Queue(
      procedure
      begin
        DoAfterUnsubscribed(PopWaitAck(APackageId));
      end);
  end;

  procedure DoPingAck;
  begin
    Queue(
      procedure
      begin
{$IFDEF LogMqttContent}
        PostLog(llDebug, 'Ping 服务器往返用时 %d ms',
          [GetTickCount - FPingStarted], 'QMQTT');
{$ENDIF}
        FPingStarted := 0;
      end);
  end;

begin
{$IFDEF LogMqttContent}
  if LogPackageContent then
    PostLog(llDebug, '[接收]收到命令 %d，TopicId=%d,载荷大小:%d,总大小:%d 内容:'#13#10'%s',
      [Ord(AReq.ControlType), Integer(AReq.TopicId), Integer(AReq.PayloadSize),
      Integer(AReq.Size), AReq.ContentAsHexText], 'QMQTT')
  else
    PostLog(llDebug, '[接收]收到命令 %d，TopicId=%d,载荷大小:%d,总大小:%d',
      [Ord(AReq.ControlType), Integer(AReq.TopicId), Integer(AReq.PayloadSize),
      Integer(AReq.Size)], 'QMQTT');
{$ENDIF}
  AReq.States := AReq.States + [msDispatching];
  case AReq.ControlType of
    ctConnectAck: // 连接成功
      DispatchConnectAck;
    ctPublish: // 收到服务器发布的消息
      DispatchPublish;
    ctPublishAck: // 收到发布主题服务器端的确认
      DoPublishAck;
    ctPublishRecv:
      DoPublishRecv;
    ctPublishRelease:
      DoPublishRelease;
    ctPublishDone:
      DoPublishAck;
    ctSubscribeAck:
      DispatchSubscribeAck;
    ctUnsubscribeAck:
      DoUnsubscribeAck;
    ctPingResp:
      DoPingAck;
  end;
  AReq.States := AReq.States + [msDispatched];
end;

procedure TQMQTTMessageClient.DoError(AErrorCode: Integer);
const
  KnownErrorMessages: array [0 .. 6] of String = ('操作成功完成', '协议版本号不受支持',
    '客户端ID被拦截', '服务不可用', '用户名或密码错误', '客户端未被授权连接', '订阅指定的主题失败');
var
  AMsg: String;
begin
  if Assigned(FOnError) then
  begin
    if AErrorCode < Length(KnownErrorMessages) then
      AMsg := KnownErrorMessages[AErrorCode]
    else
    begin
      AMsg := '未知的错误代码：' + IntToStr(AErrorCode);
    end;
    Queue(
      procedure
      begin
        FOnError(Self, AErrorCode, AMsg);
      end);
  end;
end;

procedure TQMQTTMessageClient.DoFreeAfterSent(const AMessage: PQMQTTMessage);
begin
  FreeMessage(AMessage);
end;

procedure TQMQTTMessageClient.DoMQTTConnect(AJob: PQJob);
var
  Addr: sockaddr_in;
  tm: TIMEVAL;
  mode: Integer;
  ASocket: THandle;
  fdWrite, fdError:
{$IFDEF MSWINDOWS}TFdSet{$ELSE}FD_SET{$ENDIF};
begin
  if (csDestroying in ComponentState) then // 未处于析构状态
    Exit;
  FLastConnectTime :=
{$IF RTLVersion>=23}TThread.{$IFEND} GetTickCount;
  DoBeforeConnect;
  ASocket := Socket(PF_INET, SOCK_STREAM, 0);
  if ASocket = THandle(-1) then
    RaiseLastOSError;
  try
    // 连接到远程地址
    Addr.sin_family := AF_INET;
    Addr.sin_port := htons(FServerPort);
    Addr.sin_addr.S_addr := DNSLookupV4(FServerHost);
    if Addr.sin_addr.S_addr = 0 then
      RaiseLastOSError;
    tm.tv_sec := FConnectionTimeout;
    tm.tv_usec := 0;
{$IFDEF MSWINDOWS}
    mode := 1;
{$ENDIF}
    if {$IFDEF MSWINDOWS}ioctlsocket(ASocket, FIONBIO, mode) <>
      NO_ERROR{$ELSE}Fcntl(ASocket, F_SETFL, Fcntl(ASocket, F_GETFL, 0) or
      O_NONBLOCK) = -1{$ENDIF} then
      RaiseLastOSError;
    CONNECT(ASocket,
{$IFDEF MSWINDOWS}sockaddr_in{$ELSE}sockaddr{$ENDIF}(Addr), SizeOf(Addr));
    FD_ZERO(fdWrite);
    FD_ZERO(fdError);
{$IFDEF MSWINDOWS}
    FD_SET(ASocket, fdWrite);
    FD_SET(ASocket, fdError);
{$ELSE}
    _FD_SET(ASocket, fdWrite);
    _FD_SET(ASocket, fdError);
{$ENDIF}
    mode := select(ASocket + 1, nil, @fdWrite, @fdError, @tm);
    if mode >= 0 then
    begin
      if FD_ISSET(ASocket, fdError) then
        RaiseLastOSError;
      if FD_ISSET(ASocket, fdWrite) then
        DebugOut('Socket writable')
      else
      begin
        raise EMQTTAbortError.Create('连接服务器失败');
      end;
    end
    else
      RaiseLastOSError;
{$IFDEF MSWINDOWS}
    mode := 0;
{$ENDIF}
    if {$IFDEF MSWINDOWS}ioctlsocket(ASocket, FIONBIO, mode) <>
      NO_ERROR{$ELSE}Fcntl(ASocket, F_SETFL, Fcntl(ASocket, F_GETFL, 0) or
      O_NONBLOCK) = -1{$ENDIF} then
      RaiseLastOSError;
    if UseSSL and (not Assigned(FSSL)) then
    begin
      if TQSSLManager.ActiveFactory <> nil then
      begin
        // SSL 使用阻塞模式,异步模式控制有点小复杂:)
{$IFDEF MSWINDOWS}
        mode := 0;
{$ENDIF}
        if {$IFDEF MSWINDOWS}ioctlsocket(ASocket, FIONBIO, mode) <>
          NO_ERROR{$ELSE}Fcntl(ASocket, F_SETFL, Fcntl(ASocket, F_GETFL, 0) or
          O_NONBLOCK) = -1{$ENDIF} then
          RaiseLastOSError;
        FSSL := TQSSLManager.ActiveFactory.NewItem;
        if not Assigned(FSSL) then
          raise EMQTTAbortError.Create(SInitSSLFailed);
        FSSL.Handle := ASocket;
        if not FSSL.CONNECT then
        begin
          raise EMQTTAbortError.Create(FSSL.GetFactory.LastErrorMsg);
        end;
        // raise Exception.Create(FSSL.LastErrorMsg);
        // 连接完再调整回去:)
        mode := 1;
        if {$IFDEF MSWINDOWS}ioctlsocket(ASocket, FIONBIO, mode) <> NO_ERROR
{$ELSE}Fcntl(ASocket, F_SETFL, Fcntl(ASocket, F_GETFL, 0) and (not O_NONBLOCK))
          = -1{$ENDIF} then
          RaiseLastOSError;
      end
      else
      begin
        Stop;
        raise Exception.Create(SInitSSLFailed);
      end;
    end;
    FSocket := ASocket;
    DoConnect;
    FConnectJob := 0;
  except
    on E: Exception do
    begin
      mode := GetLastError;
      FConnectJob := 0;
      if not(csDestroying in ComponentState) then
      // 未处于析构状态
      begin
        TThread.Synchronize(nil,
          procedure
          begin
            if Assigned(FOnError) then
              FOnError(Self, mode, E.Message);
          end);
      end;
      FStates := FStates - [qcsConnecting];
      FReconnectTime :=
{$IF RTLVersion>=23}TThread.{$IFEND} GetTickCount;
      if not(qcsReconnecting in FStates) then
      begin
        if qcsRunning in FStates then
          FStates := FStates + [qcsReconnecting];
      end;
{$IFDEF MSWINDOWS}
      closesocket(ASocket);
{$ELSE}
      __close(ASocket);
{$ENDIF}
      if ASocket = FSocket then
        FSocket := 0;
      TThread.ForceQueue(nil, DoCloseSocket);
    end;
  end;

end;

procedure TQMQTTMessageClient.DoPing;
var
  AReq: PQMQTTMessage;
begin
  if (FPingStarted = 0) and Assigned(FSendThread) then
  begin
    FPingStarted :=
{$IF RTLVersion>=23}TThread.{$IFEND} GetTickCount;
    FPingTime := FPingStarted;
    AReq := TQMQTTMessage.Create(Self);
    AReq.PayloadSize := 0;
    AReq.ControlType := TQMQTTControlType.ctPing;
    AReq.FAfterSent := DoFreeAfterSent;
    TSocketSendThread(FSendThread).Post(AReq);
  end;
end;

procedure TQMQTTMessageClient.DoQueueCallback(AJob: PQJob);
begin
  TThreadProcedure(AJob.Data)();
end;

procedure TQMQTTMessageClient.FreeMessage(AMsg: PQMQTTMessage);
var
  APkgId: Word;
begin
  if Assigned(AMsg) then
  begin
    if msNeedAck in AMsg.States then
    begin
      APkgId := AMsg.PackageId;
      Lock;
      try
        if FWaitAcks[APkgId] = AMsg then
        begin
          FWaitAcks[APkgId] := nil;
          Dec(FWaitAckTopics);
        end;
      finally
        Unlock;
      end;
    end;
    Dispose(AMsg);
  end;
end;

function TQMQTTMessageClient.GetClientId: String;
begin
  ValidClientId;
  Result := FClientId;
end;

function TQMQTTMessageClient.GetConnectProps: TQMQTT5Props;
begin
  if not Assigned(FConnectProps) then
    FConnectProps := TQMQTT5Props.Create;
  Result := FConnectProps;
end;

function TQMQTTMessageClient.GetIsRunning: Boolean;
begin
  Result := Assigned(FRecvThread) and Assigned(FSendThread) and (FSocket <> 0);
end;

function TQMQTTMessageClient.GetSSLManager: TQSSLManager;
begin
  Result := TQSSLManager.Current;
end;

procedure TQMQTTMessageClient.Lock;
begin
  FLocker.Enter;
end;

procedure TQMQTTMessageClient.Publish(const ATopic, AContent: String;
AQoSLevel: TQMQTTQoSLevel);
var
  AUtf8Content: QStringA;
begin
  AUtf8Content := qstring.Utf8Encode(AContent);
  Publish(ATopic, AUtf8Content.Data^, AUtf8Content.Length, AQoSLevel);
end;

procedure TQMQTTMessageClient.Publish(const ATopic: String; AContent: TBytes;
AQoSLevel: TQMQTTQoSLevel);
begin
  Publish(ATopic, AContent[0], Length(AContent), AQoSLevel)
end;

function TQMQTTMessageClient.PopWaitAck(APackageId: Word): PQMQTTMessage;
begin
  Lock;
  try
    if APackageId < Length(FWaitAcks) then
    begin
      Result := FWaitAcks[APackageId];
      FWaitAcks[APackageId] := nil;
      if Assigned(Result) then
        Dec(FWaitAckTopics);
    end
    else
      Result := nil;
  finally
    Unlock;
  end;
  if Assigned(Result) then
    Result.States := Result.States - [msNeedAck];
end;

procedure TQMQTTMessageClient.Publish(const ATopic: String; const AContent;
ALen: Cardinal; AQoSLevel: TQMQTTQoSLevel);
var
  AReq: PQMQTTMessage;
  AUtf8Topic: QStringA;
  APackageId: Word;
  ANeedAck: Boolean;
  APayloadSize: Cardinal;
  procedure PushPendings;
  var
    AItem: PQMQTTPublishPendingItem;
  begin
    New(AItem);
    AItem.Topic := ATopic;
    SetLength(AItem.Content, ALen);
    Move(AContent, AItem.Content[0], ALen);
    AItem.Qos := AQoSLevel;
    AItem.Next := nil;
    AItem.PushTime := {$IF RTLVersion>=23}TThread.{$IFEND}GetTickCount;
    AItem.Prior := nil;
    AItem.Props := nil;
    // Support for MQTT 5.0
    Queue(
      procedure
      begin
        AItem.Prior := FPublishPendings.Last;
        if Assigned(FPublishPendings.Last) then
          FPublishPendings.Last.Next := AItem
        else
          FPublishPendings.First := AItem;
        FPublishPendings.Last := AItem;
      end);
  end;

begin
  if not(IsRunning and Connected) then
  begin
    if poCacheUnpublish in FPublishOptions then // 发布的主题是否放到待发送队列中
      PushPendings;
    Exit;
  end;
  Assert(Length(ATopic) > 0);
  AUtf8Topic := qstring.Utf8Encode(ATopic);
  // 判断总长度不能超过限制
  APayloadSize := SizeOf(Word) + Cardinal(AUtf8Topic.Length) + ALen;
  AReq := TQMQTTMessage.Create(Self);
  ANeedAck := AQoSLevel <> TQMQTTQoSLevel.qlMax1;
  if ANeedAck then
  begin
    AReq.States := AReq.States + [msNeedAck];
    Inc(APayloadSize, 2);
    AReq.PayloadSize := APayloadSize;
    APackageId := AcquirePackageId(AReq, ANeedAck);
    if APackageId = 0 then
    begin
      FreeAndNil(AReq);
      if poCacheUnpublish in FPublishOptions then
        PushPendings
      else
        DoError(MQERR_PUBLISH_FAILED);
      Exit;
    end;
  end
  else
  begin
    AReq.PayloadSize := APayloadSize;
    APackageId := 0;
  end;
  AReq.ControlType := TQMQTTControlType.ctPublish;
  AReq.QosLevel := AQoSLevel;
  AReq.IsRetain := IsRetain;
  // 主题名
  AReq.Cat(AUtf8Topic);
  // 标志符
  if ANeedAck then
    AReq.Cat(APackageId);
  AReq.Cat(@AContent, ALen);
  DoBeforePublish(ATopic, AReq);
  if AQoSLevel = TQMQTTQoSLevel.qlMax1 then
    AReq.FAfterSent := DoTopicPublished;
  TSocketSendThread(FSendThread).Post(AReq);
end;

procedure TQMQTTMessageClient.PublishFirstPending;
var
  AItem: PQMQTTPublishPendingItem;
begin
  if Assigned(FPublishPendings.First) then
  begin
    AItem := FPublishPendings.First;
    FPublishPendings.First := AItem.Next;
    if not Assigned(AItem.Next) then
      FPublishPendings.Last := nil;
    Publish(AItem.Topic, AItem.Content, AItem.Qos);
    if Assigned(AItem.Props) then
      FreeAndNil(AItem.Props);
    Dispose(AItem);
  end;
end;

procedure TQMQTTMessageClient.Queue(ACallback: TThreadProcedure;
AIsForce: Boolean);
type
  PInterface = ^IInterface;
begin
  if AIsForce then
    Workers.Post(DoQueueCallback, Pointer(PInterface(@ACallback)^),
      not EventInThread, jdfFreeAsInterface)
  else
  begin
    if EventInThread or (GetCurrentThreadId = MainThreadId) then
      ACallback
    else
      Workers.Post(DoQueueCallback, Pointer(PInterface(@ACallback)^), True,
        jdfFreeAsInterface);
  end;
end;

procedure TQMQTTMessageClient.ReconnectNeeded;
begin
  if [qcsConnecting, qcsReconnecting] * FStates <> [] then
    Exit;
  if Assigned(FSendThread) then
    TSocketSendThread(FSendThread).Clear;
  DoCloseSocket;
  FStates := FStates + [qcsReconnecting];
  if FReconnectTimes = 0 then
    FReconnectTime :=
{$IF RTLVersion>=23}TThread.{$IFEND} GetTickCount;
  if FReconnectTimes < 5 then // 5次尽快连接,超过5次，就要去靠定时器延时重连
  begin
    Inc(FReconnectTimes);
    if not(qcsStopping in FStates) then
      RecreateSocket;
  end;
end;

procedure TQMQTTMessageClient.RecreateSocket;
begin
  if FConnectJob = 0 then
  begin
    DoCloseSocket;
    FConnectJob := Workers.Post(DoMQTTConnect, nil);
  end;
end;

procedure TQMQTTMessageClient.RegisterDispatch(const ATopic: String;
AHandler: TQMQTTTopicDispatchEventG; AType: TTopicMatchType);
var
  AMethod: TMethod;
  ATemp: TQMQTTTopicDispatchEvent absolute AMethod;
begin
  AMethod.Data := nil;
  AMethod.Code := @AHandler;
  RegisterDispatch(ATopic, ATemp, AType);
end;

procedure TQMQTTMessageClient.RegisterDispatch(const ATopic: String;
AHandler: TQMQTTTopicDispatchEventA; AType: TTopicMatchType);
var
  AMethod: TMethod;
  ATemp: TQMQTTTopicDispatchEvent absolute AMethod;
begin
  AMethod.Data := nil;
  TQMQTTTopicDispatchEventA(AMethod.Code) := AHandler;
  RegisterDispatch(ATopic, ATemp, AType);
end;

procedure TQMQTTMessageClient.RemoveSubscribePendings(const ATopics
  : array of String);
var
  I: Integer;
  procedure RemoveItem(const ATopic: String);
  var
    J: Integer;
  begin
    J := 0;
    while J < FSubscribes.Count - 1 do
    begin
      if CompareTopic(ATopic, ValueOfW(FSubscribes[J], '|')) >= 0 then
        FSubscribes.Delete(J)
      else
        Inc(J);
    end;
  end;

begin
  I := 0;
  while (FSubscribes.Count > 0) and (I < Length(ATopics)) do
  begin
    RemoveItem(ATopics[I]);
    Inc(I);
  end;
end;

procedure TQMQTTMessageClient.RegisterDispatch(const ATopic: String;
AHandler: TQMQTTTopicDispatchEvent; AType: TTopicMatchType);
var
  AItem, AFirst: TTopicHandler;
  AIdx: Integer;
  ARealTopic: String;
begin
  if AType = TTopicMatchType.mtRegex then
    ARealTopic := RegexTopic
  else if AType = TTopicMatchType.mtPattern then
    ARealTopic := PatternTopic
  else
    ARealTopic := ATopic;
  if FTopicHandlers.Find(ARealTopic, AIdx) then
    AFirst := TTopicHandler(FTopicHandlers.Objects[AIdx])
  else
    AFirst := nil;
  AItem := AFirst;
  // 检查主题响应是否注册过，以避免重复注册
  while Assigned(AItem) do
  begin
    if MethodEqual(TMethod(AItem.FOnDispatch), TMethod(AHandler)) and
      (AItem.Topic = ATopic) then
      Exit;
    AItem := AItem.FNext;
  end;
  // 没找到创建一个新的添加进去
  AItem := TTopicHandler.Create(ATopic, AHandler, AType);
  AItem.FNext := AFirst;
  if Assigned(AFirst) then
    FTopicHandlers.Objects[AIdx] := AItem
  else
    FTopicHandlers.AddObject(ARealTopic, AItem);
end;

procedure TQMQTTMessageClient.DoRecv;
var
  AReq: PQMQTTMessage;
  AReaded, ATotal, ATick, ALastLargeIoTick: Cardinal;
  ARecv: Integer;
  tm: TIMEVAL;
  fdRead, fdError:
{$IFDEF MSWINDOWS}TFdSet{$ELSE}FD_SET{$ENDIF};
  rc: Integer;
  AErrorCode: Integer;
const
  InvalidSize = Cardinal(-1);
  MinBufferSize = 4096;
  function ReadSize: Boolean;
  begin
    Result := AReq.ReloadSize(AReaded);
    if Result then
      ATotal := AReq.Size
    else
      ATotal := InvalidSize;
  end;

begin
  AReq := TQMQTTMessage.Create(Self);
  try
    FReconnectTimes := 0;
    ALastLargeIoTick := 0;
    if FSocket = 0 then
      Queue(RecreateSocket);
    repeat
      while (FSocket = 0) do
      begin
        if TSocketRecvThread(TThread.Current).Terminated then
          Exit;
        Sleep(10);
      end;
      AReq.States := [msRecving];
      AReq.Capacity := MinBufferSize;
      AReaded := 0;
      ATotal := Cardinal(-1);

      AErrorCode := 0;
      repeat
        FD_ZERO(fdRead);
        FD_ZERO(fdError);
{$IFDEF MSWINDOWS}
        FD_SET(FSocket, fdRead);
        FD_SET(FSocket, fdError);
{$ELSE}
        __FD_SET(FSocket, fdRead);
        __FD_SET(FSocket, fdError);
{$ENDIF}
        try
          tm.tv_sec := 1;
          tm.tv_usec := 0;
          rc := select(FSocket + 1, @fdRead, nil, @fdError, @tm);
          if (rc > 0) then
          begin
            if FD_ISSET(FSocket, fdRead) and (not FD_ISSET(FSocket, fdError))
            then
            begin
              if UseSSL then
              begin
                if Assigned(FSSL) then
                  ARecv := FSSL.Read(AReq.FData[AReaded],
                    Cardinal(Length(AReq.FData)) - AReaded)
                else
                  Exit;
              end
              else
                ARecv := Recv(FSocket, AReq.FData[AReaded],
                  Cardinal(Length(AReq.FData)) - AReaded, 0);
              if TSocketRecvThread(TThread.Current).Terminated then
                Break;
              if ARecv = -1 then
              begin
                if GetLastError =
{$IFDEF MSWINDOWS}WSAEWOULDBLOCK{$ELSE}EWOULDBLOCK{$ENDIF} then
                begin
                  Sleep(10);
                  continue;
                end
                else
                begin
                  AErrorCode := GetLastError;
                  Break;
                end;
              end
              else if ARecv = 0 then // 没有进一步的数据时，让出CPU
              begin
                Queue(DoPing);
                Sleep(10);
                continue;
              end;
              Inc(AReaded, ARecv);
              FLastIoTick :=
{$IF RTLVersion>=23}TThread.{$IFEND} GetTickCount;
              if AReaded > 4096 then
                ALastLargeIoTick := FLastIoTick;
              if ATotal = InvalidSize then
                ReadSize; // 尝试解析总字节数
              if ATotal <= AReaded then
              begin
                if AReaded >= AReq.Size then
                begin
                  AReq.FRecvTime := FLastIoTick;
                  AReq.States := AReq.States + [msRecved];
                  repeat
                    DoDispatch(AReq^);
                    ATotal := AReq.Size;
                    if AReaded > AReq.Size then
                      Move(AReq.FData[ATotal], AReq.FData[0], AReaded - ATotal);
                    Dec(AReaded, ATotal);
                    if not ReadSize then
                      Break;
                  until AReaded < AReq.Size;
                end;
              end;
            end
            else
            begin
              AErrorCode := GetLastError;
              Break;
            end;
          end
          else if rc < 0 then
          begin
            AErrorCode := GetLastError;
            Break;
          end
          else if rc = 0 then // 超时，检查是否需要Ping
          begin
            ATick := {$IF RTLVersion>=23}TThread.{$IFEND} GetTickCount;
            // 连续5秒读取不到填充够缓冲的数据，则缩小内存占用
            if (AReq.Capacity > MinBufferSize) and (AReaded < MinBufferSize) and
              (ATick - ALastLargeIoTick > 5000) then
            begin
              AReq.PayloadSize := 0;
              AReq.Capacity := MinBufferSize;
            end;
          end;
        except

        end;
      until TSocketRecvThread(TThread.Current).Terminated;
      if (AErrorCode <> 0) and (not TSocketThread(TThread.Current).Terminated)
        and (FSocket <> 0) then
      begin
        DebugOut(SysErrorMessage(AErrorCode));
        if not(csDestroying in ComponentState) then // 未处于析构状态
          TThread.Synchronize(nil, ReconnectNeeded);
      end;
    until TSocketThread(TThread.Current).Terminated;
  finally
    TThread.ForceQueue(nil, DoCloseSocket);
    FreeMessage(AReq);
    FRecvThread := nil;
  end;
end;

function TQMQTTMessageClient.DoSend(AReq: PQMQTTMessage): Boolean;
var
  ASent: Integer;
  p: PByte;
  ASize: Integer;
begin
{$IFDEF LogMqttContent}
  if LogPackageContent then
    PostLog(llDebug, '发送请求 %d(%x),ID=%d,载荷大小:%d,总大小:%d,内容:'#13#10'%s',
      [Ord(AReq.ControlType), IntPtr(AReq), Integer(AReq.TopicId),
      Integer(AReq.PayloadSize), Integer(AReq.Size),
      AReq.ContentAsHexText], 'QMQTT')
  else
    PostLog(llDebug, '发送请求 %d(%x),ID=%d,载荷大小:%d,总大小:%d',
      [Ord(AReq.ControlType), IntPtr(AReq), Integer(AReq.TopicId),
      Integer(AReq.PayloadSize), Integer(AReq.Size)], 'QMQTT');
{$ENDIF}
  try
    if FSocket <> 0 then
    begin
      Result := false;
      DoBeforeSend(AReq);
      try
        p := AReq.Bof;
        ASize := AReq.Size;
        while (ASize > 0) and (not TSocketThread(TThread.Current).Terminated) do
        begin
          if UseSSL then
          begin
            if not Assigned(FSSL) then
              Exit;
            ASent := FSSL.Write(p^, ASize);
            if ASent > 0 then
            begin
              Inc(p, ASent);
              Dec(ASize, ASent);
            end
            else
            begin
              // 暂时不了解SSL这块，待定
            end;
          end
          else
          begin
            ASent := Send(FSocket, p^, ASize, 0);
            if ASent <> -1 then
            begin
              Inc(p, ASent);
              Dec(ASize, ASent);
            end
            else if GetLastError =
{$IFDEF MSWINDOWS} WSAEWOULDBLOCK{$ELSE}EWOULDBLOCK{$ENDIF} then
            begin
              Sleep(10);
              continue
            end
            else
              Break;
          end;
        end;
        if ASize = 0 then
        begin
          AReq.States := AReq.States + [msSent];
          AReq.FSentTime :=
{$IF RTLVersion>=23}TThread.{$IFEND} GetTickCount;
          Inc(AReq.FSentTimes);
          Result := True;
          FLastIoTick := AReq.FSentTime;
        end;
      except
        on E: Exception do
        begin
          DebugOut('发送数据时发生异常:' + E.Message);
        end
      end;
    end;
  finally
    AReq.States := AReq.States - [msSending];
    if Assigned(AReq.FWaitEvent) then
      AReq.FWaitEvent.SetEvent;
    DoAfterSent(AReq);
  end;
end;

procedure TQMQTTMessageClient.DoTimer(AJob: PQJob);
var
  ATick: Cardinal;
  procedure CheckPublishPendings;
  var
    AItem: PQMQTTPublishPendingItem;
  begin
    while Assigned(FPublishPendings.First) do
    begin
      AItem := FPublishPendings.First;
      if ATick - AItem.PushTime >= FPublishTTL then
      begin
        FPublishPendings.First := AItem.Next;
        if not Assigned(AItem.Next) then
          FPublishPendings.Last := nil;
        if Assigned(AItem.Props) then
          FreeAndNil(AItem.Props);
        Dispose(AItem);
      end
      else
        Break;
    end;
  end;

begin
  ATick :=
{$IF RTLVersion>=23}TThread.{$IFEND} GetTickCount;
  CheckPublishPendings;
  if ([qcsConnecting, qcsReconnecting] * FStates) <> [] then // 连接中
  begin
    if (ATick - FLastConnectTime) > (FConnectionTimeout * 1000) then
      RecreateSocket;
    Exit;
  end
  else if (qcsRunning in FStates) then // QoS 1等
  begin
    if ((ATick - FPingTime) >= (FPeekInterval * 1000)) then
      DoPing
    else if (FPingStarted > 0) and ((ATick - FPingStarted) > 1000) then
      // Ping 必需在1秒内返回，如果不返回直接重连
      ReconnectNeeded
    else
      CheckPublished;
  end;
end;

procedure TQMQTTMessageClient.DoTopicPublished(const AMessage: PQMQTTMessage);
var
  ATopic: String;
begin
  if Assigned(FAfterPublished) and Assigned(AMessage) then
  begin
    Queue(
      procedure
      begin
        try
          Inc(FSentTopics);
          if Assigned(FAfterPublished) then
          begin
            AMessage.Position := AMessage.VarHeaderOffset;
            ATopic := AMessage.NextString;
            FAfterPublished(Self, ATopic, AMessage);
          end;
        finally
          FreeMessage(AMessage);
        end;
      end);
  end
  else
    FreeMessage(AMessage);
end;

procedure TQMQTTMessageClient.SetWillMessage(const AValue: TBytes);
begin
  FWillMessage := Copy(AValue, 0, Length(AValue));
end;

procedure TQMQTTMessageClient.Start;
begin
  if UseSSL then
  begin

  end;
  if Length(FServerHost) = 0 then
    raise EMQTTError.Create(SServerHostUnknown);
  if FServerPort = 0 then
    raise EMQTTError.Create(SServerPortInvalid);
  FStates := [qcsConnecting, qcsRunning];
  ClearWaitAcks;
  SetLength(FWaitAcks, 65536);
  if not Assigned(FRecvThread) then
    FRecvThread := TSocketRecvThread.Create(Self);
  if not Assigned(FSendThread) then
    FSendThread := TSocketSendThread.Create(Self);
  Workers.Delay(DoTimer, 1000, nil, True, jdfFreeByUser, True);
end;

procedure TQMQTTMessageClient.Stop;
var
  T: Cardinal;
begin
  FStates := FStates + [qcsStopping] - [qcsRunning];
  FReconnectTimes := 0;
  FReconnectTime := 0;
  Workers.Clear(Self, -1, false);
  if [qcsConnecting, qcsReconnecting] * FStates = [] then
    Disconnect;
  DoCloseSocket;
  if Assigned(FRecvThread) then
  begin
    FRecvThread.Terminate;
    FRecvThread := nil;
  end;
  if Assigned(FSendThread) then
  begin
    FSendThread.Terminate;
    TSocketSendThread(FSendThread).FNotifyEvent.SetEvent;
    FSendThread := nil;
  end;
  T := TThread.GetTickCount;
  while (FThreadCount > 0) and (TThread.GetTickCount - T < 5000) do
    Sleep(10);
  ClearWaitAcks;
  FStates := FStates - [qcsStopping];
end;

procedure TQMQTTMessageClient.Subscribe(const ATopics: array of String;
const AQoS: TQMQTTQoSLevel; AProps: TQMQTT5Props);
var
  AReq: PQMQTTMessage;
  APayloadSize: Integer;
  APayloads: TArray<QStringA>;
  I, C: Integer;
  APackageId: Word;
begin
  for I := Low(ATopics) to High(ATopics) do
  begin
    if Assigned(AProps) then
      FSubscribes.AddObject(IntToStr(Ord(AQoS)) + '|' + ATopics[I], AProps.Copy)
    else
      FSubscribes.AddObject(IntToStr(Ord(AQoS)) + '|' + ATopics[I], nil);
  end;
  if not IsRunning then
    Exit;
  SetLength(APayloads, Length(ATopics));
  APayloadSize := Length(ATopics) * 3 + 2;
  C := 0;
  for I := 0 to High(ATopics) do
  begin
    APayloads[I] := qstring.Utf8Encode(ATopics[I]);
    if APayloads[I].Length > 0 then
    begin
      Inc(APayloadSize, APayloads[I].Length);
      Inc(C);
    end;
  end;
  if C > 0 then
  begin
    if (ProtocolVersion = pv5_0) then
    begin
      if Assigned(AProps) then
        Inc(APayloadSize, AProps.PayloadSize)
      else
        Inc(APayloadSize);
    end;
    AReq := TQMQTTMessage.Create(Self);
    AReq.PayloadSize := APayloadSize;
    AReq.ControlType := TQMQTTControlType.ctSubscribe;
    AReq.QosLevel := TQMQTTQoSLevel.qlAtLeast1;
    AReq.States := AReq.States + [msNeedAck];
    // 报文标志符
    APackageId := AcquirePackageId(AReq, True);
    AReq.Cat(APackageId);
    if (ProtocolVersion = pv5_0) then
    begin
      if Assigned(AProps) then
        AProps.WriteProps(AReq)
      else
        AReq.Cat(Byte(0));
      // EncodeInt
    end;
    for I := 0 to High(APayloads) do
      AReq.Cat(APayloads[I]).Cat(Byte(Ord(AQoS)));
    TSocketSendThread(FSendThread).Post(AReq);
  end;
end;

procedure TQMQTTMessageClient.SubscribePendings;
var
  ALevel, ALastLevel: TQMQTTQoSLevel;
  ATopics: array of String;
  ATopic: String;
  I, C: Integer;
begin
  if FSubscribes.Count > 0 then
  begin
    SetLength(ATopics, FSubscribes.Count);
    ALevel := qlMax1;
    ALastLevel := ALevel;
    C := 0;
    for I := 0 to FSubscribes.Count - 1 do
    begin
      ATopic := FSubscribes[I];
      ALevel := TQMQTTQoSLevel(StrToIntDef(NameOfW(ATopic, '|'), 0));
      if ALevel <> ALastLevel then
      begin
        Subscribe(Copy(ATopics, 0, C), ALastLevel);
        C := 0;
        ALastLevel := ALevel;
      end;
      ATopics[C] := ValueOfW(ATopic, '|');
      Inc(C);
    end;
    if C > 0 then
      Subscribe(Copy(ATopics, 0, C), ALastLevel);
  end;
end;

function TQMQTTMessageClient.CompareTopic(const ATopic1,
  ATopic2: String): Integer;
// 需要实现一个主题的比较算法，1和2都包含模式匹配符时，要确定
  function IsContains(p1, p2: PChar): Boolean;
  begin
    Result := false;
  end;

begin
  if ATopic1 = ATopic2 then
    Result := 0
  else if IsContains(PChar(ATopic1), PChar(ATopic2)) then
    Result := 1
  else
    Result := -1;
end;

procedure TQMQTTMessageClient.Unlock;
begin
  FLocker.Leave;
end;

procedure TQMQTTMessageClient.UnregisterDispatch
  (AHandler: TQMQTTTopicDispatchEvent);
var
  AItem, APrior, ANext: TTopicHandler;
  I: Integer;
begin
  I := 0;
  while I < FTopicHandlers.Count do
  begin
    AItem := TTopicHandler(FTopicHandlers.Objects[I]);
    APrior := nil;
    while Assigned(AItem) do
    begin
      if MethodEqual(TMethod(AItem.FOnDispatch), TMethod(AHandler)) then
      begin
        if Assigned(APrior) then
          APrior.FNext := AItem.FNext
        else
          FTopicHandlers.Objects[I] := AItem.FNext;
        ANext := AItem.FNext;
        FreeAndNil(AItem);
        AItem := ANext;
      end
      else
        AItem := AItem.FNext;
    end;
    if FTopicHandlers.Objects[I] = nil then
      FTopicHandlers.Delete(I)
    else
      Inc(I);
  end;
end;

procedure TQMQTTMessageClient.Unsubscribe(const ATopics: array of String);
var
  AReq: PQMQTTMessage;
  APayloadSize: Integer;
  AUtf8Topics: TArray<QStringA>;
  I, C: Integer;
  APackageId: Word;
begin
  RemoveSubscribePendings(ATopics);
  if not IsRunning then
    Exit;
  if Length(ATopics) > 0 then
  begin
    SetLength(AUtf8Topics, Length(ATopics));
    C := 0;
    APayloadSize := 2;
    for I := 0 to High(ATopics) do
    begin
      if Length(ATopics[I]) > 0 then
      begin
        AUtf8Topics[C] := qstring.Utf8Encode(ATopics[I]);
        Inc(APayloadSize, AUtf8Topics[C].Length + 2);
        Inc(C);
      end;
    end;
    if C = 0 then
      // 没有要取消的订阅，退出
      Exit;
    AReq := TQMQTTMessage.Create(Self);
    AReq.PayloadSize := APayloadSize;
    AReq.ControlType := TQMQTTControlType.ctUnsubscribe;
    AReq.QosLevel := TQMQTTQoSLevel.qlAtLeast1;
    AReq.States := AReq.States + [msNeedAck];
    APackageId := AcquirePackageId(AReq, True);
    AReq.Cat(APackageId);
    for I := 0 to C - 1 do
      AReq.Cat(AUtf8Topics[I]);
    if Assigned(BeforeUnsubscribe) then
    begin
      for I := 0 to High(ATopics) do
      begin
        if Length(ATopics[I]) > 0 then
          BeforeUnsubscribe(Self, ATopics[I], AReq);
      end;
    end;
    TSocketSendThread(FSendThread).Post(AReq);
  end;
end;

procedure TQMQTTMessageClient.ValidClientId;
var
  AId: TGuid;
begin
  if Length(FClientId) = 0 then
  begin
    CreateGUID(AId);
    FClientId := DeleteRightW(TNetEncoding.Base64.EncodeBytesToString(@AId,
      SizeOf(AId)), '=', false, 1);
  end;
end;

{ TQMQTTMessage }

function TQMQTTMessage.Cat(

  const V: Cardinal; AEncode: Boolean): PQMQTTMessage;
begin
  Result := @Self;
  if AEncode then
    EncodeInt(FCurrent, V)
  else
  begin
    PCardinal(FCurrent)^ := ExchangeByteOrder(V);
    Inc(FCurrent, SizeOf(V));
  end;
end;

function TQMQTTMessage.Cat(

  const V: Shortint): PQMQTTMessage;
begin
  Result := @Self;
  PShortint(FCurrent)^ := V;
  Inc(FCurrent, SizeOf(V));
end;

function TQMQTTMessage.Cat(

  const V: Smallint; AEncode: Boolean): PQMQTTMessage;
begin
  Result := @Self;
  if AEncode then
    EncodeInt(FCurrent, Word(V))
  else
  begin
    PSmallint(FCurrent)^ := ExchangeByteOrder(V);
    Inc(FCurrent, SizeOf(V));
  end;
end;

function TQMQTTMessage.Cat(

  const S: QStringW; AWriteZeroLen: Boolean): PQMQTTMessage;
var
  T: QStringA;
begin
  Result := @Self;
  if (Length(S) > 0) or AWriteZeroLen then
  begin
    T := qstring.Utf8Encode(S);
    Cat(Word(T.Length)).Cat(PQCharA(S), T.Length);
  end;
end;

function TQMQTTMessage.Cat(

  const V: Byte): PQMQTTMessage;
begin
  Result := @Self;
  FCurrent^ := V;
  Inc(FCurrent);
  Assert(FCurrent <= Eof);
end;

function TQMQTTMessage.Cat(

  const V: Word; AEncode: Boolean): PQMQTTMessage;
begin
  Result := @Self;
  if AEncode then
    EncodeInt(FCurrent, V)
  else
  begin
    PWord(FCurrent)^ := ExchangeByteOrder(V);
    Inc(FCurrent, SizeOf(V));
  end;
  Assert(FCurrent <= Eof);
end;

function TQMQTTMessage.Cat(

  const V: Double; AEncode: Boolean): PQMQTTMessage;
var
  T: UInt64 absolute V;
begin
  Result := Cat(T, AEncode);
end;

function TQMQTTMessage.Copy: PQMQTTMessage;
begin
  Result := Create(Client);
  Result.PayloadSize := PayloadSize;
  Move(FData[0], Result.FData[0], FSize);
  Result.FStates := FStates;
  Result.FRecvTime := FRecvTime;
  Result.FSentTime := FSentTime;
  Result.FSentTimes := FSentTimes;
end;

class function TQMQTTMessage.Create(AClient: TQMQTTMessageClient)
  : PQMQTTMessage;
begin
  New(Result);
  Result.FClient := AClient;
  Result.FAfterSent := nil;
  Result.FNext := nil;
  Result.FCurrent := nil;
  Result.FVarHeader := nil;
  Result.FStates := [];
  Result.FRecvTime := 0;
  Result.FSentTime := 0;
  Result.FSentTimes := 0;
  Result.FSize := 0;
  Result.FWaitEvent := nil;
end;

function TQMQTTMessage.Cat(const S: QStringA; AWriteZeroLen: Boolean)
  : PQMQTTMessage;
var
  T: QStringA;
begin
  Result := @Self;
  if (S.Length > 0) or AWriteZeroLen then
  begin
    if S.IsUtf8 then
      Cat(Word(S.Length)).Cat(PQCharA(S), S.Length)
    else
    begin
      T := qstring.Utf8Encode(AnsiDecode(S));
      Cat(Word(T.Length)).Cat(PQCharA(T), T.Length)
    end;
    Assert(FCurrent <= Eof);
  end;
end;

function TQMQTTMessage.Cat(

  const ABytes: TBytes): PQMQTTMessage;
begin
  if Length(ABytes) > 0 then
    Result := Cat(@ABytes[0], Length(ABytes))
  else
    Result := @Self;
end;

function TQMQTTMessage.Cat(

  const V: Int64; AEncode: Boolean): PQMQTTMessage;
begin
  Result := Cat(UInt64(V), AEncode);
end;

function TQMQTTMessage.Cat(

  const V: UInt64; AEncode: Boolean): PQMQTTMessage;
begin
  Result := @Self;
  if AEncode then
    EncodeInt64(FCurrent, V)
  else
  begin
    PInt64(FCurrent)^ := ExchangeByteOrder(Int64(V));
    Inc(FCurrent, SizeOf(V));
  end;
  Assert(FCurrent <= Eof);
end;

function TQMQTTMessage.Cat(

  const ABuf: Pointer;

const ALen: Cardinal): PQMQTTMessage;
begin
  if ALen > 0 then
  begin
    Assert((FCurrent >= Bof) and (FCurrent + ALen <= Eof));
    Move(ABuf^, FCurrent^, ALen);
    Inc(FCurrent, ALen);
    Assert(FCurrent <= Eof);
  end;
  Result := @Self;
end;

function TQMQTTMessage.DecodeInt(

  var ABuf: PByte; AMaxCount: Integer;

var AResult: Cardinal): Boolean;
var
  C: Integer;
  AStart: PByte;
begin
  Result := false;
  C := 0;
  AResult := 0;
  AStart := ABuf;
  if AMaxCount < 0 then
  begin
    if (ABuf >= AStart) and (ABuf <= Eof) then
      AMaxCount := Integer(FSize) - (IntPtr(ABuf) - IntPtr(AStart))
    else
      AMaxCount := 4;
  end
  else if AMaxCount > 4 then
    AMaxCount := 4;
  while ABuf - AStart < AMaxCount do
  begin
    if (ABuf^ and $80) <> 0 then
    begin
      AResult := AResult + ((ABuf^ and $7F) shl (C * 7));
      Inc(ABuf);
    end
    else
    begin
      Inc(AResult, ABuf^ shl (C * 7));
      Inc(ABuf);
      Exit(True);
    end;
    Inc(C);
  end;
  // 走到这里，说明格式无效
  AResult := 0;
end;

function TQMQTTMessage.DecodeInt64(var ABuf: PByte; AMaxCount: Integer;

var AResult: Int64): Boolean;
var
  C: Integer;
  AStart: PByte;
begin
  Result := false;
  C := 0;
  AStart := ABuf;
  AResult := 0;
  if AMaxCount < 0 then
  begin
    if (ABuf >= AStart) and (ABuf <= Eof) then
      AMaxCount := Integer(FSize) - (IntPtr(ABuf) - IntPtr(AStart))
    else
      AMaxCount := 8;
  end
  else if AMaxCount > 8 then
    AMaxCount := 8;
  while ABuf - AStart < AMaxCount do
  begin
    if (ABuf^ and $80) <> 0 then
    begin
      AResult := AResult + ((ABuf^ and $7F) shl (C * 7));
      Inc(ABuf);
    end
    else
    begin
      Inc(AResult, ABuf^ shl (C * 7));
      Inc(ABuf);
      Exit(True);
    end;
    Inc(C);
  end;
  // 走到这里，说明格式无效
  AResult := 0;
end;

procedure TQMQTTMessage.EncodeInt(var ABuf: PByte; V: Cardinal);
begin
  repeat
    ABuf^ := V and $7F;
    V := V shr 7;
    if V > 0 then
      ABuf^ := ABuf^ or $80;
    Inc(ABuf);
  until V = 0;
end;

procedure TQMQTTMessage.EncodeInt64(var ABuf: PByte; V: UInt64);
begin
  repeat
    ABuf^ := V and $7F;
    V := V shr 7;
    if V > 0 then
      ABuf^ := ABuf^ or $80;
    Inc(ABuf);
  until V = 0;
end;

function TQMQTTMessage.Cat(const V: Single; AEncode: Boolean): PQMQTTMessage;
var
  T: Cardinal absolute V;
begin
  Result := @Self;
  if AEncode then
    EncodeInt(FCurrent, T)
  else
  begin
    PCardinal(FCurrent)^ := ExchangeByteOrder(T);
    Inc(FCurrent, SizeOf(V));
  end;
  Assert(FCurrent <= Eof);
end;

function TQMQTTMessage.Cat(const V: Integer; AEncode: Boolean): PQMQTTMessage;
var
  T: Cardinal absolute V;
begin
  Result := @Self;
  if AEncode then
    EncodeInt(FCurrent, T)
  else
  begin
    PInteger(FCurrent)^ := ExchangeByteOrder(V);
    Inc(FCurrent, SizeOf(V));
  end;
  Assert(FCurrent <= Eof);
end;

function TQMQTTMessage.GetBof: PByte;
begin
  Result := @FData[0];
end;

function TQMQTTMessage.GetByte(const AIndex: Integer): Byte;
begin
  Result := FData[AIndex];
end;

function TQMQTTMessage.GetCapacity: Cardinal;
begin
  Result := Length(FData);
end;

function TQMQTTMessage.GetControlType: TQMQTTControlType;
begin
  Assert(Length(FData) > 1);
  Result := TQMQTTControlType((FData[0] and $F0) shr 4);
end;

function TQMQTTMessage.GetEof: PByte;
begin
  Result := @FData[Length(FData)];
end;

function TQMQTTMessage.GetHeaderFlags(const Index: Integer): Boolean;
const
  AMasks: array [0 .. 1] of Byte = (1, 8);
begin
  Result := (FData[0] and AMasks[Index]) <> 0;
end;

function TQMQTTMessage.GetPackageId: Word;
begin
  Result := 0;
  case ControlType of
    ctPublish: // 发布
      begin
        if QosLevel > qlMax1 then
          // 可变头+主题长度+主题内容+PackageId
          Result := ExchangeByteOrder
            (PWord(IntPtr(FVarHeader) + SizeOf(Word) +
            ExchangeByteOrder(PWord(FVarHeader)^))^);
      end;
    ctPublishAck, ctPublishRecv, ctPublishRelease, ctPublishDone, ctSubscribe,
      ctSubscribeAck, ctUnsubscribe, ctUnsubscribeAck:
      Result := ExchangeByteOrder(PWord(FVarHeader)^);
  end;
end;

function TQMQTTMessage.GetPayloadSize: Cardinal;
var
  p: PByte;
begin
  if Length(FData) > 1 then
  begin
    p := @FData[1];
    DecodeInt(p, Size - 1, Result);
  end
  else
    Result := 0;
end;

function TQMQTTMessage.GetPosition: Integer;
begin
  Result := IntPtr(FCurrent) - IntPtr(@FData[0]);
end;

function TQMQTTMessage.GetQoSLevel: TQMQTTQoSLevel;
begin
  Result := TQMQTTQoSLevel((FData[0] shr 1) and 3);
end;

function TQMQTTMessage.GetRemainSize: Integer;
begin
  Result := FSize - Cardinal(Position);
end;

function TQMQTTMessage.GetTopicContent: TBytes;
var
  p: PByte;
begin
  if ControlType = ctPublish then
  begin
    p := TopicOriginContent;
    SetLength(Result, Length(FData) - (IntPtr(p) - IntPtr(@FData[0])));
    Move(p^, Result[0], Length(Result));
  end
  else
    SetLength(Result, 0);
end;

function TQMQTTMessage.GetTopicContentSize: Integer;
begin
  if ControlType = ctPublish then
    Result := Length(FData) - (IntPtr(TopicOriginContent) - IntPtr(@FData[0]))
  else
    Result := 0;
end;

function TQMQTTMessage.GetTopicId: Word;
var
  p: PByte;
begin
  case ControlType of
    ctPublish:
      begin
        p := FVarHeader;
        Inc(p, ExchangeByteOrder(PWord(p)^) + 2);
        if QosLevel > qlMax1 then
          // 跳过可能存在的PackageId
          Result := ExchangeByteOrder(PWord(p)^)
        else
          Result := 0;
      end;
    ctPublishAck:
      Result := ExchangeByteOrder(PWord(FVarHeader)^);
    ctPublishRecv, ctPublishRelease, ctPublishDone, ctSubscribe:
      Result := ExchangeByteOrder(PWord(FVarHeader)^)
  else
    Result := 0;
  end;
end;

function TQMQTTMessage.GetTopicName: String;
var
  p: PByte;
  ASize: Word;
begin
  if ControlType = ctPublish then
  begin
    p := FVarHeader;
    ASize := ExchangeByteOrder(PWord(p)^);
    Inc(p, 2);
    Result := qstring.Utf8Decode(PQCharA(p), ASize);
  end
  else
    Result := '';
end;

function TQMQTTMessage.GetTopicOriginContent: PByte;
begin
  if ControlType = ctPublish then
  begin
    Result := FVarHeader;
    Inc(Result, ExchangeByteOrder(PWord(Result)^) + 2);
    if QosLevel > qlMax1 then
      // 跳过可能存在的PackageId
      Inc(Result, 2);
  end
  else
    Result := nil;
end;

function TQMQTTMessage.GetTopicText: String;
var
  p: PByte;
  L: Integer;
begin
  if ControlType = ctPublish then
  begin
    p := FVarHeader;
    Inc(p, ExchangeByteOrder(PWord(p)^) + 2);
    if QosLevel > qlMax1 then
      // 跳过可能存在的PackageId
      Inc(p, 2);
    L := Length(FData) - (IntPtr(p) - IntPtr(@FData[0]));
    if L > 0 then
      Result := qstring.Utf8Decode(PQCharA(p), L)
    else
      Result := '';
  end
  else
    Result := '';
end;

function TQMQTTMessage.GetVarHeaderOffset: Integer;
begin
  Result := IntPtr(FVarHeader) - IntPtr(Bof);
end;

class function TQMQTTMessage.IntEncodedSize(const V: Cardinal): Byte;
begin
  Result := 1;
  if V >= 128 then
  begin
    Inc(Result);
    if V >= 16384 then
    begin
      Inc(Result);
      if V >= 2097152 then
        Inc(Result)
      else if V > 268435455 then
        raise Exception.CreateFmt(STooLargePayload, [V]);
    end;
  end;
end;

function TQMQTTMessage.MoveBy(const ADelta: Integer): PQMQTTMessage;
begin
  Inc(FCurrent, ADelta);
  if FCurrent < Bof then
    FCurrent := Bof
  else if FCurrent > Eof then
    FCurrent := Eof;
  Result := @Self;
end;

procedure TQMQTTMessage.MoveToVarHeader;
begin
  FCurrent := Bof + VarHeaderOffset;
end;

function TQMQTTMessage.NextByte: Byte;
begin
  Result := FCurrent^;
  Inc(FCurrent);
  Assert(FCurrent <= Eof);
end;

function TQMQTTMessage.NextBytes(ABuf: Pointer; ALen: Integer): Integer;
begin
  Result := Length(FData) - (IntPtr(FCurrent) - IntPtr(@FData[0]));
  if Result > ALen then
    Result := ALen;
  Move(FCurrent^, ABuf^, Result);
  Inc(FCurrent, Result);
  Assert(FCurrent <= Eof);
end;

function TQMQTTMessage.NextBytes(ASize: Integer): TBytes;
begin
  SetLength(Result, ASize);
  if ASize > 0 then
  begin
    Move(FCurrent^, Result[0], ASize);
    Assert(FCurrent <= Eof);
  end;
end;

function TQMQTTMessage.NextDWord(AIsEncoded: Boolean): Cardinal;
begin
  if AIsEncoded then
    Assert(DecodeInt(FCurrent, -1, Result))
  else
  begin
    Result := ExchangeByteOrder(PCardinal(FCurrent)^);
    Inc(FCurrent, SizeOf(Result));
  end;
  Assert(FCurrent <= Eof);
end;

function TQMQTTMessage.NextFloat(AIsEncoded: Boolean): Double;
var
  T: Int64 absolute Result;
begin
  if AIsEncoded then
    Assert(DecodeInt64(FCurrent, -1, T))
  else
  begin
    Result := ExchangeByteOrder(PCardinal(FCurrent)^);
    Inc(FCurrent, SizeOf(Result));
  end;
  Assert(FCurrent <= Eof);
end;

function TQMQTTMessage.NextInt(AIsEncoded: Boolean): Integer;
begin
  if AIsEncoded then
    Assert(DecodeInt(FCurrent, -1, Cardinal(Result)))
  else
  begin
    Result := ExchangeByteOrder(PInteger(FCurrent)^);
    Inc(FCurrent, SizeOf(Result));
  end;
  Assert(FCurrent <= Eof);
end;

function TQMQTTMessage.NextInt64(AIsEncoded: Boolean): Int64;
begin
  if AIsEncoded then
    Assert(DecodeInt64(FCurrent, -1, Result))
  else
  begin
    Result := ExchangeByteOrder(PInt64(FCurrent)^);
    Inc(FCurrent, SizeOf(Result));
  end;
  Assert(FCurrent <= Eof);
end;

function TQMQTTMessage.NextSingle(AIsEncoded: Boolean): Single;
var
  T: Cardinal absolute Result;
begin
  if AIsEncoded then
    Assert(DecodeInt(FCurrent, -1, T))
  else
  begin
    T := ExchangeByteOrder(PInteger(FCurrent)^);
    Inc(FCurrent, SizeOf(Result));
  end;
  Assert(FCurrent <= Eof);
end;

function TQMQTTMessage.NextSmallInt(AIsEncoded: Boolean): Smallint;
begin
  if AIsEncoded then
    Result := NextInt(True)
  else
  begin
    Result := ExchangeByteOrder(PSmallint(FCurrent)^);
    Inc(FCurrent, SizeOf(Result));
  end;
  Assert(FCurrent <= Eof);
end;

function TQMQTTMessage.NextString: String;
var
  ALen, ARemain: Integer;
begin
  ALen := NextWord(false);
  ARemain := RemainSize;
  if ALen > ARemain then
    ALen := ARemain;
  Result := qstring.Utf8Decode(PQCharA(Current), ALen);
  Inc(FCurrent, ALen);
  Assert(FCurrent <= Eof);
end;

function TQMQTTMessage.NextTinyInt: Shortint;
begin
  Result := FCurrent^;
  Inc(FCurrent);
  Assert(FCurrent <= Eof);
end;

function TQMQTTMessage.NextUInt64(AIsEncoded: Boolean): UInt64;
begin
  if AIsEncoded then
    Assert(DecodeInt64(FCurrent, -1, Int64(Result)))
  else
  begin
    Result := UInt64(ExchangeByteOrder(PInt64(FCurrent)^));
    Inc(FCurrent, SizeOf(Result));
  end;
  Assert(FCurrent <= Eof);
end;

function TQMQTTMessage.NextWord(AIsEncoded: Boolean): Word;
begin
  if AIsEncoded then
    Result := Word(NextInt(True))
  else
  begin
    Result := ExchangeByteOrder(PWord(FCurrent)^);
    Inc(FCurrent, SizeOf(Result));
  end;
  Assert(FCurrent <= Eof);
end;

function TQMQTTMessage.ReloadSize(AKnownBytes: Integer): Boolean;
var
  AVarOffset: Integer;
begin
  if AKnownBytes > 1 then
  begin
    FCurrent := @FData[1];
    Result := DecodeInt(FCurrent, AKnownBytes - 1, FSize);
    if Result then
    begin
      AVarOffset := Position;
      Inc(FSize, Position);
      if Cardinal(Length(FData)) < FSize then
      begin
        SetLength(FData, (FSize + 15) and $FFFFFFF0);
        FCurrent := Bof + AVarOffset;
      end;
      FVarHeader := FCurrent;
    end;
  end
  else
    Result := false;
end;

procedure TQMQTTMessage.SetByte(const AIndex: Integer;

const Value: Byte);
begin
  Assert((AIndex >= 0) and (AIndex < Length(FData)));
  FData[AIndex] := Value;
end;

procedure TQMQTTMessage.SetCapacity(

  const Value: Cardinal);
begin
  if Value > FSize then
  begin
    if (Value and $F) <> 0 then
      SetLength(FData, (Value and $FFFFFFF0) + 16)
    else
      SetLength(FData, Value);
  end;
end;

procedure TQMQTTMessage.SetControlType(

  const AType: TQMQTTControlType);
begin
  Assert(Length(FData) > 0);
  FData[0] := (FData[0] and $0F) or (Ord(AType) shl 4);
end;

procedure TQMQTTMessage.SetHeaderFlags(

  const Index: Integer;

const Value: Boolean);
const
  AMasks: array [0 .. 1] of Byte = (1, 8);
begin
  Assert(Length(FData) > 1);
  if Value then
    FData[0] := FData[0] or AMasks[Index]
  else
    FData[0] := FData[0] and (not AMasks[Index]);
end;

procedure TQMQTTMessage.SetPayloadSize(Value: Cardinal);
var
  AReqSize: Integer;
begin
  AReqSize := SizeOf(Byte) + Value + IntEncodedSize(Value);
  if Length(FData) < AReqSize then
    SetLength(FData, AReqSize);
  FSize := AReqSize;
  FCurrent := @FData[1];
  EncodeInt(FCurrent, Value);
  FVarHeader := FCurrent;
  Assert(FCurrent <= Eof);
end;

procedure TQMQTTMessage.SetPosition(

  const Value: Integer);
begin
  FCurrent := Bof;
  Inc(FCurrent, Value);
  Assert(FCurrent <= Eof);
  if FCurrent < Bof then
    FCurrent := Bof
  else if FCurrent > Eof then
    FCurrent := Eof;
end;

procedure TQMQTTMessage.SetQosLevel(ALevel: TQMQTTQoSLevel);
begin
  Assert(Length(FData) > 0);
  FData[0] := (FData[0] and $F9) or (Ord(ALevel) shl 1);
end;

function TQMQTTMessage.ContentAsHexText: String;
var
  ABuilder: TQStringCatHelperW;
  C, R, L, ARows, ACols: Integer;
  B: Byte;
  T: array [0 .. 15] of Char;
begin
  ABuilder := TQStringCatHelperW.Create;
  try
    L := Length(FData);
    ARows := (L shr 4);
    if (L and $F) <> 0 then
      Inc(ARows);
    ABuilder.Cat('     ');
    for C := 0 to 15 do
      ABuilder.Cat(IntToHex(C, 2)).Cat(' ');
    ABuilder.Cat(SLineBreak);
    for R := 0 to ARows - 1 do
    begin
      ABuilder.Cat(IntToHex(R, 4)).Cat(' ');
      ACols := L - (R shl 4);
      if ACols > 16 then
        ACols := 16;
      for C := 0 to ACols - 1 do
      begin
        B := FData[R * 16 + C];
        Result := Result + IntToHex(B, 2) + ' ';
        if (B >= $20) and (B <= $7E) then
          T[C] := Char(B)
        else
          T[C] := '.';
      end;
      for C := ACols to 15 do
        T[C] := ' ';
      ABuilder.Cat(@T[0], 16).Cat(SLineBreak);
    end;
    Result := ABuilder.Value;
  finally
    FreeAndNil(ABuilder);
  end;
end;

{ TSocketRecvThread }

constructor TSocketRecvThread.Create(AOwner: TQMQTTMessageClient);
begin
  inherited;
  Suspended := false;
end;

procedure TSocketRecvThread.Execute;
begin
  try
    FOwner.DoRecv;
  except
    on E: Exception do

  end;
  if AtomicDecrement(FOwner.FThreadCount) = 0 then
    FOwner.DoCleanup;
end;

{$IFDEF MSWINDOWS}

procedure StartSocket;
var
  AData: WSAData;
begin
  if WSAStartup($202, &AData) <> NO_ERROR then
    RaiseLastOSError();
end;

procedure CleanSocket;
begin
  WSACleanup;
end;
{$ELSE}

procedure StartSocket; inline;
begin
end;

procedure CleanSocket; inline;
begin
end;
{$ENDIF}
{ TTopicHandler }

constructor TTopicHandler.Create(

  const ATopic: String; AHandler: TQMQTTTopicDispatchEvent;
AMatchType: TTopicMatchType);
begin
  inherited Create;
  FMatchType := AMatchType;
  FTopic := ATopic;
  if AMatchType = TTopicMatchType.mtRegex then
  begin
    FRegex := TPerlRegex.Create;
    try
      FRegex.RegEx := ATopic;
      FRegex.Compile;
    except
      FreeAndNil(FRegex);
      raise;
    end;
  end;
  FOnDispatch := AHandler;
end;

destructor TTopicHandler.Destroy;
begin
  if Assigned(FRegex) then
    FreeAndNil(FRegex);
  if TMethod(FOnDispatch).Data = Pointer(-1) then
    TQMQTTTopicDispatchEventA(TMethod(FOnDispatch).Code) := nil;
  inherited;
end;

function TTopicHandler.IsMatch(

  const ATopic: String): Boolean;
  function PatternMatch: Boolean;
  var
    ps, pd: PChar;
    ARegex: String;
  begin
    // 先不实现具体的算法，转换为正则匹配
    if not Assigned(FRegex) then
    begin
      FRegex := TPerlRegex.Create;
      SetLength(ARegex, Length(FTopic) shl 2);
      ps := PChar(FTopic);
      pd := PChar(ARegex);
      while ps^ <> #0 do
      begin
        if ps^ = '#' then
        begin
          pd[0] := '.';
          pd[1] := '*';
          Inc(pd, 2);
        end
        else if ps^ = '+' then
        begin
          pd[0] := '[';
          pd[1] := '^';
          pd[2] := '/';
          pd[3] := ']';
          pd[4] := '+';
          Inc(pd, 5);
        end
        else if ps^ = '$' then
        begin
          pd^ := '.';
          Inc(pd);
        end
        else
        begin
          pd^ := ps^;
          Inc(pd);
        end;
        Inc(ps);
      end;
      pd^ := #0;
      FRegex.RegEx := PChar(ARegex);
      FRegex.Compile;
    end;
    FRegex.Subject := ATopic;
    Result := FRegex.Match;
  end;

begin
  if FMatchType = mtRegex then
  begin
    FRegex.Subject := ATopic;
    Result := FRegex.Match;
  end
  else if FMatchType = mtPattern then
    Result := PatternMatch
  else
    Result := True;
end;

{ TSocketThread }

constructor TSocketThread.Create(AOwner: TQMQTTMessageClient);
begin
  inherited Create(True);
  FOwner := AOwner;
  FreeOnTerminate := True;
  AtomicIncrement(AOwner.FThreadCount);
end;

{ TSocketSendThread }

procedure TSocketSendThread.Clear;
var
  AFirst, ANext: PQMQTTMessage;
begin
  FOwner.Lock;
  try
    AFirst := FFirst;
    FFirst := nil;
    FLast := nil;
  finally
    FOwner.Unlock;
  end;
  while Assigned(AFirst) do
  begin
    ANext := AFirst.Next;
    FOwner.FreeMessage(AFirst);
    AFirst := ANext;
  end;
end;

constructor TSocketSendThread.Create(AOwner: TQMQTTMessageClient);
begin
  inherited;
  FNotifyEvent := TEvent.Create(nil, false, false, '');
  Suspended := false;
end;

destructor TSocketSendThread.Destroy;
begin
  FreeAndNil(FNotifyEvent);
  inherited;
end;

procedure TSocketSendThread.Execute;
var
  AFirst, ANext: PQMQTTMessage;
begin
  try
    AFirst := nil;
    while not Terminated do
    begin
      if FNotifyEvent.WaitFor = wrSignaled then
      begin
        if Terminated then
          Break;
        if not Assigned(AFirst) then
        begin
          // 全部弹出
          FOwner.Lock;
          try
            AFirst := FFirst;
            FFirst := nil;
            FLast := nil;
          finally
            FOwner.Unlock;
          end;
        end;
        while Assigned(AFirst) do
        begin
          ANext := AFirst.Next;
          FOwner.DoSend(AFirst);
          AFirst := ANext
        end;
        Queue(FOwner.PublishFirstPending);
      end;
    end;
  finally
    Clear;
    if AtomicDecrement(FOwner.FThreadCount) = 0 then
      FOwner.DoCleanup;
  end;
end;

procedure TSocketSendThread.Post(AMessage: PQMQTTMessage);
begin
  FOwner.Lock;
  try
    if Assigned(FLast) then
      FLast.Next := AMessage;
    if not Assigned(FFirst) then
      FFirst := AMessage;
    FLast := AMessage;
  finally
    FOwner.Unlock;
    FNotifyEvent.SetEvent;
  end;
end;

function TSocketSendThread.Send(AMessage: PQMQTTMessage;
ATimeout: Cardinal): Boolean;
var
  AEvent: TEvent;
begin
  AEvent := TEvent.Create(nil, false, false, '');
  try
    AMessage.FWaitEvent := AEvent;
    Post(AMessage);
    Result := AEvent.WaitFor(ATimeout) = wrSignaled;
  finally
    FreeAndNil(AEvent);
  end;
end;

{ TQMQTT5Props }

function TQMQTT5Props.Copy: TQMQTT5Props;
begin
  Result := TQMQTT5Props.Create;
  Result.Replace(Self);
end;

constructor TQMQTT5Props.Create;
begin
  SetLength(FItems, 42);
end;

destructor TQMQTT5Props.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FItems) do
  begin
    if (MQTT5PropTypes[I].DataType in [ptString, ptBinary]) and
      Assigned(FItems[I].AsString) then
    begin
      if MQTT5PropTypes[I].DataType = ptString then
        Dispose(FItems[I].AsString)
      else
        Dispose(FItems[I].AsBytes);
    end;
  end;
  inherited;
end;

function TQMQTT5Props.GetAsInt(const APropId: Byte): Cardinal;
begin
  case MQTT5PropTypes[APropId - 1].DataType of
    ptByte:
      Result := FItems[APropId - 1].AsByte;
    ptWord:
      Result := FItems[APropId - 1].AsWord;
    ptInt, ptVarInt:
      Result := FItems[APropId - 1].AsInteger;
    ptString:
      begin
        if Assigned(FItems[APropId - 1].AsString) then
          Result := StrToInt(FItems[APropId - 1].AsString^)
        else
          raise Exception.Create('不支持的类型转换');
      end
  else
    raise Exception.Create('不支持的类型转换');
  end;
end;

function TQMQTT5Props.GetAsString(const APropId: Byte): String;
begin
  case MQTT5PropTypes[APropId - 1].DataType of
    ptByte:
      Result := IntToStr(FItems[APropId - 1].AsByte);
    ptWord:
      Result := IntToStr(FItems[APropId - 1].AsWord);
    ptInt, ptVarInt:
      Result := IntToStr(FItems[APropId - 1].AsInteger);
    ptString:
      begin
        if Assigned(FItems[APropId - 1].AsString) then
          Result := FItems[APropId - 1].AsString^
        else
          SetLength(Result, 0);
      end;
    ptBinary:
      begin
        if Assigned(FItems[APropId - 1].AsBytes) then
          Result := BinToHex(FItems[APropId - 1].AsBytes^)
        else
          SetLength(Result, 0);
      end
  else
    raise Exception.Create('不支持的类型转换');
  end;
end;

function TQMQTT5Props.GetAsVariant(const APropId: Byte): Variant;
var
  p: PByte;
begin
  case MQTT5PropTypes[APropId - 1].DataType of
    ptByte:
      Result := FItems[APropId - 1].AsByte;
    ptWord:
      Result := FItems[APropId - 1].AsWord;
    ptInt, ptVarInt:
      Result := FItems[APropId - 1].AsInteger;
    ptString:
      begin
        if Assigned(FItems[APropId - 1].AsString) then
          Result := Utf8Decode(FItems[APropId - 1].AsString^)
        else
          Result := '';
      end;
    ptBinary:
      begin
        if Assigned(FItems[APropId - 1].AsBytes) then
        begin
          Result := VarArrayCreate([0, Length(FItems[APropId - 1].AsBytes^) -
            1], varByte);
          p := VarArrayLock(Result);
          Move(FItems[APropId - 1].AsBytes^[0], p^,
            Length(FItems[APropId - 1].AsBytes^));
          VarArrayUnlock(Result);
        end
        else
          Result := Null;
      end
  else
    raise Exception.Create('不支持的类型转换');
  end;
end;

function TQMQTT5Props.GetDataSize(const APropId: Byte): Integer;
var
  AType: PQMQTT5PropType;
begin
  AType := @MQTT5PropTypes[APropId - 1];
  Result := AType.DataSize;
  if Result < 0 then
  begin
    if AType.DataType = ptVarInt then
      Result := TQMQTTMessage.IntEncodedSize(AsInt[APropId])
    else if AType.DataType = ptString then
      Result := FItems[APropId - 1].AsString^.Length
    else if AType.DataType = ptBinary then
      Result := Length(FItems[APropId - 1].AsBytes^);
  end;
end;

function TQMQTT5Props.GetIsSet(const APropId: Byte): Boolean;
begin
  Result := FItems[APropId - 1].IsSet;
end;

function TQMQTT5Props.GetMinMaxId(const Index: Integer): Byte;
begin
  if Index = 0 then
    Result := 1
  else
    Result := Length(FItems);
end;

function TQMQTT5Props.GetPayloadSize: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(FItems) do
  begin
    if FItems[I].IsSet then
      Inc(Result, DataSize[I + 1] + 1);
  end;
  Inc(Result, TQMQTTMessage.IntEncodedSize(Result));
end;

function TQMQTT5Props.GetPropTypes(const APropId: Byte): PQMQTT5PropType;
begin
  Result := @MQTT5PropTypes[APropId - 1];
end;

procedure TQMQTT5Props.ReadProps(const AMessage: PQMQTTMessage);
var
  APropLen: Cardinal;
  pe: PByte;
  I: Integer;
  APropId: Byte;
begin
  APropLen := AMessage.NextDWord(True);
  pe := AMessage.Current + APropLen;
  for I := 0 to High(FItems) do
    FItems[I].IsSet := false;
  while AMessage.Current < pe do
  begin
    APropId := AMessage.NextByte;
    if APropId in [1 .. 42] then
    begin
      with FItems[APropId - 1] do
      begin
        case MQTT5PropTypes[APropId - 1].DataType of
          ptByte:
            AsByte := AMessage.NextByte;
          ptWord:
            AsWord := AMessage.NextWord(false);
          ptInt:
            AsInteger := AMessage.NextDWord(false);
          ptVarInt:
            AsInteger := AMessage.NextDWord(True);
          ptString:
            begin
              if not Assigned(AsString) then
                New(AsString);
              AsString^.Length := AMessage.NextWord(false);
              AsString^.IsUtf8 := True;
              AMessage.NextBytes(PQCharA(AsString^), AsString^.Length);
            end;
          ptBinary:
            begin
              if not Assigned(AsBytes) then
                New(AsBytes);
              SetLength(AsBytes^, AMessage.NextWord(false));
              AMessage.NextBytes(@AsBytes^[0], Length(AsBytes^));
            end;
        end;
        IsSet := True;
      end;
    end;
  end;
end;

procedure TQMQTT5Props.Replace(AProps: TQMQTT5Props);
var
  I: Integer;
begin
  for I := 0 to High(FItems) do
  begin
    if AProps.FItems[I].IsSet then
    begin
      case MQTT5PropTypes[I].DataType of
        ptString:
          begin
            if not Assigned(FItems[I].AsString) then
              New(FItems[I].AsString);
            FItems[I].AsString^ := AProps.FItems[I].AsString^;
          end;
        ptBinary:
          begin
            if not Assigned(FItems[I].AsBytes) then
              New(FItems[I].AsBytes);
            FItems[I].AsBytes^ := AProps.FItems[I].AsBytes^;
          end
      else
        FItems[I].AsInteger := AProps.FItems[I].AsInteger;
      end;
    end;
  end;
end;

procedure TQMQTT5Props.SetAsInt(const APropId: Byte; const Value: Cardinal);
begin
  FItems[APropId - 1].IsSet := True;
  case MQTT5PropTypes[APropId - 1].DataType of
    ptByte:
      FItems[APropId - 1].AsByte := Value;
    ptWord:
      FItems[APropId - 1].AsWord := Value;
    ptInt, ptVarInt:
      FItems[APropId - 1].AsInteger := Value;
    ptString:
      begin
        if not Assigned(FItems[APropId - 1].AsString) then
          New(FItems[APropId - 1].AsString);
        FItems[APropId - 1].AsString^ := IntToStr(Value);
      end
  else
    begin
      FItems[APropId - 1].IsSet := false;
      raise Exception.Create('不支持的类型转换');
    end;
  end;
end;

procedure TQMQTT5Props.SetAsString(const APropId: Byte; const Value: String);
begin
  FItems[APropId - 1].IsSet := True;
  case MQTT5PropTypes[APropId - 1].DataType of
    ptByte:
      FItems[APropId - 1].AsByte := Byte(StrToInt(Value));
    ptWord:
      FItems[APropId - 1].AsWord := Word(StrToInt(Value));
    ptInt, ptVarInt:
      FItems[APropId - 1].AsInteger := Cardinal(StrToInt(Value));
    ptString:
      begin
        if not Assigned(FItems[APropId - 1].AsString) then
          New(FItems[APropId - 1].AsString);
        FItems[APropId - 1].AsString^ := qstring.Utf8Encode(Value);
      end;
    ptBinary:
      begin
        if not Assigned(FItems[APropId - 1].AsBytes) then
          New(FItems[APropId - 1].AsBytes);
        FItems[APropId - 1].AsBytes^ := qstring.Utf8Encode(Value);
      end;
  end;
end;

procedure TQMQTT5Props.SetAsVariant(const APropId: Byte; const Value: Variant);
begin
  FItems[APropId - 1].IsSet := True;
  case MQTT5PropTypes[APropId - 1].DataType of
    ptByte:
      FItems[APropId - 1].AsByte := Value;
    ptWord:
      FItems[APropId - 1].AsWord := Value;
    ptInt, ptVarInt:
      FItems[APropId - 1].AsInteger := Value;
    ptString:
      begin
        if not Assigned(FItems[APropId - 1].AsString) then
          New(FItems[APropId - 1].AsString);
        FItems[APropId - 1].AsString^ := qstring.Utf8Encode(VarToStr(Value));
      end;
    ptBinary:
      begin
        if not Assigned(FItems[APropId - 1].AsBytes) then
          New(FItems[APropId - 1].AsBytes);
        SetLength(FItems[APropId - 1].AsBytes^,
          VarArrayHighBound(Value, 1) + 1);
        Move(VarArrayLock(Value)^, FItems[APropId - 1].AsBytes^[0],
          Length(FItems[APropId - 1].AsBytes^));
        VarArrayUnlock(Value);
      end;
  end;
end;

procedure TQMQTT5Props.WriteProps(AMessage: PQMQTTMessage);
var
  I: Integer;
  ASize: Cardinal;
  APropType: PQMQTT5PropType;
begin
  ASize := 0;
  for I := 0 to High(FItems) do
  begin
    if FItems[I].IsSet then
      Inc(ASize, DataSize[I + 1] + 1);
  end;
  AMessage.Cat(ASize, True);
  for I := 0 to High(FItems) do
  begin
    if FItems[I].IsSet then
    begin
      APropType := @MQTT5PropTypes[I];
      AMessage.Cat(APropType.Id);
      case APropType.DataType of
        ptByte:
          AMessage.Cat(FItems[I].AsByte);
        ptWord:
          AMessage.Cat(FItems[I].AsWord);
        ptInt:
          AMessage.Cat(FItems[I].AsInteger);
        ptVarInt:
          AMessage.Cat(FItems[I].AsInteger, True);
        ptString:
          AMessage.Cat(FItems[I].AsString^);
        ptBinary:
          AMessage.Cat(FItems[I].AsBytes^);
      end;
    end;
  end;
end;

{ TQMQTT5PropType }

function TQMQTT5PropType.GetDataSize: Integer;
const
  DataSizes: array [TQMQTT5PropDataType] of Integer = (0, 1, 2, 4, -4, -1, -1);
begin
  Result := DataSizes[DataType];
end;

{ TQMQTTSubscribeResult }

function TQMQTTSubscribeResult.GetSuccess: Boolean;
begin
  Result := (ErrorCode = 0);
end;

initialization

StartSocket;

finalization

if Assigned(_DefaultClient) then
  FreeAndNil(_DefaultClient);
CleanSocket;

end.
