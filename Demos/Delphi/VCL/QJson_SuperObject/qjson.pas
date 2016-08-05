unit qjson;
{$I 'qdac.inc'}
interface
{
  本源码来自QDAC项目，版权归swish(QQ:109867294)所有。
  (1)、使用许可及限制
  您可以自由复制、分发、修改本源码，但您的修改应该反馈给作者，并允许作者在必要时，
合并到本项目中以供使用，合并后的源码同样遵循QDAC版权声明限制。
  您的产品的关于中，应包含以下的版本声明:
  本产品使用的JSON解析器来自QDAC项目中的QJSON，版权归作者所有。
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

{修订日志

2014.5.27
==========
  + TQHashedJson 支持，这是一个面向查询优化的版本，使用哈希表加快ItemByName的查询速度，
    如果您的应用中大量的使用ItemByName、ItemByPath等查询，使用它代替TQJson，否则应直接
    使用TQJson

2014.5.14
=========
  + 加入CopyIf/DeleteIf/FindIf函数
  + 加入for..in语法支持
  * 修正了Encode和ForcePath可能存在的问题

2014.5.6
========
  + 加入ParseBlock函数，以支持流式传送分段解析
  * 修正了解析\uxxxx时的识别错误
  * 修改Parse函数为清除已有子结点

2014.5.4
========
  + 加入对JavaScript和.net的日期时间类型/DATE(MillSeconds+TimeZone)/格式的支持
  * Json数据支持加入VCL的TDateTime类型支持，生成的JSON数据默认由JsonDateFormat、
    JsonTimeFormat,JsonDateTimeFormat三个变量控制，如果StrictJson变量为True，则
    生成/DATE(MillSeconds+TimeZone)/格式
  【注】
  日期时间类型仅适用于运行时，生成JSON后实际上仍为字符串，这种字符串再次打开时
  将丢失类型信息，但您仍可以直接以AsDateTime属性来读写。如果日期时间类型使用
  JavaScript和.net格式并且包含了时区信息，则时间将被转换为格林威治时间。

2014.5.1
========
  + 加入AddRecord函数，支持直接保存记录数据，但以下类型的成员会被忽略
    对象(Class)、函数(Method)、接口(Interface)、类型引用(ClassRef),指针(Pointer)、过程(Procedure)
    将来可能根据实际需要决定是否添加支持
  + 加入ToRecord函数，完成Json直接到记录类型的转换
  + 加入Copy函数用于创建当前结点的一个拷贝实例，注意目前版本克隆内部调用了Copy函数，将来可能改掉
  * 修正了Assign函数的一处错误
}
//测试环境仅为Delphi 2007或XE6，其它版本的开发环境，请自行修改
uses classes,sysutils,math,qstring,typinfo,qrbtree
  {$IFDEF QDAC_UNICODE}
  ,Generics.Collections,RegularExpressionsCore
  {$ENDIF}
  {$IFDEF QDAC_RTTI}
  ,Rtti
  {$ENDIF}
  ;
{$M+}
//如果要使用类名来表示方式，如TForm1.FormCreate,则启用下面的定义，否则方法名为Form1.FormCreate
{.$DEFINE TYPENAMEASMETHODPREF}
type
  ///本单元是QDAC的组成部分，受QDAC授权限制，详情访问QDAC网站了解
  /// <summary>
  /// JSON解析单元，用于快速解析和维护JSON结构.全局变量StrictJson为False时，支持
  ///注释和名称不包含'"'。
  /// </summary>
  ///  TQJsonDataType用于记录JSON数据元的类型，可取值包括：
  ///  <list>
  ///    <item>
  ///    <term>jdtUnknown</term><description>未知类型，只有新构造对象未赋值时，才会是该类型</description>
  ///    </item>
  ///    <item>
  ///    <term>jdtNull</term><description>NULL</description>
  ///    </item>
  ///    <item>
  ///    <term>jdtString</term><description>字符串</description>
  ///    </item>
  ///    <item>
  ///    <term>jdtInteger</term><description>整数(Int64,不管整数值多大，内部均使用64位整数存贮)</description>
  ///    </item>
  ///    <item>
  ///    <term>jdtFloat</term><description>双精度浮点数(Double)</description>
  ///   </item>
  ///    <item>
  ///    <term>jdtBoolean</term><description>布尔</description>
  ///    </item>
  ///   <item>
  ///   <term>jdtDateTime</term><description>日期时间类型</description>
  ///    </item>
  ///    <item>
  ///    <term>jdtArray</term><description>数组</description>
  ///    </item>
  ///    <item>
  ///    <term>jdtObject</term><description>对象</description>
  ///    </item>
  ///  </list>
  TQJsonDataType=(jdtUnknown,jdtNull,jdtString,jdtInteger,jdtFloat,jdtBoolean,jdtDateTime,jdtArray,jdtObject);
  TQJson=class;
  {$IFDEF QDAC_UNICODE}
  /// <summary>
  ///   RTTI信息过滤回调函数，在XE6上支持匿名函数，在XE及以前的版本采用事件回调
  /// </summary>
  /// <param name="ASender">触发事件的TQJson对象</param>
  ///  <param name="AName">属性名(AddObject)或字段名(AddRecord)</param>
  ///  <param name="AType">属性或字段的类型信息</param>
  ///  <param name="Accept">是否记录该属性或字段</param>
  ///  <param name="ATag">用户自定义的附加数据成员</param>
  TQJsonRttiFilterEventA=reference to procedure (ASender:TQJson;AObject:Pointer;AName:QStringW;AType:PTypeInfo;var Accept:Boolean;ATag:Pointer);
  /// <summary>
  /// 结点过滤处理函数，以在XE6上支持匿名函数
  /// </summary>
  /// <param name="ASender">触发事件的TQJson对象</param>
  /// <param name="AItem">要过滤的对象</param>
  /// <param name="Accept">是否要处理该对象</param>
  /// <param name="ATag">用户附加的数据项</param>
  TQJsonFilterEventA=reference to procedure(ASender,AItem:TQJson;var Accept:Boolean;ATag:Pointer);
  {$ENDIF}
  /// <summary>
  ///   RTTI信息过滤回调函数，在XE6上支持匿名函数，在XE及以前的版本采用事件回调
  /// </summary>
  /// <param name="ASender">触发事件的TQJson对象</param>
  ///  <param name="AName">属性名(AddObject)或字段名(AddRecord)</param>
  ///  <param name="AType">属性或字段的类型信息</param>
  ///  <param name="Accept">是否记录该属性或字段</param>
  ///  <param name="ATag">用户自定义的附加数据成员</param>
  TQJsonRttiFilterEvent=procedure (ASender:TQJson;AObject:Pointer;AName:QStringW;AType:PTypeInfo;var Accept:Boolean;ATag:Pointer) of object;
  /// <summary>
  /// 结点过滤处理函数，以在XE6上支持匿名函数
  /// </summary>
  /// <param name="ASender">触发事件的TQJson对象</param>
  /// <param name="AItem">要过滤的对象</param>
  /// <param name="Accept">是否要处理该对象</param>
  /// <param name="ATag">用户附加的数据项</param>
  TQJsonFilterEvent=procedure(ASender,AItem:TQJson;var Accept:Boolean;ATag:Pointer) of object;
  PQJson=^TQJson;
  {$IFDEF QDAC_UNICODE}
  TQJsonItemList=TList<TQJson>;
  {$ELSE}
  TQJsonItemList=TList;
  {$ENDIF}
  /// <summary>
  ///   TQJsonTagType用于内部AddObject和AddRecord函数的内部过滤使用
  /// </summary>
  /// <list>
  ///  <item>
  ///    <term>ttAnonEvent</term><description>回调匿名函数</description>
  ///    <term>ttNameFilter</term><description>属性或成员名称过滤</descriptio>
  /// </list>
  TQJsonTagType=(ttAnonEvent,ttNameFilter);
  PQJsonInternalTagData=^TQJsonInternalTagData;
  /// <summary>
  /// TQJsonInternalTagData用于AddRecord和AddObject函数需要内部过滤RTTI信息时使用
  /// </summary>
  TQJsonInternalTagData=record
    /// <summary>Tag数据的类型</summary>
    TagType:TQJsonTagType;
    {$IFDEF QDAC_RTTI}
    /// <summary>过滤使用的匿名函数</summary>
    OnEvent:TQJsonRttiFilterEventA;
    {$ENDIF QDAC_RTTI}
    /// <summary>接受的属性(AddObject)或记录字段(AddRecord)名称，如果名称同时在IgnoreNames出现，则IgnoreNames里的信息被忽略</summary>
    AcceptNames:QStringW;
    /// <summary>忽略的属性(AddObject)或记录字段(AddRecord)名称，如果名称同时在AcceptNameds里，则AcceptNames优先</summary>
    IgnoreNames:QStringW;
    /// <summary>原始传递给AddObject或AddRecord的附加数据成员，它将被传递给OnEvent的Tag，以供用户使用</summary>
    Tag:Pointer;
  end;
  TQJsonEnumerator=class;
  ///<summary>用于外部支持对象池的函数，创建一个新的QJSON对象，注意从池中创建的对象</summary>
  ///  <returns>返回新创建的QJSON对象</returns>
  TQJsonCreateEvent=function:TQJson;
  ///<summary>用于外部将对象缓存，以便重用对象</summary>
  ///  <param name="AJson">要释放的Json对象</param>
  TQJsonFreeEvent=procedure (AJson:TQJson);
  TValueArray=array of TValue;
  /// <summary>
  ///  TQJson用于解析并维护JSON格式的对象类型，要使用前，需要先在堆中创建对应的实例。
  ///  TQJson和TQXML在绝大多数接口上保持一致，但由于Json是有类型信息，而XML没有类型
  ///  信息（始终认为是字符串），因此少部分接口会略有不同.
  ///  与其它实现不同，QJSON所有的类型都是同一个对象实现，根据DataType的不同，而使用
  ///  不同的成员来访问。当类型为jdtArray或者是jdtObject时，它可以有子结点.
  /// </summary>
  TQJson=class
  private
    function GetAsValue: TValue;
    procedure SetAsValue(const Value: TValue);
  protected
    FName:QStringW;
    FNameHash:Cardinal;
    FDataType:TQJsonDataType;
    FValue:QStringW;
    FParent:TQJson;
    FData:Pointer;
    FItems:TQJsonItemList;
    function GetValue: QStringW;
    procedure SetValue(const Value: QStringW);
    procedure SetDataType(const Value: TQJsonDataType);
    function GetAsBoolean: Boolean;
    function GetAsFloat: Extended;
    function GetAsInt64: Int64;
    function GetAsInteger: Integer;
    function GetAsString: QStringW;
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsFloat(const Value: Extended);
    procedure SetAsInt64(const Value: Int64);
    procedure SetAsInteger(const Value: Integer);
    procedure SetAsString(const Value: QStringW);
    function GetAsObject: QStringW;
    procedure SetAsObject(const Value: QStringW);
    function GetAsDateTime: TDateTime;
    procedure SetAsDateTime(const Value: TDateTime);
    function GetCount: Integer;
    function GetItems(AIndex: Integer): TQJson;
    function CharUnescape(var p:PQCharW):QCharW;
    function CharEscape(c:QCharW;pd:PQCharW):Integer;
    procedure ArrayNeeded(ANewType:TQJsonDataType);
    procedure ValidArray;
    procedure ParseObject(var p:PQCharW);
    procedure ParseJsonPair(ABuilder:TQStringCatHelperW;var p:PQCharW);
    procedure BuildJsonString(ABuilder:TQStringCatHelperW;var p:PQCharW);
    procedure ParseName(ABuilder:TQStringCatHelperW;var p:PQCharW);
    procedure ParseValue(ABuilder:TQStringCatHelperW;var p:PQCharW);
    function BoolToStr(const b:Boolean):QStringW;
    function GetIsNull: Boolean;
    function GetIsNumeric: Boolean;
    function GetIsArray: Boolean;
    function GetIsObject: Boolean;
    function GetIsString: Boolean;
    function GetIsDateTime: Boolean;
    function GetAsArray: QStringW;
    procedure SetAsArray(const Value: QStringW);
    function GetPath: QStringW;
    function GetAsVariant: Variant;
    procedure SetAsVariant(const Value: Variant);
    function GetAsJson: QStringW;
    procedure SetAsJson(const Value: QStringW);
    function GetItemIndex: Integer;
    function ParseJsonTime(p:PQCharW;var ATime:TDateTime):Boolean;
    function CreateJson:TQJson;virtual;
    procedure FreeJson(AJson:TQJson);inline;
    procedure CopyValue(ASource:TQJson);inline;
    procedure Replace(AIndex:Integer;ANewItem:TQJson);virtual;
    {$IFDEF QDAC_RTTI}
    function InternalAddRecord(ATypeInfo: PTypeInfo; ABaseAddr: Pointer;AOnFilter:TQJsonRttiFilterEvent;ATag:Pointer):Boolean;
    procedure InternalToRecord(ATypeInfo:PTypeInfo;ABaseAddr:Pointer);
    {$ENDIF QDAC_RTTI}
    procedure InternalRttiFilter(ASender:TQJson;AObject:Pointer;APropName:QStringW;APropType:PTypeInfo;var Accept:Boolean;ATag:Pointer);
    function InternalEncode(ABuilder:TQStringCatHelperW;ADoFormat:Boolean;const AIndent:QStringW):TQStringCatHelperW;
  public
    ///<summary>构造函数</summary>
    constructor Create;overload;
    constructor Create(const AName,AValue:QStringW;ADataType:TQJsonDataType=jdtUnknown);overload;
    ///<summary>析构函数</summary>
    destructor Destroy;override;
    {<summary》添加一个子结点<、summary>
    <param name="ANode">要添加的结点</param>
    <returns>返回添加的结点索引</returns>
    }
    function Add(ANode:TQJson): Integer;overload;
    /// <summary>添加一个未命名的JSON子结点</summary>
    /// <returns>返回添加的结点实例</returns>
    /// <remarks>
    /// 一般情况下，除非数组类型，不应添加未命名的实例
    /// </remarks>
    function Add:TQJson;overload;
    /// <summary>添加一个对象到Json对象</summary>
    ///  <param name="AName">要添加的对象结点名称</param>
    ///  <param name="AObject">要添加的对象</param>
    /// <returns>返回添加的结点</returns>
    function AddObject(AName:QStringW;AObject:TObject):TQJson;overload;
    /// <summary>添加一个对象到Json对象</summary>
    ///  <param name="AName">要添加的对象结点名称</param>
    ///  <param name="AObject">要添加的对象</param>
    ///  <param name="AOnFilter">指定属性过滤事件，以过滤掉不需要转换的属性</param>
    ///  <param name="ANest">如果属性的是对象，是否递归</param>
    ///  <param name="ATag">附加的数据成功，用于事件回调中用户自己的用途</param>
    /// <returns>返回添加的结点</returns>
    function AddObject(AName:QStringW;AObject:TObject;AOnFilter:TQJsonRttiFilterEvent;ANest:Boolean;ATag:Pointer):TQJson;overload;
    {$IFDEF QDAC_RTTI}
    /// <summary>添加一个对象到Json对象</summary>
    ///  <param name="AName">要添加的对象结点名称</param>
    ///  <param name="AObject">要添加的对象</param>
    ///  <param name="AOnFilter">指定属性过滤事件，以过滤掉不需要转换的属性</param>
    ///  <param name="ANest">如果属性的是对象，是否递归</param>
    ///  <param name="ATag">附加的数据成功，用于事件回调中用户自己的用途</param>
    /// <returns>返回添加的结点</returns>
    function AddObject(AName:QStringW;AObject:TObject;AOnFilter:TQJsonRttiFilterEventA;ANest:Boolean;ATag:Pointer):TQJson;overload;
    {$ENDIF QDAC_RTTI}
    /// <summary>添加一个对象到Json对象</summary>
    ///  <param name="AName">要添加的对象结点名称</param>
    ///  <param name="AObject">要添加的对象</param>
    ///  <param name="AcceptProps">允许加入的属性/param>
    ///  <param name="AIgnoreProps">忽略不加入的属性</param>
    /// <returns>返回添加的结点</returns>
    function AddObject(AName:QStringW;AObject:TObject;ANest:Boolean;AcceptProps,AIgnoreProps:QStringW):TQJson;overload;
    {$IFDEF QDAC_RTTI}
    /// <summary>添加一个记录（结构体）到Json中</summary>
    /// <param name="AName">要添加的对象的结点名称</param>
    /// <param name="AObject">要添加的记录实例</param>
    /// <returns>返回创建的结点实例</returns>
    /// <remarks>
    function AddRecord<T>(AName:QStringW;const AObject:T;AcceptFields,AIgnoreFields:QStringW):TQJson;overload;
    /// <summary>添加一个记录（结构体）到Json中</summary>
    /// <param name="AName">要添加的对象的结点名称</param>
    /// <param name="AObject">要添加的对象实例</param>
    /// <returns>返回创建的结点实例</returns>
    function AddRecord<T>(AName:QStringW;const AObject:T):TQJson;overload;
    /// <summary>添加一个记录（结构体）到Json中</summary>
    /// <param name="AName">要添加的对象的结点名称</param>
    /// <param name="AObject">要添加的对象实例</param>
    /// <param name="AOnFilter">指定属性过滤事件，以过滤掉不需要转换的属性</param>
    /// <param name="ATag">附加的数据成功，用于事件回调中用户自己的用途</param>
    /// <returns>返回创建的结点实例</returns>
    function AddRecord<T>(AName:QStringW;const AObject:T;AOnFilter:TQJsonRttiFilterEvent;ATag:Pointer):TQJson;overload;
    {$IFDEF QDAC_UNICODE}
    /// <summary>添加一个记录（结构体）到Json中</summary>
    /// <param name="AName">要添加的对象的结点名称</param>
    /// <param name="AObject">要添加的对象实例</param>
    /// <param name="AOnFilter">指定属性过滤事件，以过滤掉不需要转换的属性（匿名函数版本）</param>
    /// <param name="ATag">附加的数据成功，用于事件回调中用户自己的用途</param>
    /// <returns>返回创建的结点实例</returns>
    function AddRecord<T>(AName:QStringW;const AObject:T;AOnFilter:TQJsonRttiFilterEventA;ATag:Pointer):TQJson;overload;
    {$ENDIF QDAC_UNICODE}
    {$ENDIF QDAC_RTTI}
    {<summary>添加一个子结点</summary>
    <param name="AName">要添加的结点名，如果当前结点为数组，则在输出时会忽略该值</param>
    <param name="AValue">要添加的结点值</param>
    <param name="ADataType">要添加的结点数据类型，如果省略，则自动根据值的内容检测</param>
    <returns>返回新子结点的索引</returns>
    <remarks>
      1.如果当前类型不是jdtObject或jdtArray，将自动转换为jdtObject类型
      2.上层应自己负责重名检查
    </remarks>
    }
    function Add(AName,AValue: QStringW;ADataType:TQJsonDataType=jdtUnknown):Integer;overload;
    /// <summary>添加一个数组</summary>
    /// <param name="AName">要添加的对象的结点名称</param>
    /// <param name="AItems">要添加的数组内容</param>
    /// <returns>返回创建的结点实例</returns>
    function Add(const AName:QStringW;AItems:array of const):TQJson;overload;
    {<summary>添加一个子结点</summary>
    <param name="AName">要添加的结点名</param>
    <param name="ADataType">要添加的结点数据类型，如果省略，则自动根据值的内容检测</param>
    <returns>返回添加的新对象</returns>
    <remarks>
      1.如果当前类型不是jdtObject或jdtArray，将自动转换为jdtObject类型
      2.上层应自己负责重名检查
    </remarks>
    }
    function Add(AName:QStringW;ADataType:TQJsonDataType):TQJson;overload;

    /// <summary>添加一个子结点</summary>
    /// <param name="AName">要添加的结点名，如果当前结点为数组，则在输出时会忽略该值</param>
    /// <param name="AValue">要添加的结点值</param>
    /// <returns>返回添加的新对象</returns>
    function Add(AName:QStringW;AValue:Extended):TQJson;overload;
    /// <summary>添加一个子结点</summary>
    /// <param name="AName">要添加的结点名，如果当前结点为数组，则在输出时会忽略该值</param>
    /// <param name="AValue">要添加的结点值</param>
    /// <returns>返回添加的新对象</returns>
    function Add(AName:QStringW;AValue:Int64):TQJson;overload;
    /// <summary>添加一个子结点</summary>
    /// <param name="AName">要添加的结点名，如果当前结点为数组，则在输出时会忽略该值</param>
    /// <param name="AValue">要添加的结点值</param>
    /// <returns>返回添加的新对象</returns>
    function Add(AName:QStringW;AValue:Boolean):TQJson;overload;
    /// <summary>添加一个子结点</summary>
    /// <param name="AName">要添加的结点名，如果当前结点为数组，则在输出时会忽略该值</param>
    /// <param name="AValue">要添加的结点值</param>
    /// <returns>返回添加的新对象</returns>
    function AddDateTime(AName:QStringW;AValue:TDateTime):TQJson;overload;
    /// <summary>添加一个子结点(Null)</summary>
    /// <param name="AName">要添加的结点名，如果当前结点为数组，则在输出时会忽略该值</param>
    /// <returns>返回添加的新对象</returns>
    function Add(AName:QStringW):TQJson;overload;virtual;

    /// <summary>强制一个路径存在,如果不存在,则依次创建需要的结点(jdtObject或jdtArray)</summary>
    /// <param name="APath">要添加的结点路径</param>
    /// <returns>返回路径对应的对象</returns>
    /// <remarks>
    /// 假设以下路径完全不存在，则ForcePath会按如下规则执行:
    /// 1、如果APath中包含[]，则认为对应的路径结点为数组，示例如下：
    ///  (1)、'a.b[].name'：
    ///   a -> jdtObject
    ///   b -> jdtArray
    ///   b[0].name -> jdtNull(b的索引未指定，自动认为是b[0]
    ///  (2)、'a.c[2].name'：
    ///   a -> jdtObject
    ///   c -> jdtArray
    ///   c[2].name -> jdtNull
    ///   其中,c[0],c[1]被自动创建，并赋值为jdtNull，执行完成后c为包含三个元素的数组
    ///  (3)、'a[0]'：
    ///   a -> jdtArray
    ///   a[0] -> jdtNull
    /// 2、路径分隔符./\是等价的，并且结点名称中不应包含上述三个字符之一,即：
    ///  a.b.c和a\b\c和a/b/c是完全相同的路径
    /// 3、如果APath指定的对象类型不匹配，则会抛出异常，如a为对象，但使用a[0].b访问时。
    /// </remarks>
    function ForcePath(APath:QStringW):TQJson;
    /// <summary>解析指定的JSON字符串</summary>
    /// <param name="p">要解析的字符串</param>
    /// <param name="l">字符串长度，<=0认为是以\0(#0)结尾的C语言标准字符串</param>
    /// <remarks>如果l>=0，会检测p[l]是否为\0，如果不为\0，则会创建拷贝实例并解析拷贝实例</remarks>
    procedure Parse(p:PWideChar;l:Integer=-1);overload;
    /// <summary>解析指定的JSON字符串</summary>
    /// <param name="s">要解析的JSON字符串</param>
    procedure Parse(const s:QStringW);overload;
    /// <summmary>从流中解析首个JSON数据块</summary>
    ///  <param name="AStream">流对象</param>
    ///  <param name="AEncoding">流数据的编码方式</param>
    /// <remarks>ParseBlock适合解析分段式JSON，它会从当前位置开始，解析到当前对象结束为止.
    ///  可以很好的满足渐进式传输的需要</remarks>
    procedure ParseBlock(AStream:TStream;AEncoding:TTextEncoding);
    /// <summary>拷贝生成一个新的实例</summary>
    /// <returns>返回新的拷贝实例</returns>
    /// <remarks>因为是拷贝，所以新旧对象之间的内容变更没有任何关系，更改任意一个
    ///  对象，不会对另外一个对象造成影响。
    ///  </remarks>
    function Copy:TQJson;
    {$IFDEF QDAC_RTTI}
    /// <summary>拷贝生成一个新的实例</summary>
    /// <param name="ATag">用户附加的标签数据</param>
    /// <param name="AFilter">用户过滤事件，用于控制要拷贝的内容</param>
    /// <returns>返回新的拷贝实例</returns>
    /// <remarks>因为是拷贝，所以新旧对象之间的内容变更没有任何关系，更改任意一个
    ///  对象，不会对另外一个对象造成影响。
    ///  </remarks>
    function CopyIf(const ATag:Pointer;AFilter:TQJsonFilterEventA):TQJson;overload;
    {$ENDIF}
     /// <summary>拷贝生成一个新的实例</summary>
    /// <param name="ATag">用户附加的标签数据</param>
    /// <param name="AFilter">用户过滤事件，用于控制要拷贝的内容</param>
    /// <returns>返回新的拷贝实例</returns>
    /// <remarks>因为是拷贝，所以新旧对象之间的内容变更没有任何关系，更改任意一个
    ///  对象，不会对另外一个对象造成影响。
    ///  </remarks>
    function CopyIf(const ATag:Pointer;AFilter:TQJsonFilterEvent):TQJson;overload;
    /// <summary>克隆生成一个新的实例</summary>
    /// <returns>返回新的拷贝实例</returns>
    /// <remarks>因为实际上执行的是拷贝，所以新旧对象之间的内容变更没有任何关系，
    ///  更改任意一个对象，不会对另外一个对象造成影响，但此行为将来并不保证，可能
    ///  会调整为引用，以便相互影响。
    ///  </remarks>
    function Clone:TQJson;
    /// <summary>编码为字符串</summary>
    /// <param name="ADoFormat">是否格式化字符串，以增加可读性</param>
    /// <param name="AIndent">ADoFormat参数为True时，缩进内容，默认为两个空格</param>
    /// <returns>返回编码后的字符串</returns>
    ///  <remarks>AsJson等价于Encode(True,'  ')</remarks>
    function Encode(ADoFormat:Boolean;AIndent:QStringW='  '):QStringW;
    /// <summary>获取指定名称获取结点的值的字符串表示</summary>
    /// <param name="AName">结点名称</param>
    /// <returns>返回应结点的值</returns>
    function ValueByName(AName,ADefVal:QStringW):QStringW;
    /// <summary>获取指定路径结点的值的字符串表示</summary>
    /// <param name="AName">结点名称</param>
    /// <returns>如果结果不存在，返回默认值，否则，返回原始值</returns>
    function ValueByPath(APath,ADefVal:QStringW):QStringW;
    /// <summary>获取指定名称的第一个结点</summary>
    /// <param name="AName">结点名称</param>
    /// <returns>返回找到的结点，如果未找到，返回空(NULL/nil)</returns>
    /// <remarks>注意QJson并不检查重名，因此，如果存在重名的结点，只会返回第一个结点</remarks>
    function ItemByName(AName:QStringW):TQJson;overload;
    /// <summary>获取指定名称的结点到列表中</summary>
    /// <param name="AName">结点名称</param>
    ///  <param name="AList">用于保存结点的列表对象</param>
    ///  <param name="ANest">是否递归查找子结点</param>
    /// <returns>返回找到的结点数量，如果未找到，返回0</returns>
    function ItemByName(const AName:QStringW;AList:TQJsonItemList;ANest:Boolean=False):Integer;overload;
    /// <summary>获取符合指定名称规则的结点到列表中</summary>
    /// <param name="ARegex">正则表达式</param>
    ///  <param name="AList">用于保存结点的列表对象</param>
    ///  <param name="ANest">是否递归查找子结点</param>
    /// <returns>返回找到的结点数量，如果未找到，返回0</returns>
    function ItemByRegex(const ARegex:QStringW;AList:TQJsonItemList;ANest:Boolean=False):Integer;overload;
    /// <summary>获取指定路径的JSON对象</summary>
    ///  <param name="APath">路径，以"."或"/"或"\"分隔</param>
    ///  <returns>返回找到的子结点，如果未找到返回NULL(nil)</returns>
    function ItemByPath(APath:QStringW):TQJson;
    /// <summary>从源对象复制JSON对象内容</summary>
    /// <param name="ANode">要复制的源结点</param>
    /// <remarks>注意不要复制子结点给自己，否则会造成死循环。要复制子结点，先复
    /// 制一个子结点的新实例，再从新实例复制
    /// </remarks>
    procedure Assign(ANode:TQJson);virtual;
    /// <summary>删除指定索引的结点</summary>
    /// <param name="AIndex">要删除的结点索引</param>
    /// <remarks>
    /// 如果指定索引的结点不存在，则抛出EOutRange异常
    /// </remarks>
    procedure Delete(AIndex:Integer);overload;virtual;
    /// <summary>删除指定名称的结点</summary>
    ///  <param name="AName">要删除的结点名称</param>
    ///  <remarks>
    ///  如果要多个重名的结点，则只删除第一个
    procedure Delete(AName:QStringW);overload;
    {$IFDEF QDAC_RTTI}
    ///<summary>
    /// 删除符合条件的子结点
    ///</summary>
    ///  <param name="ATag">用户自己附加的额外标记</param>
    ///  <param name="ANest">是否嵌套调用，如果为false，则只对当前子结点过滤</param>
    ///  <param name="AFilter">过滤回调函数，如果为nil，等价于Clear</param>
    procedure DeleteIf(const ATag:Pointer;ANest:Boolean;AFilter:TQJsonFilterEventA);overload;
    {$ENDIF QDAC_RTTI}
    ///<summary>
    /// 删除符合条件的子结点
    ///</summary>
    ///  <param name="ATag">用户自己附加的额外标记</param>
    ///  <param name="ANest">是否嵌套调用，如果为false，则只对当前子结点过滤</param>
    ///  <param name="AFilter">过滤回调函数，如果为nil，等价于Clear</param>
    procedure DeleteIf(const ATag:Pointer;ANest:Boolean;AFilter:TQJsonFilterEvent);overload;
    /// <summary>查找指定名称的结点的索引</summary>
    ///  <param name="AName">要查找的结点名称</param>
    ///  <returns>返回索引值，未找到返回-1</returns>
    function IndexOf(const AName:QStringW):Integer;virtual;
    {$IFDEF QDAC_RTTI}
    ///<summary>遍历结点查找符合条件的结点</summary>
    /// <param name="ATag">用户自定义的附加额外标记</param>
    ///  <param name="ANest">是否嵌套调用，如果为false，则只对当前子结点过滤</param>
    ///  <param name="AFilter">过滤回调函数，如果为nil，则返回nil</param>
    function FindIf(const ATag:Pointer;ANest:Boolean;AFilter:TQJsonFilterEventA):TQJson;overload;
    {$ENDIF QDAC_RTTI}
    ///<summary>遍历结点查找符合条件的结点</summary>
    /// <param name="ATag">用户自定义的附加额外标记</param>
    ///  <param name="ANest">是否嵌套调用，如果为false，则只对当前子结点过滤</param>
    ///  <param name="AFilter">过滤回调函数，如果为nil，则返回nil</param>
    function FindIf(const ATag:Pointer;ANest:Boolean;AFilter:TQJsonFilterEvent):TQJson;overload;
    /// <summary>清除所有的结点</summary>
    procedure Clear;virtual;
    /// <summary>保存当前对象内容到流中</summary>
    ///  <param name="AStream">目标流对象</param>
    ///  <param name="AEncoding">编码格式</param>
    ///  <param name="AWriteBom">是否写入BOM</param>
    ///  <remarks>注意当前结点的名称不会被写入</remarks>
    procedure SaveToStream(AStream:TStream;AEncoding:TTextEncoding;AWriteBOM:Boolean);
    /// <summary>从流的当前位置开始加载JSON对象</summary>
    ///  <param name="AStream">源数据流</param>
    ///  <param name="AEncoding">源文件编码，如果为teUnknown，则自动判断</param>
    ///  <remarks>流的当前位置到结束的长度必需大于2字节，否则无意义</remarks>
    procedure LoadFromStream(AStream:TStream;AEncoding:TTextEncoding=teUnknown);
    /// <summary>保存当前对象内容到文件中</summary>
    ///  <param name="AFileName">文件名</param>
    ///  <param name="AEncoding">编码格式</param>
    ///  <param name="AWriteBOM">是否写入UTF-8的BOM</param>
    ///  <remarks>注意当前结点的名称不会被写入</remarks>
    procedure SaveToFile(AFileName:String;AEncoding:TTextEncoding;AWriteBOM:Boolean);
    /// <summary>从指定的文件中加载当前对象</summary>
    ///  <param name="AFileName">要加载的文件名</param>
    ///  <param name="AEncoding">源文件编码，如果为teUnknown，则自动判断</param>
    procedure LoadFromFile(AFileName:String;AEncoding:TTextEncoding=teUnknown);
    //// <summary>重置值为Null，等价于直接设置DataType为jdtNull</summary>
    procedure ResetNull;
    /// <summary>重载TObject.ToString函数</summary>
    function ToString: string;{$IFDEF QDAC_UNICODE}override;{$ELSE}virtual;{$ENDIF}
    /// <summary>将Json的数据还原回原来的对象属性</summary>
    procedure ToObject(AObject:TObject);
    {$IFDEF QDAC_RTTI}
    /// <summary>将Json的数据还原回原来的结构体（记录）字段值</summary>
    /// <param name="ARecord">目标结构体实例</param>
    procedure ToRecord<T>(const ARecord:T);
    {$ENDIF QDAC_RTTI}
    function GetEnumerator: TQJsonEnumerator;
    function Invoke(Instance: TObject): TValue; overload;
    function Invoke(Instance: TClass): TValue; overload;
    function IsChildOf(AParent:TQJson):Boolean;
    function IsParentOf(AChild:TQJson):Boolean;
    /// <summary>父结点</summary>
    property Parent:TQJson read FParent;
    ///<summary>结点类型</summary>
    /// <seealso>TQJsonDataType</seealso>
    property DataType:TQJsonDataType read FDataType write SetDataType;
    ///<summary>结点名称</summary>
    property Name:QStringW read FName;
    ///<summary>子结点数量</<summary>summary>
    property Count:Integer read GetCount;
    ///<summary>子结点数组</summary>
    property Items[AIndex:Integer]:TQJson read GetItems;default;
    ///<summary>子结点的值</summary>
    property Value:QStringW read GetValue write SetValue;
    ///<summary>判断是否是NULL类型</summary>
    property IsNull:Boolean read GetIsNull;
    ///<summary>判断是否是数字类型</summary>
    property IsNumeric:Boolean read GetIsNumeric;
    ///<summary>判断是否是日期时间类型</summary>
    property IsDateTime:Boolean read GetIsDateTime;
    ///<summary>判断是否是字符串类型</summary>
    property IsString:Boolean read GetIsString;
    ///<summary>判断是否是对象</summary>
    property IsObject:Boolean read GetIsObject;
    ///<summary>判断是否是数组</summary>
    property IsArray:Boolean read GetIsArray;
    ///<summary>将当前结点当作布尔类型访问</summary>
    property AsBoolean:Boolean read GetAsBoolean write SetAsBoolean;
    ///<summary>将当前结点当作整数类型来访问</summary>
    property AsInteger:Integer read GetAsInteger write SetAsInteger;
    ///<summary>将当前结点当作64位整数类型来访问</summary>
    property AsInt64:Int64 read GetAsInt64 write SetAsInt64;
    ///<summary>将当前结点当作浮点类型来访问</summary>
    property AsFloat:Extended read GetAsFloat write SetAsFloat;
    ///<summary>将当前结点当作日期时间类型来访问</summary>
    property AsDateTime:TDateTime read GetAsDateTime write SetAsDateTime;
    ///<summary>将当前结点当作字符串类型访问</summary>
    property AsString:QStringW read GetAsString write SetAsString;
    ///<summary>将当前结点当作一个对象字符串来访问</summary>
    property AsObject:QStringW read GetAsObject write SetAsObject;
    ///<summary>将当前结点当作一个字符串数组来访问</summary>
    property AsArray:QStringW read GetAsArray write SetAsArray;
    ///<summary>将自己当做Variant类型来访问</summary>
    property AsVariant:Variant read GetAsVariant write SetAsVariant;
    /// <summary>将自己当做Json对象来访问</summary>
    property AsJson:QStringW read GetAsJson write SetAsJson;
    //<summary>额外的附加数据成员，供用户关联附加内容</summary>
    property Data:Pointer read FData write FData;
    ///<summary>结点的路径，路径中间以"\"分隔</summary>
    property Path:QStringW read GetPath;
    ///<summary>在父结点中的索引顺序，从0开始，如果是-1，则代表自己是根结点</summary>
    property ItemIndex:Integer read GetItemIndex;
    ///<summary>名称哈希值</summary>
    property NameHash:Cardinal read FNameHash;
    //<summary>作为TValue值访问</summary>
    property AsValue:TValue read GetAsValue write SetAsValue;
  end;
  TQJsonEnumerator = class
  private
    FIndex: Integer;
    FList: TQJson;
  public
    constructor Create(AList: TQJson);
    function GetCurrent: TQJson; inline;
    function MoveNext: Boolean;
    property Current: TQJson read GetCurrent;
  end;

  TQHashedJson=class(TQJson)
  protected
    FHashTable:TQHashTable;
    function CreateJson:TQJson;override;
    procedure Replace(AIndex:Integer;ANewItem:TQJson);override;
  public
    constructor Create;overload;
    destructor Destroy;override;
    procedure Assign(ANode:TQJson);override;
    function Add(AName:QStringW):TQJson;override;
    function IndexOf(const AName:QStringW):Integer;override;
    procedure Delete(AIndex:Integer);override;
    procedure Clear;override;
  end;


var
  ///<summary>是否启用严格检查模式，在严格模式下：
  /// 1.名称或字符串必需使用双引号包含起来,如果为False，则名称可以没有引号或使用单引号。
  /// 2.注释不受支持，如果为False，则支持//行注释和/**/的块注释
  /// </summary>
  StrictJson:Boolean;
  /// <summary>日期类型转换为Json数据时会转换成字符串，这个变量控制如何格式化</summary>
  JsonDateFormat:QStringW;
  /// <summary>时间类型转换为Json数据时会转换成字符串，这个变量控制如何格式化</summary>
  JsonTimeFormat:QStringW;
  /// <summary>日期时间类型转换为Json数据时会转换成字符串，这个变量控制如何格式化</summary>
  JsonDateTimeFormat:QStringW;
  /// <summary>在ItemByName/ItemByPath/ValueByName/ValueByPath等函数的判断中，是否区分名称大小写</summary>
  JsonCaseSensitive:Boolean;
  /// 在需要新建一个TQJson对象时触发
  OnQJsonCreate:TQJsonCreateEvent;
  /// 在需要释放一个TQJson对象时触发
  OnQJsonFree:TQJsonFreeEvent;
implementation
uses variants,dateutils;
resourcestring
  SBadJson='当前内容不是有效的JSON字符串。';
  SNotArrayOrObject='%s 不是一个JSON数组或对象。';
  SVarNotArray='%s 的类型不是数组或对象。';
  SBadConvert='%s 不是一个有效的 %s 类型的值。';
  SCharNeeded='当前位置应该是 "%s" ，而不是 "%s"。';
  SBadNumeric='"%s"不是有效的数值。';
  SBadJsonTime='"%s"不是一个有效的日期时间值。';
  SNameNotFound='项目名称未找到。';
  SCommentNotSupport='严格模式下不支持注释，要解析包括注释的JSON内容，请将StrictJson变量设置为False。';
  SUnsupportArrayItem='添加的动态数组第%d个元素类型不受支持。';
  SBadStringStart='严格栻上JSON字符串必需以"开始。';
  SUnknownToken='无法识别的注释符，注释必需以//或/**/包括。';
  SNotSupport='函数 [%s] 在当前开发环境下不受支持。';
  SBadJsonArray='%s 不是一个有效的JSON数组定义。';
  SBadJsonObject='%s 不是一个有效的JSON对象定义。';
  SBadJsonEncoding='无效的编码，编码只能是UTF-8，ANSI，Unicode 16 LE，Unicode 16 BE。';
  SJsonParseError='第%d行第%d列:%s '#13#10'行:%s';
  SBadJsonName='%s 不是一个有效的JSON对象名称。';
  SObjectChildNeedName='对象 %s 的第 %d 个子结点名称未赋值，编码输出前必需赋值。';
  SReplaceTypeNeed='替换结点的类型要求是 %s 或其子类。';
const
  JsonTypeName:array [0..8] of QStringW=(
    'Unknown','Null','String','Integer','Float','Boolean','DateTime','Array','Object');
{ TQJson }

function TQJson.Add(AName: QStringW; AValue: Int64): TQJson;
begin
Result:=Add(AName,jdtInteger);
PInt64(PQCharW(Result.FValue))^:=AValue;
end;

function TQJson.Add(AName: QStringW; AValue: Extended): TQJson;
begin
Result:=Add(AName,jdtFloat);
PExtended(PQCharW(Result.FValue))^:=AValue;
end;

function TQJson.Add(AName: QStringW; AValue: Boolean): TQJson;
begin
Result:=Add(AName,jdtBoolean);
PBoolean(PQCharW(Result.FValue))^:=AValue;
end;

function TQJson.Add(AName: QStringW): TQJson;
begin
Result:=Add;
Result.FName:=AName;
end;

function TQJson.AddObject(AName: QStringW; AObject: TObject; ANest: Boolean;
  AcceptProps, AIgnoreProps: QStringW): TQJson;
var
  ATagData:TQJsonInternalTagData;
begin
ATagData.TagType:=ttNameFilter;
ATagData.Tag:=nil;
ATagData.AcceptNames:=AcceptProps;
ATagData.IgnoreNames:=AIgnoreProps;
Result:=AddObject(AName,AObject,InternalRttiFilter,ANest,@ATagData);
end;

function TQJson.AddObject(AName: QStringW; AObject: TObject): TQJson;
begin
Result:=AddObject(AName,AObject,TQJsonRttiFilterEvent(nil),false,nil);
end;

{$IFDEF QDAC_RTTI}
function TQJson.AddRecord<T>(AName: QStringW; const AObject: T): TQJson;
begin
Result:=Add(AName);
Result.InternalAddRecord(TypeInfo(T),@AObject,nil,nil);
end;


function TQJson.AddRecord<T>(AName: QStringW; const AObject: T; AcceptFields,
  AIgnoreFields: QStringW): TQJson;
var
  ATagData:TQJsonInternalTagData;
begin
ATagData.TagType:=ttNameFilter;
ATagData.Tag:=nil;
ATagData.AcceptNames:=AcceptFields;
ATagData.IgnoreNames:=AIgnoreFields;
Result:=Add(AName);
Result.InternalAddRecord(TypeInfo(T),@AObject,InternalRttiFilter,@ATagData);
end;

function TQJson.AddRecord<T>(AName: QStringW; const AObject: T;
  AOnFilter: TQJsonRttiFilterEvent; ATag: Pointer): TQJson;
begin
Result:=Add(AName);
Result.InternalAddRecord(TypeInfo(T),@AObject,AOnFilter,ATag);
end;

function TQJson.AddRecord<T>(AName: QStringW; const AObject: T;
  AOnFilter: TQJsonRttiFilterEventA; ATag: Pointer): TQJson;
var
  ATagData:TQJsonInternalTagData;
begin
ATagData.TagType:=ttAnonEvent;
ATagData.Tag:=ATag;
ATagData.OnEvent:=AOnFilter;
Result:=Add(AName);
Result.InternalAddRecord(TypeInfo(T),@AObject,InternalRttiFilter,@ATagData);
end;

//匿名函数支持
function TQJson.AddObject(AName: QStringW; AObject: TObject;
  AOnFilter: TQJsonRttiFilterEventA; ANest: Boolean; ATag: Pointer): TQJson;
var
  ATagData:TQJsonInternalTagData;
begin
if Assigned(AOnFilter) then
  begin
  ATagData.TagType:=ttAnonEvent;
  ATagData.OnEvent:=AOnFilter;
  ATagData.Tag:=ATag;
  Result:=AddObject(AName,AObject,InternalRttiFilter,ANest,@ATagData)
  end
else
  Result:=AddObject(AName,AObject,TQJsonRttiFilterEvent(nil),ANest,ATag);
end;
{$ENDIF QDAC_RTTI}

function TQJson.AddObject(AName:QStringW;AObject:TObject;
  AOnFilter:TQJsonRttiFilterEvent;ANest:Boolean;ATag:Pointer): TQJson;
  function GetObjectName(AObj:TObject):String;
  begin
  if AObj<>nil then
    begin
    {$IFDEF TYPENAMEASMETHODPREF}
    Result:=TObject(AObj).ClassName;
    {$ELSE}
    if TObject(AObj) is TComponent then
      Result:=TComponent(AObj).GetNamePath
    else if GetPropInfo(AObj,'Name')<>nil then
      Result:=GetStrProp(AObj,'Name');
    if Length(Result)=0 then
      Result:=TObject(AObj).ClassName;
    {$ENDIF TYPENAMEASMETHODPREF}
    end
  else
    SetLength(Result,0);
  end;

  function GetMethodName(AMethod:TMethod):String;
  var
    AMethodName:String;
  begin
  if AMethod.Data<>nil then
    begin
    Result:=GetObjectName(AMethod.Data);
    AMethodName:=TObject(AMethod.Data).MethodName(AMethod.Code);
    {$IFDEF CPUX64}
    if Length(Result)=0 then
      Result:=IntToHex(Int64(AMethod.Data),16);
    if Length(AMethodName)=0 then
      AMethodName:=IntToHex(Int64(AMethod.Code),16);
    {$ELSE}
    if Length(Result)=0 then
      Result:=IntToHex(IntPtr(AMethod.Data),8);
    if Length(AMethodName)=0 then
      AMethodName:=IntToHex(IntPtr(AMethod.Code),8);
    {$ENDIF CPUX64}
    Result:=Result+'.'+AMethodName;
    end
  else if AMethod.Code<>nil then
    begin
    {$IFDEF CPUX64}
    Result:=IntToHex(Int64(AMethod.Code),16);
    {$ELSE}
    Result:=IntToHex(IntPtr(AMethod.Code),8);
    {$ENDIF CPUX64}
    end
  else
    SetLength(Result,0);
  end;

  procedure AddChildren(AParent:TQJson;AObj:TObject);
  var
    AList:PPropList;
    ACount:Integer;
    I:Integer;
    AChild:TQJson;
    ACharVal:QStringA;
    V:Variant;
    Accept:Boolean;
  begin
  if AObj=nil then
    Exit;
  if PTypeInfo(AObject.ClassInfo)=nil then//对象没有RTTI信息
    Exit;
  AList:=nil;
  ACount:=GetPropList(AObj,AList);
  try
  for I := 0 to ACount-1 do
    begin
    if Assigned(AOnFilter) then
      begin
      Accept:=True;
      {$IFDEF QDAC_RTTI_NAMEFIELD}
      AOnFilter(AParent,AObj,AList[I].NameFld.ToString,AList[I].PropType^,Accept,ATag);
      {$ELSE}
      AOnFilter(AParent,AObj,AList[I].Name,AList[I].PropType^,Accept,ATag);
      {$ENDIF QDAC_RTTI_NAMEFIELD}
      if not Accept then
        Continue;
      end;
    {$IFDEF QDAC_RTTI_NAMEFIELD}
    AChild:=AParent.Add(AList[I].NameFld.ToString);
    {$ELSE}
    AChild:=AParent.Add(AList[I].Name);
    {$ENDIF QDAC_RTTI_NAMEFIELD}
    case AList[I].PropType^.Kind of
      tkChar:
        begin
        ACharVal.Length:=1;
        ACharVal.Chars[0]:=GetOrdProp(AObj, AList[I]);
        AChild.AsString:=ACharVal;
        end;
      tkWChar:
        AChild.AsString:=QCharW(GetOrdProp(AObj, AList[I]));
      tkInteger:
        AChild.AsInteger:=GetOrdProp(AObj, AList[I]);
      tkClass:
        begin
        if ANest then
          AddChildren(AChild,TObject(GetOrdProp(AObj,AList[I])))
        else
          AChild.AsString:=GetObjectName(TObject(GetOrdProp(AObj,AList[I])));
        end;
      tkEnumeration:
        begin
        if GetTypeData(AList[I]^.PropType^)^.BaseType^=TypeInfo(Boolean) then
          AChild.AsBoolean:=Boolean(GetOrdProp(AObj,AList[I]))
        else
          AChild.AsString:=GetEnumProp(AObj,AList[I]);
        end;
      tkSet:
        AChild.AsString:='['+GetSetProp(AObj,AList[I])+']';
      tkFloat:
        AChild.AsFloat:=GetFloatProp(AObj, AList[I]);
      tkMethod:
        AChild.AsString:=GetMethodName(GetMethodProp(AObj,AList[I]));
      {$IFNDEF NEXTGEN}
      tkString, tkLString:
        AChild.AsString := GetStrProp(AObj, AList[I]);
      tkWString:
        AChild.AsString :=GetWideStrProp(AObj, AList[I]);
      {$ENDIF !NEXTGEN}
      {$IFDEF QDAC_UNICODE}
      tkUString:
        AChild.AsString := GetStrProp(AObj, AList[I]);
      {$ENDIF QDAC_UNICODE}
      tkVariant:
        AChild.AsVariant := GetVariantProp(AObj, AList[I]);
      tkInt64:
        AChild.AsInt64 := GetInt64Prop(AObj, AList[I]);
      tkDynArray:
        begin
        DynArrayToVariant(V,GetDynArrayProp(AObj, AList[I]),AList[I].PropType^);
        AChild.AsVariant:=V;
        end;
    end;//End case
    end;//End for
  finally
    if AList<>nil then
      FreeMem(AList);
  end;
  end;
begin
//利用RTTI直接获取对象的属性信息并保存到结果中
Result:=Add(AName);
AddChildren(Result,AObject);
end;

function TQJson.AddDateTime(AName: QStringW; AValue: TDateTime): TQJson;
begin
Result:=Add;
Result.FName:=AName;
Result.DataType:=jdtString;
Result.AsDateTime:=AValue;
end;

function TQJson.Add: TQJson;
begin
Result:=CreateJson;
Add(Result);
end;

function TQJson.Add(ANode: TQJson): Integer;
begin
ArrayNeeded(jdtObject);
Result:=FItems.Add(ANode);
ANode.FParent:=Self;
end;

function TQJson.Add(AName, AValue: QStringW;
  ADataType: TQJsonDataType): Integer;
var
  ANode:TQJson;
  p:PQCharW;
  ABuilder:TQStringCatHelperW;
  procedure AddAsDateTime;
  var
    ATime:TDateTime;
  begin
  if ParseDateTime(PQCharW(AValue),ATime) then
    ANode.AsDateTime:=ATime
  else if ParseJsonTime(PQCharW(AValue),ATime) then
    ANode.AsDateTime:=ATime
  else
    raise Exception.Create(SBadJsonTime);
  end;
begin
ANode:=CreateJson;
ANode.FName:=AName;
Result:=Add(ANode);
p:=PQCharW(AValue);
if ADataType=jdtUnknown then
  begin
  ABuilder:=TQStringCatHelperW.Create;
  try
    ANode.ParseValue(ABuilder,p);
  except
    ANode.AsString:=AValue;
  end;
  FreeObject(ABuilder);
  end
else
  begin
  case ADataType of
    jdtString:
      ANode.AsString:=AValue;
    jdtInteger:
      ANode.AsInteger:=StrToInt(AValue);
    jdtFloat:
      ANode.AsFloat:=StrToFloat(AValue);
    jdtBoolean:
      ANode.AsBoolean:=StrToBool(AValue);
    jdtDateTime:
      AddAsDateTime;
    jdtArray:
      begin
      if p^<>'[' then
        raise Exception.CreateFmt(SBadJsonArray,[Value]);
      ANode.ParseObject(p);
      end;
    jdtObject:
      begin
      if p^<>'{' then
        raise Exception.CreateFmt(SBadJsonObject,[Value]);
      ANode.ParseObject(p);
      end;
  end;

  end;
end;

function TQJson.Add(AName: QStringW; ADataType: TQJsonDataType): TQJson;
begin
Result:=Add(AName);
Result.DataType:=ADataType;
end;

function TQJson.Add(const AName: QStringW; AItems: array of const): TQJson;
var
  I:Integer;
begin
Result:=Add(AName,True);
Result.DataType:=jdtArray;
for I := Low(AItems) to High(AItems) do
  begin
  case AItems[I].VType of
    vtInteger:
      Result.Add.AsInteger:=AItems[I].VInteger;
    vtBoolean:
      Result.Add.AsBoolean:=AItems[I].VBoolean;
    {$IFNDEF NEXTGEN}
    vtChar:
      Result.Add.AsString:=QStringW(AItems[I].VChar);
    {$ENDIF !NEXTGEN}
    vtExtended:
      Result.Add.AsFloat:=AItems[I].VExtended^;
    {$IFNDEF NEXTGEN}
    vtPChar:
      Result.Add.AsString:=QStringW(AItems[I].VPChar);
    vtString:
      Result.Add.AsString:=QStringW(AItems[I].VString^);
    vtAnsiString:
      Result.Add.AsString:=QStringW(
        {$IFDEF QDAC_UNICODE}
        PAnsiString(AItems[I].VAnsiString)^
        {$ELSE}
        AItems[I].VPChar
        {$ENDIF QDAC_UNICODE}
        );
    vtWideString:
      Result.Add.AsString:=PWideString(AItems[I].VWideString)^;
    {$ENDIF !NEXTGEN}
    vtPointer:
      Result.Add.AsInt64:=IntPtr(AItems[I].VPointer);
    vtWideChar:
      Result.Add.AsString:=AItems[I].VWideChar;
    vtPWideChar:
      Result.Add.AsString:=AItems[I].VPWideChar;
    vtCurrency:
      Result.Add.AsFloat:=AItems[I].VCurrency^;
    vtInt64:
      Result.Add.AsInt64:=AItems[I].VInt64^;
    {$IFDEF QDAC_UNICODE}       //variants
    vtUnicodeString:
      Result.Add.AsString:=AItems[I].VPWideChar;
    {$ENDIF QDAC_UNICODE}
    vtVariant:
      Result.Add.AsVariant:=AItems[I].VVariant^;
    vtObject:
      begin
      if TObject(AItems[I].VObject) is TQJson then
        Result.Add(TObject(AItems[I].VObject) as TQJson)
      else
        raise Exception.Create(Format(SUnsupportArrayItem,[I]));
      end
    else
      raise Exception.Create(Format(SUnsupportArrayItem,[I]));
  end;//End case
  end;//End for
end;

procedure TQJson.ArrayNeeded(ANewType: TQJsonDataType);
begin
if not (DataType in [jdtArray,jdtObject]) then
  begin
  FDataType:=ANewType;
  ValidArray;
  end;
end;

procedure TQJson.Assign(ANode: TQJson);
var
  I:Integer;
begin
FName:=ANode.FName;
FNameHash:=ANode.FNameHash;
if ANode.FDataType in [jdtArray,jdtObject] then
  begin
  DataType:=ANode.FDataType;
  Clear;
  for I := 0 to ANode.Count - 1 do
    begin
    Add.Assign(ANode[I])
    end;
  end
else
  CopyValue(ANode);
end;

function TQJson.BoolToStr(const b: Boolean): QStringW;
begin
if b then
  Result:='true'
else
  Result:='false';
end;

procedure TQJson.BuildJsonString(ABuilder: TQStringCatHelperW; var p: PQCharW);
var
  AQuoter:QCharW;
  ps:PQCharW;
begin
ABuilder.Position:=0;
if (p^='"') or (p^='''')  then
  begin
  AQuoter:=p^;
  Inc(p);
  ps:=p;
  while p^<>#0 do
    begin
    if (p^=AQuoter) then
      begin
      if ps<>p then
        ABuilder.Cat(ps,p-ps);
      if p[1]=AQuoter then
        begin
        ABuilder.Cat(AQuoter);
        Inc(p,2);
        ps:=p;
        end
      else
        begin
        Inc(p);
        SkipSpaceW(p);
        ps:=p;
        Break;
        end;
      end
    else if p^='\' then
      begin
      if ps<>p then
        ABuilder.Cat(ps,p-ps);
      ABuilder.Cat(CharUnescape(p));
      ps:=p;
      end
    else
      Inc(p);
    end;
  if ps<>p then
    ABuilder.Cat(ps,p-ps);
  end
else
  begin
  while p^<>#0 do
    begin
    if (p^=':') or (p^=']') or (p^=',') or (p^='}') then
      Break
    else
      ABuilder.Cat(p,1);
    Inc(p);
    end
  end;
end;

function TQJson.CharEscape(c: QCharW; pd: PQCharW): Integer;
begin
case c of
  #7:
    begin
    pd[0]:='\';
    pd[1]:='b';
    Result:=2;
    end;
  #9:
    begin
    pd[0]:='\';
    pd[1]:='t';
    Result:=2;
    end;
  #10:
    begin
    pd[0]:='\';
    pd[1]:='n';
    Result:=2;
    end;
  #12:
    begin
    pd[0]:='\';
    pd[1]:='f';
    Result:=2;
    end;
  #13:
    begin
    pd[0]:='\';
    pd[1]:='r';
    Result:=2;
    end;
  '\':
    begin
    pd[0]:='\';
    pd[1]:='\';
    Result:=2;
    end;
  '''':
    begin
    pd[0]:='\';
    pd[1]:='''';
    Result:=2;
    end;
  '"':
    begin
    pd[0]:='\';
    pd[1]:='"';
    Result:=2;
    end;
  '/':
    begin
    pd[0]:='\';
    pd[1]:='/';
    Result:=2;
    end
  else
    begin
    pd[0]:=c;
    Result:=1;
    end;
end;
end;

function TQJson.CharUnescape(var p: PQCharW): QCharW;
  function DecodeOrd:Integer;
  var
    C:Integer;
  begin
  Result:=0;
  C:=0;
  while (p^<>#0) and (C<4) do
    begin
    if IsHexChar(p^) then
      Result:=(Result shl 4)+HexValue(p^)
    else
      Break;
    Inc(p);
    Inc(C);
    end
  end;
begin
if p^=#0 then
  begin
  Result:=#0;
  Exit;
  end;
if p^<>'\' then
  begin
  Result:=p^;
  Inc(p);
  Exit;
  end;
Inc(p);
case p^ of
  'b':
    begin
    Result:=#7;
    Inc(p);
    end;
  't':
    begin
    Result:=#9;
    Inc(p);
    end;
  'n':
    begin
    Result:=#10;
    Inc(p);
    end;
  'f':
    begin
    Result:=#12;
    Inc(p);
    end;
  'r':
    begin
    Result:=#13;
    Inc(p);
    end;
  '\':
    begin
    Result:='\';
    Inc(p);
    end;
  '''':
    begin
    Result:='''';
    Inc(p);
    end;
  '"':
    begin
    Result:='"';
    Inc(p);
    end;
  'u':
    begin
    //\uXXXX
    if IsHexChar(p[1]) and IsHexChar(p[2]) and IsHexChar(p[3]) and IsHexChar(p[4]) then
      begin
      Result:=WideChar((HexValue(p[1]) shl 12) or (HexValue(p[2]) shl 8) or
        (HexValue(p[3]) shl 4) or HexValue(p[4]));
      Inc(p,5);
      end
    else
      raise Exception.CreateFmt(SCharNeeded,['0-9A-Fa-f',StrDupW(p,0,4)]);
    end;
  '/':
    begin
    Result:='/';
    Inc(p);
    end
  else
    begin
    if StrictJson then
      raise Exception.CreateFmt(SCharNeeded,['btfrn"u''/',StrDupW(p,0,4)])
    else
      begin
      Result:=p^;
      Inc(p);
      end;
    end;
  end;
end;

procedure TQJson.Clear;
var
  I:Integer;
begin
if FDataType in [jdtArray,jdtObject] then
  begin
  for I := 0 to Count - 1 do
    FreeJson(FItems[I]);
  FItems.Clear;
  end;
end;

function TQJson.Clone: TQJson;
begin
Result:=Copy;
end;

function TQJson.Copy: TQJson;
begin
Result:=CreateJson;
Result.Assign(Self);
end;
{$IFDEF QDAC_RTTI}
function TQJson.CopyIf(const ATag: Pointer;
  AFilter: TQJsonFilterEventA): TQJson;
  procedure NestCopy(AParentSource,AParentDest:TQJson);
  var
    I:Integer;
    Accept:Boolean;
    AChildSource,AChildDest:TQJson;
  begin
  for I := 0 to AParentSource.Count-1 do
    begin
    Accept:=True;
    AChildSource:=AParentSource[I];
    AFilter(Self,AChildSource,Accept,ATag);
    if Accept then
      begin
      AChildDest:=AParentDest.Add(AChildSource.FName,AChildSource.DataType);
      if AChildSource.DataType in [jdtArray,jdtObject] then
        begin
        AChildDest.DataType:=AChildSource.DataType;
        NestCopy(AChildSource,AChildDest)
        end
      else
        AChildDest.CopyValue(AChildSource);
      end;
    end;
  end;
begin
if Assigned(AFilter) then
  begin
  Result:=CreateJson;
  Result.FName:=Name;
  if DataType in [jdtObject,jdtArray] then
    begin
    NestCopy(Self,Result);
    end
  else
    Result.CopyValue(Self);
  end
else
  Result:=Copy;
end;
{$ENDIF QDAC_RTTI}
function TQJson.CopyIf(const ATag: Pointer; AFilter: TQJsonFilterEvent): TQJson;
  procedure NestCopy(AParentSource,AParentDest:TQJson);
  var
    I:Integer;
    Accept:Boolean;
    AChildSource,AChildDest:TQJson;
  begin
  for I := 0 to AParentSource.Count-1 do
    begin
    Accept:=True;
    AChildSource:=AParentSource[I];
    AFilter(Self,AChildSource,Accept,ATag);
    if Accept then
      begin
      AChildDest:=AParentDest.Add(AChildSource.FName,AChildSource.DataType);
      if AChildSource.DataType in [jdtArray,jdtObject] then
        NestCopy(AChildSource,AChildDest)
      else
        AChildDest.CopyValue(AChildSource);
      end;
    end;
  end;
begin
if Assigned(AFilter) then
  begin
  Result:=CreateJson;
  Result.FName:=Name;
  if DataType in [jdtObject,jdtArray] then
    begin
    NestCopy(Self,Result);
    end
  else
    Result.CopyValue(Self);
  end
else
  Result:=Copy;
end;

procedure TQJson.CopyValue(ASource: TQJson);
var
  L:Integer;
begin
L:=Length(ASource.FValue);
DataType:=ASource.DataType;
SetLength(FValue,L);
if L>0 then
  Move(PQCharW(ASource.FValue)^,PQCharW(FValue)^,L shl 1);
end;

constructor TQJson.Create(const AName, AValue: QStringW;
  ADataType: TQJsonDataType);
begin
inherited Create;
FName:=AName;
if ADataType<>jdtUnknown then
  DataType:=ADataType;
Value:=AValue;
end;

function TQJson.CreateJson: TQJson;
begin
if Assigned(OnQJsonCreate) then
  Result:=OnQJsonCreate
else
  Result:=TQJson.Create;
end;

constructor TQJson.Create;
begin
inherited;
end;

procedure TQJson.Delete(AName: QStringW);
var
  I:Integer;
begin
I:=IndexOf(AName);
if I<>-1 then
  Delete(I);
end;
{$IFDEF QDAC_RTTI}
procedure TQJson.DeleteIf(const ATag: Pointer; ANest: Boolean;
  AFilter: TQJsonFilterEventA);
  procedure DeleteChildren(AParent:TQJson);
  var
    I:Integer;
    Accept:Boolean;
    AChild:TQJson;
  begin
  I:=0;
  while I<AParent.Count do
    begin
    Accept:=True;
    AChild:=AParent.Items[I];
    if ANest then
      DeleteChildren(AChild);
    AFilter(Self,AChild,Accept,ATag);
    if Accept then
      AParent.Delete(I)
    else
      Inc(I);
    end;
  end;
begin
if Assigned(AFilter) then
  DeleteChildren(Self)
else
  Clear;
end;
{$ENDIF QDAC_RTTI}
procedure TQJson.DeleteIf(const ATag: Pointer; ANest: Boolean;
  AFilter: TQJsonFilterEvent);
  procedure DeleteChildren(AParent:TQJson);
  var
    I:Integer;
    Accept:Boolean;
    AChild:TQJson;
  begin
  I:=0;
  while I<AParent.Count do
    begin
    Accept:=True;
    AChild:=AParent.Items[I];
    if ANest then
      DeleteChildren(AChild);
    AFilter(Self,AChild,Accept,ATag);
    if Accept then
      AParent.Delete(I)
    else
      Inc(I);
    end;
  end;
begin
if Assigned(AFilter) then
  DeleteChildren(Self)
else
  Clear;
end;

procedure TQJson.Delete(AIndex: Integer);
begin
if FDataType in [jdtArray,jdtObject] then
  begin
  FreeJson(Items[AIndex]);
  FItems.Delete(AIndex);
  end
else
  raise Exception.Create(Format(SNotArrayOrObject,[FName]));
end;

destructor TQJson.Destroy;
begin
if DataType in [jdtArray,jdtObject] then
  begin
  Clear;
  FreeObject(FItems);
  end;
inherited;
end;

function TQJson.Encode(ADoFormat: Boolean; AIndent: QStringW): QStringW;
var
  ABuilder:TQStringCatHelperW;
begin
ABuilder:=TQStringCatHelperW.Create;
try
  InternalEncode(ABuilder,ADoFormat,AIndent);
  ABuilder.Back(1);//删除最后一个逗号
  Result:=ABuilder.Value;
finally
  FreeObject(ABuilder);
end;
end;
{$IFDEF QDAC_RTTI}
function TQJson.FindIf(const ATag: Pointer; ANest: Boolean;
  AFilter: TQJsonFilterEventA): TQJson;
  function DoFind(AParent:TQJson):TQJson;
  var
    I:Integer;
    AChild:TQJson;
    Accept:Boolean;
  begin
  Result:=nil;
  for I := 0 to AParent.Count-1 do
    begin
    AChild:=AParent[I];
    Accept:=True;
    AFilter(Self,AChild,Accept,ATag);
    if Accept then
      Result:=AChild
    else if ANest then
      Result:=DoFind(AChild);
    if Result<>nil then
      Break;
    end;
  end;
begin
if Assigned(AFilter) then
  Result:=DoFind(Self)
else
  Result:=nil;
end;
{$ENDIF QDAC_RTTI}

function TQJson.FindIf(const ATag: Pointer; ANest: Boolean;
  AFilter: TQJsonFilterEvent): TQJson;
  function DoFind(AParent:TQJson):TQJson;
  var
    I:Integer;
    AChild:TQJson;
    Accept:Boolean;
  begin
  Result:=nil;
  for I := 0 to AParent.Count-1 do
    begin
    AChild:=AParent[I];
    Accept:=True;
    AFilter(Self,AChild,Accept,ATag);
    if Accept then
      Result:=AChild
    else if ANest then
      Result:=DoFind(AChild);
    if Result<>nil then
      Break;
    end;
  end;
begin
if Assigned(AFilter) then
  Result:=DoFind(Self)
else
  Result:=nil;
end;

function TQJson.ForcePath(APath: QStringW): TQJson;
var
  AName:QStringW;
  p,pn,ws:PQCharW;
  AParent:TQJson;
  L:Integer;
  AIndex:Int64;
const
  PathDelimiters:PWideChar='./\';
begin
p:=PQCharW(APath);
AParent:=Self;
Result:=Self;
while p^<>#0 do
  begin
  AName:=DecodeTokenW(p,PathDelimiters,WideChar(0),True);
  if not (AParent.DataType in [jdtObject,jdtArray]) then
    AParent.DataType:=jdtObject;
  Result:=AParent.ItemByName(AName);
  if not Assigned(Result) then
    begin
    pn:=PQCharW(AName);
    L:=Length(AName);
    AIndex:=-1;
    if (pn[L-1]=']') then
      begin
      repeat
        if pn[L]='[' then
          begin
          ws:=pn+L+1;
          ParseInt(ws,AIndex);
          Break;
          end
        else
          Dec(L);
      until L=0;
      if L>0 then
        begin
        AName:=StrDupX(pn,L);
        Result:=AParent.ItemByName(AName);
        if Result=nil then
          Result:=AParent.Add(AName,jdtArray)
        else if Result.DataType<>jdtArray then
          raise Exception.CreateFmt(SBadJsonArray,[AName]);
        if AIndex>=0 then
          begin
          while Result.Count<=AIndex do
            Result.Add;
          Result:=Result[AIndex];
          end;
        end
      else
        raise Exception.CreateFmt(SBadJsonName,[AName]);
      end
    else
      Result:=AParent.Add(AName);
    end;
  AParent:=Result;
  end;
end;

procedure TQJson.FreeJson(AJson:TQJson);
begin
if Assigned(OnQJsonFree) then
  OnQJsonFree(AJson)
else
  FreeObject(AJson);
end;

function TQJson.GetAsArray: QStringW;
begin
if DataType=jdtArray then
  Result:=Value
else
  raise Exception.Create(Format(SBadConvert,[AsString,'Array']));
end;

function TQJson.GetAsBoolean: Boolean;
begin
if DataType = jdtBoolean then
  Result:=PBoolean(FValue)^
else if DataType = jdtString then
  begin
  if not TryStrToBool(FValue,Result) then
    raise Exception.Create(Format(SBadConvert,[FValue,'Boolean']));
  end
else if DataType in [jdtFloat,jdtDateTime] then
  Result:=not SameValue(AsFloat,0)
else if DataType = jdtInteger then
  Result:=AsInt64<>0
else
  raise Exception.Create(Format(SBadConvert,[JsonTypeName[Integer(DataType)],'Boolean']));
end;

function TQJson.GetAsDateTime: TDateTime;
begin
if DataType in [jdtDateTime,jdtFloat] then
  Result:=PExtended(FValue)^
else if DataType=jdtString then
  begin
  if not (ParseDateTime(PWideChar(FValue),Result) or ParseJsonTime(PWideChar(FValue),Result)
    or ParseWebTime(PQCharW(FValue),Result)) then
      raise Exception.Create(Format(SBadConvert,['String','DateTime']))
  end
else if DataType=jdtInteger then
  Result:=AsInt64
else
  raise Exception.Create(Format(SBadConvert,[JsonTypeName[Integer(DataType)],'DateTime']));
end;

function TQJson.GetAsFloat: Extended;
begin
if DataType in [jdtFloat,jdtDateTime] then
  Result:=PExtended(FValue)^
else if DataType = jdtBoolean then
  Result:=Integer(AsBoolean)
else if DataType = jdtString then
  begin
  if not TryStrToFloat(FValue,Result) then
    raise Exception.Create(Format(SBadConvert,[FValue,'Numeric']));
  end
else if DataType =  jdtInteger then
  Result:=AsInt64
else if DataType = jdtNull then
  Result:=0
else
  raise Exception.Create(Format(SBadConvert,[JsonTypeName[Integer(DataType)],'Numeric']))
end;

function TQJson.GetAsInt64: Int64;
begin
if DataType =  jdtInteger then
  Result:=PInt64(FValue)^
else if DataType in [jdtFloat,jdtDateTime] then
  Result:=Trunc(PExtended(FValue)^)
else if DataType = jdtBoolean then
  Result:=Integer(AsBoolean)
else if DataType = jdtString then
  Result:=Trunc(AsFloat)
else if DataType = jdtNull then
  Result:=0
else
  raise Exception.Create(Format(SBadConvert,[JsonTypename[Integer(DataType)],'Numeric']))
end;

function TQJson.GetAsInteger: Integer;
begin
Result:=GetAsInt64;
end;

function TQJson.GetAsJson: QStringW;
begin
Result:=Encode(True,'  ');
end;

function TQJson.GetAsObject: QStringW;
begin
if DataType=jdtObject then
  Result:=Value
else
  raise Exception.Create(Format(SBadConvert,[AsString,'Object']));
end;

function TQJson.GetAsString: QStringW;
begin
Result:=Value;
end;

function TQJson.GetAsValue: TValue;
  procedure AsDynValueArray;
  var
    AValues:TValueArray;
    I:Integer;
  begin
  SetLength(AValues,Count);
  for I := 0 to Count-1 do
    AValues[I]:=Items[I].AsValue;
  Result:=TValue.FromArray(TypeInfo(TValueArray),AValues);
  end;
begin
case DataType of
  jdtString:
    Result:=AsString;
  jdtInteger:
    Result:=AsInt64;
  jdtFloat:
    Result:=AsFloat;
  jdtDateTime:
    Result:=AsDateTime;
  jdtBoolean:
    Result:=AsBoolean;
  jdtArray,jdtObject://数组和对象都只能当成数组来处理
    AsDynValueArray
  else
    Result:=TValue.Empty;
end;
end;

function TQJson.GetAsVariant: Variant;
var
  I:Integer;
begin
case DataType of
  jdtString:
    Result:=AsString;
  jdtInteger:
    Result:=AsInt64;
  jdtFloat:
    Result:=AsFloat;
  jdtDateTime:
    Result:=AsDateTime;
  jdtBoolean:
    Result:=AsBoolean;
  jdtArray,jdtObject:
    begin
    Result:=VarArrayCreate([0,Count-1],varVariant);
    for I := 0 to Count-1 do
      Result[I]:=Items[I].AsVariant;
    end
  else
    VarClear(Result);
end;
end;

function TQJson.GetCount: Integer;
begin
if DataType in [jdtObject,jdtArray] then
  Result:=FItems.Count
else
  Result:=0;
end;

function TQJson.GetEnumerator: TQJsonEnumerator;
begin
Result:=TQJsonEnumerator.Create(Self);
end;

function TQJson.GetIsArray: Boolean;
begin
Result:=(DataType=jdtArray);
end;

function TQJson.GetIsDateTime: Boolean;
var
  ATime:TDateTime;
begin
Result:=(DataType=jdtDateTime);
if not Result then
  begin
  if DataType=jdtString then
    Result:=ParseDateTime(PQCharW(FValue),ATime) or
      ParseJsonTime(PQCharW(FValue),ATime) or ParseWebTime(PQCharW(FValue),ATime)
  end;
end;

function TQJson.GetIsNull: Boolean;
begin
Result:=(DataType=jdtNull);
end;

function TQJson.GetIsNumeric: Boolean;
begin
Result:=(DataType in [jdtInteger,jdtFloat]);
end;

function TQJson.GetIsObject: Boolean;
begin
Result:=(DataType=jdtObject);
end;

function TQJson.GetIsString: Boolean;
begin
Result:=(DataType=jdtString);
end;

function TQJson.GetItemIndex: Integer;
var
  I:Integer;
begin
Result:=-1;
if Assigned(Parent) then
  begin
  for I := 0 to Parent.Count-1 do
    begin
    if Parent.Items[I]=Self then
      begin
      Result:=I;
      Break;
      end;
    end;
  end;
end;

function TQJson.GetItems(AIndex: Integer): TQJson;
begin
Result:=FItems[AIndex];
end;

function TQJson.GetPath: QStringW;
var
  AParent,AItem:TQJson;
begin
AParent:=FParent;
AItem:=Self;
repeat
  if Assigned(AParent) and AParent.IsArray then
    Result:='['+IntToStr(AItem.ItemIndex)+']'+Result
  else if AItem.IsArray then
    Result:='\'+AItem.FName+Result
  else
    Result:='\'+AItem.FName+Result;
  AItem:=AParent;
  AParent:=AItem.Parent;
until AParent=nil;
if Length(Result)>0 then
  Result:=StrDupX(PQCharW(Result)+1,Length(Result)-1);
end;

function TQJson.GetValue: QStringW;
  procedure ValueAsDateTime;
  var
    ADate:Integer;
    AValue:Extended;
  begin
  AValue:=PExtended(FValue)^;
  ADate:=Trunc(AValue);
  if SameValue(ADate,0) then//Date为0，是时间
    begin
    if SameValue(AValue,0) then
      Result:=FormatDateTime(JsonDateFormat,AValue)
    else
      Result:=FormatDateTime(JsonTimeFormat,AValue);
    end
  else
    begin
    if SameValue(AValue-ADate,0) then
      Result:=FormatDateTime(JsonDateFormat,AValue)
    else
      Result:=FormatDateTime(JsonDateTimeFormat,AValue);
    end;
  end;
begin
case DataType of
  jdtNull,jdtUnknown:
    Result:='null';
  jdtString:
    Result:=FValue;
  jdtInteger:
    Result:=IntToStr(PInt64(FValue)^);
  jdtFloat:
    Result:=FloatToStr(PExtended(FValue)^);
  jdtDateTime:
    ValueAsDateTime;
  jdtBoolean:
    Result:=BoolToStr(PBoolean(FValue)^);
  jdtArray,jdtObject:
    Result:=Encode(True);
end;
end;

function TQJson.IndexOf(const AName: QStringW): Integer;
var
  I,L:Integer;
  AItem:TQJson;
  AHash:Cardinal;
begin
Result:=-1;
L:=Length(AName);
if L>0 then
  AHash:=HashOf(PQCharW(AName),L shl 1)
else
  AHash:=0;
for I := 0 to Count - 1 do
  begin
  AItem:=Items[I];
  if Length(AItem.FName)=L then
    begin
    if JsonCaseSensitive then
      begin
      if AItem.FNameHash=0 then
        AItem.FNameHash:=HashOf(PQCharW(AItem.FName),L shl 1);
      if AItem.FNameHash=AHash then
        begin
        if AItem.FName=AName then
          begin
          Result:=I;
          break;
          end;
        end;
      end
    else if StartWithW(PQCharW(AItem.FName),PQCharW(AName),True) then
      begin
      Result:=I;
      break;
      end;
    end;
  end;
end;
{$IFDEF QDAC_RTTI}
function TQJson.InternalAddRecord(ATypeInfo: PTypeInfo;
  ABaseAddr: Pointer;AOnFilter:TQJsonRttiFilterEvent;ATag:Pointer): Boolean;
var
  AContext:TRttiContext;
  AType:TRttiType;
  I:Integer;
  AFields:TArray<TRttiField>;
  AChild:TQJson;
  Accept:Boolean;
  function ValueToVariant(AValue:TValue):Variant;
  var
    J,L:Integer;
    AItemValue:TValue;
  begin
  L:=AValue.GetArrayLength;
  Result:=VarArrayCreate([0,L-1],varVariant);
  for J := 0 to L-1 do
    begin
    AItemValue:=AValue.GetArrayElement(J);
    if AItemValue.IsArray then
      Result[J]:=ValueToVariant(AItemValue)
    else
      Result[J]:=AItemValue.AsVariant;
    end;
  end;

  function ParseDynArray:Variant;
  var
    AValue:TValue;
  begin
  AValue:=AFields[I].GetValue(ABaseAddr);
  if AValue.IsArray then
    Result:=ValueToVariant(AValue)
  else
    VarClear(Result);
  end;
begin
Result:=False;
if Assigned(ATypeInfo) then
  begin
  AType:=AContext.GetType(ATypeInfo);
  if AType<>nil then
    begin
    if AType.TypeKind=tkRecord then
      begin
      Result:=True;
      AFields:=AType.GetFields;
      for I := Low(AFields) to High(AFields) do
        begin
        Accept:=True;
        if Assigned(AOnFilter) then
          AOnFilter(Self,ABaseAddr,AFields[I].Name,AFields[I].FieldType.Handle,Accept,ATag);
        if not Accept then
          Continue;
        AChild:=Add(AFields[I].Name);
        case AFields[I].FieldType.TypeKind of
          tkInteger,tkInt64:
            AChild.AsInt64:=AFields[I].GetValue(ABaseAddr).AsInt64;
          tkUString{$IFNDEF NEXTGEN},tkString,tkLString,tkWString,tkChar,tkWChar{$ENDIF !NEXTGEN}:
            AChild.AsString:=AFields[I].GetValue(ABaseAddr).ToString;
          tkRecord:
            AChild.InternalAddRecord(AFields[I].FieldType.Handle,Pointer(IntPtr(ABaseAddr)+AFields[I].Offset),AOnFilter,ATag);
          tkEnumeration:
            AChild.AsString:=AFields[I].GetValue(ABaseAddr).ToString;
          tkSet:
            AChild.AsString:=AFields[I].GetValue(ABaseAddr).ToString;
          tkFloat:
            begin
            if (AFields[I].FieldType.Handle=TypeInfo(TDate)) or
              (AFields[I].FieldType.Handle=TypeInfo(TTime)) or
              (AFields[I].FieldType.Handle=TypeInfo(TDateTime))
             then
              AChild.AsDateTime:=AFields[I].GetValue(ABaseAddr).AsExtended
            else
              AChild.AsFloat:=AFields[I].GetValue(ABaseAddr).AsExtended;
            end;
          tkVariant:
            AChild.AsVariant:=AFields[I].GetValue(ABaseAddr).AsVariant;
          tkArray,tkDynArray:
            AChild.AsVariant:=ParseDynArray;
          tkClass,tkMethod,tkInterface,tkClassRef,tkPointer,tkProcedure:
            AChild.AsString:='<OBJECT>';
        end;
        end;
      end;
    end;
  end;
end;
{$ENDIF QDAC_RTTI}

function TQJson.InternalEncode(ABuilder: TQStringCatHelperW; ADoFormat: Boolean;
  const AIndent: QStringW): TQStringCatHelperW;
  procedure CatValue(const AValue:QStringW);
  var
    ps:PQCharW;
  const
    CharNum1:PWideChar='1';
    CharNum0:PWideChar='0';
    Char7:PWideChar='\b';
    Char9:PWideChar='\t';
    Char10:PWideChar='\n';
    Char12:PWideChar='\f';
    Char13:PWideChar='\r';
    CharQuoter:PWideChar='\"';
    CharBackslash:PWidechar='\\';
    CharCode:PWideChar='\u00';
  begin
  ps:=PQCharW(AValue);
  while ps^<>#0 do
    begin
    case ps^ of
      #7:
        ABuilder.Cat(Char7,2);
      #9:
        ABuilder.Cat(Char9,2);
      #10:
        ABuilder.Cat(Char10,2);
      #12:
        ABuilder.Cat(Char12,2);
      #13:
        ABuilder.Cat(Char13,2);
      '\':
        ABuilder.Cat(CharBackslash,2);
      '"':
        ABuilder.Cat(CharQuoter,2);
      else
        begin
        if ps^<#$1F then
          begin
          ABuilder.Cat(CharCode,4);
          if ps^>#$F then
            ABuilder.Cat(CharNum1,1)
          else
            ABuilder.Cat(CharNum0,1);
          ABuilder.Cat(HexChar(Ord(ps^) and $0F));
          end
        else
          ABuilder.Cat(ps,1);
        end;
      end;
    Inc(ps);
    end;
  end;

  procedure StrictJsonTime(ATime:TDateTime);
  var
    MS:Int64;//时区信息不保存
  const
    JsonTimeStart:PWideChar='"/DATE(';
    JsonTimeEnd:PWideChar=')/"';
  begin
  MS:=Trunc(ATime*86400000);
  ABuilder.Cat(JsonTimeStart,7);
  ABuilder.Cat(IntToStr(MS));
  ABuilder.Cat(JsonTimeEnd,3);
  end;

  procedure DoEncode(ANode:TQJson;ALevel:Integer);
  var
    I:Integer;
    ArrayWraped:Boolean;
    AChild:TQJson;
  const
    CharStringStart:PWideChar='"';
    CharStringEnd:PWideChar='",';
    CharNameEnd:PWideChar='":';
    CharArrayStart:PWideChar='[';
    CharArrayEnd:PWideChar='],';
    CharObjectStart:PWideChar='{';
    CharObjectEnd:PWideChar='},';
    CharNull:PWideChar='null,';
    CharFalse:PWideChar='false,';
    CharTrue:PWideChar='true,';
    CharComma:PWideChar=',';
  begin
  if (ANode.Parent<>nil) and (ANode.Parent.DataType<>jdtArray) and (Length(ANode.FName)>0) then
    begin
    if ADoFormat then
      ABuilder.Replicate(AIndent,ALevel);
    ABuilder.Cat(CharStringStart,1);
    CatValue(ANode.FName);
    ABuilder.Cat(CharNameEnd,2);
    end;
  case ANode.DataType of
    jdtArray:
      begin
      ABuilder.Cat(CharArrayStart,1);
      if ANode.Count>0 then
        begin
        ArrayWraped:=False;
        for I := 0 to ANode.Count - 1 do
          begin
          AChild:=ANode.Items[I];
          if AChild.DataType in [jdtArray,jdtObject] then
            begin
            if ADoFormat then
              begin
              ABuilder.Cat(SLineBreak);//对于对象和数组，换行
              ABuilder.Replicate(AIndent,ALevel+1);
              ArrayWraped:=True;
              end;
            end;
          DoEncode(AChild,ALevel+1);
          end;
        ABuilder.Back(1);
        if ArrayWraped then
          begin
          ABuilder.Cat(SLineBreak);
          ABuilder.Replicate(AIndent,ALevel);
          end;
        end;
        ABuilder.Cat(CharArrayEnd,2);
      end;
    jdtObject:
      begin
      if ADoFormat then
        begin
        ABuilder.Cat(CharObjectStart,1);
        ABuilder.Cat(SLineBreak);
        end
      else
        ABuilder.Cat(CharObjectStart,1);
      if ANode.Count>0 then
        begin
        for I := 0 to ANode.Count - 1 do
          begin
          AChild:=ANode.Items[I];
          if Length(AChild.Name)=0 then
            raise Exception.CreateFmt(SObjectChildNeedName,[ANode.Name,I]);
          DoEncode(AChild,ALevel+1);
          if ADoFormat then
            ABuilder.Cat(SLineBreak);
          end;
        if ADoFormat then
          ABuilder.Back(Length(SLineBreak)+1)
        else
          ABuilder.Back(1);
        end;
      if ADoFormat then
        begin
        ABuilder.Cat(SLineBreak);
        ABuilder.Replicate(AIndent,ALevel);
        end;
      ABuilder.Cat(CharObjectEnd,2);
      end;
    jdtNull,jdtUnknown:
      ABuilder.Cat(CharNull,5);
    jdtString:
      begin
      ABuilder.Cat(CharStringStart,1);
      CatValue(ANode.FValue);
      ABuilder.Cat(CharStringEnd,2);
      end;
    jdtInteger,jdtFloat,jdtBoolean:
      begin
      ABuilder.Cat(ANode.Value);
      ABuilder.Cat(CharComma,1);
      end;
    jdtDateTime:
      begin
      ABuilder.Cat(CharStringStart,1);
      if StrictJson then
        StrictJsonTime(ANode.AsDateTime)
      else
        ABuilder.Cat(ANode.Value);
      ABuilder.Cat(CharStringEnd,1);
      ABuilder.Cat(CharComma,1);
      end;
    end;
  end;
begin
Result:=ABuilder;
DoEncode(Self,0);
end;

procedure TQJson.InternalRttiFilter(ASender: TQJson; AObject: Pointer;
  APropName: QStringW; APropType: PTypeInfo; var Accept: Boolean; ATag: Pointer);
var
  ATagData:PQJsonInternalTagData;
  procedure DoNameFilter;
  var
    ps:PQCharW;
  begin
  if Length(ATagData.AcceptNames)>0 then
    begin
    Accept:=False;
    ps:=StrIStrW(PQCharW(ATagData.AcceptNames),PQCharW(APropName));
    if (ps<>nil) and ((ps=PQCharW(ATagData.AcceptNames)) or (ps[-1]=',') or (ps[-1]=';')) then
      begin
      ps:=ps+Length(APropName);
      Accept:=(ps^=',') or (ps^=';') or (ps^=#0);
      end;
    end
  else if Length(ATagData.IgnoreNames)>0 then
    begin
    ps:=StrIStrW(PQCharW(ATagData.IgnoreNames),PQCharW(APropName));
    Accept:=True;
    if (ps<>nil) and ((ps=PQCharW(ATagData.IgnoreNames)) or (ps[-1]=',') or (ps[-1]=';')) then
      begin
      ps:=ps+Length(APropName);
      Accept:=not ((ps^=',') or (ps^=';') or (ps^=#0));
      end;
    end;
  end;
begin
ATagData:=PQJsonInternalTagData(ATag);
if ATagData.TagType=ttNameFilter then
  begin
  DoNameFilter;
  Exit;
  end;
{$IFDEF QDAC_RTTI}
if ATagData.TagType=ttAnonEvent then
  begin
  ATagData.OnEvent(ASender,AObject,APropName,APropType,Accept,ATagData.Tag);
  end;
{$ENDIF}
end;

function TQJson.Invoke(Instance: TObject): TValue;
var
  AMethods:TArray<TRttiMethod>;
  AParams:TArray<TRttiParameter>;
  AMethod:TRttiMethod;
  AType:TRttiType;
  AContext:TRttiContext;
  AParamValues:TValueArray;
  I: Integer;
begin
AContext:=TRttiContext.Create;
AType:=AContext.GetType(Instance.ClassInfo);
AMethods:=AType.GetMethods(Name);
Result:=TValue.Empty;
for AMethod in AMethods do
  begin
  AParams:=AMethod.GetParameters;
  if Length(AParams)=Count then
    begin
    SetLength(AParamValues,Count);
    for I := 0 to Count-1 do
      AParamValues[I]:=Items[I].AsValue;
    Result:=AMethod.Invoke(Instance,AParamValues);
    end;
  end;
end;

function TQJson.Invoke(Instance: TClass): TValue;
var
  AMethods:TArray<TRttiMethod>;
  AParams:TArray<TRttiParameter>;
  AMethod:TRttiMethod;
  AType:TRttiType;
  AContext:TRttiContext;
  AParamValues:TValueArray;
  I: Integer;
begin
AContext:=TRttiContext.Create;
AType:=AContext.GetType(Instance);
AMethods:=AType.GetMethods(Name);
Result:=TValue.Empty;
for AMethod in AMethods do
  begin
  AParams:=AMethod.GetParameters;
  if Length(AParams)=Count then
    begin
    SetLength(AParamValues,Count);
    for I := 0 to Count-1 do
      AParamValues[I]:=Items[I].AsValue;
    Result:=AMethod.Invoke(Instance,AParamValues);
    end;
  end;
end;


function TQJson.IsChildOf(AParent: TQJson): Boolean;
begin
if Assigned(AParent) then
  begin
  if AParent=FParent then
    Result:=True
  else
    Result:=FParent.IsChildOf(AParent);
  end
else
  Result:=False;
end;

function TQJson.IsParentOf(AChild: TQJson): Boolean;
begin
if Assigned(AChild) then
  Result:=AChild.IsChildOf(Self)
else
  Result:=False;
end;

function TQJson.ItemByName(AName: QStringW): TQJson;
var
  AChild:TQJson;
  I:Integer;
  ASelfName:String;
  function ArrayName:String;
  var
    ANamedItem:TQJson;
    AParentIndexes:String;
  begin
  ANamedItem:=Self;
  while ANamedItem.Parent<>nil do
    begin
    if ANamedItem.Parent.IsArray then
      begin
      AParentIndexes:=AParentIndexes+'['+IntToStr(ANamedItem.ItemIndex)+']';
      ANamedItem:=ANamedItem.Parent;
      end
    else
      Break;
    end;
  Result:=ANamedItem.Name+AParentIndexes;
  end;
begin
Result:=nil;
if DataType=jdtObject then
  begin
  I:=IndexOf(AName);
  if I<>-1 then
    Result:=Items[I];
//  else
//  for I := 0 to Count - 1 do
//    begin
//    AChild:=Items[I];
//    if CompareJsonName(AChild.Name,AName) then
//      begin
//      Result:=AChild;
//      Exit;
//      end
//    else if AChild.IsArray then
//      begin
//      Result:=AChild.ItemByName(AName);
//      if Assigned(Result) then
//        Exit;
//      end;
//    end;
  end
else if DataType=jdtArray then
  begin
  ASelfName:=ArrayName;
  for I := 0 to Count-1 do
    begin
    AChild:=Items[I];
    if ASelfName+'['+IntToStr(I)+']'=AName then
      begin
      Result:=AChild;
      Exit;
      end
    else if AChild.IsArray then
      begin
      Result:=AChild.ItemByName(AName);
      if Assigned(Result) then
        Exit;
      end
    else
    end;
  end;
end;

function TQJson.ItemByName(const AName: QStringW; AList: TQJsonItemList;
  ANest: Boolean): Integer;
var
  ANode:TQJson;
  function InternalFind(AParent:TQJson):Integer;
  var
    I:Integer;
  begin
  Result:=0;
  for I := 0 to AParent.Count-1 do
    begin
    ANode:=AParent.Items[I];
    if ANode.Name=AName then
      begin
      AList.Add(ANode);
      Inc(Result);
      end;
    if ANest then
      Inc(Result,InternalFind(ANode));
    end;
  end;
begin
Result:=InternalFind(Self);
end;

function TQJson.ItemByPath(APath: QStringW): TQJson;
var
  AParent:TQJson;
  AName:QStringW;
  p,pn,ws:PQCharW;
  L:Integer;
  AIndex:Int64;
const
  PathDelimiters:PWideChar='./\';
begin
AParent:=Self;
p:=PQCharW(APath);
Result:=nil;
while Assigned(AParent) and (p^<>#0) do
  begin
  AName:=DecodeTokenW(p,PathDelimiters,WideChar(0),False);
  if Length(AName)>0 then
    begin
    //查找的是数组？
    L:=Length(AName);
    AIndex:=-1;
    pn:=PQCharW(AName);
    if (pn[L-1]=']') then
      begin
      repeat
        if pn[L]='[' then
          begin
          ws:=pn+L+1;
          ParseInt(ws,AIndex);
          Break;
          end
        else
          Dec(L);
      until L=0;
      if L>0 then
        begin
        AName:=StrDupX(pn,L);
        Result:=AParent.ItemByName(AName);
        if Result<>nil then
          begin
          if Result.DataType<>jdtArray then
            Result:=nil
          else if (AIndex>=0) and (AIndex<Result.Count) then
            Result:=Result[AIndex];
          end;
        end;
      end
    else
      Result:=AParent.ItemByName(AName);
    if Assigned(Result) then
      AParent:=Result
    else
      begin
      Exit;
      end;
    end;
  if CharInW(p,PathDelimiters) then
    Inc(p);
  //否则是..或//\\等路径，忽略
  end;
if p^<>#0 then
  Result:=nil;
end;

function TQJson.ItemByRegex(const ARegex: QStringW; AList: TQJsonItemList;
  ANest: Boolean): Integer;
var
  ANode:TQJson;
{$IFDEF QDAC_UNICODE}
  APcre:TPerlRegEx;
{$ENDIF}
  function InternalFind(AParent:TQJson):Integer;
  var
    I:Integer;
  begin
  Result:=0;
  for I := 0 to AParent.Count-1 do
    begin
    ANode:=AParent.Items[I];
    {$IFDEF QDAC_UNICODE}
    APcre.Subject:=ANode.Name;
    if APcre.Match then
    {$ELSE}
    if ANode.Name=ARegex then
    {$ENDIF}
      begin
      AList.Add(ANode);
      Inc(Result);
      end;
    if ANest then
      Inc(Result,InternalFind(ANode));
    end;
  end;
begin
{$IFDEF QDAC_UNICODE}
APcre:=TPerlRegex.Create;
try
  APcre.RegEx:=ARegex;
  APcre.Compile;
  Result:=InternalFind(Self);
finally
  FreeObject(APcre);
end;
{$ELSE}
raise Exception.Create(Format(SNotSupport,['ItemByRegex']));
{$ENDIF}
end;

procedure TQJson.LoadFromFile(AFileName: String;AEncoding:TTextEncoding);
var
  AStream:TFileStream;
begin
AStream:=TFileStream.Create(AFileName,fmOpenRead or fmShareDenyWrite);
try
  LoadFromStream(AStream);
finally
  FreeObject(AStream);
end;
end;

procedure TQJson.LoadFromStream(AStream: TStream;AEncoding:TTextEncoding);
var
  S:QStringW;
begin
S:=LoadTextW(AStream,AEncoding);
if Length(S)>2 then
  Parse(PQCharW(S),Length(S))
else
  raise Exception.Create(SBadJson);
end;

procedure TQJson.Parse(p: PWideChar; l: Integer);
  procedure ParseCopy;
  var
    S:QStringW;
  begin
  S:=StrDupW(p,0,l);
  p:=PQCharW(S);
  ParseObject(p);
  end;
begin
if DataType in [jdtObject,jdtArray] then
  Clear;
if (l>0) and (p[l]<>#0) then
  ParseCopy
else
  ParseObject(p);
end;

procedure TQJson.Parse(const s: QStringW);
begin
Parse(PQCharW(S),Length(S));
end;

procedure TQJson.ParseBlock(AStream: TStream; AEncoding: TTextEncoding);
var
  AMS:TMemoryStream;
  procedure ParseUCS2;
  var
    c:QCharW;
    ABlockCount:Integer;
  begin
  ABlockCount:=0;
  repeat
    if ABlockCount=0 then
      begin
      repeat
        AStream.ReadBuffer(c,SizeOf(QCharW));
        AMS.WriteBuffer(c,SizeOf(QCharW));
      until c='{';
      Inc(ABlockCount);
      end;
    AStream.ReadBuffer(c,SizeOf(QCharW));
    if c='{' then
      Inc(ABlockCount)
    else if c='}' then
      Dec(ABlockCount);
    AMS.WriteBuffer(c,SizeOf(QCharW));
  until ABlockCount=0;
  c:=#0;
  AMS.Write(c,SizeOf(QCharW));
  Parse(AMS.Memory,AMS.Size-1);
  end;

  procedure ParseUCS2BE;
  var
    c:Word;
    ABlockCount:Integer;
    p:PQCharW;
  begin
  ABlockCount:=0;
  repeat
    if ABlockCount=0 then
      begin
      repeat
        AStream.ReadBuffer(c,SizeOf(Word));
        c:=(c shr 8) or ((c shl 8) and $FF00);
        AMS.WriteBuffer(c,SizeOf(Word));
      until c=$7B;//#$7B={
      Inc(ABlockCount);
      end;
    AStream.ReadBuffer(c,SizeOf(Word));
    c:=(c shr 8) or ((c shl 8) and $FF00);
    if c=$7B then
      Inc(ABlockCount)
    else if c=$7D then//#$7D=}
      Dec(ABlockCount);
    AMS.WriteBuffer(c,SizeOf(QCharW));
  until ABlockCount=0;
  c:=0;
  AMS.Write(c,SizeOf(QCharW));
  p:=AMS.Memory;
  ParseObject(p);
  end;

  procedure ParseByByte;
  var
    c:Byte;
    ABlockCount:Integer;
  begin
  ABlockCount:=0;
  repeat
    if ABlockCount=0 then
      begin
      repeat
        AStream.ReadBuffer(c,SizeOf(Byte));
        AMS.WriteBuffer(c,SizeOf(Byte));
      until c=$7B;//#$7B={
      Inc(ABlockCount);
      end;
    AStream.ReadBuffer(c,SizeOf(Byte));
    if c=$7B then
      Inc(ABlockCount)
    else if c=$7D then//#$7D=}
      Dec(ABlockCount);
    AMS.WriteBuffer(c,SizeOf(Byte));
  until ABlockCount=0;
  end;

  procedure ParseUtf8;
  var
    S:QStringW;
    p:PQCharW;
  begin
  ParseByByte;
  S:=QString.Utf8Decode(AMS.Memory,AMS.Size);
  p:=PQCharW(S);
  ParseObject(p);
  end;

  procedure ParseAnsi;
  var
    S:QStringW;
  begin
  ParseByByte;
  S:=QString.AnsiDecode(AMS.Memory,AMS.Size);
  Parse(PQCharW(S));
  end;

begin
AMS:=TMemoryStream.Create;
try
  if AEncoding=teAnsi then
    ParseAnsi
  else if AEncoding=teUtf8 then
    ParseUtf8
  else if AEncoding=teUnicode16LE then
    ParseUCS2
  else if AEncoding=teUnicode16BE then
    ParseUCS2BE
  else
    raise Exception.Create(SBadJsonEncoding);
finally
  AMS.Free;
end;
end;

procedure TQJson.ParseJsonPair(ABuilder: TQStringCatHelperW; var p: PQCharW);
const
  SpaceWithSemicolon:PWideChar=': '#9#10#13#$3000;
  CommaWithSpace:PWideChar=', '#9#10#13#$3000;
  JsonEndChars:PWideChar=',}]';
  JsonComplexEnd:PWideChar='}]';
var
  AChild:TQJson;
  AObjEnd:QCharW;
begin
//遇到注释，则直接跳过
while p^='/' do
  begin
  if StrictJson then
    raise Exception.Create(SCommentNotSupport);
  if p[1]='/' then
    begin
    SkipUntilW(p,[WideChar(10)]);
    SkipSpaceW(p);
    end
  else if p[1]='*' then
    begin
    Inc(p,2);
    while p^<>#0 do
      begin
      if (p[0]='*') and (p[1]='/') then
        begin
        Inc(p,2);
        SkipSpaceW(p);
        Break;
        end;
      end;
    end
  else
    raise Exception.CreateFmt(SUnknownToken,[StrDupW(p,0,10)]);
  end;
//解析值
if (p^='{') or (p^='[') then//对象
  begin
  try
    if p^='{' then
      begin
      DataType:=jdtObject;
      AObjEnd:='}';
      end
    else
      begin
      DataType:=jdtArray;
      AObjEnd:=']';
      end;
    Inc(p);
    SkipSpaceW(p);
    while (p^<>#0) and (p^<>AObjEnd) do
      begin
      AChild:=Add;
      AChild.ParseJsonPair(ABuilder,p);
      if p^=',' then
        begin
        Inc(p);
        SkipSpaceW(p);
        end;
      end;
    if p^<>AObjEnd then
      raise Exception.Create(SBadJson)
    else
      begin
      Inc(p);
      SkipSpaceW(p);
      end;
  except
    Clear;
    raise;
  end;
  end
else if Parent<>nil then
  begin
  if (Parent.DataType=jdtObject) and (Length(FName)=0) then
    ParseName(ABuilder,p);
  ParseValue(ABuilder,p);
  if not CharInW(p,JsonEndChars) then
    raise Exception.CreateFmt(SCharNeeded,[JsonEndChars,StrDupW(p,0,10)]);
  end
else
  raise Exception.Create(SBadJson);
end;

function TQJson.ParseJsonTime(p: PQCharW; var ATime: TDateTime): Boolean;
var
  MS,TimeZone:Int64;
begin
//Javascript日期格式为/DATE(自1970.1.1起到现在的毫秒数+时区)/
Result:=False;
if not StartWithW(p,'/DATE',false) then
  Exit;
Inc(p,5);
SkipSpaceW(p);
if p^<>'(' then
  Exit;
Inc(p);
SkipSpaceW(p);
if ParseInt(p,MS)=0 then
  Exit;
SkipSpaceW(p);
if (p^='+') or (p^='-') then
  begin
  if ParseInt(p,TimeZone)=0 then
    Exit;
  SkipSpaceW(p);
  end
else
  TimeZone:=0;
if p^=')' then
  begin
  ATime:=(MS div 86400000)+((MS mod 86400000)/86400000.0);
  if TimeZone<>0 then
    ATime:=IncHour(ATime,-TimeZone);
  Inc(p);
  SkipSpaceW(p);
  Result:=True
  end;
end;

procedure TQJson.ParseName(ABuilder: TQStringCatHelperW; var p: PQCharW);
begin
if StrictJson and (p^<>'"') then
  raise Exception.CreateFmt(SCharNeeded,['"',DecodeLineW(p)]);
BuildJsonString(ABuilder,p);
if p^<>':' then
  raise Exception.Create(SNameNotFound);
if ABuilder.Position=0 then
  raise Exception.Create(SNameNotFound);
FName:=ABuilder.Value;
//解析名称完成
Inc(p);
SkipSpaceW(p);
end;

procedure TQJson.ParseObject(var p: PQCharW);
var
  ABuilder:TQStringCatHelperW;
  ps:PQCharW;
  procedure RaiseParseException(E:Exception);
  var
    ACol,ARow:Integer;
    ALine:QStringW;
  begin
  p:=StrPosW(ps,p,ACol,ARow);
  ALine:=DecodeLineW(p,False);
  raise Exception.CreateFmt(SJsonParseError,[ARow,ACol,E.Message,ALine]);
  end;
begin
ABuilder:=TQStringCatHelperW.Create;
try
  try
    ps:=p;
    SkipSpaceW(p);
    ParseJsonPair(ABuilder,p);
  except on E:Exception do
    RaiseParseException(E);
  end;
finally
  FreeObject(ABuilder);
end;
end;
procedure TQJson.ParseValue(ABuilder: TQStringCatHelperW; var p: PQCharW);
var
  ANum:Extended;
begin
if p^='"' then
  begin
  BuildJsonString(ABuilder,p);
  AsString:=ABuilder.Value;
  end
else if p^='''' then
  begin
  if StrictJson then
    raise Exception.Create(SBadStringStart);
  BuildJsonString(ABuilder,p);
  AsString:=ABuilder.Value;
  end
else if ParseNumeric(p,ANum) then//数字？
  begin
  SkipSpaceW(p);
  if SameValue(ANum,Trunc(ANum)) then
    AsInt64:=Trunc(ANum)
  else
    AsFloat:=ANum;
  end
else if StartWithW(p,'False',True) then//False
  begin
  Inc(p,5);
  SkipSpaceW(p);
  AsBoolean:=False
  end
else if StartWithW(p,'True',True) then//True
  begin
  Inc(p,4);
  SkipSpaceW(p);
  AsBoolean:=True;
  end
else if StartWithW(p,'NULL',True) then//Null
  begin
  Inc(p,4);
  SkipSpaceW(p);
  ResetNull;
  end
else if (p^='[') or (p^='{') then
  ParseJsonPair(ABuilder,p)
else
  raise Exception.Create(SBadJson);
end;

procedure TQJson.Replace(AIndex: Integer; ANewItem: TQJson);
begin
FreeObject(Items[AIndex]);
FItems[AIndex]:=ANewItem;
end;

procedure TQJson.ResetNull;
begin
DataType:=jdtNull;
end;

procedure TQJson.SaveToFile(AFileName: String; AEncoding: TTextEncoding;
  AWriteBOM: Boolean);
var
  AStream:TMemoryStream;
begin
AStream:=TMemoryStream.Create;
try
  SaveToStream(AStream,AEncoding,AWriteBOM);
  AStream.SaveToFile(AFileName);
finally
  FreeObject(AStream);
end;
end;

procedure TQJson.SaveToStream(AStream: TStream; AEncoding: TTextEncoding;
  AWriteBOM: Boolean);
begin
if AEncoding=teUTF8 then
  SaveTextU(AStream,QString.Utf8Encode(Value),AWriteBom)
else if AEncoding=teAnsi then
  SaveTextA(AStream,QString.AnsiEncode(Value))
else if AEncoding=teUnicode16LE then
  SaveTextW(AStream,Value,AWriteBom)
else
  SaveTextWBE(AStream,Value,AWriteBom);
end;

procedure TQJson.SetAsArray(const Value: QStringW);
var
  p:PQCharW;
begin
DataType:=jdtArray;
Clear;
p:=PQCharW(Value);
ParseObject(p);
end;

procedure TQJson.SetAsBoolean(const Value: Boolean);
begin
DataType:=jdtBoolean;
PBoolean(FValue)^:=Value;
end;

procedure TQJson.SetAsDateTime(const Value: TDateTime);
begin
DataType:=jdtDateTime;
PExtended(FValue)^:=Value;
end;

procedure TQJson.SetAsFloat(const Value: Extended);
begin
DataType:=jdtFloat;
PExtended(FValue)^:=Value;
end;

procedure TQJson.SetAsInt64(const Value: Int64);
begin
DataType:=jdtInteger;
PInt64(FValue)^:=Value;
end;

procedure TQJson.SetAsInteger(const Value: Integer);
begin
SetAsInt64(Value);
end;

procedure TQJson.SetAsJson(const Value: QStringW);
var
  ABuilder:TQStringCatHelperW;
  p:PQCharW;
begin
ABuilder:=TQStringCatHelperW.Create;
try
  try
    if DataType in [jdtArray,jdtObject] then
      Clear;
    p:=PQCharW(Value);
    ParseValue(ABuilder,p);
  except
    AsString:=Value;
  end;
finally
  FreeObject(ABuilder);
end;
end;

procedure TQJson.SetAsObject(const Value: QStringW);
begin
Parse(PQCharW(Value),Length(Value));
end;

procedure TQJson.SetAsString(const Value: QStringW);
begin
DataType:=jdtString;
FValue:=Value;
end;

procedure TQJson.SetAsValue(const Value: TValue);
var
  I:Integer;
  APointer:Pointer;
begin
case Value.Kind of
  tkInteger:
    AsInt64:=Value.AsInteger;
  tkChar:
    AsString:=Value.AsString;
  tkEnumeration:
    begin
    if Value.IsType<Boolean> then
      AsBoolean:=Value.AsBoolean
    else
      AsString:=Value.ToString;
    end;
  tkFloat:
    AsFloat:=Value.AsExtended;
  tkString:
    AsString:=Value.AsString;
  tkSet:
    AsString:=Value.ToString;
  tkClass:
    AsString:=Value.AsClass.ClassName;
  tkMethod:
    AsString:=Value.ToString;
  tkWChar:
    AsString:=Value.ToString;
  tkLString:
    AsString:=Value.ToString;
  tkWString:
    AsString:=Value.ToString;
  tkVariant:
    AsVariant:=Value.AsVariant;
  tkArray,tkDynArray:
    begin
    DataType:=jdtArray;
    Clear;
    for I := 0 to Value.GetArrayLength-1 do
      Add.AsValue:=Value.GetArrayElement(I);
    end;
  tkRecord:
    begin
    if Value.TypeInfo=TypeInfo(TValue) then
      AsValue:=Value.AsType<TValue>
    else
      begin
      Value.ExtractRawData(@APointer);
      InternalAddRecord(Value.TypeInfo,APointer,nil,nil);
      end;
    end;
  tkInt64:
    AsInt64:=Value.AsInt64;
  tkUString:
    AsString:=Value.AsString;
  tkInterface,tkClassRef,tkPointer,tkProcedure:
    ;
end;
end;

procedure TQJson.SetAsVariant(const Value: Variant);
var
  I:Integer;
begin
if VarIsArray(Value) then
  begin
  ArrayNeeded(jdtArray);
  Clear;
  for I := VarArrayLowBound(Value,VarArrayDimCount(Value)) to VarArrayHighBound(Value,VarArrayDimCount(Value)) do
    Add.AsVariant:=Value[I];
  end
else
  begin
  case VarType(Value) of
    varSmallInt,varInteger,varByte,varShortInt,varWord,varLongWord,varInt64:
      AsInt64:=Value;
    varSingle,varDouble,varCurrency:
      AsFloat:=Value;
    varDate:
      AsDateTime:=Value;
    varOleStr,varString{$IFDEF QDAC_UNICODE},varUString{$ENDIF}:
      AsString:=Value;
    varBoolean:
      AsBoolean:=Value;
    varRecord:
      InternalAddRecord(PVarRecord(@Value).RecInfo,PVarRecord(@Value).PRecord,nil,nil);
  end;
  end;
end;

procedure TQJson.SetDataType(const Value: TQJsonDataType);
begin
if FDataType<>Value then
  begin
  if DataType in [jdtArray,jdtObject] then
    begin
    Clear;
    if not (Value in [jdtArray,jdtObject]) then
      begin
      FreeObject(FItems);
      end;
    end;
  case Value of
    jdtNull,jdtUnknown,jdtString:
      SetLength(FValue,0);
    jdtInteger:
      begin
      SetLength(FValue,SizeOf(Int64) shr 1);
      PInt64(FValue)^:=0;
      end;
    jdtFloat,jdtDateTime:
      begin
      SetLength(FValue,SizeOf(Extended) shr 1);
      PDouble(FValue)^:=0;
      end;
    jdtBoolean:
      begin
      SetLength(FValue,1);
      PBoolean(FValue)^:=False;
      end;
    jdtArray,jdtObject:
      if not (FDataType in [jdtArray,jdtObject]) then
        ArrayNeeded(Value);
  end;
  FDataType := Value;
  end;
end;

procedure TQJson.SetValue(const Value: QStringW);
var
  p:PQCharW;
  procedure ParseNum;
  var
    ANum:Extended;
  begin
  if ParseNumeric(p,ANum) then
    begin
    if SameValue(ANum,Trunc(ANum)) then
      AsInt64:=Trunc(ANum)
    else
      AsFloat:=ANum;
    end
  else
    raise Exception.Create(Format(SBadNumeric,[Value]));
  end;
  procedure SetDateTime;
  var
    ATime:TDateTime;
  begin
  if ParseDateTime(PQCharW(Value),ATime) then
    AsDateTime:=ATime
  else if ParseJsonTime(PQCharW(Value),ATime) then
    AsDateTime:=ATime
  else
    raise Exception.Create(SBadJsonTime);
  end;
  procedure DetectValue;
  var
    ABuilder:TQStringCatHelperW;
    p:PQCharW;
  begin
  ABuilder:=TQStringCatHelperW.Create;
  try
    p:=PQCharW(Value);
    ParseValue(ABuilder,p);
  except
    AsString:=Value;
  end;
  FreeObject(ABuilder);
  end;
begin
if DataType = jdtString then
  FValue:=Value
else if DataType=jdtBoolean then
  AsBoolean:=StrToBool(Value)
else
  begin
  p:=PQCharW(Value);
  if DataType in [jdtInteger,jdtFloat] then
    ParseNum
  else if DataType=jdtDateTime then
    SetDateTime
  else if DataType in [jdtArray,jdtObject] then
    begin
    Clear;
    ParseObject(p);
    end
  else //jdtUnknown
    DetectValue;
  end;
end;

procedure TQJson.ToObject(AObject: TObject);
  procedure AssignProp(AParent:TQJson;AObj:TObject);
  var
    APropInfo:PPropInfo;
    I:Integer;
    AChild:TQJson;
    dynArray:Pointer;
  begin
  if AObj=nil then
    Exit;
  for I := 0 to Count-1 do
    begin
    AChild:=AParent[I];
    APropInfo:=GetPropInfo(AObj,AChild.Name);
    if Assigned(APropInfo) then
      begin
      case APropInfo.PropType^.Kind of
        tkChar:
          SetOrdProp(AObj,APropInfo,QString.AnsiEncode(AChild.AsString)[0]);
        tkWChar:
          SetOrdProp(AObj,APropInfo,PWord(PWideChar(AChild.AsString))^);
        tkInteger:
          SetOrdProp(AObj,APropInfo,AChild.AsInteger);
        tkClass:
          AChild.ToObject(TObject(GetOrdProp(AObj,APropInfo)));
        tkEnumeration:
          SetEnumProp(AObj,APropInfo,AChild.AsString);
        tkSet:
          SetSetProp(AObj,APropInfo,AChild.AsString);
        tkFloat:
          SetFloatProp(AObj,APropInfo,AChild.AsFloat);
        tkMethod:
          {绑定函数的值暂时忽略};
        {$IFNDEF NEXTGEN}
        tkString, tkLString,tkWString:
          SetStrProp(AObj,APropInfo,AChild.AsString);
        {$ENDIF !NEXTGEN}
        {$IFDEF QDAC_UNICODE}
        tkUString:
          SetStrProp(AObj,APropInfo,AChild.AsString);
        {$ENDIF QDAC_UNICODE}
        tkVariant:
          SetVariantProp(AObj,APropInfo,AChild.AsVariant);
        tkInt64:
          SetInt64Prop(AObj,APropInfo,AChild.AsInt64);
        tkDynArray:
          begin
          dynArray:=nil;
          DynArrayFromVariant(dynArray,AChild.AsVariant,APropInfo.PropType^);
          SetDynArrayProp(AObj,APropInfo,dynArray);
          end;
      end;
      end;
    end;
  end;
begin
if Assigned(AObject) then
  AssignProp(Self,AObject);
end;
{$IFDEF QDAC_RTTI}
procedure TQJson.InternalToRecord(ATypeInfo:PTypeInfo;ABaseAddr:Pointer);
var
  AContext:TRttiContext;
  AType:TRttiType;
  I:Integer;
  AFields:TArray<TRttiField>;
  AChild:TQJson;
  procedure SetAsIntValue(AValue:Int64);
  begin
  if AFields[I].FieldType.TypeKind=tkInt64 then
    PInt64(IntPtr(ABaseAddr)+AFields[I].Offset)^:=AValue
  else
    begin
    case GetTypeData(AFields[I].FieldType.Handle)^.OrdType
//    AFields[I].FieldType.Handle.TypeData.OrdType
      of
        otSByte, otUByte:
          PByte(IntPtr(ABaseAddr)+AFields[I].Offset)^:=AValue;
        otSWord, otUWord:
          PSmallint(IntPtr(ABaseAddr)+AFields[I].Offset)^:=AValue;
        otSLong, otULong:
          PInteger(IntPtr(ABaseAddr)+AFields[I].Offset)^:=AValue;
      end;
    end
  end;
  procedure ParseDynArray;
  var
    AValue:array of TValue;
    J:Integer;
  begin
  SetLength(AValue,AChild.Count);
  for J := 0 to AChild.Count-1 do
   AValue[J]:=TValue.FromVariant(AChild[J].AsVariant);
  AFields[I].SetValue(ABaseAddr,TValue.FromArray(AFields[I].FieldType.Handle,AValue));
  end;
  {$IFNDEF NEXTGEN}
  procedure ParseShortString;
  var
    S: ShortString;
    AValue:TValue;
  begin
  S:= ShortString(AChild.AsString);
  TValue.Make(@S, AFields[I].FieldType.Handle, AValue);
  AFields[I].SetValue(ABaseAddr,AValue);
  end;
  {$ENDIF}
begin
if not Assigned(ATypeInfo) then
  Exit;
AType:=AContext.GetType(ATypeInfo);
if AType=nil then
  Exit;
if AType.TypeKind<>tkRecord then
  Exit;
AFields:=AType.GetFields;
for I := Low(AFields) to High(AFields) do
  begin
  AChild:=ItemByName(AFields[I].Name);
  if Assigned(AChild) then
    begin
    if  AFields[I].FieldType.TypeKind =tkInteger then
      SetAsIntValue(AChild.AsInt64)
    else if AFields[I].FieldType.TypeKind=tkInt64 then
      SetAsIntValue(AChild.AsInt64)
    else if AFields[I].FieldType.TypeKind in [tkUString{$IFNDEF NEXTGEN},tkString,tkLString,tkWString,tkChar,tkWChar{$ENDIF}] then
      begin
      {$IFDEF NEXTGEN}
      AFields[I].SetValue(ABaseAddr,TValue.From(AChild.AsString));
      {$ELSE}
      if AFields[I].FieldType.TypeKind=tkString then
        ParseShortString
      else
        AFields[I].SetValue(ABaseAddr,TValue.From(AChild.AsString));
      {$ENDIF}
      end
    else if AFields[I].FieldType.TypeKind=tkRecord then
      InternalToRecord(AFields[I].FieldType.Handle,Pointer(IntPtr(ABaseAddr)+AFields[I].Offset))
    else if AFields[I].FieldType.TypeKind=tkEnumeration then
      SetAsIntValue(GetEnumValue(AFields[I].FieldType.Handle,AChild.AsString))
    else if AFields[I].FieldType.TypeKind=tkSet then
      SetAsIntValue(StringToSet(AFields[I].FieldType.Handle,AChild.AsString))
    else if AFields[I].FieldType.TypeKind=tkFloat then
      begin
      if (AFields[I].FieldType.Handle=TypeInfo(TDate)) or
        (AFields[I].FieldType.Handle=TypeInfo(TTime)) or
        (AFields[I].FieldType.Handle=TypeInfo(TDateTime))
        then
        AFields[I].SetValue(ABaseAddr,TValue.From(AChild.AsDateTime))
      else
        AFields[I].SetValue(ABaseAddr,TValue.From(AChild.AsFloat));
      end
    else if (AFields[I].FieldType.TypeKind=tkVariant) then
      AFields[I].SetValue(ABaseAddr,TValue.From(AChild.AsVariant))
    else if (AFields[I].FieldType.TypeKind in [tkArray,tkDynArray]) and AChild.IsArray then
      ParseDynArray
    else//
      begin
//      tkClass,tkMethod,tkInterface,tkClassRef,tkPointer,tkProcedure:
        {不受支持的类型，忽略};
      end;
    end;
  end;
end;
procedure TQJson.ToRecord<T>(const ARecord: T);
begin
InternalToRecord(TypeInfo(T),@ARecord);
end;
{$ENDIF}
function TQJson.ToString: string;
begin
Result:=AsString;
end;

procedure TQJson.ValidArray;
begin
if DataType in [jdtArray,jdtObject] then
  {$IFDEF QDAC_UNICODE}
  FItems:=TList<TQJson>.Create
  {$ELSE}
  FItems:=TList.Create
  {$ENDIF}
else
  raise Exception.Create(Format(SVarNotArray,[FName]));
end;

function TQJson.ValueByName(AName, ADefVal: QStringW): QStringW;
var
  AChild:TQJson;
begin
AChild:=ItemByName(AName);
if Assigned(AChild) then
  Result:=AChild.Value
else
  Result:=ADefVal;
end;

function TQJson.ValueByPath(APath, ADefVal: QStringW): QStringW;
var
  AItem:TQJson;
begin
AItem:=ItemByPath(APath);
if Assigned(AItem) then
  Result:=AItem.Value
else
  Result:=ADefVal;
end;
{ TQJsonEnumerator }

constructor TQJsonEnumerator.Create(AList: TQJson);
begin
inherited Create;
FList:=AList;
FIndex:=-1;
end;

function TQJsonEnumerator.GetCurrent: TQJson;
begin
Result:=FList[FIndex];
end;

function TQJsonEnumerator.MoveNext: Boolean;
begin
if FIndex<FList.Count-1 then
  begin
  Inc(FIndex);
  Result:=True;
  end
else
  Result:=False;
end;

{ TQHashedJson }

function TQHashedJson.Add(AName: QStringW): TQJson;
begin
Result:=inherited Add(AName);
Result.FNameHash:=HashOf(PQCharW(AName),Length(AName) shl 1);
FHashTable.Add(Pointer(Count-1),Result.FNameHash);
end;

procedure TQHashedJson.Assign(ANode: TQJson);
begin
  inherited;
if (Length(FName)>0) then
  begin
  if FNameHash=0 then
    FNameHash:=HashOf(PQCharW(FName),Length(FName) shl 1);
  if Assigned(Parent) then
    TQHashedJson(Parent).FHashTable.Add(Pointer(Parent.Count-1),FNameHash);
  end;
end;

procedure TQHashedJson.Clear;
begin
inherited;
FHashTable.Clear;
end;

constructor TQHashedJson.Create;
begin
inherited;
FHashTable:=TQHashTable.Create();
FHashTable.AutoSize:=True;
end;

function TQHashedJson.CreateJson: TQJson;
begin
if Assigned(OnQJsonCreate) then
  Result:=OnQJsonCreate
else
  Result:=TQHashedJson.Create;
end;

procedure TQHashedJson.Delete(AIndex: Integer);
var
  AItem:TQJson;
begin
AItem:=Items[AIndex];
FHashTable.Delete(Pointer(AIndex),AItem.NameHash);
inherited;
end;

destructor TQHashedJson.Destroy;
begin
FreeObject(FHashTable);
inherited;
end;

function TQHashedJson.IndexOf(const AName: QStringW): Integer;
var
  AIndex,AHash:Integer;
  AList:PQHashList;
  AItem:TQJson;
begin
AHash:=HashOf(PQCharW(AName),Length(AName) shl 1);
AList:=FHashTable.FindFirst(AHash);
Result:=-1;
while AList<>nil do
  begin
  AIndex:=Integer(AList.Data);
  AItem:=Items[AIndex];
  if AItem.Name=AName then
    begin
    Result:=AIndex;
    Break;
    end
  else
    AList:=FHashTable.FindNext(AList);
  end;
end;
procedure TQHashedJson.Replace(AIndex: Integer; ANewItem: TQJson);
var
  AOld:TQJson;
begin
if not (ANewItem is TQHashedJson) then
  raise Exception.CreateFmt(SReplaceTypeNeed,['TQHashedJson']);
AOld:=Items[AIndex];
FHashTable.Delete(Pointer(AIndex),AOld.NameHash);
inherited;
if Length(ANewItem.FName)>0 then
  FHashTable.Add(Pointer(AIndex),ANewItem.FNameHash);
end;

initialization
  StrictJson:=False;
  JsonCaseSensitive:=True;
  JsonDateFormat:='yyyy-mm-dd';
  JsonDateTimeFormat:='yyyy-mm-dd''T''hh:nn:ss.zzz';
  JsonTimeFormat:='hh:nn:ss.zzz';
  OnQJsonCreate:=nil;
  OnQJsonFree:=nil;
end.
