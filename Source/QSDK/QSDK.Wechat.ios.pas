{ ******************************************************* }
{ QSDK.Wechat.iOS 1.0 }
{ Interfaces for libWeChatSDK 1.7.1 }
{ Created by TU2(Ticr),and agree to share with QSDK }
{ ******************************************************* }

unit QSDK.Wechat.ios;

interface

uses
  System.ZLib {libz.dylib} , System.Sqlite {libsqlite3.0.dylib} ,
  iOSapi.Foundation, Macapi.ObjectiveC, iOSapi.UIKit, Macapi.Helpers,
  iOSapi.Foundation, FMX.Platform.ios;

type
  BaseReq = interface;
  BaseResp = interface;
  SendAuthReq = interface;
  WXMediaMessage = interface;

  /// <summary>微信Api接口函数对象</summary>
  WXApi = interface(NSObject) // 在这里声明对象方法(-)
    ['{D2B85162-56EC-49A1-8147-44FF05A43147}']
  end;

  /// <summary>微信Api接口函数类</summary>
  WXApiClass = interface(NSObjectClass) // 在这里声明类方法(+)
    ['{1EFADA09-CB9B-4625-857D-733E00C5B266}']
    /// <summary>向微信终端程序注册第三方应用。</summary>
    /// <param name="appid">微信开发者ID</param>
    /// <remarks>需要在每次启动第三方应用程序时调用。请保证在主线程中调用此函数。
    /// 第一次调用后，会在微信的可用应用列表中出现。
    /// iOS7及以上系统需要调起一次微信才会出现在微信的可用应用列表中。
    /// </remarks>
    function registerApp(appid: NSString): Boolean; cdecl;
    /// <summary>向微信终端程序注册第三方应用。</summary>
    /// <param name="appid">微信开发者ID</param>
    /// <param name="appdesc">应用附加信息，长度不超过1024字节</param>
    /// <remarks>需要在每次启动第三方应用程序时调用。请保证在主线程中调用此函数。
    /// 第一次调用后，会在微信的可用应用列表中出现。
    /// iOS7及以上系统需要调起一次微信才会出现在微信的可用应用列表中。
    /// </remarks>
    [MethodName('registerApp:withDescription:')]
    function registerAppwithDescription(appid, appdesc: NSString)
      : Boolean; cdecl;
    /// <summary>向微信终端程序注册应用支持打开的文件类型。</summary>
    /// <param name="typeFlag">应用支持打开的数据类型,enAppSupportContentFlag枚举类型 “|” 操作后结果</param>
    /// <remarks>需要在每次启动第三方应用程序时调用。调用后并第一次成功分享数据到微信后，会在微信的可用应用列表中出现。</remarks>
    procedure registerAppSupportContentFlag(typeFlag: UInt64); cdecl;
    /// <summary>处理微信通过URL启动App时传递的数据。</summary>
    /// <param name="url">微信启动第三方应用时传递过来的URL</param>
    /// <param name="delegate">WXApiDelegate对象，用来接收微信触发的消息</param>
    /// <remarks>需要在 application:openURL:sourceApplication:annotation:或者application:handleOpenURL中调用。</remarks>
    function handleOpenURL(url: NSURL; delegate: Pointer): Boolean; cdecl;
    /// <summary>检查微信是否已被用户安装。</summary>
    function isWXAppInstalled: Boolean; cdecl;
    /// <summary>判断当前微信的版本是否支持OpenApi。</summary>
    function isWXAppSupportApi: Boolean; cdecl;
    /// <summary>获取微信的itunes安装地址。</summary>
    function getWXAppInstallUrl: NSString; cdecl;
    /// <summary>获取当前微信SDK的版本号。</summary>
    function getApiVersion: NSString; cdecl;
    /// <summary>打开微信。</summary>
    function openWXApp: Boolean; cdecl;
    /// <summary>发送请求到微信，等待微信返回onResp。</summary>
    /// <param name="req">具体的发送请求，在调用函数后，请自己释放</param>
    /// <remarks>函数调用后，会切换到微信的界面。第三方应用程序等待微信返回onResp。
    /// 微信在异步处理完成后一定会调用onResp。支持SendAuthReq、SendMessageToWXReq、PayReq等</remarks>
    function sendReq(req: BaseReq): Boolean; cdecl;
    /// <summary>发送Auth请求到微信，支持用户没安装微信，等待微信返回onResp。</summary>
    /// <param name="req">具体的发送请求，在调用函数后，请自己释放</param>
    /// <param name="viewController">当前界面对象</param>
    /// <param name="delegate">WXApiDelegate对象，用来接收微信触发的消息</param>
    /// <remarks>函数调用后，会切换到微信的界面。第三方应用程序等待微信返回onResp。
    /// 微信在异步处理完成后一定会调用onResp。支持SendAuthReq类型。</remarks>
    function SendAuthReq(req: SendAuthReq; viewController: UIViewController;
      delegate: Pointer): Boolean; cdecl;
    /// <summary>收到微信onReq的请求，发送对应的应答给微信，并切换到微信界面。</summary>
    /// <param name="resp">具体的应答内容，调用函数后，请自己释放</param>
    /// <remarks>函数调用后，会切换到微信的界面。第三方应用程序收到微信onReq的请求，异步处理该请求，完成后必须调用该函数。
    /// 可能发送的相应有GetMessageFromWXResp、ShowMessageFromWXResp等。</remarks>
    function sendResp(resp: BaseResp): Boolean; cdecl;
  end;

  TWXApi = class(TOCGenericImport<WXApiClass, WXApi>)
  end;

  // ------------------------------------------------------------------------------
  /// <summary>微信终端SDK所有请求消息的基类</summary>
  BaseReq = interface(NSObject)
    ['{2669AE3E-02DD-46B9-B1B8-7705A47FF5E6}']
    /// <summary>请求类型</summary>
    [MethodName('type:')]
    function _type: Integer; cdecl;
    procedure setType(AType: Integer); cdecl;
    /// <summary>由用户微信号和AppID组成的唯一标识</summary>
    /// <remarks>发送请求时第三方程序必须填写，用于校验微信用户是否换号登录</remarks>
    function openID: NSString; cdecl;
    procedure setOpenID(openID: NSString); cdecl;
  end;

  /// <summary>认证请求</summary>
  SendAuthReq = interface(BaseReq)
    ['{0719D7AB-B0CB-4D16-8A64-6E270D9D98A7}']
    function scope: NSString; cdecl;
    /// <remarks>scope字符串长度不能超过1K</remarks>
    procedure setScope(scope: NSString); cdecl;
    function state: NSString; cdecl;
    /// <remarks>state字符串长度不能超过1K</remarks>
    procedure setState(state: NSString); cdecl;
  end;

  /// <summary>支付请求</summary>
  PayReq = interface(BaseReq)
    ['{5B7DC0E5-386E-4B5F-AADE-EA646C8F40E5}']
    /// <summary>商家向财付通申请的商家id</summary>
    function partnerId: NSString; cdecl;
    procedure setPartnerId(partnerId: NSString); cdecl;
    /// <summary>预支付订单id</summary>
    function prepayId: NSString; cdecl;
    procedure setPrepayId(prepayId: NSString); cdecl;
    /// <summary>随机串，防重发</summary>
    function nonceStr: NSString; cdecl;
    procedure setNonceStr(nonceStr: NSString); cdecl;
    /// <summary>时间戳，防重发</summary>
    function timeStamp: UInt32; cdecl;
    procedure setTimeStamp(timeStamp: UInt32); cdecl;
    /// <summary>商家根据财付通文档填写的数据和签名</summary>
    function package: NSString; cdecl;
    procedure setPackage(package: NSString); cdecl;
    /// <summary>商家根据微信开放平台文档对数据做的签名</summary>
    function sign: NSString; cdecl;
    procedure setSign(sign: NSString); cdecl;
  end;

  /// <summary>拆企业红包请求</summary>
  HBReq = interface(BaseReq)
    ['{F0886BC2-E740-48C2-8B24-99CC9357B081}']
    /// <summary>随机串，防重发</summary>
    function nonceStr: NSString; cdecl;
    procedure setNonceStr(nonceStr: NSString); cdecl;
    /// <summary>时间戳，防重发</summary>
    function timeStamp: UInt32; cdecl;
    procedure setTimeStamp(timeStamp: UInt32); cdecl;
    /// <summary>商家根据微信企业红包开发文档填写的数据和签名</summary>
    function package: NSString; cdecl;
    procedure setPackage(package: NSString); cdecl;
    /// <summary>商家根据微信企业红包开发文档对数据做的签名</summary>
    function sign: NSString; cdecl;
    procedure setSign(sign: NSString); cdecl;
  end;

  /// <summary>发消息请求</summary>
  SendMessageToWXReq = interface(BaseReq)
    ['{97149A16-9975-406C-A992-4AEF5323CE7E}']
    /// <summary>发送消息的文本内容</summary>
    /// <remarks>文本长度必须大于0且小于10K</remarks>
    function text: NSString; cdecl;
    procedure setText(text: NSString); cdecl;
    /// <summary>发送消息的多媒体内容</summary>
    function message: WXMediaMessage; cdecl;
    procedure setMessage(message: WXMediaMessage); cdecl;
    /// <summary>是否是文本消息</summary>
    function bText: Boolean; cdecl;
    procedure setBText(bText: Boolean); cdecl;
    /// <summary>目标场景（默认发送到会话）</summary>
    function scene: Integer; cdecl;
    procedure setScene(scene: Integer); cdecl;
  end;

  SendMessageToWXReqClass = interface(NSObjectClass)
    ['{25BA201D-4209-4987-8A0B-249A0D3D8ED3}']
  end;

  TSendMessageToWXReq = class(TOCGenericImport<SendMessageToWXReqClass,
    SendMessageToWXReq>)
  end;

  /// <summary>打开临时会话请求</summary>
  OpenTempSessionReq = interface(BaseReq)
    ['{5E4F94F8-2526-471F-BAFE-CEECC7650822}']
    /// <summary>需要打开的用户名(长度不能超过512字节)</summary>
    function username: NSString; cdecl;
    procedure setUsername(username: NSString); cdecl;
    /// <summary>开发者自定义参数，拉起临时会话后会发给开发者后台，可以用于识别场景(长度不能超过32位)</summary>
    function sessionFrom: NSString; cdecl;
    procedure setSessionFrom(sessionFrom: NSString); cdecl;
  end;

  /// <summary>打开指定网址请求</summary>
  OpenWebviewReq = interface(BaseReq)
    ['{D60EDE96-9B9A-4508-A68C-98C95C2972F1}']
    /// <summary>需要打开的网页对应的Url(长度不能超过1024)</summary>
    function url: NSString; cdecl;
    procedure setUrl(url: NSString); cdecl;
  end;

  /// <summary>打开硬件排行榜请求</summary>
  OpenRankListReq = interface(BaseReq)
    ['{955D93EC-AA2F-4C4D-9193-D94EC81A1C37}']
  end;

  /// <summary>打开指定微信公众号profile页面</summary>
  JumpToBizProfileReq = interface(BaseReq)
    ['{141D08FA-D6B2-4BCC-8114-09639561AD87}']
    /// <summary>跳转到该公众号的profile(长度不能超过512字节)</summary>
    function username: NSString; cdecl;
    procedure setUsername(username: NSString); cdecl;
    /// <summary>如果用户加了该公众号为好友，extMsg会上传到服务器(长度不能超过1024字节)</summary>
    function extMsg: NSString; cdecl;
    procedure setExtMsg(extMsg: NSString); cdecl;
    /// <summary>跳转的公众号类型</summary>
    function profileType: Integer; cdecl;
    procedure setProfileType(profileType: Integer); cdecl;
  end;

  /// <summary>打开指定微信公众号profile网页版</summary>
  JumpToBizWebviewReq = interface(BaseReq)
    ['{3154F62A-9D93-4ABC-AE24-CA547B211249}']
    /// <summary>跳转的网页类型，目前只支持广告页</summary>
    function webType: Integer; cdecl;
    procedure setWebType(webType: Integer); cdecl;
    /// <summary>跳转到该公众号的profile网页版(长度不能超过512字节)</summary>
    function tousrname: NSString; cdecl;
    procedure setTousrname(tousrname: NSString); cdecl;
    /// <summary>如果用户加了该公众号为好友，extMsg会上传到服务器(长度不能超过1024字节)</summary>
    function extMsg: NSString; cdecl;
    procedure setExtMsg(extMsg: NSString); cdecl;
  end;

  /// <summary>请求添加卡券</summary>
  AddCardToWXCardPackageReq = interface(BaseReq)
    ['{F7547572-9BD0-431F-8AAA-DE8FB0AB8814}']
    /// <summary>卡列表（个数不能超过40个WXCardItem）</summary>
    function cardAry: NSArray; cdecl;
    procedure setCardAry(cardAry: NSArray); cdecl;
  end;

  /// <summary>请求选取卡券</summary>
  WXChooseCardReq = interface(BaseReq)
    ['{B9B20AC0-C4CC-4CB2-B3D5-EFC32EF56CCF}']
    function appid: NSString; cdecl;
    procedure setAppID(appid: NSString); cdecl;
    function shopID: UInt32; cdecl;
    procedure setShopID(shopID: UInt32); cdecl;
    function canMultiSelect: UInt32; cdecl;
    procedure setCanMultiSelect(canMultiSelect: UInt32); cdecl;
    function cardType: NSString; cdecl;
    procedure setCardType(cardType: NSString); cdecl;
    function cardTpID: NSString; cdecl;
    procedure setCardTpID(cardTpID: NSString); cdecl;
    function signType: NSString; cdecl;
    procedure setSignType(signType: NSString); cdecl;
    function cardSign: NSString; cdecl;
    procedure setCardSign(cardSign: NSString); cdecl;
    function timeStamp: UInt32; cdecl;
    procedure setTimeStamp(timeStamp: UInt32); cdecl;
    function nonceStr: NSString; cdecl;
    procedure setNonceStr(nonceStr: NSString); cdecl;
  end;

  /// <summary>微信请求提供内容的消息</summary>
  GetMessageFromWXReq = interface(BaseReq)
    ['{A78D6373-B23A-4EAC-958B-F90F3304226D}']
    function lang: NSString; cdecl;
    procedure setLang(lang: NSString); cdecl;
    function country: NSString; cdecl;
    procedure setCountry(country: NSString); cdecl;
  end;

  /// <summary>微信通知显示内容的消息</summary>
  ShowMessageFromWXReq = interface(BaseReq)
    ['{0AC5F853-9268-4223-85EA-E405F61BA277}']
    /// <summary>第三方程序需处理的多媒体内容</summary>
    function message: WXMediaMessage; cdecl;
    procedure setMessage(message: WXMediaMessage); cdecl;
    function lang: NSString; cdecl;
    procedure setLang(lang: NSString); cdecl;
    function country: NSString; cdecl;
    procedure setCountry(country: NSString); cdecl;
  end;

  /// <summary>微信发送的打开命令消息（无需响应）</summary>
  LaunchFromWXReq = interface(BaseReq)
    ['{18AC138F-C24F-490B-8C6A-CAEAC8294241}']
    /// <summary>第三方程序需处理的多媒体内容</summary>
    function message: WXMediaMessage; cdecl;
    procedure setMessage(message: WXMediaMessage); cdecl;
    function lang: NSString; cdecl;
    procedure setLang(lang: NSString); cdecl;
    function country: NSString; cdecl;
    procedure setCountry(country: NSString); cdecl;
  end;

  // ------------------------------------------------------------------------------
  /// <summary>该类为微信终端SDK所有响应消息的基类</summary>
  BaseResp = interface(NSObject)
    ['{535824F8-CAE8-4C73-A688-136A72745D38}']
    /// <summary>错误码</summary>
    function errCode: Integer; cdecl;
    procedure setErrCode(errCode: Integer); cdecl;
    /// <summary>错误提示字符串</summary>
    function errStr: NSString; cdecl;
    procedure setErrStr(errStr: NSString); cdecl;
    /// <summary>响应类型</summary>
    [MethodName('type:')]
    function _type: Integer; cdecl;
    procedure setType(AType: Integer); cdecl;
  end;

  /// <summary>认证响应</summary>
  SendAuthResp = interface(BaseResp)
    ['{42C82024-4966-456A-98A4-164F7F1269A0}']
    function code: NSString; cdecl;
    procedure setCode(code: NSString); cdecl;
    function state: NSString; cdecl;
    procedure setState(state: NSString); cdecl;
    function lang: NSString; cdecl;
    procedure setLang(lang: NSString); cdecl;
    function country: NSString; cdecl;
    procedure setCountry(country: NSString); cdecl;
  end;

  /// <summary>支付响应</summary>
  PayResp = interface(BaseResp)
    ['{BD7E06A1-1EC0-4150-86D7-F83BE56BDA33}']
    function returnKey: NSString; cdecl;
    procedure setReturnKey(returnKey: NSString); cdecl;
  end;

  /// <summary>拆企业红包响应</summary>
  HBResp = interface(BaseResp)
    ['{C702597C-8F91-4EA0-AA06-01BB91F71E4A}']
  end;

  /// <summary>发消息响应</summary>
  SendMessageToWXResp = interface(BaseResp)
    ['{3F183020-33C1-469A-8689-DB9CE8C62F1C}']
    function lang: NSString; cdecl;
    procedure setLang(lang: NSString); cdecl;
    function country: NSString; cdecl;
    procedure setCountry(country: NSString); cdecl;
  end;

  /// <summary>打开临时会话响应</summary>
  OpenTempSessionResp = interface(BaseResp)
    ['{8934A492-0051-4E83-B652-5D685ED573EE}']
  end;

  /// <summary>打开指定网址响应</summary>
  OpenWebviewResp = interface(BaseResp)
    ['{E132F3B9-471C-4F6E-B5AA-89181CEF4069}']
  end;

  /// <summary>打开硬件排行榜响应</summary>
  OpenRanklistResp = interface(BaseResp)
    ['{FC4635D7-8680-4526-94DD-9AFD6E1AD5A1}']
  end;

  /// <summary>请求添加卡券响应</summary>
  AddCardToWXCardPackageResp = interface(BaseResp)
    ['{44A8BDC7-0366-4566-B52C-BCC0527090EA}']
    /// <summary>卡列表（个数不能超过40个WXCardItem）</summary>
    function cardAry: NSArray; cdecl;
    procedure setCardAry(cardAry: NSArray); cdecl;
  end;

  /// <summary>请求选取卡券响应</summary>
  WXChooseCardResp = interface(BaseResp)
    ['{72913849-CE84-4964-81CC-643B7F110695}']
    /// <summary>卡列表（个数不能超过40个WXCardItem）</summary>
    function cardAry: NSArray; cdecl;
    procedure setCardAry(cardAry: NSArray); cdecl;
  end;

  /// <summary>微信请求提供内容的响应</summary>
  GetMessageFromWXResp = interface(BaseResp)
    ['{59AA8D34-236E-4983-B27C-C0E252DCE551}']
    /// <summary>向微信终端提供的文本内容</summary>
    /// <remarks>文本长度必须大于0且小于10K</remarks>
    function text: NSString; cdecl;
    procedure setText(text: NSString); cdecl;
    /// <summary>向微信终端提供的多媒体内容</summary>
    function message: WXMediaMessage; cdecl;
    procedure setMessage(message: WXMediaMessage); cdecl;
    /// <summary>是否是文本消息</summary>
    function bText: Boolean; cdecl;
    procedure setBText(bText: Boolean); cdecl;
  end;

  /// <summary>微信通知显示内容的响应</summary>
  ShowMessageFromWXResp = interface(BaseResp)
    ['{7C91EA04-C0A0-417F-9C61-07DF77091877}']
  end;

  // ------------------------------------------------------------------------------
  /// <summary>多媒体消息</summary>
  WXMediaMessage = interface(NSObject)
    ['{DBE9475E-43DE-4699-9D5F-41AC0D6666CA}']
    /// <summary>标题(长度不能超过512字节)</summary>
    function title: NSString; cdecl;
    procedure setTitle(title: NSString); cdecl;
    /// <summary>描述内容(长度不能超过1K)</summary>
    function description: NSString; cdecl;
    procedure setDescription(description: NSString); cdecl;
    /// <summary>缩略图数据(大小不能超过32K)</summary>
    function thumbData: NSData; cdecl;
    procedure setThumbData(thumbData: NSData); cdecl;
    /// <summary>媒体标签名(长度不能超过64字节)</summary>
    function mediaTagName: NSString; cdecl;
    procedure setMediaTagName(mediaTagName: NSString); cdecl;
    function messageExt: NSString; cdecl;
    procedure setMessageExt(messageExt: NSString); cdecl;
    function messageAction: NSString; cdecl;
    procedure setMessageAction(messageAction: NSString); cdecl;
    /// <summary>多媒体数据对象，可以为WXImageObject，WXMusicObject，WXVideoObject，WXWebpageObject等</summary>
    function mediaObject: Pointer; cdecl;
    procedure setMediaObject(mediaObject: Pointer); cdecl;
    /// <summary>设置消息缩略图(大小不能超过32K)</summary>
    procedure setThumbImage(image: UIImage); cdecl;
  end;

  WXMediaMessageClass = interface(NSObjectClass)
    ['{E47D92D1-ED26-419E-840A-CACFD7797FC4}']
    function message: WXMediaMessage; cdecl;
  end;

  TWXMediaMessage = class(TOCGenericImport<WXMediaMessageClass, WXMediaMessage>)
  end;

  /// <summary>图片数据对象</summary>
  WXImageObject = interface(NSObject)
    ['{5A346557-3ABB-4BEE-9E0A-03ABA319737F}']
    /// <summary>图片真实数据内容(大小不能超过10M)</summary>
    function imageData: NSData; cdecl;
    procedure setImageData(imageData: NSData); cdecl;
  end;

  WXImageObjectClass = interface(NSObjectClass)
    ['{57F80B15-17FF-4C18-977E-8706DE5BE724}']
    [MethodName('object')]
    function _object: WXImageObject; cdecl;
  end;

  TWXImageObject = class(TOCGenericImport<WXImageObjectClass, WXImageObject>)
  end;

  /// <summary>音乐数据对象</summary>
  WXMusicObject = interface(NSObject)
    ['{9EFD0E25-7B2A-4044-ACB0-220EF151E0FC}']
    /// <summary>音乐网页的url地址(长度不能超过10K)</summary>
    function musicUrl: NSString; cdecl;
    procedure setMusicUrl(musicUrl: NSString); cdecl;
    /// <summary>音乐lowband网页的url地址(长度不能超过10K)</summary>
    function musicLowBandUrl: NSString; cdecl;
    procedure setMusicLowBandUrl(musicLowBandUrl: NSString); cdecl;
    /// <summary>音乐数据url地址(长度不能超过10K)</summary>
    function musicDataUrl: NSString; cdecl;
    procedure setMusicDataUrl(musicDataUrl: NSString); cdecl;
    /// <summary>音乐lowband数据url地址(长度不能超过10K)</summary>
    function musicLowBandDataUrl: NSString; cdecl;
    procedure setMusicLowBandDataUrl(musicLowBandDataUrl: NSString); cdecl;
  end;

  WXMusicObjectClass = interface(NSObjectClass)
    ['{FF46237F-F8BC-4444-A96C-CC40901406D7}']
    /// <summary>返回一个WXMusicObject对象(自动释放)</summary>
    [MethodName('object')]
    function _object: WXMusicObject; cdecl;
  end;

  TWXMusicObject = class(TOCGenericImport<WXMusicObjectClass, WXMusicObject>)
  end;

  /// <summary>视频数据对象</summary>
  WXVideoObject = interface(NSObject)
    ['{DF30900E-6A3E-4296-98A9-E1DE67D18703}']
    /// <summary>视频网页的url地址(长度不能超过10K)</summary>
    function videoUrl: NSString; cdecl;
    procedure setVideoUrl(videoUrl: NSString); cdecl;
    /// <summary>视频lowband网页的url地址(长度不能超过10K)</summary>
    function videoLowBandUrl: NSString; cdecl;
    procedure setVideoLowBandUrl(videoLowBandUrl: NSString); cdecl;
  end;

  WXVideoObjectClass = interface(NSObjectClass)
    ['{BC4FA2FD-C81E-4D72-8626-C3BEAC1CDB6C}']
    [MethodName('object')]
    function _object: WXVideoObject; cdecl;
  end;

  TWXVideoObject = class(TOCGenericImport<WXVideoObjectClass, WXVideoObject>)
  end;

  /// <summary>网页数据对象</summary>
  WXWebpageObject = interface(NSObject)
    ['{4A693270-7940-4D5E-A659-87A2867D72DE}']
    /// <summary>网页的url地址(长度不能超过10K)</summary>
    function webpageUrl: NSString; cdecl;
    procedure setWebpageUrl(webpageUrl: NSString); cdecl;
  end;

  WXWebpageObjectClass = interface(NSObjectClass)
    ['{619F490C-4C0A-427D-B2EE-C5B924EBEDC9}']
    [MethodName('object')]
    function _object: WXWebpageObject; cdecl;
  end;

  TWXWebpageObject = class(TOCGenericImport<WXWebpageObjectClass,
    WXWebpageObject>)
  end;

  /// <summary>App扩展数据对象</summary>
  WXAppExtendObject = interface(NSObject)
    ['{2AC0B025-CC2C-4B07-AF1A-57614450DC6F}']
    /// <summary>若第三方程序不存在，微信终端会打开该url所指的App下载地址(长度不能超过10K)</summary>
    function url: NSString; cdecl;
    procedure setUrl(url: NSString); cdecl;
    /// <summary>第三方程序自定义简单数据，微信终端会回传给第三方程序处理(长度不能超过2K)</summary>
    function extInfo: NSString; cdecl;
    procedure setExtInfo(extInfo: NSString); cdecl;
    /// <summary>App文件数据，该数据发送给微信好友，微信好友需要点击后下载数据，微信终端会回传给第三方程序处理(不能超过10M)</summary>
    function fileData: NSData; cdecl;
    procedure setFileData(fileData: NSData); cdecl;
  end;

  WXAppExtendObjectClass = interface(NSObjectClass)
    ['{57D0899C-454D-4A97-B77E-3C1CAB515239}']
    [MethodName('object')]
    function _object: WXAppExtendObject; cdecl;
  end;

  TWXAppExtendObject = class(TOCGenericImport<WXAppExtendObjectClass,
    WXAppExtendObject>)
  end;

  WXEmoticonObject = interface(NSObject)
    ['{468BA611-B5EC-4546-A8BD-56EC57F0931C}']
    /// <summary>表情真实数据内容(不能超过10M)</summary>
    function emoticonData: NSData; cdecl;
    procedure setEmoticonData(emoticonData: NSData); cdecl;
  end;

  WXEmoticonObjectClass = interface(NSObjectClass)
    ['{CF96FDDA-4AE9-443A-903C-F7D76FA1DA5B}']
    [MethodName('object')]
    function _object: WXEmoticonObject; cdecl;
  end;

  TWXEmoticonObject = class(TOCGenericImport<WXEmoticonObjectClass,
    WXEmoticonObject>)
  end;

  WXFileObject = interface(NSObject)
    ['{B8B681B2-5CA8-425B-B802-41F63A4C1A73}']
    /// <summary>文件后缀名(长度不能超过64B)</summary>
    function fileExtension: NSString; cdecl;
    procedure setFileExtension(fileExtension: NSString); cdecl;
    /// <summary>文件真实数据内容(不能超过10M)</summary>
    function fileData: NSData; cdecl;
    procedure setFileData(fileData: NSData); cdecl;
  end;

  WXFileObjectClass = interface(NSObjectClass)
    ['{7FE49721-CC4B-4E0E-8B52-D3EF481FB642}']
    [MethodName('object')]
    function _object: WXFileObject; cdecl;
  end;

  TWXFileObject = class(TOCGenericImport<WXFileObjectClass, WXFileObject>)
  end;

  WXLocationObject = interface(NSObject)
    ['{F2817744-0887-4BF6-B31E-8DF1CF2CA434}']
    /// <summary>经度</summary>
    function lng: Double; cdecl;
    procedure setLng(lng: Double); cdecl;
    /// <summary>纬度</summary>
    function lat: Double; cdecl;
    procedure setLat(lat: Double); cdecl;
  end;

  WXLocationObjectClass = interface(NSObjectClass)
    ['{F3426D6B-0BDF-497B-8086-95DAF300D660}']
    [MethodName('object')]
    function _object: WXLocationObject; cdecl;
  end;

  TWXLocationObject = class(TOCGenericImport<WXLocationObjectClass,
    WXLocationObject>)
  end;

  WXTextObject = interface(NSObject)
    ['{816ABB06-F0FA-4DF7-A7E8-90399C783806}']
    /// <summary>文本内容</summary>
    function contentText: NSString; cdecl;
    procedure setContentText(contentText: NSString); cdecl;
  end;

  WXTextObjectClass = interface(NSObjectClass)
    ['{01B24EA7-9EFB-46A9-B470-A778AA87EDBC}']
    [MethodName('object')]
    function _object: WXTextObject; cdecl;
  end;

  TWXTextObject = class(TOCGenericImport<WXTextObjectClass, WXTextObject>)
  end;

  /// <summary>卡券</summary>
  WXCardItem = interface(NSObject)
    ['{97C86C90-37A8-4B9D-A6A7-276DBB42A168}']
    /// <summary>卡id(长度不能超过512字节)</summary>
    function cardId: NSString; cdecl;
    procedure setCardId(cardId: NSString); cdecl;
    /// <summary>ext信息(长度不能超过2024字节)</summary>
    function extMsg: NSString; cdecl;
    procedure setExtMsg(extMsg: NSString); cdecl;
    /// <summary>卡的状态,req不需要填(resp:0为未添加，1为已添加)</summary>
    function cardState: UInt32; cdecl;
    procedure setCardState(cardState: UInt32); cdecl;
    /// <remarks>req不需要填，chooseCard返回的</remarks>
    function encryptCode: NSString; cdecl;
    procedure setEncryptCode(encryptCode: NSString); cdecl;
    /// <remarks>req不需要填，chooseCard返回的</remarks>
    function appid: NSString; cdecl;
    procedure setAppID(appid: NSString); cdecl;
  end;

  WXCardItemClass = interface(NSObjectClass)
    ['{52B17245-F914-4970-B4F2-637948A15022}']
    { class method declarations }
  end;

  TWXCardItem = class(TOCGenericImport<WXCardItemClass, WXCardItem>)
  end;

  WechatAuthSDK = interface(NSObject)
    ['{148DE279-2C4F-4777-A6FF-E7BCAEF953D2}']
    /// <summary> WechatAuthAPIDelegate</summary>
    function delegate: Pointer; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    /// <summary>authSDK版本号</summary>
    function sdkVersion: NSString; cdecl;
    procedure setSdkVersion(sdkVersion: NSString); cdecl;
    /// <summary>发送登录请求，等待WechatAuthAPIDelegate回调</summary>
    /// <param name="appId">微信开发者ID</param>
    /// <param name="nonceStr">一个随机的尽量不重复的字符串，用来使得每次的signature不同</param>
    /// <param name="timeStamp">时间戳</param>
    /// <param name="scope">应用授权作用域，拥有多个作用域用逗号（,）分隔</param>
    /// <param name="signature">签名</param>
    /// <param name="schemeData">会在扫码后拼在scheme后</param>
    /// <remarks>该实现只保证同时只有一个Auth在运行，Auth未完成或未Stop再次调用Auth接口时会返回False</remarks>
    function Auth(appid, nonceStr, timeStamp, scope, signature,
      schemeData: NSString): Boolean; cdecl;
    /// <summary>暂停登录请求</summary>
    function StopAuth: Boolean; cdecl;
  end;

  WechatAuthSDKClass = interface(NSObjectClass)
    ['{14058241-BD3B-42C7-90D0-D3F0D2B6CF41}']
    { class method declarations }
  end;

  TWechatAuthSDK = class(TOCGenericImport<WechatAuthSDKClass, WechatAuthSDK>)
  end;

  /// <summary>接收并处理来自微信终端程序的事件委托</summary>
  WXApiDelegate = interface(IObjectiveC)
    ['{892B17F1-1C38-497B-B6E6-12A451B09D6B}']
    /// <summary>收到一个来自微信的请求</summary>
    /// <param name="req">具体请求内容，是自动释放的</param>
    /// <remarks>第三方应用程序异步处理完成后必须调用sendResp发送处理结果给微信。可能收到的请求有GetMessageFromWXReq、ShowMessageFromWXReq等。</remarks>
    procedure onReq(req: BaseReq); cdecl;
    /// <summary>发送一个sendReq后，收到微信的回应</summary>
    /// <param name="resp">具体的回应内容，是自动释放的</param>
    /// <remarks>可能收到的处理结果有SendMessageToWXResp、SendAuthResp等。</remarks>
    procedure onResp(resp: BaseResp); cdecl;
  end;

  WechatAuthAPIDelegate = interface(IObjectiveC)
    ['{44080D14-CDAE-4178-938A-268438059B7C}']
    /// <summary>得到二维码</summary>
    procedure onAuthGotQrcode(image: UIImage); cdecl;
    /// <summary>二维码被扫描</summary>
    procedure onQrcodeScanned; cdecl;
    /// <summary>成功登录</summary>
    procedure onAuthFinish(errCode: Integer; authCode: NSString); cdecl;
  end;

  TWeChatHelper = class(TOCLocal, WXApiDelegate)
  private
    class

      var WCH: TWeChatHelper;
    class function GetDefault: TWeChatHelper; static;
  public
    /// <summary>检查微信是否已被用户安装。</summary>
    /// <remarks>该方法已失效</remarks>
    class function isWXAppInstalled: Boolean;
    /// <summary>判断当前微信的版本是否支持OpenApi</summary>
    /// <remarks>该方法已失效</remarks>
    class function isWXAppSupportApi: Boolean;
    /// <summary>获取微信的itunes安装地址。</summary>
    class function getWXAppInstallUrl: string;
    /// <summary>获取当前微信SDK的版本号。</summary>
    class function getApiVersion: string;
    class property Default: TWeChatHelper read GetDefault;
  private
    FScene: Integer;
  protected
    { WXApiDelegate }
    procedure onReq(req: BaseReq); cdecl;
    procedure onResp(resp: BaseResp); cdecl;
  public
    function registerApp(const appid: string;
      const appdesc: string = ''): Boolean;
    /// <summary>打开微信。</summary>
    function openWXApp: Boolean;
    function DoHandleOpenURL(AContext: TObject): Boolean;
    procedure SendMessageToWX(const AText: string);
    property scene: Integer read FScene write FScene;
  end;

const
  /// <summary>发送场景：聊天界面</summary>
  WXSceneSession = 0;
  /// <summary>发送场景：朋友圈</summary>
  WXSceneTimeline = 1;
  /// <summary>发送场景：收藏</summary>
  WXSceneFavorite = 2;

  /// <summary>跳转profile类型：普通公众号</summary>
  WXBizProfileType_Normal = 0;
  /// <summary>跳转profile类型：硬件公众号</summary>
  WXBizProfileType_Device = 1;
  /// <summary>跳转profile网页版类型：广告网页</summary>
  WXMPWebviewType_Ad = 0;

  /// <summary>成功</summary>
  WXSuccess = 0;
  /// <summary>普通错误类型</summary>
  WXErrCodeCommon = -1;
  /// <summary>用户点击取消并返回</summary>
  WXErrCodeUserCancel = -2;
  /// <summary>发送失败</summary>
  WXErrCodeSentFail = -3;
  /// <summary>授权失败</summary>
  WXErrCodeAuthDeny = -4;
  /// <summary>微信不支持</summary>
  WXErrCodeUnsupport = -5;

  WXAPISupportSession = 0;

  // 应用支持接收微信的文件类型
  MMAPP_SUPPORT_NOCONTENT = $0;
  MMAPP_SUPPORT_TEXT = $1;
  MMAPP_SUPPORT_PICTURE = $2;
  MMAPP_SUPPORT_LOCATION = $4;
  MMAPP_SUPPORT_VIDEO = $8;
  MMAPP_SUPPORT_AUDIO = $10;
  MMAPP_SUPPORT_WEBPAGE = $20;
  // Suport File Type
  MMAPP_SUPPORT_DOC = $40; // doc
  MMAPP_SUPPORT_DOCX = $80; // docx
  MMAPP_SUPPORT_PPT = $100; // ppt
  MMAPP_SUPPORT_PPTX = $200; // pptx
  MMAPP_SUPPORT_XLS = $400; // xls
  MMAPP_SUPPORT_XLSX = $800; // xlsx
  MMAPP_SUPPORT_PDF = $1000; // pdf

  WechatAuth_Err_Ok = 0; // Auth成功
  WechatAuth_Err_NormalErr = -1; // 普通错误
  WechatAuth_Err_NetworkErr = -2; // 网络错误
  WechatAuth_Err_GetQrcodeFailed = -3; // 获取二维码失败
  WechatAuth_Err_Cancel = -4; // 用户取消授权
  WechatAuth_Err_Timeout = -5; // 超时

implementation

uses QSDK.Wechat;
{$IFDEF CPUARM}
{$O-}
function WXApi_FakeLoader: WXApi; cdecl;
  external 'libWeChatSDK.a' name 'OBJC_CLASS_$_WXApi';
{$O+}
{$ENDIF}

{ TWeChatHelper }
class function TWeChatHelper.getApiVersion: string;
begin
  Result := NSStrToStr(TWXApi.OCClass.getApiVersion);
end;

class function TWeChatHelper.getWXAppInstallUrl: string;
begin
  Result := NSStrToStr(TWXApi.OCClass.getWXAppInstallUrl);
end;

class function TWeChatHelper.isWXAppInstalled: Boolean;
begin
  Result := TWXApi.OCClass.isWXAppInstalled;
end;

class function TWeChatHelper.isWXAppSupportApi: Boolean;
begin
  Result := TWXApi.OCClass.isWXAppSupportApi;
end;

class function TWeChatHelper.GetDefault: TWeChatHelper;
begin
  if WCH = nil then
  begin
    WCH := TWeChatHelper.Create;
  end;
  Result := WCH;
end;

function TWeChatHelper.registerApp(const appid, appdesc: string): Boolean;
begin
  if (appdesc = '') then
    Result := TWXApi.OCClass.registerApp(StrToNSStr(appid))
  else
    Result := TWXApi.OCClass.registerAppwithDescription(StrToNSStr(appid),
      StrToNSStr(appdesc));
end;

function TWeChatHelper.openWXApp: Boolean;
begin
  Result := TWXApi.OCClass.openWXApp;
end;

function TWeChatHelper.DoHandleOpenURL(AContext: TObject): Boolean;
begin
  Result := TWXApi.OCClass.handleOpenURL
    (StrToNSUrl(TiOSOpenApplicationContext(AContext).url), GetObjectID);
end;

procedure TWeChatHelper.SendMessageToWX(const AText: string);
var
  req: SendMessageToWXReq;
begin
  req := TSendMessageToWXReq.Wrap(TSendMessageToWXReq.Alloc.init);
  req.setText(StrToNSStr(AText));
  req.setBText(True);
  req.setScene(FScene);
  TWXApi.OCClass.sendReq(req);
end;

procedure TWeChatHelper.onReq(req: BaseReq);
begin
  // {
  // if([req isKindOfClass:[GetMessageFromWXReq class]])
  // {
  // // 微信请求App提供内容， 需要app提供内容后使用sendRsp返回
  // NSString *strTitle = [NSString stringWithFormat:@"微信请求App提供内容"];
  // NSString *strMsg = @"微信请求App提供内容，App要调用sendResp:GetMessageFromWXResp返回给微信";
  //
  // UIAlertView *alert = [[UIAlertView alloc] initWithTitle:strTitle message:strMsg delegate:self cancelButtonTitle:@"OK" otherButtonTitles:nil, nil];
  // alert.tag = 1000;
  // [alert show];
  // [alert release];
  // }
  // else if([req isKindOfClass:[ShowMessageFromWXReq class]])
  // {
  // ShowMessageFromWXReq* temp = (ShowMessageFromWXReq*)req;
  // WXMediaMessage *msg = temp.message;
  //
  // //显示微信传过来的内容
  // WXAppExtendObject *obj = msg.mediaObject;
  //
  // NSString *strTitle = [NSString stringWithFormat:@"微信请求App显示内容"];
  // NSString *strMsg = [NSString stringWithFormat:@"标题：%@ \n内容：%@ \n附带信息：%@ \n缩略图:%u bytes\n\n", msg.title, msg.description, obj.extInfo, msg.thumbData.length];
  //
  // UIAlertView *alert = [[UIAlertView alloc] initWithTitle:strTitle message:strMsg delegate:self cancelButtonTitle:@"OK" otherButtonTitles:nil, nil];
  // [alert show];
  // [alert release];
  // }
  // else if([req isKindOfClass:[LaunchFromWXReq class]])
  // {
  // //从微信启动App
  // NSString *strTitle = [NSString stringWithFormat:@"从微信启动"];
  // NSString *strMsg = @"这是从微信启动的消息";
  //
  // UIAlertView *alert = [[UIAlertView alloc] initWithTitle:strTitle message:strMsg delegate:self cancelButtonTitle:@"OK" otherButtonTitles:nil, nil];
  // [alert show];
  // [alert release];
  // }
  // }

end;

procedure TWeChatHelper.onResp(resp: BaseResp);
begin
  // -(void) onResp:(BaseResp*)resp
  // {
  // if([resp isKindOfClass:[SendMessageToWXResp class]])
  // {
  // NSString *strTitle = [NSString stringWithFormat:@"发送媒体消息结果"];
  // NSString *strMsg = [NSString stringWithFormat:@"errcode:%d", resp.errCode];
  //
  // UIAlertView *alert = [[UIAlertView alloc] initWithTitle:strTitle message:strMsg delegate:self cancelButtonTitle:@"OK" otherButtonTitles:nil, nil];
  // [alert show];
  // [alert release];
  // }
  // }

end;

end.
