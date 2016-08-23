unit qsdk.wechat;

interface

uses system.classes, system.SysUtils, FMX.Graphics;

type

  // Wechat Objects
  IWechatObject = IInterface;

  TMediaObjectType = (otUnknown, otText, otImage, otMusic, otVideo, otUrl,
    otFile, otAppData, otEmoji, otProduct, otEmotionGift, otDeviceAccess,
    otMailProduct, otOldTV, otEmotionShared, otCardShare, otLocationShare,
    otRecord, otTV, otDesignerShared);

  IWechatMediaMessage = interface
    ['{46D42E74-E148-4FC5-AFEC-706F4BA77B62}']
    function getTitle: String;
    procedure setTitle(const Value: String);
    function getDescription: String;
    procedure setDescription(const Value: String);
    function getThumb: TBitmap;
    procedure setThumb(ABitmap: TBitmap);
    function getMediaObject: IWechatObject;
    procedure setMediaObject(AObject: IWechatObject);
    function getMediaTagName: String;
    procedure setMediaTagName(const AValue: String);
    function getMessageAction: String;
    procedure setMessageAction(const AValue: String);
    function getMessageExt: String;
    procedure setMessageExt(const AValue: String);

    property Title: String read getTitle write setTitle;
    property Description: String read getDescription write setDescription;
    property Thumb: TBitmap read getThumb write setThumb;
    property MediaObject: IWechatObject read getMediaObject
      write setMediaObject;
    property MediaTagName: String read getMediaTagName write setMediaTagName;
    property MessageAction: String read getMessageAction write setMessageAction;
    property MessageExt: String read getMessageExt write setMessageExt;
  end;

  IWechatAppExt = interface
    ['{93D6174A-8DE6-4B70-AD60-94FB1935B97D}']
    function getExtInfo: String;
    procedure setExtInfo(const AValue: String);
    function getFilePath: String;
    procedure setFilePath(const AValue: String);
    function getFileData: TBytes;
    procedure setFileData(const AValue: TBytes);
    property ExtInfo: String read getExtInfo write setExtInfo;
    property FilePath: String read getFilePath write setFilePath;
    property FileData: TBytes read getFileData write setFileData;
  end;

  IWechatText = interface
    ['{01239E30-1D80-4155-BEB5-907226753781}']
    function getText: String;
    procedure setText(const AValue: String);
    property Text: String read getText write setText;
  end;

  IWechatImage = interface
    ['{B015BE08-4F95-4C08-8F2A-11B1B9430B20}']
    function getImagePath: String;
    procedure setImagePath(AValue: String);
    function getImage: TBitmap;
    procedure setImage(ABitmap: TBitmap);

    property ImagePath: String read getImagePath write setImagePath;
    property Imaget: TBitmap read getImage write setImage;
  end;

  IWechatMusic = interface
    ['{C4ED7560-E07B-43A2-AB17-2D287C17BAD4}']
    function getMusicUrl: String;
    procedure setMusicUrl(const AValue: String);
    function getMusicLowBandUrl: String;
    procedure setMusicLowBandUrl(const AValue: String);
    function getMusicDataUrl: String;
    procedure setMusicDataUrl(const AValue: String);
    function getMusicLowBandDataUrl: String;
    procedure setMusicLowBandDataUrl(const AValue: String);

    property MusicUrl: String read getMusicUrl write setMusicUrl;
    property MusicLowBandUrl: String read getMusicLowBandUrl
      write setMusicLowBandUrl;
    property MusicDataUrl: String read getMusicDataUrl write setMusicDataUrl;
    property MusicLowBandDataUrl: String read getMusicLowBandDataUrl
      write setMusicLowBandDataUrl;
  end;

  IWechatVideo = interface
    ['{0C43F90E-02FF-4292-9485-2287F5CB4749}']
    function getVideoUrl: String;
    procedure setVideoUrl(const AValue: String);
    function getVideoLowBandUrl: String;
    procedure setVideoLowBandUrl(const AValue: String);
    property VideoUrl: String read getVideoUrl write setVideoUrl;
    property VideoLowBandUrl: String read getVideoLowBandUrl
      write setVideoLowBandUrl;
  end;

  IWechatWebPage = interface
    ['{B7414EA8-AEDA-4DFD-9DC9-A167204D1E70}']
    function getWebPageUrl: String;
    procedure setWebPageUrl(const AValue: String);
    function getExtInfo: String;
    procedure setExtInfo(const AValue: String);
    { Property }
    property WebPageUrl: String read getWebPageUrl write setWebPageUrl;
    property ExtInfo: String read getExtInfo write setExtInfo;
  end;

  IWechatEmoticon = interface
    ['{3CA5F5CB-38FF-4DDF-BBDF-EECC7C9F344F}']
    function getEmoticonData: TBytes;
    procedure setEmoticonData(const AValue: TBytes);
    property EmoticonData: TBytes read getEmoticonData write setEmoticonData;
  end;

  IWechatFile = interface
    ['{C5FB3AC2-3D65-4A16-9F15-0943B7F4F819}']
    function getFileExt: String;
    procedure setFileExt(const AValue: String);
    function getFileData: TBytes;
    procedure setFileData(const AValue: TBytes);
    property FileExt: String read getFileExt write setFileExt;
    property FileData: TBytes read getFileData write setFileData;
  end;

  IWechatLocation = interface
    ['{D0EA31ED-5972-4430-8720-DCEA91657862}']
    function getLng: Double;
    procedure setLng(const AValue: Double);
    function getLat: Double;
    procedure setLat(const AValue: Double);
    property Lng: Double read getLng write setLng;
    property Lat: Double read getLat write setLat;
  end;

  IWechatRequest = interface
    ['{4FF6EC96-563B-4F66-9129-BD7ABF7416F1}']
    function getOpenID: String;
    procedure setOpenID(const AValue: String);
    function getTansaction: String;
    procedure setTransaction(const AValue: String);
    property OpenID: String read getOpenID write setOpenID;
    property Transaction: String read getTansaction write setTransaction;
  end;

  IWechatResponse = interface
    ['{02FF62AF-5705-4047-947E-7F6019A9474E}']
    function getOpenID: String;
    procedure setOpenID(const AValue: String);
    function getTansaction: String;
    procedure setTransaction(const AValue: String);
    function getErrorCode: Integer;
    procedure setErrorCode(const ACode: Integer);
    function getErrorMsg: String;
    procedure setErrorMsg(const AValue: String);
    property OpenID: String read getOpenID write setOpenID;
    property Transaction: String read getTansaction write setTransaction;
    property ErrorCode: Integer read getErrorCode write setErrorCode;
    property ErrorMsg: String read getErrorMsg write setErrorMsg;
  end;

  IWechatAddCardToPackageRequest = interface(IWechatRequest)

  end;

  TWechatRequestEvent = procedure(ARequest: IWechatRequest) of object;
  TWechatResponseEvent = procedure(AResponse: IWechatResponse) of object;
  TWechatSession=(Session,Timeline,Favorite);
  IWechatService = interface
    ['{10370690-72BC-438C-8105-042D2029B895}']
    procedure Unregister;
    function IsInstalled: Boolean;
    function getAppId: String;
    function OpenWechat: Boolean;
    function IsAPISupported: Boolean;
    function SendRequest(ARequest: IWechatRequest): Boolean;
    function SendResponse(AResp: IWechatResponse): Boolean;
    function getOnRequest: TWechatRequestEvent;
    procedure setOnRequest(const AEvent: TWechatRequestEvent);
    function getOnResponse: TWechatResponseEvent;
    procedure setOnResponse(const AEvent: TWechatResponseEvent);
    procedure setAppId(const AId: String);
    function OpenUrl(const AUrl: String): Boolean;
    function SendText(const AText: String;ASession:TWechatSession=TWechatSession.Session): Boolean;
    function CreateObject(AObjId: TGuid): IWechatObject;
    property AppId: String read getAppId write setAppId;
    property Installed: Boolean read IsInstalled;
    property APISupported: Boolean read IsAPISupported;
    property OnRequest: TWechatRequestEvent read getOnRequest
      write setOnRequest;
    property OnResponse: TWechatResponseEvent read getOnResponse
      write setOnResponse;
  end;

  TWechatObject = class(TObject, IInterface)
  protected
{$IFNDEF AUTOREFCOUNT}
    [Volatile]
    FRefCount: Integer;
{$ENDIF}
  public
    function QueryInterface(const IID: TGuid; out Obj): HResult;
      virtual; stdcall;
    function _AddRef: Integer; virtual; stdcall;
    function _Release: Integer; virtual; stdcall;
  end;

function WechatService: IWechatService;

implementation

uses FMX.platform{$IFDEF ANDROID}, qsdk.wechat.android{$ENDIF}
{$IFDEF IOS}, qsdk.wechat.ios{$ENDIF};

function WechatService: IWechatService;
begin
  if not TPlatformServices.Current.SupportsPlatformService(IWechatService,
    Result) then
    Result := nil;
end;

{ TWechatObject }

function TWechatObject.QueryInterface(const IID: TGuid; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TWechatObject._AddRef: Integer;
begin
{$IFNDEF AUTOREFCOUNT}
  Result := AtomicIncrement(FRefCount);
{$ELSE}
  Result := __ObjAddRef;
{$ENDIF}
end;

function TWechatObject._Release: Integer;
begin
{$IFNDEF AUTOREFCOUNT}
  Result := AtomicDecrement(FRefCount);
  if Result = 0 then
  begin
    // Mark the refcount field so that any refcounting during destruction doesn't infinitely recurse.
    __MarkDestroying(Self);
    Destroy;
  end;
{$ELSE}
  Result := __ObjRelease;
{$ENDIF}
end;

initialization

RegisterWechatService;

end.
