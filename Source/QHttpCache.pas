unit qhttpcache;

interface

uses classes, sysutils, system.ioutils, system.Generics.collections,
  system.net.httpclient, system.net.UrlClient, dateutils,
  system.SyncObjs, qworker, qlog;

type
  /// 缓存项目的状态：未知、已存在、获取中、已就绪、无法找到，过期
  TCacheStatus = (Unknown, Exists, Fetching, Ready, Missed, Timeout);
  PCacheItemResultCallback = ^TCacheItemResultCallback;

  TCacheItemResultCallback = record
    Callback: TMethod;
    UserTag: Pointer;
    Next: PCacheItemResultCallback;
  end;

  /// <summary>缓存项目定义</summary>
  TCacheItem = record
    Url: String; // 原始URL
    ContentType: String; // 内容
    CacheId: String; // 缓存文件名
    LastModified, ExpireTime: TDateTime; // 过期时间
    ETag: String; // 项目的ETag，以便比较更新
    Status: TCacheStatus; // 缓存状态
    Hits: Integer; // 命中数
    FirstNotify: PCacheItemResultCallback; // 首个回调地址，先进后出
  end;

  PCacheItem = ^TCacheItem;

  // 缓存项目索引
  TCacheIndexes = TDictionary<String, PCacheItem>;

  /// <summary>
  /// 查找缓存结果就绪回调
  /// </summary>
  /// <param name="AItem">找到的缓存项目</param>             。
  /// <param name="AUserTag">用户自行定义的回调标记</param>

  TCacheLookupResult = procedure(const AItem: TCacheItem; AUserTag: Pointer) of object;
  /// TCacheLookupResult的内联函数版
  TCacheLookupResultA = reference to procedure(const AItem: TCacheItem; AUserTag: Pointer);
  // HttpClient对象支持函数，以便可以利用额外的缓冲池
  THttpClientGetEvent = procedure(var AClient: THttpClient) of object;
  THttpClientReleaseEvent = procedure(AClient: THttpClient) of object;

  // 本地缓存支持类

  TLocalCaches = class
  private
    FCacheDir: String;
    FMaxCacheTime: Cardinal; // 最大缓存时间，超过则认为缓存失效，单位为秒
    FMinCacheTime: Cardinal; // 最小缓存时间，单位为秒
    FItems: TCacheIndexes; // 索引表
    FSaveJob: IntPtr; // 保存作业句柄
    FLocker: TCriticalSection; // 临界锁
    FOnReleaseHttpClient: THttpClientReleaseEvent;
    FOnGetHttpClient: THttpClientGetEvent;
    FCacheIndexFileName: String;
    FIndexDirty: Boolean; // 索引是否需要刷新
    procedure GetContent(var ACache: TCacheItem);
    function GetHttpClient: THttpClient;
    procedure ReleaseHttpClient(AClient: THttpClient);
    procedure SetCacheDir(const Value: String);
    procedure SaveIndexes(AJob: PQJob);
    procedure LoadIndexes;
    procedure DoAysnGetContent(AJob: PQJob);
    procedure DoCacheReady(ACache: PCacheItem);
    procedure SetCacheIndexFileName(const Value: String);
  public
    constructor Create; overload;
    destructor Destroy; override;
    /// <summary>清除缓存信息</summary>
    /// <param name="ADeleteFile">是否同时清除真正的缓存文件</param>
    procedure Clear(ADeleteFile: Boolean = true);
    /// <summary>查找指定URL和缓存ID对应的文件，如果ACacheId为空，则以AUrl的MD5值为ACacheId的值</summary>
    /// <param name="AUrl">要缓存的URL地址</param>
    /// <param name="ACacheId">缓存后的文件名</param>
    /// <returns>返回完整的缓存文件路径</returns>
    function Lookup(AUrl, ACacheId: String): String; overload;
    /// <summary>查找指定URL和缓存ID对应的文件，如果ACacheId为空，则以AUrl的MD5值为ACacheId的值</summary>
    /// <param name="AUrl">要缓存的URL地址</param>
    /// <param name="ACacheId">缓存后的文件名</param>
    /// <param name="ACallback">缓存就绪后的回调函数</param>
    /// <param name="ATag">用户自定义的额外标签</param>
    /// <returns>返回完整的缓存文件路径</returns>
    procedure Lookup(AUrl, ACacheId: String; ACallback: TCacheLookupResult; ATag: Pointer); overload;
    /// <summary>查找指定URL和缓存ID对应的文件，如果ACacheId为空，则以AUrl的MD5值为ACacheId的值</summary>
    /// <param name="AUrl">要缓存的URL地址</param>
    /// <param name="ACacheId">缓存后的文件名</param>
    /// <param name="ACallback">缓存就绪后的回调函数</param>
    /// <param name="ATag">用户自定义的额外标签</param>
    /// <returns>返回完整的缓存文件路径</returns>
    procedure Lookup(AUrl, ACacheId: String; ACallback: TCacheLookupResultA; ATag: Pointer); overload;
    /// <summary>返回指定的缓存项目对应的文件的完整路径</summary>
    function CacheFile(const AItem: TCacheItem): String;
    /// <summary>判断指定的项目是否已经缓存</summary>
    /// <param name="AUrl">要判断的URL地址，区分大小写</param>
    /// <returns>如果已经缓存，返回true，否则返回false</returns>
    function Cached(const AUrl: String): Boolean;
    /// 缓存目录
    property CacheDir: String read FCacheDir write SetCacheDir;
    /// 获取HttpClient对象事件
    property OnGetHttpClient: THttpClientGetEvent read FOnGetHttpClient write FOnGetHttpClient;
    // 释放HttpClient对象事件
    property OnReleaseHttpClient: THttpClientReleaseEvent read FOnReleaseHttpClient write FOnReleaseHttpClient;
    property CacheIndexFileName: String read FCacheIndexFileName write SetCacheIndexFileName;
  end;

function HttpCaches: TLocalCaches;

implementation

uses qstring, qjson, qdigest;

var
  _Localcaches: TLocalCaches;

function HttpCaches: TLocalCaches;
begin
  Result := _Localcaches;
end;

{ TLocalCaches }

procedure TLocalCaches.LoadIndexes;
var
  AJson, AItem: TQJson;
  I: Integer;
  ACache: PCacheItem;
  AUrl: String;
begin
  FLocker.Enter;
  AJson := TQJson.Create;
  try
    if FileExists(CacheDir + CacheIndexFileName) then
    begin
      AJson.LoadFromFile(CacheDir + CacheIndexFileName);
      for I := 0 to AJson.Count - 1 do
      begin
        AItem := AJson[I];
        AUrl := AItem.ValueByName('url', '');
        if not FItems.TryGetValue(AUrl, ACache) then
        begin
          New(ACache);
          ACache.Url := AUrl;
          FItems.Add(ACache.Url, ACache);
        end;
        ACache.ContentType := AItem.ValueByName('type', '');
        ACache.CacheId := AItem.ValueByName('id', '');
        ACache.LastModified := AItem.DateTimeByName('modified', 0);
        ACache.ExpireTime := AItem.DateTimeByName('expires', 0);
        ACache.ETag := AItem.ValueByName('etag', '');
        ACache.FirstNotify := nil;
        if FileExists(CacheDir + ACache.CacheId) then
        begin
          if ACache.ExpireTime < Now then // 过期了，删除原来的文件
            ACache.Status := TCacheStatus.Timeout
          else
            ACache.Status := TCacheStatus.Ready;
        end
        else
          ACache.Status := TCacheStatus.Missed;
      end;
    end;
    FIndexDirty := false;
  finally
    FLocker.Leave;
    FreeObject(AJson);
  end;
end;

procedure TLocalCaches.Lookup(AUrl, ACacheId: String; ACallback: TCacheLookupResult; ATag: Pointer);
var
  ACache: PCacheItem;
  AFetchNeeded, AResultReady: Boolean;
  AData: PCacheItemResultCallback;
begin
  if FIndexDirty then
    LoadIndexes;
  AFetchNeeded := false;
  AResultReady := false;
  FLocker.Enter;
  try
    if (not FItems.TryGetValue(AUrl, ACache)) or (not FileExists(CacheDir + ACache.CacheId)) or (ACache.ExpireTime < Now) then
    begin
      AFetchNeeded := true;
      if not Assigned(ACache) then
      begin
        New(ACache);
        ACache.Url := AUrl;
        ACache.CacheId := ACacheId;
        ACache.Hits := 0;
        ACache.Status := TCacheStatus.Fetching;
        ACache.FirstNotify := nil;
        FItems.Add(AUrl, ACache);
      end
      else if ACache.Status = TCacheStatus.Fetching then
        AFetchNeeded := false;
      New(AData);
      AData.Callback := TMethod(ACallback);
      AData.UserTag := ATag;
      AData.Next := ACache.FirstNotify;
      ACache.FirstNotify := AData;
      if AFetchNeeded then
        ACache.Status := TCacheStatus.Fetching;
    end
    else
      AResultReady := true;
  finally
    FLocker.Leave;
  end;
  if AResultReady then
  begin
    Inc(ACache.Hits);
    if TMethod(ACallback).Data = Pointer(-1) then
    begin
      TCacheLookupResultA(TMethod(ACallback).Code)(ACache^, ATag);
      TCacheLookupResultA(TMethod(ACallback).Code) := nil;
    end
    else
      ACallback(ACache^, ATag);
  end
  else if AFetchNeeded then
    Workers.Post(DoAysnGetContent, ACache, false, jdfFreeByUser);
end;

procedure TLocalCaches.Lookup(AUrl, ACacheId: String; ACallback: TCacheLookupResultA; ATag: Pointer);
var
  AEvent: TCacheLookupResult;
  Alias: TMethod absolute AEvent;
begin
  Alias.Data := Pointer(-1);
  Alias.Code := nil;
  TCacheLookupResultA(Alias.Code) := ACallback;
  Lookup(AUrl, ACacheId, AEvent, ATag);
end;

function TLocalCaches.Lookup(AUrl, ACacheId: String): String;
var
  ACache: PCacheItem;
  AFetchNeeded: Boolean;
begin
  if FIndexDirty then
    LoadIndexes;
  AFetchNeeded := false;
  FLocker.Enter;
  try
    ACache := FItems.ExtractPair(AUrl).Value;
    if (not Assigned(ACache)) or (not FileExists(CacheDir + ACache.CacheId)) or (ACache.ExpireTime < Now) then
    begin
      AFetchNeeded := true;
      if not Assigned(ACache) then
      begin
        New(ACache);
        ACache.Url := AUrl;
        ACache.CacheId := ACacheId;
        ACache.Hits := 0;
        ACache.Status := TCacheStatus.Missed;
        ACache.FirstNotify := nil;
        FItems.Add(AUrl, ACache);
      end;
    end;
  finally
    FLocker.Leave;
  end;
  if AFetchNeeded then
    GetContent(ACache^)
  else
    AtomicIncrement(ACache.Hits);
  if ACache.Status = TCacheStatus.Ready then
    Result := CacheDir + ACache.CacheId
  else
    SetLength(Result, 0);
end;

function TLocalCaches.Cached(const AUrl: String): Boolean;
begin
  if FIndexDirty then
    LoadIndexes;
  FLocker.Enter;
  try
    Result := FItems.ContainsKey(AUrl);
  finally
    FLocker.Leave;
  end;
end;

function TLocalCaches.CacheFile(const AItem: TCacheItem): String;
begin
  Result := CacheDir + AItem.CacheId;
end;

procedure TLocalCaches.Clear(ADeleteFile: Boolean);
var
  AItem: PCacheItem;
begin
  FSaveJob := 0;

  for AItem in FItems.Values do
  begin
    if ADeleteFile then
      DeleteFile(CacheDir + AItem.CacheId);
    Dispose(AItem);
  end;
  FItems.Clear;
end;

constructor TLocalCaches.Create;
begin
  FItems := TDictionary<String, PCacheItem>.Create;
  FMaxCacheTime := MaxInt;
  FLocker := TCriticalSection.Create;
  FCacheIndexFileName := 'caches.index';
  FIndexDirty := true;
end;

destructor TLocalCaches.Destroy;
begin
  Clear(false);
  FreeObject(FItems);
  FreeObject(FLocker);
  inherited;
end;

procedure TLocalCaches.DoAysnGetContent(AJob: PQJob);
var
  ACache: PCacheItem;
begin
  ACache := AJob.Data;
  GetContent(ACache^);
  DoCacheReady(ACache);
end;

procedure TLocalCaches.DoCacheReady(ACache: PCacheItem);
var
  ACallback: PCacheItemResultCallback;
begin
  FLocker.Enter;
  try
    ACallback := ACache.FirstNotify;
    ACache.FirstNotify := nil;
  finally
    FLocker.Leave;
  end;
  while Assigned(ACallback) do
  begin
    if ACallback.Callback.Data = Pointer(-1) then
    begin
      TCacheLookupResultA(ACallback.Callback.Code)(ACache^, ACallback.UserTag);
      TCacheLookupResultA(ACallback.Callback.Code) := nil;
    end
    else
      TCacheLookupResult(ACallback.Callback)(ACache^, ACallback.UserTag);
    ACallback := ACallback.Next;
  end;
end;

procedure TLocalCaches.GetContent(var ACache: TCacheItem);
var
  ARequest: THttpClient;
  ACacheStream: TStream;
  AHeaders: TNetHeaders;
  AReply: IHttpResponse;
  ADelta: Int64;
  S, T: String;
  procedure UpdateCache;
  var
    AMaxExpireTime: TDateTime;
  begin
    FLocker.Enter;
    try
      ACache.ETag := DequotedStrW(AReply.HeaderValue['ETag'], '"');
      if not ParseWebTime(PQCharW(AReply.HeaderValue['Last-Modified']), ACache.LastModified) then
        ACache.LastModified := Now;
      if not ParseWebTime(PQCharW(AReply.HeaderValue['Expires']), ACache.ExpireTime) then
      begin
        S := AReply.HeaderValue['Cache-Control'];
        while Length(S) > 0 do
        // 取出MaxAge的值
        begin
          T := DecodeTokenW(S, ',', #0, true, true);
          if StartWithW(PQCharW(T), 'max-age', true) then
          begin
            T := Trim(ValueOfW(T, '='));
            ACache.ExpireTime := IncSecond(Now, StrToIntDef(T, 0));
            break;
          end;
        end;
      end;
      AMaxExpireTime := IncDay(Now, 60);
      if ACache.ExpireTime > AMaxExpireTime then // 超期时间如果大于60天，则按60天计算
        ACache.ExpireTime := AMaxExpireTime;
      ACache.ContentType := AReply.HeaderValue['Content-Type'];
      ADelta := SecondsBetween(ACache.LastModified, ACache.ExpireTime);
      if ADelta < FMinCacheTime then
        ACache.ExpireTime := IncSecond(ACache.LastModified, FMinCacheTime);
      if ADelta > FMaxCacheTime then
        ACache.ExpireTime := IncSecond(ACache.LastModified, FMaxCacheTime);
      ACache.Status := TCacheStatus.Ready;
    finally
      FLocker.Leave;
    end;
  end;

begin
  if Length(ACache.CacheId) = 0 then
    ACache.CacheId := DigestToString(MD5Hash(ACache.Url));
  ARequest := GetHttpClient;
  if ACache.Status in [TCacheStatus.Exists, TCacheStatus.Ready, TCacheStatus.Timeout] then
  begin
    if Length(ACache.ETag) > 0 then
    begin
      SetLength(AHeaders, 2);
      AHeaders[0].Name := 'If-None-Match';
      AHeaders[0].Value := ACache.ETag;
    end
    else
      SetLength(AHeaders, 1);
    AHeaders[High(AHeaders)].Name := 'If-Modified-Since';
    AHeaders[High(AHeaders)].Value := EncodeWebTime(ACache.LastModified);
  end
  else
    AHeaders := nil;
  ACacheStream := nil;
  try
    AReply := ARequest.Get(ACache.Url, nil, AHeaders);
    if AReply.StatusCode = 200 then
    begin
      UpdateCache;
      ACacheStream := TFileStream.Create(CacheDir + ACache.CacheId, fmCreate);
      ACacheStream.CopyFrom(AReply.ContentStream, 0);
      if FSaveJob = 0 then
        // 延时15秒而不是立即保存索引，这有助于减少不必要的IO操作
        FSaveJob := Workers.Delay(SaveIndexes, 15 * Q1Second, nil, true);
    end
    else
    begin
      if AReply.StatusCode = 304 then // Not modified
      begin
        ACache.Status := TCacheStatus.Ready;
      end
      else if AReply.StatusCode = 404 then // 找不到了
      begin
        if ACache.Status = TCacheStatus.Exists then
          DeleteFile(ACache.CacheId);
        ACache.Status := TCacheStatus.Missed;
      end
      else // Todo:出错处理
      begin
        ACache.Status := TCacheStatus.Missed;
      end;
    end;
  finally
    if Assigned(ACacheStream) then
      FreeObject(ACacheStream);
    ReleaseHttpClient(ARequest);
  end;
end;

function TLocalCaches.GetHttpClient: THttpClient;
begin

  Result := THttpClient.Create;
end;

procedure TLocalCaches.ReleaseHttpClient(AClient: THttpClient);
begin
  FreeObject(AClient);
end;

procedure TLocalCaches.SaveIndexes(AJob: PQJob);
var
  AJson: TQJson;
  AItem: PCacheItem;
begin
  CalcPerf('保存索引');
  FSaveJob := 0;
  AJson := TQJson.Create;
  try
    AJson.DataType := jdtArray;
    for AItem in FItems.Values do
    begin
      with AJson.Add() do
      begin
        Add('url').AsString := AItem.Url;
        Add('id').AsString := AItem.CacheId;
        Add('type').AsString := AItem.ContentType;
        Add('modified').AsDateTime := AItem.LastModified;
        Add('expires').AsDateTime := AItem.ExpireTime;
        Add('etag').AsString := AItem.ETag;
      end;
    end;
    AJson.SaveToFile(CacheDir + FCacheIndexFileName);
  finally
    FreeObject(AJson);
  end;
end;

procedure TLocalCaches.SetCacheDir(const Value: String);
begin
  if FCacheDir <> Value then
  begin
    Clear(false);
    if not EndWithW(Value, TPath.DirectorySeparatorChar, false) then
      FCacheDir := Value + TPath.DirectorySeparatorChar
    else
      FCacheDir := Value;
    FIndexDirty := true;
  end;

end;

procedure TLocalCaches.SetCacheIndexFileName(const Value: String);
begin
  if FCacheIndexFileName <> Value then
  begin
    FCacheIndexFileName := Value;
    FIndexDirty := true;
  end;
end;

initialization

_Localcaches := TLocalCaches.Create;
_Localcaches.CacheDir := TPath.GetCachePath;

finalization

FreeAndNil(_Localcaches)

end.
