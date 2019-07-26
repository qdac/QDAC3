unit qhttpcache;

interface

uses classes, sysutils, system.ioutils, system.Generics.collections,
  system.net.httpclient, system.net.UrlClient, dateutils,
  system.SyncObjs, qworker, qlog;

type
  /// ������Ŀ��״̬��δ֪���Ѵ��ڡ���ȡ�С��Ѿ������޷��ҵ�������
  TCacheStatus = (Unknown, Exists, Fetching, Ready, Missed, Timeout);
  PCacheItemResultCallback = ^TCacheItemResultCallback;

  TCacheItemResultCallback = record
    Callback: TMethod;
    UserTag: Pointer;
    Next: PCacheItemResultCallback;
  end;

  /// <summary>������Ŀ����</summary>
  TCacheItem = record
    Url: String; // ԭʼURL
    ContentType: String; // ����
    CacheId: String; // �����ļ���
    LastModified, ExpireTime: TDateTime; // ����ʱ��
    ETag: String; // ��Ŀ��ETag���Ա�Ƚϸ���
    Status: TCacheStatus; // ����״̬
    Hits: Integer; // ������
    FirstNotify: PCacheItemResultCallback; // �׸��ص���ַ���Ƚ����
  end;

  PCacheItem = ^TCacheItem;

  // ������Ŀ����
  TCacheIndexes = TDictionary<String, PCacheItem>;

  /// <summary>
  /// ���һ����������ص�
  /// </summary>
  /// <param name="AItem">�ҵ��Ļ�����Ŀ</param>             ��
  /// <param name="AUserTag">�û����ж���Ļص����</param>

  TCacheLookupResult = procedure(const AItem: TCacheItem; AUserTag: Pointer) of object;
  /// TCacheLookupResult������������
  TCacheLookupResultA = reference to procedure(const AItem: TCacheItem; AUserTag: Pointer);
  // HttpClient����֧�ֺ������Ա�������ö���Ļ����
  THttpClientGetEvent = procedure(var AClient: THttpClient) of object;
  THttpClientReleaseEvent = procedure(AClient: THttpClient) of object;

  // ���ػ���֧����

  TLocalCaches = class
  private
    FCacheDir: String;
    FMaxCacheTime: Cardinal; // ��󻺴�ʱ�䣬��������Ϊ����ʧЧ����λΪ��
    FMinCacheTime: Cardinal; // ��С����ʱ�䣬��λΪ��
    FItems: TCacheIndexes; // ������
    FSaveJob: IntPtr; // ������ҵ���
    FLocker: TCriticalSection; // �ٽ���
    FOnReleaseHttpClient: THttpClientReleaseEvent;
    FOnGetHttpClient: THttpClientGetEvent;
    FCacheIndexFileName: String;
    FIndexDirty: Boolean; // �����Ƿ���Ҫˢ��
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
    /// <summary>���������Ϣ</summary>
    /// <param name="ADeleteFile">�Ƿ�ͬʱ��������Ļ����ļ�</param>
    procedure Clear(ADeleteFile: Boolean = true);
    /// <summary>����ָ��URL�ͻ���ID��Ӧ���ļ������ACacheIdΪ�գ�����AUrl��MD5ֵΪACacheId��ֵ</summary>
    /// <param name="AUrl">Ҫ�����URL��ַ</param>
    /// <param name="ACacheId">�������ļ���</param>
    /// <returns>���������Ļ����ļ�·��</returns>
    function Lookup(AUrl, ACacheId: String): String; overload;
    /// <summary>����ָ��URL�ͻ���ID��Ӧ���ļ������ACacheIdΪ�գ�����AUrl��MD5ֵΪACacheId��ֵ</summary>
    /// <param name="AUrl">Ҫ�����URL��ַ</param>
    /// <param name="ACacheId">�������ļ���</param>
    /// <param name="ACallback">���������Ļص�����</param>
    /// <param name="ATag">�û��Զ���Ķ����ǩ</param>
    /// <returns>���������Ļ����ļ�·��</returns>
    procedure Lookup(AUrl, ACacheId: String; ACallback: TCacheLookupResult; ATag: Pointer); overload;
    /// <summary>����ָ��URL�ͻ���ID��Ӧ���ļ������ACacheIdΪ�գ�����AUrl��MD5ֵΪACacheId��ֵ</summary>
    /// <param name="AUrl">Ҫ�����URL��ַ</param>
    /// <param name="ACacheId">�������ļ���</param>
    /// <param name="ACallback">���������Ļص�����</param>
    /// <param name="ATag">�û��Զ���Ķ����ǩ</param>
    /// <returns>���������Ļ����ļ�·��</returns>
    procedure Lookup(AUrl, ACacheId: String; ACallback: TCacheLookupResultA; ATag: Pointer); overload;
    /// <summary>����ָ���Ļ�����Ŀ��Ӧ���ļ�������·��</summary>
    function CacheFile(const AItem: TCacheItem): String;
    /// <summary>�ж�ָ������Ŀ�Ƿ��Ѿ�����</summary>
    /// <param name="AUrl">Ҫ�жϵ�URL��ַ�����ִ�Сд</param>
    /// <returns>����Ѿ����棬����true�����򷵻�false</returns>
    function Cached(const AUrl: String): Boolean;
    /// ����Ŀ¼
    property CacheDir: String read FCacheDir write SetCacheDir;
    /// ��ȡHttpClient�����¼�
    property OnGetHttpClient: THttpClientGetEvent read FOnGetHttpClient write FOnGetHttpClient;
    // �ͷ�HttpClient�����¼�
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
          if ACache.ExpireTime < Now then // �����ˣ�ɾ��ԭ�����ļ�
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
        // ȡ��MaxAge��ֵ
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
      if ACache.ExpireTime > AMaxExpireTime then // ����ʱ���������60�죬��60�����
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
        // ��ʱ15����������������������������ڼ��ٲ���Ҫ��IO����
        FSaveJob := Workers.Delay(SaveIndexes, 15 * Q1Second, nil, true);
    end
    else
    begin
      if AReply.StatusCode = 304 then // Not modified
      begin
        ACache.Status := TCacheStatus.Ready;
      end
      else if AReply.StatusCode = 404 then // �Ҳ�����
      begin
        if ACache.Status = TCacheStatus.Exists then
          DeleteFile(ACache.CacheId);
        ACache.Status := TCacheStatus.Missed;
      end
      else // Todo:��������
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
  CalcPerf('��������');
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
