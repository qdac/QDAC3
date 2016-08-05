unit qplugins_loader_lib;

{ DLL/SO/BPL加载器实现单元，在使用时加载特定的加载器实现加载特定的插件
  TQDLLLoader - Windows 的 DLL 加载器
  TQBPLLoader - BPL 加载器
  TQSOLoader - Linux 系的 SO 加载器
}
interface

uses classes, sysutils{$IFDEF MSWINDOWS}, windows, MemoryModule{$ENDIF},
  qstring, qplugins;
{$HPPEMIT '#pragma link "qplugins_loader_lib"'}
type
  // 基类
  TQLibLoader = class(TQBaseLoader)
  protected
    function GetServiceSource(AService: IQService; ABuf: PWideChar;
      ALen: Integer): Integer; override; stdcall;
  public
  end;
{$IFDEF MSWINDOWS}

  TQDLLLoader = class(TQLibLoader)
  protected
    function InternalLoadServices(const AFileName: PWideChar): THandle;
      overload; override;
    function InternalLoadServices(const AStream: TStream): THandle;
      overload; override;
    function InternalUnloadServices(const AHandle: THandle): Boolean; override;
  public
    constructor Create(APath, AExt: QStringW); overload;
  end;

  TQBPLLoader = class(TQLibLoader)
  protected
    function InternalLoadServices(const AFileName: PWideChar): THandle;
      overload; override;
    function InternalLoadServices(const AStream: TStream): THandle;
      overload; override;
    function InternalUnloadServices(const AHandle: THandle): Boolean; override;
  public
    constructor Create(APath, AExt: QStringW); overload;
  end;

{$ELSE}

  TQSOLoader = class(TQLibLoader)
  protected
    function InternalLoadServices(const AFileName: PWideChar): THandle;
      override;
    function InternalUnloadServices(const AHandle: THandle): Boolean; override;
  public
    constructor Create(APath, AExt: QStringW); overload;
  end;
{$ENDIF}

function GetCodeFileName(Addr: Pointer): QStringW;

const
  LID_DLL: TGuid = '{558F1A1B-52E6-4E88-8FD2-9F02027E8C39}';
  LID_SO: TGuid = '{65689986-CEC7-4FFF-99F5-BA86D4E48F2F}';
  LID_PACKAGE: TGuid = '{5EB06801-995A-4AA9-95E8-E450D38DD5CA}';

implementation

resourcestring
  SServiceAlreayLoaded = '服务可能已经被静态加载，不需要再动态导入。';
  { TQLibLoader }

function TQLibLoader.GetServiceSource(AService: IQService; ABuf: PWideChar;
  ALen: Integer): Integer;
begin
Result:={$IFDEF UNICODE}GetModuleFileName{$ELSE}GetModuleFileNameW{$ENDIF}(AService.GetOwnerInstance, ABuf, ALen);
end;

function GetCodeFileName(Addr: Pointer): QStringW;
begin
  SetLength(Result, MAX_PATH);
  SetLength(Result,
{$IFDEF UNICODE}GetModuleFileName{$ELSE}GetModuleFileNameW{$ENDIF}(FindHInstance(Addr), PWideChar(Result), MAX_PATH));
end;

// 获取指定地址所隶属的实例，暂时只实现了 Windows，Linux下的回头再研究
{$IFDEF MSWINDOWS}
{ TQDLLLoader }

constructor TQDLLLoader.Create(APath, AExt: QStringW);
begin
  if Length(AExt) = 0 then
    AExt := '.DLL';
  inherited Create(LID_DLL, 'Loader_DLL', APath, AExt);
end;

function TQDLLLoader.InternalLoadServices(const AStream: TStream): THandle;
var
  AMemStream: TMemoryStream;
begin
  if AStream is TMemoryStream then
    AMemStream := AStream as TMemoryStream
  else
  begin
    AMemStream := TMemoryStream.Create;
    AMemStream.Size := AStream.Size;
    AMemStream.CopyFrom(AStream, 0);
  end;
  try
    Result := MemLoadLibrary(AMemStream.Memory);
    if Result <> 0 then
      Result := Result + 1;
  finally
    if AMemStream <> AStream then
      FreeAndNil(AMemStream);
  end;
end;

function TQDLLLoader.InternalUnloadServices(const AHandle: THandle): Boolean;
begin
  if (AHandle and $1) <> 0 then
    MemFreeLibrary(AHandle and (not THandle(1)))
  else
    FreeLibrary(AHandle);
  Result := True;
end;

function TQDLLLoader.InternalLoadServices(const AFileName: PWideChar): THandle;
begin
  try
    Result := GetModuleHandleW(AFileName);
    if Result <> 0 then
    begin
      Result := 0;
      SetLastError(1, SServiceAlreayLoaded);
    end
    else
      Result := {$IFDEF UNICODE}LoadLibrary{$ELSE}LoadLibraryW{$ENDIF}(PWideChar(AFileName));
  except
    on E: Exception do
    begin
      Result := 0;
      SetLastError(1, E.Message);
    end;
  end;
end;

{ TQBPLLoader }

constructor TQBPLLoader.Create(APath, AExt: QStringW);
begin
  if Length(AExt) = 0 then
    AExt := '.BPL';
  inherited Create(LID_PACKAGE, 'Loader_BPL', APath, AExt);
end;

function TQBPLLoader.InternalLoadServices(const AFileName: PWideChar): THandle;
begin
  try
    Result := LoadPackage(AFileName);
  except
    on E: Exception do
    begin
      Result := 0;
      SetLastError(1, E.Message);
    end;
  end;
end;

function TQBPLLoader.InternalLoadServices(const AStream: TStream): THandle;
var
  AMemStream: TMemoryStream;
begin
  if AStream is TMemoryStream then
    AMemStream := AStream as TMemoryStream
  else
  begin
    AMemStream := TMemoryStream.Create;
    AMemStream.Size := AStream.Size;
    AMemStream.CopyFrom(AStream, 0);
  end;
  try
    Result := MemLoadLibrary(AMemStream.Memory);
    if Result <> 0 then
      InitializePackage(Result);
  finally
    if AMemStream <> AStream then
      FreeAndNil(AMemStream);
  end;
end;

function TQBPLLoader.InternalUnloadServices(const AHandle: THandle): Boolean;
begin
  UnloadPackage(AHandle);
  Result := True;
end;
{$ELSE}
{ TQSOLoader }

constructor TQSOLoader.Create(APath, AExt: QStringW);
begin
  if Length(AExt) = 0 then
    AExt := '.SO';
  inherited Create(LID_SO, 'Loader_SO', APath, AExt);
end;

function TQSOLoader.InternalLoadServices(const AFileName: PWideChar): THandle;
begin
  try
    Result := LoadLibrary(PWideChar(AFileName));
  except
    on E: Exception do
    begin
      Result := 0;
      SetLastError(1, E.Message);
    end;
  end;
end;

function TQSOLoader.InternalUnloadServices(const AHandle: THandle): Boolean;
begin
  FreeLibrary(AHandle);
  Result := True;
end;
{$ENDIF}

end.
