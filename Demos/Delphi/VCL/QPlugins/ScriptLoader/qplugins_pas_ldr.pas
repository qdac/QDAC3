unit qplugins_pas_ldr;

interface

uses classes, sysutils, JvComponentBase, JvInterpreter, JvInterpreterFm,
  qstring, qpluginsintf, qplugins,qplugins_base;

type
  TQPascalScriptLoader = class(TQPluginService, IQPluginsLoader)
  protected
    FScriptEngine: TJvInterpreterFm;
    function Execute(const AParams:IQParams;AResult:IQParams): Boolean; override; stdcall;
    function GetVersion(var AVersion: TQPluginVersion): Boolean;
    class procedure DoRegisterService(var Value: Variant;
      Args: TJvInterpreterArgs); static;
  public
    constructor Create; overload;
    destructor Destroy; override;
  end;

  TQPascalScriptService = class(TQPluginService)
  protected
    FSource, FEntry: QStringW;
    FOwner: TQPascalScriptLoader;
    function Execute(const AParams:IQParams;AResult:IQParams): Boolean; override; stdcall;
  public
    constructor Create(AOwner: TQPascalScriptLoader; const AId: TGuid;
      ASource, AName, AEntry: QStringW);
  end;

implementation

uses forms;

{ TQPascalScriptLoader }
var
  ScriptLoader: TQPascalScriptLoader;

constructor TQPascalScriptLoader.Create;
begin
  inherited Create;
  FId := StringToGuid('{256CD4D2-B722-4679-8477-B72006CFB5DC}');
  FName := 'Pascal 脚本加载器';
  FScriptEngine := TJvInterpreterFm.Create(nil);
  GlobalJvInterpreterAdapter.AddFunction('', 'RegisterService',
    DoRegisterService, 4, [varUString, varUString, varUString,
    varUString], varNull);
  FAttrs := TQParams.Create;
end;

destructor TQPascalScriptLoader.Destroy;
begin
  FreeAndNil(FScriptEngine);
  inherited;
end;

class procedure TQPascalScriptLoader.DoRegisterService(var Value: Variant;
  Args: TJvInterpreterArgs);
var
  APath, AName, AEntry: QStringW;
  AId: TGuid;
  AService: IQPluginService;
  AServices: IQPluginServices;
begin
  AId := StringToGuid(Args.Values[0]);
  APath := Args.Values[1];
  AName := Args.Values[2];
  AEntry := Args.Values[3];
  AService := PluginsManager.ByPath(PWideChar(APath));
  if AService.QueryInterface(IQPluginServices, AServices) = S_OK then
    AServices := AService as IQPluginServices
  else
    AServices := nil;
  if (AServices <> nil) then
  begin
    if ScriptLoader.FScriptEngine.FunctionExists('', AEntry) then
      AServices.Add(TQPascalScriptService.Create(ScriptLoader, AId,
        ScriptLoader.FScriptEngine.Source, AName, AEntry))
    else
      raise Exception.CreateFmt('请求的函数 %s 不存在。', [AEntry]);
  end
  else
    raise Exception.CreateFmt('请求的服务路径 %s 不存在。', [APath]);
end;

function TQPascalScriptLoader.Execute(const AParams:IQParams;AResult:IQParams): Boolean;
var
  ARec: TSearchRec;
  APath: String;
  Args: TJvInterpreterArgs;
begin
  Result := True;
  if AParams[0].AsInteger = LA_STARTING then
  begin
    { 扫描*.pas/*.dfm 文件，然后尝试编译并查找相关的注册函数，然后调用其并注册
      注册函数格式（暂定）：
      procedure QPluginEntry;
      begin
      RegisterService(AId,APath,AName,AFunction);
      end;
    }
    Args := TJvInterpreterArgs.Create;
    Args.Count := 0;
    APath := ExtractFilePath(Application.ExeName) + 'Plugins\Pascal\';
    if FindFirst(APath + '*.pas', faNormal, ARec) = 0 then
    begin
      repeat
        try
          if FileExists(APath + Copy(ARec.Name, Length(ARec.Name) - 3) + 'dfm')
          then // 如果存在窗体文件
            FScriptEngine.MakeForm(APath + ARec.Name)
          else
          begin
            FScriptEngine.Source := LoadTextW(APath + ARec.Name);
            FScriptEngine.Compile;
          end;
          if FScriptEngine.FunctionExists('', 'PluginEntry') then
            FScriptEngine.CallFunction('PluginEntry', nil, [PluginsManager]);
        except
          on E: Exception do
          begin
            FLastError := -1;
            FLastErrorMsg := E.Message;
          end;
        end;
      until FindNext(ARec) <> 0;
    end;
  end;
end;

function TQPascalScriptLoader.GetVersion(var AVersion: TQPluginVersion)
  : Boolean;
begin
  AVersion.Version.Major := 1;
  AVersion.Version.Minor := 0;
  AVersion.Version.Release := 1;
  AVersion.Version.Build := 0;
  StrPCopy(PWideChar(@AVersion.Company[0]), 'QDAC team');
  StrPCopy(PWideChar(@AVersion.Name[0]), 'PascalScript Plugins Loader');
  StrPCopy(PWideChar(@AVersion.Description[0]),
    '(C)2015,copyright by QDAC Team'#13#10'Base on JVCL Pascal Script Engine');
  StrPCopy(PWideChar(@AVersion.FileName[0]), 'qplugins_pas_ldr.pas');
  Result := True;
end;

{ TQPascalScriptService }

constructor TQPascalScriptService.Create(AOwner: TQPascalScriptLoader;
  const AId: TGuid; ASource, AName, AEntry: QStringW);
begin
  inherited Create;
  FId := AId;
  FOwner := AOwner;
  FSource := ASource;
  FName := AName;
  FEntry := AEntry;
  FAttrs := TQParams.Create;
  FLoader := ScriptLoader;
end;

function TQPascalScriptService.Execute(const AParams:IQParams;AResult:IQParams): Boolean;
var
  Args: TJvInterpreterArgs;
  I: Integer;
  AFunc: TJvInterpreterFunctionDesc;
  function ParamAsString(AParam: IQParam): QStringW;
  var
    L: Integer;
  begin
    L := AParam.GetAsString(nil, 0);
    SetLength(Result, L);
    AParam.GetAsString(PWideChar(Result), L);
  end;

begin
  Args := TJvInterpreterArgs.Create;
  Args.Count := AParams.Count;
  FOwner.FScriptEngine.Source := FSource;
  FOwner.FScriptEngine.Compile;
  for I := FOwner.FScriptEngine.Adapter.SrcFunctionList.Count - 1 downto 0 do
  begin
    AFunc := FOwner.FScriptEngine.Adapter.SrcFunctionList[I];
    if AFunc.Identifier = FEntry then
      Break;
  end;
  if I < 0 then
  begin
    FLastError := -1;
    FLastErrorMsg := FSource + ' 的服务处理函数 ' + FEntry + ' 已丢失，无法提供服务。';
    Result := False;
    Exit;
  end;
  // if AFunc.ParamCount <> AParams.Count then
  // begin
  // FLastError := -1;
  // FLastErrorMsg := FSource + ' 的服务处理函数 ' + FEntry + ' 的参数个数不匹配。';
  // Result := False;
  // Exit;
  // end;
  Args.Count := AParams.Count;
  Args.Values[0] := AParams[0].AsInteger;
  Args.Values[1] := AParams[1].AsInteger;
  // for I := 0 to AParams.Count - 1 do
  // begin
  // case AFunc.ParamTypes[I] of
  // varSmallint:
  // Args.Values[I] := Smallint(AParams[I].AsInteger);
  // varInteger:
  // Args.Values[I] := AParams[I].AsInteger;
  // varSingle:
  // Args.Values[I] := AParams[I].AsSingle;
  // varDouble:
  // Args.Values[I] := AParams[I].AsFloat;
  // varCurrency:
  // Args.Values[I] := Currency(AParams[I].AsFloat);
  // varDate:
  // Args.Values[I] := AParams[I].AsFloat;
  // varOleStr, varString, varUString:
  // Args.Values[I] := ParamAsString(AParams[I]);
  // varBoolean:
  // Args.Values[I] := AParams[I].AsBoolean;
  // varShortInt:
  // Args.Values[I] := Shortint(AParams[I].AsInteger);
  // varByte:
  // Args.Values[I] := Byte(AParams[I].AsInteger);
  // varWord:
  // Args.Values[I] := Word(AParams[I].AsInteger);
  // varLongWord:
  // Args.Values[I] := Cardinal(AParams[I].AsInt64);
  // varInt64, varUInt64:
  // Args.Values[I] := AParams[I].AsInt64;
  // end;
  // end;
  try
    FOwner.FScriptEngine.CallFunction(FEntry, Args, []);
    AParams.Add('Result', ptInt32).AsInteger := FOwner.FScriptEngine.VResult;
    {
      case AFunc.ResTyp of
      varSmallint:
      AParams.Add('Result', ptInt16).AsInteger :=
      FOwner.FScriptEngine.VResult;
      varInteger:
      AParams.Add('Result', ptInt32).AsInteger :=
      FOwner.FScriptEngine.VResult;
      varSingle:
      AParams.Add('Result', ptFloat4).AsSingle :=
      FOwner.FScriptEngine.VResult;
      varDouble:
      AParams.Add('Result', ptFloat8).AsFloat := FOwner.FScriptEngine.VResult;
      varCurrency:
      AParams.Add('Result', ptFloat8).AsFloat := FOwner.FScriptEngine.VResult;
      varDate:
      AParams.Add('Result', ptDateTime).AsFloat :=
      FOwner.FScriptEngine.VResult;
      varOleStr, varString, varUString:
      AParams.Add('Result', ptUnicodeString)
      .SetAsString(PWideChar(QStringW(FOwner.FScriptEngine.VResult)));
      varBoolean:
      AParams.Add('Result', ptBoolean).AsBoolean :=
      FOwner.FScriptEngine.VResult;
      varShortInt:
      AParams.Add('Result', ptInt8).AsInteger := FOwner.FScriptEngine.VResult;
      varByte:
      AParams.Add('Result', ptUInt8).AsInteger :=
      FOwner.FScriptEngine.VResult;
      varWord:
      AParams.Add('Result', ptUInt16).AsInteger :=
      FOwner.FScriptEngine.VResult;
      varLongWord, varInt64, varUInt64:
      AParams.Add('Result', ptUInt32).AsInt64 := FOwner.FScriptEngine.VResult;
      end; }
  except
    on E: Exception do
    begin
      FLastError := -1;
      FLastErrorMsg := E.Message;
    end;
  end;
  FreeAndNil(Args);
  Result := True;
end;

initialization

ScriptLoader := TQPascalScriptLoader.Create;
PluginsManager.GetLoaders.Add(ScriptLoader as IQPluginService);

end.
