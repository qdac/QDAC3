unit rttisvc;

interface

uses classes, sysutils, qpluginsintf, qplugins,qplugins_base, typinfo, system.rtti, qstring,
  qtimetypes;

type
  // 这个只支持Delphi，所以可以直接用Delphi的类型
  TQRttiService = class(TQPluginService)
  protected
    FObject: TObject;
    FRttiContext: TRttiContext;
    FType: TRttiType;
    function Execute(const AParams: IQParams; AResult: IQParams)
      : Boolean; stdcall;
    function Invoke(const AFunction: String; const AParams: array of TValue;
      var AResult: TValue): Boolean;

  public
    constructor Create(AInstance: TObject; const AId: TGuid;
      const AName: QStringW);
  end;

procedure ParamToValue(const AParam: IQParam; var AValue: TValue);
procedure ValueToParam(const AValue: TValue; AParam: IQParam);

implementation

procedure ParamToValue(const AParam: IQParam; var AValue: TValue);
var
  I: Integer;
  procedure AsBytes(ATypeInfo: PTypeInfo);
  var
    L: Integer;
    ABytes: TBytes;
  begin
    L := AParam.GetAsBytes(nil, 0);
    SetLength(ABytes, L);
    TValue.Make(@ABytes[0], ATypeInfo, AValue);
  end;

  function AsString: UnicodeString;
  begin
    SetLength(Result, AParam.GetAsString(nil, 0));
    AParam.GetAsString(PWideChar(Result), Length(Result));
  end;

begin
  case AParam.ParamType of
    ptUnknown:
      AValue := TValue.Empty;
    ptInt8:
      AValue := TValue.From<Shortint>(AParam.GetAsInteger);
    ptUInt8:
      AValue := TValue.From<Byte>(AParam.GetAsInteger);
    ptInt16:
      AValue := TValue.From<Smallint>(AParam.GetAsInteger);
    ptUInt16:
      AValue := TValue.From<Word>(AParam.GetAsInteger);
    ptInt32:
      AValue := TValue.From<Integer>(AParam.GetAsInteger);
    ptUInt32:
      AValue := TValue.From<UInt32>(AParam.GetAsInt64);
    ptInt64:
      AValue := TValue.From<Int64>(AParam.GetAsInt64);
    ptUInt64:
      AValue := TValue.From<UInt64>(UInt64(AParam.GetAsInt64));
    ptFloat4:
      AValue := TValue.From<Single>(AParam.GetAsSingle);
    ptFloat8:
      AValue := TValue.From<Double>(AParam.GetAsFloat);
    ptDateTime:
      AValue := TValue.From<TDateTime>(AParam.GetAsFloat);
    ptInterval:
      AsBytes(TypeInfo(TQInterval));
    ptAnsiString,ptUnicodeString,ptUtf8String:
      AValue := TValue.From<UnicodeString>(AsString);
    ptBoolean:
      AValue := TValue.From<Boolean>(AParam.AsBoolean);
    ptGuid:
      ;
    ptBytes:
      ;
    ptPointer:
      ;
    ptInterface:
      ;
    ptStream:
      ;
    ptArray:
      ;
  end;
end;

procedure ValueToParam(const AValue: TValue; AParam: IQParam);

{ TQRttiService }

  constructor TQRttiService.Create(AInstance: TObject; const AId: TGuid;
    const AName: QStringW);
  begin
    inherited Create(AId, AName);
    FObject := AInstance;
    FRttiContext := TRttiContext.Create;
    FType := FRttiContext.GetType(FObject.ClassType);
  end;

  function TQRttiService.Execute(const AParams: IQParams;
    AResult: IQParams): Boolean;
  var
    AName: String;
    AValues: array of TValue;
    ARetVal: TValue;
    I: Integer;
  begin
    if AParams.Count > 0 then
    begin
      SetLength(AName, AParams[0].GetAsString(nil, 0));
      AParams[0].GetAsString(PWideChar(AName), Length(AName));
      SetLength(AValues, AParams.Count - 1);
      for I := 1 to AParams.Count - 1 do
        ParamToValue(AParams[I], AValues[I - 1]);
      Result := Invoke(AName, AValues, ARetVal);
      if Result then
        ValueToParam(ARetVal, AResult.Add('Result'));
    end;
  end;

  function TQRttiService.Invoke(const AFunction: String;
    const AParams: array of TValue; var AResult: TValue): Boolean;
  var
    AFunc: TRttiMethod;
  begin
    AFunc := FType.GetMethod(AFunction);
    if Assigned(AFunc) then
    begin
      try
        AResult := AFunc.Invoke(FObject, AParams)
      except
        on E: Exception do
          SetLastError(-1, E.Message);
      end;
    end
    else
      Result := False;
  end;

  procedure TQRttiService.ParamToValue(const AParam: IQParam;
    var AValue: TValue);
  begin

  end;

  procedure TQRttiService.ValueToParam(const AValue: TValue; AParam: IQParam);
  begin

  end;

end.
