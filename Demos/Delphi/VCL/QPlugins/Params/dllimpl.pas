unit dllimpl;

interface

uses classes, sysutils, types, qplugins, qstring,qplugins_base,qplugins_params;

type
  IList = interface
    ['{6D1A9CAB-9284-42DC-95C0-342DC72FAC03}']
    function Add(Item: Pointer): Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    function ExtractItem(Item: Pointer; Direction: TDirection): Pointer;
    function First: Pointer;
    function IndexOf(Item: Pointer): Integer;
    function IndexOfItem(Item: Pointer; Direction: TDirection): Integer;
    procedure Insert(Index: Integer; Item: Pointer);
    function Last: Pointer;
    procedure Move(CurIndex, NewIndex: Integer);
    function Remove(Item: Pointer): Integer;
    function RemoveItem(Item: Pointer; Direction: TDirection): Integer;
    procedure Pack;
    procedure Sort(Compare: TListSortCompare);
    procedure SortList(const Compare: TListSortCompareFunc);
  end;

  IParamTestService = interface
    ['{FB9443D9-EF9A-43F2-9F8D-9B838981AEBE}']
    procedure ObjectTest(AList: IList);
    procedure ArrayTest(AParams: IQParams);
    procedure StreamTest(AParams: IQParams);
    procedure StandTest(AParams: IQParams);
  end;

  TParamTestService = class(TQService, IParamTestService)
  public
    procedure ObjectTest(AList: IList);
    procedure ArrayTest(AParams: IQParams);
    procedure StreamTest(AParams: IQParams);
    procedure StandTest(AParams: IQParams);
  end;

  ILogService = interface
    ['{C45581C0-C290-4A9A-BF9E-AC2D814593FE}']
    procedure Log(S: PWideChar);
  end;

implementation

{ TParamTestService }

procedure TParamTestService.ArrayTest(AParams: IQParams);
var
  ALog: ILogService;
  procedure LogParams(AParent: IQParams);
  var
    I: Integer;
    AParam: IQParam;
    S: QStringW;
  begin
    S := '参数共 ' + IntToStr(AParent.Count) + ' 个';
    ALog.Log(PWideChar(S));
    for I := 0 to AParent.Count - 1 do
    begin
      AParam := AParent[I];
      S := AParam.Name;
      if AParam.ParamType = ptArray then
      begin
        S := S + ' 是一个数组，遍历其元素:';
        ALog.Log(PWideChar(S));
        LogParams(AParam.AsArray);
        S := '子参数枚举结束';
        ALog.Log(PWideChar(S));
      end
      else
        ALog.Log(PWideChar(S + ' 的值为:' + AParam.AsString.Value));
    end;
  end;

begin
  ALog := PluginsManager as ILogService;
  LogParams(AParams);

end;

procedure TParamTestService.ObjectTest(AList: IList);
var
  I: Integer;
begin
  for I := 0 to 9 do
    AList.Add(Pointer(I));
end;

procedure TParamTestService.StandTest(AParams: IQParams);
var
  ALog: ILogService;
  procedure LogParams(AParent: IQParams);
  var
    I: Integer;
    AParam: IQParam;
    S: QStringW;
  begin
    S := '参数共 ' + IntToStr(AParent.Count) + ' 个';
    ALog.Log(PWideChar(S));
    for I := 0 to AParent.Count - 1 do
    begin
      AParam := AParent[I];
      S := AParam.Name;
      if AParam.ParamType = ptArray then
      begin
        S := S + ' 是一个数组，遍历其元素:';
        ALog.Log(PWideChar(S));
        LogParams(AParam.AsArray);
        S := '子参数枚举结束';
        ALog.Log(PWideChar(S));
      end
      else
        ALog.Log(PWideChar(S + ' 的值为:' + AParam.AsString.Value));
    end;
  end;

begin
  ALog := PluginsManager as ILogService;
  LogParams(AParams);
end;

procedure TParamTestService.StreamTest(AParams: IQParams);
var
  AStream: TQStream;
  S: QStringW;
begin
  AStream := TQStream.Create(AParams[0].AsStream);
  try
    S := '这是从插件中返回的内容。';
    AStream.Size := 0;
    AStream.WriteBuffer(PWideChar(S)^, Length(S) shl 1);
  finally
    FreeObject(AStream);
  end;
end;

initialization

RegisterServices('Services', [TParamTestService.Create(IParamTestService,
  'Params')]);

finalization

UnregisterServices('Services', ['Params']);

end.
