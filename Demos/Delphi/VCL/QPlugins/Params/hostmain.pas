unit hostmain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, Types, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, QString, QPlugins, Vcl.StdCtrls,
  Vcl.ExtCtrls, qplugins_base,QPlugins_params, QPlugins_loader_lib;

type
  // 这个演示如何将对象进行封装，做为参数在服务间传递
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
    function GetCount: Integer;
    function GetItem(Index: Integer): Pointer;
  end;

  IParamTestService = interface
    ['{FB9443D9-EF9A-43F2-9F8D-9B838981AEBE}']
    procedure ObjectTest(AList: IList);
    procedure ArrayTest(AParams: IQParams);
    procedure StreamTest(AParams: IQParams);
    procedure StandTest(AParams: IQParams);
  end;

  // 对上面的接口进行封装，以便在服务间传递
  TListWrap = class(TQInterfacedObject, IList)
  protected
    FList: TList;
  public
    constructor Create; override;
    destructor Destroy; override;
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
    function GetCount: Integer;
    function GetItem(Index: Integer): Pointer;
  end;

  ILogService = interface
    ['{C45581C0-C290-4A9A-BF9E-AC2D814593FE}']
    procedure Log(S: PWideChar);
  end;

  TLogService = class(TQService, ILogService)
  public
    procedure Log(S: PWideChar);
  end;

  TForm1 = class(TForm)
    Panel1: TPanel;
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
{ TListWrap }

function TListWrap.Add(Item: Pointer): Integer;
begin
  Result := FList.Add(Item);
end;

procedure TListWrap.Clear;
begin
  FList.Clear;
end;

constructor TListWrap.Create;
begin
  inherited;
  FList := TList.Create;
end;

procedure TListWrap.Delete(Index: Integer);
begin
  FList.Delete(Index);
end;

destructor TListWrap.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

procedure TListWrap.Exchange(Index1, Index2: Integer);
begin
  FList.Exchange(Index1, Index2);
end;

function TListWrap.ExtractItem(Item: Pointer; Direction: TDirection): Pointer;
begin
  Result := FList.ExtractItem(Item, Direction);
end;

function TListWrap.First: Pointer;
begin
  Result := FList.First;
end;

function TListWrap.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TListWrap.GetItem(Index: Integer): Pointer;
begin
  Result := FList[Index];
end;

function TListWrap.IndexOf(Item: Pointer): Integer;
begin
  Result := FList.IndexOf(Item);
end;

function TListWrap.IndexOfItem(Item: Pointer; Direction: TDirection): Integer;
begin
  Result := FList.IndexOfItem(Item, Direction);
end;

procedure TListWrap.Insert(Index: Integer; Item: Pointer);
begin
  FList.Insert(Index, Item);
end;

function TListWrap.Last: Pointer;
begin
  Result := FList.Last;
end;

procedure TListWrap.Move(CurIndex, NewIndex: Integer);
begin
  FList.Move(CurIndex, NewIndex);
end;

procedure TListWrap.Pack;
begin
  FList.Pack;
end;

function TListWrap.Remove(Item: Pointer): Integer;
begin
  Result := FList.Remove(Item);
end;

function TListWrap.RemoveItem(Item: Pointer; Direction: TDirection): Integer;
begin
  Result := FList.RemoveItem(Item, Direction);
end;

procedure TListWrap.Sort(Compare: TListSortCompare);
begin
  FList.Sort(Compare);
end;

procedure TListWrap.SortList(const Compare: TListSortCompareFunc);
begin
  FList.SortList(Compare);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  AList: IList;
  AService: IParamTestService;
  I: Integer;
begin
  Memo1.Lines.Add('【通过接口传递对象演示】');
  AList := TListWrap.Create;
  AService := PluginsManager.ByPath('Services/Params') as IParamTestService;
  if Assigned(AService) then
  begin
    AService.ObjectTest(AList);
    for I := 0 to AList.GetCount - 1 do
      Memo1.Lines.Add(IntToStr(I) + ' - ' + IntToHex(IntPtr(AList.GetItem(I)),
        SizeOf(Pointer) shl 1));
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  AParams: IQParams;
  AService: IParamTestService;
  AStream: TQStream;
  S: QStringW;
begin
  Memo1.Lines.Add('【流做为参数传递演示】');
  AParams := TQParams.Create;
  AStream := QStream(AParams.Add('Stream', ptStream).GetAsStream);
  S := 'Hello,world';
  AStream.WriteBuffer(PWideChar(S)^, Length(S) shl 1);
  AService := PluginsManager.ByPath('Services/Params') as IParamTestService;
  if Assigned(AService) then
  begin
    AService.StreamTest(AParams);
    AStream.Position := 0;
    S := LoadTextW(AStream, teUnicode16LE);
    Memo1.Lines.Add('新的流内容：');
    Memo1.Lines.Add(S);
  end;
  FreeAndNil(AStream);
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  AParams, ASubParams: IQParams;
  AService: IParamTestService;
begin
  Memo1.Lines.Add('【二维参数演示】');
  AParams := TQParams.Create;
  ASubParams := AParams.Add('Subs', ptArray).AsArray;
  ASubParams.Add('Name', ptUnicodeString).AsString := NewString('QDAC');
  ASubParams.Add('Version', ptUInt8).AsInteger := 3;
  Memo1.Lines.Add('原始参数');
  Memo1.Lines.Add(AParams.AsString.Value);
  AService := PluginsManager.ByPath('Services/Params') as IParamTestService;
  if Assigned(AService) then
  begin
    AService.ArrayTest(AParams);
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  AParams: IQParams;
  AService: IParamTestService;
begin
  Memo1.Lines.Add('【常规参数传递演示】');
  AParams := TQParams.Create;
  AParams.Add('Int', ptInt32).AsInteger := 100;
  AParams.Add('DT', ptDateTime).AsFloat := Now;
  AService := PluginsManager.ByPath('Services/Params') as IParamTestService;
  if Assigned(AService) then
  begin
    AService.StandTest(AParams);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutDown := True;
  RegisterServices('/Services', [TLogService.Create(ILogService, 'Log')]);
  PluginsManager.Loaders.Add
    (TQDLLLoader.Create(ExtractFilePath(Application.ExeName), '.DLL'));
  PluginsManager.Start;
end;

{ TLogService }

procedure TLogService.Log(S: PWideChar);
begin
  Form1.Memo1.Lines.Add(S);
end;

end.
