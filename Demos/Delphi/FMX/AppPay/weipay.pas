unit weipay;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, FMX.Platform,
  System.Variants, FMX.Controls, FMX.Objects, FMX.StdCtrls, FMX.Layouts,
  FMX.Types, FMX.Forms, FMX.Controls.Presentation, System.Generics.Collections,
  qsdk.wechat, qsdk.alipay, qworker;

type
  // 购买项目
  TCartItem = record
    Id: String; // 商品ID
    Name: String; // 商品名称
    Num: Integer; // 商品数量
    Price: Currency; // 商品单价
    Remark: String; // 备注
    Selected: Boolean; // 是否选择
  end;

  PCartItem = ^TCartItem;

  // 购买项目列表管理
  ICartItems = interface
    ['{513C0F8C-8261-4A1D-8FE5-55DA70E4C273}']
    function Add(const AId, AName: String; APrice: Currency; ANum: Integer;
      const ARemark: String = ''): Integer;
    procedure Delete(AIndex: Integer);
    procedure Clear;
    procedure SelectAll(ASelected: Boolean);
    procedure InvertSelect;
    function getSubject: String;
    procedure setSubject(const AValue: String);
    function GetCount: Integer;
    function GetItems(const AIndex: Integer): PCartItem;
    function GetSelected(const AIndex: Integer): Boolean;
    function getOrderId: String;
    function PrepareForPay(var ASummary: String; var ATotal: Currency): Integer;
    procedure setOrderId(const AId: String);
    procedure SetSelected(const AIndex: Integer; ASelected: Boolean);
    property Count: Integer read GetCount;
    property Items[const AIndex: Integer]: PCartItem read GetItems; default;
    property Selected[const AIndex: Integer]: Boolean read GetSelected
      write SetSelected;
    property Subject: String read getSubject write setSubject;
    property OrderId: String read getOrderId write setOrderId;
  end;

  // 付款来源：未知、微信、支付宝、其它
  TPaySource = (psUnknown, psWechat, psAlipay, psOther);
  // 支付结果：未知、成功、用户取消、错误
  TPayResult = (pcrUnknown, prOk, prCancel, prError);
  TPaySources = set of TPaySource;

  // 生成的支付订单信息
  IPayOrder = interface
    ['{DA61ADE4-5DF1-4571-BA69-C7572DBC0DF7}']
    function getOrderId: String;
    procedure setOrderId(const AValue: String);
    function getSubject: String;
    procedure setSubject(const AValue: String);
    function getFee: Currency;
    procedure setFee(const AValue: Currency);
    function getTimestamp: Integer;
    procedure setTimestamp(const AValue: Integer);
    function getSign: String;
    procedure setSign(const ASign: String);
    function getSource: TPaySource;
    procedure setSource(const ASource: TPaySource);
    function getPayResult: TPayResult;
    procedure setPayResult(const AResult: TPayResult);
    function getErrorCode: Integer;
    procedure setErrorCode(const AValue: Integer);
    function getErrorMsg: String;
    procedure setErrorMsg(const AValue: String);
    function getCartItems: ICartItems;
    property OrderId: String read getOrderId write setOrderId; // 商家订单号
    property Subject: String read getSubject write setSubject;
    // 订单主题（对应微信的body和阿里的subject）
    property Fee: Currency read getFee write setFee; // 订单金额
    property Timestamp: Integer read getTimestamp write setTimestamp;
    // 订单时间戳（Unix）
    property Sign: String read getSign write setSign; // 服务器返回的订单签名
    property Source: TPaySource read getSource write setSource; // 支付来源
    property PayResult: TPayResult read getPayResult write setPayResult; // 支付结果
    property ErrorCode: Integer read getErrorCode write setErrorCode; // 错误代码
    property ErrorMsg: String read getErrorMsg write setErrorMsg; // 错误消息
    property CartItems: ICartItems read getCartItems; // 关联的购物车项目
  end;

  // 微信支付订单信息
  IWechatPayOrder = interface(IPayOrder)
    ['{F7A6C431-328F-454C-AB77-FBE723FE8834}']
    function getPrepayId: String;
    procedure setPrepayId(const AValue: String);
    function getNonceStr: String;
    procedure setNonceStr(const AValue: String);
    property PrepayId: String read getPrepayId write setPrepayId;
    // 统一下单生成的预支付ID
    property NonceStr: String read getNonceStr write setNonceStr;
    // 服务器端返回的随机字符串
  end;

  IAlipayOrder = interface(IPayOrder)
    ['{26BF3C4C-D90C-47F6-B0CD-B5292439AE70}']
    function getTimeout: Cardinal;
    function getPayStr: String;
    procedure setPayStr(const AValue: String);
    procedure setTimeout(AValue: Cardinal);
    property Timeout: Cardinal read getTimeout write setTimeout; // 订单超时时间
    property PayStr: String read getPayStr write setPayStr;
    // 服务器返回的用于调起支付宝支付的订单字符串
  end;

  // IPaymentService 用于处理订单的生成和支付完成，由外部模块提供，注册到 TPlatformServices

  IPaymentService = interface
    ['{FDCFD64C-0BE7-4207-8051-DBE27FEDAE59}']
    function Prepay(ACartItems: ICartItems; APayOrder: IPayOrder): Boolean;
    // 处理订单预支付
    procedure AfterPay(ACartItems: ICartItems; APayOrder: IPayOrder); // 支付完成的处理
  end;

  TfrmWeiPay = class(TForm)
    ToolBar1: TToolBar;
    sbBack: TSpeedButton;
    lblTitle: TLabel;
    loOrderInfo: TLayout;
    lblOrderSummary: TLabel;
    lblOrderFee: TLabel;
    loPay: TLayout;
    loWechat: TLayout;
    rbByWechat: TRadioButton;
    rectPayHeader: TRectangle;
    Label4: TLabel;
    Layout4: TLayout;
    Image1: TImage;
    Layout5: TLayout;
    Label5: TLabel;
    lblWechatHint: TLabel;
    sbPayNow: TSpeedButton;
    loAlipay: TLayout;
    Layout3: TLayout;
    Image3: TImage;
    Layout6: TLayout;
    Label7: TLabel;
    lblAlipayHint: TLabel;
    rbByAlipay: TRadioButton;
    PayStyle: TStyleBook;
    procedure sbPayNowClick(Sender: TObject);
    procedure loPayClick(Sender: TObject);
    procedure rbByWechatChange(Sender: TObject);
    procedure sbBackClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FOrder: IPayOrder;
    FCartItems: ICartItems;
    FService: IPaymentService;
    FFee: Currency;
    { Private declarations }
    procedure DoPrepay(AJob: PQJob);
    procedure DoAfterPay;
    procedure StartPay(AJob: PQJob);
    procedure DoWechatResponse(AResp: IWechatResponse);
    procedure DoPay;
    procedure setFee(const Value: Currency);
  public
    { Public declarations }
    property Order: IPayOrder read FOrder write FOrder;
    property CartItems: ICartItems read FCartItems write FCartItems;
    property Fee: Currency read FFee write setFee;
  end;

var
  frmWeiPay: TfrmWeiPay;

procedure AppPay(const AOrderId, ASubject, ASummary: String; AFee: Currency;
  ASources: TPaySources = []); overload;
procedure AppPay(ACartItems: ICartItems; ASources: TPaySources = []); overload;
function NewCartItems: ICartItems;
function PaymentService: IPaymentService;

implementation

uses qstring, QJson, qdac_fmx_modaldlg;
{$R *.fmx}

type
  // 常规订单信息
  TPayOrder = class(TInterfacedObject, IPayOrder)
  private
    FAppId: String; // 微信或支付宝的应用编码
    FOrderId: String; // 商户自己的订单编号
    FSubject: String; // 订单标题
    FSummary: String; // 下单名称
    FFee: Currency; // 金额
    FTimestamp: Integer; // 时间戳(Unix格式）
    FSign: String; // 签名
    FPayResult: TPayResult; // 支付结果
    FPaySource: TPaySource; // 支付来源
    FErrorCode: Integer; // 错误代码
    FErrorMsg: String; // 错误消息
    FCartItems: ICartItems; // 购物车项目
    function getOrderId: String;
    procedure setOrderId(const AValue: String);
    function getSubject: String;
    procedure setSubject(const AValue: String);
    function getFee: Currency;
    procedure setFee(const AValue: Currency);
    function getTimestamp: Integer;
    procedure setTimestamp(const AValue: Integer);
    function getSign: String;
    procedure setSign(const ASign: String);
    function getSource: TPaySource;
    procedure setSource(const ASource: TPaySource);
    function getPayResult: TPayResult;
    procedure setPayResult(const AResult: TPayResult);
    function getErrorCode: Integer;
    procedure setErrorCode(const AValue: Integer);
    function getErrorMsg: String;
    procedure setErrorMsg(const AValue: String);
    function getCartItems: ICartItems;
  public
    constructor Create(ACartItems: ICartItems); overload;

  end;
  // 微信订单信息

  TWechatOrder = class(TPayOrder, IWechatPayOrder)
  private
    FPrepayId: String;
    FNonceStr: String;
    FMchId: String;
    FPayKey: String;
    function getPrepayId: String;
    procedure setPrepayId(const AValue: String);
    function getNonceStr: String;
    procedure setNonceStr(const AValue: String);
    function getMchId: String;
    procedure setMchId(const AValue: String);
    function getPayKey: String;
    procedure setPayKey(const AValue: String);
  end;

  TAlipayOrder = class(TPayOrder, IAlipayOrder)
  private
    FTimeout: Cardinal;
    FPayStr: String;
    function getTimeout: Cardinal;
    procedure setTimeout(AValue: Cardinal);
    function getPayStr: String;
    procedure setPayStr(const AValue: String);
  public
    constructor Create(ACartItems: ICartItems); overload;
  end;

  TCartItems = class(TInterfacedObject, ICartItems)
  protected
    FOrderId: String;
    FSubject: String;
    FItems: TList<PCartItem>;
    function Add(const AId, AName: String; APrice: Currency; ANum: Integer;
      const ARemark: String = ''): Integer;
    procedure Delete(AIndex: Integer);
    procedure Clear;
    function GetCount: Integer;
    function GetItems(const AIndex: Integer): PCartItem;
    function getSubject: String;
    procedure setSubject(const AValue: String);
    function GetSelected(const AIndex: Integer): Boolean;
    procedure SetSelected(const AIndex: Integer; ASelected: Boolean);
    function getOrderId: String;
    procedure setOrderId(const AId: String);
    function PrepareForPay(var ASummary: String; var ATotal: Currency): Integer;
    procedure SelectAll(ASelected: Boolean);
    procedure InvertSelect;
  public
    constructor Create; overload;
    destructor Destroy; override;
  end;

function NewCartItems: ICartItems;
begin
  Result := TCartItems.Create;
end;

procedure AppPay(const AOrderId, ASubject, ASummary: String; AFee: Currency;
  ASources: TPaySources);
var
  F: TfrmWeiPay;
  T: Single;
  ACartItems: ICartItems;
begin
  ACartItems := TCartItems.Create;
  ACartItems.OrderId := AOrderId;
  ACartItems.Subject := ASubject;
  ACartItems.Add('', ASummary, AFee, 1);
  AppPay(ACartItems, ASources);
end;

procedure AppPay(ACartItems: ICartItems; ASources: TPaySources);
var
  F: TfrmWeiPay;
  T: Single;
  AFee: Currency;
  ASummary: String;
  I: Integer;
begin
  if Assigned(ACartItems) and (ACartItems.PrepareForPay(ASummary, AFee) > 0)
  then
  begin
    F := TfrmWeiPay.Create(Application);
    F.CartItems := ACartItems;
    F.lblOrderSummary.Text := ASummary;
    F.lblTitle.Text := '支付-' + ACartItems.Subject;
    F.Fee := AFee; // 金额预计算出来缓存
    if ASources = [] then
      ASources := [TPaySource.psWechat, TPaySource.psAlipay];
    F.loWechat.Visible := TPaySource.psWechat in ASources;
    F.loAlipay.Visible := TPaySource.psAlipay in ASources;
    T := F.loWechat.Position.Y;
    if F.loWechat.Visible then
    begin
      if not F.loAlipay.Visible then
        F.sbPayNow.Position.Y := F.loAlipay.Position.Y;
    end
    else
    begin
      if F.loAlipay.Visible then
      begin
        F.sbPayNow.Position.Y := F.loAlipay.Position.Y;
        F.loAlipay.Position.Y := T;
      end
      else
      begin
        F.sbPayNow.Position.Y := T;
        F.sbPayNow.Text := '无可用的支付方式';
      end;
    end;
    ModalDialog(F,
      procedure(AForm: TForm)
      begin
        (AForm as TfrmWeiPay).DoAfterPay;
      end);
  end;
end;

{ TPayOrder }

constructor TPayOrder.Create(ACartItems: ICartItems);
begin
  inherited Create;
  FCartItems := ACartItems;
end;

function TPayOrder.getCartItems: ICartItems;
begin
  Result := FCartItems;
end;

function TPayOrder.getErrorCode: Integer;
begin
  Result := FErrorCode;
end;

function TPayOrder.getErrorMsg: String;
begin
  Result := FErrorMsg;
end;

function TPayOrder.getFee: Currency;
begin
  Result := FFee;
end;

function TPayOrder.getOrderId: String;
begin
  Result := FOrderId;
end;

function TPayOrder.getPayResult: TPayResult;
begin
  Result := FPayResult;
end;

function TPayOrder.getSign: String;
begin
  Result := FSign;
end;

function TPayOrder.getSource: TPaySource;
begin
  Result := FPaySource;
end;

function TPayOrder.getSubject: String;
begin
  Result := FSubject;
end;

function TPayOrder.getTimestamp: Integer;
begin
  Result := FTimestamp;
end;

procedure TPayOrder.setErrorCode(const AValue: Integer);
begin
  FErrorCode := AValue;
end;

procedure TPayOrder.setErrorMsg(const AValue: String);
begin
  FErrorMsg := AValue;
end;

procedure TPayOrder.setFee(const AValue: Currency);
begin
  FFee := AValue;
end;

procedure TPayOrder.setOrderId(const AValue: String);
begin
  FOrderId := AValue;
end;

procedure TPayOrder.setPayResult(const AResult: TPayResult);
begin
  FPayResult := AResult;
end;

procedure TPayOrder.setSign(const ASign: String);
begin
  FSign := ASign;
end;

procedure TPayOrder.setSource(const ASource: TPaySource);
begin
  FPaySource := ASource;
end;

procedure TPayOrder.setSubject(const AValue: String);
begin
  FSubject := AValue;
end;

procedure TPayOrder.setTimestamp(const AValue: Integer);
begin
  FTimestamp := AValue;
end;

{ TWechatOrder }

function TWechatOrder.getMchId: String;
begin
  Result := FMchId;
end;

function TWechatOrder.getNonceStr: String;
begin
  Result := FNonceStr;
end;

function TWechatOrder.getPayKey: String;
begin
  Result := FPayKey;
end;

function TWechatOrder.getPrepayId: String;
begin
  Result := FPrepayId;
end;

procedure TWechatOrder.setMchId(const AValue: String);
begin
  FMchId := AValue;
end;

procedure TWechatOrder.setNonceStr(const AValue: String);
begin
  FNonceStr := AValue;
end;

procedure TWechatOrder.setPayKey(const AValue: String);
begin
  FPayKey := AValue;
end;

procedure TWechatOrder.setPrepayId(const AValue: String);
begin
  FPrepayId := AValue;
end;

{ TfrmWeiPay }

procedure TfrmWeiPay.DoPay;
begin
  if rbByWechat.IsChecked then
  begin
    FOrder := TWechatOrder.Create(FCartItems);
    FOrder.Source := TPaySource.psWechat;
  end
  else if rbByAlipay.IsChecked then
  begin
    FOrder := TAlipayOrder.Create(FCartItems);
    FOrder.Source := TPaySource.psAlipay;
  end
  else
  begin
    FOrder := nil;
    ModalResult := mrCancel;
    sbPayNow.Enabled := true;
  end;
  if Assigned(FOrder) then
  begin
    FOrder.OrderId := FCartItems.OrderId;
    FOrder.Subject := FCartItems.Subject;
    FOrder.Fee := FFee;
    Workers.Post(DoPrepay, nil, false);
  end;
end;

procedure TfrmWeiPay.DoAfterPay;
begin
  if Assigned(FService) then
    FService.AfterPay(FCartItems, FOrder);
end;

procedure TfrmWeiPay.DoPrepay(AJob: PQJob);
var
  Accept: Boolean;
begin
  if Assigned(FService) then
    Accept := FService.Prepay(FCartItems, FOrder)
  else
    Accept := false;
  if not Accept then
  begin
    Workers.Post(
      procedure(AJob: PQJob)
      begin
        ModalResult := mrCancel;
      end, nil, true);
  end
  else
    Workers.Post(StartPay, nil);
end;

procedure TfrmWeiPay.DoWechatResponse(AResp: IWechatResponse);
var
  APayResp: IWechatPayResponse;
begin
  APayResp := AResp as IWechatPayResponse;
  with FOrder as TPayOrder do
  begin
    if APayResp.PayResult = TWechatPayResult.wprOk then
      FPayResult := TPayResult.prOk
    else
    begin
      FErrorCode := AResp.ErrorCode;
      FErrorMsg := AResp.ErrorMsg;
      if APayResp.PayResult = TWechatPayResult.wprCancel then
        FPayResult := TPayResult.prCancel
      else
        FPayResult := TPayResult.prError;
    end;
  end;
  Workers.Post(
    procedure(AJob: PQJob)
    begin
      ModalResult := mrOk;
    end, nil, true);
end;

procedure TfrmWeiPay.FormCreate(Sender: TObject);
begin
  FService := PaymentService;
end;

procedure TfrmWeiPay.loPayClick(Sender: TObject);
begin
  if not WechatService.Installed then
  begin
    rbByWechat.Enabled := false;
    lblWechatHint.Text := '微信未安装，无法使用微信支付';
  end;

end;

procedure TfrmWeiPay.rbByWechatChange(Sender: TObject);
begin
  sbPayNow.Enabled := rbByWechat.IsChecked or rbByAlipay.IsChecked;
end;

procedure TfrmWeiPay.sbPayNowClick(Sender: TObject);
begin
  sbPayNow.Enabled := false;
  DoPay;
end;

procedure TfrmWeiPay.setFee(const Value: Currency);
begin
  FFee := Value;
  lblOrderFee.Text := '订单金额:' + FormatFloat('0.00', Value);
end;

procedure TfrmWeiPay.sbBackClick(Sender: TObject);
begin
  ModalResult := mrCancel;
  FOrder := nil;
end;

procedure TfrmWeiPay.StartPay(AJob: PQJob);

  procedure PayByWechat;
  var
    AOrder: IWechatPayOrder;
  begin
    AOrder := Order as IWechatPayOrder;
    with WechatService do
    begin
      WechatService.OnResponse := DoWechatResponse;
      WechatService.Pay(AOrder.PrepayId, AOrder.NonceStr, AOrder.Sign,
        AOrder.Timestamp);
    end;
  end;

  procedure PayByAlipay;
  var
    AOrder: IAlipayOrder;
    AResult, AItem, AName, AValue: String;
    pr: PWideChar;
  begin
    AOrder := Order as IAlipayOrder;
    AResult := AlipayService.Pay(AOrder.PayStr);
    pr := PWideChar(AResult);
    while pr^ <> #0 do
    begin
      AItem := DecodeTokenW(pr, ';', #0, true, true);
      AName := NameOfW(AItem, '=');
      if AName = 'resultStatus' then
      begin
        AValue := DeleteSideCharsW(ValueOfW(AItem, '='), '{}');
        if AValue = '9000' then // 成功
        begin
          AOrder.ErrorCode := 0;
          (AOrder as TPayOrder).FPayResult := TPayResult.prOk;
        end
        else
        begin
          AOrder.ErrorCode := StrToIntDef(AValue, -1);
          if AValue = '6001' then
            (AOrder as TPayOrder).FPayResult := TPayResult.prCancel
          else
            (AOrder as TPayOrder).FPayResult := TPayResult.prError;
        end;
      end
      else if AName = 'memo' then
        AOrder.ErrorMsg := DeleteSideCharsW(ValueOfW(AItem, '='), '{}');
    end;
    Workers.Post(
      procedure(AJob: PQJob)
      begin
        ModalResult := mrOk;
      end, nil, true);
  end;

begin
  if FOrder.Source = TPaySource.psWechat then
    PayByWechat
  else
    PayByAlipay;
end;

{ TAlipayOrder }

constructor TAlipayOrder.Create(ACartItems: ICartItems);
begin
  inherited Create(ACartItems);
  FTimeout := 30;
end;

function TAlipayOrder.getPayStr: String;
begin
  Result := FPayStr;
end;

function TAlipayOrder.getTimeout: Cardinal;
begin
  Result := FTimeout;
end;

procedure TAlipayOrder.setPayStr(const AValue: String);
var
  AList: TStringList;
begin
  if AValue <> FPayStr then
  begin
    FPayStr := AValue;
    // 解析字符串，获取签名的值
    AList := TStringList.Create;
    try
      AList.Delimiter := '&';
      AList.DelimitedText := AValue;
      FSign := AList.Values['sign'];
    finally
      FreeAndNil(AList);
    end;
  end;
end;

procedure TAlipayOrder.setTimeout(AValue: Cardinal);
begin
  FTimeout := AValue;
end;
{ TCartItems }

function TCartItems.Add(const AId, AName: String; APrice: Currency;
ANum: Integer; const ARemark: String): Integer;
var
  AItem: PCartItem;
begin
  New(AItem);
  AItem.Id := AId;
  AItem.Name := AName;
  AItem.Price := APrice;
  AItem.Num := ANum;
  AItem.Remark := ARemark;
  AItem.Selected := true;
  FItems.Add(AItem);
end;

procedure TCartItems.Clear;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    Dispose(PCartItem(FItems[I]));
  FItems.Clear;
end;

constructor TCartItems.Create;
begin
  inherited Create;
  FItems := TList<PCartItem>.Create;
end;

procedure TCartItems.Delete(AIndex: Integer);
begin
  Dispose(PCartItem(FItems[AIndex]));
  FItems.Delete(AIndex);
end;

destructor TCartItems.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited;
end;

function TCartItems.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TCartItems.GetItems(const AIndex: Integer): PCartItem;
begin
  Result := FItems[AIndex];
end;

function TCartItems.getOrderId: String;
begin
  Result := FOrderId;
end;

function TCartItems.GetSelected(const AIndex: Integer): Boolean;
begin
  Result := GetItems(AIndex).Selected;
end;

function TCartItems.getSubject: String;
begin
  Result := FSubject;
end;

procedure TCartItems.InvertSelect;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
  begin
    with FItems[I]^ do
      Selected := not Selected;
  end;
end;

function TCartItems.PrepareForPay(var ASummary: String;
var ATotal: Currency): Integer;
var
  I: Integer;
  AItem: PCartItem;
begin
  Result := 0;
  ATotal := 0;
  ASummary := '';
  for I := 0 to FItems.Count - 1 do
  begin
    AItem := FItems[I];
    if AItem.Selected then
    begin
      Inc(Result);
      ASummary := ASummary + AItem.Name + ' ' + FormatFloat('0.00', AItem.Price)
        + '×' + IntToStr(AItem.Num) + '=￥' + FormatFloat('0.00',
        AItem.Num * AItem.Price) + SLineBreak;
      ATotal := ATotal + AItem.Num * AItem.Price;
    end;
  end;
end;

procedure TCartItems.SelectAll(ASelected: Boolean);
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    FItems[I].Selected := ASelected;
end;

procedure TCartItems.setOrderId(const AId: String);
begin
  FOrderId := AId;
end;

procedure TCartItems.SetSelected(const AIndex: Integer; ASelected: Boolean);
begin
  GetItems(AIndex).Selected := true;
end;

procedure TCartItems.setSubject(const AValue: String);
begin
  FSubject := AValue;
end;

function PaymentService: IPaymentService;
begin
  if not TPlatformServices.Current.SupportsPlatformService(IPaymentService,
    Result) then
    Result := nil;
end;

end.
