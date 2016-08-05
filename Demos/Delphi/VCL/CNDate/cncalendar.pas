unit cncalendar;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Types,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Menus, Vcl.StdCtrls,
  Vcl.Grids, Vcl.Samples.Spin, QString, qcndate, Vcl.Buttons;

type
  /// <summary>日期状态参数
  /// cisSelected : 当前处于选中状态
  /// cisHoliday : 当前单元格是节假日
  /// cisHasTask : 当前单元格有日程安排
  /// cisOutOfMonth : 当前单元格不属于选择的月份
  /// icsTitle : 当前单元格是标题行
  /// cisDayOff : 当前日期是休息日
  /// </summary>
  TCalendarItemState = (cisSelected, cisHoliday, cisHasTask, cisOutOfMonth,
    cisTitle, cisDayOff);
  TCalendarItemStates = set of TCalendarItemState;
  TCalendarTaskMarker = (ctdsText, ctdsTrigle, ctdsSignal, ctdsHorizBar,
    ctdsVertBar, ctdsMin = ctdsText, ctdsMax = ctdsVertBar);

  TCalendarItem = record
    Index: Integer; // 所在的格索引位置
    EnDate: TDateTime; // 实际的阳历日期
    CnDate: TCnDate;
    EnYear, EnMonth, EnDay, CnYear, CnMonth, CnDay, Holiday: QStringW; // 节假日名称
    BoundsRect: TRect; // 绑定的显示区域坐标
    DoneTasks, TotalTasks: Integer; // 完成任务数量和总数量
    States: TCalendarItemStates; // 状态
    Data: Pointer; // 用户自己附加的数据内容
  end;

  PCalendarItem = ^TCalendarItem;
  TCalendarNotifyEvent = procedure(ASender: TObject; AItem: PCalendarItem)
    of object;
  TCalendarCellDrawEvent = procedure(ASender: TObject; ACanvas: TCanvas;
    const AItem: PCalendarItem) of object;

  TCalendarItemStyle = class
  protected
    FBackground: TColor;
    FFont: TFont;
    procedure SetFont(const Value: TFont);
  public
    constructor Create; overload;
    destructor Destroy; override;
    property Background: TColor read FBackground write FBackground;
    property Font: TFont read FFont write SetFont;
  end;

  TCalendarTaskItemStyle = class(TCalendarItemStyle)
  private
    FStatusBackground: TColor;
    FStatusColor: TColor;
  public
    property StatusColor: TColor read FStatusColor write FStatusColor;
    property StatusBackground: TColor read FStatusBackground
      write FStatusBackground;
  end;

  TCalendarStyles = class
  protected
    FNormal: TCalendarItemStyle;
    FTaskStatus: TCalendarTaskItemStyle;
    FHoliday: TCalendarItemStyle;
    FTitle: TCalendarItemStyle;
    FDayoff: TCalendarItemStyle;
    FOutOfMonth: TCalendarItemStyle;
    FSelected: TCalendarItemStyle;
  public
    constructor Create; overload;
    destructor Destroy; override;
    property Normal: TCalendarItemStyle read FNormal;
    property TaskStatus: TCalendarTaskItemStyle read FTaskStatus;
    property Holiday: TCalendarItemStyle read FHoliday;
    property Title: TCalendarItemStyle read FTitle;
    property Dayoff: TCalendarItemStyle read FDayoff;
    property OutOfMonth: TCalendarItemStyle read FOutOfMonth;
    property Selected: TCalendarItemStyle read FSelected;
  end;

  TfrmCnCalendar = class(TForm)
    Label2: TLabel;
    dgCalendar: TDrawGrid;
    pnlTitle: TPanel;
    lblYear: TLabel;
    Label3: TLabel;
    lblMonth: TLabel;
    lblCnAnimal: TLabel;
    pnlTypes: TPanel;
    cbxTypes: TComboBox;
    pnlToday: TPanel;
    lblToday: TLabel;
    pmMonthes: TPopupMenu;
    tmHintTimer: TTimer;
    pmExtends: TPopupMenu;
    cseYear: TSpinEdit;
    sbPriorMonth: TSpeedButton;
    sbNextMonth: TSpeedButton;
    procedure dgCalendarSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure dgCalendarDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure tmHintTimerTimer(Sender: TObject);
    procedure cbxTypesChange(Sender: TObject);
    procedure lblMonthClick(Sender: TObject);
    procedure lblYearClick(Sender: TObject);
    procedure cseYearChange(Sender: TObject);
    procedure dgCalendarClick(Sender: TObject);
    procedure dgCalendarDblClick(Sender: TObject);
    procedure dgCalendarMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure lblTodayDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure sbPriorMonthClick(Sender: TObject);
    procedure sbNextMonthClick(Sender: TObject);
  private
    FOnClickDate: TCalendarNotifyEvent;
    FOnLeaveDate: TCalendarNotifyEvent;
    FOnDrawDate: TCalendarCellDrawEvent;
    FOnEnterDate: TCalendarNotifyEvent;
    FOnDblClickDate: TCalendarNotifyEvent;
    FOnInitDate: TCalendarNotifyEvent;
    FSelected: PCalendarItem;
    FMonthCalendar: array [0 .. 48] of TCalendarItem;
    FHoverIndex: Integer;
    FHintIndex: Integer;
    FTaskMarker: TCalendarTaskMarker;
    FItemStyles: TCalendarStyles;
    function GetSelectedDate: TDate;
    procedure SetSelectedDate(const Value: TDate);
    procedure SetTaskMarker(const Value: TCalendarTaskMarker);
    procedure DrawTaskMarker(AItem: PCalendarItem);
    function GetSelectedCnDate: TCnDate;
    procedure SetSelectedCnDate(const Value: TCnDate);
    function FirstDate(const Y: Word; M: Shortint; AByCnDate: Boolean)
      : TDateTime;
    procedure InitCalendar(Y: Word; M: Short; AByCnDate: Boolean);
    procedure InternalMoveToDate(ADate: TDateTime);
    procedure RebuildMonthes(Y: Word; AByCnDate: Boolean);
    procedure SelectMonth(ASender: TObject);
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DefaultDrawDate(ACanvas: TCanvas; AItem: PCalendarItem);
    { Public declarations }
    property OnClickDate: TCalendarNotifyEvent read FOnClickDate
      write FOnClickDate;
    property OnEnterDate: TCalendarNotifyEvent read FOnEnterDate
      write FOnEnterDate;
    property OnLeaveDate: TCalendarNotifyEvent read FOnLeaveDate
      write FOnLeaveDate;
    property OnDrawDate: TCalendarCellDrawEvent read FOnDrawDate
      write FOnDrawDate;
    property OnDblClickDate: TCalendarNotifyEvent read FOnDblClickDate
      write FOnDblClickDate;
    property OnInitDate: TCalendarNotifyEvent read FOnInitDate
      write FOnInitDate;
    property Selected: PCalendarItem read FSelected;
    property SelectedDate: TDate read GetSelectedDate write SetSelectedDate;
    property SelectedCnDate: TCnDate read GetSelectedCnDate
      write SetSelectedCnDate;
    property TaskMarker: TCalendarTaskMarker read FTaskMarker
      write SetTaskMarker;
    property ItemStyles: TCalendarStyles read FItemStyles;
  end;

  TTimeDisplayLevel = (tdlNone, tdlHour, tdlMinute, tdlSecond);
  TDateDisplayLevel = (ddlNone, ddlYear, ddlMonth, ddlDay);

  TCnDatePicker = class(TCustomEdit)
  private
    function GetDate: TDate;
    function GetDateTime: TDateTime;
    function GetTime: TTime;
    procedure SetDate(const Value: TDate);
    procedure SetDateTime(const Value: TDateTime);
    procedure SetTime(const Value: TTime);
    function GetIsDropDown: Boolean;
    procedure SetIsDropDown(const Value: Boolean);
    procedure SetDateLevel(const Value: TDateDisplayLevel);
    procedure SetDisplayCnText(const Value: Boolean);
    procedure SetTimeLevel(const Value: TTimeDisplayLevel);
    procedure SetDisplayDate(const Value: Boolean);
    procedure SetDisplayTime(const Value: Boolean);
  protected
    // 下拉按钮
    FDropDownButton: TSpeedButton;
    // 日期时间值
    FDate: TDateTime;
    // 是否显示农历
    FDisplayCnText: Boolean;
    // 是否显示时间
    FDisplayTime: Boolean;
    // 是否显示日期
    FDisplayDate: Boolean;
    // 是否正在解析日期时间字符串
    FParsing: Boolean;
    // 是否正在更新字符串显示文本内容
    FTextUpdating: Boolean;
    // 显示时间级别
    FTimeLevel: TTimeDisplayLevel;
    // 显示日期级别
    FDateLevel: TDateDisplayLevel;
    // 下拉的日历选择窗口
    FCalendar: TfrmCnCalendar;
    FCalendarProc: TWndMethod;
    procedure UpdateText;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure SetEditRect;
    procedure Resize; override;
    procedure CreateWnd; override;
    procedure DoDateClicked(ASender: TObject; AItem: PCalendarItem);
    procedure DoDateSelected(ASender: TObject; AItem: PCalendarItem);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoCalendarKeyDown(ASender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure ParseText;
    procedure WndProc(var AMsg: TMessage); override;
    procedure CalendarProc(var AMsg: TMessage);
    procedure DropDownCalendar(ASender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DropDown;
    property Date: TDate read GetDate write SetDate;
    property Time: TTime read GetTime write SetTime;
    property DateTime: TDateTime read GetDateTime write SetDateTime;
    property IsDropDown: Boolean read GetIsDropDown write SetIsDropDown;
    property DisplayCnText: Boolean read FDisplayCnText write SetDisplayCnText;
    property DisplayTime: Boolean read FDisplayTime write SetDisplayTime;
    property DisplayDate: Boolean read FDisplayDate write SetDisplayDate;
    property TimeLevel: TTimeDisplayLevel read FTimeLevel write SetTimeLevel;
    property DateLevel: TDateDisplayLevel read FDateLevel write SetDateLevel;
  end;

var
  frmCnCalendar: TfrmCnCalendar;

implementation

uses dateutils;
{$R *.dfm}
{ TfrmCnCalendar }

function TfrmCnCalendar.FirstDate(const Y: Word; M: Shortint;
  AByCnDate: Boolean): TDateTime;
var
  ATemp: TCnDate;
begin
if AByCnDate then // 阴历，则月分为阴历月分
  begin
  ATemp.Year := Y;
  ATemp.Month := M;
  ATemp.Day := 1;
  Result := ToEnDate(ATemp);
  end
else
  Result := EncodeDate(Y, M, 1);
Result := Result - DayOfTheWeek(Result) + 1;
end;

procedure TfrmCnCalendar.FormCreate(Sender: TObject);
begin
TaskMarker := ctdsHorizBar;
end;

procedure TfrmCnCalendar.cbxTypesChange(Sender: TObject);
var
  ADate: TDateTime;
begin
ADate := SelectedDate;
case cbxTypes.ItemIndex of
  0, 3:
    InitCalendar(Selected.CnDate.Year, Selected.CnDate.Month, True);
  1, 2:
    InitCalendar(YearOf(Selected.EnDate), MonthOf(Selected.EnDate), False);
end;
InternalMoveToDate(ADate);
end;

constructor TfrmCnCalendar.Create(AOwner: TComponent);
var
  I: Integer;
begin
inherited;
Constraints.MinHeight := Height;
Constraints.MinWidth := Width;
FHoverIndex := -1;
FHintIndex := -1;
tmHintTimer.Interval := Application.HintPause;
SelectedDate := Now;
for I := 0 to 48 do
  FMonthCalendar[I].Index := I;
FItemStyles := TCalendarStyles.Create;
ItemStyles.Normal.Background := clWindow;
ItemStyles.Normal.Font.Assign(Font);
ItemStyles.Normal.Font.Color := clWindowText;
ItemStyles.TaskStatus.Background := clWindow;
ItemStyles.TaskStatus.Font.Assign(Font);
ItemStyles.TaskStatus.Font.Color := clWindowText;
ItemStyles.TaskStatus.StatusColor := clDkGray;
ItemStyles.TaskStatus.StatusBackground := clLtGray;
ItemStyles.Holiday.Background := clWindow;
ItemStyles.Holiday.Font.Assign(Font);
ItemStyles.Holiday.Font.Color := clRed;
ItemStyles.Title.Background := clLtGray;
ItemStyles.Title.Font.Assign(Font);
ItemStyles.Title.Font.Color := clWindowText;
ItemStyles.Dayoff.Background := clMoneyGreen;
ItemStyles.Dayoff.Font.Assign(Font);
ItemStyles.OutOfMonth.Background := clWindow;
ItemStyles.OutOfMonth.Font.Assign(Font);
ItemStyles.OutOfMonth.Font.Color := clLtGray;
ItemStyles.Selected.Background := clHighlight;
ItemStyles.Selected.Font.Assign(Font);
ItemStyles.Selected.Font.Color := clHighlightText;
end;

procedure TfrmCnCalendar.cseYearChange(Sender: TObject);
begin
if Assigned(FSelected) then
  begin
  if cbxTypes.ItemIndex in [0, 3] then
    SelectedCnDate := CnIncYear(SelectedCnDate, SelectedCnDate.Year -
      cseYear.Value)
  else
    SelectedDate := IncYear(SelectedDate, YearOf(SelectedDate) - cseYear.Value);
  end;
end;

procedure TfrmCnCalendar.DefaultDrawDate(ACanvas: TCanvas;
  AItem: PCalendarItem);
var
  ASize: TSize;
  S: String;
  procedure DrawByCnEn(ACnFirst: Boolean);
  begin
  // 阴阳历
  // 此时布局如下：
  // 阴历 阳历
  // 节日 任务数
  // 如果月分与当前月份不一致，则在左上角标志月份
  // 绘制阳历
  // 一个64*35的矩形区域，
  // 绘制农历日期
  if Length(AItem.CnYear) > 0 then // 第一年
    S := AItem.CnYear
  else if Length(AItem.CnMonth) > 0 then
    S := AItem.CnMonth
  else
    S := AItem.CnDay;
  ASize := ACanvas.TextExtent(S);
  if ACnFirst then
    ACanvas.TextOut(AItem.BoundsRect.Left + 1, AItem.BoundsRect.Top +
      ((17 - ASize.cy) shr 1), S)
  else
    ACanvas.TextOut(AItem.BoundsRect.Right - ASize.cx - 1,
      AItem.BoundsRect.Top + ((17 - ASize.cy) shr 1), S);
  // 绘制阳历日期
  if Length(AItem.EnYear) > 0 then
    S := AItem.EnYear
  else if Length(AItem.EnMonth) > 0 then
    S := AItem.EnMonth + '月'
  else
    S := AItem.EnDay;
  ASize := ACanvas.TextExtent(S);
  if ACnFirst then
    ACanvas.TextOut(AItem.BoundsRect.Right - ASize.cx - 1,
      AItem.BoundsRect.Top + ((17 - ASize.cy) shr 1), S)
  else
    ACanvas.TextOut(AItem.BoundsRect.Left + 1, AItem.BoundsRect.Top +
      ((17 - ASize.cy) shr 1), S);
  end;
  procedure DrawByEn;
  begin
  if Length(AItem.EnYear) > 0 then
    S := AItem.EnYear
  else if Length(AItem.EnMonth) > 0 then
    S := AItem.EnMonth
  else
    S := AItem.EnDay;
  ASize := ACanvas.TextExtent(S);
  ACanvas.TextOut((AItem.BoundsRect.Left + AItem.BoundsRect.Right - ASize.cx)
    shr 1, AItem.BoundsRect.Top + ((17 - ASize.cy) shr 1), S);
  end;

  procedure DrawByCn;
  begin
  if Length(AItem.CnYear) > 0 then // 第一年
    S := AItem.CnYear
  else if Length(AItem.CnMonth) > 0 then
    S := AItem.CnMonth
  else
    S := AItem.CnDay;
  ASize := ACanvas.TextExtent(S);
  ACanvas.TextOut((AItem.BoundsRect.Left + AItem.BoundsRect.Right - ASize.cx)
    shr 1, AItem.BoundsRect.Top + ((17 - ASize.cy) shr 1), S);
  end;

begin
if cisTitle in AItem.States then // 标题
  begin
  S := CnWeekNames[DayOfTheWeek(AItem.EnDate)]; // 标题时与第一行的日期相等
  ASize := ACanvas.TextExtent(S);
  ACanvas.Brush.Color := ItemStyles.Title.Background;
  ACanvas.FillRect(AItem.BoundsRect);
  ACanvas.Font.Assign(ItemStyles.Title.Font);
  ACanvas.TextRect(AItem.BoundsRect,
    (AItem.BoundsRect.Left + AItem.BoundsRect.Right - ASize.cx) shr 1,
    (AItem.BoundsRect.Top + AItem.BoundsRect.Bottom - ASize.cy) shr 1, S);
  end
else // 数据行
  begin
  if cisSelected in AItem.States then
    begin
    ACanvas.Brush.Color := ItemStyles.Selected.Background;
    ACanvas.Font.Assign(ItemStyles.Selected.Font);
    end
  else
    begin
    if cisDayOff in AItem.States then // 休息日？
      begin
      ACanvas.Brush.Color := ItemStyles.Dayoff.Background;
      ACanvas.Font.Assign(ItemStyles.Dayoff.Font);
      end
    else if cisOutOfMonth in AItem.States then
      begin
      ACanvas.Brush.Color := ItemStyles.OutOfMonth.Background;
      ACanvas.Font.Assign(ItemStyles.OutOfMonth.Font);
      end
    else if (cisHoliday in AItem.States) then
      begin
      ACanvas.Brush.Color := ItemStyles.Holiday.Background;
      ACanvas.Font.Assign(ItemStyles.Holiday.Font);
      end
    else if cisHasTask in AItem.States then
      begin
      ACanvas.Brush.Color := ItemStyles.TaskStatus.Background;
      ACanvas.Font.Assign(ItemStyles.TaskStatus.Font);
      end
    else
      begin
      ACanvas.Brush.Color := ItemStyles.Normal.Background;
      ACanvas.Font.Assign(ItemStyles.Normal.Font);
      end;
    end;
  ACanvas.FillRect(AItem.BoundsRect);
  DrawTaskMarker(AItem);
  SetBkMode(ACanvas.Handle, TRANSPARENT);
  case cbxTypes.ItemIndex of
    0: // 阴阳历
      DrawByCnEn(True);
    1: // 阳历
      DrawByCnEn(False);
    2:
      DrawByEn;
    3:
      DrawByCn;
  end;
  if Length(AItem.Holiday) > 0 then
    begin
    ASize := ACanvas.TextExtent(AItem.Holiday);
    if (TaskMarker = ctdsText) and (cisHasTask in AItem.States) then
      ACanvas.TextOut(AItem.BoundsRect.Left + 1, AItem.BoundsRect.Bottom - 17 +
        ((17 - ASize.cy) shr 1), AItem.Holiday)
    else
      ACanvas.TextOut((AItem.BoundsRect.Left + AItem.BoundsRect.Right -
        ASize.cx) shr 1, AItem.BoundsRect.Bottom - 17 + ((17 - ASize.cy) shr 1),
        AItem.Holiday);
    end;
  end;
end;

destructor TfrmCnCalendar.Destroy;
begin
FreeAndNil(FItemStyles);
inherited;
end;

procedure TfrmCnCalendar.dgCalendarClick(Sender: TObject);
begin
cseYear.Visible := False;
if Assigned(OnClickDate) then
  OnClickDate(Self, Selected);
end;

procedure TfrmCnCalendar.dgCalendarDblClick(Sender: TObject);
begin
cseYear.Visible := False;
if Assigned(OnDblClickDate) then
  OnDblClickDate(Self, Selected);
end;

procedure TfrmCnCalendar.dgCalendarDrawCell(Sender: TObject;
  ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
if Assigned(OnDrawDate) then
  OnDrawDate(Self, dgCalendar.Canvas, @FMonthCalendar[ARow * 7 + ACol])
else
  DefaultDrawDate(dgCalendar.Canvas, @FMonthCalendar[ARow * 7 + ACol]);
end;

procedure TfrmCnCalendar.dgCalendarMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow, AIndex: Integer;
begin
if Assigned(OnEnterDate) or Assigned(OnLeaveDate) then
  begin
  ARow := Y div dgCalendar.DefaultRowHeight;
  ACol := X div dgCalendar.DefaultColWidth;
  AIndex := ARow * 7 + ACol;
  if AIndex <> FHoverIndex then
    begin
    if FHoverIndex >= 7 then
      begin
      if Assigned(OnLeaveDate) then
        OnLeaveDate(Self, @FMonthCalendar[FHoverIndex]);
      end;
    FHoverIndex := AIndex;
    if (FHoverIndex >= 7) and Assigned(OnEnterDate) then
      OnEnterDate(Self, @FMonthCalendar[FHoverIndex]);
    end;
  end;
end;

procedure TfrmCnCalendar.dgCalendarSelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
begin
CanSelect := ARow <> 0;
if CanSelect then
  begin
  if Assigned(FSelected) then
    FSelected.States := FSelected.States - [cisSelected];
  FSelected := @FMonthCalendar[ARow * 7 + ACol];
  FSelected.States := FSelected.States + [cisSelected];
  if Assigned(OnEnterDate) then
    OnEnterDate(Self, FSelected);
  end;
end;

procedure TfrmCnCalendar.DrawTaskMarker(AItem: PCalendarItem);
var
  ACanvas: TCanvas;
  procedure DrawTrigle;
  var
    APoints: array [0 .. 2] of TPoint;
    AColor: TColor;
  begin
  APoints[0].X := AItem.BoundsRect.Right - 6;
  APoints[0].Y := AItem.BoundsRect.Bottom - 1;
  APoints[1].X := AItem.BoundsRect.Right - 1;
  APoints[1].Y := AItem.BoundsRect.Bottom - 1;
  APoints[2].X := AItem.BoundsRect.Right - 1;
  APoints[2].Y := AItem.BoundsRect.Bottom - 6;
  if AItem.TotalTasks - AItem.DoneTasks > 0 then
    AColor := ItemStyles.TaskStatus.StatusColor
  else
    AColor := ItemStyles.TaskStatus.StatusBackground;
  if cisSelected in AItem.States then
    AColor := ItemStyles.Selected.FFont.Color;
  ACanvas.Pen.Color := AColor;
  ACanvas.Brush.Color := AColor;
  ACanvas.Polygon(APoints);
  end;
  procedure DrawSignal;
  var
    R: TRect;
    ABkColor, AColor: TColor;
    W, H, L, T, I, ALevel: Integer;
  begin
  R.Left := AItem.BoundsRect.Right - 17;
  R.Top := AItem.BoundsRect.Bottom - 10;
  R.Right := AItem.BoundsRect.Right - 1;
  R.Bottom := AItem.BoundsRect.Bottom - 1;
  if AItem.TotalTasks > 0 then
    begin
    if cisSelected in AItem.States then
      begin
      AColor := ItemStyles.TaskStatus.StatusBackground;
      ABkColor := ItemStyles.Selected.Font.Color;
      end
    else
      begin
      AColor := ItemStyles.TaskStatus.StatusColor;
      ABkColor := ItemStyles.TaskStatus.StatusBackground;
      end;
    ALevel := AItem.DoneTasks * 5 div AItem.TotalTasks;
    end
  else
    Exit;
  W := (R.Right - R.Left) div 9;
  H := (R.Bottom - R.Top) div 5;
  if W = 0 then
    W := 1;
  L := R.Right;
  T := R.Bottom - 5 * H;
  ACanvas.Pen.Color := ABkColor;
  ACanvas.Brush.Color := ABkColor;
  I := 5;
  while I > ALevel do
    begin // 从右向左绘制
    if (W > 1) then
      ACanvas.Rectangle(L - W, T, L, R.Bottom)
    else
      begin
      ACanvas.MoveTo(L, T);
      ACanvas.LineTo(L, R.Bottom);
      end;
    L := L - W - W;
    T := T + H;
    Dec(I);
    end;
  ACanvas.Pen.Color := AColor;
  ACanvas.Brush.Color := AColor;
  while I > 0 do
    begin
    if (W > 1) then
      ACanvas.Rectangle(L - W, T, L, R.Bottom)
    else
      begin
      ACanvas.MoveTo(L, T);
      ACanvas.LineTo(L, R.Bottom);
      end;
    L := L - W - W;
    T := T + H;
    Dec(I);
    end;
  end;

  procedure DrawTaskText;
  var
    S: String;
    ASize: TSize;
    ANum, ALeft, AW: Integer;
  begin
  ANum := AItem.TotalTasks - AItem.DoneTasks;
  if ANum <= 0 then // 没有未完成的任务
    Exit;
  if ANum > 9 then
    S := '9+'
  else
    S := IntToStr(ANum);
  ASize := ACanvas.TextExtent(S);
  AW := (AItem.BoundsRect.Right - AItem.BoundsRect.Left) shr 1;
  ALeft := AItem.BoundsRect.Right - ASize.cx - 1;
  if (ALeft < AItem.BoundsRect.Left + 1 + AW) then
    ALeft := AItem.BoundsRect.Left + 1 + AW;
  ACanvas.TextOut(ALeft, AItem.BoundsRect.Bottom - 17 +
    ((17 - ASize.cy) shr 1), S);
  end;

  procedure DrawHorizBar;
  var
    W, AProgress: Integer;
  begin
  if AItem.TotalTasks > 0 then
    begin
    W := AItem.BoundsRect.Right - AItem.BoundsRect.Left;
    AProgress := W * AItem.DoneTasks div AItem.TotalTasks;
    if AProgress > W then
      AProgress := W;
    end
  else
    AProgress := 0;
  if cisSelected in AItem.States then
    ACanvas.Pen.Color := ItemStyles.Selected.Font.Color
  else
    ACanvas.Pen.Color := ItemStyles.TaskStatus.StatusColor;
  ACanvas.MoveTo(AItem.BoundsRect.Left, AItem.BoundsRect.Bottom - 1);
  ACanvas.LineTo(AItem.BoundsRect.Left + AProgress,
    AItem.BoundsRect.Bottom - 1);
  ACanvas.Pen.Color := ItemStyles.TaskStatus.StatusBackground;
  ACanvas.LineTo(AItem.BoundsRect.Right, AItem.BoundsRect.Bottom - 1);
  end;

  procedure DrawVertBar;
  var
    W, AProgress: Integer;
  begin
  if AItem.TotalTasks > 0 then
    begin
    W := AItem.BoundsRect.Right - AItem.BoundsRect.Left;
    AProgress := W * AItem.DoneTasks div AItem.TotalTasks;
    if AProgress > W then
      AProgress := W;
    end
  else
    AProgress := 0;
  if cisSelected in AItem.States then
    ACanvas.Pen.Color := ItemStyles.Selected.Font.Color
  else
    ACanvas.Pen.Color := ItemStyles.TaskStatus.StatusColor;
  ACanvas.MoveTo(AItem.BoundsRect.Right - 1, AItem.BoundsRect.Bottom);
  ACanvas.LineTo(AItem.BoundsRect.Right - 1, AItem.BoundsRect.Bottom -
    AProgress);
  ACanvas.Pen.Color := ItemStyles.TaskStatus.StatusBackground;
  ACanvas.LineTo(AItem.BoundsRect.Right - 1, AItem.BoundsRect.Top);
  end;

begin
if cisHasTask in AItem.States then
  begin
  ACanvas := dgCalendar.Canvas;
  ACanvas.Lock;
  try
    case TaskMarker of
      ctdsText:
        DrawTaskText;
      ctdsTrigle:
        DrawTrigle;
      ctdsSignal:
        DrawSignal;
      ctdsHorizBar:
        DrawHorizBar;
      ctdsVertBar:
        DrawVertBar;
    end;
  finally
    ACanvas.Unlock;
  end;
  end;
end;

function TfrmCnCalendar.GetSelectedCnDate: TCnDate;
begin
Result := FSelected.CnDate;
end;

function TfrmCnCalendar.GetSelectedDate: TDate;
begin
Result := FSelected.EnDate;
end;

procedure TfrmCnCalendar.InitCalendar(Y: Word; M: Short; AByCnDate: Boolean);
var
  ADate: TDateTime;
  I: Integer;
  AItem: PCalendarItem;
  EY, EM, ED: Word;
  ALastCNDate: TCnDate;
  ALastEY, ALastEM: Word;
begin
ADate := FirstDate(Y, M, AByCnDate);
if AByCnDate then
  begin
  ALastCNDate.Year := Y;
  ALastCNDate.Month := M;
  ALastCNDate.Day := CnDayOf(ADate);
  end;
DecodeDate(ADate, ALastEY, ALastEM, ED);
for I := 7 to 48 do
  begin
  AItem := @FMonthCalendar[I];
  AItem.EnDate := ADate;
  AItem.States := [];
  SetLength(AItem.Holiday, 0);
  DecodeDate(ADate, EY, EM, ED);
  if EY <> ALastEY then
    begin
    AItem.EnYear := IntToStr(EY) + '年';
    if (EM = 1) and (ED = 1) then
      begin
      AItem.Holiday := '元旦';
      AItem.States := AItem.States + [cisHoliday];
      end;
    ALastEY := EY;
    end
  else
    SetLength(AItem.EnYear, 0);
  if EM <> ALastEM then
    begin
    AItem.EnMonth := IntToStr(EM);
    ALastEM := EM;
    end
  else
    SetLength(AItem.EnMonth, 0);
  if (EM <> Word(M)) and (cbxTypes.ItemIndex in [1, 2]) then // 阳历优先
    AItem.States := AItem.States + [cisOutOfMonth];
  AItem.EnDay := IntToStr(ED);
  AItem.CnDate := ToCnDate(ADate);
  if AItem.CnDate.Year <> ALastCNDate.Year then
    begin
    AItem.CnYear := IntToStr(AItem.CnDate.Year) + '年';
    if (AItem.CnDate.Month = 1) or (AItem.CnDate.Day = 1) then
      begin
      AItem.Holiday := '春节';
      AItem.States := AItem.States + [cisHoliday];
      end;
    ALastCNDate.Value := AItem.CnDate.Value;
    end
  else
    SetLength(AItem.CnYear, 0);
  if AItem.CnDate.Month <> ALastCNDate.Month then
    begin
    AItem.CnMonth := CnMonthName(AItem.CnDate);
    ALastCNDate.Value := AItem.CnDate.Value;
    end
  else
    SetLength(AItem.CnMonth, 0);
  if (AItem.CnDate.Month <> M) and (cbxTypes.ItemIndex in [0, 3]) then
    AItem.States := AItem.States + [cisOutOfMonth];
  AItem.CnDay := CnDayName(AItem.CnDate);
  AItem.BoundsRect := dgCalendar.CellRect(I mod 7, I div 7);
  AItem.Data := nil;
  if DayOfTheWeek(AItem.EnDate) in [6, 7] then
    AItem.States := AItem.States + [cisDayOff];
  if Assigned(OnInitDate) then
    OnInitDate(Self, AItem);
  ADate := IncDay(ADate);
  end;
// 初始化标题行的状态
for I := 0 to 6 do
  begin
  FMonthCalendar[I].EnDate := FMonthCalendar[I + 7].EnDate;
  FMonthCalendar[I].CnDate := FMonthCalendar[I + 7].CnDate;
  FMonthCalendar[I].BoundsRect := dgCalendar.CellRect(I, 0);
  FMonthCalendar[I].States := [cisTitle];
  end;
FSelected := nil;
end;

procedure TfrmCnCalendar.InternalMoveToDate(ADate: TDateTime);
var
  AIndex, AYear, AMonth: Integer;
begin
AIndex := 7 + Trunc(ADate - FMonthCalendar[0].EnDate);
FSelected := @FMonthCalendar[AIndex];
dgCalendar.Invalidate;
if cbxTypes.ItemIndex in [1, 2] then
  begin
  AYear := YearOf(ADate);
  AMonth := MonthOf(ADate);
  RebuildMonthes(AYear, False);
  if lblMonth.Tag <> AMonth then
    begin
    lblMonth.Caption := IntToStr(AMonth) + '月';
    lblMonth.Tag := AMonth;
    end;
  end
else
  begin
  AYear := FSelected.CnDate.Year;
  AMonth := FSelected.CnDate.Month;
  RebuildMonthes(AYear, True);
  if lblMonth.Tag <> AMonth then
    begin
    lblMonth.Caption := CnMonthName(FSelected.CnDate);
    lblMonth.Tag := AMonth;
    end;
  end;
if cseYear.Value <> AYear then
  begin
  cseYear.Value := AYear;
  lblYear.Caption := IntToStr(AYear);
  end;
dgCalendar.Row := AIndex div 7;
dgCalendar.Col := AIndex mod 7;
end;

procedure TfrmCnCalendar.lblMonthClick(Sender: TObject);
begin
pmMonthes.Popup(lblMonth.ClientOrigin.X, lblMonth.ClientOrigin.Y +
  lblMonth.Height);
end;

procedure TfrmCnCalendar.lblTodayDblClick(Sender: TObject);
begin
SelectedDate := Today;
end;

procedure TfrmCnCalendar.lblYearClick(Sender: TObject);
begin
cseYear.SetBounds(lblYear.BoundsRect.Right - cseYear.Width,
  lblYear.BoundsRect.Top + ((lblYear.Height - cseYear.Height) div 2),
  cseYear.Width, cseYear.Height);
cseYear.Visible := True;
cseYear.SetFocus;
end;

procedure TfrmCnCalendar.RebuildMonthes(Y: Word; AByCnDate: Boolean);
var
  AMenu: TMenuItem;
  I: Integer;
  ACnDate: TCnDate;
begin
pmMonthes.Items.Clear;
if AByCnDate then
  begin
  ACnDate.Year := Y;
  ACnDate.Month := 1;
  ACnDate.Day := 1;
  while ACnDate.Year = Y do
    begin
    AMenu := TMenuItem.Create(pmMonthes);
    AMenu.Tag := ACnDate.Month;
    AMenu.Caption := CnMonthName(ACnDate);
    AMenu.OnClick := SelectMonth;
    pmMonthes.Items.Add(AMenu);
    ACnDate := CnIncMonth(ACnDate);
    end;
  end
else
  begin
  for I := 1 to 12 do
    begin
    AMenu := TMenuItem.Create(pmMonthes);
    AMenu.Caption := IntToStr(I) + '月';
    AMenu.Tag := I;
    AMenu.OnClick := SelectMonth;
    pmMonthes.Items.Add(AMenu);
    end;
  end;
end;

procedure TfrmCnCalendar.sbNextMonthClick(Sender: TObject);
begin
if cbxTypes.ItemIndex in [0, 3] then
  SelectedCnDate := CnIncMonth(SelectedCnDate)
else
  SelectedDate := IncMonth(SelectedDate);
end;

procedure TfrmCnCalendar.sbPriorMonthClick(Sender: TObject);
begin
if cbxTypes.ItemIndex in [0, 3] then
  SelectedCnDate := CnIncMonth(SelectedCnDate, -1)
else
  SelectedDate := IncMonth(SelectedDate, -1);
end;

procedure TfrmCnCalendar.SelectMonth(ASender: TObject);
var
  Y, M, D, ADays: Integer;
begin
Y := cseYear.Value;
M := TMenuItem(ASender).Tag;
if cbxTypes.ItemIndex in [0, 3] then
  begin
  D := FSelected.CnDate.Day;
  ADays := CnMonthDays(Y, M);
  if D > ADays then
    D := ADays;
  SetSelectedCnDate(CnDate(Y, M, D))
  end
else
  begin
  D := DayOf(FSelected.EnDate);
  if D > MonthDays[IsLeapYear(Y)][M] then
    D := MonthDays[IsLeapYear(Y)][M];
  SetSelectedDate(EncodeDate(cseYear.Value, TMenuItem(ASender).Tag, D));
  end;
end;

procedure TfrmCnCalendar.SetSelectedCnDate(const Value: TCnDate);
begin
if (not Assigned(FSelected)) or ((Value.Year <> FSelected.CnDate.Year) or
  (Value.Month <> FSelected.CnDate.Month)) then
  InitCalendar(Value.Year, Value.Month, True);
InternalMoveToDate(ToEnDate(Value));
end;

procedure TfrmCnCalendar.SetSelectedDate(const Value: TDate);
var
  OY, OM, OD, NY, NM, ND: Word;
begin
if not Assigned(FSelected) or (Value <> FSelected.EnDate) then
  begin
  if cbxTypes.ItemIndex in [1, 2] then
    // 阳阴历或阳历
    begin
    DecodeDate(Value, NY, NM, ND);
    if Assigned(FSelected) then
      begin
      DecodeDate(FSelected.EnDate, OY, OM, OD);
      if (OY <> NY) or (OM <> NM) then
        InitCalendar(NY, NM, False);
      end
    else
      InitCalendar(NY, NM, False);
    InternalMoveToDate(Value);
    end
  else
    SetSelectedCnDate(ToCnDate(Value));
  end;
end;

procedure TfrmCnCalendar.SetTaskMarker(const Value: TCalendarTaskMarker);
begin
if FTaskMarker <> Value then
  begin
  FTaskMarker := Value;
  dgCalendar.Invalidate;
  end;
end;

procedure TfrmCnCalendar.tmHintTimerTimer(Sender: TObject);
var
  Y, M, D, H, N, S, MS: Word;
  ACnDate: TCnDate;
  AEnDate: TDateTime;
begin
AEnDate := Now;
DecodeDateTime(AEnDate, Y, M, D, H, N, S, MS);
ACnDate := ToCnDate(AEnDate);
lblToday.Caption := '阳历: ' + IntToStr(Y) + ' 年 ' + IntToStr(M) + ' 月 ' +
  IntToStr(D) + ' 日 ' + '农历: ' + IntToStr(ACnDate.Year) + ' ' +
  CnYearName(ACnDate) + '年' + CnMonthName(ACnDate) + CnDayName(ACnDate) +
  CnHourName(H) + '时 ' + FormatDateTime('hh:nn:ss', AEnDate);
end;

{ TCnDatePicker }

procedure TCnDatePicker.CalendarProc(var AMsg: TMessage);
begin

FCalendarProc(AMsg);
end;

constructor TCnDatePicker.Create(AOwner: TComponent);
begin
inherited;
FDropDownButton := TSpeedButton.Create(Self);
FDropDownButton.Width := 17;
FDropDownButton.Height := 17;
FDropDownButton.Visible := True;
FDropDownButton.Font.Name := 'Marlett';
FDropDownButton.Font.Size := 9;
FDropDownButton.Caption := '6';
FDropDownButton.OnClick := DropDownCalendar;
FDropDownButton.Parent := Self;
FDropDownButton.Cursor := crArrow;
FDisplayCnText := True;
FDisplayDate := True;
FDisplayTime := False;
FTimeLevel := tdlSecond; // 默认时间只显示到秒
FDateLevel := ddlDay; // 默认显示到日
Width := 230;
Height := 21;
DateTime := Now();
ControlStyle := ControlStyle - [csSetCaption];
ReadOnly := False; // 只读
end;

procedure TCnDatePicker.CreateParams(var Params: TCreateParams);
begin
inherited;
Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN; //
end;

procedure TCnDatePicker.CreateWnd;
begin
inherited;
SetEditRect;
end;

destructor TCnDatePicker.Destroy;
begin

inherited;
end;

procedure TCnDatePicker.DoCalendarKeyDown(ASender: TObject; var Key: Word;
  Shift: TShiftState);
begin
if FCalendar.Visible then
  begin
  if Key = VK_ESCAPE then
    begin
    if (FCalendar.ActiveControl = FCalendar.dgCalendar) then
      FCalendar.Close;
    end
  else if Key = VK_RETURN then
    FCalendar.ModalResult := mrOk;
  end;
end;

procedure TCnDatePicker.DoDateClicked(ASender: TObject; AItem: PCalendarItem);
begin
DateTime := FCalendar.SelectedDate;
end;

procedure TCnDatePicker.DoDateSelected(ASender: TObject; AItem: PCalendarItem);
begin
FCalendar.ModalResult := mrOk;
end;

procedure TCnDatePicker.DoEnter;
begin
inherited;
SelectAll;
end;

procedure TCnDatePicker.DoExit;
begin
inherited;
end;

procedure TCnDatePicker.DropDown;
var
  LT: TPoint;
  AMonitor: TMonitor;
  AForm: TCustomForm;
  ALast: TDateTime;
  function CalendarFocused: Boolean;
  begin
  Result := (Screen.ActiveControl = Self) or (Screen.ActiveForm = FCalendar);
  end;
  procedure AjdustPosition;
  begin
  LT := ClientToScreen(Point(0, 0));
  AForm := GetParentForm(Self);
  AMonitor := AForm.Monitor;
  if LT.Y + Height + FCalendar.Height > AMonitor.BoundsRect.Bottom then
    begin
    if FCalendar.Height < LT.Y - AMonitor.BoundsRect.Top then
      LT.Y := LT.Y - FCalendar.Height;
    end
  else
    Inc(LT.Y, Height);
  if LT.X + FCalendar.Width > AMonitor.BoundsRect.Right then
    LT.X := AMonitor.BoundsRect.Right - FCalendar.Width;
  if (LT.X <> FCalendar.Left) or (LT.Y <> FCalendar.Top) then
    FCalendar.SetBounds(LT.X, LT.Y, FCalendar.Width, FCalendar.Height);
  end;

begin
ALast := FDate;
FCalendar := TfrmCnCalendar.Create(Self);
try
  FCalendarProc := FCalendar.WindowProc;
  FCalendar.WindowProc := CalendarProc;
  FCalendar.SelectedDate := FDate;
  FCalendar.OnClickDate := DoDateClicked;
  FCalendar.OnDblClickDate := DoDateSelected;
  FCalendar.FormStyle := fsStayOnTop;
  FCalendar.Position := poDesigned;
  FCalendar.KeyPreview := True;
  FCalendar.OnKeyDown := DoCalendarKeyDown;
  AjdustPosition;
  FCalendar.Show;
  repeat
    if WaitMessage then
      begin
      Application.ProcessMessages;
      AjdustPosition;
      end;
  until Application.Terminated or (not AForm.Visible) or
    (FCalendar.ModalResult <> mrNone) or (not FCalendar.Visible) or
    (not CalendarFocused);
  if FCalendar.ModalResult = mrOk then
    DateTime := FCalendar.SelectedDate
  else
    DateTime := ALast;
finally
  FreeAndNil(FCalendar);
end;
end;

procedure TCnDatePicker.DropDownCalendar(ASender: TObject);
begin
ParseText;
UpdateText;
DropDown;
end;

function TCnDatePicker.GetDate: TDate;
begin
Result := GetDateTime;
end;

function TCnDatePicker.GetDateTime: TDateTime;
var
  Y, M, D, H, N, S, MS: Word;
begin
Result := 0;
if FDisplayDate then
  begin
  DecodeDate(FDate, Y, M, D);
  case FDateLevel of
    ddlYear:
      Result := EncodeDate(Y, 1, 1);
    ddlMonth:
      Result := EncodeDate(Y, M, 1);
    ddlDay:
      Result := EncodeDate(Y, M, D);
  end;
  end;
if FDisplayTime then
  begin
  DecodeTime(FDate, H, N, S, MS);
  case FTimeLevel of
    tdlHour:
      Result := Result + EncodeTime(H, 0, 0, 0);
    tdlMinute:
      Result := Result + EncodeTime(H, N, 0, 0);
    tdlSecond:
      Result := Result + EncodeTime(H, N, S, 0);
  end;
  end;
end;

function TCnDatePicker.GetIsDropDown: Boolean;
begin
Result := Assigned(FCalendar);
end;

function TCnDatePicker.GetTime: TTime;
begin
Result := GetDateTime;
end;

procedure TCnDatePicker.KeyDown(var Key: Word; Shift: TShiftState);
begin
if Key = VK_RETURN then
  begin
  ParseText;
  UpdateText;
  IsDropDown := False;
  inherited;
  Key := 0;
  end
else
  begin
  case Key of
    VK_ADD, VK_OEM_PLUS, VK_SUBTRACT, VK_OEM_MINUS, VK_PRIOR, VK_NEXT:
      // Page Down
      begin
      case Key of
        VK_ADD, VK_OEM_PLUS, VK_NEXT:
          begin
          case FDateLevel of
            ddlYear:
              DateTime := IncYear(DateTime);
            ddlMonth:
              DateTime := IncMonth(DateTime);
            ddlDay:
              DateTime := IncDay(DateTime);
          end;
          end
      else
        begin
        case FDateLevel of
          ddlYear:
            DateTime := IncYear(DateTime, -1);
          ddlMonth:
            DateTime := IncMonth(DateTime, -1);
          ddlDay:
            DateTime := IncDay(DateTime, -1);
        end;
        end;
      end;
      end;
  end;
  inherited;
  end;
end;

procedure TCnDatePicker.ParseText;
var
  ADate: TDateTime;
  S: QStringW;
  Y, M, D, H, N, Sec: Int64;
  function IsDigits: Boolean;
  var
    p: PQCharW;
  begin
  p := PQCharW(S);
  Result := True;
  while p^ <> #0 do
    begin
    if (p^ >= '0') and (p^ <= '9') then
      Inc(p)
    else
      begin
      Result := False;
      Break;
      end;
    end;
  end;
  procedure ParseAsDigits;
  begin
  // yyyymmddhhnnss?
  if Length(S) = 6 then // yymmdd/hhnnss
    begin
    if TryStrToInt64(LeftStrW(S, 2, False), Y) and
      TryStrToInt64(Copy(S, 3, 2), M) and
      TryStrToInt64(RightStrW(S, 2, False), D) then
      begin
      if not TryEncodeTime(Y, M, D, 0, ADate) then
        begin
        if (Y < 50) then
          begin
          if not TryEncodeDate(2000 + Y, M, D, ADate) then
            raise EConvertError.CreateFmt('%s 不是一个有效的日期时间值', [S]);
          end
        else if not TryEncodeDate(1900 + Y, M, D, ADate) then
          raise EConvertError.CreateFmt('%s 不是一个有效的日期时间值', [S]);
        end;
      end
    else
      raise EConvertError.CreateFmt('%s 不是一个有效的日期时间值', [S]);
    end
  else if Length(S) >= 8 then // yyyymmdd
    begin
    if TryStrToInt64(LeftStrW(S, 4, False), Y) and
      TryStrToInt64(Copy(S, 5, 2), M) and
      TryStrToInt64(RightStrW(S, 2, False), D) then
      begin
      if not TryEncodeDate(Y, M, D, ADate) then
        raise EConvertError.CreateFmt('%s 不是一个有效的日期时间值', [S]);
      if Length(S) >= 10 then
        begin
        if TryStrToInt64(Copy(S, 9, 2), H) then
          begin
          if Length(S) >= 12 then
            begin
            if TryStrToInt64(Copy(S, 11, 2), N) then
              begin
              if Length(S) = 14 then
                begin
                if TryStrToInt64(Copy(S, 13, 2), Sec) then
                  ADate := EncodeDateTime(Y, M, D, H, N, Sec, 0)
                else
                  raise EConvertError.CreateFmt('%s 不是一个有效的日期时间值', [S]);
                end
              else
                ADate := EncodeDateTime(Y, M, D, H, N, 0, 0);
              end
            else
              raise EConvertError.CreateFmt('%s 不是一个有效的日期时间值', [S]);
            end
          else
            ADate := EncodeDateTime(Y, M, D, H, 0, 0, 0);
          end
        else
          raise EConvertError.CreateFmt('%s 不是一个有效的日期时间值', [S]);
        end
      else
        ADate := EncodeDate(Y, M, D);
      end
    else
      raise EConvertError.CreateFmt('%s 不是一个有效的日期时间值', [S]);;
    end;
  end;

  procedure ParseAsCnFormat;
  var
    p: PQCharW;
    V: Int64;
  begin
  p := PQCharW(S);
  Y := 0;
  M := 0;
  D := 0;
  H := 255;
  N := 255;
  Sec := 255;
  repeat
    if ParseInt(p, V) <> 0 then
      begin
      SkipSpaceW(p);
      if p^ = '年' then
        Y := V
      else if p^ = '月' then
        M := V
      else if p^ = '日' then
        D := V
      else if p^ = ':' then
        begin
        if H = 255 then
          H := V
        else
          N := V;
        end
      else
        begin
        Sec := V;
        Break;
        end;
      while (p^ <> #0) and ((p^ < '0') or (p^ > '9')) do
        Inc(p);
      end
    else
      Break;
  until p^ = #0;
  if H = 255 then
    H := 0;
  if N = 255 then
    N := 0;
  if Sec = 255 then
    Sec := 0;
  if (Y <> 0) and (M <> 0) and (D <> 0) then
    begin
    if not TryEncodeDateTime(Y, M, D, H, N, Sec, 0, ADate) then
      raise EConvertError.CreateFmt('%s 不是一个有效的日期时间值', [S]);
    end
  else
    begin
    if not TryEncodeTime(H, N, Sec, 0, ADate) then
      raise EConvertError.CreateFmt('%s 不是一个有效的日期时间值', [S]);
    end;
  end;

begin
S := Trim(Text);
ADate := 0;
if not(TryStrToDate(PQCharW(S), ADate) or ParseDateTime(PQCharW(S), ADate) or
  ParseWebTime(PQCharW(S), ADate)) then
  begin
  if IsDigits then
    ParseAsDigits
  else
    ParseAsCnFormat;
  end;
DateTime := ADate;
end;

procedure TCnDatePicker.Resize;
begin
if FDisplayDate then
  FDropDownButton.SetBounds(ClientWidth - FDropDownButton.Width, 0,
    FDropDownButton.Width, ClientHeight);
SetEditRect();
inherited;
end;

procedure TCnDatePicker.SetDate(const Value: TDate);
begin
SetDateTime(Value);
end;

procedure TCnDatePicker.SetDateLevel(const Value: TDateDisplayLevel);
begin
if FDateLevel <> Value then
  begin
  FDateLevel := Value;
  Invalidate;
  end;
end;

procedure TCnDatePicker.SetDateTime(const Value: TDateTime);
var
  Y, M, D: Word;
begin
if FDate <> Value then
  begin
  DecodeDate(Value, Y, M, D);
  if (Y < 1901) or (Y > 2050) then
    raise Exception.Create('日期的表示范围限于1901-2050之间。');
  FDate := Value;
  if Assigned(FCalendar) then
    FCalendar.SelectedDate := Value;
  UpdateText;
  if Assigned(OnChange) then
    OnChange(Self);
  end;
end;

procedure TCnDatePicker.SetDisplayCnText(const Value: Boolean);
begin
if FDisplayCnText <> Value then
  begin
  FDisplayCnText := Value;
  UpdateText;
  Invalidate;
  end;
end;

procedure TCnDatePicker.SetDisplayDate(const Value: Boolean);
begin
if FDisplayDate <> Value then
  begin
  FDisplayDate := Value;
  UpdateText;
  Invalidate;
  end;
end;

procedure TCnDatePicker.SetDisplayTime(const Value: Boolean);
begin
if FDisplayTime <> Value then
  begin
  FDisplayTime := Value;
  UpdateText;
  Invalidate;
  end;
end;

procedure TCnDatePicker.SetEditRect;
var
  Loc: TRect;
begin
SendMessage(Handle, EM_GETRECT, 0, LPARAM(@Loc));
Loc.Bottom := ClientHeight + 1; // +1 is workaround for windows paint bug
if FDisplayDate then
  Loc.Right := ClientWidth - FDropDownButton.Width - 2
else
  Loc.Right := ClientWidth - 2;
Loc.Top := 0;
Loc.Left := 0;
SendMessage(Handle, EM_SETRECTNP, 0, LPARAM(@Loc));
end;

procedure TCnDatePicker.SetIsDropDown(const Value: Boolean);
begin
if Value <> IsDropDown then
  begin
  if Value then
    DropDown
  else
    FCalendar.Close;
  end;
end;

procedure TCnDatePicker.SetTime(const Value: TTime);
begin
SetDateTime(Value);
end;

procedure TCnDatePicker.SetTimeLevel(const Value: TTimeDisplayLevel);
begin
FTimeLevel := Value;
end;

procedure TCnDatePicker.UpdateText;
var
  Y, M, D: Word;
  ACnDate: TCnDate;
  S: String;
begin
if FTextUpdating then
  Exit;
DecodeDate(FDate, Y, M, D);
ACnDate := ToCnDate(FDate);
if FDisplayDate then
  begin
  case (FDateLevel) of
    ddlYear:
      begin
      if FDisplayCnText then
        S := Format('%d年 %s年', [Integer(Y), CnYearName(ACnDate)])
      else
        S := IntToStr(Y) + '年';
      end;
    ddlMonth:
      begin
      if (FDisplayCnText) then
        S := Format('%.4d年%.2d月 %s年%s', [Integer(Y), Integer(M),
          CnYearName(ACnDate), CnMonthName(ACnDate)])
      else
        S := Format('%.4d年%.2d月', [Integer(Y), Integer(M)]);
      end;
    ddlDay:
      begin
      if (FDisplayCnText) then
        S := Format('%.4d年%.2d月%.2d日 %s年%s%s', [Integer(Y), Integer(M),
          Integer(D), CnYearName(ACnDate), CnMonthName(ACnDate),
          CnDayName(ACnDate)])
      else
        S := Format('%.4d年%.2d月%.2d日', [Integer(Y), Integer(M), Integer(D)]);
      end;
  end;
  end
else
  SetLength(S, 0);
if FDisplayTime then
  begin
  case FTimeLevel of
    tdlHour:
      S := S + FormatDateTime(' hh', FDate);
    tdlMinute:
      S := S + FormatDateTime(' hh:nn', FDate);
    tdlSecond:
      S := S + FormatDateTime(' hh:nn:ss', FDate);
  end;
  end;
if Text <> S then
  begin
  FTextUpdating := True;
  try
    Text := S;
  finally
    FTextUpdating := False;
  end;
  end;
end;

procedure TCnDatePicker.WndProc(var AMsg: TMessage);
begin
if (AMsg.Msg = CM_TEXTCHANGED) or (AMsg.Msg = CM_EXIT) then
  begin
  ParseText;
  end
else if (AMsg.Msg = EM_SETREADONLY) or (AMsg.Msg = CM_VISIBLECHANGED) then
  FDropDownButton.Visible := AMsg.WParam = 0
else if AMsg.Msg = CM_SHOWINGCHANGED then
  FDropDownButton.Visible := not ReadOnly;
inherited;
end;

{ TCalendarItemStyle }

constructor TCalendarItemStyle.Create;
begin
inherited;
FFont := TFont.Create;
end;

destructor TCalendarItemStyle.Destroy;
begin
FreeAndNil(FFont);
inherited;
end;

procedure TCalendarItemStyle.SetFont(const Value: TFont);
begin
FFont.Assign(Value);
end;

{ TCalendarStyles }

constructor TCalendarStyles.Create;
begin
inherited;
FNormal := TCalendarItemStyle.Create;
FTaskStatus := TCalendarTaskItemStyle.Create;
FHoliday := TCalendarItemStyle.Create;
FTitle := TCalendarItemStyle.Create;
FDayoff := TCalendarItemStyle.Create;
FOutOfMonth := TCalendarItemStyle.Create;
FSelected := TCalendarItemStyle.Create;
end;

destructor TCalendarStyles.Destroy;
begin
FreeObject(FNormal);
FreeObject(FTaskStatus);
FreeObject(FHoliday);
FreeObject(FTitle);
FreeObject(FDayoff);
FreeObject(FOutOfMonth);
FreeObject(FSelected);
inherited;
end;

end.
