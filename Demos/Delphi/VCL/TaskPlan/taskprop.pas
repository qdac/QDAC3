unit taskprop;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, formdropdown, Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.CheckLst, Vcl.ExtCtrls, Vcl.Samples.Spin, QTimeTypes, QString;

type
  TJobFireTime = record
    ByWeeks: Int64; // 周次
    ByMinutes: Int64; // 特定分钟
    BySeconds: Int64; // 特定秒
    ByHours: Cardinal; // 特定小时
    ByDays: Cardinal; // 特定天数
    ByMonthes: Word; // 特定月份
    ByWeekDays: Byte; // 特定星期
    ByInterval: TQInterval;
    FirstRunAt: TDateTime;
  private
    function GetByDays(AIndex: Byte): Boolean;
    function GetByHours(AIndex: Byte): Boolean;
    function GetByMinites(AIndex: Byte): Boolean;
    function GetByMonthes(AIndex: Byte): Boolean;
    function GetBySeconds(AIndex: Byte): Boolean;
    function GetByWeeks(AIndex: Byte): Boolean;
    function GetByWeekDays(AIndex: Byte): Boolean;
    procedure SetByDays(AIndex: Byte; const Value: Boolean);
    procedure SetByHours(AIndex: Byte; const Value: Boolean);
    procedure SetByMinutes(AIndex: Byte; const Value: Boolean);
    procedure SetByMonthes(AIndex: Byte; const Value: Boolean);
    procedure SetBySeconds(AIndex: Byte; const Value: Boolean);
    procedure SetByWeeks(AIndex: Byte; const Value: Boolean);
    procedure SetByWeekDays(AIndex: Byte; const Value: Boolean);
    function GetNextTime: TDateTime; // 重复时间间隔
  public
    property IsByWeeks[AIndex: Byte]: Boolean read GetByWeeks write SetByWeeks;
    property IsByMinutes[AIndex: Byte]: Boolean read GetByMinites
      write SetByMinutes;
    property IsBySeconds[AIndex: Byte]: Boolean read GetBySeconds
      write SetBySeconds;
    property IsByHours[AIndex: Byte]: Boolean read GetByHours write SetByHours;
    property IsByDays[AIndex: Byte]: Boolean read GetByDays write SetByDays;
    property IsByMonthes[AIndex: Byte]: Boolean read GetByMonthes
      write SetByMonthes;
    property IsByWeekDays[AIndex: Byte]: Boolean read GetByWeekDays
      write SetByWeekDays;
    property NextTime: TDateTime read GetNextTime;
  end;

  TJobTaskType = (jttShell, jttMessage);

  TJobPlan = record
    Name: String;
    BySignals: String; // 信号触发时，关联的信号列表
    ByTime: TJobFireTime; // 时间触发时，关联的时间安排
    TaskType: TJobTaskType; // 任务类型
    TaskText: String; // 任务内容文本
  end;

  PJobPlan = ^TJobPlan;
  TJobPlans = array of TJobPlan;

  TfrmTaskProps = class(TfrmDropDownBase)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    tsTask: TTabSheet;
    RadioButton9: TRadioButton;
    edtShellCmd: TEdit;
    Button1: TButton;
    rbIsMessageTask: TRadioButton;
    mmHintMsg: TMemo;
    odExecute: TOpenDialog;
    pgcFireSources: TPageControl;
    gbxExecWeek: TGroupBox;
    Label10: TLabel;
    clbWeekDayList: TCheckListBox;
    chkAnyWeekDay: TCheckBox;
    seWeekDayInterval: TSpinEdit;
    gbxExecDay: TGroupBox;
    Label11: TLabel;
    clbDayList: TCheckListBox;
    chkAnyDay: TCheckBox;
    seDayInterval: TSpinEdit;
    gbxExecHour: TGroupBox;
    Label8: TLabel;
    clbHourList: TCheckListBox;
    chkAnyHour: TCheckBox;
    seHourInterval: TSpinEdit;
    gbxExecMinute: TGroupBox;
    clbMinuteList: TCheckListBox;
    Panel2: TPanel;
    Label9: TLabel;
    chkAnyMinute: TCheckBox;
    seMinuteInterval: TSpinEdit;
    gbxExecMonth: TGroupBox;
    Label3: TLabel;
    clbMonthList: TCheckListBox;
    chkAnyMonth: TCheckBox;
    seMonthInterval: TSpinEdit;
    Panel3: TPanel;
    Button2: TButton;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
    FPlan: TJobPlan;
    procedure EnableChildren(AParent: TWinControl; AEnabled: Boolean);
    function GetActivePlan: TJobPlan;
    function CreatePlan: TQPlanMask;
  public
    { Public declarations }
    property ActivePlan: TJobPlan read GetActivePlan;
  end;

var
  frmTaskProps: TfrmTaskProps;

implementation

uses dateutils, math;
{$R *.dfm}
{ TJobFireTime }

function TJobFireTime.GetByDays(AIndex: Byte): Boolean;
begin
assert((AIndex > 0) and (AIndex <= 31));
Result := (ByDays and (1 shl (AIndex - 1))) <> 0;
end;

function TJobFireTime.GetByHours(AIndex: Byte): Boolean;
begin
assert(AIndex < 24);
Result := (ByHours and (1 shl AIndex)) <> 0;
end;

function TJobFireTime.GetByMinites(AIndex: Byte): Boolean;
begin
assert(AIndex <= 59);
Result := (ByMinutes and (1 shl AIndex)) <> 0;
end;

function TJobFireTime.GetByMonthes(AIndex: Byte): Boolean;
begin
assert((AIndex > 0) and (AIndex <= 12));
Result := (ByMonthes and (1 shl (AIndex - 1))) <> 0;
end;

function TJobFireTime.GetBySeconds(AIndex: Byte): Boolean;
begin
assert((AIndex > 0) and (AIndex <= 59));
Result := (BySeconds and (1 shl AIndex)) <> 0;
end;

function TJobFireTime.GetByWeeks(AIndex: Byte): Boolean;
begin
assert((AIndex > 0) and (AIndex < 55));
Result := (ByWeeks and (1 shl AIndex)) <> 0;
end;

// 计算作业下次执行时间
function TJobFireTime.GetNextTime: TDateTime;
var
  ANow: TDateTime;
  ANextWeekDay, ANextMonthDay, ANextIntervalTime, ANextTime: TDateTime;
  function WeekStartDate(ADate: TDateTime): TDateTime;
  begin
  Result := IncDay(ADate, 1 - DayOfWeek(ADate));
  end;
  procedure CalcWeeksNext;
  var
    I, J: Integer;
    AWeek: Integer;
    ADate: TDateTime;
  begin
  ANextWeekDay := 0;
  if ByWeeks <> 0 then
    begin
    AWeek := WeekOfTheYear(ANow);
    for I := AWeek to 54 do
      begin
      if IsByWeeks[I] then
        begin
        // 看周内的那一天？
        for J := 1 to 7 do
          begin
          if IsByWeekDays[J] then
            begin
            ADate := IncDay(WeekStartDate(ANow), 7 * (I - 1) + J);
            if ADate >= ANow then
              begin
              ANextWeekDay := Result;
              Break;
              end;
            end;
          end;
        end;
      end;
    end;
  end;

  procedure CalcMonthNext;
  var
    AYear, AMonth: Integer;
    I, J: Integer;
    AIsLeapYear: Boolean;
    ADate: TDateTime;
  begin
  if ByMonthes <> 0 then
    begin
    AMonth := MonthOf(ANow);
    AYear := YearOf(ANow);
    AIsLeapYear := IsLeapYear(AYear);
    ANextMonthDay := 0;
    for I := AMonth to 12 do
      begin
      if IsByMonthes[I] then
        begin
        for J := 1 to MonthDays[AIsLeapYear][I] do
          begin
          if IsByDays[J] then
            begin
            ADate := EncodeDate(AYear, AMonth, J);
            if ADate >= ANow then
              begin
              ANextMonthDay := ADate;
              Break;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
  procedure CalcInterval;
  var
    ADate: TDateTime;
  begin
  ANextIntervalTime := 0;
  if not ByInterval.IsZero then
    begin
    ADate := Result;
    while ADate < ANow do
      ADate := ADate + ByInterval;
    ANextIntervalTime := ADate;
    end;
  end;
  function MinNext: TDateTime;
  begin
  Result := ANextWeekDay;
  if SameValue(Result, 0) then
    Result := ANextMonthDay
  else if Result > ANextMonthDay then
    Result := ANextMonthDay;
  if SameValue(Result, 0) then
    Result := ANextIntervalTime
  else if Result > ANextIntervalTime then
    Result := ANextIntervalTime;
  end;
  procedure CalcTime;
  var
    H, M, S: Integer;
    I: Integer;
    ATime: TDateTime;
  begin
  H := -1;
  if ByHours <> 0 then
    begin
    for I := HourOf(ANow) to 23 do
      begin
      if IsByHours[I] then
        begin
        H := I;
        Break;
        end;
      end;
    end;
  M := -1;
  for I := 0 to 59 do
    begin
    if IsByMinutes[I] then
      begin
      M := I;
      Break;
      end;
    end;
  S := -1;
  for I := 0 to 59 do
    begin
    if IsBySeconds[I] then
      begin
      S := I;
      Break;
      end;
    end;
  if (H = -1) and (M = -1) and (S = -1) then
    ANextTime := 0
  else
    begin
    if H = -1 then
      H := HourOf(FirstRunAt);
    if M = -1 then
      M := MinuteOf(FirstRunAt);
    if S = -1 then
      S := SecondOf(FirstRunAt);
    ATime := EncodeTime(H, M, S, 0);
    // Todo:计算下一时刻
    while ATime < ANow do
      begin
      if S <> -1 then
        begin
        if M <> -1 then
          begin
          if H <> -1 then
            ATime := IncDay(ATime)
          else
            ATime := IncHour(ATime);
          end
        else
          ATime := IncHour(ATime);
        end
      else
        ATime := IncMinute(ATime);
      end;
    end;
  end;

begin
Result := FirstRunAt;
ANow := Now;
if FirstRunAt > ANow then
  Result := FirstRunAt
else
  begin
  CalcWeeksNext;
  CalcMonthNext;
  CalcInterval;
  Result := MinNext;
  end;
end;

function TJobFireTime.GetByWeekDays(AIndex: Byte): Boolean;
begin
assert((AIndex > 0) and (AIndex <= 7));
Result := (ByDays and (1 shl (AIndex - 1))) <> 0;
end;

procedure TJobFireTime.SetByDays(AIndex: Byte; const Value: Boolean);
begin
assert((AIndex > 0) and (AIndex <= 31));
if Value then
  ByDays := ByDays or (1 shl (AIndex - 1))
else
  ByDays := ByDays and (not(1 shl (AIndex - 1)));
end;

procedure TJobFireTime.SetByHours(AIndex: Byte; const Value: Boolean);
begin
assert(AIndex <= 23);
if Value then
  ByDays := ByHours or (1 shl AIndex)
else
  ByDays := ByHours and (not(1 shl AIndex));
end;

procedure TJobFireTime.SetByMinutes(AIndex: Byte; const Value: Boolean);
begin
assert(AIndex <= 59);
if Value then
  ByMinutes := ByMinutes or (1 shl AIndex)
else
  ByMinutes := ByMinutes and (not(1 shl AIndex));
end;

procedure TJobFireTime.SetByMonthes(AIndex: Byte; const Value: Boolean);
begin
assert((AIndex > 0) and (AIndex <= 12));
if Value then
  ByMonthes := ByMonthes or (1 shl (AIndex - 1))
else
  ByMonthes := ByMonthes and (not(1 shl (AIndex - 1)));
end;

procedure TJobFireTime.SetBySeconds(AIndex: Byte; const Value: Boolean);
begin
assert(AIndex <= 59);
if Value then
  BySeconds := BySeconds or (1 shl AIndex)
else
  BySeconds := BySeconds and (not(1 shl AIndex));
end;

procedure TJobFireTime.SetByWeeks(AIndex: Byte; const Value: Boolean);
begin
assert((AIndex > 0) and (AIndex <= 54));
if Value then
  ByWeeks := ByWeeks or (1 shl (AIndex - 1))
else
  ByWeeks := ByWeeks and (not(1 shl (AIndex - 1)));
end;

procedure TJobFireTime.SetByWeekDays(AIndex: Byte; const Value: Boolean);
begin
assert((AIndex > 0) and (AIndex <= 7));
if Value then
  ByWeekDays := ByWeekDays or (1 shl (AIndex - 1))
else
  ByWeekDays := ByWeekDays and (not(1 shl (AIndex - 1)));
end;

procedure TfrmTaskProps.Button1Click(Sender: TObject);
begin
if odExecute.Execute then
  edtShellCmd.Text := odExecute.FileName;
end;

procedure TfrmTaskProps.Button2Click(Sender: TObject);
begin
Close;
ModalResult := mrOk;
end;

procedure TfrmTaskProps.Button3Click(Sender: TObject);
begin
Close;
end;

function TfrmTaskProps.CreatePlan: TQPlanMask;
  function FormatPlan(AList: TCheckListBox; ACheck: TCheckBox;
    AInterval: TSpinEdit; ADelta: Integer = 1): String;
  var
    I, C: Integer;
  begin
    if ACheck.Checked then
      Result := '*'
    else
    begin
      Result := '';
      C := 0;
      for I := 0 to AList.Count - 1 do
      begin
        if AList.Checked[I] then
        begin
          Result := Result + IntToStr(I + ADelta) + ',';
          Inc(C);
        end;
      end;
      if C > 0 then
        Result := Copy(Result, 1, Length(Result) - 1);
      if C = 1 then
      begin
        if AInterval.Value <> 0 then
          Result := Result + '/' + IntToStr(AInterval.Value);
      end;
    end;
  end;

begin
  Result.AsString := FormatPlan(clbMinuteList, chkAnyMinute, seMinuteInterval,
    0) + ' ' + FormatPlan(clbHourList, chkAnyHour, seHourInterval, 0) + ' ' +
    FormatPlan(clbDayList, chkAnyDay, seDayInterval) + ' ' +
    FormatPlan(clbMonthList, chkAnyMonth, seMonthInterval) + ' ' +
    FormatPlan(clbWeekDayList, chkAnyWeekDay, seWeekDayInterval) +
    ' ';
end;

procedure TfrmTaskProps.EnableChildren(AParent: TWinControl; AEnabled: Boolean);
var
  I: Integer;
  ACtrl: TControl;
begin
for I := 0 to AParent.ControlCount - 1 do
  begin
  ACtrl := AParent.Controls[I];
  if ACtrl is TWinControl then
    EnableChildren(ACtrl as TWinControl, AEnabled)
  else
    ACtrl.Enabled := AEnabled;
  end;
AParent.Enabled := AEnabled;
end;

procedure TfrmTaskProps.FormCreate(Sender: TObject);
var
  ANow: TDateTime;
begin
inherited;
ANow := Now;
dtFirstDate.Date := Trunc(ANow);
dtFirstTime.Time := ANow - Trunc(ANow);
end;

function TfrmTaskProps.GetActivePlan: TJobPlan;
var
  I: Integer;
  S: String;
  function CalcFirstRunTime: TDateTime;
  var
    Y, M, D, H, N, S, MS: Word;
  begin
  DecodeDate(dtFirstDate.Date, Y, M, D);
  DecodeTime(dtFirstTime.Time, H, N, S, MS);
  Result := EncodeDateTime(Y, M, D, H, N, S, MS);
  end;

begin
FPlan.Name := edtTaskName.Text;
// 信号触发列表
SetLength(S, 0);
for I := 0 to clbSignals.Items.Count - 1 do
  begin
  if clbSignals.Checked[I] then
    S := S + clbSignals.Items[I] + ',';
  end;
if Length(S) > 0 then
  FPlan.BySignals := S
else
  FPlan.BySignals := '';
if cbxRepeat.ItemIndex = 0 then // 只运行一次
  FillChar(FPlan.ByTime, SizeOf(TJobFireTime), 0)
else
  begin
  // 时间间隔
  FPlan.ByTime.ByInterval.Encode(seYearInterval.Value, seMonthInterval.Value,
    seDayInterval.Value, seHourInterval.Value, seMinuteInterval.Value,
    seSecondInterval.Value, seMilliSecond.Value);
  // 特定月份
  for I := 1 to 12 do
    FPlan.ByTime.IsByMonthes[I] := clbByMonths.Checked[I - 1];
  // 特定周次
  for I := 1 to 54 do
    FPlan.ByTime.IsByWeeks[I] := clbByWeeks.Checked[I - 1];
  // 周内特定日期
  for I := 1 to 7 do
    FPlan.ByTime.IsByWeekDays[I] := clbByWeekDays.Checked[I - 1];
  // 月内特定日期
  for I := 1 to 31 do
    FPlan.ByTime.IsByDays[I] := clbByMonthDays.Checked[I - 1];
  // 特定小时
  for I := 0 to 23 do
    FPlan.ByTime.IsByHours[I] := clbByHours.Checked[I];
  // 特定分钟数
  for I := 0 to 59 do
    FPlan.ByTime.IsByMinutes[I] := clbByMinutes.Checked[I];
  // 特定秒数
  for I := 0 to 59 do
    FPlan.ByTime.IsBySeconds[I] := clbBySeconds.Checked[I];
  end;
FPlan.ByTime.FirstRunAt := CalcFirstRunTime;
if rbIsMessageTask.Checked then
  begin
  FPlan.TaskType := jttMessage;
  FPlan.TaskText := mmHintMsg.Text;
  end
else
  begin
  FPlan.TaskType := jttShell;
  FPlan.TaskText := edtShellCmd.Text;
  end;
Result := FPlan;
end;

end.
