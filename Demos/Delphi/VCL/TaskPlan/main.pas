unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, QString, QWorker, QTimeTypes, taskprop,
  Vcl.StdCtrls, Vcl.ExtCtrls;

{ 本示例演示使用QWorker进行计划任务作业，主要演示以下内容使用：
  1、At和Delay函数的使用
  2、TQInterval时间间隔类型的使用
  3、使用Json来存贮作业数据
}
type
  TForm5 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    lbxPlans: TListBox;
    btnAddNew: TButton;
    Panel3: TPanel;
    Panel4: TPanel;
    mmLog: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure btnAddNewClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FPlans: TJobPlans;
    procedure SavePlans;
    procedure AddPlanJob(const AIndex: Integer);
    procedure DoPlanJob(AJob: PQJob);
  public
    { Public declarations }
  end;

var
  Form5: TForm5;

implementation

uses QJson, Math;
{$R *.dfm}

procedure TForm5.AddPlanJob(const AIndex: Integer);
var
  ANextTime: TDateTime;
begin
ANextTime := FPlans[AIndex].ByTime.NextTime;
if not SameValue(ANextTime, 0) then // 作业首次执行时间已经过了，检查第二次执行
  Workers.At(DoPlanJob, ANextTime, 0, Pointer(AIndex), True);
end;

procedure TForm5.btnAddNewClick(Sender: TObject);
var
  F: TfrmTaskProps;
begin
F := TfrmTaskProps.Create(Application);
F.DropDown(btnAddNew, True, 0);
if F.ModalResult = mrOk then
  begin
  SetLength(FPlans, Length(FPlans) + 1);
  FPlans[Length(FPlans) - 1] := F.ActivePlan;
  lbxPlans.Items.Add(F.ActivePlan.Name);
  AddPlanJob(Length(FPlans) - 1);
  SavePlans;
  end;
FreeObject(F);
end;

procedure TForm5.Button1Click(Sender: TObject);
begin
Workers.Plan(
  procedure (AJob:PQJob)
  begin
  ShowMessage('OK '+AJob.ExtData.AsPlan.Plan.AsString);
  end,'3 13 * * 1 测试作业',nil,true);
end;

procedure TForm5.Button4Click(Sender: TObject);
begin
SetLength(FPlans, 0);
lbxPlans.Clear;
Workers.Clear(DoPlanJob, INVALID_JOB_DATA);
end;

procedure TForm5.DoPlanJob(AJob: PQJob);
var
  AIndex: Integer;
begin
AIndex := IntPtr(AJob.Data);
if Length(FPlans) > AIndex then
  begin
  if FPlans[AIndex].TaskType = jttMessage then
    mmLog.Lines.Add(FormatDateTime('hh:nn:ss.zzz', Now) + ' 作业 ' +
      IntToStr(AIndex) + '，名为 ' + FPlans[AIndex].Name + ' 已经执行,消息内容:' +
      FPlans[AIndex].TaskText)
  else
    begin
    mmLog.Lines.Add(FormatDateTime('hh:nn:ss.zzz', Now) + ' 作业 ' +
      IntToStr(AIndex) + '，名为 ' + FPlans[AIndex].Name + ' 请求执行命令行（不支持，已忽略）: ' +
      FPlans[AIndex].TaskText)
    end;
  AddPlanJob(AIndex);
  end
else
  mmLog.Lines.Add('作业 ' + IntToStr(AIndex) + ' 已经不存在。');
end;

procedure TForm5.FormCreate(Sender: TObject);
var
  AJson, APlans: TQJson;
  I: Integer;
  procedure LoadPlan(ADefine: TQJson; var APlan: TJobPlan);
  var
    AByTime: TQJson;
  begin
  APlan.Name := ADefine.ValueByName('Name', '');
  APlan.BySignals := ADefine.ValueByName('BySignals', '');
  APlan.TaskType := TJobTaskType(ADefine.ItemByName('Type').AsInteger);
  APlan.TaskText := ADefine.ValueByName('Text', '');
  AByTime := ADefine.ItemByName('ByTime');
  if Assigned(AByTime) then
    begin
    APlan.ByTime.FirstRunAt := AByTime.ItemByName('FirstRunAt').AsDateTime;
    APlan.ByTime.ByWeeks := AByTime.ItemByName('ByWeeks').AsInt64;
    APlan.ByTime.ByMinutes := AByTime.ItemByName('ByMinutes').AsInt64;
    APlan.ByTime.BySeconds := AByTime.ItemByName('BySeconds').AsInt64;
    APlan.ByTime.ByHours := AByTime.ItemByName('ByHours').AsInt64;
    APlan.ByTime.ByDays := AByTime.ItemByName('ByDays').AsInt64;
    APlan.ByTime.ByMonthes := AByTime.ItemByName('ByMonthes').AsInt64;
    APlan.ByTime.ByWeekDays := AByTime.ItemByName('ByWeekDays').AsInt64;
    APlan.ByTime.ByInterval.AsString :=
      AByTime.ItemByName('ByInterval').AsString;
    end;
  end;

begin
if FileExists('plans.json') then
  begin
  AJson := TQJson.Create;
  lbxPlans.Items.BeginUpdate;
  try
    AJson.LoadFromFile('plans.json');
    if AJson.ValueByName('Ident', '') = 'Job Plans' then
      begin
      APlans := AJson.ItemByName('Plans');
      SetLength(FPlans, APlans.Count);
      for I := 0 to APlans.Count - 1 do
        begin
        LoadPlan(APlans[I], FPlans[I]);
        lbxPlans.Items.Add(FPlans[I].Name);
        AddPlanJob(I);
        end;
      end;
  finally
    lbxPlans.Items.EndUpdate;
    FreeObject(AJson);
  end;
  end;
end;

procedure TForm5.SavePlans;
var
  AJson, APlans: TQJson;
  I: Integer;
  procedure SavePlan(ADest: TQJson; APlan: TJobPlan);
  begin
  ADest.Add('Name').AsString := APlan.Name;
  ADest.Add('BySingals').AsString := APlan.BySignals;
  ADest.Add('Type').AsInteger := Integer(APlan.TaskType);
  ADest.Add('Text').AsString := APlan.TaskText;
  ADest := ADest.Add('ByTime');
  ADest.Add('FirstRunAt').AsDateTime := APlan.ByTime.FirstRunAt;
  ADest.Add('ByWeeks').AsInt64 := APlan.ByTime.ByWeeks;
  ADest.Add('ByMinutes').AsInt64 := APlan.ByTime.ByMinutes;
  ADest.Add('BySeconds').AsInt64 := APlan.ByTime.BySeconds;
  ADest.Add('ByHours').AsInt64 := APlan.ByTime.ByHours;
  ADest.Add('ByDays').AsInt64 := APlan.ByTime.ByDays;
  ADest.Add('ByMonthes').AsInt64 := APlan.ByTime.ByMonthes;
  ADest.Add('ByWeekDays').AsInt64 := APlan.ByTime.ByWeekDays;
  ADest.Add('ByInterval').AsString := APlan.ByTime.ByInterval.AsString;
  end;

begin
AJson := TQJson.Create;
try
  AJson.Add('Ident').AsString := 'Job Plans';
  APlans := AJson.AddArray('Plans');
  for I := 0 to High(FPlans) do
    SavePlan(APlans.Add, FPlans[I]);
  AJson.SaveToFile('plans.json', teUtf8, True);
finally
  FreeObject(AJson);
end;
end;

end.
