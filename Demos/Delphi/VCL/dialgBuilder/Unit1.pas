unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, System.Types, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  qdialog_builder, Vcl.ComCtrls, Vcl.Samples.Gauges;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Label1: TLabel;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    GroupBox1: TGroupBox;
    RadioGroup1: TRadioGroup;
    Button13: TButton;
    Button14: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
  private
    { Private declarations }
    FBuilder: IDialogBuilder;
    FEditor: TEdit;
    procedure DoDialogResult(ABuilder: IDialogBuilder);
    procedure ValidBuilder;
    procedure LoadUser32Icon(APicture: TPicture; AResId: Integer);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button10Click(Sender: TObject);
begin
  CustomDialog('自动关闭窗口', '这个窗口将在5秒后关闭', 'AFlags 参数低16位为倒计时秒数', ['立即关闭'], diInformation, CDF_DISPLAY_REMAIN_TIME or 5);
end;

procedure TForm1.Button11Click(Sender: TObject);
var
  ABuilder: IDialogBuilder;
  AHint: TLabel;
  AProgress: TProgressBar;
  I: Integer;
  T: Cardinal;
begin
  ABuilder := NewDialog('进度窗口');
  ABuilder.ItemSpace := 10;
  ABuilder.AutoSize := True;
  ABuilder.Dialog.Padding.SetBounds(5, 5, 5, 5);
  AHint := TLabel(ABuilder.AddControl(TLabel).Control);
  AHint.Caption := '正在处理，已完成0%...';
  AHint.AlignWithMargins := True;
  AProgress := TProgressBar(ABuilder.AddControl(TProgressBar).Control);
  AProgress.AlignWithMargins := True;
  with ABuilder.AddContainer(amHorizCenter) do
  begin
    Height := 24;
    with TButton(AddControl(TButton).Control) do
    begin
      Caption := '取消';
      ModalResult := mrCancel;
    end;
  end;
  ABuilder.CanClose := False;
  ABuilder.Realign;
  ABuilder.Popup(Button11);
  for I := 0 to 100 do
  begin
    AHint.Caption := '正在处理，已完成' + IntToStr(I) + '%';
    AProgress.Position := I;
    T := GetTickCount;
    while GetTickCount - T < 100 do
    begin
      Application.ProcessMessages;
      if ABuilder.Dialog.ModalResult = mrCancel then
        Break;
    end;
  end;
  ABuilder._Release;
end;

procedure TForm1.Button12Click(Sender: TObject);
var
  ABuilder: IDialogBuilder;
  I, T, ADelta: Integer;
  AGauges: array [0 .. 5] of TGauge;
  J: Integer;
begin
  ABuilder := NewDialog;
  ABuilder.AutoSize := True;
  ABuilder.Dialog.Padding.SetBounds(5, 5, 5, 5);
  for I := 0 to 5 do
  begin
    with ABuilder.AddContainer(amHorizLeft) do
    begin
      AutoSize := True;
      with TLabel(AddControl(TLabel).Control) do
      begin
        Caption := '进度 ' + IntToStr(I);
        Layout := tlCenter;
      end;
      AGauges[I] := TGauge(AddControl(TGauge).Control);
      with AGauges[I] do
      begin
        Progress := random(100);
        Height := 10;
      end;
    end;
  end;
  ABuilder.Popup(Button12);
  T := Button12.Top;
  for I := 0 to 99 do
  begin
    Application.ProcessMessages;
    Sleep(10);
    if (I mod 10) = 0 then
    begin
      for J := 0 to 5 do
        AGauges[J].Progress := random(100);
    end;
    if I < 50 then
      Button12.Top := Button12.Top + 1
    else
      Button12.Top := Button12.Top - 1;
  end;
end;

procedure TForm1.Button13Click(Sender: TObject);
begin
  ValidBuilder;
  FBuilder.PopupPosition := TQDialogPopupPosition(RadioGroup1.ItemIndex);
  FBuilder.Popup(GroupBox1);
end;

procedure TForm1.Button14Click(Sender: TObject);
begin
  ValidBuilder;
  FBuilder.PopupPosition := TQDialogPopupPosition(RadioGroup1.ItemIndex);
  FBuilder.Popup(nil);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ValidBuilder;
  FBuilder.Popup(Button1);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  ValidBuilder;
  FBuilder.ShowModal;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  ABuilder: IDialogBuilder;
  I: Integer;
  ACtrl: IControlDialogItem;
begin
  // 此示例演示通过PropText定义属性及用户定义大小单选列表处理的问题
  ABuilder := NewDialog('单选项目');
  ABuilder.Dialog.Padding.SetBounds(5, 5, 5, 5);
  ABuilder.PropText := '{"Width":300,"Height":150}';
  ABuilder.AddControl(TRadioButton, '{"Caption":"这个是项目一","AlignWithMargins":True,"Margins":{"Left":10},"Height":30}');
  ABuilder.AddControl(TRadioButton, '{"Caption":"这个是项目二","AlignWithMargins":True,"Margins":{"Left":10},"Height":30}');
  with ABuilder.AddContainer(amHorizRight) do
  begin
    Height := 32;
    AddControl(TButton, '{"Caption":"确定","ModalResult":1}');
    AddControl(TButton, '{"Caption":"取消","ModalResult":2}');
  end;
  ABuilder.ShowModal;
  if ABuilder.ModalResult = 1 then
  begin
    for I := 0 to ABuilder.Count - 1 do
    begin
      if Supports(ABuilder[I], IControlDialogItem, ACtrl) then
      begin
        if (ACtrl.Control is TRadioButton) and (TRadioButton(ACtrl.Control).Checked) then
        begin
          ShowMessage(TRadioButton(ACtrl.Control).Caption + ' 被选择');
          Break;
        end;
      end;
    end;
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  ABuilder: IDialogBuilder;
begin
  ABuilder := NewDialog('下拉示例');
  ABuilder.ItemSpace := 10;
  ABuilder.AutoSize := True;
  ABuilder.Dialog.Padding.SetBounds(5, 5, 5, 5);
  with TLabel(ABuilder.AddControl(TLabel).Control) do
  begin
    Caption := '起床换衣服了！！！';
    Font.Color := clWhite;
    Font.Name := '微软雅黑';
    Font.Size := 12;
    Alignment := taCenter;
    Transparent := False;
    Color := clGray;
  end;
  with TLabel(ABuilder.AddControl(TLabel).Control) do
  begin
    Caption := '这是一个美丽的早晨，你还在睡懒觉吗？'#13#10 + //
      '怎么可以这样！！！'#13#10 + //
      '起床换衣服了~~~~';
    WordWrap := True;
    Font.Color := clGray;
    AlignWithMargins := True;
  end;
  ABuilder.Popup(Button4);
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  ABuilder: IDialogBuilder;
  AGroup: IDialogItemGroup;
  I: Integer;
begin
  // 本示例演示分组的用法
  ABuilder := NewDialog('分组示例');
  ABuilder.AutoSize := True;
  ABuilder.Dialog.Padding.SetBounds(5, 5, 5, 5);
  // 添加第一组RadioButton
  with ABuilder.AddContainer(amVertTop) do
  begin
    ItemSpace := 10;
    AutoSize := True;
    AddControl(TLabel, '{"Caption":"第一组","Color":"clGray","Transparent":False,"Font":{"Color":"clWhite","Size":11}}');
    AddControl(TRadioButton, '{"Caption":"这个是第一组的第一项","AlignWithMargins":True,"Margins":{"Left":10},"Height":30}').GroupName
      := 'Group1';
    AddControl(TRadioButton, '{"Caption":"这个是第一组的第二项","AlignWithMargins":True,"Margins":{"Left":10},"Height":30}').GroupName
      := 'Group1';
  end;
  // 添加第二组RadioButton
  with ABuilder.AddContainer(amVertTop) do
  begin
    AutoSize := True;
    ItemSpace := 10;
    AddControl(TLabel, '{"Caption":"第二组","Color":"clGray","Transparent":False,"Font":{"Color":"clWhite","Size":11}}');
    AddControl(TRadioButton, '{"Caption":"这个是第二组的第一项","AlignWithMargins":True,"Margins":{"Left":10},"Height":30}').GroupName
      := 'Group2';
    AddControl(TRadioButton, '{"Caption":"这个是第二组的第二项","AlignWithMargins":True,"Margins":{"Left":10},"Height":30}').GroupName
      := 'Group2';
  end;
  with ABuilder.AddContainer(amHorizRight) do
  begin
    AutoSize := True;
    Height := 32;
    AddControl(TButton, '{"Caption":"确定","ModalResult":1}');
    AddControl(TButton, '{"Caption":"取消","ModalResult":2}');
  end;
  ABuilder.ShowModal;
  if ABuilder.ModalResult = 1 then
  begin
    AGroup := ABuilder.GroupByName('Group1');
    for I := 0 to AGroup.Count - 1 do
    begin
      with TRadioButton((AGroup[I] as IControlDialogItem).Control) do
      begin
        if Checked then
          ShowMessage('分组一选择的项目是：' + Caption);
      end;
    end;
  end;
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  pt: TPoint;
begin
  ValidBuilder;
  pt := ClientToScreen(Point(0, 0));
  FBuilder.Popup(pt);
end;

procedure TForm1.Button7Click(Sender: TObject);
var
  ABuilder: IDialogBuilder;
begin
  ABuilder := NewDialog('警告');
  ABuilder.AutoSize := True;
  ABuilder.ItemSpace := 5;
  ABuilder.Dialog.Padding.SetBounds(5, 5, 5, 5);
  with ABuilder.AddContainer(amHorizLeft) do
  begin
    AutoSize := True;
    with TImage(AddControl(TImage).Control) do
    begin
      AlignWithMargins := True;
      AutoSize := True;
      LoadUser32Icon(Picture, 101);
    end;
    with TLabel(AddControl(TLabel).Control) do
    begin
      AlignWithMargins := True;
      Caption := '您真的想删除当前记录吗？'#13#10'是 -  删除'#13#10'否 - 取消操作';
    end;
  end;
  with ABuilder.AddContainer(amHorizRight) do
  begin
    AutoSize := True;
    with TButton(AddControl(TButton).Control) do
    begin
      Caption := '是(&Y)';
      ModalResult := mrYes;
    end;
    with TButton(AddControl(TButton).Control) do
    begin
      Caption := '否(&N)';
      ModalResult := mrNo;
    end;
  end;
  ABuilder.ShowModal;
  if ABuilder.ModalResult = mrYes then
    ShowMessage('删除记录')
  else
    ShowMessage('删除操作已取消');
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  case CustomDialog('定制提醒', '您定制的大衣已经被别人抢走了！', '选择接下来您要进行的操作：'#13#10#13#10'抢回来 - 尝试抢回，也可能失败'#13#10'放弃 - 放弃吧，总可以，至少还可以相信命运',
    ['抢回来', '放弃'], diWarning) of
    0:
      ShowMessage('勇士啊，可对方已经把大衣烧掉了');
    1:
      ShowMessage('懦夫呀，你怎么可以这样就放弃');
    -1:
      ShowMessage('中二啊，你纠结中机会已经飞走了');
  end;
end;

procedure TForm1.Button9Click(Sender: TObject);
begin
  CustomDialog('定制图标', '这个图标来自于shell32.dll', '', ['确定'], 48, 'shell32', TSize.Create(64, 64));
end;

procedure TForm1.DoDialogResult(ABuilder: IDialogBuilder);
begin
  Label1.Caption := '编辑结果：' + FEditor.Text;
  if ABuilder=FBuilder then
    FBuilder:=nil;
end;

procedure TForm1.LoadUser32Icon(APicture: TPicture; AResId: Integer);
var
  AIcon: TIcon;
begin
  AIcon := TIcon.Create;
  try
    AIcon.LoadFromResourceID(GetModuleHandle(user32), AResId);
    APicture.Assign(AIcon);
  finally
    FreeAndNil(AIcon);
  end;
end;

procedure TForm1.ValidBuilder;
begin
  if not Assigned(FBuilder) then
  begin
    FBuilder := NewDialog('DialogBuilder 示例');
    FBuilder.AutoSize := True;
    FBuilder.Dialog.Padding.SetBounds(5, 5, 5, 5);
    with FBuilder.AddContainer(amHorizLeft) do
    begin
      Height := 32;
      AutoSize := True;
      // 示例：直接访问控件的属性
      with TImage(AddControl(TImage).Control) do
      begin
        AutoSize := True;
        LoadUser32Icon(Picture, 101);
      end;
      with TLabel(AddControl(TLabel).Control) do
      begin
        Font.Color := clRed;
        Font.Size := 12;
        Font.Name := '微软雅黑';
        Layout := tlCenter;
        Caption := '请认真填写并核对，以避免出错。';
      end;
    end;
    // 示例：使用基于 JSON 的属性定义
    FBuilder.AddControl(TLabel, '{"AlignWithMargins":True,"Caption":"请输入您的姓名.","Font":{"Color":"clGray"}}');
    with FBuilder.AddControl(TEdit, '{"AlignWithMargins":true}') do
    begin
      FEditor := TEdit(Control);
      GroupName := 'Edit';
    end;
    with FBuilder.AddContainer(amHorizRight) do
    begin
      Height := 32;
      AddControl(TButton,
        procedure(ASender: TObject)
        var
          ACtrl: IControlDialogItem;
        begin
          if Supports(ASender, IControlDialogItem, ACtrl) then
          begin
            ACtrl.Builder.Dialog.Close;
            ShowMessage('用户输入的值为:' + FEditor.Text);
          end;
        end, '{"Caption":"确定","ModalResult":1}');
      AddControl(TButton,
        procedure(ASender: TObject)
        var
          ACtrl: IControlDialogItem;
        begin
          if Supports(ASender, IControlDialogItem, ACtrl) then
            ACtrl.Builder.Dialog.Close;
        end, '{"Caption":"取消","ModalResult":2}');
    end;
    FBuilder.OnResult := DoDialogResult;
  end;
end;

end.
