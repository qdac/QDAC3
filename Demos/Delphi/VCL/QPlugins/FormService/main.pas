unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, qplugins_vcl_formsvc, qplugins_loader_lib,
  qstring, qplugins_base, qplugins, qplugins_params, qplugins_vcl_Messages,
  qplugins_formsvc,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    ComboBox1: TComboBox;
    Button8: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Button8Click(Sender: TObject);
  private
    { Private declarations }
    FMultiForms: array of IQFormService;
    procedure DoDockChildFree(AForm: IQFormService);
    procedure DockPage(AFormService: IQFormService;
      AHoldNeeded: Boolean = False);
    procedure DoPageDoubleClick(ASender: TObject);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses unit2;
{$R *.dfm}

type
  THackedPageControl = class(TPageControl)
  public
    property OnDblClick;
  end;

procedure TForm1.Button1Click(Sender: TObject);
var
  I: Integer;
  AParent: IQServices;
  AFormService: IQFormService;
begin
  if Supports(PluginsManager.ByPath('/Services/Docks/Forms'), IQServices,
    AParent) then
  begin
    for I := 0 to AParent.Count - 1 do
    begin
      if Supports(AParent[I], IQFormService, AFormService) then
      begin
        if not AFormService.IsMultiInstance then
        begin
          DockPage(AFormService);
        end;
      end;
    end;
    if PageControl1.PageCount > 0 then
      PageControl1.ActivePageIndex := 0;
  end;
  // 创建的是单实例的，所以不应重复创建
  Button1.Enabled := False;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  AFormService: IQFormService;
begin
  if Supports(PluginsManager.ByPath('/Services/Docks/Forms/DLL_MutiInstance'),
    IQFormService, AFormService) then
  begin
    SetLength(FMultiForms, Length(FMultiForms) + 1);
    FMultiForms[High(FMultiForms)] := AFormService;
    AFormService.SetBounds(Random(100), Random(100), 300, 300);
    AFormService.Show;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  AFormService: IQFormService;
  AParam: IQParam;
begin
  if Supports(PluginsManager.ByPath('/Services/Docks/Forms/DLL_MutiInstance'),
    IQFormService, AFormService) then
  begin
    with (AFormService as IQService) do
    begin
      Attrs.Add('Tag', ptInt32).AsInteger := 100;
      AParam := Creator.Attrs.ByName('ParentTag');
      if not Assigned(AParam) then
        AParam := Creator.Attrs.Add('ParentTag', ptInt32);
      AParam.AsInteger := Random(100);
    end;
    ShowMessage(ServiceSource(AFormService as IQService));
    AFormService.ShowModal(nil, nil);
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  AFormService: IQFormService;
  ACustomAction: IQCustomAction;
  AParams, AResult: IQParams;
begin
  if Supports(PluginsManager.ByPath('/Services/Docks/Forms/CustomAction'),
    IQFormService, AFormService) then
  begin
    ACustomAction := nil;
    if AFormService.FormNeeded and Supports(AFormService, IQCustomAction,
      ACustomAction) then
    begin
      AParams := TQParams.Create;
      AParams.Add('@', ptUnicodeString).AsString.Value := 'SetMessage';
      AParams.Add('Caption', ptUnicodeString).AsString.Value := '点我看看';
      AParams.Add('Message', ptUnicodeString).AsString.Value := '这是宿主程序传来的值哟!';
      ACustomAction.DoAction(AParams);
    end;
    AFormService.ShowModal(nil);
    AParams.Clear;
    if Assigned(ACustomAction) then
    begin
      AParams.Add('@', ptUnicodeString).AsString.Value := 'GetMessage';
      AResult := TQParams.Create;
      ACustomAction.DoAction(AParams, AResult);
      ShowMessage(AResult.AsString.Value);
    end;
  end;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  AFormService: IQFormService;
begin
  if Supports(PluginsManager.ByPath('/Services/Docks/Forms/FMXForm'),
    IQFormService, AFormService) then
  begin
    AFormService.ShowModal(nil);
  end;

end;

procedure TForm1.Button6Click(Sender: TObject);
var
  AFormService: IQService;
begin
  AFormService := PluginsManager.ByPath
    ('/Services/Docks/Forms/DLL_MutiInstance');
  if Assigned(AFormService) then
    AFormService.Execute(NewParams(['ShowModal']), nil);
end;

procedure TForm1.Button7Click(Sender: TObject);
var
  AFormService: IQFormService;
begin
  if GetService('/Services/Docks/Forms/DLL_MutiInstance', IQFormService,
    AFormService) then
  begin
    DockPage(AFormService, True);
  end;
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  ShowMessage(IntToHex(IntPtr(InstanceOf(PluginsManager) as TQServices), 8));
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
var
  I: Integer;
  AFormService: IQFormService;
begin
  for I := 0 to PageControl1.PageCount - 1 do
  begin
    AFormService := IQFormService(PageControl1.Pages[I].Tag);
    AFormService.Align := TFormAlign(ComboBox1.ItemIndex);
  end;
end;

procedure TForm1.DockPage(AFormService: IQFormService; AHoldNeeded: Boolean);
var
  APage: TTabSheet;
  AEvents: TQFormEvents;
begin
  APage := TTabSheet.Create(PageControl1);
  APage.PageControl := PageControl1;
  APage.Caption := (AFormService as IQService).Name;
  APage.Tag := IntPtr(AFormService);
  AFormService.DockTo(APage.Handle, TFormAlign(ComboBox1.ItemIndex));
  FillChar(AEvents, SizeOf(AEvents), 0);
  AEvents.OnFree := DoDockChildFree;
  AFormService.HookEvents(AEvents);
  if AHoldNeeded then
    HoldByComponent(APage, AFormService);
end;

procedure TForm1.DoDockChildFree(AForm: IQFormService);
var
  I: Integer;
begin
  for I := 0 to PageControl1.PageCount - 1 do
  begin
    if PageControl1.Pages[I].Tag = IntPtr(AForm) then
    begin
      AForm.UnhookEvents;
      FreeObject(PageControl1.Pages[I]);
      Break;
    end;
  end;
end;

procedure TForm1.DoPageDoubleClick(ASender: TObject);
var
  AService: IQFormService;
begin
  // 双击关闭当前窗体
  if PageControl1.PageCount > 0 then
  begin
    AService := IQFormService(PageControl1.ActivePage.Tag);
    AService.Close;
    AService.UnhookEvents;
    FreeObject(PageControl1.ActivePage);
    if PageControl1.PageCount > 0 then
      PageControl1.ActivePageIndex := 0;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  APath: String;
begin
  ReportMemoryLeaksOnShutdown := True;
  APath := ExtractFilePath(Application.ExeName);
  PluginsManager.Loaders.Add(TQDLLLoader.Create(APath, '.dll'));
  PluginsManager.Loaders.Add(TQBPLLoader.Create(APath, '.bpl'));
  PluginsManager.Start;
  PageControl1.ControlStyle := PageControl1.ControlStyle +
    [csClickEvents, csDoubleClicks];
  THackedPageControl(PageControl1).OnDblClick := DoPageDoubleClick;
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  I: Integer;
  AFormService: IQFormService;
begin
  for I := 0 to PageControl1.PageCount - 1 do
  begin
    AFormService := IQFormService(PageControl1.Pages[I].Tag);
    AFormService.UnhookEvents;
  end;
end;

end.
