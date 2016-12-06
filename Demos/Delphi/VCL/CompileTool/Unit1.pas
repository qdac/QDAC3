unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, VirtualTrees, QJson, QWorker, Menus;
type
  TForm_Main = class(TForm)
    PC: TPageControl;
    TabReadme: TTabSheet;
    TabProject: TTabSheet;
    TabLog: TTabSheet;
    P3: TPanel;
    M: TMemo;
    BtnRegType: TButton;
    P2: TPanel;
    Log: TMemo;
    BtnSaveLog: TButton;
    BtnClearLog: TButton;
    P1: TPanel;
    BtnAdd: TButton;
    BtnDel: TButton;
    BtnOpen: TButton;
    BtnSave: TButton;
    BtnSaveAs: TButton;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Splitter1: TSplitter;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    VDT: TVirtualDrawTree;
    CompilerPC: TPageControl;
    TabNew: TTabSheet;
    ProjectSet: TPageControl;
    TabOutPath: TTabSheet;
    TabProjectSet: TTabSheet;
    BtnUp: TButton;
    BtnDown: TButton;
    OD: TOpenDialog;
    OutFinalDir: TLabeledEdit;
    OutIncludeDir: TLabeledEdit;
    OutLibDir: TLabeledEdit;
    Button3: TButton;
    Button2: TButton;
    Button1: TButton;
    ExtraAlias: TLabeledEdit;
    ExtraNameSpaceSearch: TLabeledEdit;
    ExtraIncludeDir: TLabeledEdit;
    ExtraLibDir: TLabeledEdit;
    ExtraSearchDir: TLabeledEdit;
    Button6: TButton;
    Button4: TButton;
    Button5: TButton;
    ExtraBuildAllUnits: TCheckBox;
    ExtraNameSpaces: TCheckBox;
    CustomRootDir: TLabeledEdit;
    Button7: TButton;
    AddIDE: TButton;
    DpkAddIDEVersion: TCheckBox;
    BuildRelease: TRadioButton;
    BuildDebug: TRadioButton;
    SD: TSaveDialog;
    PM: TPopupMenu;
    PM_All: TMenuItem;
    PM_Row: TMenuItem;
    PM_Column: TMenuItem;
    PM_Ver: TMenuItem;
    PM_Platform: TMenuItem;
    PM_Sort: TMenuItem;
    PM_PM_NoCompilerByRed: TMenuItem;
    L: TLabel;
    ShowCmd: TCheckBox;
    ShowAllLog: TCheckBox;
    BtnCompiler: TButton;
    ChangeLangID: TComboBox;
    ChangeStyle: TComboBox;
    L_Language: TLabel;
    L_Style: TLabel;
    ClangWin32: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure VDTInitNode(Sender: TBaseVirtualTree;
      ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure VDTDrawNode(Sender: TBaseVirtualTree;
      const PaintInfo: TVTPaintInfo);
    procedure VDTColumnClick(Sender: TBaseVirtualTree; Column: TColumnIndex;
      Shift: TShiftState);
    procedure BtnDownClick(Sender: TObject);
    procedure BtnUpClick(Sender: TObject);
    procedure BtnOpenClick(Sender: TObject);
    procedure BtnDelClick(Sender: TObject);
    procedure BtnAddClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure BtnSaveAsClick(Sender: TObject);
    procedure VDTMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PM_PM_NoCompilerByRedClick(Sender: TObject);
    procedure PM_SortClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure AddIDEClick(Sender: TObject);
    procedure PCChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure BtnCompilerClick(Sender: TObject);
    procedure BtnClearLogClick(Sender: TObject);
    procedure BtnSaveLogClick(Sender: TObject);
    procedure BtnRegTypeClick(Sender: TObject);
    procedure ChangeLangIDChange(Sender: TObject);
    procedure ChangeStyleChange(Sender: TObject);
    procedure VDTMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    procedure Init();
    procedure UnInit();
    procedure LoadProject(FileName: string);
    procedure AddFile(FileName: string);
    procedure GetCompilerFromReg();
    procedure CheckCompiler(QJson: TQjson);
    procedure GetCompilerInfo(QJson: TQjson);
    function GetDelphiIDEVersion(QJson: TQjson): Integer;
    procedure GetEnvVar(QJson: TQjson);
    procedure CreateColumn(QJson: TQjson);
    procedure CreateVDTColumn(ColumnName: string);
    procedure SaveProject(FileName: string);
    procedure CreatePopMenu(ParentTag: Integer; MenuName: string = '');
    procedure MenuClick(Sender: TObject);
    function GetRequireFile(FileName: string;
      NeedBrace: Boolean = True): string;
    function IsDesignOnly(FileName: string): Boolean;
    function IsFmxProj(FileName: string): Boolean;
    function GetDProjMainSource(FileName: string): string;
    procedure AcceptFiles(var Msg: TMessage); message WM_DROPFILES;
    procedure CreateTabSheet(QJson: TQjson);
    procedure GetCompilerFromConfig();
    function ReplPredefined(Path: string; QJson: TQjson): string;
    procedure CompilerJob(AJob: PQJob);
    procedure SetCompilerResult(Str: string);
    procedure ShowCompilerResult(AJob: PQJob);
    function GetIncludePaths(QJson: TQjson): string;
    function GetLibPaths(QJson: TQjson; IsDebug: Boolean = False): string;
    procedure GetDotNETPath();
    function CheckDPKSuffix(FileName: string; IDEVersion: Integer): string;
    procedure CheckResult(ResultStr, FileName, ColumnName: string);
    function FilterResult(ResultStr: string): string;
    function GetSimpleFileName(FileName: string): string;
    procedure DoLanguageChanged(Sender: TObject);
    procedure SysMenuCommand(var Msg: TWMMENUSELECT); message WM_SYSCOMMAND;
    procedure DelayRunWithCmdline(AJob: PQJob);
    function RunWithCmdline(): Boolean;
    procedure ScanFiles(Source: string; Masks: TStringList);
    procedure SelectVerAndPlatform(Ver, PlatformStr: string);
    procedure GenerateHTMLReport(Path: string);
{$IF RTLVersion > 22}
    procedure StyleChanged(var Msg: TMessage); message CM_STYLECHANGED;
{$IFEND}
  public
    { Public declarations }
  end;
{$IFNDEF QLANG_SUPPORT}

function _(const S: String): String; inline;
{$ENDIF}

var
  Form_Main: TForm_Main;

implementation

uses
{$IF RTLVersion > 22}
{$R RC\Theme.res}
  Vcl.Themes,
  Vcl.Styles,
{$IFEND}
{$IF RTLVersion > 23}
  VirtualTrees.Utils,
{$IFEND}
  QString, StrUtils, Registry, Math, ShellAPI, Common, Unit2, Unit3,
  SyncObjs{$IFDEF QLANG_SUPPORT}, Qlang{$ENDIF}
{$IF RTLVersion<22}
    , PerlRegEx, pcre
{$ELSE}
    , RegularExpressionsCore
{$IFEND}
    ;
{$R *.dfm}

type
  PCompilerResult = ^TCompilerResult;

  TCompilerResult = record
    Result: String;
  end;

var
  CfgName, DotNETPath, SeptalLine, FullLogTxt: string;
  OnlyFromConfig: Boolean;
  HitInfo: THitInfo;
  FRegPath, FExtList, FCompleteFileList, FSimpleFileList,
    FColumnList: TStringList;
  FCustomCfg, FProjectCfg, FCompilerCfg, FResultList: TQjson;
  FJobGroup: TQJobGroup;
  FCol, FRow, AllTimes, SuccessTimes, ErrorTimes, SelectLangID: Integer;
{$IFNDEF QLANG_SUPPORT}

function _(const S: String): String;
begin
  Result := S;
end;
{$ENDIF}

procedure TForm_Main.Init();
var
  I: Integer;
begin
  SeptalLine :=
    _('============================================================');
  OnlyFromConfig := False;
  CompleteCurrPath := MyGetCurrentDirectory;
  ComSpec := GetEnvironmentVariable('ComSpec');
  GetDotNETPath;
  FRegPath := TStringList.Create;
  FExtList := TStringList.Create;
  FCompleteFileList := TStringList.Create;
  FSimpleFileList := TStringList.Create;
  FColumnList := TStringList.Create;
  FRegPath.Add('Software\Borland\Delphi');
  FRegPath.Add('Software\Borland\C++Builder');
  FRegPath.Add('Software\Borland\BDS');
  FRegPath.Add('Software\Codegear\BDS');
  FRegPath.Add('Software\Embarcadero\BDS');
  FExtList.Add('.pas');
  FExtList.Add('.dpr');
  FExtList.Add('.dpk');
  FExtList.Add('.dproj');
  FExtList.Add('.c');
  FExtList.Add('.cpp');
  FExtList.Add('.bpr');
  FExtList.Add('.bpk');
  FExtList.Add('.cbproj');
  FCustomCfg := TQjson.Create;
  FProjectCfg := TQjson.Create;
  FCompilerCfg := TQjson.Create;
  FResultList := TQjson.Create;
  FProjectCfg.ForcePath('Ver').AsInteger := 2;
  FProjectCfg.ForcePath('UpdateTime').AsDateTime := Now;
  FProjectCfg.ForcePath('Project.Output.Bin').AsString := '';
  FProjectCfg.ForcePath('Project.Output.Include').AsString := '';
  FProjectCfg.ForcePath('Project.Output.Lib').AsString := '';
  FProjectCfg.ForcePath('Project.Extra.Alias').AsString := '';
  FProjectCfg.ForcePath('Project.Extra.NameSpaceSearch').AsString := '';
  FProjectCfg.ForcePath('Project.Extra.Include').AsString := '';
  FProjectCfg.ForcePath('Project.Extra.Lib').AsString := '';
  FProjectCfg.ForcePath('Project.Extra.Search').AsString := '';
  FProjectCfg.ForcePath('Project.Extra.NameSpaces').AsBoolean := True;
  FProjectCfg.ForcePath('Project.Extra.BuildAllUnits').AsBoolean := True;
  FProjectCfg.ForcePath('Project.Extra.DpkAddIDEVersion').AsBoolean := False;
  FProjectCfg.ForcePath('Project').AddArray('Files');
  FCompilerCfg.AddArray('Compiler');
  FJobGroup := TQJobGroup.Create(True);
  VDT.NodeDataSize := sizeof(Pointer);
  FCustomCfg.ForcePath('OnlyFromConfig').AsBoolean := False;
  FCustomCfg.ForcePath('UpdateTime').AsDateTime := Now;
  FCustomCfg.ForcePath('Custom.Compiles').DataType := jdtArray;

  if FileExists(ExtractFilePath(Application.ExeName) + 'Data.cfg') then
    FCustomCfg.LoadFromFile(ExtractFilePath(Application.ExeName) + 'Data.cfg');

  if FCustomCfg.IndexOf('LangID') < 0 then
    FCustomCfg.ForcePath('LangID').AsInteger := GetSystemDefaultLangID;
{$IFDEF QLANG_SUPPORT}
  LangManager.AddListener(DoLanguageChanged);
  SelectLangID := FCustomCfg.ForcePath('LangID').AsInteger;
  if LangManager.LanguageExists(SelectLangID) then
    LangManager.ActiveLocale := SelectLangID;
{$ENDIF}
  ChangeLangID.Items.Add('English');
{$IFDEF QLANG_SUPPORT}
  for I := 1 to LangManager.Count - 1 do
    ChangeLangID.Items.Add(LangManager.Languages[I].Name);
  ChangeLangID.ItemIndex := LangManager.ActiveIndex;
{$ELSE}
  ChangeLangID.ItemIndex := 0;
{$ENDIF}
  DoLanguageChanged(nil);

  AppendMenu(GetSystemMenu(Handle, False), MF_SEPARATOR, 0, '');
  AppendMenu(GetSystemMenu(Handle, False), MF_STRING, 200,
    PWideChar(_('About') + '(&A)'));
  AppendMenu(GetSystemMenu(Handle, False), MF_SEPARATOR, 0, '');
  AppendMenu(GetSystemMenu(Handle, False), MF_STRING, 201,
    PWideChar(_('Only read from the Data.Cfg')));

  if FCustomCfg.IndexOf('OnlyFromConfig') < 0 then
    OnlyFromConfig := False
  else
    OnlyFromConfig := FCustomCfg.ForcePath('OnlyFromConfig').AsBoolean;
  if not OnlyFromConfig then
    GetCompilerFromReg
  else
    CheckMenuItem(GetSystemMenu(Handle, False), 201, MF_CHECKED);
{$IF RTLVersion > 22}
  L_Style.Visible := True;
  ChangeStyle.Visible := True;
  for I := 0 to Length(TStyleManager.StyleNames) - 1 do
  begin
    ChangeStyle.Items.Add(TStyleManager.StyleNames[I]);
  end;
  if FCustomCfg.IndexOf('Style') < 0 then
    FCustomCfg.ForcePath('Style').AsString := 'Luna';
  TStyleManager.TrySetStyle(FCustomCfg.ForcePath('Style').AsString, False);
  ChangeStyle.ItemIndex := ChangeStyle.Items.IndexOf
    (FCustomCfg.ForcePath('Style').AsString);
{$IFEND}
  GetCompilerFromConfig;

  CreatePopMenu(1);
  CreatePopMenu(2);
  CreatePopMenu(3);
  CreateColumn(FCompilerCfg);
end;

procedure TForm_Main.UnInit();
begin
{$IFDEF QLANG_SUPPORT}
  LangManager.RemoveListener(DoLanguageChanged);
{$ENDIF}
  FRegPath.Clear;
  FRegPath.Free;
  FExtList.Clear;
  FExtList.Free;
  FCompleteFileList.Clear;
  FCompleteFileList.Free;
  FSimpleFileList.Clear;
  FSimpleFileList.Free;
  FColumnList.Clear;
  FColumnList.Free;
  FCustomCfg.Clear;
  FCustomCfg.Free;
  FProjectCfg.Clear;
  FProjectCfg.Free;
  FResultList.Clear;
  FResultList.Free;
  FJobGroup.Free;
  Workers.Clear;
end;

procedure TForm_Main.BtnAddClick(Sender: TObject);
var
  I: Integer;
begin
  OD.DefaultExt := '';
  OD.FileName := '';
  OD.Filter := _('All available files') +
    '|*c;*.cpp;*.pas;*.bpr;*.dpr;*.bpk;*.dpk;*.cbproj;*.dproj|Delphi' +
    _(' Unit') + '|*.Pas|Delphi' + _(' Project') +
    '|*.dpr;*.dpk;*.dproj|C++ Builder' + _(' Unit') + '|*.cpp;*.c|C++ Builder' +
    _(' Project') + '|*.bpr;*.bpk;*.cbproj';
  OD.FilterIndex := 0;
  OD.Title := _('Please select files to open');
  if OD.Execute then
  begin
    for I := 0 to OD.Files.Count - 1 do
      AddFile(OD.Files.Strings[I]);
  end;
end;

procedure TForm_Main.BtnClearLogClick(Sender: TObject);
begin
  Log.Lines.Clear;
end;

procedure TForm_Main.BtnCompilerClick(Sender: TObject);
var
  QJson, QJsonC: TQjson;
  I, J: Integer;
  ColumnName, PlatformName, PlatformStr: string;
begin
  if BtnCompiler.Tag = 1 then
  begin
    FJobGroup.Cancel(False);
    BtnCompiler.Tag := 0;
    P1.Enabled := True;
    BtnCompiler.Caption := _('Start');
    Exit;
  end;
  if VDT.RootNodeCount = 0 then
  begin
    ShowErrorMessage(_('To add a file at least') + '!');
    BtnAdd.SetFocus;
    Exit;
  end;
  if Length(OutIncludeDir.Text) = 0 then
  begin
    ShowErrorMessage(_('You must choose a Include directory') + '!');
    OutIncludeDir.SetFocus;
    Exit;
  end;
  if Length(OutLibDir.Text) = 0 then
  begin
    ShowErrorMessage(_('You must choose a Lib directory') + '!');
    OutLibDir.SetFocus;
    Exit;
  end;
  if Length(OutFinalDir.Text) = 0 then
  begin
    ShowErrorMessage(_('You must choose a Bin directory') + '!');
    OutFinalDir.SetFocus;
    Exit;
  end;
  P1.Enabled := False;
  BtnCompiler.Tag := 1;
  BtnCompiler.Caption := _('Stop');
  FResultList.Clear;
  try
    begin
      try
        begin
          if FJobGroup.MsgWaitFor() <> wrSignaled then
            ShowErrorMessage(_('JobGroup Error') + '!');
          FJobGroup.Prepare;
          AllTimes := 0;
          SuccessTimes := 0;
          ErrorTimes := 0;
          for I := 0 to FColumnList.Count - 1 do
          begin
            ColumnName := FColumnList.Names[I];
            PlatformStr := Copy(ColumnName, Length(ColumnName) - 1, 2);
            if PlatformStr = '32' then
              PlatformName := 'Win32'
            else if PlatformStr = '64' then
              PlatformName := 'Win64'
            else if PlatformStr = 'AD' then
              PlatformName := 'Android'
            else if PlatformStr = 'AI6' then
              PlatformName := 'Android64'
            else if PlatformStr = 'IO' then
              PlatformName := 'IOS'
            else if PlatformStr = 'I6' then
              PlatformName := 'IOS64'
            else if PlatformStr = 'OS' then
              PlatformName := 'OSX'
            else if PlatformStr = 'O6' then
              PlatformName := 'OSX64';
            for J := 0 to FProjectCfg.ForcePath('Project.Files').Count - 1 do
            begin
              if FProjectCfg.ForcePath('Project.Files[' + IntToStr(J) +
                '].Compile').IndexOf(ColumnName) < 0 then
                FProjectCfg.ForcePath('Project.Files[' + IntToStr(J) +
                  '].Compile.' + ColumnName).AsBoolean := False;
              if FProjectCfg.ForcePath('Project.Files[' + IntToStr(J) + '].Use')
                .AsBoolean and FProjectCfg.ForcePath
                ('Project.Files[' + IntToStr(J) + '].Compile.' + ColumnName).AsBoolean
              then
              begin
                QJson := TQjson(FColumnList.Objects[I]).Copy;
                QJson.ForcePath('Project.ColumnName').AsString := ColumnName;
                QJson.ForcePath('Project.PlatformName').AsString :=
                  PlatformName;
                QJson.ForcePath('Project.Platform').AsString := PlatformStr;
                QJson.ForcePath('Project.FileName').AsString :=
                  FProjectCfg.ForcePath('Project.Files[' + IntToStr(J) +
                  '].FileName').AsString;
                QJson.ForcePath('Project.CompleteFileName').AsString :=
                  GetCompleteFileName
                  (FProjectCfg.ForcePath('Project.Files[' + IntToStr(J) +
                  '].FileName').AsString);
                QJson.ForcePath('Project.OutFinalDir').AsString :=
                  GetCompleteFileName(OutFinalDir.Text);
                QJson.ForcePath('Project.OutIncludeDir').AsString :=
                  GetCompleteFileName(OutIncludeDir.Text);
                QJson.ForcePath('Project.OutLibDir').AsString :=
                  GetCompleteFileName(OutLibDir.Text);
                QJson.ForcePath('Project.ExtraAlias').AsString :=
                  ExtraAlias.Text;
                QJson.ForcePath('Project.ExtraNameSpaceSearch').AsString :=
                  ExtraNameSpaceSearch.Text;
                QJson.ForcePath('Project.ShowCMD').AsBoolean := ShowCmd.Checked;
                QJson.ForcePath('Project.ShowAllLog').AsBoolean :=
                  ShowAllLog.Checked;
                QJson.ForcePath('Project.Config').AsBoolean :=
                  BuildDebug.Checked;
                QJson.ForcePath('Project.NameSpaces').AsBoolean :=
                  ExtraNameSpaces.Checked;
                QJson.ForcePath('Project.BuildAllUnits').AsBoolean :=
                  ExtraBuildAllUnits.Checked;
                QJson.ForcePath('Project.AddIDEVersion').AsBoolean :=
                  DpkAddIDEVersion.Checked;
                QJson.ForcePath('Project.ExtraIncludeDir').AsString :=
                  ReplPredefined(ExtraIncludeDir.Text, QJson);
                QJson.ForcePath('Project.ExtraLibDir').AsString :=
                  ReplPredefined(ExtraLibDir.Text, QJson);
                QJson.ForcePath('Project.ExtraSearchDir').AsString :=
                  ReplPredefined(ExtraSearchDir.Text, QJson);

                if ((ExtractFileExt(LowerCase(QJson.ForcePath
                  ('Project.FileName').AsString)) = '.cpp') or
                  (ExtractFileExt(LowerCase(QJson.ForcePath('Project.FileName')
                  .AsString)) = '.c')) and (ClangWin32.Checked) and
                  (PlatformStr = '32') and
                  (QJson.ForcePath('IDEVersion').AsInteger > 22) and
                  (FileExists(QJson.ForcePath('RootDir').AsString +
                  '\bin\bcc32c.exe')) then
                begin
                  QJsonC := QJson.Copy;
                  QJsonC.ForcePath('Project.Platform').AsString := '3C';
                  QJsonC.ForcePath('Project.PlatformName').AsString := 'Win32c';
                  FJobGroup.Add(CompilerJob, QJsonC, False, jdfFreeAsObject);
                end;
                FJobGroup.Add(CompilerJob, QJson, False, jdfFreeAsObject);

                AllTimes := AllTimes + 1;
              end;
            end;
          end;
          FullLogTxt := '';
          FJobGroup.Run();
          case FJobGroup.MsgWaitFor of
            wrAbandoned:
              PlatformStr := _('Abandoned');
            wrTimeout:
              PlatformStr := _('Timeout');
            wrError:
              PlatformStr := _('Error');
          else
            PlatformStr := _('Finish');
          end;

          SetCompilerResult(_('End compilation process') + '(' + PlatformStr +
            ':All ' + IntToStr(AllTimes) + ' /Success ' + IntToStr(SuccessTimes)
            + ' /Error ' + IntToStr(ErrorTimes) + ') !');
        end;
      except
        on e: Exception do
        begin
          ShowErrorMessage(_('Compiler Error, error description') + ': ' +
            e.Message);
        end;
      end;
    end;
  finally
    begin
      P1.Enabled := True;
      BtnCompiler.Tag := 0;
      BtnCompiler.Caption := _('Start');
    end;
  end;
end;

procedure TForm_Main.BtnDelClick(Sender: TObject);
var
  CurrentNode: PVirtualNode;
  Data: PQJson;
  Str: string;
  FileName: string;
  I: Integer;
begin
  if VDT.SelectedCount < 1 then
    Exit;
  CurrentNode := VDT.GetFirstSelected;
  Data := VDT.GetNodeData(CurrentNode);
  FileName := Data.ForcePath('FileName').AsString;
  for I := 1 to VDT.SelectedCount - 1 do
  begin
    CurrentNode := VDT.GetNextSelected(CurrentNode);
    Data := VDT.GetNodeData(CurrentNode);
    FileName := FileName + #13 + #10 + Data.ForcePath('FileName').AsString;
  end;

  Str := _('Are you sure you want to delete the files') + ' ?' + #13 + #10
    + FileName;
  if ShowQuestionMessage(Str, _('Delete')) = IDYES then
  begin
    while VDT.SelectedCount > 0 do
    begin
      CurrentNode := VDT.GetFirstSelected;
      Data := VDT.GetNodeData(CurrentNode);
      FileName := Data.ForcePath('FileName').AsString;
      I := FCompleteFileList.IndexOf(LowerCase(GetCompleteFileName(FileName)));
      if I >= 0 then
        FCompleteFileList.Delete(I);
      FileName := ExtractFileName(FileName);
      FileName := GetSimpleFileName(FileName);
      I := FSimpleFileList.IndexOf(LowerCase(FileName));
      if I >= 0 then
        FSimpleFileList.Delete(I);
      VDT.DeleteNode(CurrentNode);
      Data.Delete();
    end;
  end;
end;

procedure TForm_Main.BtnDownClick(Sender: TObject);
var
  CurrentNode: PVirtualNode;
  NextNode: PVirtualNode;
  Data: PQJson;
begin
  if VDT.SelectedCount < 1 then
    Exit;
  CurrentNode := VDT.FocusedNode;
  if CurrentNode.Index >= VDT.RootNodeCount - 1 then
    Exit;
  NextNode := CurrentNode.NextSibling;
  VDT.MoveTo(CurrentNode, NextNode, amInsertAfter, False);
  Data := VDT.GetNodeData(CurrentNode);
  FProjectCfg.ForcePath('Project.Files').ExchangeOrder(Data.ItemIndex,
    Data.ItemIndex + 1);
end;

procedure TForm_Main.BtnUpClick(Sender: TObject);
var
  CurrentNode: PVirtualNode;
  PrevNode: PVirtualNode;
  Data: PQJson;
begin
  if VDT.SelectedCount < 1 then
    Exit;
  CurrentNode := VDT.FocusedNode;
  if CurrentNode.Index = 0 then
    Exit;
  PrevNode := CurrentNode.PrevSibling;
  VDT.MoveTo(CurrentNode, PrevNode, amInsertBefore, False);
  Data := VDT.GetNodeData(CurrentNode);
  FProjectCfg.ForcePath('Project.Files').ExchangeOrder(Data.ItemIndex,
    Data.ItemIndex - 1);
end;

procedure TForm_Main.Button1Click(Sender: TObject);
var
  Directory: string;
begin
  Directory := FolderBrowser(GetCompleteFileName(OutFinalDir.Text));
  if Length(Directory) > 0 then
    OutFinalDir.Text := Directory;
end;

procedure TForm_Main.Button2Click(Sender: TObject);
var
  Directory: string;
begin
  Directory := FolderBrowser(GetCompleteFileName(OutIncludeDir.Text));
  if Length(Directory) > 0 then
    OutIncludeDir.Text := Directory;
end;

procedure TForm_Main.Button3Click(Sender: TObject);
var
  Directory: string;
begin
  Directory := FolderBrowser(GetCompleteFileName(OutLibDir.Text));
  if Length(Directory) > 0 then
    OutLibDir.Text := Directory;
end;

procedure TForm_Main.Button4Click(Sender: TObject);
var
  Form: TForm_PathSetup;
begin
  Form := TForm_PathSetup.Create(Self);
  Form.Init(ExtraLibDir.Text);
  Form.ShowModal;
  if Form.BtnSave.Tag = 1 then
    ExtraLibDir.Text := Form.GetPaths;
  Form.Free;
end;

procedure TForm_Main.Button5Click(Sender: TObject);
var
  Form: TForm_PathSetup;
begin
  Form := TForm_PathSetup.Create(Self);
  Form.Init(ExtraIncludeDir.Text);
  Form.ShowModal;
  if Form.BtnSave.Tag = 1 then
    ExtraIncludeDir.Text := Form.GetPaths;
  Form.Free;
end;

procedure TForm_Main.Button6Click(Sender: TObject);
var
  Form: TForm_PathSetup;
begin
  Form := TForm_PathSetup.Create(Self);
  Form.Init(ExtraSearchDir.Text);
  Form.ShowModal;
  if Form.BtnSave.Tag = 1 then
    ExtraSearchDir.Text := Form.GetPaths;
  Form.Free;
end;

procedure TForm_Main.Button7Click(Sender: TObject);
var
  RootDir, App: string;
  QJson: TQjson;
begin
  OD.DefaultExt := 'exe';
  OD.FileName := '';
  OD.Filter := _('All available files') +
    '|bds.exe;bcb.exe;delphi32.exe;dcc32.exe;dcc64.exe|' +
    _('The main program file') + '|bds.exe;bcb.exe;delphi32.exe|Delphi' +
    _(' command line') + '|dcc32.exe;dcc64.exe';
  OD.Title := _('Please select a file to open');
  if OD.Execute then
  begin
    App := OD.FileName;
    RootDir := ExcludeTrailingPathDelimiter
      (ExtractFilePath(ExcludeTrailingPathDelimiter(ExtractFilePath(App))));
    QJson := TQjson.Create;
    QJson.ForcePath('Valid').AsBoolean := True;
    QJson.ForcePath('RootDir').AsString := RootDir;
    CheckCompiler(QJson);
    if QJson.ForcePath('Valid').AsBoolean then
    begin
      L.Caption := Repl(QJson.ForcePath('Name').AsString, '(', #13 + '(');
      CustomRootDir.Text := QJson.ForcePath('RootDir').AsString;
    end;

  end;
end;

procedure TForm_Main.BtnOpenClick(Sender: TObject);
begin
  OD.DefaultExt := 'CompilerCfg';
  OD.FileName := '';
  OD.Filter := _('Configuration File') + '|*.CompilerCfg';
  OD.FilterIndex := 0;
  OD.Title := _('Please select the saved configuration file');
  if OD.Execute then
  begin
    LoadProject(OD.FileName);
  end;
end;

procedure TForm_Main.BtnRegTypeClick(Sender: TObject);
begin
  RegisterFileType('CompilerCfg', Application.ExeName);
end;

procedure TForm_Main.BtnSaveAsClick(Sender: TObject);
begin
  SD.DefaultExt := 'CompilerCfg';
  SD.Filter := _('Profiles') + '|*.CompilerCfg';
  SD.Title := _('Select Save Configuration File');
  SD.InitialDir := CompleteCurrPath;
  SD.FileName := ExtractFileName(CfgName);
  if SD.Execute then
    SaveProject(SD.FileName);
end;

procedure TForm_Main.BtnSaveClick(Sender: TObject);
var
  Save: Boolean;
begin
  if Length(CfgName) > 0 then
    SaveProject(CfgName)
  else
  begin
    Save := True;
    if VDT.RootNodeCount = 0 then
      if ShowQuestionMessage
        (_('Has not added a file, you really want to save it?'),
        _('Save Project')) = IDNO then
      begin
        Save := False;
      end;
    if Save then
    begin
      SD.DefaultExt := 'CompilerCfg';
      SD.Filter := _('Profiles') + '|*.CompilerCfg';
      SD.Title := _('Select Save Configuration File');
      if SD.Execute then
        SaveProject(SD.FileName);
    end;
  end;
end;

procedure TForm_Main.BtnSaveLogClick(Sender: TObject);
begin
  SD.DefaultExt := 'Log';
  SD.Filter := _('Log File') + '|*.Log';
  SD.Title := _('Select Save Log File');
  if Length(CfgName) > 0 then
  begin
    SD.InitialDir := ExtractFilePath(CfgName);
    SD.FileName := ChangeFileExt(ExtractFileName(CfgName), '.Compiler.Log');
  end;
  if SD.Execute() then
  begin
    Log.Lines.SaveToFile(SD.FileName);
  end;
end;

procedure TForm_Main.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if BtnCompiler.Tag = 1 then
  begin
    if ShowQuestionMessage(_('Being compiled, it is determined to terminate it')
      + '?') = IDNO then
      CanClose := False
    else
    begin
      FJobGroup.Cancel(False);
      BtnCompiler.Tag := 0;
      P1.Enabled := True;
      BtnCompiler.Caption := _('Start');
    end;
  end
  else
    CanClose := True;
end;

procedure TForm_Main.FormCreate(Sender: TObject);
begin
  DragAcceptFiles(Handle, True);
  Init;
  if FileTypeIsRegister('.CompilerCfg') then
  begin
    P3.Visible := IsAdministrator;
    PC.ActivePage := TabProject;
    BtnCompiler.Visible := True;
  end
  else
  begin
    PC.ActivePage := TabReadme;
    BtnCompiler.Visible := False;
  end;

  if (ParamCount = 1) and FileExists(ParamStr(1)) then
  begin
    if LowerCase(ExtractFileExt(ParamStr(1))) = '.compilercfg' then
    begin
      LoadProject(ParamStr(1));
    end;
  end
  else if ParamCount > 0 then
  begin
    if FindCmdLineSwitch('NU', True) then
    begin
      if RunWithCmdline then
      begin
        Application.ShowMainForm := False;
        Application.Terminate;
      end;
    end
    else
    begin
      Workers.Delay(DelayRunWithCmdline, 3000, nil, True);
    end;
  end;

end;

procedure TForm_Main.FormDestroy(Sender: TObject);
begin
  UnInit;
end;

procedure TForm_Main.VDTInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode;

  var InitialStates: TVirtualNodeInitStates);
var
  Data: TQjson;
begin
  Data := FProjectCfg.ForcePath('Project.Files[' + IntToStr(Node.Index) + ']');
  PQJson(Sender.GetNodeData(Node))^ := Data;
  Node.CheckType := ctCheckBox;
end;

procedure TForm_Main.VDTMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  Col, Row: Integer;
  BarInfo: TScrollBarInfo;
begin
  if VDT.RootNodeCount = 0 then
  begin
    FCol := -1;
    FRow := -1;
    Exit;
  end;
  Col := 4;
  Row := VDT.Header.Height + 5;
  BarInfo.cbSize := sizeof(BarInfo);
  if (GetWindowlong(VDT.Handle, GWL_STYLE) and WS_VSCROLL) <> 0 then
  begin
    GetScrollBarInfo(VDT.Handle, Integer(OBJID_VSCROLL), BarInfo);
    Col := Col + (BarInfo.rcScrollBar.Right - BarInfo.rcScrollBar.Left);
  end;
  if (GetWindowlong(VDT.Handle, GWL_STYLE) and WS_HSCROLL) <> 0 then
  begin
    GetScrollBarInfo(VDT.Handle, Integer(OBJID_HSCROLL), BarInfo);
    Row := Row + (BarInfo.rcScrollBar.Bottom - BarInfo.rcScrollBar.Top);
  end;
  if (X = 0) or (Y = 0) or (X >= VDT.Width - Col) or (Y >= VDT.Height - Row)
  then
  begin
    FCol := -1;
    FRow := -1;
    VDT.Repaint;
    Exit;
  end;
  Row := Y div Integer(VDT.DefaultNodeHeight);
  if X <= 80 then
    Col := X div 40
  else if X <= 80 + VDT.Header.Columns.Items[2].Width then
    Col := 2
  else
    Col := 3 + (X - 80 - VDT.Header.Columns.Items[2].Width) div 60;
  if (Col <> FCol) or (Row <> FRow) then
  begin
    FCol := Col;
    FRow := Row;
    VDT.Repaint;
  end;
end;

procedure TForm_Main.VDTMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  if Button <> mbRight then
    Exit;
  if VDT.RootNodeCount = 0 then
    Exit;
  VDT.GetHitTestInfoAt(X, Y, True, HitInfo);
  if (HitInfo.HitColumn <= 0) or (HitInfo.HitColumn = 2) then
    PM_Column.Enabled := False
  else
    PM_Column.Enabled := True;
  if HitInfo.HitNode = nil then
    PM_Row.Enabled := False
  else
    PM_Row.Enabled := True;
  P := Mouse.CursorPos;
  PM.Popup(P.X, P.Y);
end;

procedure TForm_Main.VDTDrawNode(Sender: TBaseVirtualTree;

  const PaintInfo: TVTPaintInfo);
var
  Data: PQJson;
  S, FileName: UnicodeString;
  R: TRect;
  Checked, IsHot: Boolean;
  CompilerResult: Integer;
begin
  with Sender as TVirtualDrawTree, PaintInfo do
  begin
    Data := Sender.GetNodeData(Node);
    FileName := Data.ForcePath('FileName').AsString;
    if (Column = FocusedColumn) and (Selected[Node]) then
      Canvas.Font.Color := clHighlightText
    else
      Canvas.Font.Color := clWindowText;
    SetBKMode(Canvas.Handle, TRANSPARENT);
    R := CellRect;
    S := '';
    case Column of
      0, 2:
        begin
          if Column = 2 then
            S := FileName
          else
            S := IntToStr(Node.Index + 1);
          if Column = 0 then
            DrawTextW(Canvas.Handle, PWideChar(S), Length(S), R,
              DT_TOP or DT_CENTER or DT_VCENTER or DT_SINGLELINE)
          else if Length(S) > 0 then
          begin
            S := ReverseString(S);
            if (Canvas.TextWidth(S) - 2 * Margin) > (R.Right - R.Left) then
              S := ShortenString(Canvas.Handle, S, R.Right - R.Left);
            S := ReverseString(S);
            if Copy(S, 1, 3) = '...' then
              S := Copy(FileName, 1, 5) + '...' + Copy(S, 9, Length(S) - 8);
            DrawTextW(Canvas.Handle, PWideChar(S), Length(S), R,
              DT_TOP or DT_LEFT or DT_VCENTER or DT_SINGLELINE);
          end;
        end;
    else

      begin
        if Column = 1 then
          Checked := Data.ForcePath('Use').AsBoolean
        else
        begin
          S := FColumnList.Names[Column - 3];
          if Data.ForcePath('Compile').IndexOf(S) < 0 then
          begin
            Checked := False;
            Data.ForcePath('Compile.' + S).AsBoolean := Checked;
          end
          else
            Checked := Data.ForcePath('Compile.' + S).AsBoolean;
        end;
        Canvas.Brush.Color := clWindow;
        if Column <> 1 then
        begin
          CompilerResult := 0;
          FileName := GetSimpleFileName(FileName);
          if FResultList.ForcePath(FileName).IndexOf(S) < 0 then
          begin
            FResultList.ForcePath(FileName + '.' + S).AsInteger := 0;
          end
          else
            CompilerResult := FResultList.ForcePath(FileName + '.' + S)
              .AsInteger;
          if CompilerResult = -1 then
            Canvas.Brush.Color := clRed
          else if CompilerResult = 1 then
            Canvas.Brush.Color := clLime;
        end;
        if not(Selected[Node] and VDT.Focused) and (Column <> 1) then
          Canvas.FillRect(R);
        IsHot := False;
        if (Integer(Node.Index) = FRow) and (Column = FCol) then
          IsHot := True;
        PaintCheckImage(Canvas.Handle, R, Checked, IsHot);
      end;
    end;
  end;
end;

procedure TForm_Main.VDTColumnClick(Sender: TBaseVirtualTree;
  Column: TColumnIndex; Shift: TShiftState);
var
  Data: PQJson;
  CurrentNode: PVirtualNode;
  Checked: Boolean;
  R: TRect;
  P: TPoint;
  Left, Right: Integer;
begin
  case Column of
    0, 2:
      begin
        Exit;
      end;
  else

    begin
      CurrentNode := VDT.FocusedNode;
      R := Sender.GetDisplayRect(CurrentNode, Column, False);
      InflateRect(R, -VDT.TextMargin, 0);
      Dec(R.Right);
      Dec(R.Bottom);
      Left := R.Left + (R.Right - R.Left - (R.Bottom - R.Top)) div 2 + 4;
      Right := Left + (R.Bottom - R.Top) - 4;
      P := Mouse.CursorPos;
      P := Sender.ScreenToClient(P);
      if (P.X >= Left) and (P.X <= Right) then
      begin
        Data := Sender.GetNodeData(CurrentNode);
        if Column = 1 then
        begin
          Checked := Data.ForcePath('Use').AsBoolean;
          Data.ForcePath('Use').AsBoolean := not Checked;
        end
        else
        begin
          Checked := Data.ForcePath('Compile.' + FColumnList.Names[Column - 3])
            .AsBoolean;
          Data.ForcePath('Compile.' + FColumnList.Names[Column - 3]).AsBoolean
            := not Checked;
        end;
        VDT.RepaintNode(CurrentNode);
      end;
    end;
  end;

end;

procedure TForm_Main.LoadProject(FileName: string);
var
  I: Integer;
  Name: string;
begin
  FProjectCfg.LoadFromFile(FileName, teUTF8);
  CfgName := FileName;
  Form_Main.Caption := 'CompilerTool  ' + CfgName;
  CompleteCurrPath := ExtractFilePath(FileName);
  FResultList.Clear;
  VDT.RootNodeCount := 0;
  FCompleteFileList.Clear;
  FSimpleFileList.Clear;
  VDT.RootNodeCount := FProjectCfg.ForcePath('Project.Files').Count;
  OutFinalDir.Text := FProjectCfg.ForcePath('Project.Output.Bin').AsString;
  OutIncludeDir.Text := FProjectCfg.ForcePath('Project.Output.Include')
    .AsString;
  OutLibDir.Text := FProjectCfg.ForcePath('Project.Output.Lib').AsString;
  ExtraAlias.Text := FProjectCfg.ForcePath('Project.Extra.Alias').AsString;
  ExtraNameSpaceSearch.Text := FProjectCfg.ForcePath
    ('Project.Extra.NameSpaceSearch').AsString;
  ExtraIncludeDir.Text := FProjectCfg.ForcePath
    ('Project.Extra.Include').AsString;
  ExtraLibDir.Text := FProjectCfg.ForcePath('Project.Extra.Lib').AsString;
  ExtraSearchDir.Text := FProjectCfg.ForcePath('Project.Extra.Search').AsString;
  if FProjectCfg.ForcePath('Project.Extra').IndexOf('NameSpaces') >= 0 then
    ExtraNameSpaces.Checked := FProjectCfg.ForcePath
      ('Project.Extra.NameSpaces').AsBoolean
  else
    ExtraNameSpaces.Checked := True;
  if FProjectCfg.ForcePath('Project.Extra').IndexOf('BuildAllUnits') >= 0 then
    ExtraBuildAllUnits.Checked := FProjectCfg.ForcePath
      ('Project.Extra.BuildAllUnits').AsBoolean
  else
    ExtraBuildAllUnits.Checked := True;
  if FProjectCfg.ForcePath('Project.Extra').IndexOf('DpkAddIDEVersion') >= 0
  then
    DpkAddIDEVersion.Checked := FProjectCfg.ForcePath
      ('Project.Extra.DpkAddIDEVersion').AsBoolean
  else
    DpkAddIDEVersion.Checked := False;
  for I := 0 to FProjectCfg.ForcePath('Project.Files').Count - 1 do
  begin
    Name := FProjectCfg.ForcePath('Project.Files[' + IntToStr(I) +
      '].FileName').AsString;
    FCompleteFileList.Add(LowerCase(GetCompleteFileName(Name)));
    Name := GetSimpleFileName(Name);
    FSimpleFileList.Add(LowerCase(Name));
  end;
end;

procedure TForm_Main.AddFile(FileName: string);
var
  CompleteFileName, Ext, ColumnName, Other, FileCount, PlatformStr: string;
  I, RTLVer: Integer;
  QJson: TQjson;
  IsDesign, IsFmx: Boolean;
begin
  Ext := LowerCase(ExtractFileExt(FileName));
  I := FExtList.IndexOf(Ext);
  if I = -1 then
    Exit;
  CompleteFileName := GetCompleteFileName(FileName);
  Other := GetSimpleFileName(FileName);
  if (FCompleteFileList.IndexOf(LowerCase(CompleteFileName)) < 0) and
    (FSimpleFileList.IndexOf(LowerCase(Other)) < 0) then
  begin
    FileCount := IntToStr(FCompleteFileList.Count);
    QJson := FProjectCfg.ForcePath('Project.Files[' + FileCount + ']');
    QJson.ForcePath('Use').AsBoolean := True;
    QJson.ForcePath('FileName').AsString := FileName;
    FCompleteFileList.Add(LowerCase(CompleteFileName));

    FSimpleFileList.Add(LowerCase(Other));

    IsFmx := IsFmxProj(CompleteFileName);
    IsDesign := IsDesignOnly(CompleteFileName);
    CompleteFileName := GetDProjMainSource(CompleteFileName);
    if I <> 3 then
      IsDesign := IsDesignOnly(CompleteFileName);
    Ext := LowerCase(ExtractFileExt(CompleteFileName));
    I := FExtList.IndexOf(Ext);
    case I of
      0, 1, 2:
        begin
          for I := 0 to FColumnList.Count - 1 do
          begin
            ColumnName := FColumnList.Names[I];
            PlatformStr := Copy(ColumnName, Length(ColumnName) - 1, 2);
            Other := FColumnList.ValueFromIndex[I];
            RTLVer := StrToint(PWideChar(Copy(Other, 1, 3)));
            Ext := GetRequireFile(CompleteFileName);
            QJson.ForcePath('Compile.' + ColumnName).AsBoolean := True;
            if Pos('D', Other) = 0 then
            begin
              QJson.ForcePath('Compile.' + ColumnName).AsBoolean := False;
            end
            else if (RTLVer < 230) and
              ((Pos('|fmx|', Ext) > 0) or (Pos('|fmx.', Ext) > 0)) then
            begin
              QJson.ForcePath('Compile.' + ColumnName).AsBoolean := False;
            end
            else if (Pos(PlatformStr, '|AD|A6|IO|I6|OS|O6|') > 0) and
              ((Pos('|vcl|', Ext) > 0) or (Pos('|windows|', Ext) > 0) or
              (Pos('|forms|', Ext) > 0) or (Pos('|winapi.', Ext) > 0)) then
            begin
              QJson.ForcePath('Compile.' + ColumnName).AsBoolean := False;
            end
            else if IsDesign and (PlatformStr <> '32') then
            begin
              QJson.ForcePath('Compile.' + ColumnName).AsBoolean := False;
            end
            else
            begin

            end;
          end;
        end;
      3:
        begin // not work, because GetDProjMainSource(XXX.dproj)=>XXX.dpk or XXX.dpk
        end;
      4, 5:
        begin
          for I := 0 to FColumnList.Count - 1 do
          begin
            ColumnName := FColumnList.Names[I];
            PlatformStr := Copy(ColumnName, Length(ColumnName) - 1, 2);
            Other := FColumnList.ValueFromIndex[I];
            Ext := GetRequireFile(CompleteFileName);
            QJson.ForcePath('Compile.' + ColumnName).AsBoolean := True;
            if Pos('C', Other) = 0 then
            begin
              QJson.ForcePath('Compile.' + ColumnName).AsBoolean := False;
            end
            else
            begin

            end;
          end;
        end;
      6, 7:
        begin
          for I := 0 to FColumnList.Count - 1 do
          begin
            ColumnName := FColumnList.Names[I];
            PlatformStr := Copy(ColumnName, Length(ColumnName) - 1, 2);
            Other := FColumnList.ValueFromIndex[I];
            Ext := GetRequireFile(CompleteFileName);
            QJson.ForcePath('Compile.' + ColumnName).AsBoolean := True;
            if Copy(ColumnName, 1, 2) <> 'C6' then
            begin
              QJson.ForcePath('Compile.' + ColumnName).AsBoolean := False;
            end
            else
            begin

            end;
          end;
        end;
      8:
        begin
          for I := 0 to FColumnList.Count - 1 do
          begin
            ColumnName := FColumnList.Names[I];
            PlatformStr := Copy(ColumnName, Length(ColumnName) - 1, 2);
            Other := FColumnList.ValueFromIndex[I];
            RTLVer := StrToint(PWideChar(Copy(Other, 1, 3)));
            Ext := GetRequireFile(CompleteFileName);
            QJson.ForcePath('Compile.' + ColumnName).AsBoolean := True;
            if Pos('C', Other) = 0 then
            begin
              QJson.ForcePath('Compile.' + ColumnName).AsBoolean := False;
            end
            else if IsFmx and (RTLVer < 230) then
            begin
              QJson.ForcePath('Compile.' + ColumnName).AsBoolean := False;
            end
            else if not IsFmx and (Pos(PlatformStr, '|AD|A6|IO|I6|OS|O6|') > 0)
            then
            begin
              QJson.ForcePath('Compile.' + ColumnName).AsBoolean := False;
            end
            else
            begin

            end;
          end;
        end;
    end;
    VDT.RootNodeCount := FProjectCfg.ForcePath('Project.Files').Count;
  end
  else
  begin
    (* if FSimpleFileList.IndexOf(LowerCase(Other)) >= 0 then
      begin
      Other := '';
      if I = 3 then
      begin
      Other := ExtractFileName(GetDProjMainSource(FileName));
      FileName := _('File') + '(' + ExtractFileName(FileName)
      + ' or ' + Other + ') ' + _('already exists in the list') + '!';
      end
      else
      FileName := _('File') + '(' + ExtractFileName(FileName) + ') ' + _
      ('already exists in the list') + '!';
      ShowErrorMessage(FileName, _('Add File'));
      end; *)
  end;
end;

procedure TForm_Main.AddIDEClick(Sender: TObject);
var
  Count: Integer;
begin
  if CustomRootDir.Text = '' then
  begin
    ShowErrorMessage(_('Please set the root directory') + '!');
    CustomRootDir.SetFocus;
    Exit;
  end;
  Count := FCompilerCfg.ForcePath('Compiler').Count;
  FCompilerCfg.ForcePath('Compiler[' + IntToStr(Count) + '].Valid')
    .AsBoolean := True;
  FCompilerCfg.ForcePath('Compiler[' + IntToStr(Count) + '].RootDir').AsString
    := CustomRootDir.Text;
  CheckCompiler(FCompilerCfg.ForcePath('Compiler[' + IntToStr(Count) + ']'));
  CreateColumn(FCompilerCfg);
  if FCompilerCfg.ForcePath('Compiler[' + IntToStr(Count) + '].Valid').AsBoolean
  then
  begin
    Count := FCustomCfg.ForcePath('Custom.Compiles').Count;
    FCustomCfg.ForcePath('Custom.Compiles[' + IntToStr(Count) + '].RootDir')
      .AsString := CustomRootDir.Text;
    FCustomCfg.ForcePath('UpdateTime').AsDateTime := Now;
    FCustomCfg.SaveToFile(ExtractFilePath(Application.ExeName) +
      'Data.cfg', teUTF8);
  end;
end;

procedure TForm_Main.GetCompilerFromReg();
var
  Reg: TRegistry;
  IDEVersionStr, RegPathName, IDECount, KeyName, Key, IDEType, RootDir: string;
  IDEVersion, I, J: Integer;
  RegIDEList: TStringList;
begin
  Reg := TRegistry.Create(KEY_READ);
  Reg.RootKey := HKEY_LOCAL_MACHINE;
  RegIDEList := TStringList.Create;
  IDECount := '0';
  for I := 0 to FRegPath.Count - 1 do
  begin
    if I = 0 then
      IDEType := 'DELPHI'
    else if I = 1 then
      IDEType := 'BCB'
    else
      IDEType := 'BDS';
    RegPathName := FRegPath.Strings[I];
    if Reg.KeyExists(RegPathName) then
    begin
      Reg.OpenKey(RegPathName, False);
      Reg.GetKeyNames(RegIDEList);
      Reg.CloseKey;
      for J := 0 to RegIDEList.Count - 1 do
      begin
        Key := RegIDEList.Strings[J];
        IDEVersion := Integer(Trunc(StrToFloatDef(Key, 0)));
        IDEVersionStr := IntToStr(IDEVersion) + '.0';
        if IDEVersionStr = Key then
        begin
          KeyName := RegPathName + '\' + Key;
          Reg.OpenKey(KeyName, False);
          if Reg.ValueExists('RootDir') then
          begin
            RootDir := ExcludeTrailingPathDelimiter(Reg.ReadString('RootDir'));
            if RootDir = '' then
              Continue
            else if not DirectoryExists(RootDir) then
              Continue
            else
            begin
              FCompilerCfg.ForcePath('Compiler[' + IDECount + '].Valid')
                .AsBoolean := True;
              FCompilerCfg.ForcePath('Compiler[' + IDECount + '].IDEVersion')
                .AsInteger := IDEVersion;
              FCompilerCfg.ForcePath('Compiler[' + IDECount + '].KeyName')
                .AsString := KeyName;
              FCompilerCfg.ForcePath('Compiler[' + IDECount + '].RootDir')
                .AsString := RootDir;
              FCompilerCfg.ForcePath('Compiler[' + IDECount + '].App').AsString
                := Reg.ReadString('App');
              FCompilerCfg.ForcePath('Compiler[' + IDECount + '].Type').AsString
                := IDEType;
              CheckCompiler(FCompilerCfg.ForcePath('Compiler[' +
                IDECount + ']'));
              if FCompilerCfg.ForcePath('Compiler[' + IDECount + '].Valid').AsBoolean
              then
              begin
                Key := 'Library';
                Reg.OpenKey(Key, False);
                if Reg.HasSubKeys then
                begin
                  Reg.OpenKey('Win32', False);
                  FCompilerCfg.ForcePath('Compiler[' + IDECount +
                    '].BrowsingPath').AsString :=
                    Reg.ReadString('Browsing Path');
                end
                else
                begin
                  FCompilerCfg.ForcePath('Compiler[' + IDECount +
                    '].BrowsingPath').AsString :=
                    Reg.ReadString('Browsing Path');
                end;
                IDECount := IntToStr(StrToint(PWideChar(IDECount)) + 1);
              end
              else
              begin
                FCompilerCfg.ForcePath('Compiler[' + IDECount + ']').Delete();
              end;
            end;
          end;
        end;
      end;
      Reg.CloseKey;
    end;
    Reg.CloseKey;
  end;
  Reg.Free;
  RegIDEList.Free;
end;

procedure TForm_Main.ChangeLangIDChange(Sender: TObject);
begin
{$IFDEF QLANG_SUPPORT}
  LangManager.ActiveIndex := ChangeLangID.ItemIndex;
  FCustomCfg.ForcePath('LangID').AsInteger := LangManager.ActiveLocale;
  FCustomCfg.SaveToFile(ExtractFilePath(Application.ExeName) + 'Data.Cfg',
    teUTF8, True);
{$ENDIF}
end;

procedure TForm_Main.ChangeStyleChange(Sender: TObject);
begin
{$IF RTLVersion > 22}
  if ChangeStyle.ItemIndex >= 0 then
    TStyleManager.TrySetStyle(ChangeStyle.Items.Strings
      [ChangeStyle.ItemIndex], False);
  FCustomCfg.ForcePath('Style').AsString := ChangeStyle.Items.Strings
    [ChangeStyle.ItemIndex];
  FCustomCfg.SaveToFile(ExtractFilePath(Application.ExeName) +
    'Data.cfg', teUTF8);
{$IFEND}
end;

procedure TForm_Main.CheckCompiler(QJson: TQjson);
var
  RootDir, PlatformStr: string;
begin
  RootDir := QJson.ForcePath('RootDir').AsString;
  if not DirectoryExists(RootDir) then
  begin
    QJson.ForcePath('Valid').AsBoolean := False;
    Exit;
  end;
  PlatformStr := '|';
  if FileExists(RootDir + '\Bin\Dcc32.exe') then
    PlatformStr := PlatformStr + 'D32|';
  if FileExists(RootDir + '\Bin\Dcc64.exe') then
    PlatformStr := PlatformStr + 'D64|';
  if FileExists(RootDir + '\Bin\Dccaarm.exe') then
    PlatformStr := PlatformStr + 'DAD|';
  if FileExists(RootDir + '\Bin\Dccaarm64.exe') then
    PlatformStr := PlatformStr + 'DA6|';
  if FileExists(RootDir + '\Bin\Dcciosarm.exe') then
    PlatformStr := PlatformStr + 'DIO|';
  if FileExists(RootDir + '\Bin\Dcciosarm64.exe') then
    PlatformStr := PlatformStr + 'DI6|';
  if FileExists(RootDir + '\Bin\Dccosx.exe') then
    PlatformStr := PlatformStr + 'DOS|';
  if FileExists(RootDir + '\Bin\Dccosx64.exe') then
    PlatformStr := PlatformStr + 'DO6|';
  if FileExists(RootDir + '\Bin\Bcc32.exe') then
    PlatformStr := PlatformStr + 'C32|';
  if FileExists(RootDir + '\Bin\Bcc64.exe') then
    PlatformStr := PlatformStr + 'C64|';
  if FileExists(RootDir + '\Bin\Bccaarm.exe') then
    PlatformStr := PlatformStr + 'CAD|';
  if FileExists(RootDir + '\Bin\Bccaarm64.exe') then
    PlatformStr := PlatformStr + 'CA6|';
  if FileExists(RootDir + '\Bin\Bcciosarm.exe') then
    PlatformStr := PlatformStr + 'CIO|';
  if FileExists(RootDir + '\Bin\Bcciosarm64.exe') then
    PlatformStr := PlatformStr + 'CI6|';
  if FileExists(RootDir + '\Bin\Bccosx.exe') then
    PlatformStr := PlatformStr + 'COS|';
  if FileExists(RootDir + '\Bin\Bccosx64.exe') then
    PlatformStr := PlatformStr + 'CO6|';
  if PlatformStr = '|' then
  begin
    QJson.ForcePath('Valid').AsBoolean := False;
    Exit;
  end;
  QJson.ForcePath('Platform').AsString := PlatformStr;
  GetCompilerInfo(QJson);
  GetEnvVar(QJson);
end;

procedure TForm_Main.GetCompilerInfo(QJson: TQjson);
var
  App, RootDir, IDEType, Ver, Name, PlatformStr, Cmd: string;
  IDEVersion, MyRTLVersion, MyProductVersion: Integer;
begin
  App := QJson.ForcePath('App').AsString;
  RootDir := QJson.ForcePath('RootDir').AsString;
  IDEType := QJson.ForcePath('Type').AsString;
  PlatformStr := QJson.ForcePath('Platform').AsString;
  Cmd := '';
  IDEVersion := 0;
  if FileExists(App) then
    Cmd := App
  else if FileExists(RootDir + '\Bin\BDS.exe') then
  begin
    IDEType := 'BDS';
    Cmd := RootDir + '\Bin\BDS.exe';
    QJson.ForcePath('App').AsString := Cmd;
  end
  else if FileExists(RootDir + '\Bin\Delphi32.exe') then
  begin
    IDEType := 'DELPHI';
    Cmd := RootDir + '\Bin\Delphi32.exe';
    QJson.ForcePath('App').AsString := Cmd;
  end
  else if FileExists(RootDir + '\Bin\BCB.exe') then
  begin
    IDEType := 'BCB';
    Cmd := RootDir + '\Bin\BCB.exe';
    QJson.ForcePath('App').AsString := Cmd;
  end;
  if Length(Cmd) > 0 then
    IDEVersion := GetFileVersion(Cmd) Div $10000
  else if Pos('|D', PlatformStr) > 0 then
  begin
    IDEVersion := GetDelphiIDEVersion(QJson);
    if IDEVersion < 9 then
      IDEType := 'DELPHI'
    else
      IDEType := 'BDS';
  end
  else
  begin
    IDEType := 'BCB';
  end;
  if IDEVersion < 6 then
  begin
    QJson.ForcePath('Valid').AsBoolean := False;
    Exit;
  end
  else if IDEType = 'BCB' then
  begin
    Ver := 'C' + IntToStr(IDEVersion);
    Name := 'Borland C++ Builder ' + IntToStr(IDEVersion);
  end
  else if IDEType = 'DELPHI' then
  begin
    Ver := 'D' + IntToStr(IDEVersion);
    Name := 'Delphi ' + IntToStr(IDEVersion);
  end
  else
  begin
    case IDEVersion of
      9:
        begin
          Name := 'Delphi 2005';
          Ver := 'D2005';
        end;
      10:
        begin
          Name := 'Borland Developer Studio 2006';
          Ver := 'RS2006';
        end;
      11:
        begin
          Name := 'CodeGear Delphi 2007 for Win32';
          Ver := 'RS2007';
        end;
      12, 13:
        begin
          Name := 'CodeGear RAD Studio 2009';
          Ver := 'RS2009';
        end;
      14:
        begin
          Name := 'Embarcadero RAD Studio 2010';
          Ver := 'RS2010';
        end;
      15:
        begin
          Name := 'Embarcadero RAD Studio XE';
          Ver := 'RSXE';
        end;
      23:
        begin
          Name := 'Embarcadero RAD Studio 10 Seattle';
          Ver := 'RS10';
        end;
      24:
        begin
          Name := 'Embarcadero RAD Studio 10.1 Berlin';
          Ver := 'RS10.1';
        end;
    else

      begin
        if (IDEVersion > 15) and (IDEVersion <= 22) then
        begin
          Name := 'Embarcadero RAD Studio XE' + IntToStr(IDEVersion - 14);
          Ver := 'RSXE' + IntToStr(IDEVersion - 14);
        end
        else if IDEVersion > 24 then
        begin
          Name := 'Embarcadero RAD Studio 10.' + IntToStr(IDEVersion - 23);
          Ver := 'RS10.' + IntToStr(IDEVersion - 23);
        end;
      end;
    end;
    if (Pos('|D', PlatformStr) > 0) and (Pos('|C', PlatformStr) > 0) then
      Name := Name + ' (Delphi and C++ Builder)'
    else if (Pos('|D', PlatformStr) > 0) and (Pos('|C', PlatformStr) = 0) then
      Name := Name + ' (Only Delphi)'
    else if (Pos('|D', PlatformStr) = 0) and (Pos('|C', PlatformStr) > 0) then
      Name := Name + ' (Only C++ Builder)';
  end;
  if IDEVersion = 11 then
    MyRTLVersion := 185
  else if IDEVersion < 13 then
    MyRTLVersion := (IDEVersion + 8) * 10
  else
    MyRTLVersion := (IDEVersion + 7) * 10;
  if (IDEVersion >= 20) or (IDEVersion < 14) then
    MyProductVersion := IDEVersion - 6
  else
    MyProductVersion := IDEVersion - 7;
  QJson.ForcePath('Type').AsString := IDEType;
  QJson.ForcePath('Name').AsString := Name;
  QJson.ForcePath('Ver').AsString := Ver;
  QJson.ForcePath('IDEVersion').AsInteger := IDEVersion;
  QJson.ForcePath('RTLVersion').AsInteger := MyRTLVersion;
  QJson.ForcePath('ProductVersion').AsInteger := MyProductVersion;
end;

function TForm_Main.GetDelphiIDEVersion(QJson: TQjson): Integer;
var
  RootDir, PlatformStr, Cmd: string;
  IDEVersion, P: Integer;
  Version: double;
begin
  RootDir := QJson.ForcePath('RootDir').AsString;
  PlatformStr := QJson.ForcePath('Platform').AsString;
  if Pos('|D32|', PlatformStr) > 0 then
    Cmd := RootDir + '\Bin\Dcc32.exe'
  else if Pos('|D64|', PlatformStr) > 0 then
    Cmd := RootDir + '\Bin\Dcc64.exe'
  else
  begin
    Result := 0;
    Exit;
  end;
  Version := 0.0;
  IDEVersion := 0;
  Cmd := PipeCall(Cmd);
  P := Pos('compiler version ', Cmd);
  if P > 0 then
  begin
    Cmd := Copy(Cmd, P + 17, 4);
    Version := StrToFloatDef(Cmd, 0.0);
  end;
  if Version > 0 then
  begin
    if Version >= 19 then
      IDEVersion := Trunc(Version) - 7
    else if Version = 18.5 then
      IDEVersion := Trunc(Version) - 7
    else if Version > 8 then
      IDEVersion := Trunc(Version) - 8
    else
      IDEVersion := 0;
  end;
  Result := IDEVersion;
end;

procedure TForm_Main.GetEnvVar(QJson: TQjson);
var
  RootDir, IDEType, Str, BDS, Common, Boost, Boost64: string;
  EnvList: TStringList;
  I: Integer;
begin
  if not QJson.ForcePath('Valid').AsBoolean then
    Exit;
  RootDir := QJson.ForcePath('RootDir').AsString;
  IDEType := QJson.ForcePath('Type').AsString;
  if IDEType <> 'BDS' then
  begin
    QJson.ForcePath('CommonDir').AsString := RootDir + '\Projects';
    Exit;
  end;
  if not FileExists(RootDir + '\Bin\RSVars.bat') then
    Exit;
  EnvList := TStringList.Create;
  EnvList.LoadFromFile(RootDir + '\Bin\RSVars.bat');
  for I := 0 to EnvList.Count - 1 do
  begin
    Str := LowerCase(EnvList.Strings[I]);
    if Pos('bds=', Str) > 0 then
    begin
      BDS := LowerCase(ExcludeTrailingPathDelimiter(EnvList.ValueFromIndex[I]))
    end
    else if Pos('bdscommondir=', Str) > 0 then
    begin
      Common := ExcludeTrailingPathDelimiter(EnvList.ValueFromIndex[I]);
    end
    else if Pos('cg_boost_root=', Str) > 0 then
    begin
      Boost := ExcludeTrailingPathDelimiter(EnvList.ValueFromIndex[I]);
    end
    else if Pos('cg_64_boost_root=', Str) > 0 then
    begin
      Boost64 := ExcludeTrailingPathDelimiter(EnvList.ValueFromIndex[I]);
    end;
  end;
  if Length(Common) > 0 then
    if DirectoryExists(Common) then
      QJson.ForcePath('CommonDir').AsString := Common;
  if Length(Boost) = 0 then
  begin
    Boost := RootDir + '\Include\boost_1_39';
  end
  else if Pos('\include\', LowerCase(Boost)) > 0 then
  begin
    Boost := Copy(Boost, Pos('\include\', LowerCase(Boost)),
      Length(Boost) - Pos('\include\', LowerCase(Boost)) + 1);
    Boost := RootDir + Boost;
  end;
  if DirectoryExists(Boost) then
    QJson.ForcePath('BoostDir').AsString := Boost;
  if Length(Boost64) = 0 then
  begin
    Boost64 := RootDir + '\Include\boost_1_50';
    if not DirectoryExists(Boost64) then
      Boost64 := RootDir + '\Include\boost_1_55';
  end
  else if Pos('\include\', LowerCase(Boost64)) > 0 then
  begin
    Boost64 := Copy(Boost64, Pos('\include\', LowerCase(Boost64)),
      Length(Boost64) - Pos('\include\', LowerCase(Boost64)) + 1);
    Boost64 := RootDir + Boost64;
  end;
  if DirectoryExists(Boost64) then
    QJson.ForcePath('BoostDir64').AsString := Boost64;
  EnvList.Clear;
  EnvList.Free;
end;

procedure TForm_Main.CreateColumn(QJson: TQjson);
var
  Ver, PlatformStr, Name, ColumnName, Str: string;
  I: Integer;
begin
  for I := 0 to QJson.ForcePath('Compiler').Count - 1 do
  begin
    if not QJson.ForcePath('Compiler[' + IntToStr(I) + '].Valid').AsBoolean then
      Continue;
    Ver := QJson.ForcePath('Compiler[' + IntToStr(I) + '].Ver').AsString;
    if Pos(Repl(Ver, '.', '_'), FColumnList.Text) = 0 then
    begin
      CreatePopMenu(4, Ver);
      CreateTabSheet(QJson.ForcePath('Compiler[' + IntToStr(I) + ']'));
    end;
    PlatformStr := QJson.ForcePath('Compiler[' + IntToStr(I) +
      '].Platform').AsString;
    if Pos('32|', PlatformStr) > 0 then
    begin
      Name := Ver + #13 + 'Win32';
      ColumnName := Repl(Ver, '.', '_') + '_32';
      if Pos('_32', FColumnList.Text) = 0 then
        CreatePopMenu(5, 'Win32');
      if FColumnList.IndexOfName(ColumnName) < 0 then
      begin
        Str := QJson.ForcePath('Compiler[' + IntToStr(I) +
          '].RTLVersion').AsString;
        if Pos('|C32|', PlatformStr) > 0 then
          Str := Str + 'C';
        if Pos('|D32|', PlatformStr) > 0 then
          Str := Str + 'D';
        FColumnList.AddObject(ColumnName + '=' + Str,
          QJson.ForcePath('Compiler[' + IntToStr(I) + ']'));
        CreateVDTColumn(Name);
      end;
    end;
    if Pos('64|', PlatformStr) > 0 then
    begin
      Name := Ver + #13 + 'Win64';
      ColumnName := Repl(Ver, '.', '_') + '_64';
      if Pos('_64', FColumnList.Text) = 0 then
        CreatePopMenu(5, 'Win64');
      if FColumnList.IndexOfName(ColumnName) < 0 then
      begin
        Str := QJson.ForcePath('Compiler[' + IntToStr(I) +
          '].RTLVersion').AsString;
        if Pos('|C64|', PlatformStr) > 0 then
          Str := Str + 'C';
        if Pos('|D64|', PlatformStr) > 0 then
          Str := Str + 'D';
        FColumnList.AddObject(ColumnName + '=' + Str,
          QJson.ForcePath('Compiler[' + IntToStr(I) + ']'));
        CreateVDTColumn(Name);
      end;
    end;
    if Pos('AD|', PlatformStr) > 0 then
    begin
      Name := Ver + #13 + 'Android';
      ColumnName := Repl(Ver, '.', '_') + '_AD';
      if Pos('_AD', FColumnList.Text) = 0 then
        CreatePopMenu(5, 'Android');
      if FColumnList.IndexOfName(ColumnName) < 0 then
      begin
        Str := QJson.ForcePath('Compiler[' + IntToStr(I) +
          '].RTLVersion').AsString;
        if Pos('|CAD|', PlatformStr) > 0 then
          Str := Str + 'C';
        if Pos('|DAD|', PlatformStr) > 0 then
          Str := Str + 'D';
        FColumnList.AddObject(ColumnName + '=' + Str,
          QJson.ForcePath('Compiler[' + IntToStr(I) + ']'));
        CreateVDTColumn(Name);
      end;
    end;
    if Pos('A6|', PlatformStr) > 0 then
    begin
      Name := Ver + #13 + 'Android64';
      ColumnName := Repl(Ver, '.', '_') + '_A6';
      if Pos('_A6', FColumnList.Text) = 0 then
        CreatePopMenu(5, 'Android64');
      if FColumnList.IndexOfName(ColumnName) < 0 then
      begin
        Str := QJson.ForcePath('Compiler[' + IntToStr(I) +
          '].RTLVersion').AsString;
        if Pos('|CA6|', PlatformStr) > 0 then
          Str := Str + 'C';
        if Pos('|DA6|', PlatformStr) > 0 then
          Str := Str + 'D';
        FColumnList.AddObject(ColumnName + '=' + Str,
          QJson.ForcePath('Compiler[' + IntToStr(I) + ']'));
        CreateVDTColumn(Name);
      end;
    end;
    if Pos('IO|', PlatformStr) > 0 then
    begin
      Name := Ver + #13 + 'IOS';
      ColumnName := Repl(Ver, '.', '_') + '_IO';
      if Pos('_IO', FColumnList.Text) = 0 then
        CreatePopMenu(5, 'IOS');
      if FColumnList.IndexOfName(ColumnName) < 0 then
      begin
        Str := QJson.ForcePath('Compiler[' + IntToStr(I) +
          '].RTLVersion').AsString;
        if Pos('|CIO|', PlatformStr) > 0 then
          Str := Str + 'C';
        if Pos('|DIO|', PlatformStr) > 0 then
          Str := Str + 'D';
        FColumnList.AddObject(ColumnName + '=' + Str,
          QJson.ForcePath('Compiler[' + IntToStr(I) + ']'));
        CreateVDTColumn(Name);
      end;
    end;
    if Pos('I6|', PlatformStr) > 0 then
    begin
      Name := Ver + #13 + 'IOS64';
      ColumnName := Repl(Ver, '.', '_') + '_I6';
      if Pos('_I6', FColumnList.Text) = 0 then
        CreatePopMenu(5, 'IOS64');
      if FColumnList.IndexOfName(ColumnName) < 0 then
      begin
        Str := QJson.ForcePath('Compiler[' + IntToStr(I) +
          '].RTLVersion').AsString;
        if Pos('|CI6|', PlatformStr) > 0 then
          Str := Str + 'C';
        if Pos('|DI6|', PlatformStr) > 0 then
          Str := Str + 'D';
        FColumnList.AddObject(ColumnName + '=' + Str,
          QJson.ForcePath('Compiler[' + IntToStr(I) + ']'));
        CreateVDTColumn(Name);
      end;
    end;
    if Pos('OS|', PlatformStr) > 0 then
    begin
      Name := Ver + #13 + 'OSX';
      ColumnName := Repl(Ver, '.', '_') + '_OS';
      if Pos('_OS', FColumnList.Text) = 0 then
        CreatePopMenu(5, 'OSX');
      if FColumnList.IndexOfName(ColumnName) < 0 then
      begin
        Str := QJson.ForcePath('Compiler[' + IntToStr(I) +
          '].RTLVersion').AsString;
        if Pos('|COS|', PlatformStr) > 0 then
          Str := Str + 'C';
        if Pos('|DOS|', PlatformStr) > 0 then
          Str := Str + 'D';
        FColumnList.AddObject(ColumnName + '=' + Str,
          QJson.ForcePath('Compiler[' + IntToStr(I) + ']'));
        CreateVDTColumn(Name);
      end;
    end;
    if Pos('O6|', PlatformStr) > 0 then
    begin
      Name := Ver + #13 + 'OSX64';
      ColumnName := Repl(Ver, '.', '_') + '_O6';
      if Pos('_O6', FColumnList.Text) = 0 then
        CreatePopMenu(5, 'OSX64');
      if FColumnList.IndexOfName(ColumnName) < 0 then
      begin
        Str := QJson.ForcePath('Compiler[' + IntToStr(I) +
          '].RTLVersion').AsString;
        if Pos('|CO6|', PlatformStr) > 0 then
          Str := Str + 'C';
        if Pos('|DO6|', PlatformStr) > 0 then
          Str := Str + 'D';
        FColumnList.AddObject(ColumnName + '=' + Str,
          QJson.ForcePath('Compiler[' + IntToStr(I) + ']'));
        CreateVDTColumn(Name);
      end;
    end;
  end;
end;

procedure TForm_Main.CreateVDTColumn(ColumnName: string);
var
  VTC: TVirtualTreeColumn;
begin
  VTC := VDT.Header.Columns.Add;
  VTC.Width := 60;
  VTC.MaxWidth := 60;
  VTC.MinWidth := 60;
  VTC.CaptionAlignment := taCenter;
  VTC.Options := VTC.Options + [coWrapCaption] - [coAllowClick];
  VTC.Text := ColumnName;
end;

procedure TForm_Main.SaveProject(FileName: string);
begin
  FProjectCfg.ForcePath('Ver').AsInteger := 2;
  FProjectCfg.ForcePath('UpdateTime').AsDateTime := Now;
  if OutFinalDir.Text = '' then
    OutFinalDir.Text := '.\Bin';
  if OutIncludeDir.Text = '' then
    OutIncludeDir.Text := '.\Include';
  if OutLibDir.Text = '' then
    OutLibDir.Text := '.\Lib';
  FProjectCfg.ForcePath('Project.Output.Bin').AsString := OutFinalDir.Text;
  FProjectCfg.ForcePath('Project.Output.Include').AsString :=
    OutIncludeDir.Text;
  FProjectCfg.ForcePath('Project.Output.Lib').AsString := OutLibDir.Text;
  FProjectCfg.ForcePath('Project.Extra.Alias').AsString := ExtraAlias.Text;
  FProjectCfg.ForcePath('Project.Extra.NameSpaceSearch').AsString :=
    ExtraNameSpaceSearch.Text;
  FProjectCfg.ForcePath('Project.Extra.Include').AsString :=
    ExtraIncludeDir.Text;
  FProjectCfg.ForcePath('Project.Extra.Lib').AsString := ExtraLibDir.Text;
  FProjectCfg.ForcePath('Project.Extra.Search').AsString := ExtraSearchDir.Text;
  FProjectCfg.ForcePath('Project.Extra.NameSpaces').AsBoolean :=
    ExtraNameSpaces.Checked;
  FProjectCfg.ForcePath('Project.Extra.BuildAllUnits').AsBoolean :=
    ExtraBuildAllUnits.Checked;
  FProjectCfg.ForcePath('Project.Extra.DpkAddIDEVersion').AsBoolean :=
    DpkAddIDEVersion.Checked;
  FProjectCfg.SaveToFile(FileName, teUTF8, True);
  CfgName := FileName;
  CompleteCurrPath := ExtractFilePath(FileName);
end;

procedure TForm_Main.CreatePopMenu(ParentTag: Integer; MenuName: string);
var
  ParentItem, Item: TMenuItem;
  I: Integer;
  procedure ThreeMenu();
  begin
    Item := nil;
    Item := TMenuItem.Create(PM);
    Item.Name := ParentItem.Name + '_SelectAll';
    Item.Caption := _('Select All');
    Item.Tag := 101;
    Item.OnClick := MenuClick;
    ParentItem.Add(Item);
    Item := nil;
    Item := TMenuItem.Create(PM);
    Item.Name := ParentItem.Name + '_ClearAll';
    Item.Caption := _('Clear All');
    Item.Tag := 102;
    Item.OnClick := MenuClick;
    ParentItem.Add(Item);
    Item := nil;
    Item := TMenuItem.Create(PM);
    Item.Name := ParentItem.Name + '_Unselected';
    Item.Caption := _('Unselected');
    Item.Tag := 103;
    Item.OnClick := MenuClick;
    ParentItem.Add(Item);
  end;

begin
  for I := 0 to PM.Items.Count - 1 do
  begin
    if PM.Items.Items[I].Tag = ParentTag then
    begin
      ParentItem := PM.Items.Items[I];
      if Length(MenuName) = 0 then
      begin
        ThreeMenu;
      end
      else
      begin
        if PM.FindComponent('PM_' + Repl(MenuName, '.', '_')) = nil then
        begin
          Item := nil;
          Item := TMenuItem.Create(PM);
          Item.Name := 'PM_' + Repl(MenuName, '.', '_');
          Item.Tag := ParentTag;
          Item.Caption := MenuName;
          ParentItem.Add(Item);
          ParentItem := Item;
          ThreeMenu;
        end;
      end;
      Break;
    end;
  end;
end;

procedure TForm_Main.MenuClick(Sender: TObject);
var
  ParentItem, Item: TMenuItem;
  I, J: Integer;
  QJson: TQjson;
  Checked: Boolean;
  Node: PVirtualNode;
  Data: PQJson;
  ColumnName, Name, Str: string;
begin
  Item := TMenuItem(Sender);
  ParentItem := Item.Parent;
  QJson := FProjectCfg.ForcePath('Project.Files');
  case ParentItem.Tag of
    1:
      begin
        case Item.Tag of
          101:
            begin
              for I := 0 to QJson.Count - 1 do
              begin
                QJson.Items[I].ForcePath('Use').AsBoolean := True;
                for J := 0 to QJson.Items[I].ForcePath('Compile').Count - 1 do
                  QJson.Items[I].ForcePath('Compile').Items[J]
                    .AsBoolean := True;
                for J := 0 to FColumnList.Count - 1 do
                  QJson.Items[I].ForcePath('Compile.' + FColumnList.Names[J])
                    .AsBoolean := True;
              end;
            end;
          102:
            begin
              for I := 0 to QJson.Count - 1 do
              begin
                QJson.Items[I].ForcePath('Use').AsBoolean := False;
                for J := 0 to QJson.Items[I].ForcePath('Compile').Count - 1 do
                  QJson.Items[I].ForcePath('Compile').Items[J]
                    .AsBoolean := False;
                for J := 0 to FColumnList.Count - 1 do
                  QJson.Items[I].ForcePath('Compile.' + FColumnList.Names[J])
                    .AsBoolean := False;
              end;
            end;
          103:
            begin
              for I := 0 to QJson.Count - 1 do
              begin
                Checked := QJson.Items[I].ForcePath('Use').AsBoolean;
                QJson.Items[I].ForcePath('Use').AsBoolean := not Checked;
                for J := 0 to QJson.Items[I].ForcePath('Compile').Count - 1 do
                begin
                  Checked := QJson.Items[I].ForcePath('Compile')
                    .Items[J].AsBoolean;
                  QJson.Items[I].ForcePath('Compile').Items[J].AsBoolean :=
                    not Checked;
                end;
                for J := 0 to FColumnList.Count - 1 do
                begin
                  if QJson.Items[I].ForcePath('Compile')
                    .IndexOf(FColumnList.Names[J]) < 0 then
                    QJson.Items[I].ForcePath('Compile.' + FColumnList.Names[J])
                      .AsBoolean := True;
                end;
              end;
            end;
        end;
      end;
    2:
      begin
        Node := HitInfo.HitNode;
        Data := VDT.GetNodeData(Node);
        case Item.Tag of
          101:
            begin
              Data.ForcePath('Use').AsBoolean := True;
              for I := 0 to Data.ForcePath('Compile').Count - 1 do
                Data.ForcePath('Compile').Items[I].AsBoolean := True;
              for I := 0 to FColumnList.Count - 1 do
                Data.ForcePath('Compile.' + FColumnList.Names[I])
                  .AsBoolean := True;
            end;
          102:
            begin
              Data.ForcePath('Use').AsBoolean := False;
              for I := 0 to Data.ForcePath('Compile').Count - 1 do
                Data.ForcePath('Compile').Items[I].AsBoolean := False;
              for I := 0 to FColumnList.Count - 1 do
                Data.ForcePath('Compile.' + FColumnList.Names[I])
                  .AsBoolean := False;
            end;
          103:
            begin
              Checked := Data.ForcePath('Use').AsBoolean;
              Data.ForcePath('Use').AsBoolean := not Checked;
              for I := 0 to Data.ForcePath('Compile').Count - 1 do
              begin
                Checked := Data.ForcePath('Compile').Items[I].AsBoolean;
                Data.ForcePath('Compile').Items[I].AsBoolean := not Checked;
              end;
              for I := 0 to FColumnList.Count - 1 do
              begin
                if Data.ForcePath('Compile').IndexOf(FColumnList.Names[I]) < 0
                then
                  Data.ForcePath('Compile.' + FColumnList.Names[I])
                    .AsBoolean := True;
              end;
            end;
        end;
      end;
    3:
      begin
        J := HitInfo.HitColumn;
        case Item.Tag of
          101:
            begin
              if J = 1 then
              begin
                for I := 0 to QJson.Count - 1 do
                  QJson.Items[I].ForcePath('Use').AsBoolean := True;
              end
              else
              begin
                ColumnName := FColumnList.Names[J - 3];
                for I := 0 to QJson.Count - 1 do
                begin
                  QJson.Items[I].ForcePath('Compile.' + ColumnName)
                    .AsBoolean := True;
                end;
              end;
            end;
          102:
            begin
              if J = 1 then
              begin
                for I := 0 to QJson.Count - 1 do
                  QJson.Items[I].ForcePath('Use').AsBoolean := False;
              end
              else
              begin
                ColumnName := FColumnList.Names[J - 3];
                for I := 0 to QJson.Count - 1 do
                begin
                  QJson.Items[I].ForcePath('Compile.' + ColumnName)
                    .AsBoolean := False;
                end;
              end;
            end;
          103:
            begin
              if J = 1 then
              begin
                for I := 0 to QJson.Count - 1 do
                begin
                  Checked := QJson.Items[I].ForcePath('Use').AsBoolean;
                  QJson.Items[I].ForcePath('Use').AsBoolean := not Checked;
                end;
              end
              else
              begin
                ColumnName := FColumnList.Names[J - 3];
                for I := 0 to QJson.Count - 1 do
                begin
                  if QJson.Items[I].ForcePath('Compile').IndexOf(ColumnName) < 0
                  then
                    QJson.Items[I].ForcePath('Compile.' + ColumnName)
                      .AsBoolean := True
                  else
                  begin
                    Checked := QJson.Items[I].ForcePath('Compile.' + ColumnName)
                      .AsBoolean;
                    QJson.Items[I].ForcePath('Compile.' + ColumnName).AsBoolean
                      := not Checked;
                  end;
                end;
              end;
            end;
        end;
      end;
    4, 5:
      begin
        Name := ParentItem.Name;
        if ParentItem.Tag = 4 then
          Name := Copy(Name, 4, Length(Name) - 3) + '_'
        else
        begin
          Name := Copy(Name, 4, Length(Name) - 3);
          if Name = 'Win32' then
            Name := '_32'
          else if Name = 'Win64' then
            Name := '_64'
          else if Name = 'Android' then
            Name := '_AD'
          else if Name = 'Android64' then
            Name := '_A6'
          else if Name = 'IOS' then
            Name := '_IO'
          else if Name = 'IOS64' then
            Name := '_I6'
          else if Name = 'OSX' then
            Name := 'OS'
          else if Name = 'OSX64' then
            Name := '_O6';
        end;
        case Item.Tag of
          101:
            begin
              for I := 0 to QJson.Count - 1 do
              begin
                for J := 0 to FColumnList.Count - 1 do
                begin
                  Str := FColumnList.Names[J];
                  if Pos(Name, Str) > 0 then
                    QJson.Items[I].ForcePath('Compile.' + Str)
                      .AsBoolean := True;
                end;
              end;
            end;
          102:
            begin
              for I := 0 to QJson.Count - 1 do
              begin
                for J := 0 to FColumnList.Count - 1 do
                begin
                  Str := FColumnList.Names[J];
                  if Pos(Name, Str) > 0 then
                    QJson.Items[I].ForcePath('Compile.' + Str)
                      .AsBoolean := False;
                end;
              end;
            end;
          103:
            begin
              for I := 0 to QJson.Count - 1 do
              begin
                for J := 0 to FColumnList.Count - 1 do
                begin
                  Str := FColumnList.Names[J];
                  if Pos(Name, Str) > 0 then
                  begin
                    if QJson.Items[I].ForcePath('Compile').IndexOf(Str) < 0 then
                      QJson.Items[I].ForcePath('Compile.' + Str)
                        .AsBoolean := True
                    else
                    begin
                      Checked := QJson.Items[I].ForcePath('Compile.' + Str)
                        .AsBoolean;
                      QJson.Items[I].ForcePath('Compile.' + Str).AsBoolean :=
                        not Checked;
                    end;
                  end;
                end;
              end;
            end;
        end;
      end;
  end;
  VDT.Repaint;
end;

procedure TForm_Main.PCChange(Sender: TObject);
begin
  if PC.ActivePage = TabProject then
    BtnCompiler.Visible := True
  else
    BtnCompiler.Visible := False;
end;

procedure TForm_Main.PM_PM_NoCompilerByRedClick(Sender: TObject);
var
  I, J: Integer;
  FileName, ColumnName: string;
  QJson: TQjson;
begin
  QJson := FProjectCfg.ForcePath('Project.Files');
  for I := 0 to QJson.Count - 1 do
  begin
    FileName := GetSimpleFileName(QJson.Items[I].ForcePath('FileName')
      .AsString);
    for J := 0 to QJson.Items[I].ForcePath('Compile').Count - 1 do
    begin
      ColumnName := QJson.Items[I].ForcePath('Compile').Items[J].Name;
      if FResultList.ForcePath(FileName).IndexOf(ColumnName) >= 0 then
      begin
        if FResultList.ForcePath(FileName + '.' + ColumnName).AsInteger = -1
        then
          QJson.Items[I].ForcePath('Compile.' + ColumnName).AsBoolean := False;
      end;
    end;
  end;
  VDT.Repaint;
end;

procedure TForm_Main.PM_SortClick(Sender: TObject);
var
  QJson: TQjson;
  List, ListCount: TStringList;
  I, P: Integer;
  FileName, CompleteFileName, Ext, NeedFile: string;
begin
  List := TStringList.Create;
  ListCount := TStringList.Create;
  QJson := FProjectCfg.ForcePath('Project.Files');
  for I := 0 to QJson.Count - 1 do
  begin
    FileName := LowerCase(QJson.Items[I].ForcePath('FileName').AsString);
    Ext := ExtractFileExt(FileName);
    CompleteFileName := GetCompleteFileName(FileName);
    if FExtList.IndexOf(Ext) <= 3 then
    begin
      FileName := GetDProjMainSource(CompleteFileName);
      FileName := GetRequireFile(FileName, False);
      NeedFile := '|';
      while Length(FileName) > 0 do
      begin
        Ext := DecodeTokenW(FileName, '|', '"', True, True, True);
        if FSimpleFileList.IndexOf(LowerCase(Ext)) >= 0 then
        begin
          NeedFile := NeedFile + Ext + '|';
        end;
      end;
      Ext := ExtractFileName(GetDProjMainSource(CompleteFileName));
      Ext := Copy(Ext, 1, Length(Ext) - 4);
    end
    else
    begin
      Ext := ExtractFileName(CompleteFileName);
      NeedFile := '';
    end;
    List.Add(Ext + '=' + NeedFile);
  end;
  for I := 0 to List.Count - 1 do
  begin
    Ext := List.Names[I];
    ListCount.Add(Ext + '=' + IntToStr(LeftStrCount(List.Text,
      '|' + Ext + '|', True)));
  end;
  ListCount.CustomSort(DescSortByValue);
  for I := 0 to ListCount.Count - 1 do
  begin
    Ext := ListCount.Names[I];
    NeedFile := List.Values[Ext];
    ListCount.Strings[I] := Ext + '=' + NeedFile;
  end;
  List.Clear;
  for I := 0 to ListCount.Count - 1 do
  begin
    NeedFile := ListCount.ValueFromIndex[I];
    Ext := ListCount.Names[I];
    while Length(NeedFile) > 0 do
    begin
      Ext := DecodeTokenW(NeedFile, '|', '"', True, True, True);
      if Ext <> '' then
        if List.IndexOf(Ext) < 0 then
          List.Add(Ext);
    end;
    Ext := ListCount.Names[I];
    if List.IndexOf(Ext) < 0 then
      List.Add(Ext);
  end;
  ListCount.Clear;
  for I := 0 to FCompleteFileList.Count - 1 do
  begin
    CompleteFileName := FCompleteFileList.Strings[I];
    FileName := LowerCase(ExtractFileName(CompleteFileName));
    Ext := ExtractFileExt(FileName);
    if FExtList.IndexOf(Ext) <= 3 then
      FileName := Copy(FileName, 1, Length(FileName) - Length(Ext));
    ListCount.Add(FileName);
  end;
  VDT.RootNodeCount := 0;
  for I := 0 to List.Count - 1 do
  begin
    FileName := List.Strings[I];
    P := ListCount.IndexOf(FileName);
    if P <> I then
    begin
      QJson.ExchangeOrder(P, I);
      FCompleteFileList.Exchange(P, I);
      ListCount.Exchange(P, I);
    end;
  end;
  VDT.RootNodeCount := FCompleteFileList.Count;
  List.Free;
  ListCount.Free;
  VDT.Repaint;
end;

function TForm_Main.GetRequireFile(FileName: string;
  NeedBrace: Boolean): string;
var
  List: TStringList;
  Str, Name: string;
  I, P, Pif: Integer;
  Start, Stop: Boolean;
  APcre: TPerlRegEx;
begin
  Start := False;
  Stop := False;
  Pif := 0;
  FileName := GetCompleteFileName(FileName);
  List := TStringList.Create;
  List.LoadFromFile(FileName);
  Str := StringReplaceWithW(List.Text, '/*', '*/', '', False, False);
  Str := LowerCase(StringReplaceWithW(Str, '(*', '*)', '', False, False));
  APcre := TPerlRegEx.Create;
  APcre.RegEx := '\{[^\$\}]*\}';
  APcre.Subject := {$IF RTLVersion > 24}Str{$ELSE}UTF8String(Str){$IFEND};
  APcre.Replacement := '';
  APcre.ReplaceAll;
  Str := string(APcre.Subject);
  List.Text := StringReplaceWithW(Str, '{$ifdef implicitbuilding',
    '{$endif implicitbuilding}', '', False, False);
  Result := '|';
  for I := 0 to List.Count - 1 do
  begin
    Str := List.Strings[I];
    P := Pos('//', Str);
    if P > 0 then
      Str := Copy(Str, 1, P - 1);
    Str := Trim(Str);
    if (Str = 'requires') or (Copy(Str, 1, 9) = 'requires ') or (Str = 'uses')
      or (Copy(Str, 1, 5) = 'uses ') then
    begin
      Start := True;
      if Copy(Str, 1, 4) = 'uses' then
        Str := Trim(Copy(Str, 5, Length(Str) - 4))
      else if Copy(Str, 1, 8) = 'requires' then
        Str := Trim(Copy(Str, 9, Length(Str) - 8));
    end;
    Str := Repl(Str, ' ', '');
    if Start then
    begin
      while (Length(Str) > 0) and Start do
      begin
        Name := DecodeTokenW(Str, ',}{', '"', True, True, True);
        if (Copy(Name, 1, 6) = '$ifend') or (Copy(Name, 1, 6) = '$endif') then
        begin
          Pif := Pif - 1;
        end
        else if Copy(Name, 1, 3) = '$if' then
          Pif := Pif + 1
        else if Copy(Name, 1, 5) <> '$else' then
        begin
          P := Pos(';', Name);
          if (P > 0) then
          begin
            Stop := True;
            Name := Copy(Name, 1, P - 1);
          end;
          if Copy(Name, 1, 1) = '$' then
            Name := '';
          if Length(Name) > 0 then
          begin
            P := Pos('in''', Name);
            if P > 1 then
              Name := Copy(Name, 1, P - 1);
            if (Pif > 0) and NeedBrace then
              Result := Result + '{' + Name + '}|'
            else
              Result := Result + Name + '|';
          end;
        end;
        if (Pif = 0) and Stop then
        begin
          Start := False;
          Stop := False;
        end;
      end;
    end;
  end;
  List.Free;
end;

function TForm_Main.IsDesignOnly(FileName: string): Boolean;
var
  List: TStringList;
  Ext: string;
  I, P: Integer;
begin
  Result := False;
  Ext := LowerCase(ExtractFileExt(FileName));
  I := FExtList.IndexOf(Ext);
  if I in [2, 3, 8] then
  begin
    FileName := GetCompleteFileName(FileName);
    List := TStringList.Create;
    List.LoadFromFile(FileName);
    List.Text := LowerCase(List.Text);
    if I = 2 then
    begin
      if Pos('|designide|', GetRequireFile(FileName)) > 0 then
        Result := True
      else
      begin
        List.Text := StringReplaceWithW(List.Text, '/*', '*/', '',
          False, False);
        List.Text := LowerCase(StringReplaceWithW(List.Text, '(*', '*)', '',
          False, False));
        List.Text := StringReplaceWithW(List.Text, '{$ifdef implicitbuilding',
          '{$endif implicitbuilding}', '', False, False);
        for I := 0 to List.Count - 1 do
        begin
          Ext := List.Strings[I];
          P := Pos('//', Ext);
          if P > 0 then
            Ext := Copy(Ext, 1, P - 1);
          Ext := Trim(Ext);
          if Pos('{$designonly}', Ext) > 0 then
          begin
            Result := True;
            Break;
          end;
        end;
      end;
    end
    else
    begin
      if Pos('<designonlypackage>true</designonlypackage>', List.Text) > 0 then
        Result := True;
    end;
    List.Free;
  end;
end;

function TForm_Main.IsFmxProj(FileName: string): Boolean;
var
  List: TStringList;
  Ext: string;
  I: Integer;
begin
  Result := False;
  Ext := LowerCase(ExtractFileExt(FileName));
  I := FExtList.IndexOf(Ext);
  if I in [3, 8] then
  begin
    Result := False;
    FileName := GetCompleteFileName(FileName);
    List := TStringList.Create;
    if I in [3, 8] then
    begin
      List.LoadFromFile(FileName);
      List.Text := LowerCase(List.Text);
      if Pos('<frameworktype>fmx</frameworktype>', List.Text) > 0 then
        Result := True;
    end;
    List.Free;
  end;
end;

function TForm_Main.GetDProjMainSource(FileName: string): string;
var
  List: TStringList;
  Ext: string;
  P: Integer;
begin
  FileName := GetCompleteFileName(FileName);
  Result := FileName;
  if LowerCase(ExtractFileExt(FileName)) <> '.dproj' then
    Exit;
  List := TStringList.Create;
  List.LoadFromFile(FileName);
  Ext := LowerCase(List.Text);
  P := Pos('<mainsource>', Ext);
  if P > 0 then
  begin
    Ext := Copy(List.Text, P + 12, 255);
    Result := ExtractFilePath(FileName) + DecodeTokenW(Ext, '<', '"', True,
      True, True);
  end;
  List.Free;
end;

procedure TForm_Main.AcceptFiles(var Msg: TMessage);
var
  FileName: array [0 .. 254] of Char;
  Count, I: Word;
begin
  inherited;
  FileName[0] := #0;
  Count := DragQueryFile(Msg.WParam, $FFFFFFFF, nil, 0);
  for I := 0 to Count - 1 do
  begin
    DragQueryFile(Msg.WParam, I, FileName, 255);
    AddFile(FileName);
  end;
  DragFinish(Msg.WParam);
end;

procedure TForm_Main.CreateTabSheet(QJson: TQjson);
var
  Tab: TTabSheet;
  Frame: TFrame_Compiler;
begin
  if not QJson.ForcePath('Valid').AsBoolean then
    Exit;
  if QJson.ForcePath('IDEVersion').AsInteger < 6 then
    Exit;
  Tab := TTabSheet.Create(CompilerPC);
  Tab.PageControl := CompilerPC;
  CompilerPC.ActivePage := Tab;
  Tab.Name := 'Tab' + QJson.ForcePath('IDEVersion').AsString;
  Tab.Caption := QJson.ForcePath('Ver').AsString;
  Frame := TFrame_Compiler.Create(Self);
  Frame.Name := 'Frame' + QJson.ForcePath('IDEVersion').AsString;
  Frame.Parent := Tab;
  Frame.Align := alClient;
  Frame.Init(QJson);
end;

procedure TForm_Main.GetCompilerFromConfig();
var
  I, Count: Integer;
  RootDir: string;
begin
  Count := FCompilerCfg.ForcePath('Compiler').Count;
  for I := 0 to FCustomCfg.ForcePath('Custom.Compiles').Count - 1 do
  begin
    FCompilerCfg.ForcePath('Compiler[' + IntToStr(I + Count) + '].Valid')
      .AsBoolean := True;
    RootDir := FCustomCfg.ForcePath('Custom.Compiles[' + IntToStr(I) +
      '].RootDir').AsString;
    FCompilerCfg.ForcePath('Compiler[' + IntToStr(I + Count) + '].RootDir')
      .AsString := RootDir;
    CheckCompiler(FCompilerCfg.ForcePath('Compiler[' +
      IntToStr(I + Count) + ']'));
  end;
end;

function TForm_Main.ReplPredefined(Path: string; QJson: TQjson): string;
begin
  Path := Repl(Path, '%BDS%', QJson.ForcePath('RootDir').AsString);
  Path := Repl(Path, '%Ver%', QJson.ForcePath('Ver').AsString);
  if QJson.ForcePath('Project.Config').AsBoolean then
    Path := Repl(Path, '%Config%', 'Debug')
  else
    Path := Repl(Path, '%Config%', 'Release');
  if QJson.ForcePath('RTLVersion').AsInteger < 220 then
  begin
    Path := Repl(Path, '%Platform%\', '');
    Path := Repl(Path, '%Platform%', '');
  end
  else
  begin
    Path := Repl(Path, '%Platform%', QJson.ForcePath('Project.PlatformName')
      .AsString);
  end;
  Result := Path;
end;

procedure TForm_Main.CompilerJob(AJob: PQJob);
var
  QJson: TQjson;
  RootDir, Compiler, CompilerOptions, PlatformStr, PlatformName, CmdStr,
    FileName, CompleteFileName, ResultStr, FullResult, ExtName, LibDirs,
    IncludeDirs, OutFinalDir, OutIncludeDir, OutLibDir, ExtraAlias,
    ExtraNameSpaceSearch, ExtraIncludeDir, ExtraLibDir, ExtraSearchDir, Ver,
    BoostDir, BoostDir64, Config, RealFileName: string;
  IDEVersion, MyRTLVersion, MyProductVersion, P: Integer;
  IsShowCMD, IsShowAllLog, IsDebug, IsNameSpace, IsBuildAllUnits,
    IsAddIDEVersion: Boolean;
begin
  QJson := AJob.Data;
  RootDir := QJson.ForcePath('RootDir').AsString;
  BoostDir := QJson.ForcePath('BoostDir').AsString;
  BoostDir64 := QJson.ForcePath('BoostDir64').AsString;
  Ver := QJson.ForcePath('Ver').AsString;
  IsShowCMD := QJson.ForcePath('Project.ShowCMD').AsBoolean;
  IsShowAllLog := QJson.ForcePath('Project.ShowAllLog').AsBoolean;
  IsDebug := QJson.ForcePath('Project.Config').AsBoolean;
  if IsDebug then
    Config := 'Debug'
  else
    Config := 'Release';
  IsNameSpace := QJson.ForcePath('Project.NameSpaces').AsBoolean;
  IsBuildAllUnits := QJson.ForcePath('Project.BuildAllUnits').AsBoolean;
  IsAddIDEVersion := QJson.ForcePath('Project.AddIDEVersion').AsBoolean;
  IDEVersion := QJson.ForcePath('IDEVersion').AsInteger;
  MyRTLVersion := QJson.ForcePath('RTLVersion').AsInteger;
  MyProductVersion := QJson.ForcePath('ProductVersion').AsInteger;
  FileName := QJson.ForcePath('Project.FileName').AsString;
  CompleteFileName := QJson.ForcePath('Project.CompleteFileName').AsString;
  RealFileName := GetDProjMainSource(CompleteFileName);
  ExtName := ExtractFileExt(RealFileName);
  PlatformName := QJson.ForcePath('Project.PlatformName').AsString;
  PlatformStr := QJson.ForcePath('Project.Platform').AsString;
  ExtraAlias := QJson.ForcePath('Project.ExtraAlias').AsString;
  ExtraNameSpaceSearch := QJson.ForcePath
    ('Project.ExtraNameSpaceSearch').AsString;
  ExtraIncludeDir := QJson.ForcePath('Project.ExtraIncludeDir').AsString;
  ExtraLibDir := QJson.ForcePath('Project.ExtraLibDir').AsString;
  ExtraSearchDir := QJson.ForcePath('Project.ExtraSearchDir').AsString;
  OutFinalDir := QJson.ForcePath('Project.OutFinalDir').AsString;
  OutFinalDir := OutFinalDir + '\' + Ver;
  if MyRTLVersion > 210 then
    OutFinalDir := OutFinalDir + '\' + PlatformName;
  OutFinalDir := OutFinalDir + '\' + Config;
  OutIncludeDir := QJson.ForcePath('Project.OutIncludeDir').AsString;
  OutIncludeDir := OutIncludeDir + '\' + Ver;
  if MyRTLVersion > 210 then
    OutIncludeDir := OutIncludeDir + '\' + PlatformName;
  OutIncludeDir := OutIncludeDir + '\' + Config;
  OutLibDir := QJson.ForcePath('Project.OutLibDir').AsString;
  OutLibDir := OutLibDir + '\' + Ver;
  if MyRTLVersion > 210 then
    OutLibDir := OutLibDir + '\' + PlatformName;
  OutLibDir := OutLibDir + '\' + Config;
  IncludeDirs := OutIncludeDir + ';' + ExtraIncludeDir + ';' +
    GetIncludePaths(QJson);
  LibDirs := OutLibDir + ';' + ExtraLibDir + ';' + GetLibPaths(QJson, IsDebug) +
    ';' + ExtraSearchDir;
  IncludeDirs := GetQuotePaths(IncludeDirs);
  LibDirs := GetQuotePaths(LibDirs);
  if not DirectoryExists(OutFinalDir) then
  begin
    ForceDirectories(OutFinalDir);
    if not DirectoryExists(OutFinalDir) then
    begin
      ResultStr := _('Can not create Bin directory') + '(' + Ver + ' ' +
        PlatformName + '):' + OutFinalDir;
      SetCompilerResult(ResultStr);
      Exit;
    end;
  end;
  if not DirectoryExists(OutIncludeDir) then
  begin
    ForceDirectories(OutIncludeDir);
    if not DirectoryExists(OutIncludeDir) then
    begin
      ResultStr := _('Can not create Include directory') + '(' + Ver + ' ' +
        PlatformName + '):' + OutIncludeDir;
      SetCompilerResult(ResultStr);
      Exit;
    end;
  end;
  if not DirectoryExists(OutLibDir) then
  begin
    ForceDirectories(OutLibDir);
    if not DirectoryExists(OutLibDir) then
    begin
      ResultStr := _('Can not create Lib directory') + '(' + Ver + ' ' +
        PlatformName + '):' + OutLibDir;
      SetCompilerResult(ResultStr);
      Exit;
    end;
  end;
  OutFinalDir := GetQuotePaths(OutFinalDir);
  OutIncludeDir := GetQuotePaths(OutIncludeDir);
  OutLibDir := GetQuotePaths(OutLibDir);
  CmdStr := '';
  P := FExtList.IndexOf(ExtName);
  if P < 0 then // not suppert
  begin
    ResultStr := _('This file type is not supported') + '(' + Ver + ' ' +
      PlatformName + '):' + CompleteFileName;
    SetCompilerResult(ResultStr);
    Exit;
  end
  else if P < 3 then // pas dpr dpk
  begin
    if (PlatformStr = '32') or (PlatformStr = '64') then
      Compiler := '"' + RootDir + '\bin\dcc' + PlatformStr + '.exe"'
    else if PlatformStr = 'AD' then
      Compiler := '"' + RootDir + '\bin\dccaarm.exe"'
    else if PlatformStr = 'A6' then
      Compiler := '"' + RootDir + '\bin\dccaarm64.exe"'
    else if PlatformStr = 'IO' then
      Compiler := '"' + RootDir + '\bin\dcciosarm.exe"'
    else if PlatformStr = 'I6' then
      Compiler := '"' + RootDir + '\bin\dcciosarm64.exe"'
    else if PlatformStr = 'OS' then
      Compiler := '"' + RootDir + '\bin\dccosx.exe"'
    else if PlatformStr = 'O6' then
      Compiler := '"' + RootDir + '\bin\dccosx64.exe"';
    if IsDebug then
      CompilerOptions := ' -DDEBUG;_DEBUG -V -VN'
    else
      CompilerOptions := ' -DNDEBUG';
    if (P = 0) and IsNameSpace then
      CompilerOptions := CompilerOptions + ' -JPHNE'
    else if P = 0 then
      CompilerOptions := CompilerOptions + ' -JPHE'
    else if P = 2 then
      CompilerOptions := CompilerOptions + ' -JL';
    if IDEVersion > 15 then
      CompilerOptions := CompilerOptions +
        ' -AGenerics.Collections=System.Generics.Collections;Generics.Defaults=System.Generics.Defaults;WinTypes=Winapi.Windows;WinProcs=Winapi.Windows;DbiTypes=BDE;DbiProcs=BDE;DbiErrs=BDE'
    else
      CompilerOptions := CompilerOptions +
        ' -ADbiTypes=BDE;DbiProcs=BDE;DbiErrs=BDE';
    if Length(ExtraAlias) > 0 then
      CompilerOptions := CompilerOptions + ';' + ExtraAlias;
    CompilerOptions := CompilerOptions +
      ' -NSWinapi;System.Win;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;System;Xml;Data;Datasnap;Web;Soap;Vcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell';
    if Length(ExtraNameSpaceSearch) > 0 then
      CompilerOptions := CompilerOptions + ';' + ExtraNameSpaceSearch;
    CompilerOptions := CompilerOptions + ' -I' + LibDirs + ' -O' + LibDirs +
      ' -R' + LibDirs + ' -U' + LibDirs;
    CompilerOptions := CompilerOptions + ' -E' + OutFinalDir + ' -LE' +
      OutFinalDir + ' -LN' + OutLibDir;
    if IDEVersion < 17 then
      CompilerOptions := CompilerOptions + ' -N0' + OutLibDir
    else
      CompilerOptions := CompilerOptions + ' -NU' + OutLibDir;
    CompilerOptions := CompilerOptions + ' -NB' + OutLibDir + ' -NO' + OutLibDir
      + ' -NH' + OutIncludeDir;
    CompilerOptions := CompilerOptions + ' --BCB';
    if IsBuildAllUnits then
      CompilerOptions := CompilerOptions + ' -B';
  end
  else if P < 6 then
  // C cpp
  begin
    if (PlatformStr = '32') or (PlatformStr = '64') then
      Compiler := '"' + RootDir + '\bin\bcc' + PlatformStr + '.exe"'
    else if PlatformStr = '3C' then
      Compiler := '"' + RootDir + '\bin\bcc32c.exe"'
    else if PlatformStr = 'AD' then
      Compiler := '"' + RootDir + '\bin\bccaarm.exe"'
    else if PlatformStr = 'A6' then
      Compiler := '"' + RootDir + '\bin\bccaarm64.exe"'
    else if PlatformStr = 'IO' then
      Compiler := '"' + RootDir + '\bin\bcciosarm.exe"'
    else if PlatformStr = 'I6' then
      Compiler := '"' + RootDir + '\bin\bcciosarm64.exe"'
    else if PlatformStr = 'OS' then
      Compiler := '"' + RootDir + '\bin\bccosx.exe"'
    else if PlatformStr = 'O6' then
      Compiler := '"' + RootDir + '\bin\bccosx64.exe"';
    if IsDebug then
      CompilerOptions := ' -DDEBUG;_DEBUG -v'
    else
      CompilerOptions := ' -DNDEBUG';
    if PlatformStr = '64' then
      CompilerOptions := CompilerOptions + ' -I' + IncludeDirs + ' -L ' +
        LibDirs + ' -n ' + OutLibDir + ' -c -O2'
    else
    begin
      CompilerOptions := CompilerOptions + ' -I' + IncludeDirs + ' -L' + LibDirs
        + ' -n' + OutLibDir + ' -Q -c';
      if IDEVersion >= 14 then
        CompilerOptions := CompilerOptions + ' -tU -C8 ';
      CompilerOptions := CompilerOptions + ' -w-par -O2 ';

    end;
  end
  else if P < 8 then // bpr bpk
  begin
    if FileExists(RootDir + '\bin\Bpr2Mak.exe') then
    begin
      ResultStr := _('Lack Bpr2Mak.exe, so I do not support this file type') +
        '(' + Ver + ' ' + PlatformName + '):' + CompleteFileName;
      SetCompilerResult(ResultStr);
      Exit;
    end;
    if FileExists(RootDir + '\bin\Make.exe') then
    begin
      ResultStr := _('Lack Make.exe, so I do not support this file type') + '('
        + Ver + ' ' + PlatformName + '):' + CompleteFileName;
      SetCompilerResult(ResultStr);
      Exit;
    end;
    CmdStr := ComSpec + '/c ';
    CompilerOptions := RootDir + '\bin\bpr2mak.exe';
    SetLength(Compiler, MAX_PATH);
    GetShortPathName(PWideChar(CompilerOptions), PWideChar(Compiler),
      MAX_PATH - 1);
    CmdStr := CmdStr + Compiler + ' -o"' + ChangeFileExt(CompleteFileName,
      '.mak') + '" "' + CompleteFileName + '"&';
    CompilerOptions := RootDir + '\bin\make.exe';
    SetLength(Compiler, MAX_PATH);
    GetShortPathName(PWideChar(CompilerOptions), PWideChar(Compiler),
      MAX_PATH - 1);
    CmdStr := CmdStr + Compiler + ' -f"' + ChangeFileExt(CompleteFileName,
      '.mak') + '"';
  end
  else
  // only cbproj    GetDProjMainSource(dproj)=>dpk
  begin
    if Length(DotNETPath) < 1 then
    begin
      ResultStr :=
        _('No install .Net Framework, so I do not support this file type') + '('
        + Ver + ' ' + PlatformName + '):' + CompleteFileName;
      SetCompilerResult(ResultStr);
      Exit;
    end;
    CmdStr := ComSpec + ' /c';
    CmdStr := CmdStr + 'SET BDS=' + RootDir + '&';
    CmdStr := CmdStr + 'SET BDSINCLUDE=' + RootDir + '\Include&';
    if (PlatformStr = '32') and (Length(BoostDir) > 0) then
      CmdStr := CmdStr + 'SET CG_BOOST_ROOT=' + BoostDir + '&'
    else if (PlatformStr = '64') and (Length(BoostDir64) > 0) then
      CmdStr := CmdStr + 'SET CG_64_BOOST_ROOT=' + BoostDir64 + '&';
    Compiler := '"' + DotNETPath + '\MSBuild.exe"';
    CompilerOptions := ' /t:ReBuild /p:Config=' + Config + ';';
    if (PlatformStr = '32') or (PlatformStr = '64') then
      CompilerOptions := CompilerOptions + 'Platform=Win' + PlatformStr
    else if PlatformStr = 'AD' then
      CompilerOptions := CompilerOptions + 'Platform=Android'
    else if PlatformStr = 'A6' then
      CompilerOptions := CompilerOptions + 'Platform=Android64'
    else if (PlatformStr = 'IO') and (IDEVersion < 22) then
      CompilerOptions := CompilerOptions + 'Platform=iOSDevice'
    else if PlatformStr = 'IO' then
      CompilerOptions := CompilerOptions + 'Platform=iOSDevice32'
    else if PlatformStr = 'I6' then
      CompilerOptions := CompilerOptions + 'Platform=iOSDevice64'
    else if PlatformStr = 'OS' then
      CompilerOptions := CompilerOptions + 'Platform=OSX32'
    else if PlatformStr = 'O6' then
      CompilerOptions := CompilerOptions + 'Platform=OSX64';
    if P = 3 then
    begin
      CompilerOptions := CompilerOptions + ';DCC_ExeOutput=' +
        OutFinalDir + '\';
      CompilerOptions := CompilerOptions + ';DCC_BplOutput=' +
        OutFinalDir + '\';
      CompilerOptions := CompilerOptions + ';DCC_HppOutput=' +
        OutIncludeDir + '\';
      CompilerOptions := CompilerOptions + ';DCC_ObjOutput=' + OutLibDir + '\';
      CompilerOptions := CompilerOptions + ';DCC_BpiOutput=' + OutLibDir + '\';
      CompilerOptions := CompilerOptions + ';DCC_DcuOutput=' + OutLibDir + '\';
      CompilerOptions := CompilerOptions + ';DCC_DcpOutput=' + OutLibDir + '\';
      CompilerOptions := CompilerOptions + ';DCC_UnitSearchPath="' +
        GetQuotePaths(LibDirs, False) + '"';
      CompilerOptions := CompilerOptions + ';DCC_CBuilderOutput=All';
    end
    else if P = 8 then
    begin
      CompilerOptions := CompilerOptions + ';IncludePath="' +
        GetQuotePaths(IncludeDirs, False) + '"';
      CompilerOptions := CompilerOptions + ';ILINK_LibraryPath="' +
        GetQuotePaths(LibDirs, False) + '"';
      CompilerOptions := CompilerOptions + ';FinalOutputDir=' +
        OutFinalDir + '\';
      CompilerOptions := CompilerOptions + ';IntermediateOutputDir=' +
        OutLibDir + '\';
      CompilerOptions := CompilerOptions + ';BPILibOutputDir=' +
        OutLibDir + '\';
    end;
  end;
  if not((P = 6) or (P = 7)) then
    CmdStr := CmdStr + Compiler + ' ' + CompilerOptions + ' "' +
      RealFileName + '"';
  ResultStr := _('Start compiling ') + '(' + Ver + ' ' + PlatformName + ' VER' +
    IntToStr(MyRTLVersion) + ' RTLVersion=' +
    Format('%5.1f', [MyRTLVersion / 10.0]) + ' IDEVersion=' +
    IntToStr(IDEVersion) + ' ProductVersion=' + IntToStr(MyProductVersion) + ')'
    + ExtractFileName(FileName);
  ResultStr := ResultStr + #13 + #10 + SeptalLine;
  if IsAddIDEVersion and (P = 2) then
  begin
    FullResult := CheckDPKSuffix(RealFileName, IDEVersion);
    if Length(FullResult) > 0 then
    begin
      ResultStr := ResultStr + #13 + #10 +
        _('Adding IDE version number failed, error description') + ':' +
        FullResult + #13 + #10 + SeptalLine;
    end;
  end;
  FullLogTxt := FullLogTxt + ResultStr + #13 + #10 + CmdStr + #13 + #10 +
    SeptalLine;
  if IsShowCMD then
  begin
    ResultStr := ResultStr + #13 + #10 + CmdStr + #13 + #10 + SeptalLine;
  end;
  SetCompilerResult(ResultStr);
  ResultStr := PipeCall(CmdStr,
    ExcludeTrailingPathDelimiter(ExtractFilePath(RealFileName)));
  FullLogTxt := FullLogTxt + #13 + #10 + ResultStr + #13 + #10 + SeptalLine +
    #13 + #10 + #13 + #10;
  FullResult := ResultStr;
  CheckResult(ResultStr, FileName, QJson.ForcePath('Project.ColumnName')
    .AsString);
  if not IsShowAllLog then
    ResultStr := FilterResult(FullResult);
  ResultStr := ResultStr + #13 + #10 + SeptalLine + #13 + #10 + #13 + #10;
  SetCompilerResult(ResultStr);
  Sleep(100);
end;

procedure TForm_Main.SetCompilerResult(Str: string);
var
  P: PCompilerResult;
begin
  New(P);
  P.Result := Str;
  Workers.Post(ShowCompilerResult, P, True);
end;

procedure TForm_Main.ShowCompilerResult(AJob: PQJob);
var
  P: PCompilerResult;
begin
  P := AJob.Data;
  Log.Lines.Add(P.Result);
  VDT.Repaint;
end;

function TForm_Main.GetIncludePaths(QJson: TQjson): string;
var
  IncludeDirs, RootDir, PlatformStr, BoostDir, BoostDir64: string;
  IDEVersion: Integer;
begin
  RootDir := QJson.ForcePath('RootDir').AsString;
  PlatformStr := QJson.ForcePath('Project.Platform').AsString;
  BoostDir := QJson.ForcePath('BoostDir').AsString;
  BoostDir64 := QJson.ForcePath('BoostDir64').AsString;
  IDEVersion := QJson.ForcePath('IDEVersion').AsInteger;
  if (IDEVersion > 14) and (PlatformStr = '32') then
  begin
    IncludeDirs := RootDir + '\include\windows\crtl';
    IncludeDirs := IncludeDirs + ';' + RootDir + '\include\windows\rtl';
    IncludeDirs := IncludeDirs + ';' + RootDir + '\include\windows\vcl';
    IncludeDirs := IncludeDirs + ';' + RootDir + '\include\windows\fmx';
    IncludeDirs := IncludeDirs + ';' + RootDir + '\include\dinkumware';
    if Length(BoostDir) > 0 then
    begin
      IncludeDirs := IncludeDirs + ';' + BoostDir + '\boost\tr1\tr1';
    end;
  end
  else if (IDEVersion > 22) and (PlatformStr = '3C') then // Seattle
  begin
    IncludeDirs := RootDir + '\include\windows\crtl';
    IncludeDirs := IncludeDirs + ';' + RootDir + '\include\windows\rtl';
    IncludeDirs := IncludeDirs + ';' + RootDir + '\include\windows\vcl';
    IncludeDirs := IncludeDirs + ';' + RootDir + '\include\windows\fmx';
    IncludeDirs := IncludeDirs + ';' + RootDir + '\include\dinkumware64';
    if Length(BoostDir64) > 0 then
    begin
      IncludeDirs := IncludeDirs + ';' + BoostDir64 + '\boost\tr1\tr1';
    end;
  end
  else if (IDEVersion > 14) and (PlatformStr = '64') then
  begin
    IncludeDirs := RootDir + '\include\windows\crtl';
    IncludeDirs := IncludeDirs + ';' + RootDir + '\include\windows\rtl';
    IncludeDirs := IncludeDirs + ';' + RootDir + '\include\windows\vcl';
    IncludeDirs := IncludeDirs + ';' + RootDir + '\include\windows\fmx';
    IncludeDirs := IncludeDirs + ';' + RootDir + '\include\dinkumware64';
    if Length(BoostDir64) > 0 then
    begin
      IncludeDirs := IncludeDirs + ';' + BoostDir64 + '\boost\tr1\tr1';
    end;
  end
  else if (IDEVersion > 14) and (PlatformStr = 'AD') then
  begin
    IncludeDirs := RootDir + '\include\android\crtl';
    IncludeDirs := IncludeDirs + ';' + RootDir + '\include\android\rtl';
    IncludeDirs := IncludeDirs + ';' + RootDir + '\include\android\fmx';
  end
  else if (IDEVersion > 14) and (PlatformStr = 'A6') then
  begin
    IncludeDirs := RootDir + '\include\android\crtl';
    IncludeDirs := IncludeDirs + ';' + RootDir + '\include\android\rtl';
    IncludeDirs := IncludeDirs + ';' + RootDir + '\include\android\fmx';
  end
  else if (IDEVersion > 14) and (PlatformStr = 'IO') then
  begin
    IncludeDirs := RootDir + '\include\ios\crtl';
    IncludeDirs := IncludeDirs + ';' + RootDir + '\include\ios\rtl';
    IncludeDirs := IncludeDirs + ';' + RootDir + '\include\ios\fmx';
  end
  else if (IDEVersion > 14) and (PlatformStr = 'I6') then
  begin
    IncludeDirs := RootDir + '\include\ios\crtl';
    IncludeDirs := IncludeDirs + ';' + RootDir + '\include\ios\rtl';
    IncludeDirs := IncludeDirs + ';' + RootDir + '\include\ios\fmx';
  end
  else if (IDEVersion > 14) and (PlatformStr = 'OS') then
  begin
    IncludeDirs := RootDir + '\include\osx\crtl';
    IncludeDirs := IncludeDirs + ';' + RootDir + '\include\osx\rtl';
    IncludeDirs := IncludeDirs + ';' + RootDir + '\include\osx\fmx';
  end
  else if (IDEVersion > 14) and (PlatformStr = '64') then
  begin
    IncludeDirs := RootDir + '\include\osx\crtl';
    IncludeDirs := IncludeDirs + ';' + RootDir + '\include\osx\rtl';
    IncludeDirs := IncludeDirs + ';' + RootDir + '\include\osx\fmx';
  end
  else // 6~2010
  begin
    IncludeDirs := RootDir + '\include';
    IncludeDirs := IncludeDirs + ';' + RootDir + '\include\vcl';
    IncludeDirs := IncludeDirs + ';' + RootDir + '\include\dinkumware';
    IncludeDirs := IncludeDirs + ';' + RootDir + '\include\indy10';
    if Length(BoostDir) > 0 then
    begin
      IncludeDirs := IncludeDirs + ';' + BoostDir + '\boost\tr1\tr1';
    end;
  end;
  Result := IncludeDirs;
end;

function TForm_Main.GetLibPaths(QJson: TQjson; IsDebug: Boolean): string;
var
  LibDirs, RootDir, PlatformStr, Config: string;
  IDEVersion: Integer;
begin
  RootDir := QJson.ForcePath('RootDir').AsString;
  PlatformStr := QJson.ForcePath('Project.Platform').AsString;
  IDEVersion := QJson.ForcePath('IDEVersion').AsInteger;
  if IsDebug then
    Config := 'debug'
  else
    Config := 'release';
  if (IDEVersion > 14) and (PlatformStr = '32') then
  begin
    LibDirs := RootDir + '\lib\win32\' + Config;
    if not IsDebug then
      LibDirs := LibDirs + ';' + RootDir + '\lib\win32\' + Config + '\psdk';
    if IsDebug then
      LibDirs := LibDirs + ';' + RootDir + '\lib\win32\release';
  end
  else if (IDEVersion > 22) and (PlatformStr = '3C') then
  begin
    LibDirs := RootDir + '\lib\win32c\' + Config;
    LibDirs := LibDirs + ';' + RootDir + '\lib\win32\' + Config;
    if not IsDebug then
      LibDirs := LibDirs + ';' + RootDir + '\lib\win32c\' + Config + '\psdk';
    if IsDebug then
      LibDirs := LibDirs + ';' + RootDir + '\lib\win32\release';
  end
  else if (IDEVersion > 14) and (PlatformStr = '64') then
  begin
    LibDirs := RootDir + '\lib\win64\' + Config;
    if not IsDebug then
      LibDirs := LibDirs + ';' + RootDir + '\lib\win64\' + Config + '\psdk';
    if IsDebug then
      LibDirs := LibDirs + ';' + RootDir + '\lib\win64\release';
  end
  else if (IDEVersion > 14) and (PlatformStr = 'AD') then
  begin
    LibDirs := RootDir + '\lib\android\' + Config;
    if IsDebug then
      LibDirs := LibDirs + ';' + RootDir + '\lib\android\release';
  end
  else if (IDEVersion > 14) and (PlatformStr = 'A6') then
  begin
    LibDirs := RootDir + '\lib\android64\' + Config;
    if IsDebug then
      LibDirs := LibDirs + ';' + RootDir + '\lib\android64\release';
  end
  else if (IDEVersion > 21) and (PlatformStr = 'IO') then
  begin
    LibDirs := RootDir + '\lib\iosDevice32\' + Config;
    if IsDebug then
      LibDirs := LibDirs + ';' + RootDir + '\lib\iosDevice32\release';
  end
  else if (IDEVersion > 14) and (PlatformStr = 'IO') then
  begin
    LibDirs := RootDir + '\lib\iosDevice\' + Config;
    if IsDebug then
      LibDirs := LibDirs + ';' + RootDir + '\lib\iosDevice\release';
  end
  else if (IDEVersion > 14) and (PlatformStr = 'I6') then
  begin
    LibDirs := RootDir + '\lib\iosDevice64\' + Config;
    if IsDebug then
      LibDirs := LibDirs + ';' + RootDir + '\lib\iosDevice64\release';
  end
  else if (IDEVersion > 14) and (PlatformStr = 'OS') then
  begin
    LibDirs := RootDir + '\lib\osx32\' + Config;
    if IsDebug then
      LibDirs := LibDirs + ';' + RootDir + '\lib\osx32\release';
  end
  else if (IDEVersion > 14) and (PlatformStr = '64') then
  begin
    LibDirs := RootDir + '\lib\osx64\' + Config;
    if IsDebug then
      LibDirs := LibDirs + ';' + RootDir + '\lib\osx64\release';
  end
  else
  // 6~2010
  begin
    if IsDebug then
    begin
      LibDirs := RootDir + '\lib\debug';
      LibDirs := LibDirs + ';' + RootDir + '\lib\obj';
      LibDirs := LibDirs + ';' + RootDir + '\lib\psdk';
      LibDirs := LibDirs + ';' + RootDir + '\lib\debug\indy10';
      LibDirs := LibDirs + ';' + RootDir + '\lib';
    end
    else
    begin
      LibDirs := RootDir + '\lib';
      LibDirs := LibDirs + ';' + RootDir + '\lib\obj';
      LibDirs := LibDirs + ';' + RootDir + '\lib\' + Config;
      LibDirs := LibDirs + ';' + RootDir + '\lib\psdk';
      LibDirs := LibDirs + ';' + RootDir + '\lib\indy10';
    end;

  end;
  Result := LibDirs;
end;

procedure TForm_Main.GetDotNETPath();
var
  Reg: TRegistry;
  DotNETVer: string;
  sr: TSearchRec;
begin
  Reg := TRegistry.Create(KEY_READ);
  Reg.RootKey := HKEY_LOCAL_MACHINE;
  if Reg.OpenKey('SOFTWARE\Microsoft\.NETFramework', False) then
    if Reg.ValueExists('InstallRoot') then
      DotNETPath := ExcludeTrailingPathDelimiter(Reg.ReadString('InstallRoot'));
  if DotNETPath = '' then
    Exit;
  if FindFirst(DotNETPath + '\*.*', faAnyFile, sr) = 0 then
  begin
    repeat
    begin
      if ((sr.Attr and faDirectory) <> 0) and (sr.Name <> '.') and
        (sr.Name <> '..') then
        if FileExists(DotNETPath + '\' + sr.Name + '\MsBuild.exe  ') then
          DotNETVer := sr.Name;
    end;
    until (FindNext(sr) <> 0);
    FindClose(sr);
  end;
  if Length(DotNETVer) < 1 then
  begin
    DotNETPath := '';
    Exit;
  end;
  DotNETPath := DotNETPath + '\' + DotNETVer;
end;

function TForm_Main.CheckDPKSuffix(FileName: string;
  IDEVersion: Integer): string;
var
  List: TStringList;
  Text: string;
  P, P2: Integer;
begin
  List := TStringList.Create;
  Result := '';
  List.LoadFromFile(FileName);
  Text := StringReplaceWithW(List.Text, '/*', '*/', '', False, False);
  Text := LowerCase(StringReplaceWithW(Text, '(*', '*)', '', False, False));
  Text := StringReplaceWithW(Text, '{$ifdef implicitbuilding',
    '{$endif implicitbuilding}', '', False, False);
  if Pos('{$libsuffix ', Text) = 0 then
  begin
    P := Pos('{$implicitbuild off}', LowerCase(List.Text));
    if P = 0 then
      P := Pos('{$implicitbuild on}', LowerCase(List.Text));
    if P = 0 then
      Result := _('Not find registration marks') +
        ': ''{$IMPLICITBUILD OFF}'' !'
    else
    begin
      List.Text := Copy(List.Text, 1, P - 1) + '{$LIBSUFFIX ''' +
        IntToStr(IDEVersion * 10) + '''}' + #13 + #10 +
        Copy(List.Text, P, Length(List.Text) - P);
      try
        List.SaveToFile(FileName);
      except
        on e: Exception do
          Result := e.Message;
      end;
    end;
  end
  else
  begin
    P := Pos('{$libsuffix ', LowerCase(List.Text));
    if P = 0 then
      Result := _('Not find registration marks') + ': ''{$LIBSUFFIX '' !'
    else
    begin
      Text := Copy(List.Text, P, Length(List.Text) - P);
      P2 := Pos('}', Text);
      Text := Copy(Text, P2, Length(Text) - P2);
      List.Text := Copy(List.Text, 1, P - 1 + 12) + '''' +
        IntToStr(IDEVersion * 10) + '''' + Text;
      try
        List.SaveToFile(FileName);
      except
        on e: Exception do
          Result := e.Message;
      end;
    end;
  end;
end;

procedure TForm_Main.CheckResult(ResultStr, FileName, ColumnName: string);
begin
  ResultStr := LowerCase(ResultStr);
  FileName := GetSimpleFileName(FileName);
  if (Pos('error: ', ResultStr) > 0) or (Pos('error e', ResultStr) > 0) or
    (Pos('): error ', ResultStr) > 0) or (Pos('fatal: ', ResultStr) > 0) or
    (Pos('fatal f', ResultStr) > 0) or (Pos('): fatal ', ResultStr) > 0) then
  begin
    FResultList.ForcePath(FileName + '.' + ColumnName).AsInteger := -1;
    ErrorTimes := ErrorTimes + 1;
  end
  else
  begin
    FResultList.ForcePath(FileName + '.' + ColumnName).AsInteger := 1;
    SuccessTimes := SuccessTimes + 1;
  end;
end;

function TForm_Main.FilterResult(ResultStr: string): string;
var
  List: TStringList;
  Str: string;
  IsSuccess: Boolean;
  I: Integer;
begin
  List := TStringList.Create;
  List.Text := ResultStr;
  IsSuccess := True;
  Result := '';
  for I := 0 to List.Count - 1 do
  begin
    Str := LowerCase(Trim(List.Strings[I]));
    if (Pos('error: ', Str) > 0) or (Pos('error e', Str) > 0) or
      (Pos('): error ', Str) > 0) or (Pos('fatal: ', Str) > 0) or
      (Pos('fatal f', Str) > 0) or (Pos('): fatal ', Str) > 0) then
    begin
      Result := Result + List.Strings[I] + #13 + #10;
      IsSuccess := False;
    end
    else if (Pos('hint: ', Str) > 0) or (Pos('hint h', Str) > 0) or
      (Pos('): hint ', Str) > 0) or (Pos('warning: ', Str) > 0) or
      (Pos('warning w', Str) > 0) or (Pos('): warning ', Str) > 0) then
    begin
      Result := Result + List.Strings[I] + #13 + #10;
    end;
  end;
  if Length(Result) > 0 then
    Result := Result + SeptalLine + #13 + #10;
  if IsSuccess then
    Result := Result + _('Compile success') + '!'
  else
    Result := Result + _('Compile failed') + '!'
end;

function TForm_Main.GetSimpleFileName(FileName: string): string;
var
  ExtName: string;
begin
  FileName := ExtractFileName(FileName);
  ExtName := LowerCase(ExtractFileExt(FileName));
  if FExtList.IndexOf(ExtName) <= 3 then
    FileName := Copy(FileName, 1, Length(FileName) - Length(ExtName));
  Result := FileName;
end;

procedure TForm_Main.DoLanguageChanged(Sender: TObject);
var
  hSysMenu: HMENU;
  MenuCount, I, J, MenuID: Integer;
  ParentItem: TMenuItem;
  procedure ChangeThreemenu(Item: TMenuItem);
  var
    K: Integer;
  begin
    if Item.Count > 0 then
    begin
      for K := 0 to Item.Count - 1 do
      begin
        if Item.Items[K].Tag = 101 then
          Item.Items[K].Caption := _('Select All')
        else if Item.Items[K].Tag = 102 then
          Item.Items[K].Caption := _('Clear All')
        else if Item.Items[K].Tag = 103 then
          Item.Items[K].Caption := _('Unselected');
      end;
    end;
  end;

begin
  hSysMenu := GetSystemMenu(Handle, False);
  MenuCount := GetMenuItemCount(hSysMenu);
  for I := 0 to MenuCount - 1 do
  begin
    MenuID := GetMenuItemID(hSysMenu, I);
    if MenuID = 200 then
    begin
      ModifyMenu(hSysMenu, I, MF_STRING or MF_BYPOSITION, 200,
        PWideChar(_('About') + '(&A)'));
    end
    else if MenuID = 201 then
    begin
      ModifyMenu(hSysMenu, I, MF_STRING or MF_BYPOSITION, 201,
        PWideChar(_('Only read from the Data.Cfg')));
    end;
  end;
  for I := 0 to PM.Items.Count - 1 do
  begin
    ParentItem := PM.Items.Items[I];
    if ParentItem.Tag = 1 then
    begin
      ParentItem.Caption := _('All Ranks');
      ChangeThreemenu(ParentItem);
    end
    else if ParentItem.Tag = 2 then
    begin
      ParentItem.Caption := _('Current Row');
      ChangeThreemenu(ParentItem);
    end
    else if ParentItem.Tag = 3 then
    begin
      ParentItem.Caption := _('Current Column');
      ChangeThreemenu(ParentItem);
    end
    else if ParentItem.Tag = 4 then
    begin
      ParentItem.Caption := _('IDE Version');
      for J := 0 to ParentItem.Count - 1 do
        ChangeThreemenu(ParentItem.Items[J]);
    end
    else if ParentItem.Tag = 5 then
    begin
      ParentItem.Caption := _('Platform');
      for J := 0 to ParentItem.Count - 1 do
        ChangeThreemenu(ParentItem.Items[J]);
    end
    else if ParentItem.Tag = 6 then
    begin
      ParentItem.Caption := _('Sort by dependency(Only Delphi Files)');
    end
    else if ParentItem.Tag = 7 then
    begin
      ParentItem.Caption := _('No compilation error file');
    end;

  end;

  VDT.Header.Columns.Items[0].Text := _('No');
  VDT.Header.Columns.Items[1].Text := _('Use');
  VDT.Header.Columns.Items[2].Text := _('FileName');
end;

procedure TForm_Main.SysMenuCommand(var Msg: TWMMENUSELECT);
begin
  if Msg.IDItem = 200 then
  begin
    ShowInformationMessage('Command-line compiler tool : ' +
      GetVersionString(Application.ExeName) + #13 + #10 + ' By WangStudio''s!',
      _('About'));
  end
  else if Msg.IDItem = 201 then
  begin
    OnlyFromConfig := not OnlyFromConfig;
    if OnlyFromConfig then
      CheckMenuItem(GetSystemMenu(Handle, False), 201, MF_CHECKED)
    else
      CheckMenuItem(GetSystemMenu(Handle, False), 201, MF_UNCHECKED);
    FCustomCfg.ForcePath('OnlyFromConfig').AsBoolean := OnlyFromConfig;
    FCustomCfg.SaveToFile(ExtractFilePath(Application.ExeName) +
      'Data.cfg', teUTF8);
  end;
  Inherited;
end;

procedure TForm_Main.DelayRunWithCmdline(AJob: PQJob);
begin
  if RunWithCmdline then
  begin
    if FindCmdLineSwitch('Q', True) then
      Application.Terminate;
  end;
end;

function TForm_Main.RunWithCmdline(): Boolean;
var
  Temp, Param, SID, OutPath, Ver, PlatformStr: string;
  Masks: TStringList;
  I: Integer;
begin
  Masks := TStringList.Create;

  if FindCmdLineSwitch('A', Param, True) then
  begin
    if ExtraAlias.Text <> '' then
      ExtraAlias.Text := ExtraAlias.Text + ';';
    ExtraAlias.Text := ExtraAlias.Text + Param;
  end;

  if FindCmdLineSwitch('C', Param, True) then
  begin
    LoadProject(Param);
  end;

  if FindCmdLineSwitch('D', Param, True) then
  begin
    BuildDebug.Checked := True;
  end;

  if FindCmdLineSwitch('O', Param, True) then
  begin
    OutPath := IncludeTrailingPathDelimiter(Param);
  end;
  if FindCmdLineSwitch('SID', Param, True) then
  begin
    SID := Param;
  end;
  if OutPath <> '' then
  begin
    if SID <> '' then
      OutPath := OutPath + SID + '\';
    OutFinalDir.Text := OutPath + 'Bin';
    OutIncludeDir.Text := OutPath + 'Include';
    OutLibDir.Text := OutPath + 'Lib';
  end;

  if FindCmdLineSwitch('LS', Param, True) then
  begin
    DpkAddIDEVersion.Checked := True;
  end;

  if FindCmdLineSwitch('M', Param, True) then
  begin
    if Length(Param) > 0 then
    begin
      while Length(Param) > 0 do
      begin
        Temp := Trim(DecodeTokenW(Param, ';,', '"', True, True, True));
        if Temp <> '' then
          Masks.Add(Temp);
      end;
    end;
  end;
  if FindCmdLineSwitch('I', Param, True) then
  begin
    if Length(Param) > 0 then
    begin
      while Length(Param) > 0 do
      begin
        Temp := Trim(DecodeTokenW(Param, ';,', '"', True, True, True));
        if Temp <> '' then
        begin
          Temp := GetCompleteFileName(Temp);
          if FileOrDir(Temp) = 2 then
          begin
            Temp := Repl(IncludeTrailingPathDelimiter(Temp), '/', '\');
            ScanFiles(Temp, Masks);
          end
          else if FileOrDir(Temp) = 1 then
          begin
            AddFile(Temp);
          end;
        end;
      end;
    end;
  end;

  if FindCmdLineSwitch('NB', Param, True) then
  begin
    ExtraBuildAllUnits.Checked := False;
  end;

  if FindCmdLineSwitch('NN', Param, True) then
  begin
    ExtraNameSpaces.Checked := False;
  end;

  if FindCmdLineSwitch('P', Param, True) then
  begin
    if Length(Param) > 0 then
    begin
      PlatformStr := '|';
      while Length(Param) > 0 do
      begin
        Temp := Trim(DecodeTokenW(Param, ';,', '"', True, True, True));
        if Temp <> '' then
        begin
          if SameText(Temp, 'win32') or SameText(Temp, 'w32') or
            SameText(Temp, '32') then
            PlatformStr := PlatformStr + '32|'
          else if SameText(Temp, 'win64') or SameText(Temp, 'w64') or
            SameText(Temp, '64') then
            PlatformStr := PlatformStr + '64|'
          else if SameText(Temp, 'android') or SameText(Temp, 'ad') or
            SameText(Temp, 'android32') or SameText(Temp, 'an') or
            SameText(Temp, 'an32') then
            PlatformStr := PlatformStr + 'AD|'
          else if SameText(Temp, 'android64') or SameText(Temp, 'a6') or
            SameText(Temp, 'an64') or SameText(Temp, 'ad64') then
            PlatformStr := PlatformStr + 'A6|'
          else if SameText(Temp, 'ios') or SameText(Temp, 'io') or
            SameText(Temp, 'ios32') then
            PlatformStr := PlatformStr + 'IO|'
          else if SameText(Temp, 'ios64') or SameText(Temp, 'i6') then
            PlatformStr := PlatformStr + 'I6|'
          else if SameText(Temp, 'osx32') or SameText(Temp, 'os') or
            SameText(Temp, 'osx') then
            PlatformStr := PlatformStr + 'OS|'
          else if SameText(Temp, 'osx64') or SameText(Temp, 'o6') then
            PlatformStr := PlatformStr + 'O6|';
        end;
      end;
    end;
  end;

  if FindCmdLineSwitch('V', Param, True) then
  begin
    if Length(Param) > 0 then
    begin
      Ver := '|';
      while Length(Param) > 0 do
      begin
        Temp := Trim(DecodeTokenW(Param, ';,', '"', True, True, True));
        if Temp <> '' then
        begin
          I := Trunc(StrToFloatDef(Temp, 0.0));
          if (Copy(Temp, 1, 1) = 'C') or (Copy(Temp, 1, 1) = 'D') or
            (Copy(Temp, 1, 2) = 'RS') then
            Ver := Ver + Temp + '|'
          else if Copy(Temp, 1, 2) = 'XE' then
            Ver := Ver + 'RS' + Temp + '|'
          else if I > 2000 then
            Ver := Ver + 'RS' + Temp + '|'
          else if I >= 10 then
            Ver := Ver + 'RS' + Temp + '|'
          else if I > 6 then
            Ver := Ver + 'D' + Temp + '|'
          else if I > 0 then
            Ver := Ver + 'C' + Temp + '|' + 'D' + Temp + '|';
        end;
      end;
    end;
  end;

  SelectVerAndPlatform(Ver, PlatformStr);

  if FindCmdLineSwitch('XI', Param, True) then
  begin
    if ExtraIncludeDir.Text <> '' then
      ExtraIncludeDir.Text := ExtraIncludeDir.Text + ';';
    ExtraIncludeDir.Text := ExtraIncludeDir.Text + Param;
  end;

  if FindCmdLineSwitch('XL', Param, True) then
  begin
    if ExtraLibDir.Text <> '' then
      ExtraLibDir.Text := ExtraLibDir.Text + ';';
    ExtraLibDir.Text := ExtraLibDir.Text + Param;
  end;

  if FindCmdLineSwitch('XS', Param, True) then
  begin
    if ExtraSearchDir.Text <> '' then
      ExtraSearchDir.Text := ExtraSearchDir.Text + ';';
    ExtraSearchDir.Text := ExtraSearchDir.Text + Param;
  end;

  Result := True;

  if VDT.RootNodeCount = 0 then
    Result := False;
  if OutFinalDir.Text = '' then
    Result := False;
  if OutIncludeDir.Text = '' then
    Result := False;
  if OutLibDir.Text = '' then
    Result := False;

  if Result then
  begin
    PM_Sort.Click;
    BtnCompiler.Click;
    if OutPath <> '' then
    begin
      SaveTextU(OutPath + 'Result.log', FullLogTxt);
      GenerateHTMLReport(OutPath);
      if FindCmdLineSwitch('LR', True) then
        ShellExecute(Handle, nil, PWideChar(OutPath + 'Report.html'), nil, nil,
          SW_SHOWNORMAL);
    end;

  end;
  Masks.Free;
end;

procedure TForm_Main.ScanFiles(Source: string; Masks: TStringList);
var
  sr: TSearchRec;
  I: Integer;
begin
  Source := IncludeTrailingPathDelimiter(Source);
  if FindFirst(Source + '*.*', faAnyFile, sr) = 0 then
  begin
    repeat
    begin
      if ((sr.Attr and faDirectory) <> 0) and (sr.Name <> '.') and
        (sr.Name <> '..') then
      begin
        ScanFiles(Source + sr.Name, Masks);
      end
      else if (sr.Attr and faDirectory) = 0 then
      begin
        if Masks.Count = 0 then
          AddFile(Source + sr.Name)
        else
        begin
          for I := 0 to Masks.Count - 1 do
          begin
            if StrLikeW(PWideChar(sr.Name), PWideChar(Masks.Strings[I]), True)
            then
              AddFile(Source + sr.Name);
          end;
        end;
      end;
    end;
    until (FindNext(sr) <> 0);
    FindClose(sr);
  end;
end;

procedure TForm_Main.SelectVerAndPlatform(Ver, PlatformStr: string);
var
  ColumnName, V, P: string;
  I, J: Integer;
  QJson: TQjson;
begin
  if (Ver = '') and (PlatformStr = '') then
    Exit
  else if Ver = '' then
  begin
    for I := 0 to PM_Platform.Count - 1 do
    begin
      P := PM_Platform.Items[I].Caption;
      if P = 'Win32' then
        P := '32'
      else if P = 'Win64' then
        P := '64'
      else if P = 'Android' then
        P := 'AD'
      else if P = 'Android64' then
        P := 'A6'
      else if P = 'IOS' then
        P := 'IO'
      else if P = 'IOS64' then
        P := 'I6'
      else if P = 'OSX' then
        P := 'OS'
      else if P = 'OSX64' then
        P := 'O6';
      if Pos('|' + P + '|', PlatformStr) > 0 then
      begin
        for J := 0 to PM_Platform.Items[I].Count - 1 do
        begin
          if PM_Platform.Items[I].Items[J].Tag = 101 then
          begin
            PM_Platform.Items[I].Items[J].Click;
            Break;
          end;
        end;
      end
      else
      begin
        for J := 0 to PM_Platform.Items[I].Count - 1 do
        begin
          if PM_Platform.Items[I].Items[J].Tag = 102 then
          begin
            PM_Platform.Items[I].Items[J].Click;
            Break;
          end;
        end;
      end;
    end;
  end
  else if PlatformStr = '' then
  begin
    for I := 0 to PM_Ver.Count - 1 do
    begin
      V := PM_Ver.Items[I].Caption;
      if Pos('|' + V + '|', Ver) > 0 then
      begin
        for J := 0 to PM_Ver.Items[I].Count - 1 do
        begin
          if PM_Ver.Items[I].Items[J].Tag = 101 then
          begin
            PM_Ver.Items[I].Items[J].Click;
            Break;
          end;
        end;
      end
      else
      begin
        for J := 0 to PM_Ver.Items[I].Count - 1 do
        begin
          if PM_Ver.Items[I].Items[J].Tag = 102 then
          begin
            PM_Ver.Items[I].Items[J].Click;
            Break;
          end;
        end;
      end;
    end;
  end
  else
  begin
    Ver := Repl(Ver, '.', '_');
    for I := 0 to FProjectCfg.ForcePath('Project.Files').Count - 1 do
    begin
      QJson := FProjectCfg.ForcePath('Project.Files[' + IntToStr(I) + ']');
      for J := 0 to QJson.ForcePath('Compile').Count - 1 do
      begin
        ColumnName := QJson.ForcePath('Compile').Items[J].Name;
        V := Copy(ColumnName, 1, Length(ColumnName) - 3);
        P := Copy(ColumnName, Length(ColumnName) - 1, 2);
        if (Pos('|' + V + '|', Ver) > 0) and
          (Pos('|' + P + '|', PlatformStr) > 0) then
          QJson.ForcePath('Compile').Items[J].AsBoolean := True
        else
          QJson.ForcePath('Compile').Items[J].AsBoolean := False;
      end;
    end;
  end;

end;

procedure TForm_Main.GenerateHTMLReport(Path: string);
var
  ABuilder: TQStringCatHelperW;
  ATitle, FileName, ColumnName: string;
  I, J: Integer;
  QJson: TQjson;
begin
  ABuilder := TQStringCatHelperW.Create;
  ATitle := _('Compilation Report') + FormatDateTime
    ('yyyy-mm-dd hh:nn:ss', Now);
  ABuilder.Cat('<!Doctype html><HTML>', -1);
  ABuilder.Cat('<head>' + #13 + #10, -1);
  ABuilder.Cat
    ('<meta http-equiv=Content-Type content="text/html;charset=utf-8">' + #13
    + #10, -1);
  ABuilder.Cat('<title>' + ATitle + '</title>' + #13 + #10);
  ABuilder.Cat('<style type="text/css">' + #13 + #10, -1);
  ABuilder.Cat('table.gridtable ' + #13 + #10, -1);
  ABuilder.Cat('{' + #13 + #10, -1);
  ABuilder.Cat('font-family: verdana,arial,sans-serif;' + #13 + #10, -1);
  ABuilder.Cat('font-size:11px;' + #13 + #10, -1);
  ABuilder.Cat('color:#333333;' + #13 + #10, -1);
  ABuilder.Cat('border-width: 1px;' + #13 + #10, -1);
  ABuilder.Cat('border-color: #666666;' + #13 + #10, -1);
  ABuilder.Cat('border-collapse: collapse;' + #13 + #10, -1);
  ABuilder.Cat('}' + #13 + #10, -1);
  ABuilder.Cat('table.gridtable thead ' + #13 + #10, -1);
  ABuilder.Cat('{' + #13 + #10, -1);
  ABuilder.Cat('border-width: 1px;' + #13 + #10, -1);
  ABuilder.Cat('padding: 8px;' + #13 + #10, -1);
  ABuilder.Cat('border-style: solid;' + #13 + #10, -1);
  ABuilder.Cat('border-color: #666666;' + #13 + #10, -1);
  ABuilder.Cat('background-color: #dedede;' + #13 + #10, -1);
  ABuilder.Cat('}' + #13 + #10, -1);
  ABuilder.Cat('table.gridtable td ' + #13 + #10, -1);
  ABuilder.Cat('{' + #13 + #10, -1);
  ABuilder.Cat('border-width: 1px;' + #13 + #10, -1);
  ABuilder.Cat('padding: 8px;' + #13 + #10, -1);
  ABuilder.Cat('border-style: solid;' + #13 + #10, -1);
  ABuilder.Cat('border-color: #666666;' + #13 + #10, -1);
  ABuilder.Cat('background-color: #ffffff;' + #13 + #10, -1);
  ABuilder.Cat('}' + #13 + #10, -1);
  ABuilder.Cat('</style>' + #13 + #10, -1);
  ABuilder.Cat('</head>' + #13 + #10, -1);
  ABuilder.Cat('<center><body>' + #13 + #10, -1);
  ABuilder.Cat('<table class="gridtable"><thead><caption><h3>' + ATitle +
    '</h3><br/>' + #13 + #10);
  ABuilder.Cat('<font color="#FF0000">¡ö&nbsp;&nbsp;' +
    _('Red - Compilation failed') + '</font>' + #13 + #10);
  ABuilder.Cat('&nbsp;&nbsp;<font color="#00FF00">¡ö&nbsp;&nbsp;' +
    _('Green - Compiled by') + '</font>' + #13 + #10);
  ABuilder.Cat('&nbsp;&nbsp;<font color="#000000">¡õ&nbsp;&nbsp;' +
    _('White - Platform uncompiled') + '</font></caption>' + #13 + #10);
  ABuilder.Cat('<tr>' + #13 + #10, -1);

  for I := 0 to VDT.Header.Columns.Count - 1 do
  begin
    ABuilder.Cat('<td align="center">' + Repl(VDT.Header.Columns.Items[I].Text,
      #13, '<br/>') + '</td>' + #13 + #10);
  end;
  ABuilder.Cat('</tr></thead>' + #13 + #10, -1);
  ABuilder.Cat('<tbody>' + #13 + #10, -1);

  for I := 0 to FProjectCfg.ForcePath('Project.Files').Count - 1 do
  begin
    QJson := FProjectCfg.ForcePath('Project.Files[' + IntToStr(I) + ']');
    ABuilder.Cat('<tr>' + #13 + #10, -1);
    ABuilder.Cat('<td align="center">' + IntToStr(I + 1) + '</td>' + #13 + #10);
    if QJson.ForcePath('Use').AsBoolean then
      ABuilder.Cat('<td align="center">¡Ì</td>' + #13 + #10)
    else
      ABuilder.Cat('<td align="center"></td>' + #13 + #10);
    FileName := QJson.ForcePath('FileName').AsString;
    ABuilder.Cat('<td align="left">' + FileName + '</td>' + #13 + #10);
    FileName := GetSimpleFileName(FileName);
    for J := 0 to FColumnList.Count - 1 do
    begin
      ColumnName := FColumnList.Names[J];
      if FResultList.ForcePath(FileName).IndexOf(ColumnName) = -1 then
        ABuilder.Cat('<td align="center">', -1)
      else if FResultList.ForcePath(FileName + '.' + ColumnName).AsInteger = -1
      then
        ABuilder.Cat('<td align="center" style="background-color:red">', -1)
      else if FResultList.ForcePath(FileName + '.' + ColumnName).AsInteger = 1
      then
        ABuilder.Cat('<td align="center" style="background-color:lime">', -1)
      else
        ABuilder.Cat('<td align="center">', -1);
      if QJson.ForcePath('Compile.' + ColumnName).AsBoolean then
        ABuilder.Cat('¡Ì', -1);
      ABuilder.Cat('</td>' + #13 + #10);
    end;
    ABuilder.Cat('</tr>' + #13 + #10, -1);
  end;

  ABuilder.Cat('</tbody>' + #13 + #10, -1);
  ABuilder.Cat('</table>' + #13 + #10, -1);
  ABuilder.Cat('<p>').Cat(_('Journal')).Cat('<p><textarea cols=120 rows=80> ' +
    #13 + #10, -1);
  ABuilder.Cat(Log.Text);
  ABuilder.Cat('</textarea></body></center>' + #13 + #10, -1);
  ABuilder.Cat('</HTML>', -1);
  SaveTextU(Path + 'Report.html', ABuilder.Value, False);
  FreeAndNil(ABuilder);
end;
{$IF RTLVersion > 22}

procedure TForm_Main.StyleChanged(var Msg: TMessage);
begin
  DragAcceptFiles(Handle, True);
end;
{$IFEND}

end.
