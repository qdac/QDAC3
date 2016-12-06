unit Common;

interface

uses
  Windows, SysUtils, Classes;

var
  CompleteCurrPath, ComSpec: string;

Procedure ShowErrorMessage(Text: string; Cation: string = 'Error');
Procedure ShowInformationMessage(Text: string; Cation: string = 'Information');
function ShowQuestionMessage(Text: string;
  Cation: string = 'Question'): Integer;
function BrowseCallbackProc(hWin: THandle; uMsg: Cardinal; lParam: lParam;
  lpData: lParam): LRESULT; stdcall;
function FolderBrowser(Folder: string): string;
function Repl(Str, Old, New: string): string;
function GetCompleteFileName(FileName: string; CurrPath: string = ''): string;
function FileOrDir(FileName: string): Integer;
function GetQuotePaths(Paths: string; NeedQuote: Boolean = True): string;
function MyGetCurrentDirectory(): string;
function IsAdministrator(): Boolean;
function PipeCall(CmdLine: string; Path: string = 'C:\'): string;
function DescSortByValue(List: TStringList; Index1, Index2: Integer): Integer;
procedure PaintCheckImage(DC: HDC; const R: TRect; Checked: Boolean;
  IsHot: Boolean = False);
procedure RegisterFileType(ExtName, AppName: string; IconName: string = '';
  IconIndex: Integer = 0);
function FileTypeIsRegister(ExtName: string): Boolean;
function GetVersionString(FileName: string): string;
{$IF RTLVersion < 22}

type
  TCmdLineSwitchType = (clstValueNextParam, clstValueAppended);
  TCmdLineSwitchTypes = set of TCmdLineSwitchType;
function FindCmdLineSwitch(const Switch: string; var Value: string;
  IgnoreCase: Boolean = True;
  const SwitchTypes: TCmdLineSwitchTypes = [clstValueNextParam,
  clstValueAppended]): Boolean; overload;
{$IFEND}

implementation

uses shlobj, QString, Forms, ShlwApi, Math, Graphics, Controls, CommCtrl,
  Themes, Registry;

var
  MyszDir: string;

const
  SECURITY_NT_AUTHORITY: TSidIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));
  DOMAIN_ALIAS_RID_ADMINS = ($00000220);
  SECURITY_BUILTIN_DOMAIN_RID = ($00000020);

Procedure ShowErrorMessage(Text, Cation: string);
begin
  Application.MessageBox(PWideChar(Text), PWideChar(Cation),
    MB_OK + MB_ICONSTOP + MB_TOPMOST);
end;

Procedure ShowInformationMessage(Text, Cation: string);
begin
  Application.MessageBox(PWideChar(Text), PWideChar(Cation),
    MB_OK + MB_ICONINFORMATION + MB_TOPMOST);
end;

function ShowQuestionMessage(Text, Cation: string): Integer;
begin
  Result := Application.MessageBox(PWideChar(Text), PWideChar(Cation),
    MB_YESNO + MB_DEFBUTTON2 + MB_ICONQUESTION + MB_TOPMOST);
end;

function BrowseCallbackProc(hWin: THandle; uMsg: Cardinal; lParam: lParam;
  lpData: lParam): LRESULT; stdcall;
begin
  if uMsg = BFFM_INITIALIZED then
    SendMessage(hWin, BFFM_SETSELECTION, 1, Longint(PWideChar(MyszDir)));
  Result := 0;
end;

function FolderBrowser(Folder: string): string;
var
  Info: TBrowseInfo;
  IDList: pItemIDList;
  DisplayName: array [0 .. MAX_PATH] of WideChar;
begin
  Result := '';
  MyszDir := Folder;
  FillChar(Info, sizeof(TBrowseInfo), #0);
  with Info do
  begin
    hwndOwner := Application.Handle;
    pidlRoot := nil;
    pszDisplayName := @DisplayName;
    lpszTitle := 'Please select a directory';
    ulFlags := BIF_STATUSTEXT or BIF_USENEWUI or BIF_RETURNONLYFSDIRS;
    lpfn := @BrowseCallbackProc;
    lParam := 0;
    IDList := SHBrowseForFolder(Info);
  end;
  if IDList <> nil then
  begin
    SHGetPathFromIDList(IDList, DisplayName);
    GlobalFreePtr(IDList);
    Result := string(DisplayName);
  end;
end;

function Repl(Str, Old, New: string): string;
begin
  Result := StringReplace(Str, Old, New, [rfReplaceAll, rfIgnoreCase]);
end;

function GetCompleteFileName(FileName: string; CurrPath: string): string;
var
  Dest: array [0 .. MAX_PATH] of WideChar;
begin
  FillChar(Dest, MAX_PATH + 1, 0);
  if CurrPath = '' then
    CurrPath := CompleteCurrPath;
  PathCombine(Dest, PWideChar(CurrPath), PWideChar(FileName));
  Result := string(Dest);
end;

function FileOrDir(FileName: string): Integer;
begin
  Result := 0;
  if DirectoryExists(FileName) then
    Result := 2
  else if FileExists(FileName) then
    Result := 1;
end;

function GetQuotePaths(Paths: string; NeedQuote: Boolean): string;
var
  Str: string;
begin
  Paths := Repl(Paths, '"', '');
  Result := '';
  while Length(Paths) > 0 do
  begin
    Str := Trim(DecodeTokenW(Paths, ';', '"', True, True, True));
    if Str <> '' then
    begin
      Str := ExcludeTrailingPathDelimiter(GetCompleteFileName(Str));
      if NeedQuote then
        Str := '"' + Str + '"';
      if Result <> '' then
        Result := Result + ';';
      Result := Result + Str;
    end;
  end;
end;

function MyGetCurrentDirectory(): string;
var
  Dest: array [0 .. MAX_PATH] of WideChar;
begin
  FillChar(Dest, MAX_PATH + 1, 0);
  GetCurrentDirectory(MAX_PATH, Dest);
  Result := string(Dest);
end;

function IsAdministrator(): Boolean;
var
  psidAdmin: Pointer;
  Token: THandle;
  Count: DWORD;
  TokenInfo: PTokenGroups;
  HaveToken: Boolean;
  I: Integer;
const
  SE_GROUP_USE_FOR_DENY_ONLY = $00000010;
begin
  Result := not(Win32Platform = VER_PLATFORM_WIN32_NT);
  if Result then // Win9x and ME don't have user groups
    Exit;
  psidAdmin := nil;
  TokenInfo := nil;
  HaveToken := False;
  try
    Token := 0;
    HaveToken := OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True, Token);
    if (not HaveToken) and (GetLastError = ERROR_NO_TOKEN) then
      HaveToken := OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, Token);
    if HaveToken then
    begin
      if not AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2,
        SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS, 0, 0, 0, 0, 0, 0,
        psidAdmin) then
      begin
        Result := False;
        Exit;
      end;
      if GetTokenInformation(Token, TokenGroups, nil, 0, Count) or
        (GetLastError <> ERROR_INSUFFICIENT_BUFFER) then
        RaiseLastOSError;
      TokenInfo := PTokenGroups(AllocMem(Count));
      if not GetTokenInformation(Token, TokenGroups, TokenInfo, Count, Count)
        then
      begin
        Result := False;
        Exit;
      end;
      for I := 0 to TokenInfo^.GroupCount - 1 do
      begin
{$RANGECHECKS OFF} // Groups is an array [0..0] of TSIDAndAttributes, ignore ERangeError
        Result := EqualSid(psidAdmin, TokenInfo^.Groups[I].Sid);
        if Result then
        begin
          // consider denied ACE with Administrator SID
          Result := TokenInfo^.Groups[I]
            .Attributes and SE_GROUP_USE_FOR_DENY_ONLY <>
            SE_GROUP_USE_FOR_DENY_ONLY;
          Break;
        end;
{$IFDEF RANGECHECKS_ON}
{$RANGECHECKS ON}
{$ENDIF RANGECHECKS_ON}
      end;
    end;
  finally
    if TokenInfo <> nil then
      FreeMem(TokenInfo);
    if HaveToken then
      CloseHandle(Token);
    if psidAdmin <> nil then
      FreeSid(psidAdmin);
  end;
end;

function PipeCall(CmdLine: string; Path: string): string;
var
  sa: TSecurityAttributes;
  si: TStartupInfo;
  pi: TProcessInformation;
  hRead, hWrite: THandle;
  Buffer: array [0 .. 4095] of AnsiChar;
  BytesRead, TotalBytesAvail: Cardinal;
begin
  Result := '';
  with sa do
  begin
    nLength := sizeof(sa);
    bInheritHandle := True;
    lpSecurityDescriptor := nil;
  end;
  if not CreatePipe(hRead, hWrite, @sa, 0) then
  begin
    Result := 'Anonymous pipe creation failed, wrong number:' + IntToStr
      (GetLastError) + ' !';
    Exit;
  end;
  with si do
  begin
    FillChar(si, sizeof(si), 0);
    cb := sizeof(si);
    GetStartupInfo(si);
    hStdError := hWrite;
    hStdOutput := hWrite;
    hStdInput := GetStdHandle(STD_INPUT_HANDLE); // don't redirect stdin
    wShowWindow := SW_HIDE;
    dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
  end;
  SetCurrentDirectory(PWideChar(Path));
  if not CreateProcess(nil, PWideChar(CmdLine), nil, nil, True, 0, nil,
    PWideChar(Path), si, pi) then
  begin
    Result := 'Process creation failed, wrong number:' + IntToStr(GetLastError)
      + ' !';
    Exit;
  end;
  CloseHandle(hWrite);
  while PeekNamedPipe(hRead, nil, 0, nil, @TotalBytesAvail, nil) do
  begin
    FillChar(Buffer, sizeof(Buffer), 0);
    if not ReadFile(hRead, Buffer, Min(sizeof(Buffer), TotalBytesAvail),
      BytesRead, nil) then
      Break;
    Result := Result + string(Buffer);
  end;
  CloseHandle(hRead);
end;

function DescSortByValue(List: TStringList; Index1, Index2: Integer): Integer;
begin
  if StrToint(PWideChar(List.ValueFromIndex[Index1])) < StrToint
    (PWideChar(List.ValueFromIndex[Index2])) then
    Result := 1
  else if StrToint(PWideChar(List.ValueFromIndex[Index1])) = StrToint
    (PWideChar(List.ValueFromIndex[Index2])) then
    Result := 0
  else
    Result := -1;
end;
{$IF CompilerVersion < 23}

type
  TElementEdge = (eeRaisedOuter);

  TElementEdges = set of TElementEdge;

  TElementEdgeFlag = (efRect);

  TElementEdgeFlags = set of TElementEdgeFlag;

  // For compatibility with Delphi XE and earlier, prevents deprecated warnings in Delphi XE2 and higher
  StyleServices = class
    class function Enabled: Boolean;
    class function DrawEdge(DC: HDC; Details: TThemedElementDetails;
      const R: TRect; Edges: TElementEdges; Flags: TElementEdgeFlags;
      ContentRect: PRect = nil): Boolean;
    class function DrawElement(DC: HDC; Details: TThemedElementDetails;
      const R: TRect; ClipRect: PRect = nil): Boolean;
    class function GetElementDetails(Detail: TThemedHeader)
      : TThemedElementDetails; overload;
    class function GetElementDetails(Detail: TThemedToolTip)
      : TThemedElementDetails; overload;
    class function GetElementDetails(Detail: TThemedWindow)
      : TThemedElementDetails; overload;
    class function GetElementDetails(Detail: TThemedButton)
      : TThemedElementDetails; overload;
    class procedure PaintBorder(Control: TWinControl; EraseLRCorner: Boolean);
  end;

class function StyleServices.Enabled: Boolean;
begin
  Result := ThemeServices.ThemesEnabled;
end;

class function StyleServices.DrawEdge(DC: HDC; Details: TThemedElementDetails;
  const R: TRect; Edges: TElementEdges; Flags: TElementEdgeFlags;
  ContentRect: PRect = nil): Boolean;
begin
  Assert((Edges = [eeRaisedOuter]) and (Flags = [efRect]));
  ThemeServices.DrawEdge(DC, Details, R, BDR_RAISEDOUTER, BF_RECT);
  Result := Enabled;
end;

class function StyleServices.DrawElement(DC: HDC;
  Details: TThemedElementDetails; const R: TRect;
  ClipRect: PRect = nil): Boolean;
begin
  ThemeServices.DrawElement(DC, Details, R, ClipRect);
  Result := Enabled;
end;

class function StyleServices.GetElementDetails(Detail: TThemedHeader)
  : TThemedElementDetails;
begin
  Result := ThemeServices.GetElementDetails(Detail);
end;

class function StyleServices.GetElementDetails(Detail: TThemedToolTip)
  : TThemedElementDetails;
begin
  Result := ThemeServices.GetElementDetails(Detail);
end;

class function StyleServices.GetElementDetails(Detail: TThemedWindow)
  : TThemedElementDetails;
begin
  Result := ThemeServices.GetElementDetails(Detail);
end;

class function StyleServices.GetElementDetails(Detail: TThemedButton)
  : TThemedElementDetails;
begin
  Result := ThemeServices.GetElementDetails(Detail);
end;

class procedure StyleServices.PaintBorder(Control: TWinControl;
  EraseLRCorner: Boolean);
begin
  ThemeServices.PaintBorder(Control, EraseLRCorner);
end;
{$IFEND}

procedure PaintCheckImage(DC: HDC; const R: TRect; Checked: Boolean;
  IsHot: Boolean);
var
  Details: TThemedElementDetails;
begin
  Details.Element := teButton;
  if Checked then
  begin
    if IsHot then
      Details := StyleServices.GetElementDetails(tbCheckBoxCheckedHot)
    else
      Details := StyleServices.GetElementDetails(tbCheckBoxCheckedNormal);
  end
  else
  begin
    if IsHot then
      Details := StyleServices.GetElementDetails(tbCheckBoxUncheckedHot)
    else
      Details := StyleServices.GetElementDetails(tbCheckBoxUncheckedNormal);
  end;
  StyleServices.DrawElement(DC, Details, R);
end;

procedure RegisterFileType(ExtName, AppName: string; IconName: string;
  IconIndex: Integer);
var
  Reg: TRegistry;
  ExtKey: string;
begin
  if not IsAdministrator then
  begin
    ShowErrorMessage('Administrator rights are required to register on ' +
        ExtName + ' file! Please run as administrator of these programs!');
    Exit;
  end;
  if IconName = '' then
    IconName := AppName;
  ExtName := LowerCase(ExtName);
  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    if Copy(ExtName, 1, 1) = '.' then
      ExtKey := Copy(ExtName, 2, Length(ExtName) - 1)
    else
    begin
      ExtKey := ExtName;
      ExtName := '.' + ExtName;
    end;
    ExtKey := ExtKey + 'file';
    Reg.OpenKey(ExtName, True);
    Reg.WriteString('', ExtKey);
    Reg.CloseKey;
    Reg.OpenKey(ExtKey, True);
    Reg.OpenKey('DefaultIcon', True);
    if ExtractFileExt(LowerCase(IconName)) = '.ico' then
      Reg.WriteString('', IconName)
    else
      Reg.WriteString('', IconName + ',' + IntToStr(IconIndex));
    Reg.CloseKey;
    Reg.OpenKey(ExtKey + '\shell\open\command', True);
    Reg.WriteString('', AppName + ' "%1"');
    Reg.CloseKey;
    SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
  finally
    Reg.Free;
  end;
end;

function FileTypeIsRegister(ExtName: string): Boolean;
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    ExtName := LowerCase(ExtName);
    if not(Copy(ExtName, 1, 1) = '.') then
      ExtName := '.' + ExtName;
    Result := Reg.KeyExists(ExtName);
    Reg.CloseKey;
  finally
    Reg.Free;
  end;
end;

function GetVersionString(FileName: string): string;
var
  VerInfoSize: DWORD;
  VerInfo: Pointer;
  VerValueSize: DWORD;
  Dummy: DWORD;
  VerValue: PVSFixedFileInfo;
begin
  Result := '';
  VerInfoSize := GetFileVersionInfoSize(PChar(FileName), Dummy);
  if VerInfoSize = 0 then
    Exit;
  GetMem(VerInfo, VerInfoSize);
  GetFileVersionInfo(PChar(FileName), 0, VerInfoSize, VerInfo);
  VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
  Result := IntToStr(HIWORD(VerValue^.dwFileVersionMS)) + '.' + IntToStr
    (LOWORD(VerValue^.dwFileVersionMS)) + '.' + IntToStr
    (HIWORD(VerValue^.dwFileVersionLS)) + '.' + IntToStr
    (LOWORD(VerValue^.dwFileVersionLS));
  FreeMem(VerInfo);
end;
{$IF RTLVersion < 22}

function FindCmdLineSwitch(const Switch: string; var Value: string;
  IgnoreCase: Boolean = True;
  const SwitchTypes: TCmdLineSwitchTypes = [clstValueNextParam,
  clstValueAppended]): Boolean; overload;
type
  TCompareProc = function(const S1, S2: string): Boolean;
var
  Param: string;
  I, ValueOfs, SwitchLen, ParamLen: Integer;
  SameSwitch: TCompareProc;
begin
  Result := False;
  Value := '';
  if IgnoreCase then
    SameSwitch := SameText
  else
    SameSwitch := SameStr;
  SwitchLen := Length(Switch);

  for I := 1 to ParamCount do
  begin
    Param := ParamStr(I);
    if CharInSet(Param[1], SwitchChars) and SameSwitch
      (Copy(Param, 2, SwitchLen), Switch) then
    begin
      ParamLen := Length(Param);
      // Look for an appended value if the param is longer than the switch
      if (ParamLen > SwitchLen + 1) then
      begin
        // If not looking for appended value switches then this is not a matching switch
        if not(clstValueAppended in SwitchTypes) then
          Continue;
        ValueOfs := SwitchLen + 2;
        if Param[ValueOfs] = ':' then
          Inc(ValueOfs);
        Value := Copy(Param, ValueOfs, MaxInt);
      end
      // If the next param is not a switch, then treat it as the value
      else if (clstValueNextParam in SwitchTypes) and (I < ParamCount)
        and not CharInSet(ParamStr(I + 1)[1], SwitchChars) then
        Value := ParamStr(I + 1);
      Result := True;
      Break;
    end;
  end;
end;
{$IFEND}

end.
