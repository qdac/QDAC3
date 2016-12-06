unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm_PathSetup = class(TForm)
    Label14: TLabel;
    LB: TListBox;
    Button3: TButton;
    Button4: TButton;
    E1: TEdit;
    Button1: TButton;
    Button2: TButton;
    Button5: TButton;
    BtnSave: TButton;
    Button7: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Label14Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure LBDblClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure Init(Paths: string);
    function GetPaths(): string;
  end;

var
  Form_PathSetup: TForm_PathSetup;

implementation

uses QString,Unit1{$IFDEF QLANG_SUPPORT}, QLang{$ENDIF}, Common;
{$R *.dfm}

procedure TForm_PathSetup.BtnSaveClick(Sender: TObject);
begin
  BtnSave.Tag := 1;
  Close;
end;

procedure TForm_PathSetup.Button1Click(Sender: TObject);
var
  Directory: string;
begin
  Directory := FolderBrowser(GetCompleteFileName(E1.Text));
  if Length(Directory) > 0 then
    E1.Text := Directory;
end;

procedure TForm_PathSetup.Button2Click(Sender: TObject);
begin
  if Length(Trim(E1.Text)) > 0 then
    LB.Items.Add(Trim(E1.Text));
end;

procedure TForm_PathSetup.Button3Click(Sender: TObject);
begin
  if (LB.ItemIndex >= 0) and (LB.ItemIndex < LB.Count - 1) then
    LB.Items.Exchange(LB.ItemIndex, LB.ItemIndex + 1);
end;

procedure TForm_PathSetup.Button4Click(Sender: TObject);
begin
  if LB.ItemIndex > 0 then
    LB.Items.Exchange(LB.ItemIndex, LB.ItemIndex - 1);
end;

procedure TForm_PathSetup.Button5Click(Sender: TObject);
begin
  if LB.ItemIndex >= 0 then
    LB.DeleteSelected;
end;

procedure TForm_PathSetup.Button7Click(Sender: TObject);
begin
  Close;
end;

procedure TForm_PathSetup.Label14Click(Sender: TObject);
var
  Str: string;
begin
  Str := _(
    'Relative dir: GetCurrentDirectory (not saved) or *.CompilerCfg directory (saved)');
  Str := Str + #13 + '%BDS%: Berlin : C:\Program Files\Embarcadero\Studio\18.0';
  Str := Str + #13 + '%Config%: Release or Debug';
  Str := Str + #13 + '%Ver%: Seattle : RS10 Berlin : RS10.1';
  Str := Str + #13 +
    '%Platform%: Win32 or Win64 or Android or IOS os IOS64 or OSX';
  ShowInformationMessage(Str, _('Directions'));
end;

procedure TForm_PathSetup.LBDblClick(Sender: TObject);
begin
  if LB.ItemIndex >= 0 then
    E1.Text := LB.Items.Strings[LB.ItemIndex];
end;

procedure TForm_PathSetup.Init(Paths: string);
var
  Str: string;
begin
  while Length(Paths) > 0 do
  begin
    Str := DecodeTokenW(Paths, ';', '"', True, True, True);
    Str := ExcludeTrailingPathDelimiter(Repl(Str, '"', ''));
    if Length(Str) > 0 then
    begin
      if (Length(Str) = 2) and (Copy(Str, 2, 1) = ':') then
        Str := Str + '\';
      LB.Items.Add(Str);
    end;
  end;
end;

function TForm_PathSetup.GetPaths(): string;
var
  Str: string;
  I: Integer;
begin
  Result := '';
  for I := 0 to LB.Count - 1 do
  begin
    Str := LB.Items.Strings[I];
    Str := Trim(Repl(Str, '"', ''));
    if Length(Str) > 0 then
    begin
      if Length(Result) > 0 then
        Result := Result + ';';
      if (Length(Str) = 2) and (Copy(Str, 2, 1) = ':') then
        Str := Str + '\';
      Result := Result + Str;
    end;
  end;
end;

end.
