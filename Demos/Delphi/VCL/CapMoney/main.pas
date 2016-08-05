unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, QString,
  Vcl.Samples.Spin;

type
  TForm4 = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    Button1: TButton;
    edtMoney: TEdit;
    Button2: TButton;
    GroupBox1: TGroupBox;
    chkIncNum: TCheckBox;
    chkIncUnit: TCheckBox;
    chkHideZero: TCheckBox;
    chkMergeZero: TCheckBox;
    chkPatchEnd: TCheckBox;
    cbxPreset: TComboBox;
    Label1: TLabel;
    edtPreStr: TEdit;
    Label2: TLabel;
    edtPatchStr: TEdit;
    Label3: TLabel;
    edtNegStr: TEdit;
    Label4: TLabel;
    cbxRoundMode: TComboBox;
    Label5: TLabel;
    seEndDigits: TSpinEdit;
    Label6: TLabel;
    seGroupNum: TSpinEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure cbxPresetChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}
procedure TForm4.Button1Click(Sender: TObject);
  procedure DoTest(AVal: Currency);
  begin
    Memo1.Lines.Add('格式化值:' + CurrToStr(AVal));
    Memo1.Lines.Add('    阅读格式（四舍五入）：' + CapMoney(AVal, MC_READ, '-', '￥', '整',
      0, mrmSimple));
    Memo1.Lines.Add('    套打格式（四舍五入）：' + CapMoney(AVal, MC_PRINT, '-', '￥', '整',
      0, mrmSimple));
    Memo1.Lines.Add('    阅读格式（银行家）：' + CapMoney(AVal, MC_READ, '-', '￥', '整', 0,
      mrmBank));
    Memo1.Lines.Add('    套打格式（银行家）：' + CapMoney(AVal, MC_PRINT, '-', '￥', '整',
      0, mrmBank));

  end;

begin
  DoTest(1.235);
  DoTest(1.245);
  DoTest(1.234);
  DoTest(1.236);
  DoTest(100.24);
  DoTest(10012.235);
  DoTest(-922337203685477.5807);
  DoTest(922337203685477.5807);
end;

procedure TForm4.Button2Click(Sender: TObject);
var
  AFlags: Integer;
  ARoundMode: TMoneyRoundMethod;
begin
  AFlags := 0;
  if chkIncNum.Checked then
    AFlags := AFlags or MC_NUM;
  if chkIncUnit.Checked then
    AFlags := AFlags or MC_UNIT;
  if chkHideZero.Checked then
    AFlags := AFlags or MC_HIDE_ZERO;
  if chkMergeZero.Checked then
    AFlags := AFlags or MC_MERGE_ZERO;
  if chkPatchEnd.Checked then
    AFlags := AFlags or MC_END_PATCH;
  case cbxRoundMode.ItemIndex of
    1:
      ARoundMode := mrmSimple;
    2:
      ARoundMode := mrmBank
  else
    ARoundMode := mrmNone;
  end;
  Memo1.Lines.Add(CapMoney(StrToCurr(edtMoney.Text), AFlags, edtNegStr.Text,
    edtPreStr.Text, edtPatchStr.Text, seGroupNum.Value, ARoundMode,
    seEndDigits.Value));
end;

procedure TForm4.cbxPresetChange(Sender: TObject);
var
  AFlags: Integer;
begin
  if cbxPreset.ItemIndex = 0 then
    AFlags := MC_READ
  else if cbxPreset.ItemIndex = 1 then
    AFlags := MC_PRINT
  else
    Exit;
  chkIncNum.Checked := (AFlags and MC_NUM) <> 0;
  chkIncUnit.Checked := (AFlags and MC_UNIT) <> 0;
  chkHideZero.Checked := (AFlags and MC_HIDE_ZERO) <> 0;
  chkMergeZero.Checked := (AFlags and MC_MERGE_ZERO) <> 0;
  chkPatchEnd.Checked := (AFlags and MC_END_PATCH) <> 0;
end;

end.
