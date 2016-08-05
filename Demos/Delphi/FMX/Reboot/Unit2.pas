unit Unit2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Effects,Androidapi.JNI.App;

type
  TForm2 = class(TForm)
    Panel2: TPanel;
    rbShutdownNow: TRadioButton;
    Label2: TLabel;
    rb30Min: TRadioButton;
    rb60Min: TRadioButton;
    rb90Min: TRadioButton;
    rb120Min: TRadioButton;
    rb150Min: TRadioButton;
    GlowEffect1: TGlowEffect;
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure rbShutdownNowClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation
uses Unit1,QString;
{$R *.fmx}
// 按键支持
procedure TForm2.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
var
  AEvent: TNotifyEvent;
begin
case Key of
  vkUp, vkLeft, vkVolumeUp:
    begin
    if ActiveControl = rbShutdownNow then
      ActiveControl := rb150Min
    else if ActiveControl = rb30Min then
      ActiveControl := rbShutdownNow
    else if ActiveControl = rb60Min then
      ActiveControl := rb30Min
    else if ActiveControl = rb90Min then
      ActiveControl := rb60Min
    else if ActiveControl = rb120Min then
      ActiveControl := rb90Min
    else if ActiveControl = rb150Min then
      ActiveControl := rb120Min
    else
      ActiveControl := rbShutdownNow;
    Key := 0;
    end;
  vkDown, vkRight, vkVolumeDown:
    begin
    if ActiveControl = rbShutdownNow then
      ActiveControl := rb30Min
    else if ActiveControl = rb30Min then
      ActiveControl := rb60Min
    else if ActiveControl = rb60Min then
      ActiveControl := rb90Min
    else if ActiveControl = rb90Min then
      ActiveControl := rb120Min
    else if ActiveControl = rb120Min then
      ActiveControl := rb150Min
    else
      ActiveControl := rbShutdownNow;
    Key := 0;
    end;
  vkReturn:
    begin
    AEvent := (Focused.GetObject as TControl).OnClick;
    if Assigned(AEvent) then
      AEvent(Self);
    Key := 0;
    Exit;
    end;
  vkSleep:
    begin
    if Assigned(FActiveControl) and Assigned((FActiveControl as TControl).OnClick) then
      (FActiveControl as TControl).OnClick(Self);
    end;
end;
if Assigned(ActiveControl) then
  GlowEffect1.Parent := ActiveControl;
end;
// 立即关闭
procedure TForm2.rbShutdownNowClick(Sender: TObject);
var
  AButton: TRadioButton;
begin
if Form1.RootNeeded then
  begin
  AButton := Sender as TRadioButton;
  Form1.PendShutdown(AButton.Tag);
  Close;
  end;
end;

end.
