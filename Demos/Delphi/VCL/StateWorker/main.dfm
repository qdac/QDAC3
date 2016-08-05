object frmMain: TfrmMain
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #20381#36182#29366#24577#20316#19994#25511#21046#28436#31034
  ClientHeight = 134
  ClientWidth = 411
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblStatus: TLabel
    Left = 32
    Top = 80
    Width = 64
    Height = 13
    Caption = #29366#24577':'#24050#20572#27490
  end
  object btnStart: TButton
    Left = 32
    Top = 32
    Width = 75
    Height = 25
    Caption = #24320#22987
    TabOrder = 0
    OnClick = btnStartClick
  end
  object btnPause: TButton
    Left = 144
    Top = 32
    Width = 75
    Height = 25
    Caption = #26242#20572
    Enabled = False
    TabOrder = 1
    OnClick = btnPauseClick
  end
  object btnStop: TButton
    Left = 256
    Top = 32
    Width = 75
    Height = 25
    Caption = #20572#27490
    Enabled = False
    TabOrder = 2
    OnClick = btnStopClick
  end
  object Button1: TButton
    Left = 144
    Top = 75
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 3
    OnClick = Button1Click
  end
end
