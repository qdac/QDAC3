object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'QPlugins '#36890#30693#28436#31034'(Notify Demo)'
  ClientHeight = 116
  ClientWidth = 698
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 32
    Top = 80
    Width = 168
    Height = 13
    Caption = #31227#21160#19979#36827#24230#26465#65292#23601#21487#20197#30475#21040#25928#26524
  end
  object TrackBar1: TTrackBar
    Left = 32
    Top = 16
    Width = 625
    Height = 45
    Max = 100
    TabOrder = 0
    OnChange = TrackBar1Change
  end
  object Button1: TButton
    Left = 240
    Top = 67
    Width = 153
    Height = 25
    Caption = #26032#24314#25509#25910#31383#21475
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 424
    Top = 67
    Width = 153
    Height = 25
    Caption = #26032#24314'DLL'#25554#20214#25509#25910#31383#21475
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 608
    Top = 80
    Width = 75
    Height = 25
    Caption = 'Button3'
    TabOrder = 3
    OnClick = Button3Click
  end
end
