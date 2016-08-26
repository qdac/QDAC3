object Form1: TForm1
  Left = 0
  Top = 0
  Caption = #25554#20214#38388#21442#25968#20256#36882#28436#31034
  ClientHeight = 201
  ClientWidth = 447
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 447
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Button1: TButton
      Left = 8
      Top = 10
      Width = 85
      Height = 25
      Caption = #23545#35937#20570#20026#21442#25968
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 104
      Top = 10
      Width = 85
      Height = 25
      Caption = #27969#20570#20026#21442#25968
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 200
      Top = 10
      Width = 85
      Height = 25
      Caption = #20108#32500#21442#25968
      TabOrder = 2
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 296
      Top = 10
      Width = 85
      Height = 25
      Caption = #26222#36890#21442#25968
      TabOrder = 3
      OnClick = Button4Click
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 41
    Width = 447
    Height = 160
    Align = alClient
    TabOrder = 1
  end
end
