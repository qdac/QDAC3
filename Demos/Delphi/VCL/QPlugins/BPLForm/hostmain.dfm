object Form1: TForm1
  Left = 0
  Top = 0
  Caption = #22810#23454#20363#28436#31034'(Multi-Instance Demo'#65289
  ClientHeight = 309
  ClientWidth = 597
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
    Width = 597
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Button1: TButton
      Left = 16
      Top = 9
      Width = 75
      Height = 25
      Caption = #26032#39029
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 97
      Top = 10
      Width = 75
      Height = 25
      Caption = 'bpl'#26032#39029
      TabOrder = 1
      OnClick = Button2Click
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 41
    Width = 597
    Height = 268
    Align = alClient
    TabOrder = 1
    OnResize = PageControl1Resize
  end
end
