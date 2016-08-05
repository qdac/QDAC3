object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Html '#27169#26495#28436#31034
  ClientHeight = 380
  ClientWidth = 601
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
  object WebBrowser1: TWebBrowser
    Left = 0
    Top = 41
    Width = 601
    Height = 339
    Align = alClient
    TabOrder = 0
    ExplicitLeft = 8
    ExplicitTop = 80
    ExplicitWidth = 300
    ExplicitHeight = 150
    ControlData = {
      4C0000001D3E0000092300000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 601
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = #34920#26684
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 96
      Top = 8
      Width = 75
      Height = 25
      Caption = #26465#20214#26367#25442
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 184
      Top = 8
      Width = 75
      Height = 25
      Caption = #22810#21442#25968
      TabOrder = 2
      OnClick = Button3Click
    end
  end
  object adsData: TADODataSet
    Parameters = <>
    Left = 296
    Top = 192
    object adsDataId: TIntegerField
      FieldName = 'Id'
    end
    object adsDataName: TStringField
      FieldName = 'Name'
      Size = 50
    end
  end
end
