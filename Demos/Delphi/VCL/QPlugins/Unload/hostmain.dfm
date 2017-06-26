object Form1: TForm1
  Left = 0
  Top = 0
  Caption = #25554#20214#30340#21160#24577#21152#36733#19982#21368#36733
  ClientHeight = 380
  ClientWidth = 639
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 639
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Button1: TButton
      Left = 24
      Top = 8
      Width = 75
      Height = 25
      Caption = #37325#26032#21152#36733
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 124
      Top = 8
      Width = 75
      Height = 25
      Caption = #21368#36733#25554#20214
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 224
      Top = 8
      Width = 97
      Height = 25
      Caption = #26174#31034#25554#20214#31383#21475
      TabOrder = 2
      OnClick = Button3Click
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 41
    Width = 639
    Height = 339
    ActivePage = TabSheet2
    Align = alClient
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'Logs'
      ExplicitLeft = 8
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object mmLogs: TMemo
        Left = 0
        Top = 0
        Width = 631
        Height = 311
        Align = alClient
        TabOrder = 0
        ExplicitTop = 39
        ExplicitWidth = 639
        ExplicitHeight = 339
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Dock'
      ImageIndex = 1
    end
  end
end
