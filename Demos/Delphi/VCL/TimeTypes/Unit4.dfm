object Form4: TForm4
  Left = 0
  Top = 0
  Caption = #26102#38388#31867#22411#28436#31034
  ClientHeight = 272
  ClientWidth = 651
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 129
    Width = 651
    Height = 143
    Align = alClient
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 651
    Height = 129
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = -6
    object Label1: TLabel
      Left = 16
      Top = 8
      Width = 80
      Height = 13
      Caption = 'TQTimeStamp'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label2: TLabel
      Left = 16
      Top = 72
      Width = 61
      Height = 13
      Caption = 'TQInterval'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Button1: TButton
      Left = 32
      Top = 32
      Width = 75
      Height = 25
      Caption = #36171#20540
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 32
      Top = 91
      Width = 75
      Height = 25
      Caption = #36171#20540
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 136
      Top = 91
      Width = 75
      Height = 25
      Caption = #21152#20943
      TabOrder = 2
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 240
      Top = 91
      Width = 75
      Height = 25
      Caption = #26684#24335#21270
      TabOrder = 3
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 136
      Top = 32
      Width = 75
      Height = 25
      Caption = #21152#20943
      TabOrder = 4
      OnClick = Button5Click
    end
  end
end
