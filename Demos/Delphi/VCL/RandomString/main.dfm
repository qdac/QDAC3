object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'RandomString '#31034#20363
  ClientHeight = 577
  ClientWidth = 711
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 711
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = -6
    object Label1: TLabel
      Left = 16
      Top = 14
      Width = 64
      Height = 13
      Caption = #23383#31526#20018#38271#24230':'
    end
    object SpinEdit1: TSpinEdit
      Left = 86
      Top = 10
      Width = 59
      Height = 22
      MaxValue = 100
      MinValue = 1
      TabOrder = 0
      Value = 10
    end
    object CheckBox1: TCheckBox
      Left = 151
      Top = 12
      Width = 50
      Height = 17
      Caption = #23383#27597
      TabOrder = 1
    end
    object CheckBox2: TCheckBox
      Left = 207
      Top = 12
      Width = 50
      Height = 17
      Caption = #25968#23383
      TabOrder = 2
    end
    object CheckBox3: TCheckBox
      Left = 263
      Top = 12
      Width = 50
      Height = 17
      Caption = #27721#23383
      TabOrder = 3
    end
    object CheckBox4: TCheckBox
      Left = 319
      Top = 12
      Width = 50
      Height = 17
      Caption = #31526#21495
      TabOrder = 4
    end
    object CheckBox5: TCheckBox
      Left = 375
      Top = 12
      Width = 50
      Height = 17
      Caption = #31354#30333
      TabOrder = 5
    end
    object Button1: TButton
      Left = 432
      Top = 8
      Width = 75
      Height = 25
      Caption = #29983#25104
      TabOrder = 6
      OnClick = Button1Click
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 41
    Width = 711
    Height = 536
    Align = alClient
    TabOrder = 1
    ExplicitLeft = 264
    ExplicitTop = -24
    ExplicitWidth = 185
    ExplicitHeight = 89
  end
end
