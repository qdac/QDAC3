object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'QJson Demo'
  ClientHeight = 425
  ClientWidth = 856
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
    Width = 856
    Height = 129
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    OnClick = Panel1Click
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 117
      Height = 25
      Caption = #21019#24314' 100000 '#32467#28857
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 140
      Top = 8
      Width = 117
      Height = 25
      Caption = #28155#21152#32467#28857
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 273
      Top = 8
      Width = 117
      Height = 25
      Caption = #21152#36733#25991#20214
      TabOrder = 2
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 406
      Top = 8
      Width = 117
      Height = 25
      Caption = #20445#23384#21040#25991#20214
      TabOrder = 3
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 539
      Top = 8
      Width = 117
      Height = 25
      Caption = #35299#26512#25991#26412
      TabOrder = 4
      OnClick = Button5Click
    end
    object Button6: TButton
      Left = 672
      Top = 8
      Width = 75
      Height = 25
      Caption = 'RTTI'#36816#34892#26102
      TabOrder = 5
      OnClick = Button6Click
    end
    object Button7: TButton
      Left = 8
      Top = 39
      Width = 117
      Height = 25
      Caption = #27969#24335#21152#36733
      TabOrder = 6
      OnClick = Button7Click
    end
    object Button8: TButton
      Left = 140
      Top = 39
      Width = 117
      Height = 25
      Caption = #25968#32452
      TabOrder = 7
      OnClick = Button8Click
    end
    object Button10: TButton
      Left = 273
      Top = 39
      Width = 117
      Height = 25
      Caption = 'For..In'
      TabOrder = 8
      OnClick = Button10Click
    end
    object Button11: TButton
      Left = 406
      Top = 39
      Width = 117
      Height = 25
      Caption = #25353#36335#24452#36171#20540
      TabOrder = 9
      OnClick = Button11Click
    end
    object Button12: TButton
      Left = 539
      Top = 39
      Width = 117
      Height = 25
      Caption = #26597#25214
      TabOrder = 10
      OnClick = Button12Click
    end
    object Button9: TButton
      Left = 672
      Top = 39
      Width = 75
      Height = 25
      Caption = 'XXXXIf'
      TabOrder = 11
      OnClick = Button9Click
    end
    object Button13: TButton
      Left = 753
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Invoke'
      TabOrder = 12
      OnClick = Button13Click
    end
    object Button14: TButton
      Left = 753
      Top = 39
      Width = 75
      Height = 25
      Caption = 'ArrayRTTI'
      TabOrder = 13
    end
    object Button15: TButton
      Left = 8
      Top = 70
      Width = 117
      Height = 25
      Caption = #32534#30721#27979#35797
      TabOrder = 14
      OnClick = Button15Click
    end
    object Button16: TButton
      Left = 140
      Top = 70
      Width = 117
      Height = 25
      Caption = 'TryParse'
      TabOrder = 15
      OnClick = Button16Click
    end
    object Invoke: TButton
      Left = 273
      Top = 70
      Width = 117
      Height = 25
      Caption = 'Invoke'
      TabOrder = 16
      OnClick = Button13Click
    end
    object Button17: TButton
      Left = 408
      Top = 72
      Width = 115
      Height = 25
      Caption = 'HasChild'
      TabOrder = 17
      OnClick = Button17Click
    end
    object Button19: TButton
      Left = 539
      Top = 74
      Width = 117
      Height = 25
      Caption = #21547#27880#37322'JSON'#35299#26512
      TabOrder = 18
      OnClick = Button19Click
    end
    object Button18: TButton
      Left = 672
      Top = 74
      Width = 75
      Height = 25
      Caption = #27969#21040'Json'
      TabOrder = 19
      OnClick = Button18Click
    end
    object Button20: TButton
      Left = 753
      Top = 74
      Width = 75
      Height = 25
      Caption = #25490#24207
      TabOrder = 20
      OnClick = Button20Click
    end
    object Button21: TButton
      Left = 8
      Top = 101
      Width = 117
      Height = 25
      Caption = #21253#21547#27880#37322
      TabOrder = 21
      OnClick = Button21Click
    end
    object Button22: TButton
      Left = 140
      Top = 101
      Width = 117
      Height = 25
      Caption = #21512#24182
      TabOrder = 22
      OnClick = Button22Click
    end
    object Button23: TButton
      Left = 272
      Top = 101
      Width = 118
      Height = 25
      Caption = #20132#38598
      TabOrder = 23
      OnClick = Button23Click
    end
    object Button24: TButton
      Left = 406
      Top = 101
      Width = 118
      Height = 25
      Caption = #24046#24322
      TabOrder = 24
      OnClick = Button24Click
    end
    object Button25: TButton
      Left = 544
      Top = 104
      Width = 75
      Height = 25
      Caption = #20020#26102#27979#35797
      TabOrder = 25
      OnClick = Button25Click
    end
    object Button26: TButton
      Left = 632
      Top = 104
      Width = 75
      Height = 25
      Caption = 'IndexOfValue'
      TabOrder = 26
      OnClick = Button26Click
    end
  end
  object mmResult: TMemo
    Left = 0
    Top = 129
    Width = 856
    Height = 296
    Align = alClient
    ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
    TabOrder = 1
  end
  object OpenDialog1: TOpenDialog
    Left = 80
    Top = 176
  end
  object SaveDialog1: TSaveDialog
    Left = 24
    Top = 176
  end
end
