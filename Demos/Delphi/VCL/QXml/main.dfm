object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'QXML Demo'
  ClientHeight = 399
  ClientWidth = 745
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
    Width = 745
    Height = 65
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 120
      Height = 25
      Caption = #21019#24314'10'#19975#32467#28857
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 134
      Top = 8
      Width = 80
      Height = 25
      Caption = #28155#21152#28436#31034
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 222
      Top = 8
      Width = 80
      Height = 25
      Caption = #21152#36733
      TabOrder = 2
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 310
      Top = 8
      Width = 80
      Height = 25
      Caption = #20445#23384
      TabOrder = 3
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 401
      Top = 8
      Width = 80
      Height = 25
      Caption = #35299#26512#25991#26412
      TabOrder = 4
      OnClick = Button5Click
    end
    object Search: TButton
      Left = 487
      Top = 8
      Width = 80
      Height = 25
      Caption = #26597#25214
      TabOrder = 5
      OnClick = SearchClick
    end
    object Button6: TButton
      Left = 576
      Top = 8
      Width = 80
      Height = 25
      Caption = #33719#21462#25991#26412
      TabOrder = 6
      OnClick = Button6Click
    end
    object Button7: TButton
      Left = 662
      Top = 8
      Width = 75
      Height = 25
      Caption = #36816#34892#26102'RTTI'
      TabOrder = 7
      OnClick = Button7Click
    end
    object Button8: TButton
      Left = 8
      Top = 36
      Width = 75
      Height = 25
      Caption = #21160#24577#32465#23450
      TabOrder = 8
      OnClick = Button8Click
    end
    object Button9: TButton
      Left = 89
      Top = 36
      Width = 75
      Height = 25
      Caption = #27969#24335#35299#26512
      TabOrder = 9
      OnClick = Button9Click
    end
    object Button10: TButton
      Left = 170
      Top = 36
      Width = 75
      Height = 25
      Caption = #20540'/'#31867#22411#26367#25442
      TabOrder = 10
      OnClick = Button10Click
    end
    object Button11: TButton
      Left = 251
      Top = 36
      Width = 75
      Height = 25
      Caption = 'For..In'
      TabOrder = 11
      OnClick = Button11Click
    end
    object Button12: TButton
      Left = 332
      Top = 36
      Width = 75
      Height = 25
      Caption = #31227#21160#32467#28857
      TabOrder = 12
      OnClick = Button12Click
    end
    object Button13: TButton
      Left = 413
      Top = 36
      Width = 75
      Height = 25
      Caption = 'Soap'
      TabOrder = 13
      OnClick = Button13Click
    end
    object Button14: TButton
      Left = 494
      Top = 36
      Width = 75
      Height = 25
      Caption = 'HasChild'
      TabOrder = 14
      OnClick = Button14Click
    end
    object Button15: TButton
      Left = 575
      Top = 34
      Width = 75
      Height = 25
      Caption = 'Button15'
      TabOrder = 15
      OnClick = Button15Click
    end
  end
  object mmResult: TMemo
    Left = 0
    Top = 65
    Width = 745
    Height = 334
    Align = alClient
    ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
    TabOrder = 1
  end
  object OpenDialog1: TOpenDialog
    Left = 400
    Top = 64
  end
  object SaveDialog1: TSaveDialog
    Left = 320
    Top = 64
  end
end
