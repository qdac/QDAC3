object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'QMsgPack Demo'
  ClientHeight = 202
  ClientWidth = 735
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
    Width = 735
    Height = 73
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    OnClick = Panel1Click
    object Button1: TButton
      Left = 111
      Top = 10
      Width = 75
      Height = 25
      Caption = #21152#36733#25991#20214
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 294
      Top = 10
      Width = 75
      Height = 25
      Caption = #28155#21152
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 199
      Top = 10
      Width = 75
      Height = 25
      Caption = #20445#23384#21040#25991#20214
      TabOrder = 2
      OnClick = Button3Click
    end
    object Button10: TButton
      Left = 375
      Top = 10
      Width = 78
      Height = 25
      Caption = 'For..In'
      TabOrder = 3
      OnClick = Button10Click
    end
    object Button11: TButton
      Left = 463
      Top = 10
      Width = 82
      Height = 25
      Caption = #25353#36335#24452#36171#20540
      TabOrder = 4
      OnClick = Button11Click
    end
    object Button12: TButton
      Left = 551
      Top = 10
      Width = 82
      Height = 25
      Caption = #26597#25214
      TabOrder = 5
      OnClick = Button12Click
    end
    object Button8: TButton
      Left = 639
      Top = 10
      Width = 90
      Height = 25
      Caption = #25968#32452
      TabOrder = 6
      OnClick = Button8Click
    end
    object Button4: TButton
      Left = 8
      Top = 10
      Width = 97
      Height = 25
      Caption = #21019#24314' 100000 '#32467#28857
      TabOrder = 7
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 8
      Top = 41
      Width = 97
      Height = 25
      Caption = #36895#24230#27979#35797
      TabOrder = 8
      OnClick = Button5Click
    end
    object Button6: TButton
      Left = 111
      Top = 41
      Width = 75
      Height = 25
      Caption = 'RTTI'
      TabOrder = 9
      OnClick = Button6Click
    end
    object Button7: TButton
      Left = 199
      Top = 41
      Width = 75
      Height = 25
      Caption = 'ClientDataSet'
      TabOrder = 10
      OnClick = Button7Click
    end
    object Button9: TButton
      Left = 294
      Top = 41
      Width = 75
      Height = 25
      Caption = 'ToJson'
      TabOrder = 11
      OnClick = Button9Click
    end
    object Button13: TButton
      Left = 376
      Top = 41
      Width = 75
      Height = 25
      Caption = 'HasChild'
      TabOrder = 12
      OnClick = Button13Click
    end
    object Button14: TButton
      Left = 459
      Top = 41
      Width = 75
      Height = 25
      Caption = 'AsJson'
      TabOrder = 13
      OnClick = Button14Click
    end
    object Button15: TButton
      Left = 551
      Top = 41
      Width = 75
      Height = 25
      Caption = 'Button15'
      TabOrder = 14
      OnClick = Button15Click
    end
  end
  object mmResult: TMemo
    Left = 0
    Top = 73
    Width = 735
    Height = 129
    Align = alClient
    ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
    TabOrder = 1
  end
  object dlgSave: TSaveDialog
    Left = 568
    Top = 120
  end
  object dlgOpen: TOpenDialog
    Left = 480
    Top = 112
  end
end
