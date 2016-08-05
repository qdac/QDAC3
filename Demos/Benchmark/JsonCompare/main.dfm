object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'QJson Compare'
  ClientHeight = 425
  ClientWidth = 772
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
    Width = 772
    Height = 49
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Button10: TButton
      Left = 672
      Top = 12
      Width = 75
      Height = 25
      Caption = #24320#22987'(&S)'
      TabOrder = 0
      OnClick = Button10Click
    end
    object chkCreate: TCheckBox
      Left = 16
      Top = 16
      Width = 97
      Height = 17
      Caption = #21019#24314#32467#28857
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object chkLoad: TCheckBox
      Left = 123
      Top = 16
      Width = 97
      Height = 17
      Caption = #21152#36733#25991#20214
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object chkSave: TCheckBox
      Left = 231
      Top = 16
      Width = 97
      Height = 17
      Caption = #20445#23384#25991#20214
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object chkTypes: TCheckBox
      Left = 339
      Top = 16
      Width = 129
      Height = 17
      Caption = #35299#26512#19981#21516#31867#22411#25968#25454
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
  end
  object mmResult: TMemo
    Left = 0
    Top = 49
    Width = 772
    Height = 376
    Align = alClient
    ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
    TabOrder = 1
  end
  object OpenDialog1: TOpenDialog
    Left = 48
    Top = 104
  end
  object SaveDialog1: TSaveDialog
    Left = 16
    Top = 96
  end
end
