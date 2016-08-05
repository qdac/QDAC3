object frmFind: TfrmFind
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #26597#25214
  ClientHeight = 209
  ClientWidth = 484
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 19
    Width = 52
    Height = 13
    Caption = #26597#25214#20869#23481':'
  end
  object edtKey: TEdit
    Left = 66
    Top = 15
    Width = 410
    Height = 21
    ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
    TabOrder = 0
  end
  object gbxRanges: TGroupBox
    Left = 8
    Top = 42
    Width = 468
    Height = 55
    Caption = #33539#22260
    TabOrder = 1
    object chkIncNodeName: TCheckBox
      Left = 8
      Top = 24
      Width = 65
      Height = 17
      Caption = #32467#28857#21517#31216
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object chkIncText: TCheckBox
      Left = 84
      Top = 24
      Width = 65
      Height = 17
      Caption = #32467#28857#20540
      TabOrder = 1
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 103
    Width = 468
    Height = 50
    Caption = #36873#39033
    TabOrder = 2
    object chkByCaseSensitive: TCheckBox
      Left = 8
      Top = 22
      Width = 97
      Height = 17
      Caption = #21306#20998#22823#23567#20889
      TabOrder = 0
    end
    object chkByRegex: TCheckBox
      Left = 122
      Top = 22
      Width = 97
      Height = 17
      Caption = #27491#21017#34920#36798#24335
      TabOrder = 1
    end
    object chkRestart: TCheckBox
      Left = 237
      Top = 22
      Width = 97
      Height = 17
      Caption = #37325#26032#20174#22836#24320#22987
      TabOrder = 2
    end
  end
  object btnNext: TButton
    Left = 192
    Top = 171
    Width = 75
    Height = 25
    Caption = #19979#19968#20010
    TabOrder = 3
    OnClick = btnNextClick
  end
  object btnPrior: TButton
    Left = 290
    Top = 171
    Width = 75
    Height = 25
    Caption = #19978#19968#20010
    TabOrder = 4
    OnClick = btnPriorClick
  end
  object btnList: TButton
    Left = 389
    Top = 171
    Width = 75
    Height = 25
    Caption = #21015#34920
    TabOrder = 5
    OnClick = btnListClick
  end
end
