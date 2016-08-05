object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'QXML'#23545#27604#27979#35797'-201405052013'
  ClientHeight = 370
  ClientWidth = 529
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 529
    Height = 57
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 539
    object lblItem: TLabel
      Left = 16
      Top = 15
      Width = 3
      Height = 13
    end
    object chkLoad: TCheckBox
      Left = 16
      Top = 34
      Width = 97
      Height = 17
      Caption = #21152#36733#27979#35797
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object chkSave: TCheckBox
      Left = 116
      Top = 34
      Width = 97
      Height = 17
      Caption = #20445#23384#27979#35797
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object chkIO: TCheckBox
      Left = 316
      Top = 34
      Width = 97
      Height = 17
      Caption = #21152#36733'+'#20445#23384#27979#35797
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object chkCreate: TCheckBox
      Left = 216
      Top = 34
      Width = 97
      Height = 17
      Caption = #21019#24314#27979#35797
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object Panel2: TPanel
      Left = 422
      Top = 0
      Width = 107
      Height = 57
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 4
      ExplicitLeft = 432
      object Button5: TButton
        Left = 22
        Top = 16
        Width = 75
        Height = 25
        Caption = #24320#22987'(&S)'
        TabOrder = 0
        OnClick = Button5Click
      end
    end
  end
  object mmResult: TMemo
    Left = 0
    Top = 57
    Width = 529
    Height = 313
    Align = alClient
    ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
    ReadOnly = True
    TabOrder = 1
    ExplicitWidth = 539
    ExplicitHeight = 323
  end
end
