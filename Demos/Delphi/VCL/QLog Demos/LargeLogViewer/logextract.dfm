object frmLogExtractor: TfrmLogExtractor
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #25277#21462#26085#24535
  ClientHeight = 216
  ClientWidth = 702
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #24494#36719#38597#40657
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 17
  object Label1: TLabel
    Left = 16
    Top = 89
    Width = 63
    Height = 17
    Caption = #30446#26631#34920#36798#24335':'
  end
  object Label2: TLabel
    Left = 16
    Top = 160
    Width = 284
    Height = 17
    Caption = #30446#26631#34920#36798#24335#20026#26367#25442#25903#25345#20351#29992#27491#21017#20998#32452#21517#21644' $+'#20998#32452#24207#21495
  end
  object Label3: TLabel
    Left = 16
    Top = 16
    Width = 87
    Height = 17
    Caption = #25277#21462#27491#21017#34920#36798#24335':'
  end
  object edtToReplace: TEdit
    AlignWithMargins = True
    Left = 16
    Top = 117
    Width = 549
    Height = 25
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    TabOrder = 1
  end
  object edtToSearch: TEdit
    AlignWithMargins = True
    Left = 16
    Top = 44
    Width = 549
    Height = 25
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    TabOrder = 0
  end
  object Button1: TButton
    Left = 600
    Top = 44
    Width = 75
    Height = 25
    Caption = #24320#22987'(&S)'
    ModalResult = 1
    TabOrder = 3
  end
  object Button2: TButton
    Left = 600
    Top = 86
    Width = 75
    Height = 25
    Caption = #21462#28040'(&C)'
    ModalResult = 2
    TabOrder = 4
  end
  object chkDistinct: TCheckBox
    Left = 16
    Top = 191
    Width = 284
    Height = 17
    Caption = #21512#24182#30456#21516#30340#25277#21462#32467#26524#20026#21516#19968#26465#35760#24405
    TabOrder = 2
  end
end
