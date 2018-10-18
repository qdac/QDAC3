object Form6: TForm6
  Left = 0
  Top = 0
  Caption = #23439#26367#25442#28436#31034
  ClientHeight = 356
  ClientWidth = 718
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
  object Memo2: TMemo
    Left = 0
    Top = 217
    Width = 718
    Height = 139
    Align = alClient
    ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 718
    Height = 217
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    OnClick = Panel1Click
    object Label1: TLabel
      Left = 24
      Top = 65
      Width = 72
      Height = 13
      Caption = #23439#36215#22987#23450#20041#65306
    end
    object Label2: TLabel
      Left = 240
      Top = 65
      Width = 72
      Height = 13
      Caption = #23439#32467#26463#23450#20041#65306
    end
    object Label3: TLabel
      Left = 297
      Top = 192
      Width = 18
      Height = 13
      Caption = #8595#8595#8595
    end
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = #26367#25442
      TabOrder = 0
      OnClick = Button1Click
    end
    object Edit1: TEdit
      Left = 102
      Top = 61
      Width = 121
      Height = 21
      ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
      TabOrder = 1
      Text = '%'
    end
    object Edit2: TEdit
      Left = 318
      Top = 61
      Width = 121
      Height = 21
      ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
      TabOrder = 2
      Text = '%'
    end
    object Memo1: TMemo
      Left = 8
      Top = 88
      Width = 566
      Height = 98
      ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
      Lines.Strings = (
        #39'%Year%'#39'-"%Month%":'
        'Current Time is %Now%')
      TabOrder = 3
    end
    object Button2: TButton
      Left = 89
      Top = 8
      Width = 75
      Height = 25
      Caption = #22534#26632
      TabOrder = 4
      OnClick = Button2Click
    end
    object chkSingleQuoter: TCheckBox
      Left = 580
      Top = 12
      Width = 138
      Height = 17
      Caption = #26367#25442#21333#24341#21495#20869#23439#23450#20041
      TabOrder = 5
    end
    object chkDoubleQuoter: TCheckBox
      Left = 580
      Top = 37
      Width = 138
      Height = 17
      Caption = #26367#25442#21452#24341#21495#20869#23439#23450#20041
      TabOrder = 6
    end
    object Button3: TButton
      Left = 170
      Top = 8
      Width = 75
      Height = 25
      Caption = #20445#23384#28857
      TabOrder = 7
      OnClick = Button3Click
    end
    object chkIgnoreCase: TCheckBox
      Left = 580
      Top = 63
      Width = 121
      Height = 17
      Caption = #23439#21517#31216#24573#30053#22823#23567#20889
      TabOrder = 8
      OnClick = chkIgnoreCaseClick
    end
    object Button4: TButton
      Left = 251
      Top = 8
      Width = 75
      Height = 25
      Caption = #36895#24230#27979#35797
      TabOrder = 9
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 332
      Top = 8
      Width = 75
      Height = 25
      Caption = #32531#23384
      TabOrder = 10
      OnClick = Button5Click
    end
    object Button6: TButton
      Left = 413
      Top = 8
      Width = 75
      Height = 25
      Caption = #21160#24577#36171#20540
      TabOrder = 11
      OnClick = Button6Click
    end
    object Button7: TButton
      Left = 8
      Top = 34
      Width = 75
      Height = 25
      Caption = #26522#20030#21015#34920
      TabOrder = 12
      OnClick = Button7Click
    end
    object chkEndBySpace: TCheckBox
      Left = 448
      Top = 63
      Width = 126
      Height = 17
      Caption = #20197#38750#26631#24535#23383#31526#32467#26463
      TabOrder = 13
      OnClick = chkEndBySpaceClick
    end
    object Button8: TButton
      Left = 89
      Top = 34
      Width = 75
      Height = 25
      Caption = #29992#25143#21028#23450#23439
      TabOrder = 14
      OnClick = Button8Click
    end
    object chkEnableEscape: TCheckBox
      Left = 580
      Top = 86
      Width = 121
      Height = 17
      Caption = #35299#26512#36716#20041#23383#31526
      Checked = True
      State = cbChecked
      TabOrder = 15
      OnClick = chkIgnoreCaseClick
    end
    object chkParseParams: TCheckBox
      Left = 580
      Top = 132
      Width = 121
      Height = 17
      Caption = #35299#26512#23439#20869#30340#21442#25968
      TabOrder = 16
    end
    object Button9: TButton
      Left = 170
      Top = 35
      Width = 75
      Height = 25
      Caption = #23439#21442#25968
      TabOrder = 17
      OnClick = Button9Click
    end
  end
  object chkIgnoreMissed: TCheckBox
    Left = 580
    Top = 109
    Width = 130
    Height = 17
    Caption = #24573#30053#25214#19981#21040#30340#23439#23450#20041
    TabOrder = 2
    OnClick = chkIgnoreCaseClick
  end
end
