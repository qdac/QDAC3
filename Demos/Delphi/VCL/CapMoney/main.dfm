object Form4: TForm4
  Left = 0
  Top = 0
  Caption = #20013#25991#36135#24065#22823#23567#36716#25442
  ClientHeight = 423
  ClientWidth = 765
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
    Left = 217
    Top = 0
    Width = 548
    Height = 423
    Align = alClient
    TabOrder = 0
    ExplicitLeft = 0
    ExplicitTop = 41
    ExplicitWidth = 447
    ExplicitHeight = 160
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 217
    Height = 423
    Align = alLeft
    TabOrder = 1
    ExplicitHeight = 415
    object Label1: TLabel
      Left = 9
      Top = 226
      Width = 64
      Height = 13
      Caption = #21069#23548#23383#31526#20018':'
    end
    object Label2: TLabel
      Left = 9
      Top = 254
      Width = 64
      Height = 13
      Caption = #32467#26463#23383#31526#20018':'
    end
    object Label3: TLabel
      Left = 9
      Top = 282
      Width = 64
      Height = 13
      Caption = #36127#20540#23383#31526#20018':'
    end
    object Label4: TLabel
      Left = 21
      Top = 310
      Width = 52
      Height = 13
      Caption = #33293#20837#31639#27861':'
    end
    object Label5: TLabel
      Left = 21
      Top = 339
      Width = 52
      Height = 13
      Caption = #32467#23614#20301#25968':'
    end
    object Label6: TLabel
      Left = 33
      Top = 367
      Width = 40
      Height = 13
      Caption = #20998#32452#25968':'
    end
    object Button1: TButton
      Left = 21
      Top = 390
      Width = 75
      Height = 25
      Caption = #27979#35797#29992#20363
      TabOrder = 0
      OnClick = Button1Click
    end
    object edtMoney: TEdit
      Left = 8
      Top = 14
      Width = 121
      Height = 21
      TabOrder = 1
      Text = '123.545'
    end
    object Button2: TButton
      Left = 135
      Top = 12
      Width = 75
      Height = 25
      Caption = #36716#25442
      TabOrder = 2
      OnClick = Button2Click
    end
    object GroupBox1: TGroupBox
      Left = 8
      Top = 41
      Width = 191
      Height = 168
      Caption = #26684#24335#26631#35760
      TabOrder = 3
      object chkIncNum: TCheckBox
        Left = 16
        Top = 43
        Width = 73
        Height = 17
        Caption = #21253#21547#25968#23383
        Checked = True
        State = cbChecked
        TabOrder = 0
      end
      object chkIncUnit: TCheckBox
        Left = 16
        Top = 66
        Width = 73
        Height = 17
        Caption = #21253#21547#21333#20301
        Checked = True
        State = cbChecked
        TabOrder = 1
      end
      object chkHideZero: TCheckBox
        Left = 16
        Top = 89
        Width = 98
        Height = 17
        Caption = #38544#34255#24038#20391#38646#20540
        Checked = True
        State = cbChecked
        TabOrder = 2
      end
      object chkMergeZero: TCheckBox
        Left = 16
        Top = 112
        Width = 98
        Height = 17
        Caption = #21512#24182#20013#38388#38646#20540
        Checked = True
        State = cbChecked
        TabOrder = 3
      end
      object chkPatchEnd: TCheckBox
        Left = 16
        Top = 135
        Width = 146
        Height = 17
        Caption = #25972#25968#20803#26102#36861#21152#32467#23614#23383#31526
        Checked = True
        State = cbChecked
        TabOrder = 4
      end
      object cbxPreset: TComboBox
        Left = 16
        Top = 16
        Width = 73
        Height = 21
        ItemIndex = 0
        TabOrder = 5
        Text = #38405#35835
        OnChange = cbxPresetChange
        Items.Strings = (
          #38405#35835
          #22871#25171
          #33258#23450#20041)
      end
    end
    object edtPreStr: TEdit
      Left = 79
      Top = 222
      Width = 121
      Height = 21
      TabOrder = 4
      Text = #65509
    end
    object edtPatchStr: TEdit
      Left = 79
      Top = 250
      Width = 121
      Height = 21
      TabOrder = 5
      Text = #25972
    end
    object edtNegStr: TEdit
      Left = 79
      Top = 278
      Width = 121
      Height = 21
      TabOrder = 6
      Text = #36127
    end
    object cbxRoundMode: TComboBox
      Left = 79
      Top = 306
      Width = 120
      Height = 21
      ItemIndex = 0
      TabOrder = 7
      Text = #26080
      Items.Strings = (
        #26080
        #22235#33293#20116#20837
        #38134#34892#23478#31639#27861)
    end
    object seEndDigits: TSpinEdit
      Left = 79
      Top = 334
      Width = 67
      Height = 22
      MaxValue = 4
      MinValue = -16
      TabOrder = 8
      Value = 2
    end
    object seGroupNum: TSpinEdit
      Left = 79
      Top = 362
      Width = 67
      Height = 22
      MaxValue = 20
      MinValue = 0
      TabOrder = 9
      Value = 0
    end
  end
end
