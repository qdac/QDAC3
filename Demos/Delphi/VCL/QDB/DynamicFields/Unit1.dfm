object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'QDataSet '#21160#24577#28155#21152#23383#27573'(Dynamic Add Field)'
  ClientHeight = 356
  ClientWidth = 608
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
    Width = 608
    Height = 73
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 5
      Top = 14
      Width = 100
      Height = 13
      Caption = #23383#27573#21517'(Field Name):'
    end
    object Label2: TLabel
      Left = 268
      Top = 14
      Width = 110
      Height = 13
      Caption = #23383#27573#31867#22411'(Data Type):'
    end
    object Label3: TLabel
      Left = 8
      Top = 42
      Width = 93
      Height = 13
      Caption = #23383#27573#38271#24230'(Length):'
    end
    object ComboBox1: TComboBox
      Left = 384
      Top = 10
      Width = 137
      Height = 21
      ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
      ItemIndex = 0
      TabOrder = 0
      Text = #25972#25968
      Items.Strings = (
        #25972#25968
        #23383#31526#20018
        #28014#28857
        #26085#26399#26102#38388)
    end
    object ComboBox2: TComboBox
      Left = 111
      Top = 10
      Width = 151
      Height = 21
      ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
      ItemIndex = 0
      TabOrder = 1
      Text = 'Id'
      TextHint = #35831#36873#25321#23383#27573'(Select field)'
      Items.Strings = (
        'Id'
        'Age'
        'Name'
        'Sex'
        'Scale'
        'Comment')
    end
    object Button1: TButton
      Left = 613
      Top = 7
      Width = 75
      Height = 25
      Caption = #19979#19968#20010'(&Next)'
      TabOrder = 2
    end
    object Button2: TButton
      Left = 532
      Top = 8
      Width = 75
      Height = 25
      Caption = #28155#21152'(&A)'
      TabOrder = 3
      OnClick = Button2Click
    end
    object Button4: TButton
      Left = 613
      Top = 40
      Width = 75
      Height = 25
      Caption = #26368#21518#19968#20010
      TabOrder = 4
    end
    object SpinEdit1: TSpinEdit
      Left = 111
      Top = 37
      Width = 58
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 5
      Value = 0
    end
    object Button3: TButton
      Left = 532
      Top = 42
      Width = 75
      Height = 25
      Caption = #21024#38500'(&D)'
      TabOrder = 6
      OnClick = Button3Click
    end
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 73
    Width = 608
    Height = 243
    Align = alClient
    DataSource = DataSource1
    ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object Panel2: TPanel
    Left = 0
    Top = 316
    Width = 608
    Height = 40
    Align = alBottom
    TabOrder = 2
  end
  object DataSource1: TDataSource
    Left = 280
    Top = 40
  end
end
