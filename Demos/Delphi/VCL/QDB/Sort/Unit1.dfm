object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'QDataSet '#25490#24207'(Sort)'
  ClientHeight = 394
  ClientWidth = 743
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
    Width = 743
    Height = 36
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 5
      Top = 10
      Width = 100
      Height = 13
      Caption = #34920#36798#24335'(Expression):'
    end
    object ComboBox1: TComboBox
      Left = 111
      Top = 6
      Width = 306
      Height = 21
      TabOrder = 0
      TextHint = #35831#36755#20837#25110#36873#25321#19968#20010#25490#24207#34920#36798#24335#65288'Please select or input the Expression)'
      Items.Strings = (
        'Id Desc'
        'Age,Id'
        'Name NASC'
        'Id NASC')
    end
    object Button1: TButton
      Left = 418
      Top = 4
      Width = 75
      Height = 25
      Caption = #24212#29992'(Apply)'
      TabOrder = 1
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 494
      Top = 4
      Width = 155
      Height = 25
      Caption = #25353#25340#38899#25490#24207'(Sort by Spell)'
      TabOrder = 2
      OnClick = Button2Click
    end
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 36
    Width = 558
    Height = 332
    Align = alClient
    DataSource = DataSource1
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object DBMemo1: TDBMemo
    Left = 558
    Top = 36
    Width = 185
    Height = 332
    Align = alRight
    TabOrder = 2
  end
  object Panel2: TPanel
    Left = 0
    Top = 368
    Width = 743
    Height = 26
    Align = alBottom
    Caption = 'Panel2'
    TabOrder = 3
    object DBNavigator1: TDBNavigator
      Left = 1
      Top = 1
      Width = 240
      Height = 24
      DataSource = DataSource1
      Align = alLeft
      TabOrder = 0
      ExplicitLeft = 256
      ExplicitHeight = 25
    end
  end
  object DataSource1: TDataSource
    Left = 312
    Top = 104
  end
end
