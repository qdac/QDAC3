object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'QDataSet '#36807#28388#28436#31034'(Filter Demo)'
  ClientHeight = 415
  ClientWidth = 656
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
    Width = 656
    Height = 65
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Panel3: TPanel
      Left = 488
      Top = 0
      Width = 168
      Height = 65
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object Button1: TButton
        Left = 6
        Top = 36
        Width = 155
        Height = 25
        Caption = #24212#29992'(Apply)'
        TabOrder = 0
        OnClick = Button1Click
      end
      object Button2: TButton
        Left = 6
        Top = 5
        Width = 155
        Height = 25
        Caption = #23450#21046#36807#28388'(Custom Filter)'
        TabOrder = 1
        OnClick = Button2Click
      end
    end
    object Panel4: TPanel
      Left = 0
      Top = 0
      Width = 488
      Height = 65
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object Label1: TLabel
        Left = 5
        Top = 10
        Width = 100
        Height = 13
        Caption = #34920#36798#24335'(Expression):'
      end
      object ComboBox1: TComboBox
        AlignWithMargins = True
        Left = 3
        Top = 36
        Width = 482
        Height = 21
        Margins.Bottom = 8
        Align = alBottom
        ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
        TabOrder = 0
        TextHint = #35831#36755#20837#25110#36873#25321#19968#20010#36807#28388#34920#36798#24335#65288'Please select or input the Expression)'
        Items.Strings = (
          'Name like '#39#29579'%'#39
          'Name like '#39#29579'%'#39' and Age>20'
          'Age>50 and (Name like '#39#29579'%'#39' or Name like '#39#36213'%'#39')'
          'Name ~ '#39#29579'.+'#39
          'Id ~ '#39'/cc_\d\d$/is'#39
          'Age>30')
      end
      object Button3: TButton
        Left = 384
        Top = 5
        Width = 75
        Height = 25
        Caption = 'Button3'
        TabOrder = 1
        OnClick = Button3Click
      end
    end
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 65
    Width = 471
    Height = 327
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
  object DBMemo1: TDBMemo
    Left = 471
    Top = 65
    Width = 185
    Height = 327
    Align = alRight
    DataField = 'Comment'
    DataSource = DataSource1
    ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
    TabOrder = 2
  end
  object Panel2: TPanel
    Left = 0
    Top = 392
    Width = 656
    Height = 23
    Align = alBottom
    TabOrder = 3
    OnClick = Panel2Click
    object DBNavigator1: TDBNavigator
      Left = 1
      Top = 1
      Width = 240
      Height = 21
      DataSource = DataSource1
      Align = alLeft
      TabOrder = 0
    end
  end
  object DataSource1: TDataSource
    Left = 312
    Top = 104
  end
end
