object Form5: TForm5
  Left = 0
  Top = 0
  Caption = 'QWorker'#20219#21153#35745#21010#28436#31034
  ClientHeight = 424
  ClientWidth = 637
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 12
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 424
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitHeight = 324
    object Panel2: TPanel
      Left = 0
      Top = 0
      Width = 185
      Height = 25
      Align = alTop
      BevelOuter = bvNone
      Caption = #20316#19994#21015#34920
      TabOrder = 0
      object btnAddNew: TButton
        Left = 160
        Top = 0
        Width = 25
        Height = 25
        Align = alRight
        Caption = '+'
        TabOrder = 0
        OnClick = btnAddNewClick
      end
      object Button4: TButton
        Left = 135
        Top = 0
        Width = 25
        Height = 25
        Align = alRight
        Caption = 'C'
        TabOrder = 1
        OnClick = Button4Click
      end
    end
    object lbxPlans: TListBox
      Left = 0
      Top = 25
      Width = 185
      Height = 399
      Align = alClient
      ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
      ItemHeight = 12
      TabOrder = 1
      ExplicitHeight = 299
    end
  end
  object Panel3: TPanel
    Left = 185
    Top = 0
    Width = 452
    Height = 424
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 341
    ExplicitHeight = 324
    object Panel4: TPanel
      Left = 0
      Top = 0
      Width = 452
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitWidth = 341
      object Button1: TButton
        Left = 16
        Top = 8
        Width = 75
        Height = 25
        Caption = #20449#21495'1'
        TabOrder = 0
        OnClick = Button1Click
      end
      object Button2: TButton
        Left = 100
        Top = 8
        Width = 75
        Height = 25
        Caption = #20449#21495'2'
        TabOrder = 1
      end
      object Button3: TButton
        Left = 181
        Top = 8
        Width = 75
        Height = 25
        Caption = #20449#21495'3'
        TabOrder = 2
      end
    end
    object mmLog: TMemo
      Left = 0
      Top = 41
      Width = 452
      Height = 383
      Align = alClient
      ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
      TabOrder = 1
      ExplicitWidth = 341
      ExplicitHeight = 283
    end
  end
end
