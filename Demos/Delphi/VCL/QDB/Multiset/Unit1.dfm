object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'QDataSet '#22810#32467#26524#38598#23637#31034
  ClientHeight = 445
  ClientWidth = 629
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
  object DBGrid1: TDBGrid
    Left = 0
    Top = 41
    Width = 629
    Height = 404
    Align = alClient
    DataSource = DataSource1
    ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
    Options = [dgEditing, dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgCancelOnExit, dgTitleClick, dgTitleHotTrack]
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 629
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Button1: TButton
      Left = 100
      Top = 10
      Width = 75
      Height = 25
      Caption = #19979#19968#32467#26524#38598
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 8
      Top = 10
      Width = 75
      Height = 25
      Caption = #19978#19968#32467#26524#38598
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 192
      Top = 10
      Width = 75
      Height = 25
      Caption = #21024#38500#32467#26524#38598
      TabOrder = 2
      OnClick = Button3Click
    end
  end
  object DataSource1: TDataSource
    Left = 312
    Top = 232
  end
end
