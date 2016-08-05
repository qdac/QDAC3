object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'QDataSet '#21435#37325#20989#25968#28436#31034'(Distinct  Demo)'
  ClientHeight = 356
  ClientWidth = 717
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
  object Panel2: TPanel
    Left = 0
    Top = 41
    Width = 717
    Height = 315
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitLeft = -84
    ExplicitTop = 57
    ExplicitWidth = 713
    ExplicitHeight = 299
    object Splitter1: TSplitter
      Left = 361
      Top = 0
      Height = 315
      ExplicitLeft = 355
      ExplicitHeight = 392
    end
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 361
      Height = 315
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitHeight = 299
      object Panel4: TPanel
        Left = 0
        Top = 0
        Width = 361
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        Caption = #21407#22987#25968#25454'(Origin Data)'
        TabOrder = 0
      end
      object DBGrid1: TDBGrid
        Left = 0
        Top = 41
        Width = 361
        Height = 274
        Align = alClient
        DataSource = DataSource1
        ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
        Options = [dgEditing, dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgCancelOnExit, dgTitleClick, dgTitleHotTrack]
        TabOrder = 1
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
      end
    end
    object Panel5: TPanel
      Left = 364
      Top = 0
      Width = 353
      Height = 315
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitWidth = 349
      ExplicitHeight = 299
      object Panel6: TPanel
        Left = 0
        Top = 0
        Width = 353
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        Caption = #21435#37325#21518#25968#25454'(Distinct Data)'
        TabOrder = 0
        ExplicitWidth = 349
      end
      object DBGrid2: TDBGrid
        Left = 0
        Top = 41
        Width = 353
        Height = 274
        Align = alClient
        DataSource = DataSource2
        ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
        TabOrder = 1
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 717
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = 272
    ExplicitTop = 176
    ExplicitWidth = 185
    object Label1: TLabel
      Left = 8
      Top = 14
      Width = 141
      Height = 13
      Caption = #26816#26597#23383#27573#21015#34920'(Check fields):'
    end
    object Edit1: TEdit
      Left = 155
      Top = 11
      Width = 326
      Height = 21
      TabOrder = 0
      Text = 'Age'
    end
    object Button1: TButton
      Left = 487
      Top = 9
      Width = 98
      Height = 25
      Caption = #21435#37325'(Distinct)'
      TabOrder = 1
      OnClick = Button1Click
    end
  end
  object DataSource2: TDataSource
    Left = 440
    Top = 328
  end
  object DataSource1: TDataSource
    Left = 16
    Top = 328
  end
end
