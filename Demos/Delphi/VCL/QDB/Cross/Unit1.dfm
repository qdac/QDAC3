object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'QDataSet '#20132#21449#34920#28436#31034'(Cross table demo)'
  ClientHeight = 388
  ClientWidth = 650
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
    Width = 650
    Height = 347
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Splitter1: TSplitter
      Left = 361
      Top = 0
      Height = 347
      ExplicitLeft = 355
      ExplicitHeight = 392
    end
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 361
      Height = 347
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
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
        Height = 306
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
      Width = 286
      Height = 347
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object Panel6: TPanel
        Left = 0
        Top = 0
        Width = 286
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        Caption = #20132#21449#32467#26524'(Exchanged Result)'
        TabOrder = 0
      end
      object DBGrid2: TDBGrid
        Left = 0
        Top = 41
        Width = 286
        Height = 306
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
    Width = 650
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Button1: TButton
      Left = 487
      Top = 9
      Width = 98
      Height = 25
      Caption = #20132#21449'(Cross)'
      TabOrder = 0
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
