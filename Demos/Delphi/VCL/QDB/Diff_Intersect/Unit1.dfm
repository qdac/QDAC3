object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'QDataSet '#24046#24322#27604#36739#28436#31034'(Diff compare demo)'
  ClientHeight = 426
  ClientWidth = 712
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
    Width = 712
    Height = 79
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitLeft = 1
    ExplicitTop = -5
    object Bevel1: TBevel
      Left = 0
      Top = 77
      Width = 712
      Height = 2
      Align = alBottom
      Shape = bsBottomLine
      ExplicitTop = -9
      ExplicitWidth = 740
    end
    object Label1: TLabel
      Left = 16
      Top = 16
      Width = 130
      Height = 13
      Caption = #27604#36739#23383#27573'(Lookup fields)'#65306
    end
    object Button1: TButton
      Left = 559
      Top = 11
      Width = 89
      Height = 25
      Caption = #24046#24322'(Diff)'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 447
      Top = 11
      Width = 106
      Height = 25
      Caption = #20849#26377'(Intersect)'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Edit1: TEdit
      Left = 152
      Top = 13
      Width = 289
      Height = 21
      ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
      TabOrder = 2
      Text = 'Id,Age,Name'
    end
    object CheckBox1: TCheckBox
      Left = 24
      Top = 48
      Width = 177
      Height = 17
      Caption = #21253#21547#28304#25968#25454#38598'1'#20013#30340#24046#24322#35760#24405
      TabOrder = 3
    end
    object CheckBox2: TCheckBox
      Left = 216
      Top = 48
      Width = 177
      Height = 17
      Caption = #21253#21547#28304#25968#25454#38598'1'#20013#30340#24046#24322#35760#24405
      TabOrder = 4
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 79
    Width = 712
    Height = 347
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 49
    ExplicitHeight = 377
    object Splitter1: TSplitter
      Left = 345
      Top = 0
      Height = 347
      ExplicitLeft = 355
      ExplicitHeight = 392
    end
    object Panel5: TPanel
      Left = 348
      Top = 0
      Width = 364
      Height = 347
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitHeight = 377
      object Panel6: TPanel
        Left = 0
        Top = 0
        Width = 364
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        Caption = #32467#26524'(Result)'
        TabOrder = 0
      end
      object DBGrid2: TDBGrid
        Left = 0
        Top = 41
        Width = 364
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
    object Panel9: TPanel
      Left = 0
      Top = 0
      Width = 345
      Height = 347
      Align = alLeft
      TabOrder = 1
      ExplicitHeight = 377
      object Panel7: TPanel
        Left = 1
        Top = 169
        Width = 343
        Height = 177
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitHeight = 207
        object Panel8: TPanel
          Left = 0
          Top = 0
          Width = 343
          Height = 35
          Align = alTop
          BevelOuter = bvNone
          Caption = #28304#25968#25454#38598'2(Source DataSet2)'
          TabOrder = 0
        end
        object DBGrid3: TDBGrid
          Left = 0
          Top = 35
          Width = 343
          Height = 142
          Align = alClient
          DataSource = DataSource3
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
      object Panel3: TPanel
        Left = 1
        Top = 1
        Width = 343
        Height = 168
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object Panel4: TPanel
          Left = 0
          Top = 0
          Width = 343
          Height = 35
          Align = alTop
          BevelOuter = bvNone
          Caption = #28304#25968#25454#38598'1(Source DataSet1)'
          TabOrder = 0
        end
        object DBGrid1: TDBGrid
          Left = 0
          Top = 35
          Width = 343
          Height = 133
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
    end
  end
  object DataSource1: TDataSource
    Left = 40
    Top = 136
  end
  object DataSource2: TDataSource
    Left = 456
    Top = 320
  end
  object DataSource3: TDataSource
    Left = 24
    Top = 288
  end
end
