object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'QDataSet '#25968#25454#38598#21512#24182#28436#31034'(Merge demo)'
  ClientHeight = 455
  ClientWidth = 729
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
    Width = 729
    Height = 49
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Bevel1: TBevel
      Left = 0
      Top = 47
      Width = 729
      Height = 2
      Align = alBottom
      Shape = bsBottomLine
      ExplicitTop = -9
      ExplicitWidth = 740
    end
    object Button1: TButton
      Left = 639
      Top = 13
      Width = 75
      Height = 25
      Caption = #21512#24182
      TabOrder = 0
      OnClick = Button1Click
    end
    object RadioGroup1: TRadioGroup
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 339
      Height = 41
      Align = alLeft
      Caption = #27169#24335'(Mode)'
      Columns = 3
      ItemIndex = 0
      Items.Strings = (
        #36861#21152'(Union All)'
        #26367#25442'(Replace)'
        #34701#21512'(Union)')
      TabOrder = 1
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 49
    Width = 729
    Height = 406
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Splitter1: TSplitter
      Left = 345
      Top = 0
      Height = 406
      ExplicitLeft = 355
      ExplicitHeight = 392
    end
    object Panel5: TPanel
      Left = 348
      Top = 0
      Width = 381
      Height = 406
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object Panel6: TPanel
        Left = 0
        Top = 0
        Width = 381
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        Caption = #21512#24182#32467#26524'(Merge result)'
        TabOrder = 0
      end
      object DBGrid2: TDBGrid
        Left = 0
        Top = 41
        Width = 381
        Height = 365
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
      Height = 406
      Align = alLeft
      TabOrder = 1
      object Panel7: TPanel
        Left = 1
        Top = 169
        Width = 343
        Height = 236
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
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
          Height = 201
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
