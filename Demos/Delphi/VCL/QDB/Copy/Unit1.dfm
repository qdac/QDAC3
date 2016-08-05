object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'QDataSet '#25968#25454#38598#22797#21046#28436#31034'(Copy Demo)'
  ClientHeight = 404
  ClientWidth = 713
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
    Width = 713
    Height = 105
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Bevel1: TBevel
      Left = 0
      Top = 103
      Width = 713
      Height = 2
      Align = alBottom
      Shape = bsBottomLine
      ExplicitTop = -9
      ExplicitWidth = 740
    end
    object Label1: TLabel
      Left = 8
      Top = 72
      Width = 64
      Height = 13
      Caption = #36807#28388#34920#36798#24335':'
    end
    object Label2: TLabel
      Left = 328
      Top = 72
      Width = 64
      Height = 13
      Caption = #25490#24207#34920#36798#24335':'
    end
    object Button1: TButton
      Left = 631
      Top = 66
      Width = 75
      Height = 25
      Caption = #22797#21046'(&C)'
      TabOrder = 0
      OnClick = Button1Click
    end
    object RadioGroup1: TRadioGroup
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 707
      Height = 54
      Align = alTop
      Caption = #27169#24335'(Mode)'
      Columns = 9
      ItemIndex = 0
      Items.Strings = (
        #24403#21069#25968#25454
        #21407#22987#25968#25454
        #25554#20837#25968#25454
        #21024#38500#25968#25454
        #20462#25913#25968#25454
        #21464#26356#25968#25454
        #24050#25490#24207#25968#25454
        #24050#36807#28388#25968#25454
        #20165#20803#25968#25454)
      TabOrder = 1
    end
    object Edit1: TEdit
      Left = 78
      Top = 68
      Width = 227
      Height = 21
      ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
      TabOrder = 2
      OnKeyDown = Edit1KeyDown
    end
    object Edit2: TEdit
      Left = 398
      Top = 68
      Width = 227
      Height = 21
      ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
      TabOrder = 3
      OnKeyDown = Edit2KeyDown
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 105
    Width = 713
    Height = 299
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Splitter1: TSplitter
      Left = 361
      Top = 0
      Height = 299
      ExplicitLeft = 355
      ExplicitHeight = 392
    end
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 361
      Height = 299
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object Panel4: TPanel
        Left = 0
        Top = 0
        Width = 361
        Height = 41
        Align = alTop
        Alignment = taLeftJustify
        BevelOuter = bvNone
        Caption = '  '#28304#25968#25454#38598'(Source DataSet)'
        TabOrder = 0
        object chkPaged: TCheckBox
          Left = 234
          Top = 13
          Width = 121
          Height = 17
          Alignment = taLeftJustify
          Caption = #20998#39029#26174#31034'(PageView)'
          TabOrder = 0
          OnClick = chkPagedClick
        end
      end
      object DBGrid1: TDBGrid
        Left = 0
        Top = 41
        Width = 361
        Height = 258
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
      Width = 349
      Height = 299
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object Panel6: TPanel
        Left = 0
        Top = 0
        Width = 349
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        Caption = #30446#26631#25968#25454#38598'(Dest Dataset)'
        TabOrder = 0
      end
      object DBGrid2: TDBGrid
        Left = 0
        Top = 41
        Width = 349
        Height = 258
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
  object DataSource1: TDataSource
    Left = 16
    Top = 376
  end
  object DataSource2: TDataSource
    Left = 440
    Top = 376
  end
end
