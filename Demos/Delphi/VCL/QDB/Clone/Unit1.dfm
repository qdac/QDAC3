object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'QDataSet '#20811#38534#31034#20363'(Clone Demo)'
  ClientHeight = 433
  ClientWidth = 740
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
    Width = 740
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Bevel1: TBevel
      Left = 0
      Top = 39
      Width = 740
      Height = 2
      Align = alBottom
      Shape = bsBottomLine
      ExplicitTop = -9
    end
    object Label1: TLabel
      Left = 16
      Top = 13
      Width = 216
      Height = 13
      Caption = #20811#38534#21518#30340#25968#25454#38598#30340#22686#21024#38500#25913#20250#30456#20114#24433#21709#12290
    end
    object Button1: TButton
      Left = 616
      Top = 8
      Width = 75
      Height = 25
      Caption = #20811#38534'(&C)'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 41
    Width = 740
    Height = 392
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Splitter1: TSplitter
      Left = 361
      Top = 0
      Height = 392
      ExplicitLeft = 355
    end
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 361
      Height = 392
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
        Caption = #28304#25968#25454#38598'(Source DataSet)'
        TabOrder = 0
      end
      object DBGrid1: TDBGrid
        Left = 0
        Top = 41
        Width = 361
        Height = 351
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
    end
    object Panel5: TPanel
      Left = 364
      Top = 0
      Width = 376
      Height = 392
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object Panel6: TPanel
        Left = 0
        Top = 0
        Width = 376
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        Caption = #30446#26631#25968#25454#38598'(Dest Dataset)'
        TabOrder = 0
      end
      object DBGrid2: TDBGrid
        Left = 0
        Top = 41
        Width = 376
        Height = 351
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
    Top = 400
  end
  object DataSource2: TDataSource
    Left = 440
    Top = 392
  end
end
