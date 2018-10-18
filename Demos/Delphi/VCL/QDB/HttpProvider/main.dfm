object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Http Provider '#31034#20363
  ClientHeight = 627
  ClientWidth = 1009
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PopupMode = pmAuto
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1009
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Button1: TButton
      Left = 16
      Top = 10
      Width = 75
      Height = 25
      Caption = #25171#24320#36830#25509
      TabOrder = 0
      OnClick = Button1Click
    end
    object ComboBox1: TComboBox
      Left = 97
      Top = 12
      Width = 145
      Height = 21
      ItemIndex = 0
      TabOrder = 1
      Text = 'PgSQLDemo'
      OnChange = ComboBox1Change
      Items.Strings = (
        'PgSQLDemo'
        'MySQLDemo')
    end
    object Button2: TButton
      Left = 360
      Top = 10
      Width = 75
      Height = 25
      Caption = #25191#34892#33050#26412
      TabOrder = 2
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 271
      Top = 10
      Width = 75
      Height = 25
      Caption = #25171#24320#25968#25454#38598
      TabOrder = 3
      OnClick = Button3Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 41
    Width = 1009
    Height = 264
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 1
    object mmSQL: TMemo
      Left = 1
      Top = 1
      Width = 1007
      Height = 231
      Align = alClient
      Lines.Strings = (
        'Information_Schema.Columns')
      ScrollBars = ssBoth
      TabOrder = 0
    end
    object Panel3: TPanel
      Left = 1
      Top = 232
      Width = 1007
      Height = 31
      Align = alBottom
      Alignment = taLeftJustify
      BevelOuter = bvSpace
      Caption = 
        ' '#20986#20110#23433#20840#21407#22240#65292#24744#21482#33021#36755#20837' Information_Schema.Columns/Information_Schema.Tabl' +
        'es '#26469#27979#35797#65292#35201#27979#35797#20854#23427#33050#26412#65292#35831#22312#24744#30340#26381#21153#22120#19978#20462#25913'$Config["EnableSQL"]'#30340#20540#20026'true'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 305
    Width = 1009
    Height = 322
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 2
    object TabSheet1: TTabSheet
      Caption = 'Result'
      object DBGrid1: TDBGrid
        Left = 0
        Top = 0
        Width = 1001
        Height = 294
        Align = alClient
        DataSource = DataSource1
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Statics'
      ImageIndex = 1
      object mmLogs: TMemo
        Left = 0
        Top = 0
        Width = 1001
        Height = 294
        Align = alClient
        TabOrder = 0
      end
    end
  end
  object DataSource1: TDataSource
    Left = 880
    Top = 8
  end
end
