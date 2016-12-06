object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'SQL '#33050#26412#29983#25104#36716#25442#22120#31034#20363'(SQL script converter demo)'
  ClientHeight = 362
  ClientWidth = 807
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 12
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 807
    Height = 185
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object DBGrid1: TDBGrid
      Left = 0
      Top = 0
      Width = 624
      Height = 185
      Align = alClient
      DataSource = DataSource1
      TabOrder = 0
      TitleFont.Charset = GB2312_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -12
      TitleFont.Name = #23435#20307
      TitleFont.Style = []
    end
    object Panel2: TPanel
      Left = 624
      Top = 0
      Width = 183
      Height = 185
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      object Label1: TLabel
        Left = 16
        Top = 56
        Width = 48
        Height = 12
        Caption = #30446#26631#34920#21517
      end
      object Button1: TButton
        Left = 16
        Top = 16
        Width = 145
        Height = 25
        Caption = #29983#25104#33050#26412'(Generate SQL)'
        TabOrder = 0
        OnClick = Button1Click
      end
      object edtTableName: TEdit
        Left = 70
        Top = 53
        Width = 99
        Height = 20
        TabOrder = 1
        Text = 'test'
      end
      object chkAllAsInsert: TCheckBox
        Left = 16
        Top = 80
        Width = 153
        Height = 17
        Caption = #20840#37096#25353#25554#20837#25805#20316#29983#25104
        TabOrder = 2
      end
      object chkByTemplate: TCheckBox
        Left = 16
        Top = 104
        Width = 137
        Height = 17
        Caption = #20351#29992#27169#26495#29983#25104'SQL'#33050#26412
        TabOrder = 3
      end
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 185
    Width = 807
    Height = 177
    ActivePage = tsMSSQL
    Align = alClient
    TabOrder = 1
    object tsMSSQL: TTabSheet
      Caption = 'MSSQL'
      object mmMSSQL: TMemo
        Left = 0
        Top = 0
        Width = 799
        Height = 149
        Align = alClient
        TabOrder = 0
      end
    end
    object tsPgSQL: TTabSheet
      Caption = 'PostgreSQL'
      ImageIndex = 1
      object mmPgSQL: TMemo
        Left = 0
        Top = 0
        Width = 799
        Height = 149
        Align = alClient
        TabOrder = 0
      end
    end
    object tsMySQL: TTabSheet
      Caption = 'MySQL'
      ImageIndex = 2
      object mmMySQL: TMemo
        Left = 0
        Top = 0
        Width = 799
        Height = 149
        Align = alClient
        TabOrder = 0
      end
    end
  end
  object DataSource1: TDataSource
    Left = 584
    Top = 152
  end
end
