object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'QDB PostgreSQL '#28436#31034'(Demo)'
  ClientHeight = 403
  ClientWidth = 788
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 281
    Top = 105
    Height = 298
    ExplicitLeft = 336
    ExplicitTop = 168
    ExplicitHeight = 100
  end
  object Memo1: TMemo
    Left = 0
    Top = 105
    Width = 281
    Height = 298
    Align = alLeft
    ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 788
    Height = 105
    Align = alTop
    TabOrder = 1
    object Button1: TButton
      Left = 116
      Top = 11
      Width = 99
      Height = 25
      Caption = #25171#24320#25968#25454#38598
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 217
      Top = 11
      Width = 99
      Height = 25
      Caption = #25171#24320#25968#25454#27969
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 318
      Top = 11
      Width = 99
      Height = 25
      Caption = #25191#34892#33050#26412
      TabOrder = 2
      OnClick = Button3Click
    end
    object btnConnect: TButton
      Left = 15
      Top = 11
      Width = 99
      Height = 25
      Caption = #36830#25509#25968#25454#24211
      TabOrder = 3
      OnClick = btnConnectClick
    end
    object Button4: TButton
      Left = 520
      Top = 11
      Width = 99
      Height = 25
      Caption = #21442#25968#21270#25191#34892
      TabOrder = 4
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 419
      Top = 11
      Width = 99
      Height = 25
      Caption = #22810#25968#25454#38598
      TabOrder = 5
      OnClick = Button5Click
    end
    object Panel4: TPanel
      Left = 1
      Top = 42
      Width = 786
      Height = 62
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 6
      object Label2: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 14
        Height = 56
        Align = alLeft
        AutoSize = False
        Caption = #33050#26412
        WordWrap = True
      end
      object mmSQL: TMemo
        Left = 20
        Top = 0
        Width = 766
        Height = 62
        Align = alClient
        Lines.Strings = (
          
            'select * from dict_cities where code like '#39'22%'#39' and type='#39'PP'#39' or' +
            'der by code')
        TabOrder = 0
      end
    end
    object btnApply: TButton
      Left = 621
      Top = 11
      Width = 99
      Height = 25
      Caption = #25552#20132#21464#26356
      TabOrder = 7
      OnClick = btnApplyClick
    end
  end
  object Panel2: TPanel
    Left = 284
    Top = 105
    Width = 504
    Height = 298
    Align = alClient
    TabOrder = 2
    object DBGrid1: TDBGrid
      Left = 1
      Top = 1
      Width = 384
      Height = 296
      Align = alClient
      DataSource = DataSource1
      ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
      TabOrder = 0
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'Tahoma'
      TitleFont.Style = []
    end
    object Panel3: TPanel
      Left = 385
      Top = 1
      Width = 118
      Height = 296
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      object Button6: TButton
        Left = 16
        Top = 16
        Width = 75
        Height = 25
        Caption = #19978#19968#25968#25454#38598
        TabOrder = 0
        OnClick = Button6Click
      end
      object Button7: TButton
        Left = 16
        Top = 47
        Width = 75
        Height = 25
        Caption = #19979#19968#25968#25454#38598
        TabOrder = 1
        OnClick = Button7Click
      end
    end
  end
  object DataSource1: TDataSource
    Left = 160
    Top = 184
  end
  object UniConnection1: TUniConnection
    ProviderName = 'PostgreSQL'
    Port = 15432
    Database = 'QDAC_Demo'
    Username = 'qdac'
    Server = 'www.qdac.cc'
    Connected = True
    Left = 432
    Top = 160
    EncryptedPassword = 'AEFF9BFF9EFF9CFFD1FFBBFF9AFF92FF90FF'
  end
  object PostgreSQLUniProvider1: TPostgreSQLUniProvider
    Left = 432
    Top = 224
  end
  object UniQuery1: TUniQuery
    Connection = UniConnection1
    SQL.Strings = (
      'select * from config where id<:id')
    Left = 512
    Top = 160
    ParamData = <
      item
        DataType = ftInteger
        Name = 'id'
        ParamType = ptInput
        Value = 100
      end>
  end
end
