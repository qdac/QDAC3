object Form1: TForm1
  Left = 0
  Top = 0
  Caption = #20027#20174#34920#31034#20363
  ClientHeight = 385
  ClientWidth = 628
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
    Width = 320
    Height = 320
    Align = alLeft
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object DBGrid2: TDBGrid
    Left = 320
    Top = 41
    Width = 308
    Height = 320
    Align = alClient
    DataSource = DataSource2
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 628
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object Label1: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 622
      Height = 35
      Align = alClient
      Caption = 
        ' '#26412#31034#20363#32467#21512' OnMasterChanged '#21644'AddDataSet '#26469#32531#23384#26126#32454#32467#26524#26469#23454#29616#26126#32454#25968#25454#30340#28176#22686#24335#26597#35810#30340#25928#26524#65292#20174#32780#20943#23569#19981 +
        #24517#35201#30340#25968#25454#26597#35810#24320#38144#12290#24744#21487#20197#32467#21512' MasterDetail '#30340#31532#20108#20010#26469#24310#36831#21152#36733#26126#32454#36827#19968#27493#20943#23569#19981#24517#35201#30340#26597#35810#12290
      Layout = tlCenter
      WordWrap = True
      ExplicitWidth = 616
      ExplicitHeight = 26
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 361
    Width = 628
    Height = 24
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
  end
  object DataSource1: TDataSource
    Left = 240
    Top = 24
  end
  object DataSource2: TDataSource
    Left = 600
    Top = 32
  end
end
