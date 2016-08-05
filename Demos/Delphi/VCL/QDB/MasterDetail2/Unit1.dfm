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
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object DBGrid1: TDBGrid
    Left = 0
    Top = 41
    Width = 320
    Height = 344
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
    Height = 344
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
    ExplicitLeft = 232
    ExplicitTop = 192
    ExplicitWidth = 185
    object chkDelayFetch: TCheckBox
      Left = 16
      Top = 13
      Width = 121
      Height = 17
      Caption = #24310#36831#26356#26032#26126#32454#25968#25454
      TabOrder = 0
    end
  end
  object DataSource1: TDataSource
    Left = 240
    Top = 24
  end
  object DataSource2: TDataSource
    Left = 600
    Top = 32
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 50
    OnTimer = Timer1Timer
    Left = 233
    Top = 137
  end
end
