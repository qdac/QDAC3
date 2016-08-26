object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'DLL Loader Host'
  ClientHeight = 401
  ClientWidth = 836
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
  object Splitter1: TSplitter
    Left = 353
    Top = 41
    Height = 360
    ExplicitLeft = 368
    ExplicitTop = 168
    ExplicitHeight = 100
  end
  object DBGrid1: TDBGrid
    Left = 356
    Top = 41
    Width = 480
    Height = 360
    Align = alClient
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 836
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Button1: TButton
      Left = 16
      Top = 10
      Width = 129
      Height = 25
      Caption = #25171#24320#25968#25454#38598
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 151
      Top = 10
      Width = 129
      Height = 25
      Caption = #25191#34892#33050#26412
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 287
      Top = 10
      Width = 129
      Height = 25
      Caption = #26522#20030#26381#21153
      TabOrder = 2
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 423
      Top = 10
      Width = 129
      Height = 25
      Caption = 'BPL '#27714#21512#26381#21153
      TabOrder = 3
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 559
      Top = 10
      Width = 129
      Height = 25
      Caption = #21160#24577#21152#36733
      TabOrder = 4
      OnClick = Button5Click
    end
    object Button6: TButton
      Left = 695
      Top = 10
      Width = 129
      Height = 25
      Caption = #20174#27969#20013#21152#36733
      TabOrder = 5
      OnClick = Button6Click
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 41
    Width = 353
    Height = 360
    Align = alLeft
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object ADODataSet1: TADODataSet
    Parameters = <>
    Left = 552
    Top = 120
  end
  object DataSource1: TDataSource
    DataSet = ADODataSet1
    Left = 624
    Top = 120
  end
  object OpenDialog1: TOpenDialog
    Left = 352
    Top = 208
  end
end
