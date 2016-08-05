object Form1: TForm1
  Left = 0
  Top = 0
  Caption = #32479#35745#20989#25968#31034#20363'(Static functions demo)'
  ClientHeight = 414
  ClientWidth = 741
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
    Width = 741
    Height = 65
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 707
    object Button1: TButton
      Left = 8
      Top = 9
      Width = 100
      Height = 25
      Caption = #27714#21512'(Sum)'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 110
      Top = 9
      Width = 100
      Height = 25
      Caption = #24179#22343#20540'(Avg)'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 212
      Top = 9
      Width = 100
      Height = 25
      Caption = #35745#25968'(DistinctCount)'
      TabOrder = 2
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 519
      Top = 9
      Width = 100
      Height = 25
      Caption = #20998#32452'(Group By)'
      TabOrder = 3
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 314
      Top = 9
      Width = 100
      Height = 25
      Caption = #26368#22823#20540'(Max)'
      TabOrder = 4
      OnClick = Button5Click
    end
    object Button6: TButton
      Left = 416
      Top = 9
      Width = 100
      Height = 25
      Caption = #26368#23567#20540'(Min)'
      TabOrder = 5
      OnClick = Button6Click
    end
    object Button7: TButton
      Left = 625
      Top = 9
      Width = 100
      Height = 25
      Caption = #32858#21512#23383#27573'(AggFlds)'
      TabOrder = 6
      OnClick = Button7Click
    end
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 65
    Width = 741
    Height = 183
    Align = alClient
    DataSource = DataSource1
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object DBGrid2: TDBGrid
    Left = 0
    Top = 248
    Width = 741
    Height = 166
    Align = alBottom
    DataSource = DataSource2
    TabOrder = 2
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object DataSource1: TDataSource
    Left = 344
    Top = 216
  end
  object DataSource2: TDataSource
    Left = 536
    Top = 72
  end
end
