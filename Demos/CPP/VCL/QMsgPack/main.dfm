object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 202
  ClientWidth = 788
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 104
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object mmResult: TMemo
    Left = 24
    Top = 72
    Width = 425
    Height = 121
    Lines.Strings = (
      'mmResult')
    TabOrder = 1
  end
  object Button2: TButton
    Left = 200
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 2
  end
  object DBGrid1: TDBGrid
    Left = 448
    Top = 74
    Width = 320
    Height = 120
    DataSource = DataSource1
    TabOrder = 3
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object ClientDataSet1: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 360
    Top = 104
    object ClientDataSet1Id: TIntegerField
      FieldName = 'Id'
    end
    object ClientDataSet1Name: TStringField
      FieldName = 'Name'
    end
  end
  object DataSource1: TDataSource
    DataSet = ClientDataSet1
    Left = 216
    Top = 104
  end
end
