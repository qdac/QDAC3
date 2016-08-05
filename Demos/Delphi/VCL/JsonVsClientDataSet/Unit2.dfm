object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 202
  ClientWidth = 447
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 336
    Top = 0
    Width = 111
    Height = 202
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    object Button1: TButton
      Left = 16
      Top = 16
      Width = 89
      Height = 25
      Caption = 'CreateDataSet'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 16
      Top = 47
      Width = 89
      Height = 25
      Caption = 'SaveToJson'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 16
      Top = 79
      Width = 89
      Height = 25
      Caption = 'LoadFromJson'
      TabOrder = 2
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 16
      Top = 112
      Width = 89
      Height = 25
      Caption = 'DataSetToJson'
      TabOrder = 3
    end
    object Button5: TButton
      Left = 16
      Top = 144
      Width = 89
      Height = 25
      Caption = 'JsonToDataSet'
      TabOrder = 4
    end
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 0
    Width = 336
    Height = 202
    Align = alClient
    DataSource = DataSource1
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object ClientDataSet1: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 168
    Top = 96
  end
  object DataSource1: TDataSource
    DataSet = ClientDataSet1
    Left = 112
    Top = 96
  end
  object SaveDialog1: TSaveDialog
    Left = 216
    Top = 104
  end
  object OpenDialog1: TOpenDialog
    Left = 224
    Top = 112
  end
end
