object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 324
  ClientWidth = 525
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
  object Button1: TButton
    Left = 424
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 39
    Width = 525
    Height = 285
    Align = alBottom
    DataSource = DataSource1
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object Button2: TButton
    Left = 343
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 2
    OnClick = Button2Click
  end
  object ADODataSet1: TADODataSet
    Active = True
    ConnectionString = 
      'Provider=SQLOLEDB.1;Password=Goldenbay;Persist Security Info=Tru' +
      'e;User ID=sa;Initial Catalog=nah;Data Source=192.168.0.19'
    CursorType = ctStatic
    CommandText = 'select * from sufferer'
    Parameters = <>
    Left = 432
    Top = 32
  end
  object ClientDataSet1: TClientDataSet
    Aggregates = <>
    Params = <>
    ProviderName = 'DataSetProvider1'
    Left = 208
    Top = 32
  end
  object DataSetProvider1: TDataSetProvider
    DataSet = ADODataSet1
    Left = 288
    Top = 32
  end
  object DataSource1: TDataSource
    DataSet = ClientDataSet1
    Left = 136
    Top = 32
  end
end
