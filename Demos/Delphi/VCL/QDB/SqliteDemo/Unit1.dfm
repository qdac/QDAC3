object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 292
  ClientWidth = 477
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
  object Image1: TImage
    Left = 320
    Top = 175
    Width = 105
    Height = 105
  end
  object DBGrid1: TDBGrid
    Left = 8
    Top = 16
    Width = 385
    Height = 153
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object Button1: TButton
    Left = 56
    Top = 175
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 120
    Top = 232
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 40
    Top = 240
    Width = 75
    Height = 25
    Caption = 'Button3'
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 224
    Top = 208
    Width = 75
    Height = 25
    Caption = 'Button4'
    TabOrder = 4
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 399
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Button5'
    TabOrder = 5
    OnClick = Button5Click
  end
  object DataSource1: TDataSource
    Left = 232
    Top = 152
  end
  object FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink
    Left = 232
    Top = 48
  end
  object FDSQLiteFunction1: TFDSQLiteFunction
    Left = 336
    Top = 40
  end
end
