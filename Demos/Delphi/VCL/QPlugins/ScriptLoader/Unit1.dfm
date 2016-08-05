object Form1: TForm1
  Left = 0
  Top = 0
  ClientHeight = 286
  ClientWidth = 788
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 113
    Height = 25
    Caption = #26597#25214#31649#29702#22120#23454#20363
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 136
    Top = 8
    Width = 113
    Height = 25
    Caption = 'IQParams'#21442#25968#36716#25442
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 39
    Width = 113
    Height = 25
    Caption = #21551#21160#25554#20214#24341#25806
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 136
    Top = 39
    Width = 113
    Height = 25
    Caption = #20572#27490#25554#20214#24341#25806
    TabOrder = 3
    OnClick = Button4Click
  end
  object mmLogs: TMemo
    Left = 0
    Top = 93
    Width = 788
    Height = 193
    Align = alBottom
    TabOrder = 4
  end
  object Button5: TButton
    Left = 264
    Top = 8
    Width = 113
    Height = 25
    Caption = #35843#29992#33050#26412#26381#21153
    TabOrder = 5
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 264
    Top = 39
    Width = 113
    Height = 25
    Caption = #25353#36335#24452#35843#29992'MyService'
    TabOrder = 6
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 392
    Top = 39
    Width = 113
    Height = 25
    Caption = #25353'ID'#35843#29992'MyService'
    TabOrder = 7
    OnClick = Button7Click
  end
  object MainMenu1: TMainMenu
    Left = 392
    Top = 152
    object Services1: TMenuItem
      Caption = 'Services'
    end
  end
end
