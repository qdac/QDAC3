object Form3: TForm3
  Left = 0
  Top = 0
  Caption = #25490#24207#28436#31034#31243#24207
  ClientHeight = 242
  ClientWidth = 447
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
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 439
    Height = 13
    Caption = #36825#20010#31034#20363#28436#31034#20102#29992'QWorker'#26469#36827#34892#22810#32447#31243#25490#24207#30340#25928#26524#65292#24182#25552#20379#21644#20027#32447#31243#25490#24207#30340#23545#27604#12290
  end
  object Label2: TLabel
    Left = 8
    Top = 40
    Width = 72
    Height = 13
    Caption = #24037#20316#32773#25968#37327#65306
  end
  object Label3: TLabel
    Left = 140
    Top = 40
    Width = 48
    Height = 13
    Caption = #25968#25454#37327#65306
  end
  object Button1: TButton
    Left = 16
    Top = 64
    Width = 100
    Height = 25
    Caption = #22810#32447#31243#25490#24207'('#31561#24453')'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 231
    Top = 64
    Width = 100
    Height = 25
    Caption = #20027#32447#31243#25490#24207
    TabOrder = 1
    OnClick = Button2Click
  end
  object Memo1: TMemo
    Left = 16
    Top = 97
    Width = 423
    Height = 137
    ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
    TabOrder = 2
  end
  object Button3: TButton
    Left = 339
    Top = 64
    Width = 100
    Height = 25
    Caption = #23545#27604#27979#35797
    TabOrder = 3
    OnClick = Button3Click
  end
  object seWorkers: TSpinEdit
    Left = 86
    Top = 36
    Width = 46
    Height = 22
    MaxValue = 128
    MinValue = 1
    TabOrder = 4
    Value = 1
  end
  object seNum: TSpinEdit
    Left = 194
    Top = 36
    Width = 111
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 5
    Value = 1
  end
  object Button4: TButton
    Left = 123
    Top = 64
    Width = 100
    Height = 25
    Caption = #22810#32447#31243#25490#24207'('#24322#27493')'
    TabOrder = 6
    OnClick = Button4Click
  end
end
