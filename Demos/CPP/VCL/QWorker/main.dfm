object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'QWorker Demo for C++ Builder'
  ClientHeight = 355
  ClientWidth = 572
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 219
    Width = 3
    Height = 13
  end
  object Label2: TLabel
    Left = 8
    Top = 200
    Width = 3
    Height = 13
  end
  object Label3: TLabel
    Left = 273
    Top = 200
    Width = 3
    Height = 13
  end
  object Label4: TLabel
    Left = 273
    Top = 219
    Width = 3
    Height = 13
  end
  object Button1: TButton
    Left = 8
    Top = 132
    Width = 100
    Height = 25
    Caption = #25353'ID'#35302#21457#20449#21495
    TabOrder = 0
  end
  object Button2: TButton
    Left = 8
    Top = 8
    Width = 100
    Height = 25
    Caption = #25237#23492#32447#31243#20316#19994
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button4: TButton
    Left = 226
    Top = 8
    Width = 100
    Height = 25
    Caption = #36895#24230#27979#35797
    TabOrder = 2
  end
  object Button3: TButton
    Left = 335
    Top = 8
    Width = 100
    Height = 25
    Caption = #25139#19968#19979#26102#38388
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button5: TButton
    Left = 117
    Top = 8
    Width = 100
    Height = 25
    Caption = #20027#32447#31243#20316#19994
    TabOrder = 4
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 8
    Top = 163
    Width = 100
    Height = 25
    Caption = #25353#21517#31216#35302#21457#20449#21495
    TabOrder = 5
  end
  object Button7: TButton
    Left = 226
    Top = 39
    Width = 100
    Height = 25
    Caption = #38271#26102#38388#20219#21153
    TabOrder = 6
  end
  object Button8: TButton
    Left = 8
    Top = 70
    Width = 100
    Height = 25
    Caption = #23450#26102#25191#34892'1'
    TabOrder = 7
  end
  object Button9: TButton
    Left = 335
    Top = 39
    Width = 100
    Height = 25
    Caption = #24310#36831#25191#34892#20316#19994
    TabOrder = 8
  end
  object Button10: TButton
    Left = 117
    Top = 70
    Width = 100
    Height = 25
    Caption = #23450#26102#25191#34892'2'
    TabOrder = 9
  end
  object Button11: TButton
    Left = 226
    Top = 70
    Width = 100
    Height = 25
    Caption = #21462#28040#20316#19994
    TabOrder = 10
  end
  object Button12: TButton
    Left = 335
    Top = 70
    Width = 100
    Height = 25
    Caption = #33258#21160#37322#25918#38468#21152#25968#25454
    TabOrder = 11
  end
  object Button13: TButton
    Left = 117
    Top = 101
    Width = 100
    Height = 25
    Caption = 'COM'#20316#19994
    TabOrder = 12
  end
  object Button14: TButton
    Left = 117
    Top = 132
    Width = 100
    Height = 25
    Caption = #21462#28040#20449#21495#20316#19994
    TabOrder = 13
  end
  object Button15: TButton
    Left = 226
    Top = 101
    Width = 100
    Height = 25
    Caption = #38543#26426#24310#36831
    TabOrder = 14
  end
  object Button16: TButton
    Left = 335
    Top = 101
    Width = 100
    Height = 25
    Caption = #20840#23616#36807#31243
    TabOrder = 15
  end
  object Button17: TButton
    Left = 8
    Top = 39
    Width = 100
    Height = 25
    Caption = #33258#32467#26463#23450#26102#20316#19994
    TabOrder = 16
  end
  object Button18: TButton
    Left = 117
    Top = 39
    Width = 100
    Height = 25
    Caption = #33258#32467#26463#20449#21495#20316#19994
    TabOrder = 17
  end
  object Button19: TButton
    Left = 445
    Top = 132
    Width = 100
    Height = 25
    Caption = #20998#32452#20316#19994
    TabOrder = 18
  end
  object Button20: TButton
    Left = 226
    Top = 132
    Width = 100
    Height = 25
    Caption = #21311#21517#20316#19994#36807#31243
    TabOrder = 19
    OnClick = Button20Click
  end
  object Button21: TButton
    Left = 335
    Top = 132
    Width = 100
    Height = 25
    Caption = #38468#21152#21442#25968
    TabOrder = 20
  end
  object Button22: TButton
    Left = 335
    Top = 163
    Width = 100
    Height = 25
    Caption = #25163#21160#24207#21015#21270#20316#19994
    TabOrder = 21
  end
  object Button23: TButton
    Left = 445
    Top = 163
    Width = 100
    Height = 25
    Caption = #33258#21160#24207#21015#21270#20316#19994
    TabOrder = 22
  end
  object Button24: TButton
    Left = 8
    Top = 101
    Width = 100
    Height = 25
    Caption = #20449#21495#22810#25773
    TabOrder = 23
  end
  object Button25: TButton
    Left = 445
    Top = 8
    Width = 100
    Height = 25
    Caption = #20998#32452#20316#19994#36229#26102
    TabOrder = 24
  end
  object Button26: TButton
    Left = 445
    Top = 39
    Width = 100
    Height = 25
    Caption = #28165#38500#25191#34892#20013#20316#19994
    TabOrder = 25
  end
  object Button27: TButton
    Left = 445
    Top = 70
    Width = 100
    Height = 25
    Caption = #20351#29992#32447#31243#21516#27493#26041#27861
    TabOrder = 26
  end
  object Button28: TButton
    Left = 445
    Top = 101
    Width = 100
    Height = 25
    Caption = #20027#32447#31243#20998#32452#31561#24453
    TabOrder = 27
  end
  object Timer1: TTimer
    Left = 294
    Top = 166
  end
end
