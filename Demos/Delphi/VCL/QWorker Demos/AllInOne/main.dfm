object Form1: TForm1
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'QWorker Demo'
  ClientHeight = 310
  ClientWidth = 558
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 282
    Width = 3
    Height = 13
  end
  object Label2: TLabel
    Left = 16
    Top = 263
    Width = 3
    Height = 13
  end
  object Label3: TLabel
    Left = 281
    Top = 263
    Width = 3
    Height = 13
  end
  object Label4: TLabel
    Left = 281
    Top = 282
    Width = 3
    Height = 13
  end
  object lblPlanStatic: TLabel
    Left = 182
    Top = 256
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
    OnClick = Button1Click
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
    OnClick = Button4Click
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
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 226
    Top = 39
    Width = 100
    Height = 25
    Caption = #38271#26102#38388#20219#21153
    TabOrder = 6
    OnClick = Button7Click
  end
  object Button8: TButton
    Left = 8
    Top = 70
    Width = 100
    Height = 25
    Caption = #23450#26102#25191#34892'1'
    TabOrder = 7
    OnClick = Button8Click
  end
  object Button9: TButton
    Left = 335
    Top = 39
    Width = 100
    Height = 25
    Caption = #24310#36831#25191#34892#20316#19994
    TabOrder = 8
    OnClick = Button9Click
  end
  object Button10: TButton
    Left = 117
    Top = 70
    Width = 100
    Height = 25
    Caption = #23450#26102#25191#34892'2'
    TabOrder = 9
    OnClick = Button10Click
  end
  object Button11: TButton
    Left = 226
    Top = 70
    Width = 100
    Height = 25
    Caption = #21462#28040#20316#19994
    TabOrder = 10
    OnClick = Button11Click
  end
  object Button12: TButton
    Left = 335
    Top = 70
    Width = 100
    Height = 25
    Caption = #33258#21160#37322#25918#38468#21152#25968#25454
    TabOrder = 11
    OnClick = Button12Click
  end
  object Button13: TButton
    Left = 117
    Top = 101
    Width = 100
    Height = 25
    Caption = 'COM'#20316#19994
    TabOrder = 12
    OnClick = Button13Click
  end
  object Button14: TButton
    Left = 117
    Top = 132
    Width = 100
    Height = 25
    Caption = #21462#28040#20449#21495#20316#19994
    TabOrder = 13
    OnClick = Button14Click
  end
  object Button15: TButton
    Left = 226
    Top = 101
    Width = 100
    Height = 25
    Caption = #38543#26426#24310#36831
    TabOrder = 14
    OnClick = Button15Click
  end
  object Button16: TButton
    Left = 335
    Top = 101
    Width = 100
    Height = 25
    Caption = #20840#23616#36807#31243
    TabOrder = 15
    OnClick = Button16Click
  end
  object Button17: TButton
    Left = 8
    Top = 39
    Width = 100
    Height = 25
    Caption = #33258#32467#26463#23450#26102#20316#19994
    TabOrder = 16
    OnClick = Button17Click
  end
  object Button18: TButton
    Left = 117
    Top = 39
    Width = 100
    Height = 25
    Caption = #33258#32467#26463#20449#21495#20316#19994
    TabOrder = 17
    OnClick = Button18Click
  end
  object Button19: TButton
    Left = 445
    Top = 132
    Width = 100
    Height = 25
    Caption = #20998#32452#20316#19994
    TabOrder = 18
    OnClick = Button19Click
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
    OnClick = Button21Click
  end
  object Button22: TButton
    Left = 335
    Top = 163
    Width = 100
    Height = 25
    Caption = #25163#21160#24207#21015#21270#20316#19994
    TabOrder = 21
    OnClick = Button22Click
  end
  object Button23: TButton
    Left = 445
    Top = 163
    Width = 100
    Height = 25
    Caption = #33258#21160#24207#21015#21270#20316#19994
    TabOrder = 22
    OnClick = Button23Click
  end
  object Button24: TButton
    Left = 8
    Top = 101
    Width = 100
    Height = 25
    Caption = #20449#21495#22810#25773
    TabOrder = 23
    OnClick = Button24Click
  end
  object Button25: TButton
    Left = 445
    Top = 8
    Width = 100
    Height = 25
    Caption = #20998#32452#20316#19994#36229#26102
    TabOrder = 24
    OnClick = Button25Click
  end
  object Button26: TButton
    Left = 445
    Top = 39
    Width = 100
    Height = 25
    Caption = #28165#38500#25191#34892#20013#20316#19994
    TabOrder = 25
    OnClick = Button26Click
  end
  object Button27: TButton
    Left = 445
    Top = 70
    Width = 100
    Height = 25
    Caption = #20351#29992#32447#31243#21516#27493#26041#27861
    TabOrder = 26
    OnClick = Button27Click
  end
  object Button28: TButton
    Left = 445
    Top = 101
    Width = 100
    Height = 25
    Caption = #20027#32447#31243#20998#32452#31561#24453
    TabOrder = 27
    OnClick = Button28Click
  end
  object Button29: TButton
    Left = 117
    Top = 163
    Width = 100
    Height = 25
    Caption = #21462#28040#23450#26102#20316#19994
    TabOrder = 28
    OnClick = Button29Click
  end
  object Button30: TButton
    Left = 226
    Top = 163
    Width = 97
    Height = 25
    Caption = #22797#26434#31867#22411#37322#25918
    TabOrder = 29
    OnClick = Button30Click
  end
  object Button31: TButton
    Left = 8
    Top = 194
    Width = 100
    Height = 25
    Caption = 'For'#24490#29615
    TabOrder = 30
    OnClick = Button31Click
  end
  object Button32: TButton
    Left = 117
    Top = 194
    Width = 100
    Height = 25
    Caption = #26522#20030#29366#24577
    TabOrder = 31
    OnClick = Button32Click
  end
  object Button33: TButton
    Left = 226
    Top = 194
    Width = 97
    Height = 25
    Caption = #26522#20030#31561#24453#38142#34920
    TabOrder = 32
    OnClick = Button33Click
  end
  object Button34: TButton
    Left = 335
    Top = 194
    Width = 100
    Height = 25
    Caption = #37325#22797#25237#23492
    TabOrder = 33
    OnClick = Button34Click
  end
  object Button35: TButton
    Left = 445
    Top = 194
    Width = 100
    Height = 25
    Caption = #33258#21160#37322#25918#22797#26434#35760#24405
    TabOrder = 34
    OnClick = Button35Click
  end
  object Button36: TButton
    Left = 8
    Top = 225
    Width = 100
    Height = 25
    Caption = #26522#20030#20316#19994#29366#24577
    TabOrder = 35
    OnClick = Button36Click
  end
  object Button37: TButton
    Left = 117
    Top = 225
    Width = 100
    Height = 25
    Caption = #33719#21462#20316#19994#29366#24577
    TabOrder = 36
    OnClick = Button37Click
  end
  object Button38: TButton
    Left = 226
    Top = 225
    Width = 97
    Height = 25
    Caption = 'RunInMainThread'
    TabOrder = 37
    OnClick = Button38Click
  end
  object Button39: TButton
    Left = 335
    Top = 225
    Width = 99
    Height = 25
    Caption = 'Linux Cron'#35745#21010
    TabOrder = 38
    OnClick = Button39Click
  end
  object btnHandleTest: TButton
    Left = 445
    Top = 225
    Width = 100
    Height = 25
    Caption = #21477#26564#27979#35797
    TabOrder = 39
    OnClick = btnHandleTestClick
  end
  object Button40: TButton
    Left = 445
    Top = 256
    Width = 100
    Height = 25
    Caption = 'Button40'
    TabOrder = 40
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 334
    Top = 254
  end
end
