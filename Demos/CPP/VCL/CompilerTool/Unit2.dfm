object Form_PathSetup: TForm_PathSetup
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #36335#24452#35774#32622
  ClientHeight = 216
  ClientWidth = 383
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label14: TLabel
    Left = 3
    Top = 142
    Width = 258
    Height = 12
    Caption = #21487#29992#39044#23450#20041#65306#29256#26412#65306'%Ver%    '#24179#21488#65306'%Platform%'
    Font.Charset = GB2312_CHARSET
    Font.Color = clRed
    Font.Height = -12
    Font.Name = #23435#20307
    Font.Style = []
    ParentFont = False
  end
  object LB: TListBox
    Left = 3
    Top = 3
    Width = 351
    Height = 136
    ItemHeight = 13
    TabOrder = 0
  end
  object Button3: TButton
    Left = 360
    Top = 22
    Width = 18
    Height = 25
    Caption = #8595
    TabOrder = 1
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 360
    Top = 95
    Width = 18
    Height = 25
    Caption = #8593
    TabOrder = 2
    OnClick = Button4Click
  end
  object E1: TEdit
    Left = 3
    Top = 158
    Width = 351
    Height = 21
    TabOrder = 3
  end
  object Button1: TButton
    Left = 356
    Top = 158
    Width = 22
    Height = 22
    Caption = '...'
    TabOrder = 4
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 25
    Top = 186
    Width = 60
    Height = 25
    Caption = #28155#21152
    TabOrder = 5
    OnClick = Button2Click
  end
  object Button5: TButton
    Left = 115
    Top = 186
    Width = 60
    Height = 25
    Caption = #21024#38500
    TabOrder = 6
    OnClick = Button5Click
  end
  object BtnSave: TButton
    Left = 206
    Top = 186
    Width = 60
    Height = 25
    Caption = #20445#23384
    TabOrder = 7
    OnClick = BtnSaveClick
  end
  object Button7: TButton
    Left = 297
    Top = 186
    Width = 60
    Height = 25
    Caption = #21462#28040
    TabOrder = 8
    OnClick = Button7Click
  end
end
