object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Dialog Builder '#31034#20363
  ClientHeight = 375
  ClientWidth = 574
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 354
    Width = 3
    Height = 13
  end
  object Button1: TButton
    Left = 16
    Top = 8
    Width = 120
    Height = 32
    Caption = 'DropDown 1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 16
    Top = 60
    Width = 120
    Height = 32
    Caption = 'ShowModal'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 298
    Top = 8
    Width = 120
    Height = 32
    Caption = 'Radio Groups1'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 157
    Top = 8
    Width = 120
    Height = 32
    Caption = 'DropDown 2'
    TabOrder = 3
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 439
    Top = 8
    Width = 120
    Height = 32
    Caption = 'Radio Groups2'
    TabOrder = 4
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 157
    Top = 60
    Width = 120
    Height = 32
    Caption = 'PopupAt'
    TabOrder = 5
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 16
    Top = 112
    Width = 120
    Height = 32
    Caption = 'Warning Dialog'
    TabOrder = 6
    OnClick = Button7Click
  end
  object Button8: TButton
    Left = 298
    Top = 60
    Width = 120
    Height = 32
    Caption = 'CustomDialog 1'
    TabOrder = 7
    OnClick = Button8Click
  end
  object Button9: TButton
    Left = 439
    Top = 60
    Width = 120
    Height = 32
    Caption = 'CustomDialog 2'
    TabOrder = 8
    OnClick = Button9Click
  end
  object Button10: TButton
    Left = 157
    Top = 112
    Width = 120
    Height = 32
    Caption = 'Close Delay'
    TabOrder = 9
    OnClick = Button10Click
  end
  object Button11: TButton
    Left = 298
    Top = 112
    Width = 120
    Height = 32
    Caption = 'Progress '
    TabOrder = 10
    OnClick = Button11Click
  end
end