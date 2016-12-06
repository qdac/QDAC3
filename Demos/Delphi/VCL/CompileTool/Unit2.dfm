object Form_PathSetup: TForm_PathSetup
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Path Setup'
  ClientHeight = 217
  ClientWidth = 385
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 14
  object Label14: TLabel
    Left = 3
    Top = 142
    Width = 336
    Height = 12
    Caption = 'Pre-defined: %BDS% %Config% %Ver% %Platform%   Click me!'
    Font.Charset = GB2312_CHARSET
    Font.Color = clRed
    Font.Height = -12
    Font.Name = #23435#20307
    Font.Style = []
    ParentFont = False
    OnClick = Label14Click
  end
  object LB: TListBox
    Left = 3
    Top = 3
    Width = 351
    Height = 136
    ItemHeight = 14
    TabOrder = 0
    OnDblClick = LBDblClick
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
    Top = 159
    Width = 351
    Height = 22
    TabOrder = 3
  end
  object Button1: TButton
    Left = 356
    Top = 159
    Width = 22
    Height = 22
    Caption = '...'
    TabOrder = 4
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 25
    Top = 187
    Width = 60
    Height = 25
    Caption = 'Add'
    TabOrder = 5
    OnClick = Button2Click
  end
  object Button5: TButton
    Left = 115
    Top = 187
    Width = 60
    Height = 25
    Caption = 'Delete'
    TabOrder = 6
    OnClick = Button5Click
  end
  object BtnSave: TButton
    Left = 206
    Top = 187
    Width = 60
    Height = 25
    Caption = 'Save'
    TabOrder = 7
    OnClick = BtnSaveClick
  end
  object Button7: TButton
    Left = 297
    Top = 187
    Width = 60
    Height = 25
    Caption = 'Cancel'
    TabOrder = 8
    OnClick = Button7Click
  end
end
