object frmUrlInput: TfrmUrlInput
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #35775#38382#32593#22336
  ClientHeight = 109
  ClientWidth = 457
  Color = clBtnFace
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
    Left = 16
    Top = 8
    Width = 47
    Height = 13
    Caption = 'URL'#22320#22336':'
  end
  object edtUrl: TEdit
    Left = 16
    Top = 27
    Width = 423
    Height = 21
    TabOrder = 0
  end
  object Button1: TButton
    Left = 283
    Top = 64
    Width = 75
    Height = 25
    Caption = #30830#23450'(&O)'
    ModalResult = 1
    TabOrder = 1
  end
  object Button2: TButton
    Left = 364
    Top = 64
    Width = 75
    Height = 25
    Caption = #21462#28040'(&C)'
    ModalResult = 2
    TabOrder = 2
  end
end
