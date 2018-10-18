object frmNodeValueEditor: TfrmNodeValueEditor
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #20462#25913#32467#28857
  ClientHeight = 310
  ClientWidth = 572
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
  object Label2: TLabel
    Left = 8
    Top = 135
    Width = 36
    Height = 13
    Caption = #21462#20540#65306
  end
  object lblName: TLabel
    Left = 8
    Top = 12
    Width = 28
    Height = 13
    Caption = #21517#31216':'
  end
  object Label3: TLabel
    Left = 184
    Top = 12
    Width = 76
    Height = 13
    Caption = #25193#23637#31867#22411#32534#30721':'
  end
  object rgDataType: TRadioGroup
    Left = 8
    Top = 40
    Width = 409
    Height = 89
    Caption = #25968#25454#31867#22411
    Columns = 4
    ItemIndex = 1
    Items.Strings = (
      #25972#25968
      #31354#20540
      #24067#23572
      #21333#31934#24230#28014#28857#25968
      #21452#31934#24230#28014#28857#25968
      #23383#31526#20018#31867#22411
      #20108#36827#21046#25968#25454
      #25968#32452
      #26144#23556#65288#23545#35937#65289
      #25193#23637
      #26085#26399#26102#38388)
    TabOrder = 0
    OnClick = rgDataTypeClick
  end
  object btnLoadFromFile: TButton
    Left = 467
    Top = 129
    Width = 97
    Height = 25
    Caption = #20174#25991#20214#20013#21152#36733'...'
    Enabled = False
    TabOrder = 1
    OnClick = btnLoadFromFileClick
  end
  object Button2: TButton
    Left = 480
    Top = 11
    Width = 75
    Height = 25
    Caption = #30830#23450'(&O)'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 480
    Top = 42
    Width = 75
    Height = 25
    Caption = #21462#28040'(&C)'
    ModalResult = 2
    TabOrder = 3
  end
  object edtValue: TMemo
    Left = 8
    Top = 160
    Width = 556
    Height = 142
    TabOrder = 4
  end
  object edtName: TEdit
    Left = 42
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 5
  end
  object seExtType: TSpinEdit
    Left = 266
    Top = 7
    Width = 47
    Height = 22
    MaxValue = 127
    MinValue = 0
    TabOrder = 6
    Value = 0
  end
  object odFromFile: TOpenDialog
    Left = 272
    Top = 96
  end
end
