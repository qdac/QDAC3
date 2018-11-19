object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'QLog Strings Writer Demo'
  ClientHeight = 638
  ClientWidth = 799
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
    Left = 32
    Top = 61
    Width = 47
    Height = 13
    Caption = 'MaxItems'
  end
  object Label2: TLabel
    Left = 400
    Top = 61
    Width = 50
    Height = 13
    Caption = 'Max Items'
  end
  object Label3: TLabel
    Left = 40
    Top = 592
    Width = 68
    Height = 13
    Caption = 'Log post time:'
  end
  object Label4: TLabel
    Left = 32
    Top = 112
    Width = 33
    Height = 13
    Caption = 'All logs'
  end
  object Label5: TLabel
    Left = 400
    Top = 109
    Width = 46
    Height = 13
    Caption = 'Error logs'
  end
  object Memo1: TMemo
    Left = 32
    Top = 128
    Width = 345
    Height = 449
    Lines.Strings = (
      'Log will fill into this memo')
    TabOrder = 0
  end
  object Button1: TButton
    Left = 32
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 1
    OnClick = Button1Click
  end
  object ListBox1: TListBox
    Left = 400
    Top = 128
    Width = 337
    Height = 449
    ItemHeight = 13
    TabOrder = 2
  end
  object SpinEdit1: TSpinEdit
    Left = 32
    Top = 80
    Width = 121
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 3
    Value = 20
    OnChange = SpinEdit1Change
  end
  object SpinEdit2: TSpinEdit
    Left = 400
    Top = 80
    Width = 121
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 4
    Value = 10
    OnChange = SpinEdit2Change
  end
  object CheckBox1: TCheckBox
    Left = 128
    Top = 20
    Width = 97
    Height = 17
    Caption = 'Lazy Mode'
    Checked = True
    State = cbChecked
    TabOrder = 5
    OnClick = CheckBox1Click
  end
end
