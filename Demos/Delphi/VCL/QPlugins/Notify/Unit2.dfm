object Form2: TForm2
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  ClientHeight = 174
  ClientWidth = 258
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Gauge1: TGauge
    Left = 22
    Top = 8
    Width = 150
    Height = 150
    Kind = gkPie
    Progress = 50
  end
  object CheckBox1: TCheckBox
    Left = 184
    Top = 16
    Width = 97
    Height = 17
    Caption = #25509#25910#28040#24687
    Checked = True
    State = cbChecked
    TabOrder = 0
    OnClick = CheckBox1Click
  end
end
