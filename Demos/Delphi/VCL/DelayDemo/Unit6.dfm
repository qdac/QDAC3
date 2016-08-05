object Form6: TForm6
  Left = 0
  Top = 0
  Caption = #24310#36831#28436#31034
  ClientHeight = 393
  ClientWidth = 660
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
  object Chart1: TChart
    Left = 0
    Top = 0
    Width = 540
    Height = 393
    Title.Text.Strings = (
      'TChart')
    Title.Visible = False
    BottomAxis.Grid.Visible = False
    LeftAxis.Grid.Visible = False
    View3D = False
    Align = alClient
    TabOrder = 0
    DefaultCanvas = 'TGDIPlusCanvas'
    ColorPaletteIndex = 13
    object slCos: TLineSeries
      Title = 'COS'
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object slSin: TLineSeries
      Title = 'SIN'
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
  end
  object Panel1: TPanel
    Left = 540
    Top = 0
    Width = 120
    Height = 393
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object Button1: TButton
      Left = 24
      Top = 16
      Width = 75
      Height = 25
      Caption = #24320#22987'(&S)'
      TabOrder = 0
      OnClick = Button1Click
    end
    object CheckBox1: TCheckBox
      Left = 16
      Top = 56
      Width = 97
      Height = 17
      Caption = #22266#23450#22352#26631
      TabOrder = 1
    end
  end
end
