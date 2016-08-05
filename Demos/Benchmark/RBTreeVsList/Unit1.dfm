object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'RBTree '#12289'TList<T>'#12289'TPagedList '#24615#33021#27604#36739
  ClientHeight = 349
  ClientWidth = 631
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
  object mmResult: TMemo
    Left = 0
    Top = 41
    Width = 631
    Height = 308
    Align = alClient
    ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 631
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 200
      Top = 15
      Width = 52
      Height = 13
      Caption = #20998#39029#22823#23567':'
    end
    object Button3: TButton
      Left = 16
      Top = 9
      Width = 75
      Height = 25
      Caption = #24320#22987'(&S)'
      TabOrder = 0
      OnClick = Button3Click
    end
    object Button1: TButton
      Left = 112
      Top = 9
      Width = 75
      Height = 25
      Caption = #39564#35777#27979#35797
      TabOrder = 1
      OnClick = Button1Click
    end
    object cbxPageSize: TComboBox
      Left = 258
      Top = 11
      Width = 87
      Height = 21
      Style = csDropDownList
      ItemIndex = 2
      TabOrder = 2
      Text = '512'
      Items.Strings = (
        '128'
        '256'
        '512'
        '1024'
        '2048'
        '4096'
        '8192')
    end
    object Button2: TButton
      Left = 360
      Top = 9
      Width = 75
      Height = 25
      Caption = #25910#32553#27979#35797
      TabOrder = 3
      OnClick = Button2Click
    end
    object Button4: TButton
      Left = 448
      Top = 8
      Width = 75
      Height = 25
      Caption = #25209#37327#27979#35797
      Default = True
      TabOrder = 4
      OnClick = Button4Click
    end
  end
end
