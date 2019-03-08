object Form1: TForm1
  Left = 0
  Top = 0
  Caption = #25968#23398#34920#36798#24335#35745#31639#31034#20363
  ClientHeight = 375
  ClientWidth = 644
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 644
    Height = 73
    Align = alTop
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 0
    ExplicitWidth = 609
    object Edit1: TEdit
      Left = 24
      Top = 16
      Width = 362
      Height = 21
      TabOrder = 0
      Text = '2+(8+4*2)*3'
    end
    object Calc: TButton
      Left = 392
      Top = 14
      Width = 75
      Height = 25
      Caption = #35745#31639
      TabOrder = 1
      OnClick = CalcClick
    end
    object Button1: TButton
      Left = 473
      Top = 14
      Width = 75
      Height = 25
      Caption = #31034#20363
      TabOrder = 2
      OnClick = Button1Click
    end
    object chkStdDiv: TCheckBox
      Left = 24
      Top = 48
      Width = 345
      Height = 17
      Caption = #20351#29992#24120#35268#38500#27861#31639#27861#65288#22914'1/2=0.5'#65292#32780#19981#26159'1/2=0)'
      TabOrder = 3
    end
    object chkNumIdentAsMultiply: TCheckBox
      Left = 336
      Top = 50
      Width = 257
      Height = 17
      Caption = #25968#20540'+'#26631#24535#31526'('#22914':2x)'#35299#26512#20026#25968#20540'*'#26631#24535#31526'(2*x)'
      TabOrder = 4
    end
    object Button2: TButton
      Left = 554
      Top = 14
      Width = 75
      Height = 25
      Caption = 'Json '#31034#20363
      TabOrder = 5
      OnClick = Button2Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 73
    Width = 644
    Height = 302
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 609
    object Memo1: TMemo
      AlignWithMargins = True
      Left = 115
      Top = 3
      Width = 526
      Height = 255
      Align = alClient
      TabOrder = 0
      ExplicitWidth = 491
    end
    object ListBox1: TListBox
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 106
      Height = 255
      Align = alLeft
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ItemHeight = 13
      ParentFont = False
      TabOrder = 1
    end
    object Panel3: TPanel
      Left = 0
      Top = 261
      Width = 644
      Height = 41
      Align = alBottom
      BevelOuter = bvLowered
      Caption = #24038#19978#20989#25968#25110#21464#37327#21015#34920#20013#65292'X '#20026#27979#35797#29992#24120#37327#65292#20540#20026'100'#65292'Y'#20026#29992#25143#36755#20837#20540#65292'ST'#20026#22266#23450#38543#26426#20540#65292#20854#23427#20026#20869#32622#20989#25968#25110#21464#37327
      TabOrder = 2
      ExplicitWidth = 609
    end
  end
end
