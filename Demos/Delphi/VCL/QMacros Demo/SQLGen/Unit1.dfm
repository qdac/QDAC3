object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'SQLGen'
  ClientHeight = 366
  ClientWidth = 707
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
  object DBGrid1: TDBGrid
    Left = 0
    Top = 29
    Width = 320
    Height = 337
    Align = alLeft
    DataSource = DataSource1
    ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object Memo1: TMemo
    Left = 320
    Top = 29
    Width = 387
    Height = 337
    Align = alClient
    ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 707
    Height = 29
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object Button1: TButton
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 113
      Height = 23
      Align = alLeft
      Caption = #29983#25104'(Generate)'
      TabOrder = 0
      OnClick = Button1Click
    end
    object edtExp: TEdit
      AlignWithMargins = True
      Left = 122
      Top = 3
      Width = 582
      Height = 23
      Align = alClient
      ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
      TabOrder = 1
      Text = 
        '[Repeat:"insert into mytable(Id,Name,Age,Sex,Scale,Comment) valu' +
        'es ([Id.Quoted],[Name.Quoted],[Age],[Sex],[Scale],[Comment.Quote' +
        'd]);"]'
      ExplicitHeight = 21
    end
  end
  object DataSource1: TDataSource
    Left = 136
    Top = 112
  end
end
