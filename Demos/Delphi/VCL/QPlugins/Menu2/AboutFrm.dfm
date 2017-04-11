object frmAbout: TfrmAbout
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  ClientHeight = 474
  ClientWidth = 713
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object bvl1: TBevel
    Left = 0
    Top = 0
    Width = 713
    Height = 2
    Align = alTop
    Shape = bsTopLine
    ExplicitLeft = -13
    ExplicitTop = 298
    ExplicitWidth = 482
  end
  object lblHomePage: TLabel
    Left = 630
    Top = 393
    Width = 68
    Height = 13
    Cursor = crHandPoint
    Caption = 'QQ:25458532'
    Color = 16579061
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsUnderline]
    ParentColor = False
    ParentFont = False
    Transparent = False
    OnClick = lblHomePageClick
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 428
    Width = 713
    Height = 46
    Align = alBottom
    BevelOuter = bvNone
    Color = 16185078
    ParentBackground = False
    TabOrder = 0
    DesignSize = (
      713
      46)
    object lblVersion: TLabel
      Left = 8
      Top = 20
      Width = 52
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = #24403#21069#29256#26412':'
      ExplicitTop = 16
    end
    object btn1: TButton
      Left = 630
      Top = 13
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = #30830#23450
      TabOrder = 0
      OnClick = btn1Click
    end
  end
end
