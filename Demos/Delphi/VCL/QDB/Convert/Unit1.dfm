object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'QDataSet '#23548#20837#23548#20986#28436#31034'(Import/Export demo)'
  ClientHeight = 456
  ClientWidth = 779
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
  object Label2: TLabel
    Left = 24
    Top = 60
    Width = 52
    Height = 13
    Caption = #23494#38053#31867#22411':'
  end
  object Panel1: TPanel
    Left = 560
    Top = 41
    Width = 219
    Height = 415
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    object GroupBox1: TGroupBox
      Left = 6
      Top = 6
      Width = 195
      Height = 169
      Caption = #20445#23384#36873#39033
      TabOrder = 0
      OnClick = GroupBox1Click
      object chkSaveUnmodified: TCheckBox
        Left = 16
        Top = 53
        Width = 175
        Height = 17
        Caption = #26410#21464#26356#25968#25454'(Unmodified data)'
        Checked = True
        State = cbChecked
        TabOrder = 0
      end
      object chkSaveInserted: TCheckBox
        Left = 16
        Top = 82
        Width = 175
        Height = 17
        Caption = #26032#25554#20837#25968#25454'(Inserted Data)'
        Checked = True
        State = cbChecked
        TabOrder = 1
      end
      object chkSaveModified: TCheckBox
        Left = 16
        Top = 111
        Width = 175
        Height = 17
        Caption = #24050#20462#25913#25968#25454'(Modified Data)'
        Checked = True
        State = cbChecked
        TabOrder = 2
      end
      object chkSaveDeleted: TCheckBox
        Left = 16
        Top = 140
        Width = 175
        Height = 17
        Caption = #24050#21024#38500#25968#25454'(Deleted Data)'
        Checked = True
        State = cbChecked
        TabOrder = 3
      end
      object chkSaveMeta: TCheckBox
        Left = 16
        Top = 24
        Width = 175
        Height = 17
        Caption = #20803#25968#25454'(Meta data)'
        Checked = True
        State = cbChecked
        TabOrder = 4
      end
    end
    object GroupBox2: TGroupBox
      Left = 6
      Top = 181
      Width = 195
      Height = 228
      Caption = #27969#22788#29702#36873#39033
      TabOrder = 1
      OnClick = GroupBox2Click
      object Label1: TLabel
        Left = 16
        Top = 83
        Width = 52
        Height = 13
        Caption = #23494#38053#31867#22411':'
      end
      object Label3: TLabel
        Left = 16
        Top = 111
        Width = 28
        Height = 13
        Caption = #23494#38053':'
      end
      object Label4: TLabel
        Left = 16
        Top = 132
        Width = 52
        Height = 13
        Caption = #21021#22987#21521#37327':'
      end
      object Label5: TLabel
        Left = 16
        Top = 203
        Width = 52
        Height = 13
        Caption = #21387#32553#32423#21035':'
      end
      object Label6: TLabel
        Left = 16
        Top = 54
        Width = 52
        Height = 13
        Caption = #21152#23494#31639#27861':'
      end
      object chkCompress: TCheckBox
        Left = 16
        Top = 176
        Width = 169
        Height = 17
        Caption = #21387#32553#25968#25454'(Compress mode)'
        TabOrder = 0
      end
      object chkEncrypt: TCheckBox
        Left = 16
        Top = 25
        Width = 169
        Height = 17
        Caption = #21152#23494#25968#25454'(Encrypt mode)'
        TabOrder = 1
      end
      object cbxKeyType: TComboBox
        Left = 74
        Top = 79
        Width = 95
        Height = 21
        Style = csDropDownList
        ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
        ItemIndex = 2
        TabOrder = 2
        Text = '256'#20301
        Items.Strings = (
          '128'#20301
          '192'#20301
          '256'#20301)
      end
      object edtKey: TEdit
        Left = 50
        Top = 107
        Width = 121
        Height = 21
        ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
        TabOrder = 3
        Text = 'QDAC.Encrypt.Demo'
      end
      object edtInitVector: TEdit
        Left = 50
        Top = 151
        Width = 121
        Height = 21
        ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
        TabOrder = 4
        Text = 'QDAC.QDataSet'
      end
      object cbxCompressLevel: TComboBox
        Left = 74
        Top = 199
        Width = 95
        Height = 21
        Style = csDropDownList
        ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
        ItemIndex = 2
        TabOrder = 5
        Text = #40664#35748
        Items.Strings = (
          #26080
          #24555#36895
          #40664#35748
          #26368#20339)
      end
      object cbxEncryptMode: TComboBox
        Left = 74
        Top = 50
        Width = 95
        Height = 21
        Style = csDropDownList
        ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
        ItemIndex = 0
        TabOrder = 6
        Text = 'ECB'
        Items.Strings = (
          'ECB'
          'CBC')
      end
    end
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 41
    Width = 560
    Height = 415
    Align = alClient
    DataSource = DataSource1
    ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 779
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object Button5: TButton
      Left = 8
      Top = 10
      Width = 137
      Height = 25
      Caption = #19978#19968#25968#25454#38598'(Prior)'
      TabOrder = 0
      OnClick = Button5Click
    end
    object Button6: TButton
      Left = 151
      Top = 10
      Width = 133
      Height = 25
      Caption = #19979#19968#25968#25454#38598'(Next)'
      TabOrder = 1
      OnClick = Button6Click
    end
    object Button3: TButton
      Left = 290
      Top = 10
      Width = 136
      Height = 25
      Caption = #21333#25968#25454#38598'(Single DataSet)'
      TabOrder = 2
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 432
      Top = 10
      Width = 135
      Height = 25
      Caption = #22810#25968#25454#38598'(Multi DataSet)'
      TabOrder = 3
      OnClick = Button4Click
    end
    object Button1: TButton
      Left = 573
      Top = 10
      Width = 91
      Height = 25
      Caption = #25171#24320'(&Load)...'
      TabOrder = 4
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 670
      Top = 10
      Width = 91
      Height = 25
      Caption = #20445#23384'(&Save)...'
      TabOrder = 5
      OnClick = Button2Click
    end
  end
  object DataSource1: TDataSource
    Left = 328
    Top = 232
  end
  object OpenDialog1: TOpenDialog
    Filter = 
      #21407#29983#26684#24335'|*.qdb|MessagePack '#26684#24335'|*.mpk|Json'#26684#24335'|*.json|FireDAC'#20108#36827#21046'|*.fdbin' +
      '|FireDAC JSON|*.fdjson|FireDAC XML|*.fdxml|CSV '#25991#26412#25991#20214'|*.csv|ADO XM' +
      'L '#26684#24335'|*.xml|ClientDataSet XML '#26684#24335'|*.xml|'#25152#26377#25991#20214'|*.*'
    Title = #23548#20837#25968#25454#38598
    Left = 128
    Top = 80
  end
  object SaveDialog1: TSaveDialog
    Filter = 
      #21407#29983#26684#24335'|*.qdb|MessagePack '#26684#24335'|*.mpk|Json'#26684#24335'|*.json|FireDAC'#20108#36827#21046'|*.fdbin' +
      '|FireDAC JSON|*.fdjson|FireDAC XML|*.fdxml|CSV '#25991#26412#25991#20214'|*.csv|MSSQL'#33050 +
      #26412'|*.sql|PostgreSQL'#33050#26412'|*.sql|MySQL'#33050#26412'|*.sql|'#25152#26377#25991#20214'|*.*'
    Title = #23548#20986#25968#25454#38598
    Left = 216
    Top = 80
  end
end
