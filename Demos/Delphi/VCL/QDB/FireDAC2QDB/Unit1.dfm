object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'FireDAC->QDB '#36716#25442#22120#28436#31034
  ClientHeight = 410
  ClientWidth = 704
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
  object Splitter1: TSplitter
    Left = 329
    Top = 162
    Height = 160
    ExplicitLeft = 360
    ExplicitTop = 176
    ExplicitHeight = 100
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 704
    Height = 65
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label3: TLabel
      Left = 16
      Top = 45
      Width = 36
      Height = 13
      Caption = #36716#25442#22120
    end
    object Button3: TButton
      Left = 135
      Top = 10
      Width = 114
      Height = 25
      Caption = #36716#25442#21040' QDataSet'
      TabOrder = 0
      OnClick = Button3Click
    end
    object Button1: TButton
      Left = 15
      Top = 10
      Width = 114
      Height = 25
      Caption = #25171#24320#21407#22987#25968#25454#38598
      TabOrder = 1
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 255
      Top = 10
      Width = 98
      Height = 25
      Caption = #36716#25442#21040' FireDAC'
      TabOrder = 2
      OnClick = Button2Click
    end
    object Button4: TButton
      Left = 368
      Top = 10
      Width = 75
      Height = 25
      Caption = #25552#20132#26356#26032
      TabOrder = 3
      OnClick = Button4Click
    end
    object chkDeleted: TCheckBox
      Left = 456
      Top = 16
      Width = 57
      Height = 17
      Caption = #24050#21024#38500
      TabOrder = 4
      OnClick = chkInsertedClick
    end
    object chkUnchange: TCheckBox
      Left = 514
      Top = 16
      Width = 57
      Height = 17
      Caption = #26410#21464#26356
      Checked = True
      State = cbChecked
      TabOrder = 5
      OnClick = chkInsertedClick
    end
    object chkModified: TCheckBox
      Left = 573
      Top = 16
      Width = 57
      Height = 17
      Caption = #24050#20462#25913
      Checked = True
      State = cbChecked
      TabOrder = 6
      OnClick = chkInsertedClick
    end
    object chkInserted: TCheckBox
      Left = 632
      Top = 16
      Width = 57
      Height = 17
      Caption = #26032#25554#20837
      Checked = True
      State = cbChecked
      TabOrder = 7
      OnClick = chkInsertedClick
    end
    object cbxConverterType: TComboBox
      Left = 64
      Top = 41
      Width = 185
      Height = 21
      ItemIndex = 0
      TabOrder = 8
      Text = 'FireDAC<->QDB '#20108#36827#21046#36716#25442#22120
      Items.Strings = (
        'FireDAC<->QDB '#20108#36827#21046#36716#25442#22120
        'FireDAC<->QDB JSON '#36716#25442#22120
        'FireDAC<->QDB XML '#36716#25442#22120)
    end
    object Button5: TButton
      Left = 255
      Top = 34
      Width = 75
      Height = 25
      Caption = 'Button5'
      TabOrder = 9
      OnClick = Button5Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 162
    Width = 329
    Height = 160
    Align = alLeft
    BevelOuter = bvNone
    Caption = 'Panel2'
    TabOrder = 1
    object Label2: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 323
      Height = 13
      Align = alTop
      Caption = 'FireDAC '#26597#35810#32467#26524
      ExplicitWidth = 90
    end
    object DBGrid1: TDBGrid
      Left = 0
      Top = 19
      Width = 329
      Height = 141
      Align = alClient
      Ctl3D = False
      DataSource = DataSource1
      ParentCtl3D = False
      TabOrder = 0
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'Tahoma'
      TitleFont.Style = []
    end
  end
  object Panel4: TPanel
    Left = 332
    Top = 162
    Width = 372
    Height = 160
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object Label1: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 366
      Height = 13
      Align = alTop
      Caption = 'QDB '#36716#25442#32467#26524
      ExplicitWidth = 72
    end
    object DBGrid2: TDBGrid
      Left = 0
      Top = 19
      Width = 372
      Height = 141
      Align = alClient
      Ctl3D = False
      DataSource = DataSource2
      ParentCtl3D = False
      TabOrder = 0
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'Tahoma'
      TitleFont.Style = []
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 65
    Width = 704
    Height = 97
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
    object mmSQL: TMemo
      Left = 0
      Top = 0
      Width = 704
      Height = 97
      Align = alClient
      Lines.Strings = (
        'select id,char_d from dbtypes')
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
    end
  end
  object Panel5: TPanel
    Left = 0
    Top = 322
    Width = 704
    Height = 88
    Align = alBottom
    TabOrder = 4
    object Image1: TImage
      Left = 1
      Top = 1
      Width = 105
      Height = 86
      Align = alLeft
      OnDblClick = Image1DblClick
      ExplicitLeft = 224
      ExplicitTop = -17
      ExplicitHeight = 105
    end
    object Image2: TImage
      Left = 598
      Top = 1
      Width = 105
      Height = 86
      Align = alRight
      OnDblClick = Image2DblClick
      ExplicitLeft = 224
      ExplicitTop = -17
      ExplicitHeight = 105
    end
    object Memo1: TMemo
      Left = 106
      Top = 1
      Width = 185
      Height = 86
      Align = alLeft
      TabOrder = 0
    end
    object Memo2: TMemo
      Left = 413
      Top = 1
      Width = 185
      Height = 86
      Align = alRight
      TabOrder = 1
    end
    object Button6: TButton
      Left = 335
      Top = 37
      Width = 75
      Height = 25
      Caption = 'Button6'
      TabOrder = 2
      OnClick = Button6Click
    end
    object Button7: TButton
      Left = 290
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Button6'
      TabOrder = 3
      OnClick = Button7Click
    end
  end
  object FDQuery1: TFDQuery
    AfterScroll = FDQuery1AfterScroll
    CachedUpdates = True
    Connection = fdSQL
    SchemaAdapter = FDSchemaAdapter1
    UpdateOptions.AssignedValues = [uvLockMode]
    SQL.Strings = (
      
        'select id,abstime_d,abstime_a,bigint_d,bigint_a,bit_d,bit_a,varb' +
        'it_d,varbit_a,bool_d,bool_a,box_d,box_a'
      'uuid_d,'
      'uuid_a,'
      'xid_d,'
      'xid_a,'
      'xml_d,'
      'xml_a from dbtypes')
    Left = 576
    Top = 56
  end
  object fdPg: TFDConnection
    Params.Strings = (
      'User_Name=qdac'
      'Password=Qdac.Demo'
      'Database=QDAC_Demo'
      'Port=15432'
      'Server=www.qdac.cc'
      'DriverID=PG')
    LoginPrompt = False
    Left = 192
    Top = 176
  end
  object FDPhysPgDriverLink1: TFDPhysPgDriverLink
    VendorLib = 'D:\pgAdmin III\1.20\libpq.dll'
    Left = 192
    Top = 128
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 248
    Top = 72
  end
  object FDStanStorageJSONLink1: TFDStanStorageJSONLink
    Left = 608
    Top = 184
  end
  object FDStanStorageBinLink1: TFDStanStorageBinLink
    Left = 504
    Top = 160
  end
  object DataSource1: TDataSource
    DataSet = FDQuery1
    Left = 488
    Top = 64
  end
  object DataSource2: TDataSource
    Left = 649
    Top = 56
  end
  object FDSchemaAdapter1: TFDSchemaAdapter
    Left = 184
    Top = 72
  end
  object FDStanStorageXMLLink1: TFDStanStorageXMLLink
    Left = 504
    Top = 248
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Left = 344
    Top = 208
  end
  object fdSQL: TFDConnection
    Params.Strings = (
      'Database=tadertest'
      'User_Name=sa'
      'Password=770328'
      'Server=127.0.0.1'
      'OSAuthent=No'
      'DriverID=MSSQL')
    LoginPrompt = False
    Left = 224
    Top = 176
  end
  object OpenDialog1: TOpenDialog
    Left = 408
    Top = 168
  end
end
