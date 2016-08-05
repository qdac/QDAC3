inherited frmTaskProps: TfrmTaskProps
  Caption = 'TaskProperty'
  ClientHeight = 600
  ClientWidth = 863
  Font.Charset = GB2312_CHARSET
  Font.Height = -12
  Font.Name = #23435#20307
  OnCreate = FormCreate
  ExplicitWidth = 863
  ExplicitHeight = 600
  PixelsPerInch = 96
  TextHeight = 12
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 760
    Height = 600
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = #35302#21457#26102#38388
      ExplicitWidth = 704
      ExplicitHeight = 459
      object pgcFireSources: TPageControl
        Left = 0
        Top = 428
        Width = 752
        Height = 144
        Align = alClient
        MultiLine = True
        TabOrder = 0
        ExplicitTop = 0
        ExplicitHeight = 572
      end
      object gbxExecWeek: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 115
        Width = 746
        Height = 76
        Align = alTop
        Caption = #21608
        TabOrder = 1
        ExplicitWidth = 736
        object Label10: TLabel
          Left = 120
          Top = 51
          Width = 108
          Height = 12
          Caption = #27599#38548'            '#21608
        end
        object clbWeekDayList: TCheckListBox
          AlignWithMargins = True
          Left = 5
          Top = 17
          Width = 736
          Height = 21
          Align = alTop
          Columns = 7
          ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
          ItemHeight = 13
          Items.Strings = (
            #26143#26399#19968
            #26143#26399#20108
            #26143#26399#19977
            #26143#26399#22235
            #26143#26399#20116
            #26143#26399#20845
            #26143#26399#26085)
          TabOrder = 0
          ExplicitWidth = 726
        end
        object chkAnyWeekDay: TCheckBox
          Left = 8
          Top = 50
          Width = 73
          Height = 17
          Caption = #20219#24847
          TabOrder = 1
        end
        object seWeekDayInterval: TSpinEdit
          Left = 148
          Top = 48
          Width = 55
          Height = 21
          MaxValue = 0
          MinValue = 0
          TabOrder = 2
          Value = 0
        end
      end
      object gbxExecDay: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 197
        Width = 746
        Height = 117
        Align = alTop
        Caption = #22825
        TabOrder = 2
        ExplicitWidth = 736
        object Label11: TLabel
          Left = 120
          Top = 95
          Width = 108
          Height = 12
          Caption = #27599#38548'            '#22825
        end
        object clbDayList: TCheckListBox
          AlignWithMargins = True
          Left = 5
          Top = 17
          Width = 736
          Height = 69
          Align = alTop
          Columns = 8
          ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
          ItemHeight = 13
          Items.Strings = (
            '1'
            '2'
            '3'
            '4'
            '5'
            '6'
            '7'
            '8'
            '9'
            '10'
            '11'
            '12'
            '13'
            '14'
            '15'
            '16'
            '17'
            '18'
            '19'
            '20'
            '21'
            '22'
            '23'
            '24'
            '25'
            '26'
            '27'
            '28'
            '29'
            '30'
            '31')
          TabOrder = 0
          ExplicitWidth = 726
        end
        object chkAnyDay: TCheckBox
          Left = 8
          Top = 93
          Width = 73
          Height = 17
          Caption = #20219#24847
          TabOrder = 1
        end
        object seDayInterval: TSpinEdit
          Left = 148
          Top = 91
          Width = 55
          Height = 21
          MaxValue = 0
          MinValue = 0
          TabOrder = 2
          Value = 0
        end
      end
      object gbxExecHour: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 320
        Width = 746
        Height = 105
        Align = alTop
        Caption = #26102
        TabOrder = 3
        ExplicitWidth = 736
        object Label8: TLabel
          Left = 115
          Top = 83
          Width = 120
          Height = 12
          Caption = #27599#38548'            '#23567#26102
        end
        object clbHourList: TCheckListBox
          AlignWithMargins = True
          Left = 5
          Top = 17
          Width = 736
          Height = 57
          Align = alTop
          Columns = 8
          ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
          ItemHeight = 13
          Items.Strings = (
            '0'
            '1'
            '2'
            '3'
            '4'
            '5'
            '6'
            '7'
            '8'
            '9'
            '10'
            '11'
            '12'
            '13'
            '14'
            '15'
            '16'
            '17'
            '18'
            '19'
            '20'
            '21'
            '22'
            '23')
          TabOrder = 0
          ExplicitWidth = 726
        end
        object chkAnyHour: TCheckBox
          Left = 3
          Top = 81
          Width = 73
          Height = 17
          Caption = #20219#24847
          TabOrder = 1
        end
        object seHourInterval: TSpinEdit
          Left = 148
          Top = 79
          Width = 55
          Height = 21
          MaxValue = 0
          MinValue = 0
          TabOrder = 2
          Value = 0
        end
      end
      object gbxExecMinute: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 431
        Width = 746
        Height = 138
        Align = alClient
        Caption = #20998
        TabOrder = 4
        ExplicitWidth = 736
        ExplicitHeight = 155
        object clbMinuteList: TCheckListBox
          AlignWithMargins = True
          Left = 5
          Top = 17
          Width = 736
          Height = 85
          Align = alClient
          Columns = 8
          ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
          ItemHeight = 13
          Items.Strings = (
            '0'
            '1'
            '2'
            '3'
            '4'
            '5'
            '6'
            '7'
            '8'
            '9'
            '10'
            '11'
            '12'
            '13'
            '14'
            '15'
            '16'
            '17'
            '18'
            '19'
            '20'
            '21'
            '22'
            '23'
            '24'
            '25'
            '26'
            '27'
            '28'
            '29'
            '30'
            '31'
            '32'
            '33'
            '34'
            '35'
            '36'
            '36'
            '38'
            '39'
            '40'
            '41'
            '42'
            '43'
            '44'
            '45'
            '46'
            '47'
            '48'
            '49'
            '50'
            '51'
            '52'
            '53'
            '54'
            '55'
            '56'
            '57'
            '58'
            '59')
          TabOrder = 0
          ExplicitWidth = 726
          ExplicitHeight = 102
        end
        object Panel2: TPanel
          Left = 2
          Top = 105
          Width = 742
          Height = 31
          Align = alBottom
          TabOrder = 1
          ExplicitTop = 217
          object Label9: TLabel
            Left = 118
            Top = 11
            Width = 120
            Height = 12
            Caption = #27599#38548'            '#20998#38047
          end
          object chkAnyMinute: TCheckBox
            Left = 6
            Top = 9
            Width = 73
            Height = 17
            Caption = #20219#24847
            TabOrder = 0
          end
          object seMinuteInterval: TSpinEdit
            Left = 146
            Top = 7
            Width = 55
            Height = 21
            MaxValue = 0
            MinValue = 0
            TabOrder = 1
            Value = 0
          end
        end
      end
      object gbxExecMonth: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 746
        Height = 106
        Align = alTop
        Caption = #26376
        TabOrder = 5
        ExplicitWidth = 736
        object Label3: TLabel
          Left = 120
          Top = 81
          Width = 108
          Height = 12
          Caption = #27599#38548'            '#26376
        end
        object clbMonthList: TCheckListBox
          AlignWithMargins = True
          Left = 5
          Top = 17
          Width = 736
          Height = 56
          Align = alTop
          Columns = 4
          ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
          ItemHeight = 13
          Items.Strings = (
            #19968#26376
            #20108#26376
            #19977#26376
            #22235#26376
            #20116#26376
            #20845#26376
            #19971#26376
            #20843#26376
            #20061#26376
            #21313#26376
            #21313#19968#26376
            #21313#20108#26376)
          TabOrder = 0
          ExplicitWidth = 726
        end
        object chkAnyMonth: TCheckBox
          Left = 8
          Top = 80
          Width = 73
          Height = 17
          Caption = #20219#24847
          TabOrder = 1
        end
        object seMonthInterval: TSpinEdit
          Left = 148
          Top = 78
          Width = 55
          Height = 21
          MaxValue = 0
          MinValue = 0
          TabOrder = 2
          Value = 0
        end
      end
    end
    object tsTask: TTabSheet
      Caption = #20219#21153
      ImageIndex = 1
      ExplicitWidth = 704
      ExplicitHeight = 459
      object RadioButton9: TRadioButton
        Left = 16
        Top = 16
        Width = 113
        Height = 17
        Caption = #25191#34892#22806#37096#31243#24207
        TabOrder = 0
      end
      object edtShellCmd: TEdit
        Left = 24
        Top = 39
        Width = 681
        Height = 20
        ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
        TabOrder = 1
      end
      object Button1: TButton
        Left = 704
        Top = 38
        Width = 25
        Height = 23
        Caption = '...'
        TabOrder = 2
        OnClick = Button1Click
      end
      object rbIsMessageTask: TRadioButton
        Left = 16
        Top = 72
        Width = 113
        Height = 17
        Caption = #25552#37266#20197#19979#20869#23481
        Checked = True
        TabOrder = 3
        TabStop = True
      end
      object mmHintMsg: TMemo
        Left = 24
        Top = 95
        Width = 705
        Height = 458
        ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
        TabOrder = 4
      end
    end
  end
  object Panel3: TPanel
    Left = 760
    Top = 0
    Width = 103
    Height = 600
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = 759
    object Button2: TButton
      Left = 10
      Top = 30
      Width = 75
      Height = 25
      Caption = #30830#23450'(&O)'
      TabOrder = 0
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 10
      Top = 61
      Width = 75
      Height = 25
      Caption = #21462#28040'(&C)'
      TabOrder = 1
      OnClick = Button3Click
    end
  end
  object odExecute: TOpenDialog
    Filter = #21487#25191#34892#25991#20214'|*.EXE|'#25209#22788#29702#31243#24207'|*.BAT;*.CMD|'#25152#26377#25991#20214'|*.*'
    Left = 640
    Top = 176
  end
end
