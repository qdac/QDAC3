object Form1: TForm1
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'QScript VM '#25216#26415#36335#32447#28436#31034
  ClientHeight = 359
  ClientWidth = 475
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 475
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitLeft = 144
    ExplicitTop = 168
    ExplicitWidth = 185
    object Button1: TButton
      Left = 8
      Top = 9
      Width = 75
      Height = 25
      Caption = #36895#24230#27979#35797
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 41
    Width = 475
    Height = 318
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = #31243#24207#31616#20171
      ExplicitWidth = 281
      ExplicitHeight = 165
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 467
        Height = 290
        Align = alClient
        Lines.Strings = (
          #26412#31243#24207#21482#26159#28436#31034#19968#19979' QScript '#23558#35201#25191#34892#30340#25216#26415#36335#32447#12290
          '1'#12289'QScript '#26377#20004#23618#26500#25104#65306
          '  '#65288'1'#65289' '#23383#33410#30721#25191#34892#23618#65292#29992#20110#23454#38469#25191#34892' QScript '#33050#26412#65307
          '  '#65288'2'#65289' '#35299#37322#23618#65292#36127#36131#23558#33050#26412#35299#37322#20026' QScript '#30340#23383#33410#30721#65307
          '2'#12289#26412#31243#24207#28436#31034#30340#26159#25191#34892#23618#30340#25928#29575#65292#35299#37322#23618#21040#23383#33410#30721#26159#20154#24037#28155#21152#36827#21435#30340#65307
          '3'#12289#26412#31243#24207#23454#38469#28436#31034#30340#26159#19979#38754#30340#20195#30721#25191#34892#36895#24230#65306
          ''
          #12304'Delphi'#12305
          ''
          'I:=0'
          'while I<1000000 do'
          '   Inc(I);'
          ''
          #12304'C++'#12305
          ''
          'I=0;'
          'while(I<1000000)'#11
          '   I++;'
          ''
          ''
          '4'#12289' QScript '#30446#21069#36824#21482#23384#22312#36335#32447#22270#19978#65292#38656#35201#26377#26102#38388#20877#21435#23454#29616#12290#22914#26524#20320#26377#20852#36259#65292#27426#36814#21442#19982
          #12290)
        ReadOnly = True
        TabOrder = 0
        ExplicitLeft = 8
        ExplicitTop = -5
        ExplicitWidth = 455
        ExplicitHeight = 281
      end
    end
    object tsResult: TTabSheet
      Caption = #27979#35797#32467#26524
      ImageIndex = 1
      TabVisible = False
      ExplicitWidth = 281
      ExplicitHeight = 165
      object mmResult: TMemo
        Left = 0
        Top = 0
        Width = 467
        Height = 290
        Align = alClient
        ReadOnly = True
        TabOrder = 0
        ExplicitLeft = 144
        ExplicitTop = 104
        ExplicitWidth = 185
        ExplicitHeight = 89
      end
    end
  end
  object JvInterpreterProgram1: TJvInterpreterProgram
    Pas.Strings = (
      'unit SumTest;'
      ''
      'procedure DoSum;'
      'var'
      '   I,S:Integer;'
      'begin'
      'I:=0;'
      'while I<1000000 do'
      '   I:=I+1;'
      'end;'
      ''
      'end.')
    Left = 208
    Top = 8
  end
  object PaxCompiler1: TPaxCompiler
    Alignment = 8
    DebugMode = False
    Left = 416
    Top = 8
  end
  object PaxInterpreter1: TPaxInterpreter
    Console = False
    Left = 344
    Top = 8
  end
  object PaxPascalLanguage1: TPaxPascalLanguage
    ExplicitOff = False
    CompleteBooleanEval = False
    UnitLookup = True
    PrintKeyword = 'print'
    PrintlnKeyword = 'println'
    Left = 272
    Top = 8
  end
end
