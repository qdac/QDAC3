unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, JvComponentBase,
  JvInterpreter, PaxRegister, PaxCompiler, PaxRunner, PaxInterpreter,
  Vcl.ComCtrls, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    JvInterpreterProgram1: TJvInterpreterProgram;
    PaxCompiler1: TPaxCompiler;
    PaxInterpreter1: TPaxInterpreter;
    PaxPascalLanguage1: TPaxPascalLanguage;
    Panel1: TPanel;
    Button1: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    tsResult: TTabSheet;
    Memo1: TMemo;
    mmResult: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

type
  TQVMRegister = record
    case Integer of
      // Integer
      0:
        (I64: Int64);
      1:
        (I32: array [0 .. 1] of Integer);
      2:
        (I16: array [0 .. 3] of Smallint);
      3:
        (I8: array [0 .. 7] of Shortint);
      4:
        (U64: UInt64);
      5:
        (U32: array [0 .. 1] of Cardinal);
      6:
        (U16: array [0 .. 3] of Word);
      7:
        (U8: array [0 .. 7] of Byte);
      // Float
      8:
        (F64: Double);
      9:
        (F32: Single);
      // Pointer
      10:
        (P: Pointer);
  end;

  TQVMRegisters = record
    Op: array [0 .. 2] of TQVMRegister;
    Result: TQVMRegister;
  end;

  TQVMOpFunction = procedure of object;

  TQVMByteCode = record
    OpCode: TQVMOpFunction;
    Data: array [0 .. 1] of TQVMRegister;
  end;

  TQVM = class
  protected
    FStep, FCount: Integer;
    FByteCodes: array of TQVMByteCode;
    FResult: TQVMRegister;
    procedure Inc_I8;
    procedure Inc_I16;
    procedure Inc_I32;
    procedure Inc_I64;
    procedure AssignC_I32;
    procedure Cmp_I32;
    procedure Jmp;
    procedure JmpZ;
    procedure DoExit;
    procedure DoExitIfZero;
  public
    constructor Create;
    procedure Push_Int8_Inc(var V: Shortint);
    procedure Push_Int16_Inc(var V: Smallint);
    procedure Push_Int32_Inc(var V: Integer);
    procedure Push_Int64_Inc(var V: Int64);
    procedure Push_Int32_Assign(var V: Integer; AValue: Integer);
    procedure PushJmp(AOffset: Integer);
    procedure PushCmp(var V: Integer; const AValue: Integer);
    procedure PushJmpIfZero(AOffset: Integer);
    procedure PushExitIfZero;
    procedure PushExit;
    procedure Run;
  end;
  { TQVM }

procedure TForm1.Button1Click(Sender: TObject);
var
  AVM: TQVM;
  I: Integer;
  T1, T2, T3, T4: Cardinal;
  procedure DoPaxTest;
  begin
    PaxCompiler1.Reset;
    PaxCompiler1.RegisterLanguage(PaxPascalLanguage1);
    PaxCompiler1.RegisterVariable(0, 'I', _typeINTEGER, @I);
    PaxCompiler1.AddModule('1', PaxPascalLanguage1.LanguageName);
    PaxCompiler1.AddCode('1', 'procedure DoSum;');
    PaxCompiler1.AddCode('1', 'begin');
    PaxCompiler1.AddCode('1', ' I := 0;');
    PaxCompiler1.AddCode('1', ' while I<1000000 do');
    PaxCompiler1.AddCode('1', '  Inc(I);');
    PaxCompiler1.AddCode('1', 'end;');
    PaxCompiler1.AddCode('1', 'begin');
    PaxCompiler1.AddCode('1', 'end.');

    if PaxCompiler1.Compile(PaxInterpreter1) then
    begin
      PaxInterpreter1.RunInitialization;
      T4 := GetTickCount;
      PaxInterpreter1.CallRoutine('DoSum', []); // call it
      T4 := GetTickCount - T4;
      mmResult.Lines.Add('PaxCompiler 用时 '+IntToStr(T4)+'ms');
      Application.ProcessMessages;
    end;
  end;

begin
  tsResult.TabVisible:=True;
  PageControl1.ActivePage:=tsResult;
  mmResult.Lines.Add('开始原生循环测试');
  Application.ProcessMessages;
  T1 := GetTickCount;
  I := 0;
  while I < 1000000 do
    Inc(I);
  T1 := GetTickCount - T1;
  mmResult.Lines.Add('原生用时 '+IntToStr(T1)+'ms');
  mmResult.Lines.Add('开始 QScript 字节码执行测试');
  Application.ProcessMessages;
  T2 := GetTickCount;
  AVM := TQVM.Create;
  // 添加上面循环对象的QScript字节码序列
  AVM.Push_Int32_Assign(I, 0); // 1、将 I 初始值赋值为0
  AVM.PushCmp(I, 1000000); // 2、比较 I 与目标值，如果相等，VM.Result.I8[0] 被设置为0，否则为1
  AVM.PushExitIfZero; // 3、比较 VM.Result.I8[0] 是否为0，是的话，退出执行
  AVM.Push_Int32_Inc(I); // 4、将 I 的值+1：Inc(I)
  AVM.PushJmp(-3); // 5、往回跳3步，退到第 2 步
  // 添加完成，执行
  AVM.Run;
  T2 := GetTickCount - T2;
  mmResult.Lines.Add('QScript 用时 '+IntToStr(T2)+'ms');
  mmResult.Lines.Add('开始 jvInterpreter.Pascal 执行测试');
  Application.ProcessMessages;
  AVM.Free;
  JvInterpreterProgram1.Compile;
  T3 := GetTickCount;
  JvInterpreterProgram1.CallFunction('DoSum', nil, []);
  T3 := GetTickCount - T3;
  mmResult.Lines.Add('jvInterpreter.Pascal 用时 '+IntToStr(T3)+'ms');
  mmResult.Lines.Add('开始 PaxCompiler 执行测试');
  Application.ProcessMessages;
  DoPaxTest;
  mmResult.Lines.Add('测试完成(以 QScript 为基准对比）:');
  mmResult.Lines.Add('jvInterpreter:'+FormatFloat('0.##',T3/T2)+'x');
  mmResult.Lines.Add('PaxCompiler:'+FormatFloat('0.##',T4/T2)+'x');
end;

{ TQVM }

procedure TQVM.AssignC_I32;
begin
  PInteger(FByteCodes[FStep].Data[0].P)^ := FByteCodes[FStep].Data[1].I32[0];
end;

procedure TQVM.Cmp_I32;
begin
  if PInteger(FByteCodes[FStep].Data[0].P)^ = FByteCodes[FStep].Data[1].I32[0]
  then
    FResult.I8[0] := 0
  else
    FResult.I8[0] := 1;
end;

constructor TQVM.Create;
begin
  SetLength(FByteCodes, 16384); // 18KB
end;

procedure TQVM.DoExit;
begin
  FStep := FCount;
end;

procedure TQVM.DoExitIfZero;
begin
  if FResult.I8[0] = 0 then
    FStep := FCount
end;

procedure TQVM.Run;
begin
  FStep := 0;
  while FStep < FCount do
  begin
    FByteCodes[FStep].OpCode;
    Inc(FStep);
  end;
end;

procedure TQVM.Inc_I16;
begin
  Inc(PSmallint(FByteCodes[FStep].Data[0].P)^);
end;

procedure TQVM.Inc_I32;
begin
  Inc(PInteger(FByteCodes[FStep].Data[0].P)^);
end;

procedure TQVM.Inc_I64;
begin
  Inc(PInt64(FByteCodes[FStep].Data[0].P)^);
end;

procedure TQVM.Inc_I8;
begin
  Inc(PShortint(FByteCodes[FStep].Data[0].P)^);
end;

procedure TQVM.Jmp;
begin
  Inc(FStep, FByteCodes[FStep].Data[0].I32[0]);
end;

procedure TQVM.JmpZ;
begin
  if FResult.I8[0] = 0 then
    Inc(FStep, FByteCodes[FStep].Data[0].I32[0]);
end;

procedure TQVM.PushCmp(var V: Integer; const AValue: Integer);
begin
  FByteCodes[FCount].OpCode := Cmp_I32;
  FByteCodes[FCount].Data[0].P := @V;
  FByteCodes[FCount].Data[1].I32[0] := AValue;
  Inc(FCount);
end;

procedure TQVM.PushExit;
begin
  FByteCodes[FCount].OpCode := DoExit;
  Inc(FCount);
end;

procedure TQVM.PushExitIfZero;
begin
  FByteCodes[FCount].OpCode := DoExitIfZero;
  Inc(FCount);
end;

procedure TQVM.PushJmp(AOffset: Integer);
begin
  FByteCodes[FCount].OpCode := Jmp;
  FByteCodes[FCount].Data[0].I32[0] := AOffset - 1; // Inc(FStep) Fix
  Inc(FCount);
end;

procedure TQVM.PushJmpIfZero(AOffset: Integer);
begin
  FByteCodes[FCount].OpCode := JmpZ;
  FByteCodes[FCount].Data[0].I32[0] := AOffset - 1; // Inc(FStep) Fix
  Inc(FCount);
end;

procedure TQVM.Push_Int16_Inc(var V: Smallint);
begin
  FByteCodes[FCount].OpCode := Inc_I16;
  FByteCodes[FCount].Data[0].P := @V;
  Inc(FCount);
end;

procedure TQVM.Push_Int32_Assign(var V: Integer; AValue: Integer);
begin
  FByteCodes[FCount].OpCode := AssignC_I32;
  FByteCodes[FCount].Data[0].P := @V;
  FByteCodes[FCount].Data[1].I32[0] := AValue;
  Inc(FCount);
end;

procedure TQVM.Push_Int32_Inc(var V: Integer);
begin
  FByteCodes[FCount].OpCode := Inc_I32;
  FByteCodes[FCount].Data[0].P := @V;
  Inc(FCount);
end;

procedure TQVM.Push_Int64_Inc(var V: Int64);
begin
  FByteCodes[FCount].OpCode := Inc_I64;
  FByteCodes[FCount].Data[0].P := @V;
  Inc(FCount);
end;

procedure TQVM.Push_Int8_Inc(var V: Shortint);
begin
  FByteCodes[FCount].OpCode := Inc_I8;
  FByteCodes[FCount].Data[0].P := @V;
  Inc(FCount);
end;

end.
