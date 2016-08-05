{-------------------------------------------------------------------------------
 
 Copyright (c) 1999-2014 Ralf Junker, Yunqa
 Internet: http://www.yunqa.de
 E-Mail:   delphi@yunqa.de

-------------------------------------------------------------------------------}

unit DIXmlConverters;

{$I DICompilers.inc}

interface

uses
  DIXml;

function xmlGB2312ToUtf8(
  Out_: C_char_ptr;
  OutLen: C_int_ptr;
  In_: C_char_ptr;
  InLen: C_int_ptr): C_int;

function xmlUtf8ToGB2312(
  Out_: C_char_ptr;
  OutLen: C_int_ptr;
  In_: C_char_ptr;
  InLen: C_int_ptr): C_int;

function xmlWin1251ToUtf8(
  Out_: C_char_ptr;
  OutLen: C_int_ptr;
  In_: C_char_ptr;
  InLen: C_int_ptr): C_int;

function xmlUtf8ToWin1251(
  Out_: C_char_ptr;
  OutLen: C_int_ptr;
  In_: C_char_ptr;
  InLen: C_int_ptr): C_int;

implementation

uses
  DIConverters;

function xmlEncodingWrapper(
  Out_: C_char_ptr;
  OutLen: C_int_ptr;
  In_: C_char_ptr;
  InLen: C_int_ptr;
  mbtowc: xxx_mbtowc;
  wctomb: xxx_wctomb): C_int;
var
  convIn, convOut: conv_struct;
  pIn, pOut: PAnsiChar;
  lIn, lOut, rIn, rOut: Integer;
  u: ucs4_t;
begin
  if Assigned(Out_) and Assigned(OutLen) and Assigned(InLen) then
    begin
      Result := 0;
      if Assigned(In_) then
        begin
          lIn := InLen^;
          if lIn > 0 then
            begin
              pOut := Out_; lOut := OutLen^; pIn := In_;
              convIn.ioState := 0; convOut.ioState := 0;
              repeat

                rIn := mbtowc(@convIn, u, pIn, lIn);
                if rIn > 0 then
                  begin
                    if lOut > 0 then
                      begin

                        rOut := wctomb(@convOut, pOut, u, lOut);
                        if rOut > 0 then
                          begin
                            Inc(pOut, rOut); Dec(lOut, rOut);
                          end
                        else
                          begin
                            if lOut = RET_ILUNI then
                              Result := -2;
                            Break;
                          end;
                      end
                    else
                      Break;
                    Inc(pIn, rIn); Dec(lIn, rIn);
                  end
                else
                  begin
                    if rIn = RET_ILSEQ then
                      Result := -2;
                    Break;
                  end;
              until lIn = 0;

              InLen^ := pIn - In_;
              OutLen^ := pOut - Out_;
              if Result = 0 then
                Result := OutLen^;
              Exit;
            end
        end;
      InLen^ := Result;
      OutLen^ := Result;
    end
  else
    Result := -1;
end;

function xmlGB2312ToUtf8(
  Out_: C_char_ptr;
  OutLen: C_int_ptr;
  In_: C_char_ptr;
  InLen: C_int_ptr): C_int;
begin
  Result := xmlEncodingWrapper(Out_, OutLen, In_, InLen, euc_cn_mbtowc, utf8_wctomb);
end;

function xmlUtf8ToGB2312(
  Out_: C_char_ptr;
  OutLen: C_int_ptr;
  In_: C_char_ptr;
  InLen: C_int_ptr): C_int;
begin
  Result := xmlEncodingWrapper(Out_, OutLen, In_, InLen, utf8_mbtowc, euc_cn_wctomb);
end;

function xmlWin1251ToUtf8(
  Out_: C_char_ptr;
  OutLen: C_int_ptr;
  In_: C_char_ptr;
  InLen: C_int_ptr): C_int;
begin
  Result := xmlEncodingWrapper(Out_, OutLen, In_, InLen, cp1251_mbtowc, utf8_wctomb);
end;

function xmlUtf8ToWin1251(
  Out_: C_char_ptr;
  OutLen: C_int_ptr;
  In_: C_char_ptr;
  InLen: C_int_ptr): C_int;
begin
  Result := xmlEncodingWrapper(Out_, OutLen, In_, InLen, utf8_mbtowc, cp1251_wctomb);
end;

end.

