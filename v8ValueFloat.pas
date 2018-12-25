unit v8ValueFloat;

{$I v8Defines.inc}

interface

uses v8Face, strutils, sysutils, math;

function Float_AllocRaw(Size: NativeInt): Pv8Float; stdcall;
function Float_AddRef(Value: Pv8Float): NativeInt; stdcall;
function Float_Release(Value: Pv8Float): NativeInt; stdcall;
procedure Float_Dispose(Value: Pv8Float);

function Float_SetValueFromString(Data: PChar; Len: Integer; out Value: Pv8Float): Boolean;
function Float_SetValueFromDouble(Data: Double; out Value: Pv8Float): Boolean;
function Float_SetValueFromInt32(Data: Int32; out Value: Pv8Float): Boolean;
function Float_SetValueFromUInt32(Data: UInt32; out Value: Pv8Float): Boolean;
function Float_SetValueFromInt64(Data: Int64; out Value: Pv8Float): Boolean;
function Float_SetValueFromUInt64(Data: UInt64; Negative: Boolean; var Value: Pv8Float): Boolean;

function Float_DumpToString(Value: Pv8Float; var fs: TFormatSettings): string; overload;
function Float_DumpToString(Value: Pv8Float): string; overload;

procedure Float_DumpToInt32(Value: Pv8Float; var Res: Int32);
procedure Float_DumpToUInt32(Value: Pv8Float; var Res: UInt32);
procedure Float_DumpToInt64(Value: Pv8Float; var Res: Int64);
procedure Float_DumpToUInt64(Value: Pv8Float; var Res: UInt64);
procedure Float_DumpToDouble(Value: Pv8Float; var Res: Double);

function Float_IsZero(Value: Pv8Float): Boolean;
function Float_Compare(V1, V2: Pv8Float): Integer;
function Float_CompareAbs(V1, V2: Pv8Float): Integer;
function Float_Compare32(V1: Pv8Float; V2: Int32): Integer;
function Float_Compare64(V1: Pv8Float; V2: Int64): Integer;

function Float_AddAbs_OLD(V1, V2: Pv8Float; out Res: Pv8Float): Boolean;

function Float_Diff_OLD(V1, V2: Pv8Float; out Res: Pv8Float): Boolean;
function Float_Add(V1, V2: Pv8Float; out Res: Pv8Float): Boolean;
function Float_Multiply(V1, V2: Pv8Float; out Res: Pv8Float): Boolean;
function Float_Division(V1, V2: Pv8Float; out Res: Pv8Float): Boolean;
function Float_Modulo(V1, V2: Pv8Float; var Res: Pv8Float): Boolean;

function Float_Unique(Val: Pv8Float): Pv8Float;

function Float_Add_Int32(Val: Pv8Float; ValInt: Int32): Pv8Float;
function Float_Add_Int64(Val: Pv8Float; ValInt: Int64): Pv8Float;

function Float_TryAdd_Int32(Val: Pv8Float; ValInt: Int32): Boolean;
function Float_TryAdd_Int64(Val: Pv8Float; ValInt: Int64): Boolean;

function Float_AddAbs(V1, V2: Pv8Float; out Res: Pv8Float): Boolean;

function Float_Diff_Prep(V1, V2: Pv8Float; out Res: Pv8Float): Boolean;

function Float_Diff(V1, V2: Pv8Float; out Res: Pv8Float): Boolean;

const
  NUMBER_MASK = 10000;
  HEADER_SIZE = SizeOf(Tv8ReferenceValue) + SizeOf(NativeInt) + 2;

var
  FloatFS: TFormatSettings;

implementation

type

  Tv8FloatHead = packed record
    Reference: Tv8ReferenceValue;
    Negative: Boolean;
    reserved: array [0 .. 2] of byte;
    Size: Cardinal;
    Scale: Cardinal;
  end;

function Float_AllocRaw(Size: NativeInt): Pv8Float; stdcall;
var Len: NativeInt;
begin
  Len := Size;
  if Len < 8 then Len := 8;
  GetMem(pointer(Result), SizeOf(Tv8FloatHead) + Len shl 2);
  Result.Reference.Opacity := Len;
  Result.Size := Size;
  Result.Reference.RefCount := 1;
  Result.Negative := false;
  FillChar(Result.Data[0], Len shl 2, 0);
end;

function Float_AddRef(Value: Pv8Float): NativeInt; stdcall;
begin
  if not Assigned(Value) then exit(0);
  Result := AtomicIncrement(Value.Reference.RefCount);
end;

function Float_Release(Value: Pv8Float): NativeInt; stdcall;
begin
  if not Assigned(Value) then exit(0);
  Result := AtomicDecrement(Value.Reference.RefCount);
  if Result = 0 then FreeMem(Value);
end;

procedure Float_Dispose(Value: Pv8Float);
begin
  if not Assigned(Value) then exit;
  FreeMem(Value);
end;

procedure Float_Trim(Val: Pv8Float);
begin
  while Val.Size - 1 > Val.Scale do
  begin
    if Val.Data[0] > 0 then break;
    Move(Val.Data[1], Val.Data[0], Val.Size * 4);
    Dec(Val.Size);
  end;
  while Val.Scale > 0 do
  begin
    if Val.Data[Val.Size - 1] > 0 then break;
    Dec(Val.Size);
    Dec(Val.Scale);
  end;
end;

function Float_SetValueFromString(Data: PChar; Len: Integer; out Value: Pv8Float): Boolean;
var Negative: Boolean;
  CountL, CountR, CountBlock, Val, i: Integer;
  HasDot: Boolean;
  StartData, EndData, FloatPos: PChar;
  ValPointer: PInteger;

begin
  Result := false;
  Value := nil;
  while (Len > 0) and (Data^ < #33) do
  begin
    case ord(Data^) of
      0 .. 32:;
      160:;
    else break;
    end;
    Inc(Data);
    Dec(Len);
  end;
  if Len < 1 then exit;
  Negative := false;

  case Data^ of
    '-': begin
        Negative := True;
        Inc(Data);
        Dec(Len);
      end;
    '+': begin
        Inc(Data);
        Dec(Len);
      end;
  end;
  HasDot := false;
  CountL := 0;
  CountR := 0;
  StartData := Data;
  EndData := Data + Len;
  FloatPos := EndData;
  while Data < EndData do
  begin
    case ord(Data^) of
      ord('0') .. ord('9'): begin
          if HasDot then Inc(CountR)
          else Inc(CountL)
        end;
      0 .. 32:;
      160:;
      ord('.'), ord(','): begin
          if HasDot then exit;
          HasDot := True;
          FloatPos := Data;
        end
    else exit;
    end;
    Inc(Data);
  end;
  CountBlock := (CountL shr 2) + (CountR shr 2);
  if CountL and $3 > 0 then Inc(CountBlock);
  if CountR and $3 > 0 then Inc(CountBlock);

  Data := StartData;
  Value := Float_AllocRaw(CountBlock shl 2);
  Value.Size := CountBlock;
  Value.Scale := CountR shr 2;
  if CountR and $3 > 0 then Inc(Value.Scale);

  Result := True;

  ValPointer := @Value.Data[0];
  Data := StartData;
  ValPointer^ := 0;
  while Data < EndData do
  begin
    case ord(Data^) of
      ord('0') .. ord('9'): begin
          ValPointer^ := ValPointer^ * 10 + ord(Data^) - 48;

          Dec(CountL);
          if CountL and $3 = 0 then
          begin
            Inc(ValPointer);
            ValPointer^ := 0;
          end;
          if CountL = 0 then break;
        end;
      0 .. 32, 160:;
    end;
    Inc(Data);
  end;
  i := 0;
  if CountR > 0 then
  begin
    Data := FloatPos + 1;
    while Data < EndData do
    begin
      case ord(Data^) of
        ord('0') .. ord('9'): begin
            ValPointer^ := ValPointer^ * 10 + ord(Data^) - 48;
            Inc(i);
            if i and $3 = 0 then
            begin
              Inc(ValPointer);
              ValPointer^ := 0;
            end;
            if i = CountR then
            begin
              case i and $3 of
                1: ValPointer^ := ValPointer^ * 1000;
                2: ValPointer^ := ValPointer^ * 100;
                3: ValPointer^ := ValPointer^ * 10;
              end;
              break;
            end;
          end;
        0 .. 32, 160:;
      end;
      Inc(Data);
    end;
  end;
  Value.Negative := Negative;
end;

function Float_SetValueFromDouble(Data: Double; out Value: Pv8Float): Boolean;
var
  numStr: String;
begin
  numStr := FloatToStr(Data);
  Float_SetValueFromString(PChar(numStr), Length(numStr), Value);
end;

function Float_SetValueFromInt32(Data: Int32; out Value: Pv8Float): Boolean;
begin
  Float_SetValueFromUInt64(UInt64(Abs(Data)), Data < 0, Value);
end;

function Float_SetValueFromUInt32(Data: UInt32; out Value: Pv8Float): Boolean;
begin
  Float_SetValueFromUInt64(UInt64(Abs(Data)), false, Value);
end;

function Float_SetValueFromInt64(Data: Int64; out Value: Pv8Float): Boolean;
begin
  Float_SetValueFromUInt64(UInt64(Abs(Data)), Data < 0, Value);
end;

function Float_SetValueFromUInt64(Data: UInt64; Negative: Boolean; var Value: Pv8Float): Boolean;
var
  i, j, n: Integer;
  tempRes: array [0 .. 15] of Integer;
begin

  if Data = 0 then
  begin
    Value := nil;
    exit;
  end;
  i := 0;
  while Data > 0 do
  begin
    tempRes[i] := Data mod 10000;
    Data := Data div 10000;
    Inc(i);
  end;

  n := i - 1;
  Value := Float_AllocRaw(n);
  j := 0;
  for i := n downto 0 do
  begin
    Value.Data[i] := tempRes[n - i];
  end;
  Value.Size := n + 1;
  Value.Scale := 0;
  Value.Negative := Negative;
end;

function Float_DumpToString(Value: Pv8Float; var fs: TFormatSettings): string; overload;
var i, j, Val, ValD: Integer;
  RawData: array [0 .. 127] of char;
  RDStart, RDCurrent, RDMin, RDMax: PChar;
  PVal: PInteger;
begin
  if Float_IsZero(Value) then exit('0');
  i := (Value.Size shl 3) + 1;
  if i <= Length(RawData) then RDStart := @RawData[0]
  else GetMem(RDStart, i shl 1);
  //
  RDCurrent := RDStart;
  RDMax := RDStart;
  RDMin := nil;
  PVal := @Value.Data[Value.Size - 1];

  for i := 0 to Value.Scale - 1 do
  begin
    Val := PVal^;
    Dec(PVal);
    for j := 1 to 4 do
    begin
      ValD := Val mod 10;
      if (ValD > 0) and (RDMin = nil) then RDMin := RDCurrent;
      RDCurrent^ := char(48 + ValD);
      Inc(RDCurrent);
      Val := Val div 10;
    end;
  end;

  //Выводим разделитель дробной части
  if Assigned(RDMin) then
  begin
    if fs.DecimalSeparator = #0 then RDCurrent^ := '.'
    else RDCurrent^ := fs.DecimalSeparator;
    Inc(RDCurrent);
  end;

  if not Assigned(RDMin) then RDMin := RDCurrent;

  if (Value.Size - Value.Scale = 1) and (PVal^ = 0) then
  begin
    PWord(RDCurrent)^ := 48;
    Inc(RDCurrent);
  end
  else
    for i := 0 to (Value.Size - Value.Scale) - 1 do
    begin
      Val := PVal^;
      Dec(PVal);
      for j := 1 to 4 do
      begin
        ValD := Val mod 10;
        RDCurrent^ := char(48 + ValD);
        Inc(RDCurrent);
        if (ValD > 0) then RDMax := RDCurrent;
        Val := Val div 10;
        if (i * 4 + j) mod 3 = 0 then
        begin
          if fs.ThousandSeparator <> #0 then
          begin
            RDCurrent^ := fs.ThousandSeparator;
            Inc(RDCurrent);
          end;
        end;
      end;
    end;
  j := RDMax - RDMin;
  SetLength(Result, j);
  RDCurrent := pointer(Result);
  for i := 1 to j do
  begin
    Dec(RDMax);
    RDCurrent^ := RDMax^;
    Inc(RDCurrent);
  end;

  if RDStart <> @RawData[0] then FreeMem(RDStart);
end;

function Float_DumpToString(Value: Pv8Float): string; overload;
begin
  Result := Float_DumpToString(Value, FloatFS);
end;

procedure Float_DumpToInt32(Value: Pv8Float; var Res: Int32);
var
  i: Integer;
  dI: Int32;
begin
  dI := 0;
  for i := 0 to Value.Scale do
  begin
    dI := dI * 1000 + Value.Data[i];
  end;
  if Value.Negative then dI := dI * -1;
  Res := dI;
end;

procedure Float_DumpToUInt32(Value: Pv8Float; var Res: UInt32);
var
  i: Integer;
  dI: UInt32;
begin
  dI := 0;
  for i := 0 to Value.Scale do
  begin
    dI := dI * 1000 + Value.Data[i];
  end;
  Res := dI;
end;

procedure Float_DumpToInt64(Value: Pv8Float; var Res: Int64);
var
  i: Integer;
  dI: Int64;
begin
  dI := 0;
  for i := 0 to Value.Scale do
  begin
    dI := dI * 1000 + Value.Data[i];
  end;
  if Value.Negative then dI := dI * -1;
  Res := dI;
end;

procedure Float_DumpToUInt64(Value: Pv8Float; var Res: UInt64);
var
  i: Integer;
  dI: UInt64;
begin
  dI := 0;
  for i := 0 to Value.Scale do
  begin
    dI := dI * 1000 + Value.Data[i];
  end;
  Res := dI;
end;

procedure Float_DumpToDouble(Value: Pv8Float; var Res: Double);
var
  dI, dF: Double;
  i: Integer;
begin
  dI := 0;
  dF := 0;
  for i := 0 to Value.Size - Value.Scale - 1 do
  begin
    dI := dI * 10000 + Value.Data[i];
  end;
  for i := Value.Size - 1 downto Value.Size - Value.Scale do
  begin
    dF := dF / 1000 + Value.Data[i] / 1000;
  end;

  Res := dI + dF / 10;
  if Value.Negative then Res := -Res;

end;

function Float_IsZero(Value: Pv8Float): Boolean;
var
  i: Integer;
begin
  Result := True;
  if not Assigned(Value) then exit;
  if Value = nil then exit;
  for i := 0 to Value.Size - 1 do
  begin
    if Value.Data[i] > 0 then exit(false);
  end;
end;

function Float_Compare(V1, V2: Pv8Float): Integer;
var
  i, n: Integer;
begin
  if (Float_IsZero(V1)) and (Float_IsZero(V2)) then exit(0);
  if (Float_IsZero(V1) or (V1.Negative)) and (not Float_IsZero(V2) and (not V2.Negative)) then exit(1);
  if Float_IsZero(V1) and V2.Negative then exit(-1);
  if Float_IsZero(V2) and V1.Negative then exit(1);
  if (Float_IsZero(V2) or (V2.Negative)) and (not Float_IsZero(V1) and (not V1.Negative)) then exit(-1);
  if V1.Scale > V2.Scale then exit(-1);
  if V1.Scale < V2.Scale then exit(1);
  if V1.Scale = V2.Scale then
  begin
    if V2.Size > V1.Size then n := V2.Size
    else n := V1.Size;
    for i := 0 to n do
    begin
      if V1.Data[i] > V2.Data[i] then
      begin
        if V1.Negative and V2.Negative then exit(1)
        else exit(-1);
      end
      else if V1.Data[i] < V2.Data[i] then
      begin
        if V1.Negative and V2.Negative then exit(-1)
        else exit(1);
      end
      else continue;
    end;
    exit(0);
  end;
end;

function Float_CompareAbs(V1, V2: Pv8Float): Integer;
var
  i, n: Integer;
begin
  if V1.Size - V1.Scale > V2.Size - V2.Scale then exit(-1);
  if V1.Size - V1.Scale < V2.Size - V2.Scale then exit(1);
  if V1.Size - V1.Scale = V2.Size - V2.Scale then
  begin
    if V2.Size > V1.Size then n := V2.Size
    else n := V1.Size;
    for i := 0 to n do
    begin
      if V1.Data[i] > V2.Data[i] then exit(-1)
      else if V1.Data[i] < V2.Data[i] then exit(1)
      else continue;
    end;
    exit(0);
  end;
end;

function Float_AddAbs_OLD(V1, V2: Pv8Float; out Res: Pv8Float): Boolean;
var
  i, n, offset1, offset2, tempInt, offset, tmp1, tmp2, Delta, RStart, REnd: Integer;
  tempRes: array [0 .. 15] of Integer;
  incScale: Boolean;
begin
  Res := Float_AllocRaw(Max(V1.Size, V2.Size) + 1);
  for i := 0 to 15 do tempRes[i] := 0;
  n := Max(V1.Size, V2.Size);
  if V1.Size - V1.Scale >= V2.Size - V2.Scale then
  begin
    offset1 := (V1.Size - V1.Scale) - (V2.Size - V2.Scale);
    offset2 := 0;
  end
  else
  begin
    offset1 := 0;
    offset2 := (V2.Size - V2.Scale) - (V1.Size - V1.Scale);
  end;
  n := n + offset1 + offset2 - 1;
  tempInt := 0;
  for i := n downto -Max(offset1, offset2) do
  begin
    if (i + offset1 > V1.Size - 1) or (i + offset1 < 0) then tmp1 := 0
    else tmp1 := V1.Data[i + offset1];
    if (i + offset2 > V2.Size - 1) or (i + offset2 < 0) then tmp2 := 0
    else tmp2 := V2.Data[i + offset2];
    tempRes[i + 1] := (tmp1 + tmp2 + tempInt) mod 10000;
    tempInt := (tmp1 + tmp2 + tempInt) div 10000;
  end;

  incScale := false;
  if tempInt > 0 then
  begin
    tempRes[0] := tempInt;
    incScale := True;
  end
  else
  begin
    if tempRes[0] > 0 then
    begin
      incScale := True;
    end;
  end;

  for i := 0 to n do
  begin
    if incScale then
    begin
      Res.Data[i] := tempRes[i]
    end
    else
    begin
      Res.Data[i] := tempRes[i + 1]
    end;
  end;

  if incScale then
  begin
    Res.Size := Max(V1.Size, V2.Size) + 1;
    Res.Scale := Max(V1.Scale, V2.Scale);
  end
  else
  begin
    Res.Size := Max(V1.Size, V2.Size);
    Res.Scale := Max(V1.Scale, V2.Scale);
  end;
  for i := 15 downto 0 do
    if Res.Data[i] > 0 then
    begin
      Res.Size := i + 1;
      exit;
    end;

  for i := Res.Size downto Res.Size - Res.Scale do
  begin
    if Res.Data[i] = 0 then
    begin
      //Dec(Res.Size);
      Dec(Res.Scale);
      break;
    end;
  end;
end;

function Float_Diff_OLD(V1, V2: Pv8Float; out Res: Pv8Float): Boolean;
var
  i, n, j, offset1, offset2, tempInt: Integer;
  tempRes: array [0 .. 15] of Integer;
begin
  Res := Float_AllocRaw(Max(V1.Size, V2.Size) + 1);
  FillChar(tempRes[0], SizeOf(tempRes), 0);
  n := Max(V1.Size, V2.Size);

  if V1.Size - V1.Scale >= V2.Size - V2.Scale then
  begin
    offset1 := (V1.Size - V1.Scale) - (V2.Size - V2.Scale);
    offset2 := 0;
  end
  else
  begin
    offset1 := 0;
    offset2 := (V2.Size - V2.Scale) - (V1.Size - V1.Scale);
  end;
  n := n + offset1 + offset2 - 1;
  tempInt := 0;
  j := 1;
  for i := n downto 0 do
  begin
    if V2.Data[i - offset2] > V1.Data[i - offset1] then
    begin
      while V1.Data[i - offset1 - j] = 0 do
      begin
        V1.Data[i - offset1 - j] := 9999;
        Inc(j);
      end;
      Dec(V1.Data[i - offset1 - j]);
      tempRes[i] := 10000 + V1.Data[i - offset1] - V2.Data[i - offset2];
      j := 1;
    end
    else tempRes[i] := V1.Data[i - offset1] - V2.Data[i - offset2];

  end;
  for i := 0 to n do
  begin
    Res.Data[i] := tempRes[i];
  end;
  Res.Size := n + 1;
  Res.Scale := Max(V1.Scale, V2.Scale);
end;

function Float_Add(V1, V2: Pv8Float; out Res: Pv8Float): Boolean;
var
  i, n, cmp: Integer;
begin
  if V1.Negative = V2.Negative then
  begin
    Result := Float_AddAbs(V1, V2, Res);
    Res.Negative := V1.Negative;
    exit;
  end
  else
  begin
    cmp := Float_CompareAbs(V1, V2);
    case cmp of
      - 1:
        begin
          Float_Diff_Prep(V1, V2, Res);
          Res.Negative := V1.Negative;
        end;
      1:
        begin
          Float_Diff_Prep(V2, V1, Res);
          Res.Negative := V2.Negative;
        end;
      0:
        begin
          Float_Diff_Prep(V1, V2, Res);
          Res.Negative := false;
        end;
    end;
  end;
end;

function Float_Multiply(V1, V2: Pv8Float; out Res: Pv8Float): Boolean;
var
  i, j, k, m: Integer;
  Val: Integer;
begin
  if Float_IsZero(V1) or Float_IsZero(V2) then
  begin
    Res := nil;
    exit;
  end;

  Res := Float_AllocRaw(V1.Size + V2.Size);
  Res.Scale := V1.Scale + V2.Scale;
  Res.Negative := V1.Negative xor V2.Negative;

  for i := 0 to V1.Size - 1 do
    for j := 0 to V2.Size - 1 do Inc(Res.Data[i + j + 1], V1.Data[i] * V2.Data[j]);

  for i := Res.Size downto 1 do
  begin
    Val := Res.Data[i];
    if Val >= NUMBER_MASK then
    begin
      Res.Data[i - 1] := Res.Data[i - 1] + Val div NUMBER_MASK;
      Res.Data[i] := Val mod NUMBER_MASK;
    end;
  end;

  Float_Trim(Res);
end;

function Float_Division(V1, V2: Pv8Float; out Res: Pv8Float): Boolean;
var
  d1, d2: Double;
begin
  Result := True;
  if Float_IsZero(V2) then exit(false);
  if Float_IsZero(V1) then
  begin
    Res := nil;
    exit;
  end;
  Float_DumpToDouble(V1, d1);
  Float_DumpToDouble(V2, d2);
  Float_SetValueFromDouble(d1 / d2, Res);
  Res.Negative := V1.Negative xor V2.Negative;
end;

function Float_Modulo(V1, V2: Pv8Float; var Res: Pv8Float): Boolean;
var
  d1, d2: Double;
begin
  Result := True;
  if Float_IsZero(V2) then exit(false);
  if Float_IsZero(V1) then
  begin
    Res := Float_AllocRaw(V1.Size);
    Res.Data := V1.Data;
    Res.Size := V1.Size;
    Res.Scale := V1.Scale;
    Res.Negative := V1.Negative;
    exit;
  end;
  Float_DumpToDouble(V1, d1);
  Float_DumpToDouble(V2, d2);
  Float_SetValueFromDouble(d1 - Floor(d1 / d2) * d2, Res);
end;

function Float_AddAbs(V1, V2: Pv8Float; out Res: Pv8Float): Boolean;
var
  i, j, offset, Size: Integer;
  Val: Integer;
begin
  if (Float_IsZero(V1)) and (Float_IsZero(V2)) then
  begin
    Res := nil;
    exit;
  end;
  Size := Max((V1.Size - V1.Scale), (V2.Size - V2.Scale)) + Max(V1.Scale, V2.Scale);
  Res := Float_AllocRaw(Size + 1);
  Res.Scale := Max(V1.Scale, V2.Scale);

  offset := (V1.Size - V1.Scale) - (V2.Size - V2.Scale);

  if offset >= 0 then
  begin
    for i := 0 to Size + Abs(offset) - 1 do
    begin
      if i - Abs(offset) < 0 then Val := 0
      else Val := V2.Data[i - Abs(offset)];
      Inc(Res.Data[i + 1], V1.Data[i] + Val);
    end;
  end
  else
  begin
    for i := 0 to Size + Abs(offset) - 1 do
    begin
      if i - Abs(offset) < 0 then Val := 0
      else Val := V1.Data[i - Abs(offset)];
      Inc(Res.Data[i + 1], V2.Data[i] + Val);
    end;
  end;

  for j := i + 1 downto 1 do
  begin
    Val := Res.Data[j];
    if Val > NUMBER_MASK then
    begin
      //Inc(Res.Data[j - 1], Val mod NUMBER_MASK);
      //Res.Data[j] := Val div NUMBER_MASK;
      Inc(Res.Data[j - 1], Val div NUMBER_MASK);
      Res.Data[j] := Val mod NUMBER_MASK;
    end
    else if Val = NUMBER_MASK then
    begin
      Inc(Res.Data[j - 1]);
      Res.Data[j] := 0;
    end;
  end;

  while Res.Size - 1 > Res.Scale do
  begin
    if Res.Data[0] > 0 then break;
    Move(Res.Data[1], Res.Data[0], Res.Size * 4);
    Dec(Res.Size);
  end;
  while Res.Scale > 0 do
  begin
    if Res.Data[Res.Size - 1] > 0 then break;
    Dec(Res.Size);
    Dec(Res.Scale);
  end;
end;

function Float_Diff_Prep(V1, V2: Pv8Float; out Res: Pv8Float): Boolean;
var
  i, j, offset, Size: Integer;
  Val: Integer;
begin
  if (Float_IsZero(V1)) and (Float_IsZero(V2)) then
  begin
    Res := nil;
    exit;
  end;
  Size := Max((V1.Size - V1.Scale), (V2.Size - V2.Scale)) + Max(V1.Scale, V2.Scale);
  Res := Float_AllocRaw(Size);
  Res.Scale := Max(V1.Scale, V2.Scale);

  offset := (V1.Size - V1.Scale) - (V2.Size - V2.Scale);

  if offset >= 0 then
  begin
    for i := 0 to Size + Abs(offset) - 1 do
    begin
      if i - Abs(offset) < 0 then Val := 0
      else Val := V2.Data[i - Abs(offset)];
      if V1.Data[i] < Val then
      begin
        j := i;
        if j < 0 then exit(false);
        while Res.Data[j - 1] = 0 do
        begin
          Res.Data[j] := 9999;
          Dec(j);
        end;
        Dec(Res.Data[j - 1]);
        Res.Data[i] := V1.Data[i] - Val + NUMBER_MASK;
      end
      else Res.Data[i] := V1.Data[i] - Val;
    end;
  end
  else
  begin
    for i := 0 to Size + Abs(offset) - 1 do
    begin
      if i - Abs(offset) < 0 then Val := 0
      else Val := V1.Data[i - Abs(offset)];
      if V1.Data[i] < Val then
      begin
        j := i;
        if j < 0 then exit(false);
        while Res.Data[j - 1] = 0 do
        begin
          Res.Data[j] := 9999;
          Dec(j);
        end;
        Dec(Res.Data[j - 1]);
        Res.Data[i] := V2.Data[i] - Val + NUMBER_MASK;
      end
      else Res.Data[i] := V2.Data[i] - Val;
    end;
  end;

  for j := i + 1 downto 1 do
  begin
    Val := Res.Data[i];
    if Val >= NUMBER_MASK then
    begin
      Inc(Res.Data[i - 1], Val mod NUMBER_MASK);
      Res.Data[i] := Val div NUMBER_MASK;
    end;
  end;

  while Res.Size - 1 > Res.Scale do
  begin
    if Res.Data[0] > 0 then break;
    Move(Res.Data[1], Res.Data[0], Res.Size * 4);
    Dec(Res.Size);
  end;
  while Res.Scale > 0 do
  begin
    if Res.Data[Res.Size - 1] > 0 then break;
    Dec(Res.Size);
    Dec(Res.Scale);
  end;
end;

function Float_Diff(V1, V2: Pv8Float; out Res: Pv8Float): Boolean;
var
  i, n, cmp: Integer;
begin
  cmp := Float_CompareAbs(V1, V2);
  if not(V1.Negative) and V2.Negative then
  begin
    Result := Float_AddAbs(V1, V2, Res);
    Res.Negative := false;
    exit;
  end
  else if V1.Negative and V2.Negative then
  begin
    case cmp of
      - 1:
        begin
          Float_Diff_Prep(V1, V2, Res);
          Res.Negative := True;
        end;
      1:
        begin
          Float_Diff_Prep(V2, V1, Res);
          Res.Negative := false;
        end;
      0:
        begin
          Res := nil;
        end;
    end;
  end
  else if V1.Negative and not(V2.Negative) then
  begin
    case cmp of
      - 1:
        begin
          Float_AddAbs(V1, V2, Res);
          Res.Negative := True;
        end;
      1:
        begin
          Float_AddAbs(V2, V1, Res);
          Res.Negative := True;
        end;
      0:
        begin
          Float_AddAbs(V1, V2, Res);
          Res.Negative := True;
        end;
    end;
  end
  else if not(V1.Negative) and not(V2.Negative) then
  begin
    case cmp of
      - 1:
        begin
          Float_Diff_Prep(V1, V2, Res);
          Res.Negative := false;
        end;
      1:
        begin
          Float_Diff_Prep(V2, V1, Res);
          Res.Negative := True;
        end;
      0:
        begin
          Res := nil;
        end;
    end;
  end;
end;

function Float_Unique(Val: Pv8Float): Pv8Float;
begin
  if Val.Reference.RefCount = 1 then
  begin
    exit(Val);
  end
  else
  begin
    Result := Float_AllocRaw(Val.Size shl 2);
    Result.Size := Val.Size;
    Result.Scale := Val.Scale;
    Result.Data := Val.Data;
    Result.Negative := Val.Negative;
  end;
end;

function Float_Add_Int32(Val: Pv8Float; ValInt: Int32): Pv8Float;
var
  tmpValue: Pv8Float;
begin
  if ValInt = 0 then exit(Val);
  Float_SetValueFromInt32(ValInt, tmpValue);
  Float_Add(Val, tmpValue, Result);
  Float_Release(tmpValue);
end;

function Float_Add_Int64(Val: Pv8Float; ValInt: Int64): Pv8Float;
var
  tmpValue: Pv8Float;
begin
  if ValInt = 0 then exit(Val);
  Float_SetValueFromInt64(ValInt, tmpValue);
  Float_Add(Val, tmpValue, Result);
  Float_Release(tmpValue);
end;

function Float_TryAdd_Int32(Val: Pv8Float; ValInt: Int32): Boolean;
var
  tmpValue: Pv8Float;
begin
  Result := True;
  if Float_IsZero(Val) then
  begin
    if ValInt = 0 then exit(True)
    else exit(false);
  end
  else
  begin
    if ValInt = 0 then exit(True);
    tmpValue := Float_Add_Int32(Val, ValInt);
    if (tmpValue.Size shl 2) > Val.Reference.Opacity then
    begin
      Result := false;
    end;
  end;
  Float_Release(tmpValue);
end;

function Float_TryAdd_Int64(Val: Pv8Float; ValInt: Int64): Boolean;
var
  tmpValue: Pv8Float;
begin
  Result := True;
  if Float_IsZero(Val) then
  begin
    if ValInt = 0 then exit(True)
    else exit(false);
  end
  else
  begin
    if ValInt = 0 then exit(True);
    tmpValue := Float_Add_Int64(Val, ValInt);
    if (tmpValue.Size shl 2) > Val.Reference.Opacity then
    begin
      Result := false;
    end;
  end;
  Float_Release(tmpValue);
end;

function Float_Compare32(V1: Pv8Float; V2: Int32): Integer;
begin
  Result := Float_Compare64(V1, V2);
end;

function Float_Compare64(V1: Pv8Float; V2: Int64): Integer;
var count1, count2, tmp, i: Integer;
begin
  Result := 0;
  if Float_IsZero(V1) then
  begin
    if V2 = 0 then exit(0);
    if V2 > 0 then exit(-1);
    if V2 < 0 then exit(1);
  end;
  if V2 = 0 then
  begin
    if V1.Negative then exit(1)
    else exit(-1);
  end;
  if (V2 > 0) then
  begin
    if (V1.Negative) then exit(1);
  end
  else if V2 < 0 then
    if not V1.Negative then exit(-1);

  count1 := V1.Size - V1.Scale;
  count2 := 0;
  tmp := Abs(V2);
  while tmp mod NUMBER_MASK > 0 do
  begin
    Inc(count2);
    tmp := tmp div NUMBER_MASK;
  end;

  if count1 > count2 then Result := -1
  else if count1 < count2 then Result := 1
  else
  begin
    tmp := Abs(V2);
    i := count1 - 1;
    while tmp mod NUMBER_MASK > 0 do
    begin
      if (tmp mod NUMBER_MASK) > V1.Data[i] then Result := 1
      else if (tmp mod NUMBER_MASK) < V1.Data[i] then Result := -1;
      tmp := tmp div NUMBER_MASK;
      Dec(i);
    end;
  end;
  if Result = 0 then
    for i := count1 to V1.Size - 1 do
      if V1.Data[i] > 0 then Result := -1;
  if (V2 < 0) and V1.Negative then Result := Result * -1;
end;

initialization

FloatFS := FormatSettings;
FloatFS.DecimalSeparator := ',';
FloatFS.ThousandSeparator := char(160);

end.
