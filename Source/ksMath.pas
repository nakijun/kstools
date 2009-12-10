{ *********************************************************** }
{ *                    ksTools Library                      * }
{ *       Copyright (c) Sergey Kasandrov 1997, 2009         * }
{ *       -----------------------------------------         * }
{ *         http://sergworks.wordpress.com/kstools          * }
{ *********************************************************** }

unit ksMath;

interface

uses Math;

type
  PksComplex = ^TksComplex;
  TksComplex = packed record
    Re, Im: Extended;
    class operator Implicit(A: Extended): TksComplex;
    class operator Add(A, B: TksComplex): TksComplex;
    class operator Subtract(A, B: TksComplex): TksComplex;
    class operator Multiply(A, B: TksComplex): TksComplex;
    class operator Divide(A, B: TksComplex): TksComplex;
    class function Exp(Value: TksComplex): TksComplex; static;
  end;

procedure ComplexFFT(Data: Pointer; DataSize: Integer; Sign: Integer = 0);
procedure RealFFT(Data: Pointer; DataSize: Integer; Sign: Integer = 0);
procedure RealFFT2(Data1, Data2: Pointer; fft1, fft2: Pointer; DataSize: Integer);
procedure RealCorr(Data1, Data2: Pointer; Corr: Pointer; DataSize: Integer);
procedure RealAutoCorr(Data: Pointer; DataSize: Integer; Spectrum: Boolean = False);
function MaxPowerOfTwo(Value: Integer): Integer;
function RFTSpectrum(Data: Pointer; DataSize, Index: Integer): TksComplex;
function RFTAmplitude(Data: Pointer; DataSize, Index: Integer): Extended;
function RFTPhase(Data: Pointer; DataSize, Index: Integer): Extended;

implementation

class operator TksComplex.Implicit(A: Extended): TksComplex;
begin
  Result.Re:= A;
  Result.Im:= 0;
end;

class operator TksComplex.Add(A, B: TksComplex): TksComplex;
begin
  Result.Re:= A.Re + B.Re;
  Result.Im:= A.Im + B.Im;
end;

class operator TksComplex.Subtract(A, B: TksComplex): TksComplex;
begin
  Result.Re:= A.Re + B.Re;
  Result.Im:= A.Im + B.Im;
end;

class operator TksComplex.Multiply(A, B: TksComplex): TksComplex;
begin
  Result.Re:= A.Re * B.Re - A.Im * B.Im;
  Result.Im:= A.Re * B.Im + A.Im * B.Re;
end;

class operator TksComplex.Divide(A, B: TksComplex): TksComplex;
var
  D: Extended;

begin
  D:= Sqr(B.Re) + Sqr(B.Im);
  Result.Re:= (A.Re * B.Re + A.Im * B.Im) / D;
  Result.Im:= (A.Im * B.Re - A.Re * B.Im) / D;
end;

type
  PCArray = ^TCArray;
  TCArray = array[0..$FFFFFF] of TksComplex;

  PDArray = ^TDArray;
  TDArray = array[0..$FFFFFF] of Extended;

class function TksComplex.Exp(Value: TksComplex): TksComplex;
var
  A: Extended;

begin
  A:= System.Exp(Value.Re);
  Result.Re:= A * Cos(Value.Im);
  Result.Im:= A * Sin(Value.Im);
end;

(* ************************************************************************ *)

procedure ComplexFFT(Data: Pointer; DataSize: Integer; Sign: Integer = 0);
var
  I, J, L, M, Step: Integer;
  Tmp: TksComplex;
  P1, P2: PksComplex;
  Theta, WT, TR, TI: Extended;
  WPR, WPI, WR, WI: Extended;

begin
                    // Бит - реверсивная перестановка исходных данных
  I:= 0;
  J:= 0;
  while (I < DataSize - 1) do begin
    if (J > I) then begin
      Tmp:= PCArray(Data)^[I];        // перестановка комплексных чисел
      PCArray(Data)^[I]:= PCArray(Data)^[J];
      PCArray(Data)^[J]:= Tmp;
    end;
    M:= DataSize shr 1;
    while (M >= 1) and (J >= M) do begin
      Dec(J, M);
      M:= M shr 1;
    end;
    Inc(J, M);
    Inc(I);
  end;
  L:= 1;
  while L < DataSize do begin
    Step:= L shl 1;
    Theta:= Pi/L;
    if Sign < 0 then Theta:= -Theta;

    WT:= Sin(0.5 * Theta);
    WPR:= -2.0 * WT * WT;                 // = Cos(Theta) - 1;
    WPI:= Sin(Theta);
    WR:= 1.0;
    WI:= 0.0;
    M:= 0;
    while M < L do begin
      I:= M;
      while I < DataSize do begin
        P2:= @PCArray(Data)^[I + L];      // умножаем 2-ю точку
        TR:= WR * P2^.Re - WI * P2^.Im;   //  на поворачивающий множитель
        TI:= WR * P2^.Im + WI * P2^.Re;
        P1:= @PCArray(Data)^[I];
        P2^.Re:= P1^.Re - TR;             // 2-я точка
        P2^.Im:= P1^.Im - TI;
        P1^.Re:= P1^.Re + TR;             // 1-я точка
        P1^.Im:= P1^.Im + TI;
        Inc(I, Step);
      end;
      WT:= WR;
      WR:= WR * WPR - WI * WPI + WR;
      WI:= WI * WPR + WT * WPI + WI;
      Inc(M);
    end;
    L:= Step;
  end;
end;

procedure RealFFT(Data: Pointer; DataSize: Integer; Sign: Integer = 0);
var
  Theta: Extended;
  C1, C2: Extended;
  P1, P2: PksComplex;
  H1, H2: TksComplex;
  WT, WPR, WPI, WR, WI: Extended;
  I: Integer;

begin
  DataSize:= DataSize shr 1;
  Theta:= Pi / DataSize;
  C1:= 0.5;
  if Sign < 0 then begin
    C2:= 0.5;
    Theta:= - Theta;
  end
  else begin
    C2:= -0.5;
    ComplexFFT(Data, DataSize, Sign);
  end;

// W = exp(pi * i * theta) = WR + i*WI

  WT:= Sin(0.5 * Theta);
  WPR:= -2.0 * WT * WT;                 // = Cos(Theta) - 1;
  WPI:= Sin(Theta);
  WR:= 1.0 + WPR;                       // = cos(theta)
  WI:= WPI;                             // = sin(theta)
  I:= 1;
  while I < DataSize shr 1 do begin     // case I=0 done separately
    P1:= @PCArray(Data)^[I];
    P2:= @PCArray(Data)^[DataSize - I];

    H1.Re:= C1 * (P1^.Re + P2^.Re);
    H1.Im:= C1 * (P1^.Im - P2^.Im);
    H2.Re:= -C2 * (P1^.Im + P2^.Im);
    H2.Im:= C2 * (P1^.Re - P2^.Re);
    P1^.Re:= H1.Re + WR * H2.Re - WI * H2.Im;
    P1^.Im:= H1.Im + WR * H2.Im + WI * H2.Re;
    P2^.Re:= H1.Re - WR * H2.Re + WI * H2.Im;
    P2^.Im:= -H1.Im + WR * H2.Im + WI * H2.Re;

    WT:= WR;
    WR:= WR + WR * WPR - WI * WPI;
    WI:= WI + WI * WPR + WT * WPI;
    Inc(I);
  end;
  P1:= @PCArray(Data)^[0];
  H1.Re:= P1^.Re;
  P1^.Re:= P1^.Re + P1^.Im;
  P1^.Im:= H1.Re - P1^.Im;
  if Sign < 0 then begin
    P1^.Re:= C1 * P1^.Re;
    P1^.Im:= C1 * P1^.Im;
    ComplexFFT(Data, DataSize, Sign);
  end;
end;

procedure RealFFT2(Data1, Data2: Pointer; fft1, fft2: Pointer; DataSize: Integer);
var
  I: Integer;
  P, M: TksComplex;
  P1, P2: PksComplex;

begin
  for I:= 0 to DataSize - 1 do begin
    with PCArray(fft1)^[I] do begin
      Re:= PDArray(Data1)^[I];
      Im:= PDArray(Data2)^[I];
    end;
  end;
  ComplexFFT(fft1, DataSize, 0);
  PCArray(fft2)^[0].Re:= PCArray(fft1)^[0].Im;
  PCArray(fft1)^[0].Im:= 0;
  PCArray(fft2)^[0].Im:= 0;
  for I:= 1 to (DataSize shr 1) do begin
    P1:= @PCArray(fft1)^[I];
    P2:= @PCArray(fft1)^[DataSize - I];
    P.Re:= 0.5 * (P1^.Re + P2^.Re);
    P.Im:= 0.5 * (P1^.Im + P2^.Im);
    M.Re:= 0.5 * (P1^.Re - P2^.Re);
    M.Im:= 0.5 * (P1^.Im - P2^.Im);
    with P1^ do begin
      Re:= P.Re;
      Im:= M.Im;
    end;
    with P2^ do begin       // комплесно сопр. P1^
      Re:= P.Re;
      Im:= -M.Im;
    end;
    with PCArray(fft2)^[I] do begin
      Re:= P.Im;
      Im:= -M.Re;
    end;
    with PCArray(fft2)^[DataSize - I] do begin
      Re:= P.Im;
      Im:= M.Re;
    end;
  end;
end;

procedure RealCorr(Data1, Data2: Pointer; Corr: Pointer; DataSize: Integer);
var
  fft1: Pointer;
  I: Integer;
  C1, C2: TksComplex;
  P1, P2, P3: PksComplex;
  D: Extended;

begin
  GetMem(fft1, DataSize * SizeOf(TksComplex));
  try
    for I:= 0 to DataSize - 1 do begin
      with PCArray(fft1)^[I] do begin
        Re:= PDArray(Data1)^[I];
        Im:= PDArray(Data2)^[I];
      end;
    end;
    ComplexFFT(fft1, DataSize, 0);

    D:= 1 / (DataSize shr 1);         // нормализующий множитель

// умножаем фурье-образ первой функции
//   на комплесно-сопряжённый фурье-образ второй функции

    P1:= @PCArray(fft1)^[0];
    P2:= @PCArray(fft1)^[DataSize];   // out of bounds - that's OK
    P3:= @PCArray(Corr)^[0];

    P3^.Re:= P1^.Re * P1^.Im * D;

    for I:= 1 to (DataSize shr 1) - 1 do begin
      Inc(P1);
      Dec(P2);                    // decremented - not out of bounds
      Inc(P3);

// значение первой функции
      C1.Re:= 0.5 * (P1^.Re + P2^.Re);
      C1.Im:= 0.5 * (P1^.Im - P2^.Im);

// комплексно сопряжённое значение второй функции
      C2.Re:= 0.5 * (P1^.Im + P2^.Im);
      C2.Im:= 0.5 * (P1^.Re - P2^.Re);

// первое умножается на второе:
//   P3^:= (C1.Re, C1.Im) * (C2.Re, C2.Im);

      P3^.Re:= (C1.Re * C2.Re - C1.Im * C2.Im) * D;
      P3^.Im:= (C1.Re * C2.Im + C1.Im * C2.Re) * D;
    end;

    Inc(P1);
    P3:= @PCArray(Corr)^[0];
    P3^.Im:= P1^.Re * P1^.Im * D;

    RealFFT(Corr, DataSize, -1);

  finally
    FreeMem(fft1);
  end;
end;

procedure RealAutoCorr(Data: Pointer; DataSize: Integer; Spectrum: Boolean = False);
var
  I: Integer;
  PC: PksComplex;

begin
  if not Spectrum then RealFFT(Data, DataSize, 0);
  DataSize:= DataSize shr 1;
  PC:= Data;
  PC^.Re:= Sqr(PC^.Re) / DataSize;
  PC^.Im:= Sqr(PC^.Im) / DataSize;
  I:= DataSize - 1;
  while I > 0 do begin
    Inc(PC);
    PC^.Re:= (Sqr(PC^.Re) + Sqr(PC^.Im)) / DataSize;
    PC^.Im:= 0;
    Dec(I);
  end;
  RealFFT(Data, DataSize shl 1, -1);
end;

function MaxPowerOfTwo(Value: Integer): Integer;
begin
  Result:= 1;
  repeat
    Result:= Result shl 1;
  until (Result > Value) or (Result < 0);
  Result:= Result shr 1;
end;

function RFTSpectrum(Data: Pointer; DataSize, Index: Integer): TksComplex;
var
  SecondHalf: Boolean;

begin
  if Index = 0 then Result:= PExtended(Data)^
  else if Index = DataSize shr 1 then Result:= PDArray(Data)^[1]
  else if Index < DataSize then begin
    SecondHalf:= Index > DataSize shr 1;
    if SecondHalf then
      Index:= DataSize - Index;
    Result:= PCArray(Data)^[Index];
    if not SecondHalf then
      Result.Im:= - Result.Im;
  end
  else Result:= 0;
end;

function RFTAmplitude(Data: Pointer; DataSize, Index: Integer): Extended;
var
  C: TksComplex;

begin
  C:= RFTSpectrum(Data, DataSize, Index);
  Result:= Sqrt(Sqr(C.Re) + Sqr(C.Im));
end;

function RFTPhase(Data: Pointer; DataSize, Index: Integer): Extended;
var
  C: TksComplex;

begin
  C:= RFTSpectrum(Data, DataSize, Index);
  if C.Re = 0 then begin
    if C.Im > 0 then Result:= Pi/2
    else if C.Im < 0 then Result:= -Pi/2
    else Result:= 0;
  end
  else Result:= Math.ArcTan2(C.Im, C.Re);
end;

end.
