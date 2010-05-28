{ *********************************************************** }
{ *                    ksTools Library                      * }
{ *       Copyright (c) Sergey Kasandrov 1997, 2010         * }
{ *       -----------------------------------------         * }
{ *         http://sergworks.wordpress.com/kstools          * }
{ *********************************************************** }

unit ksFlateHuffman;

interface

uses
  SysUtils, ksUtils;

{ Huffman encoder for Zip Deflate compression algorithm }

type
  TksFlateHuffEncoder = record
  private const
    MaxCodeSize = 32;
    MaxNumCodes = 320;
    Capacity = 2 * MaxNumCodes + 1;
  public type
    PHuffmanCode = ^THuffmanCode;
    THuffmanCode = packed record
      case Byte of
        0: (Freq, Parent: LongInt);  {frequency count & parent node}
        1: (Code: LongWord;          {bit string & length of bit string}
            Len: LongInt);
    end;

{ !!! Reserve twice as needed space for Huffman codes;
      it is required for Huffman tree }
    PHuffmanCodes = ^THuffmanCodes;
    THuffmanCodes = packed array[0..65535 div SizeOf(THuffmanCode)] of THuffmanCode;

  private
    FCodes: PHuffmanCodes;       { the Huffman codes (tree) }
    FDepth: packed array[0..Capacity - 1] of Byte;
    FHeap: packed array[0..Capacity - 1] of Word;
                                 { The sons of Heap[n] are Heap[2*n]
                                   and Heap[2*n+1]. Heap[0] is not used. }

    FFixedCodes: PHuffmanCodes;  { corresponding fixed codes or NULL }
    FExtraBits: PWordArray;      { extra bits for each code or NULL }
    FExtraBase: Integer;         { base index for ExtraBits }
    FFixedLen: Integer;          { corresponding fixed code length }

    FNumCodes: Integer;
    FMaxBits: Integer;
    FMaxCode: Integer;           { largest code with non zero frequency }
    FHeapLen: Integer;           { Number of elements in the heap }
    FHeapMax: Integer;           { Element of largest frequency }
    FHeapSize: Integer;
    FBitLenCount: packed array[0..MaxCodeSize] of Word;
    FCodeLen: Integer;
    procedure InternalGenerateCodes;
    function Smaller(N, M: Integer): Boolean;
    procedure Sift(L: Integer);
    procedure GenerateBitLengths;
    function GetCodeLen(N, Bits: Integer): Integer;
    function GetExtraLen(Node: Integer): Integer;
  public
    procedure BuildCodes(ACodes: PHuffmanCodes;
                         ANumCodes, AMaxBits: Integer);
    procedure GenerateCodes(ACodes: PHuffmanCodes; AMaxCode, AMaxBits: Integer);

    property Codes: PHuffmanCodes read FCodes;
    property MaxCode: Integer read FMaxCode;
    property CodeLen: Integer read FCodeLen;

    property FixedCodes: PHuffmanCodes read FFixedCodes write FFixedCodes;
    property ExtraBits: PWordArray read FExtraBits write FExtraBits;
    property ExtraBase: Integer read FExtraBase write FExtraBase;
    property FixedLen: Integer read FFixedLen;
  end;

{ Huffman decoder for Zip Inflate decompression algorithm }

type
  TksFlateHuffDecoder = record
  public const
                                 // Values returned by Init function
    VALID_DATA = 0;              //  All is OK
    INVALID_DATA = -1;           //  Can not build decoding tree,
                                 //    decoding impossible
    INCOMPLETE_DATA = 1;         //  Incomplete data detected,
                                 //    decoding tree is built
                                 //    and decoding is possible

  private const
    MaxCodeSize = 16;            // Max Code Size = 16 bits
//    MaxCodeBits = 15;            { All code lengths must not
//                                     exceed MaxCodeBits + 1 }
    MaxNumCodes = 320;
    InvalidHuffmanCode = 99;

  private type                   { Node of decoding tree }
    PHuff = ^THuff;
    THuff = packed record
      ExtraBits: Byte;           { Size of next level table }
      case Byte of
        0: (Value: Cardinal);    { Huffman code, ExtraBits = 0 }
        1: (NextLevel: PHuff);   { Pointer to next level table }
    end;

  private
    FTree: Pointer;
    FTreeSize: Integer;
    FLookupBits: Integer;
    FGetBits: TksGetBits;
//    procedure AddCodes(Code: LongWord; CodeLen: Integer;
//      Table: Pointer; TableSize: Integer; Target: TStringList);
  public
    function Init(AGetBits: TksGetBits; CodeLength: PWord; //CodeLength: PCardinal;
                    NumCodes: Integer; AllowIncomplete: Boolean): Integer;
    procedure Done;
//    procedure ListCodes(Target: TStringList);
    function Decode: Cardinal;
  end;

implementation

{ TksFlateHuffEncoder }

function TksFlateHuffEncoder.Smaller(N, M: Integer): Boolean;
{ Compares two subtrees, using the tree depth as tie breaker when the
  subtrees have equal frequency. This minimizes the worst case length }
var
  NFreq, MFreq: Cardinal;

begin
  NFreq:= FCodes^[N].Freq;
  MFreq:= FCodes^[M].Freq;
  Result:= (NFreq < MFreq) or
    ((NFreq = MFreq) and (FDepth[N] <= FDepth[M]));
end;

procedure TksFlateHuffEncoder.Sift(L: Integer);
var
  V, J: Integer;

begin
  J:= L shl 1;
  V:= FHeap[L];
  while J <= FHeapLen do begin
    if (J < FHeapLen) and Smaller(FHeap[J+1], FHeap[J]) then Inc(J);
    if Smaller(V, FHeap[J]) then Break;
    FHeap[L]:= FHeap[J];
    L:= J;
    J:= J shl 1;
  end;
  FHeap[L]:= V;
end;

procedure TksFlateHuffEncoder.InternalGenerateCodes;
var
  Code: LongWord;       {Running code value}
  Bits: Integer;        {Bit index}
  N: Integer;           {Code index}
  L: Integer;           {Current code length}
  {Next code value for each bit length}
  NextCode: array[0..MaxCodeSize] of LongWord;   // 0 not used

begin
  Code:= 0;
{ The distribution counts are used to generate the code values }
  for Bits:= 1 to FMaxBits do begin
    Code:= (Code + FBitLenCount[Bits-1]) shl 1;
    NextCode[Bits]:= Code;  {without bit reversal}
  end;

{ Generate the codes}
  for N:= 0 to FMaxCode do begin
    L:= FCodes^[N].Len;
    if L <> 0 then begin {Reverse the bits}
      FCodes^[N].Code:= SwapBits16(NextCode[L], L);
      Inc(NextCode[L]);
    end;
  end;
end;

procedure TksFlateHuffEncoder.GenerateCodes(ACodes: PHuffmanCodes;
  AMaxCode, AMaxBits: Integer);
var
  I: Integer;

begin
  FCodes:= ACodes;
  FMaxCode:= AMaxCode;
  FMaxBits:= AMaxBits;
  FillChar(FBitLenCount, SizeOf(FBitLenCount), 0);
  for I:= 0 to FMaxCode do Inc(FBitLenCount[FCodes^[I].Len]);
  InternalGenerateCodes;
end;

procedure TksFlateHuffEncoder.BuildCodes(ACodes: PHuffmanCodes;
  ANumCodes, AMaxBits: Integer);
var
  N, M: Integer;
  Node, NewNode: Integer;

begin
  FFixedLen:= 0;

  FCodes:= ACodes;
  FNumCodes:= ANumCodes;
  FMaxBits:= AMaxBits;
  FMaxCode:= -1;
  FCodeLen:= 0;

  Node:= FNumCodes;

{ Fill the heap array; the heap will be built later on, FHeap[0] is not used }
  FHeapLen:= 0;
  FHeapSize:= 2 * FNumCodes + 1;
  FHeapMax:= FHeapSize;
  for N:= 0 to FNumCodes - 1 do begin
    if (FCodes^[N].Freq <> 0) then begin
      Inc(FHeapLen);
      FMaxCode:= N;
      FHeap[FHeapLen]:= N;
      FDepth[N]:= 0;
    end
    else FCodes^[N].Len:= 0;
  end;

{ force at least two codes of non zero frequency }
  while FHeapLen < 2 do begin
    Inc(FHeapLen);
    if FMaxCode < 2 then begin
      Inc(FMaxCode);
      FHeap[FHeapLen]:= FMaxCode;
      NewNode:= FMaxCode;
    end
    else begin
      FHeap[FHeapLen]:= 0;
      NewNode:= 0;
    end;
    FCodes^[NewNode].Freq:= 1;
    FDepth[NewNode]:= 0;
    FCodeLen:= FCodeLen - 1 - GetExtraLen(NewNode);
  end;

{ Build the heap; least frequent element is FHeap[1], FHeap[0] is not used }
  for N:= FHeapLen div 2 downto 1 do Sift(N);

{ Construct the Huffman tree by repeatedly combining the least two
  frequent nodes }
  repeat
    N:= FHeap[1];  {n = node of least frequency}
{ Remove the least frequent element from the heap and
  rebuild the heap with one less element }
    FHeap[1]:= FHeap[FHeapLen];
    Dec(FHeapLen);
    Sift(1);

    M:= FHeap[1];  {m = node of next least frequency}

    Dec(FHeapMax);
    FHeap[FHeapMax]:= N; {keep the nodes sorted by frequency}
    Dec(FHeapMax);
    FHeap[FHeapMax]:= M;

{ Create a new node father of n and m }
    FCodes^[Node].Freq:= FCodes^[N].Freq + FCodes^[M].Freq;
    FDepth[Node]:= MaxByteOf(FDepth[N], FDepth[M]) + 1;
    FCodes^[N].Parent:= Node;
    FCodes^[M].Parent:= Node;

{   and insert the new node in the heap }
    FHeap[1]:= Node;
    Inc(Node);
    Sift(1);
  until FHeapLen < 2;

  Dec(FHeapMax);
  FHeap[FHeapMax]:= FHeap[1];

{ At this point, Freq and Parent are set. We can generate the bit lengths }
  GenerateBitLengths;

{ Len is now set, we can generate the bit codes }
  InternalGenerateCodes;
end;

procedure TksFlateHuffEncoder.GenerateBitLengths;
{ Compute the optimal bit lengths for a tree and update the total bit
  length for the current block }
var
  H: Integer;           { heap index }
  N, M: Integer;        { iterate over the tree elements }
  Bits: Integer;        { bit length }
  Overflow: Integer;    { number of elements with bit length too large }

begin
  Overflow:= 0;
  FillChar(FBitLenCount, SizeOf(FBitLenCount), 0);

{ In a first pass, compute the optimal bit lengths }
  FCodes^[FHeap[FHeapMax]].Len:= 0;    {root of the heap}

  for H:= FHeapMax+1 to FHeapSize-1 do begin
    N:= FHeap[H];
    Bits:= FCodes^[FCodes^[N].Parent].Len + 1;
    if Bits > FMaxBits then begin
      Bits:= FMaxBits;
      Inc(Overflow);
    end;
    FCodes^[N].Len:= Bits;
{ Overwrite FCodes^[N].Parent which is no longer needed}

    if N <= FMaxCode then begin {a leaf node}
      Inc(FBitLenCount[Bits]);
      FCodeLen:= FCodeLen + GetCodeLen(N, Bits);
    end;
  end;

  if Overflow = 0 then Exit;

{ Find the first bit length which could increase }
  repeat
    Bits:= FMaxBits - 1;
    while FBitLenCount[Bits] = 0 do Dec(Bits);
    Dec(FBitLenCount[Bits]);        {move one leaf down the tree}
    Inc(FBitLenCount[Bits + 1], 2); {move one overflow item as its brother}
    Dec(FBitLenCount[FMaxBits]);
{ The brother of the overflow item also moves one step up,
  but this does not affect BitLenCount[MaxBits]}
    Dec(Overflow, 2);
  until Overflow <= 0;

{ Now recompute all bit lengths, scanning in increasing frequency.
  (It is simpler to reconstruct all lengths instead of fixing only
  the wrong ones }
  H:= FHeapSize;
  for Bits:= FMaxBits downto 1 do begin
    N:= FBitLenCount[Bits];
    while N <> 0 do begin
      Dec(H);
      M:= FHeap[H];
      if M <= FMaxCode then begin
        if FCodes^[M].Len <> Bits then begin
          FCodeLen:= FCodeLen + (Bits - FCodes^[M].Len) * FCodes^[M].Freq;
          FCodes^[M].Len:= Bits;
        end;
        Dec(N);
      end;
    end;
  end;
end;

function TksFlateHuffEncoder.GetCodeLen(N, Bits: Integer): Integer;
var
  XBits: Integer;       { extra bits }
  Freq: Cardinal;       { frequency }

begin
  XBits:= 0;
  if (FExtraBits <> nil) and (N >= FExtraBase) then begin
    XBits:= FExtraBits^[N - FExtraBase];
  end;

  Freq:= Codes^[N].Freq;
  Result:= LongInt(Freq) * (Bits + XBits);

  if FFixedCodes <> nil then begin
    FFixedLen:= FFixedLen + LongInt(Freq) *
      (LongInt(FFixedCodes^[n].Len) + XBits);
  end;
end;

function TksFlateHuffEncoder.GetExtraLen(Node: Integer): Integer;
var
  N: Integer;

begin
  if FFixedCodes <> nil then begin
    FFixedLen:= FFixedLen - FFixedCodes^[Node].Len;
  end;

  if (FExtraBits <> nil) and (Node >= FExtraBase) then begin
    N:= FExtraBits^[Node - FExtraBase];
    Result:= N;
    if FFixedCodes <> nil then begin
      FFixedLen:= FFixedLen - N;
    end
  end
  else begin
    Result:= 0;
  end;
end;

{ TksFlateHuffDecoder }

procedure TksFlateHuffDecoder.Done;
begin
  if FTreeSize > 0 then begin
    FreeMem(FTree, FTreeSize * SizeOf(THuff));
    FTreeSize:= 0;
  end;
end;

{
procedure TksSHuffDecoder.AddCodes(Code: LongWord; CodeLen: Integer;
  Table: Pointer; TableSize: Integer; Target: TStringList);
var
  I: Integer;

begin
  for I:= 0 to (1 shl TableSize) - 1 do begin
    if PHuff(Table).ExtraBits = 0 then
      Target.Add(IntToStr(PHuff(Table).Value) + '='
        + IntToBin(SwapBits(Code, CodeLen + TableSize), CodeLen + TableSize))
    else if PHuff(Table).ExtraBits = InvalidHuffmanCode then
      Target.Add('<InvalidCode>='
        + IntToBin(Code, CodeLen + TableSize))
    else AddCodes(Code, CodeLen + TableSize, PHuff(Table).NextLevel,
           PHuff(Table).ExtraBits, Target);
    Inc(PHuff(Table));
    Inc(Code, 1 shl CodeLen);
  end;
end;

procedure TksSHuffDecoder.ListCodes(Target: TStringList);
begin
  AddCodes(0, 0, FTree, FLookupBits, Target);
end;
}

function TksFlateHuffDecoder.Init(AGetBits: TksGetBits; CodeLength: PWord;
  NumCodes: Integer; AllowIncomplete: Boolean): Integer;
  {Given a list of code lengths and a maximum table size,
   make a set of tables to decode that set of codes.}

  {CodeLength: code lengths in bits (all assumed <= MaxCodeBits)}
  {NumCodes:   number of codes (assumed <= MaxNumCodes)}

var
  MinCodeLen: Integer;  { minimum or current code length }
  MaxCodeLen: Integer;  { maximum code length }
  TP, NP: PHuff;        { pointers to FTree nodes }
  I, J: Integer;        { general counters }
//  K: Integer;           { general counters }
  P, XP: PWord;         { general array pointers }
  TmpWord: Word;
//  P, XP: PCardinal;     { general array pointers }

// Bit Length Count table; containes MaxCodeBits + 1 entries
//   the 0-th entry is included since some symbols may have zero code length,
//   i.e. have no corresponding Huffman Code at all;
//   the 0-th entry is actually not required and can be removed,
//   but preserved here since simplifies the code.
  BitLenCount: array[0..MaxCodeSize] of Word;

// Huffman Code's Values
  BitValues: array[0..MaxNumCodes - 1] of Word;

// 0-th entry is not used
  BitOffsets: array[0..MaxCodeSize] of Word;

// 0-th entry is not used
  Tables: packed array[0..MaxCodeSize] of Integer; { indice of tree levels }
  TH: THuff;

begin
  FGetBits:= AGetBits;
  FTreeSize:= 0;

{ Generate counts for each bit length }
  FillChar(BitLenCount, SizeOf(BitLenCount), 0);
  P:= CodeLength;
  I:= NumCodes;
  repeat
    TmpWord:= P^;
    if TmpWord <= MaxCodeSize then begin
      Inc(BitLenCount[TmpWord]);
    end
    else begin
      Result:= INVALID_DATA;
      Exit;
    end;
    Inc(P);
    Dec(I);
  until (I = 0);

{ Find minimum and maximum length }
  MinCodeLen:= 1;
  while (BitLenCount[MinCodeLen] = 0) and (MinCodeLen < MaxCodeSize) do begin
    Inc(MinCodeLen);
  end;

  FLookupBits:= MinCodeLen;

  MaxCodeLen:= MaxCodeSize;
  while (BitLenCount[MaxCodeLen] = 0) and (MaxCodeLen > MinCodeLen) do begin
    Dec(MaxCodeLen);
  end;

{ Adjust last length count to fill out codes, if needed }
  I:= MinCodeLen;
  J:= 1 shl (I - 1);
  repeat
    J:= (J shl 1) - Integer(BitLenCount[I]);
    if (J < 0) then begin
      Result:= INVALID_DATA;
      Exit;
    end;
    Inc(I);
  until I > MaxCodeLen;
  BitLenCount[MaxCodeLen]:= BitLenCount[MaxCodeLen] + J;

  if (J <> 0) then begin
    if AllowIncomplete then begin
      Result:= INCOMPLETE_DATA;
    end
    else begin
      Result:= INVALID_DATA;
      Exit;
    end;
  end
  else begin
    Result:= VALID_DATA;
  end;

{ Generate starting offsets into the value table for each length }
  I:= 0;
  P:= @BitLenCount[1];
  BitOffsets[1]:= 0;      // Offset for 1-bit codes is zero
  XP:= @BitOffsets[2];

  J:= MaxCodeLen - 1;
  while J > 0 do begin
    Dec(J);
    I:= I + Integer(P^);
    XP^:= I;
    Inc(P);
    Inc(XP);
  end;

{ Make a table of values in order of bit lengths }
  P:= CodeLength;
  I:= 0;
  repeat
    J:= P^;
    Inc(P);
    if J <> 0 then begin
      BitValues[BitOffsets[J]]:= I;
      Inc(BitOffsets[J]);
    end;
    Inc(I);
  until (I >= NumCodes);

{ Compute decoding tree size }
  I:= MinCodeLen;
  J:= 1 shl (I - 1);
  while I <= MaxCodeLen do begin
    Tables[I]:= FTreeSize;
    J:= J shl 1;
//    if BitLenCount[I] > 0 then begin
      Inc(FTreeSize, J);
      Dec(J, BitLenCount[I]);
//    end;
    Inc(I);
  end;
  GetMem(FTree, FTreeSize * SizeOf(THuff));

{ Go through the bit lengths }
  P:= @BitValues;                {Grab values in bit order}
  TP:= FTree;
  J:= 1 shl MinCodeLen;
  while MinCodeLen <= MaxCodeLen do begin

{ avoid array bounds error if MinCodeLen = MaxCodeLen }
    NP:= FTree;
    if MinCodeLen < MaxCodeLen then
      Inc(NP, Tables[MinCodeLen+1]);
{
    K:= 0;
    if MinCodeLen < MaxCodeLen then begin
      while BitLenCount[MinCodeLen + K] = 0 do Inc(K);
    end;
    Inc(K);
}
    I:= 0;
    repeat
      if I < BitLenCount[MinCodeLen] then begin
        if Cardinal(P) >= Cardinal(@BitValues) +
           Cardinal(NumCodes) * SizeOf(Cardinal)
        then TP^.ExtraBits:= InvalidHuffmanCode    { Out of values }
        else begin
          TP^.ExtraBits:= 0;
          TP^.Value:= P^;                   { Simple code is just the value }
          Inc(P);
        end;
      end
      else begin
        TP^.ExtraBits:= 1;
//        TP^.ExtraBits:= K;         // !!
        TP^.NextLevel:= NP;
        Inc(NP, 2);                         // = 1 shl TP^.ExtraBits
//        Inc(NP, 1 shl K);                         // = 1 shl TP^.ExtraBits
      end;
      Inc(TP);
      Inc(I);
    until I = J;
//  end;
    J:= (J - BitLenCount[MinCodeLen]) shl 1;
    Inc(MinCodeLen);
  end;

  MinCodeLen:= FLookupBits;

//while MinCodeLen <= MaxCodeLen do begin

  for I:= 0 to (1 shl MinCodeLen) - 1 do begin
    J:= SwapBits16(I, MinCodeLen);
    if I < J then begin
      TP:= FTree;
      Inc(TP, I);
      NP:= FTree;
      Inc(NP, J);
      TH:= TP^;
      TP^:= NP^;
      NP^:= TH;
    end;
  end;
  Result:= 0;
//  Inc(MinCodeLen);
//end;
end;

function TksFlateHuffDecoder.Decode: Cardinal;
var
  E: Cardinal;     {Table entry flag/number of extra bits}
  P: PHuff;        {Pointer to table entry}

begin
  P:= FTree;
  Inc(P, FGetBits(FLookupBits));
  repeat
    E:= P^.ExtraBits;
    if E = 0 then Break;
    if E = InvalidHuffmanCode then
      raise Exception.Create('Invalid Huffman Code');
    P:= P^.NextLevel;
    Inc(P, FGetBits(E));
  until False;
  Result:= P^.Value;
end;

end.
