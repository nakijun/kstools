{ *********************************************************** }
{ *                    ksTools Library                      * }
{ *       Copyright (c) Sergey Kasandrov 1997, 2010         * }
{ *       -----------------------------------------         * }
{ *         http://sergworks.wordpress.com/kstools          * }
{ *********************************************************** }

unit ksCompress;

interface

uses SysUtils, Classes, ksUtils, ksClasses, ksShrink, ksFlateHuffman;

{ Shrink Algorithm }

function ShrinkBytes(const Source: TBytes; Count: LongWord = 0): TBytes;
function UnshrinkBytes(const Source: TBytes; Count: LongWord): TBytes;

procedure ShrinkStream(Source, Target: TStream; Count: LongWord);
procedure UnshrinkStream(Source, Target: TStream; Count: LongWord);

procedure ShrinkFile(const Source, Target: string; Count: LongWord = 0);
procedure UnshrinkFile(const Source, Target: string; Count: LongWord);

{ Test routines for Huffman encoding/decoding as used in Deflate/Inflate
    algorithms.
  The encoder (procedure FlateHuffEncodeXXX) writes a 256-word
    Huffman code length table prefix to the Target and encodes
    the Source to the rest of the Target.
  The decoder (procedure FlateHuffDecodeXXX) reads the prefix to build
    the Huffman codes and decodes the rest}
function FlateHuffEncodeBytes(const Source: TBytes; Count: LongWord = 0): TBytes;
function FlateHuffDecodeBytes(const Source: TBytes; Count: LongWord): TBytes;

procedure FlateHuffEncodeStream(Source, Target: TStream; Count: LongWord);
procedure FlateHuffDecodeStream(Source, Target: TStream; Count: LongWord);

procedure FlateHuffEncodeFile(const Source, Target: string; Count: LongWord = 0);
procedure FlateHuffDecodeFile(const Source, Target: string; Count: LongWord);

implementation

function ShrinkBytes(const Source: TBytes; Count: LongWord): TBytes;
var
  SourceStream,
  TargetStream: TksBytesStream;

begin
  if Count = 0 then Count:= Length(Source);
  SourceStream:= TksBytesStream.Create(Source);
  try
    TargetStream:= TksBytesStream.Create;
    try
      ShrinkStream(SourceStream, TargetStream, Count);
      Result:= TargetStream.Bytes;
    finally
      TargetStream.Free;
    end;
  finally
    SourceStream.Free;
  end;
end;

function UnshrinkBytes(const Source: TBytes; Count: LongWord): TBytes;
var
  SourceStream,
  TargetStream: TksBytesStream;

begin
  SourceStream:= TksBytesStream.Create(Source);
  try
    TargetStream:= TksBytesStream.Create;
    try
      UnshrinkStream(SourceStream, TargetStream, Count);
      Result:= Copy(TargetStream.Bytes);
    finally
      TargetStream.Free;
    end;
  finally
    SourceStream.Free;
  end;
end;

procedure ShrinkStream(Source, Target: TStream; Count: LongWord);
var
  Reader: TksReader;
  Writer: TksWriter;

begin
  Reader:= TksReader.Create(Source, 16 * 1024);
  try
    Writer:= TksWriter.Create(Target, 16 * 1024);
    try
      Shrink(Reader.ReadByte, Writer.WriteBits, Count);
    finally
      Writer.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure UnshrinkStream(Source, Target: TStream; Count: LongWord);
var
  Reader: TksReader;
  Writer: TksWriter;

begin
  Reader:= TksReader.Create(Source, 16 * 1024);
  try
    Writer:= TksWriter.Create(Target, 16 * 1024);
    try
      Unshrink(Reader.ReadBits, Writer.WriteByte, Count);
    finally
      Writer.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure ShrinkFile(const Source, Target: string; Count: LongWord);
var
  Src, Tgt: TFileStream;

begin
  Src:= TFileStream.Create(Source, fmOpenRead or fmShareDenyWrite);
  try
    if Count = 0 then Count:= Src.Size;
    Tgt:= TFileStream.Create(Target, fmCreate);
    try
      ShrinkStream(Src, Tgt, Count);
    finally
      Tgt.Free;
    end;
  finally
    Src.Free;
  end;
end;

procedure UnshrinkFile(const Source, Target: string; Count: LongWord);
var
  Src, Tgt: TFileStream;

begin
  Src:= TFileStream.Create(Source, fmOpenRead or fmShareDenyWrite);
  try
    Tgt:= TFileStream.Create(Target, fmCreate);
    try
      UnshrinkStream(Src, Tgt, Count);
    finally
      Tgt.Free;
    end;
  finally
    Src.Free;
  end;
end;

function FlateHuffEncodeBytes(const Source: TBytes; Count: LongWord = 0): TBytes;
var
  SourceStream,
  TargetStream: TksBytesStream;

begin
  if Count = 0 then Count:= Length(Source);
  SourceStream:= TksBytesStream.Create(Source);
  try
    TargetStream:= TksBytesStream.Create;
    try
      FlateHuffEncodeStream(SourceStream, TargetStream, Count);
      Result:= TargetStream.Bytes;
    finally
      TargetStream.Free;
    end;
  finally
    SourceStream.Free;
  end;
end;

function FlateHuffDecodeBytes(const Source: TBytes; Count: LongWord): TBytes;
var
  SourceStream,
  TargetStream: TksBytesStream;

begin
  SourceStream:= TksBytesStream.Create(Source);
  try
    TargetStream:= TksBytesStream.Create;
    try
      FlateHuffDecodeStream(SourceStream, TargetStream, Count);
      Result:= Copy(TargetStream.Bytes);
    finally
      TargetStream.Free;
    end;
  finally
    SourceStream.Free;
  end;
end;

procedure FlateHuffEncodeStream(Source, Target: TStream; Count: LongWord);
var
// Warning! We actually need 256 codes (for bytes),
//   but we must allocate space for 512 codes to build a Huffman tree.
  HuffmanCodes: packed array[0..511] of TksFlateHuffEncoder.THuffmanCode;

  Encoder: TksFlateHuffEncoder;
  Code: TksFlateHuffEncoder.PHuffmanCode;
  Buf, BufPtr, BufSentinel: PByte;
  Writer: TksWriter;
  I: Integer;

begin
  GetMem(Buf, Count);
  try
    Source.ReadBuffer(Buf^, Count);
    FillChar(HuffmanCodes, SizeOf(HuffmanCodes), 0);

// initialize frequency counts
    BufPtr:= Buf;
    BufSentinel:= Buf;
    Inc(BufSentinel, Count);
    while BufPtr <> BufSentinel do begin
      Inc(HuffmanCodes[BufPtr^].Freq);
      Inc(BufPtr);
    end;

    Writer:= TksWriter.Create(Target, 16 * 1024);
    try
// build Huffman tree
      Encoder.FixedCodes:= nil;
      Encoder.ExtraBits:= nil;
      Encoder.BuildCodes(@HuffmanCodes, 256, 15);  //?? probably 16

// write code length table
      for I:= 0 to 255 do begin
        Writer.Write(HuffmanCodes[I].Len, SizeOf(Word));
      end;

// encode the Source
      BufPtr:= Buf;
      while BufPtr <> BufSentinel do begin
        Code:= @HuffmanCodes[BufPtr^];
        Writer.WriteBits(Code^.Code, Code^.Len);
        Inc(BufPtr);
      end;

    finally
      Writer.Free;
    end;
  finally
    FreeMem(Buf, Count);
  end;
end;

procedure FlateHuffDecodeStream(Source, Target: TStream; Count: LongWord);
var
  CodeLengths: packed array[0..255] of Word;
  Decoder: TksFlateHuffDecoder;
  Code: Cardinal;
  Reader: TksReader;
  Writer: TksWriter;

begin
  Reader:= TksReader.Create(Source, 16 * 1024);
  try
    Reader.Read(CodeLengths, SizeOf(CodeLengths));
    Decoder.Init(Reader.ReadBits, @CodeLengths, 256, False);
    try
      Writer:= TksWriter.Create(Target, 16 * 1024);
      try
        while Count > 0 do begin
          Code:= Decoder.Decode;
          Writer.Write(Code, SizeOf(Byte));
          Dec(Count);
        end;
      finally
        Writer.Free;
      end;
    finally
      Decoder.Done;
    end;
  finally
    Reader.Free;
  end;
end;

procedure FlateHuffEncodeFile(const Source, Target: string; Count: LongWord = 0);
var
  Src, Tgt: TFileStream;

begin
  Src:= TFileStream.Create(Source, fmOpenRead or fmShareDenyWrite);
  try
    if Count = 0 then Count:= Src.Size;
    Tgt:= TFileStream.Create(Target, fmCreate);
    try
      FlateHuffEncodeStream(Src, Tgt, Count);
    finally
      Tgt.Free;
    end;
  finally
    Src.Free;
  end;
end;

procedure FlateHuffDecodeFile(const Source, Target: string; Count: LongWord);
var
  Src, Tgt: TFileStream;

begin
  Src:= TFileStream.Create(Source, fmOpenRead or fmShareDenyWrite);
  try
    Tgt:= TFileStream.Create(Target, fmCreate);
    try
      FlateHuffDecodeStream(Src, Tgt, Count);
    finally
      Tgt.Free;
    end;
  finally
    Src.Free;
  end;
end;

end.
