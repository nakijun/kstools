{ *********************************************************** }
{ *                    ksTools Library                      * }
{ *       Copyright (c) Sergey Kasandrov 1997, 2010         * }
{ *       -----------------------------------------         * }
{ *         http://sergworks.wordpress.com/kstools          * }
{ *********************************************************** }

unit ksZip;

interface

uses
  ksConsts, SysUtils, Classes, ksUtils, ksClasses, ksArchives, ksShrink,
  ksZCrypt;

{
  Some comments were copied from APPNOTE.TXT Version 6.2.0
}

type
  EksZipError = class(EksArchiveError);
  EksZipOpenError = class(EksZipError);
  EksZipAppendError = class(EksZipError);
  EksZipExtractError = class(EksZipError);

type
  TksZipArchive = class(TksArcHelper)
  public const
    ZipVersion = 20;
    ZipLocalFileHeaderSignature = $04034B50;
    ZipCentralDirFileHeaderSignature = $02014B50;
    ZipEndCentralDirSignature = $06054B50;

                      // General purpose bit flag bits (2 bytes):
  public const
{
          Bit 0: If set, indicates that the file is encrypted.
}
    FLAG_ENCRYPTED = 1;
{
          (For Method 6 - Imploding)
          Bit 1: If the compression method used was type 6,
                 Imploding, then this bit, if set, indicates
                 an 8K sliding dictionary was used.  If clear,
                 then a 4K sliding dictionary was used.
          Bit 2: If the compression method used was type 6,
                 Imploding, then this bit, if set, indicates
                 3 Shannon-Fano trees were used to encode the
                 sliding dictionary output.  If clear, then 2
                 Shannon-Fano trees were used.

          (For Methods 8 and 9 - Deflating)
          Bit 2  Bit 1
            0      0    Normal (-en) compression option was used.
            0      1    Maximum (-exx/-ex) compression option was used.
            1      0    Fast (-ef) compression option was used.
            1      1    Super Fast (-es) compression option was used.

          Note:  Bits 1 and 2 are undefined if the compression
                 method is any other.
}
    FLAG_DEFLATE_LEVEL     = 6;
    FLAG_DEFLATE_NORMAL    = 0;
    FLAG_DEFLATE_MAXIMUM   = 2;
    FLAG_DEFLATE_FAST      = 4;
    FLAG_DEFLATE_SUPERFAST = 6;
{
          Bit 3: If this bit is set, the fields crc-32, compressed
                 size and uncompressed size are set to zero in the
                 local header.  The correct values are put in the
                 data descriptor immediately following the compressed
                 data.  (Note: PKZIP version 2.04g for DOS only
                 recognizes this bit for method 8 compression, newer
                 versions of PKZIP recognize this bit for any
                 compression method.)
                [Info-ZIP note: This bit was introduced by PKZIP 2.04 for
                 DOS. In general, this feature can only be reliably used
                 together with compression methods that allow intrinsic
                 detection of the "end-of-compressed-data" condition. From
                 the set of compression methods described in this Zip archive
                 specification, only "deflate" and "bzip2" fulfill this
                 requirement.
                 Especially, the method STORED does not work!
                 The Info-ZIP tools recognize this bit regardless of the
                 compression method; but, they rely on correctly set
                 "compressed size" information in the central directory entry.]
}
    FLAG_DATADESCRIPTOR = 8;
{
          Bit 4: Reserved for use with method 8, for enhanced
                 deflating.

          Bit 5: If this bit is set, this indicates that the file is
                 compressed patched data.  (Note: Requires PKZIP
                 version 2.70 or greater)

          Bit 6: Strong encryption.  If this bit is set, you should
                 set the version needed to extract value to at least
                 50 and you must also set bit 0.  If AES encryption
                 is used, the version needed to extract value must
                 be at least 51.

          Bit 7: Currently unused.

          Bit 8: Currently unused.

          Bit 9: Currently unused.

          Bit 10: Currently unused.

          Bit 11: Currently unused.

          Bit 12: Reserved by PKWARE for enhanced compression.

          Bit 13: Used when encrypting the Central Directory to indicate
                  selected data values in the Local Header are masked to
                  hide their actual values.  See the section describing
                  the Strong Encryption Specification for details.

          Bit 14: Reserved by PKWARE.

          Bit 15: Reserved by PKWARE.
}


  public const
// ZIP compression methods
    METHOD_STORE = 0;           // Full support
    METHOD_SHRINK = 1;          // Full support
    METHOD_REDUCE1 = 2;         // Not supported
    METHOD_REDUCE2 = 3;         // Not supported
    METHOD_REDUCE3 = 4;         // Not supported
    METHOD_REDUCE4 = 5;         // Not supported
    METHOD_IMPLODE = 6;         // Not supported
    METHOD_TOKENIZE = 7;        // Not supported
    METHOD_DEFLATE = 8;         // Not supported
    METHOD_ENHANCEDDEFLATE = 9; // Not supported
    METHOD_DCLIMPLODE = 10;     // Not supported
    METHOD_RESERVED = 11;
    METHOD_BZIP2 = 12;          // Not supported

  public type
                                  // ZIP end of central directory record
    PEndOfCentralDirRec = ^TEndOfCentralDirRec;
    TEndOfCentralDirRec = packed record
      Signature: LongWord;        // ZipEndCentralDirSignature = $06054B50
      DiskNumber: Word;           // Number of this disk
      StartDiskNumber: Word;      // Disk where central directory starts
      EntriesOnDisk: Word;        // Number of central directory records
                                  //   on this disk
      TotalEntries: Word;         // Total number of central directory records
      DirectorySize: LongWord;    // Size of central directory (bytes)
      DirectoryOffset: LongWord;  // Offset of start of central directory,
                                  //   relative to start of archive
      CommentLen: Word;           // ZIP file comment length
      Comment: record end;        // ZIP file comment
    end;

    PZipLocalFileHeader = ^TZipLocalFileHeader;
    TZipLocalFileHeader = packed record
      Signature: LongWord;
      VersionToExtract: Word;
      Flags: Word;
      CompressionMethod: Word;
      LastModDateTime: LongWord;
      CRC32: LongWord;
      CompressedSize: LongWord;
      UncompressedSize: LongWord;
      FileNameLen: Word;
      ExtraFieldLen: Word;
      Data: record end;
    end;

    PZipDataDescriptor = ^TZipDataDescriptor;
    TZipDataDescriptor = packed record
      CRC32: LongWord;
      CompressedSize: LongWord;
      UncompressedSize: LongWord;
    end;

    PZipCentralDirFileHeader = ^TZipCentralDirFileHeader;
    TZipCentralDirFileHeader = packed record
      Signature: LongWord;
      VersionMadeBy: Word;
      VersionToExtract: Word;
      Flags: Word;
      CompressionMethod: Word;
      LastModDateTime: LongWord;
      CRC32: LongWord;
      CompressedSize: LongWord;
      UncompressedSize: LongWord;
      FileNameLen: Word;
      ExtraFieldLen: Word;
      FileCommentLen: Word;
      DiskNumberStart: Word;
      InternalAttributes: Word;
      ExternalAttributes: LongWord;
      RelativeOffset: LongWord;
      Data: record end;
      function Size: Integer;
      function LocalSize: Integer;
      function Clone: PZipCentralDirFileHeader;
      function LocalHeader: PZipLocalFileHeader;
      procedure GetDataDescriptor(var Desc: TZipDataDescriptor);
      function GetFileName: string;
//      procedure SaveLocalHeader(Writer: TksWriter);
//      procedure SaveLocalFooter(Writer: TksWriter);
//      function FileNameOfs: Integer;
//      function ExtraFieldOfs: Integer;
//      function FileCommentOfs: Integer;
    end;
  private
    function GetItem(Index: Integer): PZipCentralDirFileHeader;

  protected
                            // End of central directory record data
    FEndOfCentralDirRecOffset: Int64;
    FEndOfCentralDirRecSize: Integer;
    FEndOfCentralDirRec: PEndOfCentralDirRec;
                            // List of PZipCentralDirFileHeader entries
    FItemList: TList;
    FCurrentItem: PZipCentralDirFileHeader;
                            // Used to calcutate CRC32
                            //   while compressing/decompressing
    FItemCRC: LongWord;
                            // Traditional PKWARE Encryption
    FZipCrypto: TksZipCrypto;
// TODO: remove to local var in AppendItem method
    FZipCryptoHeader: TksZipCrypto.THeader;

    FCRCCheckSize: Integer;   // Used with traditional PKWARE encryption;
                              //   determines the number of crypto header bytes
                              //     to check an item password;
                              //   valid values are 1 (default)
                              //     and 2 (old PKZIP spec prior to 2.0)
                              //   any other value means no password checking
                              //     is performed.

    FCheckCRC: Boolean;
    FCheckSize: Boolean;

//    FItemPassword: TBytes;

    procedure ZipEncrypt(Sender: TObject; Buf: Pointer; BufSize: Integer);

    procedure BeforeRead(Sender: TObject; Buf: Pointer; BufSize: Integer); override;

// Assigned to Reader.AfterFlush while reading encrypted compressed stream
    procedure ReadCompressed(Sender: TObject; Buf: Pointer; BufSize: Integer);

// Assigned to Writer.BeforeFlush while writing uncompressed stream
    procedure WriteUncompressed(Sender: TObject; Buf: Pointer; BufSize: Integer);

    procedure FreeItems;

    procedure AppendItem(Item: PZipCentralDirFileHeader;
                         Source: TStream; OnProgress: TksProgressEvent);

    procedure ExtractItem(Item: PZipCentralDirFileHeader;
                          Target: TStream; OnProgress: TksProgressEvent);

    procedure SaveCentralDir;

  public
    constructor Create(AStream: TStream; AFreeOnClose: Boolean);
    destructor Destroy; override;
    function FindEndOfCentralDir(Stream: TStream): Boolean;
    procedure AllocEndOfCentralDirRec(NewSize: Integer);
    procedure LoadEndOfCentralDirRec;
    function LoadCentralDir: Boolean;
    procedure EndUpdate; override;
    procedure Load; override;
    procedure AppendStream(const ExtName, IntName: string; Source: TStream;
                           OnProgress: TksProgressEvent); override;
    procedure ExtractStream(Index: Integer; Target: TStream;
                           OnProgress: TksProgressEvent); override;

    function ItemCount: Integer; override;
    function GetItemInfo(var Item: TksArchive.TItemInfo;
                             Index: Integer): Boolean; override;

    property Items[Index: Integer]: PZipCentralDirFileHeader read GetItem;
    property EndOfCentralDirRec: PEndOfCentralDirRec read FEndOfCentralDirRec;
    property CRCCheckSize: Integer read FCRCCheckSize write FCRCCheckSize;

    property CompressionMethod;
    property FileEncrypted;
    property FilePassword;
    property FilePasswordString;

  end;

implementation

{ TksZipHelper }

constructor TksZipArchive.Create(AStream: TStream; AFreeOnClose: Boolean);
begin
  inherited Create(AStream, AFreeOnClose);
  FItemList:= TList.Create;
  FCRCCheckSize:= 2;
  FCheckCRC:= True;
  FCheckSize:= True;
end;

destructor TksZipArchive.Destroy;
begin
  EndUpdate;
  FreeItems;
  FItemList.Free;
  inherited Destroy;
end;

{ Allocate the End of Central Directory Record }
procedure TksZipArchive.AllocEndOfCentralDirRec(NewSize: Integer);
begin
  if NewSize <> FEndOfCentralDirRecSize then begin
    if FEndOfCentralDirRecSize <> 0 then begin
      FreeMem(FEndOfCentralDirRec, FEndOfCentralDirRecSize);
      FEndOfCentralDirRecSize:= 0;
    end;
    if NewSize <> 0 then begin
      GetMem(FEndOfCentralDirRec, NewSize);
      FEndOfCentralDirRec^.Signature:= ZipEndCentralDirSignature;
      FEndOfCentralDirRecSize:= NewSize;
    end;
  end;
end;

procedure TksZipArchive.BeforeRead(Sender: TObject; Buf: Pointer;
  BufSize: Integer);
begin
  FItemCRC:= CRC32OfData(Buf^, BufSize, FItemCRC);
  inherited;
end;

function TksZipArchive.FindEndOfCentralDir(Stream: TStream): Boolean;
const
  MaxOffset = 2048;       // максимальное смещение сигнатуры записи
                          //   конца центрального каталога архива
                          //     (end of central directory record)
                          // если сигнатура не найдена в пределах
                          //   MaxOffset байтов от конца архива,
                          //   считаем что еЄ нет

  BufSize = 32;

var
  Buf: packed array[0..BufSize - 1] of Byte;
  Signature: LongWord;
  SeekOffset: LongInt;
  SearchLength: LongInt;
  StreamSize: Int64;
  Pos: Integer;

begin
// сигнатура, которую будем искать
  Signature:= ZipEndCentralDirSignature;

// скорее всего (но не об€зательно) искома€ сигнатура находитс€
//   на таком смещении от конца архива; с него и начнЄм
  SeekOffset:= SizeOf(TEndOfCentralDirRec);

  Result:= False;
  StreamSize:= Stream.Size;
  if StreamSize >= SizeOf(Signature) then repeat
    if SeekOffset >= StreamSize then SeekOffset:= StreamSize;
    if (SeekOffset > MaxOffset) then Exit;
    Stream.Seek(-SeekOffset, soEnd);

    SearchLength:= BufSize;
    if SearchLength > SeekOffset then SearchLength:= SeekOffset;
    Stream.ReadBuffer(Buf, SearchLength);

  { see if the signature is in the buffer}
    Pos:= ScanBytes(@Buf, @Signature, SearchLength, SizeOf(Signature));
    if (Pos >= 0) then begin
  { move to the start of the End of Central Directory Record }
      Stream.Seek(Pos - SeekOffset, soEnd);
      FEndOfCentralDirRecOffset:= Stream.Position;
      Result:= True;
      Exit;
    end;
    if (SeekOffset = StreamSize) then Exit;
    Inc(SeekOffset, BufSize - SizeOf(Signature));
  until False;
end;

procedure TksZipArchive.FreeItems;
begin

end;

function TksZipArchive.GetItem(Index: Integer): PZipCentralDirFileHeader;
begin
//  if Cardinal(Index) < Cardinal(FItemList.Count) then
  Result:= FItemList[Index];
end;

function TksZipArchive.GetItemInfo(var Item: TksArchive.TItemInfo;
                                       Index: Integer): Boolean;
var
  P: PZipCentralDirFileHeader;

begin
  Result:= Index < FItemList.Count;
  if Result then begin
    P:= FItemList[Index];
    Item.CompressedSize:= P^.CompressedSize;
    Item.Attributes:= P^.ExternalAttributes;
    Item.FileDate:= FileDateToDateTime(P^.LastModDateTime);
    Item.FileName:= P^.GetFileName;
    Item.FileSize:= P^.UncompressedSize;
  end;
end;

function TksZipArchive.ItemCount: Integer;
begin
  Result:= FItemList.Count;
end;

{ Load the End of Central Directory Record }
procedure TksZipArchive.LoadEndOfCentralDirRec;
var
  Rec: TEndOfCentralDirRec;
  RecSize: Cardinal;

begin
  FStream.ReadBuffer(Rec, SizeOf(TEndOfCentralDirRec));
  RecSize:= SizeOf(TEndOfCentralDirRec);

{ Check the Comment Length is nonzero and valid;
   if CommentLen is too long, fix it  }
  if (Rec.CommentLen > 0) then begin
    if (FStream.Position + Rec.CommentLen > FStream.Size) then begin
      Rec.CommentLen:= FStream.Size - FStream.Position;
    end;
    Inc(RecSize, Rec.CommentLen);
  end;

  AllocEndOfCentralDirRec(RecSize);
  Move(Rec, FEndOfCentralDirRec^, SizeOf(TEndOfCentralDirRec));

  if (Rec.CommentLen > 0) then begin
    FStream.ReadBuffer(FEndOfCentralDirRec^.Comment, Rec.CommentLen);
  end;
end;

procedure TksZipArchive.ReadCompressed(Sender: TObject;
                        Buf: Pointer; BufSize: Integer);
begin

// if the data is encrypted, decrypt it
//  if FCurrentItem^.Flags and FLAG_ENCRYPTED <> 0 then begin
  FZipCrypto.Decode(Buf^, BufSize);
//  end;
end;

procedure TksZipArchive.WriteUncompressed(Sender: TObject; Buf: Pointer;
  BufSize: Integer);
begin
// update the item's CRC32
  FItemCRC:= CRC32OfData(Buf^, BufSize, FItemCRC);

// and update progress (indicator)
  UpdateProgress(BufSize);
end;

procedure TksZipArchive.ZipEncrypt(Sender: TObject; Buf: Pointer;
                                  BufSize: Integer);
begin
  FZipCrypto.Encode(Buf^, BufSize);
end;

procedure TksZipArchive.SaveCentralDir;
var
  I: Integer;
  P: PZipCentralDirFileHeader;
  DirSize, DirOffs: Cardinal;

begin
// TODO: Multivolume support
  DirSize:= 0;
  DirOffs:= LongWord(FStream.Position);
  for I:= 0 to FItemList.Count - 1 do begin
    P:= FItemList[I];
    FStream.WriteBuffer(P^, P^.Size);
    Inc(DirSize, P^.Size);
  end;

  if FEndOfCentralDirRecSize = 0 then
    AllocEndOfCentralDirRec(SizeOf(TEndOfCentralDirRec));

// TODO: Multivolume support
  with FEndOfCentralDirRec^ do begin
    DiskNumber:= 0;                   // Number of this disk
    StartDiskNumber:= 0;              // Disk where central directory starts
    EntriesOnDisk:= FItemList.Count;  // Number of central directory records
                                      //   on this disk
    TotalEntries:= FItemList.Count;   // Total number of central directory records
    DirectorySize:= DirSize;          // Size of central directory (bytes)
    DirectoryOffset:= DirOffs;        // Offset of start of central directory,
                                      //   relative to start of archive
// Don't change  CommentLen: Word               // ZIP file comment length
  end;

  FStream.WriteBuffer(FEndOfCentralDirRec^, FEndOfCentralDirRecSize);
  FStream.Size:= FStream.Position;
end;

{ Load the Central Directory from a single stream }
function TksZipArchive.LoadCentralDir: Boolean;
type
  TBuffer = packed array[0..64 * 1024 - 1] of Byte;

var
  Buffer: TBuffer;
  BufPtr: PByte;
  ItemPtr, P: PZipCentralDirFileHeader;
  ItemSize: Integer;
//  ItemInBuffer: Boolean;
  BufSize: Integer;
  BytesAvailable: Integer;
  DirSize: Int64;
  BytesToAdd: Cardinal;
  Count: Integer;
  StubCount: LongWord;

begin
  StubCount:= $FFFFFFFF;

// TODO: createCentral dir List

// „исло элементов в центральном каталоге
  Count:= FEndOfCentralDirRec^.TotalEntries;
// TODO: Check Count is valid

// ћаксимально возможный размер центрального каталога в байтах
  DirSize:= FEndOfCentralDirRecOffset - FStream.Position;

// „исло байтов, уже загруженных в Buffer
  BytesAvailable:= 0;
  while (Count > 0) and (DirSize > 0) do begin

// «агружаем очередную порцию данных центрального каталога в буфер
    BufPtr:= @Buffer;
    if (BytesAvailable > 0) then begin
      Move(ItemPtr^, BufPtr^, BytesAvailable);
      Inc(BufPtr, BytesAvailable);
    end;
    BufSize:= SizeOf(Buffer) - BytesAvailable;
    if BufSize > DirSize then BufSize:= DirSize;
    Dec(DirSize, BufSize);
    FStream.ReadBuffer(BufPtr^, BufSize);
    Inc(BytesAvailable, BufSize);
    BufSize:= BytesAvailable;
    ItemPtr:= @Buffer;

// »звлекаем элементы центрального каталога из буфера
    while (BytesAvailable >= SizeOf(TZipCentralDirFileHeader)) and (Count > 0) do begin
      ItemSize:= SizeOf(TZipCentralDirFileHeader);
      if ItemPtr^.Signature = ZipCentralDirFileHeaderSignature then begin
        if ItemPtr^.RelativeOffset < StubCount
          then StubCount:= ItemPtr^.RelativeOffset;
        Inc(ItemSize, ItemPtr^.FileNameLen);
        Inc(ItemSize, ItemPtr^.ExtraFieldLen);
        Inc(ItemSize, ItemPtr^.FileCommentLen);
        if (ItemSize > BufSize) then begin
// ≈сли элемент не помещаетс€ в буфер, считаем это ошибкой
// TODO:
          Exit;
        end;
        if (ItemSize <= BytesAvailable) then begin
// »звлекаем очередной элемент каталога
          FItemList.Add(ItemPtr^.Clone);
          Dec(Count);
          Dec(BytesAvailable, ItemSize);
          Inc(PByte(ItemPtr), ItemSize);
        end
        else begin
          Break;
        end;
      end
      else begin
// Ќеверна€ сигнатура - прекращаем извлечение элементов
// TODO:

      end;
    end;
  end;
  if FIsExecutable then FStubSize:= StubCount;
end;

procedure TksZipArchive.Load;
var
  Signature: LongWord;

begin
  FStubSize:= 0;

// ѕровер€ем сигнатуру файла;
//   должно быть Word($5A4D) - executable
//   или LongWord($04034B50)
  if FStream.Size < 4 then begin
    raise EksZipOpenError.CreateResFmt(@SInvalidArchive, [FFileName]);
  end;
  FStream.Position:= 0;
  FStream.ReadBuffer(Signature, SizeOf(Signature));
  FIsExecutable:= Word(Signature) = $5A4D;
  if not FIsExecutable and (Signature <> ZipLocalFileHeaderSignature)
    then begin
      raise EksZipOpenError.CreateResFmt(@SInvalidFileSignature,
                                          [FFileName, Signature]);
    end;

// »щем сигнатуру записи конца центрального каталога архива;
// TODO: дл€ многотомных архивов сигнатура на первом томе будет отсутствовать,
//       в этом случае следует перейти на последний том
  if not FindEndOfCentralDir(FStream) then begin
    raise EksZipOpenError.CreateResFmt(@SEndOfCentralDirNotFound,
                                          [FFileName]);
  end;

  LoadEndOfCentralDirRec; //(FStream);

// TODO: MultiVolume not implemented
  FStream.Position:= FEndOfCentralDirRec^.DirectoryOffset;

//  Force ReadOnly mode for broken archives;
  if not LoadCentralDir then FReadOnly:= True;

  FLoaded:= True;
end;

procedure TksZipArchive.AppendItem(Item: PZipCentralDirFileHeader;
          Source: TStream; OnProgress: TksProgressEvent);
var
  Reader: TksReader;
  Writer: TksWriter;
  StartOffset: LongInt;
  Save: Integer;
  SavePos: Int64;
  StreamSize: Int64;
  CRC: LongWord;
  LocalHeader: PZipLocalFileHeader;
  LocalFooter: TZipDataDescriptor;

begin
  if {not FLoaded or} FMultiVolume or FReadOnly then begin
    raise EksZipAppendError.CreateResFmt(@SArchiveIsReadOnly, [FFileName]);
  end;
  try
    Writer:= TksWriter.Create(FStream, FIOBufSize);
    try
      Writer.Position:= FEndOfCentralDirRecOffset;
      Item^.RelativeOffset:= FEndOfCentralDirRecOffset;
      LocalHeader:= Item^.LocalHeader;
      try
        Writer.Write(LocalHeader^, Item^.LocalSize);
      finally
        FreeMem(LocalHeader, Item^.LocalSize);
      end;
      StartOffset:= Writer.Position;
(* MOVED - should be encrypted
      if Item^.Flags and FLAG_ENCRYPTED <> 0 then begin
{
  Traditional PKWARE Encryption. We need CRC32 of Source stream to
    initialize crypto header
}
// TODO: возможно это не нужно, обойтись без этого если
//       не об€зательно знать CRC дл€ шифровани€,
//       а достаточно посчитать CRC в процессе сжати€
//       и после вернутьс€ и перезаписать FZipCryptoHeader
        SavePos:= Source.Position;
        CRC:= CRC32Stream(Source);
// TODO: check StreamSize = Item^.UncompressedSize
        StreamSize:= Source.Position - SavePos;
        Source.Position:= SavePos;
        FZipCrypto.InitHeader(FZipCryptoHeader, FItemPassword, CRC);
// TODO: write a random value to last byte of FZipCryptoHeader
        Writer.Write(FZipCryptoHeader, SizeOf(FZipCryptoHeader));
      end;
*)
{
  Write local file header and crypto header without encryption
}
      Writer.FlushBuffer;

      if Item^.Flags and FLAG_ENCRYPTED <> 0 then begin
{
  Traditional PKWARE Encryption. We need CRC32 of Source stream to
    initialize crypto header
}
// TODO: возможно это не нужно, обойтись без этого если
//       не об€зательно знать CRC дл€ шифровани€,
//       а достаточно посчитать CRC в процессе сжати€
//       и после вернутьс€ и перезаписать FZipCryptoHeader
        SavePos:= Source.Position;
        CRC:= CRC32Stream(Source);
// TODO: check StreamSize = Item^.UncompressedSize
        StreamSize:= Source.Position - SavePos;
        Source.Position:= SavePos;
        FZipCrypto.InitHeader(FZipCryptoHeader, FFilePassword, CRC, FCRCCheckSize);
// TODO: write a random value to last byte of FZipCryptoHeader
        Writer.Write(FZipCryptoHeader, SizeOf(FZipCryptoHeader));
{
  Assign a handler to encrypt the compressed data
}
        Writer.FlushBuffer;
        Writer.BeforeFlush:= ZipEncrypt;


      end;

      try
// TODO:       DoBeforeAppend(Item);
        try
          Reader:= TksReader.Create(Source, FIOBufSize);
          try
{
  Assign a handler to calculate CRC of uncompressed data;
    we need "Before" handler here to avoid excessive data
      that is possible in "After" handler.
}
            Reader.BeforeFlush:= BeforeRead;


            FItemCRC:= $FFFFFFFF;
            FItemPos:= 0;
            FItemSize:= Item^.UncompressedSize;
            FOnItemProgress:= OnProgress;
            case Item^.CompressionMethod of
              METHOD_STORE: CopyFiler(Reader, Writer, Item^.UncompressedSize);
              METHOD_SHRINK: Shrink(Reader.ReadByte, Writer.WriteBits,
                                                   Item^.UncompressedSize);
//              METHOD_DEFLATE: Deflate(Reader, Writer, Item.FileSize, ZipItem.DeflateLevel);
            else
              Item^.CompressionMethod:= METHOD_SHRINK;
              Shrink(Reader.ReadByte, Writer.WriteBits, Item^.UncompressedSize);
            end;
            Writer.FlushBits;                // make sure
            Writer.FlushBuffer;              //   all encrypted

          finally
            Writer.BeforeFlush:= nil;        // stop encryption
            Reader.Free;                     // CRC is ready now
            FItemCRC:= not FItemCRC;
          end;

          Item^.CRC32:= FItemCRC;
// for encrypted file the size of crypto header
//   is included into the CompressedSize
          Item^.CompressedSize:= Writer.Position - StartOffset;

          if Item^.Flags and FLAG_DATADESCRIPTOR <> 0 then begin
            Item^.GetDataDescriptor(LocalFooter);
            Writer.Write(LocalFooter, SizeOf(TZipDataDescriptor));
            Writer.FlushBuffer;
          end;

        except
          Writer.Position:= FEndOfCentralDirRecOffset;   // restore archive footer
//          InternalTruncate(Writer);
//TODO:          SaveFooter(Writer);
//          Writer.FlushBuffer;
//TODO:          SetEndOfFile((Writer.Stream as THandleStream).Handle);
          raise;
        end;

//        FItemList.Add(Item);
        FEndOfCentralDirRecOffset:= Writer.Position;

// Update Local File Header (CompressedSize and CRC32 fields)
        FStream.Position:= Item^.RelativeOffset;
        LocalHeader:= Item^.LocalHeader;
        try
          FStream.WriteBuffer(LocalHeader^, Item^.LocalSize);
        finally
          FreeMem(LocalHeader, Item^.LocalSize);
        end;
        FStream.Position:= FEndOfCentralDirRecOffset;

{        if Item.UpdateLocalHeader then begin
          Writer.Position:= Item.FHeaderOffset;
  //          if Assigned(FCrypter) then Item.SetCryptoHeader(FCrypter, FPassword);
          Item.SaveHeader(Writer);
        end;}

//        Writer.FlushBuffer;
//TODO:        DoAfterAppend(Item);
      finally
//TODO:        EncryptDone;
      end;
    finally;
      Writer.Free;
    end;
  finally
  end;
end;

procedure TksZipArchive.EndUpdate;
begin
  if FUpdating then begin
    FUpdating:= False;
    SaveCentralDir;
  end;
end;

procedure TksZipArchive.ExtractItem(Item: PZipCentralDirFileHeader;
                        Target: TStream; OnProgress: TksProgressEvent);
var
  Reader: TksReader;
  Writer: TksWriter;
  L: Int64;
  PassW: TBytes;
  CryptoHeader: TksZipCrypto.THeader;
  LocalFooter: TZipDataDescriptor;

begin
// TODO:  DoBeforeExtract(Item);

  FCurrentItem:= Item;
  FStream.Position:= Item^.RelativeOffset + Cardinal(Item^.LocalSize);
  if Item^.Flags and FLAG_ENCRYPTED <> 0 then begin
                                       // Exception if password invalid
// TODO: FItemPassword            PassW:= GetPassword;
//            DecryptInit(Item);
    FStream.ReadBuffer(CryptoHeader, SizeOf(CryptoHeader));
    if not FZipCrypto.CheckHeader(CryptoHeader, FFilePassword,
           Item^.CRC32, FCRCCheckSize) then
             raise EksInvalidPassword.CreateRes(@SInvalidPassword);
  end;

  Writer:= TksWriter.Create(Target, FIOBufSize);
  try
    L:= Writer.Position;
    FItemCRC:= $FFFFFFFF;
    Writer.BeforeFlush:= WriteUncompressed;  // CRC of decompressed data

    Reader:= TksReader.Create(FStream, FIOBufSize);
    try
      if Item^.Flags and FLAG_ENCRYPTED <> 0 then begin
        Reader.AfterFlush:= ReadCompressed;
      end;

      FItemPos:= 0;
      FItemSize:= PZipCentralDirFileHeader(Item)^.UncompressedSize;
      case PZipCentralDirFileHeader(Item)^.CompressionMethod of
        METHOD_STORE: CopyFiler(Reader, Writer, FItemSize);
        METHOD_SHRINK: UnShrink(Reader.ReadBits, Writer.WriteByte, FItemSize);
// TODO:                ZIP_Deflate: Inflate(Reader, Writer);//, Item.FileSize);
      else
        raise EksInvalidMethod.CreateResFmt(@SInvalidMethod,
              [PZipCentralDirFileHeader(Item)^.CompressionMethod]);
      end;

// We don't need data descriptor (if any)
//   and actually just move the reader position to the right place
// If we need it, the code below is probably incorrect for encrypted file:
//   the reader decrypts but data descriptor is not encrypted.
      if Item^.Flags and FLAG_DATADESCRIPTOR <> 0 then begin
        Reader.Read(LocalFooter, SizeOf(LocalFooter));
      end;

    finally
      Reader.Free;
    end;


  finally
    L:= Writer.Position - L;
    Writer.Free;
    FItemCRC:= not FItemCRC;    // called after last buffer flushed
  end;

  if FCheckCRC and (FItemCRC <> Item^.CRC32) then
    raise EksZipExtractError.CreateResFmt(@SInvalidCRC,
      [FItemList.IndexOf(Item), Item^.GetFileName]);
  if FCheckSize and (L <> Item^.UncompressedSize) then
    raise EksZipExtractError.CreateResFmt(@SInvalidSize,
      [FItemList.IndexOf(Item), Item^.GetFileName, Item^.UncompressedSize, L]);

//TODO:
//    DoAfterExtract(Item);}
end;

procedure TksZipArchive.ExtractStream(Index: Integer; Target: TStream;
                                      OnProgress: TksProgressEvent);
var
  Item: PZipCentralDirFileHeader;

begin
  Item:= FItemList[Index];
  ExtractItem(Item, Target, OnProgress);
end;

procedure TksZipArchive.AppendStream(const ExtName, IntName: string;
                       Source: TStream; OnProgress: TksProgressEvent);
var
  Item: PZipCentralDirFileHeader;
  ItemSize: Integer;
  AnsiName: AnsiString;
  NameLen: Integer;

begin
  AnsiName:= AnsiString(StringReplace(IntName, '/', '\', [rfReplaceAll]));
  NameLen:= Length(AnsiName);
  ItemSize:= SizeOf(TZipCentralDirFileHeader) + NameLen;
  Item:= AllocMem(ItemSize);
  try
    Item^.Signature:= ZipCentralDirFileHeaderSignature;
    Item^.VersionMadeBy:= 20;
    Item^.VersionToExtract:= 20;
    Item^.CompressionMethod:= FCompressionMethod;
    if FFileEncrypted then Item^.Flags:= Item^.Flags or FLAG_ENCRYPTED;
    if ExtName <> '' then
      Item^.ExternalAttributes:= FileGetAttr(ExtName);
    Item^.UncompressedSize:= Source.Size;
    if Source is THandleStream then
      Item^.LastModDateTime:= FileGetDate(THandleStream(Source).Handle);
    Item^.FileNameLen:= NameLen;
    Move(AnsiName[1], Item^.Data, NameLen);
    AppendItem(Item, Source, OnProgress);
    FItemList.Add(Item);
    Item:= nil;
    if not FUpdating then SaveCentralDir;
  finally
    if Item <> nil then FreeMem(Item, ItemSize);
  end;
end;

{ TksZipHelper.TZipCentralDirFileHeader }

function TksZipArchive.TZipCentralDirFileHeader.Clone: PZipCentralDirFileHeader;
begin
  GetMem(Result, Size);
  Move(Self, Result^, Size);
end;

procedure TksZipArchive.TZipCentralDirFileHeader.GetDataDescriptor(
                            var Desc: TZipDataDescriptor);
begin
  Desc.CRC32:= CRC32;
  Desc.CompressedSize:= CompressedSize;
  Desc.UncompressedSize:= UncompressedSize;
end;

function TksZipArchive.TZipCentralDirFileHeader.GetFileName: string;
var
  AnsiName: AnsiString;

begin
  if FileNameLen = 0 then Result:= ''
  else begin
    SetLength(AnsiName, FileNameLen);
    Move(Self.Data, PAnsiChar(AnsiName)^, FileNameLen);
    Result:= string(AnsiName);
  end;
end;

function TksZipArchive.TZipCentralDirFileHeader.LocalHeader: PZipLocalFileHeader;
begin
  GetMem(Result, LocalSize);
  Result^.Signature:= ZipLocalFileHeaderSignature;
  Result^.VersionToExtract:= VersionToExtract;
  Result^.Flags:= Flags;
  Result^.CompressionMethod:= CompressionMethod;
  Result^.LastModDateTime:= LastModDateTime;
  Result^.CRC32:= CRC32;
  Result^.CompressedSize:= CompressedSize;
  Result^.UncompressedSize:= UncompressedSize;
  Result^.FileNameLen:= FileNameLen;
  Result^.ExtraFieldLen:= ExtraFieldLen;
  Move(Data, Result^.Data, FileNameLen + ExtraFieldLen);
end;

function TksZipArchive.TZipCentralDirFileHeader.LocalSize: Integer;
begin
  Result:= SizeOf(TZipLocalFileHeader) + FileNameLen + ExtraFieldLen;
end;

{
procedure TksZipHelper.TZipCentralDirFileHeader.SaveLocalFooter(
  Writer: TksWriter);
begin

end;

procedure TksZipHelper.TZipCentralDirFileHeader.SaveLocalHeader(
  Writer: TksWriter);
begin

end;
}

function TksZipArchive.TZipCentralDirFileHeader.Size: Integer;
begin
  Result:= SizeOf(TZipCentralDirFileHeader) + FileNameLen + ExtraFieldLen
                                            + FileCommentLen;
end;

end.
