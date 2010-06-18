{ *********************************************************** }
{ *                    ksTools Library                      * }
{ *       Copyright (c) Sergey Kasandrov 1997, 2010         * }
{ *       -----------------------------------------         * }
{ *         http://sergworks.wordpress.com/kstools          * }
{ *********************************************************** }

unit ksArchives;

interface

uses
  RTLConsts, Windows, SysUtils, Classes, ksUtils, ksClasses;

type
  TksPasswordEvent = procedure(Sender: TObject; var Passw: string) of
          object;

  TksArchiveEvent = procedure(Sender: TObject; Item: Pointer) of object;

  TksProgressEvent = procedure(Sender: TObject; Progress: Word; var Abort:
          Boolean) of object;

type
  EksArchiveError = class(Exception);
  EksInvalidPassword = class(EksArchiveError);
  EksInvalidMethod = class(EksArchiveError);

type
  TksArcHelper = class;

  TksArchive = class(TComponent)
  public const
                              // File Format
    UNDEFINED = 0;
    ZIP = 1;

  public type
    PItemInfo = ^TItemInfo;
    TItemInfo = record
      CompressedSize: Int64;
      Attributes: LongWord;
      FileDate: TDateTime;
      FileName: string;
      FileSize: Int64;
    end;
  private
    FArchive: TksArcHelper;
    FFileFormat: Integer;
    FAfterAppend: TksArchiveEvent;
    FAfterExtract: TksArchiveEvent;
    FBeforeAppend: TksArchiveEvent;
    FBeforeExtract: TksArchiveEvent;

//    FLock: TRTLCriticalSection;
//    FOnNeedPassword: TksPasswordEvent;
//    FPassword: string;
//    FProcessCount: Integer;
//    FStream: TStream;
    FReadOnly: Boolean;
    function GetItemCount: Integer;

  protected
//    FStubSize: LongWord;    // для исполняемых файлов - размер (в байтах)
//                            //   исполняемого заголовка

//    FFooterOffset: Integer;
//    FForcePasswords: Boolean;
//    FHeaderSize: Integer;
    FIOBufSize: Integer;
    FItemPos: LongInt;
    FItemSize: Int64;
//    FMaxAttempts: Integer;
    FMultiVolume: Boolean;
    FOnItemProgress: TksProgressEvent;
//    FProcessed: Boolean;

//    procedure DoAfterAppend(Item: TksArchiveItem); virtual;
//    procedure DoAfterExtract(Item: TksArchiveItem); virtual;
//    procedure DoBeforeAppend(Item: TksArchiveItem); virtual;
//    procedure DoBeforeExtract(Item: Pointer); virtual;
//    procedure EncryptBuffer(Sender: TObject; Buf: Pointer; BufSize: Integer);
//            virtual;
//    procedure EncryptDone; virtual;
//    procedure EncryptInit(Item: TksArchiveItem; AStream: TStream); virtual;
//    procedure ExtractItem(Item: TksArchiveItem; Reader: TksReader; Writer:
//            TksWriter); virtual; abstract;
//    function GetDateTime(TimeStamp: LongInt): TDateTime; virtual;
//    function GetTimeStamp(Age: TDateTime): LongInt; virtual;

//    procedure InternalExtract(Item: Pointer; Target: TStream);

//    procedure InternalTruncate(Writer: TksWriter);
//    procedure Lock;
//    procedure NeedPassword;
//    procedure SaveFooter(Writer: TksWriter); virtual;
//    procedure UnLock;

    property AfterAppend: TksArchiveEvent read FAfterAppend write FAfterAppend;
    property AfterExtract: TksArchiveEvent read FAfterExtract write
            FAfterExtract;
    property BeforeAppend: TksArchiveEvent read FBeforeAppend write
            FBeforeAppend;
    property BeforeExtract: TksArchiveEvent read FBeforeExtract write
            FBeforeExtract;
//    property ForcePasswords: Boolean read FForcePasswords write FForcePasswords;
//    property MaxAttempts: Integer read FMaxAttempts write FMaxAttempts;
//    property OnNeedPassword: TksPasswordEvent read FOnNeedPassword write
//            FOnNeedPassword;
//    property Password: string read FPassword write FPassword;
//    property Stream: TStream read FStream write FStream;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AppendFile(const ExtName: string; IntName: string = '');
    procedure AppendStream(const ExtName, IntName: string; Source: TStream);
    procedure ExtractFile(Index: Integer; const NewName: string = '');
    procedure ExtractStream(Index: Integer; Target: TStream);
    procedure Open(const FileName: string; Mode: Word);
    procedure OpenFromStream(AStream: TStream; OpenExisting: Boolean;
                                               FreeOnClose: Boolean = False);
    procedure Close;
    function GetItemInfo(var Item: TItemInfo; Index: Integer): Boolean;

    property ItemCount: Integer read GetItemCount;
    property Archive: TksArcHelper read FArchive;

    property OnItemProgress: TksProgressEvent read FOnItemProgress
                                             write FOnItemProgress;
  end;

  TksArcHelper = class
  private
    function GetFilePasswordString: string;
    procedure SetFilePasswordString(const Value: string);

  protected
    FStream: TStream;
    FFreeOnClose: Boolean;
    FFileName: string;        // имя файла архива;
    FStubSize: LongWord;      // для исполняемых файлов - размер (в байтах)
                              //   исполняемого заголовка
    FIsExecutable: Boolean;   // archive is executable file
    FLoaded: Boolean;
    FReadOnly: Boolean;
    FMultiVolume: Boolean;
    FIOBufSize: Integer;

    FCompressionMethod: Integer;
    FFileEncrypted: Boolean;
    FFilePassword: TBytes;


    FOnItemProgress: TksProgressEvent;
    FItemPos: Int64;
    FItemSize: Int64;

    FUpdating: Boolean;       // для ZIP-архивов флаг означает
                              //   отсутствие центрального каталога
                              //   после добавления файлов в архив

    procedure BeforeRead(Sender: TObject; Buf: Pointer; BufSize: Integer); virtual;
    procedure UpdateProgress(Count: Integer);

    property CompressionMethod: Integer read FCompressionMethod
                                        write FCompressionMethod;

    property FileEncrypted: Boolean read FFileEncrypted write FFileEncrypted;
    property FilePassword: TBytes read FFilePassword write FFilePassword;
    property FilePasswordString: string read GetFilePasswordString
                                       write SetFilePasswordString;

  public
    constructor Create(AStream: TStream; AFreeOnClose: Boolean);
    destructor Destroy; override;

    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;

    procedure AppendFile(const ExtName: string; IntName: string = '';
                        OnProgress: TksProgressEvent = nil); virtual;
    procedure AppendStream(const ExtName, IntName: string; Source: TStream;
                        OnProgress: TksProgressEvent = nil); virtual; abstract;

    procedure ExtractFile(Index: Integer; const ExtName: string;
                        OnProgress: TksProgressEvent = nil); virtual;
    procedure ExtractStream(Index: Integer; Target: TStream;
                        OnProgress: TksProgressEvent = nil); virtual; abstract;


    procedure Load; virtual; abstract;
    function ItemCount: Integer; virtual;
    function GetItemInfo(var Item: TksArchive.TItemInfo;
                             Index: Integer): Boolean; virtual;
  end;

implementation

uses ksZip;

{ TksArchive }

constructor TksArchive.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
//  FItemList:= TList.Create;
//  InitializeCriticalSection(FLock);
  FIOBufSize:= 16*1024;
//  FCheckCRC:= True;
//  FMaxAttempts:= 3;
end;

destructor TksArchive.Destroy;
begin
  Close;
//  ZipAllocEndOfCentralDirRec(0);
//  FItemList.Free;
//  DeleteCriticalSection(FLock);
  inherited Destroy;
end;

function TksArchive.GetItemCount: Integer;
begin
  if Assigned(FArchive) then Result:= FArchive.ItemCount
  else Result:= -1;
end;

function TksArchive.GetItemInfo(var Item: TItemInfo; Index: Integer): Boolean;
begin
  if Assigned(FArchive) then Result:= FArchive.GetItemInfo(Item, Index)
  else Result:= False;
end;

procedure TksArchive.Open(const FileName: string; Mode: Word);
var
  Stream: TStream;

begin
  Stream:= TFileStream.Create(FileName, Mode);
// Warning - sharing modes for CreateFile are supported only since Delphi 2010
  OpenFromStream(Stream, Mode and fmCreate <> fmCreate, True);
end;

procedure TksArchive.OpenFromStream(AStream: TStream; OpenExisting: Boolean;
                                                      FreeOnClose: Boolean);
begin
  FreeAndNil(FArchive);
// TODO: пока только ZIP-архивы
  FArchive:= TksZipArchive.Create(AStream, FreeOnClose);
  if OpenExisting then FArchive.Load;
end;

procedure TksArchive.Close;
begin
  FreeAndNil(FArchive);
end;

(*
function TksArchive.CreateItem(const FileName: string): Pointer;
var
  AnsiName: AnsiString;

begin
  AnsiName:= FileName;
  case FFileFormat of
    ZIP: begin
      Result:= AllocMem(SizeOf(TZipCentralDirFileHeader) + Length(AnsiName));
      with PZipCentralDirFileHeader(Result) do begin
        Signature:= ZipCentralDirFileHeaderSignature;
        VersionMadeBy:= ZipVersion;
        VersionToExtract:= ZipVersion;
//       Flags: Word;
//      CompressionMethod: Word;
//      LastModDateTime: LongWord;
//      CRC32: LongWord;
//      CompressedSize: LongWord;
//      UncompressedSize: LongWord;
        FileNameLen:= Length(AnsiName);
//      ExtraFieldLen: Word;
//      FileCommentLen: Word;
//      DiskNumberStart: Word;
//      InternalAttributes: Word;
//      ExternalAttributes: LongWord;
//      RelativeOffset: LongWord;

      end;
// TODO: Copy AnsiName into header

     end;
  end;
end;
*)
procedure TksArchive.AppendFile(const ExtName: string; IntName: string = '');
begin
  FArchive.AppendFile(ExtName, IntName, FOnItemProgress);
end;

procedure TksArchive.AppendStream(const ExtName, IntName: string; Source: TStream);
begin
  FArchive.AppendStream(ExtName, IntName, Source, FOnItemProgress);
end;

procedure TksArchive.ExtractFile(Index: Integer; const NewName: string);
var
  Info: TItemInfo;

begin
  if (NewName = '') and GetItemInfo(Info, Index) then begin
    FArchive.ExtractFile(Index, Info.FileName, FOnItemProgress);
  end
  else
    FArchive.ExtractFile(Index, NewName, FOnItemProgress);
end;

procedure TksArchive.ExtractStream(Index: Integer; Target: TStream);
begin
  FArchive.ExtractStream(Index, Target, FOnItemProgress);
end;

{
procedure TksArchive.AppendFromStream(Item: Pointer; AStream: TStream);
var
  Reader: TksReader;
  Writer: TksWriter;
  StartOffset: LongInt;
  Save: Integer;
begin
  if not FProcessed or FMultiVolume then
    raise EksArchiveError.Create(aeCannotAppend);
  Lock;
  try
    Writer:= TksWriter.Create(FStream, FIOBufSize);
    try
      Writer.Position:= FFooterOffset;
      Item.FHeaderOffset:= FFooterOffset;
      Item.SaveHeader(Writer);
  //    Item.FDataOffset:= Writer.Position;
      StartOffset:= Writer.Position;
      Writer.FlushBuffer;
      Save:= AStream.Position;
      EncryptInit(Item, AStream);
      AStream.Position:= Save;
      try
        DoBeforeAppend(Item);
        try
          Reader:= TksReader.Create(AStream, FIOBufSize);
          try
            Reader.BeforeFlush:= BeforeFlush;    // calc CRC of uncompressed data
            Writer.BeforeFlush:= EncryptBuffer;  // encrypt compressed data
            CRCInit;
            FItemPos:= 0;
            FItemSize:= Item.FFileSize;
            FOnItemProgress:= OnProgress;
            AppendItem(Item, Reader, Writer);
            Writer.FlushBuffer;              // make sure all encrypted
          finally
            Writer.BeforeFlush:= nil;        // stop encryption
            Reader.Free;                     // CRC is ready now
            CRCDone;
          end;
        except
          Writer.Position:= FFooterOffset;   // restore archive footer
          InternalTruncate(Writer);
          raise;
        end;
        CRCStore(Item);
  //      Item.FCRC:= FCRC;
        Item.FCompressedSize:= Writer.Position - StartOffset;
        Item.SaveFooter(Writer);
        FItemList.Add(Item);
        FFooterOffset:= Writer.Position;
        SaveFooter(Writer);
        if Item.UpdateLocalHeader then begin
          Writer.Position:= Item.FHeaderOffset;
  //          if Assigned(FCrypter) then Item.SetCryptoHeader(FCrypter, FPassword);
          Item.SaveHeader(Writer);
        end;
        Writer.FlushBuffer;
        DoAfterAppend(Item);
      finally
        EncryptDone;
      end;
    finally;
      Writer.Free;
    end;
  finally
    UnLock;
  end;
end;
}
{
procedure TksArchive.BeforeFlush(Sender: TObject; Buf: Pointer; BufSize:
        Integer);
var
  I: Integer;
  Stop: Boolean;
begin
  CRCUpdate(Buf, BufSize);
  if Assigned(FOnItemProgress) then begin
    Inc(FItemPos, BufSize);
    I:= Percentage(FItemPos, FItemSize);
    Stop:= False;
    FOnItemProgress(Self, I, Stop);
    if Stop then raise EksArchiveError.Create(aeUserAbort);
  end;
end;
}
{
procedure TksArchive.DoAfterAppend(Item: TksArchiveItem);
begin
  if Assigned(FAfterAppend) then FAfterAppend(Self, Item);
end;

procedure TksArchive.DoAfterExtract(Item: TksArchiveItem);
begin
  if Assigned(FAfterExtract) then FAfterExtract(Self, Item);
end;

procedure TksArchive.DoBeforeAppend(Item: TksArchiveItem);
begin
  if Assigned(FBeforeAppend) then FBeforeAppend(Self, Item);
end;

procedure TksArchive.DoBeforeExtract(Item: TksArchiveItem);
begin
  if Assigned(FBeforeExtract) then FBeforeExtract(Self, Item);
end;

procedure TksArchive.EncryptBuffer(Sender: TObject; Buf: Pointer; BufSize:
        Integer);
begin
end;

procedure TksArchive.EncryptDone;
begin
end;

procedure TksArchive.EncryptInit(Item: TksArchiveItem; AStream: TStream);
begin
end;
}
{
procedure TksArchive.ExtractToFile(Index: Integer; const NewName: string;
        OnProgress: TksProgressEvent = nil);
var
  FileName: string;
  Stream: TFileStream;
begin
  if Length(NewName) <> 0 then FileName:= NewName
  else FileName:= TksArchiveItem(FItemList[Index]).FileName;
  Stream:= TFileStream.Create(FileName, fmCreate);
  try
    try
      ExtractToStream(Index, Stream, OnProgress);
// whatever internal archive timestamp is, set file date as OS timestamp.
      FileSetDate(Stream.Handle,
        DateTimeToFileDate(TksArchiveItem(FItemList[Index]).FileDate));
    finally
      Stream.Free;
    end;
    FileSetAttr(FileName, TksArchiveItem(FItemList[Index]).FileAttr);
  except
    DeleteFile(FileName);
    raise;
  end;
end;

procedure TksArchive.ExtractToStream(Index: Integer; AStream: TStream;
        OnProgress: TksProgressEvent = nil);
begin
  InternalExtract(FItemList[Index], AStream, OnProgress);
end;

function TksArchive.GetCount: Integer;
begin
  Result:= FItemList.Count;
end;

function TksArchive.GetDateTime(TimeStamp: LongInt): TDateTime;
begin
  Result:= FileDateToDateTime(TimeStamp);
end;

function TksArchive.GetItem(Index: Integer): TksArchiveItem;
begin
  Result:= FItemList[Index];
end;
}
{
function TksArchive.GetTimeStamp(Age: TDateTime): LongInt;
begin
  Result:= DateTimeToFileDate(Age);
end;

procedure TksArchive.InternalExtract(Item: Pointer; Target: TStream);
// for ZIP archive Item is PZipLocalFileHeader

var
  Reader: TksReader;
  Writer: TksWriter;
  L: Int64;
  NullStream: Boolean;
begin
  NullStream:= Target = nil;
  try
    if NullStream then Target:= TksNullStream.Create;
//    Lock;
//    try
    DoBeforeExtract(Item);
    Writer:= TksWriter.Create(Target, IOBufSize);
    try
      L:= Writer.Position;
      CRCInit;
      Writer.BeforeFlush:= BeforeFlush;  // CRC of decompressed data
      DecryptInit(Item);                 // Exception if password invalid
      try
        Reader:= TksReader.Create(FStream, FIOBufSize);
        try
          Reader.AfterFlush:= DecryptBuffer;
          FItemPos:= 0;
          case FFileFormat of
            ZIP: begin
              FItemSize:= PZipCentralDirFileHeader(Item)^.UncompressedSize;
              case PZipCentralDirFileHeader(Item)^.CompressionMethod of
                ZIP_Store: CopyFiler(Reader, Writer, FItemSize);
                ZIP_Shrink: UnShrink(Reader, Writer, FItemSize);
// TODO:                ZIP_Deflate: Inflate(Reader, Writer);//, Item.FileSize);
              else
                raise EksArchiveError.Create(EksArchiveError.BadMethod);
              end;
            end;
          else
            raise EksArchiveError.Create(EksArchiveError.BadFormat);
          end;
        finally
          Reader.Free;
        end;
      finally
        DecryptDone;
      end;
    finally
      L:= Writer.Position - L;
      Writer.Free;
      CRCDone;    // called after last buffer flushed
    end;
      Item.LoadFooter(FStream);
      if CheckCRC and not CRCValid(Item) then
        raise EksArchiveError.Create(aeBadCRC);
      if CheckSize and (L <> Item.FFileSize) then
        raise EksArchiveError.Create(aeBadSize);
    DoAfterExtract(Item);
//    finally
//      UnLock;
//    end;
  finally
    if NullStream then Target.Free;
  end;
end;

procedure TksArchive.Lock;
begin
  EnterCriticalSection(FLock);
end;

procedure TksArchive.NeedPassword;
begin
  if Assigned(FOnNeedPassword) then FOnNeedPassword(Self, FPassword);
end;

}
{ TksArcHelper }

constructor TksArcHelper.Create(AStream: TStream; AFreeOnClose: Boolean);
begin
  FStream:= AStream;
  FFreeOnClose:= AFreeOnClose;
  FIOBufSize:= 1024 * 8;
end;

destructor TksArcHelper.Destroy;
begin
  if FFreeOnClose then FStream.Free;
  inherited;
end;

function TksArcHelper.GetFilePasswordString: string;
begin
  Result:= BytesToString(FFilePassword);
end;

procedure TksArcHelper.SetFilePasswordString(const Value: string);
begin
  FFilePassword:= StringToBytes(Value);
end;

procedure TksArcHelper.BeginUpdate;
begin
  FUpdating:= True;
end;

procedure TksArcHelper.EndUpdate;
begin
  FUpdating:= False;
end;

function TksArcHelper.GetItemInfo(var Item: TksArchive.TItemInfo;
                                      Index: Integer): Boolean;
begin
  Result:= False;
end;

function TksArcHelper.ItemCount: Integer;
begin
// -1 means feature is not supported
  Result:= -1;
end;

procedure TksArcHelper.AppendFile(const ExtName: string; IntName: string;
                                  OnProgress: TksProgressEvent);
var
  Source: TStream;

begin
  if IntName = '' then IntName:= ExtractFileName(ExtName);
  Source:= TFileStream.Create(ExtName, fmOpenRead or fmShareDenyWrite);
  try
    AppendStream(ExtName, IntName, Source, OnProgress);
  finally
    Source.Free;
  end;
end;

procedure TksArcHelper.ExtractFile(Index: Integer; const ExtName: string;
                                   OnProgress: TksProgressEvent);
var
  Target: TStream;

begin
// Sharing for fmCreate is supported only since Delphi 2010
  Target:= TFileStream.Create(ExtName, fmCreate or fmShareDenyWrite);
  try
    ExtractStream(Index, Target, OnProgress);
  finally
    Target.Free;
  end;
end;

procedure TksArcHelper.UpdateProgress(Count: Integer);
var
  I: Integer;
  Stop: Boolean;

begin
  if Assigned(FOnItemProgress) then begin
    Inc(FItemPos, Count);
    I:= Percentage64(FItemPos, FItemSize);
    Stop:= False;
    if Assigned(FOnItemProgress) then begin
      FOnItemProgress(Self, I, Stop);
    end;
    if Stop then Abort;
  end;
end;

procedure TksArcHelper.BeforeRead(Sender: TObject; Buf: Pointer; BufSize: Integer);
var
  I: Integer;
  Stop: Boolean;
begin
  if Assigned(FOnItemProgress) then begin
    Inc(FItemPos, BufSize);

// TODO: 64-bit
    I:= Percentage(FItemPos, FItemSize);
    Stop:= False;
    FOnItemProgress(Self, I, Stop);
    if Stop then Abort;
  end;
end;

end.
