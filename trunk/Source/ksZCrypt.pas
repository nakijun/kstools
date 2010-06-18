{ *********************************************************** }
{ *                    ksTools Library                      * }
{ *       Copyright (c) Sergey Kasandrov 1997, 2010         * }
{ *       -----------------------------------------         * }
{ *         http://sergworks.wordpress.com/kstools          * }
{ *********************************************************** }

unit ksZCrypt;

interface

uses
  SysUtils, Windows, Classes, ksUtils, ksClasses;

type
  TksZipCrypto = record
  public type
    THeader = packed array[0..11] of Byte;
  private
    FKeys: packed array [0..2] of LongWord;

    function CryptoByte: Byte;
    procedure UpdateKeys(B: Byte);
  public
    function DecodeByte(B: Byte): Byte;
    function EncodeByte(B: Byte): Byte;
    procedure Decode(var Data; Count: Integer);
    procedure Encode(var Data; Count: Integer);
    procedure Init(const Password: TBytes);
    function CheckHeader(const Header: TksZipCrypto.THeader;
         const Password: TBytes; CRC: LongWord; CheckSize: Byte): Boolean;
    procedure InitHeader(var Header: TksZipCrypto.THeader;
         const Password: TBytes; CRC: LongWord; CheckSize: Byte);
  end;

implementation

{
  TksZipCrypto implements traditional PKWARE encryption algorithm;
  to encrypt data according to PKZIP 2.0 specification:
  - calculate CRC32 of data to be encrypted;
  - initialize 12-byte encryption header using Password and
    calculated CRC32 value by calling TksZipCrypto.InitHeader;
    this method also initializes keys needed for encryption;
  - encrypt data by calling TksZipCrypto.Encode;

  to decrypt data according to PKZIP 2.0 specification:
  - make sure encryption header corresponds to Password and CRC32
    by calling TksZipCrypto.CheckHeader;
    this method also initializes keys needed for decryption;
  - decrypt data by calling TksZipCrypto.Decode;

  A test version of encryption/decryption (without using
  encryption header and CRC32) can be implemented as follows:
  to encrypt data you must:
  - initialize keys by password (call TksZipCrypto.Init);
  - encrypt data by calling TksZipCrypto.Encode;

  to decrypt data you must:
  - initialize keys by password (call TksZipCrypto.Init);
  - decrypt data by calling TksZipCrypto.Decode;

  see also ZipEncryptStream/ZipDecryptStream code as example
}

{  FROM APPNOTE.TXT Version 6.2.0:

XII. Traditional PKWARE Encryption
----------------------------------

The following information discusses the decryption steps
required to support traditional PKWARE encryption.  This
form of encryption is considered weak by today's standards
and its use is recommended only for situations with
low security needs or for compatibility with older .ZIP
applications.

XIII. Decryption
----------------

The encryption used in PKZIP was generously supplied by Roger
Schlafly.  PKWARE is grateful to Mr. Schlafly for his expert
help and advice in the field of data encryption.

PKZIP encrypts the compressed data stream.  Encrypted files must
be decrypted before they can be extracted.

Each encrypted file has an extra 12 bytes stored at the start of
the data area defining the encryption header for that file.  The
encryption header is originally set to random values, and then
itself encrypted, using three, 32-bit keys.  The key values are
initialized using the supplied encryption password.  After each byte
is encrypted, the keys are then updated using pseudo-random number
generation techniques in combination with the same CRC-32 algorithm
used in PKZIP and described elsewhere in this document.

The following is the basic steps required to decrypt a file:

1) Initialize the three 32-bit keys with the password.
2) Read and decrypt the 12-byte encryption header, further
   initializing the encryption keys.
3) Read and decrypt the compressed data stream using the
   encryption keys.


Step 1 - Initializing the encryption keys
-----------------------------------------

Key(0) <- 305419896
Key(1) <- 591751049
Key(2) <- 878082192

loop for i <- 0 to length(password)-1
    update_keys(password(i))
end loop


Where update_keys() is defined as:


update_keys(char):
  Key(0) <- crc32(key(0),char)
  Key(1) <- Key(1) + (Key(0) & 000000ffH)
  Key(1) <- Key(1) * 134775813 + 1
  Key(2) <- crc32(key(2),key(1) >> 24)
end update_keys


Where crc32(old_crc,char) is a routine that given a CRC value and a
character, returns an updated CRC value after applying the CRC-32
algorithm described elsewhere in this document.


Step 2 - Decrypting the encryption header
-----------------------------------------

The purpose of this step is to further initialize the encryption
keys, based on random data, to render a plaintext attack on the
data ineffective.


Read the 12-byte encryption header into Buffer, in locations
Buffer(0) thru Buffer(11).

loop for i <- 0 to 11
    C <- buffer(i) ^ decrypt_byte()
    update_keys(C)
    buffer(i) <- C
end loop


Where decrypt_byte() is defined as:


unsigned char decrypt_byte()
    local unsigned short temp
    temp <- Key(2) | 2
    decrypt_byte <- (temp * (temp ^ 1)) >> 8
end decrypt_byte


After the header is decrypted,  the last 1 or 2 bytes in Buffer
should be the high-order word/byte of the CRC for the file being
decrypted, stored in Intel low-byte/high-byte order, or the high-order
byte of the file time if bit 3 of the general purpose bit flag is set.
Versions of PKZIP prior to 2.0 used a 2 byte CRC check; a 1 byte CRC check is
used on versions after 2.0.  This can be used to test if the password
supplied is correct or not.


Step 3 - Decrypting the compressed data stream
----------------------------------------------

The compressed data stream can be decrypted as follows:


loop until done
    read a character into C
    Temp <- C ^ decrypt_byte()
    update_keys(temp)
    output Temp
end loop
}

const
  ZipKey0 = 305419896;
  ZipKey1 = 591751049;
  ZipKey2 = 878082192;
  ZipMagicNumber = 134775813;

type
  TByteArray = packed array [0..$3FFFFFFF] of Byte;
  TBytes4 = packed array [0..3] of Byte;

function TksZipCrypto.CryptoByte: Byte;
var
  Temp: Word;

begin
  Temp:= Word(FKeys[2]) or 2;
  Result:= (Temp * (Temp xor 1)) shr 8;
end;

function TksZipCrypto.DecodeByte(B: Byte): Byte;
begin
  Result:= B xor CryptoByte;
  UpdateKeys(Result);
end;

procedure TksZipCrypto.Decode(var Data; Count: Integer);
var
  I: Integer;

begin
  for I:= 0 to Pred(Count) do begin
    TByteArray(Data)[I]:= TByteArray(Data)[I] xor CryptoByte;
    UpdateKeys(TByteArray(Data)[I]);
  end;
end;

function TksZipCrypto.EncodeByte(B: Byte): Byte;
begin
  Result:= B xor CryptoByte;
  UpdateKeys(B);
end;

procedure TksZipCrypto.Encode(var Data; Count: Integer);
var
  I: Integer;
  J: Byte;

begin
  for I:= 0 to Pred(Count) do begin
    J:= CryptoByte;
    UpdateKeys(TByteArray(Data)[I]);
    TByteArray(Data)[I]:= TByteArray(Data)[I] xor J;
  end;
end;

procedure TksZipCrypto.Init(const Password: TBytes);
var
  I: Integer;

begin
  FKeys[0]:= ZipKey0;
  FKeys[1]:= ZipKey1;
  FKeys[2]:= ZipKey2;
  for I:= 0 to Length(Password) - 1 do UpdateKeys(Ord(Password[I]));
end;

procedure TksZipCrypto.UpdateKeys(B: Byte);
begin
  FKeys[0]:= Crc32OfByte(B, FKeys[0]);
  FKeys[1]:= FKeys[1] + (FKeys[0] and $FF);
  FKeys[1]:= (FKeys[1] * ZipMagicNumber) + 1;
  FKeys[2]:= Crc32OfByte(FKeys[1] shr 24, FKeys[2]);
end;

function TksZipCrypto.CheckHeader(const Header: TksZipCrypto.THeader;
         const Password: TBytes; CRC: LongWord; CheckSize: Byte): Boolean;
var
  LHeader: TksZipCrypto.THeader;

begin
  Init(Password);
  LHeader:= Header;
  Decode(LHeader, 12);
  if CheckSize = 1 then                   // version 2.0 or better
    Result:= (LHeader[11] = TBytes4(CRC)[3])
  else if CheckSize = 2 then              // prior to version 2.0
    Result:= (LHeader[11] = TBytes4(CRC)[3]) and
             (LHeader[10] = TBytes4(CRC)[2])
  else begin                              // not used by zip archive
    Result:= True;
    Exit;
  end;
end;

procedure TksZipCrypto.InitHeader(var Header: TksZipCrypto.THeader;
          const Password: TBytes; CRC: LongWord; CheckSize: Byte);
var
  N: Integer;
  B: Byte;
  T: Word;

begin
  Init(Password);                   // initialize Header[0..9]
  for N:= 0 to 9 do begin           //   with random values
    B:= Random(256);
    T:= CryptoByte;
    UpdateKeys(B);
    Header[N]:= T xor B;
  end;
                                    // Header[10..11] = CRC32 higher word
  if CheckSize = 2 then begin
    Header[10]:= TBytes4(CRC)[2];
  end
  else begin
    Header[10]:= Random(256);
  end;
  Header[11]:= TBytes4(CRC)[3];
  Init(Password);                   // encrypt Header
  Encode(Header, 12)
end;

initialization
  Randomize;
end.
