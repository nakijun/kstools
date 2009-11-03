{ *********************************************************** }
{ *                    ksTools Library                      * }
{ *       Copyright (c) Sergey Kasandrov 1997, 2009         * }
{ *       -----------------------------------------         * }
{ *      Blog: http://sergworks.wordpress.com/kstools       * }
{ *********************************************************** }

unit ksClasses;

{$I ksTools.inc}

interface

uses
  Windows;

type
  TksRingBuffer = class
  private
    FLock: TRTLCriticalSection;
    FBuffer: PByte;
    FSize: Integer;
    FOrigin: Integer;
    FLength: Integer;
  public
    constructor Create(ASize: Integer);
    destructor Destroy; override;
    function Read(var Buf; Count: Integer): Integer;
    function Write(const Buf; Count: Integer): Integer;
    procedure Clear;
    function Length: Integer;

    property Size: Integer read FSize;
  end;

implementation

{ TksRingBuffer }

constructor TksRingBuffer.Create(ASize: Integer);
begin
  GetMem(FBuffer, ASize);
  FSize:= ASize;
  InitializeCriticalSection(FLock);
end;

destructor TksRingBuffer.Destroy;
begin
  DeleteCriticalSection(FLock);
  FreeMem(FBuffer, FSize);
  inherited Destroy;
end;

procedure TksRingBuffer.Clear;
begin
  EnterCriticalSection(FLock);
  FLength:= 0;
  FOrigin:= 0;
  LeaveCriticalSection(FLock);
end;

function TksRingBuffer.Length: Integer;
begin
  EnterCriticalSection(FLock);
  Result:= FLength;
  LeaveCriticalSection(FLock);
end;

function TksRingBuffer.Read(var Buf; Count: Integer): Integer;
var
  P1, P2: PByte;
  N: Integer;

begin
  EnterCriticalSection(FLock);
  try
    if Count > FLength then Count:= FLength;
    if Count > 0 then begin
      Dec(FLength, Count);
{$IFDEF OldVersion}
      P1:= @PChar(FBuffer)[FOrigin];
{$ELSE}
      P1:= @FBuffer[FOrigin];
{$ENDIF}
      P2:= @Buf;
      if FOrigin + Count <= FSize then begin
        Move(P1^, P2^, Count);
        Inc(FOrigin, Count);
        if (FOrigin = FSize) then FOrigin:= 0;
      end
      else begin
        N:= FSize - FOrigin;
        Move(P1^, P2^, N);
        Inc(P2, N);
        P1:= FBuffer;
        N:= Count - N;
        Move(P1^, P2^, N);
        FOrigin:= N;
      end;
    end;
    Result:= Count;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TksRingBuffer.Write(const Buf; Count: Integer): Integer;
var
  P1, P2: PByte;
  Org, N: Integer;

begin
  Result:= 0;
//  if (Count <= 0) then Exit;
  EnterCriticalSection(FLock);
  try
    if FLength < FSize then begin
      if FLength = 0 then FOrigin:= 0;
      if Count > FSize - FLength then Count:= FSize - FLength;
      Org:= FOrigin + FLength;
      if Org >= FSize then Org:= Org - FSize;
      Inc(FLength, Count);
      P1:= @Buf;
{$IFDEF OldVersion}
      P2:= @PChar(FBuffer)[Org];
{$ELSE}
      P2:= @FBuffer[Org];
{$ENDIF}
      if Org + Count <= FSize then Move(P1^, P2^, Count)
      else begin
        N:= FSize - Org;
        Move(P1^, P2^, N);
        Inc(P1, N);
        P2:= FBuffer;
        Move(P1^, P2^, Count - N);
      end;
      Result:= Count;
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

end.
