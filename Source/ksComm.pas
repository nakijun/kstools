{ *********************************************************** }
{ *                    ksTools Library                      * }
{ *       Copyright (c) Sergey Kasandrov 1997, 2009         * }
{ *       -----------------------------------------         * }
{ *      Blog: http://sergworks.wordpress.com/kstools       * }
{ *********************************************************** }

unit ksComm;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, ksClasses;

const
{ Event type }
  etCommEvent     = 0;
  etCommError     = 1;
  etCommDataReady = 2;
  etWriteBufEmpty = 3;
  etReadOverflow  = 4;
  etFatalError    = 5;

const
  DefaultCommTimeouts: TCommTimeouts = (
    ReadIntervalTimeout: MAXDWORD;
    ReadTotalTimeoutMultiplier: 0;
    ReadTotalTimeoutConstant: 0;
    WriteTotalTimeoutMultiplier: 0;
    WriteTotalTimeoutConstant: 0);

  DefaultEventMask = EV_CTS + EV_DSR + EV_RLSD + EV_RING +
                     {EV_RXCHAR +} EV_ERR + EV_BREAK;

type
  TksMessageEvent = procedure(Sender: TObject; var Msg: TMessage) of object;

  TksCommStateEvent = procedure(Sender: TObject; var DCB: TDCB) of object;
  TksCommTimeoutsEvent = procedure(Sender: TObject; var Timeouts: TCommTimeouts) of object;
  TksCommMaskEvent = procedure(Sender: TObject; var Mask: LongWord) of object;
  TksCommEvent = procedure(Sender: TObject; Value: LongWord) of object;

  TksCommParity = (cpNone, cpOdd, cpEven, cpMark, cpSpace);
  TksCommStopBits = (sbOneBit, sbOne5Bits, sbTwoBits);
  TksCommDTRControl = (cdDisable, cdEnable, cdHandShake);
  TksCommRTSControl = (crDisable, crEnable, crHandShake, crToggle);

  TksComPort = class(TComponent)
  private
    FWndHandle: HWND;
    FPortName: string;
    FPortHandle: THandle;
    FRBuffer: TksRingBuffer;
    FWBuffer: TksRingBuffer;

    FRXEvent: THandle;          // сигнал потоку чтения
                                //   от потока мониторинга
    FWriteEvent: THandle;       // сигнал потоку записи
                                //   от главного потока
    FExitEvent: THandle;        // сигнал завершения фоновых потоков

    FReadThread: TThread;
    FWriteThread: TThread;
    FEventThread: TThread;

    FEventMask: LongWord;       // маска эвентов, о которых поток мониторинга
                                //  оповещает главный поток
    FErrorMask: LongWord;

    FBaudRate: LongWord;
    FByteSize: Byte;
    FParity: TksCommParity;
    FStopBits: TksCommStopBits;
    FRBufSize: Integer;
    FWBufSize: Integer;

    FParityCheck: Boolean;
    FCTSControl: Boolean;
    FDSRControl: Boolean;
    FDSRSensitivity: Boolean;
    FDTRControl: TksCommDTRControl;
    FRTSControl: TksCommRTSControl;

    FOnMessage: TksMessageEvent;
    FOnError: TksCommEvent;
    FOnEvent: TksCommEvent;
    FOnRead: TksCommEvent;
    FOnReadOverflow: TksCommEvent;
    FOnWrite: TksCommEvent;
    FOnFatalError: TksCommEvent;

//    FOnGetCommState: TksCommStateEvent;
    FOnSetCommState: TksCommStateEvent;
    FOnSetCommTimeouts: TksCommTimeoutsEvent;
    FOnSetCommMask: TksCommMaskEvent;

    const MsgID = WM_APP + 11;
    procedure WndProc(var Msg: TMessage);

    procedure SetRBufSize(Value: Integer);
    procedure SetWBufSize(Value: Integer);
    procedure SetBaudRate(const Value: LongWord);
    procedure SetByteSize(const Value: Byte);
    procedure SetParity(const Value: TksCommParity);
    procedure SetStopBits(const Value: TksCommStopBits);
    procedure SetRTSControl(const Value: TksCommRTSControl);
    procedure SetDTRControl(const Value: TksCommDTRControl);
    procedure SetDSRSensitivity(const Value: Boolean);
    procedure SetParityCheck(const Value: Boolean);
    procedure SetCTSControl(const Value: Boolean);
    procedure SetDSRControl(const Value: Boolean);

    procedure UpdateDCB(var DCB: TDCB);
    procedure UpdateProperties(const DCB: TDCB);
  protected
    procedure HandleMessage(Msg: TMessage);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Open: Boolean;
    procedure Close;
    function Active: Boolean;
    function GetPortState(var DCB: TDCB): Boolean;
    function GetPortTimeouts(var Timeouts: TCommTimeouts): Boolean;
    function GetPortMask(var Mask: LongWord): Boolean;
    function SetPortState(const DCB: TDCB): Boolean;
    function Read(var Buf; Count: Integer): Integer;
    function Write(var Buf; Count: Integer): Integer;
  published
    { Published declarations }
    property PortName: string read FPortName write FPortName;
    property RBufSize: Integer read FRBufSize write SetRBufSize default 4096;
    property WBufSize: Integer read FWBufSize write SetWBufSize default 4096;

    property BaudRate: LongWord read FBaudRate write SetBaudRate default 9600;
    property ByteSize: Byte read FByteSize write SetByteSize default 8;
    property Parity: TksCommParity read FParity write SetParity default cpNone;
    property StopBits: TksCommStopBits read FStopBits write SetStopBits default sbOneBit;
    property ParityCheck: Boolean read FParityCheck write SetParityCheck default False;
    property CTSControl: Boolean read FCTSControl write SetCTSControl default False;
    property DSRControl: Boolean read FDSRControl write SetDSRControl default False;
    property DTRControl: TksCommDTRControl read FDTRControl write SetDTRControl default cdDisable;
    property RTSControl: TksCommRTSControl read FRTSControl write SetRTSControl default crDisable;
    property DSRSensitivity: Boolean read FDSRSensitivity write SetDSRSensitivity default False;

//    property ReadFileName: string read FReadName write FReadName;

    property OnMessage: TksMessageEvent read FOnMessage write FOnMessage;
    property OnReadOverflow: TksCommEvent read FOnReadOverflow write FOnReadOverflow;
    property OnError: TksCommEvent read FOnError write FOnError;
    property OnEvent: TksCommEvent read FOnEvent write FOnEvent;
    property OnRead: TksCommEvent read FOnRead write FOnRead;
    property OnWrite: TksCommEvent read FOnWrite write FOnWrite;
    property OnFatalError: TksCommEvent read FOnFatalError write FOnFatalError;
//    property OnGetCommState: TksCommStateEvent read FOnGetCommState write FOnGetCommState;
    property OnSetCommState: TksCommStateEvent read FOnSetCommState write FOnSetCommState;
    property OnSetCommTimeouts: TksCommTimeoutsEvent read FOnSetCommTimeouts write FOnSetCommTimeouts;
    property OnSetCommMask: TksCommMaskEvent read FOnSetCommMask write FOnSetCommMask;
  end;

implementation

type
  TCommThread = class(TThread)
  protected
    FOwner: TksComPort;
//    FInternalError: Integer;
    procedure PortMessage(Code, SubCode: Integer);
  public
    constructor Create(AOwner: TksComPort);
//    property InternalError: Integer read FInternalError;
  end;

  TCommEventThread = class(TCommThread)
  protected
    procedure Execute; override;
  end;

  TCommReadThread = class(TCommThread)
  protected
    procedure Execute; override;
  end;

  TCommWriteThread = class(TCommThread)
  protected
    procedure Execute; override;
  end;

{ TCommThread }

constructor TCommThread.Create(AOwner: TksComPort);
begin
  FOwner:= AOwner;
  inherited Create(False);
end;

procedure TCommThread.PortMessage(Code, SubCode: Integer);
begin
  if not Terminated then
    PostMessage(FOwner.FWndHandle, FOwner.MsgID, Code, SubCode);
end;

{ TCommEventThread }

procedure TCommEventThread.Execute;
var
  OL: TOverlapped;
  EvtMask: DWORD;
  Junk: DWORD;
  OK: Boolean;
//  LastEvent: LongWord;
  H: array[0..1] of THandle;

begin
//  LastEvent:= 0;
  try
    FillChar(OL, SizeOf(OL), 0);
    OL.hEvent:= CreateEvent(nil,
                            True,     // !!! - manual reset
                            False,    // initial state = not signaled
                            nil);
    H[0]:= OL.hEvent;
    H[1]:= FOwner.FExitEvent;
    try
      while not Terminated do begin
        OK:= WaitCommEvent(FOwner.FPortHandle, EvtMask, @OL);
{
  Overlapped WaitCommEvent returns False immediately;
  checking WaitCommEvent result here is not required
}
        if not OK and (GetLastError = ERROR_IO_PENDING) then begin
{
  Wait for CommEvent to occur or ExitEvent signalled
}
          case WaitForMultipleObjects(2, @H, False, INFINITE) of

            WAIT_OBJECT_0: begin
              OK:= GetOverLappedResult(FOwner.FPortHandle, OL, Junk, False);
              ResetEvent(OL.hEvent);
            end;

            WAIT_OBJECT_0 + 1: begin
              Exit;
            end;

          end; { case }
        end;

        if OK then begin
          if {(EvtMask <> LastEvent) and} (EvtMask and FOwner.FEventMask <> 0) then begin
{ Inform main thread }
            PortMessage(etCommEvent, LPARAM(EvtMask));
//            LastEvent:= EvtMask;
          end;
{ Signal com event to read thread}
          if (EvtMask and EV_RXCHAR <> 0) and (FOwner.FRXEvent <> 0)
            then SetEvent(FOwner.FRXEvent);
        end
        else begin
{ Port closed or other fatal condition, just exit the thread }
//          FInternalError:= 2;
          PortMessage(etFatalError, 1);
          Exit;
        end;

      end; { while }

    finally
      CloseHandle(OL.hEvent);
    end;

  except
    on E:Exception do begin
//      FInternalError:= 1;
      PortMessage(etFatalError, 2);
//      raise;
    end;
  end;
end;

{ TCommReadThread }

procedure TCommReadThread.Execute;
const
  DefaultTimeout = 50;

var
  ReadOL: TOverlapped;
  H: packed array[0..1] of THandle;
  OK: Bool;
  ReadCount: DWORD;
  Errors: DWORD;
  Stat: TComStat;
  InBuf: packed array[0..511] of AnsiChar;//Byte;
  BytesToRead: LongWord;
  BytesLost: LongWord;
  WaitResult: Cardinal;

//  LastError: LongWord;

  Timeout: LongWord;    // Specifies the time-out interval, in milliseconds.

begin
//  LastError:= 0;
  try
    FillChar(ReadOL, SizeOf(ReadOL), 0);
    ReadOL.hEvent:= CreateEvent(nil, True, False, nil);   // !!! manual reset
    Timeout:= DefaultTimeout;
    try
      while not Terminated do begin
{
  Wait for FReadEvent signalled from Comm Event Thread (EV_RXCHAR detected)
        or FWriteEvent signalled from main thread (write requested)
  We expect these events to happen, but we don't rely on them;
    if nothing happens for 50 milliseconds after last event, thread wakes up,
    checks port state and performs Read, if necessary.
}
//        WaitForMultipleObjects(FOwner.FReadEvent, 50);

        H[0]:= FOwner.FRXEvent;
        H[1]:= FOwner.FExitEvent;
        WaitResult:= WaitForMultipleObjects(2, @H, False, Timeout);

        case WaitResult of

// RX Event signalled
          WAIT_OBJECT_0: begin
            ResetEvent(FOwner.FRXEvent);
            Timeout:= DefaultTimeout;
          end;

// Exit Event signalled
          WAIT_OBJECT_0 + 1: Exit;

// Timeout
          WAIT_TIMEOUT: Timeout:= {DefaultTimeout;  //} INFINITE;

// Unknown
        else
//          FInternalError:= 5;
          PortMessage(etFatalError, $101);
          Exit;
        end;


        while ClearCommError(FOwner.FPortHandle, Errors, @Stat) do begin
          Errors:= Errors and FOwner.FErrorMask;
          if (Errors <> 0) {and (Errors <> LastError)} then begin
            PortMessage(etCommError, Errors);
          end;
//            LastError:= Error;

          if Stat.cbInQue = 0 then Break;

          Timeout:= DefaultTimeout;

          BytesToRead:= Stat.cbInQue;
          if BytesToRead > SizeOf(InBuf) then BytesToRead:= SizeOf(InBuf);
          OK:= ReadFile(FOwner.FPortHandle,          {handle}
                        InBuf,                       {buffer}
                        BytesToRead,                 {bytes to read}
                        ReadCount,                   {bytes read}
                        @ReadOL);                    {overlap record}

          if not OK then begin
            if GetLastError = ERROR_IO_PENDING then begin
              H[0]:= ReadOL.hEvent;
              H[1]:= FOwner.FExitEvent;
              WaitResult:= WaitForMultipleObjects(2, @H, False, INFINITE);
              case WaitResult of

// Read completed
                WAIT_OBJECT_0: begin
                  if GetOverlappedResult(FOwner.FPortHandle,  {handle}
                                         ReadOL,              {overlap record}
                                         ReadCount,           {bytes read}
                                         False)               {do not wait}
                  then begin
                    ResetEvent(ReadOL.hEvent);
                    OK:= True;
                  end;
                end;

// Exit signalled
                WAIT_OBJECT_0 + 1: Exit;

// Unknown
              else
//                FInternalError:= 5;
                PortMessage(etFatalError, $102);
                Exit;
              end; {case}

            end;
          end;

          if OK and (ReadCount > 0) then begin
//                  if Assigned(FOwner.FReadLog) then
//                    FOwner.FReadLog.Write(InBuf, ReadCount);
            BytesLost:= ReadCount - LongWord(FOwner.FRBuffer.Write(InBuf, ReadCount));
            if BytesLost <> 0 then PortMessage(etReadOverflow, BytesLost);
//        Dec(Stat.cbInQue, ReadCount);
            PortMessage(etCommDataReady, ReadCount);
          end;

        end; {while}

      end;
    finally
      CloseHandle(ReadOL.hEvent);
    end;
  except
    on E:Exception do begin
//      FInternalError:= 1;
      PortMessage(etFatalError, $103);
      raise;
    end;
  end;
end;

{ TCommWriteThread }

procedure TCommWriteThread.Execute;
var
  WriteOL: TOverlapped;
  H: packed array[0..1] of THandle;
  OK: Bool;
  WriteCount: DWORD;
  Errors: DWORD;
  Stat: TComStat;
  OutBuf: packed array[0..127] of Byte;
//  OutBufPtr: Pointer;
//  P: PByte;
  BytesToWrite: LongWord;
  WaitResult: Cardinal;

begin
//  PostPos:= 0;
//  LastError:= 0;
  try
    FillChar(WriteOL, SizeOf(WriteOL), 0);
    WriteOL.hEvent:= CreateEvent(nil, True, False, nil);  // !!! manual reset

 //   BytesWritten:= 0;
    BytesToWrite:= 0;
    try
      while not Terminated do begin

//        H[0]:= WriteOL.hEvent;
        H[0]:= FOwner.FWriteEvent;
        H[1]:= FOwner.FExitEvent;
        WaitResult:= WaitForMultipleObjects(2, @H, False, INFINITE);

        case WaitResult of

// Write signalled
          WAIT_OBJECT_0: ResetEvent(FOwner.FWriteEvent);

          WAIT_OBJECT_0 + 1: Exit;

// Unknown
        else
//          FInternalError:= 5;
          PortMessage(etFatalError, $201);
          Exit;
        end; {case}

        while True do begin

          if ClearCommError(FOwner.FPortHandle, Errors, @Stat) then begin
            Errors:= Errors and FOwner.FErrorMask;
            if (Errors <> 0) {and (Errors <> LastError)} then begin
              PortMessage(etCommError, Errors);
            end;
//            LastError:= Error;
          end;

          Inc(BytesToWrite, FOwner.FWBuffer.Read(OutBuf[BytesToWrite],
                                          SizeOf(OutBuf) - BytesToWrite));
//            BytesToWrite:= FOwner.FWBuffer.Read(OutBuf, SizeOf(OutBuf));
          if BytesToWrite = 0 then begin
            PortMessage(etWriteBufEmpty, 0);
            Break;
          end;

          OK:= WriteFile(FOwner.FPortHandle,         {handle}
                         OutBuf,                     {buffer}
                         BytesToWrite,               {bytes to write}
                         WriteCount,                 {bytes written}
                         @WriteOL);                  {overlap record}

          if not OK then begin
            if GetLastError = ERROR_IO_PENDING then begin
              H[0]:= WriteOL.hEvent;
              H[1]:= FOwner.FExitEvent;
              WaitResult:= WaitForMultipleObjects(2, @H, False, INFINITE);
              case WaitResult of

// Write completed
                WAIT_OBJECT_0: begin
                  if GetOverlappedResult(FOwner.FPortHandle,  {handle}
                                         WriteOL,         {overlap record}
                                         WriteCount,      {bytes written}
                                         False)           {don't wait for completion}
                  then begin
                    ResetEvent(WriteOL.hEvent);
                    OK:= True;
//              Inc(BytesWritten, WriteCount);
                  end;
                end;

                WAIT_OBJECT_0 + 1: Exit;

              else
//                FInternalError:= 5;
                PortMessage(etFatalError, $202);
                Exit;
              end;  {case}
            end;
          end;

          if OK then begin
            if (WriteCount > 0) then begin
              Dec(BytesToWrite, WriteCount);
              if (BytesToWrite > 0) then
                Move(OutBuf[WriteCount], OutBuf, BytesToWrite);
            end;
          end
          else begin
//            FInternalError:= 2;
            PortMessage(etFatalError, $203);
            Exit;
          end;
        end;
      end;
    finally
      CloseHandle(WriteOL.hEvent);
    end;
  except
    on E:Exception do begin
//      FInternalError:= 1;
      PortMessage(etFatalError, $204);
      raise;
    end;
  end;
end;

{ TksComPort }

constructor TksComPort.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWndHandle:= Classes.AllocateHWnd(WndProc);
  FPortHandle:= INVALID_HANDLE_VALUE;
  FErrorMask:= $FFFFFFFF;
  FEventMask:= DefaultEventMask;
  FBaudRate:= 9600;
  FByteSize:= 8;
  FRBufSize:= 4096;
  FWBufSize:= 4096;
  FPortName:= 'COM1';
end;

destructor TksComPort.Destroy;
begin
  Classes.DeallocateHWnd(FWndHandle);
  inherited Destroy;
end;

procedure TksComPort.HandleMessage(Msg: TMessage);
begin
  Msg.Result:= 1;
  if Assigned(FOnMessage) then FOnMessage(Self, Msg);
  if Msg.Result <> 0 then begin
    case Msg.WParam of
      etCommEvent: if Assigned(FOnEvent) then FOnEvent(Self, Msg.LParam);
      etCommError: if Assigned(FOnError) then FOnError(Self, Msg.LParam);
      etCommDataReady: if Assigned(FOnRead) then FOnRead(Self, Msg.LParam);
      etWriteBufEmpty: if Assigned(FOnWrite) then FOnWrite(Self, Msg.LParam);
      etReadOverflow: if Assigned(FOnReadOverflow) then FOnReadOverflow(Self, Msg.LParam);
      etFatalError: if Assigned(FOnFatalError) then FOnFatalError(Self, Msg.LParam);
    end;
    Msg.Result:= 0;
  end;
end;

function TksComPort.Open: Boolean;
var
  DCB: TDCB;
  Timeouts: TCommTimeouts;
  Mask: LongWord;

begin
  if FPortName = '' then begin
    Result:= False;
    Exit;
  end;
  FPortHandle:= CreateFile(PChar(FPortName),          {name}
                       GENERIC_READ or GENERIC_WRITE, {access attributes}
                       0,                             {no sharing}
                       nil,                           {no security}
                       OPEN_EXISTING,                 {creation action}
                       FILE_ATTRIBUTE_NORMAL          {attributes}
                    or FILE_FLAG_OVERLAPPED,          { and flags}
                       0);                            {no template}
  Result:= FPortHandle <> INVALID_HANDLE_VALUE;
  if Result then begin
    FRBuffer:= TksRingBuffer.Create(FRBufSize);
    FWBuffer:= TksRingBuffer.Create(FWBufSize);

{ Signalled by Event Thread to Read Thread}
    FRXEvent:= CreateEvent(nil,
                           True,      // manual reset
                           False,     // initial state = not signaled
                           nil);

{ Signalled by main Thread to Write Thread}
    FWriteEvent:= CreateEvent(nil,
                              True,      // manual reset
                              False,     // initial state = not signaled
                              nil);

{ Signalled by main Thread to all background threads}
    FExitEvent:= CreateEvent(nil,
                             True,      // manual reset
                             False,     // initial state = not signaled
                             nil);

    if GetCommState(FPortHandle, DCB) then begin
      UpdateDCB(DCB);
      if Assigned(FOnSetCommState) then FOnSetCommState(Self, DCB);
      SetCommState(FPortHandle, DCB);
    end;
    Timeouts:= DefaultCommTimeouts;
    if Assigned(FOnSetCommTimeouts) then FOnSetCommTimeouts(Self, Timeouts);
    SetCommTimeouts(FPortHandle, Timeouts);

//    if FReadName <> '' then FReadLog:= TFileStream.Create(FReadName, fmCreate);

    Mask:= DefaultEventMask;
    if Assigned(FOnSetCommMask) then FOnSetCommMask(Self, Mask);
    SetCommMask(FPortHandle, Mask or EV_RXCHAR);
    FEventMask:= Mask;
    FReadThread:= TCommReadThread.Create(Self);
    FWriteThread:= TCommWriteThread.Create(Self);
    FEventThread:= TCommEventThread.Create(Self);
  end;
end;

function TksComPort.Read(var Buf; Count: Integer): Integer;
begin
  if (FPortHandle = INVALID_HANDLE_VALUE) or (Count <= 0)
    then Result:= 0
    else Result:= FRBuffer.Read(Buf, Count);
end;

function TksComPort.Write(var Buf; Count: Integer): Integer;
begin
  if (FPortHandle = INVALID_HANDLE_VALUE) or (Count <= 0)
    then Result:= 0
    else Result:= FWBuffer.Write(Buf, Count);
  SetEvent(FWriteEvent);
end;

function TksComPort.Active: Boolean;
begin
  Result:= FPortHandle <> INVALID_HANDLE_VALUE;
end;

procedure TksComPort.Close;
begin
  if FPortHandle <> INVALID_HANDLE_VALUE then begin
    SetEvent(FExitEvent);
    FEventThread.Free;
    FEventThread:= nil;
    FReadThread.Free;
    FReadThread:= nil;
    FWriteThread.Free;
    FWriteThread:= nil;
    ResetEvent(FExitEvent);
    CloseHandle(FRXEvent);
    CloseHandle(FWriteEvent);
    CloseHandle(FExitEvent);
//    DeleteCriticalSection(FErrLock);
    CloseHandle(FPortHandle);
    FRBuffer.Free;
    FWBuffer.Free;
//    FReadLog.Free;
//    FReadLog:= nil;
    FPortHandle:= INVALID_HANDLE_VALUE;
  end;
end;

function TksComPort.GetPortMask(var Mask: LongWord): Boolean;
begin
  Result:= (FPortHandle <> INVALID_HANDLE_VALUE)
           and GetCommMask(FPortHandle, Mask);
end;

function TksComPort.GetPortState(var DCB: TDCB): Boolean;
begin
  Result:= (FPortHandle <> INVALID_HANDLE_VALUE)
           and GetCommState(FPortHandle, DCB);
end;

function TksComPort.GetPortTimeouts(var Timeouts: TCommTimeouts): Boolean;
begin
  Result:= (FPortHandle <> INVALID_HANDLE_VALUE)
           and GetCommTimeouts(FPortHandle, Timeouts);
end;

procedure TksComPort.SetRBufSize(Value: Integer);
begin
  if (FPortHandle = INVALID_HANDLE_VALUE) then begin
    if (Value < 64) then Value:= 64
    else if (Value > 64*1024) then Value:= 64*1024;
    FRBufSize:= Value;
  end;
end;

procedure TksComPort.SetWBufSize(Value: Integer);
begin
  if (FPortHandle = INVALID_HANDLE_VALUE) then begin
    if (Value < 64) then Value:= 64
    else if (Value > 64*1024) then Value:= 64*1024;
    FWBufSize:= Value;
  end;
end;

procedure TksComPort.SetBaudRate(const Value: LongWord);
var
  DCB: TDCB;

begin
  if GetPortState(DCB) then begin
    DCB.BaudRate:= FBaudRate;
    SetCommState(FPortHandle, DCB);
  end;
end;

procedure TksComPort.SetByteSize(const Value: Byte);
var
  DCB: TDCB;

begin
  FByteSize:= Value;
  if GetPortState(DCB) then begin
    DCB.ByteSize:= FByteSize;
    SetCommState(FPortHandle, DCB);
  end;
end;

procedure TksComPort.SetParity(const Value: TksCommParity);
var
  DCB: TDCB;

begin
  FParity:= Value;
  if GetPortState(DCB) then begin
    DCB.Parity:= Byte(FParity);
    SetCommState(FPortHandle, DCB);
  end;
end;

procedure TksComPort.SetStopBits(const Value: TksCommStopBits);
var
  DCB: TDCB;

begin
  FStopBits:= Value;
  if GetPortState(DCB) then begin
    DCB.StopBits:= Byte(FStopBits);
    SetCommState(FPortHandle, DCB);
  end;
end;

procedure TksComPort.SetRTSControl(const Value: TksCommRTSControl);
var
  DCB: TDCB;

begin
  FRTSControl:= Value;
  if GetPortState(DCB) then begin
    DCB.Flags:= (DCB.Flags and $FFFFFFCF) or LongWord(Ord(Value) shl 4);
    SetCommState(FPortHandle, DCB);
  end;
end;

procedure TksComPort.SetDSRSensitivity(const Value: Boolean);
var
  DCB: TDCB;

begin
  FDSRSensitivity:= Value;
  if GetPortState(DCB) then begin
    DCB.Flags:= (DCB.Flags and $FFFFFFBF) or LongWord(Ord(Value) shl 6);
    SetCommState(FPortHandle, DCB);
  end;
end;

procedure TksComPort.SetDTRControl(const Value: TksCommDTRControl);
var
  DCB: TDCB;

begin
  FDTRControl:= Value;
  if GetPortState(DCB) then begin
    DCB.Flags:= (DCB.Flags and $FFFFCFFF) or LongWord(Ord(Value) shl 12);
    SetCommState(FPortHandle, DCB);
  end;
end;

procedure TksComPort.SetParityCheck(const Value: Boolean);
var
  DCB: TDCB;

begin
  FParityCheck:= Value;
  if GetPortState(DCB) then begin
    DCB.Flags:= (DCB.Flags and $FFFFFFFD) or LongWord(Ord(Value) shl 1);
    SetCommState(FPortHandle, DCB);
  end;
end;

function TksComPort.SetPortState(const DCB: TDCB): Boolean;
begin
  Result:= (FPortHandle <> INVALID_HANDLE_VALUE)
    and SetCommState(FPortHandle, DCB);
  if Result then UpdateProperties(DCB);
end;

procedure TksComPort.SetCTSControl(const Value: Boolean);
var
  DCB: TDCB;

begin
  FCTSControl:= Value;
  if GetPortState(DCB) then begin
    DCB.Flags:= (DCB.Flags and $FFFFFFFB) or LongWord(Ord(Value) shl 2);
    SetCommState(FPortHandle, DCB);
  end;
end;

procedure TksComPort.SetDSRControl(const Value: Boolean);
var
  DCB: TDCB;

begin
  FDSRControl:= Value;
  if GetPortState(DCB) then begin
    DCB.Flags:= (DCB.Flags and $FFFFFFF7) or LongWord(Ord(Value) shl 3);
    SetCommState(FPortHandle, DCB);
  end;
end;

procedure TksComPort.UpdateDCB(var DCB: TDCB);
begin
  DCB.BaudRate:= FBaudRate;
  DCB.ByteSize:= FByteSize;
  DCB.Parity:= Byte(FParity);
  DCB.StopBits:= Byte(FStopBits);
  DCB.Flags:= (DCB.Flags and $FFFF8001)
      or (LongWord(FDTRControl) shl 4) or (LongWord(FRTSControl) shl 12);
  if FParityCheck then DCB.Flags:= DCB.Flags or $02;
  if FCTSControl then DCB.Flags:= DCB.Flags or $04;
  if FDSRControl then DCB.Flags:= DCB.Flags or $08;
  if FDSRSensitivity then DCB.Flags:= DCB.Flags or $0040;
end;

procedure TksComPort.UpdateProperties(const DCB: TDCB);
begin
  FBaudRate:= DCB.BaudRate;
  FByteSize:= DCB.ByteSize;
  FParity:= TksCommParity(DCB.Parity);
  FStopBits:= TksCommStopBits(DCB.StopBits);
  FParityCheck:= DCB.Flags and $02 <> 0;
  FCTSControl:= DCB.Flags and $04 <> 0;
  FDSRControl:= DCB.Flags and $08 <> 0;
  FDTRControl:= TksCommDTRControl((DCB.Flags shr 4) and $03);
  FDSRSensitivity:= DCB.Flags and $40 <> 0;
  FRTSControl:= TksCommRTSControl((DCB.Flags shr 12) and $03);
end;

procedure TksComPort.WndProc(var Msg: TMessage);
begin
  if Msg.Msg = MsgID then begin
    try
      HandleMessage(Msg);
    except
      Application.HandleException(Self);
    end
  end
  else
    Msg.Result:= DefWindowProc(FWndHandle, Msg.Msg, Msg.WParam, Msg.LParam);
end;

end.
