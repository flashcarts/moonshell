unit dpgshow_PCMOut;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, _const, MMSystem, ddsd;

type
  TPCMOut = class
  private
    { Private êÈåæ }
    DDSD:TDDSD;
    NowDSoundSafeTime:LongInt;
    NowPCMRate:word;
    WaveMonitor:TDDSDWaveData;
  public
    { Public êÈåæ }
    WaveEnabled:boolean;
    WaveSize,WavePos:integer;
    WaveBuffer:array of SmallInt;
    WaveFreq:integer;
    PCMBusy:boolean;
    PCMBufSize:integer;
    function  Init(_NowDSoundSafeTime:LongInt;_NowPCMRate:word):boolean;
    procedure StopDriver;
    procedure FreeMemory;
    procedure UpdateWaveMonitor(Sender:TDDSDGenWave; Player:TDDSDChannel; ofs,len:Cardinal);
    procedure StartDSound;
  end;

implementation

uses dpgshow_MainWin;

function TPCMOut.Init(_NowDSoundSafeTime:LongInt;_NowPCMRate:word):boolean;
var
  TempSec:integer;
begin
  PCMBusy:=False;

  NowDSoundSafeTime:=_NowDSoundSafeTime;
  NowPCMRate:=_NowPCMRate;

  DDSD:=TDDSD.Create(Main);
  DDSD.forceInitialize;
  if DDSD.Initialized=False then begin
    Result:=False;
    exit;
  end;

  with DDSD do begin
    ChannelCount:=1;
    StickyFocus:=True;
    Use3D:=False;
    DebugOption:=[dsoHaltOnError];
  end;

  TempSec:=NowDSoundSafeTime;
  PCMBufSize:=(integer(NowPCMRate)*2*2)*TempSec div 1000; // (44khz*16bit*2ch)=1sec/1000ms*(num)ms
  PCMBufSize:=PCMBufSize and not 1;

  Result:=True;
end;

procedure TPCMOut.StartDSound;
var
  PCMBuffer:array[0..32768*2*2] of SmallInt; // len=size(PCMBuffer) of 16bit stereo 1000ms
begin
  DDSD[0].Stop;
  if WaveMonitor<>nil then WaveMonitor.Free;

  ZeroMemory(@PCMBuffer[0],PCMBufSize*2);

  DDSD.SetPrimaryBufferFotmat(NowPCMRate,16,True);
  WaveMonitor:=TDDSDWaveData.CreateStream(DDSD,NowPCMRate,16,True,PCMBufSize*2*2);
  WaveMonitor.OnUpdate:=UpdateWaveMonitor;
  DDSD[0].WaveData:=WaveMonitor;
  DDSD[0].Stop;
  WaveMonitor.BlockCopy(0, @PCMBuffer, PCMBufSize);
  WaveMonitor.BlockCopy(PCMBufSize, @PCMBuffer, PCMBufSize);
  DDSD[0].LoopPlay;

  WaveEnabled:=True;
end;

procedure TPCMOut.StopDriver;
var
  WaitTick:dword;
begin
  DDSD[0].Stop;
  WaveMonitor.OnUpdate:=nil;
  Application.ProcessMessages;
  while (DDSD[0].Status<>0) do begin
    sleep(16);
    Application.ProcessMessages;
  end;
  WaitTick:=timeGetTime;
  while ((WaitTick+dword(NowDSoundSafeTime*1))>timeGetTime) do begin
    sleep(16);
    Application.ProcessMessages;
  end;
  WaveMonitor.Free;
end;

procedure TPCMOut.FreeMemory;
begin
  DDSD.Free;
end;

procedure TPCMOut.UpdateWaveMonitor(Sender:TDDSDGenWave; Player:TDDSDChannel; ofs,len:Cardinal);
var
  PCMBuffer:array[0..32768*2*2] of SmallInt; // len=size(PCMBuffer) of 16bit stereo 1000ms
begin
  if PCMBusy=True then exit;
  PCMBusy:=True;

  if WaveEnabled=True then begin
    if Main.UpdateWaveMonitor(addr(PCMBuffer[0]),len div 4)=False then WaveEnabled:=False;
  end;

  if WaveEnabled=False then ZeroMemory(@PCMBuffer[0],len);

  WaveMonitor.BlockCopy(ofs,@PCMBuffer,len);

  PCMBusy:=False;
end;

end.
