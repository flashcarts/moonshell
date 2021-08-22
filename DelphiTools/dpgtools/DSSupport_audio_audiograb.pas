unit DSSupport_audio_audiograb;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SyncObjs,
  DSPack, DSUtil, BaseClass,
  DirectShow9,mmsystem, ActiveX,  DirectDraw, ExtCtrls, StdCtrls;

const SamplesMaxCount=1*1024*1024;

type

//******************************************************************************
//
//  TFilterSampleGrabber declaration
//  description: Sample Grabber Wrapper Filter
//
//******************************************************************************
  {@exclude}
  TAudioSampleGrabber = class;

  { This class is designed make a snapshoot of Video or Audio Datas.
    WARNING: There is know problems with some DIVX movies, so use RGB32 Media Type
    instead of RBG24.}
  TAudioSampleGrabber = class(TComponent, IFilter, ISampleGrabberCB)
  private
    function GetFilter: IBaseFilter;
    function GetName: string;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure NotifyFilter(operation: TFilterOperation; Param: integer = 0);
    function  SampleCB(SampleTime: Double; pSample: IMediaSample): HResult; stdcall;
    function  BufferCB(SampleStartTime: Double; pBuffer: PByte; BufferLen: longint): HResult; stdcall;
    procedure ConvertWave(pBuffer:Pointer;BufferLen:integer;var Samples:array of SmallInt;var SamplesCount:integer);
  public
    FCriticalSection: TCriticalSection;
    FBaseFilter: IBaseFilter;
    SampleGrabber: ISampleGrabber;
    InPutPin  : IPin;
    OutPutPin : IPin;
    SampleRate,SampleBits,SampleChs:integer;
    StartTimeSec,EndTimeSec:double;
    GetBitmapRequestStop:boolean;
    GetBitmapBusyFlag:boolean;
    SamplesBuf:array[0..SamplesMaxCount-1] of SmallInt;
    SamplesPosition:integer;
    SamplesCurrentTime:double;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
    function StartRender(_StartTimeSec,_EndTimeSec:double):boolean;
    function StopRender:boolean;
    function GetBitmap_isRequestStop:boolean;
    procedure GetBitmap_Cancel;
    function GetSampleStart:boolean;
    procedure GetSampleEnd;
  published
  end;

implementation

// *****************************************************************************
//  TSampleGrabber
// *****************************************************************************

  function TAudioSampleGrabber.GetFilter: IBaseFilter;
  begin
    result := FBaseFilter;
  end;

  function TAudioSampleGrabber.GetName: string;
  begin
    result := name;
  end;

  constructor TAudioSampleGrabber.Create(AOwner: TComponent);
  var
    EnumPins: IEnumPins;
    FMediaType: TMediaType;
  begin
    inherited Create(AOwner);
    FCriticalSection := TCriticalSection.Create;

    GetBitmapRequestStop:=False;
    GetBitmapBusyFlag:=False;

    SampleBits:=0;
    SampleChs:=0;
    SampleRate:=0;
    SamplesPosition:=0;
    SamplesCurrentTime:=0;

    Cocreateinstance(CLSID_SampleGrabber, nil, CLSCTX_INPROC ,IID_IBASEFilter, FBaseFilter);
    FBaseFilter.QueryInterface(IID_ISampleGrabber,SampleGrabber);

    FBaseFilter.EnumPins(EnumPins);
    EnumPins.Next(1,InPutPin,nil);
    EnumPins.Next(1,OutPutPin,nil);
    EnumPins := nil;

    FMediaType := TMediaType.Create(MEDIATYPE_Audio);
    FMediaType.SubType := MEDIASUBTYPE_PCM;
    FMediaType.FormatType := FORMAT_WaveFormatEx;
    FMediaType.AMMediaType.bFixedSizeSamples:=False;
    SampleGrabber.SetMediaType(FMediaType.AMMediaType^);
    FMediaType.Free;

    SampleGrabber.SetBufferSamples(true);
    SampleGrabber.SetOneShot(false);
    SampleGrabber.SetCallback(Self,1);
  end;

  destructor TAudioSampleGrabber.Destroy;
  begin
    FBaseFilter.Stop;
    InPutPin.Disconnect;
    OutPutPin.Disconnect;

    SampleGrabber.SetCallback(nil ,1);
    SampleGrabber.SetBufferSamples(false);
    FBaseFilter   := nil;
    SampleGrabber := nil;
    InPutPin      := nil;
    OutPutPin     := nil;

    FCriticalSection.Free;
    inherited destroy;
  end;

  procedure TAudioSampleGrabber.Notification(AComponent: TComponent; Operation: TOperation);
  begin
    inherited Notification(AComponent, Operation);
  end;

  procedure TAudioSampleGrabber.NotifyFilter(operation: TFilterOperation; Param: integer = 0);
  begin
  end;

  function TAudioSampleGrabber.QueryInterface(const IID: TGUID; out Obj): HResult;
  begin
    result := inherited QueryInterface(IID, Obj);
    if failed(result) and assigned(FBaseFilter) then
      result := FBaseFilter.QueryInterface(IID, Obj);
  end;

  function TAudioSampleGrabber.StartRender(_StartTimeSec,_EndTimeSec:double):boolean;
  var
    hr: HRESULT;
    MediaType: TAMMediaType;
    pwfex:PWAVEFORMATEX;
  begin
    Result:=False;

    if not Assigned(SampleGrabber) then exit;

    hr:=SampleGrabber.GetConnectedMediaType(MediaType);
    if Failed(hr) then exit;

    pwfex:=PWAVEFORMATEX(MediaType.pbFormat);
    SampleRate:=pwfex.nSamplesPerSec;
    SampleBits:=pwfex.wBitsPerSample;
    SampleChs:=pwfex.nChannels;

    StartTimeSec:=_StartTimeSec;
    EndTimeSec:=_EndTimeSec;

    if SampleRate=0 then exit;
    if (SampleBits<>8) and (SampleBits<>16) then exit;
    if (SampleChs<>1) and (SampleChs<>2) then exit;

    Result:=True;
  end;

  function TAudioSampleGrabber.StopRender:boolean;
  var
    readflag:boolean;
  begin
    FCriticalSection.Enter;
    GetBitmapRequestStop:=True;
    FCriticalSection.Leave;

    while(True) do begin
      FCriticalSection.Enter;
      readflag:=GetBitmapBusyFlag;
      FCriticalSection.Leave;
      if readflag=False then break;
      sleep(1);
    end;

    Result:=True;
  end;

type
  PLargeByteArray = ^TLargeByteArray;
  TLargeByteArray = array[0..1024*1024*1024] of Byte;

type
  PLargeSmallIntArray = ^TLargeSmallIntArray;
  TLargeSmallIntArray = array[0..1024*1024*1024 div 2] of SmallInt;

  procedure TAudioSampleGrabber.ConvertWave(pBuffer:Pointer;BufferLen:integer;var Samples:array of SmallInt;var SamplesCount:integer);
  var
    c:integer;
    psrc8:PLargeByteArray;
    psrc16:PLargeSmallIntArray;
    pdst16:PLargeSmallIntArray;
    DstSamplesCount:integer;
    idx:integer;
  begin
    SamplesCount:=0;

    if BufferLen=0 then exit;

    DstSamplesCount:=BufferLen;
    if SampleBits=16 then DstSamplesCount:=DstSamplesCount div 2;
    if SampleChs=1 then DstSamplesCount:=DstSamplesCount*2;

    psrc8:=PLargeByteArray(pBuffer);
    psrc16:=PLargeSmallIntArray(pBuffer);
    pdst16:=@Samples[0];

    if SampleChs=1 then begin
      if SampleBits=8 then begin
        // 8bit1chs
        for idx:=0 to (DstSamplesCount div 2)-1 do begin
          c:=psrc8[idx];
          c:=(c-128) shl 8;
          pdst16[idx*2+0]:=SmallInt(c);
          pdst16[idx*2+1]:=SmallInt(c);
        end;
        end else begin
        // 16bit1chs
        for idx:=0 to (DstSamplesCount div 2)-1 do begin
          c:=psrc16[idx];
          pdst16[idx*2+0]:=SmallInt(c);
          pdst16[idx*2+1]:=SmallInt(c);
        end;
      end;
      end else begin
      if SampleBits=8 then begin
        // 8bit2chs
        for idx:=0 to DstSamplesCount-1 do begin
          c:=psrc8[idx];
          c:=(c-128) shl 8;
          pdst16[idx]:=SmallInt(c);
        end;
        end else begin
        // 16bit2chs
        MoveMemory(pdst16,psrc16,DstSamplesCount*2);
{
        for idx:=0 to DstSamplesCount-1 do begin
          c:=psrc16[idx];
          pdst16[idx]:=SmallInt(c);
        end;
}
      end;
    end;

    SamplesCount:=DstSamplesCount;
  end;

var
  Samples:array[0..SamplesMaxCount-1] of SmallInt;

  function TAudioSampleGrabber.BufferCB(SampleStartTime: Double; pBuffer: PByte; BufferLen: Integer): HResult;
  var
    fr:boolean;
    SampleEndTime:double;
    SamplesCount:integer;
    SamplesTopIndex:integer;
    tmp:integer;
  begin
    result:=S_OK;

    FCriticalSection.Enter;
    fr:=GetBitmapRequestStop;
    FCriticalSection.Leave;
    if fr=True then begin
      FCriticalSection.Enter;
      GetBitmapBusyFlag:=False;
      FCriticalSection.Leave;
      result := S_FALSE;
      exit;
    end;

    if SampleStartTime<0 then exit;

    FCriticalSection.Enter;
    GetBitmapBusyFlag:=True;
    FCriticalSection.Leave;

    FCriticalSection.Enter;
    ConvertWave(pBuffer,BufferLen,Samples,SamplesCount);
    FCriticalSection.Leave;

    while(True) do begin
      FCriticalSection.Enter;
      fr:=GetBitmapRequestStop;
      FCriticalSection.Leave;
      if fr=True then begin
        FCriticalSection.Enter;
        GetBitmapBusyFlag:=False;
        FCriticalSection.Leave;
        result := S_FALSE;
        exit;
      end;

      FCriticalSection.Enter;
      if (SamplesPosition+SamplesCount)<SamplesMaxCount then begin
        FCriticalSection.Leave;
        break;
      end;
      FCriticalSection.Leave;

      sleep(1);
    end;

    SamplesCount:=SamplesCount div 2;

    SampleEndTime:=SampleStartTime+(SamplesCount/SampleRate);
    SamplesTopIndex:=0;

    if (SampleEndTime<=StartTimeSec) or (EndTimeSec<SampleStartTime) then begin
      FCriticalSection.Enter;
      GetBitmapBusyFlag:=False;
      FCriticalSection.Leave;
      exit;
    end;

    if SampleStartTime<StartTimeSec then begin
      SamplesTopIndex:=trunc((StartTimeSec-SampleStartTime)*SampleRate);
      SamplesCount:=SamplesCount-SamplesTopIndex;
      SampleStartTime:=StartTimeSec;
    end;

    if EndTimeSec<SampleEndTime then begin
      SampleEndTime:=EndTimeSec;
      tmp:=SamplesCount;
      SamplesCount:=trunc((SampleEndTime-SampleStartTime)*SampleRate);
      if tmp<SamplesCount then SamplesCount:=tmp;
    end;

    if SamplesCount<0 then SamplesCount:=0;

    SamplesCount:=SamplesCount*2;

    FCriticalSection.Enter;
    SamplesCurrentTime:=SampleStartTime-StartTimeSec;
    MoveMemory(@SamplesBuf[SamplesPosition],@Samples[SamplesTopIndex],SamplesCount*2);
    inc(SamplesPosition,SamplesCount);
    FCriticalSection.Leave;

    FCriticalSection.Enter;
    GetBitmapBusyFlag:=False;
    FCriticalSection.Leave;
  end;

  function TAudioSampleGrabber.SampleCB(SampleTime: Double;
    pSample: IMediaSample): HResult;
  begin
    result := S_OK;
  end;

  function TAudioSampleGrabber.GetBitmap_isRequestStop:boolean;
  begin
    FCriticalSection.Enter;
    Result:=GetBitmapRequestStop;
    FCriticalSection.Leave;
  end;

  procedure TAudioSampleGrabber.GetBitmap_Cancel;
  begin
    FCriticalSection.Enter;
    GetBitmapRequestStop:=True;
    GetBitmapBusyFlag:=True;
    FCriticalSection.Leave;
  end;

  function TAudioSampleGrabber.GetSampleStart:boolean;
  begin
    FCriticalSection.Enter;

    if SamplesPosition=0 then begin
      FCriticalSection.Leave;
      Result:=False;
      exit;
    end;

    Result:=True;
  end;

  procedure TAudioSampleGrabber.GetSampleEnd;
  begin
    FCriticalSection.Leave;
  end;

end.
