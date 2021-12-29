unit DSSupport_audio;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, _PicTools, _m_Tools,
  DSPack, DirectShow9,mmsystem, ActiveX,  DirectDraw, ExtCtrls, StdCtrls, DSSupport_audio_audiograb;

function DSAudio_Start(fn:string;StartTimeSec,EndTimeSec:double):boolean;
function DSAudio_Run:boolean;
function DSAudio_Close:boolean;

function DSAudio_GetLastError:string;
function DSAudio_GetSampleRate:integer;
function DSAudio_GetSampleBits:integer;
function DSAudio_GetSampleChs:integer;
function DSAudio_GetTotalTimeSec:double;

procedure RewriteWaveHeader(var wfs:TFileStream;rate:integer;is16bit:boolean);

var
  AudioSampleGrabber:TAudioSampleGrabber;

implementation

uses DSUtil;

type
  TDSAudioInfo=record
    ErrorFlag:boolean;
    SampleRate,SampleBits,SampleChs:integer;
    TotalTimeSec:double;
  end;

var
  DSAudioInfo:TDSAudioInfo;

// --------------------------------------

var
  RotRegistID:integer;

function AddToRot(pUnkGraph:IUnknown):HRESULT;
var
  pMoniker:IMoniker;
  pROT:IRunningObjectTable;
  ttl:widestring;
  hr:HRESULT;
begin
  RotRegistID:=-1;

  if (Failed(GetRunningObjectTable(0, pROT))) then begin
    Result:=E_FAIL;
    exit;
  end;

  ttl:=format('FilterGraph %8x pid %8x', [dword(pUnkGraph), GetCurrentProcessId()]);
  hr:=CreateItemMoniker(PWideChar(WideString('!')), PWideChar(ttl), pMoniker);
  if Succeeded(hr) then begin
    hr:=pROT.Register(0, pUnkGraph, pMoniker, RotRegistID);
    pMoniker:=nil;
  end;

  pROT:=nil;
  Result:=hr;
end;

procedure RemoveFromRot;
var
  pROT:IRunningObjectTable;
begin
  if Succeeded(GetRunningObjectTable(0, pROT)) then begin
    pROT.Revoke(RotRegistID);
    pROT:=nil;
  end;
end;

// --------------------------------------

function GetUnconnectedPinCount(pFilter:IBaseFilter;PinDir:PIN_DIRECTION):integer;
var
  hr:HRESULT;
  pEnum:IEnumPins;
  pPin:IPin;
  ThisPinDir:PIN_DIRECTION;
  pTmp:IPin;
  PinCount:integer;
begin
  hr:=pFilter.EnumPins(pEnum);
  if (FAILED(hr)) then begin
    Result:=0;
    exit;
  end;

  PinCount:=0;

  while (pEnum.Next(1, pPin, nil) = S_OK) do begin
    pPin.QueryDirection(ThisPinDir);
    if (ThisPinDir=PinDir) then begin
      hr:=pPin.ConnectedTo(pTmp);
      if Succeeded(hr) then begin
        // 既に接続済み、必要なピンではない。
        pTmp:=nil;
        end else begin
        // 未接続、これが必要なピンである。
        inc(PinCount);
      end;
    end;
    pPin:=nil;
  end;

  pEnum:=nil;

  Result:=PinCount;
end;

function GetUnconnectedPin(pFilter:IBaseFilter;PinDir:PIN_DIRECTION;IgnoreCount:integer;out ppPin:IPin):HRESULT;
var
  hr:HRESULT;
  pEnum:IEnumPins;
  pPin:IPin;
  ThisPinDir:PIN_DIRECTION;
  pTmp:IPin;
begin
  ppPin:=nil;

  hr:=pFilter.EnumPins(pEnum);
  if FAILED(hr) then begin
    Result:=hr;
    exit;
  end;

  while (pEnum.Next(1, pPin, nil) = S_OK) do begin
    hr:=pPin.QueryDirection(ThisPinDir);
    if FAILED(hr) then begin
      Result:=hr;
      exit;
    end;
    if (ThisPinDir=PinDir) then begin
      hr:=pPin.ConnectedTo(pTmp);
      if Succeeded(hr) then begin
        // 既に接続済み、必要なピンではない。
        pTmp:=nil;
        end else begin
        // 未接続、これが必要なピンである。
        if IgnoreCount=0 then begin
          pEnum:=nil;
          ppPin:=pPin;
          Result:=S_OK;
          exit;
        end;
        dec(IgnoreCount);
      end;
    end;
    pPin:=nil;
  end;

  pEnum:=nil;

  // 一致するピンが見つからなかった。
  Result:=E_FAIL;
end;

// --------------------------------------

var
  DSLastError:string;
  pGraph:IGraphBuilder;

function DSSetNullClock:boolean;
var
  hr:HRESULT;
  pMediaFilter:IMediaFilter;
begin
  Result:=False;

  hr:=pGraph.QueryInterface(IID_IMediaFilter, pMediaFilter);
  if Failed(hr) then begin
    DSLastError:='Error pGraph.QueryInterface(IID_IMediaFilter) $'+inttohex(hr,8);
    exit;
  end;

  hr:=pMediaFilter.SetSyncSource(nil);
  if Failed(hr) then begin
    DSLastError:='Error pMediaFilter.SetSyncSource $'+inttohex(hr,8);
    exit;
  end;

  Result:=True;
end;

function DSPinConnectToNullFilter(pPinOut:IPin):boolean;
var
  hr:HRESULT;
  pNullF:IBaseFilter;
  pPinIn:IPin;
begin
  Result:=False;

  hr:=CoCreateInstance(CLSID_NullRenderer, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, pNullF);
  if Failed(hr) then begin
    DSLastError:='Error CoCreateInstance(CLSID_NullRenderer) $'+inttohex(hr,8);
    exit;
  end;

  hr:=pGraph.AddFilter(pNullF, PWideChar(WideString('Null Renderer')));
  if Failed(hr) then begin
    DSLastError:='Error pGraph.AddFilter(pNullF) $'+inttohex(hr,8);
    exit;
  end;

  hr:=GetUnconnectedPin(pNullF, PINDIR_INPUT, 0, pPinIn);
  if Failed(hr) then begin
    DSLastError:='Error GetUnconnectedPin pNullF $'+inttohex(hr,8);
    exit;
  end;

  hr:=pGraph.Connect(pPinOut,pPinIn);
  if Failed(hr) then begin
    DSLastError:='Error Connect $'+inttohex(hr,8);
    exit;
  end;

  Result:=True;
end;

function DSOpenFile(fn:string):IPin;
var
  hr:HRESULT;
  pSrcFileFilter:IBaseFilter;
  pSrcFileVideoOutPin:IPin;
  ext:string;
  PinCount:integer;
  TargetPinIndex:integer;
  NullPinIndex:integer;
  pNullPin:IPin;
begin
  Result:=nil;

  hr:=pGraph.AddSourceFilter(PWideChar(WideString(fn)), PWideChar(WideString('SourceFile')), pSrcFileFilter);
  if Failed(hr) then begin
    DSLastError:='Error pGraph.AddSourceFilter $'+inttohex(hr,8);
    exit;
  end;

  ext:=ansilowercase(ExtractFileExt(fn));

  PinCount:=GetUnconnectedPinCount(pSrcFileFilter,PINDIR_OUTPUT);

  if PinCount=0 then begin
    DSLastError:='Error pSrcFileFilter PinCount=0';
    exit;
  end;

  TargetPinIndex:=0;
  NullPinIndex:=-1;

  if PinCount=1 then TargetPinIndex:=0;

  if PinCount=2 then begin
    if ext='.wmv' then begin
      TargetPinIndex:=0;
      NullPinIndex:=1;
    end;
    if ext='.mp4' then begin
      TargetPinIndex:=1;
      NullPinIndex:=0;
    end;
  end;

  hr:=GetUnconnectedPin(pSrcFileFilter, PINDIR_OUTPUT, TargetPinIndex, pSrcFileVideoOutPin);
  if Failed(hr) then begin
    DSLastError:='Error GetUnconnectedPin pSrcFileFilter $'+inttohex(hr,8);
    exit;
  end;

  if NullPinIndex<>-1 then begin
    hr:=GetUnconnectedPin(pSrcFileFilter, PINDIR_OUTPUT, NullPinIndex, pNullPin);
    if Failed(hr) then begin
      DSLastError:='Error GetUnconnectedPin pSrcFileFilter $'+inttohex(hr,8);
      exit;
    end;
    if DSPinConnectToNullFilter(pNullPin)=False then exit;
  end;

  Result:=pSrcFileVideoOutPin;
end;

function DSSetTimeFormat:boolean;
var
  hr:HRESULT;
  pMediaSeeking:IMediaSeeking;
begin
  Result:=False;

  hr:=pGraph.QueryInterface(IID_IMediaSeeking, pMediaSeeking);
  if Failed(hr) then begin
    DSLastError:='Error pGraph.QueryInterface(IID_IMediaSeeking $'+inttohex(hr,8);
    exit;
  end;

  hr:=pMediaSeeking.SetTimeFormat(TIME_FORMAT_MEDIA_TIME);
  if Failed(hr) then begin
    DSLastError:='Error pMediaSeeking.SetTimeFormat(TIME_FORMAT_MEDIA_TIME) $'+inttohex(hr,8);
    exit;
  end;

  pMediaSeeking:=nil;

  Result:=True;
end;

function DSGetTotalTimeSec:double;
var
  hr:HRESULT;
  pMediaSeeking:IMediaSeeking;
  sec:int64;
begin
  Result:=0;

  hr:=pGraph.QueryInterface(IID_IMediaSeeking, pMediaSeeking);
  if Failed(hr) then begin
    DSLastError:='Error pGraph.QueryInterface(IID_IMediaSeeking $'+inttohex(hr,8);
    exit;
  end;

  hr:=pMediaSeeking.GetDuration(sec);
  if Failed(hr) then begin
    DSLastError:='Error pMediaSeeking.SetTimeFormat(TIME_FORMAT_MEDIA_TIME) $'+inttohex(hr,8);
    exit;
  end;

  pMediaSeeking:=nil;

  Result:=sec/10000000;
end;

function DSSeekTimeSec(TimeSec:double):boolean;
var
  hr:HRESULT;
  pMediaSeeking:IMediaSeeking;
  starttimeus,endtimeus:int64;
begin
  Result:=False;

  hr:=pGraph.QueryInterface(IID_IMediaSeeking, pMediaSeeking);
  if Failed(hr) then begin
    DSLastError:='Error pGraph.QueryInterface(IID_IMediaSeeking $'+inttohex(hr,8);
    exit;
  end;

  starttimeus:=trunc(TimeSec*10000000);
  endtimeus:=0;

  hr:=pMediaSeeking.SetPositions(starttimeus,AM_SEEKING_AbsolutePositioning,endtimeus,AM_SEEKING_NoPositioning);
  if Failed(hr) then begin
    DSLastError:='Error pMediaSeeking.SetPositions $'+inttohex(hr,8);
    exit;
  end;

  pMediaSeeking:=nil;

  Result:=True;
end;

function DSAudio_Start(fn:string;StartTimeSec,EndTimeSec:double):boolean;
var
  hr:HRESULT;
  pSrcFileVideoOutPin:IPin;
  _TotalTimeSec:double;
begin
  Result:=False;

  with DSAudioInfo do begin
    ErrorFlag:=True;
    SampleRate:=0;
    SampleBits:=0;
    SampleChs:=0;
    TotalTimeSec:=0;
  end;

  DSLastError:='';

  hr:=CoCreateInstance(CLSID_FilterGraph, nil, CLSCTX_INPROC_SERVER,IID_IGraphBuilder, pGraph);
  if Failed(hr) then begin
    DSLastError:='Error CoCreateInstance CLSID_FilterGraph $'+inttohex(hr,8);
    exit;
  end;

  hr:=AddToRot(pGraph);
  if Failed(hr) then begin
    DSLastError:='Error AddToRot(pGraph) $'+inttohex(hr,8);
    exit;
  end;

  AudioSampleGrabber:=TAudioSampleGrabber.Create(nil);

  pSrcFileVideoOutPin:=DSOpenFile(fn);
  if pSrcFileVideoOutPin=nil then exit;

  // set clock

  if DSSetNullClock=False then exit;

  // ----

  hr:=pGraph.AddFilter(AudioSampleGrabber.FBaseFilter,PWideChar(WideString('Custom audio sample grabber')));
  if Failed(hr) then begin
    DSLastError:='Error pGraph.AddFilter pGrabberF $'+inttohex(hr,8);
    exit;
  end;

  hr:=pGraph.Connect(pSrcFileVideoOutPin, AudioSampleGrabber.InPutPin);
  if Failed(hr) then begin
    DSLastError:='Error Connect $'+inttohex(hr,8);
    exit;
  end;

  pSrcFileVideoOutPin:=nil;

  // -------------------------------------------------------------

  if DSPinConnectToNullFilter(AudioSampleGrabber.OutPutPin)=False then exit;

  if DSSetTimeFormat=False then exit;

  // ----------

  _TotalTimeSec:=DSGetTotalTimeSec;
  if _TotalTimeSec=0 then exit;

  if (StartTimeSec<>0) and (EndTimeSec<>0) then begin
    _TotalTimeSec:=EndTimeSec-StartTimeSec;
  end;

  if 3<StartTimeSec then begin
{ // なにかおかしいのでオーディオストリームはシークしない。72.0秒で失敗する？
    DSSeekTimeSec(StartTimeSec-3); // 失敗しても無視
}
  end;

  if AudioSampleGrabber.StartRender(StartTimeSec,EndTimeSec)=False then begin
    DSLastError:='Error AudioSampleGrabber.StartRender';
    exit;
  end;

  with DSAudioInfo do begin
    ErrorFlag:=False;
    SampleRate:=AudioSampleGrabber.SampleRate;
    SampleBits:=AudioSampleGrabber.SampleBits;
    SampleChs:=AudioSampleGrabber.SampleChs;
    TotalTimeSec:=_TotalTimeSec;
  end;

  Result:=True;
end;

function DSAudio_Run:boolean;
var
  hr:HRESULT;
  pMediaControl:IMediaControl;
begin
  Result:=False;

  hr:=pGraph.QueryInterface(IID_IMediaControl,pMediaControl);
  if Failed(hr) then begin
    DSLastError:='Error pGraph.QueryInterface IID_IMediaControl $'+inttohex(hr,8);
    exit;
  end;

  hr:=pMediaControl.Run;
  if Failed(hr) then begin
    DSLastError:='Error pMediaControl.Run $'+inttohex(hr,8);
    exit;
  end;

  pMediaControl:=nil;

  Result:=True;
end;

function DSAudio_Close:boolean;
var
  hr:HRESULT;
  pMediaControl:IMediaControl;
  pMediaEvent:IMediaEvent;
  EventCode:integer;
begin
  Result:=False;

  hr:=pGraph.QueryInterface(IID_IMediaEvent,pMediaEvent);
  if Failed(hr) then begin
    DSLastError:='Error pGraph.QueryInterface IID_IMediaEvent $'+inttohex(hr,8);
    exit;
  end;

  hr:=pMediaEvent.WaitForCompletion(100,EventCode);
  if hr=VFW_E_WRONG_STATE then begin
    pMediaEvent:=nil;
    AudioSampleGrabber.Free; AudioSampleGrabber:=nil;
    Result:=True;
    exit;
  end;

  if EventCode<>EC_COMPLETE then begin
    if AudioSampleGrabber.StopRender=False then begin
      DSLastError:='Error AudioSampleGrabber.StopRender';
      exit;
    end;
  end;

  hr:=pGraph.QueryInterface(IID_IMediaControl,pMediaControl);
  if Failed(hr) then begin
    DSLastError:='Error pGraph.QueryInterface IID_IMediaControl $'+inttohex(hr,8);
    exit;
  end;

  hr:=pMediaControl.Stop;
  if Failed(hr) then begin
    DSLastError:='Error pMediaControl.Run $'+inttohex(hr,8);
    exit;
  end;

  pMediaControl:=nil;

  while(True) do begin
    hr:=pMediaEvent.WaitForCompletion(100,EventCode);
    if hr=VFW_E_WRONG_STATE then break;
  end;

  pMediaEvent:=nil;

  AudioSampleGrabber.Free; AudioSampleGrabber:=nil;

  RemoveFromRot;
end;

const RiffHeaderSize=11;

procedure RewriteWaveHeader(var wfs:TFileStream;rate:integer;is16bit:boolean);
var
  oldpos:integer;
  Count:integer;
  RiffHeader:array[0..RiffHeaderSize-1] of dword;
  fsize:integer;
  function SwapHiLow(d:dword):dword;
  begin
    Result:=dword((int64(d) div $1000000 and $FF)+((int64(d) div $10000 and $FF)*$100)+((int64(d) div $100 and $FF)*$10000)+((int64(d) and $FF)*$1000000));
  end;
begin
  // RiffWave Header of 44.1khz 16bit stereo
  RiffHeader[ 0]:=$52494646; // RIFF Header
  RiffHeader[ 1]:=$00000000; // TotalFileSize-8;
  RiffHeader[ 2]:=$57415645; // WAVE Header
  RiffHeader[ 3]:=$666D7420; // fmt  Header
  RiffHeader[ 4]:=$10000000;
  RiffHeader[ 5]:=$01000200; // (word)wFormatTag,(word)nChannels
  RiffHeader[ 6]:=$44AC0000; // (dword)nSamplesPerSec(44kHz)
  RiffHeader[ 7]:=$10B10200; // (dword)nAvgBytesPerSec(44kHz*4)
  RiffHeader[ 8]:=$04001000; // (word)nBlockAlign,(word)wBitsPerSample
  if is16bit=False then RiffHeader[8]:=RiffHeader[8] div 2;
  RiffHeader[ 9]:=$64617461; // data Header
  RiffHeader[10]:=$00000000; // WaveSize (bytesize)

  RiffHeader[ 6]:=SwapHiLow(rate);
  RiffHeader[ 7]:=SwapHiLow(rate*4);

  if wfs.Size<(RiffHeaderSize*4) then begin
    fsize:=RiffHeaderSize*4;
    end else begin
    fsize:=wfs.Size-(RiffHeaderSize*4);
  end;
  RiffHeader[ 1]:=SwapHiLow(fsize-8);
  RiffHeader[10]:=SwapHiLow(fsize-44);

  // Swap Hi Low
  for Count:=0 to RiffHeaderSize-1 do begin
    RiffHeader[Count]:=SwapHiLow(RiffHeader[Count]);
  end;

  oldpos:=wfs.Position;
  wfs.Position:=0;
  wfs.WriteBuffer(RiffHeader[0],RiffHeaderSize*4);
  wfs.Position:=oldpos;
end;

function DSAudio_GetLastError:string;
begin
  Result:=DSLastError;
end;

function DSAudio_GetSampleRate:integer;
begin
  Result:=DSAudioInfo.SampleRate;
end;

function DSAudio_GetSampleBits:integer;
begin
  Result:=DSAudioInfo.SampleBits;
end;

function DSAudio_GetSampleChs:integer;
begin
  Result:=DSAudioInfo.SampleChs;
end;

function DSAudio_GetTotalTimeSec:double;
begin
  Result:=DSAudioInfo.TotalTimeSec;
end;

end.

