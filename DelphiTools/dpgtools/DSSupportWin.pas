unit DSSupportWin;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls,_PicTools,
  DSPack, DirectShow9,mmsystem, ActiveX,  DirectDraw;

type
  TDirectShowInfo=record
    ErrorFlag:boolean;
    BitmapWidth,BitmapHeight:integer;
    TotalTimeSec,FramePerSec:double;
  end;

type
  TDSSupport = class(TForm)
  private
    { Private 宣言 }
    MediaFilename:string;
    RequestFPS:double;
    srcx,srcy,srcw,srch,dstw,dsth:integer;
    StartTimeSec:double;
    VerticalSwap,SmoothFrameBlending:boolean;
    RenderLastBM,RenderCurBM:TBitmap;
    RenderFirst:boolean;
    LastTimeSec,CurTimeSec:double;
    EndVideoFlag:boolean;
    procedure GetBitmap(FrameIndex:integer;bm:TBitmap);
  public
    { Public 宣言 }
    DirectShowInfo:TDirectShowInfo;
    function DirectShowCheck(MediaFilename:string):boolean;
    function DirectShowOpen(_MediaFilename:string;_RequestFPS:double;_srcx,_srcy,_srcw,_srch,_dstw,_dsth:integer;_VerticalSwap,_SmoothFrameBlending:boolean;_StartTimeSec,_EndTimeSec:double):boolean;
    function DirectShowRun:boolean;
    function DirectShowClose:boolean;
    procedure GetStretchBitmap(FrameIndex:integer;bm:TBitmap);
    function GetLastError:string;
  end;

var
  DSSupport: TDSSupport;

implementation

uses MainWin,DSSupport_videograb,DSUtil,enclogWin;

{$R *.dfm}

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
  VideoSampleGrabber:TVideoSampleGrabber;

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
  isWMV:boolean;
  pSrcFileFilter:IBaseFilter;
  pSrcFileVideoOutPin:IPin;
  ext:string;
  PinCount:integer;
  TargetPinIndex:integer;
  NullPinIndex:integer;
  pNullPin:IPin;
  function CheckWMV(fn:string):boolean;
  var
    rfs:TFileStream;
    id:dword;
  begin
    rfs:=TFileStream.Create(fn,fmOpenRead or fmShareDenyWrite);
    rfs.ReadBuffer(id,4);
    rfs.Free;
    if id=$75b22630 then begin
      Result:=True;
      end else begin
      Result:=False;
    end;
  end;
begin
  Result:=nil;

  if FileExists(fn)=False then exit;

  isWMV:=CheckWMV(fn);

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
    if (ext='.wmv') or (isWMV=True) then begin
      TargetPinIndex:=1;
      NullPinIndex:=0;
    end;
    if ext='.mp4' then begin
      TargetPinIndex:=0;
      NullPinIndex:=1;
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

function DirectShowStart(fn:string;var DirectShowInfo:TDirectShowInfo;StartTimeSec,EndTimeSec:double):boolean;
var
  hr:HRESULT;
  pSrcFileVideoOutPin:IPin;
  _TotalTimeSec:double;
begin
  Result:=False;

  with DirectShowInfo do begin
    ErrorFlag:=True;
    BitmapWidth:=0;
    BitmapHeight:=0;
    TotalTimeSec:=0;
    FramePerSec:=0;
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

  VideoSampleGrabber:=TVideoSampleGrabber.Create(nil);

  pSrcFileVideoOutPin:=DSOpenFile(fn);
  if pSrcFileVideoOutPin=nil then exit;

  // set clock

  if DSSetNullClock=False then exit;

  // ----

  hr:=pGraph.AddFilter(VideoSampleGrabber.FBaseFilter,PWideChar(WideString('Custom video sample grabber')));
  if Failed(hr) then begin
    DSLastError:='Error pGraph.AddFilter pGrabberF $'+inttohex(hr,8);
    exit;
  end;

  hr:=pGraph.Connect(pSrcFileVideoOutPin, VideoSampleGrabber.InPutPin);
  if Failed(hr) then begin
    DSLastError:='Error Connect $'+inttohex(hr,8);
    exit;
  end;

  pSrcFileVideoOutPin:=nil;

  // -------------------------------------------------------------

  if DSPinConnectToNullFilter(VideoSampleGrabber.OutPutPin)=False then exit;

  // ----------

  if DSSetTimeFormat=False then exit;

  _TotalTimeSec:=DSGetTotalTimeSec;
  if _TotalTimeSec=0 then exit;

  if (StartTimeSec<>0) and (EndTimeSec<>0) then begin
    _TotalTimeSec:=EndTimeSec-StartTimeSec;
  end;
  
  if 3<StartTimeSec then begin
    DSSeekTimeSec(StartTimeSec-3); // 失敗しても無視
  end;

  if VideoSampleGrabber.StartRender=False then begin
    DSLastError:='Error VideoSampleGrabber.StartRender';
    exit;
  end;

  with DirectShowInfo do begin
    ErrorFlag:=False;
    BitmapWidth:=VideoSampleGrabber.CurrentBM.Width;
    BitmapHeight:=VideoSampleGrabber.CurrentBM.Height;
    TotalTimeSec:=_TotalTimeSec;
    FramePerSec:=VideoSampleGrabber.FPS;
  end;

  Result:=True;
end;

procedure DirectShowEnd;
begin
  pGraph:=nil;

  VideoSampleGrabber.Free; VideoSampleGrabber:=nil;
end;

// --------------------------------------

function TDSSupport.DirectShowCheck(MediaFilename:string):boolean;
begin
  if DirectShowStart(MediaFilename,DirectShowInfo,0,0)=False then begin
    Result:=False;
    exit;
  end;

  if DirectShowInfo.ErrorFlag=True then begin
    DSLastError:='Error DirectShowInfo.ErrorFlag=True';
    Result:=False;
    exit;
  end;

  DirectShowEnd;

  Result:=True;
end;

function TDSSupport.DirectShowOpen(_MediaFilename:string;_RequestFPS:double;_srcx,_srcy,_srcw,_srch,_dstw,_dsth:integer;_VerticalSwap,_SmoothFrameBlending:boolean;_StartTimeSec,_EndTimeSec:double):boolean;
begin
  MediaFilename:=_MediaFilename;
  RequestFPS:=_RequestFPS;

  srcx:=_srcx;
  srcy:=_srcy;
  srcw:=_srcw;
  srch:=_srch;
  dstw:=_dstw;
  dsth:=_dsth;

  StartTimeSec:=_StartTimeSec;

  VerticalSwap:=_VerticalSwap;
  SmoothFrameBlending:=_SmoothFrameBlending;

  RenderLastBM:=TBitmap.Create;
  MakeBlankBM(RenderLastBM,dstw,dsth,pf24bit);
  RenderCurBM:=TBitmap.Create;
  MakeBlankBM(RenderCurBM,dstw,dsth,pf24bit);
  SetStretchBltMode(RenderCurBM.Canvas.Handle,HALFTONE);

  RenderFirst:=True;

  LastTimeSec:=-1000;
  CurTimeSec:=-1000;

  EndVideoFlag:=False;

  if DirectShowStart(MediaFilename,DirectShowInfo,_StartTimeSec,_EndTimeSec)=False then begin
    RenderLastBM.Free; RenderLastBM:=nil;
    RenderCurBM.Free; RenderCurBM:=nil;
    Result:=False;
    exit;
  end;

  if DirectShowInfo.ErrorFlag=True then begin
    RenderLastBM.Free; RenderLastBM:=nil;
    RenderCurBM.Free; RenderCurBM:=nil;
    DSLastError:='Error DirectShowInfo.ErrorFlag=True';
    Result:=False;
    exit;
  end;

  VideoSampleGrabber.GetBitmapExecFlip:=VerticalSwap;

  Result:=True;
end;

function TDSSupport.DirectShowRun:boolean;
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

function TDSSupport.DirectShowClose:boolean;
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
    VideoSampleGrabber.Free; VideoSampleGrabber:=nil;
    Result:=True;
    exit;
  end;

  if EventCode<>EC_COMPLETE then begin
    if VideoSampleGrabber.StopRender=False then begin
      DSLastError:='Error VideoSampleGrabber.StopRender';
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
    DSLastError:='Error pMediaControl.Stop $'+inttohex(hr,8);
    exit;
  end;

  pMediaControl:=nil;

  while(True) do begin
    hr:=pMediaEvent.WaitForCompletion(100,EventCode);
    if hr=VFW_E_WRONG_STATE then break;
  end;

  pMediaEvent:=nil;

  DirectShowEnd;

  if assigned(RenderLastBM) then begin
    RenderLastBM.Free; RenderLastBM:=nil;
  end;
  if assigned(RenderCurBM) then begin
    RenderCurBM.Free; RenderCurBM:=nil;
  end;
end;

procedure TDSSupport.GetBitmap(FrameIndex:integer;bm:TBitmap);
var
  TargetTimeSec:Double;
  x,y:integer;
  w,h:integer;
  blend,iblend:integer;
  plb,pcb,ptb:PByteArray;
  TimeoutCount:integer;
  BlendRangeSec:double;
  GetFrameCount:integer;
  procedure ProcSimpleCopy;
  begin
    BitBlt(bm.Canvas.Handle,0,0,w,h,RenderLastBM.Canvas.Handle,0,0,SRCCOPY);
  end;
begin
  w:=RenderCurBM.Width;
  h:=RenderCurBM.Height;

  if EndVideoFlag=True then begin
    ProcSimpleCopy;
    exit;
  end;

  TargetTimeSec:=FrameIndex/RequestFPS;

  TimeoutCount:=2000; // 4sec.

  GetFrameCount:=0;

  while(CurTimeSec<=(TargetTimeSec+0.0000002)) do begin
    inc(GetFrameCount);

    VideoSampleGrabber.GetBitmap_Start;
    while(True) do begin
      if VideoSampleGrabber.GetBitmap_isBusy=False then break;
      Application.ProcessMessages;
      if not assigned(VideoSampleGrabber) then exit;
      if VideoSampleGrabber.GetBitmap_isRequestStop=True then exit;
      sleep(2);
      dec(TimeoutCount);
      if TimeoutCount=0 then begin
        enclog.loglst.Lines.Add('end of video stream.');
        EndVideoFlag:=True;
        ProcSimpleCopy;
        exit;
      end;
    end;
    VideoSampleGrabber.GetBitmap_End;

    LastTimeSec:=CurTimeSec;
    CurTimeSec:=VideoSampleGrabber.CurrentTimeSec-StartTimeSec;

    BitBlt(RenderLastBM.Canvas.Handle,0,0,w,h,RenderCurBM.Canvas.Handle,0,0,SRCCOPY);

    SetStretchBltMode(RenderCurBM.Canvas.Handle,HALFTONE);
    StretchBlt(RenderCurBM.Canvas.Handle,0,0,w,h,VideoSampleGrabber.CurrentBM.Canvas.Handle,srcx,srcy,srcw,srch,SRCCOPY);
//    VideoSampleGrabber.CurrentBM.SaveToFile('c:\a.bmp');

//    RenderCurBM.SaveToFile('temp\'+format('%f,%f',[CurTimeSec,TargetTimeSec])+'.bmp');
  end;

  if GetFrameCount=0 then enclog.loglst.Lines.Add(format('Frame:%d Duplicate frame.',[FrameIndex]));
//  if 2<=GetFrameCount then enclog.loglst.Lines.Add(format('Frame:%d Drop frame. (%d)',[FrameIndex,GetFrameCount]));

  if SmoothFrameBlending=False then begin
    ProcSimpleCopy;
    exit;
  end;

  BlendRangeSec:=1/VideoSampleGrabber.FPS;
  if (CurTimeSec-LastTimeSec)<BlendRangeSec then BlendRangeSec:=CurTimeSec-LastTimeSec;

  if TargetTimeSec<=(CurTimeSec-BlendRangeSec) then begin
    ProcSimpleCopy;
    exit;
  end;

  iblend:=trunc((CurTimeSec-TargetTimeSec)/BlendRangeSec*$100);
  if iblend<0 then iblend:=0;
  if $100<iblend then iblend:=$100;
  blend:=$100-iblend;

  for y:=0 to h-1 do begin
    plb:=RenderLastBM.ScanLine[y];
    pcb:=RenderCurBM.ScanLine[y];
    ptb:=bm.ScanLine[y];
    for x:=0 to (w*3)-1 do begin
      ptb[x]:=(plb[x]*iblend div $100)+(pcb[x]*blend div $100);
    end;
  end;
end;

procedure TDSSupport.GetStretchBitmap(FrameIndex:integer;bm:TBitmap);
begin
  GetBitmap(FrameIndex,bm);
end;

function TDSSupport.GetLastError:string;
begin
  Result:=DSLastError;
end;

end.
