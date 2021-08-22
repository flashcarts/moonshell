unit DSSupport_videograb;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SyncObjs,
  DSPack, DSUtil, BaseClass,
  DirectShow9,mmsystem, ActiveX,  DirectDraw, ExtCtrls, StdCtrls;

type

//******************************************************************************
//
//  TFilterSampleGrabber declaration
//  description: Sample Grabber Wrapper Filter
//
//******************************************************************************
  {@exclude}
  TVideoSampleGrabber = class;

  { This class is designed make a snapshoot of Video or Audio Datas.
    WARNING: There is know problems with some DIVX movies, so use RGB32 Media Type
    instead of RBG24.}
  TVideoSampleGrabber = class(TComponent, IFilter, ISampleGrabberCB)
  private
    function GetFilter: IBaseFilter;
    function GetName: string;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure NotifyFilter(operation: TFilterOperation; Param: integer = 0);
    function GetBitmap(Buffer: Pointer; BufferLen: Integer): boolean; overload;
    function  SampleCB(SampleTime: Double; pSample: IMediaSample): HResult; stdcall;
    function  BufferCB(SampleTime: Double; pBuffer: PByte; BufferLen: longint): HResult; stdcall;
  public
    FCriticalSection: TCriticalSection;
    FBaseFilter: IBaseFilter;
    SampleGrabber: ISampleGrabber;
    InPutPin  : IPin;
    OutPutPin : IPin;
    FPS:double;
    GetBitmapExecFlip:boolean;
    GetBitmapReady:boolean;
    GetBitmapRequestStop:boolean;
    CurrentBM:TBitmap;
    CurrentBMFlip:boolean;
    CurrentTimeSec:double;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
    function StartRender:boolean;
    function StopRender:boolean;
    procedure GetBitmap_Start;
    function GetBitmap_isBusy:boolean;
    function GetBitmap_isRequestStop:boolean;
    procedure GetBitmap_Cancel;
    procedure GetBitmap_End;
  published
  end;

implementation

// *****************************************************************************
//  TSampleGrabber
// *****************************************************************************

  function TVideoSampleGrabber.GetFilter: IBaseFilter;
  begin
    result := FBaseFilter;
  end;

  function TVideoSampleGrabber.GetName: string;
  begin
    result := name;
  end;

  constructor TVideoSampleGrabber.Create(AOwner: TComponent);
  var
    EnumPins: IEnumPins;
    FMediaType: TMediaType;
  begin
    inherited Create(AOwner);
    FCriticalSection := TCriticalSection.Create;

    GetBitmapExecFlip:=False;
    GetBitmapReady:=False;
    GetBitmapRequestStop:=False;
    CurrentBM:=nil;
    CurrentBMFlip:=False;
    CurrentTimeSec:=0;

    Cocreateinstance(CLSID_SampleGrabber, nil, CLSCTX_INPROC ,IID_IBASEFilter, FBaseFilter);
    FBaseFilter.QueryInterface(IID_ISampleGrabber,SampleGrabber);

    FBaseFilter.EnumPins(EnumPins);
    EnumPins.Next(1,InPutPin,nil);
    EnumPins.Next(1,OutPutPin,nil);
    EnumPins := nil;

    FMediaType := TMediaType.Create(MEDIATYPE_Video);
    FMediaType.SubType := MEDIASUBTYPE_RGB24;
    FMediaType.FormatType := FORMAT_VideoInfo;
    SampleGrabber.SetMediaType(FMediaType.AMMediaType^);
    FMediaType.Free;

    SampleGrabber.SetBufferSamples(true);
    SampleGrabber.SetOneShot(false);
    SampleGrabber.SetCallback(Self,1);
  end;

  destructor TVideoSampleGrabber.Destroy;
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

    if CurrentBM<>nil then begin
      CurrentBM.Free; CurrentBM:=nil;
    end;

    FCriticalSection.Free;
    inherited destroy;
  end;

  procedure TVideoSampleGrabber.Notification(AComponent: TComponent; Operation: TOperation);
  begin
    inherited Notification(AComponent, Operation);
  end;

  procedure TVideoSampleGrabber.NotifyFilter(operation: TFilterOperation; Param: integer = 0);
  begin
  end;

  function TVideoSampleGrabber.QueryInterface(const IID: TGUID; out Obj): HResult;
  begin
    result := inherited QueryInterface(IID, Obj);
    if failed(result) and assigned(FBaseFilter) then
      result := FBaseFilter.QueryInterface(IID, Obj);
  end;

  function TVideoSampleGrabber.StartRender:boolean;
  var
    hr: HRESULT;
    MediaType: TAMMediaType;
    pvih:PVideoInfoHeader;
    pvih2:PVideoInfoHeader2;
    w,h:integer;
    OneFrameSec:int64;
  begin
    Result:=False;

    if not Assigned(SampleGrabber) then exit;
    if assigned(CurrentBM) then exit; // ä˘Ç…äJénÇ≥ÇÍÇƒÇ¢ÇÈÅH

    w:=0;
    h:=0;
    OneFrameSec:=0;

    hr:=SampleGrabber.GetConnectedMediaType(MediaType);
    if hr=S_OK then begin
      if IsEqualGUID(MediaType.majortype, MEDIATYPE_Video) then begin
        pvih:=PVideoInfoHeader(MediaType.pbFormat);
        w:=pvih.bmiHeader.biWidth;
        h:=pvih.bmiHeader.biHeight;
        OneFrameSec:=pvih.AvgTimePerFrame;
      end;
      if IsEqualGUID(MediaType.formattype, FORMAT_VideoInfo2) then begin
        pvih2:=PVideoInfoHeader2(MediaType.pbFormat);
        w:=pvih2.bmiHeader.biWidth;
        h:=pvih2.bmiHeader.biHeight;
        OneFrameSec:=pvih2.AvgTimePerFrame;
      end;
      FreeMediaType(@MediaType);
    end;

    if (w=0) or (h=0) or (OneFrameSec=0) then exit;

    case OneFrameSec of
      417083-1: FPS:=23.976;
      417083+0: FPS:=23.976;
      417083+1: FPS:=23.976;
      416666-1: FPS:=24;
      416666+0: FPS:=24;
      416666+1: FPS:=24;
      400000-1: FPS:=25;
      400000+0: FPS:=25;
      400000+1: FPS:=25;
      333667-1: FPS:=29.97;
      333667+0: FPS:=29.97;
      333667+1: FPS:=29.97;
      333333-1: FPS:=30;
      333333+0: FPS:=30;
      333333+1: FPS:=30;
      200000-1: FPS:=50;
      200000+0: FPS:=50;
      200000+1: FPS:=50;
      166833-1: FPS:=59.94;
      166833+0: FPS:=59.94;
      166833+1: FPS:=59.94;
      166666-1: FPS:=60;
      166666+0: FPS:=60;
      166666+1: FPS:=60;
      else FPS:=10000000/OneFrameSec;
    end;

    CurrentBM:=TBitmap.Create;
    CurrentBM.PixelFormat:=pf24bit;
    CurrentBM.Width:=w;
    CurrentBM.Height:=h;
    CurrentBMFlip:=True;
    if CurrentBM.Height<0 then begin
      CurrentBMFlip:=False;
      CurrentBM.Height:=-CurrentBM.Height;
    end;

    Result:=True;
  end;

  function TVideoSampleGrabber.StopRender:boolean;
  var
    readflag:boolean;
  begin
    FCriticalSection.Enter;
    GetBitmapRequestStop:=True;
    FCriticalSection.Leave;

    FCriticalSection.Enter;
    GetBitmapReady:=True;
    FCriticalSection.Leave;
    while(True) do begin
      FCriticalSection.Enter;
      readflag:=GetBitmapReady;
      FCriticalSection.Leave;
      if readflag=False then break;
      sleep(1);
    end;

    Result:=True;
  end;

type
  PLargeByteArray = ^TLargeByteArray;
  TLargeByteArray = array[0..1024*1024*1024] of Byte;

  function TVideoSampleGrabber.GetBitmap(Buffer: Pointer; BufferLen: Integer): Boolean;
  var
    w,h,y:integer;
    psrc,pdst:PLargeByteArray;
    flip:boolean;
  begin
    Result := False;

    if not Assigned(CurrentBM) then exit;

    if not Assigned(Buffer) then exit;
    if BufferLen=0 then exit;

    flip:=CurrentBMFlip;
    if GetBitmapExecFlip=True then flip:=not flip;

    w:=CurrentBM.Width;
    h:=CurrentBM.Height;

    for y:=0 to h-1 do begin
      psrc:=PLargeByteArray(Buffer);
      psrc:=@psrc[y*(w*3)];
      if flip=False then begin
        pdst:=CurrentBM.ScanLine[y];
        end else begin
        pdst:=CurrentBM.ScanLine[h-1-y];
      end;
      MoveMemory(pdst,psrc,w*3);
    end;
  end;

  function TVideoSampleGrabber.BufferCB(SampleTime: Double; pBuffer: PByte;
    BufferLen: Integer): HResult;
  var
    fr:boolean;
  begin
    while(true) do begin
      FCriticalSection.Enter;
      fr:=GetBitmapRequestStop;
      FCriticalSection.Leave;
      if fr=True then begin
        FCriticalSection.Enter;
        GetBitmapReady:=False;
        FCriticalSection.Leave;
        result := S_FALSE;
        exit;
      end;

      FCriticalSection.Enter;
      fr:=GetBitmapReady;
      FCriticalSection.Leave;
      if fr=True then break;

      sleep(2);
    end;

    CurrentTimeSec:=SampleTime;

    if GetBitmap(pBuffer, BufferLen)=False then begin
      result := S_FALSE;
      end else begin
      result := S_OK;
    end;

    FCriticalSection.Enter;
    GetBitmapReady:=False;
    FCriticalSection.Leave;
  end;

  function TVideoSampleGrabber.SampleCB(SampleTime: Double;
    pSample: IMediaSample): HResult;
  begin
    result := S_OK;
  end;

  procedure TVideoSampleGrabber.GetBitmap_Start;
  begin
    FCriticalSection.Enter;
    GetBitmapReady:=True;
    FCriticalSection.Leave;
  end;

  function TVideoSampleGrabber.GetBitmap_isBusy:boolean;
  begin
    FCriticalSection.Enter;
    Result:=GetBitmapReady;
    FCriticalSection.Leave;
  end;

  function TVideoSampleGrabber.GetBitmap_isRequestStop:boolean;
  begin
    FCriticalSection.Enter;
    Result:=GetBitmapRequestStop;
    FCriticalSection.Leave;
  end;

  procedure TVideoSampleGrabber.GetBitmap_Cancel;
  begin
    FCriticalSection.Enter;
    GetBitmapRequestStop:=True;
    GetBitmapReady:=True;
    FCriticalSection.Leave;
  end;

  procedure TVideoSampleGrabber.GetBitmap_End;
  begin
  end;

end.
