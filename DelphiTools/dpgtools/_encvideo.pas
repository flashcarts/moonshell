unit _encvideo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls,_dpg_const,MainWin;

var
  encvideo_StartPath:string;
  encvideo_PluginPath:string;

function EncodeVideoDPG(avifn,m1vfn:string;DPGEncode:TDPGEncode):boolean;
function CreateGOPList(m1vfn,GOPListfn:string):boolean;

implementation

uses DSSupportWin,_m_Tools,_queue,_dosbox,dpgenc_language,enclogWin,_PicTools,encprvWin,_dpgfs;

const CRLF:string=char($0d)+char($0a);

type
  TDSMVideoInfo=record
    FrameCount:dword;
    Width:dword;
    Height:dword;
    SampleRate:dword;
  end;

// ----------------------------------------


// ---------------------------------------

procedure BrightnessEffect(var bm:TBitmap;Brightness:integer);
var
  br:integer;
  pb:PByteArray;
  y,x:integer;
  c:integer;
begin
  if Brightness=100 then exit;
  br:=(Brightness*$100) div 100;
  for y:=0 to bm.Height-1 do begin
    pb:=bm.ScanLine[y];
    for x:=0 to bm.Width*3-1 do begin
      c:=pb[x];
      c:=(c*br) div $100;
      if c<=$ff then begin
        pb[x]:=c;
        end else begin
        pb[x]:=$ff;
      end;
    end;
  end;
end;

var
  Progress_StartTick:dword;

procedure Progress_Start;
begin
  Progress_StartTick:=GetTickCount;
end;

procedure Progress_Refresh(FrameIdx,TotalFrame:integer);
var
  ttf:integer;
  edt:dword;
  per:double;
  unt:double;
  function tick2time(t:double):string;
  var
    ti:integer;
    h,m,s:integer;
  begin
    ti:=trunc(t/1000);
    s:=ti mod 60;
    ti:=ti div 60;
    m:=ti mod 60;
    ti:=ti div 60;
    h:=ti;
    Result:=format('%d:%2.2d:%2.2d',[h,m,s]);
  end;
  function size2str(s:double):string;
  begin
    Result:=format('%dMByte',[trunc(s/1024/1024)]);
  end;
begin
  if FrameIdx=0 then exit;
  ttf:=TotalFrame;
  edt:=GetTickCount-Progress_StartTick;
  per:=edt/FrameIdx;
  unt:=per*(ttf-FrameIdx);

  Current_SetProgress(format(lng(LI_VideoEncodeProgressFormat),[tick2time(edt),tick2time(unt)]));
end;

procedure Progress_End;
begin
  Current_SetProgress('');
end;

function EncodeVideoDPG(avifn,m1vfn:string;DPGEncode:TDPGEncode):boolean;
var
  lasttick_bar,lasttick_info:dword;
  hInputRead,hInputWrite:THANDLE;
  hOutputRead,hOutputWrite:THANDLE;
  FrameIdx:integer;
  w,h:integer;
  bm:TBitmap;
  ErrorStr:string;
  readsize:dword;
  PipeBufStr:string;
  PipeBufFlag:boolean;
  CaptionText:string;
  function CreatePipes(ReadBufSize:dword):boolean;
  var
    SA:SECURITY_ATTRIBUTES;
    SD:TSecurityDescriptor;
  begin
    hInputRead:=0;
    hInputWrite:=0;
    hOutputRead:=0;
    hOutputWrite:=0;

    sa.nLength:=sizeof(SECURITY_ATTRIBUTES);
    sa.lpSecurityDescriptor:=nil;
    sa.bInheritHandle:=True;
    InitializeSecurityDescriptor(@SD,SECURITY_DESCRIPTOR_REVISION);
    SetSecurityDescriptorDacl(@SD,True,nil,False);
    sa.lpSecurityDescriptor:=@SD;

    if CreatePipe(hInputRead,hInputWrite,@sa,ReadBufSize)=False then begin
      Current_SetError(lng(LI_PipeErrorCreate),'');
      Result:=False;
      exit;
    end;
    if DuplicateHandle(GetCurrentProcess(),hInputWrite,GetCurrentProcess(),nil,0,False,DUPLICATE_SAME_ACCESS)=False then begin
      Current_SetError(lng(LI_PipeErrorAttribute),'');
      Result:=False;
      exit;
    end;

    if CreatePipe(hOutputRead,hOutputWrite,@sa,0)=False then begin
      Current_SetError(lng(LI_PipeErrorCreate),'');
      Result:=False;
      exit;
    end;
    if DuplicateHandle(GetCurrentProcess(),hOutputRead,GetCurrentProcess(),nil,0,False,DUPLICATE_SAME_ACCESS)=False then begin
      Current_SetError(lng(LI_PipeErrorAttribute),'');
      Result:=False;
      exit;
    end;

    Result:=True;
  end;
  function StartEncode(w,h:integer;fps:double;frames,kbps:integer):boolean;
  var
    appfn:string;
    cmdline:string;
    fixkbps:integer;
  begin
    appfn:=PluginPath+'mencoder.exe';

    if fileexists(appfn)=False then begin
      Current_SetError('not found plugin.',appfn);
      Result:=False;
      exit;
    end;

    if CreatePipes(w*3*h)=False then begin
      Result:=False;
      exit;
    end;

    fixkbps:=trunc(kbps*24/fps);
    cmdline:=format(DPGEncode.CmdLineFormat,[frames,w,h,w*3*h,fixkbps]);
    cmdline:='-v - -o "'+m1vfn+'" '+cmdline;
    enclog.loglst.Lines.Add(appfn);
    enclog.loglst.Lines.Add(cmdline);
    enclog.loglst.Lines.Add('');

    if CreateDOSBOX2(StartPath,hInputRead,hOutputWrite,hOutputWrite,appfn,cmdline)=False then begin
      Current_SetError('CreateDOSBOX error.','');
      Result:=False;
      exit;
    end;

    Result:=True;
  end;
  function ReadPipe(hnd:THANDLE):string;
  var
    ansistr:array[0..1024] of ansichar;
    i:integer;
    len:dword;
    readsize:dword;
    c:ansichar;
  begin
    Result:='';

    len:=0;
    if PeekNamedPipe(hnd, nil, 0, nil,@len,nil)=True then begin
      if len<>0 then begin
        if 1024<=len then len:=1024;
        if ReadFile(hnd,ansistr[0],len,readsize,nil)=True then begin
          for i:=0 to readsize-1 do begin
            c:=ansistr[i];
            if PipeBufFlag=False then begin
              if c=ansichar($0d) then begin
                PipeBufFlag:=True;
                end else begin
                PipeBufStr:=PipeBufStr+c;
              end;
              end else begin
              PipeBufFlag:=False;
              if c=ansichar($0a) then begin
                Result:=Result+PipeBufStr+CRLF;
                PipeBufStr:='';
                end else begin
                CaptionText:=PipeBufStr;
                PipeBufStr:=c;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
  procedure EndEncode;
  begin
    if Current_GetRequestCancel=False then begin
      CloseDOSBOX2(True); // I—¹‚ð‘Ò‚Â
      end else begin
      CloseDOSBOX2(False);
    end;

    ErrorStr:=ReadPipe(hOutputRead);
    if ErrorStr<>'' then begin
      enclog.loglst.Lines.Add(ErrorStr);
      enclog.loglst.Refresh;
    end;
    try
      enclog.loglst.Lines.SaveToFile(changefileext(Application.ExeName,'')+'_mencoder.log');
      except else begin
      end;
    end;

    CloseHandle(hInputRead);
    CloseHandle(hInputWrite);
    CloseHandle(hOutputRead);
    CloseHandle(hOutputWrite);
  end;
  procedure SendRAW(var bm:TBitmap;w,h:integer);
  var
    pb:PByteArray;
    size,y:integer;
    buf:array of byte;
  begin
    size:=w*3*h;
    setlength(buf,size);
    for y:=0 to h-1 do begin
      pb:=bm.ScanLine[y];
      CopyMemory(@buf[w*3*y],@pb[0],w*3);
    end;
    WriteFile(hInputWrite,buf[0],size,readsize,nil);
  end;
begin
  with DPGEncode do begin
    w:=dstw;
    h:=dsth;
    if StartEncode(w,h,FPS,DPGINFO.TotalFrame,kbps)=False then begin
      Result:=False;
      exit;
    end;
  end;

  with DPGEncode do begin
    DSSupport.DirectShowOpen(avifn,FPS,srcx,srcy,srcw,srch,dstw,dsth,VerticalSwap,SmoothFrameBlending,StartTimeSec,EndTimeSec);
  end;

  if DSSupport.DirectShowRun=False then begin
    DSSupport.DirectShowClose;
    Result:=False;
    exit;
  end;

  bm:=TBitmap.Create;
  MakeBlankBM(bm,w,h,pf24bit);

  enclog.loglst.Lines.Add('--- Start DPG encode');
  enclog.loglst.Lines.Add(format('SourceVideoSize=%dx%dpixels',[DSSupport.DirectShowInfo.BitmapWidth,DSSupport.DirectShowInfo.BitmapHeight]));
  enclog.loglst.Lines.Add(format('SourceFPS=%fframes TotalTime=%fsecs',[DSSupport.DirectShowInfo.FramePerSec,DSSupport.DirectShowInfo.TotalTimeSec]));
  enclog.loglst.Lines.Add('');

  SetPrgBarPos(0,'');
  SetPrgBarMax(DPGEncode.TotalFrame);

  CaptionText:='';

  lasttick_bar:=GetTickCount-1000;
  lasttick_info:=GetTickCount-5000;

  for FrameIdx:=0 to DPGINFO.TotalFrame-1 do begin
    DSSupport.GetStretchBitmap(FrameIdx,bm);
    BrightnessEffect(bm,DPGEncode.Brightness);

    if FrameIdx=0 then Progress_Start;

    if (FrameIdx<>0) and (1000<=(GetTickCount-lasttick_bar)) then begin
      lasttick_bar:=GetTickCount;
      SetPrgBarPos(FrameIdx,'');
      BitBlt(encprv.Canvas.Handle,0,0,w,h,bm.Canvas.Handle,0,0,SRCCOPY);
      if CaptionText<>'' then begin
        enclog.Caption:=CaptionText;
        CaptionText:='';
      end;
    end;
    if (FrameIdx<>0) and (5000<=(GetTickCount-lasttick_info)) then begin
      lasttick_info:=GetTickCount;
      Progress_Refresh(FrameIdx,DPGEncode.TotalFrame);
    end;

    SendRAW(bm,w,h);

    ErrorStr:=ReadPipe(hOutputRead);
    if ErrorStr<>'' then begin
      enclog.loglst.Lines.Add(ErrorStr);
      enclog.loglst.Refresh;
    end;

    Application.ProcessMessages;
    if Current_GetRequestCancel=True then break;

    if isTerminatedDOSBOX2=True then break;
  end;

  if Current_GetRequestCancel=False then Progress_End;

  enclog.Caption:='Encode Terminate';
  SetPrgBarPos(0,'Encode Terminate');

  bm.Free;

  DSSupport.DirectShowClose;

  EndEncode;

  try
    enclog.loglst.Lines.SaveToFile(changefileext(Application.ExeName,'')+'_encode.log');
    except else begin
    end;
  end;

  if Current_GetRequestCancel=True then begin
    Result:=False;
    exit;
  end;

  if GetFileSize(m1vfn)=0 then begin
    Current_SetError('unknown encoding error.','');
    Result:=False;
    exit;
  end;

  Result:=True;
end;

const readbufsize=64*1024;
const chkbufsize=$14+2;

type
  TGOPList=record
    FrameIndex:dword;
    Offset:dword;
  end;

function CreateGOPList(m1vfn,GOPListfn:string):boolean;
var
  GOPList:array of TGOPList;
  GOPListCount:integer;
  rfs:TFileStream;
  filepos,filesize:integer;
  readbufpos:integer;
  readbuf:array[0..readbufsize-1] of byte;
  chkbuf:array[0..chkbufsize-1] of byte;
  chkflag:boolean;
  FrameIndex:integer;
  PicData:dword;
  PicData_refidx,PicData_type:integer;
  wfs:TFileStream;
  idx:integer;
  procedure getchkbuf;
  var
    idx:integer;
  begin
    if readbufsize<=(readbufpos+chkbufsize) then begin
      rfs.Position:=filepos;
      if (filepos+readbufsize)<filesize then begin
        rfs.ReadBuffer(readbuf[0],readbufsize);
        end else begin
        rfs.ReadBuffer(readbuf[0],filesize-filepos);
      end;
      readbufpos:=0;
    end;
    for idx:=0 to chkbufsize-1 do begin
      chkbuf[idx]:=readbuf[readbufpos+idx];
    end;
  end;
begin
  FrameIndex:=0;

  GOPListCount:=0;

  rfs:=TFileStream.Create(m1vfn,fmOpenRead);

  filepos:=0;
  filesize:=rfs.Size;

  SetPrgBarPos(0,'');
  Main.prgbar.Max:=filesize;

  rfs.Position:=filepos;
  rfs.ReadBuffer(readbuf[0],readbufsize);
  readbufpos:=0;

  while((filepos+chkbufsize)<filesize) do begin
    getchkbuf;

    if (chkbuf[$00]=$00) and (chkbuf[$01]=$00) then begin
      chkflag:=True;
      // Seq.Header
      if chkbuf[$02]<>$01 then chkflag:=False;
      if chkbuf[$03]<>$b3 then chkflag:=False;
      if chkbuf[$08]<>$ff then chkflag:=False;
      if chkbuf[$09]<>$ff then chkflag:=False;

      if chkflag=True then begin
        SetPrgBarPos(filepos,'');
        setlength(GOPList,GOPListCount+1);
        GOPList[GOPListCount].FrameIndex:=FrameIndex;
        GOPList[GOPListCount].Offset:=filepos;
        inc(GOPListCount);
      end;

      chkflag:=True;
      // Pic.Header
      if chkbuf[$02]<>$01 then chkflag:=False;
      if chkbuf[$03]<>$00 then chkflag:=False;

      if chkflag=True then begin
        PicData:=0;
        PicData:=PicData or (chkbuf[$04] shl 24);
        PicData:=PicData or (chkbuf[$05] shl 16);
        PicData:=PicData or (chkbuf[$06] shl 8);
        PicData:=PicData or (chkbuf[$07] shl 0);
        PicData_refidx:=PicData shr 22;
        PicData_type:=(PicData shr 19) and 7;
        if (PicData_refidx<$400) and ((PicData_type=1) or (PicData_type=2) or (PicData_type=3)) then begin
          inc(FrameIndex);
        end;
      end;
    end;

    inc(filepos);
    inc(readbufpos);
  end;

  rfs.Free;

  SetPrgBarPos(0,'');

  wfs:=TFileStream.Create(GOPListfn,fmCreate);
  for idx:=0 to GOPListCount-1 do begin
    wfs.WriteBuffer(GOPList[idx].FrameIndex,4);
    wfs.WriteBuffer(GOPList[idx].Offset,4);
  end;
  wfs.Free;

  Result:=True;
end;

end.

