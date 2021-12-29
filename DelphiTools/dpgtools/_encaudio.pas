unit _encaudio;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls,_dpg_const;

function DemuxAudio_DirectShow(avifn:string;wavfn:string;Volume:integer):boolean;
function ConvertAudio_ssrc(srcfn,dstfn:string;TransRate2Pass:boolean;DstFreq,FakeFreq:integer):boolean;
function ConvertAudio_sox(srcfn,dstfn:string;TransRate2Pass:boolean;DstFreq,FakeFreq:integer):boolean;

function EncodeMP2_HQ32768Hz_twolame(srcfn,dstfn:string;freq:integer;kbps:integer):boolean;
function EncodeOGG_HQ_oggenc2(srcfn,dstfn:string;CommandLineFormat:string):boolean;

var
  encaudio_StartPath:string;
  encaudio_PluginPath:string;

implementation

uses _m_Tools,_queue,_dosbox,dpgenc_language,MainWin,enclogWin,DSSupport_audio;

const CRLF:string=char($0d)+char($0a);

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

  if wfs.Size<=(RiffHeaderSize*4) then begin
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

function DemuxAudio_DirectShow(avifn:string;wavfn:string;Volume:integer):boolean;
var
  wfs:TFileStream;
  TimeoutCount:integer;
  sampletime:double;
  lasttick:dword;
begin
  enclog.loglst.Lines.Add(format('Start DemuxAudio_DirectShow(%s,%s,%d);',[avifn,wavfn,Volume]));

  if DSAudio_Start(avifn,DPGEncode.StartTimeSec,DPGEncode.EndTimeSec)=False then begin
    enclog.loglst.Lines.Add(DSAudio_GetLastError);
    Current_SetError(lng(LI_GetAudioError),'');
    Result:=False;
    exit;
  end;

  if DSAudio_Run=False then begin
    enclog.loglst.Lines.Add(DSAudio_GetLastError);
    Current_SetError(lng(LI_GetAudioError),'');
    Result:=False;
    exit;
  end;

  enclog.loglst.Lines.Add(format('SampleRate=%dHz %dbits %dchannels',[DSAudio_GetSampleRate,DSAudio_GetSampleBits,DSAudio_GetSampleChs]));
  enclog.loglst.Lines.Add(format('TotalTime=%fsec',[DSAudio_GetTotalTimeSec]));

  SetMainTitle('Decode to wave... (use DirectShow)');

  if DPGEncode.StartTimeSec<>0 then enclog.loglst.Lines.Add(format('Audio seeking to %fsec',[DPGEncode.StartTimeSec]));

  SetPrgBarPos(0,'');
  SetPrgBarMax(trunc(DSAudio_GetTotalTimeSec*10));

  wfs:=TFileStream.Create(wavfn,fmCreate);
  RewriteWaveHeader(wfs,DSAudio_GetSampleRate,True);

  TimeoutCount:=2*60*100; // first timeout is 2min.

  lasttick:=GetTickCount-1000;

  while(True) do begin
    if AudioSampleGrabber.GetSampleStart=True then begin
      sampletime:=AudioSampleGrabber.SamplesCurrentTime;
      wfs.WriteBuffer(AudioSampleGrabber.SamplesBuf[0],AudioSampleGrabber.SamplesPosition*2);
      AudioSampleGrabber.SamplesPosition:=0;
      AudioSampleGrabber.GetSampleEnd;

      if (1000<=(GetTickCount-lasttick)) then begin
        lasttick:=GetTickCount;
        SetPrgBarPos(trunc(sampletime*10),'');
      end;

      TimeoutCount:=2*100; // timeout is 2sec.
    end;
    Application.ProcessMessages;
    if Current_GetRequestCancel=True then break;

    dec(TimeoutCount);
    if TimeoutCount=0 then break;
    sleep(10);
  end;

  enclog.Caption:='Encode Terminate';
  SetPrgBarPos(0,'Encode Terminate');

  DSAudio_Close;

  RewriteWaveHeader(wfs,DSAudio_GetSampleRate,True);

  wfs.Free;

  enclog.loglst.Lines.Add('Close DirectShow');

  try
    enclog.loglst.Lines.SaveToFile(changefileext(Application.ExeName,'')+'_encode.log');
    except else begin
    end;
  end;

  Result:=True;
end;

function ConvertAudio_ssrc(srcfn,dstfn:string;TransRate2Pass:boolean;DstFreq,FakeFreq:integer):boolean;
var
  PassCount,PassMax:integer;
  hInputRead,hInputWrite:THANDLE;
  hOutputRead,hOutputWrite:THANDLE;
  ErrorStr:string;
  PipeBufStr:string;
  CaptionText:string;
  lasttick:dword;
  wfs:TFileStream;
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
  function StartEncode:boolean;
  var
    appfn:string;
    cmdline:string;
    batfn:string;
    batfs:TFileStream;
    str:string;
  begin
    appfn:=encaudio_PluginPath+'ssrc.exe';
    batfn:=encaudio_PluginPath+'ssrc.bat';

    if fileexists(appfn)=False then begin
      Current_SetError('not found plugin.',appfn);
      Result:=False;
      exit;
    end;

    if CreatePipes(1024)=False then begin
      Result:=False;
      exit;
    end;

    cmdline:='--rate '+inttostr(DstFreq)+' --dither 0 --bits 16 --normalize --pdf 2 --profile fast';
    if TransRate2Pass=True then cmdline:=cmdline+' --twopass';
    cmdline:=cmdline+' --tmpfile "'+ChangeFileExt(dstfn,'.$$$')+'"';
    cmdline:=cmdline+' "'+srcfn+'" "'+dstfn+'"';

    enclog.loglst.Lines.Add(appfn);
    enclog.loglst.Lines.Add(cmdline);
    enclog.loglst.Lines.Add('');

    batfs:=TFileStream.Create(batfn,fmCreate);
    str:='"'+appfn+'" '+cmdline+CRLF;
    batfs.WriteBuffer(str[1],length(str));
    batfs.Free;

    if CreateDOSBOX2(encaudio_PluginPath,hInputRead,hOutputWrite,hOutputWrite,batfn,'')=False then begin
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
    PerPos:integer;
    PerStr:string;
    PerInt:integer;
  begin
    Result:='';

    len:=0;
    if PeekNamedPipe(hnd, nil, 0, nil,@len,nil)=True then begin
      if len<>0 then begin
        if 1024<=len then len:=1024;
        if ReadFile(hnd,ansistr[0],len,readsize,nil)=True then begin
          for i:=0 to readsize-1 do begin
            c:=ansistr[i];
            if (c<>ansichar($0d)) and (c<>ansichar($0a)) then begin
              PipeBufStr:=PipeBufStr+c;
              end else begin
              if PipeBufStr<>'' then begin
                if PipeBufStr='Pass 2' then PassCount:=1;

                PerStr:=PipeBufStr;
                PerPos:=ansipos('% processed',PerStr);
                if PerPos<>0 then begin
                  PerStr:=copy(PerStr,1,PerPos-1);
                  PerInt:=strtointdef(PerStr,-1);
                  if (1000<=(GetTickCount-lasttick)) then begin
                    lasttick:=GetTickCount;
                    if PerInt<>-1 then SetPrgBarPos((PassCount*100)+PerInt,'');
                  end;
                  PipeBufStr:='';
                end;

                if PipeBufStr<>'' then begin
                  CaptionText:=PipeBufStr;
                  Result:=Result+PipeBufStr+CRLF;
                end;
              end;
              PipeBufStr:='';
            end;
          end;
        end;
      end;
    end;
  end;
  procedure EndEncode;
  var
    batfn:string;
  begin
    if Current_GetRequestCancel=False then begin
      CloseDOSBOX2(True); // èIóπÇë“Ç¬
      end else begin
      CloseDOSBOX2(False);
    end;

    ErrorStr:=ReadPipe(hOutputRead);
    if ErrorStr<>'' then begin
      enclog.loglst.Lines.Add(ErrorStr);
      enclog.loglst.Refresh;
    end;
    try
      enclog.loglst.Lines.SaveToFile(changefileext(Application.ExeName,'')+'_encode.log');
      except else begin
      end;
    end;

    CloseHandle(hInputRead);
    CloseHandle(hInputWrite);
    CloseHandle(hOutputRead);
    CloseHandle(hOutputWrite);

    batfn:=encaudio_PluginPath+'ssrc.bat';
    DeleteFile(batfn);
  end;
begin
  if StartEncode=False then begin
    Result:=False;
    exit;
  end;

  SetMainTitle('Convert sampling rate... (use ssrc)');

  PassCount:=0;
  if TransRate2Pass=False then begin
    PassMax:=1;
    end else begin
    PassMax:=2;
  end;

  SetPrgBarPos(0,'');
  SetPrgBarMax(PassMax*100);

  PipeBufStr:='';
  CaptionText:='';

  lasttick:=GetTickCount-1000;

  while(isTerminatedDOSBOX2=False) do begin
    ErrorStr:=ReadPipe(hOutputRead);
    if ErrorStr<>'' then begin
      enclog.loglst.Lines.Add(ErrorStr);
      enclog.loglst.Refresh;
    end;

    Application.ProcessMessages;
    if Current_GetRequestCancel=True then break;
    sleep(100);
  end;

  enclog.Caption:='Encode Terminate';
  SetPrgBarPos(0,'Encode Terminate');

  EndEncode;

  if Current_GetRequestCancel=True then begin
    Result:=False;
    exit;
  end;

  if FileExists(ChangeFileExt(dstfn,'.$$$'))=True then DeleteFile(ChangeFileExt(dstfn,'.$$$'));

  if GetFileSize(dstfn)<=44 then begin
    Result:=ConvertAudio_sox(srcfn,dstfn,TransRate2Pass,DstFreq,FakeFreq);
    exit;
  end;

  wfs:=TFileStream.Create(dstfn,fmOpenReadWrite);
  RewriteWaveHeader(wfs,FakeFreq,True);
  wfs.Free;

  Result:=True;
end;

function ConvertAudio_sox(srcfn,dstfn:string;TransRate2Pass:boolean;DstFreq,FakeFreq:integer):boolean;
var
  appfn:string;
  cmdline:string;
  wfs:TFileStream;
begin
  appfn:=encaudio_PluginPath+'sox.exe';

  if fileexists(appfn)=False then begin
    Current_SetError('not found plugin.',appfn);
    Result:=False;
    exit;
  end;

  SetMainTitle('Convert sampling rate... (use sox)');

  cmdline:=format('"%s" -c 2 -r %d "%s"',[srcfn,DstFreq,dstfn]);

  enclog.loglst.Lines.Add(appfn);
  enclog.loglst.Lines.Add(cmdline);
  enclog.loglst.Lines.Add('');
  enclog.loglst.Lines.SaveToFile(changefileext(Application.ExeName,'')+'_encode.log');

  CreateDOSBOX_UseCMD(ExtractFilePath(srcfn),appfn,cmdline);

  enclog.Caption:='Encode Terminate';
  SetPrgBarPos(0,'Encode Terminate');

  if GetFileSize(dstfn)<=44 then begin
    Current_SetError(lng(LI_GetAudioError),'');
    Result:=False;
    exit;
  end;

  wfs:=TFileStream.Create(dstfn,fmOpenReadWrite);
  RewriteWaveHeader(wfs,FakeFreq,True);
  wfs.Free;

  Result:=True;
end;

function EncodeMP2_HQ32768Hz_twolame(srcfn,dstfn:string;freq:integer;kbps:integer):boolean;
var
  hInputRead,hInputWrite:THANDLE;
  hOutputRead,hOutputWrite:THANDLE;
  ErrorStr:string;
  PipeBufStr:string;
  PipeBufFlag:boolean;
  lasttick:dword;
  CaptionText:string;
  perlen:integer;
  permax:integer;
  perstr:string;
  perint:integer;
  batfn:string;
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
  function StartEncode:boolean;
  var
    appfn:string;
    cmdline:string;
    wfs:TFileStream;
    str:string;
  begin
    appfn:=encaudio_PluginPath+'twolame.exe';
    batfn:=ChangeFileExt(appfn,'.bat');

    if fileexists(appfn)=False then begin
      Current_SetError('not found plugin.',appfn);
      Result:=False;
      exit;
    end;

    if CreatePipes(1024)=False then begin
      Result:=False;
      exit;
    end;

    cmdline:=format('-t 4 -b %d -m j "'+srcfn+'" "'+dstfn+'"',[kbps]);
    enclog.loglst.Lines.Add(appfn);
    enclog.loglst.Lines.Add(cmdline);
    enclog.loglst.Lines.Add('');

    wfs:=TFileStream.Create(batfn,fmCreate);
    str:='"'+ChangeFileExt(appfn,'.exe')+'" '+cmdline;
    wfs.WriteBuffer(str[1],length(str));
    wfs.Free;

    cmdline:='';
    if CreateDOSBOX2(encaudio_PluginPath,hInputRead,hOutputWrite,hOutputWrite,batfn,cmdline)=False then begin
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
      CloseDOSBOX2(True); // èIóπÇë“Ç¬
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
begin
  if StartEncode=False then begin
    Result:=False;
    exit;
  end;

  SetMainTitle('High quality encode to mp2... (use twolame)');

  permax:=GetFileSize(srcfn) div 6000;

  SetPrgBarPos(0,'');
  SetPrgBarMax(permax);

  PipeBufStr:='';
  PipeBufFlag:=False;
  CaptionText:='';

  lasttick:=GetTickCount-1000;

  while(isTerminatedDOSBOX2=False) do begin
    if (1000<=(GetTickCount-lasttick)) then begin
      lasttick:=GetTickCount;
      PerStr:=CaptionText;
      PerLen:=length(PerStr);
      if (6<=PerLen) and (PerLen<10) then begin
        if (PerStr[1]='[') and (PerStr[PerLen]=']') then begin
          PerStr:=copy(PerStr,2,PerLen-2);
          PerInt:=strtointdef(PerStr,0);
          if perint<permax then SetPrgBarPos(PerInt,'');
          enclog.Caption:=CaptionText;
        end;
        CaptionText:='';
      end;
    end;

    ErrorStr:=ReadPipe(hOutputRead);
    if ErrorStr<>'' then begin
      enclog.loglst.Lines.Add(ErrorStr);
      enclog.loglst.Refresh;
    end;

    Application.ProcessMessages;
    if Current_GetRequestCancel=True then break;
  end;

  enclog.Caption:='Encode Terminate';
  SetPrgBarPos(0,'Encode Terminate');

  EndEncode;

  DeleteFile(batfn);

  if Current_GetRequestCancel=True then begin
    Result:=False;
    exit;
  end;

  if GetFileSize(dstfn)=0 then begin
    Current_SetError('error func EncodeMP2_HQ32768Hz_twolame','');
    Result:=False;
    exit;
  end;

  Result:=True;
end;

function EncodeOGG_HQ_oggenc2(srcfn,dstfn:string;CommandLineFormat:string):boolean;
var
  appfn:string;
  datapath:string;
  tmpwavfn,tmpoggfn:string;
begin
  appfn:=encaudio_PluginPath+'oggenc2.exe';

  if fileexists(appfn)=False then begin
    Current_SetError('not found plugin.',appfn);
    Result:=False;
    exit;
  end;

  datapath:=ExtractFilePath(srcfn);

  tmpwavfn:=datapath+'$$temp$$.wav';
  tmpoggfn:=datapath+'$$temp$$.ogg';
  RenameFile(srcfn,tmpwavfn);

  SetMainTitle('High quality encode to ogg... (use oggenc2)');

  CreateDOSBOX_UseCMD(datapath,appfn,format(CommandLineFormat,[ExtractFilename(tmpwavfn),ExtractFilename(tmpoggfn)]));

  RenameFile(tmpwavfn,srcfn);
  RenameFile(tmpoggfn,dstfn);

  if GetFileSize(dstfn)=0 then begin
    Current_SetError('error func EncodeOGG_HQ_oggenc2','');
    Result:=False;
    exit;
  end;

  Result:=True;
end;

end.

