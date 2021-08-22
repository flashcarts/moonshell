unit _dpg_const;

interface

uses
  Windows, SysUtils, Classes;

type
  TDPGINFO=record
    TotalFrame:integer;
    FPS:double;
    SndFreq,SndCh:integer;
    AudioPos,AudioSize:integer;
    MoviePos,MovieSize:integer;
    GOPListPos,GOPListSize:integer;
    PixelFormat:integer;
  end;

const DPGIDBETA=$32475044; // DPG2

const DPG0ID=$30475044;
const DPG1ID=$31475044;
const DPG2ID=$32475044;
const DPG3ID=$33475044;
const DPG4ID=$34475044;

const DPGPixelFormat_RGB15=0;
const DPGPixelFormat_RGB18=1;
const DPGPixelFormat_RGB21=2;
const DPGPixelFormat_RGB24=3;
const DPGPixelFormat_ENUMCOUNT=4;

function LoadDPGINFO(var DPGINFO:TDPGINFO;fn:string;var FileSize:integer):dword;
function LoadDPGINFOString(DPGfn:string):string;

implementation

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

function LoadDPGINFO(var DPGINFO:TDPGINFO;fn:string;var FileSize:integer):dword;
var
  rfs:TFileStream;
  function ru32:dword;
  var
    d:dword;
  begin
    rfs.ReadBuffer(d,4);
    Result:=d;
  end;
  function ri32:integer;
  var
    d:integer;
  begin
    rfs.ReadBuffer(d,4);
    Result:=d;
  end;
begin
  rfs:=TFileStream.Create(fn,fmOpenRead);
  FileSize:=rfs.Size;

  Result:=ru32;

{
  if Result=DPGIDBETA then begin
    rfs.Free;
    exit;
  end;
}

  if (Result=DPG0ID) or (Result=DPG1ID) or (Result=DPG2ID) or (Result=DPG3ID) or (Result=DPG4ID) then begin
    DPGINFO.TotalFrame:=ri32;
    DPGINFO.FPS:=ri32/$100;
    DPGINFO.SndFreq:=ri32;
    DPGINFO.SndCh:=ri32;
    DPGINFO.AudioPos:=ri32;
    DPGINFO.AudioSize:=ri32;
    DPGINFO.MoviePos:=ri32;
    DPGINFO.MovieSize:=ri32;

    if (Result=DPG2ID) or (Result=DPG3ID) or (Result=DPG4ID) then begin
      DPGINFO.GOPListPos:=ri32;
      DPGINFO.GOPListSize:=ri32;
    end;

    if Result=DPG0ID then DPGINFO.PixelFormat:=DPGPixelFormat_RGB24;
    if (Result=DPG1ID) or (Result=DPG2ID) or (Result=DPG3ID) or (Result=DPG4ID) then DPGINFO.PixelFormat:=ri32;

    end else begin
    Result:=0;
  end;

  rfs.Free;
end;

function LoadDPGINFOString(DPGfn:string):string;
var
  strlst:TStringList;
  DPGINFO:TDPGINFO;
  ID:dword;
  cnt:integer;
  FileSize:integer;
  tmp:string;
  pfstr:string;
begin
  if FileExists(DPGfn)=False then begin
    Result:='file not found.';
    exit;
  end;

  ID:=LoadDPGINFO(DPGINFO,DPGfn,FileSize);

  if ID=0 then begin
    Result:='Error. no DPG file.';
    exit;
  end;

  strlst:=TStringList.Create;

  strlst.Clear;

  strlst.Add(format('DPGFilename=%s',[ExtractFilename(DPGfn)]));
  strlst.Add(format('FileSize=%dbyte',[FileSize]));
  strlst.Add('');

  if ID=DPGIDBETA then begin
    strlst.Add('beta version data');
//    Result:=strlst.Text;
//    exit;
  end;

  tmp:='';
  for cnt:=0 to 4-1 do begin
    tmp:=tmp+char(ID and $ff);
    ID:=ID shr 8;
  end;

  strlst.Add('HeaderID:'+tmp);
  strlst.Add(format('TotalFrame=%d',[DPGINFO.TotalFrame]));
  strlst.Add(format('FramePerSec=%.4f',[DPGINFO.FPS]));
  strlst.Add(format('TotalTime=%.4fsec(%.4fmin)',[DPGINFO.TotalFrame/DPGINFO.FPS,DPGINFO.TotalFrame/DPGINFO.FPS/60]));
  strlst.Add(format('SoundRate=%dHz',[DPGINFO.SndFreq]));
  if DPGINFO.SndCh=0 then begin
    strlst.Add(format('SoundChannels=Build-in mpeg1-audio-layer-2 (mp2)',[]));
  end;
  if (DPGINFO.SndCh=1) or (DPGINFO.SndCh=2) then begin
    strlst.Add(format('SoundChannels=GSM %dch(s)',[DPGINFO.SndCh]));
  end;
  if DPGINFO.SndCh=3 then begin
    strlst.Add(format('SoundChannels=OGG %dch(s)',[DPGINFO.SndCh]));
  end;
  strlst.Add(format('AudioData=%d->%dbyte',[DPGINFO.AudioPos,DPGINFO.AudioSize]));
  strlst.Add(format('MovieData=%d->%dbyte',[DPGINFO.MoviePos,DPGINFO.MovieSize]));
  strlst.Add(format('GOPListData=%d->%dbyte',[DPGINFO.GOPListPos,DPGINFO.GOPListSize]));

  pfstr:='unknown';
  case DPGINFO.PixelFormat of
    DPGPixelFormat_RGB15: pfstr:='RGB15';
    DPGPixelFormat_RGB18: pfstr:='RGB18';
    DPGPixelFormat_RGB21: pfstr:='RGB21';
    DPGPixelFormat_RGB24: pfstr:='RGB24';
  end;

  strlst.Add(format('PixelFormat=%d %s',[DPGINFO.PixelFormat,pfstr]));

  Result:=strlst.Text;
end;

end.
