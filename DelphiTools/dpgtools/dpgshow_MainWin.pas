unit dpgshow_MainWin;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls,_PicTools, MPlayer, MMSystem ,dpgshow_PCMOut;

type
  TMain = class(TForm)
    DPGOpenDlg: TOpenDialog;
    Timer1: TTimer;
    IntervalTimer: TTimer;
    StatusBar1: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure IntervalTimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
  private
    { Private 宣言 }
    PCMOut:TPCMOut;
  public
    { Public 宣言 }
    procedure playmovie;
    function UpdateWaveMonitor(buf:PSmallInt;SamplesCount:integer):boolean;
  end;

var
  Main: TMain;

const AppID='dpgshow for DPG0/1/2HQ';

implementation

{$R *.dfm}

uses _dpg_const, dpgshow_DSSupportWin, dpgshow_info,dpgshow_inifile;

var
  CanClose:boolean;

procedure CopyData(itemname:string;srcfn,dstfn:string;pos,size:integer);
var
  rfs,wfs:TFileStream;
  buf:array of byte;
  blocksize:integer;
begin
  setlength(buf,size);

  Main.Caption:=itemname+' ReadData.';

  rfs:=TFileStream.Create(srcfn,fmOpenRead or fmShareDenyWrite);
  rfs.Position:=pos;
  wfs:=TFileStream.Create(dstfn,fmCreate);
  wfs.Position:=0;

  while(size<>0) do begin
    blocksize:=1*1024*1024;
    if size<blocksize then blocksize:=size;
    setlength(buf,blocksize);
    rfs.ReadBuffer(buf[0],blocksize);
    wfs.WriteBuffer(buf[0],blocksize);
    dec(size,blocksize);
    Main.StatusBar1.SimpleText:=format(' %dbyte',[size]);
    Main.StatusBar1.Refresh;
    Application.ProcessMessages;
  end;

  Main.StatusBar1.SimpleText:='';
  Main.StatusBar1.Refresh;

  rfs.Free;
  wfs.Free;
end;

procedure TMain.FormCreate(Sender: TObject);
begin
  Application.Title:=AppID;
  Main.Caption:=AppID;

  LoadINI;
  SaveINI;

  Timer1.Enabled:=True;
end;

var
  dpgfn,mp2fn,oggfn,m1vfn:string;
  DPGINFO:TDPGINFO;
  SourceFPS:double;
  VideoWidth,VideoHeight:integer;

function GetDSInfo(m1vfn:string):boolean;
begin
  DSSupport.DSOpen(m1vfn,0,0,0);
  SourceFPS:=DSSupport.GetSourceFPS;
  DSSupport.DSClose;

  VideoWidth:=DSSupport.VideoWidth;
  VideoHeight:=DSSupport.VideoHeight;

  Result:=True;
  if SourceFPS=0 then Result:=False;
  if (VideoWidth=0) or (VideoHeight=0) then Result:=False;
end;

procedure TMain.Timer1Timer(Sender: TObject);
var
  fsize:integer;
  path:string;
  wh:integer;
begin
  Timer1.Enabled:=False;
  CanClose:=False;

  dpgfn:=ParamStr(1);
  if FileExists(dpgfn)=False then dpgfn:='';

  if dpgfn='' then begin
    if DPGOpenDlg.Execute=True then dpgfn:=DPGOpenDlg.FileName;
  end;

  if dpgfn='' then begin
    Application.Terminate;
    exit;
  end;

  Main.Caption:='Read DPGINFO...';

  if LoadDPGINFO(DPGINFO,path+dpgfn,fsize)=0 then begin
    ShowMessage('対応していないバージョンです。');
    exit;
  end;

  path:=ExtractFilePath(dpgfn);
  dpgfn:=ExtractFilename(dpgfn);

  mp2fn:=path+'$$$tmp_'+ChangeFileExt(dpgfn,'.mp2');
  oggfn:=path+'$$$tmp_'+ChangeFileExt(dpgfn,'.ogg');
  m1vfn:=path+'$$$tmp_'+ChangeFileExt(dpgfn,'.m1v');
  dpgfn:=path+dpgfn;

  if DPGINFO.SndCh=0 then begin
    CopyData('Audio',dpgfn,mp2fn,DPGINFO.AudioPos,DPGINFO.AudioSize);
  end;
  if (DPGINFO.SndCh=1) or (DPGINFO.SndCh=2) then begin
    ShowMessage('Can not decode GSM audio. (old DPG?)');
    exit;
  end;
  if DPGINFO.SndCh=3 then begin
    CopyData('Audio',dpgfn,oggfn,DPGINFO.AudioPos,DPGINFO.AudioSize);
  end;

  CopyData('Movie',dpgfn,m1vfn,DPGINFO.MoviePos,DPGINFO.MovieSize);

  Main.Caption:='Open DPG...';
  GetDSInfo(m1vfn);

  Main.ClientWidth:=VideoWidth;
  if ShowStatusBar=False then begin
    wh:=VideoHeight;
    end else begin
    wh:=VideoHeight+StatusBar1.Height;
  end;
  Main.ClientHeight:=wh;
  StatusBar1.Refresh;

  if ShowInfomationWindow=True then begin
    Info.Left:=Main.Left;
    Info.Top:=Main.Top+Main.Height;
    Info.DPGInfoMemo.Lines.Text:=LoadDPGINFOString(dpgfn);
    Info.Visible:=True;
  end;

  Main.Caption:=AppID;
  playmovie;

  CanClose:=True;
end;

var
  FrameIndex:integer;
  FrameCount:integer;
  LastTick:dword;

procedure TMain.playmovie;
var
  tc:TTIMECAPS;
begin
  timeGetDevCaps(@tc,sizeof(TTIMECAPS));
  timeBeginPeriod(tc.wPeriodMin);

  FrameCount:=DPGINFO.TotalFrame;
  FrameIndex:=0;

  DSSupport.DSOpen(m1vfn,DPGINFO.FPS,VideoWidth,VideoHeight);

  PCMOut:=TPCMOut.Create;
  PCMOut.Init(200,DPGINFO.SndFreq);

  if DPGINFO.SndCh=0 then begin
    GetRAWPCM_Start(mp2fn,PCMOut.PCMBufSize div 2);
  end;
  if (DPGINFO.SndCh=1) or (DPGINFO.SndCh=2) then begin
  end;
  if DPGINFO.SndCh=3 then begin
    GetRAWPCM_Start(oggfn,PCMOut.PCMBufSize div 2);
  end;
  PCMOut.StartDSound;

  LastTick:=GetTickCount+(200*2);

  StatusBar1.Visible:=ShowStatusBar;

  IntervalTimer.Enabled:=True;
end;

function TMain.UpdateWaveMonitor(buf:PSmallInt;SamplesCount:integer):boolean;
begin
  Result:=GetRAWPCM_Decode(buf,SamplesCount);
end;

procedure TMain.IntervalTimerTimer(Sender: TObject);
var
  bm:TBitmap;
  Aspect:double;
  w,h:integer;
  procedure ConvertPixelFormat;
  var
    pb:PByteArray;
    x,y:integer;
    shift:integer;
    c:dword;
  begin
    case DPGINFO.PixelFormat of
      DPGPixelFormat_RGB15: shift:=3;
      DPGPixelFormat_RGB18: shift:=2;
      DPGPixelFormat_RGB21: shift:=1;
      DPGPixelFormat_RGB24: shift:=0;
      else shift:=0;
    end;
    for y:=0 to VideoHeight-1 do begin
      pb:=bm.ScanLine[y];
      for x:=0 to (VideoWidth*3)-1 do begin
        c:=pb[x] shl shift;
        if $ff<c then c:=$ff;
        pb[x]:=c;
      end;
    end;
  end;
begin
  IntervalTimer.Enabled:=False;

  if FrameIndex=FrameCount then begin
    Main.Close;
    exit;
  end;

  if GetTickCount<trunc(lasttick+(FrameIndex/DPGINFO.FPS*1000)) then begin
    IntervalTimer.Enabled:=True;
    exit;
  end;

  Aspect:=VideoWidth/VideoHeight;
  w:=Main.ClientWidth;
  h:=trunc(w/Aspect);

  bm:=TBitmap.Create;
  MakeBlankBM(bm,VideoWidth,VideoHeight,pf24bit);

  DSSupport.GetStretchBitmap(FrameIndex,bm,False);
  ConvertPixelFormat;

  SetStretchBltMode(Main.Canvas.Handle,HALFTONE);
  StretchBlt(Main.Canvas.Handle,0,0,w,h,bm.Canvas.Handle,0,0,VideoWidth,VideoHeight,SRCCOPY);

  bm.Free;

  inc(FrameIndex);

  StatusBar1.SimpleText:=format(' %d/%dframes %d/%dsec.',[FrameIndex,FrameCount,trunc(FrameIndex/DPGINFO.FPS),trunc(FrameCount/DPGINFO.FPS)]);

  IntervalTimer.Enabled:=True;
end;

procedure TMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if CanClose=False then begin
    Action:=caNone;
    exit;
  end;

  IntervalTimer.Enabled:=False;
  Info.Visible:=False;

  Main.Caption:='Close DPG...';

  PCMOut.StopDriver;
  PCMOut.FreeMemory;

  GetRAWPCM_End;

  DSSupport.Close;

  if FileExists(mp2fn)=True then DeleteFile(mp2fn);
  if FileExists(oggfn)=True then DeleteFile(oggfn);
  if FileExists(m1vfn)=True then DeleteFile(m1vfn);

  timeEndPeriod(1);
end;

procedure TMain.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
var
  xofs,yofs:integer;
  Aspect:double;
begin
  if IntervalTimer.Enabled=False then exit;

  xofs:=Main.Width-Main.ClientWidth;
  if ShowStatusBar=False then begin
    yofs:=Main.Height-(Main.ClientHeight-0);
    end else begin
    yofs:=Main.Height-(Main.ClientHeight-Main.StatusBar1.Height);
  end;

  Aspect:=VideoWidth/VideoHeight;
  NewHeight:=trunc((NewWidth-xofs)/Aspect)+yofs;
end;

end.
