unit MainWin;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls,_PicTools, StdCtrls,_dpg_const,_dpgfs,_dosbox, ComCtrls,_m_Tools,dpgenc_language,
  Menus, ShellAPI, Grids;

const DPGEncVersion='dpgenc.exe for MoonShell2';

type
  TMain = class(TForm)
    EncodeProcTimer: TTimer;
    StartupTimer: TTimer;
    prgbar: TProgressBar;
    EncodeStartTimer: TTimer;
    PreencodeffmpegTimer: TTimer;
    StatusBar1: TStatusBar;
    MainMenu1: TMainMenu;
    MainMenu_Option: TMenuItem;
    MainMenu_Language: TMenuItem;
    MainMenu_About: TMenuItem;
    MainMenu_Language_Template: TMenuItem;
    StandbyTimer: TTimer;
    ing_ngimg: TImage;
    ing_okimg: TImage;
    QueueGrid: TStringGrid;
    MainMenu_ShowLog: TMenuItem;
    QueuePopup: TPopupMenu;
    QueuePopup_Delete: TMenuItem;
    OutputPathChangeBtn: TButton;
    OutputPathLbl: TLabel;
    OutputPathDlg: TSaveDialog;
    EncodeEndTimer: TTimer;
    PreencodeAutoDetectTimer: TTimer;
    MainMenu_AutoShutdown: TMenuItem;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure EncodeProcTimerTimer(Sender: TObject);
    procedure StartupTimerTimer(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure PreencodeffmpegTimerTimer(Sender: TObject);
    procedure EncodeStartTimerTimer(Sender: TObject);
    procedure MainMenu_Language_TemplateClick(Sender: TObject);
    procedure MainMenu_OptionClick(Sender: TObject);
    procedure StandbyTimerTimer(Sender: TObject);
    procedure QueueGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure MainMenu_ShowLogClick(Sender: TObject);
    procedure QueuePopup_DeleteClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure MainMenu_AboutClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure OutputPathChangeBtnClick(Sender: TObject);
    procedure EncodeEndTimerTimer(Sender: TObject);
    procedure PreencodeAutoDetectTimerTimer(Sender: TObject);
    procedure QueueGridKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure QueueGridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure QueueGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure MainMenu_AutoShutdownClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private 宣言 }
    procedure WMDROPFILES(var msg:TWMDROPFILES);message WM_DROPFILES;
  public
    { Public 宣言 }
    optReencWidth,optReencKBPS,optReencFPS:integer;
    OptOutputPath:string;
    MainFormWidth,MainFormHeight:integer;
    function StartEncode:boolean;
    procedure LoadLanguage;
  end;

var
  Main: TMain;

procedure SetPrgBarMax(max:integer);
procedure SetPrgBarPos(pos:integer;zerostr:string);

const SndFormat_MP2=0;
const SndFormat_OGG=1;

type
  TDPGEncode=record
    StartTimeSec,EndTimeSec:double;
    TotalFrame:integer;
    srcx,srcy,srcw,srch:integer;
    dstw,dsth:integer;
    FPS:double;
    kbps:integer;
    Brightness:integer;
    VerticalSwap:boolean;
    SmoothFrameBlending:boolean;
    CmdLineFormat:string;
    RequestDeleteOverrideSource:boolean;
    SndVolume:integer;
    Sndkbps:integer;
  end;

var
  DPGEncode:TDPGEncode;

var
  StartPath:string;
  LanguagePath:string;
  PluginPath:string;

var
  DPGINFO:TDPGINFO;

implementation

uses OptionWin, enclogWin, encprvWin, OptionCmdLineWin,_queue,_encvideo,_encaudio,_extset,
  DSSupportWin, ShutdownWin, NetworkWin;

{$R *.dfm}

const CRLF:string=char($0d)+char($0a);

//const ffmpeg_cmdlineformat='-v 1 -y -vcodec mpeg1video -qscale 4 -acodec mp3 -ab 160 -i "%s" "%s"';
const ffmpeg_cmdlineformat='-v 1 -y -i "%s" -f avi -vcodec msmpeg4v2 -b 1024000 -bt 512000 -acodec libmp3lame -ab 256000 -ar 48000 -ac 2 "%s"';

const BELOW_NORMAL_PRIORITY_CLASS=$00004000;
const ABOVE_NORMAL_PRIORITY_CLASS=$00008000;

procedure SetPriorityLevel(Level:integer);
begin
  case Level of
    0: SetPriorityClass(GetCurrentProcess,IDLE_PRIORITY_CLASS);
    1: SetPriorityClass(GetCurrentProcess,BELOW_NORMAL_PRIORITY_CLASS);
    2: SetPriorityClass(GetCurrentProcess,NORMAL_PRIORITY_CLASS);
    3: SetPriorityClass(GetCurrentProcess,ABOVE_NORMAL_PRIORITY_CLASS);
    4: SetPriorityClass(GetCurrentProcess,HIGH_PRIORITY_CLASS);
    5: SetPriorityClass(GetCurrentProcess,REALTIME_PRIORITY_CLASS);
    else SetPriorityClass(GetCurrentProcess,NORMAL_PRIORITY_CLASS);
  end;
end;

function isCheckOS:boolean;
var
  osverinfo:OSVERSIONINFO;
begin
  osverinfo.dwOSVersionInfoSize:=sizeof(OSVERSIONINFO);
  GetVersionEx(osverinfo);

  if osverinfo.dwPlatformId=VER_PLATFORM_WIN32_WINDOWS then begin
    Result:=False;
    end else begin
    Result:=True;
  end;

end;

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------

var
  MasterFontCodepage:integer;
  MasterFontName:string;
  MasterFontSize:integer;

procedure ResetLanguageSet;
var
  res:integer;
  SearchRec: TSearchRec;
  fname:string;
  NewItem:TMenuItem;
  ttl:TStringList;
  procedure ClearAndFree(var t:TMenuItem);
  var
    cnt,count:integer;
  begin
    count:=t.Count;
    for cnt:=count-1 downto 0 do begin
      t.Delete(cnt);
    end;
  end;
begin
  ClearAndFree(Main.MainMenu_Language);

  res:=FindFirst(LanguagePath+ChangeFileExt(extractfilename(Application.ExeName),'')+'.*.lng', (FaAnyFile), SearchRec);
  if res=0 then begin
    repeat
      fname:=SearchRec.Name;
      if (SearchRec.Attr and faDirectory)=0 then begin
        ttl:=TStringList.Create;
        ttl.LoadFromFile(LanguagePath+fname);
        fname:=ChangeFileExt(fname,'');
        fname:=ExtractFileExt(fname);
        NewItem:=TMenuItem.Create(Main.MainMenu_Language);
        NewItem.Enabled:=True;
        NewItem.Caption:=fname+' / '+ttl[0];
        NewItem.Tag:=0;
        NewItem.Checked:=False;
        NewItem.OnClick:=Main.MainMenu_Language_TemplateClick;
        Main.MainMenu_Language.Add(NewItem);
        ttl.Free;
      end;
      res:=FindNext(SearchRec);
    until (res<>0);
  end;
  FindClose(SearchRec);
end;

procedure TMain.LoadLanguage;
var
  FontName:string;
  FontSize:integer;
  FontCodepage:integer;
  idx:integer;
  function GetLang:string;
  var
    t:TMenuItem;
    cnt,count:integer;
    tlang:string;
  begin
    t:=Main.MainMenu_Language;
    count:=t.Count;
    for cnt:=0 to count-1 do begin
      if t.Items[cnt].Checked=True then begin
        tlang:=t.Items[cnt].Caption;
        tlang:=copy(tlang,1,ansipos(' ',tlang)-1);
        Result:=tlang;
        exit;
      end;
    end;
    tlang:=t.Items[0].Caption;
    tlang:=copy(tlang,1,ansipos(' ',tlang)-1);
    Result:=tlang;
  end;
  procedure SetFont(font:TFont);
  begin
    with font do begin
      Charset:=FontCodepage;
      Name:=FontName;
      Size:=FontSize;
      Charset:=FontCodepage;
    end;
  end;
begin
  LoadLngFile(LanguagePath+ChangeFileExt(extractfilename(Application.ExeName),'')+GetLang+'.lng');

  FontName:=lngID(LI_ID_FontName);
  if FontName='' then FontName:=MasterFontName;
  FontSize:=strtointdef(lngID(LI_ID_FontSize),MasterFontSize);
  FontCodepage:=strtointdef(lngID(LI_ID_FontCodepage),MasterFontCodepage);

  SetFont(Main.Font);
  SetFont(Option.Font);
  SetFont(OptionCmdLine.Font);
  SetFont(enclog.Font);
  SetFont(QueueGrid.Font);

  QueueGrid.DefaultRowHeight:=2+(QueueGrid.Canvas.TextHeight('Agyz')*2);
  for idx:=0 to QueueGrid.RowCount-1 do begin
    QueueGrid.RowHeights[idx]:=QueueGrid.DefaultRowHeight;
  end;

  with Main do begin
    OutputPathChangeBtn.Caption:=lng(LI_OutputPathChange);
    MainMenu_Option.Caption:=lng(LI_MainMenu_Option);
    MainMenu_ShowLog.Caption:=lng(LI_MainMenu_ShowLog);
    MainMenu_About.Caption:=lng(LI_MainMenu_About);
    MainMenu_AutoShutdown.Caption:=lng(LI_MainMenu_AutoShutdown);
    QueuePopup_Delete.Caption:=lng(LI_CancelPopup);
  end;

  with Option do begin
    CancelBtn.Caption:=lng(LI_FormCancel);
    StartBtn.Caption:=lng(LI_FormStart);
    with PreencLst do begin
      idx:=ItemIndex;
      Items[0]:=lng(LI_Preenc_DS);
      Items[1]:=lng(LI_Preenc_ffmpeg);
      ItemIndex:=idx;
    end;
    SoundGrp.Caption:=lng(LI_FormAudioOptions);
    SoundBPSLbl.Caption:=lng(LI_FormAudioBPS);
    SoundBPSUnitLbl.Caption:=lng(LI_FormAudioBPSUnit);
    SoundVolumeLbl.Caption:=lng(LI_FormAudioVolume);
    ReencOptGroup.Caption:=lng(LI_FormVideoOptions);

    VideoAspectLbl.Caption:=lng(LI_FormVideoAspect);
    with ReencAspectLst do begin
      idx:=ItemIndex;
      Items[0]:=lng(LI_FormVideoAspectItem0);
      Items[1]:=lng(LI_FormVideoAspectItem1);
      Items[2]:=lng(LI_FormVideoAspectItem2);
      Items[3]:=lng(LI_FormVideoAspectItem3);
      ItemIndex:=idx;
    end;
    VideoBPSLbl.Caption:=lng(LI_FormVideoBPS);
    VideoBPSUnitLbl.Caption:=lng(LI_FormVideoBPSUnit);
    VideoFPSLbl.Caption:=lng(LI_FormVideoFPS);
    VideoFPSUnitLbl.Caption:=lng(LI_FormVideoFPSUnit);
    ReencFPSAutoChk.Caption:=lng(LI_FormVideoFPSAuto);
    VideoBrightLbl.Caption:=lng(LI_FormVideoBright);
    VideoBlurLbl.Caption:=lng(LI_FormVideoBlur);
    VideoBlurDeepLbl.Caption:=lng(LI_FormVideoBlurDeep);
    VideoBlurLightLbl.Caption:=lng(LI_FormVideoBlurLight);
    ReencVerticalSwapChk.Caption:=lng(LI_FormVideoFlip);

    AdvanceChk.Caption:=lng(LI_OptionAdvance);

  end;

  MainMenu_Language.Caption:='Language (&L)';
end;

procedure RefreshOutputPathLbl;
begin
  Main.OutputPathLbl.Caption:=lng(LI_OutputPathTag)+Main.OptOutputPath;
end;

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------

const StoreBufSize=1*1024*1024;

function CreateDPG:boolean;
var
  wfs:TFileStream;
  ofspos:integer;
  posa,sizea:integer;
  posv,sizev:integer;
  posg,sizeg:integer;
  function AddFile(ttl:string;fn:string):integer;
  var
    rfs:TFileStream;
    buf:array of byte;
    size:integer;
    idx:integer;
    bufsize:integer;
  begin
    if FileExists(fn)=False then begin
      Current_SetError('empty file.',fn);
      Result:=0;
      exit;
    end;

    Main.Caption:=ttl;

    setlength(buf,StoreBufSize);

    try
      rfs:=TFileStream.Create(fn,fmOpenRead);
      except else begin
        Current_SetError('can not open file.',fn);
        Result:=0;
        exit;
      end;
    end;

    size:=rfs.Size;
    if size<>0 then begin
      SetPrgBarPos(0,'');
      Main.prgbar.Max:=size div StoreBufSize;
      idx:=0;
      while(idx<size) do begin
        bufsize:=size-idx;
        if StoreBufSize<=bufsize then bufsize:=StoreBufSize;
        rfs.ReadBuffer(buf[0],bufsize);
        wfs.WriteBuffer(buf[0],bufsize);
        inc(idx,bufsize);

        SetPrgBarPos(idx div StoreBufSize,'');
      end;
      SetPrgBarPos(0,'');
    end;
    rfs.Free;

    Result:=size;
  end;
  procedure AddDW(dw:dword);
  begin
    wfs.WriteBuffer(dw,4);
  end;
  procedure padding32bit;
  var
    tmp:byte;
  begin
    tmp:=$00;
    if (wfs.Size mod 4)<>0 then wfs.WriteBuffer(tmp,1);
    if (wfs.Size mod 4)<>0 then wfs.WriteBuffer(tmp,1);
    if (wfs.Size mod 4)<>0 then wfs.WriteBuffer(tmp,1);
  end;
begin
  SetPrgBarPos(0,'Create DPG file');

  try
    wfs:=TFileStream.Create(GetDPGFilename,fmCreate,fmShareExclusive);
    except else begin
      Current_SetError('can not open file.',GetDPGFilename);
      Result:=False;
      exit;
    end;
  end;

  AddDW(DPG4ID);

  AddDW(DPGINFO.TotalFrame);
  AddDW(trunc(DPGINFO.FPS*$100));
  AddDW(DPGINFO.SndFreq);
  AddDW(DPGINFO.SndCh);

  ofspos:=wfs.Position;

  AddDW(0);
  AddDW(0);
  AddDW(0);
  AddDW(0);
  AddDW(0);
  AddDW(0);

  AddDW(DPGINFO.PixelFormat);

  posa:=wfs.Position;
  sizea:=AddFile('Store Audio...',GetDPGMP2Filename);
  padding32bit();

  posv:=wfs.Position;
  sizev:=AddFile('Store Video...',GetDPGMovieFilename);
  padding32bit();

  posg:=wfs.Position;
  sizeg:=AddFile('Store GOP list...',GetDPGGOPListFilename);
  padding32bit();

  wfs.Position:=ofspos;

  AddDW(posa);
  AddDW(sizea);
  AddDW(posv);
  AddDW(sizev);
  AddDW(posg);
  AddDW(sizeg);

  wfs.Free;

  Result:=True;
end;

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------

procedure TMain.FormCreate(Sender: TObject);
begin
  Main.OnResize:=nil;

  StartPath:=ExtractFilePath(Application.ExeName);
  LanguagePath:=StartPath+'dpgenclng\';
  PluginPath:=StartPath+'dpgencplugin\';

  if isCheckOS=False then begin
    Application.Title:='windows version check.';
    ShowMessage('WindowsNT系(Win2000/XP等)のOSでしか起動できません。'+CRLF+CRLF+'The tool related to DPG works only in the WindowsNT system. (Win2k/WinXP etc.)');
    Application.Terminate;
    exit;
  end;

  MainFormWidth:=Main.Width;
  MainFormHeight:=Main.Height;

  setlog;

  SetBaseFilename('','');

  DragAcceptFiles(Main.handle,True); // D&D Start

  StartupTimer.Enabled:=True;
end;

var
  LastPrgBarMsg:string;

procedure SetPrgBarMax(max:integer);
begin
  Main.prgbar.Max:=max;
end;

procedure SetPrgBarPos(pos:integer;zerostr:string);
begin
  Main.prgbar.Position:=pos;
  if (pos<>0) and (Main.prgbar.Max<>0) then begin
//    Main.QueueGrid.Cells[0,QueueGetQueueIdx]:=inttostr((Main.prgbar.Position*100) div Main.prgbar.Max)+'%';
    Main.QueueGrid.Cells[0,QueueGetQueueIdx]:=inttostr(Main.prgbar.Position)+'/ '+inttostr(Main.prgbar.Max);
    end else begin
    if zerostr<>'' then LastPrgBarMsg:=zerostr;
    Main.QueueGrid.Cells[0,QueueGetQueueIdx]:=LastPrgBarMsg;
    Application.ProcessMessages;
  end;
end;

procedure TMain.WMDROPFILES(var msg:TWMDROPFILES);
var
  Drop:hdrop;
  index:longint;
  idx:integer;
  Filename:string;
  filebuf:array[0..1024] of char;
  cnt:integer;
  str:string;
begin
  Filename:=StringOfChar(' ',1024);
  Drop:=msg.Drop;
  index:=DragQueryFile(Drop,$FFFFFFFF,nil,0);

  for idx:=0 to index-1 do begin
    DragQueryFile(Drop,idx,filebuf,1024);
    Filename:='';
    cnt:=0;
    while ((filebuf[cnt]<>char($00)) and (cnt<1024)) do begin
      Filename:=Filename+filebuf[cnt];
      inc(cnt);
    end;
    if lowercase(ExtractFileExt(Filename))='.ini' then begin
      str:=ExtendSetting_CheckFormat(Filename);
      if str<>'' then begin
        ShowMessage('拡張設定ファイル読み込みエラー'+CRLF+CRLF+str);
        Filename:='';
      end;
    end;
    if FileExists(Filename)=True then QueueAdd(Filename);
  end;

  DragFinish(Drop);
end;

procedure TMain.StartupTimerTimer(Sender: TObject);
var
  idx:integer;
begin
  StartupTimer.Enabled:=False;

//  MainMenu_ShowLogClick(Sender);

  MasterFontCodepage:=Main.Font.Charset;
  MasterFontName:=Main.Font.Name;
  MasterFontSize:=Main.Font.Size;

  Application.Title:=DPGEncVersion;
  Main.Caption:=Application.Title;

  ResetLanguageSet;

  OptOutputPath:=GetDesktopPath+'\';

  Option.LoadINI;
  LoadLanguage;
  Option.Init;

  Main.Width:=MainFormWidth;
  Main.Height:=MainFormHeight;

  Main.OnResize:=Main.FormResize;
  Main.FormResize(nil);

  if DirectoryExists(OptOutputPath)=False then OptOutputPath:=GetDesktopPath+'\';
  RefreshOutputPathLbl;

  QueueInit;
  Current_Init;

  QueueGrid.ColWidths[0]:=48;
  QueueGrid.ColWidths[1]:=1024;
  QueueGrid.Cells[0,0]:='';
  QueueGrid.Cells[1,0]:=lng(LI_QueueMessage);

  QueueRefreshStatus;

  for idx:=1 to ParamCount do begin
    if FileExists(ParamStr(idx))=True then begin
      QueueAdd(ParamStr(idx));
    end;
  end;

  StandbyTimer.Enabled:=True;
end;

procedure TMain.StandbyTimerTimer(Sender: TObject);
var
  fn:string;
begin
  if QueueGetQueueCount<=QueueGetQueueIdx then exit;

  Main.StandbyTimer.Enabled:=False;

  SetPrgBarPos(0,'');

  fn:=QueueGrid.Cells[1,QueueGetQueueIdx];

  if ansilowercase(ExtractFileExt(fn))='.dpg' then begin
    Current_SetError('Can not re-encode from DPG file.','');
    EncodeEndTimer.Enabled:=True;
    exit;
  end;

  SetPriorityLevel(Option.PriorityLevelLst.ItemIndex);
  SetDefaultPriorityLevel(Option.PriorityLevelLst.ItemIndex);

  Current_Init;
  Current_SetSrcFilename(fn);
  Current_SetDstFilename(OptOutputPath+ExtractFilename(ChangeFileExt(fn,'.dpg')));

  DPGEncode.RequestDeleteOverrideSource:=False;
  
  case Option.PreencLst.ItemIndex of
    0: PreencodeAutoDetectTimer.Enabled:=True;
    1: PreencodeffmpegTimer.Enabled:=True;
    2: Application.Terminate;
    else Application.Terminate;
  end;
end;

procedure TMain.PreencodeAutoDetectTimerTimer(Sender: TObject);
  function supportds:boolean;
  begin
    if DSSupport.DirectShowCheck(GetSourceFilename)=False then begin
      Result:=False;
      exit;
    end;

    Result:=True;
  end;
  function supportffmpeg:boolean;
  var
    srcfn,dstfn,appfn:string;
  begin
    Result:=True;

    srcfn:=GetSourceFilename;
    dstfn:=GetTempffmpegFilename;

    if FileExists(dstfn)=True then begin
      if DeleteFile(dstfn)=False then Result:=False;
    end;

    if Result=True then begin
      appfn:=PluginPath+'ffmpeg.exe';

      if fileexists(appfn)=False then Result:=False;
    end;

    if Result=True then begin
      CreateDOSBOX_UseCMD(StartPath,appfn,format('-t 1 '+ffmpeg_cmdlineformat,[srcfn,dstfn]));

      if GetFileSize(dstfn)=0 then Result:=False;
      DeleteFile(dstfn);
    end;
  end;
begin
  PreencodeAutoDetectTimer.Enabled:=False;

//  EncodeStartTimer.Enabled:=True; exit;

  SetPrgBarPos(0,'Detect DirectShow');
  Main.Caption:='Check stream by DirectShow.';
  if supportds=True then begin
    EncodeStartTimer.Enabled:=True;
    exit;
  end;

  SetPrgBarPos(0,'Detect ffmpeg');
  Main.Caption:='Check stream by ffmpeg.';
  if supportffmpeg=True then begin
    PreencodeffmpegTimer.Enabled:=True;
    exit;
  end;

  Current_SetError(lng(LI_GetAudioError),'');
  EncodeEndTimer.Enabled:=True;
end;

procedure TMain.PreencodeffmpegTimerTimer(Sender: TObject);
var
  appfn:string;
  srcfn,dstfn:string;
begin
  PreencodeffmpegTimer.Enabled:=False;
  if Current_GetRequestCancel=True then begin
    EncodeEndTimer.Enabled:=True;
    exit;
  end;

  SetPrgBarPos(0,'Preencode ffmpeg');

  srcfn:=GetSourceFilename;
  dstfn:=GetTempffmpegFilename;

  if FileExists(srcfn)=False then begin
    Current_SetError('file not found.','');
    EncodeEndTimer.Enabled:=True;
    exit;
  end;

  if FileExists(dstfn)=True then begin
    if DeleteFile(dstfn)=False then begin
      Current_SetError('can not delete file.',dstfn);
      EncodeEndTimer.Enabled:=True;
      exit;
    end;
  end;

  appfn:=PluginPath+'ffmpeg.exe';

  if fileexists(appfn)=False then begin
    Current_SetError('can not found plugin.',appfn);
    EncodeEndTimer.Enabled:=True;
    exit;
  end;

  Main.Caption:='Pre-encode using ffmpeg...';
  CreateDOSBOX_UseCMD(StartPath,appfn,format(ffmpeg_cmdlineformat,[srcfn,dstfn]));

  SetBaseFilename(dstfn,Current_GetDstFilename); // override
  DPGEncode.RequestDeleteOverrideSource:=True;

  if Current_GetRequestCancel=True then begin
    EncodeEndTimer.Enabled:=True;
    exit;
  end;

  if GetFileSize(dstfn)=0 then begin
    Current_SetError('format not supported by ffmpeg.','');
    EncodeEndTimer.Enabled:=True;
    exit;
  end;

  EncodeStartTimer.Enabled:=True;
end;

procedure TMain.EncodeStartTimerTimer(Sender: TObject);
var
  Aspect:double;
  SourceFPS:double;
  ExtendSetting:TExtendSetting;
  msg:string;
begin
  EncodeStartTimer.Enabled:=False;
  if Current_GetRequestCancel=True then begin
    EncodeEndTimer.Enabled:=True;
    exit;
  end;

  SetPrgBarPos(0,'Encode Start');

  if DSSupport.DirectShowCheck(GetSourceFilename)=False then begin
    Current_SetError(DSSupport.GetLastError,'');
    EncodeEndTimer.Enabled:=True;
    exit;
  end;

  SourceFPS:=DSSupport.DirectShowInfo.FramePerSec;
  if SourceFPS=0 then begin
    Current_SetError(lng(LI_DetectErrorFPS),'');
    EncodeEndTimer.Enabled:=True;
    exit;
  end;

  with ExtendSetting do begin
    srcx:=0;
    srcy:=0;
    srcw:=DSSupport.DirectShowInfo.BitmapWidth;
    srch:=DSSupport.DirectShowInfo.BitmapHeight;

    dstw:=256 and not $0f;
    if dstw<16 then dstw:=16;
    if 256<dstw then dstw:=256;

    case Option.ReencAspectLst.ItemIndex of
      0: Aspect:=DSSupport.DirectShowInfo.BitmapHeight/DSSupport.DirectShowInfo.BitmapWidth;
      1: Aspect:=3/4;
      2: Aspect:=9/16;
      3: Aspect:=1/2.35;
      else Aspect:=0;
    end;
    if (Aspect=0) or ((3/4)<Aspect) then Aspect:=3/4;

    dsth:=trunc(dstw*Aspect) and not $0f;
    if dsth<16 then dsth:=16;
    if 192<dsth then dsth:=192;

    Bright:=Option.ReencBrightnessBar.Position;

    StartTimeSec:=0;
    EndTimeSec:=DSSupport.DirectShowInfo.TotalTimeSec;
  end;

  if GetExtSetFilename<>'' then ExtendSetting_Load(GetExtSetFilename,ExtendSetting);
  
  with DPGEncode do begin
    srcx:=ExtendSetting.srcx;
    srcy:=ExtendSetting.srcy;
    srcw:=ExtendSetting.srcw;
    srch:=ExtendSetting.srch;
    dstw:=ExtendSetting.dstw;
    dsth:=ExtendSetting.dsth;

    dstw:=(dstw div 16)*16;
    if 256<dstw then dstw:=256;
    
    dsth:=(dsth div 16)*16;
    if 192<dsth then dsth:=192;
    
    Brightness:=ExtendSetting.Bright;

    FPS:=strtoint(Option.ReencFPSLst.Text);
    if Option.ReencFPSAutoChk.Checked=True then begin
      if 144<dsth then FPS:=trunc(FPS * (0.75+( (192-dsth)/48 *0.25)) );
    end;
    if FPS<1 then FPS:=1;
    
    kbps:=strtoint(Option.ReencKBPSLst.Text);

    StartTimeSec:=ExtendSetting.StartTimeSec;
    EndTimeSec:=ExtendSetting.EndTimeSec;

    TotalFrame:=trunc((EndTimeSec-StartTimeSec)*FPS);

    VerticalSwap:=Option.ReencVerticalSwapChk.Checked;
    SmoothFrameBlending:=Option.ReencSmoothFrameBlendingChk.Checked;
    if SourceFPS=FPS then SmoothFrameBlending:=False;

    SndVolume:=Option.SndVolumeBar.Position;
    Sndkbps:=Option.GetParam_SndKBPS;
    
    CmdLineFormat:=Option.GetParam_ReencCmdLine;
  end;

  with DPGINFO do begin
    PixelFormat:=DPGPixelFormat_RGB24;
    FPS:=DPGEncode.FPS;
    TotalFrame:=DPGEncode.TotalFrame;
    SndFreq:=32768;
    SndCh:=0;
  end;

  if 24<DPGEncode.FPS then begin
    msg:=format(lng(LI_AboutFrameRate),[DPGEncode.FPS]);
    if MessageDlg(msg,mtInformation,[mbYes,mbCancel],0)=mrCancel then begin
      Current_SetError(msg,'');
      EncodeEndTimer.Enabled:=True;
      exit;
    end;
  end;

  EncodeProcTimer.Enabled:=True;
end;

function TMain.StartEncode:boolean;
  procedure exdel(fn:string);
  var
    timeout:integer;
  begin
    if FileExists(fn)=False then exit;

    timeout:=10000; // max 10sec.
    while(True) do begin
      DeleteFile(fn);
      if FileExists(fn)=False then break;
      Application.ProcessMessages;
      sleep(100);
      dec(timeout,100);
      if timeout<0 then break;
    end;
  end;
begin
  Result:=False;

  encaudio_StartPath:=StartPath;
  encaudio_PluginPath:=PluginPath;
  encvideo_StartPath:=StartPath;
  encvideo_PluginPath:=PluginPath;

  Main.Caption:='Demultiplex AudioStream...';
  SetPrgBarPos(0,'Encode Audio');

  if DemuxAudio_DirectShow(GetSourceFilename,GetDPGWave1Filename,DPGEncode.SndVolume)=False then exit;
  if ConvertAudio_ssrc(GetDPGWave1Filename,GetDPGWave2Filename,True,DPGINFO.SndFreq,32000)=False then exit;
  exdel(GetDPGWave1Filename);
  if EncodeMP2_HQ32768Hz_twolame(GetDPGWave2Filename,GetDPGMP2Filename,DPGINFO.SndFreq,DPGEncode.Sndkbps)=False then exit;
  exdel(GetDPGWave2Filename);

  if GetFileSize(GetDPGMP2Filename)<1024 then begin
    Current_SetError(lng(LI_GetAudioError),'');
    exit;
  end;

  if Current_GetRequestCancel=True then exit;

  Main.Caption:='Encode VideoStream...';
  SetPrgBarPos(0,'Encode Video');

  Main.Caption:='Encode VideoStream...';
  if EncodeVideoDPG(GetSourceFilename,GetDPGMovieFilename,DPGEncode)=False then exit;

  if Current_GetRequestCancel=True then exit;

  Main.Caption:='Create GOP list...';
  if CreateGOPList(GetDPGMovieFilename,GetDPGGOPListFilename)=False then exit;

  Result:=True;
  Main.Caption:='';
end;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

// ------------------------------------------------------------
// ------------------------------------------------------------

procedure TMain.EncodeProcTimerTimer(Sender: TObject);
begin
  EncodeProcTimer.Enabled:=False;
  if Current_GetRequestCancel=True then begin
    EncodeEndTimer.Enabled:=True;
    exit;
  end;

  if StartEncode=True then begin
    if CreateDPG=True then begin
      EncodeEndTimer.Enabled:=True;
      exit;
    end;
  end;

  EncodeEndTimer.Enabled:=True;
end;

procedure TMain.EncodeEndTimerTimer(Sender: TObject);
  procedure exdel(fn:string);
  var
    timeout:integer;
  begin
    if FileExists(fn)=False then exit;

    timeout:=10000; // max 10sec.
    while(True) do begin
      DeleteFile(fn);
      if FileExists(fn)=False then break;
      Application.ProcessMessages;
      sleep(100);
      dec(timeout,100);
      if timeout<0 then break;
    end;
  end;
begin
  EncodeEndTimer.Enabled:=False;

  SetPriorityLevel(3);
  SetDefaultPriorityLevel(3);

  if GetSourceFilename<>GetDPGMovieFilename then exdel(GetDPGMovieFilename);
  exdel(GetDPGWave1Filename);
  exdel(GetDPGWave2Filename);
  exdel(GetDPGMP2Filename);
  exdel(GetDPGMovieFilename);
  exdel(GetDPGGOPListFilename);
  exdel(GetTempFilename);
  exdel(GetTempffmpegFilename);

  if DPGEncode.RequestDeleteOverrideSource=True then begin
    if Current_GetSrcFilename<>GetSourceFilename then exdel(GetSourceFilename);
  end;

  Main.Caption:=Application.Title;

  if Current_isError=True then begin
    exdel(GetDPGFilename);
    QueueSetResult('NG',Current_GetErrorMsg);
    end else begin
    if Current_GetRequestCancel=True then begin
      exdel(GetDPGFilename);
      QueueSetResult('NG',lng(LI_Canceled));
      end else begin
      QueueSetResult('OK','');
      if isQueueLast=True then begin
        if (enclog.Visible=True) or (encprv.Visible=True) then begin
          ShowMessage('Encode was completed.'+CRLF+CRLF+LoadDPGINFOString(GetDPGFilename));
        end;
      end;
    end;
  end;

  QueueNext;

  if MainMenu_AutoShutdown.Default=True then begin
    if QueueGetQueueCount<=QueueGetQueueIdx then begin
      Shutdown.ShutdownStart(QueueExistsError);
      Shutdown.ShowModal;
    end;
  end;
end;

procedure TMain.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_ESCAPE then Main.Close;
end;

procedure TMain.MainMenu_Language_TemplateClick(Sender: TObject);
var
  t:TMenuItem;
  cnt,count:integer;
begin
  t:=Main.MainMenu_Language;
  count:=t.Count;
  for cnt:=0 to count-1 do begin
    t.Items[cnt].Checked:=False;
  end;

  (Sender as TMenuItem).Checked:=True;

  Main.LoadLanguage;

  Option.SaveINI;

  if QueueGetQueueCount=0 then begin
    QueueGrid.Cells[1,0]:=lng(LI_QueueMessage);
    RefreshOutputPathLbl;
  end;
end;

procedure TMain.MainMenu_OptionClick(Sender: TObject);
begin
  if QueueNowEncoding=True then begin
    ShowMessage('エンコード中は変更できません。'+CRLF+'can not change while processing.');
    exit;
  end;

  Option.ShowModal;
end;

procedure TMain.QueueGridDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
  ps:string;
  fn,msg0,msg1:string;
  x,y,w,h:integer;
begin
  with (Sender as TStringGrid).Canvas do begin
    FillRect(Rect);

    x:=Rect.Left+2;
    y:=Rect.Top+1;
    w:=Rect.Right-2-x;
    h:=Rect.Bottom-1-y;

    case ACol of
      0: begin
        ps:=QueueGrid.Cells[0,ARow];
        if ps='OK' then begin
          Draw(x+((w-32) div 2),y+((h-16) div 2),Main.ing_okimg.Picture.Graphic);
          end else begin
          if ps='NG' then begin
            Draw(x+((w-32) div 2),y+((h-16) div 2),Main.ing_ngimg.Picture.Graphic);
            end else begin
            if ansipos(' ',ps)=0 then begin
              TextOut(x+((w-TextWidth(ps)) div 2),y+((h-(h div 2)) div 2),ps);
              end else begin
              msg0:=copy(ps,1,AnsiPos(' ',ps)-1);
              msg1:=copy(ps,AnsiPos(' ',ps)+1,length(ps));
              h:=h div 2;
//              TextOut(x+((w-TextWidth(msg0)) div 2),y+(h*0),msg0);
//              TextOut(x+((w-TextWidth(msg1)) div 2),y+(h*1),msg1);
              TextOut(x,y+(h*0),msg0);
              TextOut(x,y+(h*1),msg1);
            end;
          end;
        end;
      end;
      1: begin
        fn:=QueueGrid.Cells[1,ARow];

        if AnsiPos(CRLF,fn)=0 then begin
          msg0:=ExtractFilePath(fn);
          msg1:=ExtractFilename(fn);
          end else begin
          msg0:=copy(fn,1,AnsiPos(CRLF,fn)-1);
          msg1:=copy(fn,AnsiPos(CRLF,fn)+2,length(fn));
        end;

        h:=h div 2;
        TextOut(x+2,y+(h*0),msg0);
        TextOut(x+2,y+(h*1),msg1);
      end;
    end;

  end;
end;

procedure TMain.MainMenu_ShowLogClick(Sender: TObject);
begin
  enclog.Visible:=True;

  with enclog do begin
    encprv.Top:=Top;
    encprv.Left:=Left+Width;
    encprv.ClientWidth:=256;
    encprv.ClientHeight:=192;
  end;
  encprv.Visible:=True;
end;

procedure TMain.QueuePopup_DeleteClick(Sender: TObject);
var
  delidx:integer;
  idx:integer;
begin
  delidx:=QueueGrid.Selection.Top;

  if delidx<QueueGetQueueIdx then begin
    exit;
  end;

  if delidx=QueueGetQueueIdx then begin
    Current_RequestCancel;
    exit;
  end;

  for idx:=delidx to QueueGetQueueCount-1 do begin
    QueueGrid.Cells[0,idx]:=QueueGrid.Cells[0,idx+1];
    QueueGrid.Cells[1,idx]:=QueueGrid.Cells[1,idx+1];
  end;

  QueueDecQueueCount;
  QueueGrid.RowCount:=QueueGetQueueCount;
end;

procedure TMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if QueueNowEncoding=True then begin
    ShowMessage('エンコード中は終了できません。'+CRLF+'can not terminate while processing.');
    CanClose:=False;
  end;
end;

procedure TMain.MainMenu_AboutClick(Sender: TObject);
begin
  ShowMessage('nDs-mPeG encoder'+CRLF+DPGEncVersion);
end;

procedure TMain.FormResize(Sender: TObject);
begin
  with OutputPathChangeBtn do begin
    Left:=Main.ClientWidth-Width-8;
  end;

  with prgbar do begin
    Width:=Main.ClientWidth-Left-8;
  end;

  with QueueGrid do begin
    Width:=Main.ClientWidth-Left-8;
    Height:=Main.ClientHeight-Top-StatusBar1.Height-8;
  end;

  MainFormWidth:=Main.Width;
  MainFormHeight:=Main.Height;
end;

procedure TMain.OutputPathChangeBtnClick(Sender: TObject);
begin
  if QueueNowEncoding=True then begin
    ShowMessage('エンコード中は変更できません。'+CRLF+'can not change while processing.');
    exit;
  end;

  OutputPathDlg.InitialDir:=OptOutputPath;
  OutputPathDlg.Title:=lng(LI_OutputPathDlgTitle);
  OutputPathDlg.FileName:=lng(LI_OutputPathDlgFilename);

  if OutputPathDlg.Execute=True then begin
    OptOutputPath:=ExtractFilePath(OutputPathDlg.FileName);
    RefreshOutputPathLbl;
    Option.SaveINI;
  end;
end;

procedure TMain.QueueGridKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_ESCAPE then Main.Close;
  if Key=VK_DELETE then QueuePopup_DeleteClick(nil);
end;

procedure TMain.QueueGridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  p:TPoint;
begin
  if Button=mbRight then begin
    p.X:=x;
    p.Y:=y;
    p:=QueueGrid.ClientToScreen(p);
    QueuePopup.Popup(p.x,p.y);
  end;
end;

procedure TMain.QueueGridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  col,row:integer;
  sel:TGridRect;
  e:boolean;
begin
  if Button=mbRight then begin
    QueueGrid.MouseToCell(x,y,col,row);
    sel.Left:=0;
    sel.Right:=1;
    sel.Top:=row;
    sel.Bottom:=row;
    QueueGrid.Selection:=sel;
    QueueGrid.Refresh;

    if QueueGetQueueCount=0 then begin
      e:=False; // for tip
      end else begin
      if row<QueueGetQueueIdx then begin
        e:=False; // for Ended files.
        end else begin
        if row=QueueGetQueueIdx then begin
          // for Current file.
          if Current_GetRequestCancel=True then begin
            e:=False;
            end else begin
            e:=True;
          end;
          end else begin
          e:=True; // for Queue files.
        end;
      end;
    end;
    QueuePopup_Delete.Enabled:=e;
  end;
end;

procedure TMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Option.SaveINI;
end;

procedure TMain.MainMenu_AutoShutdownClick(Sender: TObject);
begin
  if MainMenu_AutoShutdown.Default=True then begin
    MainMenu_AutoShutdown.Default:=False;
    exit;
  end;

  MainMenu_AutoShutdown.Default:=True;
  ShowMessage(lng(LI_SetAutoShutdownMsg));
end;

procedure TMain.Button1Click(Sender: TObject);
var
  mr:TModalResult;
begin
  Network.Start(OptOutputPath);

  if Network.ShowModal=mrOk then begin
    if FileExists(Network.VideoFilename)=True then QueueAdd(Network.VideoFilename);
  end;
end;

end.


