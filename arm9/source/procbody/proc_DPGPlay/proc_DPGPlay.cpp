
#include <nds.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "_console.h"
#include "_consolewritelog.h"
#include "maindef.h"
#include "memtool.h"
#include "_const.h"
#include "../../ipc6.h"
#include "arm9tcm.h"
#include "lang.h"

#include "glib/glib.h"

#include "fat2.h"
#include "shell.h"
#include "splash.h"
#include "resume.h"
#include "procstate.h"
#include "strtool.h"
#include "unicode.h"
#include "rect.h"

#include "skin.h"
#include "component.h"
#include "sndeff.h"
#include "strpcm.h"
#include "cfont.h"

static bool ScreenRedrawFlag;

#define SeekBarHeight (48)

static u32 SeekBarRedrawCount;
static bool SeekBarExecuteRedraw;

static TRect VolumeBarRect;

static CglCanvas *pCompBG;

static CFont *pTimeFont;

static s32 BrightLevel;

static u32 PanelClosePowerOffTimeOut;

static bool ToCustomMode;

// -----------------------------

#include "plug_dpg.h"

static FAT_FILE *pDPGfh1,*pDPGfh2;

static void ProcDPG(void)
{
  if(DPG_RequestSyncStart==true){
    _consolePrint("DPG_RequestSyncStart\n");
    DPG_RequestSyncStart=false;
    
    if(ProcState.DPG.EnabledFastStart==false){
      _consolePrint("Load disk cache.\n");
      extern void DiskCache_LoadAllBuffer(void);
      DiskCache_LoadAllBuffer();
    }

    DPGAudioStream_SyncSamples=0;
    DPGAudioStream_PregapSamples=0;
    
    u32 PreDecodeFramesCount;
    
    if(ProcState.DPG.EnabledFastStart==false){
      PreDecodeFramesCount=FrameCache_GetReadFramesCount();
      if(DPG_GetCurrentFrameCount()!=0) PreDecodeFramesCount/=2;
      }else{
      PreDecodeFramesCount=4;
    }
    
    _consolePrintf("Pre-decode %dframes.\n",PreDecodeFramesCount);
    for(u32 idx=0;idx<PreDecodeFramesCount;idx++){
      _consolePrintf("%d,",DPG_GetCurrentFrameCount());
      UpdateDPG_Video();
    }
    _consolePrintf("rendered.\n");
    
    VBlank_AutoFlip_Disabled();
    
    switch(DPG_GetDPGAudioFormat()){
      case DPGAF_MP2: {
        IPC6->MP2PauseFlag=false;
        strpcmStart(false,DPG_GetSampleRate(),8,0,SPF_MP2);
      } break;
    }
    
    DPGAudioStream_PregapSamples=DPG_GetSampleRate()/60; // pre decode per frame
    
    _consolePrint("wait for flash.\n");
    while(IPC6->IR_flash==true){
      swiWaitForIRQ();
      UpdateDPG_Audio();
    }
    _consolePrint("flashed.\n");
    
    REG_IME=0;
    VBlankPassedCount=0;
    REG_IME=1;
    
    PrintFreeMem();
    
    SeekBarRedrawCount=0;
    SeekBarExecuteRedraw=true;
  }
  
  if(UpdateDPG_Video()==false){
    _consolePrintf("Set strpcmRequestStop.\n");
    strpcmRequestStop=true;
  }
}

void DPGOpen(const UnicodeChar *pFilenameUnicode)
{
  pDPGfh1=Shell_FAT_fopen_Split(RelationalFilePathUnicode,pFilenameUnicode);
  pDPGfh2=Shell_FAT_fopen_Split(RelationalFilePathUnicode,pFilenameUnicode);
  
  if((pDPGfh1==NULL)||(pDPGfh2==NULL)){
    _consolePrintf("Fatal error: can not open file.\n");
    ShowLogHalt();
  }
  
  if(StartDPG(pDPGfh1,pDPGfh2)==false){
    _consolePrintf("Fatal error: can not open DPG format.\n");
    ShowLogHalt();
  }
  
  IPC6->MP2PauseFlag=false;
  
  DPG_RequestSyncStart=true;
}

void DPGClose(void)
{
  _consolePrintf("satrpcmStop();\n");
  strpcmStop();
  _consolePrintf("FreeDPG();\n");
  FreeDPG();
  
  if(pDPGfh1!=NULL){
    FAT2_fclose(pDPGfh1); pDPGfh1=NULL;
  }
  if(pDPGfh2!=NULL){
    FAT2_fclose(pDPGfh2); pDPGfh2=NULL;
  }
  
  IPC6->MP2PauseFlag=false;
  
  _consolePrintf("DPG closed.\n");
  PrintFreeMem();
}

// -----------------------------

#include "proc_DPGPlay_BGM.h"

// -----------------------------

typedef struct {
  bool Visible;
  u32 TimeoutCount;
  CglTGF *pTGF;
} TModeLabel;

static TModeLabel ModeLabel;

static void ModeLabel_Init(void)
{
  TModeLabel *pml=&ModeLabel;
  
  pml->Visible=false;
  pml->TimeoutCount=0;
  pml->pTGF=NULL;
}

static void ModeLabel_Draw(CglCanvas *pCanvas)
{
  TModeLabel *pml=&ModeLabel;
  
  if(pml->Visible==false) return;
  
  u32 x=12,y=8;
  
  if(pml->pTGF!=NULL) pml->pTGF->BitBlt(pCanvas,x,y);
}

static void ModeLabel_Start(void)
{
  TProcState_DPG *pdpg=&ProcState.DPG;
  
  EMoviePlayerSkinAlpha EMPSA;

  switch(pdpg->PlayMode){
    case EDPM_Repeat: EMPSA=EMPSA_modelbl_repeat; break;
    case EDPM_AllRep: EMPSA=EMPSA_modelbl_allrep; break;
    case EDPM_Random: EMPSA=EMPSA_modelbl_random; break;
    default: return; break;
  }
  
  TModeLabel *pml=&ModeLabel;
  pml->Visible=true;
  pml->TimeoutCount=60*2;
  pml->pTGF=MoviePlayerAlpha_GetSkin(EMPSA);
}

static void ModeLabel_Vsync(u32 VsyncCount)
{
  TModeLabel *pml=&ModeLabel;
  
  if(pml->TimeoutCount==0) return;
  
  if(pml->TimeoutCount<VsyncCount){
    pml->TimeoutCount=0;
    }else{
    pml->TimeoutCount-=VsyncCount;
  }
  
  if(pml->TimeoutCount==0){
    ModeLabel_Init();
    ScreenRedrawFlag=true;
  }
}

// -----------------------------

static void Exec_Next(bool RelationalPlayMode)
{
  if(RelationalPlayMode==false){
    BGM_Next();
    return;
  }
  
  switch(ProcState.DPG.PlayMode){
    case EDPM_Repeat: {
      BGM_Repeat();
    } break;
    case EDPM_AllRep: {
      BGM_Next();
    } break;
    case EDPM_Random: {
      BGM_NextRandom();
    } break;
  }
}

// -----------------------------

enum ECompLabels {ECLSCount};
#define CompLabelsCount (ECLSCount)
static TComponentLabel CompLabels[CompLabelsCount];

enum ECompChecks {ECCSCount};
#define CompChecksCount (ECCSCount)
static TComponentCheck CompChecks[CompChecksCount];

enum ECompButtons {ECBS_ModeBtn,ECBS_PlayBtn,ECBS_StopBtn,ECBS_PrevBtn,ECBS_NextBtn,ECBSCount};
#define CompButtonsCount (ECBSCount)
static TComponentButton CompButtons[CompButtonsCount];

static void Setting_Redraw(void)
{
  pCompBG->SetCglFont(pCglFontDefault);
  
  {
    CglCanvas *pcan=pCompBG;
    
    CglB15 *pbg=MoviePlayer_GetSkin(EMPS_bg);
    pbg->pCanvas->BitBltFullBeta(pcan);
    
    char idxstr[16];
    snprintf(idxstr,32,"%d / %d",1+BGMListIndex,BGMListCount);
    u32 idxstrw=pcan->GetTextWidthA(idxstr)+8;
    
    u32 x=16+2,y=ScreenHeight-SeekBarHeight-28+2;
    
    x++; y++;
    pcan->SetFontTextColor(ColorTable.Video.FilenameShadow);
    pcan->TextOutA(x,y+(12*0),idxstr);
    pcan->TextOutW(x+idxstrw,y+(12*0),RelationalFilePathUnicode);
    pcan->TextOutW(x,y+(12*1),BGM_GetCurrentFilename());
    x--; y--;
    pcan->SetFontTextColor(ColorTable.Video.FilenameText);
    pcan->TextOutA(x,y+(12*0),idxstr);
    pcan->TextOutW(x+idxstrw,y+(12*0),RelationalFilePathUnicode);
    pcan->TextOutW(x,y+(12*1),BGM_GetCurrentFilename());
    
    CglTGF *ptgf=MoviePlayerAlpha_GetSkin(EMPSA_backlight);
    ptgf->BitBlt(pcan,ScreenWidth-ptgf->GetWidth(),ScreenHeight-SeekBarHeight-ptgf->GetHeight());
  }
  
  {
    CglCanvas *pcan=pCompBG;
    
    CglTGF *pon=MoviePlayerAlpha_GetSkin(EMPSA_volbar_on);
    CglTGF *poff=MoviePlayerAlpha_GetSkin(EMPSA_volbar_off);
    
    TRect r=VolumeBarRect;
    r.y-=SeekBarHeight;
    s32 vol=strpcmGetVolume64();
    
    s32 limy=(vol*r.h)/strpcmVolumeMax;
    limy=r.h-limy;
    if(limy<0) limy=0;
    if(r.h<limy) limy=r.h;
    
    poff->BitBltLimitY(pcan,r.x,r.y,limy,0);
    pon->BitBltLimitY(pcan,r.x,r.y+limy,r.h-limy,limy);
    
    u32 x=r.x+r.w+8;
    u32 y=r.y+r.h-24;
    
    u16 col1=ColorTable.Video.VolumeShadow;
    u16 col2=ColorTable.Video.VolumeText;
    
    char str[16];
    if(vol==0){
      snprintf(str,16,Lang_GetUTF8("DV_VolumeMute"));
      }else{
      if(vol<strpcmVolumeMax){
        snprintf(str,16,Lang_GetUTF8("DV_VolumeValue"),vol*100/64);
        }else{
        col1=ColorTable.Video.VolumeMaxShadow;
        col2=ColorTable.Video.VolumeMaxText;
        snprintf(str,16,Lang_GetUTF8("DV_VolumeMax"));
      }
    }
    
    const char *plblmsg=Lang_GetUTF8("DV_VolumeLabel");
    u32 w0=pcan->GetTextWidthA(plblmsg);
    u32 w1=pcan->GetTextWidthA(str);
    
    x++; y++;
    pcan->SetFontTextColor(col1);
    pcan->TextOutA(x-w0,y+(12*0),plblmsg);
    pcan->TextOutA(x-w1,y+(12*1),str);
    x--; y--;
    pcan->SetFontTextColor(col2);
    pcan->TextOutA(x-w0,y+(12*0),plblmsg);
    pcan->TextOutA(x-w1,y+(12*1),str);
  }
  
  {
    EMoviePlayerSkinAlpha Icon;
    
    TProcState_DPG *pdpg=&ProcState.DPG;
    
    switch(pdpg->PlayMode){
      case EDPM_Repeat: Icon=EMPSA_mode_repeat; break;
      case EDPM_AllRep: Icon=EMPSA_mode_allrep; break;
      case EDPM_Random: Icon=EMPSA_mode_random; break;
      default: {
        _consolePrintf("Unknown DPG play mode. (%d)\n",pdpg->PlayMode);
        ShowLogHalt();
      } break;
    }
      
    CompButtons[ECBS_ModeBtn].pIcon=MoviePlayerAlpha_GetSkin(Icon);
  }
  
  {
    TComponentButton *pcb=&CompButtons[ECBS_PlayBtn];
    if(IPC6->MP2PauseFlag==false){
      pcb->pIcon=MoviePlayerAlpha_GetSkin(EMPSA_pause);
      }else{
      pcb->pIcon=MoviePlayerAlpha_GetSkin(EMPSA_play);
    }
  }
  
  for(u32 idx=0;idx<CompLabelsCount;idx++){
    ComponentLabel_Draw(&CompLabels[idx]);
  }
  for(u32 idx=0;idx<CompChecksCount;idx++){
    ComponentCheck_Draw(&CompChecks[idx]);
  }
  for(u32 idx=0;idx<CompButtonsCount;idx++){
    ComponentButton_Draw(&CompButtons[idx]);
  }
  
  ModeLabel_Draw(pCompBG);
  
  pCompBG->BitBlt(pScreenSub->pCanvas,0,SeekBarHeight,pCompBG->GetWidth(),pCompBG->GetHeight(),0,0,false);
}

static void CB_ModeBtn_Click(void *pComponentButton)
{
  TProcState_DPG *pdpg=&ProcState.DPG;
  
  switch(pdpg->PlayMode){
    case EDPM_Repeat: pdpg->PlayMode=EDPM_AllRep; break;
    case EDPM_AllRep: pdpg->PlayMode=EDPM_Random; break;
    case EDPM_Random: pdpg->PlayMode=EDPM_Repeat; break;
    default: pdpg->PlayMode=EDPM_Repeat; break;
  }
  
  ProcState_RequestSave=true;
  
  ModeLabel_Start();
}

static void CB_PlayBtn_Click(void *pComponentButton)
{
  if(IPC6->MP2PauseFlag==true){
    IPC6->MP2PauseFlag=false;
    }else{ cwl();
    IPC6->MP2PauseFlag=true;
    Sound_Start(WAVFN_Click);
  }
}

static void CB_PrevBtn_Click(void *pComponentButton)
{
  Sound_Start(WAVFN_Click);
  BGM_Prev();
}

static void CB_NextBtn_Click(void *pComponentButton)
{
  Sound_Start(WAVFN_Click);
  Exec_Next(false);
}

static void CB_StopBtn_Click(void *pComponentButton)
{
  Sound_Start(WAVFN_Click);
  SetNextProc(ENP_FileList,EPFE_CrossFade);
}

static void CompsInit(void)
{
  CglCanvas *pcan=pCompBG;
  
  for(u32 idx=0;idx<CompLabelsCount;idx++){
    ComponentLabel_Init(&CompLabels[idx],pcan);
  }
  for(u32 idx=0;idx<CompChecksCount;idx++){
    ComponentCheck_Init(&CompChecks[idx],pcan);
  }
  for(u32 idx=0;idx<CompButtonsCount;idx++){
    ComponentButton_Init(&CompButtons[idx],pcan);
  }
  
  s32 x,y,w,h;
  
  x=8;
  y=0;
  w=64;
  h=64;
  
  {
    TComponentButton *pcb=&CompButtons[ECBS_PrevBtn];
    pcb->CallBack_Click=CB_PrevBtn_Click;
    pcb->pIcon=MoviePlayerAlpha_GetSkin(EMPSA_prev);
    pcb->pMsgUTF8="";
    pcb->Rect=CreateRect(x,y,w,h);
    pcb->DrawFrame=false;
  }
  x+=w;
  
  {
    TComponentButton *pcb=&CompButtons[ECBS_PlayBtn];
    pcb->CallBack_Click=CB_PlayBtn_Click;
    pcb->pIcon=MoviePlayerAlpha_GetSkin(EMPSA_play);
    pcb->pMsgUTF8="";
    pcb->Rect=CreateRect(x,y,w,h);
    pcb->DrawFrame=false;
  }
  x+=w;
  
  {
    TComponentButton *pcb=&CompButtons[ECBS_NextBtn];
    pcb->CallBack_Click=CB_NextBtn_Click;
    pcb->pIcon=MoviePlayerAlpha_GetSkin(EMPSA_next);
    pcb->pMsgUTF8="";
    pcb->Rect=CreateRect(x,y,w,h);
    pcb->DrawFrame=false;
  }
  x+=w;
  
  y+=h;
  
  w=48;
  h=48;
  
  x=8+64+((64-48)/2)-w;
  
  {
    TComponentButton *pcb=&CompButtons[ECBS_ModeBtn];
    pcb->CallBack_Click=CB_ModeBtn_Click;
    pcb->pIcon=MoviePlayerAlpha_GetSkin(EMPSA_mode_allrep);
    pcb->pMsgUTF8="";
    pcb->Rect=CreateRect(x,y,w,h);
    pcb->DrawFrame=false;
  }
  x+=w;
  
  {
    TComponentButton *pcb=&CompButtons[ECBS_StopBtn];
    pcb->CallBack_Click=CB_StopBtn_Click;
    pcb->pIcon=MoviePlayerAlpha_GetSkin(EMPSA_stop);
    pcb->pMsgUTF8="";
    pcb->Rect=CreateRect(x,y,w,h);
    pcb->DrawFrame=false;
  }
  x+=w;
  
}

// -----------------------------

// -----------------------------

static bool seeking;

static bool KeySeeking;

static void CB_KeyPress(u32 VsyncCount,u32 Keys)
{
  if((Keys&(KEY_L|KEY_R))!=0){
    if((Keys&KEY_LEFT)!=0){
      CB_PrevBtn_Click(NULL);
      ScreenRedrawFlag=true;
    }
    if((Keys&KEY_RIGHT)!=0){
      Sound_Start(WAVFN_Click);
      Exec_Next(false);
      ScreenRedrawFlag=true;
    }
    if((Keys&KEY_UP)!=0){
      CB_PlayBtn_Click(NULL);
      ScreenRedrawFlag=true;
    }
    if((Keys&KEY_DOWN)!=0){
      CB_ModeBtn_Click(NULL);
      ScreenRedrawFlag=true;
    }
    if((Keys&(KEY_X|KEY_Y))!=0){
      if((Keys&KEY_X)!=0) ChangeNextBacklightLevel();
      if((Keys&KEY_Y)!=0) ChangePrevBacklightLevel();
    }
    return;
  }
  
  if((Keys&(KEY_START|KEY_SELECT))!=0){
    Sound_Start(WAVFN_Click);
    ToCustomMode=true;
    SetNextProc(ENP_DPGCustom,EPFE_None);
  }
  
  if((Keys&KEY_B)!=0){
    Sound_Start(WAVFN_Click);
    SetNextProc(ENP_FileList,EPFE_CrossFade);
  }
  
  if((Keys&KEY_A)!=0){
    CB_PlayBtn_Click(NULL);
    ScreenRedrawFlag=true;
  }
  
  if((Keys&(KEY_Y|KEY_X))!=0){
    s32 Volume=ProcState.System.Volume64;
          
    if(Keys==(KEY_Y|KEY_X)){
      Volume=64;
      }else{
      if((Keys&KEY_Y)!=0) Volume-=2;
      if((Keys&KEY_X)!=0) Volume+=2;
      
      if(Volume<0) Volume=0;
      if(Volume>strpcmVolumeMax) Volume=strpcmVolumeMax;
    }
    
    Volume&=~1;
    strpcmSetVolume64(Volume);
    ProcState.System.Volume64=Volume;
    ProcState_RequestSave=true;
    
    // if(Volume==64) Sound_Start(WAVFN_Click);
    
    ScreenRedrawFlag=true;
  }
  
  if((Keys&(KEY_UP|KEY_DOWN|KEY_LEFT|KEY_RIGHT))!=0){
    s32 v=0;
    
    if((Keys&KEY_UP)!=0) v=-30;
    if((Keys&KEY_DOWN)!=0) v=+30;
    if((Keys&KEY_LEFT)!=0) v=-5;
    if((Keys&KEY_RIGHT)!=0) v=+5;
    
    s32 curf,tagf;
    curf=(s32)DPG_GetCurrentFrameCount();
    tagf=curf+((s32)DPG_GetFPS()*v/0x100);
    if(tagf<0) tagf=0;
    
    _consolePrintf("Move frame %d to %d.\n",curf,tagf);
    
    if((s32)DPG_GetTotalFrameCount()<=tagf){ cwl();
      strpcmRequestStop=true;
      }else{ cwl();
      if(KeySeeking==false){
        KeySeeking=true;
        strpcmStop();
        seeking=true;
      }
      DPG_SetFrameCount((u32)tagf);
      swiWaitForVBlank();
      DPG_SliceOneFrame(pScreenMain->pBackCanvas->GetVRAMBuf(),pScreenMain->pViewCanvas->GetVRAMBuf());
      VBlank_AutoFlip_Enabled();
      SeekBarRedrawCount=0;
      SeekBarExecuteRedraw=true;
    }
    
    ScreenRedrawFlag=true;
  }

}

static void CB_KeySameLRDown(void)
{
  CB_PlayBtn_Click(NULL);
  ScreenRedrawFlag=true;
}

enum EMouseMode {EMM_Idle,EMM_Comp,EMM_Seek,EMM_Volume,EMM_Title};
static EMouseMode MouseMode;
static TComponentButton *pPressingButton;
static bool DrawInfoFlag;
static u32 DrawInfoDiffCount;

static void CB_Mouse_ins_SetVolume(s32 x,s32 y)
{
  s32 Volume=(strpcmVolumeMax*(y-VolumeBarRect.y))/VolumeBarRect.h;
  Volume=strpcmVolumeMax-Volume;
    
  if(Volume<0) Volume=0;
  if(strpcmVolumeMax<Volume) Volume=strpcmVolumeMax;
  
  if(((64-4)<Volume)&&(Volume<=(64+4))) Volume=64;
      
  strpcmSetVolume64(Volume);
  ProcState.System.Volume64=Volume;
  ProcState_RequestSave=true;
  
  ScreenRedrawFlag=true;
}

static s32 CB_Mouse_ins_SeekBarGetFrameNum(s32 x)
{
  s32 curf=(s32)DPG_GetCurrentFrameCount();
  s32 ttlf=(s32)DPG_GetTotalFrameCount();
  s32 tagf=(ttlf*x)/ScreenWidth;
  
  _consolePrintf("Move frame %d to %d.\n",curf,tagf);
  
  if(tagf<0) tagf=0;
  if(ttlf<=tagf) tagf=-1;
  
  return(tagf);
}

static void CB_MouseDown(s32 x,s32 y)
{
  MouseMode=EMM_Idle;
  
  if(ProcState.DPG.BacklightFlag==false){
    ProcState.DPG.BacklightFlag=true;
    ProcState_RequestSave=true;
    ScreenRedrawFlag=true;
    SeekBarRedrawCount=0;
    SeekBarExecuteRedraw=true;
    return;
  }
  
  if(y<SeekBarHeight){
    s32 tagf=CB_Mouse_ins_SeekBarGetFrameNum(x);
    if(tagf==-1){
      strpcmRequestStop=true;
      MouseMode=EMM_Idle;
      }else{ cwl();
      strpcmStop();
      seeking=true;
      DPG_SetFrameCount((u32)tagf);
      swiWaitForVBlank();
      DPG_SliceOneFrame(pScreenMain->pBackCanvas->GetVRAMBuf(),pScreenMain->pViewCanvas->GetVRAMBuf());
      VBlank_AutoFlip_Enabled();
    }
    SeekBarRedrawCount=0;
    SeekBarExecuteRedraw=true;
    
    MouseMode=EMM_Seek;
    return;
  }
  
  if(isInsideRect(VolumeBarRect,x,y)==true){
    CB_Mouse_ins_SetVolume(x,y);
    MouseMode=EMM_Volume;
    return;
  }
  
  if((ScreenHeight-28)<=y){
    if(x<24){
      pCompBG->SetCglFont(pCglFontDefault);
      pCompBG->SetFontTextColor(ColorTable.Video.InfoText);
      pCompBG->FillFull(ColorTable.Video.InfoBG);
      DPG_DrawInfoDiff(pCompBG);
      DPG_DrawInfo(pCompBG);
      pCompBG->BitBlt(pScreenSub->pCanvas,0,SeekBarHeight,pCompBG->GetWidth(),pCompBG->GetHeight(),0,0,false);
      DrawInfoFlag=true;
      DrawInfoDiffCount=1;
      }else{
      CglTGF *ptgf=MoviePlayerAlpha_GetSkin(EMPSA_backlight);
      u32 tgfwidth=ptgf->GetWidth();
      if(x<(ScreenWidth-tgfwidth)){
        ProcState.DPG.BacklightFlag=false;
        ProcState_RequestSave=true;
        }else{
        ChangeNextBacklightLevel();
      }
    }
    MouseMode=EMM_Title;
    return;
  }
  
  y-=SeekBarHeight;
  
  for(u32 idx=0;idx<CompButtonsCount;idx++){
    TComponentButton *pcb=&CompButtons[idx];
    if(ComponentButton_GetIndexFromPos(pcb,x,y)!=-1){
      pPressingButton=pcb;
      pcb->Pressing=true;
      ComponentButton_Draw(pcb);
      Setting_Redraw();
      MouseMode=EMM_Comp;
      return;
    }
  }
  
}

static void CB_MouseMove(s32 x,s32 y)
{
  if(MouseMode==EMM_Comp) y-=SeekBarHeight;
  
  switch(MouseMode){
    case EMM_Idle: {
    } break;
    case EMM_Comp: {
    } break;
    case EMM_Seek: {
      s32 tagf=CB_Mouse_ins_SeekBarGetFrameNum(x);
      if(tagf==-1){
        strpcmRequestStop=true;
        VBlank_AutoFlip_Disabled();
        seeking=false;
        MouseMode=EMM_Idle;
        }else{ cwl();
        DPG_SetFrameCount((u32)tagf);
        swiWaitForVBlank();
        DPG_SliceOneFrame(pScreenMain->pBackCanvas->GetVRAMBuf(),pScreenMain->pViewCanvas->GetVRAMBuf());
      }
      SeekBarRedrawCount=0;
      SeekBarExecuteRedraw=true;
    } break;
    case EMM_Volume: {
      CB_Mouse_ins_SetVolume(x,y);
    } break;
    case EMM_Title: {
    } break;
  }
}

static void CB_MouseUp(s32 x,s32 y)
{
  if(MouseMode!=EMM_Seek) y-=SeekBarHeight;
  
  EMouseMode _MouseMode=MouseMode;
  MouseMode=EMM_Idle;
  
  switch(_MouseMode){
    case EMM_Idle: {
    } break;
    case EMM_Comp: {
      if(pPressingButton!=NULL){
        pPressingButton->Pressing=false;
        ComponentButton_Draw(pPressingButton);
        
        for(u32 idx=0;idx<CompButtonsCount;idx++){
          TComponentButton *pcb=&CompButtons[idx];
          if(pcb==pPressingButton){
            if(ComponentButton_GetIndexFromPos(pcb,x,y)!=-1){
              ComponentButton_MouseUp(&CompButtons[idx],x,y);
            }
          }
        }
        
        pPressingButton=NULL;
      }
      ScreenRedrawFlag=true;
      return;
    } break;
    case EMM_Seek: {
      Resume_SetPos(DPG_GetCurrentFrameCount());
      Resume_Save();
      
      DPG_RequestSyncStart=true;
      SeekBarRedrawCount=0;
      SeekBarExecuteRedraw=true;
      
      IPC6->MP2PauseFlag=false;
      ScreenRedrawFlag=true;
      
      seeking=false;
    } break;
    case EMM_Volume: {
    } break;
    case EMM_Title: {
      DrawInfoFlag=false;
      ScreenRedrawFlag=true;
    } break;
  }
}

static void CB_PanelClose(void)
{
  Resume_SetPos(DPG_GetCurrentFrameCount());
  Resume_Save();
  
  if(ProcState.DPG.PauseWhenPanelClosed==true){
    IPC6->MP2PauseFlag=true;
    PanelClosePowerOffTimeOut=10*60*60;
  }
  
  return;
  
  DPGClose();
  
  IPC6->LCDPowerControl=LCDPC_SOFT_POWEROFF;
  while(1);
}

static void CB_PanelOpen(void)
{
  IPC6->MP2PauseFlag=false;
  PanelClosePowerOffTimeOut=0;
}

// -----------------------------

static bool Process_SeekNext,Process_SeekPrev;
static u32 Process_WaitCount;

static void CB_Start(void)
{
  REG_POWERCNT = (REG_POWERCNT & ~POWER_SWAP_LCDS) | POWER_SWAP_LCDS;
  pScreenMain->SetMode(ESMM_ForARM7);
  
  pScreenSub->pCanvas->FillFull(ColorTable.Video.InitBG);
//  pScreenMainOverlay->pCanvas->FillFull(0);
  pScreenMain->pBackCanvas->FillFull(ColorTable.Video.InitBG);
  pScreenMain->pViewCanvas->FillFull(ColorTable.Video.InitBG);

  ToCustomMode=false;
  
  MouseMode=EMM_Idle;
  pPressingButton=NULL;
  seeking=false;
  DrawInfoFlag=false;
  DrawInfoDiffCount=0;
  
  KeySeeking=false;
  
  Process_SeekNext=false;
  Process_SeekPrev=false;
  Process_WaitCount=0;

  PanelClosePowerOffTimeOut=0;
  
  ModeLabel_Init();
  
  if(ProcState.DPG.BacklightFlag==false){
    IPC6->LCDPowerControl=LCDPC_ON_TOP;
    BrightLevel=0*0x100;
    }else{
    IPC6->LCDPowerControl=LCDPC_ON_BOTH;
    BrightLevel=16*0x100;
  }
  pScreenSub->SetBlackOutLevel16(16-(BrightLevel/0x100));

  {
    CglTGF *ptgf=MoviePlayerAlpha_GetSkin(EMPSA_volbar_on);
    VolumeBarRect.w=ptgf->GetWidth();
    VolumeBarRect.h=ptgf->GetHeight();
    VolumeBarRect.x=ScreenWidth-16-VolumeBarRect.w;
    VolumeBarRect.y=ScreenHeight-40-VolumeBarRect.h;
  }

  pCompBG=new CglCanvas(NULL,ScreenWidth,ScreenHeight-SeekBarHeight,pf15bit);
  if(pCompBG==NULL){
    _consolePrintf("Fatal error: pCompBG memory overflow.\n");
    ShowLogHalt();
  }
  
  pTimeFont=new CFont(EBM_TGF,NULL,MoviePlayerAlpha_GetSkin(EMPSA_digits));
  
  CompsInit();
  
  ScreenRedrawFlag=true;
  
  pDPGfh1=NULL;
  pDPGfh2=NULL;
  
  SeekBarRedrawCount=0;
  SeekBarExecuteRedraw=true;
  
  if((Unicode_isEmpty(RelationalFilePathUnicode)==false)&&(Unicode_isEmpty(RelationalFileNameUnicode)==false)){
    BGM_Start(RelationalFilePathUnicode,RelationalFileNameUnicode);
    if(RelationalFilePos!=DPG_GetCurrentFrameCount()) DPG_SetFrameCount(RelationalFilePos);
    Resume_SetPos(RelationalFilePos);
    Resume_Save();
  }
  
}

static void RedrawSeekBar(void)
{
  u32 curframe=DPG_GetCurrentFrameCount();
  u32 ttlframe=DPG_GetTotalFrameCount();
  u32 fps=DPG_GetFPS();
  
  CglCanvas *pcan=pScreenSub->pCanvas;
    
  CglB15 *pbgon=MoviePlayer_GetSkin(EMPS_seekbar_on);
  CglB15 *pbgoff=MoviePlayer_GetSkin(EMPS_seekbar_off);
    
  s32 parx=(curframe*ScreenWidth)/ttlframe;
  if(parx<0) parx=0;
  if(ScreenWidth<parx) parx=ScreenWidth;
    
  pbgon->pCanvas->BitBlt(pcan,0,0,parx,SeekBarHeight,0,0,false);
  pbgoff->pCanvas->BitBlt(pcan,parx,0,ScreenWidth-parx,SeekBarHeight,parx,0,false);
  
  {
    CglTGF *ptgf=MoviePlayerAlpha_GetSkin(EMPSA_seekbargrip);
    s32 w=ptgf->GetWidth();
    parx-=w/2;
    if(parx<0) parx=0;
    if((ScreenWidth-w)<parx) parx=ScreenWidth-w;
    DrawSkinAlpha(ptgf,pcan,parx,0);
  }
  
  u32 cursec=curframe*0x100/fps;
  u32 ttlsec=ttlframe*0x100/fps;
  
  char str[32];
  snprintf(str,256,"%02d:%02d:%02d / %02d:%02d:%02d",cursec/(60*60),(cursec/60)%60,cursec%60,ttlsec/(60*60),(ttlsec/60)%60,ttlsec%60);

  u32 x=128,y=28;
  y+=(SeekBarHeight-y)/2;
  pTimeFont->DrawText(pcan,x,y,str);
}

static void RedrawFrameCacheBar(void)
{
  CglCanvas *pcan=pScreenSub->pCanvas;
  
  u32 max=FrameCache_GetReadFramesCount();
  u32 last=FrameCache_GetReadFrameLastCount();
  if(max<=last) last=max-1;
  
  u32 con=ColorTable.Video.FrameCacheOn;
  u32 coff=ColorTable.Video.FrameCacheOff;
  
  if(last<=4){
    con=ColorTable.Video.FrameCacheWarn;
  }
  
  if((max-4)<last) return;
  
  max*=2;
  last*=2;
  
  for(u32 idx=0;idx<max;idx++){
    u16 c=con;
    if(last<idx) c=coff;
    pcan->SetPixel(4+idx,34+0,c);
    pcan->SetPixel(4+idx,34+1,c);
  }
}

static void CB_VsyncUpdate(u32 VsyncCount)
{
  UpdateDPG_Audio();
  
  if(PanelClosePowerOffTimeOut!=0){
    if(PanelClosePowerOffTimeOut<VsyncCount){
      PanelClosePowerOffTimeOut=0;
      }else{
      PanelClosePowerOffTimeOut-=VsyncCount;
    }
    if(PanelClosePowerOffTimeOut==0){
      _consolePrintf("Panel closed timeout. Auto power off.\n");
      Sound_Start(WAVFN_PowerOff);
      u32 vsync=Sound_GetCurrentPlayTimePerVsync();
      _consolePrintf("Wait for terminate. (%d)\n",vsync);
      for(u32 w=0;w<vsync;w++){
        swiWaitForVBlank();
      }
      IPC6->LCDPowerControl=LCDPC_SOFT_POWEROFF;
      while(1);
    }
  }
  
  if(BGMResumeSaveTimeVSync!=0){
    BGMResumeSaveTimeVSync+=VsyncCount;
    if((60*5)<BGMResumeSaveTimeVSync){
      BGMResumeSaveTimeVSync=1;
      u32 pos=DPG_GetCurrentFrameCount();
      if(pos!=Resume_GetPos()){
        Resume_SetPos(pos);
        Resume_Save();
      }
/*
      PrintFreeMem();
      ProcState_RequestSave=true;
*/
      ProcState_Save();
    }
  }
  
  if(KeySeeking==true){
    u32 KEYS_CUR=(~REG_KEYINPUT)&0x3ff;
    if(KEYS_CUR==0){
      KeySeeking=false;
      Resume_SetPos(DPG_GetCurrentFrameCount());
      Resume_Save();
      
      DPG_RequestSyncStart=true;
      SeekBarRedrawCount=0;
      SeekBarExecuteRedraw=true;
      
      IPC6->MP2PauseFlag=false;
      ScreenRedrawFlag=true;
      
      seeking=false;
    }
  }
  
  if((Process_SeekNext==true)||(Process_SeekPrev==true)){
    if(Process_WaitCount!=0){
      Process_WaitCount=10;
      }else{
      s32 v=0;
      
      if(Process_SeekPrev==true) v=-5;
      if(Process_SeekNext==true) v=+5;
      
      s32 curf,tagf;
      curf=(s32)DPG_GetCurrentFrameCount();
      tagf=curf+((s32)DPG_GetFPS()*v/0x100);
      if(tagf<0) tagf=0;
      
      _consolePrintf("Move frame %d to %d.\n",curf,tagf);
      
      if((s32)DPG_GetTotalFrameCount()<=tagf){ cwl();
        strpcmRequestStop=true;
        }else{
        DPG_SetFrameCount((u32)tagf);
        swiWaitForVBlank();
        DPG_SliceOneFrame(pScreenMain->pBackCanvas->GetVRAMBuf(),pScreenMain->pViewCanvas->GetVRAMBuf());
        VBlank_AutoFlip_Enabled();
        SeekBarRedrawCount=0;
        SeekBarExecuteRedraw=true;
      }
    }
  }
  
  ModeLabel_Vsync(VsyncCount);
  
  if(ProcState.DPG.BacklightFlag==true){
    { // Enable VRAM buffer write cache
      u32 r0=7;
      __asm {
        mcr p15, 0, r0, c3, c0, 0
      }
    }
  
    if(SeekBarRedrawCount!=0){
      if(SeekBarRedrawCount<=VsyncCount){
        SeekBarRedrawCount=0;
        }else{
        SeekBarRedrawCount-=VsyncCount;
      }
    }
  
    if(SeekBarRedrawCount==0){
      if(SeekBarExecuteRedraw==true){
        SeekBarExecuteRedraw=false;
        SeekBarRedrawCount=60;
        RedrawSeekBar();
        }else{
        if(VsyncCount<=3){
          SeekBarRedrawCount=60;
          RedrawSeekBar();
        }
      }
    }
    RedrawFrameCacheBar();
    
    if(ScreenRedrawFlag==true){
      ScreenRedrawFlag=false;
      Setting_Redraw();
    }
    
    if(DrawInfoFlag==true){
      if(DrawInfoDiffCount!=0){
        if(DrawInfoDiffCount<VsyncCount){
          DrawInfoDiffCount=0;
          }else{
          DrawInfoDiffCount-=VsyncCount;
        }
        if(DrawInfoDiffCount==0){
          DrawInfoDiffCount=5;
          pCompBG->SetCglFont(pCglFontDefault);
          pCompBG->SetFontTextColor(ColorTable.Video.InfoText);
          pCompBG->SetColor(ColorTable.Video.InfoBG);
          u32 height=DPG_DrawInfoDiffHeight();
          pCompBG->FillFast(0,0,ScreenWidth,height);
          DPG_DrawInfoDiff(pCompBG);
          pCompBG->BitBlt(pScreenSub->pCanvas,0,SeekBarHeight,pCompBG->GetWidth(),height,0,0,false);
        }
      }
    }
    
    { // Disable VRAM buffer write cache
      u32 r0=6;
      __asm {
        mcr p15, 0, r0, c3, c0, 0
      }
    }
  }
      
  if(seeking==false){
    while(VBlankPassedFlag==false){
      ProcDPG();
    }
    
    if(strpcmRequestStop==true){
      _consolePrint("Wait for terminate.\n");
      while(FrameCache_isReadEmpty()==false){
        UpdateDPG_Audio();
      }
      
      Exec_Next(true);
      ScreenRedrawFlag=true;
      
      ProcDPG();
    }
  }
  
  if(ProcState.DPG.BacklightFlag==false){
    if(BrightLevel!=(0*0x100)){
      BrightLevel-=VsyncCount*64;
      pScreenSub->SetBlackOutLevel16(16-(BrightLevel/0x100));
      if(BrightLevel<=(0*0x100)){
        BrightLevel=0*0x100;
        pScreenSub->SetBlackOutLevel16(16-(BrightLevel/0x100));
        IPC6->LCDPowerControl=LCDPC_ON_TOP;
      }
    }
    }else{
    if(BrightLevel!=(16*0x100)){
      IPC6->LCDPowerControl=LCDPC_ON_BOTH;
      BrightLevel+=VsyncCount*128;
      pScreenSub->SetBlackOutLevel16(16-(BrightLevel/0x100));
      if((16*0x100)<=BrightLevel){
        BrightLevel=16*0x100;
        pScreenSub->SetBlackOutLevel16(16-(BrightLevel/0x100));
      }
    }
  }
}

static void CB_End(void)
{
  TCallBack *pCallBack=CallBack_GetPointer();
  pCallBack->VBlankHandler=NULL;
  
  VBlank_AutoFlip_Disabled();
  
  if(ToCustomMode==false){
    RelationalFile_Clear();
    }else{
    Unicode_Copy(RelationalFileNameUnicode,BGM_GetCurrentFilename());
    RelationalFilePos=DPG_GetCurrentFrameCount();
  }
  
  Resume_Clear();
  
  Unicode_Copy(ProcState.FileList.SelectFilenameUnicode,BGM_GetCurrentFilename());
  ProcState_RequestSave=true;
  ProcState_Save();
  
  BGM_Free();
  
  if(pCompBG!=NULL){
    delete pCompBG; pCompBG=NULL;
  }
  
  if(pTimeFont!=NULL){
    delete pTimeFont; pTimeFont=NULL;
  }
  
  CglCanvas *ptmpcan=new CglCanvas(NULL,ScreenWidth,ScreenHeight,pf15bit);
  if(ProcState.DPG.BacklightFlag==false){
    pScreenSub->pCanvas->FillFull(ColorTable.Video.InitBG);
  }
  pScreenSub->pCanvas->BitBltFullBeta(ptmpcan);
  
  swiWaitForVBlank();
  
  REG_POWERCNT = (REG_POWERCNT & ~POWER_SWAP_LCDS); // | POWER_SWAP_LCDS;
  pScreenMain->SetMode(ESMM_Normal);
  
  pScreenMain->Flip(true);
  pScreenMainOverlay->pCanvas->FillFull(0);
//  pScreenMain->GetCanvas(ScrMainID_View)->FillFull(ColorTable.Video.InitBG);
  ptmpcan->BitBltFullBeta(pScreenMain->pViewCanvas);
  pScreenSub->pCanvas->FillFull(ColorTable.Video.InitBG);
  
  delete ptmpcan; ptmpcan=NULL;
  
  IPC6->LCDPowerControl=LCDPC_ON_BOTH;
  pScreenSub->SetBlackOutLevel16(0);
}

DATA_IN_DTCM u32 reqflip=0;

#include "plug_dpg.h"

extern u32 FrameCache_GetBufferSizeByte(void);
extern u16* FrameCache_ReadStart(u64 CurrentSamples);
extern void FrameCache_ReadEnd(void);

static void CB_VBlankHandler(void)
{
  u64 SyncSamples=DPGAudioStream_SyncSamples;
  
  if(IPC6->MP2PauseFlag==false) SyncSamples+=DPGAudioStream_PregapSamples;
  DPGAudioStream_SyncSamples=SyncSamples;
  u16 *pbuf=FrameCache_ReadStart(SyncSamples);
  
  if(pbuf!=NULL){
    if(reqflip!=0){
      if(reqflip==3) pScreenMain->Flip(false);
      pScreenMain->SetBlendLevel(16);
      reqflip=0;
    }
    
    u16 *pVRAMBuf=pScreenMain->pBackCanvas->GetVRAMBuf();
    
    u32 len=FrameCache_GetBufferSizeByte();
    
    while(DMA0_CR & DMA_BUSY);
//    DCache_CleanRangeOverrun(pbuf,len);
//    DCache_FlushRangeOverrun(pVRAMBuf,len);
    DMA0_SRC = (uint32)pbuf;
    DMA0_DEST = (uint32)&pVRAMBuf[(((ScreenWidth*ScreenHeight*2)-len)/2)/2];
    DMA0_CR = DMA_ENABLE | DMA_SRC_INC | DMA_DST_INC | DMA_32_BIT | (len>>2);
    
    reqflip=3;
    
    FrameCache_ReadEnd();
  }

  if(reqflip!=0){
    if(reqflip==3){
      pScreenMain->Flip(false);
      pScreenMain->SetBlendLevel(6);
      }else{
      if(reqflip==2){
        pScreenMain->SetBlendLevel(11);
        }else{
        pScreenMain->SetBlendLevel(16);
      }
    }
    reqflip--;
  }
}

#include "proc_DPGPlay_Trigger_CallBack.h"

void ProcDPGPlay_SetCallBack(TCallBack *pCallBack)
{
  pCallBack->Start=CB_Start;
  pCallBack->VsyncUpdate=CB_VsyncUpdate;
  pCallBack->End=CB_End;
  pCallBack->KeyPress=CB_KeyPress;
  pCallBack->KeySameLRDown=CB_KeySameLRDown;
  pCallBack->MouseDown=CB_MouseDown;
  pCallBack->MouseMove=CB_MouseMove;
  pCallBack->MouseUp=CB_MouseUp;
  pCallBack->VBlankHandler=CB_VBlankHandler;
  pCallBack->PanelClose=CB_PanelClose;
  pCallBack->PanelOpen=CB_PanelOpen;
  
  pCallBack->Trigger_ProcStart=CB_Trigger_ProcStart;
  pCallBack->Trigger_ProcEnd=CB_Trigger_ProcEnd;
  pCallBack->Trigger_Down=CB_Trigger_Down;
  pCallBack->Trigger_Up=CB_Trigger_Up;
  pCallBack->Trigger_LongStart=CB_Trigger_LongStart;
  pCallBack->Trigger_LongEnd=CB_Trigger_LongEnd;
  pCallBack->Trigger_SingleClick=CB_Trigger_SingleClick;
  pCallBack->Trigger_SingleLongStart=CB_Trigger_SingleLongStart;
  pCallBack->Trigger_SingleLongEnd=CB_Trigger_SingleLongEnd;
  pCallBack->Trigger_DoubleClick=CB_Trigger_DoubleClick;
  pCallBack->Trigger_DoubleLongStart=CB_Trigger_DoubleLongStart;
  pCallBack->Trigger_DoubleLongEnd=CB_Trigger_DoubleLongEnd;
  pCallBack->Trigger_TripleClick=CB_Trigger_TripleClick;
}

