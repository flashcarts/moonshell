
#include <nds.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "_console.h"
#include "_consolewritelog.h"
#include "maindef.h"
#include "memtool.h"
#include "_const.h"
#include "ErrorDialog.h"

#include "procstate.h"
#include "datetime.h"

#include "glib/glib.h"

#include "fat2.h"
#include "shell.h"
#include "strtool.h"

#include "skin.h"
#include "component.h"
#include "sndeff.h"
#include "lang.h"
#include "strpcm.h"
#include "rect.h"
#include "resume.h"

#include "dll.h"

#include "plug_jpeg.h"

extern "C" {
void VRAMWriteCache_Enable(void);
void VRAMWriteCache_Disable(void);
}

#include "proc_ImageView_dfs.h"
#include "proc_ImageView_Popup.h"

static s32 SrcWidth,SrcHeight;
static TRect DstRect;

#define ExceptJpeg

#ifndef ExceptJpeg
static const bool isPlugJpeg=false;
#endif

#ifdef ExceptJpeg
static bool isPlugJpeg=false;
#endif

static TPluginBody *pPluginBody=NULL;
static FAT_FILE *PluginBody_FileHandle=NULL;

#define MCUSize (32)
#define MCUSizeMask (MCUSize-1)

static s32 MultipleFix8;

static bool RequestInterruptBreak;

static bool LeftButtonPressing;

#include "proc_ImageView_preview.h"
#include "proc_ImageView_mcu.h"

static u32 NextReadTimeout;
#define NextReadTimeoutValue (6) // 0.1sec

static u16 *pDrawTempMemory0,*pDrawTempMemory1;

static bool RequestClearRelational;

static bool RequestCreateBG;

// -------------------------------------------------------

static void AjustInsideSrc(TRect *pr)
{
  s32 sw=ScreenWidth*0x100/MultipleFix8;
  s32 sh=ScreenHeight*0x100/MultipleFix8;
  
  pr->w=sw;
  pr->h=sh;
  
  if((SrcWidth-sw)<pr->x) pr->x=SrcWidth-sw;
  if((SrcHeight-sh)<pr->y) pr->y=SrcHeight-sh;
  if(pr->x<0){
    pr->x=0;
    if(SrcWidth<pr->w) pr->w=SrcWidth;
  }
  if(pr->y<0){
    pr->y=0;
    if(SrcHeight<pr->h) pr->h=SrcHeight;
  }
}

// -----------------------------

#include "proc_ImageView_redrawimage.h"
#include "proc_ImageView_createbg.h"

static void ChangeMultipleAndRedraw(u32 mul)
{
  if(mul==0) return;
  
  s32 sw=SrcWidth,sh=SrcHeight;
  if(ScreenWidth<sw) sw=ScreenWidth;
  if(ScreenHeight<sh) sh=ScreenHeight;
  s32 cx=DstRect.x+((sw*0x100/MultipleFix8)/2);
  s32 cy=DstRect.y+((sh*0x100/MultipleFix8)/2);
  
  MultipleFix8=mul;
  ProcState.Image.MultipleFix8=MultipleFix8;
  ProcState_RequestSave=true;
  Popup_Show_Zoom(MultipleFix8);
  DstRect.x=cx-((sw*0x100/MultipleFix8)/2);
  DstRect.y=cy-((sh*0x100/MultipleFix8)/2);
  AjustInsideSrc(&DstRect);
  PrfStart();
  RedrawImage();
  PrfEnd(0);
}

// -----------------------------

#include "proc_ImageView_relationalto.h"

static void RelationalToBack(void)
{
  MCU_ClearAllCache();
  
  ImageFiles_Init();
  if(ImageFilesCount==0){
    _consolePrintf("Fatal error: RelationalToBack: File not found.\n");
    ShowLogHalt();
  }
  
  u32 curidx;
  
  if(ImageFilesCount==1){
    curidx=0;
    }else{
    u32 idx=GetCurrentFileIndex();
    if(idx==0){
      curidx=ImageFilesCount-1;
      }else{
      curidx=idx-1;
    }
    _consolePrintf("MoveBack %d to %d.\n",idx,curidx);
  }
  
  Unicode_Copy(RelationalFileNameUnicode,GetCurrentFilenameUnicode(curidx));
  Unicode_Copy(ProcState.FileList.SelectFilenameUnicode,GetCurrentFilenameUnicode(curidx));
  ProcState_RequestSave=true;
  
  ImageFiles_Free();
  
  RelationalFilePos=0;
  RequestClearRelational=false;
  SetNextProc(ENP_ImageView,EPFE_None);
}

static void RelationalToNext(void)
{
  MCU_ClearAllCache();
  
  ImageFiles_Init();
  if(ImageFilesCount==0){
    _consolePrintf("Fatal error: RelationalToNext: File not found.\n");
    ShowLogHalt();
  }
  
  u32 curidx;
  
  if(ImageFilesCount==1){
    curidx=0;
    }else{
    u32 idx=GetCurrentFileIndex();
    if(idx==(ImageFilesCount-1)){
      curidx=0;
      }else{
      curidx=idx+1;
    }
    _consolePrintf("MoveNext %d to %d.\n",idx,curidx);
  }
  
  Unicode_Copy(RelationalFileNameUnicode,GetCurrentFilenameUnicode(curidx));
  Unicode_Copy(ProcState.FileList.SelectFilenameUnicode,GetCurrentFilenameUnicode(curidx));
  ProcState_RequestSave=true;
  
  ImageFiles_Free();
  
  RelationalFilePos=0;
  RequestClearRelational=false;
  SetNextProc(ENP_ImageView,EPFE_None);
}

// -----------------------------

static void CB_KeyPress(u32 VsyncCount,u32 Keys)
{
  NextReadTimeout=NextReadTimeoutValue;
  
  if(LeftButtonPressing==true){
    if((Keys&(KEY_X|KEY_Y))!=0){
      if((Keys&KEY_X)!=0) ChangeNextBacklightLevel();
      if((Keys&KEY_Y)!=0) ChangePrevBacklightLevel();
      Popup_Show_BacklightLevel();
    }
    return;
  }
  
  if((Keys&KEY_B)!=0){
    Sound_Start(WAVFN_Click);
    RequestClearRelational=true;
    SetNextProc(ENP_FileList,EPFE_None);
  }
  
  if((Keys&KEY_SELECT)!=0){
    Sound_Start(WAVFN_Click);
    RequestClearRelational=false;
    SetNextProc(ENP_ImageCustom,EPFE_None);
  }
  
  if((Keys&KEY_START)!=0){
    Sound_Start(WAVFN_Click);
    RequestCreateBG=true;
    RequestClearRelational=true;
    SetNextProc(ENP_FileList,EPFE_None);
  }
  
  if((Keys&(KEY_UP|KEY_DOWN|KEY_LEFT|KEY_RIGHT))!=0){
    s32 vx=0,vy=0;
    if((Keys&KEY_UP)!=0) vy=-1;
    if((Keys&KEY_DOWN)!=0) vy=1;
    if((Keys&KEY_LEFT)!=0) vx=-1;
    if((Keys&KEY_RIGHT)!=0) vx=1;
    if((Keys&(KEY_L|KEY_R))!=0){
      vx*=2;
      vy*=2;
    }
    if(ProcState.Image.DoubleSpeedKey==true){
      vx*=2;
      vy*=2;
    }
    DstRect.x+=vx*(32*0x100/MultipleFix8);
    DstRect.y+=vy*(32*0x100/MultipleFix8);
    AjustInsideSrc(&DstRect);
    RedrawImage();
  }
  
  if((Keys&(KEY_Y|KEY_X|KEY_A))!=0){
    u32 mul=0;
    if((Keys&KEY_Y)!=0) mul=0.5*0x100;
    if((Keys&KEY_X)!=0) mul=1*0x100;
    if((Keys&KEY_A)!=0) mul=2*0x100;
    ChangeMultipleAndRedraw(mul);
  }
}

// -----------------------------------------------------

static u32 GetIconType(s32 x,s32 y)
{
  const s32 margin=32;
  
  s32 lx=ScreenWidth-margin;
  s32 ly=ScreenHeight-margin;
  
  u32 IconType=0;
  
  if((x<margin)&&(y<margin)) IconType=1;
  if((lx<=x)&&(y<margin)) IconType=2;
  if((x<margin)&&(ly<=y)) IconType=3;
  if((lx<=x)&&(ly<=y)) IconType=4;
  
  if(IconType!=0) _consolePrintf("Clicked icon type= %d. (%d,%d)\n",IconType,x,y);
  return(IconType);
}

static void ExecIcon(u32 IconType)
{
  switch(IconType){
    case 0: break;
    case 1: RelationalToBack(); break;
    case 2: RelationalToNext(); break;
    case 3: {
      u32 mul=MultipleFix8;
      if(mul==(0.5*0x100)){
        mul=1*0x100;
        }else{
        if(mul==(1*0x100)){
          mul=2*0x100;
          }else{
          if(mul==(2*0x100)){
            mul=0.5*0x100;
            }else{
            mul=1*0x100; // except else
          }
        }
      }
      ChangeMultipleAndRedraw(mul);
    } break;
    case 4: {
      Sound_Start(WAVFN_Click);
      RequestClearRelational=false;
      SetNextProc(ENP_ImageCustom,EPFE_None);
    } break;
  }
}

// -----------------------------------------------------

static bool dragf;
static s32 dragx,dragy;

enum EMouseMode {EMM_Normal,EMM_Icon,EMM_ThumbOut,EMM_ThumbIn};
static EMouseMode MouseMode;

static void CB_MouseDown(s32 x,s32 y)
{
  dragf=true;
  dragx=x;
  dragy=y;
  
  NextReadTimeout=NextReadTimeoutValue;
  
  if(LeftButtonPressing==false){
    if(GetIconType(x,y)==0){
      MouseMode=EMM_Normal;
      }else{
      MouseMode=EMM_Icon;
    }
    }else{
    TRect r=DstRect;
    r.x=DstRect.x*PreviewMultipleFix8/0x100;
    r.y=DstRect.y*PreviewMultipleFix8/0x100;
    r.w=DstRect.w*PreviewMultipleFix8/0x100;
    r.h=DstRect.h*PreviewMultipleFix8/0x100;
    if((r.x<=x)&&(x<(r.x+r.w))&&(r.y<=y)&&(y<(r.y+r.h))){
      MouseMode=EMM_ThumbIn;
      }else{
      MouseMode=EMM_ThumbOut;
    }
  }
}

static void CB_MouseMove(s32 x,s32 y)
{
  if(dragf==false) return;
  
  NextReadTimeout=NextReadTimeoutValue;
  
  s32 mx=dragx-x;
  s32 my=dragy-y;
  dragx=x;
  dragy=y;
  
  bool RefreshPreview=false;
  
  switch(MouseMode){
    case EMM_Normal: {
      u32 keys=(~REG_KEYINPUT)&0x3ff;
      if((keys&(KEY_L|KEY_R))!=0){
        mx*=2;
        my*=2;
      }
      if(ProcState.Image.DoubleSpeedTouch==true){
        mx*=2;
        my*=2;
      }
      DstRect.x+=mx*2*0x100/MultipleFix8;
      DstRect.y+=my*2*0x100/MultipleFix8;
    } break;
    case EMM_Icon: {
    } break;
    case EMM_ThumbOut: {
      if(x<PreviewOfsX){
        x=0;
        }else{
        x-=PreviewOfsX;
      }
      if(y<PreviewOfsY){
        y=0;
        }else{
        y-=PreviewOfsY;
      }
      u32 cx=x*0x100/PreviewMultipleFix8;
      u32 cy=y*0x100/PreviewMultipleFix8;
      DstRect.x=cx-((ScreenWidth*0x100/MultipleFix8)/2);
      DstRect.y=cy-((ScreenHeight*0x100/MultipleFix8)/2);
      RefreshPreview=true;
    } break;
    case EMM_ThumbIn: {
      u32 cx=mx*0x100/PreviewMultipleFix8;
      u32 cy=my*0x100/PreviewMultipleFix8;
      DstRect.x-=cx;
      DstRect.y-=cy;
      RefreshPreview=true;
    } break;
  }
  
  AjustInsideSrc(&DstRect);
  if(RefreshPreview==true) Preview_Draw();
  RedrawImage();
}

static void CB_MouseUp(s32 x,s32 y)
{
  if(dragf==false) return;
  dragf=false;
  
  switch(MouseMode){
    case EMM_Normal: break;
    case EMM_Icon: ExecIcon(GetIconType(x,y)); break;
    case EMM_ThumbOut: break;
    case EMM_ThumbIn: break;
  }
  
  NextReadTimeout=NextReadTimeoutValue;
}

static void CB_Start(void)
{
  {
    CglCanvas *pcan=pScreenMainOverlay->pCanvas;
    const char *pstr0=Lang_GetUTF8("IV_LoadMsg0");
    const char *pstr1=Lang_GetUTF8("IV_LoadMsg1");
    u32 x0=(ScreenWidth-pcan->GetTextWidthUTF8(pstr0))/2;
    u32 x1=(ScreenWidth-pcan->GetTextWidthUTF8(pstr1))/2;
    u32 th=glCanvasTextHeight+2;
    u32 y0=(ScreenHeight-(th*2))/2;
    u32 y1=y0+th;
    
    pcan->SetColor(RGB15(24,24,24)|BIT15);
    pcan->FillFast(0,y0-4,ScreenWidth,th*2+6);
    
    pcan->SetFontTextColor(RGB15(31,31,31)|BIT15);
    pcan->TextOutUTF8(x0+1,y0+1,pstr0);
    pcan->TextOutUTF8(x1+1,y1+1,pstr1);
    
    pcan->SetFontTextColor(RGB15(0,0,0)|BIT15);
    pcan->TextOutUTF8(x0,y0,pstr0);
    pcan->TextOutUTF8(x1,y1,pstr1);
  }
  
  {
    CglCanvas *pcan=pScreenMainOverlay->pCanvas;
    u32 x=0,y=ScreenHeight-32;
    
    pcan->SetColor(RGB15(9,7,7)|BIT15);
    pcan->FillFast(x,y,ScreenWidth-x,1);
    y+=1;
    pcan->SetColor(RGB15(17,15,15)|BIT15);
    pcan->FillFast(x,y,ScreenWidth-x,1);
    y+=1;
    pcan->SetColor(RGB15(25,23,23)|BIT15);
    pcan->FillFast(x,y,ScreenWidth-x,2+28);
    y+=2;
    
    x+=8;
    
    ImageFiles_Init();
    if(ImageFilesCount==0){
      _consolePrintf("Fatal error: RelationalToNext: File not found.\n");
      ShowLogHalt();
    }
    
    u32 idx=GetCurrentFileIndex(),cnt=GetImageFilesCount();
    
    char str[32];
    snprintf(str,32,"%d / %d",1+idx,cnt);
    
    pcan->SetFontTextColor(RGB15(31,31,31)|BIT15);
    pcan->TextOutA(x+1,y+1,str);
    pcan->SetFontTextColor(RGB15(0,0,0)|BIT15);
    pcan->TextOutA(x,y,str);
    y+=14;
    
    pcan->SetFontTextColor(RGB15(31,31,31)|BIT15);
    pcan->TextOutW(x+1,y+1,RelationalFileNameUnicode);
    pcan->SetFontTextColor(RGB15(0,0,0)|BIT15);
    pcan->TextOutW(x,y,RelationalFileNameUnicode);
    
    ImageFiles_Free();
  }
  
  LeftButtonPressing=false;
  
  dragf=false;
  dragx=0;
  dragy=0;
  MouseMode=EMM_Normal;
  
  RequestClearRelational=true;
  
  RequestCreateBG=false;
  
  RequestInterruptBreak=false;
  
  pDrawTempMemory0=(u16*)safemalloc((MCUSize*2)*(MCUSize*2)*2); // 32x32 x 200% x 16bit
  pDrawTempMemory1=(u16*)safemalloc((MCUSize*2)*(MCUSize*2)*2);

  {
    FAT_FILE *pSwapFile=Shell_FAT_fopen_SwapFile();
    DFS_Init(pSwapFile);
    FAT2_fclose(pSwapFile);
  }
  
  const char *pFullFilenameAlias=ConvertFull_Unicode2Alias(RelationalFilePathUnicode,RelationalFileNameUnicode);
  const char *pext=&pFullFilenameAlias[strlen(pFullFilenameAlias)-4];
  
  PluginBody_FileHandle=FAT2_fopen_AliasForRead(pFullFilenameAlias);
  if(PluginBody_FileHandle==NULL){
    _consolePrintf("Data file not found. [%s]\n",pFullFilenameAlias);
    ShowLogHalt();
  }
  
#ifdef ExceptJpeg
  isPlugJpeg=false;
  if(isStrEqual_NoCaseSensitive(pext,".jpg")==true) isPlugJpeg=true;
  if(isStrEqual_NoCaseSensitive(pext,".jpe")==true) isPlugJpeg=true;
#endif
  
  if(isPlugJpeg==true){
    if(PlugJpeg_Start(PluginBody_FileHandle,ProcState.Image.AutoFitting)==false){
      _consolePrint("Jpeg plugin start error.\n");
      RelationalFile_Clear();
      ErrorDialog_Set(EEC_NotSupportFileFormat);
      RequestClearRelational=true;
      SetNextProc(ENP_FileList,EPFE_None);
      return;
    }
    
    SrcWidth=PlugJpeg_GetWidth();
    SrcHeight=PlugJpeg_GetHeight();
    }else{
    char fn[PluginFilenameMax];
    EPluginType PluginType=DLLList_GetPluginFilename(pext,fn);
    if(PluginType!=EPT_Image){
      FAT2_fclose(PluginBody_FileHandle); PluginBody_FileHandle=NULL;
      _consolePrintf("This plugin is not image type. (%s)\n",fn);
      ShowLogHalt();
    }
    
    pPluginBody=DLLList_LoadPlugin(fn);
    if(pPluginBody==NULL){
      FAT2_fclose(PluginBody_FileHandle); PluginBody_FileHandle=NULL;
      _consolePrint("Plugin load error.\n");
      ShowLogHalt();
    }
    
    if(pPluginBody->pIL->Start((int)PluginBody_FileHandle)==false){
      _consolePrint("Plugin start error.\n");
      RelationalFile_Clear();
      ErrorDialog_Set(EEC_NotSupportFileFormat);
      RequestClearRelational=true;
      SetNextProc(ENP_FileList,EPFE_None);
      return;
    }
    
    SrcWidth=pPluginBody->pIL->GetWidth();
    SrcHeight=pPluginBody->pIL->GetHeight();
  }
  
  MultipleFix8=1*0x100;
  if(ProcState.Image.MultipleResume==true) MultipleFix8=ProcState.Image.MultipleFix8;
  if((MultipleFix8!=(0.5*0x100))&&(MultipleFix8!=(1*0x100))&&(MultipleFix8!=(2*0x100))) MultipleFix8=1*0x100;
  
  DstRect.x=(RelationalFilePos>>0)&0xffff;
  DstRect.y=(RelationalFilePos>>16)&0xffff;
  AjustInsideSrc(&DstRect);
  
  Preview_Init();
  Popup_Init();
  
  if(MCU_Init()==false){
    RequestClearRelational=true;
    SetNextProc(ENP_FileList,EPFE_None);
    return;
  }
  
  Popup_Show_Zoom(MultipleFix8);
  
  Preview_Draw();
  PrfStart();
  RedrawImage();
  PrfEnd(0);
  VBlank_AutoFlip_Enabled();
  
  Resume_SetResumeMode(ERM_Image);
  Resume_SetFilename(ConvertFull_MargeFromSplit(RelationalFilePathUnicode,RelationalFileNameUnicode));
  Resume_SetPos(((DstRect.x&0xffff)<<0) | ((DstRect.y&0xffff)<<16));
  Resume_Save();
  
  if(RequestInterruptBreak==false){
    Sound_Start(WAVFN_Open);
    }else{
    Sound_Start(WAVFN_Notify);
  }
  
  NextReadTimeout=NextReadTimeoutValue;
  
  if(false){
    CglCanvas *pcan=pScreenMainOverlay->pCanvas;
    u32 th=glCanvasTextHeight+2;
    u32 y=(ScreenHeight-(th*2))/2;
    
    pcan->SetColor(0);
    pcan->FillFast(0,y,ScreenWidth,th*2);
  }
  
  if(ProcState.Image.ShowControlIcons==false){
    pScreenMainOverlay->pCanvas->FillFull(0);
    }else{
    CglCanvas *pbm=ImageView_GetSkin(EIVS_OverlayBG)->pCanvas;
    u16 *pb=pbm->GetVRAMBuf();
    u16 size=pbm->GetWidth()*pbm->GetHeight();
    for(u32 idx=0;idx<size;idx++){
      if(pb[idx]==(RGB15(0,31,0)|BIT15)) pb[idx]=0;
    }
    pbm->BitBltFullBeta(pScreenMainOverlay->pCanvas);
    Skin_Load_ImageView_AfterFree();
  }
  
  if(RequestInterruptBreak==true){
    pScreenMain->pViewCanvas->FillFull(RGB15(16,16,16)|BIT15);
    pScreenMain->pBackCanvas->FillFull(RGB15(16,16,16)|BIT15);
    RequestClearRelational=true;
    SetNextProc(ENP_FileList,EPFE_None);
  }
}

static void CB_VsyncUpdate(u32 VsyncCount)
{
  Popup_VsyncUpdate(VsyncCount);
  
  if(NextReadTimeout!=0){
    NextReadTimeout--;
    if(NextReadTimeout==(NextReadTimeoutValue-2)) Preview_Draw(); // preview interval at 0.032sec
    if(NextReadTimeout==0){
      ProcState_Save();
      u32 pos=((DstRect.x&0xffff)<<0) | ((DstRect.y&0xffff)<<16);
      if(Resume_GetPos()!=pos){
        Resume_SetPos(pos);
        Resume_Save();
      }
    }
  }
  
  if(NextReadTimeout==0){
    for(u32 y=0;y<=MCUSize*MCUYCount;y++){
      if(VBlankPassedFlag==true) break;
      ReadMCUBlock(y);
    }
  }
}

static void CB_End(void)
{
  VBlank_AutoFlip_Disabled();
  REG_POWERCNT = POWER_ALL_2D;
  
  if(RequestClearRelational==true) RelationalFile_Clear();
  
  if(RequestCreateBG==true) CreateBG();
  
  pScreenMainOverlay->pCanvas->FillFull(0);
  
  Resume_Clear();
  
  Popup_Free();
  
  if(pDrawTempMemory0!=NULL){
    safefree(pDrawTempMemory0); pDrawTempMemory0=NULL;
  }
  if(pDrawTempMemory1!=NULL){
    safefree(pDrawTempMemory1); pDrawTempMemory1=NULL;
  }
  
#ifdef ExceptJpeg
  if(isPlugJpeg==true){
    isPlugJpeg=false;
    PlugJpeg_Free();
  }
#endif
  
  if(pPluginBody!=NULL){
    DLLList_FreePlugin(pPluginBody); pPluginBody=NULL;
  }
  
  if(PluginBody_FileHandle!=NULL){
    FAT2_fclose(PluginBody_FileHandle); PluginBody_FileHandle=NULL;
  }
  
  DFS_Free();
  
  MCU_Free();
  Preview_Free();
  
  ProcState_RequestSave=true;
  ProcState_Save();
}

#include "proc_ImageView_Trigger_CallBack.h"

void ProcImageView_SetCallBack(TCallBack *pCallBack)
{
  pCallBack->Start=CB_Start;
  pCallBack->VsyncUpdate=CB_VsyncUpdate;
  pCallBack->End=CB_End;
  pCallBack->KeyPress=CB_KeyPress;
  pCallBack->MouseDown=CB_MouseDown;
  pCallBack->MouseMove=CB_MouseMove;
  pCallBack->MouseUp=CB_MouseUp;
  
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

