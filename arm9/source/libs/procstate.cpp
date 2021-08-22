
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <NDS.h>

#include "_console.h"
#include "_consolewritelog.h"
#include "_const.h"
#include "memtool.h"
#include "fat2.h"
#include "lang.h"
#include "zlibhelp.h"
#include "splash.h"

#include "procstate.h"

#include "shell.h"
#include "disc_io.h"
#include "../../ipc6.h"

#define CurrentProcStateVersion (38)

TProcState ProcState;
bool ProcState_RequestSave;

static void DataInit(void)
{
  TProcState *pstate=&ProcState;
  
  pstate->Version=CurrentProcStateVersion;
  
  {
    TProcState_System *psys=&pstate->System;
    psys->SkipSetup=false;
    psys->BootCheckDisk=true;
    psys->ClickSound=true;
    psys->EnableFadeEffect=true;
    psys->AutoLastState=true;

    psys->LastState=ELS_FileList;
    psys->Volume64=64;
    psys->BacklightLevel=IPC6->DefaultBrightness;
    psys->SkinFilenameUnicode[0]=0;
    
    psys->BootCount=0;
    
    psys->Reserved0=(u32)-1;
    psys->Reserved1=(u32)-1;
    psys->Reserved2=(u32)-1;
    psys->Reserved3=(u32)-1;
  }
  
  {
    TProcState_FileList *pfl=&pstate->FileList;
    pfl->CurrentPathUnicode[0]=(UnicodeChar)'/';
    pfl->CurrentPathUnicode[1]=(UnicodeChar)0;
    pfl->SelectFilenameUnicode[0]=(UnicodeChar)0;
    pfl->SelectWindowTopOffset=0;
    pfl->Mode=EPSFLM_Double;
    pfl->PlayMode=EPSFLPM_AllRep;
    pfl->MoveFolderLocked=false;
    pfl->HiddenFilenameExt=false;
    pfl->HiddenNotSupportFileType=true;
    pfl->ShowID3Tag=true;
    pfl->BacklightTimeoutSec=10;
    pfl->ScreenSaver_BlackMode=false;
    pfl->DisableLRKeyOnPanelClosed=false;
    pfl->LRClickLongSeek=true;
    
    pfl->Reserved0=(u32)-1;
    pfl->Reserved1=(u32)-1;
    pfl->Reserved2=(u32)-1;
    pfl->Reserved3=(u32)-1;
  }
  
  {
    TProcState_DPG *pdpg=&pstate->DPG;
    pdpg->EnabledFastStart=true;
    pdpg->PlayMode=EDPM_AllRep;
    pdpg->BacklightFlag=true;
    pdpg->PauseWhenPanelClosed=true;
  }
  
  {
    TProcState_Image *pimg=&pstate->Image;
    pimg->MultipleFix8=1*0x100;
    pimg->ShowInfomation=true;
    pimg->ShowControlIcons=true;
    pimg->DoubleSpeedKey=false;
    pimg->DoubleSpeedTouch=false;
    pimg->MultipleResume=true;
    pimg->AutoFitting=true;
    pimg->EffectHeightPadding=true;
    pimg->EffectPastelForTopBG=false;
    pimg->EffectPastelForBottomBG=true;
  }
  
  {
    TProcState_Text *ptxt=&pstate->Text;
    ptxt->TopScrMode=ETTSM_Clock;
    ptxt->ScreenSaver_BlackMode=true;
    ptxt->FontSize=Text_FontSize_Middle;
    ptxt->ClearTypeFont=false;
    ptxt->LineSpace=ETLS_Middle;
    ptxt->DefaultCodePage=1252;
    ptxt->DetectCharCode_ANSI=true;
    ptxt->DetectCharCode_SJIS=true;
    ptxt->DetectCharCode_UTF16BE=true;
    ptxt->DetectCharCode_UTF16LE=true;
    ptxt->DetectCharCode_UTF8=true;
  }
  
}

extern LPIO_INTERFACE active_interface;

static u32 DataSectorIndex=0;

void ProcState_Init(void)
{
  DataInit();
  
  ProcState_RequestSave=false;
  
  DataSectorIndex=0;
  
  Splash_Update();
  
  const char *fn=DefaultDataPath "/" DataFilename;
  const char *palias=ConvertFullPath_Ansi2Alias(fn);
  _consolePrintf("Open setting file. [%s]\n",fn);
  if(palias!=NULL){
    FAT_FILE *pf=pf=FAT2_fopen_AliasForModify(palias);
    if(pf!=NULL){
      if(FAT2_GetFileSize(pf)==512){
        if(pf->firstCluster!=0){
          DataSectorIndex=FAT2_ClustToSect(pf->firstCluster);
        }
      }
      FAT2_fclose(pf);
    }
  }
  
  Splash_Update();
  
  _consolePrintf("DataSectorIndex=%d.\n",DataSectorIndex);
  
  if(DataSectorIndex==0){
    _consolePrintf("FatalError: File not found or size error. [%s]\n",DataFilename);
    ShowLogHalt();
  }
}

void ProcState_Clear(void)
{
  DataInit();
  
  ProcState_RequestSave=true;
}

static u8 fbuf[512];
static u32 fbuf_pos;

static void fbuf_LoadFromDisk(void)
{
  if(DataSectorIndex==0){
    _consolePrint("FatalError: Data sector index is 0.");
    ShowLogHalt();
  }
  active_interface->fn_ReadSectors(DataSectorIndex,1,fbuf);
}

static void fbuf_SaveToDisk(void)
{
  if(DataSectorIndex==0){
    _consolePrint("FatalError: Data sector index is 0.");
    ShowLogHalt();
  }
  active_interface->fn_WriteSectors(DataSectorIndex,1,fbuf);
}

static void fbuf_SetPos(u32 pos)
{
  fbuf_pos=pos;
}

static u32 fbuf_r32(void)
{
  u32 d=*(u32*)&(fbuf[fbuf_pos]);
  fbuf_pos+=4;
  return(d);
}

static void fbuf_w32(u32 d)
{
  *(u32*)&(fbuf[fbuf_pos])=d;
  fbuf_pos+=4;
}

static void* fbuf_GetCurrentPointer(void)
{
  return(&(fbuf[fbuf_pos]));
}

void ProcState_Load(void)
{
  _consolePrintf("Load settings.\n");
  
  fbuf_LoadFromDisk();
  fbuf_SetPos(0);
  
  Splash_Update();
  
  u32 ver=fbuf_r32();
  if(ver!=CurrentProcStateVersion){
    _consolePrint("This file is old version setting. load default.\n");
    DataInit();
    }else{
    Splash_Update();
    _consolePrint("Setting load from file.\n");
    
    TZLIBData ZLIBData;
    
    ZLIBData.SrcSize=fbuf_r32();
    ZLIBData.pSrcBuf=(u8*)fbuf_GetCurrentPointer();
    ZLIBData.DstSize=sizeof(TProcState);
    ZLIBData.pDstBuf=(u8*)&ProcState;
    
    if(zlibdecompress(&ZLIBData)==false){
      _consolePrint("ZLIB decompress error. load default.\n");
      DataInit();
    }
    
    ZLIBData.pSrcBuf=NULL;
    ZLIBData.pDstBuf=NULL;
  }
  
  Splash_Update();
  
  u32 FontSize=ProcState.Text.FontSize;
  if((FontSize!=Text_FontSize_Small)&&(FontSize!=Text_FontSize_Middle)&&(FontSize!=Text_FontSize_Large)){
    _consolePrintf("Illigal font size detected. reset to middle.\n");
    FontSize=Text_FontSize_Middle;
    ProcState.Text.FontSize=FontSize;
  }
  
  ProcState_RequestSave=false;
}

static void ProcState_Save_inc_FillFutter(UnicodeChar *pustr)
{
  u32 idx=0;
  
  for(;idx<MaxFilenameLength;idx++){
    if(pustr[idx]==0) break;
  }
  for(;idx<MaxFilenameLength;idx++){
    pustr[idx]=0;
  }
}

void ProcState_Save(void)
{
  if(ProcState_RequestSave==false) return;
  ProcState_RequestSave=false;
  
//  _consolePrint("Save settings.\n");
  
  ProcState_Save_inc_FillFutter(ProcState.System.SkinFilenameUnicode);
  
  ProcState_Save_inc_FillFutter(ProcState.FileList.CurrentPathUnicode);
  ProcState_Save_inc_FillFutter(ProcState.FileList.SelectFilenameUnicode);
  
  TZLIBData ZLIBData;
  
  ZLIBData.SrcSize=sizeof(TProcState);
  ZLIBData.pSrcBuf=(u8*)&ProcState;
  ZLIBData.DstSize=0;
  ZLIBData.pDstBuf=NULL;
  
  REG_IME=0;
  if(zlibcompress(&ZLIBData,0)==false){
    _consolePrintf("Fatal error: ZLIB compress error!\n");
    ShowLogHalt();
  }
  REG_IME=1;
  
  if((512-8)<=ZLIBData.DstSize){
    _consolePrintf("Fatal error: Data file buffer overflow.\n");
    ShowLogHalt();
  }
  
  fbuf_SetPos(0);
  
  fbuf_w32(CurrentProcStateVersion);
  fbuf_w32(ZLIBData.DstSize);
  MemCopy8CPU(ZLIBData.pDstBuf,fbuf_GetCurrentPointer(),ZLIBData.DstSize);
  
  REG_IME=0;
  fbuf_SaveToDisk();
  REG_IME=1;
  
  ZLIBData.DstSize=0;
  if(ZLIBData.pDstBuf!=NULL){
    safefree(ZLIBData.pDstBuf); ZLIBData.pDstBuf=NULL;
  }
  
//  _consolePrintf("Saved setting.\n");
}

void ApplyCurrentBacklightLevel(void)
{
  TProcState_System *psys=&ProcState.System;
  IPC6->Brightness=psys->BacklightLevel;
  _consolePrintf("Backlight set to %d.\n",psys->BacklightLevel);
}

void ChangePrevBacklightLevel(void)
{
  TProcState_System *psys=&ProcState.System;
  
  if(psys->BacklightLevel==0){
    psys->BacklightLevel=3;
    }else{
    psys->BacklightLevel--;
  }
  
  ProcState_RequestSave=true;
  
  ApplyCurrentBacklightLevel();
}

void ChangeNextBacklightLevel(void)
{
  TProcState_System *psys=&ProcState.System;
  
  psys->BacklightLevel++;
  if(psys->BacklightLevel==4) psys->BacklightLevel=0;
  
  ProcState_RequestSave=true;
  
  ApplyCurrentBacklightLevel();
}

