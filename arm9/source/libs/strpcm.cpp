
#include <NDS.h>
//#include <NDS/ARM9/CP15.h>

#include "_console.h"
#include "_consoleWriteLog.h"
#include "_const.h"

#include <stdio.h>
#include <stdlib.h>

#include "memtool.h"

#include "../../ipc6.h"

#include "arm9tcm.h"
#include "maindef.h"

#include "strpcm.h"
#include "splash.h"

#include "plugin/plug_mp2.h"

DATA_IN_DTCM vu64 DPGAudioStream_SyncSamples;
DATA_IN_DTCM u32 DPGAudioStream_PregapSamples;

DATA_IN_DTCM volatile bool VBlankPassedFlag;
DATA_IN_DTCM volatile u32 VBlankPassedCount;

DATA_IN_DTCM static int strpcmVolume64;

DATA_IN_DTCM volatile bool strpcmRequestStop;

DATA_IN_DTCM volatile bool strpcmRingEmptyFlag;
DATA_IN_DTCM volatile u32 strpcmRingBufReadIndex;
DATA_IN_DTCM volatile u32 strpcmRingBufWriteIndex;

DATA_IN_DTCM s16 *strpcmRingLBuf=NULL;
DATA_IN_DTCM s16 *strpcmRingRBuf=NULL;
DATA_IN_DTCM s16 *strpcmSilentBuf=NULL;

static void strpcmUpdate(void);

#include "strpcm_ARM7_SelfCheck.h"

static volatile bool VBlank_AutoFlip=false;

static CODE_IN_ITCM void InterruptHandler_VBlank(void)
{
  ARM7_SelfCheck_Check();
  
  VBlankPassedFlag=true;
  VBlankPassedCount++;
  
  Splash_IRQVSYNC();
  
  CallBack_ExecuteVBlankHandler();
  
  if(VBlank_AutoFlip==true) pScreenMain->FlipForVSyncAuto();
}

void VBlank_AutoFlip_Enabled(void)
{
  VBlank_AutoFlip=true;
}

void VBlank_AutoFlip_Disabled(void)
{
  if(VBlank_AutoFlip==false) return;
  VBlank_AutoFlip=false;
  pScreenMain->Flip(true);
}

// ------------------------------------------

extern void IRQSYNC_MP2_flash(void);
extern void IRQSYNC_MP2_fread(void);

static CODE_IN_ITCM void InterruptHandler_IPC_SYNC(void)
{
//  _consolePrintf("CallIPC(%d)\n",IPC6->IR);
  switch(IPC6->IR){
    case IR_NULL: {
    } break;
    case IR_NextSoundData: {
      strpcmUpdate();
      
      const u32 Samples=IPC6->strpcmSamples;
      const u32 Channels=IPC6->strpcmChannels;
      
      DCache_CleanRangeOverrun(IPC6->strpcmLBuf,Samples*2);
      if(Channels==2) DCache_CleanRangeOverrun(IPC6->strpcmRBuf,Samples*2);
      
      IPC6->strpcmWriteRequest=0;
    } break;
    case IR_Flash: {
      IRQSYNC_MP2_flash();
    } break;
    case IR_MP2_fread: {
      IRQSYNC_MP2_fread();
    } break;
    case IR_SyncSamples: {
      u64 curs=IPC6->IR_SyncSamples_SendToARM9;
      u64 bufs=IPC6->strpcmSamples;
      curs+=DPGAudioStream_PregapSamples*4; // gap 4 frames
      if(curs<bufs){
        curs=0;
        }else{
        curs-=bufs;
      }
      DPGAudioStream_SyncSamples=curs;
    } break;
  }
  IPC6->IR=IR_NULL;
}

extern "C" {
extern struct IntTable irqTable[16];
}

void InitInterrupts(void)
{
  REG_IME = 0;
  
  irqInit();
  
  irqSet_u32(IRQ_VBLANK,(u32)InterruptHandler_VBlank);
  irqSet_u32(IRQ_IPC_SYNC,(u32)InterruptHandler_IPC_SYNC);
  irqEnable(IRQ_VBLANK);
  
  REG_IPC_SYNC=IPC_SYNC_IRQ_ENABLE;
  
  VBlankPassedFlag=false;
  VBlankPassedCount=0;
  
  {
    _consoleLogPause();
    _consolePrint("IRQ jump table.\n");
    u32 *p=(u32*)irqTable;
    while(1){
      if(p[1]==0) break;
      _consolePrintf("adr=0x%x trig=%x\n",p[0],p[1]);
      p+=2;
    }
    _consolePrint("----------\n");
    _consoleLogResume();
  }
  
  REG_IME = 1;
}

void strpcmStart(bool FastStart,u32 SampleRate,u32 SamplePerBuf,u32 ChannelCount,EstrpcmFormat strpcmFormat)
{
#ifdef notuseSound
  return;
#endif
  
  while(IPC6->strpcmControl!=ESC_NOP){
    ARM7_SelfCheck_Check();
  }
  
  switch(strpcmFormat){
    case SPF_PCMx1: _consolePrintf("strpcm: set format SPF_PCMx1.\n"); break;
    case SPF_PCMx2: _consolePrintf("strpcm: set format SPF_PCMx2.\n"); break;
    case SPF_PCMx4: _consolePrintf("strpcm: set format SPF_PCMx4.\n"); break;
    case SPF_MP2: _consolePrintf("strpcm: set format SPF_MP2.\n"); break;
    default: {
      _consolePrintf("Fatal error: strpcm unknown format. (%d)\n",strpcmFormat); break;
    }
  }
  
  switch(strpcmFormat){
    case SPF_PCMx1: case SPF_PCMx2: case SPF_PCMx4: case SPF_MP2: {
      strpcmRequestStop=false;
      
      u32 Samples=SamplePerBuf;
      u32 RingSamples=Samples*strpcmRingBufCount;
      
      strpcmRingEmptyFlag=false;
      strpcmRingBufReadIndex=0;
      if(FastStart==false){
        strpcmRingBufWriteIndex=strpcmRingBufCount-1;
        }else{
        strpcmRingBufWriteIndex=1;
      }
      
      strpcmRingLBuf=(s16*)safemalloc(RingSamples*2);
      strpcmRingRBuf=(s16*)safemalloc(RingSamples*2);
      strpcmSilentBuf=(s16*)safemalloc(RingSamples*2);
      
      MemSet16CPU(0,strpcmRingLBuf,RingSamples*2);
      MemSet16CPU(0,strpcmRingRBuf,RingSamples*2);
      MemSet16CPU(0,strpcmSilentBuf,RingSamples*2);
      
      IPC6->strpcmFreq=SampleRate;
      IPC6->strpcmSamples=Samples;
      IPC6->strpcmChannels=ChannelCount;
      IPC6->strpcmFormat=strpcmFormat;
      
      // ------
      
/*
      IPC6->strpcmLBuf=(s16*)safemalloc(Samples*2);
      IPC6->strpcmRBuf=(s16*)safemalloc(Samples*2);
      
      MemSet16CPU(0,IPC6->strpcmLBuf,Samples*2);
      MemSet16CPU(0,IPC6->strpcmRBuf,Samples*2);
*/

      IPC6->strpcmLBuf=NULL;
      IPC6->strpcmRBuf=NULL;
    } break;
  }
  
  // ------
  
#define _REG_WAIT_CR (*(vuint16*)0x04000204)
//  _REG_WAIT_CR|=1 << 7;

  IPC6->strpcmControl=ESC_Play;
  
  while(IPC6->strpcmControl!=ESC_NOP){
    ARM7_SelfCheck_Check();
  }
}

void strpcmStop(void)
{
#ifdef notuseSound
  return;
#endif
  
  strpcmRequestStop=true;
  
//  _consolePrint("Wait for terminate. (0)\n");
  while(IPC6->strpcmControl!=ESC_NOP){
    ARM7_SelfCheck_Check();
  }
  
  IPC6->strpcmControl=ESC_Stop;
  
//  _consolePrint("Wait for terminate. (1)\n");
  while(IPC6->strpcmControl!=ESC_NOP){
    ARM7_SelfCheck_Check();
  }
  
  _consolePrint("ARM7 strpcm terminated.\n");
  
  switch(IPC6->strpcmFormat){
    case SPF_PCMx1: case SPF_PCMx2: case SPF_PCMx4: case SPF_MP2: {
      strpcmRequestStop=false;
      
      strpcmRingEmptyFlag=false;
      strpcmRingBufReadIndex=0;
      strpcmRingBufWriteIndex=0;
      
      if(strpcmRingLBuf!=NULL){
        safefree((void*)strpcmRingLBuf); strpcmRingLBuf=NULL;
      }
      if(strpcmRingRBuf!=NULL){
        safefree((void*)strpcmRingRBuf); strpcmRingRBuf=NULL;
      }
      if(strpcmSilentBuf!=NULL){
        safefree((void*)strpcmSilentBuf); strpcmSilentBuf=NULL;
      }
      
      IPC6->strpcmFreq=0;
      IPC6->strpcmSamples=0;
      IPC6->strpcmChannels=0;
      
/*
      if(IPC6->strpcmLBuf!=NULL){
        safefree(IPC6->strpcmLBuf); IPC6->strpcmLBuf=NULL;
      }
      if(IPC6->strpcmRBuf!=NULL){
        safefree(IPC6->strpcmRBuf); IPC6->strpcmRBuf=NULL;
      }
*/
      IPC6->strpcmLBuf=NULL;
      IPC6->strpcmRBuf=NULL;
    } break;
  }
  
  _consolePrint("strpcm stopped.\n");
}

// ----------------------------------------------

static inline void ins_MemSet16CPU(u16 v,void *dst,u32 len)
{
  len>>=1;
  if(len==0) return;
  
  u16 *_dst=(u16*)dst;
  
  for(u32 cnt=0;cnt<len;cnt++){
    _dst[cnt]=v;
  }
}

static inline void ins_MemCopy16CPU(void *src,void *dst,u32 len)
{
  len>>=1;
  if(len==0) return;
  
  u16 *_src=(u16*)src;
  u16 *_dst=(u16*)dst;
  
  for(u32 idx=0;idx<len;idx++){
    _dst[idx]=_src[idx];
  }
  return;
}

void strpcmUpdate(void)
{
#ifdef notuseSound
  strpcmRingBufReadIndex=(strpcmRingBufReadIndex+1) & strpcmRingBufBitMask;
  return;
#endif

  u32 Samples=IPC6->strpcmSamples;
  const u32 Channels=IPC6->strpcmChannels;
  
/*
  s16 *ldst=IPC6->strpcmLBuf;
  s16 *rdst=IPC6->strpcmRBuf;
  if((ldst==NULL)||(rdst==NULL)) return;
*/
  
/*
  if((strpcmRingLBuf==NULL)||(strpcmRingRBuf==NULL)){
    ins_MemSet16CPU(0,ldst,Samples*2);
    if(Channels==2) ins_MemSet16CPU(0,rdst,Samples*2);
    return;
  }
*/
  
  if((strpcmRingLBuf==NULL)||(strpcmRingRBuf==NULL)){
    IPC6->strpcmLBuf=strpcmSilentBuf;
    IPC6->strpcmRBuf=strpcmSilentBuf;
    return;
  }
  
  bool IgnoreFlag=false;
  
  u32 CurIndex=(strpcmRingBufReadIndex+1) & strpcmRingBufBitMask;
  
  s16 *lsrc=&strpcmRingLBuf[Samples*CurIndex];
  s16 *rsrc=&strpcmRingRBuf[Samples*CurIndex];
  
//  if(strpcmRequestStop==true) IgnoreFlag=true;
  
  if(CurIndex==strpcmRingBufWriteIndex){
    strpcmRingEmptyFlag=true;
    IgnoreFlag=true;
  }
  
/*
  if(IgnoreFlag==true){
    ins_MemSet16CPU(0,ldst,Samples*2);
    if(Channels==2) ins_MemSet16CPU(0,rdst,Samples*2);
    }else{
    ins_MemCopy16CPU(lsrc,ldst,Samples*2);
    if(Channels==2) ins_MemCopy16CPU(rsrc,rdst,Samples*2);
  }
*/
  if(IgnoreFlag==true){
    IPC6->strpcmLBuf=strpcmSilentBuf;
    IPC6->strpcmRBuf=strpcmSilentBuf;
    }else{
    IPC6->strpcmLBuf=lsrc;
    if(Channels==1){
      IPC6->strpcmRBuf=lsrc;
      }else{
      IPC6->strpcmRBuf=rsrc;
    }
  }
 
  strpcmRingBufReadIndex=CurIndex;
}

void strpcmSetVolume64(int v)
{
  if(v<0) v=0;
  if(128<v) v=128;
  
  strpcmVolume64=v;
  
  IPC6->strpcmVolume64=strpcmVolume64;
}

int strpcmGetVolume64(void)
{
  return(strpcmVolume64);
}

