#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <nds.h>

#include "_console.h"

#include "../../ipc6.h"
#include "memtoolARM7.h"
#include "a7sleep.h"

#include "plug_mp2.h"

static inline void ShowLogHalt(void)
{
  while(1);
}

#include "main_ARM7_SelfCheck.h"

//---------------------------------------------------------------------------------
static void startSound(int sampleRate, const void* data, u32 bytes, u8 channel, u8 vol,  u8 pan, u8 format) {
//---------------------------------------------------------------------------------
	if((data==NULL)||(bytes==0)) return;
	
	SCHANNEL_TIMER(channel)  = SOUND_FREQ(sampleRate);
	SCHANNEL_SOURCE(channel) = (u32)data;
	SCHANNEL_LENGTH(channel) = bytes >> 2 ;
	SCHANNEL_REPEAT_POINT(channel) = 0;
	if(IPC6->LoopSound==true){
		SCHANNEL_CR(channel)     = SCHANNEL_ENABLE | SOUND_REPEAT | SOUND_VOL(vol) | SOUND_PAN(pan) | (format==1?SOUND_8BIT:SOUND_16BIT);
		}else{
		SCHANNEL_CR(channel)     = SCHANNEL_ENABLE | SOUND_ONE_SHOT | SOUND_VOL(vol) | SOUND_PAN(pan) | (format==1?SOUND_8BIT:SOUND_16BIT);
	}
}


//---------------------------------------------------------------------------------
static s32 getFreeSoundChannel() {
//---------------------------------------------------------------------------------
	int i;
	for (i=4; i<16; i++) { // 2 to 15
		if ( (SCHANNEL_CR(i) & SCHANNEL_ENABLE) == 0 ) return i;
	}
	return -1;
}

//---------------------------------------------------------------------------------
static void stopSound() {
//---------------------------------------------------------------------------------
	int i;
	for (i=4; i<16; i++) { // 2 to 15
		SCHANNEL_CR(i)     = 0;
	}
}

//////////////////////////////////////////////////////////////////////

extern void CallBackIRQ_strpcmUpdate(void);

// ---------------------------

#include "fpga/romeo2.h"
#include "fpga/pcm1770.h"
#include "fpga/rawpcm.h"
#include "fpga/romeo2_hp.h"

#include "main_fpga.h"

// ---------------------------

static u32 strpcmCursorFlag=0;

static u32 strpcmFreq,strpcmSamples,strpcmChannels;
static EstrpcmFormat strpcmFormat=(EstrpcmFormat)0;

static s16 *strpcmL0=NULL,*strpcmL1=NULL,*strpcmR0=NULL,*strpcmR1=NULL;
static s32 strpcmLastL,strpcmLastR;

static s32 agc;

#undef SOUND_FREQ
#define SOUND_FREQ(n)	(0x10000 - (16777216 / (n)))

static inline void strpcmPlay()
{
  _consolePrintf("strpcmPlay();\n");
  
  IPC6->IR=IR_NULL;
  
  strpcmCursorFlag=0;
  
  strpcmFormat=IPC6->strpcmFormat;
  
  switch(strpcmFormat){
    case SPF_PCMx1: case SPF_PCMx2: case SPF_PCMx4: SetMemoryMode(false); break;
    case SPF_MP2: SetMemoryMode(false); break;
    default: {
      _consolePrintf("Unknown strpcm format.\n");
      ShowLogHalt();
    }
  }
  
  switch(strpcmFormat){
    case SPF_PCMx1: case SPF_PCMx2: case SPF_PCMx4: {
    } break;
    case SPF_MP2: {
      extern void libmp2_getfromipc3(void);
      libmp2_getfromipc3();
      ARM7_SelfCheck_Check();
      if(StartMP2()==false){ a7led(3); while(1); }
      ARM7_SelfCheck_Check();
      IPC6->strpcmSamples=MP2_GetSamplePerFrame();
      IPC6->strpcmChannels=MP2_GetChannelCount();
      // Ignore MP2 stream sample rate. set from ARM9 DPGINFO.SndFreq
      // IPC6->strpcmFreq=MP2_GetSampleRate();
      ARM7_SelfCheck_Check();
    } break;
    default: {
      _consolePrintf("Unknown strpcm format.\n");
      ShowLogHalt();
    } break;
  }
  
  strpcmFreq=IPC6->strpcmFreq;
  strpcmSamples=IPC6->strpcmSamples;
  strpcmChannels=IPC6->strpcmChannels;
  
  switch(strpcmFormat){
    case SPF_PCMx1: {
    } break;
    case SPF_PCMx2: {
      strpcmFreq*=2;
      strpcmSamples*=2;
    } break;
    case SPF_PCMx4: {
      strpcmFreq*=4;
      strpcmSamples*=4;
    } break;
    case SPF_MP2: {
    } break;
  }
  
  _consolePrintf("strpcm freq=%d samples=%d chs=%d\n",strpcmFreq,strpcmSamples,strpcmChannels);
  
  strpcmL0=(s16*)safemalloc(strpcmSamples*2);
  strpcmL1=(s16*)safemalloc(strpcmSamples*2);
  strpcmR0=(s16*)safemalloc(strpcmSamples*2);
  strpcmR1=(s16*)safemalloc(strpcmSamples*2);
  
  _consolePrintf("strpcm buf 0x%x,0x%x 0x%x,0x%x\n",strpcmL0,strpcmL1,strpcmR0,strpcmR1);
  
  if((strpcmL0==NULL)||(strpcmL1==NULL)||(strpcmR0==NULL)||(strpcmR1==NULL)){
    a7led(3); while(1);
  }
  
  strpcmLastL=0;
  strpcmLastR=0;
  
//  powerON(POWER_SOUND);
//  SOUND_CR = SOUND_ENABLE | SOUND_VOL(0x7F);
  SCHANNEL_CR(0) = 0;
  SCHANNEL_CR(1) = 0;
  SCHANNEL_CR(2) = 0;
  SCHANNEL_CR(3) = 0;
  
  TIMER0_DATA = SOUND_FREQ(strpcmFreq);
  TIMER0_CR = TIMER_DIV_1 | TIMER_ENABLE;
  
  TIMER1_DATA = 0x10000 - (strpcmSamples*2);
  TIMER1_CR = TIMER_CASCADE | TIMER_IRQ_REQ | TIMER_ENABLE;
  
  for(u32 ch=0;ch<4;ch++){
    SCHANNEL_CR(ch) = 0;
    SCHANNEL_TIMER(ch) = SOUND_FREQ(strpcmFreq);
    SCHANNEL_LENGTH(ch) = (strpcmSamples*2) >> 2;
    SCHANNEL_REPEAT_POINT(ch) = 0;
  }

  agc=0;
    
  IPC6->strpcmWriteRequest=0;
  
  _consolePrintf("strpcm initialized.\n");
}

__attribute__((noinline)) static void strpcmStop()
{
  _consolePrintf("strpcm Stop: strpcm buf 0x%x,0x%x 0x%x,0x%x\n",strpcmL0,strpcmL1,strpcmR0,strpcmR1);
  
//  powerOFF(POWER_SOUND);
//  SOUND_CR = 0;
  TIMER0_CR = 0;
  TIMER1_CR = 0;
  
  for(u32 ch=0;ch<4;ch++){
    SCHANNEL_CR(ch) = 0;
  }
  
  if(strpcmL0!=NULL){
    safefree(strpcmL0); strpcmL0=NULL;
  }
  if(strpcmL1!=NULL){
    safefree(strpcmL1); strpcmL1=NULL;
  }
  if(strpcmR0!=NULL){
    safefree(strpcmR0); strpcmR0=NULL;
  }
  if(strpcmR1!=NULL){
    safefree(strpcmR1); strpcmR1=NULL;
  }
  
  switch(strpcmFormat){
    case SPF_PCMx1: case SPF_PCMx2: case SPF_PCMx4: break;
    case SPF_MP2: FreeMP2(); break;
    default: break;
  }
  strpcmFormat=(EstrpcmFormat)0;
  ARM7_SelfCheck_Check();
  
  SetMemoryMode_End();
  
  IPC6->IR=IR_NULL;
  
  _consolePrintf("strpcm Stopped.\n");
}

//////////////////////////////////////////////////////////////////////

#define MAX( x, y ) ( ( x > y ) ? x : y )
#define MIN( x, y ) ( ( x < y ) ? x : y )

__attribute__((noinline)) static void InterruptHandler_Timer1_SetSwapChannel(void)
{
  s16 *lbuf,*rbuf;
  
  if(strpcmCursorFlag==0){
    lbuf=strpcmL0;
    rbuf=strpcmR0;
    }else{
    lbuf=strpcmL1;
    rbuf=strpcmR1;
  }
  
  u32 channel=strpcmCursorFlag;
  
  u32 chcnt=IPC6->SoundChannels;
  
  if((chcnt==0)||(chcnt==1)){
  // Left channel
  SCHANNEL_CR(channel) = 0;
  SCHANNEL_SOURCE(channel) = (uint32)lbuf;
  SCHANNEL_CR(channel) = SCHANNEL_ENABLE | SOUND_ONE_SHOT | SOUND_VOL(0x7F) | SOUND_PAN(0) | SOUND_16BIT;
  }
  
  channel+=2;
  
  if((chcnt==0)||(chcnt==2)){
  // Right channel
  SCHANNEL_CR(channel) = 0;
  SCHANNEL_SOURCE(channel) = (uint32)rbuf;
  SCHANNEL_CR(channel) = SCHANNEL_ENABLE | SOUND_ONE_SHOT | SOUND_VOL(0x7F) | SOUND_PAN(0x7F) | SOUND_16BIT;
  }
  
  strpcmCursorFlag=1-strpcmCursorFlag;
}

// --------------------------------------------------------

asm void VolNoAGC(s16 *lbuf,s16 *rbuf,u32 count,s32 vol)
{
VolNoAGC_lbuf RN r0
VolNoAGC_rbuf RN r1
VolNoAGC_count RN r2
VolNoAGC_vol RN r3

VolNoAGC_tmpl RN r4
VolNoAGC_tmpr RN lr
VolNoAGC_limmin RN r5
VolNoAGC_limmax RN r6

  PUSH {r4,r5,r6,lr}

  ldr VolNoAGC_limmin,=-32768
  ldr VolNoAGC_limmax,=32767
  
VolNoAGC_loop
  ldrsh VolNoAGC_tmpl,[VolNoAGC_lbuf]
  ldrsh VolNoAGC_tmpr,[VolNoAGC_rbuf]
  mul VolNoAGC_tmpl,VolNoAGC_tmpl,VolNoAGC_vol
  mul VolNoAGC_tmpr,VolNoAGC_tmpr,VolNoAGC_vol
  asr VolNoAGC_tmpl,VolNoAGC_tmpl,#11 ; /2048
  asr VolNoAGC_tmpr,VolNoAGC_tmpr,#11 ; /2048
  cmps VolNoAGC_tmpl,VolNoAGC_limmin
  movlt VolNoAGC_tmpl,VolNoAGC_limmin
  cmps VolNoAGC_tmpl,VolNoAGC_limmax
  movgt VolNoAGC_tmpl,VolNoAGC_limmax
  cmps VolNoAGC_tmpr,VolNoAGC_limmin
  movlt VolNoAGC_tmpr,VolNoAGC_limmin
  cmps VolNoAGC_tmpr,VolNoAGC_limmax
  movgt VolNoAGC_tmpr,VolNoAGC_limmax
  strh VolNoAGC_tmpl,[VolNoAGC_lbuf],#2
  subs VolNoAGC_count,#1
  strh VolNoAGC_tmpr,[VolNoAGC_rbuf],#2
  bne VolNoAGC_loop
  
  POP {r4,r5,r6,pc}
}

asm u32 VolUseAGC(s16 *lbuf,s16 *rbuf,u32 count,s32 vol)
{
VolUseAGC_lbuf RN r0
VolUseAGC_rbuf RN r1
VolUseAGC_count RN r2
VolUseAGC_vol RN r3

VolUseAGC_tmpl RN r4
VolUseAGC_tmpr RN lr
VolUseAGC_limmin RN r5
VolUseAGC_limmax RN r6

VolUseAGC_overflag RN r7

  PUSH {r4,r5,r6,r7,lr}

  ldr VolUseAGC_limmin,=-32768
  ldr VolUseAGC_limmax,=32767
  
  mov VolUseAGC_overflag,#0

VolUseAGC_loop
  ldrsh VolUseAGC_tmpl,[VolUseAGC_lbuf]
  ldrsh VolUseAGC_tmpr,[VolUseAGC_rbuf]
  mul VolUseAGC_tmpl,VolUseAGC_tmpl,VolUseAGC_vol
  mul VolUseAGC_tmpr,VolUseAGC_tmpr,VolUseAGC_vol
  asr VolUseAGC_tmpl,VolUseAGC_tmpl,#11 ; /2048
  asr VolUseAGC_tmpr,VolUseAGC_tmpr,#11 ; /2048
  cmps VolUseAGC_tmpl,VolUseAGC_limmin
  movlt VolUseAGC_tmpl,VolUseAGC_limmin
  movlt VolUseAGC_overflag,#1
  cmps VolUseAGC_tmpl,VolUseAGC_limmax
  movgt VolUseAGC_tmpl,VolUseAGC_limmax
  movgt VolUseAGC_overflag,#1
  cmps VolUseAGC_tmpr,VolUseAGC_limmin
  movlt VolUseAGC_tmpr,VolUseAGC_limmin
  movlt VolUseAGC_overflag,#1
  cmps VolUseAGC_tmpr,VolUseAGC_limmax
  movgt VolUseAGC_tmpr,VolUseAGC_limmax
  movgt VolUseAGC_overflag,#1
  strh VolUseAGC_tmpl,[VolUseAGC_lbuf],#2
  subs VolUseAGC_count,#1
  strh VolUseAGC_tmpr,[VolUseAGC_rbuf],#2
  bne VolUseAGC_loop
  
  mov r0,VolUseAGC_overflag
  
  POP {r4,r5,r6,r7,pc}
}

  static const s32 LogVolumeCount=128;
  static const s32 LogVolume[LogVolumeCount]={
  0, 32, 64, 96,128,160,192,224,256,288,320,352,384,416,448,480,
512,544,576,608,640,672,704,736,768,800,832,864,896,928,960,992,
1024,1056,1088,1120,1152,1184,1216,1248,1280,1312,1344,1376,1408,1440,1472,1504,
1536,1568,1600,1632,1664,1696,1728,1760,1792,1824,1856,1888,1920,1952,1984,2016,
2048,2123,2202,2283,2368,2455,2546,2641,2738,2840,2945,3054,3167,3284,3405,3531,
3662,3798,3938,4084,4235,4392,4554,4723,4898,5079,5267,5462,5664,5873,6091,6316,
6550,6792,7043,7304,7574,7854,8145,8446,8759,9083,9419,9768,10129,10504,10893,11296,
11714,12147,12597,13063,13546,14047,14567,15106,15665,16244,16846,17469,18115,18785,19481,20201,};

__attribute__((noinline)) static void InterruptHandler_Timer1_ApplyVolume(s16 *lbuf,s16 *rbuf,u32 count)
{
  if((lbuf==NULL)||(rbuf==NULL)) return;
  
  s32 vol=(s32)IPC6->strpcmVolume64;
  if((LogVolumeCount-1)<vol) vol=LogVolumeCount-1;
  
  if(vol==0){
    for(u32 idx=0;idx<count;idx++){
      lbuf[idx]=0;
      rbuf[idx]=0;
    }
    return;
  }
  
  u32 MasterVolume=LogVolume[vol];
  
  VolNoAGC(lbuf,rbuf,count,MasterVolume);
  ARM7_SelfCheck_Check();
}

__attribute__((noinline)) static void InterruptHandler_Timer1_ApplyVolumeWithAGC(s16 *lbuf,s16 *rbuf,u32 count)
{
  if((lbuf==NULL)||(rbuf==NULL)) return;
  
  s32 vol=(s32)IPC6->strpcmVolume64;
  if((LogVolumeCount-1)<vol) vol=LogVolumeCount-1;
  
  if(vol==0){
    for(u32 idx=0;idx<count;idx++){
      lbuf[idx]=0;
      rbuf[idx]=0;
    }
    return;
  }
  
  u32 MasterVolume=LogVolume[vol];
  
  u32 divcnt=8;
  u32 divsize=count/divcnt;
  
  for(u32 idx=0;idx<divcnt;idx++){
    u32 vol=MasterVolume+agc;
    if(VolUseAGC(lbuf,rbuf,divsize,vol)==0){
      agc+=8;
      if(1024<agc) agc=1024;
      }else{
      agc-=16;
      if(agc<-1024) agc=-1024;
    }
    lbuf+=divsize;
    rbuf+=divsize;
  }
  
  ARM7_SelfCheck_Check();
  
}

// --------------------------------------------------------------------

static void InterruptHandler_Timer1_Null(void)
{
}

static void InterruptHandler_Timer1_PCMx1(void)
{
  InterruptHandler_Timer1_SetSwapChannel();
  
  s16 *lbuf,*rbuf;
  
  if(strpcmCursorFlag==0){
    lbuf=strpcmL0;
    rbuf=strpcmR0;
    }else{
    lbuf=strpcmL1;
    rbuf=strpcmR1;
  }
  
  s16 *lsrc=IPC6->strpcmLBuf;
  s16 *rsrc=IPC6->strpcmRBuf;
  
  u32 Samples=strpcmSamples;
  
  if(IPC6->strpcmWriteRequest!=0){
    MemSet16DMA3(0,lbuf,Samples*2);
    MemSet16DMA3(0,rbuf,Samples*2);
    }else{
    if(strpcmChannels==2){
      MemCopy16DMA3(lsrc,lbuf,Samples*2);
      MemCopy16DMA3(rsrc,rbuf,Samples*2);
      }else{
      MemCopy16DMA3(lsrc,lbuf,Samples*2);
      MemCopy16DMA3(lsrc,rbuf,Samples*2);
    }
    
    IPC6->IR=IR_NextSoundData;
    REG_IPC_SYNC|=IPC_SYNC_IRQ_REQUEST;
    IPC6->strpcmWriteRequest=1;
    
    InterruptHandler_Timer1_ApplyVolume(lbuf,rbuf,Samples);
  }
  
  CallBackIRQ_strpcmUpdate();
}

static void InterruptHandler_Timer1_PCMx2(void)
{
  InterruptHandler_Timer1_SetSwapChannel();
  
  s16 *lbuf,*rbuf;
  
  if(strpcmCursorFlag==0){
    lbuf=strpcmL0;
    rbuf=strpcmR0;
    }else{
    lbuf=strpcmL1;
    rbuf=strpcmR1;
  }
  
  s16 *lsrc=IPC6->strpcmLBuf;
  s16 *rsrc=IPC6->strpcmRBuf;
  
  u32 Samples=strpcmSamples;
  
  if(IPC6->strpcmWriteRequest!=0){
    MemSet16DMA3(0,lbuf,Samples*2);
    MemSet16DMA3(0,rbuf,Samples*2);
    }else{
    if(strpcmChannels==2){
      s32 l=strpcmLastL,r=strpcmLastR;
      s16 *plsrc=lsrc,*prsrc=rsrc;
      s16 *plbuf=lbuf,*prbuf=rbuf;
      u32 LastSamples=Samples;
      s32 sl,sr;
      const u32 mask=0xffff;
      asm{
        InterruptHandler_Timer1_PCMx2_2ch_OverSampling_Loop:
        ldrsh sl,[plsrc],#2
        ldrsh sr,[prsrc],#2
        
        add l,l,sl,asr #1
        and l,l,mask
        orr l,l,sl,lsl #16
        str l,[plbuf],#4
        mov l,sl,asr #1
        
        add r,r,sr,asr #1
        and r,r,mask
        orr r,r,sr,lsl #16
        str r,[prbuf],#4
        mov r,sr,asr #1
        
        subs LastSamples,LastSamples,#2
        bne InterruptHandler_Timer1_PCMx2_2ch_OverSampling_Loop
      }
      strpcmLastL=l; strpcmLastR=r;
      }else{
      s32 l=strpcmLastL;
      s16 *plsrc=lsrc;
      s16 *plbuf=lbuf,*prbuf=rbuf;
      u32 LastSamples=Samples;
      s32 sl;
      const u32 mask=0xffff;
      asm{
        InterruptHandler_Timer1_PCMx2_1ch_OverSampling_Loop:
        ldrsh sl,[plsrc],#2
        
        add l,l,sl,asr #1
        and l,l,mask
        orr l,l,sl,lsl #16
        str l,[plbuf],#4
        str l,[prbuf],#4
        mov l,sl,asr #1
        
        subs LastSamples,LastSamples,#2
        bne InterruptHandler_Timer1_PCMx2_1ch_OverSampling_Loop
      }
      strpcmLastL=l;
    }
    
    IPC6->IR=IR_NextSoundData;
    REG_IPC_SYNC|=IPC_SYNC_IRQ_REQUEST;
    IPC6->strpcmWriteRequest=1;
    
    InterruptHandler_Timer1_ApplyVolume(lbuf,rbuf,Samples);
  }
  
  CallBackIRQ_strpcmUpdate();
}

static void InterruptHandler_Timer1_PCMx4(void)
{
  InterruptHandler_Timer1_SetSwapChannel();
  
  s16 *lbuf,*rbuf;
  
  if(strpcmCursorFlag==0){
    lbuf=strpcmL0;
    rbuf=strpcmR0;
    }else{
    lbuf=strpcmL1;
    rbuf=strpcmR1;
  }
  
  s16 *lsrc=IPC6->strpcmLBuf;
  s16 *rsrc=IPC6->strpcmRBuf;
  
  u32 Samples=strpcmSamples;
  
  if(IPC6->strpcmWriteRequest!=0){
    MemSet16DMA3(0,lbuf,Samples*2);
    MemSet16DMA3(0,rbuf,Samples*2);
    }else{
    if(strpcmChannels==2){
      s32 l=strpcmLastL,r=strpcmLastR;
      s16 *plsrc=lsrc,*prsrc=rsrc;
      s16 *plbuf=lbuf,*prbuf=rbuf;
      u32 LastSamples=Samples;
      s32 sl,sr,tmp;
      const u32 mask=0xffff;
      asm{
        InterruptHandler_Timer1_PCMx4_2ch_OverSampling_Loop:
        ldrsh sl,[plsrc],#2
        ldrsh sr,[prsrc],#2
        
        strh l,[plbuf],#2
        add tmp,l,l
        add tmp,tmp,l
        add tmp,tmp,sl
        mov tmp,tmp,asr #2
        strh tmp,[plbuf],#2
        add tmp,l,sl
        mov tmp,tmp,asr #1
        strh tmp,[plbuf],#2
        add tmp,sl,sl
        add tmp,tmp,sl
        add tmp,tmp,l
        mov tmp,tmp,asr #2
        strh tmp,[plbuf],#2
        mov l,sl
        
        strh r,[prbuf],#2
        add tmp,r,r
        add tmp,tmp,r
        add tmp,tmp,sr
        mov tmp,tmp,asr #2
        strh tmp,[prbuf],#2
        add tmp,r,sr
        mov tmp,tmp,asr #1
        strh tmp,[prbuf],#2
        add tmp,sr,sr
        add tmp,tmp,sr
        add tmp,tmp,r
        mov tmp,tmp,asr #2
        strh tmp,[prbuf],#2
        mov r,sr
        
        subs LastSamples,LastSamples,#4
        bne InterruptHandler_Timer1_PCMx4_2ch_OverSampling_Loop
      }
      strpcmLastL=l; strpcmLastR=r;
      }else{
      s32 l=strpcmLastL;
      s16 *plsrc=lsrc;
      s16 *plbuf=lbuf,*prbuf=rbuf;
      u32 LastSamples=Samples;
      s32 sl,tmp;
      const u32 mask=0xffff;
      asm{
        InterruptHandler_Timer1_PCMx4_1ch_OverSampling_Loop:
        ldrsh sl,[plsrc],#2
        
        strh l,[plbuf],#2
        add tmp,l,l
        add tmp,tmp,l
        add tmp,tmp,sl
        mov tmp,tmp,asr #2
        strh tmp,[plbuf],#2
        add tmp,l,sl
        mov tmp,tmp,asr #1
        strh tmp,[plbuf],#2
        add tmp,sl,sl
        add tmp,tmp,sl
        add tmp,tmp,l
        mov tmp,tmp,asr #2
        strh tmp,[plbuf],#2
        mov l,sl
        
        subs LastSamples,LastSamples,#4
        bne InterruptHandler_Timer1_PCMx4_1ch_OverSampling_Loop
      }
      strpcmLastL=l;
    }
    
    IPC6->IR=IR_NextSoundData;
    REG_IPC_SYNC|=IPC_SYNC_IRQ_REQUEST;
    IPC6->strpcmWriteRequest=1;
    
    InterruptHandler_Timer1_ApplyVolume(lbuf,rbuf,Samples);
  }
  
  CallBackIRQ_strpcmUpdate();
}

static void InterruptHandler_Timer1_MP2(void)
{
  InterruptHandler_Timer1_SetSwapChannel();
  
  s16 *lbuf,*rbuf;
  
  if(strpcmCursorFlag==0){
    lbuf=strpcmL0;
    rbuf=strpcmR0;
    }else{
    lbuf=strpcmL1;
    rbuf=strpcmR1;
  }
  
  u32 Samples=strpcmSamples;
  
  if((IPC6->strpcmWriteRequest!=0)||(IPC6->MP2PauseFlag==true)){
    MemSet16DMA3(0,lbuf,Samples*2);
    MemSet16DMA3(0,rbuf,Samples*2);
    }else{
    IPC6->strpcmWriteRequest=1;
    
    if(IPC6->IR_flash==true){
      IPC6->IR=IR_Flash;
      REG_IPC_SYNC|=IPC_SYNC_IRQ_REQUEST;
//      FlashFileBufferMP2();
      while(IPC6->IR!=IR_NULL) swiDelay(1);
      IPC6->IR_flash=false;
      }else{
      IPC6->IR_SyncSamples_SendToARM9+=(u64)Samples;
    }
    
    while(IPC6->IR!=IR_NULL) swiDelay(1);
    IPC6->IR=IR_SyncSamples;
    REG_IPC_SYNC|=IPC_SYNC_IRQ_REQUEST;
    
    s16 *ldst=lbuf,*rdst=rbuf;
    
    int OutputSamples=UpdateMP2(ldst,rdst);
    ARM7_SelfCheck_Check();
    
    if(OutputSamples==0){
      for(u32 cnt=0;cnt<Samples;cnt++){
        *lbuf++=0;
        *rbuf++=0;
      }
      }else{
      InterruptHandler_Timer1_ApplyVolumeWithAGC(lbuf,rbuf,Samples);
    }
    
    IPC6->strpcmWriteRequest=0;
  }
  
  CallBackIRQ_strpcmUpdate();
}

//////////////////////////////////////////////////////////////////////

static u32 CSE_Volume;
static u32 CSE_Index,CSE_Count;
static u8 CSE_Data[5*60]; // max 5 secs.

static void StartClickSE(volatile TClickSE *pClickSE)
{
  CSE_Volume=pClickSE->Volume;
  
  CSE_Index=0;
  CSE_Count=0;
  
  for(u32 idx=0;idx<pClickSE->Count;idx++){
    CSE_Data[CSE_Count++]=1;
    for(u32 w=0;w<4;w++){
      CSE_Data[CSE_Count++]=0;
    }
    CSE_Count++;
  }
  
  if(pClickSE->AddLong==true) CSE_Data[CSE_Count++]=2;
  
  pClickSE->Apply=false;
}

//////////////////////////////////////////////////////////////////////

static bool VsyncPassed;

#include "resources/snd_click_short_c_bin.h"
#include "resources/snd_click_long_c_bin.h"

static void InterruptHandler_VBlank(void)
{
  ARM7_SelfCheck_Check();
  
  IPC6->heartbeat++;
  
  VsyncPassed=true;
  
  if(CSE_Index==CSE_Count) return;
  
  u32 data=CSE_Data[CSE_Index++];
  u32 vol=0,addr=0,size=0;
  switch(data){
    case 0: break; // wait for vblank
    case 1: vol=127; addr=(u32)snd_click_short_c_bin; size=snd_click_short_c_bin_Size; break;
    case 2: vol=96; addr=(u32)snd_click_long_c_bin; size=snd_click_long_c_bin_Size; break;
  }
  
  vol=(vol*CSE_Volume)/256;
  
  if((vol!=0)&&(addr!=0)&&(size!=0)){
    u32 ch=14;
    SCHANNEL_CR(ch)=0;
    SCHANNEL_TIMER(ch)  = SOUND_FREQ(32768);
    SCHANNEL_SOURCE(ch) = addr;
    SCHANNEL_LENGTH(ch) = size >> 2;
    SCHANNEL_REPEAT_POINT(ch) = 0;
    SCHANNEL_CR(ch)     = SCHANNEL_ENABLE | SOUND_ONE_SHOT | SOUND_VOL(vol) | SOUND_PAN(64) | SOUND_8BIT;
  }
}

//////////////////////////////////////////////////////////////////////

//---------------------------------------------------------------------------------
void readUserSettings() { return;
//---------------------------------------------------------------------------------

	PERSONAL_DATA slot1;
	PERSONAL_DATA slot2;

	short slot1count, slot2count;
	short slot1CRC, slot2CRC;

	uint32 userSettingsBase;
	readFirmware( 0x20, &userSettingsBase,2);
	
	uint32 slot1Address = userSettingsBase * 8;
	uint32 slot2Address = userSettingsBase * 8 + 0x100;
	
	readFirmware( slot1Address , &slot1, sizeof(PERSONAL_DATA));
	readFirmware( slot2Address , &slot2, sizeof(PERSONAL_DATA));
	readFirmware( slot1Address + 0x70, &slot1count, 2);
	readFirmware( slot2Address + 0x70, &slot2count, 2);
	readFirmware( slot1Address + 0x72, &slot1CRC, 2);
	readFirmware( slot2Address + 0x72, &slot2CRC, 2);

	short calc1CRC = swiCRC16( 0xffff, &slot1, sizeof(PERSONAL_DATA));
	short calc2CRC = swiCRC16( 0xffff, &slot2, sizeof(PERSONAL_DATA));

	// bail out if neither slot is valid
	if ( calc1CRC != slot1CRC && calc2CRC != slot2CRC) return;
	
	void *currentSettings=NULL;
	
	// if both slots are valid pick the most recent
	if ( (calc1CRC == slot1CRC) && (calc2CRC == slot2CRC) ) {
	  if(currentSettings==NULL){
	    // It doesn't test.
	    if((slot1count & 0x7f) == 0x00) currentSettings = &slot1;
	    if((slot2count & 0x7f) == 0x00) currentSettings = &slot2;
	  }
	  if(currentSettings==NULL){
	    if(slot2count<slot1count){
	      currentSettings = &slot1;
	      }else{
	      currentSettings = &slot2;
	    }
	  }
	}
	
	if(currentSettings==NULL){
	  if ( calc1CRC == slot1CRC ) currentSettings = &slot1;
	  if ( calc2CRC == slot2CRC ) currentSettings = &slot2;
	}
	
	memcpy ( PersonalData, currentSettings, sizeof(PERSONAL_DATA));
}

static inline void main_InitIRQ(void)
{
  REG_IME = 0;
  irqInit();
  irqSet_u32(IRQ_TIMER1,(u32)InterruptHandler_Timer1_Null);
  irqSet_u32(IRQ_VBLANK,(u32)InterruptHandler_VBlank);
  REG_IME = 1;
}

static inline void main_InitVsync(void)
{
  VsyncPassed=false;
}

#define PM_NDSLITE_ADR (4)
#define PM_NDSLITE_ISLITE BIT(6)
#define PM_NDSLITE_ExternalPowerPresent BIT(3)
#define PM_NDSLITE_BRIGHTNESS(x) ((x & 0x03)<<0)
#define PM_NDSLITE_BRIGHTNESS_MASK (PM_NDSLITE_BRIGHTNESS(3))

static inline void main_InitNDSL(void)
{
  IPC6->isNDSLite = ( (PM_GetRegister(PM_NDSLITE_ADR) & PM_NDSLITE_ISLITE) != 0) ? true : false;
  if(IPC6->isNDSLite==false){
    IPC6->DefaultBrightness=0;
    }else{
    u8 data;
    data=PM_GetRegister(PM_NDSLITE_ADR);
    data&=PM_NDSLITE_BRIGHTNESS_MASK;
    IPC6->DefaultBrightness=data;
  }
  IPC6->Brightness=0xff;
}

static inline void main_InitSoundDevice(void)
{
  powerON(POWER_SOUND);
  SOUND_CR = SOUND_ENABLE | SOUND_VOL(0x7F);
  
  REG_POWERCNT&=~POWER_UNKNOWN; // wifi power off
  
  swiChangeSoundBias(1,0x400);
  a7SetSoundAmplifier(true);
}

__attribute__((noinline)) static void main_InitAll(void)
{
  IPC6->heartbeat=0;
  
  REG_IME=0;
  REG_IE=0;
  
  // Clear DMA
  for(int i=0;i<0x30/4;i++){
    *((vu32*)(0x40000B0+i))=0;
  }
  
  // Reset the clock if needed
  rtcReset();
  
  IPC6->RequestUpdateIPC=true;
  IPC6->curtimeFlag=true;
  
  IPC6->RESET=RESET_NULL;
  
  IPC6->buttons=0;
  IPC6->PanelOpened=true;
  
  IPC6->LCDPowerControl=LCDPC_NOP;
  
  IPC6->RequestStopSound=false;
  
  IPC6->strpcmControl=ESC_NOP;
  
  IPC6->IR=IR_NULL;
  
  IPC6->ClickSE.Apply=false;
  
  REG_SPICNT = SPI_ENABLE|SPI_CONTINUOUS|SPI_DEVICE_NVRAM;
  readUserSettings();
  REG_SPICNT = 0;
  
  IPC6->UserLanguage=PersonalData->_user_data.language;
  IPC6->BirthMonth=PersonalData->birthMonth;
  IPC6->BirthDay=PersonalData->birthDay;
  
  main_InitNDSL();
  
  main_InitVsync();
  
  main_InitIRQ();
  
  main_InitSoundDevice();
}

static bool strpcmPlayFlag;

__attribute__((noinline)) static void main_Proc_strpcmControl(void)
{
  _consolePrintf("main_Proc_strpcmControl();\nIPC6->strpcmControl=%d.\n",IPC6->strpcmControl);
  switch(IPC6->strpcmControl){
    case ESC_NOP: {
    } break;
    case ESC_Play: {
      if((EnableFPGA==true)&&(IPC6->strpcmFormat!=SPF_MP2)){
        FPGA_strpcmPlay();
        irqSet_u32(IRQ_TIMER1,(u32)FPGA_InterruptHandler_Timer1);
        }else{
        strpcmPlay();
        switch(strpcmFormat){
          case SPF_PCMx1: irqSet_u32(IRQ_TIMER1,(u32)InterruptHandler_Timer1_PCMx1); break;
          case SPF_PCMx2: irqSet_u32(IRQ_TIMER1,(u32)InterruptHandler_Timer1_PCMx2); break;
          case SPF_PCMx4: irqSet_u32(IRQ_TIMER1,(u32)InterruptHandler_Timer1_PCMx4); break;
          case SPF_MP2: irqSet_u32(IRQ_TIMER1,(u32)InterruptHandler_Timer1_MP2); break;
          default: irqSet_u32(IRQ_TIMER1,(u32)InterruptHandler_Timer1_Null); break;
        }
      }
      strpcmPlayFlag=true;
    } break;
    case ESC_Stop: {
      strpcmPlayFlag=false;
      if((EnableFPGA==true)&&(IPC6->strpcmFormat!=SPF_MP2)){
        FPGA_strpcmStop();
        }else{
        strpcmStop();
      }
      irqSet_u32(IRQ_TIMER1,(u32)InterruptHandler_Timer1_Null);
    } break;
    default: {
      REG_IME=0;
      _consolePrintf("Unknown strpcmControl(%d).\n",IPC6->strpcmControl);
      ShowLogHalt();
    } break;
  }
  IPC6->strpcmControl=ESC_NOP;
  ARM7_SelfCheck_Check();
}

__attribute__((noinline)) static void main_Proc_Brightness(u32 Brightness)
{
  u8 data;
  
  data=PM_GetRegister(PM_NDSLITE_ADR);
  data&=~PM_NDSLITE_BRIGHTNESS_MASK;
  data|=PM_NDSLITE_BRIGHTNESS(Brightness);
  
  PM_SetRegister(PM_NDSLITE_ADR,data);
  
  _consolePrintf("Set brightness=%d\n",Brightness);
}

__attribute__((noinline)) static void main_Proc_LCDPowerControl(ELCDPC LCDPC)
{
  switch(LCDPC){
    case LCDPC_OFF_BOTH: {
      a7led(3);
      a7lcd_select(0);
    } break;
    case LCDPC_ON_BOTTOM: {
      a7led(0);
      a7lcd_select(PM_BACKLIGHT_BOTTOM);
    } break;
    case LCDPC_ON_TOP: {
      a7led(0);
      a7lcd_select(PM_BACKLIGHT_TOP);
    } break;
    case LCDPC_ON_BOTH: {
      a7led(0);
      a7lcd_select(PM_BACKLIGHT_BOTTOM | PM_BACKLIGHT_TOP);
    } break;
    case LCDPC_SOFT_POWEROFF: {
      a7poff();
      while(1);
    }
    default: ShowLogHalt(); break; // this execute is bug.
  }
  ARM7_SelfCheck_Check();
}

#include "main_boot_gbarom.h"

__attribute__((noinline)) static void main_Proc_Reset(ERESET RESET)
{
  reboot(RESET);
  while(1);
}

static void updateipc(void)
{
  IPC6->buttons = (~REG_KEYXY) & 0x7f;
  IPC6->buttons &= ~IPC_LID_CLOSED;
  
  if((REG_KEYXY & IPC_LID_CLOSED)!=0){
    IPC6->PanelOpened=false;
    }else{
    IPC6->PanelOpened=true;
  }
  
  IPC6->touchXpx = (u32)-1;
  IPC6->touchYpx = (u32)-1;
  
  if((IPC6->buttons & IPC_PEN_DOWN)!=0){
    touchPosition touchPos=touchReadXY();
    
    int px=(int)touchPos.px;
    int py=(int)touchPos.py;
    
    if((px<0)||(256<px)||(py<0)||(192<py)){
      IPC6->buttons&=~IPC_PEN_DOWN;
      }else{
      IPC6->touchXpx = px;
      IPC6->touchYpx = py;
    }
  }
  
  IPC6->Romeo2_HPSwitch_Pressing=FPGA_GetHPSwitch();
  
  IPC6->ExternalPowerPresent=( (PM_GetRegister(PM_NDSLITE_ADR) & PM_NDSLITE_ExternalPowerPresent) != 0) ? true : false;
  
  ARM7_SelfCheck_Check();
}

void CallBackIRQ_strpcmUpdate(void)
{
  if(IPC6->RequestUpdateIPC==true){
    updateipc();
    IPC6->RequestUpdateIPC=false;
    ARM7_SelfCheck_Check();
  }
  if(IPC6->curtimeFlag==true){
    rtcGetTimeAndDate((uint8 *)&(IPC6->time.rtc.year));
    IPC6->curtimeFlag=false;
    ARM7_SelfCheck_Check();
  }
}

static ELCDPC LCDPC_LastState;

#include "main_VarDebug.h"

int main(int argc, char ** argv)
{
//  main_VarDebug();
  
  IPC6->UserLanguage=(u32)-1;
  
  IPC6->RequestFPGAInit=false;
  IPC6->Romeo2_HPSwitch_Pressing=false;
  
  ARM7_SelfCheck_Init();
  
  main_InitAll();
  
  VsyncPassed=false;
  
  PrintFreeMem();
  
  strpcmPlayFlag=false;
  
  LCDPC_LastState=LCDPC_ON_BOTH;
  
  // Keep the ARM7 out of main RAM
  while (1){
    ARM7_SelfCheck_Check();
    while(VsyncPassed==false){
      swiWaitForVBlank();
      ARM7_SelfCheck_Check();
    }
    VsyncPassed=false;
    
    if(strpcmPlayFlag==false){
      REG_IME=0;
      CallBackIRQ_strpcmUpdate();
      REG_IME=1;
    }
    
    if(IPC6->strpcmControl!=ESC_NOP){
      REG_IME=0;
      main_Proc_strpcmControl();
      REG_IME=1;
    }
    
    if(IPC6->Brightness!=0xff){
      REG_IME=0;
      if(IPC6->isNDSLite==true) main_Proc_Brightness(IPC6->Brightness);
      IPC6->Brightness=0xff;
      REG_IME=1;
    }
    
    if(IPC6->RESET!=RESET_NULL){
      REG_IME=0;
      main_Proc_Reset(IPC6->RESET);
      while(1);
    }
    
    bool pcapply=false;
    
    if(IPC6->LCDPowerControl!=LCDPC_NOP){
      REG_IME=0;
      LCDPC_LastState=IPC6->LCDPowerControl;
      IPC6->LCDPowerControl=LCDPC_NOP;
      pcapply=true;
      REG_IME=1;
    }
    
    static u32 LastLID=IPC_LID_CLOSED;
    
    {
      u32 LID=REG_KEYXY & IPC_LID_CLOSED;
      if(LastLID!=LID){
        LastLID=LID;
        pcapply=true;
      }
    }
    
    if(pcapply==true){
      REG_IME=0;
      if(LastLID==IPC_LID_CLOSED){
        main_Proc_LCDPowerControl(LCDPC_OFF_BOTH);
        if(LCDPC_LastState==LCDPC_SOFT_POWEROFF){
          main_Proc_LCDPowerControl(LCDPC_LastState);
          while(1);
        }
        }else{
        main_Proc_LCDPowerControl(LCDPC_LastState);
      }
      REG_IME=1;
    }
    
    //sound code  :)
    TransferSound *snd = IPC->soundData;
    if ((0 != snd)&&(snd->count!=0)) {
      REG_IME=0;
      IPC->soundData = 0;
      u32 i;
      for (i=0; i<snd->count; i++) {
        s32 chan = getFreeSoundChannel();
        if(chan>=0) startSound(snd->data[i].rate, snd->data[i].data, snd->data[i].len, chan, snd->data[i].vol, snd->data[i].pan, snd->data[i].format);
      }
      REG_IME=1;
    }
    
    if(IPC6->RequestStopSound==true){
      REG_IME=0;
      stopSound();
      IPC6->RequestStopSound=false;
      REG_IME=1;
    }
    
    if(IPC6->RequestFPGAInit==true){
      FPGA_main_ins();
      IPC6->RequestFPGAInit=false;
    }
    
    if(IPC6->ClickSE.Apply==true){
      REG_IME=0;
      StartClickSE(&IPC6->ClickSE);
      REG_IME=1;
    }
  }
  
  return 0;
}

