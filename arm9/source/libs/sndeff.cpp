
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <NDS.h>

#include "_console.h"
#include "_const.h"
#include "memtool.h"
#include "shell.h"

#include "sndeff.h"

#include "procstate.h"

#include "../../ipc6.h"

static TransferSound staticTransferSound;

static u32 MaxBufSize;

typedef struct {
  u32 Freq;
  u32 BufCount;
  u32 Channels;
  s8 *lbuf,*rbuf;
} TSound;

static TSound Sound;

typedef struct {
  u16 wFormatTag;
  u16 nChannels;
  u32 nSamplesPerSec;
  u32 nAvgBytesPerSec;
  u16 nBlockAlign;
  u16 wBitsPerSample;
  u16 cbSize;
} WAVEFORMATEX;

static u8 *pfilebuf;
static u32 filepos;

static void file_setbuf(u8 *pbuf)
{
  pfilebuf=pbuf;
  filepos=0;
}

static void file_free(void)
{
  pfilebuf=NULL;
  filepos=0;
}

static u32 file_readbuf(void *pdst,u32 size)
{
  u8 *pdst8=(u8*)pdst;
  
  for(u32 idx=0;idx<size;idx++){
    pdst8[idx]=pfilebuf[filepos+idx];
  }
  filepos+=size;
  
  return(size);
}

static void* file_getcurptr(void)
{
  return(&pfilebuf[filepos]);
}

extern "C" {
  void TTB0_Decode_8bit_asm(u8 *pCodeBuf,s8 *buf,u32 DecompressedSamplesCount);
}

#define TTB0ID (0x30425454)

static void LoadTTB0File(TSound *pSnd,u8 *pbuf)
{
  file_setbuf(pbuf);
  
  u32 tmp32;
  file_readbuf(&tmp32,4);
  if(tmp32!=TTB0ID){
    _consolePrintf("LoadTTB0File: Header error. %08x!=%08x\n",tmp32,TTB0ID);
    ShowLogHalt();
  }
  
  u32 Channels;
  u32 SampleRate;
  u32 CodeBufSize;
  u8 *pCodeBuf;
  u32 SourceSamplesCount;
  s8 *pSourceSamples;
  
  file_readbuf(&Channels,4);
  file_readbuf(&SampleRate,4);
  file_readbuf(&CodeBufSize,4);
  file_readbuf(&SourceSamplesCount,4);
  
  pCodeBuf=(u8*)file_getcurptr();
//  file_readbuf(pCodeBuf,CodeBufSize);
  
  pSourceSamples=(s8*)safemalloc(SourceSamplesCount);
  
  if(pSourceSamples==NULL){
    _consolePrintf("can not allocate sound buffer. (%dbyte)\n",SourceSamplesCount);
    file_free();
    ShowLogHalt();
    return;
  }
  
  if(MaxBufSize<(SourceSamplesCount/pSnd->Channels)){
    _consolePrintf("Fatal error: Sound data is too large! %d,%d\n",MaxBufSize,SourceSamplesCount/pSnd->Channels);
    file_free();
    ShowLogHalt();
  }
  
  TTB0_Decode_8bit_asm(pCodeBuf,pSourceSamples,SourceSamplesCount);
  
  pSnd->Freq=SampleRate;
  pSnd->Channels=Channels;
  pSnd->BufCount=SourceSamplesCount/1/pSnd->Channels; // 8bit 1/2ch only.
  
  if(pSnd->Channels==1){
    for(u32 idx=0;idx<pSnd->BufCount;idx++){
      pSnd->lbuf[idx]=pSourceSamples[idx];
      pSnd->rbuf[idx]=0;
    }
    }else{
    for(u32 idx=0;idx<pSnd->BufCount;idx++){
      pSnd->lbuf[idx]=pSourceSamples[idx*2+0];
      pSnd->rbuf[idx]=pSourceSamples[idx*2+1];
    }
  }
  
  if(pSourceSamples!=NULL){
    safefree(pSourceSamples); pSourceSamples=NULL;
  }
  
  file_free();
}

static void playSoundBlock(TransferSound *snd)
{
  DCache_CleanRangeOverrun(snd, sizeof(TransferSound) );
  IPC->soundData = snd;
}

void Sound_Init(void)
{
  MaxBufSize=0;
  
  {
    u32 *pbuf;
    s32 size;
    Shell_FAT_ReadSEAlloc(SNDEFFDATFilename,(void**)&pbuf,&size);
    if(size!=4){
      _consolePrintf("Sound_Init: sndeff.dat not found or internal error.\n");
      ShowLogHalt();
    }
    if(pbuf!=NULL){
      MaxBufSize=*pbuf;
      safefree(pbuf); pbuf=NULL;
    }
  }
  
  Sound.Freq=0;
  Sound.BufCount=0;
  Sound.Channels=0;
  Sound.lbuf=(s8*)safemalloc(MaxBufSize);
  Sound.rbuf=(s8*)safemalloc(MaxBufSize);
  
  if((Sound.lbuf==NULL)||(Sound.rbuf==NULL)){
    _consolePrintf("can not allocate sound buffer. (%dbyte)\n",MaxBufSize*2);
    ShowLogHalt();
  }
}

static char lastfn[32];

static void SetupSoundBlock(volatile TransferSound *ptsnd,TSound *pSound,u32 Volume)
{
  volatile TransferSoundData *ptsnddata=ptsnd->data;
  
  s8 *lbuf,*rbuf;
  
  switch(pSound->Channels){
    case 1: {
      lbuf=pSound->lbuf;
      rbuf=pSound->lbuf;
    } break;
    case 2: {
      lbuf=pSound->lbuf;
      rbuf=pSound->rbuf;
      Volume/=2;
    } break;
    default: {
      _consolePrintf("Fatal error: sound effect illigal channels count.\n");
      ShowLogHalt();
    } break;
  }
  
  if(127<Volume) Volume=127;
  
  ptsnddata[0].rate=pSound->Freq;
  ptsnddata[0].data=lbuf;
  ptsnddata[0].len=pSound->BufCount*1;
  ptsnddata[0].vol=Volume;
  ptsnddata[0].pan=0;
  ptsnddata[0].format=1;
  
  ptsnddata[1].rate=pSound->Freq;
  ptsnddata[1].data=rbuf;
  ptsnddata[1].len=pSound->BufCount*1;
  ptsnddata[1].vol=Volume;
  ptsnddata[1].pan=127;
  ptsnddata[1].format=1;
  
  ptsnd->count=2;
}

void Sound_Start(const char *wavfn)
{
  if(ProcState.System.ClickSound==false) return;
  
  if(strcmp(lastfn,wavfn)!=0){
    Sound_Stop();
    Sound_Free();
    
    u8 *pbuf=NULL;
    s32 bufsize=0;
    
    if(Shell_FAT_ReadSEAlloc(wavfn,(void**)&pbuf,&bufsize)==false) return;
    LoadTTB0File(&Sound,pbuf);
    safefree(pbuf); pbuf=NULL;
    
    strcpy(lastfn,wavfn);
  }
  
  IPC6->LoopSound=false;
  
  SetupSoundBlock(&staticTransferSound,&Sound,128/1);
  playSoundBlock(&staticTransferSound);
}

bool Sound_isEqualLoadedFilename(const char *wavfn)
{
  if(strcmp(lastfn,wavfn)!=0){
    return(false);
    }else{
    return(true);
  }
}

void Sound_Stop(void)
{
  IPC6->RequestStopSound=true;
  while(IPC6->RequestStopSound==true){
    swiWaitForVBlank();
  }
}

void Sound_Free(void)
{
  lastfn[0]=0;
  
  Sound.Freq=0;
  Sound.BufCount=0;
  Sound.Channels=0;
}

u32 Sound_GetCurrentPlayTimePerVsync(void)
{
  u32 freq=Sound.Freq;
  u32 samples=Sound.BufCount*1;
  
  return(samples*60/freq);
}

