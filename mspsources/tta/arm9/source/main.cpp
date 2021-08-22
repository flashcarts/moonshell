
#include <stdio.h>

#include <NDS.h>

#include "_const.h"

#include "plugin.h"
#include "plugin_def.h"

// CallBack from plugin_dll.cpp

void cbLoadLibrary(void)
{
}

void cbFreeLibrary(void)
{
}

void cbQueryInterfaceLibrary(void)
{
}

extern "C" {
void end(void)
{
}
}

// -----------------

//#include "ttalib/TTALib.h"
#include "ttalib/TTAReader.h"
#include "ttalib/TTAError.h"

#define TTATitle "ttalib-1.1 - TTA decoder library"

static int FileHandle;
static s32 FileSize;

#define SamplePerFrame (256)

static u32 Channels;
static u32 SampleRate;

static u32 DecBufPos,DecBufCnt;
#define DecBufMaxCount (SamplePerFrame*2)
static s16 DecBufL[DecBufMaxCount],DecBufR[DecBufMaxCount];

// ------------------------------------------------------------------------------------

void Free(void);

void Render(void)
{
  u32 lastcnt=DecBufCnt-DecBufPos;
  
  while(lastcnt<SamplePerFrame){
//    _consolePrintf("Render: lastcnt=%d DecBufCnt=%d, DecBufPos=%d\n",lastcnt,DecBufCnt,DecBufPos);
    
    if((DecBufPos!=0)&&(lastcnt!=0)){
      MemCopy16CPU(&DecBufL[DecBufPos],&DecBufL[0],lastcnt*2);
      MemCopy16CPU(&DecBufR[DecBufPos],&DecBufR[0],lastcnt*2);
    }
    DecBufCnt-=DecBufPos;
    DecBufPos=0;
    
    u32 decsize=TTAReader_GetBlock(&DecBufL[DecBufCnt],&DecBufR[DecBufCnt],DecBufMaxCount-DecBufCnt);
    if(decsize==0) return;
    DecBufCnt+=decsize;
    
    lastcnt=DecBufCnt-DecBufPos;
  }
}

bool Start(int _FileHandle)
{
  FileHandle=_FileHandle;
  
  fseek(FileHandle,0,SEEK_END);
  FileSize=ftell(FileHandle);
  fseek(FileHandle,0,SEEK_SET);
  
  _consolePrintf("ttalib init.\n");
  
  TTAReader_Create(FileHandle);
  
  if(2<TTAReader.byte_size){
    _consolePrintf("not support %dbit/sample pcm data\n",TTAReader.ttahdr.BitsPerSample);
    Free();
    return(false);
  }
  
  if(TTAReader.is_float==true){
    _consolePrintf("not support float pcm data\n");
    Free();
    return(false);
  }
  
  if(2<TTAReader.num_chan){
    _consolePrintf("not support %dchannels\n",TTAReader.num_chan);
    Free();
    return(false);
  }
  
  Channels=TTAReader.ttahdr.NumChannels;
  SampleRate=TTAReader.ttahdr.SampleRate;
  
  _consolePrintf("Channels:%d\n",Channels);
  _consolePrintf("SampleRate:%d\n",SampleRate);
  
  if(TTAReader.seek_table==NULL){
    _consolePrintf("Not found seek table.\n");
    }else{
/*
    for(u32 idx=0;idx<TTAReader.framecount;idx++){
      _consolePrintf("SeekTable:%d,$%08x\n",idx,TTAReader.seek_table[idx]);
    }
*/
  }
  
  DecBufPos=0;
  DecBufCnt=0;
  
  Render();
  
  return(true);
}

void Free(void)
{
  TTAReader_Free();
}

u32 Update(s16 *lbuf,s16 *rbuf)
{
  Render();
  
  u32 lastcnt=DecBufCnt-DecBufPos;
  
  if(lastcnt==0) return(0);
  
  if(SamplePerFrame<lastcnt) lastcnt=SamplePerFrame;
  
  if(lbuf!=NULL) MemCopy16CPU(&DecBufL[DecBufPos],&lbuf[0],lastcnt*2);
  if(rbuf!=NULL) MemCopy16CPU(&DecBufR[DecBufPos],&rbuf[0],lastcnt*2);
  DecBufPos+=lastcnt;
  
  return(lastcnt);
}

s32 GetPosMax(void)
{
  return(TTAReader.framecount);
}

s32 GetPosOffset(void)
{
  return(TTAReader.frameindex);
}

void SetPosOffset(s32 ofs)
{
  if(FileHandle==0) return;
  
  if(TTAReader.seek_table==NULL) return;
  
  if(ofs<0) ofs=0;
  
  u32 tagframe=(u32)ofs;
  
  if(TTAReader.framecount<tagframe) tagframe=TTAReader.framecount;
  
  u32 filepos=TTAReader.firstframepos;
  
  for(u32 idx=0;idx<tagframe;idx++){
    filepos+=TTAReader.seek_table[idx];
  }
  
  if(FileHandle!=0) fseek(FileHandle,filepos,SEEK_SET);
  
  TTAReader.frameindex=tagframe;
  TTAReader.framecurindex=0;
  
  BitReader.bit_count=0;
  BitReader.bit_cache=0;
  BitReader.bit_buffer_end=bit_buffer + BIT_BUFFER_SIZE;
  BitReader.bitpos=BitReader.bit_buffer_end;
  BitReader.next_frame_pos=filepos;
  
  DecBufPos=0;
  DecBufCnt=0;
}

u32 GetChannelCount(void)
{
  return(Channels);
}

u32 GetSampleRate(void)
{
  return(SampleRate);
}

u32 GetSamplePerFrame(void)
{
  return(SamplePerFrame);
}

int GetInfoIndexCount(void)
{
  return(5);
}

bool GetInfoStrA(int idx,char *str,int len)
{
  switch(idx){
    case 0: snprintf(str,len,"Channels: %d, SampleRate: %dHz",Channels,SampleRate); return(true); break;
    case 1: {
      u32 sec=(u32)(((u64)TTAReader.framelen*TTAReader.framecount)/SampleRate);
      snprintf(str,len,"Play time: %d:%02d:%02d",sec/60/60,(sec/60)%60,sec%60);
      return(true);
    } break;
    case 2: snprintf(str,len,"%dsamples/frame %dframes",TTAReader.framelen,TTAReader.framecount); return(true); break;
    case 3: snprintf(str,len,"First frame position: $%08x",TTAReader.firstframepos); return(true); break;
    case 4: snprintf(str,len,"Seek table addr: $%08x",TTAReader.seek_table); return(true); break;
  }
  return(false);
}

bool GetInfoStrW(int idx,UnicodeChar *str,int len)
{
  return(false);
}

bool GetInfoStrUTF8(int idx,char *str,int len)
{
  return(false);
}

// -----------------------------------------------------------

