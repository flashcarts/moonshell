
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

// -----------------
typedef struct {
  u16 wFormatTag;
  u16 nChannels;
  u32 nSamplesPerSec;
  u32 nAvgBytesPerSec;
  u16 nBlockAlign;
  u16 wBitsPerSample;
  u16 cbSize;
} WAVEFORMATEX;

static int FileHandle;
static WAVEFORMATEX wfex;
static int DataTopOffset;
static int SampleCount;
static int SampleOffset;
static int BytePerSample;

static int SamplePerFrame;
static u32 *ReadBuffer;

// --------------------------------------------------------------------

static bool WaveFile_ReadWaveChunk(void)
{
  fseek(FileHandle,0x14,SEEK_SET);
  fread((void*)&wfex,sizeof(wfex),1,FileHandle);
  
  if(wfex.wFormatTag!=0x0001){
    _consolePrintf("Illigal CompressFormat Error. wFormatTag=0x%x\n",wfex.wFormatTag);
    return(false);
  }
  
  if((wfex.nChannels!=1)&&(wfex.nChannels!=2)){
    _consolePrintf("Channels Error. nChannels=%d\n",wfex.nChannels);
    return(false);
  }
  
  if((wfex.wBitsPerSample!=8)&&(wfex.wBitsPerSample!=16)){
    _consolePrintf("Bits/Sample Error. wBitsPerSample=%d\n",wfex.wBitsPerSample);
    return(false);
  }
  
  _consolePrintf("fmt:0x%x chns:%d\n",wfex.wFormatTag,wfex.nChannels);
  _consolePrintf("Smpls/Sec:%d AvgBPS:%d\n",wfex.nSamplesPerSec,wfex.nAvgBytesPerSec);
  _consolePrintf("BlockAlign:%d Bits/Smpl:%d\n",wfex.nBlockAlign,wfex.wBitsPerSample);
  
  return(true);
}

static bool WaveFile_SeekDataChunk(void)
{
  fseek(FileHandle,0,SEEK_SET);
  
  // find "data"
  {
    char *readbuf=(char*)safemalloc(256);
    int size=0;
    int ofs=0;
    
    size=fread(readbuf,1,256,FileHandle);
    if(size<4){
      safefree(readbuf);
      _consolePrintf("can not find data chunk.\n");
      return(false);
    }
    
    while(true){
      if(readbuf[ofs]=='d'){
        if((readbuf[ofs+1]=='a')&&(readbuf[ofs+2]=='t')&&(readbuf[ofs+3]=='a')){
          safefree(readbuf);
          fseek(FileHandle,ofs+4,SEEK_SET);
          break;
        }
      }
      ofs++;
      if(ofs==(size-4)){
        safefree(readbuf);
        _consolePrintf("can not find data chunk.\n");
        return(false);
      }
    }
  }
  
  u32 DataSize;
  fread(&DataSize,4,1,FileHandle);
  
  if(DataSize==0){
    _consolePrintf("DataSize is NULL\n");
    return(false);
  }
  
  BytePerSample=0;
  
  if((wfex.nChannels==1)&&(wfex.wBitsPerSample==8)) BytePerSample=1;
  if((wfex.nChannels==2)&&(wfex.wBitsPerSample==8)) BytePerSample=2;
  if((wfex.nChannels==1)&&(wfex.wBitsPerSample==16)) BytePerSample=2;
  if((wfex.nChannels==2)&&(wfex.wBitsPerSample==16)) BytePerSample=4;
  
  if(BytePerSample==0){
    _consolePrintf("Illigal Channels or Bits/Sample or no data\n");
    return(false);
  }
  
  SampleCount=DataSize/BytePerSample;
  
  DataTopOffset=ftell(FileHandle);
  
  _consolePrintf("DataTop:%d DataSize:%dbyte\n",DataTopOffset,DataSize);
  
  return(true);
}

static bool WaveFile_Open(void)
{
  fseek(FileHandle,0,SEEK_SET);
  
  u32 RIFFID;
  fread(&RIFFID,4,1,FileHandle);
  
  if(RIFFID!=0x46464952){ // check "RIFF"
    _consolePrintf("no RIFFWAVEFILE error.");
    _consolePrintf("topdata:0x%04x\n",RIFFID);
    return(false);
  }
  
  if(WaveFile_ReadWaveChunk()==false) return(false);
  if(WaveFile_SeekDataChunk()==false) return(false);
  
  return(true);
}

// ------------------------------------------------------------------------------------

bool Start(int _FileHandle)
{
  FileHandle=_FileHandle;
  
  if(WaveFile_Open()==false){
    return(false);
  }
  
  SampleOffset=0;
  SamplePerFrame=1024;
  
  ReadBuffer=(u32*)safemalloc(SamplePerFrame*BytePerSample);
  if(ReadBuffer==NULL){
    _consolePrintf("out of memory.\n");
    return(false);
  }
  
  return(true);
}

void Free(void)
{
  if(ReadBuffer!=NULL){
    safefree(ReadBuffer); ReadBuffer=NULL;
  }
}

u32 Update(s16 *lbuf,s16 *rbuf)
{
  int SampleCount;
  
  SampleCount=fread(ReadBuffer,1,SamplePerFrame*BytePerSample,FileHandle)/BytePerSample;
  
  if(wfex.wBitsPerSample==8){
    if(wfex.nChannels==1){ // 8bit mono
      s8 *readbuf=(s8*)ReadBuffer;
      for(u32 idx=SampleCount;idx!=0;idx--){
        *lbuf=(((s16)*readbuf)-128)<<8;
        lbuf++; readbuf++;
      }
      }else{ // 8bit stereo
      s8 *readbuf=(s8*)ReadBuffer;
      for(u32 idx=SampleCount;idx!=0;idx--){
        *lbuf=(((s16)*readbuf)-128)<<8;
        lbuf++; readbuf++;
        *rbuf=(((s16)*readbuf)-128)<<8;
        rbuf++; readbuf++;
      }
    }
    }else{
    if(wfex.nChannels==1){ // 16bit mono
      s16 *readbuf=(s16*)ReadBuffer;
      for(u32 idx=SampleCount;idx!=0;idx--){
        *lbuf=*readbuf;
        lbuf++; readbuf++;
      }
      }else{ // 16bit stereo
      s16 *readbuf=(s16*)ReadBuffer;
      for(u32 idx=SampleCount;idx!=0;idx--){
        *lbuf=*readbuf;
        lbuf++; readbuf++;
        *rbuf=*readbuf;
        rbuf++; readbuf++;
      }
    }
  }
  
  SampleOffset+=SampleCount;
  
  return(SampleCount);
}

s32 GetPosMax(void)
{
  return(SampleCount);
}

s32 GetPosOffset(void)
{
  return(SampleOffset);
}

void SetPosOffset(s32 ofs)
{
  if(ofs<0) ofs=0;
  ofs&=~3;
  
  SampleOffset=ofs;
  
  fseek(FileHandle,DataTopOffset+(SampleOffset*BytePerSample),SEEK_SET);
}

u32 GetChannelCount(void)
{
  return(wfex.nChannels);
}

u32 GetSampleRate(void)
{
  return(wfex.nSamplesPerSec);
}

u32 GetSamplePerFrame(void)
{
  return(SamplePerFrame);
}

int GetInfoIndexCount(void)
{
  return(3);
}

bool GetInfoStrA(int idx,char *str,int len)
{
  switch(idx){
    case 0: snprintf(str,len,"wFormatTag=0x%x",wfex.wFormatTag); return(true); break;
    case 1: snprintf(str,len,"%dHz %dbit %dChannels",wfex.nSamplesPerSec,wfex.wBitsPerSample,wfex.nChannels); return(true); break;
    case 2: snprintf(str,len,"Length=%d:%02d",(SampleCount/wfex.nSamplesPerSec)/60,(SampleCount/wfex.nSamplesPerSec)%60); return(true); break;
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

