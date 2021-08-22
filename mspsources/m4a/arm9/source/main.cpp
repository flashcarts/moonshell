
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

// http://nanncyatte.blog52.fc2.com/blog-entry-31.html
// を多大に参照させていただきました。おおにし様に感謝します。

#define AACTitle "libfaad2-2.5 - AAC decoder library"

#include "neaacdec.h"
#include "libfaad2/error.h"

static int FileHandle;
static s32 FileSize;

#define AAC_MaxChannels (4)
#define AAC_FileReadBufferSize (FAAD_MIN_STREAMSIZE*AAC_MaxChannels)

static u8 AAC_FileReadBuffer[AAC_FileReadBufferSize];

static NeAACDecHandle hDecoder;
static long lSpendbyte;
static size_t mReadSize;

#define SamplePerFrame (640)

static u32 Channels;
static u32 SampleRate;

#define DecBufMaxCount (SamplePerFrame*16)

static u32 DecBufCount;
static s16 DecBufL[DecBufMaxCount],DecBufR[DecBufMaxCount];

static NeAACDecFrameInfo mFrameInfo;

// ------------------------------------------------------------------------------------

void Free(void);

static void Render(void)
{
  if(hDecoder==NULL) return;
  
  while(DecBufCount<SamplePerFrame){
    memmove((void *)&AAC_FileReadBuffer[0],(void *)&AAC_FileReadBuffer[lSpendbyte], AAC_FileReadBufferSize - lSpendbyte);
    mReadSize += fread(AAC_FileReadBuffer + (AAC_FileReadBufferSize - lSpendbyte), 1, lSpendbyte, FileHandle);
    lSpendbyte = 0;
    
    if(mReadSize==0) return;
    
    s16 *vpDecbuffer = (s16*)(NeAACDecDecode(hDecoder, &mFrameInfo, AAC_FileReadBuffer, mReadSize));
    
    if((mFrameInfo.samples<0)||(0<mFrameInfo.error)){
      _consolePrintf("FatalError!! FrameInfo samples=%d,error=%d [%s]\n",mFrameInfo.samples,mFrameInfo.error,NeAACDecGetErrorMessage(mFrameInfo.error));
      return;
    }
    
    /*未デコードフレーム残量を取得*/
    lSpendbyte += mFrameInfo.bytesconsumed;
    mReadSize -= mFrameInfo.bytesconsumed;
    
    u32 Samples=mFrameInfo.samples;
    if(Channels==1){
      }else{
      Samples/=2;
    }
    
    if((DecBufMaxCount-DecBufCount)<Samples){
      _consolePrintf("FatalError!! Decode buffer overflow. Free buffer size=%d, Samples=%d\n",DecBufMaxCount-DecBufCount,Samples);
      ShowLogHalt();
      while(1);
    }
    
    if((vpDecbuffer!=NULL)&&(Samples!=0)){
      s16 *psrcbuf=vpDecbuffer;
      s16 *pdstl=&DecBufL[DecBufCount];
      s16 *pdstr=&DecBufR[DecBufCount];
      if(Channels==1){
        for(u32 idx=0;idx<Samples;idx++){
          pdstl[idx]=pdstr[idx]=psrcbuf[idx];
        }
        }else{
        for(u32 idx=0;idx<Samples;idx++){
          pdstl[idx]=psrcbuf[idx*2+0];
          pdstr[idx]=psrcbuf[idx*2+1];
        }
      }
      DecBufCount+=Samples;
    }
  }
}

bool Start(int _FileHandle)
{
  FileHandle=_FileHandle;
  
  fseek(FileHandle,0,SEEK_END);
  FileSize=ftell(FileHandle);
  fseek(FileHandle,0,SEEK_SET);
  
  _consolePrintf("libfaad2 init.\n");
  
  hDecoder = NeAACDecOpen();
  
  {
    NeAACDecConfigurationPtr pConfig = NeAACDecGetCurrentConfiguration(hDecoder);
//    pConfig->defObjectType = 0; // for MPEG-4 menber.
//    pConfig->defSampleRate = 32768;
    pConfig->outputFormat = FAAD_FMT_16BIT;
    NeAACDecSetConfiguration(hDecoder, pConfig);
  }
  
  unsigned long ulSamplerate;
  unsigned char ubChannels;
  
  mReadSize = fread(AAC_FileReadBuffer, 1, AAC_FileReadBufferSize, FileHandle);
  lSpendbyte = NeAACDecInit(hDecoder, AAC_FileReadBuffer, mReadSize, &ulSamplerate, &ubChannels);
  if(lSpendbyte < 0 ){
    _consolePrintf("FatalError!! FirstDecode error.\n");
    Free();
    return(false);
  }
  
  if((ubChannels==0)||(2<ubChannels)){
    _consolePrintf("Channels count error. %dchs. 1(mono) or 2(stereo) only.\n",ubChannels);
    Free();
    return(false);
  }
  
  Channels=ubChannels;
  SampleRate=ulSamplerate;
  
  _consolePrintf("Channels:%d\n",ubChannels);
  _consolePrintf("SampleRate:%d\n",SampleRate);
  
  DecBufCount=0;
  
  Render();
  
  return(true);
}

void Free(void)
{
  if(hDecoder!=NULL) NeAACDecClose(hDecoder);
}

u32 Update(s16 *lbuf,s16 *rbuf)
{
  Render();
  
  if(DecBufCount<SamplePerFrame){
    u32 Samples=DecBufCount;
    MemCopy16CPU(&DecBufL[0],&lbuf[0],Samples*2);
    MemCopy16CPU(&DecBufR[0],&rbuf[0],Samples*2);
    DecBufCount=0;
    return(Samples);
  }
  
  MemCopy16CPU(&DecBufL[0],&lbuf[0],SamplePerFrame*2);
  MemCopy16CPU(&DecBufR[0],&rbuf[0],SamplePerFrame*2);
  
  MemCopy16CPU(&DecBufL[SamplePerFrame],&DecBufL[0],(DecBufCount-SamplePerFrame)*2);
  MemCopy16CPU(&DecBufR[SamplePerFrame],&DecBufR[0],(DecBufCount-SamplePerFrame)*2);
  DecBufCount-=SamplePerFrame;
  
  return(SamplePerFrame);
}

s32 GetPosMax(void)
{
  return(FileSize);
}

s32 GetPosOffset(void)
{
  return(ftell(FileHandle));
}

void SetPosOffset(s32 ofs)
{return;
  ofs&=~1; // 16bit align
  if(FileHandle!=0) fseek(FileHandle,ofs,SEEK_SET);
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
  NeAACDecFrameInfo fi=mFrameInfo;
  
  switch(idx){
    case 0: snprintf(str,len,"Channels=%d, SampleRate=%d",fi.channels,fi.samplerate); return(true); break;
    case 1: snprintf(str,len,"MPEG-4 AAC Format:"); return(true); break;
    case 2: {
      const char *pstr=NULL;
      switch(fi.object_type){
        case MAIN: pstr="Main profile"; break;
        case LC: pstr="Low Complexity profile"; break;
        case SSR: pstr="SSR profile"; break;
        case LTP: pstr="Long Term Prediction profile"; break;
        case HE_AAC: pstr="HE profile"; break;
        case LD: pstr="LD profile"; break;
        case ER_LC: pstr="ER, Low Complexity profile"; break;
        case ER_LTP: pstr="ER, Long Term Prediction profile"; break;
        case DRM_ER_LC: pstr="DRM/ER, Low Complexity profile"; break;
        default: pstr="Unknown profile"; break;
      }
      snprintf(str,len,"[%d] %s",fi.object_type,pstr);
      return(true);
    } break;
    case 3: {
      const char *pstr=NULL;
      switch(fi.header_type){
        case RAW: pstr="RAW"; break;
        case ADIF: pstr="ADIF"; break;
        case ADTS: pstr="ADTS"; break;
        default: pstr="unknown"; break;
      }
      snprintf(str,len,"Header type: [%d] %s",fi.header_type,pstr);
      return(true);
    } break;
    case 4: {
      const char *pstr=NULL;
      switch(fi.sbr){
        case NO_SBR: pstr="off"; break;
        case SBR_UPSAMPLED: pstr="Up sampled"; break;
        case SBR_DOWNSAMPLED: pstr="Down sampled"; break;
        case NO_SBR_UPSAMPLED: pstr="No up sampled"; break;
        default: pstr="unknown"; break;
      }
      snprintf(str,len,"SBR signalling: [%d] %s",fi.sbr,pstr);
      return(true);
    } break;
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

