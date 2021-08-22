
#include <stdio.h>

#include <NDS.h>

#include "_const.h"

#include "plugin.h"
#include "plugin_def.h"

#include "boolean.h"

#include "libspc/spc_debug.h"
#include "libspc/apu.h"
#include "libspc/dsp.h"

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

#define MIXBUFCOUNT (16)

typedef struct {
  u8 *SPCFileBuf;
  u32 SPCFileSize;
  u32 SampleRate,SamplePerFrame,ChannelCount;
  s32 SampleCount,SampleOffset;
  u16 SampleBuf[MIXBUFSIZE*2]; // store format L0,L1,L2...,R0,R1,R2...
} TSPCPlayInfo;

static TSPCPlayInfo SPCPlayInfo;

struct  SpcTextTagBlock{
    char spcMagic[33];
    char spcMagicTerminator[3];
    char version;
    char spcRegister[7];
    u16 __reserved1;
    char title[32];
    char game[32];
    char dumper[16];
    char comments[32];
    char dumpDate[11];
    char length[3];
    char fadeLength[5]; // In milliseconds
    char artist[32];
    char defaultChannels;
    char emulator;
    char __reserved2[45];
} __attribute__((packed));

struct SpcBinaryTagBlock {
    char spcMagic[33];
    char spcMagicTerminator[3];
    char version;
    char spcRegister[7];
    u16 __reserved1;
    char title[32];
    char game[32];
    char dumper[16];
    char comments[32];

    // Different from text tag starting here.
    u16 dumpDateYear;
    u8 dumpDateMonth;
    u8 dumpDateDay;
    char __unused[7];
    u16 length;
    char __unused2;
    u16 fadeLength; // In milliseconds
    char artist[32];
    char defaultChannels;
    char emulator;
    char __reserved2[46];
} __attribute__((packed));

struct ID666Info {
    bool valid;
    char songTitle[33];
    char gameTitle[33];
    char comments[33];
    char songLength[33];
    int songLengthRaw;
    char artist[33];
};
static ID666Info curId666Info;

void spc_memset(void *data, int fill, int length)
{
    uint8 *data2 = (uint8*)data;
    while (length-- > 0) {
        *data2++ = fill;
    }
}

void spc_memcpy(void *dest, void *src, int length)
{
    uint8 *dest2 = (uint8*)dest;
    uint8 *src2 = (uint8*)src;
    while (length--) {
        *dest2++ = *src2++;
    }
}

static bool _SpcHeaderIsText(const char *s, int l)
{
    int i;
    for (i = 0; i < l && (s[i] == 0 || (s[i] >= '0' && s[i] <= '9') || s[i] == '/'); i++);
    if(s[i] == 0 || i == l) return true;
    return false;
}

static bool _SpcHeaderIsSet(char *s, int l)
{
    for(int i = 0; i < l; ++i) if (s[i]) return true;
    return false;
}

static void _LoadSpc(const u8 *spc) {
#ifdef ARM7SPC
#else
    // Load the SPC
    _consolePrintf("Reset APU.\n");
    ApuReset();
    _consolePrintf("Reset DSP.\n");
    DspReset();

// 0 - A, 1 - X, 2 - Y, 3 - RAMBASE, 4 - DP, 5 - PC (Adjusted into rambase)
// 6 - Cycles (bit 0 - C, bit 1 - v, bit 2 - h, bits 3+ cycles left)
// 7 - Optable
// 8 - NZ

    APU_STATE[0] = spc[0x27]<<24; // A
    APU_STATE[1] = spc[0x28]<<24; // X
    APU_STATE[2] = spc[0x29]<<24; // Y
    _consolePrintf("SetStateFromRawPSW(APU_STATE, spc[0x2A]==%d);\n",spc[0x2A]);
    SetStateFromRawPSW(APU_STATE, spc[0x2A]);
    APU_SP = 0x100 | spc[0x2B]; // SP
    APU_STATE[5] = APU_STATE[3] + (spc[0x25] | (spc[0x26] << 8)); // PC

    for (int i=0; i<=0xffff; i++) APU_MEM[i] = spc[0x100 + i];
    for (int i=0; i<=0x7f; i++) {
        DSP_MEM[i] = spc[0x10100 + i];
    }
    for (int i=0; i<=0x3f; i++) APU_EXTRA_MEM[i] = spc[0x101c0 + i];

    _consolePrintf("ApuPrepareStateAfterReload\n");
    ApuPrepareStateAfterReload();
    _consolePrintf("DspPrepareStateAfterReload\n");
    DspPrepareStateAfterReload();
#endif

    spc_memset(&curId666Info, 0, sizeof(curId666Info));

    curId666Info.valid = false;
    
    // ID 666 information
    if (spc[0x23] == 26) {
        _consolePrintf("Load ID666.\n");
        union SpcHeaderBlock {
            SpcTextTagBlock text;
            SpcBinaryTagBlock binary;
        } *spcHeader;

        spcHeader = (SpcHeaderBlock*)&spc[0];

        curId666Info.valid = true;

        spc_memcpy(curId666Info.songTitle, spcHeader->text.title, 30);
        curId666Info.songTitle[30] = 0;
        spc_memcpy(curId666Info.gameTitle, spcHeader->text.game, 30);
        curId666Info.gameTitle[30] = 0;
        spc_memcpy(curId666Info.comments, spcHeader->text.comments, 30);
        curId666Info.comments[30] = 0;
        spc_memcpy(curId666Info.artist, spcHeader->text.artist, 30);
        curId666Info.artist[30] = 0;

        bool isText = false;
        if(_SpcHeaderIsSet(spcHeader->text.length, 3)) {
            isText = _SpcHeaderIsText(spcHeader->text.length, 3);
        } else if(_SpcHeaderIsSet(spcHeader->text.dumpDate, 6)) {
            isText = _SpcHeaderIsText(spcHeader->text.dumpDate, 6);
        }

        if (isText) {
            char tmp[4];
            spc_memcpy(tmp, spcHeader->text.length, 3);
            tmp[3] = 0;
            curId666Info.songLengthRaw = atoi(tmp) + 3;
        } else {
            curId666Info.songLengthRaw = spcHeader->binary.length + 3;
        }
        if (curId666Info.songLengthRaw == 3) {
            curId666Info.songLengthRaw = 180; // 3 minutes if it's default
        }
        sprintf(curId666Info.songLength, "%d:%02d", curId666Info.songLengthRaw / 60, curId666Info.songLengthRaw % 60);
    }
}

static void _RefreahStatus(void)
{

    {
        char c[256];

        _consolePrintSet(0, 13);
        _consolePrintf("Channel states:\n");
        for (int i = 0; i < 8; i++) {
            sprintf(c, "%d %d %d %2x %4x %2x e:%3d %2x %2x", (int)channels[i].active, (int)channels[i].envState,
                (int)channels[i].echoEnabled, channels[i].envx >> 8,
                (DSP_MEM[(i << 4) | DSP_PITCH_H] << 8) | DSP_MEM[(i << 4) | DSP_PITCH_L],
                DSP_MEM[(i << 4) + DSP_SRC], DSP_MEM[(i << 4) | 0xf], channels[i].leftVolume & 0xff, channels[i].rightVolume & 0xff);
            _consolePrintf("%s\n",c);
        }

    }

}

static void _RenderSPC(bool UseDSP)
{
//  static void (*lp_ApuExecute)(u32 cycles)=ApuExecute;
//  static void (*lp_DspEmulateOneSample)(u16 *mixBuf)=DspEmulateOneSample;
  
  if(UseDSP==true){
    for (int i = 0; i < MIXBUFSIZE; i+=4) {
      ApuExecute((spcCyclesPerSec*4)/APURATE);
      DspEmulateOneSample(&SPCPlayInfo.SampleBuf[i+0]);
      DspEmulateOneSample(&SPCPlayInfo.SampleBuf[i+1]);
      DspEmulateOneSample(&SPCPlayInfo.SampleBuf[i+2]);
      DspEmulateOneSample(&SPCPlayInfo.SampleBuf[i+3]);
    }
    }else{
    ApuExecute((spcCyclesPerSec*MIXBUFSIZE)/APURATE);
  }
}

static u8 *DeflateBuf=NULL;
static u32 DeflateSize;

// ------------------------------------------------------------------------------------

bool Start(int FileHandle)
{
  fseek(FileHandle,0,SEEK_END);
  DeflateSize=ftell(FileHandle);
  fseek(FileHandle,0,SEEK_SET);
  
  DeflateBuf=(u8*)safemalloc(DeflateSize);
  if(DeflateBuf==NULL) return(false);
  fread(DeflateBuf,1,DeflateSize,FileHandle);
  
  _consolePrintSet(0, 0);
  
#ifdef SNESDS
  _consolePrintf("#define SNESDS\n");
#endif
#ifdef ARM7SPC
  _consolePrintf("#define ARM7SPC\n");
#endif
#ifdef DEBUG
  _consolePrintf("#define DEBUG\n");
#endif
  
  _consolePrintf("SPC700 RenderRate %dHz\n",MIXRATE);
  _consolePrintf("SPC700 APU clocks %dHz\n",APURATE);
  _consolePrintf("Output SampleRate %dHz\n",NDSPCMOutRate);
  
  SPCPlayInfo.SPCFileBuf=DeflateBuf;
  SPCPlayInfo.SPCFileSize=DeflateSize;
  
  SPCPlayInfo.SampleRate=NDSPCMOutRate; // MIXRATE;
  SPCPlayInfo.SamplePerFrame=MIXBUFSIZE*MIXBUFCOUNT;
  SPCPlayInfo.ChannelCount=2;
  
  SPCPlayInfo.SampleCount=180*SPCPlayInfo.SampleRate;
  SPCPlayInfo.SampleOffset=0;
  
  for(u32 cnt=0;cnt<MIXBUFSIZE*2;cnt++){
    SPCPlayInfo.SampleBuf[cnt]=0;
  }
  
  _consolePrintf("SamplePerFrame %dsamples\n",SPCPlayInfo.SamplePerFrame);
  
  {
    const u32 RAW_APU_MEM_SIZE=0x10000;
    extern u8 *APU_MEM;
    const int brrHashLength = 0x2000 + (0x40000 / 4);
    extern u32 *brrHash;
    
    if(APU_MEM!=NULL){
      _consolePrintf("APU_MEM!=NULL\n");
      ShowLogHalt();
    }
    APU_MEM=(u8*)malloc(RAW_APU_MEM_SIZE);
    if(APU_MEM==NULL){
      _consolePrintf("APU_MEM memory overflow.\n");
      ShowLogHalt();
    }
    
    if(brrHash!=NULL){
      _consolePrintf("brrHash!=NULL\n");
      ShowLogHalt();
    }
    brrHash=(u32*)malloc(brrHashLength*4);
    if(brrHash==NULL){
      _consolePrintf("brrHash memory overflow.\n");
      ShowLogHalt();
    }
  }
  
  _consolePrintf("_LoadSpc.\n");
  _LoadSpc(SPCPlayInfo.SPCFileBuf);
  _consolePrintf("SPC loaded.\n");
  
  if(curId666Info.valid==true){
    _consolePrintf("Exists curId666Info.\n");
    if(curId666Info.songLengthRaw!=0){
      SPCPlayInfo.SampleCount=curId666Info.songLengthRaw*SPCPlayInfo.SampleRate;
    }
  }
  
  _consolePrintf("ready.\n");
  
  _RefreahStatus();
  
  return(true);
}

void Free(void)
{
  {
    extern u8 *APU_MEM;
    extern u32 *brrHash;
    
    if(APU_MEM!=NULL){
      free(APU_MEM);
      APU_MEM=NULL;
    }
    
    if(brrHash!=NULL){
      free(brrHash);
      brrHash=NULL;
    }
  }
  
  SPCPlayInfo.SPCFileBuf=NULL;
  SPCPlayInfo.SPCFileSize=0;
  
  SPCPlayInfo.SampleRate=0;
  SPCPlayInfo.SamplePerFrame=0;
  SPCPlayInfo.ChannelCount=0;
  
  SPCPlayInfo.SampleCount=0;
  SPCPlayInfo.SampleOffset=0;
  
  for(u32 cnt=0;cnt<MIXBUFSIZE*2;cnt++){
    SPCPlayInfo.SampleBuf[cnt]=0;
  }
  
  if(DeflateBuf!=NULL){
    safefree(DeflateBuf); DeflateBuf=NULL;
  }
}

u32 Update(s16 *lbuf,s16 *rbuf)
{
  if(SPCPlayInfo.SampleCount<=SPCPlayInfo.SampleOffset) return(0);
  
  u32 sndbufSize=MIXBUFSIZE*MIXBUFCOUNT;
  
  if((lbuf!=NULL)&&(rbuf!=NULL)){
    u16 *slbuf=&SPCPlayInfo.SampleBuf[MIXBUFSIZE*0];
    u16 *srbuf=&SPCPlayInfo.SampleBuf[MIXBUFSIZE*1];
    u16 *dlbuf=(u16*)&lbuf[0];
    u16 *drbuf=(u16*)&rbuf[0];
    for(u32 cnt=MIXBUFCOUNT;cnt!=0;cnt--){
      _RenderSPC(true);
      MemCopy32CPU(slbuf,dlbuf,MIXBUFSIZE*2);
      MemCopy32CPU(srbuf,drbuf,MIXBUFSIZE*2);
      dlbuf+=MIXBUFSIZE;
      drbuf+=MIXBUFSIZE;
    }
//    _RefreahStatus();
    
    
    bool exists=false;
    
    s16 chkdatal=lbuf[0]/0x10;
    s16 chkdatar=rbuf[0]/0x10;
    for(u32 idx=1;idx<sndbufSize;idx+=11){
      if(chkdatal!=(lbuf[idx]/0x10)) exists=true;
      if(chkdatar!=(rbuf[idx]/0x10)) exists=true;
    }
    
    static int BlankCount=0;
    if(SPCPlayInfo.SampleOffset==0) BlankCount=0;
    
    if(exists==true){
      BlankCount=(NDSPCMOutRate/(MIXBUFSIZE*MIXBUFCOUNT))*3;
      }else{
      if(BlankCount!=0){
        BlankCount--;
        if(BlankCount==0) sndbufSize=0;
      }
    }
    
    }else{
    for(u32 cnt=0;cnt<MIXBUFCOUNT;cnt++){
      _RenderSPC(false);
    }
  }
  
  SPCPlayInfo.SampleOffset+=sndbufSize;
  
  return(sndbufSize);
}

s32 GetPosMax(void)
{
  return(SPCPlayInfo.SampleCount);
}

s32 GetPosOffset(void)
{
  return(SPCPlayInfo.SampleOffset);
}

void SetPosOffset(s32 ofs)
{
  if(SPCPlayInfo.SampleCount<ofs) ofs=SPCPlayInfo.SampleCount;
  if(SPCPlayInfo.SampleOffset==ofs) return;
  
  while(SPCPlayInfo.SampleOffset<ofs){
    _RenderSPC(false);
    SPCPlayInfo.SampleOffset+=MIXBUFSIZE;
  }
}

u32 GetChannelCount(void)
{
  return(SPCPlayInfo.ChannelCount);
}

u32 GetSampleRate(void)
{
  return(SPCPlayInfo.SampleRate);
}

u32 GetSamplePerFrame(void)
{
  return(SPCPlayInfo.SamplePerFrame);
}

int GetInfoIndexCount(void)
{
  if(curId666Info.valid==false){
    return(1);
    }else{
    return(5);
  }
}

bool GetInfoStrA(int idx,char *str,int len)
{
  if(curId666Info.valid==false){
    if(idx==0){
      snprintf(str,len,"ID666 tag not found.");
      return(true);
    }
    return(false);
  }
  
  switch(idx){
    case 0: snprintf(str,len,"SongTitle=%s",curId666Info.songTitle); return(true); break;
    case 1: snprintf(str,len,"GameTitle=%s",curId666Info.gameTitle); return(true); break;
    case 2: snprintf(str,len,"Comments=%s",curId666Info.comments); return(true); break;
    case 3: snprintf(str,len,"Artist=%s",curId666Info.artist); return(true); break;
    case 4: snprintf(str,len,"SongLength=%s",curId666Info.songLength); return(true); break;
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

