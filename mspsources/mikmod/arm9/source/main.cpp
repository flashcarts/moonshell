
#include <stdio.h>

#include <NDS.h>

#include "_const.h"

#include "plugin.h"
#include "plugin_def.h"

#include "libmikmod/include/mikmod.h"

#include "inifile.h"

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

#define PCMReadSampleCount (640)

typedef struct {
  u32 MaxVoiceCount;
  u32 SampleRate,ChannelCount;
  s32 SampleCount,SampleOffset;
} TMikModInfo;

static TMikModInfo MikModInfo;

static MODULE *StaticModule=NULL;

static s16 *wlbuf=NULL,*wrbuf=NULL;

// --------------------------------------------------------------------

extern int MikMod_SoundBufferSize;

extern "C" {
int MikMod_DRV_NS_GetMaxVoiceCount(void);
FILE* MikMod_fopen(CHAR* fname,CHAR* attrib);
int MikMod_fclose(FILE *fp);
int MikMod_fgetc(FILE *fp);
size_t MikMod_fread(void *ptr, size_t items, size_t size, FILE *fp);
int MikMod_fseek(FILE *fp, long offset,int whence);
int MikMod_feof(FILE *fp);
long MikMod_ftell(FILE *fp);
void MikMod_SoundUpdate(void *buf,int bufsize);
void MikMod_PrgStart(char *ttl,int max);
void MikMod_PrgRefresh(int pos);
void MikMod_PrgEnd(void);
}

static int FileHandle;
static int FileSize;
static int FilePos;
static bool FileEofFlag;

#define FileBufSize (256)
static u8 FileBuf[FileBufSize];
static int FileBufRemain;

int MikMod_DRV_NS_GetMaxVoiceCount(void)
{
  _consolePrintf("GetMaxVoiceCount()==%d\n",MikModInfo.MaxVoiceCount);
  return(MikModInfo.MaxVoiceCount);
}

FILE* MikMod_fopen(CHAR* fname,CHAR* attrib)
{
  _consolePrintf("fopen(%s,%s);",fname,attrib);
  
  fseek(FileHandle,0,SEEK_END);
  FileSize=ftell(FileHandle);
  fseek(FileHandle,0,SEEK_SET);
  FilePos=0;
  FileEofFlag=false;
  
  FileBufRemain=0;
  
  return((FILE*)1);
}

int MikMod_fclose(FILE *fp)
{
  _consolePrintf("fclose(0x%x);",(u32)fp);
  if(fp!=NULL){
    return(0);
    }else{
    return(EOF);
  }
}

int MikMod_fgetc(FILE *fp)
{
//  _consolePrintOne("!",0);
  
  if(FileSize<=FilePos){
    FileEofFlag=true;
    return(EOF);
  }
  
  if(FileBufRemain==0){
    fread(&FileBuf,1,FileBufSize,FileHandle);
    FileBufRemain=FileBufSize;
  }
  
  {
    u8 buf;
    buf=FileBuf[FileBufSize-FileBufRemain];
    FileBufRemain--;
    FilePos++;
    return(buf);
  }
}

size_t MikMod_fread(void *ptr, size_t items, size_t size, FILE *fp)
{
//  _consolePrintOne("fread(x,%d,x);",items*size);
  
  int readsize=items*size;
  
  if(FileSize<(FilePos+readsize)){
    readsize=FileSize-FilePos;
    FileEofFlag=true;
  }
  if(readsize==0) return(0);
  
  if(readsize<=FileBufRemain){
    u8 *writebuf=(u8*)ptr;
    u8 *readbuf=&FileBuf[FileBufSize-FileBufRemain];
    memcpy(writebuf,readbuf,readsize);
    FileBufRemain-=readsize;
    FilePos+=readsize;
    return(readsize);
  }
  
  if(FilePos!=ftell(FileHandle)){
    fseek(FileHandle,FilePos,SEEK_SET);
  }
  
  int rsize=fread(ptr,1,readsize,FileHandle);
  FilePos+=rsize;
  
  FileBufRemain=0;
  
  return(rsize);
}

int MikMod_fseek(FILE *fp, long offset,int whence)
{
//  _consolePrintOne("fseek(%d,",offset);
//  _consolePrintOne("%d);",whence);
  
  if(FilePos!=ftell(FileHandle)){
    fseek(FileHandle,FilePos,SEEK_SET);
  }
  
  int ret;
  
  ret=fseek(FileHandle,offset,whence);
  FilePos=ftell(FileHandle);
  
  FileBufRemain=0;
  
  return(ret);
}

int MikMod_feof(FILE *fp)
{
  if(FileEofFlag==true){
//    _consolePrintOne("feof(1),ftell=%d;",ftell(FileHandle));
    return(1);
    }else{
//    _consolePrintOne("feof(0),ftell=%d;",ftell(FileHandle));
    return(0);
  }
}

long MikMod_ftell(FILE *fp)
{
//  _consolePrintOne("ftell()=%d;",ftell(FileHandle));
  return(FilePos);
}

void MikMod_SoundUpdate(void *buf,int bufsize)
{
  if((wlbuf==NULL)||(wrbuf==NULL)) return;
  
  if(MikModInfo.ChannelCount==2){
    u32 idx;
    s16 *rbuf=(s16*)buf;
    for(idx=0;idx<PCMReadSampleCount;idx++){
      wlbuf[idx]=*rbuf; rbuf++;
      wrbuf[idx]=*rbuf; rbuf++;
    }
    }else{
    u32 idx;
    s16 *rbuf=(s16*)buf;
    for(idx=0;idx<PCMReadSampleCount;idx++){
      s16 data;
      data=*rbuf; rbuf++;
      wlbuf[idx]=data;
      wrbuf[idx]=data;
    }
  }
  
}

//--------------------------------

bool isPKZIP(int FileHandle)
{
  u32 MagicID=0;
  
  fseek(FileHandle,0,SEEK_SET);
  fread(&MagicID,1,4,FileHandle);
  fseek(FileHandle,0,SEEK_SET);
  
  if(MagicID==0x04034b50){ // P,K,0x03,0x04
    return(true);
    }else{
    return(false);
  }
}

// ------------------------------------------------------------------------------------

extern void Free(void);

bool Start(int _FileHandle)
{
  InitINI();
//  LoadINI(GetINIData(),GetINISize());
  
  {
    static bool regflag=false;
    
    if(regflag==false){
      regflag=true;
      MikMod_RegisterAllDrivers();
      MikMod_RegisterAllLoaders();
    }
  }
  
  if(isPKZIP(_FileHandle)==true){
    _consolePrintf("The files are formats of ZIP.\n");
    _consolePrintf("The extension is changed to *.zip.\n");
    _consolePrintf("It inflate with the decompression software.\n");
    _consolePrintf("The generated file is copied onto CF and it performs.\n");
    
    return(false);
  }
  
  FileHandle=_FileHandle;
  
  {
    TiniMikModPlugin *MikModPlugin=&GlobalINI.MikModPlugin;
    
    MikModInfo.MaxVoiceCount=MikModPlugin->MaxVoiceCount;
    MikModInfo.SampleRate=MikModPlugin->Frequency;
    MikModInfo.ChannelCount=MikModPlugin->Channels;
    MikModInfo.SampleCount=0;
    MikModInfo.SampleOffset=0;
  }
  
  {
    _consolePrintf("SampleRate %dHz\n",MikModInfo.SampleRate);
    _consolePrintf("ChannelCount %dch\n",MikModInfo.ChannelCount);
    _consolePrintf("SampleCount %dsample\n",MikModInfo.SampleCount);
    _consolePrintf("\n");
  }
  
  {
    TiniMikModPlugin *MikModPlugin=&GlobalINI.MikModPlugin;
    
    md_mixfreq=MikModPlugin->Frequency;
    
    if(MikModPlugin->Flag_Interpolate==true) md_mode|=DMODE_INTERP;
    if(MikModPlugin->Flag_Surround==true) md_mode|=DMODE_SURROUND;
    if(MikModPlugin->Flag_HQMixer==true) md_mode|=DMODE_HQMIXER;
    if(MikModPlugin->Channels==2) md_mode|=DMODE_STEREO;
    
    MikMod_SoundBufferSize=PCMReadSampleCount*2*MikModPlugin->Channels; // 16bit2ch
  }
  
  {
    /* initialize the library */
    md_device=0;
    if(MikMod_Init("")!=0){
      _consolePrintf("Could not initialize sound, reason: %s\n",MikMod_strerror(MikMod_errno));
      Free();
      return(false);
    }
    
    /* load module */
    StaticModule = Player_Load("memory", 64, 0);
    if(StaticModule==NULL){
      _consolePrintf("File load error, reason: %s\n",MikMod_strerror(MikMod_errno));
      MikMod_Exit();
      Free();
      return(false);
    }
    
    MikModInfo.SampleCount=StaticModule->numpos;
    MikModInfo.SampleOffset=0;
    
    _consolePrintf("loaded.\n");
  }
  
  /* start module */
  Player_Start(StaticModule);
  
  _consolePrintf("warrning:In the MikMod interface, there is a memory leak bug.\n");
  
  return(true);
}

void Free(void)
{
  if(StaticModule!=NULL){
    Player_Stop();
    Player_Free(StaticModule);
    MikMod_Exit();
    StaticModule=NULL;
  }
  
  _consolePrintf("FreeMikMod();\n");
  
  wlbuf=NULL;
  wrbuf=NULL;
  
  FileHandle=0;
  
  MikModInfo.SampleRate=0;
  MikModInfo.ChannelCount=0;
  MikModInfo.SampleCount=0;
  MikModInfo.SampleOffset=0;
}

u32 Update(s16 *lbuf,s16 *rbuf)
{
  if(Player_Active()==false){
    return(0);
  }
  
  wlbuf=lbuf;
  wrbuf=rbuf;
  
  MikMod_Update();
  
  MikModInfo.SampleOffset=StaticModule->sngpos;
  return(PCMReadSampleCount);
}

s32 GetPosMax(void)
{
  return(MikModInfo.SampleCount);
}

s32 GetPosOffset(void)
{
  return(MikModInfo.SampleOffset);
}

void SetPosOffset(s32 ofs)
{
  Player_SetPosition(ofs);
  MikModInfo.SampleOffset=StaticModule->sngpos;
}

u32 GetChannelCount(void)
{
  return(MikModInfo.ChannelCount);
}

u32 GetSampleRate(void)
{
  return(MikModInfo.SampleRate);
}

u32 GetSamplePerFrame(void)
{
  return(PCMReadSampleCount);
}

static bool isComment(void)
{
  char *modtype=StaticModule->modtype;
  
  if(strncmp(modtype,"ImpulseTracker",strlen("ImpulseTracker"))==0) return(true);
  if(strncmp(modtype,"Compressed ImpulseTracker",strlen("Compressed ImpulseTracker"))==0) return(true);
  if(strncmp(modtype,"MTM",strlen("MTM"))==0) return(true);
  
  return(false);
}

int GetInfoIndexCount(void)
{
  int count=2;
  
  if(isComment()==true){
    char *pcmt=StaticModule->comment;
    
    if(pcmt!=NULL){
      while(*pcmt!=0){
        if((*pcmt==0x0d)||(*pcmt==0x0a)) count++;
        pcmt++;
      }
    }
    }else{
//    count+=StaticModule->numsmp;
  }
  
  return(count);
}

bool GetInfoStrL(int idx,char *str,int len)
{
  if(idx==0){
    snprintf(str,len,"%s inst=%d smpl=%d",StaticModule->modtype,StaticModule->numins,StaticModule->numsmp);
    return(true);
  }
  
  if(idx==1){
    snprintf(str,len,"Title:%s",StaticModule->songname);
    return(true);
  }
  
  idx-=2;
  
  if(isComment()==true){
    char *pcmt=StaticModule->comment;
    len-=2;
    while(*pcmt!=0){
      if(idx==0){
        while((len!=0)&&(*pcmt!=0)&&(*pcmt!=0x0d)&&(*pcmt!=0x0a)){
          *str=*pcmt;
          str++; pcmt++; len--;
        }
        *str=0;
        return(true);
      }
      if((*pcmt==0x0d)||(*pcmt==0x0a)) idx--;
      pcmt++;
    }
    return(false);
  }
  
  return(false);
  
  if(idx<StaticModule->numsmp){
    struct SAMPLE* sample=&StaticModule->samples[idx];
    if(sample==NULL){
      snprintf(str,len,"%d:",idx);
      return(true);
    }
    char *name=sample->samplename;
    if(name==NULL){
      snprintf(str,len,"%d:",idx);
      return(true);
    }
    snprintf(str,len,"%d:%s",idx,name);
    return(true);
  }
  
  return(false);
}

bool GetInfoStrA(int idx,char *str,int len)
{
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

