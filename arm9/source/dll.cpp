
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <NDS.h>

#include "_const.h"
#include "_console.h"
#include "_consoleWriteLog.h"
#include "fat2.h"
#include "memtool.h"
#include "shell.h"
#include "splash.h"

#include "dll.h"

static long int FileSys_ftell (int hFile)
{
  return(FAT2_ftell((FAT_FILE*)hFile));
}

static int FileSys_fseek(int hFile, u32 offset, int origin)
{
  return(FAT2_fseek((FAT_FILE*)hFile,offset,origin));
}

static u32 FileSys_fread (void* buffer, u32 size, u32 count, int hFile)
{
//  _consolePrintf("fread:0x%x,%d -> 0x%08x\n",FAT2_ftell((FAT_FILE*)hFile),size*count,buffer);
  u32 res=FAT2_fread(buffer,size,count,(FAT_FILE*)hFile);

  return(res);
    
  char* data = (char*)buffer;
  for(u32 idx=0;idx<32;idx++){
    _consolePrintf("%02x,",data[idx]);
  }
  _consolePrint("\n");
  
  return(res);
}

static void* _memchr(const void *buf, int ch, size_t n)
{
  return((void*)memchr(buf,ch,n));
}

static char* _strchr(const char *s, int c)
{
  return((char*)strchr(s,c));
}

static char* _strdup(const char *s)
{
  return(NULL);
  //return((char*)strdup(s));
}

static char* _strpbrk(const char *s, const char *accept)
{
  return((char*)strpbrk(s,accept));
}

static char* _strrchr(const char *s, int c)
{
  return((char*)strrchr(s,c));
}

static char* _strsep(char **stringp, const char *delim)
{
  return(NULL);
  //return((char*)strsep(stringp,delim));
}

static char* _strstr(const char *haystack, const char *needle)
{
  return((char*)strstr(haystack,needle));
}

#define PluginGardMemorySize (256*1024)

static void* _safemalloc(int size)
{
  void *p=malloc(size+PluginGardMemorySize);
  
  if(p!=NULL){
    free(p);
//    _consolePrintf("safemalloc(%d) ok. ",size);
    return(safemalloc(size));
  }
  
  PrintFreeMem();
  _consolePrintf("safemalloc(%d) fail. ",size);
  return(NULL);
}

static void* _calloc(size_t nmemb, size_t size)
{
  void *p=calloc(nmemb+(PluginGardMemorySize/size),size);
  
  if(p!=NULL){
    free(p);
//    _consolePrintf("calloc(%d,%d) ok. ",nmemb,size);
    return(calloc(nmemb,size));
  }
  
  PrintFreeMem();
  _consolePrintf("calloc(%d,%d) fail. ",nmemb,size);
  return(NULL);
}

static void* _malloc(size_t size)
{
  void *p=malloc(size+PluginGardMemorySize);
  
  if(p!=NULL){
    free(p);
//    _consolePrintf("malloc(%d) ok. ",size);
    return(malloc(size));
  }
  
  PrintFreeMem();
  _consolePrintf("malloc(%d) fail. ",size);
  return(NULL);
}

static void* _realloc(void *ptr, size_t size)
{
  return(realloc(ptr,size));
  
  ptr=realloc(ptr,size+PluginGardMemorySize);
  
  if(ptr!=NULL){
    ptr=realloc(ptr,size);
    if(ptr!=NULL){
//      _consolePrintf("realloc(,%d) %08x ok. ",ptr,size);
      return(ptr);
    }
  }
  
  PrintFreeMem();
  _consolePrintf("realloc(%08x,%d) fail. ",ptr,size);
  return(NULL);
}

static void MWin_ProgressShow(char *TitleStr,s32 _Max)
{
  _consolePrintf("PrgShow: %s,%d\n",TitleStr,_Max);
}

static void MWin_ProgressSetPos(s32 _Position)
{
  _consolePrintf("PrgSetPos: %d\n",_Position);
}

static void MWin_ProgressHide(void)
{
  _consolePrintf("PrgHide: ok.\n");
}

static inline const TPlugin_StdLib *Plugin_GetStdLib(void)
{
  static TPlugin_StdLib sPlugin_StdLib={
    _consolePrint,_consolePrintf,
    _consolePrintSet,
    ShowLogHalt,
    MWin_ProgressShow,MWin_ProgressSetPos,MWin_ProgressHide,
    
    MemCopy8CPU,MemCopy16CPU,MemCopy32CPU,
    MemSet16CPU,MemSet32CPU,
    _safemalloc,safefree,
    
    _calloc,_malloc,free,_realloc,
    rand,
    FileSys_fread,FileSys_fseek,FileSys_ftell,
    sprintf,snprintf,
    _memchr,memcmp,memcpy,memmove,memset,
    abs,labs,llabs,fabs,fabsf,
    atof,atoi,atol,atoll,
    strcat,_strchr,strcmp,strcoll,strcpy,strcspn,_strdup,strlen,strncat,strncmp,strncpy,_strpbrk,_strrchr,_strsep,strspn,_strstr,strtok,strxfrm,
    
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
  };
  
  return(&sPlugin_StdLib);
}

//#define ShowPluginInfo

bool DLL_LoadLibrary(TPluginBody *pPB,const TPlugin_StdLib *pStdLib,void *pbin,int binsize)
{
  memset(pPB,0,sizeof(TPluginBody));
  
  TPluginHeader *pPH=&pPB->PluginHeader;
  
  memmove(pPH,pbin,sizeof(TPluginHeader));
  
#ifdef ShowPluginInfo
  _consolePrint("MSP Header\n");
  _consolePrintf("ID=%x ver%d.%d\n",pPH->ID,pPH->VersionHigh,pPH->VersionLow);
  {
    char *pts;
    switch((EPluginType)(pPH->PluginType)){
      case EPT_None: pts="NULL"; break;
      case EPT_Image: pts="Image"; break;
      case EPT_Sound: pts="Sound"; break;
      default: pts="undefined"; break;
    }
    _consolePrintf("PluginType=%x %s\n",pPH->PluginType,pts);
  }
  _consolePrintf("Data 0x%x-0x%x\n",pPH->DataStart,pPH->DataEnd);
  _consolePrintf("got  0x%x-0x%x\n",pPH->gotStart,pPH->gotEnd);
  _consolePrintf("bss  0x%x-0x%x\n",pPH->bssStart,pPH->bssEnd);
  {
    char *str=pPH->info;
    _consolePrintf("Name=%s\n",str);
    while(*str!=0){
      *str++;
    }
    *str++;
    _consolePrintf("Author=%s\n",str);
  }
  _consolePrint("\n");
#endif
  
  pPB->DataSize=binsize;
  pPB->pData=pbin;
  
#ifdef ShowPluginInfo
  _consolePrintf("Plugin LoadAddress=0x%08x\n",(u32)pPB->pData);
#endif
  
  pPB->bssSize=pPH->bssEnd-pPH->bssStart;
  pPB->pbss=safemalloc(pPB->bssSize);
  
  if(pPB->pbss==NULL){
    _consolePrint("LoadLibrary:bss malloc error.\n");
    DLL_FreeLibrary(pPB,false);
    return(false);
  }
  
  memset(pPB->pbss,0,pPB->bssSize);
  
#ifdef ShowPluginInfo
  u32 plug_got_bssbaseofs=pPH->bssStart;
#endif
  u32 plug_got_ofs=pPH->gotStart-pPH->DataStart;
  u32 plug_got_size=pPH->gotEnd-pPH->gotStart;
  
#ifdef ShowPluginInfo
  _consolePrintf("allocbss 0x%08x (0x%xbyte)\n",(u32)pPB->pbss,pPB->bssSize);
  _consolePrintf("got_bssbaseofs=0x%x\n",plug_got_bssbaseofs);
  _consolePrintf("got_ofs=0x%x got_size=0x%x\n",plug_got_ofs,plug_got_size);
#endif
  
  {
    u32 *padr=(u32*)pPB->pData;
    for(u32 idx=64/4;idx<plug_got_ofs/4;idx++){
      u32 adr=padr[idx];
      if((pPH->bssStart<=adr)&&(adr<pPH->bssEnd)){
#ifdef ShowPluginInfo
//        _consolePrintf("b%x:%x ",idx*4,adr);
#endif
        padr[idx]=(u32)pPB->pbss+(adr-pPH->bssStart);
        }else{
        if((pPH->DataStart<=adr)&&(adr<pPH->DataEnd)){
#ifdef ShowPluginInfo
//          _consolePrintf("d%x:%x ",idx*4,adr);
#endif
          padr[idx]=(u32)pPB->pData+(adr-pPH->DataStart);
        }
      }
    }
  }
  
  {
    u32 *padr=(u32*)pPB->pData;
    for(u32 idx=(plug_got_ofs+plug_got_size)/4;idx<((u32)pPH->DataEnd-(u32)pPH->DataStart)/4;idx++){
      u32 adr=padr[idx];
      if((pPH->bssStart<=adr)&&(adr<pPH->bssEnd)){
#ifdef ShowPluginInfo
//        _consolePrintf("b%x:%x ",idx*4,adr);
#endif
        padr[idx]=(u32)pPB->pbss+(adr-pPH->bssStart);
        }else{
        if((pPH->DataStart<=adr)&&(adr<pPH->DataEnd)){
#ifdef ShowPluginInfo
//          _consolePrintf("d%x:%x ",idx*4,adr);
#endif
          padr[idx]=(u32)pPB->pData+(adr-pPH->DataStart);
        }
      }
    }
  }
  
  {
    u32 *padr=(u32*)((u32)pPB->pData+plug_got_ofs);
    
    for(u32 idx=0;idx<plug_got_size/4;idx++){
      u32 adr=padr[idx];
      if((pPH->bssStart<=adr)&&(adr<pPH->bssEnd)){
#ifdef ShowPluginInfo
//        _consolePrintf("b%x:%x ",idx*4,adr);
#endif
        padr[idx]=(u32)pPB->pbss+(adr-pPH->bssStart);
        }else{
        if((pPH->DataStart<=adr)&&(adr<pPH->DataEnd)){
#ifdef ShowPluginInfo
//          _consolePrintf("d%x:%x ",idx*4,adr);
#endif
          padr[idx]=(u32)pPB->pData+(adr-pPH->DataStart);
        }
      }
    }
    
  }
  
  {
    u32 src;
    u32 *pdst;
    
    src=pPH->LoadLibrary;
    if(src==0){
      _consolePrint("LoadLibrary:BootStrap function is NULL.\n");
      DLL_FreeLibrary(pPB,false);
      return(false);
    }
    pdst=(u32*)&pPB->LoadLibrary;
    *pdst=(u32)pPB->pData+(src-pPH->DataStart);
    
    src=pPH->FreeLibrary;
    if(src==0){
      _consolePrint("FreeLibrary:BootStrap function is NULL.\n");
      DLL_FreeLibrary(pPB,false);
      return(false);
    }
    pdst=(u32*)&pPB->FreeLibrary;
    *pdst=(u32)pPB->pData+(src-pPH->DataStart);
    
    src=pPH->QueryInterfaceLibrary;
    if(src==0){
      _consolePrint("QueryInterfaceLibrary:BootStrap function is NULL.\n");
      DLL_FreeLibrary(pPB,false);
      return(false);
    }
    pdst=(u32*)&pPB->QueryInterfaceLibrary;
    *pdst=(u32)pPB->pData+(src-pPH->DataStart);
  }
  
#ifdef ShowPluginInfo
  _consolePrintf("0x%08x LoadLibrary\n",(u32)pPB->LoadLibrary);
  _consolePrintf("0x%08x FreeLibrary\n",(u32)pPB->FreeLibrary);
  _consolePrintf("0x%08x QueryInterfaceLib.\n",(u32)pPB->QueryInterfaceLibrary);
#endif
  
  if(pPB->LoadLibrary==NULL){
    _consolePrint("LoadLibrary:LoadLibrary() is NULL.\n");
    DLL_FreeLibrary(pPB,false);
    return(false);
  }
  
  bool res=pPB->LoadLibrary(pStdLib,(u32)pPB->pData);
  
  if(res==false){
    _consolePrint("LoadLibrary:LoadLibrary() false.\n");
    DLL_FreeLibrary(pPB,false);
    return(false);
  }
  
  pPB->pSL=NULL;
  pPB->pIL=NULL;
  
  switch((EPluginType)(pPH->PluginType)){
    case EPT_None: {
#ifdef ShowPluginInfo
      _consolePrint("LoadLibrary:PluginType == None.\n");
#endif
      DLL_FreeLibrary(pPB,false);
      return(false);
    } break;
    case EPT_Image: {
      pPB->pIL=(TPlugin_ImageLib*)pPB->QueryInterfaceLibrary();
#ifdef ShowPluginInfo
      _consolePrintf("ImageInterfacePtr 0x%08x\n",(u32)pPB->pIL);
#endif
    } break;
    case EPT_Sound: {
      pPB->pSL=(TPlugin_SoundLib*)pPB->QueryInterfaceLibrary();
#ifdef ShowPluginInfo
      _consolePrintf("SoundInterfacePtr 0x%08x\n",(u32)pPB->pSL);
#endif
    } break;
  }
  
  if((pPB->pIL==NULL)&&(pPB->pSL==NULL)){
    _consolePrint("LoadLibrary:not found function list error.\n");
    DLL_FreeLibrary(pPB,false);
    return(false);
  }
  
#ifdef ShowPluginInfo
  _consolePrint("LoadLibrary:Initialized.\n");
#endif
  
  return(true);
}

void DLL_FreeLibrary(TPluginBody *pPB,bool callfree)
{
  if(callfree==true){
    if(pPB!=NULL){
      if(pPB->FreeLibrary!=NULL) pPB->FreeLibrary();
    }
  }
  
  if(pPB->pData!=NULL){
    safefree(pPB->pData); pPB->pData=NULL;
  }
  if(pPB->pbss!=NULL){
    safefree(pPB->pbss); pPB->pbss=NULL;
  }
  
//  memset(pPB,0,sizeof(TPluginBody));
  
#ifdef ShowPluginInfo
  _consolePrint("FreeLibrary:Destroied.\n");
#endif
}

// ------------------------------------------------------------

typedef struct {
  u32 ext;
  EPluginType PluginType;
  char fn[PluginFilenameMax];
} TDLLList;

static int DLLListCount;
TDLLList *DLLList=NULL;

static void DLLList_Regist_Dummy(EPluginType PluginType,const char *pext)
{
  u32 Ext32=0;
  {
    const char *ptmp=pext;
    while((*ptmp!=0)&&(*ptmp!='.')){
      u32 ch=*ptmp++;
      if((0x41<=ch)&&(ch<=0x5a)) ch+=0x20;
      Ext32=(Ext32>>8)|(ch<<24);
    }
    Ext32>>=8;
  }
  if(Ext32==0) return;
  
  DLLList[DLLListCount].ext=Ext32;
  DLLList[DLLListCount].PluginType=PluginType;
  DLLList[DLLListCount].fn[0]=0;
  DLLListCount++;
}

void DLLList_Regist(char *fn)
{
  TPluginHeader PH;
  
  if(Shell_FAT_ReadHeadMSP(fn,&PH,sizeof(TPluginHeader))==false){
    _consolePrintf("%s file open error.\n",fn);
    return;
  }
  
  if(PH.ID!=0x0050534d){
    _consolePrintf("%s is illigal format.\n",fn);
    return;
  }
  if(0x05<PH.VersionHigh){
    _consolePrintf("%s check version error ver%d.%d\n",fn,PH.VersionHigh,PH.VersionLow);
    return;
  }
  
  switch((EPluginType)PH.PluginType){
    case EPT_None: {
      _consolePrintf("%s Plugin type is EPT_None(0)\n",fn);
    } break;
    case EPT_Image: case EPT_Sound: {
      for(int idx=0;idx<PluginHeader_ExtCount;idx++){
        if(PH.ext[idx]!=0){
          DLLList[DLLListCount].ext=PH.ext[idx];
          DLLList[DLLListCount].PluginType=(EPluginType)PH.PluginType;
          strncpy(DLLList[DLLListCount].fn,fn,PluginFilenameMax);
#ifdef ShowPluginInfo
          _consolePrintf("regist:%2d .%s %s\n",DLLListCount,(char*)&DLLList[DLLListCount].ext,DLLList[DLLListCount].fn);
#endif
          DLLListCount++;
        }
      }
    } break;
    default: {
      _consolePrintf("unknown plugin type(%d):%2d %s\n",PH.PluginType,DLLListCount,DLLList[DLLListCount].fn);
    } break;
  }
}

void DLLList_Init(void)
{
  const u32 cntmax=48;
  DLLListCount=cntmax;
  
  DLLList=(TDLLList*)safemalloc(sizeof(TDLLList)*DLLListCount);
  
  DLLListCount=0;
  
  _consolePrint("Plugin Read headers.\n");
  
  DLLList_Regist_Dummy(EPT_Sound,"mp1");
  DLLList_Regist_Dummy(EPT_Sound,"mp2");
  DLLList_Regist_Dummy(EPT_Sound,"mp3");
  
  DLLList_Regist_Dummy(EPT_Image,"jpg");
  DLLList_Regist_Dummy(EPT_Image,"jpe");
  
  PrfStart();
  DLLList_Regist("ogg.msp");
  DLLList_Regist("spc.msp");
  DLLList_Regist("wav.msp");
  DLLList_Regist("tta.msp");
  DLLList_Regist("mikmod.msp");
  if(ShellSet.UseM4APlugin==true) DLLList_Regist("m4a.msp");
  
  DLLList_Regist("bmp.msp");
  DLLList_Regist("png.msp");
  DLLList_Regist("psd.msp");
  DLLList_Regist("gif.msp");
  PrfEnd(0);
  
  _consolePrintf("Plugin Registed. (%dbyte, %d/%d)\n",sizeof(TDLLList),DLLListCount,cntmax);
  if(cntmax<DLLListCount){
    _consolePrintf("Plugin header memory overflow.\n");
    ShowLogHalt();
  }
}

EPluginType DLLList_isSupportFormatFromExt(const char *ExtStr)
{
  if((ExtStr==NULL)||(ExtStr[0]==0)) return(EPT_None);
  
  u32 ext=0;
  
  u32 c;
  
  c=(u32)ExtStr[1];
  if(c!=0){
    if((0x41<=c)&&(c<0x5a)) c+=0x20;
    ext|=c << 0;
    c=(u32)ExtStr[2];
    if(c!=0){
      if((0x41<=c)&&(c<0x5a)) c+=0x20;
      ext|=c << 8;
      c=(u32)ExtStr[3];
      if(c!=0){
        if((0x41<=c)&&(c<0x5a)) c+=0x20;
        ext|=c << 16;
        c=(u32)ExtStr[4];
        if(c!=0){
          if((0x41<=c)&&(c<0x5a)) c+=0x20;
          ext|=c << 24;
        }
      }
    }
  }
  
  for(int idx=0;idx<DLLListCount;idx++){
    if(DLLList[idx].ext==ext){
      return(DLLList[idx].PluginType);
    }
  }
  
  return(EPT_None);
}

EPluginType DLLList_isSupportFormatFromFilenameUnicode(const UnicodeChar *pFilenameUnicode)
{
  if((pFilenameUnicode==NULL)||(pFilenameUnicode[0]==0)) return(EPT_None);
  
  u32 Ext32=0;
  {
    const UnicodeChar *ptmp=pFilenameUnicode;
    while(*ptmp!=0){
      u32 ch=*ptmp++;
      if(ch==(u32)'.'){
        Ext32=0;
        }else{
        if((0x61<=ch)&&(ch<=0x7a)) ch-=0x20;
        Ext32=(Ext32<<8)|ch;
      }
    }
  }
  
  return(DLLList_isSupportFormatExt32(Ext32));
}

EPluginType DLLList_isSupportFormatFromFilenameAlias(const char *pFilenameAlias)
{
  if((pFilenameAlias==NULL)||(pFilenameAlias[0]==0)) return(EPT_None);
  
  u32 Ext32=0;
  {
    const char *ptmp=pFilenameAlias;
    while(*ptmp!=0){
      u32 ch=*ptmp++;
      if(ch==(u32)'.'){
        Ext32=0;
        }else{
        if((0x61<=ch)&&(ch<=0x7a)) ch-=0x20;
        Ext32=(Ext32<<8)|ch;
      }
    }
  }
  
  return(DLLList_isSupportFormatExt32(Ext32));
}

EPluginType DLLList_isSupportFormatExt32(u32 Ext32)
{
  u32 ext=0;
  
  u32 c;
  
  c=(Ext32>>0)&0xff;
  if(c!=0){
    if((0x41<=c)&&(c<0x5a)) c+=0x20;
    ext=(ext<<8)|c;
  }
  c=(Ext32>>8)&0xff;
  if(c!=0){
    if((0x41<=c)&&(c<0x5a)) c+=0x20;
    ext=(ext<<8)|c;
  }
  c=(Ext32>>16)&0xff;
  if(c!=0){
    if((0x41<=c)&&(c<0x5a)) c+=0x20;
    ext=(ext<<8)|c;
  }
  c=(Ext32>>24)&0xff;
  if(c!=0){
    if((0x41<=c)&&(c<0x5a)) c+=0x20;
    ext=(ext<<8)|c;
  }
  
  for(int idx=0;idx<DLLListCount;idx++){
    if(DLLList[idx].ext==ext){
      return(DLLList[idx].PluginType);
    }
  }
  
  return(EPT_None);
}

EPluginType DLLList_GetPluginFilename(const char *extstr,char *resfn)
{
  if(resfn==NULL){
    _consolePrintf("Fatal error: DLLList_GetPluginFilename: resfn is NULL.\n");
    ShowLogHalt();
  }
  
  resfn[0]=0;
  
  if((extstr==NULL)||(extstr[0]==0)) return(EPT_None);
  
  u32 ext=0;
  
  u32 c;
  
  c=(u32)extstr[1];
  if(c!=0){
    if((0x41<=c)&&(c<0x5a)) c+=0x20;
    ext|=c << 0;
    c=(u32)extstr[2];
    if(c!=0){
      if((0x41<=c)&&(c<0x5a)) c+=0x20;
      ext|=c << 8;
      c=(u32)extstr[3];
      if(c!=0){
        if((0x41<=c)&&(c<0x5a)) c+=0x20;
        ext|=c << 16;
        c=(u32)extstr[4];
        if(c!=0){
          if((0x41<=c)&&(c<0x5a)) c+=0x20;
          ext|=c << 24;
        }
      }
    }
  }
  
  for(int idx=0;idx<DLLListCount;idx++){
    if(DLLList[idx].ext==ext){
      strncpy(resfn,DLLList[idx].fn,PluginFilenameMax);
      return(DLLList[idx].PluginType);
    }
  }
  
  return(EPT_None);
}

// ----------------------------------

TPluginBody* DLLList_LoadPlugin(const char *fn)
{
  void *buf;
  s32 size;
  
  Shell_FAT_ReadMSP(fn,&buf,&size);
  
  if((buf==NULL)||(size==0)){
    _consolePrintf("%s file read error.\n",fn);
    return(NULL);
  }
  
  TPluginBody *pPB=(TPluginBody*)safemalloc(sizeof(TPluginBody));
  
  if(pPB==NULL){
    _consolePrint("Memory overflow.\n");
    return(NULL);
  }
  
  if(DLL_LoadLibrary(pPB,Plugin_GetStdLib(),buf,size)==false){
    safefree(pPB); pPB=NULL;
    return(NULL);
  }
  
  return(pPB);
}

void DLLList_FreePlugin(TPluginBody *pPB)
{
  if(pPB==NULL) return;
  
  if(pPB->pIL!=NULL) pPB->pIL->Free();
  if(pPB->pSL!=NULL) pPB->pSL->Free();
  
  DLL_FreeLibrary(pPB,true);
  
  safefree(pPB); pPB=NULL;
}

// ----------------------------

// ----------------------------

