
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
#include "dll.h"

#include "launchstate.h"

#include "shell.h"
#include "../../ipc6.h"

#define CurrentLaunchStateVersion (5)

TLaunchState LaunchState;

static void DataInit(void)
{
  TLaunchState *pstate=&LaunchState;
  
  pstate->Version=CurrentLaunchStateVersion;
  
  pstate->LastTab=ELST_NDS;
  
  for(u32 tabsidx=0;tabsidx<LaunchState_TabsCount;tabsidx++){
    TLaunchState_Tab *ptab=&pstate->Tabs[tabsidx];
    ptab->FilesCount=0;
    for(u32 fileidx=0;fileidx<LaunchState_Tab_FilesCountMax;fileidx++){
      ptab->FullPathUnicode[fileidx][0]=(UnicodeChar)0;
    }
  }
}

void LaunchState_Init(void)
{
  DataInit();
}

// ----------------------------------------------------------------------

static bool LaunchState_TabFile_isStoredFile(TLaunchState_Tab *pTab,const UnicodeChar *pFullPathUnicode)
{
  for(u32 idx=0;idx<pTab->FilesCount;idx++){
    if(Unicode_isEqual_NoCaseSensitive(pFullPathUnicode,pTab->FullPathUnicode[idx])==true) return(true);
  }
  return(false);
}

static void LaunchState_TabFile_AddLast(TLaunchState_Tab *pTab,const UnicodeChar *pPathUnicode,const UnicodeChar *pFilenameUnicode)
{
  if(LaunchState_Tab_FilesCountMax<=pTab->FilesCount) return;
  
  const UnicodeChar *pFullPathUnicode=ConvertFull_MargeFromSplit(pPathUnicode,pFilenameUnicode);
  
  if(LaunchState_TabFile_isStoredFile(pTab,pFullPathUnicode)==true) return;
  
  Unicode_Copy(pTab->FullPathUnicode[pTab->FilesCount],pFullPathUnicode);
  pTab->FilesCount++;
}

static void LaunchState_Load_ins_AddLaunchFiles(TLaunchState_Tab *pTab)
{
  if(LaunchState_Tab_FilesCountMax<=pTab->FilesCount) return;
  
  UnicodeChar LaunchPathUnicode[MaxFilenameLength];
  StrConvert_Ank2Unicode(DefaultDataPath "/launch",LaunchPathUnicode);
  
  const char *pLaunchPathAlias=ConvertFull_Unicode2Alias(LaunchPathUnicode,NULL);
  
  if(FAT2_chdir_Alias(pLaunchPathAlias)==false){
    _consolePrintf("Fatal error: Can not found launch folder.\n");
    ShowLogHalt();
  }
   
  const char *pafn;
  u32 FAT_FileType=FAT2_FindFirstFile(&pafn);
  
  while(FAT_FileType!=FT_NONE){
    const UnicodeChar *pufn=FAT2_GetLongFilenameUnicode();
    if(pufn==NULL){
      _consolePrintf("Fatal error: Can not read unicode filename.\n");
      ShowLogHalt();
      return;
    }
    switch(FAT_FileType){
      case FT_NONE: break;
      case FT_DIR: break;
      case FT_FILE: {
        u32 Ext32=0;
        {
          const char *ptmp=pafn;
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
        
        if(MakeExt32(0,'N','D','S')==Ext32){
          _consolePrintf("Add launch file. [%s]\n",pafn);
          LaunchState_TabFile_AddLast(pTab,LaunchPathUnicode,pufn);
        }
      } break;
    }
    
    FAT_FileType=FAT2_FindNextFile(&pafn);
  }
}

static void LaunchState_DeleteIndex(TLaunchState_Tab *pTab,u32 delidx)
{
  for(s32 idx=delidx+1;idx<pTab->FilesCount;idx++){
    Unicode_Copy(pTab->FullPathUnicode[idx-1],pTab->FullPathUnicode[idx]);
    _consolePrintf("Launch edit: copy %d from %d.\n",idx-1,idx);
  }
  pTab->FilesCount--;
  _consolePrintf("Launch edit: FilesCount set to %d.\n",pTab->FilesCount);
}

static void LaunchState_AddTop(TLaunchState_Tab *pTab,const UnicodeChar *pFullPathUnicode)
{
  if(pTab->FilesCount==LaunchState_Tab_FilesCountMax){
    _consolePrintf("Fatal error: Launch add error. item full.\n");
    ShowLogHalt();
  }
  
  for(s32 idx=pTab->FilesCount-1;idx>=0;idx--){
    Unicode_Copy(pTab->FullPathUnicode[idx+1],pTab->FullPathUnicode[idx]);
    _consolePrintf("Launch edit: copy %d from %d.\n",idx+1,idx);
  }
  pTab->FilesCount++;
  _consolePrintf("Launch edit: FilesCount set to %d.\n",pTab->FilesCount);
  
  _consolePrintf("Launch edit: New item set to 0.\n");
  Unicode_Copy(pTab->FullPathUnicode[0],pFullPathUnicode);
}

void LaunchState_Add(ELaunchState_Tabs Tabs,const UnicodeChar *pFullPathUnicode)
{
  _consoleLogPause();
  
  TLaunchState_Tab *pTab=&LaunchState.Tabs[Tabs];
  
  if(pTab->FilesCount!=0){
    for(s32 idx=pTab->FilesCount-1;idx>=0;idx--){
      if(Unicode_isEqual_NoCaseSensitive(pFullPathUnicode,pTab->FullPathUnicode[idx])==true){
        _consolePrintf("Launch edit: Delete equal item %d.\n",idx);
        LaunchState_DeleteIndex(pTab,idx);
      }
    }
  }
  
  if(pTab->FilesCount==LaunchState_Tab_FilesCountMax){
    _consolePrintf("Launch edit: Delete last item.\n");
    LaunchState_DeleteIndex(pTab,pTab->FilesCount-1);
  }
  
  _consolePrintf("Launch edit: Add to top.\n");
  LaunchState_AddTop(pTab,pFullPathUnicode);
  
  _consolePrintf("Launch edit: end.\n");
  _consoleLogResume();
}

static void LaunchState_DeleteNotExistsFile(TLaunchState_Tab *pTab)
{
  for(s32 idx=pTab->FilesCount-1;idx>=0;idx--){
    if(pTab->FullPathUnicode[idx][0]==0){
      LaunchState_DeleteIndex(pTab,idx);
      }else{
      if(FullPath_FileExistsUnicode(pTab->FullPathUnicode[idx])==false){
        _consolePrintf("Launch edit: Delete not exists item %d.\n",idx);
        LaunchState_DeleteIndex(pTab,idx);
      }
    }
  }
}

// ----------------------------------------------------------------------

void LaunchState_Load(void)
{
  DataInit();
  
  const char *fn=DefaultDataPath "/" LaunchFilename;
  
  _consolePrintf("Load launch data '%s'\n",fn);
  
  FAT_FILE *pf=NULL;
  const char *palias=ConvertFullPath_Ansi2Alias(fn);
  if(palias!=NULL) pf=FAT2_fopen_AliasForRead(palias);
  
  if(pf==NULL){
    _consolePrintf("not found '%s'. load default.\n",fn);
    }else{
    u32 filesize=FAT2_GetFileSize(pf);
    
    static u32 ver;
    if(FAT2_fread(&ver,1,4,pf)!=4){
      _consolePrint("This file is size too short. load default.\n");
      DataInit();
      }else{
      if(ver!=CurrentLaunchStateVersion){
        _consolePrint("This file is old version launch data. load default.\n");
        DataInit();
        }else{
        _consolePrintf("launch data load from '%s'.\n",fn);
        
        TZLIBData ZLIBData;
        
        ZLIBData.SrcSize=filesize-4;
        ZLIBData.pSrcBuf=(u8*)safemalloc(ZLIBData.SrcSize);
        ZLIBData.DstSize=sizeof(TLaunchState);
        ZLIBData.pDstBuf=(u8*)safemalloc(ZLIBData.DstSize);
        
        if(FAT2_fread(ZLIBData.pSrcBuf,1,ZLIBData.SrcSize,pf)!=ZLIBData.SrcSize){
          _consolePrint("This file is size too short. load default.\n");
          DataInit();
          }else{
          if(zlibdecompress(&ZLIBData)==false){
            _consolePrintf("ZLIB decompress error. load default.\n");
            DataInit();
            }else{
            MemCopy8CPU(ZLIBData.pDstBuf,(u8*)&LaunchState,sizeof(TLaunchState));
          }
        }
        if(ZLIBData.pSrcBuf!=NULL){
          safefree(ZLIBData.pSrcBuf); ZLIBData.pSrcBuf=NULL;
        }
        if(ZLIBData.pDstBuf!=NULL){
          safefree(ZLIBData.pDstBuf); ZLIBData.pDstBuf=NULL;
        }
      }
    }
    
    FAT2_fclose(pf);
  }
  
  for(u32 idx=0;idx<LaunchState_TabsCount;idx++){
    TLaunchState_Tab *pTab=&LaunchState.Tabs[idx];
    LaunchState_DeleteNotExistsFile(pTab);
  }
  
  LaunchState_Load_ins_AddLaunchFiles(&LaunchState.Tabs[ELST_Launch]);
}

static void LaunchState_Save_inc_FillFutter(UnicodeChar *pustr)
{
  u32 idx=0;
  
  for(;idx<MaxFilenameLength;idx++){
    if(pustr[idx]==0) break;
  }
  for(;idx<MaxFilenameLength;idx++){
    pustr[idx]=0;
  }
}

void LaunchState_Save(void)
{
  REG_IME=0;
  
  {
    TLaunchState *pstate=&LaunchState;
    for(u32 tabsidx=0;tabsidx<LaunchState_TabsCount;tabsidx++){
      TLaunchState_Tab *ptab=&pstate->Tabs[tabsidx];
      for(u32 fileidx=0;fileidx<LaunchState_Tab_FilesCountMax;fileidx++){
        LaunchState_Save_inc_FillFutter(ptab->FullPathUnicode[fileidx]);
      }
    }
  }
  
  const char *fn=DefaultDataPath "/" LaunchFilename;
  
  _consolePrintf("Save launch data '%s'\n",fn);
  
  TZLIBData ZLIBData;
  
  ZLIBData.SrcSize=sizeof(TLaunchState);
  ZLIBData.pSrcBuf=(u8*)&LaunchState;
  ZLIBData.DstSize=0;
  ZLIBData.pDstBuf=NULL;
  
  if(zlibcompress(&ZLIBData,0)==false){
    _consolePrintf("Fatal error: ZLIB compress error!\n");
    ShowLogHalt();
  }
  
  FAT_FILE *pf=NULL;
  const char *palias=ConvertFullPath_Ansi2Alias(fn);
  if(palias!=NULL) pf=FAT2_fopen_AliasForWrite(palias);
  
  if(pf==NULL){
    _consolePrintf("launch data file open error or writing. '%s'\n",fn);
    ShowLogHalt();
    }else{
    static u32 ver=CurrentLaunchStateVersion;
    if(FAT2_fwrite(&ver,1,4,pf)!=4){
      _consolePrintf("launch data file write error. '%s'\n",fn);
      ShowLogHalt();
    }
    if(FAT2_fwrite(ZLIBData.pDstBuf,1,ZLIBData.DstSize,pf)!=ZLIBData.DstSize){
      _consolePrintf("launch data file write error. '%s'\n",fn);
      ShowLogHalt();
    }
    FAT2_fclose(pf);
  }
  
  _consolePrintf("Saved launch data. %d->%dbyte.\n",ZLIBData.SrcSize,ZLIBData.DstSize);
  
  ZLIBData.DstSize=0;
  if(ZLIBData.pDstBuf!=NULL){
    safefree(ZLIBData.pDstBuf); ZLIBData.pDstBuf=NULL;
  }
  
  REG_IME=1;
}

