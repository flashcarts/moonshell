
#include <NDS.h>

#include <stdio.h>
#include <stdlib.h>

#include "_console.h"
#include "_consoleWriteLog.h"
#include "_const.h"

#include "skin.h"

#include "maindef.h"
#include "memtool.h"
#include "shell.h"
#include "splash.h"
#include "strtool.h"
#include "procstate.h"

#include "cstream_fs.h"

#include "zlibhelp.h"

#include "extmem.h"

Textmem extmem;

// -------------------------

static u32 GetMaxSizeByte(u32 TopAddr)
{
  if(TopAddr==NULL) return(0);
  
  u32 maxsize=32*1024*1024;
  vu32 *ptmp=(vu32*)TopAddr;
  
  for(u32 idx=0;idx<maxsize/4;idx+=256){
    ptmp[idx]=(u32)&ptmp[idx];
    if(TopAddr!=ptmp[0]) break;
  }
  ptmp[0]=TopAddr;
  
  for(u32 idx=0;idx<maxsize/4;idx+=256){
    if(ptmp[idx]!=((u32)&ptmp[idx])){
      if(idx==0){
        return(0);
        }else{
        return(idx*4);
      }
      break;
    }
  }
  
  return(maxsize);
}

// -------------------------

extern u32 extmem_RawMem_Start(void);
extern u32 extmem_M3ExtPack_Start(void);

void extmem_Init(void)
{
  Textmem *pem=&extmem;
  
  pem->pID=NULL;
  pem->MaxSizeByte=0;
  pem->TopAddr=0;
  pem->CurAddr=0;
  pem->TermAddr=0;
  pem->AllocateCount=0;
  
  u32 RawMem_TopAddr=extmem_RawMem_Start();
  u32 RawMem_MaxSizeByte=GetMaxSizeByte(RawMem_TopAddr);
  u32 M3ExtPack_TopAddr=extmem_M3ExtPack_Start();
  u32 M3ExtPack_MaxSizeByte=GetMaxSizeByte(M3ExtPack_TopAddr);
  
  u32 maxsize=0;
  
  if(maxsize<RawMem_MaxSizeByte) maxsize=RawMem_MaxSizeByte;
  if(maxsize<M3ExtPack_MaxSizeByte) maxsize=M3ExtPack_MaxSizeByte;
  
  if(maxsize==RawMem_MaxSizeByte){
    pem->pID="Raw memory pack";
    pem->MaxSizeByte=RawMem_MaxSizeByte;
    pem->TopAddr=RawMem_TopAddr;
    return;
  }
  
  if(maxsize==M3ExtPack_MaxSizeByte){
    pem->pID="M3 extention pack";
    pem->MaxSizeByte=M3ExtPack_MaxSizeByte;
    pem->TopAddr=M3ExtPack_TopAddr;
    return;
  }
}

void extmem_Start(void)
{
  Textmem *pem=&extmem;
  
  pem->CurAddr=pem->TopAddr;
  pem->TermAddr=pem->TopAddr+pem->MaxSizeByte;
  pem->AllocateCount=0;
}

void* extmem_malloc(u32 size)
{
  Textmem *pem=&extmem;
  
  if(pem->MaxSizeByte==0) return(NULL);
  
  size=(size+3)&~3;
  
  u32 StartAddr=pem->CurAddr;
  u32 EndAddr=StartAddr+size;
  
  if(pem->TermAddr<=EndAddr) return(NULL);
  
  pem->CurAddr=EndAddr;
  pem->AllocateCount++;
  return((void*)StartAddr);
}

void extmem_ShowMemoryInfo(void)
{
  Textmem *pem=&extmem;
  
  if(pem->MaxSizeByte==0){
    _consolePrintf("extmem: Extention memory was not found.");
    return;
  }
  
  _consolePrintf("extmem: ID='%s' MaxSizeByte=0x%08x TopAddr=0x%08x.\n",pem->pID,pem->MaxSizeByte,pem->TopAddr);
}

void extmem_ShowMallocInfo(void)
{
  Textmem *pem=&extmem;
  
  if(pem->MaxSizeByte==0) return;
  
  _consolePrintf("extmem: Total=%dbyte, Used=%dbyte, Free=%dbyte, Allocated=%d.\n",pem->TermAddr-pem->TopAddr,pem->CurAddr-pem->TopAddr,pem->TermAddr-pem->CurAddr,pem->AllocateCount);
}

const char* extmem_GetID(void)
{
  return(extmem.pID);
}

u32 extmem_GetMaxSizeByte(void)
{
  return(extmem.MaxSizeByte);
}

u32 extmem_GetTopAddr(void)
{
  return(extmem.TopAddr);
}

