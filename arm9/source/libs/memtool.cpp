
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <NDS.h>

#include "_console.h"
#include "_const.h"

#include "memtool.h"
#include "arm9tcm.h"

#define MEMCHK_COPY(align,p1,p2,p3) { \
  if((p1==NULL)||(p2==NULL)||(p3==0)||(((u32)p1&align)!=0)||(((u32)p2&align)!=0)){ \
    _consolePrintf("COPY. Hooked memory address error. %s%d (%d) p1=0x%x,p2=0x%x,p3=0x%x\n",__FILE__,__LINE__,align,p1,p2,p3); \
    ShowLogHalt(); \
  } \
}

#define MEMCHK_SET(align,p1,p2,p3) { \
  if((p2==NULL)||(p3==0)||(((u32)p2&align)!=0)){ \
    _consolePrintf("SET. Hooked memory address error. %s%d (%d) p1=0x%x,p2=0x%x,p3=0x%x\n",__FILE__,__LINE__,align,p1,p2,p3); \
    ShowLogHalt(); \
  } \
}

#undef MEMCHK_COPY
#define MEMCHK_COPY(align,p1,p2,p3)
#undef MEMCHK_SET
#define MEMCHK_SET(align,p1,p2,p3)

#define CACHE_LINE_SIZE (32)

CODE_IN_ITCM void DCache_FlushRangeOverrun(const void *v,u32 size)
{
//  Flush up. (ダーティーデータをライトバックせずにキャッシュを無効化する）

  u32 va=(u32)v;
  if((va<0x02000000)||(0x02400000<=va)) return;
  
  va&=~(CACHE_LINE_SIZE-1);
  size+=CACHE_LINE_SIZE;
  
  size+=CACHE_LINE_SIZE-1;
  size&=~(CACHE_LINE_SIZE-1);
  
  if(size==0) return;

  while(size!=0){  
	  asm {
	    mcr p15, 0, va, c7, c6, 1
	  }
    va+=CACHE_LINE_SIZE;
    size-=CACHE_LINE_SIZE;
	}
}

CODE_IN_ITCM void DCache_CleanRangeOverrun(const void *v,u32 size)
{
// Clean up. (ダーティーデータをライトバッファに送ってクリアする。キャッシュは有効のまま）

  u32 va=(u32)v;
  if((va<0x02000000)||(0x02400000<=va)) return;

  va&=~(CACHE_LINE_SIZE-1);
  size+=CACHE_LINE_SIZE;
  
  size+=CACHE_LINE_SIZE-1;
  size&=~(CACHE_LINE_SIZE-1);
  
  if(size==0) return;

  while(size!=0){  
	  asm {
      mcr p15, 0, va, c7, c10, 1
	  }
    va+=CACHE_LINE_SIZE;
    size-=CACHE_LINE_SIZE;
	}
}

CODE_IN_ITCM void MemCopy8CPU(const void *src,void *dst,u32 len)
{MEMCHK_COPY(0,src,dst,len);
  if((len&1)==0){
    if( (((u32)src&1)==0) && (((u32)dst&1)==0) ){
      MemCopy16CPU(src,dst,len);
      return;
    }
  }
  
  len>>=0;
  if(len==0) return;
  
  u8 *_src=(u8*)src;
  u8 *_dst=(u8*)dst;
  
  for(u32 idx=0;idx<len;idx++){
    _dst[idx]=_src[idx];
  }
}

CODE_IN_ITCM void MemCopy16CPU(const void *src,void *dst,u32 len)
{MEMCHK_COPY(1,src,dst,len);
  if((len&3)==0){
    if( (((u32)src&3)==0) && (((u32)dst&3)==0) ){
      MemCopy32CPU(src,dst,len);
      return;
    }
  }
  
  len>>=1;
  if(len==0) return;
  
  u16 *_src=(u16*)src;
  u16 *_dst=(u16*)dst;
  
  for(u32 idx=0;idx<len;idx++){
    _dst[idx]=_src[idx];
  }
}

asm CODE_IN_ITCM void MemCopy32CPU(const void *src,void *dst,u32 len)
{/* MEMCHK_COPY(3,src,dst,len); */
c32psrc RN r0
c32pdst RN r1
c32size RN r2
  
  cmp c32size,#0
  bxeq lr
  
  PUSH {r4,r5,r6,r7,r8,r9,r10}
  
  cmp c32size,#4*8
  blo c32set32x1
    
c32set32x8
  ldmia c32psrc!,{r3,r4,r5,r6,r7,r8,r9,r10}
  stmia c32pdst!,{r3,r4,r5,r6,r7,r8,r9,r10}
  subs c32size,c32size,#4*8
  cmp c32size,#4*8
  bhs c32set32x8
      
  cmp c32size,#0
  beq c32setend
   
c32set32x1
  ldr r3,[c32psrc],#4
  subs c32size,c32size,#4
  str r3,[c32pdst],#4
  bne c32set32x1

c32setend
  POP {r4,r5,r6,r7,r8,r9,r10}
  bx lr      
}

CODE_IN_ITCM void MemSet8CPU(u8 v,void *dst,u32 len)
{MEMCHK_SET(0,v,dst,len);
  len>>=0;
  if(len==0) return;
  
  u8 *_dst=(u8*)dst;
  
  for(u32 cnt=0;cnt<len;cnt++){
    _dst[cnt]=v;
  }
}

CODE_IN_ITCM void MemSet16CPU(u16 v,void *dst,u32 len)
{MEMCHK_SET(1,v,dst,len);
  len>>=1;
  if(len==0) return;

  u16 *_dst=(u16*)dst;
  
  for(u32 idx=0;idx<len;idx++){
    _dst[idx]=v;
  }
}

asm CODE_IN_ITCM void MemSet32CPU(u32 v,void *dst,u32 len)
{/* MEMCHK_SET(3,v,dst,len); */
s32data RN r0
s32pbuf RN r1
s32size RN r2
  
  cmp s32size,#0
  bxeq lr
  
  PUSH {r4,r5,r6,r7,r8,r9}
  
  mov r3,s32data
  mov r4,s32data
  mov r5,s32data
  mov r6,s32data
  mov r7,s32data
  mov r8,s32data
  mov r9,s32data
  
  cmp s32size,#4*8
  blo s32set32x1
    
s32set32x8
  stmia s32pbuf!,{s32data,r3,r4,r5,r6,r7,r8,r9}
  subs s32size,s32size,#4*8
  cmp s32size,#4*8
  bhs s32set32x8
      
  cmp s32size,#0
  beq s32setend
   
s32set32x1
  str s32data,[s32pbuf],#4
  subs s32size,s32size,#4
  bne s32set32x1

s32setend
  POP {r4,r5,r6,r7,r8,r9}
  bx lr      
}

// ----------------------------------------------

typedef struct {
  u32 adr,size;
  bool locked;
} Tatype;

#define atypecount (1024)
static Tatype atype[atypecount];

void atype_init(void)
{
  for(u32 idx=0;idx<atypecount;idx++){
    Tatype *patype=&atype[idx];
    patype->adr=0;
    patype->size=0;
    patype->locked=false;
  }
}

void atype_showallocated(void)
{
  _consoleLogPause();
  _consolePrint("Allocated memory information.\n");
  
  for(u32 idx=0;idx<atypecount;idx++){
    Tatype *patype=&atype[idx];
    if(patype->adr!=0){
      _consolePrintf("idx=%d adr=0x%08x size=%d ",idx,patype->adr,patype->size);
      if(patype->locked==false){
        _consolePrint("\n");
        }else{
        _consolePrint("locked.\n");
      }
    }
  }
  
  _consolePrint("------------------\n");
  _consoleLogResume();
}

void safemalloc_halt(void)
{
  atype_showallocated();
  ShowLogHalt();
}

void atype_lockall(void)
{
  for(u32 idx=0;idx<atypecount;idx++){
    Tatype *patype=&atype[idx];
    if(patype->adr!=0) patype->locked=true;
  }
}

void atype_set(u32 adr,u32 size)
{
  for(u32 idx=0;idx<atypecount;idx++){
    Tatype *patype=&atype[idx];
    if(patype->adr==0){
      patype->adr=adr;
      patype->size=size;
      return;
    }
  }
  _consolePrint("Fatal error! atype array overflow.\n");
  safemalloc_halt();
}

void atype_clear(u32 adr)
{
  for(u32 idx=0;idx<atypecount;idx++){
    Tatype *patype=&atype[idx];
    if(patype->adr==adr){
      if(patype->locked==true){
        _consolePrintf("Fatal error. This addres is locked. (0x%08x)\n",adr);
        safemalloc_halt();
        return;
      }
      patype->adr=0;
      patype->size=0;
      return;
    }
  }
  _consolePrintf("Fatal error! can not found clear atype array. (0x%08x)\n",adr);
  safemalloc_halt();
}

u32 atype_getsize(u32 adr)
{
  for(u32 idx=0;idx<atypecount;idx++){
    Tatype *patype=&atype[idx];
    if(patype->adr==adr){
      return(patype->size);
    }
  }
  _consolePrintf("Fatal error! not found atype adr. (0x%08x)\n",adr);
  safemalloc_halt();
  return(0);
}

void atype_checkmemoryleak(void)
{
  bool haltflag=false;
  
  for(u32 idx=0;idx<atypecount;idx++){
    Tatype *patype=&atype[idx];
    if((patype->locked==false)&&(patype->adr!=0)){
      haltflag=true;
      u32 size=patype->size;
      u8 *pbuf=(u8*)patype->adr;
      _consolePrint("Memory leak detected.\n");
      _consolePrintf("adr=0x%08x, size=%d\n",pbuf,size);
      for(u32 idx=0;idx<8;idx++){
        _consolePrintf("%02x,",pbuf[idx]);
      }
      for(u32 idx=0;idx<8;idx++){
        if((0x20<=(u8)pbuf[idx])&&((u8)pbuf[idx]<0xff)){
          _consolePrintf("%c",pbuf[idx]);
          }else{
          _consolePrintf("_");
        }
      }
      _consolePrint("\n");
    }
  }
  
  if(haltflag==true){
    _consolePrint("Halt.\n");
    ShowLogHalt();
  }
}

void atype_checkoverrange(void)
{
  bool haltflag=false;
  
  for(u32 idx=0;idx<atypecount;idx++){
    Tatype *patype=&atype[idx];
    if(patype->adr!=0){
      u32 size=patype->size;
      u8 *pbuf=(u8*)patype->adr;
      
      if((pbuf[-8]!=0xa8)||(pbuf[-7]!=0xa7)||(pbuf[-6]!=0xa6)||(pbuf[-5]!=0xa5)||(pbuf[-4]!=0xa4)||(pbuf[-3]!=0xa3)||(pbuf[-2]!=0xa2)||(pbuf[-1]!=0xa1)||
         (pbuf[size+0]!=0xb0)||(pbuf[size+1]!=0xb1)||(pbuf[size+2]!=0xb2)||(pbuf[size+3]!=0xb3)||(pbuf[size+4]!=0xb4)||(pbuf[size+5]!=0xb5)||(pbuf[size+6]!=0xb6)||(pbuf[size+7]!=0xb7)){
        haltflag=true;
        _consolePrint("Memory check error. Ignore writing code?\n");
        _consolePrintf("adr=0x%08x, size=%d\n",pbuf,size);
        for(u32 idx=8;idx>0;idx--){
          _consolePrintf("%02x,",pbuf[-idx]);
        }
        _consolePrintf("\n");
        for(u32 idx=0;idx<8;idx++){
          _consolePrintf("%02x,",pbuf[idx]);
        }
        _consolePrintf("\n");
        for(u32 idx=0;idx<8;idx++){
          _consolePrintf("%02x,",pbuf[size+idx]);
        }
        _consolePrint("\n");
      }
    }
  }
  
  if(haltflag==true){
    _consolePrint("Halt.\n");
    ShowLogHalt();
  }
}

void *safemalloc(int size)
{
// _consolePrintf("safemalloc(%d);\n",size);

  atype_checkoverrange(); // for High-level debugging. but slow.
  
//  return(malloc(size)); // for Speed.
  
  if(size<=0) return(NULL);
  
  void *ptr=malloc(size+(8*2)); // 先頭直前と終端直後に検査コードを入れる
  
  if(ptr==NULL){
    _consolePrintf("safemalloc(%d) fail allocate error.\n",size);
    return(NULL);
  }
  
//  _consolePrintf("safemalloc(%d)=0x%x\n",size,ptr);

  ptr=(void*)((u32)ptr+8);
  
  atype_set((u32)ptr,size);
  
  if(0){
    u8 *pbuf=(u8*)ptr;
    pbuf[-8]=0xa8;
    pbuf[-7]=0xa7;
    pbuf[-6]=0xa6;
    pbuf[-5]=0xa5;
    pbuf[-4]=0xa4;
    pbuf[-3]=0xa3;
    pbuf[-2]=0xa2;
    pbuf[-1]=0xa1;
    pbuf[size+0]=0xb0;
    pbuf[size+1]=0xb1;
    pbuf[size+2]=0xb2;
    pbuf[size+3]=0xb3;
    pbuf[size+4]=0xb4;
    pbuf[size+5]=0xb5;
    pbuf[size+6]=0xb6;
    pbuf[size+7]=0xb7;
    }else{
    u32 *pbuf32=(u32*)ptr;
    pbuf32[-2]=0xa5a6a7a8;
    pbuf32[-1]=0xa1a2a3a4;
    u8 *pbuf8=(u8*)ptr;
    pbuf8+=size;
    pbuf8[0]=0xb0;
    pbuf8[1]=0xb1;
    pbuf8[2]=0xb2;
    pbuf8[3]=0xb3;
    pbuf8[4]=0xb4;
    pbuf8[5]=0xb5;
    pbuf8[6]=0xb6;
    pbuf8[7]=0xb7;
  }
  
//  _consolePrintf("[0x%x,%d]",(u32)ptr,size);
  return(ptr);
}

void safefree(void *ptr)
{
  atype_checkoverrange();
  
//  _consolePrintf("free(0x%x)\n",ptr);
  
  if(ptr==NULL){
    _consolePrint("safefree Request NullPointer.\n");
    safemalloc_halt();
    return;
  }
  
//  _consolePrintf("safefree(0x%x)=0d\n",ptr,atype_getsize((u32)ptr));
  
  {
    u32 size=atype_getsize((u32)ptr);
    u8 *pbuf=(u8*)ptr;
    
    if((pbuf[-8]!=0xa8)||(pbuf[-7]!=0xa7)||(pbuf[-6]!=0xa6)||(pbuf[-5]!=0xa5)||(pbuf[-4]!=0xa4)||(pbuf[-3]!=0xa3)||(pbuf[-2]!=0xa2)||(pbuf[-1]!=0xa1)||
       (pbuf[size+0]!=0xb0)||(pbuf[size+1]!=0xb1)||(pbuf[size+2]!=0xb2)||(pbuf[size+3]!=0xb3)||(pbuf[size+4]!=0xb4)||(pbuf[size+5]!=0xb5)||(pbuf[size+6]!=0xb6)||(pbuf[size+7]!=0xb7)){
      _consolePrint("Memory check error. Ignore writing code?\n");
      _consolePrintf("adr=0x%08x, size=%d\n",pbuf,size);
      for(u32 idx=8;idx>0;idx--){
        _consolePrintf("%02x,",pbuf[-idx]);
      }
      _consolePrint("\n");
      for(u32 idx=0;idx<24;idx++){
        _consolePrintf("%02x,",pbuf[size+idx-8]);
      }
      _consolePrint("\n");
      safemalloc_halt();
    }
    
  }
  
  atype_clear((u32)ptr);
  
  ptr=(void*)((u32)ptr-8);
  free(ptr);
}

static void PrintFreeMem_Simple(void)
{
  const u32 FreeMemSeg=2*1024;
  
  for(u32 i=1*FreeMemSeg;i<4096*1024;i+=FreeMemSeg){
    void *ptr=malloc(i);
    if(ptr==NULL){
      _consolePrintf("FreeMem=%dbyte\n",i-FreeMemSeg);
      break;
    }
    free(ptr); ptr=NULL;
  }
}

void PrintFreeMem(void)
{
  PrintFreeMem_Simple(); return;
  
  const u32 maxsize=4*1024*1024;
  const u32 segsize=1*1024;
  const u32 count=maxsize/segsize;
  u32 *pptrs=(u32*)malloc(count*4);
  
  u32 FreeMemSize=0;
  u32 MaxBlockSize=0;
  
  for(u32 idx=0;idx<count;idx++){
    u32 size=maxsize-(segsize*idx);
    pptrs[idx]=(u32)malloc(size);
    if(pptrs[idx]!=0){
      FreeMemSize+=size;
      if(MaxBlockSize<size) MaxBlockSize=size;
    }
  }
  
  _consolePrintf("AccuracyFreeMem=%dbyte (MaxBlockSize=%dbyte)\n",FreeMemSize,MaxBlockSize);
  
  for(u32 idx=0;idx<count;idx++){
    if(pptrs[idx]!=0){
      free((void*)pptrs[idx]); pptrs[idx]=0;
    }
  }
  
  free(pptrs); pptrs=NULL;
}

u32 GetMaxMemoryBlockSize(void)
{
  u32 maxsize=0;
  
  const u32 FreeMemSeg=2*1024;
  for(u32 i=4*1024*1024;i!=0;i-=FreeMemSeg){
    void *ptr=malloc(i);
    if(ptr!=NULL){
      free(ptr); ptr=NULL;
      maxsize=i;
      break;
    }
  }
  
  return(maxsize);
}

void MemClearAllFreeBlocks(void)
{
  const u32 maxsize=4*1024*1024;
  const u32 segsize=512;
  const u32 count=maxsize/segsize;
  u32 *pptrs=(u32*)malloc(count*4);
  
  u32 FreeMemSize=0;
  u32 MaxBlockSize=0;
  
  for(u32 idx=0;idx<count;idx++){
    u32 size=maxsize-(segsize*idx);
    pptrs[idx]=(u32)malloc(size);
    if(pptrs[idx]!=0){
      FreeMemSize+=size;
      if(MaxBlockSize<size) MaxBlockSize=size;
      _consolePrintf("Clear block 0x%08x %dbyte.\n",pptrs[idx],size);
      MemSet32CPU(0,(void*)pptrs[idx],size);
    }
  }
  
  _consolePrintf("AccuracyFreeMem=%dbyte (MaxBlockSize=%dbyte)\n",FreeMemSize,MaxBlockSize);
  
  for(u32 idx=0;idx<count;idx++){
    if(pptrs[idx]!=0){
      free((void*)pptrs[idx]); pptrs[idx]=0;
    }
  }
  
  free(pptrs); pptrs=NULL;
}
