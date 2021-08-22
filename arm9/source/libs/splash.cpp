
#include <NDS.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "_console.h"
#include "_consoleWriteLog.h"
#include "_const.h"
#include "maindef.h"

#include "fat2.h"

#include "strtool.h"
#include "memtool.h"
#include "shell.h"
#include "glib/glib.h"
#include "zlibhelp.h"

#include "splash.h"
#include "extmem.h"

static FAT_FILE *pfh;

typedef struct {
  u32 VSyncCount;
  u32 FileOffset;
  u32 ImageSize;
  u32 b15BufCount;
} TScreen;

#define Flags_WaitForTerminate (1 << 0)
#define Flags_AlreadyAllDraw (1 << 1)

static u32 Flags;
static u32 ScreenCount;
static TScreen *pScreen;

static bool EndFlag;
static s32 LastFrameIndex;
static vu32 VSyncCount;

static u16 *pBetaBM;

void Splash_IRQVSYNC(void)
{
  VSyncCount++;
}

void Splash_Init(void)
{
  pfh=Shell_FAT_fopen_Data("splash.ani");
  if(pfh==NULL) return;
  
  pBetaBM=NULL;
  
  EndFlag=false;
  LastFrameIndex=-1;
  VSyncCount=0;
  
  FAT2_fread(&Flags,1,4,pfh);
  Flags&=~Flags_AlreadyAllDraw;
  FAT2_fread(&ScreenCount,1,4,pfh);
  
  pScreen=(TScreen*)malloc(ScreenCount*sizeof(TScreen));
  
  for(u32 idx=0;idx<ScreenCount;idx++){
    u32 buf[4];
    FAT2_fread(buf,4,4,pfh);
    TScreen *ps=&pScreen[idx];
    ps->VSyncCount=buf[0];
    ps->FileOffset=buf[1];
    ps->ImageSize=buf[2];
    ps->b15BufCount=buf[3];
//    _consolePrintf("%d: vs=%d, fo=%d, is=%d, b15=%d.\n",idx, ps->VSyncCount, ps->FileOffset, ps->ImageSize, ps->b15BufCount);
  }
  
  FAT2_fseek(pfh,0,SEEK_SET);

  REG_IME=0;  
  VSyncCount=0;
  Splash_Update();
  REG_IME=1;
  
  if((Flags&Flags_WaitForTerminate)!=0){
    while(Splash_Update()==true){
    }
  }
}

void Splash_Free(void)
{
  if(pfh==NULL) return;
  
  if((Flags&Flags_AlreadyAllDraw)!=0){
    while(Splash_Update()==true){
    }
  }
  
  FAT2_fclose(pfh); pfh=NULL;

  if(pBetaBM!=NULL){
    free(pBetaBM); pBetaBM=NULL;
  }
  
  EndFlag=false;
  LastFrameIndex=0;
  
  Flags=0;
  ScreenCount=0;
  
  if(pScreen!=NULL){
    free(pScreen); pScreen=NULL;
  }
}

static void Splash_DrawInfo_ins_DrawText(u32 x,u32 y,const char *pstr)
{
  u16 col;
  
  col=RGB15(16,16,16)|BIT15;
  pScreenSub->pCanvas->SetFontTextColor(col);
  pScreenSub->pCanvas->TextOutA(x-1,y-1,pstr);
  
  col=RGB15(31,31,31)|BIT15;
  pScreenSub->pCanvas->SetFontTextColor(col);
  pScreenSub->pCanvas->TextOutA(x+0,y+0,pstr);
}

static void Splash_DrawInfo(void)
{
  u32 x=8,y=ScreenHeight-64;
  
  const char str[]=ROMTITLE " " ROMVERSION " booting...";
  Splash_DrawInfo_ins_DrawText(x,y,str);
  y+=32;
  
  u32 maxmem=extmem_GetMaxSizeByte();
  if(maxmem!=0){
    char str[128];
    snprintf(str,64,"%d KByte %s detected.",maxmem/1024,extmem_GetID());
    Splash_DrawInfo_ins_DrawText(x,y,str);
    }else{
    Splash_DrawInfo_ins_DrawText(x,y,"Extention memory was not found.");
  }
}

/*
static inline void ins_convpal16to32(u16 *ppal16,vu32 *ppal32,u32 palcnt)
{
  for(u32 idx=0;idx<palcnt;idx++){
    ppal32[idx]=ppal16[idx];
  }
}

static void Splash_Update_Draw_16pal(u32 FrameIndex)
{
  TScreen *ps=&pScreen[FrameIndex];
  
  TZLIBData zd;
  zd.SrcSize=ps->FileSize;
  zd.pSrcBuf=(u8*)safemalloc(zd.SrcSize);
  zd.DstSize=0;
  zd.pDstBuf=(u8*)pScreenSub->GetVRAMBuf();
  
  FAT2_fseek(pfh,ps->FileOffset,SEEK_SET);
  FAT2_fread(zd.pSrcBuf,1,zd.SrcSize,pfh);
  
  u32 palcnt=*(u32*)zd.pSrcBuf;
  u16 *ppal16=(u16*)&zd.pSrcBuf[4];
  u32 pal32[256];
  
  ins_convpal16to32(ppal16,pal32,palcnt);
  
  { // Enable VRAM buffer write cache
    u32 r0=7;
    __asm {
      mcr p15, 0, r0, c3, c0, 0
    }
  }
  
  fastlzss16decpalasm_decode(&zd.pSrcBuf[4+(palcnt*2)],zd.pDstBuf,pal32);
  
  { // Disable VRAM buffer write cache
    u32 r0=6;
    __asm {
      mcr p15, 0, r0, c3, c0, 0
    }
  }
  
  safefree(zd.pSrcBuf); zd.pSrcBuf=NULL;
}
*/

static void Splash_Update_Draw_Beta(u32 FrameIndex)
{
  TScreen *ps=&pScreen[FrameIndex];
  
  u32 ImageSize=ps->ImageSize;
  u32 b15BufCount=ps->b15BufCount;
  
//  _consolePrintf("Splash(%d): fofs=%d, b15BufCount=%d, ImageSize=%d.\n",FrameIndex,ps->FileOffset,b15BufCount,ImageSize);
  
  if(ImageSize!=b15BufCount){
    _consolePrintf("Fatal error: Splash: Illigal key frame?\n");
    ShowLogHalt();
  }
  
  if(pBetaBM!=NULL){
    _consolePrintf("Fatal error: Splash: Exists multiple key frame?\n");
    ShowLogHalt();
  }
  
  pBetaBM=(u16*)malloc(ImageSize*2);
  
  FAT2_fseek(pfh,ps->FileOffset,SEEK_SET);
  FAT2_fread(pBetaBM,1,ImageSize*2,pfh);
  
  { // Enable VRAM buffer write cache
    u32 r0=7;
    __asm {
      mcr p15, 0, r0, c3, c0, 0
    }
  }
  
  MemCopy32CPU(pBetaBM,pScreenSub->GetVRAMBuf(),ImageSize*2);
  
  { // Disable VRAM buffer write cache
    u32 r0=6;
    __asm {
      mcr p15, 0, r0, c3, c0, 0
    }
  }
}

static void Splash_Update_Draw_BetaZLIB(u32 FrameIndex)
{
  TScreen *ps=&pScreen[FrameIndex];
  
  u32 ImageSize=ps->ImageSize;
  
//  _consolePrintf("Splash(%d): fofs=%d, b15BufCount=%d, ImageSize=%d.\n",FrameIndex,ps->FileOffset,b15BufCount,ImageSize);
  
  if(pBetaBM!=NULL){
    _consolePrintf("Fatal error: Splash: Exists multiple key frame?\n");
    ShowLogHalt();
  }
  
  pBetaBM=(u16*)malloc(ImageSize*2);
  
  TZLIBData z;
  
  z.SrcSize=ps->b15BufCount*2;
  z.pSrcBuf=(u8*)safemalloc(z.SrcSize);
  z.DstSize=ImageSize*2;
  z.pDstBuf=(u8*)pBetaBM;

  FAT2_fseek(pfh,ps->FileOffset,SEEK_SET);
  FAT2_fread(z.pSrcBuf,1,z.SrcSize,pfh);
  
  if(zlibdecompress(&z)==false){
    _consolePrintf("Fatal error: Splash: Key frame decompress error.\n");
    ShowLogHalt();
  }
  
  if(z.pSrcBuf!=NULL){
    safefree(z.pSrcBuf); z.pSrcBuf=NULL;
  }

  { // Enable VRAM buffer write cache
    u32 r0=7;
    __asm {
      mcr p15, 0, r0, c3, c0, 0
    }
  }
  
  MemCopy32CPU(pBetaBM,pScreenSub->GetVRAMBuf(),ImageSize*2);
  
  { // Disable VRAM buffer write cache
    u32 r0=6;
    __asm {
      mcr p15, 0, r0, c3, c0, 0
    }
  }
}

static void Splash_Update_Draw_Diff(u32 FrameIndex)
{
  TScreen *ps=&pScreen[FrameIndex];
  
  u32 ImageSize=ps->ImageSize;
  u32 b15BufCount=ps->b15BufCount;
  u16 *pb15Buf=(u16*)safemalloc(b15BufCount*2);
  u16 *pbm=(u16*)safemalloc(ImageSize*2);
  
//  _consolePrintf("Splash(%d): fofs=%d, b15BufCount=%d, ImageSize=%d.\n",FrameIndex,ps->FileOffset,b15BufCount,ImageSize);
  
  FAT2_fseek(pfh,ps->FileOffset,SEEK_SET);
  FAT2_fread(pb15Buf,1,b15BufCount*2,pfh);
  
  if(pBetaBM==NULL){
    _consolePrintf("Fatal error: Splash: Not found key frame.\n");
    ShowLogHalt();
  }
  
  MemCopy32CPU(pBetaBM,pbm,ImageSize*2);
  
  {
    u16 *psrc=pb15Buf;
    u16 *pdst=pbm;
    for(u32 idx=0;idx<b15BufCount;idx++){
      u16 data=*psrc++;
      if((data&BIT15)==0){
        pdst+=data;
        }else{
        *pdst++=data;
      }
    }
  }
  
  { // Enable VRAM buffer write cache
    u32 r0=7;
    __asm {
      mcr p15, 0, r0, c3, c0, 0
    }
  }
  
  MemCopy32CPU(pbm,pScreenSub->GetVRAMBuf(),ImageSize*2);
  
  { // Disable VRAM buffer write cache
    u32 r0=6;
    __asm {
      mcr p15, 0, r0, c3, c0, 0
    }
  }
  
  if(pb15Buf!=NULL){
    safefree(pb15Buf); pb15Buf=NULL;
  }
  if(pbm!=NULL){
    safefree(pbm); pbm=NULL;
  }
}

bool Splash_Update(void)
{
  if(pfh==NULL) return(false);
  
  if(EndFlag==true) return(false);
  
  s32 curidx=0;
  
  for(u32 idx=0;idx<ScreenCount;idx++){
    if(pScreen[idx].VSyncCount<=VSyncCount) curidx=idx;
  }
  
  if(curidx==LastFrameIndex) return(true);
  
  if(curidx==0){
    Splash_Update_Draw_BetaZLIB(curidx);
    Splash_DrawInfo();
    }else{
    Splash_Update_Draw_Diff(curidx);
  }
  LastFrameIndex=curidx;
  
  if((curidx+1)==ScreenCount){
    EndFlag=true;
    return(false);
  }
 
  return(true);
}

