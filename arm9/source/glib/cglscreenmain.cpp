
#include <stdlib.h>
#include <NDS.h>

#include "glib.h"
#include "glmemtool.h"
#include "cglscreen.h"

#define VRAMBuf ((u16*)(0x06000000))

// 15bit bitmap
#define BG2_CR_BASE_15bitBM (BG_BMP16_256x256 | BG_BMP_BASE(0))
#define BG3_CR_BASE_15bitBM (BG_BMP16_256x256 | BG_BMP_BASE(6))

CglScreenMain::CglScreenMain(void)
{
  BG2_CR = 0;
  BG3_CR = 0;
  
  {
    BG2_XDX = 1 << 8;
    BG2_XDY = 0 << 8;
    BG2_YDX = 0 << 8;
    BG2_YDY = 1 << 8;
    BG2_CX = 0;
    BG2_CY = 0;
  }
  {
    BG3_XDX = 1 << 8;
    BG3_XDY = 0 << 8;
    BG3_YDX = 0 << 8;
    BG3_YDY = 1 << 8;
    BG3_CX = 0;
    BG3_CY = 0;
  }
  
  VRAMBufArray[0]=&VRAMBuf[(ScreenHeight*ScreenWidth)*0];
  VRAMBufArray[1]=&VRAMBuf[(ScreenHeight*ScreenWidth)*1];
  
  pViewCanvas=new CglCanvas(VRAMBufArray[0],ScreenWidth,ScreenHeight,pf15bit);
  pBackCanvas=new CglCanvas(VRAMBufArray[1],ScreenWidth,ScreenHeight,pf15bit);
  
  BackVRAMPage=1;
  
  u32 color=RGB15(0,0,0)|BIT15;
  
  pViewCanvas->SetColor(color);
  glMemSet32CPU(color,pViewCanvas->GetVRAMBuf(),ScreenWidth*ScreenHeight*2);
  
  pBackCanvas->SetColor(color);
  glMemSet32CPU(color,pBackCanvas->GetVRAMBuf(),ScreenWidth*ScreenHeight*2);
  
  mode=ESMM_Normal;

  Flip(true);
}

CglScreenMain::~CglScreenMain(void)
{
  delete pViewCanvas; pViewCanvas=NULL;
  delete pBackCanvas; pBackCanvas=NULL;
}

CODE_IN_ITCM void CglScreenMain::Flip(const bool ShowFlag)
{
  BackVRAMPage=1-BackVRAMPage;
  pBackCanvas->SetVRAMBuf(VRAMBufArray[BackVRAMPage],ScreenWidth,ScreenHeight,pf15bit);
  pViewCanvas->SetVRAMBuf(VRAMBufArray[1-BackVRAMPage],ScreenWidth,ScreenHeight,pf15bit);
  
  if(BackVRAMPage==0){
    BG2_CR = BG2_CR_BASE_15bitBM | BG_PRIORITY_2;
    BG3_CR = BG3_CR_BASE_15bitBM | BG_PRIORITY_1;
    }else{
    BG2_CR = BG2_CR_BASE_15bitBM | BG_PRIORITY_1;
    BG3_CR = BG3_CR_BASE_15bitBM | BG_PRIORITY_2;
  }
  
  if(ShowFlag==true){
    SetBlendLevel(16);
    }else{
    SetBlendLevel(0);
  }
}

CODE_IN_ITCM void CglScreenMain::FlipForVSyncAuto(void)
{
  BackVRAMPage=1-BackVRAMPage;
  
  if(BackVRAMPage==0){
    BG2_CR = BG2_CR_BASE_15bitBM | BG_PRIORITY_2;
    BG3_CR = BG3_CR_BASE_15bitBM | BG_PRIORITY_1;
    }else{
    BG2_CR = BG2_CR_BASE_15bitBM | BG_PRIORITY_1;
    BG3_CR = BG3_CR_BASE_15bitBM | BG_PRIORITY_2;
  }
}

CODE_IN_ITCM void CglScreenMain::SetBlendLevel(const int BlendLevel)
{
  SetBlendLevelManual(16-BlendLevel,BlendLevel);
}

CODE_IN_ITCM void CglScreenMain::SetBlendLevelManual(const int BlendLevelBack,const int BlendLevelView)
{
  if((BlendLevelBack==0)&&(BlendLevelView==16)){
    if(BackVRAMPage==0){
      BLEND_CR=BLEND_ALPHA | BLEND_SRC_SPRITE | BLEND_DST_BG3;
      }else{
      BLEND_CR=BLEND_ALPHA | BLEND_SRC_SPRITE | BLEND_DST_BG2;
    }
    BLEND_AB=(16 << 0) | (16 << 8);
    return;
  }
  
  if(BackVRAMPage==0){
    BLEND_CR=BLEND_ALPHA | BLEND_SRC_BG3 | BLEND_DST_BG2;
    }else{
    BLEND_CR=BLEND_ALPHA | BLEND_SRC_BG2 | BLEND_DST_BG3;
  }
  
  int blb=BlendLevelBack;
  int blv=BlendLevelView;
  
  if(blb<0) blb=0;
  if(16<blb) blb=16; // Max16
  if(blv<0) blv=0;
  if(16<blv) blv=16; // Max16
  
  BLEND_AB=(blv << 0) | (blb << 8);
}

void CglScreenMain::SetMode(EScrMainMode _mode)
{return;
  mode=_mode;
  
  switch(mode){
    case ESMM_Normal: {
      vramSetMainBanks(VRAM_A_MAIN_BG_0x06000000, VRAM_B_MAIN_SPRITE_0x06400000, VRAM_C_MAIN_BG_0x06020000,VRAM_D_SUB_SPRITE);
      BG2_CR = BG2_CR_BASE_15bitBM | BG_PRIORITY_2;
      BG3_CR = BG3_CR_BASE_15bitBM | BG_PRIORITY_1;
      BackVRAMPage=1;
    } break;
    case ESMM_ForARM7: {
      vramSetBankD(VRAM_D_MAIN_BG_0x06060000);
      glMemSet32CPU(0,(u32*)0x6060000,128*1024);
      vramSetMainBanks(VRAM_A_MAIN_BG_0x06000000, VRAM_B_MAIN_BG_0x06020000, VRAM_C_ARM7_0x06000000,VRAM_D_SUB_SPRITE);
      BG2_CR = BG2_CR_BASE_15bitBM | BG_PRIORITY_2;
      BG3_CR = BG3_CR_BASE_15bitBM | BG_PRIORITY_1;
      BackVRAMPage=1;
    } break;
  }
  
  Flip(true);
}

EScrMainMode CglScreenMain::GetMode(void)
{
  return(mode);
}

