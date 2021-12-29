
#include <nds.h>

#include "_const.h"
#include "glib/glib.h"
#include "_console.h"
#include "_consoleWriteLog.h"

#include "arm9tcm.h"
#include "setarm9_reg_waitcr.h"
//#include "hio.h"
#include "memtool.h"

#define ROMTITLE "WMA decode test with rockbox-19523"
#define ROMVERSION "Ver 0.00"
#define ROMDATE ""__DATE__" "__TIME__" GMT+09:00"
#define ROMDATESHORT ""__DATE__" "__TIME__""
#define ROMENV "devkitpro r21"

// ------------------------------------------------------------------

// ------------------------------------------------------

#include "plug_wma_ins.h"

int main_wma(void)
{
  REG_IME=0;
  
  SetARM9_REG_WaitCR();
  
  REG_POWERCNT = POWER_ALL_2D; // | POWER_SWAP_LCDS;
  
  glDefaultMemorySetting();
  
  glDefaultClassCreate();
  
  {
    SUB_BG2_CR = BG_256_COLOR | BG_RS_64x64 | BG_MAP_BASE(8) | BG_TILE_BASE(0) | BG_PRIORITY_1; // Tile16kb Map2kb(64x32)
    
    BG_PALETTE_SUB[(0*16)+0] = RGB15(0,0,0); // unuse (transparent)
    BG_PALETTE_SUB[(0*16)+1] = RGB15(0,0,8) | BIT(15); // BG color
    BG_PALETTE_SUB[(0*16)+2] = RGB15(0,0,0) | BIT(15); // Shadow color
    BG_PALETTE_SUB[(0*16)+3] = RGB15(31,31,31) | BIT(15); // Text color
    
    u16 XDX=(u16)((8.0/6)*0x100);
    u16 YDY=(u16)((8.0/6)*0x100);
    
    SUB_BG2_XDX = XDX;
    SUB_BG2_XDY = 0;
    SUB_BG2_YDX = 0;
    SUB_BG2_YDY = YDY;
    
    SUB_BG2_CX=-1;
    SUB_BG2_CY=-1;
    
    //consoleInit() is a lot more flexible but this gets you up and running quick
    _consoleInitDefault((u16*)(SCREEN_BASE_BLOCK_SUB(8)), (u16*)(CHAR_BASE_BLOCK_SUB(0)));
  }
  
  _consolePrintf("boot %s %s\n%s\n%s\n\n",ROMTITLE,ROMVERSION,ROMDATE,ROMENV);
  
  if(wma_init()==false){
    _consolePrintf("wma_init() error.\n");
    while(1);
  }
  
  PrintFreeMem();
  
  while(1){
    const u32 smpbufcount=2048;
    s16 *psmpbuf=(s16*)safemalloc(smpbufcount*2*2);
    u32 smpcnt=wma_decode(psmpbuf,smpbufcount);
    _consolePrintf("decoded. %d\n",smpcnt);
    
    if(smpcnt==0){
      if(psmpbuf!=NULL){
        safefree(psmpbuf); psmpbuf=NULL;
      }
      break;
    }
    
    for(u32 idx=0;idx<16;idx++){
      _consolePrintf("%04x ",psmpbuf[idx*8]);
    }
    _consolePrintf("\n");
    
    if(psmpbuf!=NULL){
      safefree(psmpbuf); psmpbuf=NULL;
    }
  }
  
  PrintFreeMem();
  
  wma_free();
  
  atype_checkoverrange();
  _consolePrintf("Passed checkoverrange.\n");
  
  atype_checkmemoryleak();
  _consolePrintf("Passed checkmemoryleak.\n");
  
  _consolePrintf("Terminated.\n");
  while(1);
  
  return(0);
}

