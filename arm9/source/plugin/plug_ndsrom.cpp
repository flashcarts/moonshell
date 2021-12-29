
#pragma Ospace

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <NDS.h>

#include "../../../ipc6.h"

#include "../_console.h"

#include "../_const.h"

#include "fat2.h"
#include "io_dldi.h"

#include "libs/memtool.h"
#include "libs/shell.h"

#include "plug_ndsrom.h"
#include "strtool.h"
#include "../BootROM.h"

extern __declspec(noreturn) void _ShowLogHalt(void);

static void _ShowLogHalt(void)
{
  _consoleLogResume();
  IPC6->LCDPowerControl=LCDPC_ON_BOTH;
  videoSetModeSub(MODE_2_2D | DISPLAY_BG2_ACTIVE);
  _consoleSetLogOutFlag(true);
  _consolePrint("\n Application halted!!\n Please refer [/moonshl2/logbuf.txt]\n\n");
  while(1){
    swiWaitForVBlank();
  }
}

#define _VRAM_OFFSET(n) ((n)<<3)
#define _VRAM_PTR ((u16*)0x06000000)
#define _VRAM_CD_MAIN_BG_0x6000000 (1 | _VRAM_OFFSET(0))
#define _VRAM_CD_MAIN_BG_0x6020000 (1 | _VRAM_OFFSET(1))
#define _VRAM_CD_ARM7_0x6000000 (2 | _VRAM_OFFSET(0))
#define _VRAM_CD_ARM7_0x6020000 (2 | _VRAM_OFFSET(1))

#include "plug_ndsrom_resetmem.h"
#include "plug_ndsrom_dldipatch.h"

static void reboot(u8 *pFileBuf,u32 FileSize,ERESET RESET)
{
  REG_IME = IME_DISABLE;	// Disable interrupts
  REG_IF = REG_IF;	// Acknowledge interrupt
  
  IPC6->RESET_BootAddress=0;
  IPC6->RESET=RESET;
  
  VRAM_C_CR = VRAM_ENABLE | _VRAM_CD_MAIN_BG_0x6000000;
  VRAM_D_CR = VRAM_ENABLE | _VRAM_CD_MAIN_BG_0x6020000;
  
  u32 BootAddress=(u32)pFileBuf;
  
  u32 zero;
  asm {
    mov zero,#0
    MCR p15, 0, zero, c7, c10, 4 // drain write buffer
  }
  DC_FlushAll();
  
  _consolePrintf("REBOOT\n");
  
  {
    u16 KEYS_CUR;
    
    KEYS_CUR=(~REG_KEYINPUT)&0x3ff;
    while(KEYS_CUR!=0){
      KEYS_CUR=(~REG_KEYINPUT)&0x3ff;
//      _consolePrintf("!");
    }
  }
  
//  _consolePrintf("%x,%x,%x\n",&IPC6->RESET_BootAddress,IPC6->RESET_BootAddress,BootAddress);

#define _REG_WAIT_CR (*(vuint16*)0x04000204)
  _REG_WAIT_CR|=1 << 7;

//  _consolePrint("resetMemory1_ARM9\n");
  resetMemory1_ARM9();
  
  VRAM_C_CR = VRAM_ENABLE | _VRAM_CD_ARM7_0x6000000;
  VRAM_D_CR = VRAM_ENABLE | _VRAM_CD_ARM7_0x6020000;
  *((vu32*)0x027FFE04) = (u32)0xE59FF018;  // ldr pc, 0x027FFE24
  *((vu32*)0x027FFE24) = (u32)0x027FFE04;  // Set ARM9 Loop address

/*
#define _REG_WAIT_CR (*(vuint16*)0x04000204)
	REG_IME = 0;
	REG_IF = ~0;
	REG_IE = 0;
	_REG_WAIT_CR = 0xe880;
	videoSetModeSub(MODE_0_2D | DISPLAY_SPR_ACTIVE | DISPLAY_SPR_2D | DISPLAY_SPR_2D_BMP_256);
	REG_POWERCNT = (0x30F | POWER_SWAP_LCDS);
	u32 j=0x06800000;
	while(j < 0x06820000)
	{
		*(volatile u16*)(j+0) = 0;
		*(volatile u16*)(j+2) = 0;
		*(volatile u16*)(j+4) = 0;
		*(volatile u16*)(j+6) = 0;
		*(volatile u16*)(j+8) = 0; 
		*(volatile u16*)(j+10) = 0;
		*(volatile u16*)(j+12) = 0;
		*(volatile u16*)(j+14) = 0;
		j += 16;
	}
*/
	
  static void (*lp_resetMemory2_ARM9) (vu32 *pBootAddress,u32 BootAddress,bool ClearMainMemory)=(void(*)(vu32 *pBootAddress,u32 BootAddress,bool ClearMainMemory))(0x02000000+(2*1024*1024)-(16*1024));
  
  u32 *psrc=(u32*)resetMemory2_ARM9;
  u32 *pdst=(u32*)lp_resetMemory2_ARM9;
  MemCopy32CPU(psrc,pdst,16*1024);
  
  lp_resetMemory2_ARM9(&IPC6->RESET_BootAddress,BootAddress,true);
  
  while(1); // 呼び出し元には帰らない。
}

static void RebootNDSHomeBrew(const char *pFilename)
{
  u8 *pDLDIBuf=NULL;
  s32 DLDISize=0;
  
  const char fn[]="DLDIBODY.BIN";
  if(Shell_FAT_ReadAlloc(fn,(void**)&pDLDIBuf,&DLDISize)==false){
    _consolePrintf("Can not load DLDI patch file. (%s)\n",fn);
    pDLDIBuf=NULL;
    DLDISize=0;
  }
  
  FAT_FILE *FileHandle=FAT2_fopen_AliasForRead(pFilename);
  
  if(FileHandle==NULL){
    _consolePrint("Can not open NDS file.\n");
    _ShowLogHalt();
  }
  
  u32 FileSize=FAT2_GetFileSize(FileHandle);
  
  if((2*1024*1024)<FileSize){
    _consolePrint("Fatal error: Large file size!! (Limit 2MByte)\n");
    FAT2_fclose(FileHandle);
    _ShowLogHalt();
  } 
  
  u8 *pFileBuf=(u8*)(0x02000000+(2*1024*1024));
  
  MemClearAllFreeBlocks();
  FAT2_fread_fast(pFileBuf,1,(FileSize+3)&~3,FileHandle);
  
  _consolePrint("FAT_fclose\n");
  FAT2_fclose(FileHandle);
   
  _consolePrint("FAT_FreeFiles\n");
  FAT2_FreeFiles();
  
  extern void disc_SystemCache_ClearAll(void);
  disc_SystemCache_ClearAll();

  if((pDLDIBuf!=NULL)&&(DLDISize!=0)){
    if(DLDIPatch(pFileBuf,FileSize,pDLDIBuf,DLDISize)==false){
      _consolePrintf("Fatal error: DLDI patch error or not found DLDI chank.\n");
    }
    MemSet32CPU(0,pDLDIBuf,DLDISize&~3);
  }
  
  reboot(pFileBuf,FileSize,RESET_HomeBrew);
  
  _consolePrintf("Fatal error: Unknown error.\n");
  _ShowLogHalt();
}

static void BackupDLDIBody(void)
{
  const u32 *pdldibody=&dldibodytop;
  u32 dldibodysize=0;
  
  for(u32 idx=0;idx<dldibodymaxsize/4;idx++){
    if(pdldibody[idx]!=0) dldibodysize=(idx+1)*4;
  }
  
  if(dldibodysize==0) return;
  
  {
    u8 *p=(u8*)pdldibody;
    char ID[5];
    ID[0]=p[0x60+0];
    ID[1]=p[0x60+1];
    ID[2]=p[0x60+2];
    ID[3]=p[0x60+3];
    ID[4]=0;
    const char *pname=(const char*)&p[0x10];
    _consolePrintf("Backup implant DLDI body.\n");
    _consolePrintf("DLDI ID:%s\n",ID);
    _consolePrintf("DLDI Name:%s\n",pname);
    _consolePrintf("DLDI Size:%dbyte\n",dldibodysize);
  }
  
  const char fn[]="DLDIBODY.BIN";
  FAT_FILE *pf=Shell_FAT_fopenwrite_Data(fn);
  
  if(pf==NULL){
    _consolePrintf("Fatal error: Can not open DLDI patch file. (%s)\n",fn);
    ShowLogHalt();
  }
  
  if(FAT2_fwrite(pdldibody,1,dldibodysize,pf)!=dldibodysize){
    _consolePrintf("Fatal error: DLDI path backup write error. (%s)\n",fn);
    ShowLogHalt();
  }
    
  FAT2_fclose(pf);
}

void BootNDSROM(void)
{
  const char *pFilename=BootROM_GetFullPathAlias();
  
	REG_IME = 0;
	REG_IE = 0;
	REG_IF = ~0;
	
  DMA0_CR=0;
  DMA1_CR=0;
  DMA2_CR=0;
  DMA3_CR=0;
  
  BackupDLDIBody();
  
  RebootNDSHomeBrew(pFilename);
  
  _consolePrintf("Fatal error: Unknown error.\n");
  _ShowLogHalt();
}
