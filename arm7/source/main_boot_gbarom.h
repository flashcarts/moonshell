
/*
 referrence from 2006-01-13 - v2.11
 
  NDS MP
 GBAMP NDS Firmware Hack Version 2.0
 An NDS aware firmware patch for the GBA Movie Player.
 By Michael Chisholm (Chishm)
 
 Large parts are based on MultiNDS loader by Darkain.
 Filesystem code based on gbamp_cf.c by Chishm (me).
 Flashing tool written by DarkFader.
 Chunks of firmware removed with help from Dwedit.

 GBAMP firmware flasher written by DarkFader.
 
 This software is completely free. No warranty is provided.
 If you use it, please give due credit and email me about your
 project at chishm@hotmail.com
*/

#define CODE_IN_RebootLoader __attribute__ ((section (".RebootLoader")))

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Firmware stuff

#define FW_READ        0x03

static CODE_IN_RebootLoader void _readFirmware(uint32 address, uint32 size, uint8 * buffer) {
  uint32 index;

  // Read command
  while (REG_SPICNT & SPI_BUSY);
  REG_SPICNT = SPI_ENABLE | SPI_CONTINUOUS | SPI_DEVICE_NVRAM;
  REG_SPIDATA = FW_READ;
  while (REG_SPICNT & SPI_BUSY);

  // Set the address
  REG_SPIDATA =  (address>>16) & 0xFF;
  while (REG_SPICNT & SPI_BUSY);
  REG_SPIDATA =  (address>>8) & 0xFF;
  while (REG_SPICNT & SPI_BUSY);
  REG_SPIDATA =  (address) & 0xFF;
  while (REG_SPICNT & SPI_BUSY);

  for (index = 0; index < size; index++) {
    REG_SPIDATA = 0;
    while (REG_SPICNT & SPI_BUSY);
    buffer[index] = REG_SPIDATA & 0xFF;
  }
  REG_SPICNT = 0;
}

/*-------------------------------------------------------------------------
resetMemory_ARM7
Clears all of the NDS's RAM that is visible to the ARM7
Written by Darkain.
Modified by Chishm:
 * Added STMIA clear mem loop
--------------------------------------------------------------------------*/
static CODE_IN_RebootLoader void resetMemory_ARM7 (void)
{
	u32 i;
	u8 settings1, settings2;
	
	REG_IME = 0;

/*
	for (i=0; i<16; i++) {
		SCHANNEL_CR(i) = 0;
		SCHANNEL_TIMER(i) = 0;
		SCHANNEL_SOURCE(i) = 0;
		SCHANNEL_LENGTH(i) = 0;
	}
*/
	for (i=0x04000400; i<0x04000500; i+=4) {
	  *((u32*)i)=0;
	}
	SOUND_CR = 0;

	//clear out ARM7 DMA channels and timers
/*
	for (i=0; i<4; i++) {
		DMA_CR(i) = 0;
		DMA_SRC(i) = 0;
		DMA_DEST(i) = 0;
		TIMER_CR(i) = 0;
		TIMER_DATA(i) = 0;
	}
*/
  for(i=0x040000B0;i<(0x040000B0+0x30);i+=4){
    *((vu32*)i)=0;
  }
  for(i=0x04000100;i<0x04000110;i+=2){
    *((u16*)i)=0;
  }

	//switch to user mode
	u32 r0;
  __asm {
	  mov r0, #0x1F
	  msr cpsr, r0
	}

	REG_IE = 0;
	REG_IF = ~0;
	(*(vu32*)(0x04000000-4)) = 0;  //IRQ_HANDLER ARM7 version
	(*(vu32*)(0x04000000-8)) = ~0; //VBLANK_INTR_WAIT_FLAGS, ARM7 version
	REG_POWERCNT = 1;  //turn off power to stuffs
	
	// Reload DS Firmware settings
	_readFirmware((u32)0x03FE70, 0x1, &settings1);
	_readFirmware((u32)0x03FF70, 0x1, &settings2);
	
	if (settings1 > settings2) {
		_readFirmware((u32)0x03FE00, 0x70, (u8*)0x027FFC80);
	} else {
		_readFirmware((u32)0x03FF00, 0x70, (u8*)0x027FFC80);
	}
}

__attribute__ ((noinline)) static CODE_IN_RebootLoader void reboot(ERESET RESET)
{
		REG_IME = IME_DISABLE;	// Disable interrupts
		REG_IF = REG_IF;	// Acknowledge interrupt
		
    while(IPC6->RESET_BootAddress==0){
  		_console_ReenabledGBABUS();
	    _consolePrint("wait for IPC6->RESET_BootAddress.\n");
			for(vu32 w=0;w<0x100;w++){
			}
	  }
	  
		_console_ReenabledGBABUS();
		_consolePrintf("reboot(%d);\n",RESET);
	  
    _consolePrintf("resetMemory_ARM7();\n");
    resetMemory_ARM7();
    
	  switch(RESET){
	    case RESET_NULL: {
  	    _consolePrintf("IPC6->RESET == RESET_Null. relational error.\n");
  	    while(1);
	    } break;
			case RESET_HomeBrew: {
			  _consolePrintf("Stack base = 0x%08x.\n",__current_sp());
			  
  	    _consolePrintf("Reboot ARM7 to 0x%08x.\n",IPC6->RESET_BootAddress);
        *((vu32*)0x027FFE34) = IPC6->RESET_BootAddress;	// Bootloader start address
  	
        if(1){ // Final stage: Clear ARM7 internal memory.
          u32 *startadr=(u32*)0x037f8000;
    	    u32 *endadr=(u32*)(__current_sp()-0x100);
	        while(startadr!=endadr){
  	        *startadr++=0;
    	    }
  	    }
  
	      { // Clear IPC area.
	        vu32 *pbuf=(vu32*)0x027ff000;
	        for(u32 idx=0;idx<256/4;idx++){
	          *pbuf++=0;
	        }
	      }
	      
/*
        *((vu32*)0x027FFE24) = arm9_entry_addr; // Set ARM9 Loop address
        
        void (*jump)(void)=(void(*)(void))arm7_entry_addr;
        jump();
        while(1);
*/
        
     		// Jump to boot loader
     		asm {
          swi 0<<16
     		}
  	    while(1);
			} break;
	  }
		
}
