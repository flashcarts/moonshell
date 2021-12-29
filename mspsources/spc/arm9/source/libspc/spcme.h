#include <nds.h>

#include "spc_debug.h"

/*
#ifndef SNESDS
struct SpcTransferRegion {
    int arm7State;
    int arm9State;

    int curCommand;

#ifdef ARM7SPC
    u8 *rawSpc;
    long long cpuTime,dspTime,curTime;
#else
    u16 playBuffer[MIXBUFSIZE * 2];
#endif
};

#define SPC_IPC ((SpcTransferRegion volatile *)(0x027FF000))

#define ARM9COMMAND_UPDATE_APU 1
#define ARM9COMMAND_STEP_APU_DONE 2

#define ARM7COMMAND_START_SOUND 1
#define ARM7COMMAND_STOP_SOUND 2
#define ARM7COMMAND_LOAD_SPC 3
#define ARM7COMMAND_STEP_APU 4
#define ARM7COMMAND_FASTSTEP_APU 5
#define ARM7COMMAND_DONE_APU_UPDATE 6

#define ARM9STATE_INITIALIZING 1
#define ARM9STATE_READY 0x100

#define ARM7STATE_WAITING 1 
#define ARM7STATE_READY 0x101

#endif
*/

//#define DTCM_IRQ_HANDLER	(*(VoidFunctionPointer *)0x02803FFC)
//#define BIOS_INTR_ACK       (*(vu32*)0x02803FF8)

#define ALIGNED __attribute__ ((aligned(4)))

//#define CODE_IN_ITCM __attribute__ ((section (".itcm")))
#define CODE_IN_ITCM
#define ALIGNED_VAR_IN_DTCM __attribute__ ((aligned(4),section (".dtcm")))
//#define ALIGNED_VAR_IN_DTCM

