#include "spcme.h"

#include <nds/arm9/console.h>
#include "def_console.h"

extern "C" {
//#include "sprintf.h"
}
#include "spc_debug.h"
#include "arm7/source/dsp.h"
#include "arm7/source/apu.h"

#ifdef DEBUG

extern u8 *APU_MEM;

void WaitForVBlank();

// Code originally from Snes9x, modified a bit to get stuff into rom

const char* const mnemonics[256] = {
	"NOP", "TCALL0", "SET0 $%02X", "BBS0 $%02X,$%04X",							// 0x00
	"OR A,$%02X", "OR A,$%04X", "OR A,(X)", "OR A,($%02X+X)",					// 0x04
	"OR A,#$%02X", "OR $%02X,$%02X", "OR1 C,$%04X,%d", "ASL $%02X",				// 0x08
	"ASL $%04X", "PUSH PSW", "TSET1 $%04X", "BRK",								// 0x0c	
	"BPL $%04X", "TCALL1", "CLR0 $%02X", "BBC0 $%02X,$%04X",					// 0x10
	"OR A,$%02X+X", "OR A,$%04X+X", "OR A,$%04X+Y", "OR A,($%02X)+Y",
	"OR $%02X,#$%02X", "OR (X),(Y)", "DECW $%02X", "ASL $%02X+X",
	"ASL A", "DEC X", "CMP X,$%04X", "JMP ($%04X+X)",
	"CLRP", "TCALL2", "SET1 $%02X", "BBS1 $%02X,$%04X",							// 0x20
	"AND A,$%02X", "AND A,$%04X", "AND A,(X)", "AND A,($%02X+X)",
	"AND A,$%02X", "AND $%02X,$%02X", "OR1 C,$%04X, not %d", "ROL $%02X",
	"ROL $%04X", "PUSH A", "CBNE $%02X,$%04X", "BRA $%04X",
	"BMI $%04X", "TCALL3", "CLR1 $%02X", "BBC1 $%02X,$%04X",					// 0x30
	"AND A,$%02X+X", "AND A,$%04X+X", "AND A,$%04X+Y", "AND A,($%02X)+Y",
	"AND $%02X,#$%02X", "AND (X),(Y)", "INCW $%02X", "ROL $%02X+X",
	"ROL A", "INC X", "CMP X,$%02X", "CALL $%04X",
	"SETP", "TCALL4", "SET2 $%02X", "BBS2 $%02X,$%04X",							// 0x40
	"EOR A,$%02X", "EOR A,$%04X", "EOR A,(X)", "EOR A,($%02X+X)",
	"EOR A,#$%02X", "EOR $%02X,$%02X", "AND1 C,$%04X,%d", "LSR $%02X",
	"LSR $%04X", "PUSH X", "TCLR1 $%04X", "PCALL $%02X",
	"BVC $%04X", "TCALL5", "CLR2 $%02X", "BBC2 $%02X,$%04X",					// 0x50
	"EOR A,$%02X+X", "EOR A,$%04X+X", "EOR A,$%04X+Y", "EOR A,($%02X)+Y",
	"EOR $%02X,#$%02X", "EOR (X),(Y)", "CMPW YA,$%02X", "LSR $%02X+X",
	"LSR A", "MOV X,A", "CMP Y,$%04X", "JMP $%04X",
	"CLRC", "TCALL6", "SET3 $%02X", "BBS3 $%02X,$%04X",							// 0x60
	"CMP A,$%02X", "CMP A,$%04X", "CMP A,(X)", "CMP A,($%02X+X)",
	"CMP A,#$%02X", "CMP $%02X,$%02X", "AND1 C,$%04X,/%d", "ROR $%02X",
	"ROR $%04X", "PUSH Y", "DBNZ $%02X,$%04X", "RET",
	"BVS $%04X", "TCALL7", "CLR3 $%02X", "BBC3 $%02X,$%04X",					// 0x70
	"CMP A,$%02X+X", "CMP A,$%04X+X", "CMP A,$%04X+Y", "CMP A,($%02X)+Y",
	"CMP $%02X,#$%02X", "CMP (X),(Y)", "ADDW YA,$%02X", "ROR $%02X+X",
	"ROR A", "MOV A,X", "CMP Y,$%02X", "RETI",
	"SETC", "TCALL8", "SET4 $%02X", "BBS4 $%02X,$%04X",							// 0x80
	"ADC A,$%02X", "ADC A,$%04X", "ADC A,(X)", "ADC A,($%02X+X)",
	"ADC A,#$%02X", "ADC $%02X,$%02X", "EOR1 C,%04,%d", "DEC $%02X",
	"DEC $%04X", "MOV Y,#$%02X", "POP PSW", "MOV $%02X,#$%02X",
	"BCC $%04X", "TCALL9", "CLR4 $%02X", "BBC4 $%02X,$%04X",					// 0x90
	"ADC A,$%02X+X", "ADC A,$%04X+X", "ADC A,$%04X+Y", "ADC A,($%02X)+Y",
	"ADC $%02X,#$%02X", "ADC (X),(Y)", "SUBW YA,$%02X", "DEC $%02X+X",
	"DEC A", "MOV X,SP", "DIV YA,X", "XCN A",
	"EI", "TCALL10", "SET5 $%02X", "BBS5 $%02X,$%04X",
	"SBC A,$%02X", "SBC A,$%04X", "SBC A,(X)", "SBC A,($%02X+X)",
	"SBC A,#$%02X", "SBC $%02X,$%02X", "MOV1 C,$%04X,%d", "INC $%02X",
	"INC $%04X", "CMP Y,#$%02X", "POP A", "MOV (X)+,A",
	"BCS $%04X", "TCALL11", "CLR5 $%02X", "BBC5 $%02X,$%04X",
	"SBC A,$%02X+X", "SBC A,$%04X+X", "SBC A,$%04X+Y", "SBC A,($%02X)+Y",
	"SBC $%02X,#$%02X", "SBC (X),(Y)", "MOVW YA,$%02X", "INC $%02X+X",
	"INC A", "MOV SP,X", "DAS", "MOV A,(X)+",
	"DI", "TCALL12", "SET6 $%02X", "BBS6 $%02X,$%04X",
	"MOV $%02X,A", "MOV $%04X,A", "MOV (X),A", "MOV ($%02X+X),A",
	"CMP X,#$%02X", "MOV $%04X,X", "MOV1 $%04X,%d,C", "MOV $%02X,Y",
	"MOV $%04X,Y", "MOV X,#$%02X", "POP X", "MUL YA",
	"BNE $%04X", "TCALL13", "CLR6 $%02X", "BBC6 $%02X,$%04X",
	"MOV $%02X+X,A", "MOV $%04X+X,A", "MOV $%04X+Y,A", "MOV ($%02X)+Y,A",
	"MOV $%02X,X", "MOV $%02X+Y,X", "MOVW $%02X,YA", "MOV $%02X+X,Y",
	"DEC Y", "MOV A,Y", "CBNE $%02X+X,$%04X", "DAA",
	"CLRV", "TCALL14", "SET7 $%02X", "BBS7 $%02X,$%04X",
	"MOV A,$%02X", "MOV A,$%04X", "MOV A,(X)", "MOV A,($%02X+X)",
	"MOV A,#$%02X", "MOV X,$%04X", "NOT1 $%04X,%d", "MOV Y,$%02X",
	"MOV Y,$%04X", "NOTC", "POP Y", "SLEEP",
	"BEQ $%04X", "TCALL15", "CLR7 $%02X", "BBC7 $%02X,$%04X",
	"MOV A,$%02X+X", "MOV A,$%04X+X", "MOV A,$%04X+Y", "MOV A,($%02X)+Y",
	"MOV X,$%02X", "MOV X,$%02X+Y", "MOV $%02X,$%02X", "MOV Y,$%02X+X",
	"INC Y", "MOV Y,A", "DBNZ Y,$%04X", "STOP"
};

enum {
	DP = 0,
	ABS,
	IM,
	DP2DP,
	DPIM,
	DPREL,
	ABSBIT,
	REL,
	DPX
};

const u8 modes[256] = {
	IM, IM, DP, DPREL,			// 0x00
	DP, ABS, IM, DP,
	DP, DP2DP, ABSBIT, DP,
	ABS, IM, ABS, IM,
	REL, IM, DP, DPREL,			// 0x10
	DP, ABS, ABS, DP,
	DPIM, IM, DP, DP,
	IM, IM, ABS, ABS,
	IM, IM, DP, DPREL,			// 0x20
	DP, ABS, IM, DP,
	DP, DP2DP, ABSBIT, DP,
	ABS, IM, DPREL, REL,
	REL, IM, DP, DPREL,			// 0x30
	DP, ABS, ABS, DP,
	DPIM, IM, DP, DP,
	IM, IM, DP, ABS,
	IM, IM, DP, DPREL,			// 0x40
	DP, ABS, IM, DP,
	DP, DP2DP, ABSBIT, DP,
	ABS, IM, ABS, DP,
	REL, IM, DP, DPREL,			// 0x50
	DP, ABS, ABS, DP,
	DPIM, IM, DP, DP,
	IM, IM, ABS, ABS,
	IM, IM, DP, DPREL,			// 0x60
	DP, ABS, IM, DP,
	DP, DP2DP, ABSBIT, DP,
	ABS, IM, DPREL, IM,
	REL, IM, DP, DPREL,			// 0x70
	DP, ABS, ABS, DP,
	DPIM, IM, DP, DP,
	IM, IM, DP, IM,
	IM, IM, DP, DPREL,			// 0x80
	DP, ABS, IM, DP,
	DP, DP2DP, ABSBIT, DP,
	ABS, DP, IM, DPIM,
	REL, IM, DP, DPREL,			// 0x90
	DP, ABS, ABS, DP,
	DPIM, IM, DP, DP,
	IM, IM, IM, IM,
	IM, IM, DP, DPREL,			// 0xA0
	DP, ABS, IM, DP,
	DP, DP2DP, ABSBIT, DP,
	ABS, DP, IM, IM,
	REL, IM, DP, DPREL,			// 0xB0
	DP, ABS, ABS, DP,
	DPIM, IM, DP, DP,
	IM, IM, IM, IM,
	IM, IM, DP, DPREL,			// 0xC0
	DP, ABS, IM, DP,
	DP, ABS, ABSBIT, DP,
	ABS, DP, IM, IM,
	REL, IM, DP, DPREL,			// 0xD0
	DP, ABS, ABS, DP,
	DP, DP, DP, DP,
	IM, IM, DPREL, IM,
	IM, IM, DP, DPREL,			// 0xE0
	DP, ABS, IM, DP,
	DP, ABS, ABSBIT, DP,
	ABS, IM, IM, IM,
	REL, IM, DP, DPREL,			// 0xF0
	DP, ABS, ABS, DP,
	DP, DP, DP2DP, DP,
	IM, IM, REL, IM
};

const u8 modesToBytes[] = {
	2, 3, 1, 3, 3, 3, 3, 2
};

int DebugGetOpcodeDesc(char *buffer, u8 *p, bool mem, u8 *memPtr) {
	char mnem[20];
	char mems[20];
	int mode = modes[*p];
	int bytes = modesToBytes[mode];

	if (!mem) {
		switch (bytes)
		{
		case 1:
			sprintf (buffer, "%04X %02X       ", (u32)(p - memPtr), *p);
			break;
		case 2:
			sprintf (buffer, "%04X %02X %02X    ", (u32)(p - memPtr), *p, *(p + 1));
			break;
		case 3:
			sprintf (buffer, "%04X %02X %02X %02X ", (u32)(p - memPtr), *p, *(p + 1), *(p + 2));
			break;
		}
	} else {
		sprintf (buffer, "%04X %02X ", (u32)(p - memPtr), *p);
	}

	switch (mode)
	{
	case DP:
		sprintf(mnem, mnemonics [*p], *(p + 1));
		break;
	case ABS:
		sprintf(mnem, mnemonics [*p], *(p + 1) + (*(p + 2) << 8));
		break;
	case IM:
		sprintf(mnem, mnemonics [*p]);
		break;
	case DP2DP:
		sprintf(mnem, mnemonics [*p], *(p + 2), *(p + 1));
		break;
	case DPIM:
		sprintf(mnem, mnemonics [*p], *(p + 2), *(p + 1));
		break;
	case DPREL:
		sprintf(mnem, mnemonics [*p], *(p + 1), (int) (p + 3 - memPtr) + (signed char) *(p + 2));
		break;
	case ABSBIT:
		sprintf(mnem, mnemonics [*p], (*(p + 1) + (*(p + 2) << 8)) & 0x1fff, *(p + 2) >> 5);
		break;
	case REL:
		sprintf(mnem, mnemonics [*p], (int) (p + 2 - memPtr) + (signed char) *(p + 1));
		break;
	}

	mems[0] = 0;
	if (memPtr) {
		u8 *DP_MEM = memPtr + APU_STATE[4];

		switch (mode)
		{
		case DP:
			sprintf(mems, "%02X", DP_MEM[*(p + 1)]);
			break;
		case ABS:
			sprintf(mems, "%02X", memPtr[*(p + 1) + (*(p + 2) << 8)]);
			break;
		case IM:
			break;
		case DP2DP:
			sprintf(mems, "%02X %02X", DP_MEM[*(p + 2)], DP_MEM[*(p + 1)]);
			break;
		case DPIM:
			sprintf(mems, "%02X", DP_MEM[*(p + 2)]);
			break;
		case DPREL:
//			sprintf(mems, mnemonics [*p], *(p + 1), (int) (p + 3 - memPtr) + (signed char) *(p + 2));
			break;
		case ABSBIT:
//			sprintf(mems, mnemonics [*p], (*(p + 1) + (*(p + 2) << 8)) & 0x1fff, *(p + 2) >> 5);
			break;
		case REL:
			sprintf(mems, "%02X", memPtr[(int) (p + 2 - memPtr) + (signed char) *(p + 1)]);
			break;
		}
	}

	sprintf(buffer,"%s%s %s",buffer,mnem,mems);

	return bytes;
}

// 0 - A, 1 - X, 2 - Y, 3 - RAMBASE, 4 - DP, 5 - PC (Adjusted into rambase)
// 6 - Cycles (bit 0 - C, bit 1 - v, bit 2 - h, bits 3+ cycles left)
// 7 - Optable
// 8 - NZ

void DebugGetSPCState(char *buffer) {
    sprintf (buffer, "A:%02X X:%02X Y:%02X S:%3X DSP:%02X\nP:%c%c%c%c%c%c%c%c T0:%02X T1:%02X T2:%02X\nCont: %02X C0: %02X C1: %02X C2: %02X",
		APU_STATE[0] >> 24, APU_STATE[1] >> 24, APU_STATE[2] >> 24,
		APU_SP,
        APU_MEM[0xF2],
		(APU_STATE[8] & 0x80) ? 'N' : 'n',
		(APU_STATE[6] & 2) ? 'V' : 'v',
		APU_STATE[4] ? 'P' : 'p',
		0 ? 'B' : 'b', // Break flag
		0 ? 'H' : 'h', // Half carry flag
		0 ? 'I' : 'i', // Interrupt flag
		(APU_STATE[8] == 0) ? 'Z' : 'z',
		(APU_STATE[6] & 1) ? 'C' : 'c',
		APU_MEM[APU_TIMER0],
		APU_MEM[APU_TIMER1],
		APU_MEM[APU_TIMER2],
		APU_MEM[APU_CONTROL_REG],
		APU_MEM[APU_COUNTER0],
		APU_MEM[APU_COUNTER1],
		APU_MEM[APU_COUNTER2]);
}

#ifdef COMPILE_SNES9X
void s9xDspMixSamplesStereo(int numSamples, u16 *left, u16 *right);

#include "snes9x/dsp.h"

void PrintDspState() {
    consoleClear();
    consolePrintSet(0,0);

    for (int i = 0; i < 8; i++) {
        DspChannel *channel = &channels[i];
        consolePrintf("%d %d %d %d %d %d %d %d\n", channel->active, channel->prevSamp1, channel->prevSamp2,
            channel->samplePos, channel->decoded[channel->samplePos >> 12], channel->blockPos, SPC_IPC->DSP_RAM[DSP_ENVX + (i << 4)],
            SPC_IPC->DSP_RAM[DSP_OUTX + (i << 4)]);
    }
    consolePrintf("\n");
    for (int i = 0; i < 8; i++) {
        s9xDspChannel *channel = &s9xChannels[i];
        consolePrintf("%d %d %d %d %d %d %d %d\n", channel->active, channel->prevSamp1, channel->prevSamp2,
            channel->samplePos, channel->decoded[channel->samplePos >> 12], channel->blockPos, s9xDSP_MEM[DSP_ENVX + (i << 4)],
            s9xDSP_MEM[DSP_OUTX + (i << 4)]);
    }
}

#endif

/*
void debugLoop() {
#ifdef COMPILE_SNES9X
    myCompare(APU_STATE[5] - APU_STATE[3]);
#endif

    {
		u16 lastPC = APU_STATE[5] - APU_STATE[3];
        const int cyclesToExecute = spcCyclesPerSec / (32000 / MIXBUFSIZE);
        int numCycles = cyclesToExecute;

		for (;;) {
			PrintDisasm(21,lastPC,APU_STATE[5] - APU_STATE[3]);
			lastPC = APU_STATE[5] - APU_STATE[3];

            while (KEYS & KEY_A) {
                while (numCycles < 0) {
#ifdef COMPILE_SNES9X
                    s9xDspMixSamplesStereo(MIXBUFSIZE, &rawLeftBuffer[SPC_IPC->soundCursor], &rawRightBuffer[SPC_IPC->soundCursor]);
#endif
                    DspMixSamplesStereo(MIXBUFSIZE, &rawLeftBuffer[SPC_IPC->soundCursor], &rawRightBuffer[SPC_IPC->soundCursor]);
                    numCycles += cyclesToExecute;
                }

                if (!(KEYS & KEY_DOWN)) {
                    for (int i=0; i<500; i++) {
                        consolePrintSet(0,0);
                        consolePrintf("%d          ",i);
                        lastPC = APU_STATE[5] - APU_STATE[3];
                        stepApu();
                        numCycles -= lastApuCycles;
#ifdef COMPILE_SNES9X
                        step9xApu();
                        myCompare(lastPC);
#endif
                    }
                    lastPC = APU_STATE[5] - APU_STATE[3];
        			PrintDisasm(21,lastPC,APU_STATE[5] - APU_STATE[3]);
                }
				if (!(KEYS & KEY_RIGHT)) {
                    for (int j=0; j<50; j++) {
                        while (numCycles < 0) {
#ifdef COMPILE_SNES9X
                            s9xDspMixSamplesStereo(MIXBUFSIZE, &rawLeftBuffer[SPC_IPC->soundCursor], &rawRightBuffer[SPC_IPC->soundCursor]);
#endif
                            DspMixSamplesStereo(MIXBUFSIZE, &rawLeftBuffer[SPC_IPC->soundCursor], &rawRightBuffer[SPC_IPC->soundCursor]);
                            numCycles += cyclesToExecute;
                        }

                        ApuExecute(cyclesToExecute);
                        ApuUpdateTimers(cyclesToExecute);
                        numCycles -= cyclesToExecute;

#ifdef COMPILE_SNES9X
                        s9xExecute(cyclesToExecute);


                        blahIter = j;
                        consolePrintSet(0, 20);
                        consolePrintf("%d   ", j);
                        myCompare(lastPC);
#endif
                    }
                    lastPC = APU_STATE[5] - APU_STATE[3];
//        			PrintDisasm(21,lastPC,APU_STATE[5] - APU_STATE[3]);
                }
				if (!(KEYS & KEY_LEFT)) {
                    for (int i=0; i<20; i++) {
                        lastPC = APU_STATE[5] - APU_STATE[3];
                        stepApu();
                        numCycles -= lastApuCycles;
#ifdef COMPILE_SNES9X
                        step9xApu();
#endif
                    }
#ifdef COMPILE_SNES9X
                    myCompare(lastPC);
#endif
                    lastPC = APU_STATE[5] - APU_STATE[3];
        			PrintDisasm(21,lastPC,APU_STATE[5] - APU_STATE[3]);
                }
            }

            stepApu();
            numCycles -= lastApuCycles;
#ifdef COMPILE_SNES9X
            step9xApu();
            myCompare(lastPC);
#endif

            u16 pc = APU_STATE[5] - APU_STATE[3];
			if (pc > lastPC + 3 || pc < lastPC) {
				lastPC = pc;
			}

			for (int i = 0; i < 10; i++) WaitForVBlank();
		}
    }
}
*/
void PrintDisasm(int num, u16 pc, u16 actPc) {
    char buf[100];
    DebugGetSPCState(buf);

    consolePrintSet(0,0);
    for (int i = 0; i < num + 3; i++) {
        consolePrintf("                                              \n");
    }

    consolePrintSet(0,0);
    consolePrintf("%s\n", buf);

    while (num--) {
        bool hit = pc == actPc;
        pc += DebugGetOpcodeDesc(buf, &APU_MEM[pc], true, APU_MEM);
        consolePrintf("%s %s\n", buf, hit ? "<--" : "");
    }
}

#ifdef COMPILE_SNES9X
/*
void s9xExecute(int cycles, int numSamples, u16 *left, u16 *right);

int randSeed = 1;
int rand() {
    return (randSeed = (randSeed * 5439081) + 43553);
}

void s9xTestInstruction(u8 a, u8 x, u8 y, u8 psw, u8 sp, u16 pc, u8 op1, u8 op2, u8 op3);

void testInstructionSet() {
    for (int i = 0;;i++) {
        consolePrintSet(0,0);
        consolePrintf("%d   ",i);

        u16 pc = rand() & 0xfff;
        if (pc >= 0xea && pc <= 0xff) continue;

        u8 a = rand() & 0xff, x = rand() & 0xff, y = rand() & 0xff, psw = rand() & 0xff;
        u8 sp = rand() & 0xff, op1 = rand() & 0xff, op2 = rand() & 0xff, op3 = rand() & 0xff;

        psw &= ~0x14;

        // Unhandled opcodes
        if (op1 == 0xDF || op1 == 0xBE || op1 == 0x4F || op1 == 0x0F || op1 == 0x7F || op1 == 0xA0 || op1 == 0xC0 || op1 == 0xEF || op1 == 0xFF) continue;
        // Div skip for emu's sake
//        if (op1 == 0x9E) continue;

        APU_STATE[0] = a<<24; // A
        APU_STATE[1] = x<<24; // X
        APU_STATE[2] = y<<24; // Y
        APU_STATE[6] = 0;
        SetStateFromRawPSW(APU_STATE, psw);
        APU_SP = 0x100 | sp; // SP
        APU_STATE[5] = APU_STATE[3] + pc; // PC

        APU_MEM[pc] = op1;
        APU_MEM[pc+1] = op2;
        APU_MEM[pc+2] = op3;

        ApuExecute(0);
        lastApuCycles = APU_STATE[6] >> 3;
        APU_STATE[6] &= 0x7;       

        s9xTestInstruction(a,x,y,psw,sp,pc,op1,op2,op3);

        consolePrintSet(0,15);
        consolePrintf("%d,%d,%d",APU_MEM[op2+APU_STATE[4]],APU_MEM[op3+APU_STATE[4]],sp);

        myCompare(pc);
    }
}
*/
#endif
#endif
