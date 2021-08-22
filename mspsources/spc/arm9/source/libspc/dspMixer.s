#	.section .itcm,"ax",%progbits
	.section .text
	.ARM
	.ALIGN

#include "spc_debug.h"

@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@ Function called with:
@ r0 - Raw brr data (s8*)
@ r1 - Decoded sample data (s16*)
@ r2 - DspChannel *channel
@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@ Function Data:
@ r4 - sample1
@ r5 - sample2
@ r6,r7 - tmp
@ r8 - shift amount
@ r9 - number of iterations left
@ r10 - 0xf
@ r11 - low clip
@ r12 - high clip

#define PREV0 r2
#define PREV1 r3
#define SAMPLE1 r4
#define SAMPLE2 r5
#define BRR_TAB r8
#define ITER_LEFT r9
#define CONST_F r10

.GLOBAL brrHash
brrHash:
.word 0

.GLOBAL DecodeSampleBlockAsm
DecodeSampleBlockAsm:
    stmfd sp!, {r4-r12,r14}

    @ Save the last 4 samples for gaussian interpolation
    add r4, r1, #12 * 2
    ldmia r4, {r5,r6}
    sub r4, r1, #4 * 2
    stmia r4, {r5,r6}

    @ Save the channel pointer
    mov r14, r2

/*    @ Hash the block, and skip the decode if we can
    ldmia r0, {r4-r7}
    ldr r8, =0x050C5D1F @ (2166136261 * 16777619)
    ldr r9, =16777619
    eor r8, r8, r4
    mul r8, r9, r8
    eor r8, r8, r5
    mul r8, r9, r8
    eor r8, r8, r6

    @ r8 is the hashed value

    ldr r4, brrHash

    @ Compute the actual brr location minus the apu ram base
    ldr r6, =APU_MEM
    ldr r6, [r6]
    sub r6, r0, r6

    @ Compute the address of the cached samples in brrHash
    add r3, r4, #0x8000
    mov r6, r6, lsr #3
    add r3, r3, r6, lsl #5

    @ Load the previous hash value
    ldr r5, [r4, r6, lsl #2]
    str r8, [r4, r6, lsl #2]
    cmp r5, r8
    bne doDecode

    @ Load the cached samples
    ldmia r3, {r4-r11}
    stmia r1!, {r4-r11}

    ldrsh PREV0, [r1, #-2]
    ldrsh PREV1, [r1, #-4]

    b doneDecodeCached

doDecode:
    stmfd sp!, {r3}
*/

    @ Load prev0 and prev1
    ldrsh PREV0, [r14, #62]
    ldrsh PREV1, [r14, #64]
    
    ldrb r4, [r0], #1
    @ Compute the index into the brrTab to load the bytes from
    mov r9, r4, lsr #4
    ldr r8, =brrTab
    add r8, r8, r9, lsl #5 @ brrTabPtr = brrTab + (r4 * 32)

    mov CONST_F, #0xf << 1
    ldr r11, =0xffff8000
    ldr r12, =0x7fff

    @ 16 samples to decode, but do two at a time
    mov ITER_LEFT, #8
    @ Figure out the type of decode filter
    mov r4, r4, lsr #2
    and r4, r4, #3
    ldr pc, [pc, r4, lsl #2]
    nop
.word case0
.word case1
.word case2
.word case3
case0:
    ldrb r4, [r0], #1
    and r5, CONST_F, r4, lsl #1
    ldrsh SAMPLE2, [BRR_TAB, r5]
    and r4, CONST_F, r4, lsr #3
    ldrsh SAMPLE1, [BRR_TAB, r4]

    mov r4, r4, lsl #1
    mov r5, r5, lsl #1
    strh r4, [r1], #2
    strh r5, [r1], #2

    subs ITER_LEFT, ITER_LEFT, #1
    bne case0

    @ Set up prev0 and prev1
    ldrsh PREV0, [r1, #-2]
    ldrsh PREV1, [r1, #-4]
    
    b doneDecode

case1:
    ldrb r4, [r0], #1
    and r5, CONST_F, r4, lsl #1
    ldrsh SAMPLE2, [BRR_TAB, r5]
    and r4, CONST_F, r4, lsr #3
    ldrsh SAMPLE1, [BRR_TAB, r4]

    @ prev1 = sample1 + (last1 >> 1) - (last1 >> 5)    
    add PREV1, SAMPLE1, PREV0, asr #1
    sub PREV1, PREV1, PREV0, asr #5
    
    cmp PREV1, r12
    movgt PREV1, r12
    cmp PREV1, r11
    movlt PREV1, r11

    mov PREV1, PREV1, lsl #1
    strh PREV1, [r1], #2
    ldrsh PREV1, [r1, #-2]

    @ same for prev0 now
    add PREV0, SAMPLE2, PREV1, asr #1
    sub PREV0, PREV0, PREV1, asr #5

    cmp PREV0, r12
    movgt PREV0, r12
    cmp PREV0, r11
    movlt PREV0, r11

    mov PREV0, PREV0, lsl #1
    strh PREV0, [r1], #2
    ldrsh PREV0, [r1, #-2]

    subs ITER_LEFT, ITER_LEFT, #1
    bne case1

    b doneDecode

case2:
    ldrb r4, [r0], #1
    and r5, CONST_F, r4, lsl #1
    ldrsh SAMPLE2, [BRR_TAB, r5]
    and r4, CONST_F, r4, lsr #3
    ldrsh SAMPLE1, [BRR_TAB, r4]

    @ Sample 1
    mov r6, PREV1, asr #1
    rsb r6, r6, PREV1, asr #5
    mov PREV1, PREV0
    add r7, PREV0, PREV0, asr #1
    rsb r7, r7, #0
    add r6, r6, r7, asr #5
    add r7, SAMPLE1, PREV0
    add PREV0, r6, r7

    cmp PREV0, r12
    movgt PREV0, r12
    cmp PREV0, r11
    movlt PREV0, r11
    mov PREV0, PREV0, lsl #1
    strh PREV0, [r1], #2
    ldrsh PREV0, [r1, #-2]

    @ Sample 2
    mov r6, PREV1, asr #1
    rsb r6, r6, PREV1, asr #5
    mov PREV1, PREV0
    add r7, PREV0, PREV0, asr #1
    rsb r7, r7, #0
    add r6, r6, r7, asr #5
    add r7, SAMPLE2, PREV0
    add PREV0, r6, r7

    cmp PREV0, r12
    movgt PREV0, r12
    cmp PREV0, r11
    movlt PREV0, r11
    mov PREV0, PREV0, lsl #1
    strh PREV0, [r1], #2
    ldrsh PREV0, [r1, #-2]
    
    subs ITER_LEFT, ITER_LEFT, #1
    bne case2

    b doneDecode

case3:
    ldrb r4, [r0], #1
    and r5, CONST_F, r4, lsl #1
    ldrsh SAMPLE2, [BRR_TAB, r5]
    and r4, CONST_F, r4, lsr #3
    ldrsh SAMPLE1, [BRR_TAB, r4]

    @ Sample 1
    add r6, PREV1, PREV1, asr #1
    mov r6, r6, asr #4
    sub r6, r6, PREV1, asr #1
    mov PREV1, PREV0
    add r7, PREV0, PREV0, lsl #2
    add r7, r7, PREV0, lsl #3
    rsb r7, r7, #0
    add r6, r6, r7, asr #7
    add r6, r6, PREV0
    add PREV0, SAMPLE1, r6

    cmp PREV0, r12
    movgt PREV0, r12
    cmp PREV0, r11
    movlt PREV0, r11
    mov PREV0, PREV0, lsl #1
    strh PREV0, [r1], #2
    ldrsh PREV0, [r1, #-2]

    @ Sample 2
    add r6, PREV1, PREV1, asr #1
    mov r6, r6, asr #4
    sub r6, r6, PREV1, asr #1
    mov PREV1, PREV0
    add r7, PREV0, PREV0, lsl #2
    add r7, r7, PREV0, lsl #3
    rsb r7, r7, #0
    add r6, r6, r7, asr #7
    add r6, r6, PREV0
    add PREV0, SAMPLE2, r6

    cmp PREV0, r12
    movgt PREV0, r12
    cmp PREV0, r11
    movlt PREV0, r11
    mov PREV0, PREV0, lsl #1
    strh PREV0, [r1], #2
    ldrsh PREV0, [r1, #-2]

    subs ITER_LEFT, ITER_LEFT, #1
    bne case3

doneDecode:
/*    sub r1, r1, #32
    ldmia r1, {r4-r11}
    ldmfd sp!, {r1}
    stmia r1, {r4-r11}*/

doneDecodeCached:
    @ Store prev0 and prev1
    strh PREV0, [r14, #62]
    strh PREV1, [r14, #64]

    ldmfd sp!, {r4-r12,r14}
    bx lr

// Envelope state definitions
#define ENVSTATE_NONE       0
#define ENVSTATE_ATTACK		1
#define ENVSTATE_DECAY		2
#define ENVSTATE_SUSTAIN	3
#define ENVSTATE_RELEASE	4
#define ENVSTATE_DIRECT		5
#define ENVSTATE_INCREASE	6
#define ENVSTATE_BENTLINE	7
#define ENVSTATE_DECREASE	8
#define ENVSTATE_DECEXP		9

@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@ Function called with:
@ r0 - int Number of samples to mix
@ r1 - u16* mix buffer (left first, right is always MIXBUFSIZE * 4 bytes ahead
@@@@@@@@@@@@@@@@@@@@@@@@@@@@

/*
struct DspChannel {
int sampleSpeed;        0
int samplePos;          4
int envCount;           8
int envSpeed;           12
s16 prevDecode[4];      16
s16 decoded[16];        24
u16 decaySpeed;         56
u16 sustainSpeed;       58
s16 envx;               60
s16 prevSamp1;          62
s16 prevSamp2;          64
u16 blockPos;           66
s16 leftCalcVolume;     68
s16 rightCalcVolume;    70
u8 envState;            72
u8 sustainLevel;        73
s8 leftVolume;          74
s8 rightVolume;         75
s8 keyWait;             76
bool active;            77
u8 brrHeader;           78
bool echoEnabled;       79
};
*/

#define DSPCHANNEL_SIZE 80

#define SAMPLESPEED_OFFSET 0
#define SAMPLEPOS_OFFSET 4
#define ENVCOUNT_OFFSET 8
#define ENVSPEED_OFFSET 12
#define PREVDECODE_OFFSET 16
#define DECODED_OFFSET 24
#define DECAYSPEED_OFFSET 56
#define SUSTAINSPEED_OFFSET 58
#define ENVX_OFFSET 60
#define BLOCKPOS_OFFSET 66
#define LEFTCALCVOL_OFFSET 68
#define RIGHTCALCVOL_OFFSET 70
#define ENVSTATE_OFFSET 72
#define SUSTAINLEVEL_OFFSET 73
#define LEFTVOL_OFFSET 74
#define RIGHTVOL_OFFSET 75
#define KEYWAIT_OFFSET 76
#define ACTIVE_OFFSET 77
#define ECHOENABLED_OFFSET 79

#define SAMPLE_SPEED r6
#define SAMPLE_POS r7
#define ENV_COUNT r8
#define ENV_SPEED r9
#define LEFT_CALC_VOL r10
#define RIGHT_CALC_VOL r11

.GLOBAL keyingOn
keyingOn:
.byte 0

.ALIGN
.GLOBAL DspEmulateOneSample
DspEmulateOneSample:
    stmfd sp!, {r4-r12, lr}

    @ Store the original mix buffer
    stmfd sp!, {r0}

    ldr r0, =channels
    mov r1, #0
    mov r2, #0
    mov r3, #0
    mov r4, #0
    mov r5, #0

    @ r0 - channel
    @ r1 - channelNum
    @ r2 - mixleft
    @ r3 - mixright
    @ r4 - echoleft
    @ r5 - echoright
    @ r6-r9 : used
    @ r10, r11 : calculated volumes
    @ r12 - tmp
    @ r14 - tmp
channelLoopback:
    ldrb r12, keyingOn
    mov r14, #1
    tst r12, r14, lsl r1
    beq noKeyOn
    ldrb r6, [r0, #KEYWAIT_OFFSET]
    subs r6, r6, #1
    strb r6, [r0, #KEYWAIT_OFFSET]
    bne noKeyOn
    bic r12, r12, r14, lsl r1
    strb r12, keyingOn
    stmfd sp!, {r0-r3, lr}
    mov r0, r1
#    bl ITCM_DspKeyOnChannel
    bl DspKeyOnChannel
    ldmfd sp!, {r0-r3, lr}

noKeyOn:
    @ Check if active == 0, then next
    ldrb r12, [r0, #ACTIVE_OFFSET]
    cmps r12, #0
    beq nextChannelNothingDone

    @ Load the important variables into registers
    ldmia r0, {r6-r9}
    ldrsh LEFT_CALC_VOL, [r0, #LEFTCALCVOL_OFFSET]
    ldrsh RIGHT_CALC_VOL, [r0, #RIGHTCALCVOL_OFFSET]

    @ Commence the mixing
    subs ENV_COUNT, ENV_COUNT, ENV_SPEED
    cmp ENV_COUNT, #0
    bgt noEnvelopeUpdate

    @ Update envelope
    mov ENV_COUNT, #0x7800

    ldrsh r14, [r0, #ENVX_OFFSET]
    ldrb r12, [r0, #ENVSTATE_OFFSET]

    ldr pc, [pc, r12, lsl #2]
    nop
@ Jump table for envelope handling
.word noEnvelopeUpdate
.word envStateAttack
.word envStateDecay
.word envStateSustain
.word envStateRelease
.word noEnvelopeUpdate      @ Actually direct, but we don't need to do anything
.word envStateIncrease
.word envStateBentline
.word envStateDecrease
.word envStateSustain       @ Actually decrease exponential, but it's the same code

#define ENVX_SHIFT 8
#define ENVX_MAX 0x7f00

envStateAttack:
    add r14, r14, #4 << ENVX_SHIFT

    cmp r14, #ENVX_MAX
    ble storeEnvx
    @ envx = 0x7f, state = decay, speed = decaySpeed
    mov r14, #ENVX_MAX
    mov r12, #ENVSTATE_DECAY
    strb r12, [r0, #ENVSTATE_OFFSET]
    ldrh ENV_SPEED, [r0, #DECAYSPEED_OFFSET]
    b storeEnvx

envStateDecay:
    rsb r14, r14, r14, lsl #8
    mov r14, r14, asr #8

    ldrb r12, [r0, #SUSTAINLEVEL_OFFSET]
    cmp r14, r12, lsl #ENVX_SHIFT
    bge storeEnvx
    @ state = sustain, speed = sustainSpeed
    mov r12, #ENVSTATE_SUSTAIN
    strb r12, [r0, #ENVSTATE_OFFSET]
    ldrh ENV_SPEED, [r0, #SUSTAINSPEED_OFFSET]

    @ Make sure envx > 0
    cmp r14, #0
    bge storeEnvx

    @ If not, end channel, then go to next channel
    stmfd sp!, {r0-r3, r14}
    mov r0, r1
    bl DspSetEndOfSample
    ldmfd sp!, {r0-r3, r14}
    b nextChannel
    
envStateSustain:
    rsb r14, r14, r14, lsl #8
    mov r14, r14, asr #8

    @ Make sure envx > 0
    cmp r14, #0
    bge storeEnvx

    @ If not, end channel, then go to next channel
    stmfd sp!, {r0-r3,r14}
    mov r0, r1
    bl DspSetEndOfSample
    ldmfd sp!, {r0-r3,r14}
    b nextChannel

envStateRelease:
    sub r14, r14, #1 << ENVX_SHIFT

    @ Make sure envx > 0
    cmp r14, #0
    bge storeEnvx

    @ If not, end channel, then go to next channel
    stmfd sp!, {r0-r3,r14}
    mov r0, r1
    bl DspSetEndOfSample
    ldmfd sp!, {r0-r3,r14}
    b nextChannel

envStateIncrease:
    add r14, r14, #4 << ENVX_SHIFT

    cmp r14, #ENVX_MAX
    ble storeEnvx
    @ envx = 0x7f, state = direct, speed = 0
    mov r14, #ENVX_MAX
    mov r12, #ENVSTATE_DIRECT
    strb r12, [r0, #ENVSTATE_OFFSET]
    mov ENV_SPEED, #0
    b storeEnvx

envStateBentline:
    cmp r14, #0x5f << ENVX_SHIFT
    addgt r14, r14, #1 << ENVX_SHIFT
    addle r14, r14, #4 << ENVX_SHIFT

    cmp r14, #ENVX_MAX
    blt storeEnvx
    @ envx = 0x7f, state = direct, speed = 0
    mov r14, #ENVX_MAX
    mov r12, #ENVSTATE_DIRECT
    strb r12, [r0, #ENVSTATE_OFFSET]
    mov ENV_SPEED, #0
    b storeEnvx

envStateDecrease:
    sub r14, r14, #4 << ENVX_SHIFT

    @ Make sure envx > 0
    cmp r14, #0
    bge storeEnvx
    
    @ If not, end channel, then go to next channel
    stmfd sp!, {r0-r3,r14}
    mov r0, r1
    bl DspSetEndOfSample
    ldmfd sp!, {r0-r3,r14}
    b nextChannel

storeEnvx:
    strh r14, [r0, #ENVX_OFFSET]

    @ Recalculate leftCalcVol and rightCalcVol
    ldrsb LEFT_CALC_VOL, [r0, #LEFTVOL_OFFSET]
    mul LEFT_CALC_VOL, r14, LEFT_CALC_VOL
    mov LEFT_CALC_VOL, LEFT_CALC_VOL, asr #7
    strh LEFT_CALC_VOL, [r0, #LEFTCALCVOL_OFFSET]

    ldrsb RIGHT_CALC_VOL, [r0, #RIGHTVOL_OFFSET]
    mul RIGHT_CALC_VOL, r14, RIGHT_CALC_VOL
    mov RIGHT_CALC_VOL, RIGHT_CALC_VOL, asr #7
    strh RIGHT_CALC_VOL, [r0, #RIGHTCALCVOL_OFFSET]
    
noEnvelopeUpdate:
    add SAMPLE_POS, SAMPLE_POS, SAMPLE_SPEED
    cmp SAMPLE_POS, #16 << 12
    blo noSampleUpdate
    
    @ Decode next 16 bytes...
    sub SAMPLE_POS, SAMPLE_POS, #16 << 12

    @ Decode the sample block, r0 = DspChannel*
    stmfd sp!, {r0-r3, r14}
    bl DecodeSampleBlock
    cmps r0, #1
    ldmfd sp!, {r0-r3, r14}
    beq nextChannel

noSampleUpdate:
    @ This is really a >> 12 then << 1, but since samplePos bit 0 will never be set, it's safe.
    @ Must ensure that sampleSpeed bit 0 is never set, and samplePos is never set to anything but 0
    @ TODO - The speed up hack doesn't work.  Find out why
    mov r12, SAMPLE_POS, lsr #12
    add r12, r0, r12, lsl #1

@  Linear interpolation
/*
    stmfd sp!, {r0,r1}    
    ldrsh r14, [r12, #DECODED_OFFSET - 2]
    ldrsh r0, [r12, #DECODED_OFFSET]
    mov r1, #0xff
    and r1, r1, SAMPLE_POS, lsr #4
    rsb r1, r1, #0x100
    sub r0, r0, r14
    mul r0, r1, r0
    add r14, r14, r0, asr #8
    ldmfd sp!, {r0,r1}
*/

@  No interpolation 
@    ldrsh r14, [r12, #DECODED_OFFSET]

    @ Store changing values
    stmia r0, {r6-r9}

    @ r6 will be the fractional position
    ldr r6, =0x1fe
    and r6, r6, SAMPLE_POS, lsr #3
    rsb r6, r6, #0x200

    ldr r7, =gauss - 2
    ldrsh r8, [r7, r6]
    ldrsh r9, [r12, #DECODED_OFFSET - 6]
    mul r14, r8, r9
    ldr r7, =gauss + 510
    ldrsh r8, [r7, r6]
    ldrsh r9, [r12, #DECODED_OFFSET - 4]
    mla r14, r8, r9, r14

    rsb r6, r6, #0
    ldr r7, =gauss + 1024
    ldrsh r8, [r7, r6]
    ldrsh r9, [r12, #DECODED_OFFSET - 2]
    mla r14, r8, r9, r14
    ldr r7, =gauss + 512
    ldrsh r8, [r7, r6]
    ldrsh r9, [r12, #DECODED_OFFSET]
    mla r14, r8, r9, r14
    mov r14, r14, asr #11

    @ r14 is the new sample

    @ Get echo enabled
    ldrb r12, [r0, #ECHOENABLED_OFFSET]
    cmp r12, #0
    beq mixEchoDisabled

mixEchoEnabled:
    @ Echo mixing
    mla r4, r14, LEFT_CALC_VOL, r4
    mla r5, r14, RIGHT_CALC_VOL, r5

mixEchoDisabled:
    mla r2, r14, LEFT_CALC_VOL, r2
    mla r3, r14, RIGHT_CALC_VOL, r3

    b nextChannelAlreadyStored
nextChannel:
    @ Store changing values
    stmia r0, {r6-r9}
nextChannelAlreadyStored:

    @ Set ENVX and OUTX
    ldr r12, =DSP_MEM
    add r12, r12, r1, lsl #4

    @ Set ENVX
    ldrsh r9, [r0, #ENVX_OFFSET]
    mov r9, r9, asr #ENVX_SHIFT
    strb r9, [r12, #0x8]

    @ Set OUTX
    mul r9, r14, r9
    mov r9, r9, asr #15
    strb r9, [r12, #0x9]

nextChannelNothingDone:
    @ Move to next channel
    add r0, r0, #DSPCHANNEL_SIZE

    add r1, r1, #1
    cmps r1, #8
    blt channelLoopback

@ This is the end of normal mixing

    @ r0 - 
    @ r1 - 
    @ r2 - left 
    @ r3 - right
    @ r4 - echo left
    @ r5 - echo right
    @ r6 - tmp
    @ r7 - echo in apu ram (r/w)
    @ r8 - dsp_mem
    @ r9 - end of echo in apu ram
    @ r10 - tmp
    @ r11 - echo feedback
    @ r12 - FIR coefficients in DSP ram
    @ r13 - sp
    @ r14 - tmp

@ Process the echo filter stuff
echoMixSetup:
    ldr r8, =DSP_MEM
    @ Get echo base (APU_MEM + DSP_ESA << 8)
    ldr r7, =echoBase
    ldr r7, [r7]
    str r7, echoBufferStart

    @ Set up current echo cursor location
    ldr r0, =echoCursor
    ldrh r0, [r0]
    add r7, r7, r0

    @ Offset firTable to start at FIR #7
    add r12, r8, #0x7F

echoMixLoopback:
    @ Load the old echo value (l,r)
    ldrsh r0, [r7]
    ldrsh r1, [r7, #2]

    @ Increment and wrap firOffset
    ldr r6, =firOffset
    ldrb r14, [r6]
    add r14, r14, #2
    and r14, r14, #(8 * 2) - 1
    strb r14, [r6]

    @ Get &firTable[firOffset + 8] into r9
    ldr r9, =firTable + ((8 * 2) * 4)
    add r9, r9, r14, lsl #2

    @ Store the computed samples in the FIR ring buffer
    str r0, [r9]
    str r1, [r9, #4]
    str r0, [r9, #-8 * 2 * 4]
    str r1, [r9, #(-8 * 2 * 4) + 4]

    @ Process FIR sample 0 (special)
    ldrsb r4, [r12], #-0x10
    ldr r6, [r9], #4
    mul r0, r6, r4
    ldr r6, [r9], #-12
    mul r1, r6, r4

.MACRO processFir
    ldrsb r4, [r12], #-0x10
    ldr r6, [r9], #4
    mla r0, r6, r4, r0
    ldr r6, [r9], #-12
    mla r1, r6, r4, r1
.ENDM
    processFir
    processFir
    processFir
    processFir
    processFir
    processFir

    @ Last FIR sample (special)
    ldrsb r4, [r12], #0x70
    ldr r6, [r9], #4
    mla r0, r6, r4, r0
    ldr r6, [r9], #-12
    mla r1, r6, r4, r1

    @ r0,r1 contains the filtered samples (<< 8 at this point)

    @ Add echo into main mix values
    ldrsb r6, [r8, #0x0C] @ Main left volume
    mov r2, r2, asr #7
    mul r2, r6, r2
    ldrsb r6, [r8, #0x2C] @ Get left echo volume
    mla r2, r0, r6, r2
    ldrsb r6, [r8, #0x1C] @ Main right volume
    mov r3, r3, asr #7
    mul r3, r6, r3
    ldrsb r6, [r8, #0x3C] @ Get right echo volume
    mla r3, r1, r6, r3
    
    @ Get echo feedback
    ldrsb r11, [r8, #0x0D]
    @ Left echo = ((feedback * filtered) >> 7) + leftEcho
    mla r4, r11, r0, r4
    mov r4, r4, asr #15

    @ Right echo = ((feedback * filtered) >> 7) + rightEcho
    mla r5, r11, r1, r5
    mov r5, r5, asr #15

    @ Set up end of echo delay area in r9
    ldr r0, =echoDelay
    ldrh r0, [r0]
    ldr r9, echoBufferStart
    add r9, r9, r0

    @ Store computed echo into echobuffer (if writing is enabled)
    ldrb r6, [r8, #0x6C]
    tst r6, #0x20
    streqh r4, [r7], #2
    streqh r5, [r7], #2
    cmp r7, r9
    ldrge r7, echoBufferStart

doneEchoMix:

    ldr r0, echoBufferStart
    sub r7, r7, r0
    ldr r0, =echoCursor
    strh r7, [r0]

clipAndMix:
    @ Put the original output buffer into r0
    ldmfd sp!, {r0}
    
    @ Set up the preamp & overall volume
    ldr r6, =dspPreamp
    ldrh r6, [r6]

    @ r0 - output buffer
    @ r1 - left
    @ r2 - right
    @ r3 - echo left
    @ r4 - echo right
    @ r5 - tmp
    @ r6 - tmp
    @ r7 - main left
    @ r8 - main right
    @ r9 - 
    @ r10 - 
    @ r11 - 
    @ r12 - 
    @ r14 - 

    @ Load and scale by preamp (LEFT)
    mov r2, r2, asr #15
    mul r2, r6, r2
    mov r2, r2, asr #7

    @ Clip and store
    cmp r2, #0x7f00
    movgt r2, #0x7f00
    cmn r2, #0x7f00
    movlt r2, #0x8100
    
    strh r2, [r0], #MIXBUFSIZE * 2

    @ Load and scale by volume (RIGHT)
    mov r3, r3, asr #15
    mul r3, r6, r3
    mov r3, r3, asr #7

    @ Clip and store
    cmp r3, #0x7f00
    movgt r3, #0x7f00
    cmn r3, #0x7f00
    movlt r3, #0x8100
    strh r3, [r0]

doneMix:
    ldmfd sp!, {r4-r12, lr}
    bx lr

tmpSp:
.word 0
echoBufferStart:
.word 0
tmpHalfword:
.hword 0

.align
.pool
