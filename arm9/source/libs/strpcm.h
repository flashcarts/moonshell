
#ifndef strpcm_h
#define strpcm_h

#include <NDS.h>
#include "../../IPC6.h"

#define strpcmRingBufCount (16)
#define strpcmRingBufBitMask (strpcmRingBufCount-1)

extern vu64 DPGAudioStream_SyncSamples;
extern u32 DPGAudioStream_PregapSamples;

extern volatile bool VBlankPassedFlag;
extern volatile u32 VBlankPassedCount;

extern volatile bool strpcmRequestStop;

extern volatile bool strpcmRingEmptyFlag;
extern volatile u32 strpcmRingBufReadIndex;
extern volatile u32 strpcmRingBufWriteIndex;

extern s16 *strpcmRingLBuf;
extern s16 *strpcmRingRBuf;

extern void InterruptHandler(void);
extern void InitInterrupts(void);

extern void strpcmStart(bool FastStart,u32 SampleRate,u32 SamplePerBuf,u32 ChannelCount,EstrpcmFormat strpcmFormat);
extern void strpcmStop(void);

#define strpcmVolumeMax (96)

extern void strpcmSetVolume64(int v);
extern int strpcmGetVolume64(void);

extern void VBlank_AutoFlip_Enabled(void);
extern void VBlank_AutoFlip_Disabled(void);

#endif
