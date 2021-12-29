
#ifndef dllsound_h
#define dllsound_h

#include <NDS.h>

extern bool GlobalPauseFlag;

enum TID3Tag_LinesIndex {EID3TagLI_Title=0,EID3TagLI_Artist,EID3TagLI_Album,EID3TagLI_Comment,EID3TagLI_Count};

#define ID3Tag_LineMaxLength (32)

typedef struct {
  bool Exists;
  const char *pStrL;
  UnicodeChar StrW[ID3Tag_LineMaxLength];
} TID3Tag_Line;

typedef struct {
  bool Exists;
  u32 LinesExistsCount;
  TID3Tag_Line Lines[EID3TagLI_Count];
} TID3Tag;

extern TID3Tag ID3Tag;

extern void DLLSound_Open(const char *pFilename);
extern void DLLSound_Close(void);
extern bool DLLSound_isOpened(void);
extern bool DLLSound_Update(void);
extern void DLLSound_WaitForStreamPCM(void);
extern void DLLSound_SetVolume64(u32 v);

extern s32 DLLSound_GetPosMax(void);
extern s32 DLLSound_GetPosOffset(void);
extern void DLLSound_SetPosOffset(s32 pos);

extern u32 DLLSoung_GetBitRatePerByte(void);
extern u32 DLLSound_GetPlayTimeSec(u32 BitRatePerByte);
extern u32 DLLSound_GetCurrentTimeSec(u32 BitRatePerByte);

#endif

