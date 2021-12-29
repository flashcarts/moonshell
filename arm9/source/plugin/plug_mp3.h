
#ifndef _MP3_h
#define _MP3_h

#include "fat2.h"

#define MP3Title "libmad MP3 audio plugin"

extern bool PlugMP3_Start(FAT_FILE *FileHandle);
extern void PlugMP3_Free(void);
extern u32 PlugMP3_Update(s16 *lbuf,s16 *rbuf);
extern s32 PlugMP3_GetPosMax(void);
extern s32 PlugMP3_GetPosOffset(void);
extern void PlugMP3_SetPosOffset(s32 ofs);
extern u32 PlugMP3_GetChannelCount(void);
extern u32 PlugMP3_GetSampleRate(void);
extern u32 PlugMP3_GetSamplePerFrame(void);
extern u32 PlugMP3_GetBitRate(void);
extern int PlugMP3_GetInfoIndexCount(void);
extern bool PlugMP3_GetInfoStrL(int idx,char *str,int len);
extern bool PlugMP3_GetInfoStrW(int idx,UnicodeChar *str,int len);
extern bool PlugMP3_GetInfoStrUTF8(int idx,char *str,int len);
extern bool PlugMP3_ExistsID3Tag(void);
extern const char* PlugMP3_ID3Tag_GetTitle(void);
extern const char* PlugMP3_ID3Tag_GetArtist(void);
extern const char* PlugMP3_ID3Tag_GetAlbum(void);
extern const char* PlugMP3_ID3Tag_GetComment(void);

#endif
