
#ifndef _OGG_h
#define _OGG_h

#define OGGTitle "libogg Tremor 1.0 (c)2002 Xiph.org"

#ifdef __cplusplus
extern "C" {
#endif

void OGG_SetFunc_consolePrintf(u32 adr);

bool StartOGG(int _FileHandle);
u32 UpdateOGG(s16 *lbuf,s16 *rbuf);
void FreeOGG(void);

s32 OGG_GetFileSize(void);
s32 OGG_GetFileOffset(void);
void OGG_SetFileOffset(s32 ofs);

u32 OGG_GetChannelCount(void);
u32 OGG_GetSampleRate(void);
u32 OGG_GetSamplePerFrame(void);

int OGG_GetInfoIndexCount(void);
bool OGG_GetInfoStrA(int idx,char *str,int len);
bool OGG_GetInfoStrW(int idx,UnicodeChar *str,int len);
bool OGG_GetInfoStrUTF8(int idx,char *str,int len);

#ifdef __cplusplus
}
#endif

#endif
