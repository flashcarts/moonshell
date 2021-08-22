
#include <stdio.h>

#include <NDS.h>

#include "_const.h"

#include "plugin.h"
#include "plugin_def.h"

// CallBack from plugin_dll.cpp

void cbLoadLibrary(void)
{
}

void cbFreeLibrary(void)
{
}

void cbQueryInterfaceLibrary(void)
{
}

// -----------------

// ------------------------------------------------------------------------------------

extern "C" {
bool StartOGG(int _FileHandle);
void FreeOGG(void);
u32 UpdateOGG(s16 *lbuf,s16 *rbuf);
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
}

bool Start(int FileHandle)
{
  return(StartOGG(FileHandle));
}

void Free(void)
{
  FreeOGG();
}

u32 Update(s16 *lbuf,s16 *rbuf)
{
  return(UpdateOGG(lbuf,rbuf));
}

s32 GetPosMax(void)
{
  return(OGG_GetFileSize());
}

s32 GetPosOffset(void)
{
  return(OGG_GetFileOffset());
}

void SetPosOffset(s32 ofs)
{
  OGG_SetFileOffset(ofs);
}

u32 GetChannelCount(void)
{
  return(OGG_GetChannelCount());
}

u32 GetSampleRate(void)
{
  return(OGG_GetSampleRate());
}

u32 GetSamplePerFrame(void)
{
  return(OGG_GetSamplePerFrame());
}

int GetInfoIndexCount(void)
{
  return(OGG_GetInfoIndexCount());
}

bool GetInfoStrA(int idx,char *str,int len)
{
  return(OGG_GetInfoStrA(idx,str,len));
}

bool GetInfoStrW(int idx,UnicodeChar *str,int len)
{
  return(OGG_GetInfoStrW(idx,str,len));
}

bool GetInfoStrUTF8(int idx,char *str,int len)
{
  return(OGG_GetInfoStrUTF8(idx,str,len));
}

// -----------------------------------------------------------

