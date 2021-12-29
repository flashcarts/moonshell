
#include <stdio.h>

#include <NDS.h>

#include "plugin.h"
#include "plugin_def.h"

const TPlugin_StdLib *pStdLib;

bool LoadLibrary(const TPlugin_StdLib *_pStdLib)
{
  pStdLib=_pStdLib;
  
  extern void cbLoadLibrary(void);
  cbLoadLibrary();
  
  return(true);
}

void FreeLibrary(void)
{
  extern void cbFreeLibrary(void);
  cbFreeLibrary();
}

int QueryInterfaceLibrary(void)
{
/*
  extern void cbQueryInterfaceLibrary(void);
  cbQueryInterfaceLibrary();
*/
  
#ifdef PluginMode_Image
  static TPlugin_ImageLib IL;
  
  extern bool Start(int FileHandle);
  IL.Start=Start;
  extern void Free(void);
  IL.Free=Free;
  
  extern void GetBitmap24(u32 LineY,u8 *pBM);
  IL.GetBitmap24=GetBitmap24;
  
  extern s32 GetWidth(void);
  IL.GetWidth=GetWidth;
  extern s32 GetHeight(void);
  IL.GetHeight=GetHeight;
  
  extern int GetInfoIndexCount(void);
  IL.GetInfoIndexCount=GetInfoIndexCount;
  extern bool GetInfoStrA(int idx,char *str,int len);
  IL.GetInfoStrA=GetInfoStrA;
  extern bool GetInfoStrW(int idx,UnicodeChar *str,int len);
  IL.GetInfoStrW=GetInfoStrW;
  extern bool GetInfoStrUTF8(int idx,char *str,int len);
  IL.GetInfoStrUTF8=GetInfoStrUTF8;
  
  return((int)&IL);
#endif
  
#ifdef PluginMode_Sound
  static TPlugin_SoundLib SL;
  
  extern bool Start(int FileHandle);
  SL.Start=Start;
  extern void Free(void);
  SL.Free=Free;
  
  extern u32 Update(s16 *lbuf,s16 *rbuf);
  SL.Update=Update;
  
  extern s32 GetPosMax(void);
  SL.GetPosMax=GetPosMax;
  extern s32 GetPosOffset(void);
  SL.GetPosOffset=GetPosOffset;
  extern void SetPosOffset(s32 ofs);
  SL.SetPosOffset=SetPosOffset;
  
  extern u32 GetChannelCount(void);
  SL.GetChannelCount=GetChannelCount;
  extern u32 GetSampleRate(void);
  SL.GetSampleRate=GetSampleRate;
  extern u32 GetSamplePerFrame(void);
  SL.GetSamplePerFrame=GetSamplePerFrame;
  
  extern int GetInfoIndexCount(void);
  SL.GetInfoIndexCount=GetInfoIndexCount;
  extern bool GetInfoStrA(int idx,char *str,int len);
  SL.GetInfoStrA=GetInfoStrA;
  extern bool GetInfoStrW(int idx,UnicodeChar *str,int len);
  SL.GetInfoStrW=GetInfoStrW;
  extern bool GetInfoStrUTF8(int idx,char *str,int len);
  SL.GetInfoStrUTF8=GetInfoStrUTF8;
  
  return((int)&SL);
#endif
}

