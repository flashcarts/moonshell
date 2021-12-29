
#include <stdio.h>

#include <NDS.h>

#include "_const.h"

#include "plugin.h"
#include "plugin_def.h"

#include "png.h"
#include "plug_png_interface.h"

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

static s32 biWidth=0,biHeight=0;

b8 LoadPNG(int FileHandle)
{
  biWidth=0;
  biHeight=0;
  
  if(PNGINT_Init(FileHandle)==False){
    return(False);
  }
  
  biWidth=PNGINT_GetWidth();
  biHeight=PNGINT_GetHeight()-1;
  
  return(True);
}

// ------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------

bool Start(int FileHandle)
{
  if(LoadPNG(FileHandle)==False){
    _consolePrintf("PNG LoadError.\n");
    return(false);
  }
  
  return(true);
}

void Free(void)
{
  PNGINT_Free();
  
  biWidth=0;
  biHeight=0;
}

void GetBitmap24(u32 LineY,u8 *pBM)
{
  if(biHeight<=LineY) return;
  PNGINT_GetNextLine(pBM);
}

s32 GetWidth(void)
{
  return(biWidth);
}

s32 GetHeight(void)
{
  return(biHeight);
}

int GetInfoIndexCount(void)
{
  return(0);
}

bool GetInfoStrA(int idx,char *str,int len)
{
  return(false);
}

bool GetInfoStrW(int idx,UnicodeChar *str,int len)
{
  return(false);
}

bool GetInfoStrUTF8(int idx,char *str,int len)
{
  return(false);
}

