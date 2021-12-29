
#ifndef plug_png_interface_h
#define plug_png_interface_h

#include <NDS.h>
#include <NDS/ARM9/CP15.h>
#include "boolean.h"

extern "C" void PNGINT_SetFunc_ConsolePrintf(u32 adr);
extern "C" void PNGINT_SetFunc_safemalloc(u32 adr);
extern "C" void PNGINT_SetFunc_safefree(u32 adr);

extern "C" b8 PNGINT_Init(int FileHandle);
extern "C" void PNGINT_Free(void);

extern "C" s32 PNGINT_GetWidth(void);
extern "C" s32 PNGINT_GetHeight(void);

extern "C" void PNGINT_GetNextLine(u8 *buf);

#endif

