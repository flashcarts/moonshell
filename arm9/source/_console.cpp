/*---------------------------------------------------------------------------------
	$Id: console.c,v 1.4 2005/07/14 08:00:57 wntrmute Exp $

	console code -- provides basic print functionality

  Copyright (C) 2005
			Michael Noland (joat)
			Jason Rogers (dovoto)
			Dave Murphy (WinterMute)

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any
  damages arising from the use of this software.

  Permission is granted to anyone to use this software for any
  purpose, including commercial applications, and to alter it and
  redistribute it freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you
     must not claim that you wrote the original software. If you use
     this software in a product, an acknowledgment in the product
     documentation would be appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and
     must not be misrepresented as being the original software.
  3. This notice may not be removed or altered from any source
     distribution.

	$Log: console.c,v $
	Revision 1.4  2005/07/14 08:00:57  wntrmute
	resynchronise with ndslib
	

---------------------------------------------------------------------------------*/

#include <nds.h>

#include <stdio.h>
#include <string.h>
#include <stdarg.h>

#include "_console.h"
#include "_const.h"
#include "memtool.h"
#include "maindef.h"
#include "disc_io.h"
#include "splash.h"
#include "shell.h"

/////////////////////////////////////////
//global console variables

#define CONSOLE_USE_COLOR255 16

#define CONSOLE_MAPWIDTH (64)
#define CONSOLE_WIDTH (256/6)
#define CONSOLE_HEIGHT (192/6)
#define TAB_SIZE 3

//map to print to
static u16* fontMap;

//location of cursor
static u8 row, col;

static volatile u32 stNullChar=0;

static bool LogOutFlag=true;

typedef struct {
  u32 WriteTopSector;
  u32 TopPos;
  u32 BufPos;
  u32 BufSize;
  char *pBuf;
  bool Pause;
} TLogFile;

static TLogFile LogFile;

///////////////////////////////////////////////////////////
//consoleInit
// param: 
//		font: 16 color font
//		charBase: the location the font data will be loaded to
//		numCharacters: count of characters in the font
//		charStart: The ascii number of the first character in the font set
//					if you have a full set this will be zero
//		map: pointer to the map you will be printing to.
//		pal: specifies the 16 color palette to use, if > 15 it will change all non-zero
//			entries in the font to use palette index 255
/*
static void _consoleInit256(u16* font, u16* charBase, u16 numCharacters, u16* map)
{
	int i;

	row = col = 0;
	
	fontMap = map;

	for (i = 0; i < numCharacters * (8*8*8/16); i++){ // 8x8x8bit (16bitBus)
		charBase[i] = font[i];
	}
}
*/

static void _consoleInit256Packed(u8* packedfont, u16* charBase, u16 numCharacters, u16* map)
{
	int i;

  col=0;
	row=CONSOLE_HEIGHT-1;
	
	fontMap = map;

	for (i = 0; i < numCharacters * (8*8*2/8); i++){ // 8x8x2bit (8bitBus)
	  u8 src=packedfont[i];
	  {
	    u16 data=0;
	    data|=((src>>0) & 3);
	    data|=((src>>2) & 3) << 8;
	    charBase[i*2+0]=data;
	  }
	  {
	    u16 data=0;
	    data|=((src>>4) & 3);
	    data|=((src>>6) & 3) << 8;
	    charBase[i*2+1]=data;
	  }
	}
}

#include "_console_font_fixed6x6_packed_bin.h"

void _consoleInitDefault(u16* map, u16* charBase) {
//	_consoleInit256((u16*)_console_font_fixed6x6_bin, charBase, 256, map);
	_consoleInit256Packed((u8*)_console_font_fixed6x6_packed_bin, charBase, 256, map);
	
	TLogFile *plf=&LogFile;
	plf->WriteTopSector=0;
  plf->TopPos=0;
  plf->BufPos=0;
  plf->BufSize=0;
  plf->pBuf=NULL;
  plf->Pause=false;
}

void _consolePrintSet(int x, int y) {
	if(y < CONSOLE_HEIGHT)
		row = y;
	else
		row = CONSOLE_HEIGHT - 1;

	if(x < CONSOLE_WIDTH)
		col = x;
	else
		col = CONSOLE_WIDTH - 1;
}

int _consoleGetPrintSetY(void)
{
  return(row);
}

void _consoleClear(void)
{
//	for(int i = 0; i < CONSOLE_HEIGHT * (CONSOLE_MAPWIDTH/2); i++) fontMap[i] = 0;
	
	{
	  u16 *dmadst=&fontMap[0*(CONSOLE_MAPWIDTH/2)];
	  u16 dmasize=CONSOLE_HEIGHT*CONSOLE_MAPWIDTH;
	  DCache_FlushRangeOverrun((void*)dmadst,dmasize);
	  DMA3_SRC = (uint32)&stNullChar;
	  DMA3_DEST = (uint32)dmadst;
	  DMA3_CR = DMA_ENABLE | DMA_SRC_FIX | DMA_DST_INC | DMA_32_BIT | (dmasize>>2);
	  while(DMA3_CR & DMA_BUSY);
	}
	
	_consolePrintSet(0,0);
}

asm void _consolePrintChar_ScrollLine(u16 *psrc,u16 *pdst,u32 size)
{
psrc RN r0
pdst RN r1
size RN r2

  PUSH {r4,r5,r6}
  
copy32bitx4
  ldmia psrc!,{r3,r4,r5,r6}
  stmia pdst!,{r3,r4,r5,r6}
  subs size,size,#4*4
  cmp size,#4*4
  bne copy32bitx4
  
  mov r3,#0
  mov r4,#0
  mov r5,#0
  mov r6,#0
  
  stmia pdst!,{r3,r4,r5,r6}
  stmia pdst!,{r3,r4,r5,r6}
  stmia pdst!,{r3,r4,r5,r6}
  stmia pdst!,{r3,r4,r5}
  
  POP {r4,r5,r6}
  bx lr      
}

static __attribute__ ((always_inline)) void _consolePrintChar(u32 c)
{
	if(col >= CONSOLE_WIDTH) {
		col = 0;
		row++;		
	}
	
	if(row >= CONSOLE_HEIGHT) {
		row--;
		_consolePrintChar_ScrollLine(&fontMap[1*(CONSOLE_MAPWIDTH/2)],&fontMap[0*(CONSOLE_MAPWIDTH/2)],(CONSOLE_HEIGHT-1)*CONSOLE_MAPWIDTH);
	}
	
	switch(c){
	  case 10: case 11: case 12: case 13: {
	    row++;
	    col = 0;
	  } break;
	  case 9: {
	    col += TAB_SIZE;
	  } break;
	  default: {
	    u32 ofs=(col/2) + (row * (CONSOLE_MAPWIDTH/2));
	    u16 data=fontMap[ofs];
	    u16 dst=(u16)c;
	    if((col&1)==0){
	      data=(data&0xff00) | (dst << 0);
	      }else{
	      data=(data&0x00ff) | (dst << 8);
	    }
	    fontMap[ofs]=data;
	    
	    col++;
	  } break;
	}
}

static bool enagba=false;
//static bool enagba=true;
#define polllimit (0x100000)

static __attribute__ ((always_inline)) void tx_waitsend(const u32 data)
{
  if(enagba==false) return;
  
  u32 poll=0;
	while((*((vu8 *)0x0A000001) & 2)!=0){
	  poll++;
	  if(polllimit<poll){
	    enagba=false;
	    return;
	  }
	}
	
	*((vu8 *)0x0A000000)=(u8)data;
}

static __attribute__ ((always_inline)) void pcprint(const char *p)
{
  if(enagba==false) return;

  u32 len=strlen(p);
  if(len==0) return;
  
  if(254<len) len=254;
	
	s32 idx=-1;
	
	while(1){
	  u32 c;
	  idx++;
	  if(2<=idx){
	    c=p[idx-2];
	    }else{
	    if(idx==0){
	      c=0xa0;
	      }else{
	      c=len+1;
	    }
	  }
    tx_waitsend(c);
    if(c==0) break;
	}
}

static void StoreLogSector(const char *s)
{
	TLogFile *plf=&LogFile;
	
  if(plf->WriteTopSector==0) return;
  
  u32 wsidx=plf->BufPos/512;
  
  while(*s!=0){
    plf->pBuf[plf->BufPos++]=*s++;
    if(plf->BufPos==plf->BufSize) plf->BufPos=plf->TopPos;
  }
  plf->pBuf[plf->BufPos]='!';
  
  if(plf->Pause==false){
    extern LPIO_INTERFACE active_interface;
    active_interface->fn_WriteSectors(plf->WriteTopSector+wsidx,1,&plf->pBuf[wsidx*512]);
  }
}

void _consolePrint(const char* s)
{
//  DTCM_StackCheck(-1); // DPGモードでは使ってはいけない。
  
  pcprint(s);
  
  StoreLogSector(s);
  
  if(LogOutFlag==false) return;
  
  while(*s!=0){
    char c0=s[0],c1=s[1];
    if(0x80<=c0){
      _consolePrintChar('?');
      if(c1==0) return;
      s+=2;
      }else{
      _consolePrintChar(c0);
      s+=1;
    }
  }
}

void _consolePrintf(const char* format, ...)
{
  static char strbuf[126+1];
  
  va_list args;
  
  va_start( args, format );
  vsnprintf( strbuf, 126, format, args );
  _consolePrint(strbuf);
}

void _consoleSetLogFile(void *_pf)
{
  FAT_FILE *pf=(FAT_FILE*)_pf;
  
	TLogFile *plf=&LogFile;
	
	u32 spc=FAT2_GetSecPerClus();
	if(16<spc) spc=16;
	
  if(FAT2_GetFileSize(pf)<(spc*512)){
    _consolePrintf("Fatal error: Log file size error!!\n");
    ShowLogHalt();
  }
  
  plf->WriteTopSector=FAT2_ClustToSect(pf->firstCluster);
  plf->BufPos=0;
  plf->BufSize=spc*512;
  plf->pBuf=(char*)malloc(plf->BufSize);
  plf->Pause=false;
  
  for(u32 idx=0;idx<plf->BufSize;idx++){
    plf->pBuf[idx]='-';
  }
  
  extern LPIO_INTERFACE active_interface;
  active_interface->fn_WriteSectors(plf->WriteTopSector,plf->BufSize/512,plf->pBuf);
  
  LogOutFlag=false;
  
  _consolePrintf("Start log file. topsector=%d size=%dbyte\n",plf->WriteTopSector,plf->BufSize);
//  _consolePrintf("%08x\n",data);
  _consolePrintf("AppName %s %s\n%s\n%s\n",ROMTITLE,ROMVERSION,ROMDATE,ROMENV);
  _consolePrintf("__current pc=0x%08x sp=0x%08x\n",__current_pc(),__current_sp());
  
  DISCIO_ShowAdapterInfo();
  
  plf->TopPos=plf->BufPos;
}

bool _consoleGetLogFile(void)
{
	TLogFile *plf=&LogFile;
  if(plf->WriteTopSector==0) return(false);
  return(true);
}

void _consoleSetLogOutFlag(bool f)
{
  LogOutFlag=f;
}

void _consoleLogPause(void)
{
	TLogFile *plf=&LogFile;
	plf->Pause=true;
}

extern void _consoleLogResume(void)
{
	TLogFile *plf=&LogFile;
	
	if(plf->Pause==false) return;
	plf->Pause=false;
	
  if(plf->WriteTopSector==0) return;
  
  u32 wsidx=plf->BufPos/512;
  u32 wscnt=plf->BufSize/512;
  
  extern LPIO_INTERFACE active_interface;
  
  if(wsidx==0){
    active_interface->fn_WriteSectors(plf->WriteTopSector+(wscnt-1),1,&plf->pBuf[(wscnt-1)*512]);
    active_interface->fn_WriteSectors(plf->WriteTopSector+wsidx,1,&plf->pBuf[wsidx*512]);
    }else{
    active_interface->fn_WriteSectors(plf->WriteTopSector+(wsidx-1),2,&plf->pBuf[(wsidx-1)*512]);
  }
}

