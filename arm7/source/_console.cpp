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

#ifdef EnableConsoleGBALog

static vu32 enagba=true;
#define polllimit (0x10000)

static __attribute__ ((always_inline)) void tx_waitsend(const char data)
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

static __attribute__ ((noinline)) void pcprint(const char *p)
{
  u32 len=strlen(p);
	
	tx_waitsend(0xa0);
	tx_waitsend(len+1);
	
	u32 idx=0;
	while(p[idx]!=0){
    tx_waitsend(p[idx]);
    idx++;
	}
  
  tx_waitsend(0);
}

void _consolePrint(const char* s)
{
  if(enagba==false) return;
  
  pcprint(s);
}

void _consolePrintf(const char* format, ...)
{
  if(enagba==false) return;
  
  char strbuf[256];
  
  va_list args;
  
  va_start( args, format );
  vsnprintf( strbuf, 255, format, args );
  _consolePrint(strbuf);
}

void _console_ReenabledGBABUS(void)
{
  enagba=true;
}

#endif // #ifdef EnableConsoleGBALog
