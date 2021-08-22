
#ifndef _CONFIG_H
#define _CONFIG_H

#include <nds.h>
#include "../_console.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

static void DEBUGF(const char* format, ...)
{
//  return;
  
  static char strbuf[ConsoleMaxLen+1];
  
  va_list args;
  
  va_start( args, format );
  vsnprintf( strbuf, ConsoleMaxLen, format, args );
  _consolePrint(strbuf);
}

static void LOGF(const char* format, ...)
{
//  return;
  
  static char strbuf[ConsoleMaxLen+1];
  
  va_list args;
  
  va_start( args, format );
  vsnprintf( strbuf, ConsoleMaxLen, format, args );
  _consolePrint(strbuf);
}

static void av_log(void *p,int level,const char* format, ...)
{
//  return;
  
  static char strbuf[ConsoleMaxLen+1];
  
  va_list args;
  
  va_start( args, format );
  vsnprintf( strbuf, ConsoleMaxLen, format, args );
  _consolePrint(strbuf);
}

#define ICONST_ATTR
#define IBSS_ATTR

#define AV_LOG_INFO (1)
#define AV_LOG_DEBUG (2)

//#define DEBUG_VLC
//#define RB_PROFILE

//#define A32_BITSTREAM_READER

//#define ALT_BITSTREAM_READER
//#define ALT_BITSTREAM_READER_LE
//#define ALT_BITSTREAM_WRITER

#define LIBMPEG2_BITSTREAM_READER
#define LIBMPEG2_BITSTREAM_READER_HACK

//#define ALIGNED_BITSTREAM_WRITER

#define UNALIGNED_STORES_ARE_BAD

//#define BITSTREAM_TRACE

#define ROCKBOX
#define ROCKBOX_LITTLE_ENDIAN
//#define ROCKBOX_BIG_ENDIAN
//#define WORDS_BIGENDIAN

#define CPU_ARM
#define CONFIG_ALIGN 1

//#define CPU_COLDFIRE

//#define __DECC

//#define ARCH_SPARC
#define ARCH_ARMV4L
//#define ARCH_MIPS
//#define ARCH_BFIN
//#define ARCH_X86
//#define ARCH_SH4

#define LOGF _consolePrintf

#endif
