/*
 * ttacommon.h
 *
 * Description: TTA library common definitions and prototypes
 *
 * Copyright (c) 2004 Alexander Djourik. All rights reserved.
 * Copyright (c) 2004 Pavel Zhilin. All rights reserved.
 *
 */

/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * aint with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Please see the file COPYING in this directory for full copyright
 * information.
 */

#ifndef TTACOMMON_H_
#define TTACOMMON_H_

#include "../plugin.h"
#include "../plugin_def.h"

typedef bool BOOL;

typedef int HANDLE;
typedef void* LPCVOID;
typedef long LONG;
typedef LONG* PLONG;
typedef unsigned long DWORD;
typedef DWORD* LPDWORD;
typedef void* LPOVERLAPPED;
typedef void* PVOID;
typedef int SIZE_T;

#define VOID void
#define CONST const

#define FILE_CURRENT (0)
#define FILE_BEGIN (1)

static BOOL ReadFile(
    HANDLE   hFile,                // ファイルハンドル
    LPCVOID  pBuffer,              // バッファアドレス
    DWORD    nNumberOfBytesToRead, // サイズ
    LPDWORD  pNumberOfBytesRead,   // 実際のサイズを格納する変数
    LPOVERLAPPED  pOverlapped      // OVERLAPPED構造体
)
{
//  _consolePrintf("ReadFile(%d,$%x,%d,$%x,$%x);\n",hFile,pBuffer,nNumberOfBytesToRead,pNumberOfBytesRead,pOverlapped);
  *pNumberOfBytesRead=fread(pBuffer,1,nNumberOfBytesToRead,hFile);
  if(*pNumberOfBytesRead==0){
    return(false);
    }else{
    return(true);
  }
}

static DWORD SetFilePointer(
    HANDLE hFile,          // ファイルハンドル
    LONG   lDistance,      // 下位オフセット
    PLONG  pDistanceHigh,  // 上位オフセットの変数
    DWORD  dwMoveMethod    // 移動開始点
)
{
//  _consolePrintf("SetFilePointer(%d,%d,%d,%d);\n",hFile,lDistance,pDistanceHigh,dwMoveMethod);
  DWORD res=0;
  switch(dwMoveMethod){
    case FILE_CURRENT: res=fseek(hFile,lDistance,SEEK_CUR); break;
    case FILE_BEGIN: res=fseek(hFile,lDistance,SEEK_SET); break;
    default: res=0;
  }
  return(res);
}

static VOID CopyMemory(
  PVOID Destination,   // コピー先
  CONST VOID *Source,  // コピー元メモリブロック
  SIZE_T Length        // メモリブロックのサイズ
)
{
//  _consolePrintf("CopyMemory($%x,$%x,%d);\n",Destination,Source,Length);
  memcpy(Destination,Source,Length);
}

typedef s16 TSAMPLE;

#define MAX_BPS			32
#define FRAME_TIME		1.04489795918367346939

#define TTA1_SIGN		0x31415454

#define MAX_ORDER		16
#define BIT_BUFFER_SIZE (32*1024)

#ifdef _BIG_ENDIAN
	#define	ENDSWAP_INT16(x)	(((((x)>>8)&0xFF)|(((x)&0xFF)<<8)))
	#define	ENDSWAP_INT32(x)	(((((x)>>24)&0xFF)|(((x)>>8)&0xFF00)|(((x)&0xFF00)<<8)|(((x)&0xFF)<<24)))
#else
	#define	ENDSWAP_INT16(x)	(x)
	#define	ENDSWAP_INT32(x)	(x)
#endif

#define SWAP16(x) (\
	(((x)&(1<< 0))?(1<<15):0) | \
	(((x)&(1<< 1))?(1<<14):0) | \
	(((x)&(1<< 2))?(1<<13):0) | \
	(((x)&(1<< 3))?(1<<12):0) | \
	(((x)&(1<< 4))?(1<<11):0) | \
	(((x)&(1<< 5))?(1<<10):0) | \
	(((x)&(1<< 6))?(1<< 9):0) | \
	(((x)&(1<< 7))?(1<< 8):0) | \
	(((x)&(1<< 8))?(1<< 7):0) | \
	(((x)&(1<< 9))?(1<< 6):0) | \
	(((x)&(1<<10))?(1<< 5):0) | \
	(((x)&(1<<11))?(1<< 4):0) | \
	(((x)&(1<<12))?(1<< 3):0) | \
	(((x)&(1<<13))?(1<< 2):0) | \
	(((x)&(1<<14))?(1<< 1):0) | \
	(((x)&(1<<15))?(1<< 0):0))

// typedef unsigned long long uint64;
 typedef u64 uint64;

#define PREDICTOR1(x, k)	((long)((((uint64)x << k) - x) >> k))

#define ENC(x)  (((x)>0)?((x)<<1)-1:(-(x)<<1))
#define DEC(x)  (((x)&1)?(++(x)>>1):(-(x)>>1))

// basics structures definitions
struct adapt
{
	unsigned long k0;
	unsigned long k1;
	unsigned long sum0;
	unsigned long sum1;
};

struct fltst 
{
	long shift;
	long round;
	long error;
	long mutex;
	long qm[MAX_ORDER];
	long dx[MAX_ORDER];
	long dl[MAX_ORDER];
};

struct encoder 
{
	fltst fst;
	adapt rice;
	long last;
};

#define TTAHeader_Size (22)

struct TTAHeader {
	unsigned long TTAid;
	unsigned short AudioFormat;
	unsigned short NumChannels;
	unsigned short BitsPerSample;
	unsigned long SampleRate;
	unsigned long DataLength;
	unsigned long CRC32;
};

// ****************** static variables and structures *******************

#define bit_mask_count (40)
static const unsigned long bit_mask[bit_mask_count] = {
    0x00000000, 0x00000001, 0x00000003, 0x00000007,
    0x0000000f, 0x0000001f, 0x0000003f, 0x0000007f,
    0x000000ff, 0x000001ff, 0x000003ff, 0x000007ff,
    0x00000fff, 0x00001fff, 0x00003fff, 0x00007fff,
    0x0000ffff, 0x0001ffff, 0x0003ffff, 0x0007ffff,
    0x000fffff, 0x001fffff, 0x003fffff, 0x007fffff,
    0x00ffffff, 0x01ffffff, 0x03ffffff, 0x07ffffff,
    0x0fffffff, 0x1fffffff, 0x3fffffff, 0x7fffffff,
    0xffffffff
};

#define bit_shift_count (40)
static const unsigned long bit_shift[bit_shift_count] = {
    0x00000001, 0x00000002, 0x00000004, 0x00000008,
    0x00000010, 0x00000020, 0x00000040, 0x00000080,
    0x00000100, 0x00000200, 0x00000400, 0x00000800,
    0x00001000, 0x00002000, 0x00004000, 0x00008000,
    0x00010000, 0x00020000, 0x00040000, 0x00080000,
    0x00100000, 0x00200000, 0x00400000, 0x00800000,
    0x01000000, 0x02000000, 0x04000000, 0x08000000,
    0x10000000, 0x20000000, 0x40000000, 0x80000000,
    0x80000000, 0x80000000, 0x80000000, 0x80000000,
    0x80000000, 0x80000000, 0x80000000, 0x80000000
};
static  const unsigned long *shift_16 = bit_shift + 4;

#define WAVE_FORMAT_PCM	1
#define WAVE_FORMAT_IEEE_FLOAT 3

#endif // TTACOMMON_H_
