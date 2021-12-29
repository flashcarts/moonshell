/*
 * BitReader.h
 *
 * Description: Bit reader internal interface
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

#pragma once
//#include <windows.h>
#include "TTACommon.h"
#include "TTAError.h"
#include "crc32.h"

static unsigned char bit_buffer[BIT_BUFFER_SIZE + 8];

typedef struct {
  HANDLE hInFile;

  unsigned char *bit_buffer_end;

//  unsigned long frame_crc32;
  unsigned long bit_count;
  unsigned long bit_cache;
  unsigned char *bitpos;

  unsigned long *st;
  unsigned long next_frame_pos;
} TBitReader;

static TBitReader BitReader;

static void BitReader_Create(HANDLE fd)
{
//  BitReader.frame_crc32=0xFFFFFFFFUL;
  BitReader.hInFile=fd;
  BitReader.bit_count=0;
  BitReader.bit_cache=0;
  BitReader.bit_buffer_end=bit_buffer + BIT_BUFFER_SIZE;
  BitReader.bitpos=BitReader.bit_buffer_end;
}

static void BitReader_Free(void)
{
}

static void BitReader_GetHeader (TTAHeader *ttahdr)
{
#define readu8(pos) (buf[pos])
#define readu16(pos) (readu8(pos)|((u16)readu8(pos+1) << 8))
#define readu32(pos) (readu8(pos)|((u32)readu8(pos+1) << 8)|((u32)readu8(pos+2) << 16)|((u32)readu8(pos+3) << 24))

  unsigned long result, checksum;
  
  u8 buf[TTAHeader_Size];
  
  if (!ReadFile(BitReader.hInFile, buf, TTAHeader_Size, &result, NULL) || result != TTAHeader_Size){
    throw TTAException (READ_ERROR);
  }
  
  ttahdr->TTAid=readu32(0);
  ttahdr->AudioFormat = readu16(4);
  ttahdr->NumChannels = readu16(6);
  ttahdr->BitsPerSample = readu16(8);
  ttahdr->SampleRate = readu32(10);
  ttahdr->DataLength = readu32(14);
  ttahdr->CRC32 = readu32(18);
  
  // check for supported formats
  if (ttahdr->TTAid != TTA1_SIGN) throw TTAException (FORMAT_ERROR);
  
  checksum = crc32((unsigned char *) buf, TTAHeader_Size - sizeof(long));
  if (checksum != ttahdr->CRC32) throw TTAException (FILE_ERROR);

#undef readu8
#undef readu16
#undef readu32
}

static bool BitReader_GetSeekTable (unsigned long *seek_table, long st_size)
{
  unsigned long result, checksum;
  bool st_state = false;

  if (!ReadFile(BitReader.hInFile, seek_table, st_size * sizeof(long), &result, NULL) || result != st_size * sizeof(long)){
    throw TTAException (READ_ERROR);
  }

  checksum = crc32((unsigned char *) seek_table, (st_size - 1) * sizeof(long));
  if (checksum == ENDSWAP_INT32(seek_table[st_size - 1])){
    st_state = true;
  }
  
  for (BitReader.st = seek_table; BitReader.st < (seek_table + st_size); BitReader.st++){
    *BitReader.st = ENDSWAP_INT32(*BitReader.st);
  }
  
//  BitReader.next_frame_pos = SetFilePointer (BitReader.hInFile, 0, NULL, FILE_CURRENT);
  BitReader.next_frame_pos = ftell(BitReader.hInFile);
  SetFilePointer (BitReader.hInFile, 0, NULL, FILE_CURRENT);
  BitReader.st = seek_table;

  return st_state;
}

static inline void BitReader_GetBinary(unsigned long *value, unsigned long bits)
{
  while (BitReader.bit_count < bits) {
    if (BitReader.bitpos == BitReader.bit_buffer_end) {
      unsigned long result;
      if (!ReadFile (BitReader.hInFile, bit_buffer, BIT_BUFFER_SIZE, &result, NULL)){
        throw TTAException (READ_ERROR);
      }
      BitReader.bitpos = bit_buffer;
    }

//    UPDATE_CRC32(*BitReader.bitpos, BitReader.frame_crc32);
    BitReader.bit_cache |= *BitReader.bitpos << BitReader.bit_count;
    BitReader.bit_count += 8;
    BitReader.bitpos++;
  }

  *value = BitReader.bit_cache & bit_mask[bits];
  BitReader.bit_cache >>= bits;
  BitReader.bit_count -= bits;
  BitReader.bit_cache &= bit_mask[BitReader.bit_count];
}

static inline void BitReader_GetUnary(unsigned long *value)
{
  *value = 0;

  while (!(BitReader.bit_cache ^ bit_mask[BitReader.bit_count])) {
    if (BitReader.bitpos == BitReader.bit_buffer_end) {
      unsigned long result;       
      if (!ReadFile (BitReader.hInFile, bit_buffer, BIT_BUFFER_SIZE, &result, NULL)){
        throw TTAException (READ_ERROR);
      }
      BitReader.bitpos = bit_buffer;
    }

    *value += BitReader.bit_count;
    BitReader.bit_cache = *BitReader.bitpos++;
//    UPDATE_CRC32(BitReader.bit_cache, BitReader.frame_crc32);
    BitReader.bit_count = 8;
  }

  while (BitReader.bit_cache & 1) {
    (*value)++;
    BitReader.bit_cache >>= 1;
    BitReader.bit_count--;
  }

  BitReader.bit_cache >>= 1;
  BitReader.bit_count--;
}

static int BitReader_Done(void)
{
  unsigned long crc32, rbytes, result;
//  BitReader.frame_crc32 ^= 0xFFFFFFFFUL;

    BitReader.next_frame_pos += *BitReader.st++;

  rbytes = BitReader.bit_buffer_end - BitReader.bitpos;
  if (rbytes < sizeof(long)) {
    CopyMemory(bit_buffer, BitReader.bitpos, 4);
    if (!ReadFile(BitReader.hInFile, bit_buffer + rbytes, BIT_BUFFER_SIZE - rbytes, &result, NULL)){
      throw TTAException (READ_ERROR);
    }
    BitReader.bitpos = bit_buffer;
  }

//  CopyMemory(&crc32, BitReader.bitpos, 4);
//  crc32 = ENDSWAP_INT32(crc32);
  BitReader.bitpos += sizeof(long);
//  result = (crc32 != BitReader.frame_crc32);
  result=0;

  BitReader.bit_cache = BitReader.bit_count = 0;
//  BitReader.frame_crc32 = 0xFFFFFFFFUL;

  return result;
}

static void BitReader_SkipFrame(void)
{
  SetFilePointer(BitReader.hInFile, BitReader.next_frame_pos, NULL, FILE_BEGIN);
  BitReader.bitpos = BitReader.bit_buffer_end;
}

