/*
 * TTAReader.h
 *
 * Description: TTA decompressor internals
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
#include "BitReader.h"
#include "filters3.h"

#define WAVE_FORMAT_PCM	1
#define WAVE_FORMAT_IEEE_FLOAT 3

typedef struct {
  encoder ttaenc[2];
  TSAMPLE *data;
  
  u32 framecurindex;
  
  u32 frameindex,framecount;
  
  u32 firstframepos;
  
  unsigned long offset, is_float, framelen, lastlen;
  unsigned long byte_size, num_chan;
  unsigned long *seek_table;
  
  TTAHeader ttahdr;
} TTTAReader;

static TTTAReader TTAReader;

// ************************* basic functions *****************************

static inline void rice_init(adapt *rice, unsigned long k0, unsigned long k1)
{
	rice->k0 = k0;
	rice->k1 = k1;
	rice->sum0 = shift_16[k0];
	rice->sum1 = shift_16[k1];
}

static inline void encoder_init(encoder *tta, long nch, long byte_size) 
{
	long *fset = flt_set[byte_size - 1];
	long i;

	for (i = 0; i < nch; i++) {
		filter_init(&tta[i].fst, fset[0], fset[1]);
		rice_init(&tta[i].rice, 10, 10);
		tta[i].last = 0;
	}
}

// -----------------

static void TTAReader_Create(HANDLE fd)
{
  // clear statistics
  BitReader_Create(fd);
  BitReader_GetHeader (&TTAReader.ttahdr);

  TTAHeader *pttahdr=&TTAReader.ttahdr;
  
  TTAReader.byte_size = (pttahdr->BitsPerSample + 7) / 8;
  TTAReader.framelen = (long) (FRAME_TIME * pttahdr->SampleRate);
  TTAReader.is_float = (pttahdr->AudioFormat == WAVE_FORMAT_IEEE_FLOAT);
  TTAReader.num_chan = pttahdr->NumChannels << TTAReader.is_float;
  
  TTAReader.lastlen = pttahdr->DataLength % TTAReader.framelen;
  TTAReader.framecount=pttahdr->DataLength / TTAReader.framelen + (TTAReader.lastlen ? 1 : 0);
  TTAReader.frameindex=0;
  
  u32 st_size = (TTAReader.framecount + 1);
  
  TTAReader.data = (TSAMPLE*)malloc(TTAReader.framelen*TTAReader.num_chan*sizeof(TSAMPLE));
  
  TTAReader.seek_table = (unsigned long*)malloc(st_size*sizeof(unsigned long));
  if(BitReader_GetSeekTable(TTAReader.seek_table, st_size)==false){
    free(TTAReader.seek_table); TTAReader.seek_table=NULL;
  }
  encoder_init(TTAReader.ttaenc, TTAReader.num_chan, TTAReader.byte_size);
  
  TTAReader.firstframepos=BitReader.next_frame_pos;
  
  TTAReader.framecurindex=0;
}

static void TTAReader_Free(void)
{
  if(TTAReader.seek_table!=NULL){
    free(TTAReader.seek_table); TTAReader.seek_table=NULL;
  }
  if(TTAReader.data!=NULL){
    free(TTAReader.data); TTAReader.data=NULL;
  }
  BitReader_Free();
}

static inline long TTAReader_GetBlock_Start(void)
{
  if (TTAReader.frameindex==TTAReader.framecount) return(false);
  TTAReader.frameindex++;
  
  if ((TTAReader.frameindex==TTAReader.framecount)&&(TTAReader.lastlen!=0)) TTAReader.framelen = TTAReader.lastlen;
  
  {
    u32 filepos=TTAReader.firstframepos;
    
    for(u32 idx=0;idx<TTAReader.frameindex;idx++){
      filepos+=TTAReader.seek_table[idx];
    }
    
    fseek(BitReader.hInFile,filepos,SEEK_SET);
    
    BitReader.bit_count=0;
    BitReader.bit_cache=0;
    BitReader.bit_buffer_end=bit_buffer + BIT_BUFFER_SIZE;
    BitReader.bitpos=BitReader.bit_buffer_end;
    BitReader.next_frame_pos=filepos;
  }
  
  encoder_init(TTAReader.ttaenc, TTAReader.num_chan, TTAReader.byte_size);
  
  return(true);
}

static inline void TTAReader_GetBlock1ch8bit(TSAMPLE *bufl,u32 bufcount)
{
  bufcount*=1; // TTAReader.num_chan==1;
  
  for (u32 cnt=0;cnt<bufcount;cnt++){
    encoder *enc=&TTAReader.ttaenc[0];
    
    adapt *rice = &enc->rice;
    
    unsigned long  unary, binary, depth, k;
    
    // decode Rice unsigned
    BitReader_GetUnary(&unary);
    
    if(unary==0){
      depth = 0; k = rice->k0;
      }else{
      depth = 1; k = rice->k1;
      unary--;
    }
    
    TSAMPLE value;
    
    if(k){
      BitReader_GetBinary(&binary, k);
      value = (unary << k) + binary;
      }else{
      value = unary;
    }
    
    if(depth==1){
      rice->sum1 += value - (rice->sum1 >> 4);
      if (rice->k1 > 0 && rice->sum1 < shift_16[rice->k1])
        rice->k1--;
      else if (rice->sum1 > shift_16[rice->k1 + 1])
        rice->k1++;
      value += bit_shift[rice->k0];
    }
    
    {
      rice->sum0 += value - (rice->sum0 >> 4);
      if (rice->k0 > 0 && rice->sum0 < shift_16[rice->k0])
        rice->k0--;
      else if (rice->sum0 > shift_16[rice->k0 + 1])
        rice->k0++;
    }
    
    TSAMPLE sample = DEC(value);
    
    // decompress stage 1: adaptive hybrid filter
    hybrid_filter(&enc->fst, &sample, 0);
    
    // decompress stage 2: fixed order 1 prediction
    long *last = &enc->last;
    sample += PREDICTOR1(*last, 4); // bps 8
    *last = sample;
    
    bufl[cnt]=sample<<8;
  }
}

static inline void TTAReader_GetBlock1ch16bit(TSAMPLE *bufl,u32 bufcount)
{
  bufcount*=1; // TTAReader.num_chan==1;
  
  for (u32 cnt=0;cnt<bufcount;cnt++){
    encoder *enc=&TTAReader.ttaenc[0];
    
    adapt *rice = &enc->rice;
    
    unsigned long  unary, binary, depth, k;
    
    // decode Rice unsigned
    BitReader_GetUnary(&unary);
    
    if(unary==0){
      depth = 0; k = rice->k0;
      }else{
      depth = 1; k = rice->k1;
      unary--;
    }
    
    TSAMPLE value;
    
    if(k){
      BitReader_GetBinary(&binary, k);
      value = (unary << k) + binary;
      }else{
      value = unary;
    }
    
    if(depth==1){
      rice->sum1 += value - (rice->sum1 >> 4);
      if (rice->k1 > 0 && rice->sum1 < shift_16[rice->k1])
        rice->k1--;
      else if (rice->sum1 > shift_16[rice->k1 + 1])
        rice->k1++;
      value += bit_shift[rice->k0];
    }
    
    {
      rice->sum0 += value - (rice->sum0 >> 4);
      if (rice->k0 > 0 && rice->sum0 < shift_16[rice->k0])
        rice->k0--;
      else if (rice->sum0 > shift_16[rice->k0 + 1])
        rice->k0++;
    }
    
    TSAMPLE sample = DEC(value);
    
    // decompress stage 1: adaptive hybrid filter
    hybrid_filter(&enc->fst, &sample, 0);
    
    // decompress stage 2: fixed order 1 prediction
    long *last = &enc->last;
    sample += PREDICTOR1(*last, 5); // bps 16
    *last = sample;
    
    bufl[cnt]=sample;
  }
}

static inline void TTAReader_GetBlock2ch8bit(TSAMPLE *bufl,TSAMPLE *bufr,u32 bufcount)
{
  TSAMPLE *buflr[2]={bufl,bufr};
  
  bufcount*=2; // TTAReader.num_chan==2;
  
  for (u32 cnt=0;cnt<bufcount;cnt++){
    encoder *enc=&TTAReader.ttaenc[cnt&1];
    
    adapt *rice = &enc->rice;
    
    unsigned long  unary, binary, depth, k;
    
    // decode Rice unsigned
    BitReader_GetUnary(&unary);
    
    if(unary==0){
      depth = 0; k = rice->k0;
      }else{
      depth = 1; k = rice->k1;
      unary--;
    }
    
    TSAMPLE value;
    
    if(k){
      BitReader_GetBinary(&binary, k);
      value = (unary << k) + binary;
      }else{
      value = unary;
    }
    
    if(depth==1){
      rice->sum1 += value - (rice->sum1 >> 4);
      if (rice->k1 > 0 && rice->sum1 < shift_16[rice->k1])
        rice->k1--;
      else if (rice->sum1 > shift_16[rice->k1 + 1])
        rice->k1++;
      value += bit_shift[rice->k0];
    }
    
    {
      rice->sum0 += value - (rice->sum0 >> 4);
      if (rice->k0 > 0 && rice->sum0 < shift_16[rice->k0])
        rice->k0--;
      else if (rice->sum0 > shift_16[rice->k0 + 1])
        rice->k0++;
    }
    
    TSAMPLE sample = DEC(value);
    
    // decompress stage 1: adaptive hybrid filter
    hybrid_filter(&enc->fst, &sample, 0);
    
    // decompress stage 2: fixed order 1 prediction
    long *last = &enc->last;
    sample += PREDICTOR1(*last, 4); // bps 8
    *last = sample;
    
    buflr[cnt&1][cnt/2]=sample<<8;
  }
}

static inline void TTAReader_GetBlock2ch16bit(TSAMPLE *bufl,TSAMPLE *bufr,u32 bufcount)
{
  TSAMPLE *buflr[2]={bufl,bufr};
  
  bufcount*=2; // TTAReader.num_chan==2;
  
  for (u32 cnt=0;cnt<bufcount;cnt++){
    encoder *enc=&TTAReader.ttaenc[cnt&1];
    
    adapt *rice = &enc->rice;
    
    unsigned long  unary, binary, depth, k;
    
    // decode Rice unsigned
    BitReader_GetUnary(&unary);
    
    if(unary==0){
      depth = 0; k = rice->k0;
      }else{
      depth = 1; k = rice->k1;
      unary--;
    }
    
    TSAMPLE value;
    
    if(k){
      BitReader_GetBinary(&binary, k);
      value = (unary << k) + binary;
      }else{
      value = unary;
    }
    
    if(depth==1){
      rice->sum1 += value - (rice->sum1 >> 4);
      if (rice->k1 > 0 && rice->sum1 < shift_16[rice->k1])
        rice->k1--;
      else if (rice->sum1 > shift_16[rice->k1 + 1])
        rice->k1++;
      value += bit_shift[rice->k0];
    }
    
    {
      rice->sum0 += value - (rice->sum0 >> 4);
      if (rice->k0 > 0 && rice->sum0 < shift_16[rice->k0])
        rice->k0--;
      else if (rice->sum0 > shift_16[rice->k0 + 1])
        rice->k0++;
    }
    
    TSAMPLE sample = DEC(value);
    
    // decompress stage 1: adaptive hybrid filter
    hybrid_filter(&enc->fst, &sample, 0);
    
    // decompress stage 2: fixed order 1 prediction
    long *last = &enc->last;
    sample += PREDICTOR1(*last, 5); // bps 16
    *last = sample;
    
    buflr[cnt&1][cnt/2]=sample;
  }
}

static u32 TTAReader_GetBlock(TSAMPLE *bufl,TSAMPLE *bufr,u32 bufcount)
{
  if(TTAReader.framecurindex==0){
    if(TTAReader_GetBlock_Start()==false) return(0);
  }
  
  if((TTAReader.framelen-TTAReader.framecurindex)<bufcount){
    bufcount=TTAReader.framelen-TTAReader.framecurindex;
  }
  
  if(TTAReader.byte_size==2){
    if(TTAReader.num_chan==1){
      TTAReader_GetBlock1ch16bit(bufl,bufcount);
      MemCopy16CPU(bufl,bufr,bufcount*2);
      }else{
      TTAReader_GetBlock2ch16bit(bufl,bufr,bufcount);
    }
    }else{
    if(TTAReader.num_chan==1){
      TTAReader_GetBlock1ch8bit(bufl,bufcount);
      MemCopy16CPU(bufl,bufr,bufcount*2);
      }else{
      TTAReader_GetBlock2ch8bit(bufl,bufr,bufcount);
    }
  }
  
  TTAReader.framecurindex+=bufcount;
  
  if(TTAReader.framecurindex==TTAReader.framelen){
    _consolePrint("FrameEnd.\n");
    TTAReader.framecurindex=0;
    if(BitReader_Done()){ // CRC error
      return(0);
      }else{
      return(bufcount);
    }
  }
  
  return(bufcount);
}

