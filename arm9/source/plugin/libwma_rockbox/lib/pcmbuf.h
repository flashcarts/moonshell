/***************************************************************************
 *             __________               __   ___.
 *   Open      \______   \ ____   ____ |  | _\_ |__   _______  ___
 *   Source     |       _//  _ \_/ ___\|  |/ /| __ \ /  _ \  \/  /
 *   Jukebox    |    |   (  <_> )  \___|    < | \_\ (  <_> > <  <
 *   Firmware   |____|_  /\____/ \___  >__|_ \|___  /\____/__/\_ \
 *                     \/            \/     \/    \/            \/
 * $Id: pcmbuf.h 17847 2008-06-28 18:10:04Z bagder $
 *
 * Copyright (C) 2005 by Miika Pekkarinen
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied.
 *
 ****************************************************************************/
#ifndef PCMBUF_H
#define PCMBUF_H

#if MEM > 1
#define PCMBUF_TARGET_CHUNK 32768 /* This is the target fill size of chunks
                                     on the pcm buffer */
#define PCMBUF_MINAVG_CHUNK 24576 /* This is the minimum average size of
                                     chunks on the pcm buffer (or we run out
                                     of buffer descriptors, which is
                                     non-fatal) */
#else
#define PCMBUF_TARGET_CHUNK 16384
#define PCMBUF_MINAVG_CHUNK 12288
#endif

#define PCMBUF_MIN_CHUNK     4096 /* We try to never feed a chunk smaller than
                                     this to the DMA */
#define PCMBUF_MIX_CHUNK     8192 /* This is the maximum size of one packet
                                     for mixing (crossfade or voice) */

/* Returns true if the buffer needs to change size */
bool pcmbuf_is_same_size(void);
size_t pcmbuf_init(unsigned char *bufend);
/* Size in bytes used by the pcmbuffer */
size_t pcmbuf_get_bufsize(void);
#ifdef ROCKBOX_HAS_LOGF
/* just used for logging for now */
unsigned char * pcmbuf_get_meminfo(size_t *length);
#endif

void pcmbuf_pause(bool pause);
void pcmbuf_play_stop(void);
bool pcmbuf_is_crossfade_active(void);

/* These functions are for playing chained buffers of PCM data */
#if defined(HAVE_ADJUSTABLE_CPU_FREQ)
void pcmbuf_boost(bool state);
void pcmbuf_set_boost_mode(bool state);
#else
#define pcmbuf_boost(state) do { } while(0)
#define pcmbuf_set_boost_mode(state) do { } while(0)
#endif
bool pcmbuf_is_lowdata(void);
void pcmbuf_play_start(void);
bool pcmbuf_crossfade_init(bool manual_skip);
void pcmbuf_set_event_handler(void (*callback)(void));
void pcmbuf_set_position_callback(void (*callback)(size_t size));
size_t pcmbuf_free(void);
unsigned int pcmbuf_get_latency(void);
void pcmbuf_set_low_latency(bool state);
void * pcmbuf_request_buffer(int *count);
void pcmbuf_write_complete(int count);
void * pcmbuf_request_voice_buffer(int *count);
void pcmbuf_write_voice_complete(int count);
bool pcmbuf_is_crossfade_enabled(void);
void pcmbuf_crossfade_enable(bool on_off);
void pcmbuf_crossfade_enable_finished(void);
int pcmbuf_usage(void);
int pcmbuf_mix_free(void);
void pcmbuf_beep(unsigned int frequency, size_t duration, int amplitude);

int pcmbuf_used_descs(void);
int pcmbuf_descs(void);

#endif
