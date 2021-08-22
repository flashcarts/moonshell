/***************************************************************************
 *             __________               __   ___.
 *   Open      \______   \ ____   ____ |  | _\_ |__   _______  ___
 *   Source     |       _//  _ \_/ ___\|  |/ /| __ \ /  _ \  \/  /
 *   Jukebox    |    |   (  <_> )  \___|    < | \_\ (  <_> > <  <
 *   Firmware   |____|_  /\____/ \___  >__|_ \|___  /\____/__/\_ \
 *                     \/            \/     \/    \/            \/
 * $Id: codecs.h 19377 2008-12-10 08:57:10Z jethead71 $
 *
 * Copyright (C) 2002 Björn Stenberg
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
#ifndef _CODECS_H_
#define _CODECS_H_

#define CODEC_HEADER

#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>

typedef u32 off_t;

#define MAX_PATH (256)

enum codec_status {
    CODEC_OK = 0,
    CODEC_USB_CONNECTED,
    CODEC_ERROR = -1,
};

/* ASF codec IDs */
#define ASF_CODEC_ID_WMAV1 0x160
#define ASF_CODEC_ID_WMAV2 0x161

struct asf_waveformatex_s {
  uint32_t packet_size;
  int audiostream;
  uint16_t codec_id;
  uint16_t channels;
  uint32_t rate;
  uint32_t bitrate;
  uint16_t blockalign;
  uint16_t bitspersample;
  uint16_t datalen;
  uint8_t data[6];
};
typedef struct asf_waveformatex_s asf_waveformatex_t;

#define ID3V2_BUF_SIZE 300

struct mp3entry {
    char path[MAX_PATH];
    char* title;
    char* artist;
    char* album;
    char* genre_string;
    char* disc_string;
    char* track_string;
    char* year_string;
    char* composer;
    char* comment;
    char* albumartist;
    char* grouping;
    int discnum;    
    int tracknum;
    int version;
    int layer;
    int year;
    unsigned char id3version;
    unsigned int codectype;
    unsigned int bitrate;
    unsigned long frequency;
    unsigned long id3v2len;
    unsigned long id3v1len;
    unsigned long first_frame_offset; /* Byte offset to first real MP3 frame.
                                         Used for skipping leading garbage to
                                         avoid gaps between tracks. */
    unsigned long vbr_header_pos;
    unsigned long filesize; /* without headers; in bytes */
    unsigned long length;   /* song length in ms */
    unsigned long elapsed;  /* ms played */

    int lead_trim;          /* Number of samples to skip at the beginning */
    int tail_trim;          /* Number of samples to remove from the end */

    /* Added for Vorbis */
    unsigned long samples;  /* number of samples in track */

    /* MP3 stream specific info */
    unsigned long frame_count; /* number of frames in the file (if VBR) */

    /* Used for A52/AC3 */
    unsigned long bytesperframe; /* number of bytes per frame (if CBR) */

    /* Xing VBR fields */
    bool vbr;
    bool has_toc;           /* True if there is a VBR header in the file */
    asf_waveformatex_t wfx; /* table of contents */

    /* these following two fields are used for local buffering */
    char id3v2buf[ID3V2_BUF_SIZE];
    char id3v1buf[4][92];

    /* resume related */
    unsigned long offset;  /* bytes played */
    int index;             /* playlist index */

    /* runtime database fields */
    long tagcache_idx;     /* 0=invalid, otherwise idx+1 */
    int rating;
    int score;
    long playcount;
    long lastplayed;
    long playtime;
    
    /* replaygain support */
    
#if CONFIG_CODEC == SWCODEC
    char* track_gain_string;
    char* album_gain_string;
    long track_gain;    /* 7.24 signed fixed point. 0 for no gain. */
    long album_gain;
    long track_peak;    /* 7.24 signed fixed point. 0 for no peak. */
    long album_peak;
#endif

    /* Cuesheet support */
    int cuesheet_type;      /* 0: none, 1: external, 2: embedded */

    /* Musicbrainz Track ID */
    char* mb_track_id;
};

struct codec_sapi {
  const uint8_t *pfilebuf;
    off_t  filesize;          /* Total file length */
    off_t  curpos;            /* Current buffer position */
    
    /* If seek_time != 0, codec should seek to that song position (in ms)
       if codec supports seeking. */
    long seek_time;
    
    /* For gapless mp3 */
    struct mp3entry *id3;     /* TAG metadata pointer */
    
    /* Codec should periodically check if stop_codec is set to true.
       In case it is, codec must return immediately */
    bool stop_codec;
    /* Codec should periodically check if new_track is non zero.
       When it is, the codec should request a new track. */
    int new_track;
    
};

extern struct codec_sapi *csi;

/* Read next <size> amount bytes from file buffer to <ptr>.
   Will return number of bytes read or 0 if end of file. */
extern size_t CSI_read_filebuf(void *ptr, size_t size);

extern size_t CSI_GetFileSize(void);

/* Seek file buffer to position <newpos> beginning of file. */
extern bool CSI_seek_buffer_set(size_t newpos);
extern bool CSI_seek_buffer_cur(size_t pos);
/* Codec should call this function when it has done the seeking. */
extern void CSI_seek_complete(void);

/* Set song position in WPS (value in ms). */
extern void CSI_set_elapsed(unsigned int value);

/* Request pointer to file buffer which can be used to read
   <realsize> amount of data. <reqsize> tells the buffer system
   how much data it should try to allocate. If <realsize> is 0,
   end of file is reached. */
void* CSI_request_buffer(size_t *realsize, size_t reqsize);

/* Advance file buffer position by <amount> amount of bytes. */
extern void CSI_advance_buffer(size_t amount);

/* Request file change from file buffer. Returns true is next
   track is available and changed. If return value is false,
   codec should exit immediately with PLUGIN_OK status. */
extern bool CSI_request_next_track(void);

/* Insert PCM data into audio buffer for playback. Playback will start
   automatically. */
extern bool CSI_pcmbuf_insert(const void *ch1, const void *ch2, int count);

#endif
