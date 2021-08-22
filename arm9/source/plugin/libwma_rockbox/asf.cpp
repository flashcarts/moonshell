
/***************************************************************************
 *             __________               __   ___.
 *   Open      \______   \ ____   ____ |  | _\_ |__   _______  ___
 *   Source     |       _//  _ \_/ ___\|  |/ /| __ \ /  _ \  \/  /
 *   Jukebox    |    |   (  <_> )  \___|    < | \_\ (  <_> > <  <
 *   Firmware   |____|_  /\____/ \___  >__|_ \|___  /\____/__/\_ \
 *                     \/            \/     \/    \/            \/
 *
 * $Id: asf.c 18814 2008-10-15 06:38:51Z zagor $
 *
 * Copyright (C) 2007 Dave Chapman
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
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <inttypes.h>

#include "config.h"

//#include "metadata.h"
//#include "replaygain.h"
//#include "debug.h"
//#include "rbunicode.h"
//#include "metadata_common.h"
//#include "metadata_parsers.h"
//#include "system.h"

#include "asf.h"
#include "codecs.h"

#include <nds.h>
#include "../_console.h"

// ----------------------------------

#include "bswap.h"

static int read_uint32be(int fd, unsigned int* buf)
{
  size_t n;

  n = CSI_read_filebuf(buf,4);
  *buf = bswap_32(*buf);
  return n;
}

#define read_uint16le(fd,buf) CSI_read_filebuf((buf), 2)
#define read_uint32le(fd,buf) CSI_read_filebuf((buf), 4)
#define read_uint64le(fd,buf) CSI_read_filebuf((buf), 8)

#define read_buf(fd,buf,size) CSI_read_filebuf((buf), (size))

#define GetFileSize(fd) CSI_GetFileSize()

#define lseek_cur(fd,pos) CSI_seek_buffer_cur((pos))

#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) < (b) ? (a) : (b))

static const unsigned char utf8comp[6] =
{
    0x00, 0xC0, 0xE0, 0xF0, 0xF8, 0xFC
};

#define UNI_MASK   0xC0 /* 11000000 */
#define UNI_COMP   0x80 /* 10x      */

/* Encode a UCS value as UTF-8 and return a pointer after this UTF-8 char. */
char* utf8encode(unsigned long ucs, char *utf8)
{
    int tail = 0;

    if (ucs > 0x7F)
        while (ucs >> (5*tail + 6))
            tail++;

    *utf8++ = (ucs >> (6*tail)) | utf8comp[tail];
    while (tail--)
        *utf8++ = ((ucs >> (6*tail)) & (UNI_MASK ^ 0xFF)) | UNI_COMP;

    return utf8;
}

// --------------------------------

/* TODO: Just read the GUIDs into a 16-byte array, and use memcmp to compare */
struct guid_s {
    uint32_t v1;
    uint16_t v2;
    uint16_t v3;
    uint8_t  v4[8];
};
typedef struct guid_s guid_t;

struct asf_object_s {
    guid_t       guid;
    uint64_t     size;
    uint64_t     datalen;
};
typedef struct asf_object_s asf_object_t;

enum asf_error_e {
    ASF_ERROR_INTERNAL       = -1,  /* incorrect input to API calls */
    ASF_ERROR_OUTOFMEM       = -2,  /* some malloc inside program failed */
    ASF_ERROR_EOF            = -3,  /* unexpected end of file */
    ASF_ERROR_IO             = -4,  /* error reading or writing to file */
    ASF_ERROR_INVALID_LENGTH = -5,  /* length value conflict in input data */
    ASF_ERROR_INVALID_VALUE  = -6,  /* other value conflict in input data */
    ASF_ERROR_INVALID_OBJECT = -7,  /* ASF object missing or in wrong place */
    ASF_ERROR_OBJECT_SIZE    = -8,  /* invalid ASF object size (too small) */
    ASF_ERROR_SEEKABLE       = -9,  /* file not seekable */
    ASF_ERROR_SEEK           = -10, /* file is seekable but seeking failed */
    ASF_ERROR_ENCRYPTED      = -11  /* file is encrypted */
};

static const guid_t asf_guid_null =
{0x00000000, 0x0000, 0x0000, {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00}};

/* top level object guids */

static const guid_t asf_guid_header =
{0x75B22630, 0x668E, 0x11CF, {0xA6, 0xD9, 0x00, 0xAA, 0x00, 0x62, 0xCE, 0x6C}};

static const guid_t asf_guid_data =
{0x75B22636, 0x668E, 0x11CF, {0xA6, 0xD9, 0x00, 0xAA, 0x00, 0x62, 0xCE, 0x6C}};

static const guid_t asf_guid_index =
{0x33000890, 0xE5B1, 0x11CF, {0x89, 0xF4, 0x00, 0xA0, 0xC9, 0x03, 0x49, 0xCB}};

/* header level object guids */

static const guid_t asf_guid_file_properties =
{0x8cabdca1, 0xa947, 0x11cf, {0x8E, 0xe4, 0x00, 0xC0, 0x0C, 0x20, 0x53, 0x65}};

static const guid_t asf_guid_stream_properties =
{0xB7DC0791, 0xA9B7, 0x11CF, {0x8E, 0xE6, 0x00, 0xC0, 0x0C, 0x20, 0x53, 0x65}};

static const guid_t asf_guid_content_description =
{0x75B22633, 0x668E, 0x11CF, {0xA6, 0xD9, 0x00, 0xAA, 0x00, 0x62, 0xCE, 0x6C}};

static const guid_t asf_guid_extended_content_description =
{0xD2D0A440, 0xE307, 0x11D2, {0x97, 0xF0, 0x00, 0xA0, 0xC9, 0x5E, 0xA8, 0x50}};

static const guid_t asf_guid_content_encryption =
{0x2211b3fb, 0xbd23, 0x11d2, {0xb4, 0xb7, 0x00, 0xa0, 0xc9, 0x55, 0xfc, 0x6e}};

static const guid_t asf_guid_extended_content_encryption =
{0x298ae614, 0x2622, 0x4c17, {0xb9, 0x35, 0xda, 0xe0, 0x7e, 0xe9, 0x28, 0x9c}};

/* stream type guids */

static const guid_t asf_guid_stream_type_audio =
{0xF8699E40, 0x5B4D, 0x11CF, {0xA8, 0xFD, 0x00, 0x80, 0x5F, 0x5C, 0x44, 0x2B}};

static int asf_guid_match(const guid_t *guid1, const guid_t *guid2)
{
    if((guid1->v1 != guid2->v1) ||
       (guid1->v2 != guid2->v2) ||
       (guid1->v3 != guid2->v3) ||
       (memcmp(guid1->v4, guid2->v4, 8))) {
        return 0;
    }

    return 1;
}

/* Read the 16 byte GUID from a file */
static void asf_readGUID(int fd, guid_t* guid)
{
    read_uint32le(fd, &guid->v1);
    read_uint16le(fd, &guid->v2);
    read_uint16le(fd, &guid->v3);
    read_buf(fd, guid->v4, 8);
}

static void asf_read_object_header(asf_object_t *obj, int fd)
{
    asf_readGUID(fd, &obj->guid);
    read_uint64le(fd, &obj->size);
    obj->datalen = 0;
}

/* Parse an integer from the extended content object - we always
   convert to an int, regardless of native format.
*/
static int asf_intdecode(int fd, int type, int length)
{
    uint16_t tmp16;
    uint32_t tmp32;
    uint64_t tmp64;

    if (type==3) {
        read_uint32le(fd, &tmp32);
        lseek_cur(fd,length - 4);
        return (int)tmp32;
    } else if (type==4) {
        read_uint64le(fd, &tmp64);
        lseek_cur(fd,length - 8);
        return (int)tmp64;
    } else if (type == 5) {
        read_uint16le(fd, &tmp16);
        lseek_cur(fd,length - 2);
        return (int)tmp16;
    }

    return 0;
}

/* Decode a LE utf16 string from a disk buffer into a fixed-sized
   utf8 buffer.
*/

static void asf_utf16LEdecode(int fd,
                              uint16_t utf16bytes,
                              char **utf8,
                              int* utf8bytes
                             )
{
    unsigned long ucs;
    int n;
    char utf16buf[256];
    char* utf16 = utf16buf;
    char* newutf8;
    
    n = read_buf(fd, utf16buf, MIN(sizeof(utf16buf),utf16bytes));
    utf16bytes -= n;

    while (n > 0) {
        /* Check for a surrogate pair */
        if (utf16[1] >= 0xD8 && utf16[1] < 0xE0) {
            if (n < 4) {
                /* Run out of utf16 bytes, read some more */
                utf16buf[0] = utf16[0];
                utf16buf[1] = utf16[1];

                n = read_buf(fd, utf16buf + 2, MIN(sizeof(utf16buf)-2, utf16bytes));
                utf16 = utf16buf;
                utf16bytes -= n;
                n += 2;
            }

            if (n < 4) {
                /* Truncated utf16 string, abort */
                break;
            }
            ucs = 0x10000 + ((utf16[0] << 10) | ((utf16[1] - 0xD8) << 18)
                             | utf16[2] | ((utf16[3] - 0xDC) << 8));
            utf16 += 4;
            n -= 4;
        } else {
            ucs = (utf16[0] | (utf16[1] << 8));
            utf16 += 2;
            n -= 2;
        }

        if (*utf8bytes > 6) {
            newutf8 = utf8encode(ucs, *utf8);
            *utf8bytes -= (newutf8 - *utf8);
            *utf8 += (newutf8 - *utf8);
        }

        /* We have run out of utf16 bytes, read more if available */
        if ((n == 0) && (utf16bytes > 0)) {
            n = read_buf(fd, utf16buf, MIN(sizeof(utf16buf), utf16bytes));
            utf16 = utf16buf;
            utf16bytes -= n;
        }
    }

    *utf8[0] = 0;
    --*utf8bytes;

    if (utf16bytes > 0) {
        /* Skip any remaining bytes */
        lseek_cur(fd, utf16bytes);
    }
    return;
}

static int asf_parse_header(int fd, struct mp3entry* id3,
                                    asf_waveformatex_t* wfx)
{
    asf_object_t current;
    asf_object_t header;
    uint64_t datalen;
    int i;
    int fileprop = 0;
    uint64_t play_duration;
    uint16_t flags;
    uint32_t subobjects;
    uint8_t utf8buf[512];
    int id3buf_remaining = sizeof(id3->id3v2buf) + sizeof(id3->id3v1buf);
    char* id3buf = (char*)id3->id3v2buf;

    asf_read_object_header(&header, fd);

    DEBUGF("header.size=%d\n",(int)header.size);
    
    if (header.size < 30) {
        /* invalid size for header object */
        return ASF_ERROR_OBJECT_SIZE;
    }

    read_uint32le(fd, &subobjects);
    
    /* Two reserved bytes - do we need to read them? */
    lseek_cur(fd, 2);

    DEBUGF("Read header - size=%d, subobjects=%d\n",(int)header.size, (int)subobjects);

    if (subobjects > 0) {
        header.datalen = header.size - 30;

        /* TODO: Check that we have datalen bytes left in the file */
        datalen = header.datalen;

        for (i=0; i<(int)subobjects; i++) {
            DEBUGF("--- Parsing header object %d - datalen=%d\n",i,(int)datalen);
            if (datalen < 24) {
                DEBUGF("not enough data for reading object\n");
                break;
            }

            asf_read_object_header(&current, fd);
            DEBUGF("GUIDv1=0x%x, GUIDv2=0x%x, GUIDv3=0x%x, GUIDv4=....\n",current.guid.v1,current.guid.v2,current.guid.v3);

            if (current.size > datalen || current.size < 24) {
                DEBUGF("invalid object size - current.size=%d, datalen=%d\n",(int)current.size,(int)datalen);
                break;
            }

            if (asf_guid_match(&current.guid, &asf_guid_file_properties)) {
                    if (current.size < 104)
                        return ASF_ERROR_OBJECT_SIZE;

                    if (fileprop) {
                        /* multiple file properties objects not allowed */
                        return ASF_ERROR_INVALID_OBJECT;
                    }

                    fileprop = 1;
                    /* All we want is the play duration - uint64_t at offset 40 */
                    lseek_cur(fd, 40);

                    read_uint64le(fd, &play_duration);
                    id3->length = play_duration / 10000;

                    DEBUGF("****** length = %lums\n", id3->length);

                    /* Read the packet size - uint32_t at offset 68 */
                    lseek_cur(fd, 20);
                    read_uint32le(fd, &wfx->packet_size);

                    /* Skip bytes remaining in object */
                    lseek_cur(fd, current.size - 24 - 72);
            } else if (asf_guid_match(&current.guid, &asf_guid_stream_properties)) {
                    guid_t guid;
                    uint32_t propdatalen;

                    if (current.size < 78)
                        return ASF_ERROR_OBJECT_SIZE;

#if 0
                    asf_byteio_getGUID(&guid, current->data);
                    datalen = asf_byteio_getDWLE(current->data + 40);
                    flags = asf_byteio_getWLE(current->data + 48);
#endif

                    asf_readGUID(fd, &guid);

                    lseek_cur(fd, 24);
                    read_uint32le(fd, &propdatalen);
                    lseek_cur(fd, 4);
                    read_uint16le(fd, &flags);

                    if (!asf_guid_match(&guid, &asf_guid_stream_type_audio)) {
                        DEBUGF("Found stream properties for non audio stream, skipping\n");
                        lseek_cur(fd,current.size - 24 - 50);
                    } else if (wfx->audiostream == -1) {
                        lseek_cur(fd, 4);
                        DEBUGF("Found stream properties for audio stream %d\n",flags&0x7f);

                        if (propdatalen < 18) {
                            return ASF_ERROR_INVALID_LENGTH;
                        }

#if 0
                        if (asf_byteio_getWLE(data + 16) > datalen - 16) {
                            return ASF_ERROR_INVALID_LENGTH;
                        }
#endif
                        read_uint16le(fd, &wfx->codec_id);
                        read_uint16le(fd, &wfx->channels);
                        read_uint32le(fd, &wfx->rate);
                        read_uint32le(fd, &wfx->bitrate);
                        wfx->bitrate *= 8;
                        read_uint16le(fd, &wfx->blockalign);
                        read_uint16le(fd, &wfx->bitspersample);
                        read_uint16le(fd, &wfx->datalen);

                        /* Round bitrate to the nearest kbit */
                        id3->bitrate = (wfx->bitrate + 500) / 1000;
                        id3->frequency = wfx->rate;

                        if (wfx->codec_id == ASF_CODEC_ID_WMAV1) {
                            read_buf(fd, wfx->data, 4);
                            lseek_cur(fd,current.size - 24 - 72 - 4);
                            wfx->audiostream = flags&0x7f;
                        } else if (wfx->codec_id == ASF_CODEC_ID_WMAV2) {
                            read_buf(fd, wfx->data, 6);
                            lseek_cur(fd,current.size - 24 - 72 - 6);
                            wfx->audiostream = flags&0x7f;
                        } else {
                            DEBUGF("Unsupported WMA codec (Pro, Lossless, Voice, etc)\n");
                            lseek_cur(fd,current.size - 24 - 72);
                        }

                    }
            } else if (asf_guid_match(&current.guid, &asf_guid_content_description)) {
                    /* Object contains five 16-bit string lengths, followed by the five strings:
                       title, artist, copyright, description, rating
                     */
                    uint16_t strlength[5];
                    int i;

                    DEBUGF("Found GUID_CONTENT_DESCRIPTION - size=%d\n",(int)(current.size - 24));

                    /* Read the 5 string lengths - number of bytes included trailing zero */
                    for (i=0; i<5; i++) {
                        read_uint16le(fd, &strlength[i]);
                        DEBUGF("strlength = %u\n",strlength[i]);
                    }

                    if (strlength[0] > 0) {  /* 0 - Title */
                        id3->title = id3buf;
                        asf_utf16LEdecode(fd, strlength[0], &id3buf, &id3buf_remaining);
                    }

                    if (strlength[1] > 0) {  /* 1 - Artist */
                        id3->artist = id3buf;
                        asf_utf16LEdecode(fd, strlength[1], &id3buf, &id3buf_remaining);
                    }

                    lseek_cur(fd, strlength[2]); /* 2 - copyright */

                    if (strlength[3] > 0) {  /* 3 - description */
                        id3->comment = id3buf;
                        asf_utf16LEdecode(fd, strlength[3], &id3buf, &id3buf_remaining);
                    }

                    lseek_cur(fd, strlength[4]); /* 4 - rating */
            } else if (asf_guid_match(&current.guid, &asf_guid_extended_content_description)) {
                    uint16_t count;
                    int i;
                    int bytesleft = current.size - 24;
                    DEBUGF("Found GUID_EXTENDED_CONTENT_DESCRIPTION\n");

                    read_uint16le(fd, &count);
                    bytesleft -= 2;
                    DEBUGF("extended metadata count = %u\n",count);

                    for (i=0; i < count; i++) {
                        uint16_t length, type;
                        char* utf8 = (char*)utf8buf;
                        int utf8length = 512;

                        read_uint16le(fd, &length);
                        asf_utf16LEdecode(fd, length, &utf8, &utf8length);
                        bytesleft -= 2 + length;

                        read_uint16le(fd, &type);
                        read_uint16le(fd, &length);

                        if (!strcmp("WM/TrackNumber",(char*)utf8buf)) {
                            if (type == 0) {
                                id3->track_string = id3buf;
                                asf_utf16LEdecode(fd, length, &id3buf, &id3buf_remaining);
                                id3->tracknum = atoi(id3->track_string);
                            } else if ((type >=2) && (type <= 5)) {
                                id3->tracknum = asf_intdecode(fd, type, length);
                            } else {
                                lseek_cur(fd, length);
                            }
                        } else if ((!strcmp("WM/Genre",(char*)utf8buf)) && (type == 0)) {
                            id3->genre_string = id3buf;
                            asf_utf16LEdecode(fd, length, &id3buf, &id3buf_remaining);
                        } else if ((!strcmp("WM/AlbumTitle",(char*)utf8buf)) && (type == 0)) {
                            id3->album = id3buf;
                            asf_utf16LEdecode(fd, length, &id3buf, &id3buf_remaining);
                        } else if ((!strcmp("WM/AlbumArtist",(char*)utf8buf)) && (type == 0)) {
                            id3->albumartist = id3buf;
                            asf_utf16LEdecode(fd, length, &id3buf, &id3buf_remaining);
                        } else if ((!strcmp("WM/Composer",(char*)utf8buf)) && (type == 0)) {
                            id3->composer = id3buf;
                            asf_utf16LEdecode(fd, length, &id3buf, &id3buf_remaining);
                        } else if (!strcmp("WM/Year",(char*)utf8buf)) {
                            if (type == 0) {
                                id3->year_string = id3buf;
                                asf_utf16LEdecode(fd, length, &id3buf, &id3buf_remaining);
                                id3->year = atoi(id3->year_string);
                            } else if ((type >=2) && (type <= 5)) {
                                id3->year = asf_intdecode(fd, type, length);
                            } else {
                                lseek_cur(fd, length);
                            }
                        } else if (!strncmp("replaygain_", (char*)utf8buf, 11)) {
                            char* value = id3buf;
                            int buf_len = id3buf_remaining;
                            int len;
                            asf_utf16LEdecode(fd, length, &id3buf, &id3buf_remaining);
                            len=0; // len = parse_replaygain(utf8buf, value, id3, value, buf_len);
                            
                            if (len == 0) {
                                /* Don't need to keep the value */
                                id3buf = value;
                                id3buf_remaining = buf_len;
                            }
                        } else if (!strcmp("MusicBrainz/Track Id", (char*)utf8buf)) {
                            id3->mb_track_id = id3buf;
                            asf_utf16LEdecode(fd, length, &id3buf, &id3buf_remaining);
                        } else {
                            lseek_cur(fd, length);
                        }
                        bytesleft -= 4 + length;
                    }

                    lseek_cur(fd, bytesleft);
            } else if (asf_guid_match(&current.guid, &asf_guid_content_encryption)
                || asf_guid_match(&current.guid, &asf_guid_extended_content_encryption)) {
                DEBUGF("File is encrypted\n");
                return ASF_ERROR_ENCRYPTED;
            } else {
                DEBUGF("Skipping %d bytes of object\n",(int)(current.size - 24));
                lseek_cur(fd,current.size - 24);
            }

            DEBUGF("Parsed object - size = %d\n",(int)current.size);
            datalen -= current.size;
        }

        if (i != (int)subobjects || datalen != 0) {
            DEBUGF("header data doesn't match given subobject count\n");
            return ASF_ERROR_INVALID_VALUE;
        }

        DEBUGF("%d subobjects read successfully\n", i);
    }

#if 0
    tmp = asf_parse_header_validate(file, &header);
    if (tmp < 0) {
        /* header read ok but doesn't validate correctly */
        return tmp;
    }
#endif

    DEBUGF("header validated correctly\n");

    return 0;
}

bool get_asf_metadata(int fd, struct mp3entry* id3)
{
    int res;
    asf_object_t obj;
    asf_waveformatex_t wfx;

    wfx.audiostream = -1;

    res = asf_parse_header(fd, id3, &wfx);

    if (res < 0) {
        DEBUGF("ASF: parsing error - %d\n",res);
        return false;
    }

    if (wfx.audiostream == -1) {
        DEBUGF("ASF: No WMA streams found\n");
        return false;
    }

    asf_read_object_header(&obj, fd);

    if (!asf_guid_match(&obj.guid, &asf_guid_data)) {
        DEBUGF("ASF: No data object found\n");
        return false;
    }

    /* Store the current file position - no need to parse the header
       again in the codec.  The +26 skips the rest of the data object
       header.
     */
    id3->first_frame_offset = csi->curpos + 26;
    id3->filesize = GetFileSize(fd);
    
    /* We copy the wfx struct to the MP3 TOC field in the id3 struct so
       the codec doesn't need to parse the header object again */
    id3->wfx=wfx;

    return true;
}
