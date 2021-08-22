
#include "libwma_rockbox/config.h"

#include "libwma_rockbox/lib/codeclib.h"
#include "libwma_rockbox/codecs.h"
#include "libwma_rockbox/asf.h"
#include "libwma_rockbox/wmadec.h"

static struct codec_sapi static_csi;
struct codec_sapi *csi=&static_csi;

static struct mp3entry static_id3;

void CSI_set_elapsed(unsigned int value)
{
  _consolePrintf("Called CSI_set_elapsed(%d);\n",value);
}

size_t CSI_read_filebuf(void *ptr, size_t size)
{
  _consolePrintf("Called CSI_read_filebuf(0x%x,%d); curpos=0x%x\n",ptr,size,csi->curpos);
  
  u8 *pdst=(u8*)ptr;
  
  u32 remainsize=csi->filesize-csi->curpos;
  if(remainsize<size) size=remainsize;
  
  if(size!=0){
    const u8 *psrc=&csi->pfilebuf[csi->curpos];
    for(u32 idx=0;idx<size;idx++){
      pdst[idx]=psrc[idx];
    }
    csi->curpos+=size;
  }
  
  return(size);
}

size_t CSI_GetFileSize(void)
{
  _consolePrintf("Called CSI_GetFileSize(); size=0x%x\n",csi->filesize);
  
  return(csi->filesize);
}

bool CSI_seek_buffer_set(size_t newpos)
{
  _consolePrintf("Called CSI_seek_buffer_set(0x%x);\n",newpos);
  
  csi->curpos=newpos;
  
  return(true);
}

bool CSI_seek_buffer_cur(size_t pos)
{
  _consolePrintf("Called CSI_seek_buffer_cur(0x%x);\n",pos);
  
  csi->curpos+=pos;
  
  return(true);
}

void CSI_seek_complete(void)
{
  _consolePrintf("Called CSI_seek_complete();\n");
}

void* CSI_request_buffer(size_t *realsize, size_t reqsize)
{
  _consolePrintf("Called CSI_request_buffer(&0x%x,0x%x); curpos=0x%x\n",realsize,reqsize,csi->curpos);
  
  size_t remainsize=csi->filesize-csi->curpos;
  *realsize=reqsize;
  if(remainsize<*realsize) *realsize=remainsize;
  
  return((void*)&csi->pfilebuf[csi->curpos]);
}

void CSI_advance_buffer(size_t amount)
{
  _consolePrintf("Called CSI_advance_buffer(0x%x);\n",amount);
  csi->curpos+=amount;
}

bool CSI_request_next_track(void)
{
  _consolePrintf("Called CSI_request_next_track();\n");
  return(false);
}

bool CSI_pcmbuf_insert(const void *ch1, const void *ch2, int count)
{
  _consolePrintf("Called CSI_pcmbuf_insert(0x%x,0x%x,0x%x);\n",ch1,ch2,count);
  return(true);
  
#if 0
  {
    s32 *p=(s32*)ch1;
    for(u32 idx=0;idx<16;idx++){
      u16 s=*p++;
      _consolePrintf("%04x,",s&0xffff);
    }
  }
  _consolePrintf("\n");
  
  static int fh=0;
  if(fh==0) fh=PcOpen("a.wav",PC_CREAT);
  
  static u16 buf[2048*2];
  {
    s32 *p=(s32*)ch1;
    for(u32 idx=0;idx<count*2;idx++){
      u16 s=*p++;
      buf[idx]=s&0xffff;
    }
  }
  
  PcBWrite(fh,buf,count*2*2);
  
  return(true);
#endif
}

static void InitCSI(void)
{
  MemSet32CPU(0,csi,sizeof(struct codec_sapi));
  
  csi->pfilebuf=NULL; // wmadata_bin;
  csi->filesize=0; // wmadata_bin_size;
  csi->curpos=0;
  
  csi->seek_time=0;
  
  csi->id3=&static_id3;
  
  csi->id3->filesize=0;
  csi->id3->first_frame_offset=0;
  csi->id3->offset=0;
  
  {
    asf_waveformatex_t *p=&csi->id3->wfx;
    p->packet_size=0;
    p->audiostream=-1;
    p->codec_id=0x0000;
    p->channels=0;
    p->rate=0;
    p->bitrate=0;
    p->blockalign=0;
    p->bitspersample=0;
    p->datalen=0;
    p->data[0]=0;
    p->data[1]=0;
    p->data[2]=0;
    p->data[3]=0;
    p->data[4]=0;
    p->data[5]=0;
  }
  
  csi->stop_codec=false;
  csi->new_track=0;
}

// ---------------------------------------------------------

int packet_count=0;

/* The output buffer containing the decoded samples (channels 0 and 1)
   BLOCK_MAX_SIZE is 2048 (samples) and MAX_CHANNELS is 2.
 */

static uint32_t decoded[BLOCK_MAX_SIZE * MAX_CHANNELS] IBSS_ATTR;

/* NOTE: WMADecodeContext is 120152 bytes (on x86) */
static WMADecodeContext wmadec;

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
    ASF_ERROR_SEEK           = -10  /* file is seekable but seeking failed */
};

/* Read an unaligned 32-bit little endian long from buffer. */
static unsigned long get_long_le(void* buf)
{
    unsigned char* p = (unsigned char*) buf;

    return p[0] | (p[1] << 8) | (p[2] << 16) | (p[3] << 24);
}

/* Read an unaligned 16-bit little endian short from buffer. */
static unsigned short get_short_le(void* buf)
{
    unsigned char* p = (unsigned char*) buf;

    return p[0] | (p[1] << 8);
}

#define GETLEN2b(bits) (((bits) == 0x03) ? 4 : bits)

#define GETVALUE2b(bits, data) \
        (((bits) != 0x03) ? ((bits) != 0x02) ? ((bits) != 0x01) ? \
         0 : *(data) : get_short_le(data) : get_long_le(data))

static int asf_read_packet(uint8_t** audiobuf, int* audiobufsize, int* packetlength, asf_waveformatex_t* wfx)
{
    uint8_t tmp8, packet_flags, packet_property;
    int stream_id;
    int ec_length, opaque_data, ec_length_type;
    int datalen;
    uint8_t data[18];
    uint8_t* datap;
    uint32_t length;
    uint32_t padding_length;
    uint32_t send_time;
    uint16_t duration;
    uint16_t payload_count;
    int payload_length_type;
    uint32_t payload_hdrlen;
    int payload_datalen;
    int multiple;
    uint32_t replicated_length;
    uint32_t media_object_number;
    uint32_t media_object_offset;
    uint32_t bytesread = 0;
    uint8_t* buf;
    size_t bufsize;
    int i;
    DEBUGF("Reading new packet at %d bytes\n", (int)csi->curpos);

    if (CSI_read_filebuf(&tmp8, 1) == 0) {
        return ASF_ERROR_EOF;
    }
    bytesread++;

    /* TODO: We need a better way to detect endofstream */
    if (tmp8 != 0x82) {
    DEBUGF("Read failed:  packet did not sync\n");
    return -1;
    }


    if (tmp8 & 0x80) {
       ec_length = tmp8 & 0x0f;
       opaque_data = (tmp8 >> 4) & 0x01;
       ec_length_type = (tmp8 >> 5) & 0x03;

       if (ec_length_type != 0x00 || opaque_data != 0 || ec_length != 0x02) {
            DEBUGF("incorrect error correction flags\n");
            return ASF_ERROR_INVALID_VALUE;
       }

       /* Skip ec_data */
       CSI_advance_buffer(ec_length);
       bytesread += ec_length;
    } else {
        ec_length = 0;
    }

    if (CSI_read_filebuf(&packet_flags, 1) == 0) { return ASF_ERROR_EOF; }
    if (CSI_read_filebuf(&packet_property, 1) == 0) { return ASF_ERROR_EOF; }
    bytesread += 2;

    datalen = GETLEN2b((packet_flags >> 1) & 0x03) +
              GETLEN2b((packet_flags >> 3) & 0x03) +
              GETLEN2b((packet_flags >> 5) & 0x03) + 6;

#if 0
    if (datalen > sizeof(data)) {
        DEBUGF("Unexpectedly long datalen in data - %d\n",datalen);
        return ASF_ERROR_OUTOFMEM;
    }
#endif

    if (CSI_read_filebuf(data, datalen) == 0) {
        return ASF_ERROR_EOF;
    }

    bytesread += datalen;

    datap = data;
    length = GETVALUE2b((packet_flags >> 5) & 0x03, datap);
    datap += GETLEN2b((packet_flags >> 5) & 0x03);
    /* sequence value is not used */
    int sequencevalue=GETVALUE2b((packet_flags >> 1) & 0x03, datap);
    datap += GETLEN2b((packet_flags >> 1) & 0x03);
    padding_length = GETVALUE2b((packet_flags >> 3) & 0x03, datap);
    datap += GETLEN2b((packet_flags >> 3) & 0x03);
    send_time = get_long_le(datap);
    datap += 4;
    duration = get_short_le(datap);
    datap += 2;
    
    DEBUGF("packet_flags=0x%x, length=0x%x, sequencevalue=0x%x, padding_length=0x%x, send_time=%dms, duration=%dms\n",packet_flags, length, sequencevalue, padding_length, send_time, duration);
    /*DEBUGF("and duration %d ms\n", duration);*/

    /* this is really idiotic, packet length can (and often will) be
     * undefined and we just have to use the header packet size as the size
     * value */
    if (!((packet_flags >> 5) & 0x03)) {
         length = wfx->packet_size;
         DEBUGF("Length set to packet_size(0x%x)\n",length);
    }

    /* this is also really idiotic, if packet length is smaller than packet
     * size, we need to manually add the additional bytes into padding length
     */
    if (length < wfx->packet_size) {
        padding_length += wfx->packet_size - length;
        length = wfx->packet_size;
        DEBUGF("Length set to 0x%x (padding_length=%0x%x)\n",length,padding_length);
    }

    if (length > wfx->packet_size) {
        DEBUGF("packet with too big length value\n");
        return ASF_ERROR_INVALID_LENGTH;
    }

    /* check if we have multiple payloads */
    if (packet_flags & 0x01) {
        if (CSI_read_filebuf(&tmp8, 1) == 0) {
            return ASF_ERROR_EOF;
        }
        payload_count = tmp8 & 0x3f;
        payload_length_type = (tmp8 >> 6) & 0x03;
        DEBUGF("multiple payloads. count=%d, length_type=%d\n",payload_count,payload_length_type);
        bytesread++;
    } else {
        payload_count = 1;
        payload_length_type = 0x02; /* not used */
    }

    if (length < bytesread) {
        DEBUGF("header exceeded packet size, invalid file - length=%d, bytesread=%d\n",(int)length,(int)bytesread);
        /* FIXME: should this be checked earlier? */
        return ASF_ERROR_INVALID_LENGTH;
    }

    length-=12; // next header size?

    /* We now parse the individual payloads, and move all payloads
       belonging to our audio stream to a contiguous block, starting at
       the location of the first payload.
    */

    *audiobuf = NULL;
    *audiobufsize = 0;
    *packetlength = length - bytesread;

    buf = (uint8_t*)CSI_request_buffer(&bufsize, length);
    datap = buf;

    if (bufsize != length) {
        /* This should only happen with packets larger than 32KB (the
           guard buffer size).  All the streams I've seen have
           relatively small packets less than about 8KB), but I don't
           know what is expected.
        */
        DEBUGF("Could not read packet (requested %d bytes, received %d), curpos=%d, aborting\n",
               (int)length,(int)bufsize,(int)csi->curpos);
        return -1;
    }

    {
      bool multiple = packet_flags & 0x01;
      if((multiple==false)&&(payload_count!=1)){
        DEBUGF("Request payload_count!=1 in not multiple mode.\n");
        return -1;
      }
    }
    
    for (i=0; i<payload_count; i++) {
        stream_id = datap[0];
        datap++;
        bytesread++;
        
        DEBUGF("Payload(%d): stream_id:0x%x isKeyFrame=%d\n",i,stream_id&0x7f,stream_id>>7);
        stream_id&=0x7f;

        payload_hdrlen = GETLEN2b(packet_property & 0x03) +
                         GETLEN2b((packet_property >> 2) & 0x03) +
                         GETLEN2b((packet_property >> 4) & 0x03);

        //DEBUGF("payload_hdrlen = %d\n",payload_hdrlen);
#if 0
        /* TODO */
        if (payload_hdrlen > size) {
            return ASF_ERROR_INVALID_LENGTH;
        }
#endif
        if (payload_hdrlen > sizeof(data)) {
            DEBUGF("Unexpectedly long datalen in data - %d\n",datalen);
            return ASF_ERROR_OUTOFMEM;
        }

        bytesread += payload_hdrlen;
        media_object_number = GETVALUE2b((packet_property >> 4) & 0x03, datap);
        datap += GETLEN2b((packet_property >> 4) & 0x03);
        media_object_offset = GETVALUE2b((packet_property >> 2) & 0x03, datap);
        datap += GETLEN2b((packet_property >> 2) & 0x03);
        replicated_length = GETVALUE2b(packet_property & 0x03, datap);
        datap += GETLEN2b(packet_property & 0x03);
        
        DEBUGF("packet_property:0x%x length:0x%x, media_object_number:0x%x, media_object_offset:0x%x, replicated_length:0x%x\n",packet_property,payload_hdrlen,media_object_number, media_object_offset, replicated_length);
        
        /* TODO: Validate replicated_length */
        /* TODO: Is the content of this important for us? */
//        datap += replicated_length;
//        bytesread += replicated_length;
        
        if(8<=replicated_length){
          u32 packet_obj_size=get_long_le(datap);
          datap+=4;
          u32 packet_frag_timestamp=get_long_le(datap);
          datap+=4;
          if((packet_obj_size&0xff000000)!=0){
            DEBUGF("packet_obj_size invalid\n");
            return(0);
          }
          DEBUGF("Replicated packet: obj_size=0x%x, frag_timestamp=%d\n",packet_obj_size,packet_frag_timestamp);
          datap += replicated_length-8;
          bytesread += replicated_length;
          }else{
          if(2<=replicated_length){
            DEBUGF("Replicated packet: unexpected packet_replic_size of %d\n", replicated_length);
            return(0);
            }else{
            if(1<=replicated_length){
              u32 packet_time_delta=*datap;
              datap++;
              bytesread++;
              DEBUGF("Replicated packet: time_delta=%d",packet_time_delta);
            }
          }
        }
        
        multiple = packet_flags & 0x01;
        if (multiple) {
            int x;

            x = GETLEN2b(payload_length_type);

            if (x != 2) {
                /* in multiple payloads datalen should be a word */
                return ASF_ERROR_INVALID_VALUE;
            }

#if 0
            if (skip + tmp > datalen) {
                /* not enough data */
                return ASF_ERROR_INVALID_LENGTH;
            }
#endif
            payload_datalen = GETVALUE2b(payload_length_type, datap);
            datap += x;
            bytesread += x;
            DEBUGF("Multiple packet. payload_datalen= 0x%x\n",payload_datalen);
        } else {
            payload_datalen = length - bytesread - padding_length;
            DEBUGF("Single packet. payload_datalen= 0x%x\n",payload_datalen);
        }

        if (stream_id == wfx->audiostream)
        {
            DEBUGF("Found audio stream.\n");
            if (*audiobuf == NULL) {
                /* The first payload can stay where it is */
                *audiobuf = datap;
                *audiobufsize = payload_datalen;
            } else {
                /* The second and subsequent payloads in this packet
                   that belong to the audio stream need to be moved to be
                   contiguous with the first payload.
                */
                memmove(*audiobuf + *audiobufsize, datap, payload_datalen);
                *audiobufsize += payload_datalen;
            }
        }
        datap += payload_datalen;
        bytesread += payload_datalen;
    }
    
    DEBUGF("Closed packet. bytesread=0x%x, length=0x%x.\n",bytesread,length);

    if (*audiobuf != NULL)
        return 1;
    else
        return 0;
}


static int get_timestamp(int *duration)
{
    uint8_t tmp8, packet_flags, packet_property;
    int ec_length, opaque_data, ec_length_type;
    int datalen;
    uint8_t data[18];
    uint8_t* datap;
    uint32_t length;
    uint32_t padding_length;
    uint32_t send_time;

    uint32_t bytesread = 0;
    packet_count++;
    if (CSI_read_filebuf(&tmp8, 1) == 0) {
        DEBUGF("ASF ERROR (EOF?)\n");
        return ASF_ERROR_EOF;
    }
    bytesread++;

    /* TODO: We need a better way to detect endofstream */
    if (tmp8 != 0x82) {
        DEBUGF("Get timestamp:  Detected end of stream\n");
        return ASF_ERROR_EOF;
    }


    if (tmp8 & 0x80) {
        ec_length = tmp8 & 0x0f;
        opaque_data = (tmp8 >> 4) & 0x01;
        ec_length_type = (tmp8 >> 5) & 0x03;

        if (ec_length_type != 0x00 || opaque_data != 0 || ec_length != 0x02) {
             DEBUGF("incorrect error correction flags\n");
             return ASF_ERROR_INVALID_VALUE;
        }

        /* Skip ec_data */
        CSI_advance_buffer(ec_length);
        bytesread += ec_length;
    } else {
        ec_length = 0;
    }

    if (CSI_read_filebuf(&packet_flags, 1) == 0) {
//        DEBUGF("Detected end of stream 2\n");
        _consolePrintf("Detected end of stream 2\n");
        return ASF_ERROR_EOF;
    }

    if (CSI_read_filebuf(&packet_property, 1) == 0) {
        DEBUGF("Detected end of stream3\n");
        return ASF_ERROR_EOF;
    }
    bytesread += 2;

    datalen = GETLEN2b((packet_flags >> 1) & 0x03) +
              GETLEN2b((packet_flags >> 3) & 0x03) +
              GETLEN2b((packet_flags >> 5) & 0x03) + 6;

    if (CSI_read_filebuf(data, datalen) == 0) {
//        DEBUGF("Detected end of stream4\n");
        _consolePrintf("Detected end of stream4\n");
        return ASF_ERROR_EOF;
    }

    bytesread += datalen;

    datap = data;
    length = GETVALUE2b((packet_flags >> 5) & 0x03, datap);
    datap += GETLEN2b((packet_flags >> 5) & 0x03);

    /* sequence value is not used */
    GETVALUE2b((packet_flags >> 1) & 0x03, datap);
    datap += GETLEN2b((packet_flags >> 1) & 0x03);
    padding_length = GETVALUE2b((packet_flags >> 3) & 0x03, datap);
    datap += GETLEN2b((packet_flags >> 3) & 0x03);
    send_time = get_long_le(datap);
    datap += 4;
    *duration = get_short_le(datap);

    /*the get_timestamp function advances us 12-13 bytes past the packet start,
      need to undo this here so that we stay synced with the packet*/
    CSI_seek_buffer_set(csi->curpos-bytesread);

    return send_time;
}

/*entry point for seeks*/
static int seek(int ms, asf_waveformatex_t* wfx)
{
    int time, duration, delta, temp, count=0;

    /*estimate packet number from bitrate*/
    int initial_packet = csi->curpos/wfx->packet_size;
    int packet_num = (((int64_t)ms)*(wfx->bitrate>>3))/wfx->packet_size/1000;
    int last_packet = 
    csi->id3->filesize 
    / wfx->packet_size;

    if (packet_num > last_packet) {
        packet_num = last_packet;
    }

    /*calculate byte address of the start of that packet*/
    int packet_offset = packet_num*wfx->packet_size;

    /*seek to estimated packet*/
    CSI_seek_buffer_set(csi->id3->first_frame_offset+packet_offset);
    temp = ms;
    while (1)
    {
        /*for very large files it can be difficult and unimportant to find the exact packet*/
        count++;

        /*check the time stamp of our packet*/
        time = get_timestamp(&duration);
        DEBUGF("seeked to %d ms with duration %d\n", time, duration);

        if (time < 0) {
            /*unknown error, try to recover*/
            DEBUGF("UKNOWN SEEK ERROR\n");
            CSI_seek_buffer_set(csi->id3->first_frame_offset+initial_packet*wfx->packet_size);
            /*seek failed so return time stamp of the initial packet*/
            return get_timestamp(&duration);
        }

        if ((time+duration>=ms && time<=ms) || count > 10) {
            DEBUGF("Found our packet! Now at %d packet\n", packet_num);
            return time;
        } else {
            /*seek again*/
            delta = ms-time;
            /*estimate new packet number from bitrate and our current position*/
            temp += delta;
            packet_num = ((temp/1000)*(wfx->bitrate>>3) - (wfx->packet_size>>1))/wfx->packet_size;  //round down!
            packet_offset = packet_num*wfx->packet_size;
            CSI_seek_buffer_set(csi->id3->first_frame_offset+packet_offset);
        }
    }
}

// -----------------------------------------------------------

enum codec_status codec_main(void)
{
    uint32_t elapsedtime;
    codec_status retval;
    asf_waveformatex_t wfx;
    size_t resume_offset;
    int i;
    int wmares, res;
    uint8_t* audiobuf;
    int audiobufsize;
    int packetlength = 0;
    int errcount = 0;

    /* Generic codec initialisation */
//    ci->configure(CODEC_SET_FILEBUF_WATERMARK, 1024*512);

//    ci->configure(DSP_SET_SAMPLE_DEPTH, 29);

next_track:

    /* Wait for the metadata to be read */
/*
    while (!*ci->taginfo_ready && !ci->stop_codec)
        ci->sleep(1);
*/

    retval = CODEC_OK;

    /* Remember the resume position - when the codec is opened, the
       playback engine will reset it. */
    resume_offset = csi->id3->offset;
restart_track:

    /* Copy the format metadata we've stored in the id3 TOC field.  This
       saves us from parsing it again here. */
    wfx=csi->id3->wfx;

    if (wma_decode_init(&wmadec,&wfx) < 0) {
        LOGF("WMA: Unsupported or corrupt file\n");
        retval = CODEC_ERROR;
        goto exit;
    }

    DEBUGF("**************** IN WMA.C ****************\n");

    if (resume_offset > csi->id3->first_frame_offset)
    {
        /* Get start of current packet */
        int packet_offset = (resume_offset - csi->id3->first_frame_offset)
            % wfx.packet_size;
        CSI_seek_buffer_set(resume_offset - packet_offset);
        elapsedtime = get_timestamp(&i);
        CSI_set_elapsed(elapsedtime);
    }
    else
    {
        /* Now advance the file position to the first frame */
        CSI_seek_buffer_set(csi->id3->first_frame_offset);
        elapsedtime = 0;
    }

    resume_offset = 0;
//    ci->configure(DSP_SWITCH_FREQUENCY, wfx.rate);
//    ci->configure(DSP_SET_STEREO_MODE, wfx.channels == 1 ? STEREO_MONO : STEREO_INTERLEAVED);
//    codec_set_replaygain(csi->id3);

    /* The main decoding loop */

    res = 1;
    while (res >= 0)
    {
        if (csi->stop_codec || csi->new_track) {
            goto done;
        }

        /* Deal with any pending seek requests */
        if (csi->seek_time){

            if (csi->seek_time == 1) {
                CSI_seek_complete();
                goto restart_track; /* Pretend you never saw this... */
            }

            elapsedtime = seek(csi->seek_time, &wfx);
            if (elapsedtime < 1){
                CSI_seek_complete();
                goto next_track;
            }
            /*DEBUGF("Seek returned %d\n", (int)elapsedtime);*/
            CSI_set_elapsed(elapsedtime);

            /*flush the wma decoder state*/
            wmadec.last_superframe_len = 0;
            wmadec.last_bitoffset = 0;
            CSI_seek_complete();
        }
        errcount = 0;
new_packet:
        res = asf_read_packet(&audiobuf, &audiobufsize, &packetlength, &wfx);

        if (res < 0) {
            /* We'll try to recover from a parse error a certain number of
             * times. If we succeed, the error counter will be reset.
             */

            errcount++;
            DEBUGF("read_packet error %d, errcount %d\n",wmares, errcount);
            if (errcount > 5) {
                goto done;
            } else {
                CSI_advance_buffer(packetlength);
                goto new_packet;
            }
        } else if (res > 0) {
            wma_decode_superframe_init(&wmadec, audiobuf, audiobufsize);

            for (i=0; i < wmadec.nb_frames; i++)
            {
                wmares = wma_decode_superframe_frame(&wmadec,
                                                     (int32_t*)decoded,
                                                     audiobuf, audiobufsize);

                if (wmares < 0) {
                    /* Do the above, but for errors in decode. */
                    errcount++;
                    DEBUGF("WMA decode error %d, errcount %d\n",wmares, errcount);
                    if (errcount > 5) {
                        goto done;
                    } else {
                        CSI_advance_buffer(packetlength);
                        goto new_packet;
                    }
                } else if (wmares > 0) {
                    CSI_pcmbuf_insert(decoded, NULL, wmares);
                    elapsedtime += (wmares*10)/(wfx.rate/100);
                    CSI_set_elapsed(elapsedtime);
                }
            }
        }

        CSI_advance_buffer(packetlength);
    }
    retval = CODEC_OK;

done:
    LOGF("WMA: Decoded %ld samples\n",elapsedtime*wfx.rate/1000);

    if (CSI_request_next_track())
        goto next_track;
exit:
    return retval;
}

static void wma_show_wfx(asf_waveformatex_t *wfx)
{
  _consolePrintf("--- info asf_waveformatex ---\n");
  _consolePrintf("Packet size: %d\n",wfx->packet_size);
  _consolePrintf("Audio stream ID: %d\n",wfx->audiostream);
  _consolePrintf("Format Tag: %d (0x%x)\n",wfx->codec_id,wfx->codec_id);
  _consolePrintf("Channels: %d\n",wfx->channels);
  _consolePrintf("Samplerate: %d\n",wfx->rate);
  _consolePrintf("Avg.bitrate: %d\n",wfx->bitrate);
  _consolePrintf("Block align: %d\n",wfx->blockalign);
  _consolePrintf("bits/sample: %d\n",wfx->bitspersample);
  _consolePrintf("datalen: %d\n",wfx->datalen);
  
  _consolePrintf("data: ");
  for(u32 idx=0;idx<6;idx++){
    _consolePrintf("%02x, ",wfx->data[idx]);
  }
  _consolePrintf("\n");
  
  _consolePrintf("Codec: ");
  
  switch(wfx->codec_id){
    case 0x01:        _consolePrintf("PCM format\n"); break;
    case 0x50:        _consolePrintf("MPEG Layer 1/2 format\n"); break;
    case 0x55:        _consolePrintf("MPEG Layer-3 format\n"); break; // ACM
    case 0x02:        _consolePrintf("MS ADPCM format\n"); break;  // ACM
    case 0x11:        _consolePrintf("IMA ADPCM format\n"); break; // ACM
    case 0x31:
    case 0x32:        _consolePrintf("MS GSM 6.10 format\n"); break; // ACM
    case 0x160:       _consolePrintf("Microsoft Audio1\n"); break;
    case 0x161:       _consolePrintf("Windows Media Audio V2 V7 V8 V9 / DivX audio (WMA) / Alex AC3 Audio\n"); break;
    case 0x162:       _consolePrintf("Windows Media Audio Professional V9\n"); break;
    case 0x163:       _consolePrintf("Windows Media Audio Lossless V9\n"); break;
    default:          _consolePrintf("UNKNOWN (id=0x%X) format\n", wfx->codec_id);
  }
  
  _consolePrintf("-----------------------------\n");
}

// ------------------------------------------------------------------------------

static bool wma_init(void)
{
  InitCSI();
  
  if(get_asf_metadata(0,csi->id3)==false) return(false);
  
  wma_show_wfx(&csi->id3->wfx);
  
    uint32_t elapsedtime;
    codec_status retval;
    static asf_waveformatex_t wfx;
    size_t resume_offset;
    int i;
    int wmares, res;
    uint8_t* audiobuf;
    int audiobufsize;
    int packetlength = 0;
    int errcount = 0;

    /* Generic codec initialisation */
//    ci->configure(CODEC_SET_FILEBUF_WATERMARK, 1024*512);

//    ci->configure(DSP_SET_SAMPLE_DEPTH, 29);

next_track:

    /* Wait for the metadata to be read */
/*
    while (!*ci->taginfo_ready && !ci->stop_codec)
        ci->sleep(1);
*/

    retval = CODEC_OK;

    /* Remember the resume position - when the codec is opened, the
       playback engine will reset it. */
    resume_offset = csi->id3->offset;
restart_track:

    /* Copy the format metadata we've stored in the id3 TOC field.  This
       saves us from parsing it again here. */
    wfx=csi->id3->wfx;

    if (wma_decode_init(&wmadec,&wfx) < 0) {
        LOGF("WMA: Unsupported or corrupt file\n");
        return(false);
    }
    
    if (resume_offset > csi->id3->first_frame_offset)
    {
        /* Get start of current packet */
        int packet_offset = (resume_offset - csi->id3->first_frame_offset)
            % wfx.packet_size;
        CSI_seek_buffer_set(resume_offset - packet_offset);
        elapsedtime = get_timestamp(&i);
        CSI_set_elapsed(elapsedtime);
    }
    else
    {
        /* Now advance the file position to the first frame */
        CSI_seek_buffer_set(csi->id3->first_frame_offset);
        elapsedtime = 0;
    }

    resume_offset = 0;
//    ci->configure(DSP_SWITCH_FREQUENCY, wfx.rate);
//    ci->configure(DSP_SET_STEREO_MODE, wfx.channels == 1 ? STEREO_MONO : STEREO_INTERLEAVED);
//    codec_set_replaygain(csi->id3);

// --------------------------------------------------------------

    PrfStart();
    
    res = 1;
    while (res >= 0)
    {
        if (csi->stop_codec || csi->new_track) {
            goto done;
        }

        /* Deal with any pending seek requests */
        if (csi->seek_time){

            if (csi->seek_time == 1) {
                CSI_seek_complete();
                goto restart_track; /* Pretend you never saw this... */
            }

            elapsedtime = seek(csi->seek_time, &wfx);
            if (elapsedtime < 1){
                CSI_seek_complete();
                goto next_track;
            }
            /*DEBUGF("Seek returned %d\n", (int)elapsedtime);*/
            CSI_set_elapsed(elapsedtime);

            /*flush the wma decoder state*/
            wmadec.last_superframe_len = 0;
            wmadec.last_bitoffset = 0;
            CSI_seek_complete();
        }
        errcount = 0;
        
new_packet:
        DEBUGF("\n----------------- Start decode loop.\n");
        
        res = asf_read_packet(&audiobuf, &audiobufsize, &packetlength, &wfx);

        if (res < 0) {
            /* We'll try to recover from a parse error a certain number of
             * times. If we succeed, the error counter will be reset.
             */

            errcount++;
            DEBUGF("read_packet error %d, errcount %d\n",wmares, errcount);
            if (errcount > 5) {
                goto done;
            } else {
                CSI_advance_buffer(packetlength);
                goto new_packet;
            }
        } else if (res > 0) {
            wma_decode_superframe_init(&wmadec, audiobuf, audiobufsize);
            DEBUGF("wmadec.nb_frames: %d\n",wmadec.nb_frames);

            for (i=0; i < wmadec.nb_frames; i++)
            {
                wmares = wma_decode_superframe_frame(&wmadec,
                                                     (int32_t*)decoded,
                                                     audiobuf, audiobufsize);

                if (wmares < 0) {
                    /* Do the above, but for errors in decode. */
                    errcount++;
                    DEBUGF("WMA decode error %d, errcount %d\n",wmares, errcount);
                    if (errcount > 5) {
                        goto done;
                    } else {
                        CSI_advance_buffer(packetlength);
                        goto new_packet;
                    }
                } else if (wmares > 0) {
                    CSI_pcmbuf_insert(decoded, NULL, wmares);
                    elapsedtime += (wmares*10)/(wfx.rate/100);
                    CSI_set_elapsed(elapsedtime);
                }
            }
            
            DEBUGF("BitStream: Remain=0x%xbytes, size_in_bits=%d\n",wmadec.gb.buffer_end-wmadec.gb.buffer,wmadec.gb.size_in_bits);
            
            CSI_advance_buffer(12); // next header size?
        }

        CSI_advance_buffer(packetlength);
    }
    retval = CODEC_OK;
    
done:
    LOGF("WMA: Decoded %ld samples\n",elapsedtime*wfx.rate/1000);

    PrfEnd(0);

    if (CSI_request_next_track())
        goto next_track;
exit:
//------------------------------------------------

  return(true);
}

static u32 wma_decode(s16 *psmpbuf,u32 smpbufcount)
{
  return(0);
}

static void wma_free(void)
{
}

