
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <NDS.h>
#include "_console.h"
#include "maindef.h"
#include "libmp3/mad.h"
#include "memtool.h"

#include "plug_mp3.h"

// --------------------------------------------------------------------

static FAT_FILE *FileHandle;
static s32 FileSize;
static s32 FileOffset;

typedef struct mad_decoder Tmad_decoder;

static Tmad_decoder StaticMadDecoder;

#define SamplePerFrame (1152/1)

// --------------------------------------------------------------------

static s16 *outlbuf,*outrbuf;
static u32 outofs;

#define INPUT_BUFFER_SIZE (8*1024)
static u8 *InputBuffer=NULL;

static
enum mad_flow input(void *data,
                    struct mad_stream *stream)
{
  size_t ReadSize,Remaining;
  u8 *ReadStart;
  
  if(stream->next_frame!=NULL)
  {
    Remaining=stream->bufend-stream->next_frame;
    memmove(InputBuffer,stream->next_frame,Remaining);
    ReadStart=InputBuffer+Remaining;
    ReadSize=INPUT_BUFFER_SIZE-Remaining;
    
    }else{
    ReadSize=INPUT_BUFFER_SIZE;
    ReadStart=InputBuffer;
    Remaining=0;
  }
  
  if((FileOffset+ReadSize)>=FileSize){
    ReadSize=FileSize-FileOffset;
  }
  
//  _consolePrintf("r%d->%d (0x%x) (0x%x)\n",Remaining,ReadSize,ReadStart,FileHandle);
  
  if(ReadSize<=0) return MAD_FLOW_STOP;
  
  FAT2_fread(ReadStart,1,ReadSize,FileHandle);
  FileOffset+=ReadSize;
  
  mad_stream_buffer(stream,InputBuffer,Remaining+ReadSize);
  
  stream->error=MAD_ERROR_NONE;
  
  return MAD_FLOW_CONTINUE;
}

#if 0
static inline signed int scale(mad_fixed_t sample)
{
  /* round */
  sample += (1L << (MAD_F_FRACBITS - 16));

  /* clip */
  if (sample >= MAD_F_ONE)
    sample = MAD_F_ONE - 1;
  else if (sample < -MAD_F_ONE)
    sample = -MAD_F_ONE;

  /* quantize */
  return sample >> (MAD_F_FRACBITS + 1 - 16);
}
#endif

static inline signed int scale(mad_fixed_t sample)
{
  sample >>= MAD_F_FRACBITS + 1 - 16;
  if(sample<-32768) sample=-32768;
  if(32767<sample) sample=32767;
  return(sample);
}

static
enum mad_flow output(void *data,
                     struct mad_header const *header,
                     struct mad_pcm *pcm)
{
  unsigned int nchannels, nsamples;
  mad_fixed_t const *left_ch, *right_ch;

  /* pcm->samplerate contains the sampling frequency */

  nchannels = pcm->channels;
  nsamples  = pcm->length;
  left_ch   = pcm->samples[0];
  right_ch  = pcm->samples[1];

  if(nsamples!=SamplePerFrame){
//    _consolePrintf("nsamples==%d!=%d\n",nsamples,SamplePerFrame);
//    return MAD_FLOW_STOP;
  }
  
  if((outlbuf==NULL)||(outrbuf==NULL)){
    outofs+=nsamples;
    return MAD_FLOW_CONTINUE;
  }
  
  if (nchannels == 1) {
    while (nsamples--) {
      signed int sample;

      sample = scale(*left_ch++);
      outlbuf[outofs]=sample;
      outrbuf[outofs]=sample;
      outofs++;
    }
    return MAD_FLOW_CONTINUE;
  }

  if (nchannels == 2) {
    while (nsamples--) {
      outlbuf[outofs]=scale(*left_ch++);
      outrbuf[outofs]=scale(*right_ch++);
      outofs++;
    }
    return MAD_FLOW_CONTINUE;
  }

  return MAD_FLOW_STOP;
}

static
enum mad_flow error(void *data,
                    struct mad_stream *stream,
                    struct mad_frame *frame)
{
  if(stream->error==MAD_ERROR_LOSTSYNC) return MAD_FLOW_CONTINUE;
  if(stream->error==MAD_ERROR_BADLAYER) return MAD_FLOW_CONTINUE;
  if(stream->error==MAD_ERROR_BADBITRATE) return MAD_FLOW_CONTINUE;
  if(stream->error==MAD_ERROR_BADSAMPLERATE) return MAD_FLOW_CONTINUE;
  if(stream->error==MAD_ERROR_BADEMPHASIS) return MAD_FLOW_CONTINUE;
  
  _consolePrintf("decoding error 0x%04x (%s) at byte offset %u\n", stream->error, mad_stream_errorstr(stream), FileOffset);

  /* return MAD_FLOW_BREAK here to stop decoding (and propagate an error) */

  return MAD_FLOW_CONTINUE;//MAD_FLOW_STOP;
}

// --------------------------------------------------------------------

#define madusr_Return_OK (0)
#define madusr_Return_NG (-1)

#define madusr_ExecReturn_Next (0)
#define madusr_ExecReturn_End (-1)
#define madusr_ExecReturn_Fail (-2)

static
int madusr_decode_init(Tmad_decoder *decoder)
{
  decoder->sync = NULL;
  
  /* configure input, output, and error functions */
  
  mad_decoder_init(decoder, NULL, input, 0 /* header */, 0 /* filter */, output, error, 0 /* message */);
  
  if (decoder->input_func == 0) return madusr_Return_NG;
  if (decoder->output_func == 0) return madusr_Return_NG;
  if (decoder->error_func==0) return madusr_Return_NG;
  
  decoder->sync = (Tmad_decoder_sync*)safemalloc(sizeof(Tmad_decoder_sync));
  if (decoder->sync == NULL) return madusr_Return_NG;
  
  struct mad_stream *stream = &decoder->sync->stream;
  struct mad_frame *frame = &decoder->sync->frame;
  struct mad_synth *synth = &decoder->sync->synth;

  mad_stream_init(stream);
  mad_frame_init(frame);
  mad_synth_init(synth);

  mad_stream_options(stream, decoder->options);
  
  if(decoder->input_func(decoder->cb_data, stream)!=MAD_FLOW_CONTINUE) return madusr_Return_NG;
  
  return madusr_Return_OK;
}

//#include "../_consoleWriteLog.h"

static
int madusr_decode_exec(struct mad_decoder *decoder)
{
  enum mad_flow (*error_func)(void *, struct mad_stream *, struct mad_frame *);
  void *error_data;
  
  error_func = decoder->error_func;
  error_data = decoder->cb_data;
  
  struct mad_stream *stream = &decoder->sync->stream;
  struct mad_frame *frame = &decoder->sync->frame;
  struct mad_synth *synth = &decoder->sync->synth;

  if(stream->error == MAD_ERROR_BUFLEN){
    return madusr_ExecReturn_Fail;
  }
  
//  PrfStart();
  
  while(1){
    if (decoder->header_func) {
      if (mad_header_decode(&frame->header, stream) == -1) {
        if (!MAD_RECOVERABLE(stream->error)){
          if(stream->error==MAD_ERROR_BUFLEN){
            if(decoder->input_func(decoder->cb_data, stream)!=MAD_FLOW_CONTINUE) return madusr_ExecReturn_Fail;
            continue;
          }
          return madusr_ExecReturn_Fail;
        }
  
        if(stream->error==MAD_ERROR_BUFLEN){
          if(decoder->input_func(decoder->cb_data, stream)!=MAD_FLOW_CONTINUE) return madusr_ExecReturn_Fail;
          continue;
        }
        
        switch (error_func(error_data, stream, frame)) {
          case MAD_FLOW_STOP:
            return madusr_ExecReturn_End;
          case MAD_FLOW_BREAK:
            return madusr_ExecReturn_Fail;
          case MAD_FLOW_IGNORE:
          case MAD_FLOW_CONTINUE:
          default:
            return madusr_ExecReturn_Next;
        }
      }
  
      switch (decoder->header_func(decoder->cb_data, &frame->header)) {
        case MAD_FLOW_STOP:
          return madusr_ExecReturn_End;
        case MAD_FLOW_BREAK:
          return madusr_ExecReturn_Fail;
        case MAD_FLOW_IGNORE:
          return madusr_ExecReturn_Next;
        case MAD_FLOW_CONTINUE:
          continue;
          break;
      }
    }
    break;
  }

//  PrfEnd(1);
//  PrfStart();
  
  while(1){
    if (mad_frame_decode(frame, stream) == -1) {
    
      if (!MAD_RECOVERABLE(stream->error)){
        if(stream->error==MAD_ERROR_BUFLEN){
          if(decoder->input_func(decoder->cb_data, stream)!=MAD_FLOW_CONTINUE) return madusr_ExecReturn_Fail;
          continue;
        }
        return madusr_ExecReturn_Fail;
      }
      
      switch (error_func(error_data, stream, frame)) {
        case MAD_FLOW_STOP:
          return madusr_ExecReturn_End;
        case MAD_FLOW_BREAK:
          return madusr_ExecReturn_Fail;
        case MAD_FLOW_IGNORE:
          return madusr_ExecReturn_Next;
        case MAD_FLOW_CONTINUE:
          continue;
        default:
          break;
      }
    }
    break;
  }
  
//  PrfEnd(2);
//  PrfStart();
  
  if((outlbuf==NULL)||(outrbuf==NULL)){
    outofs+=synth->pcm.length;
    return madusr_ExecReturn_Next;
  }
  
  if (decoder->filter_func) {
    switch (decoder->filter_func(decoder->cb_data, stream, frame)) {
      case MAD_FLOW_STOP:
        return madusr_ExecReturn_End;
      case MAD_FLOW_BREAK:
        return madusr_ExecReturn_Fail;
      case MAD_FLOW_IGNORE:
        return madusr_ExecReturn_Next;
      case MAD_FLOW_CONTINUE:
        break;
    }
  }

//  PrfEnd(3);
//  PrfStart();
  
  mad_synth_frame(synth, frame);

//  PrfEnd(4);
//  PrfStart();
  
  if (decoder->output_func) {
    switch (decoder->output_func(decoder->cb_data, &frame->header, &synth->pcm)) {
      case MAD_FLOW_STOP:
        return madusr_ExecReturn_End;
      case MAD_FLOW_BREAK:
        return madusr_ExecReturn_Fail;
      case MAD_FLOW_IGNORE:
      case MAD_FLOW_CONTINUE:
        break;
    }
  }
  
//  PrfEnd(5);
  
  return madusr_ExecReturn_Next;
}

static
void madusr_decode_free(struct mad_decoder *decoder)
{
  if(decoder->sync!=NULL){
    struct mad_stream *stream = &decoder->sync->stream;
    struct mad_frame *frame = &decoder->sync->frame;
    struct mad_synth *synth = &decoder->sync->synth;
    
    mad_synth_finish(synth);
    mad_frame_finish(frame);
    mad_stream_finish(stream);
    
    safefree(decoder->sync);
    decoder->sync = NULL;
  }
  
  mad_decoder_finish(decoder);
}

// --------------------------------------------------------------------

#include "plug_mp3_id3tag.h"

// --------------------------------------------------------------------

bool PlugMP3_Start(FAT_FILE *_FileHandle)
{
  FileHandle=_FileHandle;
  FileOffset=0;
  
  FileSize=FAT2_GetFileSize(FileHandle);
  
  ReadID3TAG();
//  _consolePrintf("%s,%s,%s,%s\n",ID3Tag.title,ID3Tag.artist,ID3Tag.album,ID3Tag.comment);
  
  InputBuffer=(u8*)safemalloc(INPUT_BUFFER_SIZE+MAD_BUFFER_GUARD);
  if(InputBuffer==NULL){
    _consolePrint("InputBuffer out of memory.\n");
    PlugMP3_Free();
    return(false);
  }
  
//  _consolePrint("libmad init.\n");
  
  int result;
  
  result=madusr_decode_init(&StaticMadDecoder);
  if(result==madusr_Return_NG){
    _consolePrint("madusr_decode_init()==madusr_Return_NG.\n");
    PlugMP3_Free();
    return(false);
  }
  
  outlbuf=NULL;
  outrbuf=NULL;
  outofs=0;
  
  result=madusr_decode_exec(&StaticMadDecoder);
  if(result!=madusr_ExecReturn_Next){
    _consolePrintf("madusr_decode_exec()==%d!=madusr_ExecReturn_Next.\n",result);
    PlugMP3_Free();
    return(false);
  }
  
#if 0
  struct mad_frame *frame = &StaticMadDecoder.sync->frame;
  
  _consolePrint("\n");
  
  _consolePrint("Format=");
  switch(frame->header.layer){
    case MAD_LAYER_I:
      _consolePrint("Layer I\n");
      break;
    case MAD_LAYER_II:
      _consolePrint("Layer II\n");
      break;
    case MAD_LAYER_III:
      _consolePrint("Layer III\n");
      break;
    default:
      _consolePrint("Unknown layer.\n");
      PlugMP3_Free();
      return(false);
      break;
  }

  _consolePrint("ChannelMode=");
  switch(frame->header.mode){
    case MAD_MODE_SINGLE_CHANNEL:
      _consolePrint("SingleChannel\n");
      break;
    case MAD_MODE_DUAL_CHANNEL:
      _consolePrint("DualChannel\n");
      break;
    case MAD_MODE_JOINT_STEREO:
      _consolePrint("JointStereo\n");
      break;
    case MAD_MODE_STEREO:
      _consolePrint("Normal LR Stereo\n");
      break;
    default:
      _consolePrint("unknown ChannelMode.\n");
      PlugMP3_Free();
      return(false);
      break;
  }
  
  _consolePrintf("BitRate=%dkbps\n",frame->header.bitrate/1000);
  _consolePrintf("SampleRate=%dHz\n",frame->header.samplerate);
#endif
  
  return(true);
}

void PlugMP3_Free(void)
{
  madusr_decode_free(&StaticMadDecoder);
  if(InputBuffer!=NULL){
    safefree(InputBuffer); InputBuffer=NULL;
  }
}

u32 PlugMP3_Update(s16 *lbuf,s16 *rbuf)
{
  outlbuf=lbuf;
  outrbuf=rbuf;
  outofs=0;
  
  int result=madusr_ExecReturn_Next;
  
  while((result==madusr_ExecReturn_Next)&&(outofs<SamplePerFrame)){
    result=madusr_decode_exec(&StaticMadDecoder);
//    struct mad_frame *frame = &StaticMadDecoder.sync->frame;
//    _consolePrintf("%d\n",frame->header.bitrate);
  }
  
  if(result!=madusr_ExecReturn_Next){
    if(result==madusr_ExecReturn_End){
//      _consolePrint("ExecReturn_End\n");
      return(0);
    }
    if(result==madusr_ExecReturn_Fail){
//      _consolePrint("ExecReturn_Fail\n");
      return(0);
    }
  }
  
  return(SamplePerFrame);
}

s32 PlugMP3_GetPosMax(void)
{
  return(FileSize);
}

s32 PlugMP3_GetPosOffset(void)
{
  return(FileOffset);
}

void PlugMP3_SetPosOffset(s32 ofs)
{
  if(FileHandle==NULL) return;
  
  if(FileSize<=ofs) ofs=FileSize-1;
  if(ofs<0) ofs=0;
  
  ofs&=~1; // 16bit align
  
  FileOffset=ofs;
  FAT2_fseek(FileHandle,FileOffset,SEEK_SET);
  
  struct mad_stream *stream=&StaticMadDecoder.sync->stream;
  
  size_t ReadSize,Remaining;
  u8 *ReadStart;
  
  ReadSize=INPUT_BUFFER_SIZE;
  ReadStart=InputBuffer;
  Remaining=0;
  
  if((FileOffset+ReadSize)>=FileSize){
    ReadSize=FileSize-FileOffset;
  }
  
  if(0<ReadSize){
    FAT2_fread(ReadStart,1,ReadSize,FileHandle);
    FileOffset+=ReadSize;
    mad_stream_buffer(stream,InputBuffer,ReadSize+Remaining);
  }
}

u32 PlugMP3_GetChannelCount(void)
{
  struct mad_frame *frame = &StaticMadDecoder.sync->frame;
  
  switch(frame->header.mode){
    case MAD_MODE_SINGLE_CHANNEL:
      return(1);
      break;
    case MAD_MODE_DUAL_CHANNEL:
      return(2);
      break;
    case MAD_MODE_JOINT_STEREO:
      return(2);
      break;
    case MAD_MODE_STEREO:
      return(2);
      break;
    default:
      return(1);
      break;
  }
}

u32 PlugMP3_GetSampleRate(void)
{
  struct mad_frame *frame = &StaticMadDecoder.sync->frame;
  return(frame->header.samplerate);
}

u32 PlugMP3_GetSamplePerFrame(void)
{
  return(SamplePerFrame);
}

u32 PlugMP3_GetBitRate(void)
{
  struct mad_frame *frame = &StaticMadDecoder.sync->frame;
  return(frame->header.bitrate);
}

int PlugMP3_GetInfoIndexCount(void)
{
  return(0);
}

bool PlugMP3_GetInfoStrL(int idx,char *str,int len)
{
  return(false);
/*
  switch(idx){
    case 0: snprintf(str,len,"Title: %s",ID3Tag.title); return(true); break;
    case 1: snprintf(str,len,"Artist: %s",ID3Tag.artist); return(true); break;
    case 2: snprintf(str,len,"Album: %s",ID3Tag.album); return(true); break;
    case 3: snprintf(str,len,"Comment: %s",ID3Tag.comment); return(true); break;
    case 4: snprintf(str,len,"Year: %s   Genre: %s",ID3Tag.year,GetGenreStr(ID3Tag.genre)); return(true); break;
  }
*/
}

bool PlugMP3_GetInfoStrW(int idx,UnicodeChar *str,int len)
{
  return(false);
}

bool PlugMP3_GetInfoStrUTF8(int idx,char *str,int len)
{
  return(false);
}

bool PlugMP3_ExistsID3Tag(void)
{
  return(ID3Tag.Enabled);
}

const char* PlugMP3_ID3Tag_GetTitle(void)
{
  return(ID3Tag.title);
}

const char* PlugMP3_ID3Tag_GetArtist(void)
{
  return(ID3Tag.artist);
}

const char* PlugMP3_ID3Tag_GetAlbum(void)
{
  return(ID3Tag.album);
}

const char* PlugMP3_ID3Tag_GetComment(void)
{
  return(ID3Tag.comment);
}
