
#include <stdio.h>

#include <NDS.h>

#include "_const.h"

#include "_console.h"
#include "_consoleWriteLog.h"

#include "memtool.h"

#include "jpeglib/jpeglib.h"
#include "ErrorDialog.h"

#include "fat2.h"

static FAT_FILE *FileHandle;

static bool cinfoInit;
static struct jpeg_decompress_struct cinfo;

static int MasterImageWidth,MasterImageHeight;
static int ScalingFactor;

static int imgWidth;
static int imgHeight;
static int imgBits;

static int imgCmpWidth;
static int imgCmpHeight;
static u8 *imgCmpBuf=NULL;

static struct jpeg_error_mgr jerr;

static u32 LastOffsetY;

// ------------------------------------------------------------------------------------

static bool JpegGetMasterImageSize(FAT_FILE *FileHandle)
{
  MemSet32CPU(0,&cinfo,sizeof(struct jpeg_decompress_struct));
  MemSet32CPU(0,&jerr,sizeof(struct jpeg_error_mgr));
  
  FAT2_fseek(FileHandle,0,SEEK_SET);
  
  // エラーのハンドリング
  cinfo.err = jpeg_std_error(&jerr);

  // 以降の jpeg ライブラリ内でエラーが生じた場合、資源を開放して終わる。

  // 構造体の初期設定
  jpeg_create_decompress(&cinfo);
  
  // ファイル入力ハンドルの設定
  jpeg_stdio_src(&cinfo, (FILE*)FileHandle);

  // ファイルの情報ヘッダの読込み
  jpeg_read_header(&cinfo, TRUE);
  
  if(cinfo.progressive_mode==TRUE){
    _consolePrintf("Progressive JPEG is uncorrespondence!!\n");
    ErrorDialog_Set(EEC_ProgressiveJpeg);
    jpeg_destroy_decompress(&cinfo);
    cinfoInit=false;
    return(false);
  }

  jpeg_start_decompress(&cinfo);

  MasterImageWidth=cinfo.image_width;
  MasterImageHeight=cinfo.image_height;
  
  // 即解放
  jpeg_finish_decompress(&cinfo);
  jpeg_destroy_decompress(&cinfo);
  
  _consolePrintf("JpegGetMasterImageSize: free.\n");
  return(true);
}

static bool JpegStart(FAT_FILE *FileHandle)
{
  MemSet32CPU(0,&cinfo,sizeof(struct jpeg_decompress_struct));
  MemSet32CPU(0,&jerr,sizeof(struct jpeg_error_mgr));
  
  FAT2_fseek(FileHandle,0,SEEK_SET);
  
  // エラーのハンドリング
  cinfo.err = jpeg_std_error(&jerr);

  // 以降の jpeg ライブラリ内でエラーが生じた場合、資源を開放して終わる。

  // 構造体の初期設定
  jpeg_create_decompress(&cinfo);
  
  // ファイル入力ハンドルの設定
  jpeg_stdio_src(&cinfo, (FILE*)FileHandle);

  // ファイルの情報ヘッダの読込み
  jpeg_read_header(&cinfo, TRUE);
  
  cinfo.scale_num=1;
  cinfo.scale_denom=ScalingFactor;
  
  if(cinfo.progressive_mode==TRUE){
    _consolePrintf("Progressive JPEG is uncorrespondence!!\n");
    ErrorDialog_Set(EEC_ProgressiveJpeg);
    jpeg_destroy_decompress(&cinfo);
    cinfoInit=false;
    return(false);
  }

  // 解凍の開始
  jpeg_start_decompress(&cinfo);

  imgWidth=cinfo.image_width/ScalingFactor;
  imgHeight=cinfo.image_height/ScalingFactor;
  
  imgBits=cinfo.output_components*8;
  
  imgCmpWidth=cinfo.output_width;
  imgCmpHeight=cinfo.output_height;
  
  imgCmpBuf=(u8*)safemalloc(imgCmpWidth*4);
  
  if(imgCmpBuf==NULL){
    _consolePrintf("can not allocate DecompressBuffer. out of memory. (imgCmpWidth=%d)\n",imgCmpWidth);
    ErrorDialog_Set(EEC_MemoryOverflow_CanRecovery);
    jpeg_finish_decompress(&cinfo);
    jpeg_destroy_decompress(&cinfo);
    cinfoInit=false;
    return(false);
  }
  
  cinfoInit=true;
  
  _consolePrint("Jpeg decoder initialized.\n");
  
  return(true);
}

static bool JpegNextLine(u8 *dstbuf)
{
  if(cinfo.output_height<=cinfo.output_scanline){
    _consolePrintf("out of scanline. %d<=%d\n",cinfo.output_height,cinfo.output_scanline);
    return(false);
  }
  
  int res=jpeg_read_scanlines(&cinfo, &imgCmpBuf, 1);
  
  if(res!=1){
    _consolePrintf("error:jpeg_read_scanlines();\n");
    return(false);
  }
  
  u8 *src=imgCmpBuf;
  
  if(imgBits==8){
    for(int x=imgWidth;x!=0;x--){
      u32 b=*src++;
      *dstbuf++=b;
      *dstbuf++=b;
      *dstbuf++=b;
    }
    }else{
    u32 size=imgWidth*3;
    u32 wordsize=size&~3;
    MemCopy32CPU(src,dstbuf,wordsize);
    if((size&3)!=0){
      src+=wordsize;
      dstbuf+=wordsize;
      size&=3;
      while(size!=0){
        *dstbuf++=*src++;
        size--;
      }
    }
  }
  
  return(true);
}

static void JpegFree(void)
{
  if(cinfoInit==true){
    jpeg_finish_decompress(&cinfo);
    jpeg_destroy_decompress(&cinfo);
    cinfoInit=false;
  }
  
  if(imgCmpBuf!=NULL){
    safefree(imgCmpBuf); imgCmpBuf=NULL;
  }
}

// ------------------------------------------------------------------------------------

bool PlugJpeg_Start(FAT_FILE *_FileHandle,bool EnabledAutoFitting)
{
  cinfoInit=false;
  
  if(_FileHandle==0){
    _consolePrintf("FileHandle is NULL\n");
    return(false);
  }
  
  FileHandle=_FileHandle;
  
  _consolePrintf("\n");
  _consolePrintf("The Independent JPEG Groups JPEG software\n");
  _consolePrintf("Copyright (C) 1994-1998, Thomas G. Lane.\n");
  _consolePrintf("jpeglib version 6b of 27-Mar-1998.\n");
  _consolePrintf("\n");
  
  if(JpegGetMasterImageSize(FileHandle)==false){
    ErrorDialog_Set(EEC_NotSupportFileFormat);
    return(false);
  }
  
  if(EnabledAutoFitting==false){
    ScalingFactor=1;
    }else{
    u32 limw=ScreenWidth*4,limh=ScreenHeight*4;
    u32 w,h;
    ScalingFactor=1;
    w=MasterImageWidth/ScalingFactor;
    h=MasterImageHeight/ScalingFactor;
    if((limw<w)||(limh<h)){
      ScalingFactor=2;
      w=MasterImageWidth/ScalingFactor;
      h=MasterImageHeight/ScalingFactor;
      if((limw<w)||(limh<h)){
        ScalingFactor=4;
        w=MasterImageWidth/ScalingFactor;
        h=MasterImageHeight/ScalingFactor;
        if((limw<w)||(limh<h)){
          ScalingFactor=8;
        }
      }
    }
  }
  
  
  if(JpegStart(FileHandle)==false){
    ErrorDialog_Set(EEC_NotSupportFileFormat);
    return(false);
  }
  
  LastOffsetY=(u32)-1;
  
  return(true);
}

void PlugJpeg_Free(void)
{
  FileHandle=0;
  
  JpegFree();
}

void PlugJpeg_GetBitmap24(u32 LineY,u8 *pBM)
{
  if(imgHeight<=LineY) return;
  
  if((LastOffsetY+1)!=LineY){
    _consolePrintf("Fatal error: Jpeg decode: Not support seek function. (%d,%d)\n",LastOffsetY,LineY);
    ShowLogHalt();
  }
  LastOffsetY=LineY;
  
  if(JpegNextLine(pBM)==false){
    _consolePrintf("Fatal error: Jpeg decode: Decode error.\n");
    ShowLogHalt();
  }
}

s32 PlugJpeg_GetWidth(void)
{
  return(imgWidth);
}

s32 PlugJpeg_GetHeight(void)
{
  return(imgHeight);
}

int PlugJpeg_GetInfoIndexCount(void)
{
  return(2);
}

bool PlugJpeg_GetInfoStrA(int idx,char *str,int len)
{
  switch(idx){
    case 0: {
      if(ScalingFactor==1){
        snprintf(str,len,"");
        }else{
        snprintf(str,len,"Scaling factor= 1 / %d (auto fitting)",ScalingFactor);
      }
      return(true);
    } break;
    case 1: snprintf(str,len,"Image size= %dx%dx%dbitColor",MasterImageWidth,MasterImageHeight,imgBits); return(true); break;
  }
  return(false);
}

bool PlugJpeg_GetInfoStrW(int idx,UnicodeChar *str,int len)
{
  return(false);
}

bool PlugJpeg_GetInfoStrUTF8(int idx,char *str,int len)
{
  return(false);
}

