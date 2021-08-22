
#include <stdio.h>

#include <NDS.h>

#include "_const.h"

#include "plugin.h"
#include "plugin_def.h"

#include "libgif/gif_lib.h"

// CallBack from plugin_dll.cpp

void cbLoadLibrary(void)
{
}

void cbFreeLibrary(void)
{
}

void cbQueryInterfaceLibrary(void)
{
}

// -----------------

typedef struct {
  u32 Palettes[256];
  u32 PalettesCount;
  u32 Width,Height;
} TGIFInfo;

TGIFInfo GIFInfo;
u8 *pBitmap=NULL;
u32 BitmapWidth=0;

// ------------------------------------------------------------------------------------

static void _PrintGifError(void)
{
  _consolePrintf("GIF error:%s\n",GifLastError());
}

static void _PrintGifErrorStr(char *str)
{
  _consolePrintf("GIF error:%s\n",str);
}

extern "C" {
extern int readFunc_FileHandle;
}

int readFunc_FileHandle=0;

static int readFunc(GifFileType* GifFile, GifByteType* buf, int count)
{
  return(fread(buf,1,count,readFunc_FileHandle));
}

static void MemSet8CPU(u8 v,void *dst,u32 len)
{
  len>>=0;
  if(len==0) return;
  
  u8 *_dst=(u8*)dst;
  
  for(u32 cnt=0;cnt<len;cnt++){
    _dst[cnt]=v;
  }
}

#define GAMMA(x)	(x)

bool LoadGIF(int FileHandle)
{
  readFunc_FileHandle=FileHandle;
  
  if(FileHandle==0){
    _PrintGifErrorStr("SourceData Null.");
    return(false);
  }
  
  {
    GifFileType *GifFile;
    
    _consolePrintf("OpenFile.\n");
    if ((GifFile = DGifOpen((void*)FileHandle, readFunc)) == NULL) {
      _PrintGifError();
      return false;
    }
    
    /* Scan the content of the GIF file and load the image(s) in: */
    GifRecordType RecordType;
    
    do {
      _consolePrintf("DGifGetRecordType\n");
      if (DGifGetRecordType(GifFile, &RecordType) == GIF_ERROR) {
        _PrintGifError();
        return false;
      }
      
      switch (RecordType) {
        case IMAGE_DESC_RECORD_TYPE:
          _consolePrintf("IMAGE_DESC_RECORD_TYPE\n");
          
          _consolePrintf("DGifGetImageDesc\n");
          if (DGifGetImageDesc(GifFile) == GIF_ERROR) {
            _PrintGifError();
            return false;
          }
          
          s32 Width=GifFile->Image.Width;
          s32 Height=GifFile->Image.Height;
          u32 PalettesCount;
          
/*
_consolePrintf("p:GifFile=%x\n",(u32)GifFile);
_consolePrintf("p:&Image.ColorMap=%x\n",&GifFile->Image.ColorMap);
_consolePrintf("p:&SColorMap=%x\n",&GifFile->SColorMap);
*/

          _consolePrintf("Update Color map\n",0);
          
          _consolePrintf("GifFile->Image.ColorMap = %x\n",(int)GifFile->Image.ColorMap);
          _consolePrintf("GifFile->SColorMap = %x\n",(int)GifFile->SColorMap);
          
          ColorMapObject *ColorMap = (GifFile->Image.ColorMap ? GifFile->Image.ColorMap : GifFile->SColorMap);
          
          PalettesCount=ColorMap->ColorCount;
          
          for(u32 idx=0;idx<256;idx++){
            GIFInfo.Palettes[idx]=0 | BIT15;
          }
          
          for(u32 idx=0;idx<PalettesCount;idx++){
            GifColorType* pColor = &ColorMap->Colors[idx];
            
            u32 pal=0;
            
            pal|=(GAMMA(pColor->Blue) & 0xff)<< 0;
            pal|=(GAMMA(pColor->Green) & 0xff) << 8;
            pal|=(GAMMA(pColor->Red) & 0xff) << 16;
            
            GIFInfo.Palettes[idx]=pal;
          }
          
          BitmapWidth=(Width+4+4) & ~3;
          
          _consolePrintf("malloc(%d);\n",BitmapWidth*Height);
          
          if(pBitmap!=NULL) safefree(pBitmap);
          pBitmap=(u8*)safemalloc(BitmapWidth*Height);
          if(pBitmap==NULL){
            _PrintGifErrorStr("out of memory.\n");
            return(false);
          }
          MemSet8CPU(GifFile->SBackGroundColor,pBitmap,BitmapWidth*Height);
          
          _consolePrintf("GetImage.\n");
          
          if (GifFile->Image.Interlace) {
            /* Need to perform 4 passes on the images: */
            const short InterlacedOffset[] = { 0, 4, 2, 1 }; /* The way Interlaced image should. */
            const short InterlacedJumps[] = { 8, 8, 4, 2 };    /* be read - offsets and jumps... */
            for (u32 InterlaceCount=0; InterlaceCount<4; InterlaceCount++){
              u32 prgdiv=Height/16;
              MWin_ProgressShow("Decode interlace image.",Height);
              for (s32 y = InterlacedOffset[InterlaceCount]; y < Height; y += InterlacedJumps[InterlaceCount]) {
                if(prgdiv!=0){
                  prgdiv--;
                  }else{
                  prgdiv=Height/16;
                  MWin_ProgressSetPos(y);
                }
                if (DGifGetLine(GifFile, &pBitmap[(Height-1-y)*BitmapWidth], Width) == GIF_ERROR) {
                  _PrintGifError();
                  return false;
                }
              }
              MWin_ProgressHide();
            }
            }else{
            u32 prgdiv=Height/16;
            MWin_ProgressShow("Decode image.",Height);
            for (s32 y = 0; y < Height; y++) {
              if(prgdiv!=0){
                prgdiv--;
                }else{
                prgdiv=Height/16;
                MWin_ProgressSetPos(y);
              }
              if (DGifGetLine(GifFile, &pBitmap[(Height-1-y)*BitmapWidth], Width) == GIF_ERROR) {
                _PrintGifError();
                return false;
              }
            }
            MWin_ProgressHide();
          }
          
          _consolePrintf("GIFInfo Update.\n");
          
          GIFInfo.Width=Width;
          GIFInfo.Height=Height;
          GIFInfo.PalettesCount=PalettesCount;
          break;
        case EXTENSION_RECORD_TYPE:
          _consolePrintf("EXTENSION_RECORD_TYPE\n");
          /* Skip any extension blocks in file: */
          GifByteType *Extension;
          int ExtCode;
          if (DGifGetExtension(GifFile, &ExtCode, &Extension) == GIF_ERROR) {
            _PrintGifError();
            return false;
          }
          while (Extension != NULL) {
            if (DGifGetExtensionNext(GifFile, &Extension) == GIF_ERROR) {
              _PrintGifError();
              return false;
            }
          }
          break;
        case TERMINATE_RECORD_TYPE:
          _consolePrintf("TERMINATE_RECORD_TYPE\n");
          break;
        default:      /* Should be traps by DGifGetRecordType. */
          _consolePrintf("unknown RECORD_TYPE\n");
          break;
      }
    }
    while ((RecordType != TERMINATE_RECORD_TYPE)&&(pBitmap==NULL));
    
    _consolePrintf("CloseFile.\n");
    /* Close file when done */
    if (DGifCloseFile(GifFile) == GIF_ERROR) {
      _PrintGifError();
      return false;
    }
  }
  
  if(pBitmap==NULL){
    return(false);
    }else{
    return(true);
  }
}

// ------------------------------------------------------------------------------------

extern void Free(void);

bool Start(int FileHandle)
{
  if(LoadGIF(FileHandle)==false){
    _consolePrintf("GIF LoadError.\n");
    Free();
    return(false);
  }
  
  _consolePrintf("pBitmap=0x%08x\n",(u32)pBitmap);
  _consolePrintf("PalettesCount=%d\n",GIFInfo.PalettesCount);
  _consolePrintf("Size=(%d,%d) %dpixel\n",GIFInfo.Width,GIFInfo.Height,GIFInfo.Width*GIFInfo.Height);
  
  return(true);
}

void Free(void)
{
  if(pBitmap!=NULL){
    safefree(pBitmap); pBitmap=NULL;
  }
}

typedef struct {
  s32 x,y,w,h;
} TRect;

void GetBitmap24(u32 LineY,u8 *pBM)
{
  u32 *PaletteTable=GIFInfo.Palettes;
  
  u8 *pSrcBM=&pBitmap[(GIFInfo.Height-LineY-1)*BitmapWidth];
  
  for(s32 x=0;x<GIFInfo.Width;x++){
    u32 pal=PaletteTable[pSrcBM[x]];
    
    pBM[0]=pal >> 16;
    pBM[1]=pal >> 8;
    pBM[2]=pal >> 0;
    pBM+=3;
  }
}

s32 GetWidth(void)
{
  return(GIFInfo.Width);
}

s32 GetHeight(void)
{
  return(GIFInfo.Height);
}

int GetInfoIndexCount(void)
{
  return(0);
}

bool GetInfoStrA(int idx,char *str,int len)
{
  return(false);
}

bool GetInfoStrW(int idx,UnicodeChar *str,int len)
{
  return(false);
}

bool GetInfoStrUTF8(int idx,char *str,int len)
{
  return(false);
}

