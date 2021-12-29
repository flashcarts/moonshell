
#include <NDS.h>

#include "_console.h"
#include "_consoleWriteLog.h"
#include "_const.h"
#include "../../ipc6.h"
#include "arm9tcm.h"

#include "cctf.h"

#include "strtool.h"
#include "memtool.h"
#include "shell.h"

#include "cctf_dfs.h"

CCTF::CCTF(FAT_FILE *pFileHandle,u32 _FontHeight)
{
  _consolePrintf("CTF: Create class.\n");
  
  pcan=NULL;
  pIndexTable=NULL;
  pWidthsTable=NULL;
  FontHeight=_FontHeight;
  
  u32 DataOffset,DataSize;
  
  {
    DataOffset=0;
    DataSize=FAT2_GetFileSize(pFileHandle);
    
    FAT2_fseek(pFileHandle,DataOffset,SEEK_SET);
    CTF_DFS_Init(pFileHandle);
    CTF_DFS_SetAttribute(DataOffset,DataSize);
  }
  
  _consolePrintf("CTF: Check header ID.\n");
  char *pmstheader="MoonShell2 ClearTypeFont V3\0\3\0\0\0";
  char header[32];
  CTF_DFS_Read32bit(header,32);
  if(isStrEqual(pmstheader,header)==false){
    _consolePrintf("Fatal error: CTF: Illigal header ID. ['%s'!='%s']\n",pmstheader,header);
    ShowLogHalt();
  }
  
  _consolePrintf("CTF: Load index table.\n");
  pIndexTable=(u32*)safemalloc(0x10000*4);
  if(pIndexTable==NULL){
    _consolePrintf("Fatal error: CTF: Index table memory overflow.\n");
    ShowLogHalt();
  }
  CTF_DFS_Read32bit(pIndexTable,0x10000*4);
  
  _consolePrintf("CTF: Load widths table.\n");
  pWidthsTable=(u8*)safemalloc(0x10000*1);
  if(pWidthsTable==NULL){
    _consolePrintf("Fatal error: CTF: Widths table memory overflow.\n");
    ShowLogHalt();
  }
  CTF_DFS_Read32bit(pWidthsTable,0x10000*1);
  
  {
    u32 headersize=32+(0x10000*4)+(0x10000*1);
    CTF_DFS_SetAttribute(DataOffset+headersize,DataSize-DataOffset-headersize);
  }
  
  if(IPC6->isNDSLite==false){
    _consolePrintf("CTF: Display format is RGB for DS.\n");
    DisplayFormat=ECTFDF_RGB;
    }else{
    _consolePrintf("CTF: Display format is BGR for DSL.\n");
    DisplayFormat=ECTFDF_BGR;
  }
  
  _consolePrintf("CTF: Clear font cache.\n");
  for(u32 idx=0;idx<CTF_FontCacheMax;idx++){
    FontCache.uidxs[idx]=0;
  }
  FontCacheNextWriteIndex=0;
  
  _consolePrintf("CTF: Initialized.\n");
}

CCTF::~CCTF(void)
{
  CTF_DFS_Free();
  
  if(pIndexTable!=NULL){
    safefree(pIndexTable); pIndexTable=NULL;
  }
  
  if(pWidthsTable!=NULL){
    safefree(pWidthsTable); pWidthsTable=NULL;
  }
}

void CCTF::SetTargetCanvas(CglCanvas *_pcan)
{
  pcan=_pcan;
}

u32 CCTF::GetCharOffset(UnicodeChar wch) const
{
  return(pIndexTable[wch]&0xffffff);
}

u32 CCTF::GetCharDataSize(UnicodeChar wch) const
{
  return((pIndexTable[wch]>>24)&0xff);
}

u32 CCTF::GetCharWidth(UnicodeChar wch) const
{
  return(pWidthsTable[wch]);
}

void CCTF::GetWidthsList(u8 *pWidthsList) const
{
  for(u32 idx=0;idx<0x10000;idx++){
    int w=pWidthsTable[idx];
    w-=CTF_FontPadding;
    if(w<0) w=0;
    pWidthsList[idx]=w;
  }
}

const u16* CCTF::GetFontData(UnicodeChar wch)
{
  {
    const u32 *puidxs=(const u32*)FontCache.uidxs;
    for(u32 idx=0;idx<CTF_FontCacheMax/2;idx++){
      u32 data32=*puidxs++;
      if((data32&0xffff)==wch) return(FontCache.datas[idx*2+0]);
      if((data32>>16)==wch) return(FontCache.datas[idx*2+1]);
    }
  }
  
  u32 idx=FontCacheNextWriteIndex;
  FontCacheNextWriteIndex++;
  if(FontCacheNextWriteIndex==CTF_FontCacheMax) FontCacheNextWriteIndex=0;
  
  FontCache.uidxs[idx]=wch;
  
  const u32 data=pIndexTable[wch];
  const u32 ofs=data&0xffffff;
  const u32 size=data>>24;
  
  CTF_DFS_SetOffset(ofs);
  CTF_DFS_Read32bit(FontCache.datas[idx],size);
  
  return(FontCache.datas[idx]);
}

void CCTF::DrawFont_TextBlack(CglCanvas *pcan,int x,int y,UnicodeChar wch)
{
  const u32 fontwidth=pWidthsTable[wch];
  
  const u16 *pfontdata=GetFontData(wch);
  
  u16 *pdstbuf=&pcan->GetScanLine(y)[x];
  u32 bufstride=pcan->GetWidth();
  
  // Gamma for white text
  //u32 iaval[7]={0x100-0x00, 0x100-0x48, 0x100-0x70, 0x100-0x98, 0x100-0xc0, 0x100-0xe0, 0x100-0x100};
  // Gamma for black text
  u32 iaval[7]={0x100-0x00, 0x100-0x20, 0x100-0x40, 0x100-0x68+8, 0x100-0x8c, 0x100-0xb8+8, 0x100-0x100};
  
  const u32 tr=0x00;
  const u32 tg=0x00;
  const u32 tb=0x00;
  
  switch(DisplayFormat){
    case ECTFDF_RGB: {
      u32 curbits=0;
      u32 curbitscount=0;
      for(u32 dy=0;dy<FontHeight;dy++){
        for(u32 dx=0;dx<fontwidth;dx++){
          const u32 needbitscount=9;
          if(curbitscount<needbitscount){
            curbits|=(*pfontdata++)<<curbitscount;
            curbitscount+=16;
          }
          if((curbits&7)==7){
            // skip alias (ar==0)&&(ag==0)&&(ab==0).
            curbits>>=3;
            curbitscount-=3;
            }else{
            u16 srcrgb=pdstbuf[dx];
            u32 sr=(srcrgb>>0)&0x1f;
            u32 sg=(srcrgb>>5)&0x1f;
            u32 sb=(srcrgb>>10)&0x1f;
            u32 ar=(curbits>>0)&0x07;
            u32 ag=(curbits>>3)&0x07;
            u32 ab=(curbits>>6)&0x07;
            
            curbits>>=needbitscount;
            curbitscount-=needbitscount;
            
            u32 iar=iaval[ar];
            if(tr==0x00){
              sr=(sr*iar)>>8;
              }else{
              sr=((sr*iar)+(tr*(0x100-iar)))>>8;
            }
            u32 iag=iaval[ag];
            if(tg==0x00){
              sg=(sg*iag)>>8;
              }else{
              sg=((sg*iag)+(tg*(0x100-iag)))>>8;
            }
            u32 iab=iaval[ab];
            if(tb==0x00){
              sb=(sb*iab)>>8;
              }else{
              sb=((sb*iab)+(tb*(0x100-iab)))>>8;
            }
            pdstbuf[dx]=RGB15(sr,sg,sb)|BIT15;
          }
        }
        pdstbuf+=bufstride;
      }
    } break;
    case ECTFDF_BGR: {
      u32 curbits=0;
      u32 curbitscount=0;
      for(u32 dy=0;dy<FontHeight;dy++){
        for(u32 dx=0;dx<fontwidth;dx++){
          const u32 needbitscount=9;
          if(curbitscount<needbitscount){
            curbits|=(*pfontdata++)<<curbitscount;
            curbitscount+=16;
          }
          if((curbits&7)==7){
            // skip alias (ar==0)&&(ag==0)&&(ab==0).
            curbits>>=3;
            curbitscount-=3;
            }else{
            u16 srcrgb=pdstbuf[dx];
            u32 sr=(srcrgb>>0)&0x1f;
            u32 sg=(srcrgb>>5)&0x1f;
            u32 sb=(srcrgb>>10)&0x1f;
            u32 ab=(curbits>>0)&0x07;
            u32 ag=(curbits>>3)&0x07;
            u32 ar=(curbits>>6)&0x07;
            
            curbits>>=needbitscount;
            curbitscount-=needbitscount;
            
            u32 iar=iaval[ar];
            if(tr==0x00){
              sr=(sr*iar)>>8;
              }else{
              sr=((sr*iar)+(tr*(0x100-iar)))>>8;
            }
            u32 iag=iaval[ag];
            if(tg==0x00){
              sg=(sg*iag)>>8;
              }else{
              sg=((sg*iag)+(tg*(0x100-iag)))>>8;
            }
            u32 iab=iaval[ab];
            if(tb==0x00){
              sb=(sb*iab)>>8;
              }else{
              sb=((sb*iab)+(tb*(0x100-iab)))>>8;
            }
            pdstbuf[dx]=RGB15(sr,sg,sb)|BIT15;
          }
        }
        pdstbuf+=bufstride;
      }
    } break;
  }
  
}

static DATA_IN_DTCM u32 coltbl9[8*8*8]={0,};

static void make_coltbl9(ECTF_DisplayFormat DisplayFormat)
{
  // Gamma for white text
  //u32 iaval[7]={0x100-0x00, 0x100-0x48, 0x100-0x70, 0x100-0x98, 0x100-0xc0, 0x100-0xe0, 0x100-0x100};
  // Gamma for black text
  u32 iaval[7]={0x100-0x00, 0x100-0x20, 0x100-0x40, 0x100-0x68+8, 0x100-0x8c, 0x100-0xb8+8, 0x100-0x100};
  
  u8 coltbl3[7];
  for(u32 a=0;a<7;a++){
    u32 ia=iaval[a];
    coltbl3[a]=(31*ia)>>8;
  }
  
  switch(DisplayFormat){
    case ECTFDF_RGB: {
      for(u32 c1=0;c1<7;c1++){
        for(u32 c2=0;c2<7;c2++){
          for(u32 c3=0;c3<7;c3++){
            coltbl9[(c1*8*8)|(c2*8)|c3]=RGB15(coltbl3[c3],coltbl3[c2],coltbl3[c1])|BIT15;
          }
        }
      }
    } break;
    case ECTFDF_BGR: {
      for(u32 c1=0;c1<7;c1++){
        for(u32 c2=0;c2<7;c2++){
          for(u32 c3=0;c3<7;c3++){
            coltbl9[(c1*8*8)|(c2*8)|c3]=RGB15(coltbl3[c1],coltbl3[c2],coltbl3[c3])|BIT15;
          }
        }
      }
    } break;
  }
}

void CCTF::DrawFont_Fast(CglCanvas *pcan,int x,int y,UnicodeChar wch)
{
  // �����F���T�C�Y256pixels, �w�i�F0xffff, �����F0x0000.
  
  const u32 fontwidth=pWidthsTable[wch];
  
  const u16 *pfontdata=GetFontData(wch);
  
  u16 *pdstbuf=&pcan->GetScanLine(y)[x];
  
  if(coltbl9[0]==0) make_coltbl9(this->DisplayFormat);
  
  u32 curbits=*(u32*)pfontdata;
  pfontdata+=2;
  u32 curbitscount=32;
  
  for(u32 dy=0;dy<FontHeight;dy++){
    for(u32 dx=0;dx<fontwidth;dx++){
      const u32 needbitscount=9;
      if(curbitscount<needbitscount){
        curbits|=(*pfontdata++)<<curbitscount;
        curbitscount+=16;
      }
      if((curbits&7)==7){
        // skip alias (ar==0)&&(ag==0)&&(ab==0).
        curbits>>=3;
        curbitscount-=3;
        }else{
        u32 data=curbits&0x1ff;
        pdstbuf[dx]=coltbl9[data];
        
        curbits>>=needbitscount;
        curbitscount-=needbitscount;
      }
    }
    pdstbuf+=256;
  }
}

extern "C" {
void CTF_DrawFont_Fast2_asm(u32 *pcoltbl9,const u16 *pfontdata,u32 fontwidth,u16 *pdstbuf);
}

void CCTF::DrawFont_Fast2(CglCanvas *pcan,int x,int y,UnicodeChar wch)
{
  // �����F���T�C�Y256pixels, �w�i�F0xffff, �����F0x0000.
  
  _consolePrintf("Fatal error: Deleted CCTF::DrawFont_Fast2 function.\n");
  ShowLogHalt();
  
#if 0
  const u32 data=pIndexTable[wch];
  const u32 fontwidth=pWidthsTable[wch];
  
  const u16 *pfontdata=GetFontData(wch);
  
  u16 *pdstbuf=&pcan->GetScanLine(y)[x];
  
  if(coltbl9[0]==0) make_coltbl9(this->DisplayFormat);
  
  CTF_DrawFont_Fast2_asm(coltbl9,pfontdata,fontwidth,pdstbuf);
#endif
}

void CCTF::TextOutA(const int x,const int y,const char *str)
{
  if((str==NULL)||(*str==0)) return;
  
  int canw=pcan->GetWidth();
  int dx=x,dy=y;
  
  while(*str!=0){
    UnicodeChar wch=*str++;
    int w=GetCharWidth(wch);
    if(w!=0){
      if(canw<(dx+w)) break;
      DrawFont_TextBlack(pcan,dx,dy,wch);
      w-=CTF_FontPadding;
      if(w<0) w=0;
      dx+=w;
    }
  }
}

void CCTF::TextOutW(const int x,const int y,const UnicodeChar *str)
{
  if((str==NULL)||(*str==0)) return;
  
  int canw=pcan->GetWidth();
  int dx=x,dy=y;
  
  if((CTF_FontPadding==1)&&(canw==256)){
    // �t�H���g���d�Ȃ�Ɣ��w�i�œK�����w�ɂȂ�̂�CTF_FontPadding��1�����Ή��B
    while(*str!=0){
      UnicodeChar wch=*str++;
      int w=GetCharWidth(wch);
      if(w!=0){
        if(canw<(dx+w)) break;
        DrawFont_Fast(pcan,dx,dy,wch);
        w-=CTF_FontPadding;
        if(w<0) w=0;
        dx+=w;
      }
    }
    return;
  }
  
  while(*str!=0){
    UnicodeChar wch=*str++;
    int w=GetCharWidth(wch);
    if(w!=0){
      if(canw<(dx+w)) break;
      DrawFont_TextBlack(pcan,dx,dy,wch);
      w-=CTF_FontPadding;
      if(w<0) w=0;
      dx+=w;
    }
  }
}

void CCTF::TextOutUTF8(const int x,const int y,const char *str)
{
  if((str==NULL)||(*str==0)) return;
  
  u32 strlen=0;
  while(str[strlen]!=0) strlen++;
  
  UnicodeChar *unistr=(UnicodeChar*)safemalloc((strlen+1)*2);
  
  StrConvert_UTF82Unicode(str,unistr);
  
  TextOutW(x,y,unistr);
  
  safefree(unistr); unistr=NULL;
}

int CCTF::GetTextWidthA(const char *str) const
{
  if((str==NULL)||(*str==0)) return(0);
  
  int width=0;
  
  while(*str!=0){
    UnicodeChar wch=*str++;
    int w=GetCharWidth(wch);
    if(w!=0) width+=w-CTF_FontPadding;
  }
  
  width+=CTF_FontPadding;
  
  return(width);
}

int CCTF::GetTextWidthW(const UnicodeChar *str) const
{
  if((str==NULL)||(*str==0)) return(0);
  
  int width=0;
  
  while(*str!=0){
    UnicodeChar wch=*str++;
    int w=GetCharWidth(wch);
    if(w!=0) width+=w-CTF_FontPadding;
  }
  
  width+=CTF_FontPadding;
  
  return(width);
}

int CCTF::GetTextWidthUTF8(const char *str) const
{
  if((str==NULL)||(*str==0)) return(0);
  
  u32 strlen=0;
  while(str[strlen]!=0) strlen++;
  
  UnicodeChar *unistr=(UnicodeChar*)safemalloc((strlen+1)*2);
  
  StrConvert_UTF82Unicode(str,unistr);
  
  int width=GetTextWidthW(unistr);
  
  safefree(unistr); unistr=NULL;
  
  return(width);
}

