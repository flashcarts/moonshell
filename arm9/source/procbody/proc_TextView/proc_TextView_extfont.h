
#define ExtFontCount (0x10000)

typedef struct {
  u16 Color;
  u32 Height;
  u8 Widths[ExtFontCount];
  u8 *pBody;
  u8 **ppBodyLink;
  bool UseClearTypeFont;
  FAT_FILE *pClearTypeFontFileHandle;
  CCTF *pCTF;
} TExtFont;

static TExtFont *pExtFont;

static void ExtFont_Init(void)
{
  pExtFont=(TExtFont*)safemalloc(sizeof(TExtFont));
  if(pExtFont==NULL){
    _consolePrintf("Fatal error: ExtFont_Init: Memory overflow.\n");
    ShowLogHalt();
  }
  
  pExtFont->Color=RGB15(0,0,0)|BIT15;
  pExtFont->Height=ProcState.Text.FontSize;
  // MemSet32CPU(0,pExtFont->Widths,0x10000);
  pExtFont->pBody=NULL;
  pExtFont->ppBodyLink=NULL;
  pExtFont->UseClearTypeFont=false;
  pExtFont->pClearTypeFontFileHandle=NULL;
  pExtFont->pCTF=NULL;
  
  pExtFont->UseClearTypeFont=ProcState.Text.ClearTypeFont;
  
  if(pExtFont->UseClearTypeFont==false){
    char fn[32];
    snprintf(fn,32,FontWidthFilenameFormat,pExtFont->Height);
    
    FAT_FILE *pf=Shell_FAT_fopen_TextFont(fn);
    if(pf==NULL){
      _consolePrintf("Fatal error: ExtFont: Not found font width file.\n");
      ShowLogHalt();
    }
    
    FAT2_fread_fast(pExtFont->Widths,1,ExtFontCount,pf);
    for(int idx=0;idx<ExtFontCount;idx++){
      if(pExtFont->Widths[idx]!=0) pExtFont->Widths[idx]++;
    }
    
    FAT2_fclose(pf);
    
    }else{ // for ClearTypeFont
    char fn[32];
    snprintf(fn,32,FontClearTypeFilenameFormat,pExtFont->Height);
    
    pExtFont->pClearTypeFontFileHandle=Shell_FAT_fopen_TextFont(fn);
    if(pExtFont->pClearTypeFontFileHandle==NULL){
      _consolePrintf("Fatal error: ExtFont: Not found clear type font file.\n");
      ShowLogHalt();
    }
    
    CCTF *pCTF=new CCTF(pExtFont->pClearTypeFontFileHandle,pExtFont->Height);
    pCTF->GetWidthsList(pExtFont->Widths);
    delete pCTF; pCTF=NULL;
  }
}

static void ExtFont_Free(void)
{
  if(pExtFont!=NULL){  
    if(pExtFont->pBody!=NULL){
      safefree(pExtFont->pBody); pExtFont->pBody=NULL;
    }
    if(pExtFont->ppBodyLink!=NULL){
      safefree(pExtFont->ppBodyLink); pExtFont->ppBodyLink=NULL;
    }
    if(pExtFont->pClearTypeFontFileHandle!=NULL){
      FAT2_fclose(pExtFont->pClearTypeFontFileHandle); pExtFont->pClearTypeFontFileHandle=NULL;
    }
    if(pExtFont->pCTF!=NULL){
      delete pExtFont->pCTF; pExtFont->pCTF=NULL;
    }
    
    safefree(pExtFont); pExtFont=NULL;
  }
}

static void ExtFont_LoadBody(void)
{
  if(pExtFont->UseClearTypeFont==true){
    FAT2_fseek(pExtFont->pClearTypeFontFileHandle,0,SEEK_SET);
    pExtFont->pCTF=new CCTF(pExtFont->pClearTypeFontFileHandle,pExtFont->Height);
    
    CCTF *pCTF=pExtFont->pCTF;
    pCTF->SetTargetCanvas(NULL);
    
    return;
  }
  
  char fn[32];
  snprintf(fn,32,FontGlyphFilenameFormat,pExtFont->Height);
  
  FAT_FILE *pf=Shell_FAT_fopen_TextFont(fn);
  if(pf==NULL){
    _consolePrintf("Fatal error: ExtFont_LoadBody: Not found font glyph file.\n");
    ShowLogHalt();
  }
  
  pExtFont->ppBodyLink=(u8**)safemalloc(sizeof(u8**)*ExtFontCount);
  
  u32 BodySize=FAT2_GetFileSize(pf)-ExtFontCount;
  pExtFont->pBody=(u8*)safemalloc(BodySize);
  
  u8 *pDiffTbl=(u8*)safemalloc(ExtFontCount);
  
  FAT2_fread_fast(pDiffTbl,1,ExtFontCount,pf);
  
  FAT2_fread_fast(pExtFont->pBody,1,BodySize,pf);
  
  u32 LastOffset=0;
  
  for(int idx=0;idx<ExtFontCount;idx++){
    if(pDiffTbl[idx]==0){
      pExtFont->ppBodyLink[idx]=NULL;
      }else{
      LastOffset+=pDiffTbl[idx];
      pExtFont->ppBodyLink[idx]=&pExtFont->pBody[LastOffset];
    }
  }
  
  if(pDiffTbl!=NULL){
    safefree(pDiffTbl); pDiffTbl=NULL;
  }
  
  FAT2_fclose(pf);
}

asm void ExtFont_DrawFont1bpp16pix(const u8 *pBulkData,u16 *pbuf,u32 Height,u32 TextColor)
{
efdf1b16_pBulkData RN r0
efdf1b16_pbuf RN r1
efdf1b16_Height RN r2
efdf1b16_TextColor RN r3
efdf1b16_BitImage RN lr

  PUSH {lr}
  
efdf1b16_DrawFont1bpp_LoopYStart

  ldrh efdf1b16_BitImage,[efdf1b16_pBulkData],#2
  
  tsts efdf1b16_BitImage,#1<<0
  strhne efdf1b16_TextColor,[efdf1b16_pbuf,#2*0]
  tsts efdf1b16_BitImage,#1<<1
  strhne efdf1b16_TextColor,[efdf1b16_pbuf,#2*1]
  tsts efdf1b16_BitImage,#1<<2
  strhne efdf1b16_TextColor,[efdf1b16_pbuf,#2*2]
  tsts efdf1b16_BitImage,#1<<3
  strhne efdf1b16_TextColor,[efdf1b16_pbuf,#2*3]
  tsts efdf1b16_BitImage,#1<<4
  strhne efdf1b16_TextColor,[efdf1b16_pbuf,#2*4]
  tsts efdf1b16_BitImage,#1<<5
  strhne efdf1b16_TextColor,[efdf1b16_pbuf,#2*5]
  tsts efdf1b16_BitImage,#1<<6
  strhne efdf1b16_TextColor,[efdf1b16_pbuf,#2*6]
  tsts efdf1b16_BitImage,#1<<7
  strhne efdf1b16_TextColor,[efdf1b16_pbuf,#2*7]
  tsts efdf1b16_BitImage,#1<<8
  strhne efdf1b16_TextColor,[efdf1b16_pbuf,#2*8]
  tsts efdf1b16_BitImage,#1<<9
  strhne efdf1b16_TextColor,[efdf1b16_pbuf,#2*9]
  tsts efdf1b16_BitImage,#1<<10
  strhne efdf1b16_TextColor,[efdf1b16_pbuf,#2*10]
  tsts efdf1b16_BitImage,#1<<11
  strhne efdf1b16_TextColor,[efdf1b16_pbuf,#2*11]
  tsts efdf1b16_BitImage,#1<<12
  strhne efdf1b16_TextColor,[efdf1b16_pbuf,#2*12]
  tsts efdf1b16_BitImage,#1<<13
  strhne efdf1b16_TextColor,[efdf1b16_pbuf,#2*13]
  tsts efdf1b16_BitImage,#1<<14
  strhne efdf1b16_TextColor,[efdf1b16_pbuf,#2*14]
  tsts efdf1b16_BitImage,#1<<15
  strhne efdf1b16_TextColor,[efdf1b16_pbuf,#2*15]
  
  add efdf1b16_pbuf,#ScreenWidth*2
  
  subs efdf1b16_Height,#1
  bne efdf1b16_DrawFont1bpp_LoopYStart
  
  POP {pc}
}

static void ExtFont_DrawFont(CglCanvas *pCanvas,const int x,const int y,UnicodeChar uidx,const u32 Height)
{
  const u8 *pBulkData=pExtFont->ppBodyLink[uidx];
  if(pBulkData==NULL) return;
  
  u16 *pbuf=pCanvas->GetScanLine(y);
  pbuf+=x;
  
  ExtFont_DrawFont1bpp16pix(pBulkData,pbuf,Height,pExtFont->Color);
}

static void ExtFont_TextOutW(CglCanvas *pCanvas,const int x,const int y,const UnicodeChar *pstr)
{
  if(pExtFont->UseClearTypeFont==true){
    CCTF *pCTF=pExtFont->pCTF;
    pCTF->SetTargetCanvas(pCanvas);
    pCTF->TextOutW(x,y,pstr);
    return;
  }
  
  const u32 Height=pExtFont->Height;
  
  int dx=x;
  int dy=y;
  
  const u32 CanvasWidth=ScreenWidth; // 横サイズ256ピクセル以外には描画できない。
  
  while(*pstr!=0){
    const u16 widx=*pstr++;
    const u32 w=pExtFont->Widths[widx];
    if(CanvasWidth<(dx+w)) return;
    ExtFont_DrawFont(pCanvas,dx,dy,widx,Height);
    dx+=w;
  }
}

