
typedef struct {
  void *pBinary;
  s32 BinarySize;
  const u8 *panktbl;
  const u16 *ps2utbl;
} TSJIS2Unicode;

static TSJIS2Unicode SJIS2Unicode;

static void SJIS2Unicode_Init(void)
{
  TSJIS2Unicode *ps2u=&SJIS2Unicode;
  
  ps2u->pBinary=NULL;
  ps2u->BinarySize=0;
  ps2u->panktbl=NULL;
  ps2u->ps2utbl=NULL;
}

static void SJIS2Unicode_Free(void)
{
  TSJIS2Unicode *ps2u=&SJIS2Unicode;
  
  if(ps2u->pBinary!=NULL){
    safefree(ps2u->pBinary); ps2u->pBinary=NULL;
  }
  
  SJIS2Unicode_Init();
}

static void SJIS2Unicode_Load(void)
{
  TSJIS2Unicode *ps2u=&SJIS2Unicode;
  
  if(ps2u->pBinary!=NULL) return;
  
  FAT_FILE *pf=Shell_FAT_fopen_SJISToUnicodeTable();
  if(pf==NULL){
    _consolePrintf("Fatal error: Not found S-JIS to Unicode convert table file.\n");
    ShowLogHalt();
  }
  
  ps2u->BinarySize=FAT2_GetFileSize(pf);
  ps2u->pBinary=safemalloc(ps2u->BinarySize);
  if(ps2u->pBinary==NULL){
    _consolePrintf("Fatal error: S-JIS to Unicode memory overflow.\n");
    ShowLogHalt();
  }
  FAT2_fread_fast(ps2u->pBinary,1,ps2u->BinarySize,pf);
  FAT2_fclose(pf);
  
  ps2u->panktbl=(const u8*)ps2u->pBinary;
  ps2u->ps2utbl=(const u16*)&ps2u->panktbl[256];
}

static void SJIS2Unicode_Convert(const char *pStrL,UnicodeChar *pStrW)
{
  TSJIS2Unicode *ps2u=&SJIS2Unicode;
  
  if((ps2u->panktbl==NULL)||(ps2u->ps2utbl==NULL)){
    _consolePrintf("Fatal error: SJIS2Unicode_Convert: Table not loaded.\n");
    ShowLogHalt();
  }
  
  u32 widx=0;
  
  while(pStrL[0]!=0){
    if(32<=widx) break;
    u32 c0=pStrL[0];
    u32 c1=pStrL[1];
    if(ps2u->panktbl[c0]==true){
      pStrW[widx++]=c0;
      pStrL+=1;
      }else{
      u32 sjis=(c0<<8)|c1;
      pStrW[widx++]=ps2u->ps2utbl[sjis];
      pStrL+=2;
    }
  }
  
  pStrW[widx]=0;
}

