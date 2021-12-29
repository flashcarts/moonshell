
// -------------------------------------------------------

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

// -------------------------------------------------------

typedef struct {
  u32 TopOffset;
} TTextLine;

#define TextLinesMaxCount (0x20000)
static u32 TextLinesCount;
static TTextLine *pTextLines;

static u32 TotalCharsCount;

static u32 ReadSectorIndex;
static UnicodeChar *pReadBuf; // 512uc 2sectors.

// -------------------------------------------------------

enum EReturnCode {ERC_Unknown,ERC_CR,ERC_LF,ERC_CRLF,ERC_LFCR};

static EReturnCode ReturnCode;

#define CharCR (0x0d)
#define CharLF (0x0a)
#define CharLineEnd (0x00)

// -------------------------------------------------------

static const char *pEncodeID;

static u32 (*libconv_CheckErrorCharsCount)(u8 *pbuf,u32 bufsize,CglFont *pFont);
static void (*libconv_DetectReturnCode)(u8 *pbuf,u32 bufsize);
static void (*libconv_Convert)(u8 *pbuf,u32 bufsize,CglFont *pFont);

static void libconv_SelectEncode_NULL(void)
{
  libconv_CheckErrorCharsCount=NULL;
  libconv_DetectReturnCode=NULL;
  libconv_Convert=NULL;
}

#define DefaultBufSize (256*12)

#include "proc_TextView_libconv_convertbody.h"

#include "proc_TextView_libconv_ansi.h"
#include "proc_TextView_libconv_sjis.h"
#include "proc_TextView_libconv_utf16be.h"
#include "proc_TextView_libconv_utf16le.h"
#include "proc_TextView_libconv_utf8.h"

// -------------------------------------------------------

static void libconv_Init(void)
{
  SJIS2Unicode_Init();
  SJIS2Unicode_Load();
  
  TextLinesCount=0;
  pTextLines=(TTextLine*)safemalloc(sizeof(TTextLine)*TextLinesMaxCount);
  
  TotalCharsCount=0;
  
  ReturnCode=ERC_Unknown;
  
  pEncodeID=NULL;
  libconv_SelectEncode_NULL();
  
  ReadSectorIndex=(u32)-1;
  pReadBuf=NULL;
}

static void libconv_EndConvert(void)
{
  SJIS2Unicode_Free();
  libconv_SelectEncode_NULL();
  
  ReadSectorIndex=(u32)-1;
  pReadBuf=(UnicodeChar*)safemalloc(512*2); // 2sectors.
}

static void libconv_Free(void)
{
  TextLinesCount=0;
  
  if(pTextLines!=NULL){
    safefree(pTextLines); pTextLines=NULL;
  }
  
  if(pReadBuf!=NULL){
    safefree(pReadBuf); pReadBuf=NULL;
  }
}

static void libconv_AutoSelectEncode(u8 *pbuf,u32 bufsize)
{
  u32 errcnt_ANSI=0x7fffffff;
  u32 errcnt_SJIS=0x7fffffff;
  u32 errcnt_UTF16BE=0x7fffffff;
  u32 errcnt_UTF16LE=0x7fffffff;
  u32 errcnt_UTF8=0x7fffffff;
  
  TProcState_Text *ptxt=&ProcState.Text;
  
  if(ptxt->DetectCharCode_ANSI==true){
    libconv_SelectEncode_ANSI();
    errcnt_ANSI=libconv_CheckErrorCharsCount(pbuf,bufsize,pCglFontDefault);
    _consolePrintf("errcnt_ANSI= %d.\n",errcnt_ANSI);
    if(errcnt_ANSI==0) return;
    errcnt_ANSI++;
  }
  
  if(ptxt->DetectCharCode_SJIS==true){
    libconv_SelectEncode_SJIS();
    errcnt_SJIS=libconv_CheckErrorCharsCount(pbuf,bufsize,pCglFontDefault);
    _consolePrintf("errcnt_SJIS= %d.\n",errcnt_SJIS);
    if(errcnt_SJIS==0) return;
  }
  
  if(ptxt->DetectCharCode_UTF16BE==true){
    libconv_SelectEncode_UTF16BE();
    errcnt_UTF16BE=libconv_CheckErrorCharsCount(pbuf,bufsize,pCglFontDefault);
    _consolePrintf("errcnt_UTF16BE= %d.\n",errcnt_UTF16BE);
    if(errcnt_UTF16BE==0) return;
  }
  
  if(ptxt->DetectCharCode_UTF16LE==true){
    libconv_SelectEncode_UTF16LE();
    errcnt_UTF16LE=libconv_CheckErrorCharsCount(pbuf,bufsize,pCglFontDefault);
    _consolePrintf("errcnt_UTF16LE= %d.\n",errcnt_UTF16LE);
    if(errcnt_UTF16LE==0) return;
  }
  
  if(ptxt->DetectCharCode_UTF8==true){
    libconv_SelectEncode_UTF8();
    errcnt_UTF8=libconv_CheckErrorCharsCount(pbuf,bufsize,pCglFontDefault);
    _consolePrintf("errcnt_UTF8= %d.\n",errcnt_UTF8);
    if(errcnt_UTF8==0) return;
  }
  
  libconv_SelectEncode_UTF8(); // default.
  
  u32 errcnt=0x7fffffff;
  
  if(errcnt_ANSI<errcnt){
    errcnt=errcnt_ANSI;
    libconv_SelectEncode_ANSI();
  }
  
  if(errcnt_SJIS<errcnt){
    errcnt=errcnt_SJIS;
    libconv_SelectEncode_SJIS();
  }
  
  if(errcnt_UTF16BE<errcnt){
    errcnt=errcnt_UTF16BE;
    libconv_SelectEncode_UTF16BE();
  }
  
  if(errcnt_UTF16LE<errcnt){
    errcnt=errcnt_UTF16LE;
    libconv_SelectEncode_UTF16LE();
  }
  
  if(errcnt_UTF8<errcnt){
    errcnt=errcnt_UTF8;
    libconv_SelectEncode_UTF8();
  }
}

#undef EnableZeroCheck

// -------------------------------------------------------

static bool libconv_GetTextLine(u32 lineidx,UnicodeChar *pstrw)
{
  if(TextLinesCount<=lineidx) return(false);
  
  u32 ofs=pTextLines[lineidx].TopOffset;
  
  u32 secidx=ofs/256;
  if(ReadSectorIndex!=secidx){
    ReadSectorIndex=secidx;
    DFS_SeekSectorCount(secidx);
    DFS_ReadSectors((u8*)pReadBuf,2);
  }
  
  ofs&=255;
  
  while(pReadBuf[ofs]!=CharLineEnd){
    *pstrw++=pReadBuf[ofs];
    ofs++;
  }
  *pstrw++=0;
  
  return(true);
}

