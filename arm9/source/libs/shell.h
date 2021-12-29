
#ifndef shell_h
#define shell_h

#include "unicode.h"
#include "fat2.h"

#define MaxFilenameLength (256)

#define DefaultDataPath "/moonshl2"

#define DataFilename "MOONSHL2.DAT"
#define LogFilename "LOGBUF.TXT"
#define LaunchFilename "LAUNCH.DAT"
#define BGBMPFilename "BGBMP.DAT"
#define ResumeFilename "RESUME.DAT"
#define LanguageFilename "LANGUAGE.INI"
#define FPGABitStreamFilename "FPGA.NCD"
#define DiskCheck_Read16bitBinFilename "CHK16BIT.BIN"

#define FontWidthFilenameFormat "FONT%d.GLW"
#define FontGlyphFilenameFormat "FONT%d.GLF"
#define FontClearTypeFilenameFormat "FONT%d.CTF"

#define SwapFileSectorsCount (16*1024*1024/512)
#define SwapFilename "SWAPFILE.$$$"

#define ExtLinkPath DefaultDataPath "/extlink"
#define ExtLinkDATFilename "EXTLINK.DAT"

#define rq_tableImageFilename "RQ_TABLE.IMG"

#define SNDEFFDATFilename "SNDEFF.DAT"

#define DefaultNFilename "defaultn.nds"
#define DefaultXFilename "defaultx.nds"
#define DefaultYFilename "defaulty.nds"

#define FPGAFilename "NDS_FPGA.RAW"

#define ResetMSEPath DefaultDataPath "/resetmse"

void Shell_CheckDataPath(void);

bool Shell_FAT_fopen_isExists_Data(const char *fn);
FAT_FILE* Shell_FAT_fopen_Data(const char *fn);
FAT_FILE* Shell_FAT_fopenwrite_Data(const char *fn);
FAT_FILE* Shell_FAT_fopenmodify_Data(const char *fn);
FAT_FILE* Shell_FAT_fopen_FullPath(const UnicodeChar *pFullPathUnicode);
FAT_FILE* Shell_FAT_fopen_Split(const UnicodeChar *pFilePathUnicode,const UnicodeChar *pFileNameUnicode);

FAT_FILE* Shell_FAT_fopen_TextFont(const char *fn);
FAT_FILE* Shell_FAT_fopen_SJISToUnicodeTable(void);
FAT_FILE* Shell_FAT_fopen_CodepageToUnicodeTable(void);

extern bool Shell_isExistsSwapFile;
void Shell_Init_SwapFile(void);
FAT_FILE* Shell_FAT_fopen_SwapFile(void);

// ------------------------------------------------------

void Shell_FAT_fopen_LanguageInit(void);
bool Shell_isJPmode(void);
FAT_FILE* Shell_FAT_fopen_Language_chrglyph(void);
FAT_FILE* Shell_FAT_fopen_Language_messages(void);

// ------------------------------------------------------

bool Shell_FAT_ReadAlloc(const char *fn,void **pbuf,s32 *psize);
bool Shell_FAT_ReadSEAlloc(const char *fn,void **pbuf,s32 *psize);

bool Shell_FAT_ReadHeadMSP(char *fn,void *buf,s32 size);
void Shell_FAT_ReadMSP(const char *fn,void **pbuf,s32 *psize);

// ------------------------------------------------------

#define SetSwapDispFilename "SWAPDISP.TXT"
#define SetBButtonToStopFuncFilename "BBTNSTOP.TXT"
#define SetCarSupplyFilename "CARSUPLY.TXT"
#define SetCluster64kFilename "CLUST64K.TXT"
#define SetCheckDiskType1Filename "CHKDSKT1.TXT"
#define SetIgnore16bitReadTestFilename "IGNORE16.TXT"
#define SetUseM4APluginFilename "USEM4A.TXT"
#define SetPowerOffMusicEndFilename "POFFMEND.TXT"

typedef struct {
  bool SwapDisp;
  bool BButtonToStopFunc;
  bool CarSupply;
  bool Cluster64k;
  bool CheckDiskType1;
  bool Ignore16bitReadTest;
  bool UseM4APlugin;
  bool PowerOffMusicEnd;
} TShellSet;

extern TShellSet ShellSet;

void Shell_ShellSet_Init(void);

// ------------------------------------------------------

extern const char* ConvertFull_Unicode2Alias(const UnicodeChar *pBasePathUnicode,const UnicodeChar *pFilenameUnicode);
extern bool FileExistsUnicode(const UnicodeChar *pBasePathUnicode,const UnicodeChar *pFilenameUnicode);
extern bool PathExistsUnicode(const UnicodeChar *pBasePathUnicode);

extern const char* ConvertFullPath_Unicode2Alias(const UnicodeChar *pFullPathUnicode);
extern bool FullPath_FileExistsUnicode(const UnicodeChar *pFullPathUnicode);
extern bool FullPath_FileExistsAnsi(const char *pFullPathAnsi);

extern const char* ConvertFullPath_Ansi2Alias(const char *pFullPathAnsi);

extern void SplitItemFromFullPathAlias(const char *pFullPathAlias,char *pPathAlias,char *pFilenameAlias);
extern void SplitItemFromFullPathUnicode(const UnicodeChar *pFullPathUnicode,UnicodeChar *pPathUnicode,UnicodeChar *pFilenameUnicode);

extern const UnicodeChar* ConvertFull_MargeFromSplit(const UnicodeChar *pBasePathUnicode,const UnicodeChar *pFilenameUnicode);

inline static bool isSwapFilenameUnicode(UnicodeChar *puc0,UnicodeChar *puc1)
{
  if(puc0==puc1) return(false);
  
  while(1){
    u32 uc0=*puc0;
    u32 uc1=*puc1;
    if(((u32)'A'<=uc0)&&(uc0<=(u32)'Z')) uc0+=0x20;
    if(((u32)'A'<=uc1)&&(uc1<=(u32)'Z')) uc1+=0x20;
    
    if(uc0==uc1){
      if(uc0==0) return(false);
      }else{
      // ファイル名長さチェック
      if(uc0==0) return(false);
      if(uc1==0) return(true);
      if(uc0==(u32)'.') return(false);
      if(uc1==(u32)'.') return(true);
      // 文字比較
      if(uc0<uc1){
        return(false);
        }else{
        return(true);
      }
    }
    
    puc0++; puc1++;
  }
}

#endif

