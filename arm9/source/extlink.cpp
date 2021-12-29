
#pragma Ospace

#include <nds.h>

#include "_console.h"
#include "_const.h"
#include "maindef.h"
#include "unicode.h"
#include "strtool.h"
#include "shell.h"
#include "lang.h"

#include "extlink.h"

typedef struct {
  u32 Ext32;
  UnicodeChar *pNDSFullPathFilenameUnicode;
} TExtLink;

static u32 ExtLinkCount;
static TExtLink *pExtLink;

// ------------------------------------

static void DataFileClear(void)
{
  FAT_FILE *pf=Shell_FAT_fopenwrite_Data(ExtLinkDATFilename);
  if(pf==NULL){
    _consolePrintf("Fatal error: ExtLink: Create error. [%s]\n",ExtLinkDATFilename);
    ShowLogHalt();
  }
  
  u32 dummy=0;
  FAT2_fwrite(&dummy,1,4,pf);
  FAT2_fclose(pf);
}

static u32 GetExt32(const UnicodeChar *pufn)
{
  UnicodeChar ufn[256];
  Unicode_Copy(ufn,pufn);
  
  {
    UnicodeChar *ptmp=ufn;
    while(*ptmp!=0){
      u32 ch=*ptmp;
      if((0x61<=ch)&&(ch<=0x7a)) *ptmp=ch-0x20;
      ptmp++;
    }
  }
  
  u32 ufnlen=Unicode_GetLength(ufn);
  if(ufnlen<4) return(0);
  if((ufn[ufnlen-4]!='.')||(ufn[ufnlen-3]!='N')||(ufn[ufnlen-2]!='D')||(ufn[ufnlen-1]!='S')) return(0);
  
  u32 Ext32=0;
  {
    const UnicodeChar *ptmp=ufn;
    while((*ptmp!=0)&&(*ptmp!='.')){
      u32 ch=*ptmp++;
      if((0x61<=ch)&&(ch<=0x7a)) ch-=0x20;
      Ext32=(Ext32<<8)|ch;
    }
  }
  
  return(Ext32);
}

void ExtLink_Init(void)
{
  DataFileClear();
  
  UnicodeChar BasePathUnicode[256];
  StrConvert_Ank2Unicode(ExtLinkPath,BasePathUnicode);
  const char *pBasePathAlias=ConvertFull_Unicode2Alias(BasePathUnicode,NULL);
  
  if(FAT2_chdir_Alias(pBasePathAlias)==false){
    _consolePrintf("Fatal error: ExtLink_Init: Not found folder. [%s]\n",pBasePathAlias);
    ShowLogHalt();
  }
  
  ExtLinkCount=0;
  pExtLink=NULL;
  
  {
    const char *pafn;
    u32 FAT_FileType=FAT2_FindFirstFile(&pafn);
    
    while(FAT_FileType!=FT_NONE){
      if(FAT_FileType==FT_FILE){
        const UnicodeChar *pufn=FAT2_GetLongFilenameUnicode();
        if(pufn==NULL){
          _consolePrintf("Unicode filename read error.\n Alias='%s'\n",pafn);
          }else{
          u32 Ext32=GetExt32(pufn);
          if(Ext32!=0){
            ExtLinkCount++;
          }
        }
      }
      FAT_FileType=FAT2_FindNextFile(&pafn);
    }
  }
  
  pExtLink=(TExtLink*)safemalloc(sizeof(TExtLink)*ExtLinkCount);
  ExtLinkCount=0;
  
  {
    const char *pafn;
    u32 FAT_FileType=FAT2_FindFirstFile(&pafn);
    
    while(FAT_FileType!=FT_NONE){
      if(FAT_FileType==FT_FILE){
        const UnicodeChar *pufn=FAT2_GetLongFilenameUnicode();
        if(pufn==NULL){
          _consolePrintf("Unicode filename read error.\n Alias='%s'\n",pafn);
          }else{
          u32 Ext32=GetExt32(pufn);
          if(Ext32!=0){
            TExtLink *pel=&pExtLink[ExtLinkCount];
            pel->Ext32=Ext32;
            UnicodeChar ufn[256]={0,};
            Unicode_Copy(ufn,BasePathUnicode);
            static const UnicodeChar su[2]={(UnicodeChar)'/',0};
            Unicode_Add(ufn,su);
            Unicode_Add(ufn,pufn);
            pel->pNDSFullPathFilenameUnicode=Unicode_AllocateCopy(ufn);
            _consolePrintf("Found ExtLink file. (%d) [%s]\n",ExtLinkCount,pafn);
            // _consolePrintf("Debug: %s\n",StrConvert_Unicode2Ank_Test(pel->pNDSFullPathFilenameUnicode));
            ExtLinkCount++;
          }
        }
      }
      FAT_FileType=FAT2_FindNextFile(&pafn);
    }
  }
  
}

u32 ExtLink_GetTargetIndex(u32 Ext32)
{
  for(u32 idx=0;idx<ExtLinkCount;idx++){
    if(Ext32==pExtLink[idx].Ext32) return(idx);
  }
  
  return((u32)-1);
}

const UnicodeChar* ExtLink_GetNDSFullPathFilenameUnicode(u32 idx)
{
  if(ExtLinkCount<=idx){
    _consolePrintf("Fatal error: ExtLink_GetNDSFullPathFilenameUnicode index overflow. %d/%d\n",idx,ExtLinkCount);
    ShowLogHalt();
  }
  
  return(pExtLink[idx].pNDSFullPathFilenameUnicode);  
}

