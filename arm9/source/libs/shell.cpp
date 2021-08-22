
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <NDS.h>

#include "_const.h"
#include "_console.h"
#include "shell.h"
#include "strtool.h"
#include "procstate.h"

#include "memtool.h"

#include "splash.h"
#include "lang.h"

TShellSet ShellSet;

bool Shell_FAT_fopen_isExists_Data(const char *fn)
{
  static char fullfn[256];
  snprintf(fullfn,256,DefaultDataPath "/%s",fn);
  return(FullPath_FileExistsAnsi(fullfn));
}

FAT_FILE* Shell_FAT_fopen_Data(const char *fn)
{
  static char fullfn[256];
  snprintf(fullfn,256,DefaultDataPath "/%s",fn);
  const char *pfullalias;
  pfullalias=ConvertFullPath_Ansi2Alias(fullfn);
  _consolePrintf("Shell_FAT_fopen_Data=%s\n",pfullalias);
  return(FAT2_fopen_AliasForRead(pfullalias));
}

FAT_FILE* Shell_FAT_fopenwrite_Data(const char *fn)
{
  static char fullfn[256];
  snprintf(fullfn,256,DefaultDataPath "/%s",fn);
  const char *pfullalias;
  pfullalias=ConvertFullPath_Ansi2Alias(fullfn);
  _consolePrintf("Shell_FAT_fopenwrite_Data=%s\n",pfullalias);
  if(str_isEmpty(pfullalias)==true){
    _consolePrintf("Not found base file for write.\n");
    ShowLogHalt();
  }
  return(FAT2_fopen_AliasForWrite(pfullalias));
}

FAT_FILE* Shell_FAT_fopenmodify_Data(const char *fn)
{
  static char fullfn[256];
  snprintf(fullfn,256,DefaultDataPath "/%s",fn);
  const char *pfullalias;
  pfullalias=ConvertFullPath_Ansi2Alias(fullfn);
  _consolePrintf("Shell_FAT_fopenmodify_Data=%s\n",pfullalias);
  if(str_isEmpty(pfullalias)==true){
    _consolePrintf("Not found base file for modify.\n");
    ShowLogHalt();
  }
  return(FAT2_fopen_AliasForModify(pfullalias));
}

FAT_FILE* Shell_FAT_fopen_FullPath(const UnicodeChar *pFullPathUnicode)
{
  const char *pfullalias;
  pfullalias=ConvertFullPath_Unicode2Alias(pFullPathUnicode);
  _consolePrintf("Shell_FAT_fopen_FullPath=%s\n",pfullalias);
  return(FAT2_fopen_AliasForRead(pfullalias));
}

FAT_FILE* Shell_FAT_fopen_Split(const UnicodeChar *pFilePathUnicode,const UnicodeChar *pFileNameUnicode)
{
  const char *pfullalias;
  pfullalias=ConvertFull_Unicode2Alias(pFilePathUnicode,pFileNameUnicode);
  if(pfullalias==NULL) ShowLogHalt();
  _consolePrintf("Shell_FAT_fopen_Split=%s\n",pfullalias);
  return(FAT2_fopen_AliasForRead(pfullalias));
}

FAT_FILE* Shell_FAT_fopen_TextFont(const char *fn)
{
  static char fullfn[256];
  snprintf(fullfn,256,DefaultDataPath "/%s/%s",Lang_GetUTF8("TextFontFolderName"),fn);
  const char *pfullalias;
  pfullalias=ConvertFullPath_Ansi2Alias(fullfn);
  if(pfullalias==NULL){
    _consolePrintf("Fatal error: Not found text font folder. [%s]\n",fullfn);
    ShowLogHalt();
  }
  _consolePrintf("Shell_FAT_fopen_Data=%s\n",pfullalias);
  return(FAT2_fopen_AliasForRead(pfullalias));
}

FAT_FILE* Shell_FAT_fopen_CodepageToUnicodeTable(void)
{
  static char fullfn[256];
  snprintf(fullfn,256,DefaultDataPath "/unicode/cp%d.tbl",ProcState.Text.DefaultCodePage);
  const char *pfullalias;
  pfullalias=ConvertFullPath_Ansi2Alias(fullfn);
  _consolePrintf("Shell_FAT_fopen_Data=%s\n",pfullalias);
  return(FAT2_fopen_AliasForRead(pfullalias));
}

FAT_FILE* Shell_FAT_fopen_SJISToUnicodeTable(void)
{
  const char *pfullalias;
  pfullalias=ConvertFullPath_Ansi2Alias(DefaultDataPath "/unicode/sjis.tbl");
  _consolePrintf("Shell_FAT_fopen_Data=%s\n",pfullalias);
  return(FAT2_fopen_AliasForRead(pfullalias));
}

// ------------------------------------------------

bool Shell_isExistsSwapFile=false;

void Shell_Init_SwapFile(void)
{
  Shell_isExistsSwapFile=false;
  
  if(Shell_FAT_fopen_isExists_Data(SwapFilename)==false) return;
  
  FAT_FILE *pf=Shell_FAT_fopen_Data(SwapFilename);
  if(pf==NULL) return;
  
  if(FAT2_GetFileSize(pf)!=(SwapFileSectorsCount*512)){
    _consolePrintf("Fatal error: Illigal swap file size. request %dbyte.\n",SwapFileSectorsCount*512);
    ShowLogHalt();
  }
  
  FAT2_fclose(pf);
  
  Shell_isExistsSwapFile=true;
}

FAT_FILE* Shell_FAT_fopen_SwapFile(void)
{
  if(Shell_isExistsSwapFile==false){
    _consolePrintf("Fatal error: Disabled swap file.\n");
    ShowLogHalt();
  }
  
  FAT_FILE *pf=Shell_FAT_fopen_Data(SwapFilename);
  
  if(pf==NULL){
    _consolePrintf("Fatal error: Can not found swap file. [%s]\n",SwapFilename);
    ShowLogHalt();
  }
  
  return(pf);
}

// ------------------------------------------------

static char CodePageStr[4]={0,0,0,0};
static bool isJPmode;

void Shell_FAT_fopen_LanguageInit(void)
{
  static char fullfn[256];
  snprintf(fullfn,256,DefaultDataPath "/language.set");
  const char *pfullalias;
  pfullalias=ConvertFullPath_Ansi2Alias(fullfn);
  _consolePrintf("Shell_FAT_fopen_LanguageInit=%s\n",pfullalias);
  
  FAT_FILE *pf=FAT2_fopen_AliasForRead(pfullalias);
  
  if(pf==NULL){
    StrCopy("932",CodePageStr);
    }else{
    FAT2_fread(CodePageStr,1,3,pf);
    CodePageStr[3]=0;
    FAT2_fclose(pf);
  }
  
  _consolePrintf("Setup default code page is '%s'.\n",CodePageStr);
  
  isJPmode=false;
  if((strcmp("932",CodePageStr)==0)||(strcmp("933",CodePageStr)==0)){
    isJPmode=true;
    _consolePrintf("Set JPN mode.\n");
  }
}

bool Shell_isJPmode(void)
{
  return(isJPmode);
}

FAT_FILE* Shell_FAT_fopen_Language_chrglyph(void)
{
  static char fullfn[256];
  if(CodePageStr[0]=='0'){
    snprintf(fullfn,256,DefaultDataPath "/language/chrglyph.000");
    }else{
    snprintf(fullfn,256,DefaultDataPath "/language/chrglyph.%s",CodePageStr);
  }
  const char *pfullalias;
  pfullalias=ConvertFullPath_Ansi2Alias(fullfn);
  _consolePrintf("Shell_FAT_fopen_Data=%s\n",pfullalias);
  return(FAT2_fopen_AliasForRead(pfullalias));
}

FAT_FILE* Shell_FAT_fopen_Language_messages(void)
{
  static char fullfn[256];
  snprintf(fullfn,256,DefaultDataPath "/language/messages.%s",CodePageStr);
  const char *pfullalias;
  pfullalias=ConvertFullPath_Ansi2Alias(fullfn);
  _consolePrintf("Shell_FAT_fopen_Data=%s\n",pfullalias);
  return(FAT2_fopen_AliasForRead(pfullalias));
}

void Shell_CheckDataPath(void)
{
  FAT_FILE *pf=Shell_FAT_fopen_Data(DataFilename);
  
  if(pf==NULL){
    _consolePrintf("Can not found MoonShell2.00 data files. (" DefaultDataPath "/" DataFilename ")\n");
    ShowLogHalt();
  }
  
  FAT2_fclose(pf);
}

static bool Shell_FAT_FullAlias_ReadFileAlloc(const char *pFullAlias,void **pbuf,s32 *psize)
{
  FAT_FILE *pf=FAT2_fopen_AliasForRead(pFullAlias);
  
  if(pf==NULL){
    _consolePrintf("Shell_FAT_ReadAlloc: (%s) File not found.\n",pFullAlias);
    return(false);
  }
  
  *psize=FAT2_GetFileSize(pf);
  
  if(*psize==0){
    _consolePrintf("Shell_FAT_ReadAlloc: (%s) File size == 0\n",pFullAlias);
    FAT2_fclose(pf);
    return(false);
  }
  
  *pbuf=safemalloc(*psize);
  
  if(*pbuf==NULL){
    _consolePrintf("Shell_FAT_ReadAlloc: (%s) Memory overflow.\n",pFullAlias);
    FAT2_fclose(pf);
    return(false);
  }
  
  if(FAT2_fread(*pbuf,1,*psize,pf)!=*psize){
    safefree(pbuf);
    _consolePrintf("Shell_FAT_ReadAlloc: (%s) File size error.\n",pFullAlias);
    FAT2_fclose(pf);
    return(false);
  }
  
  FAT2_fclose(pf);
  
  return(true);
}

bool Shell_FAT_ReadAlloc(const char *fn,void **pbuf,s32 *psize)
{
  static char fullfn[256];
  snprintf(fullfn,256,DefaultDataPath "/%s",fn);
  const char *pfullalias=ConvertFullPath_Ansi2Alias(fullfn);
  if(str_isEmpty(pfullalias)==true) return(false);
  return(Shell_FAT_FullAlias_ReadFileAlloc(pfullalias,pbuf,psize));
}

bool Shell_FAT_ReadSEAlloc(const char *fn,void **pbuf,s32 *psize)
{
  static char fullfn[256];
  snprintf(fullfn,256,DefaultDataPath "/sndeff/%s",fn);
  const char *pfullalias=ConvertFullPath_Ansi2Alias(fullfn);
  if(str_isEmpty(pfullalias)==true) return(false);
  return(Shell_FAT_FullAlias_ReadFileAlloc(pfullalias,pbuf,psize));
}

// ----------------------------------------

bool Shell_FAT_ReadHeadMSP(char *fn,void *buf,s32 size)
{
  static char fullfn[MaxFilenameLength];
  snprintf(fullfn,MaxFilenameLength,DefaultDataPath "/plugins/%s",fn);
  const char *pFullAlias=ConvertFullPath_Ansi2Alias(fullfn);
  
  FAT_FILE *pf=FAT2_fopen_AliasForRead(pFullAlias);
  
  if(pf==NULL){
    _consolePrintf("Shell_FAT_ReadHeadMSP: (%s) File not found.\n",pFullAlias);
    return(false);
  }
  
  u32 readsize=FAT2_fread(buf,1,size,pf);
  FAT2_fclose(pf);
  
  if(readsize!=size){
    _consolePrintf("Shell_FAT_ReadHeadMSP: (%s) File size error.\n");
    return(false);
  }
  
  return(true);
}
  
void Shell_FAT_ReadMSP(const char *fn,void **pbuf,s32 *psize)
{
  static char fullfn[MaxFilenameLength];
  snprintf(fullfn,MaxFilenameLength,DefaultDataPath "/plugins/%s",fn);
  const char *pFullAlias=ConvertFullPath_Ansi2Alias(fullfn);
  
  Shell_FAT_FullAlias_ReadFileAlloc(pFullAlias,pbuf,psize);
}

// ----------------------------------------

void Shell_ShellSet_Init(void)
{
  MemSet8CPU(0,&ShellSet,sizeof(TShellSet)); // クラスが入ってないので0で埋めて全てFalseにする。
   
  const char *pfn;
  
  pfn=DefaultDataPath "/" SetSwapDispFilename;
  if(FullPath_FileExistsAnsi(pfn)==true){
    ShellSet.SwapDisp=true;
    _consolePrintf("Extend set: %s\n",pfn);
  }
  
  pfn=DefaultDataPath "/" SetBButtonToStopFuncFilename;
  if(FullPath_FileExistsAnsi(pfn)==true){
    ShellSet.BButtonToStopFunc=true;
    _consolePrintf("Extend set: %s\n",pfn);
  }
  
  pfn=DefaultDataPath "/" SetCarSupplyFilename;
  if(FullPath_FileExistsAnsi(pfn)==true){
    ShellSet.CarSupply=true;
    _consolePrintf("Extend set: %s\n",pfn);
  }
  
  pfn=DefaultDataPath "/" SetCluster64kFilename;
  if(FullPath_FileExistsAnsi(pfn)==true){
    ShellSet.Cluster64k=true;
    _consolePrintf("Extend set: %s\n",pfn);
  }
  
  pfn=DefaultDataPath "/" SetCheckDiskType1Filename;
  if(FullPath_FileExistsAnsi(pfn)==true){
    ShellSet.CheckDiskType1=true;
    _consolePrintf("Extend set: %s\n",pfn);
  }
  
  pfn=DefaultDataPath "/" SetIgnore16bitReadTestFilename;
  if(FullPath_FileExistsAnsi(pfn)==true){
    ShellSet.Ignore16bitReadTest=true;
    _consolePrintf("Extend set: %s\n",pfn);
  }

  pfn=DefaultDataPath "/" SetUseM4APluginFilename;
  if(FullPath_FileExistsAnsi(pfn)==true){
    ShellSet.UseM4APlugin=true;
    _consolePrintf("Extend set: %s\n",pfn);
  }

  
  pfn=DefaultDataPath "/" SetPowerOffMusicEndFilename;
  if(FullPath_FileExistsAnsi(pfn)==true){
    ShellSet.PowerOffMusicEnd=true;
    _consolePrintf("Extend set: %s\n",pfn);
  }
  
}

// ----------------------------------------

const char* ConvertFull_Unicode2Alias(const UnicodeChar *pBasePathUnicode,const UnicodeChar *pFilenameUnicode)
{
  if((pBasePathUnicode==NULL)||(pBasePathUnicode[0]==0)){
    static UnicodeChar su[2]={(UnicodeChar)'/',0};
    pBasePathUnicode=su;
  }
  
//  _consolePrintf("Start ConvertFull_Unicode2Alias('%s',",StrConvert_Unicode2Ank_Test(pBasePathUnicode));
//  _consolePrintf("'%s');\n",StrConvert_Unicode2Ank_Test(pFilenameUnicode));
  
  static char pfn[MaxFilenameLength]={0,};
  
  if(FAT2_chdir_Alias("/")==false){
    _consolePrint("Fatal error!! FAT_CWD('/')==false.\n");
    ShowLogHalt();
    return(NULL);
  }
  
  if(*pBasePathUnicode==(UnicodeChar)'/') pBasePathUnicode++;
  
  pfn[0]='/';
  pfn[1]=0;
  
  while(*pBasePathUnicode!=0){
    static UnicodeChar pPathUnicode[MaxFilenameLength];
    u32 PathUnicodeCount=0;
    
    while((*pBasePathUnicode!=(UnicodeChar)'/')&&(*pBasePathUnicode!=0)){
      pPathUnicode[PathUnicodeCount]=*pBasePathUnicode++;
      PathUnicodeCount++;
    }
    pPathUnicode[PathUnicodeCount]=0;
    
    bool finded=false;
    const char *pafn=NULL;
    
    {
      u32 FAT_FileType=FAT2_FindFirstFile(&pafn);
      
      while((FAT_FileType!=FT_NONE)&&(finded==false)){
        switch(FAT_FileType){
          case FT_NONE: break;
          case FT_DIR: {
            const UnicodeChar *pufn=FAT2_GetLongFilenameUnicode();
            if(pufn==NULL){
              _consolePrintf("Unicode filename read error.\n Alias='%s'\n",pafn);
              }else{
              if(Unicode_isEqual_NoCaseSensitive(pufn,pPathUnicode)==true) finded=true;
            }
          } break;
          case FT_FILE: break;
        }
        
        if(finded==false) FAT_FileType=FAT2_FindNextFile(&pafn);
      }
    }
    
    if(finded==false){
      const char *pteststr=StrConvert_Unicode2Ank_Test(pPathUnicode);
      _consolePrintf("ConvertFull_Unicode2Alias: Not find Unicode path item. [%s]\n",pteststr);
      return(NULL);
      }else{
      strcat(pfn,pafn);
      strcat(pfn,"/");
      
      if(FAT2_chdir_Alias(pafn)==false){
        _consolePrintf("Fatal error!! FAT_CWD(%s)==false.\n",pafn);
        ShowLogHalt();
        return(NULL);
      }
    }
    
    if(*pBasePathUnicode==(UnicodeChar)'/') pBasePathUnicode++;
  }
  
  if(Unicode_isEmpty(pFilenameUnicode)==false){
    bool finded=false;
    const char *pafn=NULL;
    
    {
      u32 FAT_FileType=FAT2_FindFirstFile(&pafn);
      
      while((FAT_FileType!=FT_NONE)&&(finded==false)){
        switch(FAT_FileType){
          case FT_NONE: break;
          case FT_DIR: break;
          case FT_FILE: {
            const UnicodeChar *pufn=FAT2_GetLongFilenameUnicode();
            if(pufn==NULL){
              _consolePrintf("Unicode filename read error.\n Alias='%s'\n",pafn);
              }else{
              if(Unicode_isEqual_NoCaseSensitive(pufn,pFilenameUnicode)==true) finded=true;
            }
          } break;
        }
        
        if(finded==false) FAT_FileType=FAT2_FindNextFile(&pafn);
      }
    }
    
    if(finded==false){
      const char *pteststr=StrConvert_Unicode2Ank_Test(pFilenameUnicode);
      _consolePrintf("ConvertFull_Unicode2Alias: Not find Unicode file item. [%s]\n",pteststr);
      return(NULL);
      }else{
      strcat(pfn,pafn);
    }
  }
  
//  _consolePrintf("End ConvertFull_Unicode2Alias=%s\n",pfn);
  
  return(pfn);
}

bool FileExistsUnicode(const UnicodeChar *pBasePathUnicode,const UnicodeChar *pFilenameUnicode)
{
  if((pBasePathUnicode==NULL)||(pBasePathUnicode[0]==0)){
    static UnicodeChar su[2]={(UnicodeChar)'/',0};
    pBasePathUnicode=su;
  }
  
  if((pFilenameUnicode==NULL)||(pFilenameUnicode[0]==0)) return(false);
  
  static char pfn[MaxFilenameLength]={0,};
  
  if(FAT2_chdir_Alias("/")==false){
    _consolePrint("Fatal error!! FAT_CWD('/')==false.\n");
    ShowLogHalt();
    return(false);
  }
  
  if(*pBasePathUnicode==(UnicodeChar)'/') pBasePathUnicode++;
  
  pfn[0]='/';
  pfn[1]=0;
  
  while(*pBasePathUnicode!=0){
    static UnicodeChar pPathUnicode[MaxFilenameLength];
    u32 PathUnicodeCount=0;
    
    while((*pBasePathUnicode!=(UnicodeChar)'/')&&(*pBasePathUnicode!=0)){
      pPathUnicode[PathUnicodeCount]=*pBasePathUnicode++;
      PathUnicodeCount++;
    }
    pPathUnicode[PathUnicodeCount]=0;
    
    bool finded=false;
    const char *pafn=NULL;
    
    {
      u32 FAT_FileType=FAT2_FindFirstFile(&pafn);
      
      while((FAT_FileType!=FT_NONE)&&(finded==false)){
        switch(FAT_FileType){
          case FT_NONE: break;
          case FT_DIR: {
            const UnicodeChar *pufn=FAT2_GetLongFilenameUnicode();
            if(pufn==NULL){
              _consolePrintf("Unicode filename read error.\n Alias='%s'\n",pafn);
              }else{
              if(Unicode_isEqual_NoCaseSensitive(pufn,pPathUnicode)==true) finded=true;
            }
          } break;
          case FT_FILE: break;
        }
        
        if(finded==false) FAT_FileType=FAT2_FindNextFile(&pafn);
      }
    }
    
    if(finded==false){
      const char *pteststr=StrConvert_Unicode2Ank_Test(pPathUnicode);
      _consolePrintf("FileExistsUnicode: Not find Unicode path item. [%s]\n",pteststr);
      return(false);
      }else{
      strcat(pfn,pafn);
      strcat(pfn,"/");
      
      if(FAT2_chdir_Alias(pafn)==false){
        _consolePrintf("Fatal error!! FAT_CWD(%s)==false.\n",pafn);
        ShowLogHalt();
        return(false);
      }
    }
    
    if(*pBasePathUnicode==(UnicodeChar)'/') pBasePathUnicode++;
  }
  
  if(*pFilenameUnicode!=0){
    bool finded=false;
    
    {
      const char *pafn;
      u32 FAT_FileType=FAT2_FindFirstFile(&pafn);
      
      while((FAT_FileType!=FT_NONE)&&(finded==false)){
        switch(FAT_FileType){
          case FT_NONE: break;
          case FT_DIR: break;
          case FT_FILE: {
            const UnicodeChar *pufn=FAT2_GetLongFilenameUnicode();
            if(pufn==NULL){
              _consolePrintf("Unicode filename read error.\n Alias='%s'\n",pafn);
              }else{
              if(Unicode_isEqual_NoCaseSensitive(pufn,pFilenameUnicode)==true) finded=true;
            }
          } break;
        }
        
        if(finded==false) FAT_FileType=FAT2_FindNextFile(&pafn);
      }
    }
    
    if(finded==false){
      const char *pteststr=StrConvert_Unicode2Ank_Test(pFilenameUnicode);
      _consolePrintf("FileExistsUnicode: Not find Unicode file item. [%s]\n",pteststr);
      return(false);
    }
  }
  
  return(true);
}

bool PathExistsUnicode(const UnicodeChar *pBasePathUnicode)
{
  if((pBasePathUnicode==NULL)||(pBasePathUnicode[0]==0)){
    static UnicodeChar su[2]={(UnicodeChar)'/',0};
    pBasePathUnicode=su;
  }
  
  static char pfn[MaxFilenameLength]={0,};
  
  if(FAT2_chdir_Alias("/")==false){
    _consolePrint("Fatal error!! FAT_CWD('/')==false.\n");
    ShowLogHalt();
    return(false);
  }
  
  if(*pBasePathUnicode==(UnicodeChar)'/') pBasePathUnicode++;
  
  pfn[0]='/';
  pfn[1]=0;
  
  while(*pBasePathUnicode!=0){
    static UnicodeChar pPathUnicode[MaxFilenameLength];
    u32 PathUnicodeCount=0;
    
    while((*pBasePathUnicode!=(UnicodeChar)'/')&&(*pBasePathUnicode!=0)){
      pPathUnicode[PathUnicodeCount]=*pBasePathUnicode++;
      PathUnicodeCount++;
    }
    pPathUnicode[PathUnicodeCount]=0;
    
    bool finded=false;
    const char *pafn=NULL;
    
    {
      u32 FAT_FileType=FAT2_FindFirstFile(&pafn);
      
      while((FAT_FileType!=FT_NONE)&&(finded==false)){
        switch(FAT_FileType){
          case FT_NONE: break;
          case FT_DIR: {
            const UnicodeChar *pufn=FAT2_GetLongFilenameUnicode();
            if(pufn==NULL){
              _consolePrintf("Unicode filename read error.\n Alias='%s'\n",pafn);
              }else{
              if(Unicode_isEqual_NoCaseSensitive(pufn,pPathUnicode)==true) finded=true;
            }
          } break;
          case FT_FILE: break;
        }
        
        if(finded==false) FAT_FileType=FAT2_FindNextFile(&pafn);
      }
    }
    
    if(finded==false){
      _consolePrint("Can not find Unicode path item.\n");
      return(false);
      }else{
      strcat(pfn,pafn);
      strcat(pfn,"/");
      _consolePrintf("%s\n",pfn);
      
      if(FAT2_chdir_Alias(pafn)==false){
        _consolePrintf("Fatal error!! FAT_CWD(%s)==false.\n",pafn);
        ShowLogHalt();
        return(false);
      }
    }
    
    if(*pBasePathUnicode==(UnicodeChar)'/') pBasePathUnicode++;
  }
  
  return(true);
}

const char* ConvertFullPath_Unicode2Alias(const UnicodeChar *pFullPathUnicode)
{
  if(Unicode_isEmpty(pFullPathUnicode)==true){
    static char str[1]={0};
    return(str);
  }

  u32 splitpos=(u32)-1;
  
  u32 findidx=0;
  while(pFullPathUnicode[findidx]!=0){
    if(pFullPathUnicode[findidx]==(UnicodeChar)'/') splitpos=findidx;
    findidx++;
  }
  
  if(splitpos==(u32)-1){
    _consolePrintf("ConvertFullPath_Unicode2Alias: This is not full path.\n");
    return(NULL);
  }
  
  static UnicodeChar PathNameUnicde[MaxFilenameLength],FileNameUnicde[MaxFilenameLength];
  
  if(splitpos!=0){
    for(u32 idx=0;idx<splitpos;idx++){
      PathNameUnicde[idx]=pFullPathUnicode[idx];
    }
    PathNameUnicde[splitpos]=0;
    }else{ // store in root
    PathNameUnicde[0]=(UnicodeChar)'/';
    PathNameUnicde[1]=0;
  }
  
  pFullPathUnicode+=splitpos+1;
  
  u32 idx=0;
  while(pFullPathUnicode[idx]!=0){
    FileNameUnicde[idx]=pFullPathUnicode[idx];
    idx++;
  }
  FileNameUnicde[idx]=0;
  
//  _consolePrintf("0 [%s]\n",StrConvert_Unicode2Ank_Test(PathNameUnicde));
//  _consolePrintf("  [%s]\n",StrConvert_Unicode2Ank_Test(FileNameUnicde));

  const char *pres=ConvertFull_Unicode2Alias(PathNameUnicde,FileNameUnicde);
  if(pres==NULL) ShowLogHalt();
  return(pres);
}

bool FullPath_FileExistsUnicode(const UnicodeChar *pFullPathUnicode)
{
  static UnicodeChar PathUnicode[MaxFilenameLength];
  static UnicodeChar FilenameUnicode[MaxFilenameLength];
  
  SplitItemFromFullPathUnicode(pFullPathUnicode,PathUnicode,FilenameUnicode);
  return(FileExistsUnicode(PathUnicode,FilenameUnicode));
}

bool FullPath_FileExistsAnsi(const char *pFullPathAnsi)
{
  static UnicodeChar PathUnicode[MaxFilenameLength];
  static UnicodeChar FilenameUnicode[MaxFilenameLength];
  
  UnicodeChar FullPathUnicode[MaxFilenameLength];
  StrConvert_Ank2Unicode(pFullPathAnsi,FullPathUnicode);
  SplitItemFromFullPathUnicode(FullPathUnicode,PathUnicode,FilenameUnicode);
  return(FileExistsUnicode(PathUnicode,FilenameUnicode));
}

const char* ConvertFullPath_Ansi2Alias(const char *pFullPathAnsi)
{
  static UnicodeChar FullPathUnicode[MaxFilenameLength];
  StrConvert_Ank2Unicode(pFullPathAnsi,FullPathUnicode);
  return(ConvertFullPath_Unicode2Alias(FullPathUnicode));
}

void SplitItemFromFullPathAlias(const char *pFullPathAlias,char *pPathAlias,char *pFilenameAlias)
{
  if((pPathAlias==NULL)||(pFilenameAlias==NULL)){
    _consolePrintf("Fatal error: SplitItemFromFullPath output buffer is NULL.\n");
    ShowLogHalt();
  }
  
  if((pFullPathAlias==NULL)||(pFullPathAlias[0]==0)){
    _consolePrintf("Fatal error: SplitItemFromFullPath pFullPathAlias is NULL or blank.\n");
    ShowLogHalt();
  }
  
  u32 SplitPos=0;
  {
    u32 idx=0;
    while(1){
      char uc=pFullPathAlias[idx];
      if(uc==0) break;
      if(uc=='/') SplitPos=idx+1;
      idx++;
    }
  }
  
  if(SplitPos<=1){
    pPathAlias[0]='/';
    pPathAlias[1]=0;
    }else{
    for(u32 idx=0;idx<SplitPos-1;idx++){
      pPathAlias[idx]=pFullPathAlias[idx];
    }
    pPathAlias[SplitPos-1]=0;
  }
  
  strcpy(pFilenameAlias,&pFullPathAlias[SplitPos]);
}

void SplitItemFromFullPathUnicode(const UnicodeChar *pFullPathUnicode,UnicodeChar *pPathUnicode,UnicodeChar *pFilenameUnicode)
{
  if((pPathUnicode==NULL)||(pFilenameUnicode==NULL)){
    _consolePrintf("Fatal error: SplitItemFromFullPath output buffer is NULL.\n");
    ShowLogHalt();
  }
  
  if((pFullPathUnicode==NULL)||(pFullPathUnicode[0]==0)){
    _consolePrintf("Fatal error: SplitItemFromFullPath pFullPathUnicode is NULL or blank.\n");
    ShowLogHalt();
  }
  
  u32 SplitPos=0;
  {
    u32 idx=0;
    while(1){
      UnicodeChar uc=pFullPathUnicode[idx];
      if(uc==0) break;
      if(uc==(UnicodeChar)'/') SplitPos=idx+1;
      idx++;
    }
  }
  
  if(SplitPos<=1){
    pPathUnicode[0]=(UnicodeChar)'/';
    pPathUnicode[1]=0;
    }else{
    for(u32 idx=0;idx<SplitPos-1;idx++){
      pPathUnicode[idx]=pFullPathUnicode[idx];
    }
    pPathUnicode[SplitPos-1]=0;
  }
  
  Unicode_Copy(pFilenameUnicode,&pFullPathUnicode[SplitPos]);
}

const UnicodeChar* ConvertFull_MargeFromSplit(const UnicodeChar *pBasePathUnicode,const UnicodeChar *pFilenameUnicode)
{
  if((pBasePathUnicode==NULL)||(pBasePathUnicode[0]==0)||(pBasePathUnicode[1]==0)){
    pBasePathUnicode=NULL;
  }
  
  if((pFilenameUnicode==NULL)||(pFilenameUnicode[0]==0)){
    _consolePrintf("FilenameUnicode item is NULL.\n");
    ShowLogHalt();
    return(NULL);
  }
  
  static UnicodeChar FullPathUnicode[MaxFilenameLength];
  
  const UnicodeChar us[2]={(UnicodeChar)'/',0};
  if(pBasePathUnicode==NULL){
    Unicode_Copy(FullPathUnicode,us);
    }else{
    Unicode_Copy(FullPathUnicode,pBasePathUnicode);
    Unicode_Add(FullPathUnicode,us);
  }
  
  Unicode_Add(FullPathUnicode,pFilenameUnicode);
  
  return(FullPathUnicode);
}

