
#pragma Ospace

#include <nds.h>

#include "_console.h"
#include "_const.h"
#include "maindef.h"
#include "unicode.h"
#include "strtool.h"
#include "procstate.h"
#include "launchstate.h"
#include "shell.h"
#include "lang.h"
#include "extlink.h"

#include "BootROM.h"

typedef struct {
  char FullPathAlias[MaxFilenameLength];
  bool Execute;
} TBootROMInfo;

static TBootROMInfo BootROMInfo;

// ------------------------------------

void BootROM_Init(void)
{
  BootROMInfo.FullPathAlias[0]=0;
  BootROMInfo.Execute=false;
}

bool BootROM_GetExecuteFlag(void)
{
  return(BootROMInfo.Execute);
}

const char* BootROM_GetFullPathAlias(void)
{
  return(BootROMInfo.FullPathAlias);
}

// ------------------------------------

#include "extlink_filestruct.h"

void BootROM_SetInfo_NoLaunch(const UnicodeChar *pPathUnicode,const UnicodeChar *pFilenameUnicode)
{
  u32 Ext32=0;
  {
    const UnicodeChar *ptmp=pFilenameUnicode;
    while(*ptmp!=0){
      u32 ch=*ptmp++;
      if(ch==(u32)'.'){
        Ext32=0;
        }else{
        if((0x61<=ch)&&(ch<=0x7a)) ch-=0x20;
        Ext32=(Ext32<<8)|ch;
      }
    }
  }
  
  u32 bodysize=sizeof(TExtLinkBody);
  TExtLinkBody *pbody=(TExtLinkBody*)safemalloc(bodysize);
  
  if(pbody==NULL){
    _consolePrintf("Fatal error: TExtLinkBody memory overflow.\n");
    ShowLogHalt();
  }
  
  MemSet8CPU(0,pbody,bodysize);
  pbody->ID=ExtLinkBody_ID;
  
  u32 extlinkidx=ExtLink_GetTargetIndex(Ext32);
  if(extlinkidx==(u32)-1){
    pbody->DataFullPathFilenameUnicode[0]=0;
    Unicode_Copy(pbody->NDSFullPathFilenameUnicode,ConvertFull_MargeFromSplit(pPathUnicode,pFilenameUnicode));
    }else{
    Unicode_Copy(pbody->DataFullPathFilenameUnicode,ConvertFull_MargeFromSplit(pPathUnicode,pFilenameUnicode));
    Unicode_Copy(pbody->NDSFullPathFilenameUnicode,ExtLink_GetNDSFullPathFilenameUnicode(extlinkidx));
  }
  
  if(pbody->DataFullPathFilenameUnicode[0]!=0){
    strcpy(pbody->DataFullPathFilenameAlias,ConvertFullPath_Unicode2Alias(pbody->DataFullPathFilenameUnicode));
    SplitItemFromFullPathAlias(pbody->DataFullPathFilenameAlias,pbody->DataPathAlias,pbody->DataFilenameAlias);
    SplitItemFromFullPathUnicode(pbody->DataFullPathFilenameUnicode,pbody->DataPathUnicode,pbody->DataFilenameUnicode);
  }
  
  if(pbody->NDSFullPathFilenameUnicode[0]!=0){
    strcpy(pbody->NDSFullPathFilenameAlias,ConvertFullPath_Unicode2Alias(pbody->NDSFullPathFilenameUnicode));
    SplitItemFromFullPathAlias(pbody->NDSFullPathFilenameAlias,pbody->NDSPathAlias,pbody->NDSFilenameAlias);
    SplitItemFromFullPathUnicode(pbody->NDSFullPathFilenameUnicode,pbody->NDSPathUnicode,pbody->NDSFilenameUnicode);
  }
  
  if(str_isEmpty(pbody->NDSFullPathFilenameAlias)==true){
    _consolePrintf("Fatal error: Can not found NDS file.\n");
    ShowLogHalt();
  }
  
  strcpy(BootROMInfo.FullPathAlias,pbody->NDSFullPathFilenameAlias);
  BootROMInfo.Execute=false;
  
  _consolePrintf("DataFullPathFilenameAlias=%s\n",pbody->DataFullPathFilenameAlias);
  _consolePrintf("NDSFullPathFilenameAlias=%s\n",pbody->NDSFullPathFilenameAlias);
  
  FAT_FILE *pf=Shell_FAT_fopenwrite_Data(ExtLinkDATFilename);
  if(pf==NULL){
    _consolePrintf("Fatal error: ExtLink: Create error. [%s]\n",ExtLinkDATFilename);
    ShowLogHalt();
  }
  
  FAT2_fwrite(pbody,1,bodysize,pf);
  FAT2_fclose(pf);

  if(pbody!=NULL){
    safefree(pbody); pbody=NULL;
  }
  
  SetNextProc(ENP_BootROM,EPFE_None);
}

void BootROM_SetInfo(const UnicodeChar *pPathUnicode,const UnicodeChar *pFilenameUnicode)
{
  const UnicodeChar *pFullPathUnicode=ConvertFull_MargeFromSplit(pPathUnicode,pFilenameUnicode);
  LaunchState_Add(ELST_NDS,pFullPathUnicode);
  LaunchState.LastTab=ELST_NDS;
  LaunchState_Save();
      
  if(ProcState.System.AutoLastState==true){
    ProcState.System.LastState=ELS_Launch;
    ProcState_RequestSave=true;
  }
  
  BootROM_SetInfo_NoLaunch(pPathUnicode,pFilenameUnicode);
}

// ------------------------------------

#include "mediatype.h"

bool BootROM_isExistsSoftResetToFirmware(void)
{
  TBootROMInfo *pbri=&BootROMInfo;
  
  snprintf(pbri->FullPathAlias,256,ResetMSEPath "/%s.nds",DIMediaID);
  UnicodeChar ufn[256];
  StrConvert_Ank2Unicode(pbri->FullPathAlias,ufn);
  
  pbri->FullPathAlias[0]=0;
  
  return(FullPath_FileExistsUnicode(ufn));
}

void BootROM_SoftResetToFirmware(void)
{
  TBootROMInfo *pbri=&BootROMInfo;
  
  _consolePrintf("Request soft reset. [%s] %s\n",DIMediaID,DIMediaName);
  
  snprintf(pbri->FullPathAlias,256,ResetMSEPath "/%s.nds",DIMediaID);
  UnicodeChar ufn[256];
  StrConvert_Ank2Unicode(pbri->FullPathAlias,ufn);
  
  if(FullPath_FileExistsUnicode(ufn)==false){
    _consolePrintf("Not found soft reset. [%s]\n",pbri->FullPathAlias);
    pbri->FullPathAlias[0]=0;
    pbri->Execute=false;
    return;
  }
  
  StrCopy(ConvertFullPath_Unicode2Alias(ufn),pbri->FullPathAlias);
  pbri->Execute=true;
}

// ------------------------------------

void BootROM_Execute(void)
{
  u8 header[16];
  {
    FAT_FILE *pf=FAT2_fopen_AliasForRead(BootROMInfo.FullPathAlias);
    if(pf==NULL){
      _consolePrintf("Fatal error: Can not open NDS file. [%s]\n",BootROMInfo.FullPathAlias);
      ShowLogHalt();
    }
    FAT2_fread(header,1,16,pf);
    FAT2_fclose(pf);
  }
  
  char ID[5];
  ID[0]=header[12+0];
  ID[1]=header[12+1];
  ID[2]=header[12+2];
  ID[3]=header[12+3];
  ID[4]=0;
  
  _consolePrintf("Detected ROMID: %s\n",ID);
  
  bool homebrew=false;
  
  if(strcmp("####",ID)==0) homebrew=true;
  if(strcmp("PASS",ID)==0) homebrew=true;
  if((ID[0]==0x3d)&&(ID[1]==0x84)&&(ID[2]==0x82)&&(ID[3]==0x0a)) homebrew=true;
  
  if(homebrew==false){
    _consolePrintf("Fatal error: Not support commercial NDSROM file.\n");
    ShowLogHalt();
  }
  
  BootROMInfo.Execute=true;
}

