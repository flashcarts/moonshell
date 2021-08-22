
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <NDS.h>

#include "_console.h"
#include "_consolewritelog.h"
#include "maindef.h"
#include "memtool.h"
#include "_const.h"
#include "shell.h"

#include "lang.h"

typedef struct {
  char *pItem;
  u32 ItemHash;
  char *pValue;
} TLangData;

#define LangDataMaxCount (256)
static u32 LangDataCount;
static TLangData LangData[LangDataMaxCount];

#define safemalloc malloc
#define safefree free

// ---------------------------------------------------------

static u32 CalcItemHash(const char *pItem)
{
  u32 hash=0;
  while(*pItem!=0){
    hash+=*pItem++;
  }
  return(hash);
}

// ---------------------------------------------------------

static char* str_AllocateCopy(const char *src)
{
  u32 len=0;
  if(src!=NULL) len=strlen(src);
  
  char *ptag=(char*)safemalloc(len+1);
  
  for(u32 idx=0;idx<len;idx++){
    ptag[idx]=src[idx];
  }
  
  ptag[len]=(char)0;
  
  return(ptag);
}

static bool isStrEqual(const char *s1,const char *s2)
{
  if((s1==0)&&(s2==0)) return(true);
  if((s1==0)||(s2==0)) return(false);
  
  while(1){
    char c1=*s1++;
    char c2=*s2++;
    
    if((c1==0)||(c2==0)){
      if((c1==0)&&(c2==0)){
        return(true);
        }else{
        return(false);
      }
    }
    
    if(c1!=c2) return(false);
  }
  
  return(false);
}

// ---------------------------------------------------------

void Lang_Load(void)
{
  FAT_FILE *pf=Shell_FAT_fopen_Language_messages();
  if(pf==NULL){
    _consolePrintf("Fatal error: Not found message file.\n");
    ShowLogHalt();
  }
  
  u32 bufsize=FAT2_GetFileSize(pf);
  u8 *pbuf=(u8*)safemalloc(bufsize);
  
  FAT2_fread_fast(pbuf,1,bufsize,pf);
  FAT2_fclose(pf);
  
  LangDataCount=0;
  
  char linebuf[512+1];
  u32 linelen=0;
  linebuf[linelen]=0;
  
  s32 ofs=0;
  
  while(ofs<bufsize){
    if(pbuf[ofs]==0) break;
    
    if(pbuf[ofs]<0x20){
      if((pbuf[ofs]==0x0d)||(pbuf[ofs]==0x0a)){
        linebuf[linelen]=0;
        
        if((linelen!=0)&&(linebuf[0]!=';')&&(linebuf[0]!='#')){
          u32 ofs=0;
          for(u32 idx=0;idx<linelen;idx++){
            if(linebuf[idx]=='='){
              ofs=idx;
              break;
            }
          }
          if(ofs!=0){
            linebuf[ofs]=0;
            
            if(LangDataCount==LangDataMaxCount){
              _consolePrintf("Fatal error: Language data buffer overflow.\n");
              ShowLogHalt();
            }
            
            TLangData *pld=&LangData[LangDataCount];
            pld->pItem=str_AllocateCopy(&linebuf[0]);
            pld->ItemHash=CalcItemHash(pld->pItem);
            pld->pValue=str_AllocateCopy(&linebuf[ofs+1]);
            LangDataCount++;
          }
        }
        
        linelen=0;
      }
      }else{
      if((linelen+1)<512){
        linebuf[linelen++]=pbuf[ofs];
      }
    }
    ofs++;
  }
  
  if(pbuf!=NULL){
    safefree(pbuf); pbuf=NULL;
  }
  
  _consolePrintf("Language materials count= %d\n",LangDataCount);
}

void Lang_Free(void)
{
  for(u32 idx=0;idx<LangDataCount;idx++){
    TLangData *pld=&LangData[idx];
    if(pld->pItem!=NULL){
      safefree(pld->pItem); pld->pItem=NULL;
    }
    if(pld->pValue!=NULL){
      safefree(pld->pValue); pld->pValue=NULL;
    }
  }
  
  LangDataCount=0;
}

const char* Lang_GetUTF8_internal(const char *pItemName,const char *pErrorMsg)
{
  if(LangDataCount==0){
    _consolePrintf("Fatal error: Lang_GetUTF8: Language file not loaded.\n");
    ShowLogHalt();
  }
  
  u32 hash=CalcItemHash(pItemName);
  
  for(u32 idx=0;idx<LangDataCount;idx++){
    TLangData *pld=&LangData[idx];
    if(hash==pld->ItemHash){
      if(isStrEqual(pItemName,pld->pItem)==true){
        return(pld->pValue);
      }
    }
  }
  
  _consolePrintf("Lang_GetUTF8: Can not found language item. [%s]\n",pItemName);
  return(pErrorMsg);
}

