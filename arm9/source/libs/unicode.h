
#ifndef unicode_h
#define unicode_h

#include "glib/tglunicode.h"
#include "memtool.h"

typedef u16 UnicodeChar;

extern bool Unicode_isEqual(const UnicodeChar *s1,const UnicodeChar *s2);
extern bool Unicode_isEqual_NoCaseSensitive(const UnicodeChar *s1,const UnicodeChar *s2);
extern void Unicode_Add(UnicodeChar *s1,const UnicodeChar *s2);
extern void Unicode_Copy(UnicodeChar *tag,const UnicodeChar *src);
extern u32 Unicode_GetLength(const UnicodeChar *s);

extern UnicodeChar* Unicode_AllocateCopy(const UnicodeChar *src);

extern void StrConvert_Ank2Unicode(const char *srcstr,UnicodeChar *dststr);
extern void StrConvert_UTF82Unicode(const char *srcstr,UnicodeChar *dststr);

static inline bool Unicode_isEmpty(const UnicodeChar *psrc)
{
  if(psrc==NULL) return(true);
  if(psrc[0]==0) return(true);
  return(false);
}

static const char* StrConvert_Unicode2Ank_Test(const UnicodeChar *srcstr)
{
  static char dststr[256];
  
  if(srcstr==NULL){
    dststr[0]='N';
    dststr[1]='U';
    dststr[2]='L';
    dststr[3]='L';
    dststr[4]=0;
    return(dststr);
  }
  
  u32 idx=0;
  
  while(*srcstr!=0){
    UnicodeChar uc=*srcstr++;
    if((0x20<=uc)&&(uc<0x80)){
      dststr[idx]=uc;
      }else{
      dststr[idx]='?';
    }
    idx++;
    if(idx==255) break;
  }
  dststr[idx]=0;
  
  return(dststr);
}

#endif
