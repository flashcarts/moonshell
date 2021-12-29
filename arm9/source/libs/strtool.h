
#ifndef strtool_h
#define strtool_h

#include <stdio.h>
#include <string.h>
#include <NDS.h>
#include "memtool.h"

extern void StrCopy(const char *src,char *dst);
extern bool isStrEqual(const char *s1,const char *s2);
extern bool isStrEqual_NoCaseSensitive(const char *s1,const char *s2);
extern void StrAppend(char *s,const char *add);

extern char* str_AllocateCopy(const char *src);

static inline bool str_isEmpty(const char *psrc)
{
  if(psrc==NULL) return(true);
  if(psrc[0]==0) return(true);
  return(false);
}

#endif

