
#include <NDS.h>
#include "_const.h"
#include "strtool.h"

void StrCopy(const char *src,char *dst)
{
  if(dst==0) return;
  if(src==0){
    dst[0]=0;
    return;
  }
  
  while(*src!=0){
    *dst=*src;
    src++;
    dst++;
  }
  
  *dst=0;
}

bool isStrEqual(const char *s1,const char *s2)
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

bool isStrEqual_NoCaseSensitive(const char *s1,const char *s2)
{
  if((s1==0)&&(s2==0)) return(true);
  if((s1==0)||(s2==0)) return(false);
  
  while(1){
    char c1=*s1++;
    char c2=*s2++;
    
    if(((u32)'A'<=c1)&&(c1<=(u32)'Z')) c1+=0x20;
    if(((u32)'A'<=c2)&&(c2<=(u32)'Z')) c2+=0x20;
    
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

void StrAppend(char *s,const char *add)
{
  if((s==0)||(add==0)) return;
  
  while(*s!=0){
    s++;
  }
  
  while(*add!=0){
    *s=*add;
    s++;
    add++;
  }
  
  *s=0;
}

char* str_AllocateCopy(const char *src)
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

