
#include <stdio.h>

#include <NDS.h>

#include "plugin.h"
#include "plugin_def.h"

#include "inifile.h"

TGlobalINI GlobalINI;

void InitINI(void)
{
  {
    TiniMikModPlugin *MikModPlugin=&GlobalINI.MikModPlugin;
    
    MikModPlugin->MaxVoiceCount=24;
    MikModPlugin->Flag_Interpolate=true;
    MikModPlugin->Flag_Surround=true;
    MikModPlugin->Flag_HQMixer=false;
    
    MikModPlugin->Channels=2;
    MikModPlugin->Frequency=44100;
  }
}

static char section[128];
static u32 readline;

static void readsection(char *str)
{
  str++;
  
  u32 ofs;
  
  ofs=0;
  while(*str!=']'){
    if((128<=ofs)||(*str==0)){
      _consolePrintf("line%d error.\nThe section name doesn't end correctly.\n",readline);
      ShowLogHalt();
    }
    section[ofs]=*str;
    str++;
    ofs++;
  }
  section[ofs]=0;
}

static void readkey(char *str)
{
  if(section[0]==0){
    _consolePrintf("line%d error.\nThere is a key ahead of the section name.\n",readline);
//    ShowLogHalt();
    return;
  }
  
  char key[128],value[128];
  
  u32 ofs;
  
  ofs=0;
  while(*str!='='){
    if((128<=ofs)||(*str==0)){
      _consolePrintf("line%d error.\nThe key name doesn't end correctly.\n",readline);
      ShowLogHalt();
    }
    key[ofs]=*str;
    str++;
    ofs++;
  }
  key[ofs]=0;
  
  str++;
  
  ofs=0;
  while(*str!=0){
    if(128<=ofs){
      _consolePrintf("line%d error.\nThe value doesn't end correctly.\n",readline);
      ShowLogHalt();
    }
    value[ofs]=*str;
    str++;
    ofs++;
  }
  value[ofs]=0;
  
  s32 ivalue=atoi(value);
  bool bvalue;
  
  if(ivalue==0){
    bvalue=false;
    }else{
    bvalue=true;
  }
  
  bool match=false;
  
  if((match==false)&&(strcmp(section,"MikModPlugin")==0)){
    TiniMikModPlugin *MikModPlugin=&GlobalINI.MikModPlugin;
    
    if((match==false)&&(strcmp(key,"MaxVoiceCount")==0)){
      match=true;
      if(ivalue!=0) MikModPlugin->MaxVoiceCount=ivalue;
    }
    if((match==false)&&(strcmp(key,"Flag_Interpolate")==0)){
      match=true;
      MikModPlugin->Flag_Interpolate=bvalue;
    }
    if((match==false)&&(strcmp(key,"Flag_Surround")==0)){
      match=true;
      MikModPlugin->Flag_Surround=bvalue;
    }
    if((match==false)&&(strcmp(key,"Flag_HQMixer")==0)){
      match=true;
      MikModPlugin->Flag_HQMixer=bvalue;
    }
    if((match==false)&&(strcmp(key,"Channels")==0)){
      match=true;
      if(ivalue!=0) MikModPlugin->Channels=ivalue;
    }
    if((match==false)&&(strcmp(key,"Frequency")==0)){
      match=true;
      if(ivalue!=0) MikModPlugin->Frequency=ivalue;
    }
    
  }
  
  if(match==false){
    _consolePrintf("line%d error.\ncurrent section [%s] unknown key=%s\n",readline,section,key);
//    ShowLogHalt();
  }
  
//  _consolePrintf("key=%s value=%s\n",key,value);
}

static void internal_LoadGlobalINI(char *pini,u32 inisize)
{
  section[0]=0;
  readline=0;
  
  u32 iniofs=0;
  
  while(iniofs<inisize){
    
    readline++;
    
    u32 linelen=0;
    
    // Calc Line Length
    {
      char *s=&pini[iniofs];
      
      while(0x20<=*s){
        linelen++;
        s++;
        if(inisize<=(iniofs+linelen)) break;
      }
      *s=0;
    }
    
    if(linelen!=0){
      char c=pini[iniofs];
      if((c==';')||(c=='/')||(c=='!')){
        // comment line
        }else{
        if(c=='['){
          readsection(&pini[iniofs]);
          }else{
          readkey(&pini[iniofs]);
        }
      }
    }
    
    iniofs+=linelen;
    
    // skip NULL,CR,LF
    {
      char *s=&pini[iniofs];
      
      while(*s<0x20){
        iniofs++;
        s++;
        if(inisize<=iniofs) break;
      }
    }
    
  }
}

void LoadINI(char *data,int size)
{
  if((data==NULL)||(size==0)) return;
  
  internal_LoadGlobalINI(data,size);
}

