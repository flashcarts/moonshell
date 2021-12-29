
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <NDS.h>

#include "_const.h"
#include "_console.h"
#include "_consolewritelog.h"
#include "maindef.h"
#include "fat2.h"
#include "memtool.h"
#include "shell.h"
#include "lang.h"
#include "strtool.h"

#include "dll.h"

#include "dllsound.h"
#include "strpcm.h"

bool GlobalPauseFlag;

TID3Tag ID3Tag;

typedef struct {
  u32 SourceRate;
  u32 Pos,Size;
  u32 Mod16,Add16;
  u32 CountSource,Count32768;
  s16 *pReadBufL,*pReadBufR;
  s16 LastSampleL,LastSampleR;
} TRateConv;

static TRateConv RateConv={0,};

static TPluginBody *pPluginBody=NULL;
static FAT_FILE *PluginBody_FileHandle=NULL;

#define ExceptMP3

#ifdef ExceptMP3
#include "dllsound_internal_mp3.h"
#endif

void DLLSound_Open(const char *pFilename)
{
  if(DLLSound_isOpened()==true) DLLSound_Close();
  
  {
    TID3Tag *pi3t=&ID3Tag;
    pi3t->Exists=false;
    pi3t->LinesExistsCount=0;
    
    for(u32 idx=0;idx<EID3TagLI_Count;idx++){
      TID3Tag_Line *pi3tl=&pi3t->Lines[idx];
      pi3tl->Exists=false;
      pi3tl->pStrL=NULL;
      pi3tl->StrW[0]=0;
    }
  }
  
  const char *pext=NULL;
  {
    u32 idx=0;
    while(pFilename[idx]!=0){
      if(pFilename[idx]=='.') pext=&pFilename[idx];
      idx++;
    }
  }
  
  PluginBody_FileHandle=FAT2_fopen_AliasForRead(pFilename);
  if(PluginBody_FileHandle==NULL){
    _consolePrintf("Data file not found. [%s]\n",pFilename);
    return;
  }
  
#ifdef ExceptMP3
  if(DLLSound_Open_internal_mp3(pext)==true) return;
#endif

  char fn[PluginFilenameMax];
  EPluginType PluginType;
  
  PluginType=DLLList_GetPluginFilename(pext,fn);
  if(PluginType!=EPT_Sound){
    FAT2_fclose(PluginBody_FileHandle); PluginBody_FileHandle=NULL;
    _consolePrintf("This plugin is not sound type. (%s)\n",fn);
    return;
  }
  
  _consolePrintf("DLLSound: Load plugin.\n");
  pPluginBody=DLLList_LoadPlugin(fn);
  if(pPluginBody==NULL){
    FAT2_fclose(PluginBody_FileHandle); PluginBody_FileHandle=NULL;
    _consolePrint("Plugin load error.\n");
    return;
  }
  DTCM_StackCheck(-1);
  
  _consolePrintf("DLLSound: Start plugin.\n");
  if(pPluginBody->pSL->Start((int)PluginBody_FileHandle)==false){
    DLLList_FreePlugin(pPluginBody); pPluginBody=NULL;
    FAT2_fclose(PluginBody_FileHandle); PluginBody_FileHandle=NULL;
    _consolePrint("Plugin start error.\n");
    return;
  }
  DTCM_StackCheck(-1);
  
  PrintFreeMem();
  
  _consolePrintf("DLLSound: Get info.\n");
  u32 rate=pPluginBody->pSL->GetSampleRate();
  u32 spf=pPluginBody->pSL->GetSamplePerFrame();
  u32 chs=pPluginBody->pSL->GetChannelCount();
  
  _consolePrintf("DLLSound: rate=%dHz, spf=%dsamples, chs=%dchs.\n",rate,spf,chs);
  
  RateConv.SourceRate=0;
  if(rate==16384){
    strpcmStart(false,rate,spf,chs,SPF_PCMx2);
    if(rate==32768){
      strpcmStart(false,rate,spf,chs,SPF_PCMx1);
      if(rate<=32768){
        strpcmStart(false,rate,spf,chs,SPF_PCMx4);
        }else{
        TRateConv *prc=&RateConv;
        prc->SourceRate=rate;
        prc->Pos=0;
        prc->Size=0;
        prc->Mod16=0;
        prc->Add16=(rate*0x10000)/32768;
        prc->CountSource=spf;
        prc->Count32768=(spf*32768)/prc->SourceRate;
        if((prc->Count32768&1)!=0) prc->Count32768--;
        prc->pReadBufL=(s16*)safemalloc(prc->CountSource*3*2);
        prc->pReadBufR=(s16*)safemalloc(prc->CountSource*3*2);
        prc->LastSampleL=0;
        prc->LastSampleR=0;
    
        strpcmStart(false,32768,prc->Count32768,chs,SPF_PCMx1);
      }
    }
  }
  
  atype_checkoverrange();
}

void DLLSound_Close(void)
{
  strpcmStop();
  
  ID3Tag.Exists=false;
  
  if(pPluginBody!=NULL){
    DLLList_FreePlugin(pPluginBody); pPluginBody=NULL;
  }
  DTCM_StackCheck(-1);
  
#ifdef ExceptMP3
  if(isMP3==true){
    isMP3=false;
    PlugMP3_Free();
    DTCM_StackCheck(-1);
  }
#endif
  
  if(PluginBody_FileHandle!=NULL){
    FAT2_fclose(PluginBody_FileHandle); PluginBody_FileHandle=NULL;
  }
  
  TRateConv *prc=&RateConv;
  if(prc->SourceRate!=0){
    if(prc->pReadBufL!=NULL){
      safefree(prc->pReadBufL); prc->pReadBufL=NULL;
    }
    if(prc->pReadBufR!=NULL){
      safefree(prc->pReadBufR); prc->pReadBufR=NULL;
    }
  }
  
  atype_checkoverrange();
}

bool DLLSound_isOpened(void)
{
#ifdef ExceptMP3
  if(isMP3==true) return(true);
#endif

  if(pPluginBody!=NULL){
    return(true);
    }else{
    return(false);
  }
}

bool DLLSound_Update(void)
{
  if(DLLSound_isOpened()==false) return(false);
  
//  _consolePrint("DLLSound_Update()\n");
  
  u32 BaseSamples=IPC6->strpcmSamples;
  u32 Samples=0;
  
  REG_IME=0;
  
  u32 CurIndex=(strpcmRingBufWriteIndex+1) & strpcmRingBufBitMask;
  u32 PlayIndex=strpcmRingBufReadIndex;
  bool EmptyFlag;
  
  EmptyFlag=strpcmRingEmptyFlag;
  strpcmRingEmptyFlag=false;
  
  REG_IME=1;
  
  if(CurIndex==PlayIndex) return(false);
  
  if(EmptyFlag==true){ cwl();
    _consolePrint("strpcm:CPU overflow.\n");
  }
  
  if((strpcmRingLBuf==NULL)||(strpcmRingRBuf==NULL)) return(false);
  
  s16 *ldst=&strpcmRingLBuf[BaseSamples*CurIndex];
  s16 *rdst=&strpcmRingRBuf[BaseSamples*CurIndex];
  
#ifdef ExceptMP3
  if(isMP3==true){
    bool res=DLLSound_Update_internal_mp3(BaseSamples,ldst,rdst);
    REG_IME=0;
    strpcmRingBufWriteIndex=CurIndex;
    REG_IME=1;
    return(res);
  }
#endif
  
  if(RateConv.SourceRate==0){
    if(strpcmRequestStop==true) return(false);
    
    u32 Samples=0;
    if(GlobalPauseFlag==false){
      if(strpcmRequestStop==false){
        Samples=pPluginBody->pSL->Update(ldst,rdst);
        DTCM_StackCheck(-1);
        if(Samples!=BaseSamples) strpcmRequestStop=true;
      }
    }
    if(Samples<BaseSamples){ cwl();
      for(u32 idx=Samples;idx<BaseSamples;idx++){ cwl();
        ldst[idx]=0;
        rdst[idx]=0;
      }
    }
    
    REG_IME=0;
    strpcmRingBufWriteIndex=CurIndex;
    REG_IME=1;
    
    atype_checkoverrange();
    
    if(strpcmRequestStop==false){
      return(true);
      }else{
      return(false);
    }
  }
  
  {
    TRateConv *prc=&RateConv;
    
    if(prc->Pos!=0){
      if((prc->Pos&1)==0){
        s16 *psrcbufl=&prc->pReadBufL[prc->Pos];
        s16 *psrcbufr=&prc->pReadBufR[prc->Pos];
        s16 *pdstbufl=&prc->pReadBufL[0];
        s16 *pdstbufr=&prc->pReadBufR[0];
        s32 size=(prc->Size-prc->Pos)*2;
        if(0<size){
          u32 r0,r1,r2,r3,r4,r5,r6,r7;
          asm{
            cmp size,#8*4
            blo strpcm_MemMove1_1x16bit
            
            strpcm_MemMove1_8x32bit:
            ldmia psrcbufl!,{r0,r1,r2,r3,r4,r5,r6,r7}
            stmia pdstbufl!,{r0,r1,r2,r3,r4,r5,r6,r7}
            ldmia psrcbufr!,{r0,r1,r2,r3,r4,r5,r6,r7}
            stmia pdstbufr!,{r0,r1,r2,r3,r4,r5,r6,r7}
            sub size,size,#8*4
            cmp size,#8*4
            bhs strpcm_MemMove1_8x32bit
            
            cmp size,#0
            beq strpcm_MemMove1_End
            
            strpcm_MemMove1_1x16bit:
            ldrh r0,[psrcbufl],#2
            ldrh r1,[psrcbufr],#2
            strh r0,[pdstbufl],#2
            strh r1,[pdstbufr],#2
            subs size,size,#2
            bne strpcm_MemMove1_1x16bit
            
            strpcm_MemMove1_End:
          }
          prc->Size-=prc->Pos;
          prc->Pos=0;
        }
        }else{
        s16 *psrcbufl=&prc->pReadBufL[prc->Pos-1];
        s16 *psrcbufr=&prc->pReadBufR[prc->Pos-1];
        s16 *pdstbufl=&prc->pReadBufL[0];
        s16 *pdstbufr=&prc->pReadBufR[0];
        s32 size=(prc->Size-(prc->Pos-1))*2;
        if(0<size){
          u32 r0,r1,r2,r3,r4,r5,r6,r7;
          asm{
            cmp size,#8*4
            blo strpcm_MemMove2_1x16bit
            
            strpcm_MemMove2_8x32bit:
            ldmia psrcbufl!,{r0,r1,r2,r3,r4,r5,r6,r7}
            stmia pdstbufl!,{r0,r1,r2,r3,r4,r5,r6,r7}
            ldmia psrcbufr!,{r0,r1,r2,r3,r4,r5,r6,r7}
            stmia pdstbufr!,{r0,r1,r2,r3,r4,r5,r6,r7}
            sub size,size,#8*4
            cmp size,#8*4
            bhs strpcm_MemMove2_8x32bit
            
            cmp size,#0
            beq strpcm_MemMove2_End
            
            strpcm_MemMove2_1x16bit:
            ldrh r0,[psrcbufl],#2
            ldrh r1,[psrcbufr],#2
            strh r0,[pdstbufl],#2
            strh r1,[pdstbufr],#2
            subs size,size,#2
            bne strpcm_MemMove2_1x16bit
            
            strpcm_MemMove2_End:
          }
          prc->Size-=prc->Pos-1;
          prc->Pos=1;
        }
      }
    }
    
    while((((s32)prc->Size-1)-(s32)prc->Pos)<(s32)prc->CountSource){
      if(GlobalPauseFlag==true){
        break;
        }else{
        if(strpcmRequestStop==true){ cwl();
          break;
          }else{ cwl();
          // PrfStart();
          u32 Samples=pPluginBody->pSL->Update(&prc->pReadBufL[prc->Size],&prc->pReadBufR[prc->Size]);
          DTCM_StackCheck(-1);
          // PrfEnd(2);
          prc->Size+=Samples;
          if(Samples!=prc->CountSource) strpcmRequestStop=true;
        }
      }
    }
    
    s32 LastSrcSamples=((s32)prc->Size-1)-(s32)prc->Pos;
    
    if(LastSrcSamples<=0){
      Samples=0;
      }else{
      Samples=prc->Count32768;
      
      u32 ConvSamples=(LastSrcSamples*32768)/prc->SourceRate;
      if(ConvSamples<Samples) Samples=ConvSamples;
      
/*
      PrfStart(); // 539 538 543 539us
      for(u32 idx=0;idx<Samples;idx++){
        u32 fix16=prc->Mod16;
        u32 ifix16=0x10000-fix16;
        ldst[idx]=((prc->LastSampleL*ifix16)+(prc->pReadBufL[prc->Pos]*fix16))/0x10000;
        rdst[idx]=((prc->LastSampleR*ifix16)+(prc->pReadBufR[prc->Pos]*fix16))/0x10000;
        prc->Mod16+=prc->Add16;
        while(0x10000<=prc->Mod16){
          prc->Mod16-=0x10000;
          prc->LastSampleL=prc->pReadBufL[prc->Pos];
          prc->LastSampleR=prc->pReadBufR[prc->Pos];
          prc->Pos++;
        }
      }
      PrfEnd(0);
*/
      
      // PrfStart(); // 270 268 271 266us
      
      s16 *psrcbufl=&prc->pReadBufL[prc->Pos];
      s16 *psrcbufr=&prc->pReadBufR[prc->Pos];
      s16 *pdstbufl=ldst;
      s16 *pdstbufr=rdst;
      u32 cnt=Samples+1;
      u32 Pos=prc->Pos*2;
      u32 Mod16=prc->Mod16;
      u32 Add16=prc->Add16;
      s32 LastSample=(prc->LastSampleL&0xffff)|((prc->LastSampleR&0xffff)<<16);
      
      u32 REG_iMod16,REG_tmpsmp=0,REG_tmpl,REG_tmpr;
      asm{
        Linner_Loop:
        subs cnt,cnt,#1
        beq Linner_End
        
        mov REG_iMod16,#0x10000
        sub REG_iMod16,REG_iMod16,Mod16
        
        ldrhs REG_tmpsmp,[psrcbufl,Pos]
        smulwb REG_tmpl,REG_iMod16,LastSample
        smlawb REG_tmpl,Mod16,REG_tmpsmp,REG_tmpl
        
        ldrhs REG_tmpsmp,[psrcbufr,Pos]
        smulwt REG_tmpr,REG_iMod16,LastSample
        smlawb REG_tmpr,Mod16,REG_tmpsmp,REG_tmpr
        
        strh REG_tmpl,[pdstbufl],#2
        strh REG_tmpr,[pdstbufr],#2
        
        add Mod16,Mod16,Add16
        cmp Mod16,#0x10000
        blo Linner_Loop
        
        Linner_Modulation:
        sub Mod16,Mod16,#0x10000
        ldrh REG_tmpl,[psrcbufl,Pos]
        ldrh REG_tmpr,[psrcbufr,Pos]
        orr LastSample,REG_tmpl,REG_tmpr,lsl #16
        add Pos,Pos,#2
        cmp Mod16,#0x10000
        blo Linner_Loop
        b Linner_Modulation
        
        Linner_End:
      }

      prc->Pos=Pos/2;
      prc->Mod16=Mod16;
      prc->Add16=Add16;
      prc->LastSampleL=(s32)((s16)(LastSample&0xffff));
      prc->LastSampleR=(s32)((s16)((LastSample>>16)&0xffff));
      
      // PrfEnd(0);
    }
  }
  
  if(Samples<BaseSamples){ cwl();
    for(u32 idx=Samples;idx<BaseSamples;idx++){ cwl();
      ldst[idx]=0;
      rdst[idx]=0;
    }
  }
  
  REG_IME=0;
  strpcmRingBufWriteIndex=CurIndex;
  REG_IME=1;
  
  atype_checkoverrange();
  
  if(strpcmRequestStop==false){
    return(true);
    }else{
    return(false);
  }
  
}

void DLLSound_WaitForStreamPCM(void)
{
  if(DLLSound_isOpened()==false) return;
  if(strpcmRequestStop==false) return;
  
  while(1){
    u32 CurIndex=strpcmRingBufWriteIndex;
    u32 PlayIndex=(strpcmRingBufReadIndex+1) & strpcmRingBufBitMask;
    if(CurIndex==PlayIndex) return;
    swiWaitForVBlank();
  }
}

void DLLSound_SetVolume64(u32 v)
{
  strpcmSetVolume64(v);
}

s32 DLLSound_GetPosMax(void)
{
#ifdef ExceptMP3
  if(isMP3==true){
    return(PlugMP3_GetPosMax());
  }
#endif
  if(pPluginBody==NULL) return(0);
  return(pPluginBody->pSL->GetPosMax());
}

s32 DLLSound_GetPosOffset(void)
{
#ifdef ExceptMP3
  if(isMP3==true){
    return(PlugMP3_GetPosOffset());
  }
#endif
  if(pPluginBody==NULL) return(0);
  return(pPluginBody->pSL->GetPosOffset());
}

void DLLSound_SetPosOffset(s32 pos)
{
#ifdef ExceptMP3
  if(isMP3==true){
    PlugMP3_SetPosOffset(pos);
    return;
  }
#endif
  if(pPluginBody==NULL) return;
  pPluginBody->pSL->SetPosOffset(pos);
}

u32 DLLSoung_GetBitRatePerByte(void)
{
#ifdef ExceptMP3
  if(isMP3==true){
    if(PlugMP3_BitRatePerByteDivCount!=0){
      u32 BitRatePerByte=PlugMP3_GetBitRate()/8;
      PlugMP3_BitRatePerByteValue+=BitRatePerByte;
      PlugMP3_BitRatePerByteDivCount++;
      // _consolePrintf("%d,%d kbyteps\n",PlugMP3_BitRatePerByteValue/PlugMP3_BitRatePerByteDivCount,BitRatePerByte);
      return(PlugMP3_BitRatePerByteValue/PlugMP3_BitRatePerByteDivCount);
    }
  }
#endif
  return(0);
}

u32 DLLSound_GetPlayTimeSec(u32 BitRatePerByte)
{
#ifdef ExceptMP3
  if(isMP3==true){
    if(BitRatePerByte==0) return(0);
    u32 pos=PlugMP3_GetPosMax();
    u32 sec=pos/BitRatePerByte;
    return(sec);
  }
#endif
  return(0);
}

u32 DLLSound_GetCurrentTimeSec(u32 BitRatePerByte)
{
#ifdef ExceptMP3
  if(isMP3==true){
    if(BitRatePerByte==0) return(0);
    u32 pos=PlugMP3_GetPosOffset();
    u32 sec=pos/BitRatePerByte;
    return(sec);
  }
#endif
  return(0);
}
