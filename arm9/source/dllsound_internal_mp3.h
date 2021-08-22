
#ifdef ExceptMP3

#include "plugin/plug_mp3.h"
static bool isMP3=false;
static u32 PlugMP3_BitRatePerByteValue;
static u32 PlugMP3_BitRatePerByteDivCount;

bool DLLSound_Open_internal_mp3(const char *pext)
{
  isMP3=false;
  
  if(isStrEqual_NoCaseSensitive(pext,".mp3")==false) return(false);
  
  {
    extern bool layer3_rq_table_isLoaded(void);
    extern int layer3_rq_table_GetTableSize(void);
    extern void* layer3_rq_table_GetTableData(void);
    
    if(layer3_rq_table_isLoaded()==false){
      int bufsize=layer3_rq_table_GetTableSize();
      void *pbuf=layer3_rq_table_GetTableData();
      
      FAT_FILE *pf=Shell_FAT_fopen_Data(rq_tableImageFilename);
      if(pf==NULL){
        _consolePrintf("Fatal error: Table load error. [%s]\n",rq_tableImageFilename);
        ShowLogHalt();
      }
      if(FAT2_GetFileSize(pf)!=bufsize){
        _consolePrintf("Fatal error: Table size error. [%s] (%d!=%d)\n",rq_tableImageFilename,FAT2_GetFileSize(pf),bufsize);
        ShowLogHalt();
      }
      FAT2_fread(pbuf,1,bufsize,pf);
      FAT2_fclose(pf);
      
      _consolePrintf("layer3_rq_table loaded.\n");
    }
  }
  
  if(PlugMP3_Start(PluginBody_FileHandle)==false){
    PlugMP3_Free();
    FAT2_fclose(PluginBody_FileHandle); PluginBody_FileHandle=NULL;
    _consolePrint("PlugMP3 start error.\n");
    ShowLogHalt();
    return(false);
  }
  DTCM_StackCheck(-1);
  
  u32 rate=PlugMP3_GetSampleRate();
  u32 spf=PlugMP3_GetSamplePerFrame();
  u32 chs=PlugMP3_GetChannelCount();
  
  if(rate<=48000){
    strpcmStart(false,rate,spf,chs,SPF_PCMx4);
    }else{
    strpcmStart(false,rate,spf,chs,SPF_PCMx2);
  }
  
  PlugMP3_BitRatePerByteValue=PlugMP3_GetBitRate()/8;
  PlugMP3_BitRatePerByteDivCount=1;
  
  if(PlugMP3_ExistsID3Tag!=NULL){
    if(PlugMP3_ExistsID3Tag()==true){
      TID3Tag *pi3t=&ID3Tag;
      for(u32 idx=0;idx<EID3TagLI_Count;idx++){
        TID3Tag_Line *pi3tl=&pi3t->Lines[idx];
        const char *pstr=NULL;
        switch(idx){
          case EID3TagLI_Title:   pstr=PlugMP3_ID3Tag_GetTitle(); break;
          case EID3TagLI_Artist:  pstr=PlugMP3_ID3Tag_GetArtist(); break;
          case EID3TagLI_Album:   pstr=PlugMP3_ID3Tag_GetAlbum(); break;
          case EID3TagLI_Comment: pstr=PlugMP3_ID3Tag_GetComment(); break;
        }
        // _consolePrintf("ID3Tag%d:%s\n",idx,pstr);
        bool e=false;
        if(pstr!=NULL){
          for(u32 idx=0;idx<ID3Tag_LineMaxLength;idx++){
            char ch=pstr[idx];
            if(ch==0) break;
            if(ch!=' '){
              e=true;
              break;
            }
          }
        }
        if(e==true){
          pi3t->Exists=true;
          pi3t->LinesExistsCount++;
          
          pi3tl->Exists=true;
          pi3tl->pStrL=pstr;
          pi3tl->StrW[0]=0;
        }
      }
    }
  }
  
  atype_checkoverrange();
  
  isMP3=true;
  
  return(true);
}

#endif

static bool DLLSound_Update_internal_mp3(u32 BaseSamples,s16 *ldst,s16 *rdst)
{
  u32 Samples=0;
  if(GlobalPauseFlag==true){
    }else{
    if(strpcmRequestStop==true){ cwl();
      }else{ cwl();
      // PrfStart();
      Samples=PlugMP3_Update(ldst,rdst);
      DTCM_StackCheck(-1);
      // PrfEnd(1);
      if(Samples!=BaseSamples) strpcmRequestStop=true;
    }
  }
  
  if(Samples<BaseSamples){ cwl();
    for(u32 idx=Samples;idx<BaseSamples;idx++){ cwl();
      ldst[idx]=0;
      rdst[idx]=0;
    }
  }
  
  atype_checkoverrange();
  
  if(strpcmRequestStop==false) return(true);
  
  return(false);
}

