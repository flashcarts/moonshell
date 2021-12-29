
static char *pBGMPathAlias;
static UnicodeChar *pBGMPathUnicode;

typedef struct {
  char *pFilenameAlias;
  UnicodeChar *pFilenameUnicode;
} TBGMList;

static TBGMList *pBGMList;
static u32 BGMListCount;
static u32 BGMListIndex;

static u32 BGMResumeSaveTimeVSync;

static void BGM_Init(void)
{
  pBGMList=NULL;
  BGMListCount=0;
  BGMListIndex=0;
  pBGMPathAlias=NULL;
  pBGMPathUnicode=NULL;
  
  BGMResumeSaveTimeVSync=0;
}

static void BGM_CreateListFromPath(const UnicodeChar *pFilePathUnicode)
{
  const char *pBasePathAlias=ConvertFull_Unicode2Alias(pFilePathUnicode,NULL);
  
  if(FAT2_chdir_Alias(pBasePathAlias)==false){
    _consolePrintf("Fatal error: Can not change current path. [%s]\n",pBasePathAlias);
    ShowLogHalt();
  }
  
  pBGMPathAlias=str_AllocateCopy(pBasePathAlias);
  pBGMPathUnicode=Unicode_AllocateCopy(pFilePathUnicode);
  
  BGMListCount=0;
  
  {
    const char *pafn;
    u32 FAT_FileType=FAT2_FindFirstFile(&pafn);
    
    while(FAT_FileType!=FT_NONE){
      switch(FAT_FileType){
        case FT_NONE: break;
        case FT_DIR: break;
        case FT_FILE: {
          if(DLLList_isSupportFormatFromFilenameAlias(pafn)==EPT_Sound){
            BGMListCount++;
          }
        } break;
      }
      
      FAT_FileType=FAT2_FindNextFile(&pafn);
    }
  }
  
  if(BGMListCount==0){
    if(pBGMPathAlias!=NULL){
      safefree(pBGMPathAlias); pBGMPathAlias=NULL;
    }
    if(pBGMPathUnicode!=NULL){
      safefree(pBGMPathUnicode); pBGMPathUnicode=NULL;
    }
  }
  
  pBGMList=(TBGMList*)safemalloc(BGMListCount*sizeof(TBGMList));
  for(u32 idx=0;idx<BGMListCount;idx++){
    TBGMList *pbgml=&pBGMList[idx];
    pbgml->pFilenameAlias=NULL;
    pbgml->pFilenameUnicode=NULL;
  }
  
  BGMListCount=0;
  
  {
    const char *pafn;
    u32 FAT_FileType=FAT2_FindFirstFile(&pafn);
    
    while(FAT_FileType!=FT_NONE){
      switch(FAT_FileType){
        case FT_NONE: break;
        case FT_DIR: break;
        case FT_FILE: {
          if(DLLList_isSupportFormatFromFilenameAlias(pafn)==EPT_Sound){
            // _consolePrintf("Find file. %s [%s]\n",pafn,&pafn[strlen(pafn)-4]);
            const UnicodeChar *pufn=FAT2_GetLongFilenameUnicode();
            TBGMList *pbgml=&pBGMList[BGMListCount];
            pbgml->pFilenameAlias=str_AllocateCopy(pafn);
            pbgml->pFilenameUnicode=Unicode_AllocateCopy(pufn);
            BGMListCount++;
          }
        } break;
      }
      
      FAT_FileType=FAT2_FindNextFile(&pafn);
    }
  }
  
  for(u32 idx1=0;idx1<BGMListCount-1;idx1++){
    TBGMList *pbgml1=&pBGMList[idx1];
    for(u32 idx2=idx1+1;idx2<BGMListCount;idx2++){
      TBGMList *pbgml2=&pBGMList[idx2];
      if(isSwapFilenameUnicode(pbgml1->pFilenameUnicode,pbgml2->pFilenameUnicode)==true){
        TBGMList tmp=*pbgml1;
        *pbgml1=*pbgml2;
        *pbgml2=tmp;
      }
    }
  }
}

static u32 BGM_GetIndexFromFilename(const UnicodeChar *pFileNameUnicode)
{
  for(u32 idx=0;idx<BGMListCount;idx++){
    if(Unicode_isEqual(pBGMList[idx].pFilenameUnicode,pFileNameUnicode)==true) return(idx);
  }
  return(0);
}

static void BGM_PlayCurrentIndex(void)
{
  if(BGMListCount==0) return;
  
  _consolePrint("BGM_PlayCurrentIndex: DLLSound_Close.\n");
  DLLSound_Close();
  
  _consolePrint("BGM_PlayCurrentIndex: chdir.\n");
  if(FAT2_chdir_Alias(pBGMPathAlias)==false){
    _consolePrintf("Fatal error: Can not change current path. [%s]\n",pBGMPathAlias);
    ShowLogHalt();
  }
  
  _consolePrint("BGM_PlayCurrentIndex: DLLSound_Open.\n");
  DLLSound_Open(pBGMList[BGMListIndex].pFilenameAlias);
  
//  _consolePrint("BGM_PlayCurrentIndex: SetResumeMode.\n");
  Resume_SetResumeMode(ERM_Audio);
//  _consolePrint("BGM_PlayCurrentIndex: SetFilename.\n");
  Resume_SetFilename(ConvertFull_MargeFromSplit(pBGMPathUnicode,pBGMList[BGMListIndex].pFilenameUnicode));
//  _consolePrint("BGM_PlayCurrentIndex: SetPos.\n");
  Resume_SetPos(0);
  _consolePrint("BGM_PlayCurrentIndex: Save resume.\n");
  Resume_Save();
  
//  _consolePrint("BGM_PlayCurrentIndex: Set resume save timer.\n");
  BGMResumeSaveTimeVSync=1;
  
  RequestRefreshPlayCursorIndex=true;
}

static void BGM_Stop(bool WaitForEmpty)
{
  _consolePrint("BGM_Stop: Stop.\n");
  if(BGMListCount==0) return;
  
  _consolePrint("BGM_Stop: Wait for empty.\n");
  if(WaitForEmpty==true) DLLSound_WaitForStreamPCM();
  _consolePrint("BGM_Stop: DLLSound_Close.\n");
  DLLSound_Close();
  
  _consolePrint("BGM_Stop: SetResumeMode.\n");
  Resume_SetResumeMode(ERM_None);
  _consolePrint("BGM_Stop: Save resume.\n");
  Resume_Save();
  BGMResumeSaveTimeVSync=0;
  
  RequestRefreshPlayCursorIndex=true;
}

static void BGM_Free(void)
{
  if(BGMListCount==0) return;

  BGM_Stop(false);
  
  _consolePrint("BGM_Free: Free play list.\n");
  if(pBGMList!=NULL){
    for(u32 idx=0;idx<BGMListCount;idx++){
      TBGMList *pbgml=&pBGMList[idx];
      if(pbgml->pFilenameAlias!=NULL){
        safefree(pbgml->pFilenameAlias); pbgml->pFilenameAlias=NULL;
      }
      if(pbgml->pFilenameUnicode!=NULL){
        safefree(pbgml->pFilenameUnicode); pbgml->pFilenameUnicode=NULL;
      }
    }
    BGMListCount=0;
    safefree(pBGMList); pBGMList=NULL;
  }
  
  BGMListCount=0;
  BGMListIndex=0;
  
  _consolePrint("BGM_Free: Free BGMPath.\n");
  if(pBGMPathAlias!=NULL){
    safefree(pBGMPathAlias); pBGMPathAlias=NULL;
  }
  if(pBGMPathUnicode!=NULL){
    safefree(pBGMPathUnicode); pBGMPathUnicode=NULL;
  }
}

static void BGM_Start(const UnicodeChar *pFilePathUnicode,const UnicodeChar *pFileNameUnicode)
{
  if((BGMListCount==0)||(Unicode_isEqual(pFilePathUnicode,pBGMPathUnicode)==false)){
    BGM_Free();
    BGM_CreateListFromPath(pFilePathUnicode);
    if(BGMListCount==0){
      _consolePrintf("Fatal error: Can not found files.\n");
      ShowLogHalt();
      return;
    }
  }

  BGMListIndex=BGM_GetIndexFromFilename(pFileNameUnicode);
  GlobalPauseFlag=false;
  BGM_PlayCurrentIndex();
}

static void BGM_Prev(void)
{
  if(BGMListCount==0) return;
  
  BGM_Stop(false);
  
  if(BGMListIndex==0){
    BGMListIndex=BGMListCount-1;
    }else{
    BGMListIndex--;
  }
  
  GlobalPauseFlag=false;
  
  BGM_PlayCurrentIndex();
}

static void BGM_Next(void)
{
  if(BGMListCount==0) return;
  
  BGM_Stop(false);
  
  BGMListIndex++;
  if(BGMListIndex==BGMListCount) BGMListIndex=0;
  
  GlobalPauseFlag=false;
  
  BGM_PlayCurrentIndex();
}

static void BGM_NextRepeat(void)
{
  if(BGMListCount==0) return;
  
  BGM_Stop(false);
  
  GlobalPauseFlag=false;
  
  BGM_PlayCurrentIndex();
}

static void BGM_NextRandom(void)
{
  if(BGMListCount==0) return;
  
  BGM_Stop(false);
  
  GlobalPauseFlag=false;
  
  if(BGMListCount!=1){
    u32 lastidx=BGMListIndex;
    BGMListIndex=rand()%BGMListCount;
    while(lastidx==BGMListIndex){
      BGMListIndex=rand()%BGMListCount;
    }
  }
  
  BGM_PlayCurrentIndex();
}

static bool BGM_isOpened(void)
{
  if(BGMListCount==0) return(false);
  
  return(DLLSound_isOpened());
}

static u32 BGM_GetFilesCount(void)
{
  return(BGMListCount);
}

static u32 BGM_GetCurrentIndex(void)
{
  return(BGMListIndex);
}

static const UnicodeChar* BGM_GetCurrentFilename(void)
{
  return(pBGMList[BGMListIndex].pFilenameUnicode);
}

static const UnicodeChar* BGM_GetCurrentPath(void)
{
  return(pBGMPathUnicode);
}

