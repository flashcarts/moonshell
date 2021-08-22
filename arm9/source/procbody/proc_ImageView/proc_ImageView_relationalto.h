
typedef struct {
  UnicodeChar *pFilenameUnicode;
} TImageFile;

static s32 ImageFilesCount;
static TImageFile *pImageFiles;

static void ImageFiles_Free(void)
{
  if(pImageFiles!=NULL){
    for(s32 idx=0;idx<ImageFilesCount;idx++){
      TImageFile *pfile=&pImageFiles[idx];
      if(pfile->pFilenameUnicode!=NULL){
        safefree(pfile->pFilenameUnicode); pfile->pFilenameUnicode=NULL;
      }
    }
    safefree(pImageFiles); pImageFiles=NULL;
  }
  
  ImageFilesCount=0;
}

static bool isHiddenExt32(u32 Ext32)
{
  if(DLLList_isSupportFormatExt32(Ext32)==EPT_Image) return(false);
  
  return(true);
}

#define ATTRIB_HID	0x02

static void ImageFiles_Init(void)
{
  ImageFiles_Free();
  
  const char *pBasePathAlias=ConvertFull_Unicode2Alias(ProcState.FileList.CurrentPathUnicode,NULL);
  
  if((pBasePathAlias==NULL)||(FAT2_chdir_Alias(pBasePathAlias)==false)){
    _consolePrintf("Can not change path. [%s]\n",pBasePathAlias);
    ShowLogHalt();
  }
  
  ImageFilesCount=0;
  
  {
    const char *pafn;
    u32 FAT_FileType=FAT2_FindFirstFile(&pafn);
    
    while(FAT_FileType!=FT_NONE){
      if((FAT2_GetAttrib()&ATTRIB_HID)==0){
        switch(FAT_FileType){
          case FT_NONE: break;
          case FT_DIR: {
            if((strcmp(pafn,"..")==0)){
              ImageFilesCount++;
              }else{
              if(strcmp(pafn,".")!=0){
                ImageFilesCount++;
              }
            }
          } break;
          case FT_FILE: {
            u32 Ext32=0;
            {
              const char *ptmp=pafn;
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
            
            if(isHiddenExt32(Ext32)==false){
              ImageFilesCount++;
            }
          } break;
        }
      }
      
      FAT_FileType=FAT2_FindNextFile(&pafn);
    }
  }
  
  pImageFiles=(TImageFile*)safemalloc(sizeof(TImageFile)*ImageFilesCount);
  for(s32 idx=0;idx<ImageFilesCount;idx++){
    TImageFile *pfile=&pImageFiles[idx];
    pfile->pFilenameUnicode=NULL;
  }
  
  ImageFilesCount=0;
  
  {
    const char *pafn;
    u32 FAT_FileType=FAT2_FindFirstFile(&pafn);
    
    while(FAT_FileType!=FT_NONE){
      if((FAT2_GetAttrib()&ATTRIB_HID)==0){
        const UnicodeChar *pufn=FAT2_GetLongFilenameUnicode();
        if(pufn==NULL){
          _consolePrintf("FatalError: Can not read unicode filename.\n");
          ShowLogHalt();
          return;
        }
        switch(FAT_FileType){
          case FT_NONE: break;
          case FT_DIR: break;
          case FT_FILE: {
            u32 Ext32=0;
            {
              const char *ptmp=pafn;
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
            
            if(isHiddenExt32(Ext32)==false){
              TImageFile *pfile=&pImageFiles[ImageFilesCount];
              pfile->pFilenameUnicode=Unicode_AllocateCopy(pufn);
              ImageFilesCount++;
            }
          } break;
        }
      }
      
      FAT_FileType=FAT2_FindNextFile(&pafn);
    }
  }
  
  if(2<=ImageFilesCount){
    _consolePrint("Sort for filenames.\n");
    for(s32 idx0=0;idx0<ImageFilesCount-1;idx0++){
      for(s32 idx1=idx0+1;idx1<ImageFilesCount;idx1++){
        TImageFile *pf0=&pImageFiles[idx0];
        TImageFile *pf1=&pImageFiles[idx1];
        
        if(isSwapFilenameUnicode(&pf0->pFilenameUnicode[0],&pf1->pFilenameUnicode[0])==true){
          TImageFile ftemp=*pf0;
          *pf0=*pf1;
          *pf1=ftemp;
        }
      }
    }
    
    _consolePrint("End of sort.\n");
  }
  
  PrintFreeMem();
  _consolePrintf("Current folder refreshed.\n");
}

static u32 GetImageFilesCount(void)
{
  return(ImageFilesCount);
}

static u32 GetCurrentFileIndex(void)
{
  for(u32 idx=0;idx<ImageFilesCount;idx++){
    TImageFile *pfile=&pImageFiles[idx];
    if(Unicode_isEqual(pfile->pFilenameUnicode,RelationalFileNameUnicode)==true) return(idx);
  }
  
  return(0);
}

static const UnicodeChar* GetCurrentFilenameUnicode(u32 idx)
{
  TImageFile *pfile=&pImageFiles[idx];
  return(pfile->pFilenameUnicode);
}


