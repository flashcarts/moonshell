
static bool ChangedCurrentPath;

enum ENDSFile_FileType {ENFFT_UnknownFile,ENFFT_UpFolder,ENFFT_Folder,ENFFT_Sound,ENFFT_Image,ENFFT_Text,ENFFT_Video,ENFFT_NDSROM,ENFFT_Skin};

typedef struct {
  ENDSFile_FileType FileType;
  char *pFilenameAlias;
  u32 Ext32;
  UnicodeChar *pFilenameUnicode;
  UnicodeChar *pFilenameUnicode_DoubleLine0,*pFilenameUnicode_DoubleLine1;
  TNDSROMIcon *pNDSROMIcon;
  CglTGF *pIcon;
} TNDSFile;

static s32 NDSFilesCount;
static TNDSFile *pNDSFiles;

static bool NDSIconLoaded;

static void NDSFiles_Free(void)
{
  if(pNDSFiles!=NULL){
    for(s32 idx=0;idx<NDSFilesCount;idx++){
      TNDSFile *pfile=&pNDSFiles[idx];
      pfile->pFilenameAlias=NULL;
      pfile->pFilenameUnicode=NULL;
      pfile->pFilenameUnicode_DoubleLine0=NULL;
      pfile->pFilenameUnicode_DoubleLine1=NULL;
      if(pfile->pNDSROMIcon!=NULL){
        safefree(pfile->pNDSROMIcon); pfile->pNDSROMIcon=NULL;
      }
      pfile->pIcon=NULL;
    }
    pNDSFiles=NULL;
  }
  
  NDSFilesCount=0;
  
  FreeTextPool();
}

static bool isHiddenExt32(u32 Ext32)
{
  if(ProcState.FileList.HiddenNotSupportFileType==false) return(false);
  
  if(Ext32==MakeExt32(0,'M','P','3')) return(false);
  if(Ext32==MakeExt32(0,'J','P','G')) return(false);
  
  if(Ext32==MakeExt32(0,'N','D','S')) return(false);
  
  if(ExtLink_GetTargetIndex(Ext32)!=(u32)-1) return(false);
  
  if(Ext32==MakeExt32(0,'D','P','G')) return(false);
  
  if(Ext32==MakeExt32(0,'S','K','N')) return(false);
  
  if(Shell_isExistsSwapFile==true){
    if(Ext32==MakeExt32(0,'T','X','T')) return(false);
    if(Ext32==MakeExt32(0,'D','O','C')) return(false);
    if(Ext32==MakeExt32(0,'I','N','I')) return(false);
    if(Ext32==MakeExt32(0,0,0,'C')) return(false);
    if(Ext32==MakeExt32(0,'C','P','P')) return(false);
    if(Ext32==MakeExt32(0,0,0,'H')) return(false);
    if(Ext32==MakeExt32(0,'M','M','L')) return(false);
    if(Ext32==MakeExt32(0,'F','R','M')) return(false);
    if(Ext32==MakeExt32(0,'H','T','M')) return(false);
    if(Ext32==MakeExt32(0,'N','F','O')) return(false);
    if(Ext32==MakeExt32(0,'D','I','Z')) return(false);
  }
  
  switch(DLLList_isSupportFormatExt32(Ext32)){
    case EPT_None: break;
    case EPT_Image: {
      if(Shell_isExistsSwapFile==true) return(false);
    } break;
    case EPT_Sound: return(false); break;
  }
  
  return(true);
}

#define ATTRIB_HID	0x02

static void NDSFiles_RefreshCurrentFolder(void)
{
  NDSFiles_Free();
  
  NDSIconLoaded=false;
  
  InitTextPool();
  
  const char *pBasePathAlias=ConvertFull_Unicode2Alias(ProcState.FileList.CurrentPathUnicode,NULL);
  
  if((pBasePathAlias==NULL)||(FAT2_chdir_Alias(pBasePathAlias)==false)){
    _consolePrintf("Can not change path. [%s]\n",pBasePathAlias);
    ShowLogHalt();
  }
  
  NDSFilesCount=0;
  
  {
    const char *pafn;
    u32 FAT_FileType=FAT2_FindFirstFile(&pafn);
    
    while(FAT_FileType!=FT_NONE){
      if((FAT2_GetAttrib()&ATTRIB_HID)==0){
        switch(FAT_FileType){
          case FT_NONE: break;
          case FT_DIR: {
            if((strcmp(pafn,"..")==0)){
              NDSFilesCount++;
              }else{
              if(strcmp(pafn,".")!=0){
                NDSFilesCount++;
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
              NDSFilesCount++;
            }
          } break;
        }
      }
      
      FAT_FileType=FAT2_FindNextFile(&pafn);
    }
  }
  
  pNDSFiles=(TNDSFile*)GetTextPoolVoid(sizeof(TNDSFile)*NDSFilesCount);
  if(pNDSFiles==NULL){
    _consolePrint("Fatal error. (pNDSFiles) Memory overflow.\n");
    ShowLogHalt();
    return;
  }
  
  for(s32 idx=0;idx<NDSFilesCount;idx++){
    TNDSFile *pfile=&pNDSFiles[idx];
    pfile->FileType=ENFFT_UnknownFile;
    pfile->Ext32=0;
    pfile->pFilenameAlias=NULL;
    pfile->pFilenameUnicode=NULL;
    pfile->pFilenameUnicode_DoubleLine0=NULL;
    pfile->pFilenameUnicode_DoubleLine1=NULL;
    pfile->pNDSROMIcon=NULL;
    pfile->pIcon=NULL;
  }
  
  NDSFilesCount=0;
  
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
          case FT_DIR: {
            if((strcmp(pafn,"..")==0)){
              TNDSFile *pfile=&pNDSFiles[NDSFilesCount];
              pfile->FileType=ENFFT_UpFolder;
              pfile->Ext32=0;
              pfile->pFilenameAlias=TextPoolChar_AllocateCopy("..");
              /*
              UnicodeChar ustr[MaxFilenameLength];
              StrConvert_Ank2Unicode("..",ustr);
              pfile->pFilenameUnicode=TextPoolUnicode_AllocateCopy(ustr);
              */
              pfile->pFilenameUnicode=TextPoolUnicode_AllocateCopy(ProcState.FileList.CurrentPathUnicode);
              NDSFilesCount++;
              }else{
              if(strcmp(pafn,".")!=0){
                TNDSFile *pfile=&pNDSFiles[NDSFilesCount];
                pfile->FileType=ENFFT_Folder;
                pfile->Ext32=0;
                pfile->pFilenameAlias=TextPoolChar_AllocateCopy(pafn);
                pfile->pFilenameUnicode=TextPoolUnicode_AllocateCopy(pufn);
                NDSFilesCount++;
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
              TNDSFile *pfile=&pNDSFiles[NDSFilesCount];
              pfile->FileType=ENFFT_UnknownFile;
              pfile->Ext32=Ext32;
              pfile->pFilenameAlias=TextPoolChar_AllocateCopy(pafn);
              pfile->pFilenameUnicode=TextPoolUnicode_AllocateCopy(pufn);
              NDSFilesCount++;
            }
          } break;
        }
      }
      
      FAT_FileType=FAT2_FindNextFile(&pafn);
    }
  }
  
  if(2<=NDSFilesCount){
    _consolePrint("Sort for filenames.\n");
    for(s32 idx0=0;idx0<NDSFilesCount-1;idx0++){
      for(s32 idx1=idx0+1;idx1<NDSFilesCount;idx1++){
        TNDSFile *pf0=&pNDSFiles[idx0];
        TNDSFile *pf1=&pNDSFiles[idx1];
        ENDSFile_FileType ft0=pf0->FileType;
        ENDSFile_FileType ft1=pf1->FileType;
        
        if(ft0==ft1){
          if(isSwapFilenameUnicode(&pf0->pFilenameUnicode[0],&pf1->pFilenameUnicode[0])==true){
            TNDSFile ftemp=*pf0;
            *pf0=*pf1;
            *pf1=ftemp;
          }
          }else{
          if(ft0==ENFFT_UpFolder){
            }else{
            if(ft1==ENFFT_UpFolder){
              TNDSFile ftemp=*pf0;
              *pf0=*pf1;
              *pf1=ftemp;
              }else{
              if((ft0==ENFFT_UnknownFile)&&(ft1==ENFFT_Folder)){
                TNDSFile ftemp=*pf0;
                *pf0=*pf1;
                *pf1=ftemp;
              }
            }
          }
        }
      }
    }
    
    _consolePrint("End of sort.\n");
  }
  
  _consolePrintf("Load icons.\n");
  for(s32 idx=0;idx<NDSFilesCount;idx++){
    TNDSFile *pfile=&pNDSFiles[idx];
//    _consolePrintf("%d/%d %s",idx,NDSFilesCount,pfile->pFilenameAlias);
    
    if(pfile->FileType==ENFFT_UnknownFile){
      if(pfile->Ext32==MakeExt32(0,'N','D','S')) pfile->FileType=ENFFT_NDSROM;
    }
    
    if(ExtLink_GetTargetIndex(pfile->Ext32)!=(u32)-1) pfile->FileType=ENFFT_NDSROM;
    
    if(pfile->FileType==ENFFT_UnknownFile){
      if(pfile->Ext32==MakeExt32(0,'D','P','G')) pfile->FileType=ENFFT_Video;
    }
    
    if(pfile->FileType==ENFFT_UnknownFile){
      if(pfile->Ext32==MakeExt32(0,'S','K','N')) pfile->FileType=ENFFT_Skin;
    }
    
    if(pfile->FileType==ENFFT_UnknownFile){
      if(pfile->Ext32==MakeExt32(0,'T','X','T')) pfile->FileType=ENFFT_Text;
      if(pfile->Ext32==MakeExt32(0,'D','O','C')) pfile->FileType=ENFFT_Text;
      if(pfile->Ext32==MakeExt32(0,'I','N','I')) pfile->FileType=ENFFT_Text;
      if(pfile->Ext32==MakeExt32(0,0,0,'C')) pfile->FileType=ENFFT_Text;
      if(pfile->Ext32==MakeExt32(0,'C','P','P')) pfile->FileType=ENFFT_Text;
      if(pfile->Ext32==MakeExt32(0,0,0,'H')) pfile->FileType=ENFFT_Text;
      if(pfile->Ext32==MakeExt32(0,'M','M','L')) pfile->FileType=ENFFT_Text;
      if(pfile->Ext32==MakeExt32(0,'F','R','M')) pfile->FileType=ENFFT_Text;
      if(pfile->Ext32==MakeExt32(0,'H','T','M')) pfile->FileType=ENFFT_Text;
      if(pfile->Ext32==MakeExt32(0,'N','F','O')) pfile->FileType=ENFFT_Text;
      if(pfile->Ext32==MakeExt32(0,'D','I','Z')) pfile->FileType=ENFFT_Text;
    }
    
    if(pfile->FileType==ENFFT_UnknownFile){
      switch(DLLList_isSupportFormatExt32(pfile->Ext32)){
        case EPT_None: break;
        case EPT_Image: pfile->FileType=ENFFT_Image; break;
        case EPT_Sound: pfile->FileType=ENFFT_Sound; break;
      }
    }
    
    switch(pfile->FileType){
      case ENFFT_UnknownFile: pfile->pIcon=FileListAlpha_GetSkin(EFLSA_Icon_UnknownFile); break;
      case ENFFT_UpFolder: pfile->pIcon=FileListAlpha_GetSkin(EFLSA_Icon_UpFolder); break;
      case ENFFT_Folder: pfile->pIcon=FileListAlpha_GetSkin(EFLSA_Icon_Folder); break;
      case ENFFT_Sound: pfile->pIcon=FileListAlpha_GetSkin(EFLSA_Icon_Sound); break;
      case ENFFT_Image: pfile->pIcon=FileListAlpha_GetSkin(EFLSA_Icon_Image); break;
      case ENFFT_Text: pfile->pIcon=FileListAlpha_GetSkin(EFLSA_Icon_Text); break;
      case ENFFT_Video: pfile->pIcon=FileListAlpha_GetSkin(EFLSA_Icon_Video); break;
      case ENFFT_NDSROM: break; // load after.
      case ENFFT_Skin: pfile->pIcon=FileListAlpha_GetSkin(EFLSA_Icon_Skin); break;
      default: {
        _consolePrintf("Internal error: Illigal FileType. (%d)\n",pfile->FileType);
        ShowLogHalt();
      }
    }
  }
  
  CglCanvas *pbm=new CglCanvas(NULL,1,1,pf15bit);
  pbm->SetCglFont(pCglFontDefault);
  
  for(s32 idx=0;idx<NDSFilesCount;idx++){
    TNDSFile *pfile=&pNDSFiles[idx];
    
    pfile->pFilenameUnicode_DoubleLine0=NULL;
    pfile->pFilenameUnicode_DoubleLine1=NULL;
    if(Unicode_isEmpty(pfile->pFilenameUnicode)==false){
      bool HideExt=ProcState.FileList.HiddenFilenameExt;
      if(HideExt==true){
        if((pfile->FileType==ENFFT_UpFolder)||(pfile->FileType==ENFFT_Folder)) HideExt=false;
      }
      
      UnicodeChar *psrc=pfile->pFilenameUnicode;
      if((HideExt==true)){
        psrc=Unicode_AllocateCopy(psrc);
        u32 dotpos=0;
        u32 idx=0;
        while(psrc[idx]!=0){
          if(psrc[idx]==(UnicodeChar)'.') dotpos=idx;
          idx++;
        }
        if(dotpos!=0) psrc[dotpos]=0;
      }
      u32 srclen=Unicode_GetLength(psrc);
      u32 tx=NDSROMIconXMargin+NDSROMIcon32Width+NDSROMIconXMargin;
      if((tx+pbm->GetTextWidthW(psrc))<=ScreenWidth){
        pfile->pFilenameUnicode_DoubleLine0=GetTextPoolUnicode(srclen);
        Unicode_Copy(pfile->pFilenameUnicode_DoubleLine0,psrc);
        pfile->pFilenameUnicode_DoubleLine1=NULL;
        }else{
        u32 limlen=0;
        while(1){
          limlen++;
          UnicodeChar ustr[MaxFilenameLength+1];
          for(u32 idx=0;idx<limlen;idx++){
            ustr[idx]=psrc[idx];
          }
          ustr[limlen]=0;
          if(ScreenWidth<(tx+pbm->GetTextWidthW(ustr))){
            limlen--;
            break;
          }
        }
        pfile->pFilenameUnicode_DoubleLine0=GetTextPoolUnicode(limlen);
        pfile->pFilenameUnicode_DoubleLine1=GetTextPoolUnicode(srclen-limlen);
        for(u32 idx=0;idx<limlen;idx++){
          pfile->pFilenameUnicode_DoubleLine0[idx]=psrc[idx];
        }
        pfile->pFilenameUnicode_DoubleLine0[limlen]=0;
        u32 ofs=limlen;
        limlen=srclen-limlen;
        for(u32 idx=0;idx<limlen;idx++){
          pfile->pFilenameUnicode_DoubleLine1[idx]=psrc[ofs+idx];
        }
        pfile->pFilenameUnicode_DoubleLine1[limlen]=0;
      }
      if(HideExt==true){
        safefree(psrc); psrc=NULL;
      }
    }
  }
  
  if(pbm!=NULL){
    delete pbm; pbm=NULL;
  }
  
  EndTextPool();
  PrintFreeMem();
  _consolePrintf("Current folder refreshed.\n");
}

static void NDSFiles_LoadNDSIcon(TNDSFile *pNDSFile)
{
  if(pNDSFile->FileType!=ENFFT_NDSROM) return;
  
  if((pNDSFile->pIcon!=NULL)||(pNDSFile->pNDSROMIcon!=NULL)) return;
  
  if(ChangedCurrentPath==true){
    ChangedCurrentPath=false;
    if(PathExistsUnicode(ProcState.FileList.CurrentPathUnicode)==false){
      _consolePrintf("FatalError: Can not move current folder.\n");
      ShowLogHalt();
    }
  }
  
  pNDSFile->pNDSROMIcon=(TNDSROMIcon*)safemalloc(sizeof(TNDSROMIcon));
  
  if(pNDSFile->pNDSROMIcon==NULL){
    _consolePrintf("NDSFiles_RefreshCurrentFolder NDSROM icon buffer memory over flow.\n");
    pNDSFile->pIcon=FileListAlpha_GetSkin(EFLSA_Icon_NDSROM);
    return;
  }
  
  if(pNDSFile->Ext32==MakeExt32(0,'N','D','S')){
    if(NDSROMIcon_Get(pNDSFile->pFilenameAlias,pNDSFile->pNDSROMIcon)==false){
      safefree(pNDSFile->pNDSROMIcon); pNDSFile->pNDSROMIcon=NULL;
      pNDSFile->pIcon=FileListAlpha_GetSkin(EFLSA_Icon_NDSROM);
    }
    return;
  }
  
  u32 extlinkidx=ExtLink_GetTargetIndex(pNDSFile->Ext32);
  
  if(extlinkidx==(u32)-1){
    safefree(pNDSFile->pNDSROMIcon); pNDSFile->pNDSROMIcon=NULL;
    pNDSFile->pIcon=FileListAlpha_GetSkin(EFLSA_Icon_NDSROM);
    return;
  }
  
  const char *pAlias=ConvertFullPath_Unicode2Alias(ExtLink_GetNDSFullPathFilenameUnicode(extlinkidx));
  
  ChangedCurrentPath=true;
  
  if(pAlias==NULL){
    _consolePrintf("ExtLink: Not found extlink nds file.\n");
    safefree(pNDSFile->pNDSROMIcon); pNDSFile->pNDSROMIcon=NULL;
    pNDSFile->pIcon=FileListAlpha_GetSkin(EFLSA_Icon_NDSROM);
    return;
  }
  
  if(NDSROMIcon_Get(pAlias,pNDSFile->pNDSROMIcon)==false){
    safefree(pNDSFile->pNDSROMIcon); pNDSFile->pNDSROMIcon=NULL;
    pNDSFile->pIcon=FileListAlpha_GetSkin(EFLSA_Icon_NDSROM);
  }
}
