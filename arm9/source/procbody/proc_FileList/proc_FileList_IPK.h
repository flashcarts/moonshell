
static void InitIPK(void)
{
  IPKLoadFlag=false;
  pIPKFile=NULL;
  pIPKFilenameUncode=NULL;
  pIPK=NULL;
  pIPKThumbnail=NULL;
}

static void FreeIPK(void)
{
  if(pIPKFile!=NULL){
    FAT2_fclose(pIPKFile); pIPKFile=NULL;
  }
  
  if(pIPK!=NULL){
    delete pIPK; pIPK=NULL;
  }
  
  if(pIPKThumbnail!=NULL){
    if(pIPKThumbnail->pBuf!=NULL){
      safefree(pIPKThumbnail->pBuf); pIPKThumbnail->pBuf=NULL;
    }
    safefree(pIPKThumbnail); pIPKThumbnail=NULL;
  }
  
  IPKLoadFlag=false;
}

static bool CheckThumbnailSize(TIPKThumbnail *pth)
{
  if((pth->Width!=ScreenWidth)&&(pth->Width!=ScreenHeight)) return(false);
  return(true);
}

static void ReloadIPK(s32 fileidx)
{
  FreeIPK();
  
  TNDSFile *pndsf=&pNDSFiles[fileidx];
  
  if(pndsf->FileType!=ENFFT_NDSROM) return;
  
  {
    u32 Ext32=0;
    {
      const char *ptmp=pndsf->pFilenameAlias;
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
    
    if(Ext32!=MakeExt32(0,'I','P','K')) return;
  }
  
  const UnicodeChar *pfnw=pndsf->pFilenameUnicode;
  pIPKFile=FAT2_fopen_AliasForRead(ConvertFull_Unicode2Alias(ProcState.FileList.CurrentPathUnicode,pfnw));
  
  if(FileCheck_isIPK(pIPKFile)==false){
    FreeIPK();
    return;
  }
  
  pIPK=new CIPK(pIPKFile);
  
  pIPKThumbnail=(TIPKThumbnail*)safemalloc(sizeof(TIPKThumbnail));
  pIPKThumbnail->pBuf=NULL;
  
  if(pIPKThumbnail->pBuf==NULL){
    if(pIPK->GetCoverImage(pIPKThumbnail)==true){
      _consolePrintf("IPK: Cover image loaded.\n");
    }
    if(CheckThumbnailSize(pIPKThumbnail)==false){
      _consolePrintf("IPK: Cover image ignored. (%dx%d)\n",pIPKThumbnail->Width,pIPKThumbnail->Height);
      if(pIPKThumbnail->pBuf!=NULL){
        safefree(pIPKThumbnail->pBuf); pIPKThumbnail->pBuf=NULL;
      }
    }
  }
  
  if(pIPKThumbnail->pBuf==NULL){
    if(pIPK->GetThumbnail(0,EIPKTHID_256192,pIPKThumbnail)==true){
      _consolePrintf("IPK: Horizontal thumbnail loaded.\n");
    }
    if(CheckThumbnailSize(pIPKThumbnail)==false){
      _consolePrintf("IPK: Horizontal thumbnail ignored. (%dx%d)\n",pIPKThumbnail->Width,pIPKThumbnail->Height);
      if(pIPKThumbnail->pBuf!=NULL){
        safefree(pIPKThumbnail->pBuf); pIPKThumbnail->pBuf=NULL;
      }
    }
  }
  
  if(pIPKThumbnail->pBuf==NULL){
    if(pIPK->GetThumbnail(0,EIPKTHID_192256,pIPKThumbnail)==true){
      _consolePrintf("IPK: Vertical thumbnail loaded.\n");
    }
    if(CheckThumbnailSize(pIPKThumbnail)==false){
      _consolePrintf("IPK: Vertical thumbnail ignored. (%dx%d)\n",pIPKThumbnail->Width,pIPKThumbnail->Height);
      if(pIPKThumbnail->pBuf!=NULL){
        safefree(pIPKThumbnail->pBuf); pIPKThumbnail->pBuf=NULL;
      }
    }
  }
  
  if(pIPKThumbnail->pBuf==NULL){
    FreeIPK();
    return;
  }
  
  _consolePrintf("IPK: w=%d, h=%d, pbuf=0x%08x.\n",pIPKThumbnail->Width,pIPKThumbnail->Height,pIPKThumbnail->pBuf);
  
  pIPKFilenameUncode=pndsf->pFilenameUnicode;
  IPKLoadFlag=true;
  
  _consolePrintf("IPK: Loaded.\n");
}

static void DrawIPKInfo(CglCanvas *pcan)
{
  u32 x0=8,y0=4;
  char str0[64];
  snprintf(str0,64,"Preview IPK package. (%dfiles)",pIPK->GetFilesCount());
  
  u32 x1=8,y1=ScreenHeight-glCanvasTextHeight-4;
  const UnicodeChar *pstr1=pIPKFilenameUncode;
  
  pcan->SetFontTextColor(RGB15(0,0,0)|BIT15);
  for(s32 px=-1;px<=1;px++){
    for(s32 py=-1;py<=1;py++){
      if((px!=0)||(py!=0)){
        pcan->TextOutA(x0+px,y0+py,str0);
        pcan->TextOutW(x1+px,y1+py,pstr1);
      }
    }
  }
  
  pcan->SetFontTextColor(RGB15(24,24,24)|BIT15);
  pcan->TextOutA(x0,y0,str0);
  pcan->TextOutW(x1,y1,pstr1);
}

static bool DrawIPK(CglCanvas *pcan)
{
  if(IPKLoadFlag==false) return(false);
  
  TIPKThumbnail *pth=pIPKThumbnail;
  
  if(pth->pBuf==NULL) return(false);
  
  u16 BGColor=RGB15(8,8,8)|BIT15;
  
  u32 canw=pcan->GetWidth();
  u32 canh=pcan->GetHeight();
  
  if(pth->Width==canw){
    u16 *psrcbm=pth->pBuf;
    u16 *pdstbm=pcan->GetVRAMBuf();
    s32 ofs=0;
    s32 height=pth->Height;
    if(canh<height){
      height=canh;
      }else{
      ofs=(canh-pth->Height)/2;
      if(ofs!=0){
        MemSet16CPU(BGColor,&pdstbm[0*canw],ofs*canw*2);
        MemSet16CPU(BGColor,&pdstbm[(ofs+height)*canw],(canh-ofs-height)*canw*2);
      }
    }
    pdstbm+=ofs*canw;
    MemCopy32CPU(psrcbm,pdstbm,canw*height*2);
    DrawIPKInfo(pcan);
    return(true);
  }
  
  if(pth->Width==canh){
    u16 *psrcbm=pth->pBuf;
    u16 *pdstbm=pcan->GetVRAMBuf();
    s32 ofs=0;
    s32 width=pth->Height;
    if(canw<width){
      width=canw;
      }else{
      ofs=(canw-pth->Height)/2;
      if(ofs!=0){
        for(u32 y=0;y<canh;y++){
          MemSet16CPU(BGColor,&pdstbm[y*canw+0],ofs*2);
          MemSet16CPU(BGColor,&pdstbm[y*canw+(canw-ofs)],ofs*2);
        }
        pdstbm+=ofs;
      }
    }
    for(u32 x=0;x<pth->Height;x++){
      for(u32 y=0;y<pth->Width;y++){
        u16 c=psrcbm[(x*pth->Width)+y];
        pdstbm[(y*canw)+x]=c;
      }
    }
    DrawIPKInfo(pcan);
    return(true);
  }
  
  return(false);
}

