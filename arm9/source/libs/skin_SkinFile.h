
static const char SkinFileID[]="Skin files package for MoonShell2 type.3\0\0";

static bool SkinFile_Check(const UnicodeChar *pSkinFilenameUnicode)
{
  _consolePrint("SkinFile_Check(...);\n");
  
  if(Unicode_isEmpty(pSkinFilenameUnicode)==true) return(false);
  
  if(FullPath_FileExistsUnicode(pSkinFilenameUnicode)==false){
    _consolePrintf("Skin file not found.\n");
    return(false);
  }
  
  FAT_FILE *pSkinFile=Shell_FAT_fopen_FullPath(pSkinFilenameUnicode);
  
  if(pSkinFile==NULL){
    _consolePrintf("Open file failed.\n");
    return(false);
  }
  
  CStreamFS *pSkinFileStream=new CStreamFS(pSkinFile);

  if(pSkinFileStream==NULL){
    _consolePrintf("Can not create file stream.\n");
    FAT2_fclose(pSkinFile);
    return(false);
  }
  
  {
    u32 IDLength=pSkinFileStream->Readu8();
    if(64<IDLength) IDLength=64;
    char chkid[64+1];
    pSkinFileStream->ReadBuffer(chkid,IDLength+1);
    
    if(isStrEqual(SkinFileID,chkid)==false){
      _consolePrintf("Illigal ID. [%s]\n",chkid);
      _consolePrintf("Request ID. [%s]\n",SkinFileID);
      delete pSkinFileStream; pSkinFileStream=NULL;
      FAT2_fclose(pSkinFile);
      return(false);
    }
  }
  
  delete pSkinFileStream; pSkinFileStream=NULL;
  FAT2_fclose(pSkinFile);
  
  return(true);
}

typedef struct {
  u32 FilenameOffset;
  u32 DecompSize;
  u32 CompSize;
  u32 CompDataOffset;
} TSkinFileInfo;

typedef struct {
  void *pData;
  u32 DataSize;
} TSkinFileCache;

static FAT_FILE *pSkinFile=NULL;
static CStreamFS *pSkinFileStream=NULL;
static u32 SkinFileCount=0;
static TSkinFileInfo *pSkinFileInfo=NULL;
static TSkinFileCache *pSkinFileCache=NULL;

static bool SkinFile_Open(const UnicodeChar *pSkinFilenameUnicode)
{
  extmem_Start();
  
  _consolePrint("SkinFile_Open(...);\n");
  
  if(Unicode_isEmpty(pSkinFilenameUnicode)==true) return(false);
  
  if(FullPath_FileExistsUnicode(pSkinFilenameUnicode)==false){
    _consolePrintf("Skin file not found.\n");
    return(false);
  }
  
  pSkinFile=Shell_FAT_fopen_FullPath(pSkinFilenameUnicode);
  
  if(pSkinFile==NULL){
    _consolePrintf("Open file failed.\n");
    return(false);
  }
  
  pSkinFileStream=new CStreamFS(pSkinFile);

  if(pSkinFileStream==NULL){
    _consolePrintf("Can not create file stream.\n");
    FAT2_fclose(pSkinFile);
    return(false);
  }
  
  {
    u32 IDLength=pSkinFileStream->Readu8();
    if(64<IDLength) IDLength=64;
    char chkid[64+1];
    pSkinFileStream->ReadBuffer(chkid,IDLength+1);
    
    if(isStrEqual(SkinFileID,chkid)==false){
      _consolePrintf("Illigal ID. [%s]\n",chkid);
      _consolePrintf("Request ID. [%s]\n",SkinFileID);
      delete pSkinFileStream; pSkinFileStream=NULL;
      FAT2_fclose(pSkinFile);
      return(false);
    }
  }
  
  SkinFileCount=pSkinFileStream->Readu32();
  
  u32 HeaderSize=pSkinFileStream->Readu32();
  _consolePrintf("Header size=0x%x\n",HeaderSize);
  
  u32 ItemMaxCount=256;
  u32 HeaderMaxSize=(ItemMaxCount*sizeof(TSkinFileInfo))+2048; // with filename buffer.
  if(pSkinFileInfo==NULL){
    if(HeaderMaxSize<HeaderSize){
      _consolePrintf("Fatal error: Skin header size too long. %d,%d\n",HeaderSize,HeaderMaxSize);
      ShowLogHalt();
    }
    pSkinFileInfo=(TSkinFileInfo*)safemalloc(HeaderMaxSize);
    pSkinFileCache=(TSkinFileCache*)safemalloc(ItemMaxCount*sizeof(TSkinFileCache));
    if((pSkinFileInfo==NULL)||(pSkinFileCache==NULL)){
      _consolePrint("Fatal error: SkinFile info or cache memory overflow.\n");
      ShowLogHalt();
    }
  }
  pSkinFileStream->ReadBuffer(pSkinFileInfo,HeaderSize);
  MemSet32CPU(0,pSkinFileCache,ItemMaxCount*sizeof(TSkinFileCache));
  
  return(true);
}

static void SkinFile_Close(void)
{
  _consolePrint("SkinFile_Close();\n");
  
  if(pSkinFileStream!=NULL){
    delete pSkinFileStream; pSkinFileStream=NULL;
  }
  
  if(pSkinFile!=NULL){
    FAT2_fclose(pSkinFile); pSkinFile=NULL;
  }
  
/*
  if(pSkinFileInfo!=NULL){
    free(pSkinFileInfo); pSkinFileInfo=NULL;
    free(pSkinFileCache); pSkinFileCache=NULL;
  }
*/
  
  SkinFileCount=0;
}

static u32 SkinFile_GetFileIndexFromFilename(const char *pFilename)
{
  u32 FilenameLength=strlen(pFilename);
  
  for(u32 idx=0;idx<SkinFileCount;idx++){
    TSkinFileInfo *psfi=&pSkinFileInfo[idx];
    
    char *pfn=(char*)pSkinFileInfo;
    pfn=&pfn[psfi->FilenameOffset];
    
    u32 fnlen=(u8)*pfn;
    pfn++;
    
//    _consolePrintf("%d %d %d %s\n",psfi->FilenameOffset,idx,fnlen,pfn);
    
    if(FilenameLength==fnlen){
      if(isStrEqual(pFilename,pfn)==true) return(idx);
    }
  }
  
  return((u32)-1);
}

static void SkinFile_LoadFileAllocate(u32 SkinFileIndex,void **pbuf,s32 *psize)
{
  Splash_Update();
  
  TSkinFileInfo *psfi=&pSkinFileInfo[SkinFileIndex];
  
  void *pdummy=safemalloc(psfi->DecompSize*2);
  if(pdummy==NULL){
    _consolePrint("Fatal error: Allocate dummy memory failed.");
    ShowLogHalt();
  }
  
  if(0){
    u32 tmpbufsize=psfi->DecompSize;
    u8 *ptmpbuf=(u8*)safemalloc(tmpbufsize);
    
    safefree(pdummy); pdummy=NULL;
    
    pSkinFileStream->SetOffset(psfi->CompDataOffset);  
    pSkinFileStream->ReadBuffer(ptmpbuf,tmpbufsize);
    
    *pbuf=ptmpbuf;
    *psize=tmpbufsize;
    return;
  }
  
  TZLIBData zd;
  
  zd.DstSize=psfi->DecompSize;
  zd.pDstBuf=(u8*)safemalloc(zd.DstSize);
  zd.SrcSize=psfi->CompSize;
  zd.pSrcBuf=(u8*)safemalloc(zd.SrcSize);
  
  safefree(pdummy); pdummy=NULL;
  
  pSkinFileStream->SetOffset(psfi->CompDataOffset);  
  pSkinFileStream->ReadBuffer(zd.pSrcBuf,zd.SrcSize);
  
  Splash_Update();
  
  if((zd.pSrcBuf[0]==0x78)&&(zd.pSrcBuf[1]==0x01)){
    if(zlibdecompress(&zd)==false){
      _consolePrintf("Fatal error: ZLIB decompress error.\n");
      ShowLogHalt();
    }
    }else{
    MemCopy32CPU(zd.pSrcBuf,zd.pDstBuf,zd.DstSize);
  }
  
  Splash_Update();
  
  zd.SrcSize=0;
  safefree(zd.pSrcBuf); zd.pSrcBuf=NULL;
  
  *pbuf=zd.pDstBuf;
  *psize=zd.DstSize;
}

static void SkinFile_LoadB15(const char *pFilename,CglB15 **ppB15)
{
//  _consolePrintf("Load skin %s.\n",pFilename);
  
  u32 fidx=SkinFile_GetFileIndexFromFilename(pFilename);
  
  if(fidx==(u32)-1){
    _consolePrintf("Fatal error: Skin file '%s' not found.\n",pFilename);
    ShowLogHalt();
  }
  
  TSkinFileCache *psfc=&pSkinFileCache[fidx];
  
  if(psfc->pData!=NULL){
    *ppB15=new CglB15((u8*)psfc->pData,psfc->DataSize);
    return;
  }
  
  u8 *pbuf=NULL;
  s32 bufsize=0;
  
  SkinFile_LoadFileAllocate(fidx,(void**)&pbuf,&bufsize);
  *ppB15=new CglB15(pbuf,bufsize);
  
  psfc->pData=extmem_malloc(bufsize);
  if(psfc->pData!=NULL){
    psfc->DataSize=bufsize;
    MemCopy32CPU(pbuf,psfc->pData,(psfc->DataSize+3)&~3);
  }
  
  safefree(pbuf); pbuf=NULL;
}

static void SkinFile_LoadTGF(const char *pFilename,CglTGF **ppTGF)
{
//  _consolePrintf("Load skin %s.\n",pFilename);
  
  u32 fidx=SkinFile_GetFileIndexFromFilename(pFilename);
  
  if(fidx==(u32)-1){
    _consolePrintf("Fatal error: Skin file '%s' not found.\n",pFilename);
    ShowLogHalt();
  }
  
  TSkinFileCache *psfc=&pSkinFileCache[fidx];
  
  if(psfc->pData!=NULL){
    *ppTGF=new CglTGF((u8*)psfc->pData,psfc->DataSize);
    return;
  }
  
  u8 *pbuf=NULL;
  s32 bufsize=0;
  
  SkinFile_LoadFileAllocate(fidx,(void**)&pbuf,&bufsize);
  *ppTGF=new CglTGF(pbuf,bufsize);
  
  psfc->pData=extmem_malloc(bufsize);
  if(psfc->pData!=NULL){
    psfc->DataSize=bufsize;
    MemCopy32CPU(pbuf,psfc->pData,(psfc->DataSize+3)&~3);
  }
  
  safefree(pbuf); pbuf=NULL;
}

