
static FAT_FILE* openwrite(const char *pprefix)
{
  for(u32 idx=0;idx<10;idx++){
    char fn[64];
    snprintf(fn,64,"%s%d.BMP",pprefix,idx);
    FAT_FILE *pf=FAT2_fopen_AliasForRead(fn);
    if(pf==NULL) return(FAT2_fopen_AliasForWrite(fn));
    FAT2_fclose(pf);
  }
  _consolePrintf("can not open file for write.\n");
  ShowLogHalt();
}

static void main_SavePreviewAndHalt(void)
{
  {
    u32 size;
    u8 *pbmp=pScreenMain->pViewCanvas->CreateBMPImage(&size);
    if(pbmp!=NULL){
      FAT_FILE *pf=openwrite("/PMAIN");
      if(pf!=NULL){
        FAT2_fwrite(pbmp,1,size,pf);
        FAT2_fclose(pf);
        _consolePrint("/prevmain.bmp saved.\n");
      }
      free(pbmp); pbmp=NULL;
    }
  }
  {
    u32 size;
    u8 *pbmp=pScreenMainOverlay->pCanvas->CreateBMPImage(&size);
    if(pbmp!=NULL){
      FAT_FILE *pf=openwrite("/POVER");
      if(pf!=NULL){
        FAT2_fwrite(pbmp,1,size,pf);
        FAT2_fclose(pf);
        _consolePrint("/prevover.bmp saved.\n");
      }
      free(pbmp); pbmp=NULL;
    }
  }
  {
    u32 size;
    u8 *pbmp=pScreenSub->pCanvas->CreateBMPImage(&size);
    if(pbmp!=NULL){
      FAT_FILE *pf=openwrite("/PSUB");
      if(pf!=NULL){
        FAT2_fwrite(pbmp,1,size,pf);
        FAT2_fclose(pf);
        _consolePrint("/prevsub.bmp saved.\n");
      }
      free(pbmp); pbmp=NULL;
    }
  }
  
  _consolePrint("saved preview.\n");
  ShowLogHalt();
  
}

