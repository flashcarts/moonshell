
static CglCanvas *pCustomBG=NULL;

static void LoadCustomBG(void)
{
  if(pCustomBG!=NULL) return;
  
  FAT_FILE *pfh=Shell_FAT_fopen_Data(BGBMPFilename);
  if(pfh==NULL){
    _consolePrintf("Open for read failed.\n");
    return;
  }
  
  s32 w=ScreenWidth*2;
  s32 h=ScreenHeight*2;
  
  u32 BGBMPType;
  FAT2_fread(&BGBMPType,1,4,pfh);
  
  if(BGBMPType!=EBGBT_None){
    pCustomBG=new CglCanvas(NULL,w,h,pf15bit);
    if(pCustomBG==NULL){
      _consolePrintf("pCustomBG Memory overflow.\n");
      FAT2_fclose(pfh);
      return;
    }
  }
  
  switch(BGBMPType){
    case EBGBT_None: {
      delete pCustomBG; pCustomBG=NULL;
    } break;
    case EBGBT_8bit: {
      _consolePrint("Reload CustomBG 8bit.\n");
      u16 pal15[256];
      FAT2_fread(pal15,1,256*2,pfh);
      Splash_Update();
      u32 *ptmp=(u32*)safemalloc(w*h);
      if(ptmp!=NULL){
        FAT2_fread(ptmp,1,w*h,pfh);
        Splash_Update();
        u32 *pDstBuf=(u32*)pCustomBG->GetVRAMBuf();
        for(u32 idx=0;idx<w*h/4;idx++){
          u32 palidx=*ptmp++;
          u32 col;
          col=pal15[(palidx>>0)&0xff]<<0;
          col|=pal15[(palidx>>8)&0xff]<<16;
          *pDstBuf++=col;
          col=pal15[(palidx>>16)&0xff]<<0;
          col|=pal15[(palidx>>24)&0xff]<<16;
          *pDstBuf++=col;
        }
        ptmp-=w*h/4;
        safefree(ptmp); ptmp=NULL;
        Splash_Update();
      }
    } break;
    case EBGBT_15bit: {
      _consolePrint("Reload CustomBG 15bit.\n");
      for(u32 y=0;y<h;y+=64){
        Splash_Update();
        u16 *pDstBuf=pCustomBG->GetScanLine(y);
        FAT2_fread_fast(pDstBuf,1,w*64*2,pfh);
        Splash_Update();
      }
    } break;
    default: {
      delete pCustomBG; pCustomBG=NULL;
    } break;
  }
  
  FAT2_fclose(pfh);
}

