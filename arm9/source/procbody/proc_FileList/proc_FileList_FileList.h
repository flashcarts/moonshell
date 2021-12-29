
extern "C" {
void VRAMWriteCache_Enable(void);
void VRAMWriteCache_Disable(void);
}

enum EMP3Cnt_WindowState {EMCWS_Hide,EMCWS_Show};

static EMP3Cnt_WindowState MP3Cnt_WindowState;
static s32 MP3Cnt_PosX;
static vu32 MP3Cnt_PosForVSync;

#define MP3Cnt_Width (64)
#define MP3Cnt_Height (ScreenHeight)

static void FileList_DrawMP3Cnt(CglCanvas *pDstBM)
{
  CglCanvas *pTmpBM=new CglCanvas(NULL,MP3Cnt_Width,MP3Cnt_Height,pf15bit);
  if(pTmpBM==NULL){
    _consolePrint("Fatal error: FileList_DrawMP3Cnt memory overflow.\n");
    ShowLogHalt();
  }
  
  pDstBM->BitBlt(pTmpBM,0,0,MP3Cnt_Width,MP3Cnt_Height,ScreenWidth-MP3Cnt_Width,0,false);
  
  CglTGF *pbm;
  
  u32 y=0;
  
  pbm=MP3CntAlpha_GetSkin(EMP3SA_p0);
  pbm->BitBlt(pTmpBM,0,y);
  y+=pbm->GetHeight();
  
  pbm=MP3CntAlpha_GetSkin(EMP3SA_p1_prev);
  pbm->BitBlt(pTmpBM,0,y);
  y+=pbm->GetHeight();
  
  if(BGM_isOpened()==false){
    pbm=MP3CntAlpha_GetSkin(EMP3SA_p2_stop);
    }else{
    if(GlobalPauseFlag==false){
      pbm=MP3CntAlpha_GetSkin(EMP3SA_p2_pause);
      }else{
      pbm=MP3CntAlpha_GetSkin(EMP3SA_p2_play);
    }
  }
  pbm->BitBlt(pTmpBM,0,y);
  y+=pbm->GetHeight();
  
  pbm=MP3CntAlpha_GetSkin(EMP3SA_p3_next);
  pbm->BitBlt(pTmpBM,0,y);
  y+=pbm->GetHeight();
  
  {
    TProcState_FileList *pfl=&ProcState.FileList;
    switch(pfl->PlayMode){
      case EPSFLPM_Repeat: pbm=MP3CntAlpha_GetSkin(EMP3SA_p4_repeat); break;
      case EPSFLPM_AllRep: pbm=MP3CntAlpha_GetSkin(EMP3SA_p4_allrep); break;
      case EPSFLPM_Random: pbm=MP3CntAlpha_GetSkin(EMP3SA_p4_random); break;
      default: pbm=MP3CntAlpha_GetSkin(EMP3SA_p4_repeat); break;
    }
  }
  pbm->BitBlt(pTmpBM,0,y);
  y+=pbm->GetHeight();
  
  pTmpBM->BitBlt(pScreenMainOverlay->pCanvas,ScreenWidth-MP3Cnt_Width,0,MP3Cnt_Width,MP3Cnt_Height,0,0,false);
  
  if(pTmpBM!=NULL){
    delete pTmpBM; pTmpBM=NULL;
  }
}

asm u16* FileList_DrawID3Tag_asm_Fill25per(u16 *pbuf,u32 size)
{
  push {r4,r5,r6,lr}
  
  ldr r2,=0x739C739C // RGB15(28,28,28)<<16 | RGB15(28,28,28)
  ldr r3,=(1<<15)|(1<<31)
  
FileList_DrawID3Tag_asm_Fill25per_loop
  
  ldmia r0,{r4,r5,r6,lr}
  and r4,r4,r2
  orr r4,r3,r4,lsr #2
  and r5,r5,r2
  orr r5,r3,r5,lsr #2
  and r6,r6,r2
  orr r6,r3,r6,lsr #2
  and lr,lr,r2
  orr lr,r3,lr,lsr #2
  stmia r0!,{r4,r5,r6,lr}
  
  subs r1,#2*4
  bne FileList_DrawID3Tag_asm_Fill25per_loop
  
  pop {r4,r5,r6,pc}
}

asm u16* FileList_DrawID3Tag_asm_Fill50per(u16 *pbuf,u32 size)
{
  push {r4,r5,r6,lr}
  
  ldr r2,=0x7BDE7BDE // RGB15(30,30,30)<<16 | RGB15(30,30,30)
  ldr r3,=(1<<15)|(1<<31)
  
FileList_DrawID3Tag_asm_Fill50per_loop
  
  ldmia r0,{r4,r5,r6,lr}
  and r4,r4,r2
  orr r4,r3,r4,lsr #1
  and r5,r5,r2
  orr r5,r3,r5,lsr #1
  and r6,r6,r2
  orr r6,r3,r6,lsr #1
  and lr,lr,r2
  orr lr,r3,lr,lsr #1
  stmia r0!,{r4,r5,r6,lr}
  
  subs r1,#2*4
  bne FileList_DrawID3Tag_asm_Fill50per_loop
  
  pop {r4,r5,r6,pc}
}

static void FileList_DrawID3Tag(CglCanvas *pDstBM,bool BlackMode)
{
  if(BGM_isOpened()==false) return;
  
  u32 LinesCount=1; // for Filename.
  
  if(ID3Tag.Exists==false){
    LinesCount++; // for Not exists text.
    }else{
    LinesCount+=ID3Tag.LinesExistsCount;
  }
  
  u32 x=8,y=8,h=glCanvasTextHeight+2;
  
  {
    const u32 DstBMWidth=ScreenWidth; // pDstBM->GetWidth();
    
    u16 *pb=pDstBM->GetVRAMBuf();
    u32 size;
    
    size=4*DstBMWidth;
    pb+=size;
    
    size=2*DstBMWidth;
    pb=FileList_DrawID3Tag_asm_Fill50per(pb,size);
    
    size=(2+(h*LinesCount)+0)*DstBMWidth;
    pb=FileList_DrawID3Tag_asm_Fill25per(pb,size);
    
    {
      size=DstBMWidth;
      s32 posx;
      {
        u32 ofs=DLLSound_GetPosOffset(),max=DLLSound_GetPosMax();
        if((8*1024*1024)<=max){
          ofs/=1024;
          max/=1024;
        }
        posx=(ofs*size)/max;
      }
      if(BlackMode==false){
        for(u32 idx=0;idx<posx;idx++){
          pb[size*0]=((pb[size*0]&RGB15(30,30,30))>>1) |BIT15;
          pb[size*1]=((pb[size*1]&RGB15(30,30,30))>>1) |BIT15;
          pb++;
        }
        }else{
        u16 addcol=RGB15(5,4,4);
        for(u32 idx=0;idx<posx;idx++){
          pb[size*0]=((pb[size*0]&RGB15(30,30,30))>>1) +addcol |BIT15;
          pb[size*1]=((pb[size*1]&RGB15(30,30,30))>>1) +addcol |BIT15;
          pb++;
        }
      }
      for(u32 idx=posx;idx<size;idx++){
        pb[size*0]=((pb[size*0]&RGB15(28,28,28))>>2) |BIT15;
        pb[size*1]=((pb[size*1]&RGB15(28,28,28))>>2) |BIT15;
        pb++;
      }
      pb+=size;
    }
    
    size=2*DstBMWidth;
    pb=FileList_DrawID3Tag_asm_Fill25per(pb,size);
    
    size=2*DstBMWidth;
    pb=FileList_DrawID3Tag_asm_Fill50per(pb,size);
  }
  
  y++;
  
//#define UseShadow

#ifdef UseShadow
  u16 collow=RGB15(0,0,0)|BIT15;
#endif
  u16 colhigh;
  
  u32 ScreenSaver_BlackMode=ProcState.FileList.ScreenSaver_BlackMode;
  if(Backlight_isStandby()==false) ScreenSaver_BlackMode=false;
  
  if(ScreenSaver_BlackMode==false){
    colhigh=ColorTable.FileList.ID3TagWhiteModeText;
    }else{
    colhigh=ColorTable.FileList.ID3TagBlackModeText;
  }
  
  { // for Filename.
    char idxstr[32];
    snprintf(idxstr,32,"%d/%d",1+BGM_GetCurrentIndex(),BGM_GetFilesCount());
#ifdef UseShadow
    pDstBM->SetFontTextColor(collow);
    pDstBM->TextOutA(x+1,y+1,idxstr);
#endif
    pDstBM->SetFontTextColor(colhigh);
    pDstBM->TextOutA(x+0,y+0,idxstr);
    u32 w=pDstBM->GetTextWidthA(idxstr)+4;

    const UnicodeChar *pstr=BGM_GetCurrentFilename();
#ifdef UseShadow
    pDstBM->SetFontTextColor(collow);
    pDstBM->TextOutW(x+w+1,y+1,pstr);
#endif
    pDstBM->SetFontTextColor(colhigh);
    pDstBM->TextOutW(x+w+0,y+0,pstr);
    y+=h;
    LinesCount--;
  }
  
  u32 last0width=0,last1width=0;
  
  if(ID3Tag.Exists==false){
    const char *pstr="Not found ID3Tag.";
#ifdef UseShadow
    pDstBM->SetFontTextColor(collow);
    pDstBM->TextOutA(x+1,y+1,pstr);
#endif
    pDstBM->SetFontTextColor(colhigh);
    pDstBM->TextOutA(x+0,y+0,pstr);
    y+=h;
    
    }else{
    u32 lineindex=0;
    
    for(u32 idx=0;idx<LinesCount;idx++){
      while(ID3Tag.Lines[lineindex].Exists==false){
        lineindex++;
      }
      
      TID3Tag_Line *pi3tl=&ID3Tag.Lines[lineindex];
      // _consolePrintf("ID3draw: %d,%d %s\n",idx,lineindex,pi3tl->pStrL);
      
      if(pi3tl->StrW[0]==0){
        if(Shell_isJPmode()==true){
          SJIS2Unicode_Convert(pi3tl->pStrL,pi3tl->StrW);
          }else{
          StrConvert_UTF82Unicode(pi3tl->pStrL,pi3tl->StrW);
        }
      }
      
      last0width=last1width;
      last1width=pDstBM->GetTextWidthW(pi3tl->StrW);
  
      const UnicodeChar *pstr=pi3tl->StrW;
#ifdef UseShadow
      pDstBM->SetFontTextColor(collow);
      pDstBM->TextOutW(x+1,y+1,pstr);
#endif
      pDstBM->SetFontTextColor(colhigh);
      pDstBM->TextOutW(x+0,y+0,pstr);
      y+=h;
      
      lineindex++;
    }
  }
  
  u32 BitRatePerByte=DLLSoung_GetBitRatePerByte();
  u32 cursec=DLLSound_GetCurrentTimeSec(BitRatePerByte),playsec=DLLSound_GetPlayTimeSec(BitRatePerByte);
  // _consolePrintf("%d,%d,%d %dkbps\n",LinesCount,cursec,playsec,BitRatePerByte*8);

  if(playsec!=0){
    if(LinesCount==1){
      y-=h;
      u32 x=ScreenWidth-102;
      char str[64];
      
      x-=32;
      snprintf(str,64,"%d%%",(strpcmGetVolume64()*100)/64);
#ifdef UseShadow
      pDstBM->SetFontTextColor(collow);
      pDstBM->TextOutA(x+1,y+1,str);
#endif
      pDstBM->SetFontTextColor(colhigh);
      pDstBM->TextOutA(x+0,y+0,str);
      x+=32;
      
      snprintf(str,64,"%d:%02d:%02d / %d:%02d:%02d",cursec/60/60,(cursec/60)%60,cursec%60,playsec/60/60,(playsec/60)%60,playsec%60);
#ifdef UseShadow
      pDstBM->SetFontTextColor(collow);
      pDstBM->TextOutA(x+1,y+1,str);
#endif
      pDstBM->SetFontTextColor(colhigh);
      pDstBM->TextOutA(x+0,y+0,str);
      y+=h;
      }else{
      y-=h*2;
      u32 x=ScreenWidth-48;
      char str[64];
      
      snprintf(str,64,"%d:%02d:%02d",cursec/60/60,(cursec/60)%60,cursec%60);
#ifdef UseShadow
      pDstBM->SetFontTextColor(collow);
      pDstBM->TextOutA(x+1,y+1,str);
#endif
      pDstBM->SetFontTextColor(colhigh);
      pDstBM->TextOutA(x+0,y+0,str);
      y+=h;
      
      snprintf(str,64,"%d:%02d:%02d",playsec/60/60,(playsec/60)%60,playsec%60);
#ifdef UseShadow
      pDstBM->SetFontTextColor(collow);
      pDstBM->TextOutA(x+1,y+1,str);
#endif
      pDstBM->SetFontTextColor(colhigh);
      pDstBM->TextOutA(x+0,y+0,str);
      y+=h;
      
      if(last0width<last1width){
        y-=h*2;
        }else{
        y-=h*1;
      }
      x-=32;
      snprintf(str,64,"%d%%",(strpcmGetVolume64()*100)/64);
#ifdef UseShadow
      pDstBM->SetFontTextColor(collow);
      pDstBM->TextOutA(x+1,y+1,str);
#endif
      pDstBM->SetFontTextColor(colhigh);
      pDstBM->TextOutA(x+0,y+0,str);
      x+=32;
    }
  }
  
#undef UseShadow
}

static void FileList_DrawBG_DrawNDSFile(CglCanvas *pItemBM,TNDSFile *pNDSFile,bool Selected,bool isPlay)
{
  CglTGF *psrcbm=NULL;
  
  if(Selected==false){
    psrcbm=FileListAlpha_GetSkin(EFLSA_ItemBG_Clear);
    }else{
    psrcbm=FileListAlpha_GetSkin(EFLSA_ItemBG_Select);
  }
  
  DrawSkinAlpha(psrcbm,pItemBM,0,0);
  
  if(isPlay==true) DrawSkinAlpha(FileListAlpha_GetSkin(EFLSA_ItemBG_PlayIcon),pItemBM,0,0);
  
  s32 tx=0;
  switch(FileList_Mode){
    case EPSFLM_Single: tx=NDSROMIconXMargin+NDSROMIcon16Width+NDSROMIconXMargin; break;
    case EPSFLM_Double: tx=NDSROMIconXMargin+NDSROMIcon32Width+NDSROMIconXMargin; break;
  }
  
  NDSFiles_LoadNDSIcon(pNDSFile);
  
  s32 ty;
  s32 ItemHeight=pItemBM->GetHeight();
  
  if(pNDSFile->pNDSROMIcon!=NULL){
    switch(FileList_Mode){
      case EPSFLM_Single: NDSROMIcon_DrawIcon16(pNDSFile->pNDSROMIcon,pItemBM,NDSROMIconXMargin,1); break;
      case EPSFLM_Double: NDSROMIcon_DrawIcon32(pNDSFile->pNDSROMIcon,pItemBM,NDSROMIconXMargin,1); break;
    }
    }else{
    psrcbm=pNDSFile->pIcon;
    DrawSkinAlpha(psrcbm,pItemBM,NDSROMIconXMargin,1);
  }
  
  s32 TextHeight=glCanvasTextHeight+2;
  
  if(Selected==false){
    switch(pNDSFile->FileType){
      case ENFFT_UpFolder: case ENFFT_Folder: {
        pItemBM->SetFontTextColor(ColorTable.FileList.FolderNameText);
      } break;
      default: {
        pItemBM->SetFontTextColor(ColorTable.FileList.FileNameText);
      } break;
    }
    }else{
    pItemBM->SetFontTextColor(ColorTable.FileList.SelectText);
  }
  
  switch(FileList_Mode){
    case EPSFLM_Single: {
      ty=(ItemHeight-TextHeight)/2;
      pItemBM->TextOutW(tx,ty+1,pNDSFile->pFilenameUnicode);
    } break;
    case EPSFLM_Double: {
      const UnicodeChar *plt0=pNDSFile->pFilenameUnicode_DoubleLine0;
      const UnicodeChar *plt1=pNDSFile->pFilenameUnicode_DoubleLine1;
      if(plt1!=NULL){
        ty=(ItemHeight-(TextHeight*2))/2;
        pItemBM->TextOutW(tx,ty+1,plt0);
        ty+=TextHeight;
        pItemBM->TextOutW(tx,ty+1,plt1);
        }else{
        ty=(ItemHeight-TextHeight)/2;
        pItemBM->TextOutW(tx,ty+1,plt0);
      }
    } break;
  }
}

static void FileList_MainDrawBG(TScrollBar *psb)
{
  if(Backlight_isStandby()==true) return;
  
  VRAMWriteCache_Enable();
  
  CglCanvas *pTmpBM=pScreenMain->pBackCanvas;
  
  CglCanvas *pItemBM=new CglCanvas(NULL,ScreenWidth,psb->ItemHeight,pf15bit);
  pItemBM->SetCglFont(pCglFontDefault);
  
// ------------------------------
  
  if( (psb->ShowPos<0) || ((psb->MaxPos-psb->ShowPos)<psb->ClientSize) ){
    CglB15 *pb15=FileList_GetSkin(EFLS_BG_Bottom);
    pb15->pCanvas->BitBltFullBeta(pTmpBM);
  }
  
  {
    CglCanvas *pBGBM=FileList_GetSkin(EFLS_BG_Bottom)->pCanvas;
    
    for(s32 idx=0;idx<NDSFilesCount;idx++){
      s32 DrawHeight=psb->ItemHeight;
      s32 DrawTop=(idx*DrawHeight)-psb->ShowPos;
      if((-psb->ItemHeight<DrawTop)&&(DrawTop<psb->ClientSize)){
        if(psb->ClientSize<(DrawTop+DrawHeight)) DrawHeight=psb->ClientSize-DrawTop;
        
        bool Selected=false;
        if(idx==psb->SelectedIndex) Selected=true;
        
        bool isPlay=false;
        if(PlayCursorIndex==idx) isPlay=true;
        
        pBGBM->BitBlt(pItemBM,0,0,ScreenWidth,DrawHeight,0,DrawTop,false);
        
        FileList_DrawBG_DrawNDSFile(pItemBM,&pNDSFiles[idx],Selected,isPlay);
        
        pItemBM->BitBlt(pTmpBM,0,DrawTop,ScreenWidth,DrawHeight,0,0,false);
      }
    }
    
    ScrollBar_Draw(psb,pTmpBM);
    
  }
  
// ------------------------------
  
  if(pItemBM!=NULL){
    delete pItemBM; pItemBM=NULL;
  }
  
  if(MP3Cnt_WindowState==EMCWS_Show) FileList_DrawMP3Cnt(pTmpBM);
  
  ScreenMain_Flip_ProcFadeEffect();
  
  VRAMWriteCache_Disable();
}

static void FileList_SubDrawBG_Standby(TScrollBar *psb)
{
  CglCanvas *pTmpBM=new CglCanvas(NULL,ScreenWidth,ScreenHeight,pf15bit);
  pTmpBM->SetCglFont(pCglFontDefault);
//  pTmpBM->FillFull(RGB15(31,31,31)|BIT15);
  
  Clock_Standby_Draw(pTmpBM);
  
  FileList_DrawID3Tag(pTmpBM,ProcState.FileList.ScreenSaver_BlackMode);
  
  VRAMWriteCache_Enable();
  pTmpBM->BitBltFullBeta(pScreenSub->pCanvas);
  VRAMWriteCache_Disable();
  
  if(pTmpBM!=NULL){
    delete pTmpBM; pTmpBM=NULL;
  }
}

static void FileList_SubDrawBG(TScrollBar *psb)
{
  if(Backlight_isStandby()==true){
    FileList_SubDrawBG_Standby(psb);
    return;
  }
  
  if(DrawIPK(pScreenSub->pCanvas)==true) return;
  
  CglCanvas *pTmpBM=new CglCanvas(NULL,ScreenWidth,ScreenHeight,pf15bit);
  pTmpBM->SetCglFont(pCglFontDefault);
  
  CglCanvas *pItemBM=new CglCanvas(NULL,ScreenWidth,psb->ItemHeight,pf15bit);
  pItemBM->SetCglFont(pCglFontDefault);
  
// ------------------------------

  if(ShellSet.SwapDisp==true){
    CglB15 *pb15=FileList_GetSkin(EFLS_BG_TopMsg);
    pb15->pCanvas->BitBltFullBeta(pTmpBM);
    }else{
    if(psb->ShowPos<psb->ClientSize){
      CglB15 *pb15=FileList_GetSkin(EFLS_BG_TopMsg);
      pb15->pCanvas->BitBlt(pTmpBM,0,0,ScreenWidth,psb->ClientSize-psb->ShowPos,0,0,false);
    }
    
    {
      CglCanvas *pBGBM=FileList_GetSkin(EFLS_BG_Top)->pCanvas;
      
      for(s32 idx=0;idx<NDSFilesCount;idx++){
        s32 DrawHeight=psb->ItemHeight;
        s32 DrawTop=(idx*DrawHeight)+psb->ClientSize-psb->ShowPos;
        if((-psb->ItemHeight<DrawTop)&&(DrawTop<psb->ClientSize)){
          if(psb->ClientSize<(DrawTop+DrawHeight)) DrawHeight=psb->ClientSize-DrawTop;
          
          bool Selected=false;
          if(idx==psb->SelectedIndex) Selected=true;
          
          bool isPlay=false;
          if(PlayCursorIndex==idx) isPlay=true;
          
          pBGBM->BitBlt(pItemBM,0,0,ScreenWidth,DrawHeight,0,DrawTop,false);
          
          FileList_DrawBG_DrawNDSFile(pItemBM,&pNDSFiles[idx],Selected,isPlay);
          
          pItemBM->BitBlt(pTmpBM,0,DrawTop,ScreenWidth,DrawHeight,0,0,false);
        }
      }
    }
  }
  
  // ------------------------------
  
  if(pItemBM!=NULL){
    delete pItemBM; pItemBM=NULL;
  }
  
  Clock_Draw(pTmpBM);
  
  FileList_DrawID3Tag(pTmpBM,false);
  
  VRAMWriteCache_Enable();
  pTmpBM->BitBltFullBeta(pScreenSub->pCanvas);
  VRAMWriteCache_Disable();
  
  if(pTmpBM!=NULL){
    delete pTmpBM; pTmpBM=NULL;
  }
}

static bool mb;
static s32 msx,msy;
static s32 mx,my;
static s32 movespeed;
static s32 mvsynccnt;

static bool FileList_MouseDown(TScrollBar *psb,s32 x,s32 y)
{
  mb=true;
  msx=x;
  msy=y;
  mx=x;
  my=y;
  movespeed=0;
  mvsynccnt=0;
  
  psb->UsePressImage=true;
  
  s32 fileidx=(psb->TopPos+my)/psb->ItemHeight;
  ScrollBar_SetSelectedIndex(psb,fileidx);
  
  ScreenRedrawFlag=true;
  
  return(true);
}

static bool FileList_MouseMove(TScrollBar *psb,s32 x,s32 y)
{
  if(mb==false) return(false);
  
  if(y!=my){
    s32 vec=my-y;
    ScrollBar_SetDirectTopPos(psb,psb->TopPos+vec);
    
    if(movespeed<vec){
      movespeed=vec;
      }else{
      movespeed=(movespeed+(vec*15))/16;
    }
  }
  
  mx=x;
  my=y;
  
  return(true);
}

static bool FileList_MouseUp(TScrollBar *psb,s32 x,s32 y)
{
  if(mb==false) return(false);
  
  mb=false;
  
  psb->UsePressImage=false;
  
  if(16<movespeed) movespeed=16;
  
  if((-16<(x-msx))&&((x-msx)<16)){
    }else{
    _consolePrintf("FileList_MouseUp: It was judged a horizontal scroll. (%dpixels)\n",x-msx);
    return(true);
  }
  
  if((-16<(y-msy))&&((y-msy)<16)){
    }else{
    _consolePrintf("FileList_MouseUp: It was judged a vertical scroll. (%dpixels)\n",y-msy);
    return(true);
  }
  
  u32 marginlow=0,marginhigh=0;
  
  ENDSFile_FileType FileType=pNDSFiles[psb->SelectedIndex].FileType;
  
  switch(FileType){
    case ENFFT_UnknownFile: break;
    case ENFFT_UpFolder: case ENFFT_Folder: marginlow=1; marginhigh=30; break;
    case ENFFT_Sound: case ENFFT_Image: case ENFFT_Text: case ENFFT_Video: case ENFFT_Skin: marginlow=1; marginhigh=30; break;
    case ENFFT_NDSROM: marginlow=3; marginhigh=30; break;
  }
  
  if(marginlow<mvsynccnt){
    if(mvsynccnt<marginhigh){
      if(FileType==ENFFT_NDSROM){
        if(psb->LastSelectedIndex==psb->SelectedIndex){
          StartApplication();
          return(true);
        }
        _consolePrintf("Ignored NDS file is selected. The place touched is different from the last cursor position.\n");
        }else{
        StartApplication();
        return(true);
      }
      }else{
//      _consolePrintf("クリック時間が%dframes(%dms)以上だったので無視しました。(%dframes)\n",marginhigh,marginhigh*16,mvsynccnt);
    }
    }else{
//    _consolePrintf("クリック時間が%dframes(%dms)以下だったので無視しました。(%dframes)\n",marginlow,marginlow*16,mvsynccnt);
  }
  
  return(true);
}

static void FileList_MouseIdle(TScrollBar *psb)
{
  if(mb==true){
    mvsynccnt++;
    movespeed=(movespeed*7)/8;
    return;
  }
  
  if(movespeed!=0){
    ScrollBar_SetDirectTopPos(psb,psb->TopPos+movespeed);
    movespeed=(movespeed*31)/32;
    ScreenRedrawFlag=true;
  }
}

