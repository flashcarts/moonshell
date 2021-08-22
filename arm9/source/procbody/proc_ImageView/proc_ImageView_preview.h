
static s32 PreviewMultipleFix8;
static CglCanvas *pPreviewBM;
static u32 PreviewOfsX,PreviewOfsY;
static u32 PreviewLastY;
static CglCanvas *pPreviewTempBM;

static void Preview_Init(void)
{
  pScreenSub->pCanvas->FillFull(RGB15(15,15,15)|BIT15);
  
  if((SrcWidth<=ScreenWidth)&&(SrcHeight<=ScreenHeight)){
    PreviewMultipleFix8=1*0x100;
    }else{
    s32 mx,my;
    mx=ScreenWidth*0x100/SrcWidth;
    my=ScreenHeight*0x100/SrcHeight;
    
    if(mx<my){
      PreviewMultipleFix8=mx;
      _consolePrintf("Select preview fit to width. (mul=0x%x)\n",PreviewMultipleFix8);
      }else{
      PreviewMultipleFix8=my;
      _consolePrintf("Select preview fit to height. (mul=0x%x)\n",PreviewMultipleFix8);
    }
  }
  
  u32 pw=SrcWidth*PreviewMultipleFix8/0x100;
  u32 ph=SrcHeight*PreviewMultipleFix8/0x100;
  
  pw=(pw+1)&~1;
  ph=(ph+1)&~1;
  pw+=2;
  ph+=2;
  
  pPreviewBM=new CglCanvas(NULL,pw,ph,pf15bit);
  pPreviewBM->FillFull(RGB15(24,24,24)|BIT15);
  
  pPreviewBM->SetColor(RGB15(12,12,12)|BIT15);
  pPreviewBM->DrawBox(0,0,SrcWidth*PreviewMultipleFix8/0x100,SrcHeight*PreviewMultipleFix8/0x100);
  
  if(ScreenWidth<=pw){
    PreviewOfsX=0;
    }else{
    PreviewOfsX=(ScreenWidth-pw)/2;
    PreviewOfsX=PreviewOfsX&~1;
  }
  if(ScreenHeight<=ph){
    PreviewOfsY=0;
    }else{
    PreviewOfsY=(ScreenHeight-ph)/2;
    PreviewOfsY=PreviewOfsY&~1;
  }
  
  PreviewLastY=(u32)-1;
  
  pPreviewTempBM=new CglCanvas(NULL,ScreenWidth,ScreenHeight,pf15bit);
  pPreviewTempBM->SetCglFont(pCglFontDefault);
  pPreviewTempBM->SetFontTextColor(RGB15(8,8,8)|BIT15);
}

static void Preview_Free(void)
{
  if(pPreviewBM!=NULL){
    delete pPreviewBM; pPreviewBM=NULL;
  }
  if(pPreviewTempBM!=NULL){
    delete pPreviewTempBM; pPreviewTempBM=NULL;
  }
}

static void TextOutA_Shadow(CglCanvas *pcan,u32 x,u32 y,const char *pstr)
{
  pcan->SetFontTextColor(RGB15(31,31,31)|BIT15);
  pcan->TextOutA(x+1,y+1,pstr);
  pcan->SetFontTextColor(RGB15(0,0,0)|BIT15);
  pcan->TextOutA(x,y,pstr);
}

static void TextOutUTF8_Shadow(CglCanvas *pcan,u32 x,u32 y,const char *pstr)
{
  pcan->SetFontTextColor(RGB15(31,31,31)|BIT15);
  pcan->TextOutUTF8(x+1,y+1,pstr);
  pcan->SetFontTextColor(RGB15(0,0,0)|BIT15);
  pcan->TextOutUTF8(x,y,pstr);
}

static void Preview_Draw(void)
{
  CglCanvas *pcan=pPreviewTempBM;
  pcan->FillFull(RGB15(24,24,24)|BIT15);
  
  u32 pw=pPreviewBM->GetWidth();
  u32 ph=pPreviewBM->GetHeight();
  
  pPreviewBM->BitBlt(pcan,PreviewOfsX,PreviewOfsY,pw,ph,0,0,false);
  
  TRect r;
  r.x=DstRect.x*PreviewMultipleFix8/0x100;
  r.y=DstRect.y*PreviewMultipleFix8/0x100;
  r.w=DstRect.w*PreviewMultipleFix8/0x100;
  r.h=DstRect.h*PreviewMultipleFix8/0x100;
  
  {
    u16 *pbuf=&pcan->GetVRAMBuf()[(PreviewOfsX+r.x)+((PreviewOfsY+r.y)*ScreenWidth)];
    
    for(u32 y=0;y<(u32)r.h;y++){
      if((y==0)||(y==((u32)r.h-1))){
        for(u32 x=0;x<(u32)r.w;x++){
          pbuf[x]=(((pbuf[x]&RGB15(30,30,30))>>1)+RGB15(8,7,7)) | BIT15;
        }
        }else{
        {
          u32 x=0;
          pbuf[x]=(((pbuf[x]&RGB15(30,30,30))>>1)+RGB15(8,7,7)) | BIT15;
        }
        for(u32 x=1;x<(u32)r.w-1;x++){
          pbuf[x]=(((pbuf[x]&RGB15(30,30,30))>>1)+RGB15(4,3,3)) | BIT15;
        }
        {
          u32 x=(u32)r.w-1;
          pbuf[x]=(((pbuf[x]&RGB15(30,30,30))>>1)+RGB15(8,7,7)) | BIT15;
        }
      }
      pbuf+=ScreenWidth;
    }
  }
  
  if((LeftButtonPressing==false)&&(ProcState.Image.ShowInfomation==true)){
    TextOutUTF8_Shadow(pcan,8,8+((glCanvasTextHeight+2)*0),Lang_GetUTF8("IV_Help1"));
    TextOutUTF8_Shadow(pcan,8,8+((glCanvasTextHeight+2)*1),Lang_GetUTF8("IV_Help2"));
    
    u32 linecnt;
    if(isPlugJpeg==true){
      linecnt=PlugJpeg_GetInfoIndexCount();
      }else{
      linecnt=pPluginBody->pIL->GetInfoIndexCount();
    }
    
    u32 th=glCanvasTextHeight;
    u32 y=ScreenHeight-2-(linecnt*th);
    
    for(u32 idx=0;idx<linecnt;idx++){
      char str[128];
      // UnicodeChar strw[128];
      if(isPlugJpeg==true){
        if(PlugJpeg_GetInfoStrA(idx,str,128)==true) TextOutA_Shadow(pcan,8,y,str);
        // if(PlugJpeg_GetInfoStrW(idx,strw,128)==true) TextOutW_Shadow(pcan,8,y,strw);
        if(PlugJpeg_GetInfoStrUTF8(idx,str,128)==true) TextOutUTF8_Shadow(pcan,8,y,str);
        }else{
        if(pPluginBody->pIL->GetInfoStrA(idx,str,128)==true) TextOutA_Shadow(pcan,8,y,str);
        // if(pPluginBody->pIL->GetInfoStrW(idx,strw,128)==true) TextOutW_Shadow(pcan,8,y,strw);
        if(pPluginBody->pIL->GetInfoStrUTF8(idx,str,128)==true) TextOutUTF8_Shadow(pcan,8,y,str);
      }
      y+=th;
    }
  }
  
  VRAMWriteCache_Enable();
  pPreviewTempBM->BitBltFullBeta(pScreenSub->pCanvas);
  VRAMWriteCache_Disable();
}

static void Preview_Set(u32 y,u8 *pSrcBM)
{
  u32 dy=y*PreviewMultipleFix8/0x100;
  
  if(PreviewLastY==dy) return;
  PreviewLastY=dy;
  
  u32 pw=pPreviewBM->GetWidth();
  u32 ph=pPreviewBM->GetHeight();
  
  if(ph<=dy){
    _consolePrintf("Fatal error: Create preview error. height overflow. (%d,%d %d,%d)\n",y,SrcHeight,dy,ph);
    ShowLogHalt();
  }
  
  u16 *pDstBM=pPreviewBM->GetScanLine(dy);
  
  u32 ofsf8=0;
  u32 addf8=1*0x100*0x100/PreviewMultipleFix8;
  
  u32 w=SrcWidth*0x100/addf8;
  
  if(pw<=w){
    _consolePrintf("Fatal error: Create preview error. width overflow. (%d %d,%d)\n",SrcWidth,pw,w);
    ShowLogHalt();
  }
  
  for(u32 x=0;x<w;x++){
    u32 ofs=(ofsf8/0x100)*3;
    u32 r=pSrcBM[ofs+0]>>3;
    u32 g=pSrcBM[ofs+1]>>3;
    u32 b=pSrcBM[ofs+2]>>3;
    pDstBM[x]=RGB15(r,g,b)|BIT15;
    ofsf8+=addf8;
  }
}

