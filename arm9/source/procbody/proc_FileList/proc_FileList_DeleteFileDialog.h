
static void CB_KeyLongPress(u32 Keys)
{
  if(Keys!=KEY_START) return;
  
  Backlight_ResetTimer();
  
  BGM_Free();
  
  Sound_Start(WAVFN_Notify);
  
  RequestRefreshPlayCursorIndex=false;
  PlayCursorIndex=-1;
  
  Popup_VsyncUpdate((u32)-1);
  
  FileList_MainDrawBG(&ScrollBar);
  FileList_SubDrawBG(&ScrollBar);
  
  CglCanvas *pDstBM=pScreenMain->pBackCanvas;
  
  CglB15 *pb15=FileList_GetSkin(EFLS_DeleteFileDialog);
  u32 WinPosX=(ScreenWidth-pb15->GetWidth())/2;
  u32 WinPosY=(ScreenHeight-pb15->GetHeight())/2;
  
  {
    pScreenMain->pViewCanvas->BitBltFullBeta(pDstBM);
    pb15->BitBlt(pDstBM,WinPosX,WinPosY,pb15->GetWidth(),pb15->GetHeight(),0,0);
    
    u32 tx=WinPosX,ty=WinPosY,th=glCanvasTextHeight+1;
    
    tx+=22; ty+=5;
    {
      const char *pmsg=Lang_GetUTF8("FL_DeleteFileDialog_Question_Title");
      if(pmsg!=NULL){
        pDstBM->SetFontTextColor(ColorTable.FileList.DeleteFileDialog_Title_Text);
        pDstBM->TextOutUTF8(tx,ty,pmsg);
      }
    }
    
    tx-=22;
    tx+=5; ty+=17;
    
    for(u32 idx=0;idx<3;idx++){
      const char *pmsg=NULL;
      switch(idx){
        case 0: pmsg=Lang_GetUTF8("FL_DeleteFileDialog_Question_Line0"); break;
        case 1: pmsg=Lang_GetUTF8("FL_DeleteFileDialog_Question_Line1"); break;
        case 2: pmsg=Lang_GetUTF8("FL_DeleteFileDialog_Question_Line2"); break;
      }
      if(pmsg!=NULL){
        pDstBM->SetFontTextColor(ColorTable.FileList.DeleteFileDialog_Body_Text);
        pDstBM->TextOutUTF8(tx,ty+(th*idx),pmsg);
      }
    }
  }
  
  ScreenMain_Flip_ProcFadeEffect();
  
  _consolePrintf("Wait for key releases.\n");
  while(1){
    if(IPC6->RequestUpdateIPC==false){
      u32 btns=IPC6->buttons;
      u32 keys=(~REG_KEYINPUT)&0x3ff;
      if((btns&IPC_PEN_DOWN)!=0) keys|=KEY_TOUCH;
      if((btns&IPC_X)!=0) keys|=KEY_X;
      if((btns&IPC_Y)!=0) keys|=KEY_Y;
      if(keys==0) break;
      IPC6->RequestUpdateIPC=true;
    }
  }
  
  bool execute=false;
  
  while(1){
    if(IPC6->RequestUpdateIPC==false){
      u32 btns=IPC6->buttons;
      u32 keys=(~REG_KEYINPUT)&0x3ff;
      if((btns&IPC_PEN_DOWN)!=0) keys|=KEY_TOUCH;
      if((btns&IPC_X)!=0) keys|=KEY_X;
      if((btns&IPC_Y)!=0) keys|=KEY_Y;
      if(keys==KEY_B){
        execute=false;
        break;
      }
      if(keys==(KEY_A|KEY_L|KEY_R)){
        execute=true;
        break;
      }
      IPC6->RequestUpdateIPC=true;
    }
  }
  
  if(execute==false){
    FileList_MainDrawBG(&ScrollBar);
    FileList_SubDrawBG(&ScrollBar);
    }else{
    Sound_Start(WAVFN_Notify);
    
    u32 backidx=ScrollBar.SelectedIndex;
    FileListInit();
    NDSFiles_RefreshCurrentFolder();
    ScrollBar.SelectedIndex=backidx;
    if(ScrollBar.SelectedIndex!=0) ScrollBar.SelectedIndex--;
    
    FileList_MainDrawBG(&ScrollBar);
    FileList_SubDrawBG(&ScrollBar);
    
    {
      pScreenMain->pViewCanvas->BitBltFullBeta(pDstBM);
      pb15->BitBlt(pDstBM,WinPosX,WinPosY,pb15->GetWidth(),pb15->GetHeight(),0,0);
      
      u32 tx=WinPosX,ty=WinPosY,th=glCanvasTextHeight+1;
      
      tx+=22; ty+=5;
      {
        const char *pmsg=Lang_GetUTF8("FL_DeleteFileDialog_Success_Title");
        if(pmsg!=NULL){
          pDstBM->SetFontTextColor(ColorTable.FileList.DeleteFileDialog_Title_Text);
          pDstBM->TextOutUTF8(tx,ty,pmsg);
        }
      }
      
      tx-=22;
      tx+=5; ty+=17;
      
      for(u32 idx=0;idx<3;idx++){
        const char *pmsg=NULL;
        switch(idx){
          case 0: pmsg=Lang_GetUTF8("FL_DeleteFileDialog_Success_Line0"); break;
          case 1: pmsg=Lang_GetUTF8("FL_DeleteFileDialog_Success_Line1"); break;
          case 2: pmsg=Lang_GetUTF8("FL_DeleteFileDialog_Success_Line2"); break;
        }
        if(pmsg!=NULL){
          pDstBM->SetFontTextColor(ColorTable.FileList.DeleteFileDialog_Body_Text);
          pDstBM->TextOutUTF8(tx,ty+(th*idx),pmsg);
        }
      }
    }
    
    ScreenMain_Flip_ProcFadeEffect();
  }
  
  _consolePrintf("Wait for key releases.\n");
  while(1){
    if(IPC6->RequestUpdateIPC==false){
      u32 btns=IPC6->buttons;
      u32 keys=(~REG_KEYINPUT)&0x3ff;
      if((btns&IPC_PEN_DOWN)!=0) keys|=KEY_TOUCH;
      if((btns&IPC_X)!=0) keys|=KEY_X;
      if((btns&IPC_Y)!=0) keys|=KEY_Y;
      if(keys==0) break;
      IPC6->RequestUpdateIPC=true;
    }
  }
  
  FileList_MainDrawBG(&ScrollBar);
  FileList_SubDrawBG(&ScrollBar);
  
  REG_IME=0;
  VBlankPassedCount=0;
  REG_IME=1;
}

