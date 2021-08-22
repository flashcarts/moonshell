
static u32 Popup_TimeoutVSync;

static TRect PopupRect={16,16,64,16};

static void Popup_Init(void)
{
  Popup_TimeoutVSync=0;
}

static void Popup_Free(void)
{
  Popup_TimeoutVSync=0;
  pScreenMainOverlay->SetVisible_for_LeftTop128x64(false);
}

static void Popup_VsyncUpdate(u32 VsyncCount) // (u32)-1Ç≈èÌÇ…âBÇ∑
{
  if(Popup_TimeoutVSync==0) return;
  
  if(VsyncCount!=(u32)-1){
    Popup_TimeoutVSync+=VsyncCount;
    if(Popup_TimeoutVSync<(60*2)) return;
  }
  
  Popup_TimeoutVSync=0;
  pScreenMainOverlay->SetVisible_for_LeftTop128x64(false);
}

static void Popup_Show(const char *pstr)
{
//  _consolePrintf("Popup_Show(%s);\n",pstr);
  
  CglCanvas *pCanvas=pScreenMainOverlay->pCanvas;
  
  TRect r=PopupRect;
  
  pCanvas->SetColor(ColorTable.FileList.PopupBG);
  pCanvas->FillFast(r.x,r.y,r.w,r.h);
  pCanvas->SetColor(ColorTable.FileList.PopupFrame);
  pCanvas->DrawBox(r.x,r.y,r.w,r.h);
  
  pCanvas->SetFontTextColor(ColorTable.FileList.PopupText);
  
  s32 tw=pCanvas->GetTextWidthA(pstr);
  s32 th=glCanvasTextHeight;
  s32 tx=r.x+((r.w-tw)/2);
  s32 ty=r.y+((r.h-th)/2);
  
  pCanvas->TextOutA(tx,ty,pstr);
  
  pScreenMainOverlay->SetVisible_for_LeftTop128x64(true);
  
  Popup_TimeoutVSync=1;
}

static void Popup_Show_Pause(void)
{
  if(GlobalPauseFlag==false){
    Popup_Show("PLAY");
    }else{
    Popup_Show("PAUSE");
  }
}

static void Popup_Show_Prev(void)
{
  Popup_Show("Prev file");
}

static void Popup_Show_Next(void)
{
  Popup_Show("Next file");
}

static void Popup_Show_Volume(void)
{
  char str[16];
  snprintf(str,16,"vol.%d%%",(strpcmGetVolume64()*100)/64);
  Popup_Show(str);
}

static void Popup_Show_BacklightLevel(void)
{
  char str[16];
  snprintf(str,16,"Level.%d",1+ProcState.System.BacklightLevel);
  Popup_Show(str);
}

static void Popup_Show_PlayMode(void)
{
  const char *pstr;
  
  TProcState_FileList *pfl=&ProcState.FileList;
  
  switch(pfl->PlayMode){
    case EPSFLPM_Repeat: pstr="Repeat"; break;
    case EPSFLPM_AllRep: pstr="AllRepeat"; break;
    case EPSFLPM_Random: pstr="Random"; break;
    default: pstr="Error!!"; break;
  }
  
  Popup_Show(pstr);
}

static void Popup_Show_Seek(s32 val)
{
  char str[16];
  if(val<0){
    snprintf(str,16,"Seek -%d%%",-val);
    }else{
    snprintf(str,16,"Seek +%d%%",val);
  }
  Popup_Show(str);
}

