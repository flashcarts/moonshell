
static CFont *pClockFont;

static char Clock_DateStr[16];
static char Clock_TimeStr[16];

static void Clock_Init(void)
{
  pClockFont=new CFont(EBM_TGF,NULL,FileListClockAlpha_GetSkin(EFLCLKSA_Digits));
  Clock_DateStr[0]=0;
  Clock_TimeStr[0]=0;
}

static void Clock_Free(void)
{
  if(pClockFont!=NULL){
    delete pClockFont; pClockFont=NULL;
  }
}

static void Clock_Refresh(void)
{
  DateTime_ResetNow();
  TDateTime dt=DateTime_GetNow();
  
  Date_GetDateStrBuf(Clock_DateStr,16,dt.Date);
  Date_GetTimeStrBuf_12h(Clock_TimeStr,16,dt.Time);
}

static void Clock_Draw(CglCanvas *pCanvas)
{
  CglTGF *pbm=FileListClockAlpha_GetSkin(EFLCLKSA_BG);
  
  u32 LinesCount;
  
  if(BGM_isOpened()==false){
    LinesCount=0;
    }else{
    LinesCount=1; // for Filename.
    if(ID3Tag.Exists==false){
      LinesCount++; // for Not exists text.
      }else{
      LinesCount+=ID3Tag.LinesExistsCount;
    }
    LinesCount++; // for Window padding.
  }
  
  s32 fx=172;
  s32 fy=8+(14*LinesCount);
  s32 fw=pbm->GetWidth();
  s32 fh=pbm->GetHeight();
  
  pbm->BitBlt(pCanvas,fx,fy);
  
  u32 th=10+2;
  u32 ty=fy+((fh-(th*2))/2);
  ty+=1;
  ty+=pClockFont->GetTextHeight();
  
  {
    u32 tw=pClockFont->GetTextWidth(Clock_DateStr);
    u32 tx=fx+((fw-tw)/2);
    pClockFont->DrawText(pCanvas,tx,ty,Clock_DateStr);
  }
  ty+=th;
  {
    u32 tw=pClockFont->GetTextWidth(Clock_TimeStr);
    u32 tx=fx+((fw-tw)/2);
    pClockFont->DrawText(pCanvas,tx,ty,Clock_TimeStr);
  }
}

static void Clock_Standby_Draw_Calender(CglCanvas *pCanvas,TCalendarData *pCalendarData,u32 Today,const bool BlackMode)
{
  if(Skin_Calender.Enabled==false) return;
  
  CFont *pOffFont=new CFont(EBM_TGF,NULL,StandbyClockAlpha_GetSkin(ESCA_CalenderFont));
  CFont *pOnFont=new CFont(EBM_TGF,NULL,StandbyClockAlpha_GetSkin(ESCA_CalenderTodayFont));
  
  if((pOffFont==NULL)||(pOnFont==NULL)){
    if(pOffFont!=NULL){
      delete pOffFont; pOffFont=NULL;
    }
    if(pOnFont!=NULL){
      delete pOnFont; pOnFont=NULL;
    }
    return;
  }
  
  const u32 ItemWidth=14;
  const u32 ItemHeight=8;
  const u32 _LineWidth=ItemWidth*7;
  const u32 _LineHeight=ItemHeight;
  const u32 LineCount=5;
  
  const u32 DrawX=Skin_Calender.PosX;
  const u32 DrawY=Skin_Calender.PosY;
  
  TColorTable_Calender *pcoltbl;
  
  if(BlackMode==false){
    pcoltbl=&ColorTable.Calender_White;
    }else{
    pcoltbl=&ColorTable.Calender_Black;
  }
  
  for(s32 idx=0;idx<Calender_DaysMapCount;idx++){
    u32 week=idx%7;
    u32 dx=DrawX+(week*ItemWidth);
    u32 dy=DrawY+((idx/7)*ItemHeight);
    
    char str[4]={0,};
    u32 strw=0;
    
    if(pCalendarData->DaysMap[idx]==0){
      str[0]=0;
      strw=0;
      }else{
      snprintf(str,4,"%d",pCalendarData->DaysMap[idx]);
      strw=pOnFont->GetTextWidth(str);
    }
    
    u16 bgcol=0;
    
    if(week==0) bgcol=pcoltbl->BG_Sunday;
    if(week==6) bgcol=pcoltbl->BG_Satday;
    if(Today==pCalendarData->DaysMap[idx]) bgcol=pcoltbl->BG_Today;
    if(bgcol==0) bgcol=pcoltbl->BG_Normal;
    
    if(bgcol!=(RGB15(0,0,0)|BIT15)){
      bgcol=(bgcol&0x7bde)/2;
      for(u32 y=0;y<ItemHeight;y++){
        u16 *pbuf=pCanvas->GetScanLine(dy+y);
        pbuf=&pbuf[dx];
        for(u32 x=0;x<ItemWidth;x++){
          pbuf[x]=(((pbuf[x]&0x7bde)/2)+bgcol)|BIT15;
        }
      }
    }
    
    if(strw!=0){
      dx+=1+((ItemWidth-strw)/2);
      u32 fh=pOnFont->GetTextHeight();
      dy+=(ItemHeight-fh)/2;
      if(Today!=pCalendarData->DaysMap[idx]){
        pOffFont->DrawText(pCanvas,dx,dy+fh+1,str);
        }else{
        pOnFont->DrawText(pCanvas,dx,dy+fh+1,str);
      }
    }
  }
  
  u16 FrameOutColor=pcoltbl->FrameOutColor;
  u16 FrameInColor=pcoltbl->FrameInColor;
  
  for(u32 idx=0;idx<=7;idx++){
    u32 sx=DrawX+(ItemWidth*idx);
    u32 sy=DrawY;
    u32 ey=sy+(_LineHeight*LineCount)+_LineHeight;
    if((idx==0)||(idx==7)){
      pCanvas->SetColor(FrameOutColor);
      }else{
      pCanvas->SetColor(FrameInColor);
    }
    pCanvas->DrawLine(sx,sy+1,sx,ey);
  }
  
  for(s32 idx=0;idx<=(s32)LineCount+1;idx++){
    u32 sx=DrawX;
    u32 ex=sx+_LineWidth;
    u32 sy=DrawY+(_LineHeight*idx);
    if((idx==0)||(idx==(s32)(LineCount+1))){
      pCanvas->SetColor(FrameOutColor);
      }else{
      pCanvas->SetColor(FrameInColor);
    }
    pCanvas->DrawLine(sx+1,sy,ex,sy);
  }
  
  if(pOffFont!=NULL){
    delete pOffFont; pOffFont=NULL;
  }
  if(pOnFont!=NULL){
    delete pOnFont; pOnFont=NULL;
  }
}

static void Clock_Standby_Draw(CglCanvas *pCanvas)
{
  CglB15 *pb15=StandbyClock_GetSkin(ESC_BG);
  pb15->pCanvas->BitBltFullBeta(pCanvas);
  
  DateTime_ResetNow();
  TDateTime dt=DateTime_GetNow();
  
  TCalendarData CalendarData=DateTime_CreateCalendarData(dt.Date.Year,dt.Date.Month);
  
  Clock_Standby_Draw_Calender(pCanvas,&CalendarData,dt.Date.Day,ProcState.FileList.ScreenSaver_BlackMode);
  
  char Clock_DateStr[16];
  char Clock_TimeHHMMStr[8];
  char Clock_TimeSSStr[8];
  char Clock_TimeAPStr[8];
  char Clock_TimeSSAPStr[8];
  
  Date_GetDateStrBuf(Clock_DateStr,16,dt.Date);
  Date_GetTimeStrBuf_12h_HHMM(Clock_TimeHHMMStr,8,dt.Time);
  Date_GetTimeStrBuf_12h_SS(Clock_TimeSSStr,8,dt.Time);
  Date_GetTimeStrBuf_12h_AP(Clock_TimeAPStr,8,dt.Time);
  Date_GetTimeStrBuf_12h_SSAP(Clock_TimeSSAPStr,8,dt.Time);
  
  CFont *pFontDate=new CFont(EBM_TGF,NULL,StandbyClockAlpha_GetSkin(ESCA_Font16));
  CFont *pFontTimeHHMM=new CFont(EBM_TGF,NULL,StandbyClockAlpha_GetSkin(ESCA_Font56));
  CFont *pFontTimeSSAP=new CFont(EBM_TGF,NULL,StandbyClockAlpha_GetSkin(ESCA_Font16));
  CFont *pFontTimeAP=new CFont(EBM_TGF,NULL,StandbyClockAlpha_GetSkin(ESCA_Font24));
  
  if((pFontDate!=NULL)&&(pFontTimeHHMM!=NULL)&&(pFontTimeSSAP!=NULL)&&(pFontTimeAP!=NULL)){
    s32 fx=ScreenWidth-8;
    s32 fy=80;
    pFontTimeAP->DrawText(pCanvas,8,fy,Clock_TimeAPStr);
    fy+=pFontTimeHHMM->GetTextHeight();
    pFontTimeHHMM->DrawText(pCanvas,(ScreenWidth-pFontTimeHHMM->GetTextWidth(Clock_TimeHHMMStr))/2,fy,Clock_TimeHHMMStr);
    fy+=pFontTimeSSAP->GetTextHeight();
    pFontTimeSSAP->DrawText(pCanvas,fx-pFontDate->GetTextWidth(Clock_TimeSSAPStr),fy,Clock_TimeSSAPStr);
    fy+=pFontDate->GetTextHeight();
    pFontDate->DrawText(pCanvas,fx-pFontDate->GetTextWidth(Clock_DateStr),fy+4,Clock_DateStr);
  }
  
  if(pFontDate!=NULL){
    delete pFontDate; pFontDate=NULL;
  }
  if(pFontTimeHHMM!=NULL){
    delete pFontTimeHHMM; pFontTimeHHMM=NULL;
  }
  if(pFontTimeSSAP!=NULL){
    delete pFontTimeSSAP; pFontTimeSSAP=NULL;
  }
  if(pFontTimeAP!=NULL){
    delete pFontTimeAP; pFontTimeAP=NULL;
  }
}

