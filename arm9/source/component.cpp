
#pragma Ospace

#include <NDS.h>

#include "_const.h"
#include "_console.h"

#include "maindef.h"
#include "memtool.h"
#include "shell.h"
#include "lang.h"

#include "component.h"

#include "skin.h"
#include "strtool.h"

// ---------------------

static CglFont *pDefaultFont;

void Component_SetFont(CglFont *pFont)
{
  pDefaultFont=pFont;
}

// ---------------------

void ComponentLabel_Init(TComponentLabel *pComponentLabel,CglCanvas *pCanvas)
{
  TComponentLabel *pcl=pComponentLabel;
  
  pcl->CallBack_Click=NULL;
  pcl->pCanvas=pCanvas;
  pcl->Rect=CreateRect(0,0,0,0);
  pcl->Visible=true;
  pcl->Center=false;
  pcl->isTitle=false;
  pcl->TextColor=0;
  pcl->pMsgUTF8="";
  
  static UnicodeChar ucs[1]={0};
  pcl->pMsgUnicode=ucs;
}

void ComponentLabel_Draw(TComponentLabel *pComponentLabel)
{
  TComponentLabel *pcl=pComponentLabel;
  
  if(pcl->Visible==false) return;
  
  pcl->pCanvas->SetCglFont(pDefaultFont);
  if(pcl->TextColor!=0){
    pcl->pCanvas->SetFontTextColor(pcl->TextColor);
    }else{
    if(pcl->isTitle==false){
      pcl->pCanvas->SetFontTextColor(ColorTable.Component.Label_Text);
      }else{
      pcl->pCanvas->SetFontTextColor(ColorTable.Component.TitleLabel_Text);
    }
  }
  
  const char *pmsgUTF8=pcl->pMsgUTF8;
  
  u32 TextWidth=pcl->pCanvas->GetTextWidthUTF8(pmsgUTF8);
  u32 TextHeight=glCanvasTextHeight;
  
  if(pcl->Rect.w==0) pcl->Rect.w=TextWidth;
  if(pcl->Rect.h==0) pcl->Rect.h=TextHeight;
  
  TRect r=pcl->Rect;
  
//  pcl->pCanvas->SetColor(pcl->BGColor);
//  pcl->pCanvas->FillBox(r.x,r.y,r.w,r.h);
  
  if(pmsgUTF8[0]!=0){
    if(pcl->Center==false){
      pcl->pCanvas->TextOutUTF8(r.x,r.y,pmsgUTF8);
      }else{
      pcl->pCanvas->TextOutUTF8(r.x+((r.w-TextWidth)/2),r.y,pmsgUTF8);
    }
  }
  
  if(pcl->pMsgUnicode[0]!=0) pcl->pCanvas->TextOutW(r.x,r.y,pcl->pMsgUnicode);
}

s32 ComponentLabel_GetWidth(TComponentLabel *pComponentLabel)
{
  TComponentLabel *pcl=pComponentLabel;
  
  if(pcl->Visible==false) return(0);
  
  pcl->pCanvas->SetCglFont(pDefaultFont);
  pcl->pCanvas->SetFontTextColor(ColorTable.Component.Label_Text);
  
  const char *pmsgUTF8=pcl->pMsgUTF8;
  
  u32 TextWidth=pcl->pCanvas->GetTextWidthUTF8(pmsgUTF8);
  
  if(pcl->Rect.w==0) pcl->Rect.w=TextWidth;
  
  TRect r=pcl->Rect;
  
  s32 w=0;
  
  if(pmsgUTF8[0]!=0){
    if(pcl->Center==false){
      w=pcl->pCanvas->GetTextWidthUTF8(pmsgUTF8);
      }else{
      w=r.w;
    }
  }
  
  if(pcl->pMsgUnicode[0]!=0) w=pcl->pCanvas->GetTextWidthW(pcl->pMsgUnicode);
  
  return(w+2);
}

s32 ComponentLabel_GetIndexFromPos(TComponentLabel *pComponentLabel,s32 mx,s32 my)
{
  TComponentLabel *pcl=pComponentLabel;
  TRect r=pcl->Rect;
  
  if(pcl->Visible==false) return(-1);
  
  mx-=r.x;
  my-=r.y;
  
  if((0<=mx)&&(mx<r.w)){
    if((0<=my)&&(my<r.h)){
      return(0);
    }
  }
  
  return(-1);
}

bool ComponentLabel_MouseUp(TComponentLabel *pComponentLabel,s32 mx,s32 my)
{
  TComponentLabel *pcl=pComponentLabel;
  
  if(pcl->Visible==false) return(false);
  
  if(ComponentLabel_GetIndexFromPos(pcl,mx,my)==-1) return(false);
  
  if(pcl->CallBack_Click!=NULL) pcl->CallBack_Click(pcl);
  
  return(true);
}

// ------------------

void ComponentCheck_Init(TComponentCheck *pComponentCheck,CglCanvas *pCanvas)
{
  TComponentCheck *pcc=pComponentCheck;
  
  pcc->CallBack_Click=NULL;
  pcc->UserData=0;
  pcc->pCanvas=pCanvas;
  pcc->pOnIcon=NULL;
  pcc->pOffIcon=NULL;
  pcc->Checked=false;
  pcc->Rect=CreateRect(0,0,0,0);
  pcc->Visible=true;
  pcc->pMsgUTF8="";
  pcc->TextColor=ColorTable.Component.Check_Text;
}

void ComponentCheck_Draw(TComponentCheck *pComponentCheck)
{
  TComponentCheck *pcc=pComponentCheck;
  
  if(pcc->Visible==false) return;
  
  if(pcc->pOnIcon!=NULL){
    if(pcc->Rect.w==0) pcc->Rect.w=pcc->pOnIcon->GetWidth();
    if(pcc->Rect.h==0) pcc->Rect.h=pcc->pOnIcon->GetHeight();
  }
  
  TRect r=pcc->Rect;
  
  CglTGF *pSrcTGF=NULL;
  if(pcc->Checked==true){
    pSrcTGF=pcc->pOnIcon;
    }else{
    pSrcTGF=pcc->pOffIcon;
  }
  if(pSrcTGF!=NULL){
    s32 x=r.x;
    s32 y=r.y;
    s32 w=pSrcTGF->GetWidth();
    s32 h=pSrcTGF->GetHeight();
    x+=(r.w-w+0)/2;
    y+=(r.h-h+1)/2;
    pSrcTGF->BitBlt(pcc->pCanvas,x,y);
  }
  
  pcc->pCanvas->SetCglFont(pDefaultFont);
  pcc->pCanvas->SetFontTextColor(pcc->TextColor);
  
  const char *pmsgUTF8=pcc->pMsgUTF8;
  
  u32 TextX=r.x+r.w+2;
  u32 TextY=r.y;
//  u32 TextWidth=pcc->pCanvas->GetTextWidthUTF8(pmsgUTF8);
  u32 TextHeight=glCanvasTextHeight;
  TextY+=(r.h-TextHeight+1)/2;
  
//  pcc->pCanvas->SetColor(RGB15(0,0,0)|BIT15);
//  pcc->pCanvas->FillBox(TextX,TextY,64,TextHeight);
  
  pcc->pCanvas->TextOutUTF8(TextX,TextY,pmsgUTF8);
}

s32 ComponentCheck_GetWidth(TComponentCheck *pComponentCheck)
{
  TComponentCheck *pcc=pComponentCheck;
  
  if(pcc->Visible==false) return(0);
  
  if(pcc->pOnIcon!=NULL){
    if(pcc->Rect.w==0) pcc->Rect.w=pcc->pOnIcon->GetWidth();
  }
  
  TRect r=pcc->Rect;
  
  pcc->pCanvas->SetCglFont(pDefaultFont);
  
  const char *pmsgUTF8=pcc->pMsgUTF8;
  
  return(r.w+2+pcc->pCanvas->GetTextWidthUTF8(pmsgUTF8)+2);
}

static bool ComponentCheck_GetIndexFromPos(TComponentCheck *pComponentCheck,s32 mx,s32 my)
{
  TComponentCheck *pcc=pComponentCheck;
  TRect r=pcc->Rect;
  
  if(pcc->Visible==false) return(false);
  
  pcc->pCanvas->SetCglFont(pDefaultFont);
  const char *pmsgUTF8=pcc->pMsgUTF8;
  s32 TextWidth;
  if(str_isEmpty(pcc->pMsgUTF8)==true){
    TextWidth=ScreenWidth;
    }else{
    TextWidth=r.w+2+pcc->pCanvas->GetTextWidthUTF8(pmsgUTF8);
  }
  
  mx-=r.x;
  my-=r.y;
  
  if((0<=mx)&&(mx<TextWidth)){
    if((0<=my)&&(my<r.h)){
      return(true);
    }
  }
  
  return(false);
}

bool ComponentCheck_MouseUp(TComponentCheck *pComponentCheck,s32 mx,s32 my)
{
  TComponentCheck *pcc=pComponentCheck;
  
  if(pcc->Visible==false) return(false);
  
  if(ComponentCheck_GetIndexFromPos(pcc,mx,my)==false) return(false);
  
  if(pcc->CallBack_Click!=NULL) pcc->CallBack_Click(pcc);
  
  return(true);
}

// ------------------

void ComponentButton_Init(TComponentButton *pComponentButton,CglCanvas *pCanvas)
{
  TComponentButton *pcb=pComponentButton;
  
  pcb->CallBack_Click=NULL;
  pcb->pCanvas=pCanvas;
  pcb->pIcon=NULL;
  pcb->DrawFrame=true;
  pcb->Pressing=false;
  pcb->Rect=CreateRect(0,0,0,0);
  pcb->pMsgUTF8="";
  pcb->Visible=true;
  pcb->NormalTextColor=ColorTable.Component.Button_NormalText;
  pcb->PressTextColor=ColorTable.Component.Button_PressText;
}

void ComponentButton_Draw(TComponentButton *pComponentButton)
{
  TComponentButton *pcb=pComponentButton;
  
  if(pcb->Visible==false) return;
  
  if(pcb->DrawFrame==true){
    if(pcb->Pressing==false){
      TRect r=pcb->Rect;
      pcb->pCanvas->SetColor(ColorTable.Component.Button_NormalHighlight);
      pcb->pCanvas->DrawBox(r.x-1,r.y-1,r.w+2,r.h+2);
      pcb->pCanvas->SetColor(ColorTable.Component.Button_NormalShadow);
      pcb->pCanvas->DrawBox(r.x,r.y,r.w+1,r.h+1);
      pcb->pCanvas->SetColor(ColorTable.Component.Button_NormalBG);
      pcb->pCanvas->FillBox(r.x,r.y,r.w,r.h);
      }else{
      TRect r=pcb->Rect;
      pcb->pCanvas->SetColor(ColorTable.Component.Button_PressHighlight);
      pcb->pCanvas->DrawBox(r.x-1,r.y-1,r.w+2,r.h+2);
      pcb->pCanvas->SetColor(ColorTable.Component.Button_PressShadow);
      pcb->pCanvas->DrawBox(r.x,r.y,r.w+1,r.h+1);
      pcb->pCanvas->SetColor(ColorTable.Component.Button_PressBG);
      pcb->pCanvas->FillBox(r.x,r.y,r.w,r.h);
    }
  }
  
  pcb->pCanvas->SetCglFont(pDefaultFont);
  if(pcb->Pressing==false){
    pcb->pCanvas->SetFontTextColor(pcb->NormalTextColor);
    }else{
    pcb->pCanvas->SetFontTextColor(pcb->PressTextColor);
  }
  
  const char *pmsgUTF8=pcb->pMsgUTF8;
  
  TRect r=pcb->Rect;
  
  s32 IconWidth;
  s32 IconHeight;
  if(pcb->pIcon==NULL){
    IconWidth=-8;
    IconHeight=0;
    }else{
    IconWidth=pcb->pIcon->GetWidth();
    IconHeight=pcb->pIcon->GetHeight();
  }
  
  s32 TextWidth=pcb->pCanvas->GetTextWidthUTF8(pmsgUTF8);
  s32 TextHeight=glCanvasTextHeight;
  
  s32 BodyWidth=IconWidth;
  if(TextWidth!=0) BodyWidth+=8+TextWidth;
  s32 BodyHeight=IconHeight;
  if(IconHeight<TextHeight) BodyHeight=TextHeight;
  s32 BodyX=r.x+((r.w-BodyWidth)/2);
  s32 BodyY=r.y+((r.h-BodyHeight)/2);
  
  if(pcb->pIcon!=NULL){
    s32 x=BodyX;
    s32 y=BodyY+((BodyHeight-IconHeight)/2);
    pcb->pIcon->BitBlt(pcb->pCanvas,x,y);
  }
  
  pcb->pCanvas->TextOutUTF8(BodyX+8+IconWidth,BodyY+((BodyHeight-TextHeight)/2),pmsgUTF8);
}

s32 ComponentButton_GetIndexFromPos(TComponentButton *pComponentButton,s32 mx,s32 my)
{
  TComponentButton *pcb=pComponentButton;
  TRect r=pcb->Rect;
  
  if(pcb->Visible==false) return(-1);
  
  mx-=r.x;
  my-=r.y;
  
  if((0<=mx)&&(mx<r.w)){
    if((0<=my)&&(my<r.h)){
      return(0);
    }
  }
  
  return(-1);
}

bool ComponentButton_MouseUp(TComponentButton *pComponentButton,s32 mx,s32 my)
{
  TComponentButton *pcb=pComponentButton;
  
  if(pcb->Visible==false) return(false);
  
  if(ComponentButton_GetIndexFromPos(pcb,mx,my)==-1) return(false);
  
  if(pcb->CallBack_Click!=NULL) pcb->CallBack_Click(pcb);
  
  return(true);
}

