
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <NDS.h>

#include "_console.h"
#include "_const.h"

#include "cfont.h"

#include "strtool.h"
#include "memtool.h"
#include "shell.h"

CFont::CFont(EBitmapMode _BitmapMode,CglB15 *_pB15,CglTGF *_pTGF)
{
  BitmapMode=_BitmapMode;
  
  pB15=NULL;
  pTGF=NULL;
  
  switch(BitmapMode){
    case EBM_B15: pB15=_pB15; break;
    case EBM_TGF: pTGF=_pTGF; break;
  }
  
  ImplantCharsCount=DefaultImplantCharsCount;
  pImplantChars=DefaultImplantChars;
  
  u32 fh=0,fw=0;
  
  switch(BitmapMode){
    case EBM_B15: {
      fh=pB15->GetHeight();
      fw=pB15->GetWidth();
    } break;
    case EBM_TGF: {
      fh=pTGF->GetHeight();
      fw=pTGF->GetWidth();
    } break;
  }
  
  CharHeight=fh/ImplantCharsCount;
  CharWidth=fw;
  
  for(u32 idx=0;idx<ImplantCharsCount;idx++){
    CharWidths[idx]=CharWidth;
    CharPads[idx]=-1;
  }
  
  CharWidths[10]=CharWidths[10]/2;
  CharPads[10]=CharPads[10];
  CharWidths[11]=(CharWidths[11]*3)/4;
  CharPads[11]=CharPads[11];
  
  SpaceWidth=CharWidth/2;
}

CFont::~CFont(void)
{
}

u32 CFont::DrawChar(CglCanvas *pCanvas,u32 x,u32 y,char c)
{
  y-=CharHeight;
  
  u32 cidx=(u32)-1;
  
  for(u32 idx=0;idx<ImplantCharsCount;idx++){
    if(pImplantChars[idx]==c) cidx=idx;
  }
  
  if(cidx==(u32)-1) return(SpaceWidth);
  
  u32 w=CharWidths[cidx];
  
  switch(BitmapMode){
    case EBM_B15: pB15->BitBlt(pCanvas,x,y,w,CharHeight,(CharWidth-w)/2,cidx*CharHeight); break;
    case EBM_TGF: pTGF->BitBltLimitY(pCanvas,x-((CharWidth-w)/2),y,CharHeight,cidx*CharHeight); break;
  }
  
  w+=CharPads[cidx];
  
  return(w);
}

u32 CFont::DrawText(CglCanvas *pCanvas,u32 x,u32 y,const char *pstr)
{
  u32 w=0;
  
  while(*pstr!=0){
    w+=DrawChar(pCanvas,x+w,y,*pstr);
    pstr++;
  }
  
  return(w);
}

u32 CFont::GetTextWidth(const char *pstr)
{
  u32 w=0;
  
  while(*pstr!=0){
    u32 cidx=(u32)-1;
    
    for(u32 idx=0;idx<ImplantCharsCount;idx++){
      if(pImplantChars[idx]==*pstr) cidx=idx;
    }
    
    if(cidx==(u32)-1){
      w+=SpaceWidth;
      }else{
      w+=CharWidths[cidx];
      w+=CharPads[cidx];
    }
    
    pstr++;
  }
  
  return(w);
}

u32 CFont::GetTextHeight(void)
{
  return(CharHeight);
}

