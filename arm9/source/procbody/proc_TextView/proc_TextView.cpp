
#pragma Ospace

#include <nds.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "_console.h"
#include "_consolewritelog.h"
#include "maindef.h"
#include "memtool.h"
#include "_const.h"
#include "../../ipc6.h"

#include "procstate.h"
#include "datetime.h"
#include "ErrorDialog.h"

#include "glib/glib.h"

#include "fat2.h"
#include "shell.h"
#include "skin.h"

#include "skin.h"
#include "component.h"
#include "sndeff.h"
#include "lang.h"
#include "strpcm.h"
#include "rect.h"
#include "resume.h"
#include "strtool.h"
#include "cfont.h"
#include "cctf.h"

// -----------------------------

static bool ToCustomMode;

static CglCanvas *pTmpBM;

#include "proc_TextView_Popup.h"

static u32 ClockTimeOut;

// -----------------------------

static void InitMsg_Init(void)
{
  pScreenMainOverlay->pCanvas->FillFull(0);
}

static void InitMsg_Draw(const char *pstra)
{
  CglCanvas *pcan=pScreenMainOverlay->pCanvas;
  
  u32 x,y;
  
  x=(ScreenWidth-pcan->GetTextWidthUTF8(pstra))/2;
  y=(ScreenHeight-glCanvasTextHeight)/2;
  
  pcan->SetColor(RGB15(8,8,8)|BIT15);
  pcan->FillFast(0,y-2,ScreenWidth,glCanvasTextHeight+2+2);
  
  pcan->SetFontTextColor(RGB15(0,0,0)|BIT15);
  pcan->TextOutUTF8(x+1,y+1,pstra);
  
  pcan->SetFontTextColor(RGB15(31,31,31)|BIT15);
  pcan->TextOutUTF8(x,y,pstra);
}

static void InitMsg_Free(void)
{
  InitMsg_Init();
}

// -----------------------------

#define ScrollBarWidth (12)

#define LeftMargin (6)
#define RightMargin (4+ScrollBarWidth)
#define TopMargin (8)
#define BottomMargin (2)

#define LineWidth (ScreenWidth-LeftMargin-RightMargin)
#define LineHeight (ScreenHeight-TopMargin-BottomMargin)

#include "proc_TextView_extfont.h"

// -----------------------------

#include "proc_TextView_dfs.h"
#include "proc_TextView_libconv.h"

// -----------------------------

static s32 ShowLineHeight,ShowLineMargin,ShowLineCount;

static s32 CurrentLineIndex;

static s32 GetCurrentLineIndex(void)
{
  return(CurrentLineIndex);
}

static void SetCurrentLineIndex(s32 lineidx)
{
  CurrentLineIndex=lineidx;
  
  if(CurrentLineIndex<0){
    CurrentLineIndex=0;
    }else{
    if(TextLinesCount<ShowLineCount){
      CurrentLineIndex=0;
      }else{
      if((TextLinesCount-ShowLineCount)<=CurrentLineIndex) CurrentLineIndex=TextLinesCount-ShowLineCount;
    }
  }
}

// -----------------------------

static void DrawCurrentText_Main(CglCanvas *pcan);
static void DrawCurrentText_Sub(CglCanvas *pcan);
static void RedrawSubScreen(void);

#include "proc_TextView_bookmark_fileio.h"
#include "proc_TextView_bookmark_ui.h"

// -----------------------------

static void DrawCurrentText_ins_DrawScrollBar(CglCanvas *pcan)
{
  s32 w=ScrollBarWidth,x=ScreenWidth-w;
  w--;
  
  s32 h,y;
  
  if(TextLinesCount<=ShowLineCount){
    y=0;
    h=ScreenHeight;
    }else{
    y=(CurrentLineIndex*ScreenHeight)/TextLinesCount;
    h=(ShowLineCount*ScreenHeight)/TextLinesCount;
  }
  
  if(h<8) h=8;
  if(ScreenHeight<(y+h)) y=ScreenHeight-h;
  
  pcan->SetColor(ColorTable.TextView.ScrollBar_Frame);
  pcan->DrawBox(x,y,w,h);
  
  x+=1; y+=1; w-=2; h-=2;
  u16 *pbuf=pcan->GetScanLine(y);
  pbuf+=x;
  u32 bufsize=pcan->GetWidth();
  
  u16 BGColor=ColorTable.TextView.ScrollBar_Inside;
  for(s32 py=0;py<h;py++){
    for(s32 px=0;px<w;px++){
      u16 col=pbuf[px];
      u16 mask=RGB15(30,30,30);
      col=(col&mask)>>1;
      pbuf[px]=(BGColor+col)|BIT15;
    }
    pbuf+=bufsize;
  }
}

static void DrawCurrentText_BookMark_FillLine(s32 LineNum,CglCanvas *pcan,u32 y)
{
  if(LineNum<0) return;
  
  u32 x=2;
  u32 w=ScreenWidth-ScrollBarWidth-2-x;
  u32 h=ShowLineHeight-ShowLineMargin+1;
  
  for(u32 idx=0;idx<BookmarkItemCount;idx++){
    TBookmarkItem *pbmi=Bookmark_Load(idx);
    if(LineNum==(pbmi->LineNum-1)){
      const u16 col=ColorTable.TextView.Bookmark_FillBox[idx];
      if(col!=(RGB15(0,0,0)|BIT15)){
        pcan->SetColor(col);
        pcan->FillBox(x,y,w,h);
      }
    }
  }
}

static void DrawCurrentText_Main(CglCanvas *pcan)
{
  TextView_GetSkin(ETV_PageBG)->pCanvas->BitBltFullBeta(pcan);
  
  pExtFont->Color=ColorTable.TextView.MainText;
  
  pcan->SetColor(ColorTable.TextView.Line);
  
  bool firstline=true;
  
  for(u32 idx=0;idx<ShowLineCount;idx++){
    UnicodeChar strw[128+1];
    if(libconv_GetTextLine(CurrentLineIndex+idx,strw)==true){
      if(firstline==true){
        firstline=false;
        u32 y=TopMargin+(ShowLineHeight*(idx-1));
        y++;
        y+=ShowLineHeight-ShowLineMargin;
        pcan->DrawLine(2,y,ScreenWidth-ScrollBarWidth-2,y);
      }
      u32 x=LeftMargin;
      u32 y=TopMargin+(ShowLineHeight*idx);
      DrawCurrentText_BookMark_FillLine(CurrentLineIndex+idx,pcan,y);
      y++;
      ExtFont_TextOutW(pcan,x,y,strw);
      
      y+=ShowLineHeight-ShowLineMargin;
      pcan->DrawLine(2,y,ScreenWidth-ScrollBarWidth-2,y);
    }
  }
  
  DrawCurrentText_ins_DrawScrollBar(pcan);
}

static void DrawCurrentText_Sub(CglCanvas *pcan)
{
  TextView_GetSkin(ETV_PageBG)->pCanvas->BitBltFullBeta(pcan);
  
  pExtFont->Color=ColorTable.TextView.MainText;
  
  pcan->SetColor(ColorTable.TextView.Line);
  
  bool firstline=true;
  
  for(u32 idx=0;idx<ShowLineCount;idx++){
    if(idx==0){
      u32 x=LeftMargin;
      u32 y=TopMargin+1+(ShowLineHeight*idx);
      char str[128];
      snprintf(str,128,Lang_GetUTF8("TV_Info"),1+CurrentLineIndex,TextLinesCount,(float)CurrentLineIndex*100/TextLinesCount);
      pcan->SetFontTextColor(ColorTable.TextView.MainText);
      pcan->TextOutUTF8(x,y,str);
      }else{
      
      if(ShowLineCount<=(CurrentLineIndex+idx)){
        UnicodeChar strw[128+1];
        if(libconv_GetTextLine(CurrentLineIndex+idx-ShowLineCount,strw)==true){
          if(firstline==true){
            firstline=false;
            u32 y=TopMargin+(ShowLineHeight*(idx-1));
            y++;
            y+=ShowLineHeight-ShowLineMargin;
            pcan->DrawLine(2,y,ScreenWidth-ScrollBarWidth-2,y);
          }
          u32 x=LeftMargin;
          u32 y=TopMargin+(ShowLineHeight*idx);
          DrawCurrentText_BookMark_FillLine(CurrentLineIndex+idx-ShowLineCount,pcan,y);
          y++;
          ExtFont_TextOutW(pcan,x,y,strw);
        
          y+=ShowLineHeight-ShowLineMargin;
          pcan->DrawLine(2,y,ScreenWidth-ScrollBarWidth-2,y);
        }
      }
    }
  }
}

// -----------------------------

asm u16* TextView_DrawID3Tag_asm_Fill25per(u16 *pbuf,u32 size)
{
  push {r4,r5,r6,lr}
  
  ldr r2,=0x739C739C // RGB15(28,28,28)<<16 | RGB15(28,28,28)
  ldr r3,=(1<<15)|(1<<31)
  
TextView_DrawID3Tag_asm_Fill25per_loop
  
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
  bne TextView_DrawID3Tag_asm_Fill25per_loop
  
  pop {r4,r5,r6,pc}
}

asm u16* TextView_DrawID3Tag_asm_Fill50per(u16 *pbuf,u32 size)
{
  push {r4,r5,r6,lr}
  
  ldr r2,=0x7BDE7BDE // RGB15(30,30,30)<<16 | RGB15(30,30,30)
  ldr r3,=(1<<15)|(1<<31)
  
TextView_DrawID3Tag_asm_Fill50per_loop
  
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
  bne TextView_DrawID3Tag_asm_Fill50per_loop
  
  pop {r4,r5,r6,pc}
}

static void DrawInfo(CglCanvas *pDstBM)
{
  u32 LinesCount=4;
  
  u32 x=8,y=8,h=glCanvasTextHeight+2;
  
  {
    const u32 DstBMWidth=ScreenWidth; // pDstBM->GetWidth();
    
    u16 *pb=pDstBM->GetVRAMBuf();
    u32 size;
    
    size=4*DstBMWidth;
    pb+=size;
    
    size=2*DstBMWidth;
    pb=TextView_DrawID3Tag_asm_Fill50per(pb,size);
    
    size=(2+(h*LinesCount)+0)*DstBMWidth;
    pb=TextView_DrawID3Tag_asm_Fill25per(pb,size);
    
    size=2*DstBMWidth;
    pb=TextView_DrawID3Tag_asm_Fill50per(pb,size);
  }
  
  if(ProcState.FileList.ScreenSaver_BlackMode==false){
    pDstBM->SetFontTextColor(ColorTable.FileList.ID3TagWhiteModeText);
    }else{
    pDstBM->SetFontTextColor(ColorTable.FileList.ID3TagBlackModeText);
  }
  
  char str[128];
  
  pDstBM->TextOutUTF8(x,y,Lang_GetUTF8("TV_Help1"));
  y+=h;
  pDstBM->TextOutUTF8(x,y,Lang_GetUTF8("TV_Help2"));
  y+=h;
  snprintf(str,128,Lang_GetUTF8("TV_Help3"),pEncodeID);
  pDstBM->TextOutUTF8(x,y,str);
  y+=h;
  snprintf(str,128,Lang_GetUTF8("TV_Info"),1+CurrentLineIndex,TextLinesCount,(float)CurrentLineIndex*100/TextLinesCount);
  pDstBM->TextOutUTF8(x,y,str);
  y+=h;
}

// -----------------------------

#include "proc_TextView_clock.h"

static void RedrawSubScreen(void)
{
  switch(ProcState.Text.TopScrMode){
    case ETTSM_LightOff: {
      pScreenSub->pCanvas->FillFull(RGB15(0,0,0)|BIT15);
      IPC6->LCDPowerControl=LCDPC_ON_BOTTOM;
    } break;
    case ETTSM_Text: {
      DrawCurrentText_Sub(pScreenSub->pCanvas);
    } break;
    case ETTSM_Clock: {
      Clock_Standby_Draw(pTmpBM);
      DrawInfo(pTmpBM);
      pTmpBM->BitBltFullBeta(pScreenSub->pCanvas);
      IPC6->LCDPowerControl=LCDPC_ON_BOTH;
    } break;
  }
}

// -----------------------------

enum ECompLabels {ECLSCount};
#define CompLabelsCount (ECLSCount)
static TComponentLabel CompLabels[CompLabelsCount];

enum ECompChecks {ECCSCount};
#define CompChecksCount (ECCSCount)
static TComponentCheck CompChecks[CompChecksCount];

enum ECompButtons {ECBSCount};
#define CompButtonsCount (ECBSCount)
static TComponentButton CompButtons[CompButtonsCount];

static void Setting_Redraw(void)
{
  {
    CglCanvas *pcan=pScreenMainOverlay->pCanvas;
    pcan->FillFull(0);
  }
  
  for(u32 idx=0;idx<CompLabelsCount;idx++){
    ComponentLabel_Draw(&CompLabels[idx]);
  }
  for(u32 idx=0;idx<CompChecksCount;idx++){
    ComponentCheck_Draw(&CompChecks[idx]);
  }
  for(u32 idx=0;idx<CompButtonsCount;idx++){
    ComponentButton_Draw(&CompButtons[idx]);
  }
}

static void CompsInit(void)
{
  CglCanvas *pcan=pScreenMainOverlay->pCanvas;
  
  for(u32 idx=0;idx<CompLabelsCount;idx++){
    ComponentLabel_Init(&CompLabels[idx],pcan);
  }
  for(u32 idx=0;idx<CompChecksCount;idx++){
    ComponentCheck_Init(&CompChecks[idx],pcan);
  }
  for(u32 idx=0;idx<CompButtonsCount;idx++){
    ComponentButton_Init(&CompButtons[idx],pcan);
  }
  
}

// -----------------------------

static void CB_KeyPress(u32 VsyncCount,u32 Keys)
{
  if((Keys&(KEY_L|KEY_R))!=0){
    if((Keys&(KEY_X|KEY_Y))!=0){
      if((Keys&KEY_X)!=0) ChangeNextBacklightLevel();
      if((Keys&KEY_Y)!=0) ChangePrevBacklightLevel();
      Popup_Show_BacklightLevel();
    }
    return;
  }
  
  if((Keys&(KEY_X|KEY_Y))!=0){
    if((Keys&KEY_Y)!=0) BookmarkUI_Start(false);
    if((Keys&KEY_X)!=0) BookmarkUI_Start(true);
    return;
  }
  
  if((Keys&KEY_B)!=0){
    Sound_Start(WAVFN_Click);
    ToCustomMode=false;
    SetNextProc(ENP_FileList,EPFE_CrossFade);
  }
  
  if((Keys&(KEY_START|KEY_SELECT))!=0){
    Sound_Start(WAVFN_Click);
    ToCustomMode=true;
    SetNextProc(ENP_TextCustom,EPFE_RightToLeft);
  }
  
  if((Keys&(KEY_A|KEY_UP|KEY_DOWN|KEY_LEFT|KEY_RIGHT))!=0){
    s32 v=0;
    if((Keys&KEY_A)!=0) v=ShowLineCount-1;
    if((Keys&KEY_UP)!=0) v=-1;
    if((Keys&KEY_DOWN)!=0) v=1;
    if((Keys&KEY_LEFT)!=0) v=-(ShowLineCount-1);
    if((Keys&KEY_RIGHT)!=0) v=ShowLineCount-1;
    if((Keys&(KEY_L|KEY_R))!=0) v*=2;
    if(v!=0){
      SetCurrentLineIndex(CurrentLineIndex+v);
      DrawCurrentText_Main(pScreenMain->pBackCanvas);
      RedrawSubScreen();
      ScreenMain_Flip_ProcFadeEffect();
    }
  }
}

static bool deskmf;
static TComponentButton *pPressingButton;

static bool mscrf;
static s32 mscry;

static bool mbodyf;
static s32 mbodyy;

static void CB_MouseDown(s32 x,s32 y)
{
  deskmf=false;
  mscrf=false;
  mbodyf=false;
  
  if((ScreenWidth-32)<=x){
    if(TextLinesCount<=ShowLineCount){
      }else{
      mscrf=true;
      s32 sy=(CurrentLineIndex*ScreenHeight)/TextLinesCount;
      s32 sh=(ShowLineCount*ScreenHeight)/TextLinesCount;
      if((sy<=y)&&(y<(sy+sh))){
        mscry=y-sy;
        }else{
        mscry=sh/2;
      }
      y-=mscry;
      if(y<0) y=0;
      s32 lineidx=y*TextLinesCount/ScreenHeight;
      SetCurrentLineIndex(lineidx);
      DrawCurrentText_Main(pScreenMain->pBackCanvas);
      RedrawSubScreen();
      ScreenMain_Flip_ProcFadeEffect();
      return;
    }
  }
  
  mbodyf=true;
  mbodyy=y;
  
  for(u32 idx=0;idx<CompButtonsCount;idx++){
    TComponentButton *pcb=&CompButtons[idx];
    if(ComponentButton_GetIndexFromPos(pcb,x,y)!=-1){
      pPressingButton=pcb;
      pcb->Pressing=true;
      ComponentButton_Draw(pcb);
      Setting_Redraw();
      deskmf=true;
      return;
    }
  }
}

static void CB_MouseMove(s32 x,s32 y)
{
  if(mscrf==true){
    y-=mscry;
    if(y<0) y=0;
    s32 lineidx=y*TextLinesCount/ScreenHeight;
    SetCurrentLineIndex(lineidx);
    DrawCurrentText_Main(pScreenMain->pBackCanvas);
    RedrawSubScreen();
    ScreenMain_Flip_ProcFadeEffect();
    return;
  }
  
  if(mbodyf==true){
    s32 h=ShowLineHeight/2; // タッチの倍の速度でスクロールする
    s32 mv=y-mbodyy;
    s32 v=0;
    while(mv<=-h){
      v++;
      mv+=h;
      mbodyy-=h;
    }
    while(h<=mv){
      v--;
      mv-=h;
      mbodyy+=h;
    }
    if(v!=0){
      SetCurrentLineIndex(CurrentLineIndex+v);
      DrawCurrentText_Main(pScreenMain->pBackCanvas);
      RedrawSubScreen();
      ScreenMain_Flip_ProcFadeEffect();
    }
  }
  
  if(deskmf==false) return;
  
  for(u32 idx=0;idx<CompButtonsCount;idx++){
    TComponentButton *pcb=&CompButtons[idx];
    if(pcb==pPressingButton){
      if(ComponentButton_GetIndexFromPos(pcb,x,y)==-1){
        if(pcb->Pressing==true){
          pcb->Pressing=false;
          ComponentButton_Draw(pcb);
          Setting_Redraw();
        }
        }else{
        if(pcb->Pressing==false){
          pcb->Pressing=true;
          ComponentButton_Draw(pcb);
          Setting_Redraw();
        }
      }
    }
  }
}

static void CB_MouseUp(s32 x,s32 y)
{
  mscrf=false;
  
  mbodyf=false;
  
  if(deskmf==false) return;
  deskmf=false;
  
  if(pPressingButton!=NULL){
    pPressingButton->Pressing=false;
    ComponentButton_Draw(pPressingButton);
    
    for(u32 idx=0;idx<CompButtonsCount;idx++){
      TComponentButton *pcb=&CompButtons[idx];
      if(pcb==pPressingButton){
        if(ComponentButton_GetIndexFromPos(pcb,x,y)!=-1){
          ComponentButton_MouseUp(&CompButtons[idx],x,y);
        }
      }
    }
    
    Setting_Redraw();
    pPressingButton=NULL;
    return;
  }
  
  for(u32 idx=0;idx<CompLabelsCount;idx++){
    if(ComponentLabel_MouseUp(&CompLabels[idx],x,y)==true) Setting_Redraw();
  }
  for(u32 idx=0;idx<CompChecksCount;idx++){
    if(ComponentCheck_MouseUp(&CompChecks[idx],x,y)==true) Setting_Redraw();
  }
  
}

static void CB_Start(void)
{
  deskmf=false;
  pPressingButton=NULL;
  mscrf=false;
  mbodyf=false;
  
  ToCustomMode=false;
  
  CompsInit();
  
  Bookmark_Init();
  
  InitMsg_Init();
  
  u32 FontSize=ProcState.Text.FontSize;
  switch(FontSize){
    case Text_FontSize_Small: ShowLineMargin=2; break;
    case Text_FontSize_Middle: ShowLineMargin=4; break;
    case Text_FontSize_Large: ShowLineMargin=5; break;
    default: {
      _consolePrintf("Fatal error: Illigal font size. (%d)\n",ProcState.Text.FontSize);
      ShowLogHalt();
    } break;
  }
  
  switch(ProcState.Text.LineSpace){
    case ETLS_Small: ShowLineMargin/=2; break;
    case ETLS_Middle: ShowLineMargin*=1; break;
    case ETLS_Large: ShowLineMargin*=2; break;
  }
  
  ShowLineHeight=FontSize+ShowLineMargin;
  
  ShowLineCount=(ScreenHeight-TopMargin-BottomMargin)/ShowLineHeight;
  
  InitMsg_Draw(Lang_GetUTF8("TV_PRG_LoadExtendFont"));
  ExtFont_Init();
  
  {
    FAT_FILE *pSwapFile=Shell_FAT_fopen_SwapFile();
    DFS_Init(pSwapFile);
    FAT2_fclose(pSwapFile);
  }
  
  libconv_Init();
  
  PrintFreeMem();
  
  FAT_FILE *pfh=Shell_FAT_fopen_Split(RelationalFilePathUnicode,RelationalFileNameUnicode);
  if(pfh==NULL){
    _consolePrintf("Fatal error: Can not open text file.\n");
    ShowLogHalt();
  }
  
  u32 bufsize=FAT2_GetFileSize(pfh);
  u8 *pbuf=NULL;
  if(bufsize==0){
    _consolePrintf("Not support 0byte text file.\n");
    ErrorDialog_Set(EEC_Text0byte);
    }else{
    pbuf=(u8*)safemalloc((bufsize+3)&~3);
    if(pbuf==NULL){
      _consolePrintf("Memory overflow. %dbyte.\n",bufsize);
      bufsize=0;
      }else{
      void *pdummy=safemalloc(32*1024);
      if(pdummy==NULL){
        _consolePrintf("Memory overflow for reserved area.\n");
        bufsize=0;
        safefree(pbuf); pbuf=NULL;
        }else{
        safefree(pdummy); pdummy=NULL;
        _consolePrintf("pbuf=0x%08x, bufsize=%dbyte.\n",pbuf,bufsize);
        InitMsg_Draw(Lang_GetUTF8("TV_PRG_LoadTextFile"));
        FAT2_fread(pbuf,1,bufsize,pfh);
      }
    }
  }
  
  FAT2_fclose(pfh);
  
  PrintFreeMem();
  
  if(bufsize==0){
    libconv_EndConvert();
    InitMsg_Free();
    Bookmark_Free();
    _consolePrintf("Text reader: file not found or size limit error.\n");
    ErrorDialog_Set(EEC_MemoryOverflow_CanRecovery);
    ToCustomMode=false;
    SetNextProc(ENP_FileList,EPFE_CrossFade);
    return;
  }
  
  if(ManualTextEncode_OverrideFlag==true){
    ManualTextEncode_OverrideFlag=false;
    Bookmark_SetTextEncode(ManualTextEncode);
  }
  
  switch(Bookmark_GetTextEncode()){
    case ETE_Auto: {
      InitMsg_Draw(Lang_GetUTF8("TV_PRG_DetectTextEncode"));
      const u32 maxbufsize=16*1024;
      if(bufsize<maxbufsize){
        libconv_AutoSelectEncode(pbuf,bufsize);
        }else{
        libconv_AutoSelectEncode(pbuf,maxbufsize);
      }
      _consolePrintf("Detected text encode is %s.\n",pEncodeID);
    } break;
    case ETE_ANSI: libconv_SelectEncode_ANSI(); break;
    case ETE_SJIS: libconv_SelectEncode_SJIS(); break;
    case ETE_UTF16BE: libconv_SelectEncode_UTF16BE(); break;
    case ETE_UTF16LE: libconv_SelectEncode_UTF16LE(); break;
    case ETE_UTF8: libconv_SelectEncode_UTF8(); break;
  }
  
  InitMsg_Draw(Lang_GetUTF8("TV_PRG_DetectReturnCode"));
  libconv_DetectReturnCode(pbuf,bufsize);
  switch(ReturnCode){
    case ERC_Unknown: _consolePrint("Detected return code is unknown.\n"); break;
    case ERC_CR: _consolePrint("Detected return code is CR.\n"); break;
    case ERC_LF: _consolePrint("Detected return code is LF.\n"); break;
    case ERC_CRLF: _consolePrint("Detected return code is CR+LF.\n"); break;
    case ERC_LFCR: _consolePrint("Detected return code is LF+CR.\n"); break;
  }
  
  InitMsg_Draw(Lang_GetUTF8("TV_PRG_ConvertToUnicode"));
  libconv_Convert(pbuf,bufsize,pCglFontDefault);
  _consolePrintf("Total lines count is %d.\n",TextLinesCount);
  
  if(pbuf!=NULL){
    safefree(pbuf); pbuf=NULL;
  }
  
  libconv_EndConvert();
  
  InitMsg_Draw(Lang_GetUTF8("TV_PRG_LoadExtendFont"));
  ExtFont_LoadBody();
  
  // テキスト変換後にスキンを読み込む
  InitMsg_Draw(Lang_GetUTF8("TV_PRG_LoadSkinData"));
  Skin_Load_TextView_AfterLoad();
  pTmpBM=new CglCanvas(NULL,ScreenWidth,ScreenHeight,pf15bit);
  pTmpBM->SetCglFont(pCglFontDefault);
  
  InitMsg_Free();
  
  {
    u32 linenum=0;
    if(RelationalFilePos!=0) linenum=RelationalFilePos;
    if(Bookmark_GetResumeLineNum()!=0){
      linenum=Bookmark_GetResumeLineNum()-1;
      Bookmark_SetResumeLineNum(0);
    }
    SetCurrentLineIndex(linenum);
  }
  
  DrawCurrentText_Main(pScreenMain->pBackCanvas);
  RedrawSubScreen();
  ScreenMain_Flip_ProcFadeEffect();
  
  Resume_SetResumeMode(ERM_Text);
  Resume_SetFilename(ConvertFull_MargeFromSplit(RelationalFilePathUnicode,RelationalFileNameUnicode));
  Resume_SetPos(GetCurrentLineIndex());
  Resume_Save();
  
  Sound_Start(WAVFN_Open);
  
  ClockTimeOut=0;
}

static void CB_VsyncUpdate(u32 VsyncCount)
{
  Popup_VsyncUpdate(VsyncCount);
  
  if(ClockTimeOut==0){
    ClockTimeOut=60;
    RedrawSubScreen();
    }else{
    ClockTimeOut--;    
  }
}

static void CB_End(void)
{
  if(Bookmark_Enabled==true) Bookmark_SetResumeLineNum(1+GetCurrentLineIndex());
  Bookmark_Free();
  
  Resume_Clear();
  
  if(ToCustomMode==false) RelationalFile_Clear();
  
  if(pTmpBM!=NULL){
    delete pTmpBM; pTmpBM=NULL;
  }
  
  ExtFont_Free();
  
  Popup_Free();
  
  libconv_Free();
  
  DFS_Free();
  
  ProcState_RequestSave=true;
  ProcState_Save();
}

void ProcTextView_SetCallBack(TCallBack *pCallBack)
{
  pCallBack->Start=CB_Start;
  pCallBack->VsyncUpdate=CB_VsyncUpdate;
  pCallBack->End=CB_End;
  pCallBack->KeyPress=CB_KeyPress;
  pCallBack->MouseDown=CB_MouseDown;
  pCallBack->MouseMove=CB_MouseMove;
  pCallBack->MouseUp=CB_MouseUp;
}

