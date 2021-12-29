
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <nds.h>

#include "_const.h"
#include "maindef.h"
#include "_console.h"
#include "_consoleWriteLog.h"

#include "plugin/plug_ndsrom.h"

#include "memtool.h"
#include "strpcm.h"
#include "dll.h"
#include "dllsound.h"
#include "lang.h"
#include "resume.h"
#include "procstate.h"
#include "launchstate.h"
#include "shell.h"
#include "skin.h"
#include "extmem.h"
#include "splash.h"
#include "BootROM.h"
#include "ErrorDialog.h"
#include "component.h"
#include "extlink.h"
#include "strtool.h"

#include "../../ipc6.h"

#include "arm9tcm.h"
#include "setarm9_reg_waitcr.h"

#include "fat2.h"
#include "zlibhelp.h"

#include "sndeff.h"
#include "datetime.h"

#include "fpga_helper.h"

extern "C" {
extern u32 dtcmend;
extern u32 mtcmend;
}

u32 *pDTCMEND,*pMTCMEND;

CglFont *pCglFontDefault=NULL;

bool isExistsROMEO2;

ENextProc NextProc;

UnicodeChar RelationalFilePathUnicode[MaxFilenameLength];
UnicodeChar RelationalFileNameUnicode[MaxFilenameLength];
u32 RelationalFilePos;

ETextEncode ManualTextEncode;
bool ManualTextEncode_OverrideFlag;

// ------------------------------------------------------------------

void ShowLogHalt(void)
{
  _consoleLogResume();
  IPC6->LCDPowerControl=LCDPC_ON_BOTH;
  videoSetModeSub(MODE_2_2D | DISPLAY_BG2_ACTIVE);
  
  _consoleSetLogOutFlag(true);
  
  _consolePrint("\n     Application halted!!\n");
  
  if(_consoleGetLogFile()==true){
    _consolePrint("Please refer [/moonshl2/logbuf.txt]\n\n");
    }else{
    _consolePrint("\n");
  }
  while(1);
}

static __attribute__ ((always_inline)) void _ttywrch_tx_waitsend(const u32 data)
{
	while((*((vu8 *)0x0A000001) & 2)!=0);
	*((vu8 *)0x0A000000)=(u8)data;
}

extern "C" {
extern void _ttywrch(int ch);
}

void _ttywrch(int ch)
{
  _ttywrch_tx_waitsend(0xa0);
  _ttywrch_tx_waitsend(2);
  _ttywrch_tx_waitsend((u32)ch);
}

// ------------------------------------------------------

#include "chrglyph_999.h"

static void LoadDefaultFont(void)
{
  if(pCglFontDefault!=NULL){
    delete pCglFontDefault; pCglFontDefault=NULL;
  }
  pCglFontDefault=new CglFont(chrglyph_999,chrglyph_999_Size);
  
  pScreenMain->pBackCanvas->SetCglFont(pCglFontDefault);
  pScreenMain->pViewCanvas->SetCglFont(pCglFontDefault);
  pScreenMainOverlay->pCanvas->SetCglFont(pCglFontDefault);
  pScreenSub->pCanvas->SetCglFont(pCglFontDefault);
}

static void LangInitAndLoadFont(void)
{
  Shell_FAT_fopen_LanguageInit();
  
  FAT_FILE *pf=Shell_FAT_fopen_Language_chrglyph();
  if(pf==NULL){
    _consolePrintf("Fatal error: Can not found font file.\n");
    ShowLogHalt();
  }

  u32 bufsize=FAT2_GetFileSize(pf);
  void *pdummy=(u8*)safemalloc((bufsize*2)+(192*1024));
  u8 *pbuf=(u8*)safemalloc(bufsize);
  
  FAT2_fread_fast(pbuf,1,bufsize,pf);
  FAT2_fclose(pf);
  
  safefree(pdummy); pdummy=NULL;
  
  if(pCglFontDefault!=NULL){
    delete pCglFontDefault; pCglFontDefault=NULL;
  }
  pCglFontDefault=new CglFont((const u8*)pbuf,bufsize);
  
  safefree(pbuf); pbuf=NULL;
  
  pScreenMain->pBackCanvas->SetCglFont(pCglFontDefault);
  pScreenMain->pViewCanvas->SetCglFont(pCglFontDefault);
  pScreenMainOverlay->pCanvas->SetCglFont(pCglFontDefault);
  pScreenSub->pCanvas->SetCglFont(pCglFontDefault);
  
  _consolePrintf("Loaded font for your language.\n");
}

static void DrawBootWarn(const void *pSrcBuf,u32 SrcSize,u32 palmode)
{
  TZLIBData zd;
  
  zd.DstSize=ScreenWidth*ScreenHeight;
  zd.pDstBuf=(u8*)safemalloc(zd.DstSize);
  zd.SrcSize=SrcSize;
  zd.pSrcBuf=(u8*)pSrcBuf;
  
  if((zd.pSrcBuf[0]!=0x78)||(zd.pSrcBuf[1]!=0x01)){
    _consolePrintf("Fatal error: Unknown compress format.\n");
    ShowLogHalt();
  }

  if(zlibdecompress(&zd)==false){
    _consolePrintf("Fatal error: ZLIB decompress error.\n");
    ShowLogHalt();
  }
  
  u16 pals[4];
  
  if(palmode==0){
    pals[0]=RGB15(12,0,0)|BIT15;
    pals[1]=RGB15(7,0,0)|BIT15;
    pals[2]=RGB15(31,31,31)|BIT15;
    pals[3]=RGB15(0,0,0)|BIT15;
    }else{
    pals[0]=RGB15(24,0,0)|BIT15;
    pals[1]=RGB15(28,26,27)|BIT15;
    pals[2]=RGB15(0,0,0)|BIT15;
    pals[3]=0;
  }    
  
  u16 *psrc=(u16*)zd.pDstBuf;
  u32 *pdst=(u32*)pScreenMain->pViewCanvas->GetVRAMBuf();
  for(u32 idx=0;idx<(ScreenWidth*ScreenHeight)/2;idx++){
    u32 pal16=*psrc++;
    u32 col32=pals[pal16&0xff]|(pals[pal16>>8]<<16);
    *pdst++=col32;
  }
  
  zd.DstSize=0;
  safefree(zd.pDstBuf); zd.pDstBuf=NULL;
}

// ----------------------------------------

extern u32 _pGUID;

static void CheckGUID_Pre(void)
{
  u32 *pGUID=&_pGUID;
  
  if(pGUID[2]==pGUID[5]) return;
  
  _consolePrintf("pGUID=0x%x\n",(u32)pGUID);
  
  _consolePrintf("ID=0x%x\n",pGUID[0]);
  _consolePrintf("XID=0x%x\n",pGUID[1]);
  _consolePrintf("xcrc=0x%x\n",pGUID[2]);
  _consolePrintf("MemStartAddr=0x%x\n",pGUID[3]);
  _consolePrintf("MemEndAddr=0x%x\n",pGUID[4]);
  _consolePrintf("chkxcrc=0x%x\n",pGUID[5]);
  
  _consolePrint("\n");
  _consolePrint("  --- Detected fatal error !! ---\n");
  _consolePrint("\n");
  _consolePrint("  There is a loss in the main body of \n");
  _consolePrint("  The ARM9 code.\n");
  _consolePrint("  Please copy moonshl2.nds with \n");
  _consolePrint("  Windows again.\n");
  _consolePrint("  The cause might be a loose connection \n");
  _consolePrint("  of the equipment.\n");
  _consolePrint("\n");
}

static bool CheckGUID_Pre_for_CheckTCM(void)
{
  u32 *pGUID=&_pGUID;
  
  if(pGUID[2]==pGUID[5]) return(false);
  
  _consolePrintf("pGUID=0x%x\n",(u32)pGUID);
  
  _consolePrintf("ID=0x%x\n",pGUID[0]);
  _consolePrintf("XID=0x%x\n",pGUID[1]);
  _consolePrintf("xcrc=0x%x\n",pGUID[2]);
  _consolePrintf("MemStartAddr=0x%x\n",pGUID[3]);
  _consolePrintf("MemEndAddr=0x%x\n",pGUID[4]);
  _consolePrintf("chkxcrc=0x%x\n",pGUID[5]);
  
  _consolePrint("\n");
  _consolePrint("  --- Detected fatal error !! ---\n");
  _consolePrint("\n");
  _consolePrint("  There is a loss in the main body of \n");
  _consolePrint("  The ARM9 code.\n");
  _consolePrint("  Please copy moonshl2.nds with \n");
  _consolePrint("  Windows again.\n");
  _consolePrint("  The cause might be a loose connection \n");
  _consolePrint("  of the equipment.\n");
  _consolePrint("\n");
  
  return(true);
}

#include "guidxcrcerror_b8zlib.h"

static void CheckGUID_Body(void)
{
  u32 *pGUID=&_pGUID;
  
  if(pGUID[2]==pGUID[5]) return;
  
  DrawBootWarn(guidxcrcerror_b8zlib,guidxcrcerror_b8zlib_Size,0);
  
  ShowLogHalt();
}

// ----------------------------------------

static __attribute__ ((noinline)) void mainloop(void);

#include "main_vardebug.h"

#include "bootwarn_b8zlib.h"

static bool chkflag;

static __attribute__ ((noinline)) void main_ins_start(void)
{
  u32 zero;
  asm {
    mov zero,#0
    MCR p15, 0, zero, c7, c10, 4 // drain write buffer
  }
  
  static u32 stackptr=__current_sp();
  static vu32 stackdata[32];
  
  {
    u32 *pptr=(u32*)stackptr;
    for(u32 idx=0;idx<32;idx++){
      stackdata[idx]=pptr[-16+idx];
    }
  }
  
  SetARM9_REG_WaitCR();
  
  REG_POWERCNT = POWER_ALL_2D; // | POWER_SWAP_LCDS; // SWAPするとファイルリストが下
  
  atype_init();
  
  pDTCMEND=(u32*)&dtcmend;
  pDTCMEND+=128/4;
  
  {
    // setup stack overflow checker
    u32 *p=pDTCMEND;
    for(;p<(u32*)__current_sp();p++){
      *p=(u32)p;
    }
  }
  
  glSetFuncDebugPrint(_consolePrint);
  glDefaultMemorySetting();
  
  {
    SUB_BG2_CR = BG_256_COLOR | BG_RS_64x64 | BG_MAP_BASE(8) | BG_TILE_BASE(0) | BG_PRIORITY_1; // Tile16kb Map2kb(64x32)
    
    BG_PALETTE_SUB[(0*16)+0] = RGB15(0,0,0); // unuse (transparent)
    BG_PALETTE_SUB[(0*16)+1] = RGB15(0,0,2) | BIT(15); // BG color
    BG_PALETTE_SUB[(0*16)+2] = RGB15(0,0,0) | BIT(15); // Shadow color
    BG_PALETTE_SUB[(0*16)+3] = RGB15(16,16,16) | BIT(15); // Text color
    
    u16 XDX=(u16)((8.0/6)*0x100);
    u16 YDY=(u16)((8.0/6)*0x100);
    
    SUB_BG2_XDX = XDX;
    SUB_BG2_XDY = 0;
    SUB_BG2_YDX = 0;
    SUB_BG2_YDY = YDY;
    
    SUB_BG2_CX=1;
    SUB_BG2_CY=-1;
    
    //consoleInit() is a lot more flexible but this gets you up and running quick
    _consoleInitDefault((u16*)(SCREEN_BASE_BLOCK_SUB(8)), (u16*)(CHAR_BASE_BLOCK_SUB(0)));
  }
  
  _consolePrintf("boot %s %s\n%s\n%s\n\n",ROMTITLE,ROMVERSION,ROMDATE,ROMENV);
  _consolePrintf("__current pc=0x%08x sp=0x%08x\n\n",__current_pc(),__current_sp());
  CheckGUID_Pre();
  PrintFreeMem();
  
  glDefaultClassCreate();
  LoadDefaultFont();
  
  _consolePrintf("%x,%x\n",IPC6,&IPC6->ARM7SelfCheck);
  CheckGUID_Body();
  
  DrawBootWarn(bootwarn_b8zlib,bootwarn_b8zlib_Size,1);
  
  SetARM9_REG_WaitCR();
  extmem_Init();
  extmem_ShowMemoryInfo();
  
  SetARM9_REG_WaitCR();
  if(FAT2_InitFiles()==false){
    _consolePrint("FAT_InitFiles() failed.\n");
    ShowLogHalt();
  }
  SetARM9_REG_WaitCR();
  
//  main_VarDebug();
  
  if(Shell_FAT_fopen_isExists_Data(LogFilename)==true){
    FAT_FILE *pf=Shell_FAT_fopen_Data(LogFilename);
    if(pf!=NULL){
      _consoleSetLogFile(pf);
      FAT2_fclose(pf);
    }
  }
  
  Shell_ShellSet_Init();
  
  if(ShellSet.Cluster64k==false) FAT2_Disabled64kClusterMode();
  
  _consolePrintf("pDTCMEND=0x%08x, pMTCMEND=0x%08x. MTCM size=%dbyte.\n",pDTCMEND,pMTCMEND,(u32)pMTCMEND-(u32)pDTCMEND);
  
  pMTCMEND=(u32*)&mtcmend;
  if((0x02804000-1536)<(u32)pMTCMEND){
    _consolePrintf("MTCM overflow. 0x%08x 0x%08x\n",0x02800000+1536,pMTCMEND);
    ShowLogHalt();
  }

  {
    _consolePrintf("Current stack pointer: 0x%08x.\n",stackptr);
    for(s32 idx=0;idx<32;idx+=4){
      _consolePrintf("0x%08x: 0x%08x, 0x%08x, 0x%08x, 0x%08x.\n",stackptr+((-16+idx)*4),stackdata[idx+0],stackdata[idx+1],stackdata[idx+2],stackdata[idx+3]);
    }
  }
  
  _consolePrintf("Start FPGA Initializer.\n");
  FPGA_BusOwnerARM9();
  isExistsROMEO2=false;
  if(FPGA_isExistCartridge()==false){
    _consolePrintf("Can not found ROMEO2 cartridge on GBA slot.\n");
    }else{
    bool halt=true;
    void *pFPGAData=NULL;
    s32 FPGADataSize=0;
    if(Shell_FAT_ReadAlloc(FPGAFilename,&pFPGAData,&FPGADataSize)==false){
      _consolePrintf("Can not found RAW file. [%s]\n",FPGAFilename);
      }else{
      if(FPGA_CheckBitStreamFormat(pFPGAData,FPGADataSize)==false){
        _consolePrintf("ROMEO2 FPGA bit stream unknown format?\n");
        }else{
        if(FPGA_Start(pFPGAData,FPGADataSize)==false){
          _consolePrintf("ROMEO2 configration failed.\n");
          }else{
          FPGA_BusOwnerARM7();
          _consolePrintf("Wait for ARM7 init.\n");
          IPC6->RequestFPGAInit=true;
          while(IPC6->RequestFPGAInit==true);
          _consolePrintf("Initialized.\n");
          halt=false;
          isExistsROMEO2=true;
        }
      }
      safefree(pFPGAData); pFPGAData=NULL;
    }
    if(halt==true) ShowLogHalt();
  }
  
  {
    DateTime_ResetNow();
    TDateTime dt=DateTime_GetNow();
    TFAT2_TIME ft;
    
    ft.Year=dt.Date.Year;
    ft.Month=dt.Date.Month;
    ft.Day=dt.Date.Day;
    ft.Hour=dt.Time.Hour;
    ft.Minuts=dt.Time.Min;
    ft.Second=dt.Time.Sec;
    
    FAT2_SetSystemDateTime(ft);
  }
  
  IPC6->LCDPowerControl=LCDPC_ON_BOTH;
  
  Shell_CheckDataPath();
  
  InitInterrupts();
  strpcmSetVolume64(64);
  
  if(true){
    u32 UserLanguage=(u32)-1;
    u32 Timeout=0x10000;
    
    while(UserLanguage==(u32)-1){
      UserLanguage=IPC6->UserLanguage;
      Timeout--;
      if(Timeout==0){
        _consolePrintf("NDS farmware language read error. ARM7CPU stopped...?\n");
        while(1);
      }
    }
    _consolePrintf("NDS farmware language ID : %d\n",UserLanguage);
  }
  
  LangInitAndLoadFont();
  
  chkflag=CheckGUID_Pre_for_CheckTCM();
  CheckGUID_Body();
}

static __attribute__ ((noinline)) void main_ins_end(void)
{
  FAT2_FreeFiles();
  
  glDefaultClassFree();
  
  _consolePrint("Terminated.\n");
extern int main_wma(void);
//  main_wma();
}

int main(void)
{
  REG_IME=0;
  
  main_ins_start();
  
  {
    SetARM9_REG_WaitCR();
    PrintFreeMem();
    mainloop();
    PrintFreeMem();
  }
  
  REG_IME=0;
  
  main_ins_end();
  
  return(0);
}

// -------------------------------- mainloop

void WaitForVBlank(void)
{
  if(VBlankPassedFlag==false){
    swiWaitForVBlank();
  }
  VBlankPassedFlag=false;
}

// ------------------------------------------------------------

static TCallBack CallBack;

static void CallBackInit(void)
{
  MemSet32CPU(0,&CallBack,sizeof(TCallBack));
}

void CallBack_ExecuteVBlankHandler(void)
{
  if(CallBack.VBlankHandler!=NULL) CallBack.VBlankHandler();
}

TCallBack* CallBack_GetPointer(void)
{
  return(&CallBack);
}

// ------------------------------------------------------------

static bool mf;
static s32 mx,my;

static void Proc_TouchPad(u32 VsyncCount)
{
  if(IPC6->RequestUpdateIPC==true) return;
  
  bool tpress;
  s32 tx,ty;
  
  if((IPC6->buttons&IPC_PEN_DOWN)==0){
    tpress=false;
    tx=0;
    ty=0;
    }else{
    tpress=true;
    tx=IPC6->touchXpx;
    ty=IPC6->touchYpx;
  }
  
  IPC6->RequestUpdateIPC=true;
  
  if(tpress==true){
    if(mf==false){
      mf=true;
      if(CallBack.MouseDown!=NULL) CallBack.MouseDown(tx,ty);
      mx=tx;
      my=ty;
      }else{
      s32 dx=abs(mx-tx);
      s32 dy=abs(my-ty);
      if((1<=dx)||(1<=dy)){
        if(CallBack.MouseMove!=NULL) CallBack.MouseMove(tx,ty);
        mx=tx;
        my=ty;
      }
    }
    }else{
    if(mf==true){
      mf=false;
      if(CallBack.MouseUp!=NULL) CallBack.MouseUp(mx,my);
    }
  }
}

#include "main_keyrepeat.h"

static u32 KEYS_Last;
static bool KEYS_PressedLR;
static u32 KEYS_PressStartCount,KEYS_PressSelectCount;
static bool KEYS_HPSwitch_Pressed;

void Proc_KeyInput_Init(void)
{
  KEYS_Last=~0;
  KEYS_PressedLR=false;
  KEYS_PressStartCount=0;
  KEYS_PressSelectCount=0;
  KEYS_HPSwitch_Pressed=false;
}

#include "main_savepreview.h"

void Proc_KeyInput(u32 VsyncCount)
{
  if(KeyRepeatFlag==true){ cwl();
    if(KeyRepeatCount<=VsyncCount){ cwl();
      KeyRepeatCount=0;
      }else{ cwl();
      KeyRepeatCount-=VsyncCount;
    }
  }
  
  u32 KEYS_Cur=(~REG_KEYINPUT)&0x3ff;
  
  {
    u32 btns=IPC6->buttons;
    
    KEYS_Cur|=(~REG_KEYINPUT)&0x3ff;
    if((btns&IPC_PEN_DOWN)!=0) KEYS_Cur|=KEY_TOUCH;
    if((btns&IPC_X)!=0) KEYS_Cur|=KEY_X;
    if((btns&IPC_Y)!=0) KEYS_Cur|=KEY_Y;
  }
  
  {
    const u32 Timeout=60*3;
    if((KEYS_Cur & KEY_START)!=0){
      if(KEYS_PressStartCount<Timeout){
        KEYS_PressStartCount+=VsyncCount;
        if(Timeout<=KEYS_PressStartCount){
          if(CallBack.KeyLongPress!=NULL) CallBack.KeyLongPress(KEY_START);
          KEYS_PressStartCount=Timeout;
        }
      }
      }else{
      if((KEYS_PressStartCount!=0)&&(KEYS_PressStartCount!=Timeout)){
        if(CallBack.KeyPress!=NULL) CallBack.KeyPress(VsyncCount,KEY_START);
      }
      KEYS_PressStartCount=0;
    }
    KEYS_Cur&=~KEY_START;
  }
  
  {
    const u32 Timeout=60*3;
    if((KEYS_Cur & KEY_SELECT)!=0){
      if(KEYS_PressSelectCount<Timeout){
        KEYS_PressSelectCount+=VsyncCount;
        if(Timeout<=KEYS_PressSelectCount){
          main_SavePreviewAndHalt();
          ShowLogHalt();
        }
      }
      }else{
      if((KEYS_PressSelectCount!=0)&&(KEYS_PressSelectCount!=Timeout)){
        if(CallBack.KeyPress!=NULL) CallBack.KeyPress(VsyncCount,KEY_SELECT);
      }
      KEYS_PressSelectCount=0;
    }
    KEYS_Cur&=~KEY_SELECT;
  }
  
  if((KEYS_Cur&(KEY_L|KEY_R))==(KEY_L|KEY_R)){
    if(KEYS_PressedLR==false){
      KEYS_PressedLR=true;
      if(CallBack.KeySameLRDown!=NULL) CallBack.KeySameLRDown();
    }
  }
  if((KEYS_Cur&(KEY_L|KEY_R))!=(KEY_L|KEY_R)){
    if(KEYS_PressedLR==true){
      KEYS_PressedLR=false;
      if(CallBack.KeySameLRUp!=NULL) CallBack.KeySameLRUp();
    }
  }
  
  const u32 DupMask=KEY_A|KEY_B|KEY_X|KEY_Y|KEY_UP|KEY_DOWN|KEY_LEFT|KEY_RIGHT;
  
  if((KEYS_Last&DupMask)!=0){
    u32 l=KEYS_Last&DupMask;
    u32 c=KEYS_Cur&DupMask;
    
    KEYS_Cur&=KEYS_Last;
  }
  
  KEYS_Last=KEYS_Cur;
  KEYS_Cur=KeyRepeat_Proc(KEYS_Cur,VsyncCount);
  
  if(8<VsyncCount) VsyncCount=8;
  
  if(KEYS_Cur!=0){
    if(CallBack.KeyPress!=NULL) CallBack.KeyPress(VsyncCount,KEYS_Cur);
  }
  
}

static bool Proc_PanelOpened_Last;

static void Proc_PanelOpened(void)
{
  if(Proc_PanelOpened_Last!=IPC6->PanelOpened){
    Proc_PanelOpened_Last=IPC6->PanelOpened;
    if(Proc_PanelOpened_Last==true){
      if(CallBack.PanelOpen!=NULL) CallBack.PanelOpen();
      }else{
      if(CallBack.PanelClose!=NULL) CallBack.PanelClose();
    }
  }
}

#include "main_Trigger.h"

// ------------------------------------------------------------

EProcFadeEffect ProcFadeEffect;

static const u32 HorizontalFadeStepCount=35;
static const u32 HorizontalFadeStep[HorizontalFadeStepCount]={2,4,4,6,6,8,8,8,8,10,10,10,10,10,10,10,10,10,10,10,10,8,8,8,8,8,8,6,6,6,4,4,4,2,2,};

static const u32 VerticalFadeStepCount=27;
static const u32 VerticalFadeStep[VerticalFadeStepCount]={2,4,5,7,8,8,9,9,9,9,9,9,9,9,9,9,9,9,8,8,7,7,6,5,4,3,2,};

void ScreenMain_Flip_ProcFadeEffect(void)
{
  if(NextProc!=ENP_Loop) return;
  
  if(ProcState.System.EnableFadeEffect==false) ProcFadeEffect=EPFE_None;
  
  u16 *pviewbuf=pScreenMain->pViewCanvas->GetVRAMBuf();
  u16 *pbackbuf=pScreenMain->pBackCanvas->GetVRAMBuf();
  
  switch(ProcFadeEffect){
    case EPFE_None: {
    } break;
    case EPFE_LeftToRight: {
      u32 sx=0;
      for(u32 idx=0;idx<HorizontalFadeStepCount;idx++){
        u32 step=HorizontalFadeStep[idx];
        Splash_Update();
        swiWaitForVBlank();
        u16 tmpbuf[ScreenWidth];
        for(u32 y=0;y<ScreenHeight;y++){
          u16 *pviewlinebuf=&pviewbuf[y*ScreenWidth];
          u16 *pbacklinebuf=&pbackbuf[y*ScreenWidth];
          MemCopy32CPU(&pviewlinebuf[0],tmpbuf,(ScreenWidth-step)*2);
          MemCopy32CPU(tmpbuf,&pviewlinebuf[step],(ScreenWidth-step)*2);
          MemCopy32CPU(&pbacklinebuf[ScreenWidth-step-sx],&pviewlinebuf[0],step*2);
        }
        sx+=step;
      }
    } break;
    case EPFE_RightToLeft: {
      u32 sx=0;
      for(u32 idx=0;idx<HorizontalFadeStepCount;idx++){
        u32 step=HorizontalFadeStep[idx];
        Splash_Update();
        swiWaitForVBlank();
        for(u32 y=0;y<ScreenHeight;y++){
          u16 *pviewlinebuf=&pviewbuf[y*ScreenWidth];
          u16 *pbacklinebuf=&pbackbuf[y*ScreenWidth];
          MemCopy32CPU(&pviewlinebuf[step],&pviewlinebuf[0],(ScreenWidth-step)*2);
          MemCopy32CPU(&pbacklinebuf[sx],&pviewlinebuf[ScreenWidth-step],step*2);
        }
        sx+=step;
      }
    } break;
    case EPFE_UpToDown: {
      u32 sy=0;
      for(u32 idx=0;idx<VerticalFadeStepCount;idx++){
        u32 step=VerticalFadeStep[idx];
        Splash_Update();
        swiWaitForVBlank();
        s32 y;
        y=ScreenHeight-(step*2);
        y=(y/step)*step;
        for(;y>=0;y-=step){
          u16 *pviewlinebuf=&pviewbuf[y*ScreenWidth];
          MemCopy32CPU(&pviewlinebuf[0*ScreenWidth],&pviewlinebuf[step*ScreenWidth],(step*ScreenWidth)*2);
        }
        u16 *pviewlinebuf=&pviewbuf[0*ScreenWidth];
        u16 *pbacklinebuf=&pbackbuf[0*ScreenWidth];
        MemCopy32CPU(&pbacklinebuf[(ScreenHeight-step-sy)*ScreenWidth],&pviewlinebuf[0*ScreenWidth],(step*ScreenWidth)*2);
        sy+=step;
      }
    } break;
    case EPFE_DownToUp: {
      u32 sy=0;
      for(u32 idx=0;idx<VerticalFadeStepCount;idx++){
        u32 step=VerticalFadeStep[idx];
        Splash_Update();
        swiWaitForVBlank();
        for(u32 y=0;y<ScreenHeight-step;y+=step){
          u16 *pviewlinebuf=&pviewbuf[y*ScreenWidth];
          MemCopy32CPU(&pviewlinebuf[step*ScreenWidth],&pviewlinebuf[0*ScreenWidth],(step*ScreenWidth)*2);
        }
        u16 *pviewlinebuf=&pviewbuf[0*ScreenWidth];
        u16 *pbacklinebuf=&pbackbuf[0*ScreenWidth];
        MemCopy32CPU(&pbacklinebuf[sy*ScreenWidth],&pviewlinebuf[(ScreenHeight-step)*ScreenWidth],(step*ScreenWidth)*2);
        sy+=step;
      }
    } break;
    case EPFE_CrossFade: {
      for(u32 idx=16;idx>0;idx--){
        Splash_Update();
        WaitForVBlank();
        pScreenMain->SetBlendLevel(idx);
        while(VBlankPassedFlag==false){
          DLLSound_Update();
        }
      }
    } break;
    case EPFE_FastCrossFade: {
      for(u32 idx=16;idx>0;idx-=4){
        Splash_Update();
        WaitForVBlank();
        pScreenMain->SetBlendLevel(idx);
        while(VBlankPassedFlag==false){
          DLLSound_Update();
        }
      }
    } break;
  }
  pScreenMain->Flip(true);
  
  if(ProcFadeEffect!=EPFE_None){
    ProcFadeEffect=EPFE_None;
    REG_IME=0;
    VBlankPassedCount=1;
    REG_IME=1;
  }
}

// ------------------------------------------------------------

static void Proc_ExternalPowerPresent(void)
{
  static bool LastState;
  
  static bool FirstStart=true;
  if(FirstStart==true){
    FirstStart=false;
    LastState=IPC6->ExternalPowerPresent;
    return;
  }
  
  bool curstate=IPC6->ExternalPowerPresent;
  if(LastState==curstate) return;
  LastState=curstate;
  
  if(curstate==true){
    if(CallBack.ExternalPowerAttach!=NULL) CallBack.ExternalPowerAttach();
    }else{
    if(CallBack.ExternalPowerDetach!=NULL) CallBack.ExternalPowerDetach();
  }
}

// ------------------------------------------------------------

static void mainloop_autoboot(const char *pFilename)
{
  const UnicodeChar PathUnicode[2]={(UnicodeChar)'/',0};
  UnicodeChar FilenameUnicode[16];
  StrConvert_Ank2Unicode(pFilename,FilenameUnicode);
  if(FileExistsUnicode(PathUnicode,FilenameUnicode)==true){
    _consolePrintf("Auto booting... [/%s]\n",pFilename);
    BootROM_SetInfo_NoLaunch(PathUnicode,FilenameUnicode);
  }
}

#include "bootreply_b8zlib.h"

// ------------------------------------------------------------

static __attribute__ ((noinline)) void mainloop_ins_start(void)
{
  {
    Lang_Load();
    const char *pfmt=Lang_GetUTF8("DateTimeFormat");
    if(isStrEqual_NoCaseSensitive(pfmt,"YYYYMMDD")==true) Date_SetDateFormat(EDF_YMD);
    if(isStrEqual_NoCaseSensitive(pfmt,"DDMMYYYY")==true) Date_SetDateFormat(EDF_DMY);
    if(isStrEqual_NoCaseSensitive(pfmt,"MMDDYYYY")==true) Date_SetDateFormat(EDF_MDY);
  }
  
  ProcState_Init();
  ProcState_Load();
  
  if(chkflag==true){
    ProcState.System.BootCount++;
    if(ProcState.System.BootCount==16){
      DrawBootWarn(bootreply_b8zlib,bootreply_b8zlib_Size,1);
      while(1);
    }
    ProcState_RequestSave=true;
    ProcState_Save();
  }
  
  Splash_Init();
  
  if(true){
    _consolePrintf("Initialize random seed.\n");
    DateTime_ResetNow();
    TDateTime now=DateTime_GetNow();
    u32 cnt=((now.Time.Min*60)+now.Time.Sec)&0xff;
    for(u32 idx=0;idx<cnt;idx++){
      rand();
    }
  }
  
  ErrorDialog_Init();
  
  Component_SetFont(pCglFontDefault);
  
  {
    u32 keys=(~REG_KEYINPUT)&0x3ff;
    if(keys==(KEY_A|KEY_B|KEY_L|KEY_R)){
      ProcState_Clear();
      ProcState_RequestSave=true;
      ProcState_Save();
      ProcState_Load();
      
      LaunchState_Init();
      LaunchState_Save();
      Skin_ClearCustomBG_FileBody();
      Skin_ClearCustomBG();
      Resume_Load();
      Resume_Clear();
      Resume_Save();
      
      pScreenSub->pCanvas->SetFontTextColor(RGB15(31,31,31)|BIT15);
      pScreenSub->pCanvas->TextOutA(8,96,"All settings were initialized.");
      pScreenSub->pCanvas->TextOutA(8,96+16,"Please release all key.");
    }
  }
  
  LaunchState_Load();
  Resume_Load();
  
  ApplyCurrentBacklightLevel();
  strpcmSetVolume64(ProcState.System.Volume64);
  
  BootROM_Init();
  
  Shell_Init_SwapFile();
  
  mf=false;
  mx=0;
  my=0;
  
  KEYS_Last=0;
  KeyRepeatLastKey=0;
  KeyRepeatFlag=false;
  KeyRepeatCount=0;
  
  DLLList_Init();
  Splash_Update();
  
  Sound_Init();
  Splash_Update();
  
  {
    UnicodeChar *pcpu=ProcState.FileList.CurrentPathUnicode;
    const char *pafn=StrConvert_Unicode2Ank_Test(pcpu);
    _consolePrintf("Check current path. [%s]\n",pafn);
    const char *pPathAlias=ConvertFull_Unicode2Alias(pcpu,NULL);
    if(pPathAlias==NULL){
      _consolePrintf("Set to default path.\n");
      pcpu[0]=(UnicodeChar)'/';
      pcpu[1]=(UnicodeChar)0;
      }else{
      if(FAT2_chdir_Alias(pPathAlias)==false){
        _consolePrintf("Set to default path.\n");
        pcpu[0]=(UnicodeChar)'/';
        pcpu[1]=(UnicodeChar)0;
        }else{
        _consolePrintf("finded. [%s]\n",pPathAlias);
      }
    }
  }
  
  Skin_ClearCustomBG();
  
  _consolePrintf("Set skin.\n");
  if(Skin_SetFilename(ProcState.System.SkinFilenameUnicode)==false){
    _consolePrintf("Set default skin.\n");
    StrConvert_Ank2Unicode(DefaultDataPath "/default.skn",ProcState.System.SkinFilenameUnicode);
    if(Skin_SetFilename(ProcState.System.SkinFilenameUnicode)==false){
      _consolePrintf("Not found skin package.\n");
      ShowLogHalt();
    }
  }
  
  PrintFreeMem();
  Splash_Update();

  VBlankPassedFlag=false;
  VBlankPassedCount=0;
  
  _consolePrintf("Set NextProc.\n");
  
  RelationalFile_Clear();
  
  NextProc=ENP_Loop;
  
  {
    u32 KEYS_Cur=(~REG_KEYINPUT)&0x3ff;
    {
      u32 btns=IPC6->buttons;
      KEYS_Cur|=(~REG_KEYINPUT)&0x3ff;
      if((btns&IPC_PEN_DOWN)!=0) KEYS_Cur|=KEY_TOUCH;
      if((btns&IPC_X)!=0) KEYS_Cur|=KEY_X;
      if((btns&IPC_Y)!=0) KEYS_Cur|=KEY_Y;
    }
    if((NextProc==ENP_Loop)&&(KEYS_Cur==0)) mainloop_autoboot(DefaultNFilename);
    if((NextProc==ENP_Loop)&&(KEYS_Cur==KEY_X)) mainloop_autoboot(DefaultXFilename);
    if((NextProc==ENP_Loop)&&(KEYS_Cur==KEY_Y)) mainloop_autoboot(DefaultYFilename);
    if(NextProc!=ENP_Loop) Splash_Free();
  }
  
  ExtLink_Init();
  
  atype_lockall();
  if(NextProc==ENP_Loop){
    atype_showallocated();
    atype_checkoverrange();
    atype_checkmemoryleak();
    Splash_Update();
  }
  
  if(NextProc==ENP_Loop){
    // Resume_SetResumeMode(ERM_None); // Disabled resume function.
    if(Resume_GetResumeMode()!=ERM_None){
      _consolePrintf("Restart resume.\n");
      UnicodeChar PathUnicode[MaxFilenameLength],FilenameUnicode[MaxFilenameLength];
      SplitItemFromFullPathUnicode(Resume_GetFilename(),PathUnicode,FilenameUnicode);
      if(FileExistsUnicode(PathUnicode,FilenameUnicode)==true){
        Unicode_Copy(RelationalFilePathUnicode,PathUnicode);
        Unicode_Copy(RelationalFileNameUnicode,FilenameUnicode);
        Unicode_Copy(ProcState.FileList.CurrentPathUnicode,PathUnicode);
        Unicode_Copy(ProcState.FileList.SelectFilenameUnicode,FilenameUnicode);
        RelationalFilePos=Resume_GetPos();
        Splash_Free();
        switch(Resume_GetResumeMode()){
          case ERM_None: break;
          case ERM_Audio: {
            SetNextProc(ENP_FileList,EPFE_CrossFade);
          } break;
          case ERM_Video: {
            pScreenSub->pCanvas->FillFull(ColorTable.Video.InitBG);
            SetNextProc(ENP_DPGPlay,EPFE_None);
          } break;
          case ERM_Image: {
            SetNextProc(ENP_ImageView,EPFE_None);
          } break;
          case ERM_Text: {
            SetNextProc(ENP_TextView,EPFE_None);
          } break;
        }
      }
      Resume_Clear();
    }
  }
  
  if(NextProc==ENP_Loop){
    if(ProcState.System.BootCheckDisk==false){
      if(ProcState.System.SkipSetup==false){
        SetNextProc(ENP_Setup,EPFE_CrossFade);
        }else{
        switch(ProcState.System.LastState){
          case ELS_FileList: SetNextProc(ENP_FileList,EPFE_CrossFade); break;
          case ELS_Launch: SetNextProc(ENP_Launch,EPFE_CrossFade); break;
        }
      }
      }else{
      SetNextProc(ENP_ChkDsk,EPFE_CrossFade);
    }
  }
  
//  SetNextProc(ENP_Custom,EPFE_CrossFade);
  
  Proc_PanelOpened_Last=IPC6->PanelOpened;
}

static __attribute__ ((noinline)) void mainloop_ins_end(void)
{
  Skin_Free();
  Skin_CloseFile();
  
  Lang_Free();
  
  Sound_Stop();
  Sound_Free();
  
  _consolePrint("mainloop terminated.\n");
  
  _consolePrintf("Reboot ROM '%s'\n",BootROM_GetFullPathAlias());
  BootNDSROM();
}

// ------------------------------------------------------------

static bool chkstack;

static __attribute__ ((noinline)) void mainloop_ins_loopstart(void)
{
    if(NextProc!=ENP_BootROM){
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
    }
    
    chkstack=true;
    if(NextProc==ENP_DPGPlay) chkstack=false;
    
    if(chkstack==true){ // setup stack overflow checker
      u32 *p=pDTCMEND;
      for(;p<(u32*)__current_sp();p++){
        *p=(u32)p;
      }
      }else{
      u32 *p=pMTCMEND;
      for(;p<(u32*)__current_sp();p++){
        *p=(u32)p;
      }
    }
    
    CallBackInit();
    
    Skin_Free();
    
    switch(NextProc){
      case ENP_Loop: {
        _consolePrintf("Illigal process error! NextProc==ENP_Loop\n");
        ShowLogHalt();
      } break;
      case ENP_ChkDsk: Skin_Load_ChkDsk(); break;
      case ENP_Setup: Skin_Load_Setup(); break;
      case ENP_FileList: Skin_Load_FileList(); break;
      case ENP_SysMenu: Skin_Load_SysMenu(); break;
      case ENP_DPGCustom: Skin_Load_DPGCustom(); break;
      case ENP_DPGPlay: Skin_ClearCustomBG(); Skin_Load_DPGPlay(); break;
      case ENP_ImageCustom: Skin_Load_ImageCustom(); break;
      case ENP_ImageView: Skin_Load_ImageView(); break;
      case ENP_TextCustom: Skin_Load_TextCustom(); break;
      case ENP_TextView: Skin_Load_TextView(); break;
      case ENP_Launch: Skin_Load_Launch(); break;
      case ENP_Custom: Skin_Load_Custom(); break;
      case ENP_BootROM: Skin_Load_BootROM(); break;
      default: {
        _consolePrintf("Unknown process error! NextProc==%d\n",NextProc);
        ShowLogHalt();
      } break;
    }
    
    if((NextProc==ENP_Setup)||(NextProc==ENP_FileList)||(NextProc==ENP_Launch)||(NextProc==ENP_Custom)||(NextProc==ENP_BootROM)) Splash_Free();
    
    switch(NextProc){
      case ENP_Loop: {
        _consolePrintf("Illigal process error! NextProc==ENP_Loop\n");
        ShowLogHalt();
      } break;
      case ENP_ChkDsk: ProcChkDsk_SetCallBack(&CallBack); break;
      case ENP_Setup: ProcSetup_SetCallBack(&CallBack); break;
      case ENP_FileList: ProcFileList_SetCallBack(&CallBack); break;
      case ENP_SysMenu: ProcSysMenu_SetCallBack(&CallBack); break;
      case ENP_DPGCustom: ProcDPGCustom_SetCallBack(&CallBack); break;
      case ENP_DPGPlay: ProcDPGPlay_SetCallBack(&CallBack); break;
      case ENP_ImageCustom: ProcImageCustom_SetCallBack(&CallBack); break;
      case ENP_ImageView: ProcImageView_SetCallBack(&CallBack); break;
      case ENP_TextCustom: ProcTextCustom_SetCallBack(&CallBack); break;
      case ENP_TextView: ProcTextView_SetCallBack(&CallBack); break;
      case ENP_Launch: ProcLaunch_SetCallBack(&CallBack); break;
      case ENP_Custom: ProcCustom_SetCallBack(&CallBack); break;
      case ENP_BootROM: ProcBootROM_SetCallBack(&CallBack); break;
      default: {
        _consolePrintf("Unknown process error! NextProc==%d\n",NextProc);
        ShowLogHalt();
      } break;
    }
    
    NextProc=ENP_Loop;
    
    if(CallBack.Start!=NULL) CallBack.Start();
    
    if(chkstack==true){ // fast stack overflow checker
      DTCM_StackCheck(-1);
      }else{
      MTCM_StackCheck(-1);
    }
    
    ProcState_Save();
    
    atype_checkoverrange();
    
    PrintFreeMem();
    
    VBlankPassedFlag=false;
    VBlankPassedCount=0;
    Proc_TouchPad(0);
    Proc_KeyInput_Init();
    Proc_Trigger(true,0);
    
    REG_IME=0;
    VBlankPassedCount=0;
    REG_IME=1;
}

// ------------------------------------------------------------

static __attribute__ ((noinline)) void mainloop_ins_loopend(void)
{
    VBlankPassedFlag=false;
    VBlankPassedCount=0;
    if(CallBack.End!=NULL) CallBack.End();
    
    IPC6->LCDPowerControl=LCDPC_ON_BOTH;
    
    atype_checkoverrange();
    atype_checkmemoryleak();
    
    if(chkstack==true){
      DTCM_StackCheck(0);
      }else{
      MTCM_StackCheck(0);
    }
}

// ------------------------------------------------------------

static __attribute__ ((noinline)) void mainloop(void)
{
  _consolePrint("mainloop.\n");
  
  mainloop_ins_start();
  
  DTCM_StackCheck(0);
  
  _consolePrintf("Start event loop...\n");
  
  while(1){
    mainloop_ins_loopstart();
    
    while(NextProc==ENP_Loop){
      if(DLLSound_isOpened()==true){
        while(DLLSound_Update()==true){
        }
        if(strpcmRequestStop==true){
          if(CallBack.strpcmRequestStop!=NULL) CallBack.strpcmRequestStop();
        }
      }
      
      WaitForVBlank();
      
      REG_IME=0;
      u32 vsynccount=VBlankPassedCount;
      VBlankPassedCount=0;
      REG_IME=1;
      
      if(CallBack.VsyncUpdate!=NULL) CallBack.VsyncUpdate(vsynccount);
      
      Proc_TouchPad(vsynccount);
      Proc_KeyInput(vsynccount);
      Proc_PanelOpened();
      Proc_Trigger(false,vsynccount);
      Proc_ExternalPowerPresent();
      
      if(chkstack==true){ // fast stack overflow checker
        DTCM_StackCheck(-1);
        }else{
        MTCM_StackCheck(-1);
      }
      
    }
    
    mainloop_ins_loopend();
    
    if(BootROM_GetExecuteFlag()==true) break;
  }
  
  mainloop_ins_end();
  
  ShowLogHalt();
}
