
#ifndef maindef_h
#define maindef_h

#include "_console.h"

#define ROMTITLE "MoonShell"
#define ROMVERSION "Ver 2.00 beta.5"
#define ROMDATE ""__DATE__" "__TIME__" GMT+09:00"
#define ROMDATESHORT ""__DATE__" "__TIME__""
#define ROMENV "ARM RVCT3.1 [Build 569]"

extern __declspec(noreturn) void ShowLogHalt(void);

#define TCM_StackStart (0x02803f00)
extern u32 *pDTCMEND,*pMTCMEND;

static inline void DTCM_StackCheck(s32 ID) // 動画以外モード用スタックオーバーフローチェック
{
  u32 *p=pDTCMEND;
  if(*p!=(u32)p){
    _consolePrintf("Fatal error: DTCM Stack overflow!!\n");
    ShowLogHalt();
  }
  if(ID==-1) return;
  for(;p<(u32*)__current_sp();p++){
    if(*p!=(u32)p) break;
  }
  _consolePrintf("DStack:%d Pos:%d, Used:%d, Total:%d.\n",ID,TCM_StackStart-(u32)__current_sp(),TCM_StackStart-(u32)p,TCM_StackStart-(u32)pDTCMEND);
}

static inline void MTCM_StackCheck(s32 ID) // 動画モード用スタックオーバーフローチェック
{
  u32 *p=pMTCMEND;
  if(*p!=(u32)p){
    _consolePrintf("Fatal error: MTCM Stack overflow!!\n");
    ShowLogHalt();
  }
  if(ID==-1) return;
  for(;p<(u32*)__current_sp();p++){
    if(*p!=(u32)p) break;
  }
  _consolePrintf("MStack:%d Pos:%d, Used:%d, Total:%d.\n",ID,TCM_StackStart-(u32)__current_sp(),TCM_StackStart-(u32)p,TCM_StackStart-(u32)pMTCMEND);
}

#include "glib/glib.h"

extern CglFont *pCglFontDefault;

extern void DSP_Open(const char *pFilename,s32 x,s32 y,s32 w,s32 h);
extern void DSP_Close(void);
extern bool DSP_isOpened(void);
extern bool DSP_Update(void);

extern void WaitForVBlank(void);

extern CglB15 *pSettingBGBM,*pTimerBodyBGBM;

extern u32 FolderDlg_TargetItemIndex; // (u32)-1==StartBGM

extern bool isExistsROMEO2;

enum ETriggerType {ETT_AButton=0,ETT_LButton,ETT_RButton,ETT_PhoneSwitch};

typedef struct {
  void (*Start)(void);
  void (*VsyncUpdate)(u32 VsyncCount);
  void (*End)(void);
  void (*KeyPress)(u32 VsyncCount,u32 Keys);
  void (*KeyLongPress)(u32 Keys);
  void (*KeySameLRDown)(void);
  void (*KeySameLRUp)(void);
  void (*MouseDown)(s32 x,s32 y);
  void (*MouseMove)(s32 x,s32 y);
  void (*MouseUp)(s32 x,s32 y);
  void (*strpcmRequestStop)(void);
  void (*PanelOpen)(void);
  void (*PanelClose)(void);
  void (*VBlankHandler)(void);
  void (*ExternalPowerAttach)(void);
  void (*ExternalPowerDetach)(void);
  void (*Trigger_ProcStart)(ETriggerType TriggerType);
  void (*Trigger_ProcEnd)(ETriggerType TriggerType);
  void (*Trigger_Down)(ETriggerType TriggerType);
  void (*Trigger_Up)(ETriggerType TriggerType);
  void (*Trigger_LongStart)(ETriggerType TriggerType);
  void (*Trigger_LongEnd)(ETriggerType TriggerType);
  void (*Trigger_SingleClick)(ETriggerType TriggerType);
  void (*Trigger_SingleLongStart)(ETriggerType TriggerType);
  void (*Trigger_SingleLongEnd)(ETriggerType TriggerType);
  void (*Trigger_DoubleClick)(ETriggerType TriggerType);
  void (*Trigger_DoubleLongStart)(ETriggerType TriggerType);
  void (*Trigger_DoubleLongEnd)(ETriggerType TriggerType);
  void (*Trigger_TripleClick)(ETriggerType TriggerType);
} TCallBack;

extern void CallBack_ExecuteVBlankHandler(void);
extern TCallBack* CallBack_GetPointer(void);

enum ENextProc {ENP_Loop,ENP_ChkDsk,ENP_Setup,ENP_FileList,ENP_SysMenu,ENP_DPGCustom,ENP_DPGPlay,ENP_ImageCustom,ENP_ImageView,ENP_TextCustom,ENP_TextView,ENP_Launch,ENP_Custom,ENP_BootROM};
enum EProcFadeEffect {EPFE_None,EPFE_LeftToRight,EPFE_RightToLeft,EPFE_UpToDown,EPFE_DownToUp,EPFE_CrossFade,EPFE_FastCrossFade};

static void SetNextProc(ENextProc _NextProc,EProcFadeEffect _ProcFadeEffect)
{
  extern ENextProc NextProc;
  extern EProcFadeEffect ProcFadeEffect;
  NextProc=_NextProc;
  ProcFadeEffect=_ProcFadeEffect;
}

static void SetProcFadeEffect(EProcFadeEffect _ProcFadeEffect)
{
  extern EProcFadeEffect ProcFadeEffect;
  ProcFadeEffect=_ProcFadeEffect;
}

extern void ProcChkDsk_SetCallBack(TCallBack *pCallBack);
extern void ProcSetup_SetCallBack(TCallBack *pCallBack);
extern void ProcFileList_SetCallBack(TCallBack *pCallBack);
extern void ProcSysMenu_SetCallBack(TCallBack *pCallBack);
extern void ProcDPGCustom_SetCallBack(TCallBack *pCallBack);
extern void ProcDPGPlay_SetCallBack(TCallBack *pCallBack);
extern void ProcImageCustom_SetCallBack(TCallBack *pCallBack);
extern void ProcImageView_SetCallBack(TCallBack *pCallBack);
extern void ProcTextCustom_SetCallBack(TCallBack *pCallBack);
extern void ProcTextView_SetCallBack(TCallBack *pCallBack);
extern void ProcLaunch_SetCallBack(TCallBack *pCallBack);
extern void ProcCustom_SetCallBack(TCallBack *pCallBack);
extern void ProcBootROM_SetCallBack(TCallBack *pCallBack);

extern void ScreenMain_Flip_ProcFadeEffect(void);

extern void DrawItemsListToScreenSub(bool ShowCursor);

#include "libs/unicode.h"

#define MaxFilenameLength (256)
extern UnicodeChar RelationalFilePathUnicode[MaxFilenameLength];
extern UnicodeChar RelationalFileNameUnicode[MaxFilenameLength];
extern u32 RelationalFilePos;

static inline void RelationalFile_Clear(void)
{
  RelationalFilePathUnicode[0]=0;
  RelationalFileNameUnicode[0]=0;
  RelationalFilePos=0;
}

enum ETextEncode {ETE_Auto=0,ETE_ANSI,ETE_SJIS,ETE_UTF16BE,ETE_UTF16LE,ETE_UTF8};
extern ETextEncode ManualTextEncode;
extern bool ManualTextEncode_OverrideFlag;

//#define EnableTriggerLog

#endif

