
#ifndef procstate_h
#define procstate_h

#include "unicode.h"
#include "shell.h"

extern void ProcState_Init(void);
extern void ProcState_Clear(void);
extern void ProcState_Load(void);
extern void ProcState_Save(void);

enum ELastState {ELS_FileList,ELS_Launch};

typedef struct {
  bool SkipSetup;
  bool BootCheckDisk;
  bool ClickSound;
  bool EnableFadeEffect;
  bool AutoLastState;
  ELastState LastState;
  u32 Volume64;
  u32 BacklightLevel;
  UnicodeChar SkinFilenameUnicode[MaxFilenameLength];
  u32 BootCount;
  u32 Reserved0,Reserved1,Reserved2,Reserved3;
} TProcState_System;

enum EProcStateFileListMode {EPSFLM_Single,EPSFLM_Double};

enum EProcStateFileListPlayMode {EPSFLPM_Repeat,EPSFLPM_AllRep,EPSFLPM_Random};

typedef struct {
  UnicodeChar CurrentPathUnicode[MaxFilenameLength];
  UnicodeChar SelectFilenameUnicode[MaxFilenameLength];
  s32 SelectWindowTopOffset;
  EProcStateFileListMode Mode;
  EProcStateFileListPlayMode PlayMode;
  bool MoveFolderLocked;
  bool HiddenFilenameExt;
  bool HiddenNotSupportFileType;
  bool ShowID3Tag;
  u32 BacklightTimeoutSec;
  bool ScreenSaver_BlackMode;
  bool DisableLRKeyOnPanelClosed;
  bool LRClickLongSeek;
  u32 Reserved0,Reserved1,Reserved2,Reserved3;
} TProcState_FileList;

enum EDPGPlayMode {EDPM_Repeat,EDPM_AllRep,EDPM_Random};

typedef struct {
  bool EnabledFastStart;
  EDPGPlayMode PlayMode;
  bool BacklightFlag;
  bool PauseWhenPanelClosed;
} TProcState_DPG;

typedef struct {
  s32 MultipleFix8;
  bool ShowInfomation;
  bool ShowControlIcons;
  bool DoubleSpeedKey;
  bool DoubleSpeedTouch;
  bool MultipleResume;
  bool AutoFitting;
  bool EffectHeightPadding;
  bool EffectPastelForTopBG;
  bool EffectPastelForBottomBG;
} TProcState_Image;

enum ETextTopScrMode {ETTSM_LightOff,ETTSM_Text,ETTSM_Clock};
enum ETextLineSpace {ETLS_Small,ETLS_Middle,ETLS_Large};

#define Text_FontSize_Small (12)
#define Text_FontSize_Middle (14)
#define Text_FontSize_Large (16)

typedef struct {
  ETextTopScrMode TopScrMode;
  bool ScreenSaver_BlackMode;
  u32 FontSize;
  bool ClearTypeFont;
  ETextLineSpace LineSpace;
  u32 DefaultCodePage;
  bool DetectCharCode_ANSI,DetectCharCode_SJIS,DetectCharCode_UTF16BE,DetectCharCode_UTF16LE,DetectCharCode_UTF8;
} TProcState_Text;

typedef struct {
  u32 Version;
  TProcState_System System;
  TProcState_FileList FileList;
  TProcState_DPG DPG;
  TProcState_Image Image;
  TProcState_Text Text;
  u32 Reserved1,Reserved2,Reserved3;
} TProcState;

extern TProcState ProcState;

extern bool ProcState_RequestSave;

extern void ApplyCurrentBacklightLevel(void);
extern void ChangePrevBacklightLevel(void);
extern void ChangeNextBacklightLevel(void);

enum EBGBMPType {EBGBT_None=0,EBGBT_8bit=1,EBGBT_15bit=2};

#endif

