
#ifndef launchstate_h
#define launchstate_h

#include "unicode.h"
#include "shell.h"

extern void LaunchState_Init(void);
extern void LaunchState_Load(void);
extern void LaunchState_Save(void);

enum ELaunchState_Tabs {ELST_Launch=0,ELST_NDS,ELST_Count};

extern void LaunchState_Add(ELaunchState_Tabs Tabs,const UnicodeChar *pFullPathUnicode);

#define LaunchState_TabsCount (ELST_Count)
#define LaunchState_Tab_FilesCountMax (5)

typedef struct {
  u32 FilesCount;
  UnicodeChar FullPathUnicode[LaunchState_Tab_FilesCountMax][MaxFilenameLength];
} TLaunchState_Tab;

typedef struct {
  u32 Version;
  ELaunchState_Tabs LastTab;
  TLaunchState_Tab Tabs[LaunchState_TabsCount];
  u32 Reserved1,Reserved2,Reserved3;
} TLaunchState;

extern TLaunchState LaunchState;

#endif

