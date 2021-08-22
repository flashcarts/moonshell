
#ifndef BootROM_h
#define BootROM_h

extern void BootROM_Init(void);

extern bool BootROM_GetExecuteFlag(void);
extern const char* BootROM_GetFullPathAlias(void);

extern void BootROM_SetInfo_NoLaunch(const UnicodeChar *pPathUnicode,const UnicodeChar *pFilenameUnicode);
extern void BootROM_SetInfo(const UnicodeChar *pPathUnicode,const UnicodeChar *pFilenameUnicode);

extern bool BootROM_isExistsSoftResetToFirmware(void);
extern void BootROM_SoftResetToFirmware(void);

extern void BootROM_Execute(void);

#endif

