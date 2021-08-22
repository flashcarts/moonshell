
#ifndef inifile_h
#define inifile_h

typedef struct {
  u32 MaxVoiceCount;
  bool Flag_Interpolate;
  bool Flag_Surround;
  bool Flag_HQMixer;
  u32 Channels,Frequency;
} TiniMikModPlugin;

typedef struct {
  TiniMikModPlugin MikModPlugin;
} TGlobalINI;

extern TGlobalINI GlobalINI;

extern void InitINI(void);
extern void LoadINI(char *data,int size);

#endif


