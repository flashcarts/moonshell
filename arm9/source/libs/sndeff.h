
#ifndef sndeff_h
#define sndeff_h

extern void Sound_Init(void);
extern void Sound_Start(const char *wavfn);
extern bool Sound_isEqualLoadedFilename(const char *wavfn);
extern void Sound_Stop(void);
extern void Sound_Free(void);
extern u32 Sound_GetCurrentPlayTimePerVsync(void);

#define WAVFN_Click "snd_click.ttb"
#define WAVFN_MovePage "snd_movepage.ttb"
#define WAVFN_Notify "snd_notify.ttb"
#define WAVFN_Open "snd_open.ttb"
#define WAVFN_LongTap "snd_longtap.ttb"
#define WAVFN_PowerOff "snd_poweroff.ttb"

#endif

