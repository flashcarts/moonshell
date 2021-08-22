#define NDSPCMOutRate 32768

//#define SNESDS

#ifdef SNESDS

#define ARM7SPC

#define MIXBUFSIZE 32
#define MIXRATE 24000

#else

// #define ARM7SPC

#ifdef ARM7SPC
#define MIXBUFSIZE 32
#define MIXRATE 26000
#else
//#define MIXBUFSIZE 112
//#define MIXRATE 32000
#define MIXBUFSIZE 64
#define MIXRATE (NDSPCMOutRate*(1000+24)/1000)
#define APURATE (NDSPCMOutRate)
//#define APURATE (NDSPCMOutRate)
#endif

//#define DEBUG
#ifdef DEBUG
//#define COMPILE_SNES9X
//#define COMPILE_SNEESE
#endif

#endif
