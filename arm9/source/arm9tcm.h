
#ifndef arm9tcm_h
#define arm9tcm_h

#define CODE_IN_ITCM __attribute__ ((section (".itcm")))
#define DATA_IN_DTCM __attribute__ ((section (".dtcm")))

#endif

