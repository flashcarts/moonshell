
#ifndef CONFIG_H
#define CONFIG_H

#include <nds.h>

#define CODE_IN_ITCM __attribute__ ((section (".itcm")))
#undef DATA_IN_DTCM
#define DATA_IN_MTCM_SET __attribute__ ((section (".mtcmset"))) 
#define DATA_IN_MTCM_VAR __attribute__ ((section (".mtcmvar"))) 

#include "_console.h"
#include "_consoleWriteLog.h"


#endif /* CONFIG_H */
