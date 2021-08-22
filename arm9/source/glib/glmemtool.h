
#ifndef glmemtool_h
#define glmemtool_h

extern __declspec(noreturn) void ShowLogHalt(void);

#include "../libs/memtool.h"

#define glMemCopy16CPU MemCopy16CPU
#define glMemCopy32CPU MemCopy32CPU
#define glMemSet16CPU MemSet16CPU
#define glMemSet32CPU MemSet32CPU

void *glsafemalloc(int size);
void glsafefree(void *ptr);

#endif

