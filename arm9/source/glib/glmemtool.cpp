
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include <NDS.h>

#include "glib.h"
#include "glmemtool.h"

void *glsafemalloc(int size)
{
  if(size<=0){
    glDebugPrintf("Fatal error: glsafemalloc(%dbyte);\n",size);
    ShowLogHalt();
  }
  
  return(malloc(size));
}

void glsafefree(void *ptr)
{
  if(ptr==NULL){
    glDebugPrintf("safefree Request NullPointer.\n");
    return;
  }
  
  free(ptr);
}

