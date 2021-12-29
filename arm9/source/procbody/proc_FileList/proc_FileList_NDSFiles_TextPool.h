
static u8 *pTextPoolStart=NULL;
static u8 *pTextPoolEnd;
static u8 *pTextPoolPos;

static void FreeTextPool(void)
{
  if(pTextPoolStart!=NULL){
    free(pTextPoolStart); pTextPoolStart=NULL;
  }
}

static void InitTextPool(void)
{
  FreeTextPool();
  
  u32 maxsize=GetMaxMemoryBlockSize();
  
  if(maxsize<(128*1024)){
    PrintFreeMem();
    _consolePrintf("Fatal error: The memory is extremely little.\n");
    ShowLogHalt();
  }
  maxsize-=64*1024;
  
  pTextPoolStart=(u8*)malloc(maxsize);
  
  if(pTextPoolStart==NULL){
    _consolePrintf("Fatal error: TextPool memory allocate error. malloc(%d)\n",maxsize);
    ShowLogHalt();
  }

  pTextPoolEnd=&pTextPoolStart[maxsize];
  pTextPoolPos=pTextPoolStart;
}

static void EndTextPool(void)
{
  _consolePrintf("pTextPool used.%dkbyte remain.%dkbyte\n",(pTextPoolPos-pTextPoolStart)/1024,(pTextPoolEnd-pTextPoolPos)/1024);
  
  u8 *ptmp=(u8*)realloc(pTextPoolStart,pTextPoolPos-pTextPoolStart);
  
  if(pTextPoolStart==ptmp){
    _consolePrint("Succeeded memory optimization.\n");
    }else{
    _consolePrintf("Fatal error!! realloc. 0x%x!=0x%x\n",pTextPoolStart,ptmp);
    ShowLogHalt();
  }

  pTextPoolStart=ptmp;
  
  pTextPoolEnd=NULL;
  pTextPoolPos=NULL;
}

static u8* GetTextPool(u32 size)
{
  size=(size+3)&~3;
  
  u8 *p=pTextPoolPos;
  
  if(pTextPoolEnd<=(p+size)){
    _consolePrintf("Fatal error: TextPool memory overflow. size=%dbyte\n",size);
    ShowLogHalt();
    return(NULL);
  }
  
  pTextPoolPos+=size;
  
  return(p);
}

static void* GetTextPoolVoid(u32 size)
{
  return((void*)GetTextPool(size));
}

static char* GetTextPoolChar(u32 size)
{
  return((char*)GetTextPool((size+1)*1));
}

static UnicodeChar* GetTextPoolUnicode(u32 size)
{
  return((UnicodeChar*)GetTextPool((size+1)*2));
}

static char* TextPoolChar_AllocateCopy(const char *psrc)
{
  u32 len=strlen(psrc);
  char *pdst=GetTextPoolChar(len);
  strcpy(pdst,psrc);
  return(pdst);
}

static UnicodeChar* TextPoolUnicode_AllocateCopy(const UnicodeChar *psrc)
{
  u32 len=Unicode_GetLength(psrc);
  UnicodeChar *pdst=GetTextPoolUnicode(len);
  Unicode_Copy(pdst,psrc);
  return(pdst);
}

