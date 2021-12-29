
//#define FrameCacheDebugOut

typedef struct {
  u64 ShowSamples;
  u16 *pbuf;
} TFrameCacheBody;

#define MaxBodyCount (32)

typedef struct {
  u32 BufferSizeByte;
  u32 ReadIndex,WriteIndex;
  u32 BodyCount;
  TFrameCacheBody Bodys[MaxBodyCount];
} TFrameCache;

static TFrameCache FrameCache;

static void *ptmp=NULL;

static void FrameCache_Init(u32 BufferSizeByte)
{
  TFrameCache *pfc=&FrameCache;
  
  REG_IME=0;
  
  u32 maxsize=GetMaxMemoryBlockSize();
  
  u32 reqsize=(256*1024)+(BufferSizeByte*3);
  if(maxsize<reqsize){
    _consolePrintf("Fatal error: Not empty main memory for frame cache. request size is %dbyte.\n",reqsize);
    ShowLogHalt();
  }
  
  maxsize-=384*1024; // reserved 384kbyte
  
  _consolePrintf("MaxMemoryBlockSize=%d, RequestSize=%d.\n",maxsize,reqsize);
  
  pfc->BufferSizeByte=BufferSizeByte;
  pfc->ReadIndex=0;
  pfc->WriteIndex=0;
  pfc->BodyCount=maxsize/BufferSizeByte;
  if(MaxBodyCount<pfc->BodyCount) pfc->BodyCount=MaxBodyCount;
  
  if(pfc->BodyCount==0){
    _consolePrintf("Fatal error: Frame cache memory overflow.\n");
    ShowLogHalt();
  }
  
  for(u32 idx=0;idx<pfc->BodyCount;idx++){
    TFrameCacheBody *pfcb=&pfc->Bodys[idx];
    pfcb->ShowSamples=(u64)-1;
    pfcb->pbuf=(u16*)safemalloc(BufferSizeByte);
    if(pfcb->pbuf==NULL){
      _consolePrintf("Memory overflow. can not create FrameCache buffers.\n");
      ShowLogHalt();
    }
  }
  
  _consolePrintf("FrameCacheInfo: FramesCount=%d, BufferSizeByte=%d, TotalSize=%d, maxsize=%d.\n",pfc->BodyCount,BufferSizeByte,pfc->BodyCount*BufferSizeByte,maxsize);
  PrintFreeMem();
  
  REG_IME=1;
}

static void FrameCache_Free(void)
{
  if(ptmp!=NULL){
    safefree(ptmp); ptmp=NULL;
  }
  
  TFrameCache *pfc=&FrameCache;
  
  REG_IME=0;
  
  pfc->ReadIndex=0;
  pfc->WriteIndex=0;
  
  for(u32 idx=0;idx<pfc->BodyCount;idx++){
    TFrameCacheBody *pfcb=&pfc->Bodys[idx];
    pfcb->ShowSamples=0;
    if(pfcb->pbuf!=NULL){
      safefree(pfcb->pbuf); pfcb->pbuf=NULL;
    }
  }
  
  pfc->BodyCount=0;
  
  REG_IME=1;
}

#define ProcMacro_LimitedCount(idx,cnt) { \
  while(cnt<=idx) idx-=cnt; \
}

static void FrameCache_Clear(void)
{
  TFrameCache *pfc=&FrameCache;
  
  REG_IME=0;
  
  pfc->ReadIndex=0;
  pfc->WriteIndex=0;
  
  REG_IME=1;
}

CODE_IN_ITCM bool FrameCache_isReadEmpty(void)
{
  TFrameCache *pfc=&FrameCache;
  
  if(pfc->ReadIndex==pfc->WriteIndex){
#ifdef FrameCacheDebugOut
//    _consolePrint("FrameCache_isReadEmpty=True.\n");
#endif
    return(true);
  }
  
  return(false);
}

u32 FrameCache_GetReadFramesCount(void)
{
  TFrameCache *pfc=&FrameCache;
  return(pfc->BodyCount);
}

u32 FrameCache_GetReadFrameLastCount(void)
{
  TFrameCache *pfc=&FrameCache;
  u32 cnt=pfc->BodyCount+(pfc->WriteIndex-pfc->ReadIndex);
  ProcMacro_LimitedCount(cnt,pfc->BodyCount)
  return(cnt);
}

static bool FrameCache_isWriteFull(void)
{
  TFrameCache *pfc=&FrameCache;
  
  u32 widx=pfc->WriteIndex+1;
  ProcMacro_LimitedCount(widx,pfc->BodyCount)
  if(widx==pfc->ReadIndex){
#ifdef FrameCacheDebugOut
    _consolePrint("FrameCache_isWriteFull=True.\n");
#endif
    return(true);
  }
  
  return(false);
}

static u64 FrameCache_WriteShowSamplesTemp;

static void FrameCache_WriteSetShowSamples(u64 ShowSamples)
{
  FrameCache_WriteShowSamplesTemp=ShowSamples;
}

static u64 FrameCache_WriteGetShowSamples(void)
{
  return(FrameCache_WriteShowSamplesTemp);
}

static u16* FrameCache_WriteStart(void)
{
  TFrameCache *pfc=&FrameCache;
  
  TFrameCacheBody *pfcb=&pfc->Bodys[pfc->WriteIndex];
  
  pfcb->ShowSamples=FrameCache_WriteShowSamplesTemp;
  return(pfcb->pbuf);
}

static void FrameCache_WriteEnd(void)
{
  TFrameCache *pfc=&FrameCache;
  
  pfc->WriteIndex++;
  ProcMacro_LimitedCount(pfc->WriteIndex,pfc->BodyCount)
}

CODE_IN_ITCM u32 FrameCache_GetBufferSizeByte(void)
{
  TFrameCache *pfc=&FrameCache;
  
  return(pfc->BufferSizeByte);
}

CODE_IN_ITCM u16* FrameCache_ReadStart(u64 CurrentSamples)
{
  if(FrameCache_isReadEmpty()==true) return(NULL);
  
  TFrameCache *pfc=&FrameCache;
  
  TFrameCacheBody *pfcb=&pfc->Bodys[pfc->ReadIndex];
  
  if(CurrentSamples<pfcb->ShowSamples) return(NULL);
  
  return(pfcb->pbuf);
}

CODE_IN_ITCM void FrameCache_ReadEnd(void)
{
  TFrameCache *pfc=&FrameCache;
  
  pfc->ReadIndex++;
  ProcMacro_LimitedCount(pfc->ReadIndex,pfc->BodyCount)
}

