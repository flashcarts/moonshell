
#ifndef memtool_h
#define memtool_h

extern void DCache_FlushRangeOverrun(const void *v,u32 size);
extern void DCache_CleanRangeOverrun(const void *v,u32 size);

extern void MemCopy8CPU(const void *src,void *dst,u32 len);
extern void MemCopy16CPU(const void *src,void *dst,u32 len);
extern void MemCopy32CPU(const void *src,void *dst,u32 len);
extern void MemSet8CPU(u8 v,void *dst,u32 len);
extern void MemSet16CPU(u16 v,void *dst,u32 len);
extern void MemSet32CPU(u32 v,void *dst,u32 len);

extern void atype_init(void);
extern void atype_showallocated(void);
extern void atype_lockall(void);
extern void atype_checkmemoryleak(void);
extern void atype_checkoverrange(void);

extern void *safemalloc(int size);
extern void safefree(void *ptr);

extern void PrintFreeMem(void);
extern u32 GetMaxMemoryBlockSize(void);
extern void MemClearAllFreeBlocks(void);

#endif

