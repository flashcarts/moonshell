
#ifndef cstream_fs_h
#define cstream_fs_h

#include <stdlib.h>
#include <NDS.h>

#include "cstream.h"
#include "fat2.h"

class CStreamFS: public CStream
{
  FAT_FILE *file;
  CStreamFS(const CStreamFS&);
  CStreamFS& operator=(const CStreamFS&);
public:
  CStreamFS(FAT_FILE *_file);
  ~CStreamFS(void);
  int GetOffset(void) const;
  void SetOffset(int _ofs);
  int GetSize(void) const;
  void OverrideSize(int _size);
  bool eof(void) const;
  u8 Readu8(void);
  u16 Readu16(void);
  u32 Readu32(void);
  int ReadBuffer(void *_dstbuf,const int _readsize);
  // fast request 16bit aligned file position and write buffer
  int ReadBuffer16bit(void *_dstbuf,const int _readsize);
  int ReadBuffer32bit(void *_dstbuf,const int _readsize);
  void ReadSkip(const int _readsize);
};

#endif

