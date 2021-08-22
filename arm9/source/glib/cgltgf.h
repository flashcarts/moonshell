
#ifndef CglTGF_h
#define CglTGF_h

#include <stdlib.h>
#include <NDS.h>

#include "cglcanvas.h"

class CglTGF
{
  int Width,Height;
  u16 *pdata;
  u16 **ppLineOffsets;
  CglTGF(const CglTGF&);
  CglTGF& operator=(const CglTGF&);
public:
  CglTGF(const u8 *_buf,const int _size);
  ~CglTGF(void);
  int GetWidth(void) const;
  int GetHeight(void) const;
  void BitBlt(CglCanvas *pDestCanvas,const int nDestLeft,const int nDestTop) const;
  void BitBltLimitY(CglCanvas *pDestCanvas,const int nDestLeft,const int nDestTop,const int nHeight,const int nSrcTop) const;
};

#endif

