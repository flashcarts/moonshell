/*
 Modified on 25.5.2005
 to compress data in the lzss version format
 the GBA BIOS function 11h expects as source

 Compress your data using this program

  ./gbalzss e data.raw compressed-data.raw

 If you need to decompress a compressed file on console
 sometimes you can use:

  ./gbalzss d compressed-data.raw data.raw

 IMPORTANT:

 Be SURE to align the compressed data to a 4 byte boundary
 Or the BIOS function will probably not work on it
 Heres an example how to align your data using gcc compiler,
 if you include it as C const array:

 const u8 compressed[4808]__attribute__ ((aligned (4))) = {

 and include or link it then feed it to BIOS function 11h

 You can also decode files you encoded with it

 Greetings Andre Perrot <ovaron@gmx.net>

 credits: as you can see this is based upon Haruhiko Okumura's
 original lzss code so thanks :)

 Also tribute to gbadev.org & cowbite & no$gba's gbatek without
 those I couldnt do much on gba :p

*/

/**************************************************************
  LZSS.C -- A Data Compression Program
  (tab = 4 spaces)
***************************************************************
  4/6/1989 Haruhiko Okumura
  Use, distribute, and modify this program freely.
  Please send me your improved versions.
    PC-VAN    SCIENCE
    NIFTY-Serve  PAF01022
    CompuServe  74050,1022
**************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include <sys/types.h>
#include <sys/stat.h>
//#include <unistd.h>

typedef unsigned long u32;
typedef unsigned short u16;
typedef unsigned char u8;

typedef enum { false, true } bool;

#define N     4096  /* size of ring buffer */
#define THRESHOLD  (2)   /* encode string into position and length if match_length is greater than this */
#define F       (16+THRESHOLD)  /* upper limit for match_length */
#define NIL      N  /* index for root of binary search trees */

static u8
    text_buf[N + F - 1];  /* ring buffer of size N,
      with extra F-1 bytes to facilitate string comparison */
static int    match_position, match_length,  /* of longest match.  These are
      set by the InsertNode() procedure. */
    lson[N + 1], rson[N + 257], dad[N + 1];  /* left & right children &
      parents -- These constitute binary search trees. */

#define InitTree()  /* initialize trees */ \
{ \
  int  i; \
 \
  /* For i = 0 to N - 1, rson[i] and lson[i] will be the right and \
     left children of node i.  These nodes need not be initialized. \
     Also, dad[i] is the parent of node i.  These are initialized to \
     NIL (= N), which stands for 'not used.' \
     For i = 0 to 255, rson[N + i + 1] is the root of the tree \
     for strings that begin with character i.  These are initialized \
     to NIL.  Note there are 256 trees. */ \
 \
  for (i = N + 1; i <= N + 256; i++) rson[i] = NIL; \
  for (i = 0; i < N; i++) dad[i] = NIL; \
}

static void InsertNode(int r)
  /* Inserts string of length F, text_buf[r..r+F-1], into one of the
     trees (text_buf[r]'th tree) and returns the longest-match position
     and length via the global variables match_position and match_length.
     If match_length = F, then removes the old node in favor of the new
     one, because the old one will be deleted sooner.
     Note r plays double role, as tree node and position in buffer. */
{
  int  i, p, cmp;
  u8  *key;
  
  cmp = 1;  key = &text_buf[r];  p = N + 1 + key[0];
  rson[r] = lson[r] = NIL;  match_length = 0;
  
  while(1){
    if (cmp >= 0) {
      if (rson[p] != NIL){
        p = rson[p];
        }else{
        rson[p] = r;  dad[r] = p;
        return;
      }
      }else{
      if (lson[p] != NIL){
        p = lson[p];
        }else{
        lson[p] = r;  dad[r] = p;
        return;
      }
    }
    for (i = 1; i < F; i++){
      if ((cmp = key[i] - text_buf[p + i]) != 0) break;
    }
    if (i > match_length) {
      match_position = p;
      if ((match_length = i) >= F)  break;
    }
  }
  
  dad[r] = dad[p];  lson[r] = lson[p];  rson[r] = rson[p];
  dad[lson[p]] = r;  dad[rson[p]] = r;
  if (rson[dad[p]] == p){
    rson[dad[p]] = r;
    }else{
    lson[dad[p]] = r;
  }
  dad[p] = NIL;  /* remove p */
}

static void DeleteNode(int p)  /* deletes node p from tree */ \
{
  int  q;
  
  if (dad[p] == NIL) return;  /* not in tree */
  if (rson[p] == NIL) q = lson[p];
  else if (lson[p] == NIL) q = rson[p];
  else {
    q = lson[p];
    if (rson[q] != NIL) {
      do {  q = rson[q];  } while (rson[q] != NIL);
      rson[dad[q]] = lson[q];  dad[lson[q]] = dad[q];
      lson[q] = lson[p];  dad[lson[p]] = q;
    }
    rson[q] = rson[p];  dad[rson[p]] = q;
  }
  dad[q] = dad[p];
  if (rson[dad[p]] == p) rson[dad[p]] = q;  else lson[dad[p]] = q;
  dad[p] = NIL;
}

static int Encode(u8 *pinbuf,u32 insize,u8 *poutbuf)
// function patched to produce gba bios decompression function processable data
{

#define GetData(c) { \
  if(pinbuf==pinterm) fEOF=true; \
  c=*pinbuf++; \
}

#define PutData(c) { \
  *poutbuf++=c; \
  codesize++; \
}

#define PutBuf(c,len) { \
  u8 *psrc=(u8*)&c; \
  u32 idx; \
  for(idx=0;idx<len;idx++){ \
    poutbuf[idx]=psrc[idx]; \
  } \
  codesize+=len; \
  poutbuf+=len; \
}

  int codesize=0;  /* code size counter */
  int  i, c, len, r, s, last_match_length;
  u32 code_flags;
  u32 code_buf_count;
  u16 code_buf[32];
  u32 mask;
  
  bool fEOF=false;
  u8 *pinterm=&pinbuf[insize];
  
  // write 32 bit header needed for GBA BIOS function
  // Bit 0-3   Reserved
  // Bit 4-7   Compressed type (must be 1 for LZ77) set 15 for lzss16.
  // Bit 8-31  Size of decompressed data
  u32 gbaheader = 0xf0 + (insize<<8);
  
//  for(i=0; i<4; i++) PutData(tmp[i]);
  PutBuf(gbaheader,4);


  InitTree();  /* initialize trees */
  
  code_flags=0;
  code_buf_count = 0;
  for(i=0;i<32;i++){
    code_buf[i]=0;
  }
  
  mask = 0x80000000;  // füò GBA fangen wir mit MSB an
  s = 0;
  r = N - F;   // 4078
  
  {
    u32 i;
    for (i = s; i < r; i++) text_buf[i] = 0xff;  /* Clear the buffer with any character that will appear often. */
  }
  
  for (len = 0; len < F; len++) {
    GetData(c);
    if(fEOF==true) break;
    text_buf[r + len] = c;  /* Read F bytes into the last F bytes of the buffer */
  }
  if (len == 0) return(0);  /* text of size zero */
  
  {
    u32 i;
    for (i = 1; i <= F; i++) InsertNode(r - i);  /* Insert the F strings,
    each of which begins with one or more 'space' characters.  Note
    the order in which these strings are inserted.  This way,
    degenerate trees will be less likely to occur. */
  }
  InsertNode(r);  /* Finally, insert the whole string just read.  The
    global variables match_length and match_position are set. */


  // kompressions schleife

  do {
    if (match_length > len) match_length = len;  /* match_length
      may be spuriously long near the end of text. */

    // nicht komprimieren
    if (match_length <= THRESHOLD) {
      u16 data=text_buf[r] | (text_buf[r+1] << 8);
      // original: code_buf[0] |= mask;  /* 'send one byte' flag */
      code_buf[code_buf_count++] = data;  /* Send uncoded. */
      match_length = 2;  /* Not long enough match.  Send one byte. */
    } else
    // komprimieren
    {
      u32 ofs=(r-match_position-1) & (N - 1);
      u32 len=match_length - (THRESHOLD + 1);
      
      code_flags |= mask;  // flag "komprimiert" setzen
      
      // Bit 0-3   Disp MSBs
      // Bit 4-7   Number of bytes to copy (minus 3)
      // Bit 8-15  Disp LSBs
      
      code_buf[code_buf_count++] = ofs | (len<<12);
    }
    
    mask >>= 1;
    
    // mask shift
    if (mask == 0) {  /* Shift mask right one bit. */
      u32 i;
      PutBuf(code_flags,4);
      for(i=0;i<32;i++){
        PutBuf(code_buf[i],2);
      }
      code_flags=0;
      code_buf_count = 0;
      for(i=0;i<32;i++){
        code_buf[i]=0;
      }
      mask = 0x80000000;
    }
    
    last_match_length = match_length;
    for (i = 0; i < last_match_length; i++) {
      GetData(c);
      if(fEOF==true) break;
      DeleteNode(s);    /* Delete old strings and */
      text_buf[s] = c;  /* read new bytes */
      if (s < F - 1) text_buf[s + N] = c;  /* If the position is
        near the end of buffer, extend the buffer to make
        string comparison easier. */
      s = (s + 1) & (N - 1);  r = (r + 1) & (N - 1);
        /* Since this is a ring buffer, increment the position
           modulo N. */
      InsertNode(r);  /* Register the string in text_buf[r..r+F-1] */
    }
    
    while (i++ < last_match_length) {  /* After the end of text, */
      DeleteNode(s);          /* no need to read, but */
      s = (s + 1) & (N - 1);  r = (r + 1) & (N - 1);
      if (--len) InsertNode(r);    /* buffer may not be empty. */
    }
  } while (len > 0);  /* until length of string to be processed is zero */


  if (code_buf_count!=0) {    /* Send remaining code. */
    u32 i;
    PutBuf(code_flags,4)
    for(i=0;i<32;i++){
      PutBuf(code_buf[i],2);
    }
  }

  // pad output with zeros to make it a multiply of 4
  if(codesize%4){
    for(i=0; i<4-(codesize%4); i++){
      PutData(0x00);
    }
  }

  return(codesize);
}

int gbalzss16enc_static_ExecuteEncode(u8 *pinbuf,u32 insize,u8 *poutbuf)
{
  return(Encode(pinbuf,insize,poutbuf));
}

