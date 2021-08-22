
static u32 libconv_SJIS_CheckErrorCharsCount(u8 *pbuf,u32 bufsize,CglFont *pFont)
{
  TSJIS2Unicode *ps2u=&SJIS2Unicode;
  
  u32 errcnt=0;
  
  u8 *pbufterm=&pbuf[bufsize];
  
  while(pbuf<pbufterm){
    u32 widx;
    
    {
      u32 c0=pbuf[0];
      u32 c1=pbuf[1];
      if(ps2u->panktbl[c0]==true){
        widx=c0;
        pbuf+=1;
        }else{
        u32 sjis=(c0<<8)|c1;
        widx=ps2u->ps2utbl[sjis];
        pbuf+=2;
      }
    }
    
    if((0x20<=widx)&&(pFont->isExists(widx)==false)) errcnt++;
  }
  
  return(errcnt);
}

static void libconv_SJIS_DetectReturnCode(u8 *pbuf,u32 bufsize)
{
  TSJIS2Unicode *ps2u=&SJIS2Unicode;
  
  u8 *pbufterm=&pbuf[bufsize];
  
  while(pbuf<pbufterm){
    u32 c0=pbuf[0];
    u32 c1=pbuf[1];
    
    if(ps2u->panktbl[c0]==true){
      u32 ch=c0;
      
      if(ch==CharCR){
        u32 ch=c1;
        ReturnCode=ERC_CR;
        if(ch==CharLF) ReturnCode=ERC_CRLF;
        return;
      }
      
      if(ch==CharLF){
        u32 ch=c1;
        ReturnCode=ERC_LF;
        if(ch==CharCR) ReturnCode=ERC_LFCR;
        return;
      }
      
      pbuf+=1;
      }else{
      pbuf+=2;
    }
    
  }
}

static void libconv_SJIS_Convert(u8 *pbuf,u32 bufsize,CglFont *pFont)
{
  TSJIS2Unicode *ps2u=&SJIS2Unicode;
  
  ConvertBody_Phase1;
    
  while(pbuf<pbufterm){
    u32 widx;
    
    {
      u32 c0=pbuf[0];
      u32 c1=pbuf[1];
      
      if(ps2u->panktbl[c0]==true){
        widx=c0;
        pbuf+=1;
        }else{
        u32 sjis=(c0<<8)|c1;
        widx=ps2u->ps2utbl[sjis];
        pbuf+=2;
      }
    }
    
    ConvertBody_Phase2;
  }
  
  ConvertBody_Phase3;
}

static void libconv_SelectEncode_SJIS(void)
{
  pEncodeID="S-JIS (JP)";
  libconv_CheckErrorCharsCount=libconv_SJIS_CheckErrorCharsCount;
  libconv_DetectReturnCode=libconv_SJIS_DetectReturnCode;
  libconv_Convert=libconv_SJIS_Convert;
}

