
#define ConvertBody_Phase1 \
  TextLinesCount=0; \
   \
  const u8 *pbufterm=&pbuf[bufsize]; \
  u32 msgredrawcnt=0; \
   \
  u32 TopOffset=0; \
   \
  UnicodeChar Buf[DefaultBufSize+256]; /* and 1buffer. */ \
  u32 BufOffset=0; \
   \
  const u8 *pWidths=pExtFont->Widths; \
  u32 Length=0,Width=0; \
   \
  u32 IgnoreReturnCodeChar; \
  u32 IgnoreReturnCodeSize; \
   \
  switch(ReturnCode){ \
    case ERC_Unknown: IgnoreReturnCodeChar=0; IgnoreReturnCodeSize=0; break; \
    case ERC_CR:   IgnoreReturnCodeChar=CharCR; IgnoreReturnCodeSize=1-1; break; \
    case ERC_LF:   IgnoreReturnCodeChar=CharLF; IgnoreReturnCodeSize=1-1; break; \
    case ERC_CRLF: IgnoreReturnCodeChar=CharCR; IgnoreReturnCodeSize=2-1; break; \
    case ERC_LFCR: IgnoreReturnCodeChar=CharLF; IgnoreReturnCodeSize=2-1; break; \
    default: IgnoreReturnCodeChar=0; IgnoreReturnCodeSize=0; break; \
  }
   \
  u32 IgnoreCount=0; \



#define ConvertBody_Phase2 \
    bool reqreturn=false; \
    u32 CharWidth=0; \
     \
    if(IgnoreCount!=0){ \
      IgnoreCount--; \
      widx=0; \
      }else{ \
      if(widx<0x20){ \
        if(widx==IgnoreReturnCodeChar){ \
          reqreturn=true; \
          IgnoreCount=IgnoreReturnCodeSize; \
        } \
        widx=0; \
        }else{ \
        CharWidth=pWidths[widx]; \
        if(CharWidth==0) widx=0; \
        if(LineWidth<(Width+CharWidth)) reqreturn=true; \
      } \
    } \
     \
    if(reqreturn==true){ \
      Buf[BufOffset++]=CharLineEnd; \
      pTextLines[TextLinesCount].TopOffset=TopOffset; \
      TextLinesCount++; \
      if(TextLinesMaxCount<TextLinesCount){ \
        _consolePrintf("Fatal error: Text line buffer overflow. Max lines count is %d.\n",TextLinesMaxCount); \
        ShowLogHalt(); \
      } \
      TopOffset+=Length+1; \
      Length=0; \
      Width=0; \
    } \
     \
    if(widx!=0){ \
      Buf[BufOffset++]=widx; \
      Length++; \
      Width+=CharWidth; \
    } \
     \
    if(DefaultBufSize<=BufOffset){ \
      DFS_WriteSectors((u8*)Buf,(DefaultBufSize*2)/512); \
      BufOffset-=DefaultBufSize; \
      if(BufOffset!=0) MemCopy16CPU(&Buf[DefaultBufSize],&Buf[0],BufOffset*2); \
      if(msgredrawcnt!=0){ \
        msgredrawcnt--; \
        }else{ \
        msgredrawcnt=16; \
        char msg[32]; \
        snprintf(msg,32,"Converting... %dchars",TopOffset); \
        InitMsg_Draw(msg); \
      } \
    } \



#define ConvertBody_Phase3 \
  if(Length!=0){ \
    Buf[BufOffset++]=CharLineEnd; \
    pTextLines[TextLinesCount].TopOffset=TopOffset; \
    TextLinesCount++; \
    if(TextLinesMaxCount<TextLinesCount){ \
      _consolePrintf("Fatal error: Text line buffer overflow. Max lines count is %d.\n",TextLinesMaxCount); \
      ShowLogHalt(); \
    } \
  } \
   \
  u32 sec=((BufOffset*2)+511)/512; \
  if(sec!=0) DFS_WriteSectors((u8*)Buf,sec); \
   \
  TotalCharsCount=TopOffset; \


