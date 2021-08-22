
#define BookmarkVersion (2)

typedef struct {
  u32 LineNum;
  TDateTime DateTime;
} TBookmarkItem;

#define BookmarkItemCount (4)

typedef struct {
  u32 Version;
  char Header[32];
  u32 ResumeLineNum;
  u32 CurrentItemIndex;
  TBookmarkItem BookmarkItem[BookmarkItemCount];
  ETextEncode TextEncode;
  u8 dummy[512];
} TBookmark;

#define BookmarkStructSize (512)
static TBookmark *pBookmark;

static bool Bookmark_Enabled;

static void Bookmark_Clear(void)
{
  MemSet32CPU(0,pBookmark,BookmarkStructSize);
  
  pBookmark->Version=BookmarkVersion;
  StrCopy("MoonShell2 bookmark file.",pBookmark->Header);
}

static void Bookmark_ChangeExtBKM(UnicodeChar *pfnu)
{
  u32 pos=0;
  
  u32 idx=0;
  while(pfnu[idx]!=0){
    if(pfnu[idx]=='.') pos=idx+1;
    idx++;
  }
  
  if(pos==0){
    _consolePrint("Bookmark_ChangeExtBKM: Not found extention.\n");
    ShowLogHalt();
  }
  
  pfnu[pos+0]='b';
  pfnu[pos+1]='k';
  pfnu[pos+2]='m';
  pfnu[pos+3]=0;
}

static bool Bookmark_LoadFromFile(void)
{
  bool res=false;
  
  Bookmark_Clear();
  
  UnicodeChar fnu[MaxFilenameLength];
  Unicode_Copy(fnu,RelationalFileNameUnicode);
  Bookmark_ChangeExtBKM(fnu);
  
  if(FileExistsUnicode(RelationalFilePathUnicode,fnu)==true){
    FAT_FILE *pf=FAT2_fopen_AliasForRead(ConvertFull_Unicode2Alias(RelationalFilePathUnicode,fnu));
    
    FAT2_fread(pBookmark,BookmarkStructSize,1,pf);
    if(pBookmark->Version!=BookmarkVersion){
      _consolePrintf("Bookmark_LoadFromFile: Illigal version detected. Re-initialize.\n");
      Bookmark_Clear();
      }else{
      res=true;
    }
    
    FAT2_fclose(pf);
  }
  
  return(res);
}

static char Bookmark_SaveToFile_ins_CreateNewFile_ins_uppercase(char ch)
{
  if(((u32)'a'<=ch)&&(ch<=(u32)'z')) ch-=0x20;
  return(ch);
}

static const char* Bookmark_SaveToFile_ins_CreateNewFile(const UnicodeChar *pFilenameUnicode)
{
  u32 extpos=0;
  {
    u32 idx=0;
    while(pFilenameUnicode[idx]!=0){
      if(pFilenameUnicode[idx]==(UnicodeChar)'.') extpos=idx+1;
      idx++;
    }
  }
  
  if(extpos==0){
    _consolePrintf("Fatal error: Not found extention.\n");
    ShowLogHalt();
  }
  
  const u32 fnstrmaxlen=5;
  char fnstr[fnstrmaxlen+1];
  {
    u32 aidx=0,uidx=0;
    const UnicodeChar *pufn=pFilenameUnicode;
    while((aidx<fnstrmaxlen)&&(uidx<extpos)){
      UnicodeChar ch=pufn[uidx++];
      if((0x21<=ch)&&(ch<0x80)){ // 0x20 = space
        if((ch=='\"')||(ch=='*')||(ch=='/')||(ch==':')||(ch=='<')||(ch=='>')||(ch=='?')||(ch=='\\')||(ch=='|')||
           (ch=='.')||(ch=='+')||(ch==',')||(ch==';')||(ch=='=')||(ch=='[')||(ch==']')){
          }else{
          fnstr[aidx++]=Bookmark_SaveToFile_ins_CreateNewFile_ins_uppercase((char)ch);
        }
      }
    }
    fnstr[aidx]=0;
  }
  
  char extstr[4];
  extstr[0]=Bookmark_SaveToFile_ins_CreateNewFile_ins_uppercase((char)pFilenameUnicode[extpos+0]);
  extstr[1]=Bookmark_SaveToFile_ins_CreateNewFile_ins_uppercase((char)pFilenameUnicode[extpos+1]);
  extstr[2]=Bookmark_SaveToFile_ins_CreateNewFile_ins_uppercase((char)pFilenameUnicode[extpos+2]);
  extstr[3]=(char)0;
  
  static char FilenameAlias[13];
  
  u32 digits=1;
  while(1){
    snprintf(FilenameAlias,13,"%s~%02d.%s",fnstr,digits,extstr);
    FAT_FILE *pf=FAT2_fopen_AliasForRead(FilenameAlias);
    if(pf==NULL) break;
    FAT2_fclose(pf);
    digits++;
    if(digits==100){
      _consolePrintf("Fatal error: Alias entry overflow.\n");
      ShowLogHalt();
    }
  }
  
  FAT_FILE *pf=FAT2_fopen_CreateForWrite_on_CurrentFolder(FilenameAlias,pFilenameUnicode);
  FAT2_SetSize(pf,512,0x00);
  FAT2_fclose(pf);
  
  return(FilenameAlias);
}

static void Bookmark_SaveToFile(void)
{
  UnicodeChar fnu[MaxFilenameLength];
  Unicode_Copy(fnu,RelationalFileNameUnicode);
  Bookmark_ChangeExtBKM(fnu);
  
  if(FileExistsUnicode(RelationalFilePathUnicode,fnu)==true){
    FAT_FILE *pf=FAT2_fopen_AliasForWrite(ConvertFull_Unicode2Alias(RelationalFilePathUnicode,fnu));
    FAT2_fwrite(pBookmark,BookmarkStructSize,1,pf);
    FAT2_fclose(pf);
    return;
  }
  
  const char *ppathalias=ConvertFull_Unicode2Alias(RelationalFilePathUnicode,NULL);
  if(FAT2_chdir_Alias(ppathalias)==false){
    _consolePrintf("Fatal error: Bookmark_SaveToFile: Can not change current path.\n");
    ShowLogHalt();
  }
  
  const char *pafn=Bookmark_SaveToFile_ins_CreateNewFile(fnu);
  _consolePrintf("Book mark file created. [%s]\n",pafn);
  
  FAT_FILE *pf=FAT2_fopen_AliasForWrite(ConvertFull_Unicode2Alias(RelationalFilePathUnicode,fnu));
  FAT2_fwrite(pBookmark,BookmarkStructSize,1,pf);
  FAT2_fclose(pf);
}

static void Bookmark_Init(void)
{
  _consolePrint("Bookmark initialize.\n");
  
  Bookmark_Enabled=false;
  
  pBookmark=(TBookmark*)safemalloc(sizeof(TBookmark));
  if(pBookmark==NULL){
    _consolePrintf("Fatal error: Bookmark work memory overflow.\n");
    ShowLogHalt();
  }
  
  if(Bookmark_LoadFromFile()==true){
    _consolePrint("Bookmark file loaded.\n");
    Bookmark_Enabled=true;
  }
  
  _consolePrint("Bookmark initialized.\n");
}

static void Bookmark_Free(void)
{
  if(pBookmark!=NULL){
    safefree(pBookmark); pBookmark=NULL;
  }
  
  Bookmark_Enabled=false;
}

static void Bookmark_Save(u32 itemidx,u32 linenum)
{
  TBookmarkItem *pbmi=&pBookmark->BookmarkItem[itemidx];
  
  DateTime_ResetNow();
  pbmi->DateTime=DateTime_GetNow();
  pbmi->LineNum=linenum;

  Bookmark_SaveToFile();
  
  Bookmark_Enabled=true;
}

static TBookmarkItem* Bookmark_Load(u32 itemidx)
{
  return(&pBookmark->BookmarkItem[itemidx]);
}

static void Bookmark_SetTextEncode(ETextEncode TextEncode)
{
  pBookmark->TextEncode=TextEncode;
  Bookmark_SaveToFile();
}

static ETextEncode Bookmark_GetTextEncode(void)
{
  return(pBookmark->TextEncode);
}

static void Bookmark_SetResumeLineNum(u32 linenum)
{
  pBookmark->ResumeLineNum=linenum;
  if(Bookmark_Enabled==true) Bookmark_SaveToFile();
}

static u32 Bookmark_GetResumeLineNum(void)
{
  return(pBookmark->ResumeLineNum);
}

static u32 Bookmark_GetCurrentItemIndex(void)
{
  return(pBookmark->CurrentItemIndex);
}

static void Bookmark_SetCurrentItemIndex(u32 itemidx)
{
  pBookmark->CurrentItemIndex=itemidx;
  if(Bookmark_Enabled==true) Bookmark_SaveToFile();
}
