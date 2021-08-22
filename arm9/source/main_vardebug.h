
static void main_VarDebug(void)
{
  _consolePrintf("\n");
  
  _consolePrintf("Dump firmware setting.\n");
  
  u8 *pbuf=(u8*)malloc(1*1024*1024);
  _consolePrintf("malloc()=0x%08x\n",pbuf);
  
  IPC6->VarDebug=(u32)pbuf;
  while(0x02000000<=IPC6->VarDebug){
    for(vu32 w=0;w<0x10000;w++){
    }
  }
  
  const char *pfn="/FIRM_SET.BIN";
  
  _consolePrintf("Open file for write. [%s]\n",pfn);
  FAT_FILE *pf=FAT2_fopen_AliasForWrite(pfn);
  _consolePrintf("Writing... %dbyte.\n",IPC6->VarDebug);
  FAT2_fwrite(pbuf,1,IPC6->VarDebug,pf);
  _consolePrintf("Close file.\n");
  FAT2_fclose(pf);

  _consolePrintf("Dump OK.\n");
  _consolePrintf("\n");
  
  _consolePrintf("Please [%s] send to Moonlight.\n",pfn);
  _consolePrintf("\n");
  
  IPC6->VarDebug=0;
  
  while(1);
}

