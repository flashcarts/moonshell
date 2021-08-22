
FAT_FILE* FAT2_fopen_CurrentForRead(void)
{
  int fileNum;
  FAT_FILE* file;
  DIR_ENT dirEntry;

  dirEntry = ((DIR_ENT*) globalBuffer)[wrkDirOffset];
  
  // Find a free file buffer
  for (fileNum = 0; (fileNum < MAX_FILES_OPEN) && (openFiles[fileNum].inUse == true); fileNum++);
  
  if (fileNum == MAX_FILES_OPEN) // No free files
  {
    _consolePrintf("Fatal error: Can not open. not empty files area.\n");
    ShowLogHalt();
    return NULL;
  }

  file = &openFiles[fileNum];
  // Remember where directory entry was
  file->dirEntSector = (wrkDirCluster == FAT16_ROOT_DIR_CLUSTER ? filesysRootDir : FAT_ClustToSect(wrkDirCluster)) + wrkDirSector;
  file->dirEntOffset = wrkDirOffset;

  { // if ( strpbrk(mode, "rR") != NULL )  //(ucase(mode[0]) == 'R')
    if (dirEntry.name[0] == FILE_FREE)  // File must exist
    {
      _consolePrintf("Fatal error: no file?\n");
      ShowLogHalt();
      return NULL;
    }
    
    file->read = true;
    file->write = false;
    file->append = false;
    
    // Store information about position within the file, for use
    // by FAT_fread, FAT_fseek, etc.
    file->firstCluster = dirEntry.startCluster | (dirEntry.startClusterHigh << 16);
  
#ifdef CAN_WRITE_TO_DISC
    // Check if file is openned for random. If it is, and currently has no cluster, one must be 
    // assigned to it.
    if (file->write && file->firstCluster == CLUSTER_FREE)
    {
      file->firstCluster = FAT_LinkFreeCluster (CLUSTER_FREE);
      if (file->firstCluster == CLUSTER_FREE)  // Couldn't get a free cluster
      {
         _consolePrintf("Fatal error: Disk full!!\n");
        ShowLogHalt();
        return NULL;
      }

      // Store cluster position into the directory entry
      dirEntry.startCluster = (file->firstCluster & 0xFFFF);
      dirEntry.startClusterHigh = ((file->firstCluster >> 16) & 0xFFFF);
      disc_SystemReadSector (file->dirEntSector, globalBuffer);
      ((DIR_ENT*) globalBuffer)[file->dirEntOffset] = dirEntry;
      disc_SystemWriteSector (file->dirEntSector, globalBuffer);
    }
#endif
      
    file->length = dirEntry.fileSize;
    file->curPos = 0;
    file->curClus = dirEntry.startCluster | (dirEntry.startClusterHigh << 16);
    file->curSect = 0;
    file->curByte = 0;

    // Not appending
    file->appByte = 0;
    file->appClus = 0;
    file->appSect = 0;
  
    disc_ReadSector( FAT_ClustToSect( file->curClus), file->readBuffer);
    file->inUse = true;  // We're using this file now

    return file;
  }  // mode "r"

}

