
static void _readFirmware(uint32 address, void * destination, uint32 size) {
//---------------------------------------------------------------------------------
	uint8 * buffer = (uint8 *)destination;

	// Read command
	while (REG_SPICNT & SPI_BUSY);
	REG_SPICNT = SPI_ENABLE | SPI_BYTE_MODE | SPI_CONTINUOUS | SPI_DEVICE_FIRMWARE;
	REG_SPIDATA = FIRMWARE_READ;
	while (REG_SPICNT & SPI_BUSY);

	// Set the address
	REG_SPIDATA = (address>>16) & 0xFF;
	while (REG_SPICNT & SPI_BUSY);
	REG_SPIDATA = (address>>8) & 0xFF;
	while (REG_SPICNT & SPI_BUSY);
	REG_SPIDATA = (address>>0) & 0xFF;
	while (REG_SPICNT & SPI_BUSY);

	// Read the data
	while (size--) {
		REG_SPIDATA = 0;
		if ( size == 0 ) REG_SPICNT = SPI_ENABLE | SPI_BYTE_MODE | SPI_DEVICE_FIRMWARE;
		while (REG_SPICNT & SPI_BUSY);
		*buffer++ = (REG_SPIDATA & 0xFF);
	}

	REG_SPICNT = 0;
}

static void _readUserSettings(void) {
//---------------------------------------------------------------------------------

	PERSONAL_DATA slot1;
	PERSONAL_DATA slot2;

	short slot1count, slot2count;
	short slot1CRC, slot2CRC;

	uint32 userSettingsBase;
	_readFirmware( 0x20, &userSettingsBase,2);
	
	uint32 slot1Address = userSettingsBase * 8;
	uint32 slot2Address = userSettingsBase * 8 + 0x100;
	
	_readFirmware( slot1Address , &slot1, sizeof(PERSONAL_DATA));
	_readFirmware( slot2Address , &slot2, sizeof(PERSONAL_DATA));
	_readFirmware( slot1Address + 0x70, &slot1count, 2);
	_readFirmware( slot2Address + 0x70, &slot2count, 2);
	_readFirmware( slot1Address + 0x72, &slot1CRC, 2);
	_readFirmware( slot2Address + 0x72, &slot2CRC, 2);

	short calc1CRC = swiCRC16( 0xffff, &slot1, sizeof(PERSONAL_DATA));
	short calc2CRC = swiCRC16( 0xffff, &slot2, sizeof(PERSONAL_DATA));

	// bail out if neither slot is valid
	if ( calc1CRC != slot1CRC && calc2CRC != slot2CRC) return;
	
	void *currentSettings=NULL;
	
	// if both slots are valid pick the most recent
	if ( (calc1CRC == slot1CRC) && (calc2CRC == slot2CRC) ) {
	  if(currentSettings==NULL){
	    // It doesn't test.
	    if((slot1count & 0x7f) == 0x00) currentSettings = &slot1;
	    if((slot2count & 0x7f) == 0x00) currentSettings = &slot2;
	  }
	  if(currentSettings==NULL){
	    if(slot2count<slot1count){
	      currentSettings = &slot1;
	      }else{
	      currentSettings = &slot2;
	    }
	  }
	}
	
	if(currentSettings==NULL){
	  if ( calc1CRC == slot1CRC ) currentSettings = &slot1;
	  if ( calc2CRC == slot2CRC ) currentSettings = &slot2;
	}
	
	memcpy ( PersonalData, currentSettings, sizeof(PERSONAL_DATA));
}

static void main_VarDebug(void)
{
  IPC6->VarDebug=0;
  while(IPC6->VarDebug==0){
    for(vu32 w=0;w<0x10000;w++){
    }
  }
  
  u8 *pbuf=(u8*)IPC6->VarDebug;
  u32 bodysize=sizeof(PERSONAL_DATA)*0+128;
  
  u32 writesize=0;
  
  {
    u8 *ppersonal=(u8*)PersonalData;
    for(u32 idx=0;idx<bodysize;idx++){
      *pbuf++=ppersonal[idx];
    }
    writesize+=bodysize;
  }
  
  {
    u8 *ppersonal=(u8*)PersonalData;
    for(u32 idx=0;idx<bodysize;idx++){
      ppersonal[idx]=0;
    }
  }
  
  _readUserSettings();
  
  {
    u8 *ppersonal=(u8*)PersonalData;
    for(u32 idx=0;idx<bodysize;idx++){
      *pbuf++=ppersonal[idx];
    }
    writesize+=bodysize;
  }
  
  IPC6->VarDebug=writesize;
  
  while(1);
}

