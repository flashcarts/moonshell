/*

	disc_io.c

	uniformed io-interface to work with Chishm's FAT library

	Written by MightyMax
  
	Modified by Chishm:
	2005-11-06
		* Added WAIT_CR modifications for NDS

	Modified by www.neoflash.com:
	2006-02-03
		* Added SUPPORT_* defines, comment out any of the SUPPORT_* defines in disc_io.h to remove support
		  for the given interface and stop code being linked to the binary

	    * Added support for MK2 MMC interface

		* Added disc_Cache* functions

	Modified by Chishm:
	2006-02-05
		* Added Supercard SD support
*/

#include "disc_io.h"

#ifdef NDS
	#include <nds.h>
#endif

#include "../libs/memtool.h"

// Include known io-interfaces:
#include "io_dldi.h"

// Keep a pointer to the active interface
LPIO_INTERFACE active_interface = 0;


/*

	Hardware level disc funtions

*/

#include "../_console.h"

#include "mediatype.h"

u32 DIMediaType=DIMT_NONE;
const char *DIMediaName="No adapter.";
char DIMediaID[5]="NONE";

#define SystemCacheCount (64)
#define SystemCacheCountMask (SystemCacheCount-1)

typedef struct {
  u32 RingIndex;
  u32 SectorIndex[SystemCacheCount];
  u32 SectorData[(512/4)*SystemCacheCount];
} TSystemCache;

static TSystemCache SystemCache;

#define GetSectorDataPointer(secidx) (&SystemCache.SectorData[(512/4)*secidx])

extern void disc_SystemCache_ClearAll(void);
void disc_SystemCache_ClearAll(void)
{
  _consolePrintf("Clear system cache.\n");
  
  TSystemCache *psc=&SystemCache;
  psc->RingIndex=0;
  for(u32 idx=0;idx<SystemCacheCount;idx++){
    psc->SectorIndex[idx]=(u32)-1;
  }
  MemSet32CPU(0,psc->SectorData,512*SystemCacheCount);
}

void DISCIO_ShowAdapterInfo(void)
{
  if(active_interface==0){
    _consolePrintf("Fatal error: DISCIO_ShowAdapterInfo: Not ready active interface.\n");
    while(1);
  }
  
  _consolePrintf("active_interface = 0x%08x\n",active_interface);
  _consolePrintf("DeviceID=%s\n",DIMediaID);
  _consolePrintf("DeviceName=%s\n",DIMediaName);
}

#include "maindef.h"
bool disc_Init(void) 
{
  disc_SystemCache_ClearAll();
  
    if (active_interface != 0) return true;
    
    REG_EXMEMCNT &= ~(ARM7_OWNS_ROM | ARM7_OWNS_CARD);
    
    active_interface=DLDI_GetInterface();
    if(active_interface!=0){
      DIMediaType=active_interface->ul_ioType;
      DIMediaName=DLDI_GetAdapterName();
      *(u32*)&DIMediaID[0]=active_interface->ul_ioType;
      *(u32*)&DIMediaID[4]=0;
      
      DISCIO_ShowAdapterInfo();
      
      _consolePrintf("DLDI driver stack check.\n");
      DTCM_StackCheck(1);
      if(active_interface->fn_StartUp()==true){
        DTCM_StackCheck(2);
        u8 buf[512];
        active_interface->fn_ReadSectors(8192,1,buf) ;
        DTCM_StackCheck(3);
        return true;
      }
    }
    
	// could not find a working IO Interface
	active_interface = 0 ;
	return false ;
} 

bool disc_IsInserted(void) 
{
	if (active_interface) return active_interface->fn_IsInserted() ;
	return false ;
} 

bool disc_ReadSectors(u32 sector, u8 numSecs, void* buffer) 
{
	if (active_interface) return active_interface->fn_ReadSectors(sector,numSecs,buffer) ;
	return false ;
} 

bool disc_WriteSectors(u32 sector, u8 numSecs, const void* buffer) 
{
	if (active_interface) return active_interface->fn_WriteSectors(sector,numSecs,(void*)buffer) ;
	return false ;
} 

void disc_SystemReadSector(u32 sector, void* buffer)
{
	if (!active_interface) while(1);
	
  TSystemCache *psc=&SystemCache;
  
  for(u32 idx=0;idx<SystemCacheCount;idx++){
    if(sector==psc->SectorIndex[idx]){
      MemCopy32CPU(GetSectorDataPointer(idx),buffer,512);
      return;
    }
  }
  
  psc->SectorIndex[psc->RingIndex]=sector;
  active_interface->fn_ReadSectors(sector,1,buffer);
  MemCopy32CPU(buffer,GetSectorDataPointer(psc->RingIndex),512);
  psc->RingIndex=(psc->RingIndex+1)&SystemCacheCountMask;
}

void disc_SystemWriteSector(u32 sector, const void* buffer)
{
	if (!active_interface) while(1);
	
  TSystemCache *psc=&SystemCache;
  
  for(u32 idx=0;idx<SystemCacheCount;idx++){
    if(sector==psc->SectorIndex[idx]){
      u32 *pbuf1=(u32*)buffer;
      u32 *pbuf2=(u32*)GetSectorDataPointer(idx);
      active_interface->fn_WriteSectors(sector,1,pbuf1);
      active_interface->fn_ReadSectors(sector,1,pbuf2);
      for(u32 idx=0;idx<512/4;idx++){
        if(pbuf1[idx]!=pbuf2[idx]){
          _consolePrintf("Fatal error: System disk write verify error.\n");
        }
      }
      return;
    }
  }
  
  psc->SectorIndex[psc->RingIndex]=sector;
  
  {
    u32 *pbuf1=(u32*)buffer;
    u32 *pbuf2=(u32*)GetSectorDataPointer(psc->RingIndex);
    active_interface->fn_WriteSectors(sector,1,pbuf1);
    active_interface->fn_ReadSectors(sector,1,pbuf2);
    for(u32 idx=0;idx<512/4;idx++){
      if(pbuf1[idx]!=pbuf2[idx]){
        _consolePrintf("Fatal error: System disk write verify error.\n");
      }
    }
  }
  
  psc->RingIndex=(psc->RingIndex+1)&SystemCacheCountMask;
}

bool disc_ClearStatus(void) 
{
	if (active_interface) return active_interface->fn_ClearStatus() ;
	return false ;
} 

bool disc_Shutdown(void) 
{
	if (active_interface) active_interface->fn_Shutdown() ;
	active_interface = 0 ;
	return true ;
} 

u32	disc_HostType (void)
{
	if (active_interface) {
		return active_interface->ul_ioType;
	} else {
		return 0;
	}
}

