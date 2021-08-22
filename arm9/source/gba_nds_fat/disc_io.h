#ifndef DISC_IO_H
#define DISC_IO_H

//----------------------------------------------------------------------
// Customisable features

//----------------------------------------------------------------------

// When compiling for NDS, make sure NDS is defined
#ifndef NDS
 #if defined ARM9 || defined ARM7
  #define NDS
 #endif
#endif

#ifdef NDS
 #include <nds/jtypes.h>
#else
 #include "gba_types.h"
#endif

// Disable NDS specific hardware and features if running on a GBA
#ifndef NDS 
 #undef SUPPORT_NMMC
 #undef DISC_CACHE
#endif

/*

	Interface for host program

*/

#define BYTE_PER_READ 512

/*-----------------------------------------------------------------
disc_Init
Detects the inserted hardware and initialises it if necessary
bool return OUT:  true if a suitable device was found
-----------------------------------------------------------------*/
extern bool disc_Init(void) ;

/*-----------------------------------------------------------------
disc_IsInserted
Is a usable disc inserted?
bool return OUT:  true if a disc is inserted
-----------------------------------------------------------------*/
extern bool disc_IsInserted(void) ;

/*-----------------------------------------------------------------
disc_ReadSectors
Read 512 byte sector numbered "sector" into "buffer"
u32 sector IN: address of first 512 byte sector on disc to read
u8 numSecs IN: number of 512 byte sectors to read,
 1 to 256 sectors can be read, 0 = 256
void* buffer OUT: pointer to 512 byte buffer to store data in
bool return OUT: true if successful
-----------------------------------------------------------------*/
extern bool disc_ReadSectors(u32 sector, u8 numSecs, void* buffer) ;
#define disc_ReadSector(sector,buffer)	disc_ReadSectors(sector,1,buffer)
extern void disc_SystemReadSector(u32 sector, void* buffer);

/*-----------------------------------------------------------------
disc_WriteSectors
Write 512 byte sector numbered "sector" from "buffer"
u32 sector IN: address of 512 byte sector on disc to write
u8 numSecs IN: number of 512 byte sectors to write	,
 1 to 256 sectors can be read, 0 = 256
void* buffer IN: pointer to 512 byte buffer to read data from
bool return OUT: true if successful
-----------------------------------------------------------------*/
extern bool disc_WriteSectors(u32 sector, u8 numSecs, const void* buffer) ;
#define disc_WriteSector(sector,buffer)	disc_WriteSectors(sector,1,buffer)
extern void disc_SystemWriteSector(u32 sector, const void* buffer);

/*-----------------------------------------------------------------
disc_ClearStatus
Tries to make the disc go back to idle mode
bool return OUT:  true if the disc is idle
-----------------------------------------------------------------*/
extern bool disc_ClearStatus(void) ;

/*-----------------------------------------------------------------
disc_Shutdown
unload the disc interface
bool return OUT: true if successful
-----------------------------------------------------------------*/
extern bool disc_Shutdown(void) ;

/*-----------------------------------------------------------------
disc_HostType
Returns a unique u32 number identifying the host type
u32 return OUT: 0 if no host initialised, else the identifier of
	the host
-----------------------------------------------------------------*/
extern u32 disc_HostType(void);

/*-----------------------------------------------------------------
disc_CacheFlush
Flushes any cache writes to disc
bool return OUT: true if successful, false if an error occurs
Added by www.neoflash.com
-----------------------------------------------------------------*/
#define disc_CacheFlush()

/*

	Interface for IO libs

*/

#define FEATURE_MEDIUM_CANREAD		0x00000001
#define FEATURE_MEDIUM_CANWRITE		0x00000002
#define FEATURE_SLOT_GBA			0x00000010
#define FEATURE_SLOT_NDS			0x00000020

typedef bool (* FN_MEDIUM_STARTUP)(void) ;
typedef bool (* FN_MEDIUM_ISINSERTED)(void) ;
typedef bool (* FN_MEDIUM_READSECTORS)(u32 sector, u8 numSecs, void* buffer) ;
typedef bool (* FN_MEDIUM_WRITESECTORS)(u32 sector, u8 numSecs, void* buffer) ;
typedef bool (* FN_MEDIUM_CLEARSTATUS)(void) ;
typedef bool (* FN_MEDIUM_SHUTDOWN)(void) ;


typedef struct {
	unsigned long			ul_ioType ;
	unsigned long			ul_Features ;
	FN_MEDIUM_STARTUP		fn_StartUp ;
	FN_MEDIUM_ISINSERTED	fn_IsInserted ;
	FN_MEDIUM_READSECTORS	fn_ReadSectors ;
	FN_MEDIUM_WRITESECTORS	fn_WriteSectors ;
	FN_MEDIUM_CLEARSTATUS	fn_ClearStatus ;
	FN_MEDIUM_SHUTDOWN		fn_Shutdown ;
} IO_INTERFACE, *LPIO_INTERFACE ;

extern void DISCIO_ShowAdapterInfo(void);

#endif	// define DISC_IO_H
