
// libnds -> ndslib

//#define _reg_ndslib_h // ignore

#ifndef _reg_ndslib_h
#define _reg_ndslib_h

#define SERIAL_CR (REG_SPICNT)
#define SERIAL_DATA (REG_SPIDATA)

#define SERIAL_ENABLE (SPI_ENABLE)

#define SPI_BAUDRATE_1Mhz (SPI_BAUD_1MHz)

#define IME (REG_IME)
#define IE (REG_IE)
#define IF (REG_IF)

#define IPC_SYNC (REG_IPC_SYNC)

#define XKEYS (REG_KEYXY)
#define	KEYS (REG_KEYINPUT)

#define IPC_FIFO_SEND              (REG_IPC_FIFO_TX)
#define IPC_FIFO_RECIEVE           (REG_IPC_FIFO_RX)                                      
#define IPC_FIFO_CR                (REG_IPC_FIFO_CR)

#endif
