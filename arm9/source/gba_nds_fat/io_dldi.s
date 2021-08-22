;---------------------------------------------------------------------------------
    AREA     globals,CODE,READONLY
    ALIGN 4
  	
	EXPORT dldibodytop
dldibodytop
	
	EXPORT _io_dldi
	
;---------------------------------------------------------------------------------

FEATURE_MEDIUM_CANREAD EQU		0x00000001
FEATURE_MEDIUM_CANWRITE EQU		0x00000002
FEATURE_SLOT_GBA EQU				0x00000010
FEATURE_SLOT_NDS EQU				0x00000020

;---------------------------------------------------------------------------------
; Driver patch file standard header -- 16 bytes
	DCD	0xBF8DA5ED		; Magic number to identify this region
	DCB	" Chishm",0		; Identifying Magic string (8 bytes with null terminator)
	DCB	0x01			; Version number
	DCB	0x0f	;32KiB	; Log [base-2] of the size of this driver in bytes.
	DCB	0x00			; Sections to fix
	DCB	0x0f	;32KiB	; Log [base-2] of the allocated space in bytes.
;---------------------------------------------------------------------------------
; Text identifier - can be anything up to 47 chars + terminating null -- 16 bytes
	ALIGN 4
	DCB "Default (No interface)",0

;---------------------------------------------------------------------------------
; Offsets to important sections within the data	-- 32 bytes
	ALIGN 64
	DCD 0x00000000		; data start
	DCD 0x00000000		; data end
	DCD 0x00000000		; Interworking glue start	-- Needs address fixing
	DCD 0x00000000		; Interworking glue end
	DCD 0x00000000		; GOT start					-- Needs address fixing
	DCD 0x00000000		; GOT end
	DCD 0x00000000		; bss start					-- Needs setting to zero
	DCD 0x00000000		; bss end

;---------------------------------------------------------------------------------
; IO_INTERFACE data -- 32 bytes
_io_dldi
	DCB "DLDI"					; ioType
	DCD 0x00000000				; Features
	DCD _DLDI_startup
	DCD _DLDI_isInserted
	DCD _DLDI_readSectors ;	Function pointers to standard device driver functions
	DCD _DLDI_writeSectors
	DCD _DLDI_clearStatus
	DCD _DLDI_shutdown
	
;---------------------------------------------------------------------------------

_DLDI_startup
_DLDI_isInserted
_DLDI_readSectors
_DLDI_writeSectors
_DLDI_clearStatus
_DLDI_shutdown
	mov		r0, #0x00				; Return false for every function
	bx		lr

;---------------------------------------------------------------------------------

    SPACE (32*1024)-136

	END
;---------------------------------------------------------------------------------
