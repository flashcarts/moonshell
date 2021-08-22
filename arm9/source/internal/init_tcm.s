
        AREA   INIT946TCM, CODE, READONLY   ; name this block of code

__itcm_start equ 0x01ff0000
__dtcm_start equ 0x02800000

PAGE_4K EQU 2_01011 << 1
PAGE_8K EQU 2_01100 << 1
PAGE_16K EQU 2_01101 << 1
PAGE_32K EQU 2_01110 << 1
PAGE_64K EQU 2_00111 << 1
PAGE_128K EQU 2_10000 << 1
PAGE_256K EQU 2_10001 << 1
PAGE_512K EQU 2_10010 << 1
PAGE_1M EQU 2_10011 << 1
PAGE_2M EQU 2_10100 << 1
PAGE_4M EQU 2_10101 << 1
PAGE_8M EQU 2_10110 << 1
PAGE_16M EQU 2_10111 << 1
PAGE_32M EQU 2_11000 << 1
PAGE_64M EQU 2_11001 << 1
PAGE_128M EQU 2_11010 << 1
PAGE_256M EQU 2_11011 << 1
PAGE_512M EQU 2_11100 << 1
PAGE_1G EQU 2_11101 << 1
PAGE_2G EQU 2_11110 << 1
PAGE_4G EQU 2_11111 << 1

ITCM_LOAD EQU 1<<19
ITCM_ENABLE EQU 1<<18
DTCM_LOAD EQU 1<<17
DTCM_ENABLE EQU 1<<16
DISABLE_TBIT EQU 1<<15
ROUND_ROBIT EQU 1<<14
ALT_VECTORS EQU 1<<13
ICACHE_ENABLE EQU 1<<12
BIG_ENDIAN EQU 1<<7
DCACHE_ENABLE EQU 1<<2
PROTECT_ENABLE EQU 1<<0

;---------------------------------------------------------------------------------
; _start
;---------------------------------------------------------------------------------
	ldr	r0, =0x04000208			; REG_IME = 0;
	mov r1, #0
	strh	r1, [r0]
	
  ; Clear DMA
  ldr r0,=0x40000B0
  mov r1,#0
  str r1,[r0]
  str r1,[r0]
  str r1,[r0]
  str r1,[r0]
  str r1,[r0]
  str r1,[r0]
  str r1,[r0]
  str r1,[r0]
  str r1,[r0]
  str r1,[r0]
  str r1,[r0]
  str r1,[r0]

;---------------------------------------------------------------------------------
; turn the power on for M3
;---------------------------------------------------------------------------------
	ldr	r0, =0x04000304 ; REG_POWER
	ldr r1, =0x8203 ; POWER_LCD(0) POWER_2D_A(1) POWER_2D_B(9) POWER_SWAP_LCDS(15)
	strh    r1, [r0]

	ldr	r1, =0x00002078			; disable TCM and protection unit
	mcr	p15, 0, r1, c1, c0, 0

;---------------------------------------------------------------------------------
; Protection Unit Setup added by Sasq
;---------------------------------------------------------------------------------
	; Disable cache
	mov	r0, #0
	mcr	p15, 0, r0, c7, c5, 0		; Instruction cache
	mcr	p15, 0, r0, c7, c6, 0		; Data cache

	; Wait for write buffer to empty 
	mcr	p15, 0, r0, c7, c10, 4

	ldr	r0, =(__dtcm_start | 0x0a)
	mcr	p15, 0, r0, c9, c1,0		; DTCM base = __dtcm_start, size = 16 KB

	ldr	r0, =(0x00000000 | 0x20) ; override set to 0x00000000 32MByte
	mcr	p15, 0, r0, c9, c1,1		; ITCM base = __itcm_start, size = 32 KB
	
;---------------------------------------------------------------------------------
; Setup memory regions similar to Release Version
;	this code currently breaks dualis
;---------------------------------------------------------------------------------

	;-------------------------------------------------------------------------
	; Region 0 - IO registers
	;-------------------------------------------------------------------------
	ldr	r0,=( PAGE_64M | 0x04000000 | 1)	
	mcr	p15, 0, r0, c6, c0, 0

	;-------------------------------------------------------------------------
	; Region 1 - Main Memory
	;-------------------------------------------------------------------------
	ldr	r0,=( PAGE_4M | 0x02000000 | 1)	
	mcr	p15, 0, r0, c6, c1, 0

	;-------------------------------------------------------------------------
	; Region 2 - iwram
	;-------------------------------------------------------------------------
	ldr	r0,=( PAGE_32K | 0x037F8000 | 1)	
	mcr	p15, 0, r0, c6, c2, 0

	;-------------------------------------------------------------------------
	; Region 3 - DS Accessory (GBA Cart)
	;-------------------------------------------------------------------------
	ldr	r0,=( PAGE_128M | 0x08000000 | 1)	
	mcr	p15, 0, r0, c6, c3, 0

	;-------------------------------------------------------------------------
	; Region 4 - DTCM
	;-------------------------------------------------------------------------
	ldr	r0,=(PAGE_16K | __dtcm_start | 1)
	mcr	p15, 0, r0, c6, c4, 0

	;-------------------------------------------------------------------------
	; Region 5 - ITCM
	;-------------------------------------------------------------------------
	ldr	r0,=( PAGE_32K | __itcm_start | 1)
	mcr	p15, 0, r0, c6, c5, 0

	;-------------------------------------------------------------------------
	; Region 6 - System ROM
	;-------------------------------------------------------------------------
	ldr	r0,=( PAGE_32K | 0xFFFF0000 | 1)	
	mcr	p15, 0, r0, c6, c6, 0

	;-------------------------------------------------------------------------
	; Region 7 - non cacheable main ram
	;-------------------------------------------------------------------------
	ldr	r0,=( PAGE_4M  | 0x02400000 | 1)	
	mcr	p15, 0, r0, c6, c7, 0

	;-------------------------------------------------------------------------
	; Write buffer enable
	;-------------------------------------------------------------------------
	ldr	r0,=2_00000110
	;ldr	r0,=2_00000111 ; region0 IO registers enabled.
	mcr	p15, 0, r0, c3, c0, 0

	;-------------------------------------------------------------------------
	; DCache & ICache enable
	;-------------------------------------------------------------------------
	ldr	r0,=2_01000010
	mcr	p15, 0, r0, c2, c0, 0
	mcr	p15, 0, r0, c2, c0, 1

	;-------------------------------------------------------------------------
	; IAccess
	;-------------------------------------------------------------------------
	;ldr	r0,=0x36636333
	ldr	r0,=0x36333333 ; region3,5 to RW
	mcr	p15, 0, r0, c5, c0, 3

	;-------------------------------------------------------------------------
	; DAccess
	;------------------------------------------------------------------------
	ldr	r0,=0x36333333
	mcr     p15, 0, r0, c5, c0, 2

	;-------------------------------------------------------------------------
	; Enable ICache, DCache, ITCM & DTCM
	;-------------------------------------------------------------------------
	mrc	p15, 0, r0, c1, c0, 0
	ldr r1,= ~(ITCM_ENABLE | DTCM_ENABLE | ICACHE_ENABLE | DCACHE_ENABLE | PROTECT_ENABLE)
	and r0,r0,r1
	
	ldr r1,=2_01111000 ; set SBO
	orr	r0,r0,r1
	
	;ldr	r1,= 0
	ldr	r1,= ITCM_ENABLE | DTCM_ENABLE | ICACHE_ENABLE | DCACHE_ENABLE | PROTECT_ENABLE
	;ldr	r1,= ITCM_ENABLE | DTCM_ENABLE | ICACHE_ENABLE | PROTECT_ENABLE
	;ldr	r1,= ITCM_ENABLE | DTCM_ENABLE
	orr	r0,r0,r1
	
	mcr	p15, 0, r0, c1, c0, 0
 
	mov	r0, #0x13		; Switch to SVC Mode
	msr	cpsr_cxsf, r0
	ldr	sp, =__dtcm_start+0x3fe0		; Set SVC stack

	mov	r0, #0x12		; Switch to IRQ Mode
	msr	cpsr_cxsf, r0
	ldr	sp, =__dtcm_start+0x3f80		; Set IRQ stack

	mov	r0, #0x1F		; Switch to System Mode
	msr	cpsr_cxsf, r0
	ldr	sp, =__dtcm_start+0x3f00		; Set user stack (not use this parametor. see init.s)
  
  bl CheckGUID
  
    IMPORT  __main                     ; import label to __main
    LDR pc,=__main                     ; branch to C Library entry 

	;-------------------------------------------------------------------------
	; GUID calculator
	;-------------------------------------------------------------------------
CheckGUID
    push {r4,r5,r6,r7,r8}
    
    ldr r4,=0x44495547 ; _pGUID ID
    ldr r5,=0xBF8DA5ED ; DLDI ID
    ldr r6,=0xBF8DA500 ; DisDLDI ID
    ldr r7,=(32*1024)-4 ; DLDI skip size
    
    ldr r0,=_pGUID
    ldr r1,[r0,#3*4] ; MemStartAddr
    ldr r2,[r0,#4*4] ; MemEndAddr
    mov r3,#0 ; chkxcrc
    
CheckGUID_Loop
    ldr r0,[r1],#4
    
    cmps r0,r4 ; skip _pGUID
    cmpsne r0,r5 ; skip DLDI
    cmpsne r0,r6 ; skip DisDLDI
    beq CheckGUID_Skip
    
    lsr r8,r3,#31
    orr r3,r8,r3,lsl #1
    eor r3,r3,r0
    
    cmps r1,r2
    bne CheckGUID_Loop
    
    ldr r0,=_pGUID
    str r3,[r0,#5*4] ; chkxcrc
    
    pop {r4,r5,r6,r7,r8}
    
    bx lr
    
CheckGUID_Skip
    cmps r0,r4 ; skip _pGUID
    addeq r1,#(6*4)-4
    beq CheckGUID_Loop
    
    cmps r0,r5 ; skip DLDI
    addeq r1,r7
    beq CheckGUID_Loop
    
    cmps r0,r6 ; skip DisDLDI
    addeq r1,r7
    beq CheckGUID_Loop
    
    EXPORT _pGUID
_pGUID
    ; GUID[6]=ID,XID,xcrc,MemStartAddr,MemEndAddr,chkxcrc
    DCD 0x44495547,0x47554944,0,0,0,0
    

	END
	
