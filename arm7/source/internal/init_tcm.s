
        AREA   INIT7TDMI, CODE, READONLY   ; name this block of code

;---------------------------------------------------------------------------------
_start
;---------------------------------------------------------------------------------
	ldr	r0, =0x04000208			; REG_IME = 0;
	mov r1, #0
	strh	r1, [r0]
  
	mov	r0, #0x13		; Switch to SVC Mode
	msr	cpsr_cxsf, r0
	ldr	sp, =0x0380f900		; Set SVC stack

	mov	r0, #0x12		; Switch to IRQ Mode
	msr	cpsr_cxsf, r0
	ldr	sp, =0x0380f880		; Set IRQ stack

	mov	r0, #0x1F		; Switch to System Mode
	msr	cpsr_cxsf, r0
	ldr	sp, =0x0380f800		; Set user stack (not use this parametor. see init.s)
        
    IMPORT  __main                     ; import label to __main
    LDR pc,=__main                     ; branch to C Library entry 
	
	END
	
