    IMPORT __use_no_semihosting
    IMPORT __use_two_region_memory
    
    EXPORT _sys_exit
    EXPORT _ttywrch
    EXPORT __user_initial_stackheap

    AREA     globals,CODE,READONLY
    
_sys_exit
	b {pc}
	
_ttywrch
	bx lr
	
__user_initial_stackheap
	; r0 ヒープベース
	; r2 ヒープリミット
	; r1 スタックベース（スタック領域の最上位アドレス）
	; r3 スタックリミット（スタック領域の最下位アドレス）
	
	IMPORT bottom_of_heap ; defined in heap.s

	ldr r0,=bottom_of_heap
	ldr r2,=0x0380f300
	ldr r1,=0x0380f800
	ldr r3,=0 ; スタックリミットチェックはしない

	
	bx lr

    END
    
