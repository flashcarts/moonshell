    IMPORT __use_no_semihosting
    IMPORT __use_two_region_memory
    
    EXPORT _sys_exit
;    EXPORT _ttywrch
    EXPORT __user_initial_stackheap

    AREA     globals,CODE,READONLY
    
    
_sys_exit
	b {pc}
	
_ttywrch
	bx lr
	
__user_initial_stackheap
	; r0 ヒープベース
	; r1 スタックベース（スタック領域の最上位アドレス）
	; r2 ヒープリミット
	; r3 スタックリミット（スタック領域の最下位アドレス）
	
	IMPORT bottom_of_heap ; defined in heap.s

	ldr r0,=bottom_of_heap
	ldr r2,=(0x02400000-(16*1024)) ; ヒープリミットをメインメモリから16kbyte残したアドレスに再設定
	ldr r1,=0x02803f00
	ldr r3,=0x02800000 ; スタックリミットチェックはしない
	
	bx lr

    END
    
