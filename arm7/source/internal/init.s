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
	; r0 �q�[�v�x�[�X
	; r2 �q�[�v���~�b�g
	; r1 �X�^�b�N�x�[�X�i�X�^�b�N�̈�̍ŏ�ʃA�h���X�j
	; r3 �X�^�b�N���~�b�g�i�X�^�b�N�̈�̍ŉ��ʃA�h���X�j
	
	IMPORT bottom_of_heap ; defined in heap.s

	ldr r0,=bottom_of_heap
	ldr r2,=0x0380f300
	ldr r1,=0x0380f800
	ldr r3,=0 ; �X�^�b�N���~�b�g�`�F�b�N�͂��Ȃ�

	
	bx lr

    END
    
