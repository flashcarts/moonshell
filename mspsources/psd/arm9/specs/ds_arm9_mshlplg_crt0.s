
@---------------------------------------------------------------------------------
	.section ".init"
@---------------------------------------------------------------------------------
	.global _start
	.align	4
	.arm
@---------------------------------------------------------------------------------
	.word   0x0050534d @ 'MSP\0' ID
	.byte   0x04 @ Version high
	.byte   0x00 @ Version low
	.byte   0x01 @ PluginMode 0=Null 1=Image 2=Sound
	.byte   0x00 @ dummy
	.word   __text_start @ data start
	.word   __bss_lma    @ data end
	.word   __got_start  @ got start
	.word   __got_end    @ got end
	.word   __bss_lma    @ bss start
	.word   _end         @ bss end
	.word   LoadLibrary
	.word   FreeLibrary
	.word   QueryInterfaceLibrary
	.word   0 @ dummy
@ offset 48 Support Format (ext)
	.ascii  "psd\0"
	.ascii  "\0\0\0\0"
	.ascii  "\0\0\0\0"
	.ascii  "\0\0\0\0"
	
	.ascii  "\0\0\0\0"
	.ascii  "\0\0\0\0"
	.ascii  "\0\0\0\0"
	.ascii  "\0\0\0\0"
	.ascii  "\0\0\0\0"
	.ascii  "\0\0\0\0"
	.ascii  "\0\0\0\0"
	.ascii  "\0\0\0\0"
	.ascii  "\0\0\0\0"
	.ascii  "\0\0\0\0"
	.ascii  "\0\0\0\0"
	.ascii  "\0\0\0\0"
@ offset 112 (PluginName + PluginAuthor = under 144byte)
	.asciz  "Adobe Photoshop 2.5 PSD Plugin"
	.asciz  "Moonlight"
	.asciz  ""__DATE__" "__TIME__" GMT+09:00"
@ offset < 256byte
@---------------------------------------------------------------------------------
_start:
@---------------------------------------------------------------------------------
@---------------------------------------------------------------------------------
	.align
	.pool
	.end
@---------------------------------------------------------------------------------
