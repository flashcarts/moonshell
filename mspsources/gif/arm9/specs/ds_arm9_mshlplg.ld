OUTPUT_FORMAT("elf32-littlearm", "elf32-bigarm", "elf32-littlearm")
OUTPUT_ARCH(arm)
ENTRY(_start)

MEMORY {
	dlram	: ORIGIN = 0x5e900000, LENGTH = 4M - 4k
	dtcm	: ORIGIN = 0x00800000, LENGTH = 16K
}

__dtcm_start	=	0x00800000;
__dtcm_end		=	0x00804000;

SECTIONS
{
	.init	:
	{
		__text_start = . ;
		KEEP (*(.init))
		. = ALIGN(4);  /* REQUIRED. LD is flaky without it. */
	} >dlram = 0xff

	.text :   /* ALIGN (4): */
	{
		*(.text.*)
		*(.stub)
		/* .gnu.warning sections are handled specially by elf32.em.  */
		*(.gnu.warning)
		*(.gnu.linkonce.t*)
		*(.glue_7)
		*(.glue_7t)
		. = ALIGN(4);  /* REQUIRED. LD is flaky without it. */
	} >dlram = 0xff

	.fini           :
	{
		KEEP (*(.fini))
	} >dlram =0xff

	.rodata :
	{
		*(.rodata)
		*all.rodata*(*)
		*(.roda)
		*(.rodata.*)
		*(.gnu.linkonce.r*)
		SORT(CONSTRUCTORS)
		. = ALIGN(4);   /* REQUIRED. LD is flaky without it. */
	} >dlram = 0xff

	.ctors :
	{
	/* gcc uses crtbegin.o to find the start of the constructors, so
		we make sure it is first.  Because this is a wildcard, it
		doesn't matter if the user does not actually link against
		crtbegin.o; the linker won't look for a file to match a
		wildcard.  The wildcard also means that it doesn't matter which
		directory crtbegin.o is in.  */
		KEEP (*crtbegin.o(.ctors))
		KEEP (*(EXCLUDE_FILE (*crtend.o) .ctors))
		KEEP (*(SORT(.ctors.*)))
		KEEP (*(.ctors))
		. = ALIGN(4);   /* REQUIRED. LD is flaky without it. */
	} >dlram = 0xff

	.dtors :
	{
		KEEP (*crtbegin.o(.dtors))
		KEEP (*(EXCLUDE_FILE (*crtend.o) .dtors))
		KEEP (*(SORT(.dtors.*)))
		KEEP (*(.dtors))
		. = ALIGN(4);   /* REQUIRED. LD is flaky without it. */
	} >dlram = 0xff

	.eh_frame :
	{
		KEEP (*(.eh_frame))
		. = ALIGN(4);   /* REQUIRED. LD is flaky without it. */
	} >dlram = 0xff

	.gcc_except_table :
	{
		*(.gcc_except_table)
		. = ALIGN(4);   /* REQUIRED. LD is flaky without it. */
	} >dlram = 0xff
	.jcr            : { KEEP (*(.jcr)) } >dlram = 0
	
	__got_start = . ;
	.got            : { *(.got.plt) *(.got) } >dlram = 0
	__got_end = . ;


	.data ALIGN(4) :
	{
		__data_start = ABSOLUTE(.);
		*(.data)
		*(.data.*)
		*(.gnu.linkonce.d*)
		CONSTRUCTORS
		. = ALIGN(4);
	} >dlram = 0xff

	__data_end = . ;

	
	__bss_lma = . ;
	.bss __bss_lma : AT (__bss_lma)
	{
		__bss_start = ABSOLUTE(.);
		__bss_start__ = ABSOLUTE(.);
		*(.dynbss)
		*(.gnu.linkonce.b*)
		*(COMMON)
		. = ALIGN(4);    /* REQUIRED. LD is flaky without it. */
	__bss_end = ABSOLUTE(.) ;
	__bss_end__ = ABSOLUTE(.) ;
	} >dlram

	_end = . ;
	__end__ = . ;

	__dtcm_lma = . ;

	.dtcm __dtcm_start : AT (__dtcm_lma)
	{
		__dtcm_start = ABSOLUTE(.);
		*(.dtcm)
		*(.dtcm.*)
		. = ALIGN(4);
	} >dtcm = 0xff
	__dtcm_end = . ;



}
