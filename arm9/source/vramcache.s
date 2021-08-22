;//---------------------------------------------------------------------------------
    AREA     globals,CODE,READONLY
;//---------------------------------------------------------------------------------

  EXPORT VRAMWriteCache_Enable
VRAMWriteCache_Enable
  mov r0,#0x07 ;// =0b00000111
  mcr p15, 0, r0, c3, c0, 0
  bx lr

;//---------------------------------------------------------------------------------

  EXPORT VRAMWriteCache_Disable
VRAMWriteCache_Disable
  mov r0,#0x06 ;// =0b00000110
  mcr p15, 0, r0, c3, c0, 0
  bx lr

  END