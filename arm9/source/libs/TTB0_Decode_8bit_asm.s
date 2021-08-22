
    AREA     globals,CODE,READONLY

  EXPORT TTB0_Decode_8bit_asm
TTB0_Decode_8bit_asm

;// for 8bit
Stage2_BitShift EQU 4
HybridFilter_Shift EQU 10

  MACRO
  HybridFilter_MUL $p0,$p1,$p2,$p3
  smulbb $p0,$p1,$p2,$p3
  MEND
  
  MACRO
  HybridFilter_MLA $p0,$p1,$p2,$p3
  smlabb $p0,$p1,$p2,$p3
  MEND
  
  MACRO
  StoreSampleBuffer $reg_src,$reg_dst
  strb $reg_src,[$reg_dst],#1
  MEND

;//  bkpt 0 // デバッグ用ブレークポイント設定
  
  push {r4,r5,r6,r7,r8,r9,r10,r11,r12,lr}
  
StackSize EQU (37+1)*4

  sub sp,#StackSize
  
  MACRO
  SR_bit_pos $reg
  ldr $reg,[sp,#0*4]
  MEND
  MACRO
  SW_bit_pos $reg
  str $reg,[sp,#0*4]
  MEND
  MACRO
  SR_bit_count $reg
  ldr $reg,[sp,#1*4]
  MEND
  MACRO
  SW_bit_count $reg
  str $reg,[sp,#1*4]
  MEND
  MACRO
  SR_bit_cache $reg
  ldr $reg,[sp,#2*4]
  MEND
  MACRO
  SW_bit_cache $reg
  str $reg,[sp,#2*4]
  MEND
  MACRO
  SR_bit_pbit_mask $reg
  ldr $reg,[sp,#3*4]
  MEND
  MACRO
  SW_bit_pbit_mask $reg
  str $reg,[sp,#3*4]
  MEND

  MACRO
  SR_bit_struct $reg_pos,$reg_count,$reg_cache,$reg_pbit_mask
  ldmia sp,{$reg_pos,$reg_count,$reg_cache,$reg_pbit_mask}
  MEND
  MACRO
  SW_bit_struct $reg_pos,$reg_count,$reg_cache
  stmia sp,{$reg_pos,$reg_count,$reg_cache}
  MEND

  MACRO
  SR_tta_last $reg
  ldr $reg,[sp,#4*4]
  MEND
  MACRO
  SW_tta_last $reg
  str $reg,[sp,#4*4]
  MEND

  MACRO
  SR_tta_rice_k0 $reg
  ldr $reg,[sp,#5*4]
  MEND
  MACRO
  SW_tta_rice_k0 $reg
  str $reg,[sp,#5*4]
  MEND
  MACRO
  SR_tta_rice_k1 $reg
  ldr $reg,[sp,#6*4]
  MEND
  MACRO
  SW_tta_rice_k1 $reg
  str $reg,[sp,#6*4]
  MEND
  MACRO
  SR_tta_rice_sum0 $reg
  ldr $reg,[sp,#7*4]
  MEND
  MACRO
  SW_tta_rice_sum0 $reg
  str $reg,[sp,#7*4]
  MEND
  MACRO
  SR_tta_rice_sum1 $reg
  ldr $reg,[sp,#8*4]
  MEND
  MACRO
  SW_tta_rice_sum1 $reg
  str $reg,[sp,#8*4]
  MEND

  MACRO
  StackGetPointer_tta_struct $reg
  add $reg,sp,#5*4
  MEND
  MACRO
  SR_tta_struct $reg_ptr,$reg_k0,$reg_k1,$reg_sum0,$reg_sum1
  ldmia $reg_ptr,{$reg_k0,$reg_k1,$reg_sum0,$reg_sum1}
  MEND
  MACRO
  SW_tta_struct $reg_ptr,$reg_k0,$reg_k1,$reg_sum0,$reg_sum1
  stmia $reg_ptr,{$reg_k0,$reg_k1,$reg_sum0,$reg_sum1}
  MEND

  MACRO
  SR_fltst_error $reg
  ldr $reg,[sp,#9*4]
  MEND
  MACRO
  SW_fltst_error $reg
  str $reg,[sp,#9*4]
  MEND
  MACRO
  StackGetPointer_fltst_pA $reg
  add $reg,sp,#(9+1)*4
  MEND
  MACRO
  StackGetPointer_fltst_pB $reg
  add $reg,sp,#(9+1+9)*4
  MEND
  MACRO
  StackGetPointer_fltst_pM $reg
  add $reg,sp,#(9+1+9+9)*4
  MEND

  MACRO
  SR_buf $reg
  ldr $reg,[sp,#37*4]
  MEND
  MACRO
  SW_buf $reg
  str $reg,[sp,#37*4]
  MEND

  mov r4,#0
  mov r5,#0
  mov r6,#0
  mov r7,#0
  mov r8,#0
  mov r9,#0
  mov r10,#0
  mov r11,#0
  mov r12,#0
  
  SW_bit_struct r0,r4,r5
  
  ldr r14,=array_bit_mask
  SW_bit_pbit_mask r14
  
  SW_buf r1
  
  SW_fltst_error r4
  
  StackGetPointer_fltst_pA r3
  stmia r3,{r4,r5,r6,r7,r8,r9,r10,r11,r12}
  StackGetPointer_fltst_pB r3
  stmia r3,{r4,r5,r6,r7,r8,r9,r10,r11,r12}
  StackGetPointer_fltst_pM r3
  stmia r3,{r4,r5,r6,r7,r8,r9,r10,r11,r12}
  
  SW_tta_last r4
  
  mov r4,#10
  mov r5,r4
  ldr r6,=array_shift_16
  ldr r6,[r6,r4,lsl #2]
  mov r7,r6
  
  StackGetPointer_tta_struct r8
  SW_tta_struct r8,r4,r5,r6,r7
  
REG_DecompressedSamplesCount RN r12

  mov REG_DecompressedSamplesCount,r2
  
Label_loop_start
  
REG_value RN r14

;  // -----------------------------------------------------
;  // bit operation start
  
REG_bit_pos RN r8
REG_bit_count RN r9
REG_bit_cache RN r10
REG_pbit_mask RN r11
  
  SR_bit_struct REG_bit_pos,REG_bit_count,REG_bit_cache,REG_pbit_mask
  
;  // -----------------------------------------------------
;  // unsigned int unary;
;  // GET_UNARY(unary);
  
REG_unary RN r0

  mov REG_unary,#0
  
  cmps REG_bit_cache,#0
  bne Label_GET_UNARY_loop1_end
  
Label_GET_UNARY_loop1_start
  
  add REG_unary,REG_bit_count
  ldrh REG_bit_cache,[REG_bit_pos],#2
  mov REG_bit_count,#16
  
  cmps REG_bit_cache,#0
  beq Label_GET_UNARY_loop1_start
  
Label_GET_UNARY_loop1_end
  
  tsts REG_bit_cache,#1
  bne Label_GET_UNARY_loop2_end
  
Label_GET_UNARY_loop2_start
  
  add REG_unary,#1
  lsr REG_bit_cache,#1
  sub REG_bit_count,#1
  
  tsts REG_bit_cache,#1
  beq Label_GET_UNARY_loop2_start
  
Label_GET_UNARY_loop2_end
  
  lsr REG_bit_cache,#1
  sub REG_bit_count,#1
  
;//  SW_unary REG_unary
  
;//#undef REG_unary

;  // ------------------------------------------------------
;  //  if(unary==0) {
;  //    depth = false; k = tta_rice_k0;
;  //    }else{
;  //    depth = true; k = tta_rice_k1;
;  //    unary--;
;  //  }
;  //  
;  //  if (k!=0) {
;  //    unsigned int binary;
;  //    GET_BINARY(binary, k);
;  //    value = (unary << k) + binary;
;  //    }else{
;  //    value = unary;
;  //  }
  
;//#define REG_unary r0
REG_depth RN r1
REG_k RN r2
REG_tta_rice_k0 RN r3
REG_tta_rice_k1 RN r4

;//  SR_unary REG_unary
;//  SR_depth REG_depth
;//  SR_k REG_k
  SR_tta_rice_k0 REG_tta_rice_k0
  SR_tta_rice_k1 REG_tta_rice_k1
;//  SR_value REG_value
  
  cmps REG_unary,#0
  moveq REG_depth,#0
  moveq REG_k,REG_tta_rice_k0
  movne REG_depth,#1
  movne REG_k,REG_tta_rice_k1
  subne REG_unary,#1
  
  cmps REG_k,#0
  moveq REG_value,REG_unary
  beq Label_kend
  
;  // ------------------------------------------------------
;  //    unsigned int binary;
;  //    GET_BINARY(binary, k);
  
REG_binary_binary RN r5
REG_binary_tmp RN r6
  
  cmps REG_bit_count,REG_k
  bhs Label_GET_BINARY_end
  
Label_GET_BINARY_start
  
  ldrh REG_binary_tmp,[REG_bit_pos],#2
  orr REG_bit_cache,REG_bit_cache,REG_binary_tmp,lsl REG_bit_count
  add REG_bit_count,#16
  
  cmps REG_bit_count,REG_k
  blo Label_GET_BINARY_start
  
Label_GET_BINARY_end
  
  ldr REG_binary_tmp,[REG_pbit_mask,REG_k,lsl #2]
  sub REG_bit_count,REG_k
  and REG_binary_binary,REG_bit_cache,REG_binary_tmp
  lsr REG_bit_cache,REG_k
  
  add REG_value,REG_binary_binary,REG_unary,lsl REG_k
  
;#undef REG_binary_binary
;#undef REG_binary_tmp
  
;  // ------------------------------------------------------
Label_kend
  
;//  SW_unary REG_unary
;//  SW_depth REG_depth
;//  SW_k REG_k
;//  SW_tta_rice_k0 REG_tta_rice_k0
;//  SW_tta_rice_k1 REG_tta_rice_k1
;//  SW_value REG_value

;#undef REG_unary
;//#undef REG_depth
;#undef REG_k
;//#undef REG_tta_rice_k0
;//#undef REG_tta_rice_k1
;//#undef REG_value
  
;  // -----------------------------------------------------
;  // bit operation end
  
  SW_bit_struct REG_bit_pos,REG_bit_count,REG_bit_cache
  
;#undef REG_bit_pos
;#undef REG_bit_count
;#undef REG_bit_cache
;#undef REG_pbit_mask

;  // -----------------------------------------------------
;  //  if(depth==true){
;  //    tta_rice_sum1 += value - (tta_rice_sum1 >> 4);
;  //    if (tta_rice_k1 > 0 && tta_rice_sum1 < shift_16[tta_rice_k1]){
;  //      tta_rice_k1--;
;  //      }else{
;  //      if (tta_rice_sum1 > shift_16[tta_rice_k1 + 1]) tta_rice_k1++;
;  //    }
;  //    value += bit_shift[tta_rice_k0];
;  //  }
;  //  
;  //  {
;  //    tta_rice_sum0 += value - (tta_rice_sum0 >> 4);
;  //    if (tta_rice_k0 > 0 && tta_rice_sum0 < shift_16[tta_rice_k0]){
;  //      tta_rice_k0--;
;  //      }else{
;  //      if (tta_rice_sum0 > shift_16[tta_rice_k0 + 1]) tta_rice_k0++;
;  //    }
;  //  }
  
;//#define REG_depth r1
;//#define REG_value r14
;//#define REG_tta_rice_k0 r3
;//#define REG_tta_rice_k1 r4
REG_tta_rice_sum0 RN r5
REG_tta_rice_sum1 RN r6
REG_tta_struct_pointer RN r9
REG_pshift_16 RN r0
REG_pbit_shift RN r2
REG2_tmp RN r7
REG_TableLoadedK0 RN r8
  
;//  SR_depth REG_depth
;//  SR_value REG_value
;//  SR_tta_rice_k0 REG_tta_rice_k0
;//  SR_tta_rice_k1 REG_tta_rice_k1
  SR_tta_rice_sum0 REG_tta_rice_sum0
  SR_tta_rice_sum1 REG_tta_rice_sum1
  ldr REG_pshift_16,=array_shift_16

  cmps REG_depth,#0
  beq Label_skipdepth
  
  ldr REG_pbit_shift,=array_bit_shift
  
  sub REG2_tmp,REG_value,REG_tta_rice_sum1,lsr #4
  add REG_tta_rice_sum1,REG2_tmp
  
  ldr REG_TableLoadedK0,[REG_pbit_shift,REG_tta_rice_k0,lsl #2]
  
  ldr REG2_tmp,[REG_pshift_16,REG_tta_rice_k1,lsl #2]
  cmps REG_tta_rice_k1,#0
  cmphis REG2_tmp,REG_tta_rice_sum1
  
  subhi REG_tta_rice_k1,#1
  bhi Label_depth_if1skip
  
  add REG2_tmp,REG_tta_rice_k1,#1
  ldr REG2_tmp,[REG_pshift_16,REG2_tmp,lsl #2]
  cmps REG_tta_rice_sum1,REG2_tmp
  addhi REG_tta_rice_k1,#1
  
Label_depth_if1skip
  
  add REG_value,REG_TableLoadedK0
  
Label_skipdepth
  
;  // ---------------------------
  
  sub REG2_tmp,REG_value,REG_tta_rice_sum0,lsr #4
  add REG_tta_rice_sum0,REG2_tmp
  
  ldr REG2_tmp,[REG_pshift_16,REG_tta_rice_k0,lsl #2]
  cmps REG_tta_rice_k0,#0
  cmphis REG2_tmp,REG_tta_rice_sum0
  
  subhi REG_tta_rice_k0,#1
  bhi Label_depth_if2skip
  
  add REG2_tmp,REG_tta_rice_k0,#1
  ldr REG2_tmp,[REG_pshift_16,REG2_tmp,lsl #2]
  cmps REG_tta_rice_sum0,REG2_tmp
  addhi REG_tta_rice_k0,#1
  
Label_depth_if2skip
  
;//  SW_depth REG_depth
;//  SW_value REG_value
  
;//  SW_tta_rice_k0 REG_tta_rice_k0
;//  SW_tta_rice_k1 REG_tta_rice_k1
;//  SW_tta_rice_sum0 REG_tta_rice_sum0
;//  SW_tta_rice_sum1 REG_tta_rice_sum1
  
  StackGetPointer_tta_struct REG_tta_struct_pointer
  SW_tta_struct REG_tta_struct_pointer,REG_tta_rice_k0,REG_tta_rice_k1,REG_tta_rice_sum0,REG_tta_rice_sum1
  
;#undef REG_depth
;//#undef REG_value
;#undef REG_tta_rice_k0
;#undef REG_tta_rice_k1
;#undef REG_tta_rice_sum0
;#undef REG_tta_rice_sum1
;#undef REG_tta_struct_pointer
;#undef REG_pshift_16
;#undef REG_pbit_shift
;#undef REG2_tmp

;  // -----------------------------------------------------
;  // value=(value&1) ? (value+1>>1) : (-value>>1)
  
;//#define REG_value r14
REG3_tmp RN r2
  
;//  SR_value REG_value
  
  tsts REG_value,#1
  
  addne REG_value,#1
  
  moveq REG3_tmp,#0
  subeq REG_value,REG3_tmp,REG_value
  
  asr REG_value,#1
  
;//  SW_value REG_value
  
;//#undef REG_value
;#undef REG3_tmp

;  // -----------------------------------------------------
;  // HybridFilter
  
;  // use temp r4,r5,r6,r7,r8,r9,r10,r11
  
REG_sum RN r0
REG_pA RN r1
REG_pB RN r2
REG_pM RN r3
  
  StackGetPointer_fltst_pA REG_pA
  StackGetPointer_fltst_pB REG_pB
  StackGetPointer_fltst_pM REG_pM
  
  mov REG_sum,#(1 << (HybridFilter_Shift - 1))
  
  SR_fltst_error r4
  cmps r4,#0
  
;  // --------------------------------
Label_HybridFilter_ErrorZero
  
  bne Label_HybridFilter_ErrorLow
  
  ldmia REG_pA!,{r4,r5,r6,r7}
  ldmia REG_pB!,{r8,r9,r10,r11}
  
  HybridFilter_MLA REG_sum,r4,r8,REG_sum
  HybridFilter_MLA REG_sum,r5,r9,REG_sum
  HybridFilter_MLA REG_sum,r6,r10,REG_sum
  HybridFilter_MLA REG_sum,r7,r11,REG_sum
  
  ldmia REG_pA!,{r4,r5,r6,r7}
  ldmia REG_pB!,{r8,r9,r10,r11}
  
  HybridFilter_MLA REG_sum,r4,r8,REG_sum
  HybridFilter_MLA REG_sum,r5,r9,REG_sum
  HybridFilter_MLA REG_sum,r6,r10,REG_sum
  HybridFilter_MLA REG_sum,r7,r11,REG_sum
  
  add REG_pM,#8*4
  
  b Label_HybridFilter_ErrorEnd
  
;  // --------------------------------
Label_HybridFilter_ErrorLow
  
  bgt Label_HybridFilter_ErrorHigh
  
  ldmia REG_pB,{r8,r9,r10,r11}
  ldmia REG_pM!,{r4,r5,r6,r7}
  
  sub r8,r4
  sub r9,r5
  sub r10,r6
  sub r11,r7
  
  stmia REG_pB!,{r8,r9,r10,r11}
  
  ldmia REG_pA!,{r4,r5,r6,r7}
  
  HybridFilter_MLA REG_sum,r4,r8,REG_sum
  HybridFilter_MLA REG_sum,r5,r9,REG_sum
  HybridFilter_MLA REG_sum,r6,r10,REG_sum
  HybridFilter_MLA REG_sum,r7,r11,REG_sum
  
  ldmia REG_pB,{r8,r9,r10,r11}
  ldmia REG_pM!,{r4,r5,r6,r7}
  
  sub r8,r4
  sub r9,r5
  sub r10,r6
  sub r11,r7
  
  stmia REG_pB!,{r8,r9,r10,r11}
  
  ldmia REG_pA!,{r4,r5,r6,r7}
  
  HybridFilter_MLA REG_sum,r4,r8,REG_sum
  HybridFilter_MLA REG_sum,r5,r9,REG_sum
  HybridFilter_MLA REG_sum,r6,r10,REG_sum
  HybridFilter_MLA REG_sum,r7,r11,REG_sum
  
  b Label_HybridFilter_ErrorEnd
  
;  // --------------------------------
Label_HybridFilter_ErrorHigh
  
  ldmia REG_pB,{r8,r9,r10,r11}
  ldmia REG_pM!,{r4,r5,r6,r7}
  
  add r8,r4
  add r9,r5
  add r10,r6
  add r11,r7
  
  stmia REG_pB!,{r8,r9,r10,r11}
  
  ldmia REG_pA!,{r4,r5,r6,r7}
  
  HybridFilter_MLA REG_sum,r4,r8,REG_sum
  HybridFilter_MLA REG_sum,r5,r9,REG_sum
  HybridFilter_MLA REG_sum,r6,r10,REG_sum
  HybridFilter_MLA REG_sum,r7,r11,REG_sum
  
  ldmia REG_pB,{r8,r9,r10,r11}
  ldmia REG_pM!,{r4,r5,r6,r7}
  
  add r8,r4
  add r9,r5
  add r10,r6
  add r11,r7
  
  stmia REG_pB!,{r8,r9,r10,r11}
  
  ldmia REG_pA!,{r4,r5,r6,r7}
  
  HybridFilter_MLA REG_sum,r4,r8,REG_sum
  HybridFilter_MLA REG_sum,r5,r9,REG_sum
  HybridFilter_MLA REG_sum,r6,r10,REG_sum
  HybridFilter_MLA REG_sum,r7,r11,REG_sum
  
;  // --------------------------------
Label_HybridFilter_ErrorEnd
  
  sub REG_pA,#4*4
  ldmia REG_pA!,{r8,r9,r10,r11}
  
  asr r8,#30
  orr r8,#1
  
  ldr r4,=array_HybridFilter_signedbytetable
  lsr r9,#30
  ldrsb r9,[r4,r9]
  
  lsr r10,#30
  ldrsb r10,[r4,r10]
  
  add r4,#4
  lsr r11,#30
  ldrsb r11,[r4,r11]
  
  sub REG_pM,#7*4
  ldmia REG_pM,{r4,r5,r6,r7}
  sub REG_pM,#1*4
  stmia REG_pM,{r4,r5,r6,r7,r8,r9,r10,r11}
  
;//  SR_value REG_value
  SW_fltst_error REG_value
  
  sub REG_pA,#(8-1)*4
  ldmia REG_pA,{r4,r5,r6,r7,r8,r9,r10}
  
  add REG_value,REG_value,REG_sum,asr #HybridFilter_Shift
;//  SW_value REG_value
  
  sub r10,REG_value,r10
  sub r9,r10,r9
  sub r8,r9,r8
  
  sub REG_pA,#1*4
  stmia REG_pA!,{r4,r5,r6,r7,r8,r9,r10,REG_value}
  
;#undef REG_sum
;#undef REG_pA
;#undef REG_pB
;#undef REG_pM
  
;  // HybridFilter End
;  // -----------------------------------------------------
  
;  // -----------------------------------------------------
;  //  tta_last = value + ((tta_last << 5) - tta_last) >> 5; // 5 == bps 16
;  //  *buf++ = tta_value;
  
;//#define REG_value r14
REG_tta_last RN r2
REG_buf RN r3
REG4_tmp RN r4
  
;//  SR_value REG_value
  SR_tta_last REG_tta_last
  SR_buf REG_buf
  
  lsl REG4_tmp,REG_tta_last,#Stage2_BitShift
  sub REG4_tmp,REG4_tmp,REG_tta_last
  add REG_tta_last,REG_value,REG4_tmp,asr #Stage2_BitShift
  
  StoreSampleBuffer REG_tta_last,REG_buf
  
  SW_tta_last REG_tta_last
  SW_buf REG_buf
  
;//#undef REG_value
;#undef REG_tta_last
;#undef REG_buf
;#undef REG4_tmp

;  // -----------------------------------------------------
  
  subs REG_DecompressedSamplesCount,#1
  
;#undef REG_value

  bne Label_loop_start
  
;#undef REG_DecompressedSamplesCount

;  // -----------------------------------------------------
Label_loop_end
  
;//  bkpt 0 // デバッグ用ブレークポイント設定
  
  add sp,#StackSize
  
  pop {r4,r5,r6,r7,r8,r9,r10,r11,r12,pc}
  
array_bit_mask
  DCD 0x00000000, 0x00000001, 0x00000003, 0x00000007
  DCD 0x0000000f, 0x0000001f, 0x0000003f, 0x0000007f
  DCD 0x000000ff, 0x000001ff, 0x000003ff, 0x000007ff
  DCD 0x00000fff, 0x00001fff, 0x00003fff, 0x00007fff
  DCD 0x0000ffff, 0x0001ffff, 0x0003ffff, 0x0007ffff
  DCD 0x000fffff, 0x001fffff, 0x003fffff, 0x007fffff
  DCD 0x00ffffff, 0x01ffffff, 0x03ffffff, 0x07ffffff
  DCD 0x0fffffff, 0x1fffffff, 0x3fffffff, 0x7fffffff
  DCD 0xffffffff
  
array_bit_shift
  DCD 0x00000001, 0x00000002, 0x00000004, 0x00000008
array_shift_16
  DCD 0x00000010, 0x00000020, 0x00000040, 0x00000080
  DCD 0x00000100, 0x00000200, 0x00000400, 0x00000800
  DCD 0x00001000, 0x00002000, 0x00004000, 0x00008000
  DCD 0x00010000, 0x00020000, 0x00040000, 0x00080000
  DCD 0x00100000, 0x00200000, 0x00400000, 0x00800000
  DCD 0x01000000, 0x02000000, 0x04000000, 0x08000000
  DCD 0x10000000, 0x20000000, 0x40000000, 0x80000000
  DCD 0x80000000, 0x80000000, 0x80000000, 0x80000000
  DCD 0x80000000, 0x80000000, 0x80000000, 0x80000000

array_HybridFilter_signedbytetable
  DCB 0x02,0x02,0xfe,0xfe
  DCB 0x04,0x04,0xfc,0xfc
  
  END
