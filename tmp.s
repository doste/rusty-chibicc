  .globl _start
_start:
stp x19, x30, [sp, #-16]!
  mov x0, #1
  mov x0, #2
  mov x0, #3
ldp x19, x30, [sp], #16
  ret
