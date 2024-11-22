  .globl _start
_start:
stp x19, x30, [sp, #-16]!
  mov x0, #10
  NEG x0, x0
  NEG x0, x0
ldp x19, x30, [sp], #16
  ret
