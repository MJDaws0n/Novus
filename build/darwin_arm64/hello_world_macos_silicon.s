.section __DATA,__data
.p2align 3
__str_0:
    .asciz "\n"
__str_0_len:
    .quad 1
.p2align 3
__str_1:
    .asciz "Hello, World!"
__str_1_len:
    .quad 13
.p2align 3
__str_2:
    .asciz "Goodbye, World!"
__str_2_len:
    .quad 15

.lcomm __novus_strcat_buf, 4096

.section __TEXT,__text
.globl _main
.p2align 2

_len:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #128
    // function len
    stur x0, [x29, #-8]
    mov x10, #0
    stur x10, [x29, #-16]
    mov x10, #0
    stur x10, [x29, #-24]
.Lwhile_cond_0:
    ldur x10, [x29, #-32]
    ldur x10, [x29, #-8]
    stur x10, [x29, #-32]
    ldur x10, [x29, #-40]
    ldur x10, [x29, #-24]
    stur x10, [x29, #-40]
    ldur x13, [x29, #-32]
    ldur x14, [x29, #-40]
    ldur x10, [x29, #-48]
    ldrb w15, [x13, x14]
    uxtb x10, w15
    stur x10, [x29, #-48]
    ldur x10, [x29, #-56]
    ldur x11, [x29, #-48]
    mov x12, #0
    cmp x11, x12
    cset x10, ne
    stur x10, [x29, #-56]
    ldur x10, [x29, #-56]
    cbz x10, .Lwhile_end_2
.Lwhile_body_1:
    ldur x10, [x29, #-64]
    ldur x10, [x29, #-16]
    stur x10, [x29, #-64]
    ldur x10, [x29, #-72]
    ldur x11, [x29, #-64]
    mov x12, #1
    add x10, x11, x12
    stur x10, [x29, #-72]
    ldur x10, [x29, #-72]
    stur x10, [x29, #-16]
    ldur x10, [x29, #-80]
    ldur x10, [x29, #-24]
    stur x10, [x29, #-80]
    ldur x10, [x29, #-88]
    ldur x11, [x29, #-80]
    mov x12, #1
    add x10, x11, x12
    stur x10, [x29, #-88]
    ldur x10, [x29, #-88]
    stur x10, [x29, #-24]
    b .Lwhile_cond_0
.Lwhile_end_2:
    ldur x10, [x29, #-96]
    ldur x10, [x29, #-16]
    stur x10, [x29, #-96]
    ldur x10, [x29, #-96]
    mov x0, x10
    add sp, sp, #128
    ldp x29, x30, [sp], #16
    ret

_print:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #80
    // function print
    stur x0, [x29, #-8]
    ldur x10, [x29, #-16]
    ldur x10, [x29, #-8]
    stur x10, [x29, #-16]
    ldur x13, [x29, #-16]
    adrp x14, __str_0@PAGE
    add x14, x14, __str_0@PAGEOFF
    ldur x10, [x29, #-24]
    adrp x15, __novus_strcat_buf@PAGE
    add x15, x15, __novus_strcat_buf@PAGEOFF
    mov x11, x15
    mov x12, x15
.Lsc1_1:
    ldrb w16, [x13], #1
    cbz w16, .Lsc2_1
    strb w16, [x11], #1
    b .Lsc1_1
.Lsc2_1:
    ldrb w16, [x14], #1
    strb w16, [x11], #1
    cbnz w16, .Lsc2_1
.Lscd_1:
    mov x10, x12
    stur x10, [x29, #-24]
    ldur x10, [x29, #-24]
    stur x10, [x29, #-8]
    mov x0, #1
    ldur x10, [x29, #-32]
    ldur x10, [x29, #-8]
    stur x10, [x29, #-32]
    ldur x11, [x29, #-32]
    mov x1, x11
    ldur x10, [x29, #-40]
    ldur x10, [x29, #-8]
    stur x10, [x29, #-40]
    ldur x13, [x29, #-40]
    ldur x10, [x29, #-48]
    mov x14, x13
    mov x15, #0
.Lstrlen_s_2:
    ldrb w16, [x14, x15]
    cbz w16, .Lstrlen_d_2
    add x15, x15, #1
    b .Lstrlen_s_2
.Lstrlen_d_2:
    mov x10, x15
    stur x10, [x29, #-48]
    ldur x11, [x29, #-48]
    mov x2, x11
    movz x16, #4, lsl #0
    movk x16, #512, lsl #16
    svc #0x80
    add sp, sp, #80
    ldp x29, x30, [sp], #16
    ret

_main:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #80
    // function main
    adrp x10, __str_1@PAGE
    add x10, x10, __str_1@PAGEOFF
    stur x10, [x29, #-8]
    ldur x10, [x29, #-16]
    ldur x10, [x29, #-8]
    stur x10, [x29, #-16]
    ldur x10, [x29, #-16]
    mov x0, x10
    bl _print
    ldur x10, [x29, #-24]
    mov x10, x0
    stur x10, [x29, #-24]
    adrp x10, __str_2@PAGE
    add x10, x10, __str_2@PAGEOFF
    mov x0, x10
    bl _print
    ldur x10, [x29, #-32]
    mov x10, x0
    stur x10, [x29, #-32]
    mov x0, #0
    bl _exit
    ldur x10, [x29, #-40]
    mov x10, x0
    stur x10, [x29, #-40]
    mov x0, #0
    add sp, sp, #80
    ldp x29, x30, [sp], #16
    ret

_exit:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #48
    // function exit
    stur x0, [x29, #-8]
    ldur x10, [x29, #-16]
    ldur x10, [x29, #-8]
    stur x10, [x29, #-16]
    ldur x11, [x29, #-16]
    mov x0, x11
    movz x16, #1, lsl #0
    movk x16, #512, lsl #16
    svc #0x80
    add sp, sp, #48
    ldp x29, x30, [sp], #16
    ret

