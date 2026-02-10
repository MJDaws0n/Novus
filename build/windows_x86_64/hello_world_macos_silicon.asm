bits 64
default rel

section .data
    _str_0: db 10, 0
    _str_0_len: equ $ - _str_0 - 1
    _str_1: db "Hello, World!", 0
    _str_1_len: equ $ - _str_1 - 1
    _str_2: db "Goodbye, World!", 0
    _str_2_len: equ $ - _str_2 - 1

section .bss
    _novus_strcat_buf: resb 4096

section .text
    global len
    global print
    global main
    global exit

len:
    push rbp
    mov rbp, rsp
    sub rsp, 128
    ; function len
    mov qword [rbp-8], rcx
    mov r10, 0
    mov qword [rbp-16], r10
    mov r10, 0
    mov qword [rbp-24], r10
.Lwhile_cond_0:
    mov r10, qword [rbp-8]
    mov qword [rbp-32], r10
    mov r10, qword [rbp-24]
    mov qword [rbp-40], r10
    mov r10, qword [rbp-32]
    mov r11, qword [rbp-40]
    mov rdi, r10
    mov rsi, r11
    xor rax, rax
    mov al, [rdi+rsi]
    mov qword [rbp-48], rax
    mov r10, qword [rbp-48]
    mov r11, 0
    cmp r10, r11
    setne r10b
    movzx r10, r10b
    mov qword [rbp-56], r10
    mov r10, qword [rbp-56]
    test r10, r10
    jz .Lwhile_end_2
.Lwhile_body_1:
    mov r10, qword [rbp-16]
    mov qword [rbp-64], r10
    mov r10, qword [rbp-64]
    mov r11, 1
    add r10, r11
    mov qword [rbp-72], r10
    mov r10, qword [rbp-72]
    mov qword [rbp-16], r10
    mov r10, qword [rbp-24]
    mov qword [rbp-80], r10
    mov r10, qword [rbp-80]
    mov r11, 1
    add r10, r11
    mov qword [rbp-88], r10
    mov r10, qword [rbp-88]
    mov qword [rbp-24], r10
    jmp .Lwhile_cond_0
.Lwhile_end_2:
    mov r10, qword [rbp-16]
    mov qword [rbp-96], r10
    mov r10, qword [rbp-96]
    mov rax, r10
    add rsp, 128
    pop rbp
    ret

print:
    push rbp
    mov rbp, rsp
    sub rsp, 80
    ; function print
    mov qword [rbp-8], rcx
    mov r10, qword [rbp-8]
    mov qword [rbp-16], r10
    mov r10, qword [rbp-16]
    lea r11, [rel _str_0]
    push rdi
    push rcx
    lea rdi, [rel _novus_strcat_buf]
    push rdi
.sc1_1:
    mov cl, [r10]
    test cl, cl
    jz .sc2_1
    mov [rdi], cl
    inc r10
    inc rdi
    jmp .sc1_1
.sc2_1:
    mov cl, [r11]
    mov [rdi], cl
    test cl, cl
    jz .scd_1
    inc r11
    inc rdi
    jmp .sc2_1
.scd_1:
    pop r10
    pop rcx
    pop rdi
    mov qword [rbp-24], r10
    mov r10, qword [rbp-24]
    mov qword [rbp-8], r10
    mov r10, 1
    mov rax, r10
    mov r10, qword [rbp-8]
    mov qword [rbp-32], r10
    mov r10, qword [rbp-32]
    mov rbx, r10
    mov r10, qword [rbp-8]
    mov qword [rbp-40], r10
    mov r10, qword [rbp-40]
    mov rdi, r10
    xor rcx, rcx
.strlen_s_2:
    cmp byte [rdi+rcx], 0
    je .strlen_d_2
    inc rcx
    jmp .strlen_s_2
.strlen_d_2:
    mov qword [rbp-48], rcx
    mov r10, qword [rbp-48]
    mov rcx, r10
    mov r10, 33554436
    mov r8, r10
    syscall
    add rsp, 80
    pop rbp
    ret

main:
    push rbp
    mov rbp, rsp
    sub rsp, 80
    ; function main
    lea r10, [rel _str_1]
    mov qword [rbp-8], r10
    mov r10, qword [rbp-8]
    mov qword [rbp-16], r10
    mov r10, qword [rbp-16]
    mov rcx, r10
    sub rsp, 32
    call print
    add rsp, 32
    mov qword [rbp-24], rax
    lea r10, [rel _str_2]
    mov rcx, r10
    sub rsp, 32
    call print
    add rsp, 32
    mov qword [rbp-32], rax
    mov r10, 0
    mov rcx, r10
    sub rsp, 32
    call exit
    add rsp, 32
    mov qword [rbp-40], rax
    mov r10, 0
    mov rax, r10
    add rsp, 80
    pop rbp
    ret

exit:
    push rbp
    mov rbp, rsp
    sub rsp, 48
    ; function exit
    mov qword [rbp-8], rcx
    mov r10, qword [rbp-8]
    mov qword [rbp-16], r10
    mov r10, qword [rbp-16]
    mov rax, r10
    mov r10, 33554433
    mov r8, r10
    syscall
    add rsp, 48
    pop rbp
    ret

