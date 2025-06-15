SYS_EXIT equ 0x3c
section .bss
	a:	resb	8
	c:	resb	64

section .text
    global _start

_start:
		mov rax, 8
		mov rax, 3
		push rax

		lea rax, qword [a]
		mov rbx, rax

		pop rax

		mov qword [rbx], rax

		lea rax, qword [c]
		push rax

		lea rax, qword [a]
		push rax

		mov rax, 3
		mov rbx, rax

		pop rax


		mov rax, qword [rax]
		sub rax, rbx

		mov rbx, rax

		pop rax

		lea rax, qword [rax + rbx * 8]

		push rax

		lea rax, qword [c]
		push rax

		lea rax, qword [a]
		push rax

		mov rax, 1
		mov rbx, rax

		pop rax


		mov rax, qword [rax]
		add rax, rbx

		mov rbx, rax

		pop rax

		lea rax, qword [rax + rbx * 8]

		mov rbx, rax

		pop rax


		mov rax, qword [rax]

		mov rbx, qword [rbx]
		add rax, rbx

		push rax

		lea rax, qword [c]
		push rax

		lea rax, qword [a]
		push rax

		mov rax, 1
		mov rbx, rax

		pop rax


		mov rax, qword [rax]
		sub rax, rbx

		mov rbx, rax

		pop rax

		lea rax, qword [rax + rbx * 8]

		mov rbx, rax

		pop rax

		mov qword [rbx], rax


                xor rdi, rdi
                mov rax, SYS_EXIT
                syscall
        
