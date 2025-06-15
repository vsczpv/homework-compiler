SYS_EXIT equ 0x3c
section .bss
	a:	resb	8
	b:	resb	32
	c:	resb	32
	d:	resb	32
	cin:	resb	8
	cout:	resb	8

section .text
    global _start

_start:
		mov rax, 4
		mov rax, 4
		mov rax, 4
		mov rax, 1
		push rax

		lea rax, qword [a]
		mov rbx, rax

		pop rax

		mov qword [rbx], rax

		lea rax, qword [b]
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

		push rax

		lea rax, qword [c]
		push rax

		mov rax, 3
		mov rbx, rax

		pop rax

		lea rax, qword [rax + rbx * 8]

		mov rbx, rax

		pop rax


		mov rax, qword [rax]

		mov rbx, qword [rbx]
		add rax, rbx

		push rax

		lea rax, qword [d]
		push rax

		mov rax, 5
		mov rbx, rax

		pop rax

		lea rax, qword [rax + rbx * 8]

		mov rbx, rax

		pop rax

		mov qword [rbx], rax


                xor rdi, rdi
                mov rax, SYS_EXIT
                syscall
        
