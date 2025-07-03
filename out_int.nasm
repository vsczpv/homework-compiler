section .text

	extern _funcall_arg0
	global out_int

;	argument in _funcall_arg0
out_int:	push rbp
		mov  rbp, rsp
		sub  rsp, 64

		mov  r8, 64

		mov  byte [rsp + r8], 0x0
		dec r8
		mov  byte [rsp + r8], 0xa
		dec r8


		mov  rax, qword [_funcall_arg0]
		mov  rbx, 10
.loop:		xor  rdx, rdx
		div  rbx
		
		add  rdx, '0'
		mov  byte [rsp + r8], dl
		dec r8

		cmp  rax, 0
		jne .loop

		; sys_write
		; rdi fd
		; rsi buf
		; rdx len

		mov r9, 64
		sub r9, r8

		mov rdx, r9
		lea rsi, byte [rsp + r8]
		mov rdi, 0x1
		mov rax, 0x1
		syscall

		mov rsp, rbp
		pop rbp
		ret
