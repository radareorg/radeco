global _main

section .text
	main:
		;push rbp
		;mov rbp, rsp
		;sub rsp, 0x40
		mov rax, 2048
		;shl rax, 5
		;compare:
		;shr rax, 1
		cmp rax, 2048
		je  equal
		add rax, 1
		equal:
		mov rbx, rax
		ret

