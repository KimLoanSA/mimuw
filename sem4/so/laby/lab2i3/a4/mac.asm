global mac

section .text

mac: 
	; funkcja mac(a, x, y)
	; policzyc a + x * y
	; rdi - a
	; rsi - x
	; rdx -> rcx - y
	
	mov rcx, rdx
	
	mov rax, [rsi]
	mul QWORD [rcx]
	mov r9, rax
	mov r8, rdx

	mov rax, [rsi + 8]
	mul QWORD [rcx]
	add r8, rax

	mov rax, [rsi]
	mul QWORD [rcx + 8]
	add r8, rax
	
	mov rax, r9
	
	add r9, [rdi] 
	adc r8, [rdi + 8]

	mov rax, r9
	mov rdx, r8
ret
