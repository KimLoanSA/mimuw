SYS_WRITE	equ 1
SYS_EXIT	equ 60
STDOUT		equ 1
NEW_LINE 	equ 10

global _start

section .rodata

hello 		db "Hello World!"
new_line 	db NEW_LINE

section .text

_start:
	mov rax, SYS_WRITE
	mov rdi, STDOUT
	mov rsi, hello
	mov rdx, 12

	syscall

	mov rax, SYS_WRITE
	mov rdi, STDOUT
	mov rsi, new_line
	mov rdx, 1

	syscall

	mov rax, SYS_EXIT
	mov rdi, 0
	
	syscall

