global inc_thread

section .bss

alignb 4
spin_lock resd 1 ; 1 raz 32 bity

section .text

align 8
inc_thread:
    mov     rsi, [rdi]      ; value
    mov     ecx, [rdi + 8]  ; count
    mov     rdx, spin_lock  ; W rdx jest adres blokady.

    jmp     count_test
count_loop:
    xor eax, eax
busy_wait:
    lock \
    bts     [rdx], eax      ; Jeśli blokada otwarta, zamknij ją.
    jc      busy_wait       ; Skocz, gdy blokada była zamknięta.

    inc     dword [rsi]     ; ++*value

    btr     [rdx], eax      ; Otwórz blokadę.
count_test:
    sub     ecx, 1          ; --count
    jge     count_loop      ; skok, gdy count >= 0
    xor     eax, eax        ; return NULL
    ret