extern pixtime

global modPix
global powPix
global sum1Pix
global sum2Pix
global pixPi
global pwPix

global pix

section .text


modPix:
    ; rdi - wartosc
    ; rsi - modulo

        ; TODO

        ret

powPix:
    ; rdi - wartosc
    ; rsi - potega
    ; rdx - modulo
        
        ; TODO

        ret

sum1Pix:
; rdi - n
; rsi - j
        ; TODO

        ret

sum2Pix:
; rdi - n
; rsi - j
        ; TODO

        ret

pixPi:
; rdi - n

        ; TODO

        ret

; TODO do wyjebonexa
align 8
pwPix:
; rdi - wskaznik na tablice
; rsi - wskaznik na indeks
; rdx - wartosc max

        ; TODO

        ret


; rdi - wskaznik na tablice
; rsi - wskaznik na indeks
; rdx - wartosc max
align 8
pix:

        ret
