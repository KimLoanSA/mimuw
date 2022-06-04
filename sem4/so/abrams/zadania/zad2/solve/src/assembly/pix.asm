; Marcin Abramowicz ma406058
extern pixtime

global pix

%macro DIV128 3                   ; makro do dzielenia liczby 128 bitowej przez 64 bitowa
                                  ; [%1]  - rejestr z wartoscia licznika (128 bity)
                                  ; [%2]  - rejestr z wartoscia mianownika (64 bity)
                                  ; [%3]  - rejestr do ktorego ma byc zapisany wynik

        mov     rax, %1                 ; licznik do `rax`
        div     %2                      ; dzielimy przez mianownik
        mov     %3, rax                 ; przenosimy wynik
%endmacro


%macro DIV64 3                    ; makro do dzielenia dwoch liczb 64 bitowych
                                  ; [%1]  - rejestr z wartoscia licznika
                                  ; [%2]  - rejestr z wartoscia mianownika
                                  ; [%3]  - rejestr do ktorego ma byc zapisany wynik

        xor     edx, edx                ; dzielimy 64 bitowe liczby wiec zerujemy rejestr `rdx`
        DIV128  %1, %2, %3              ; reszta jak w dzieleniu 128 bitowej przez 64 bitowa
%endmacro

%macro DIV64S 3                   ; makro do dzielenia dwoch liczb, z licznikiem przesunietym o 64 bity
                                  ; [%1]  - rejestr z wartoscia licznika
                                  ; [%2]  - rejestr z wartoscia mianownika
                                  ; [%3]  - rejestr do ktorego ma byc zapisany wynik

        mov     rdx, %1                 ; przenosimy do rdx licznik (czyli przesuwamy go o 64 bity)
        xor     eax, eax                ; przesuwamy o 64 bity, wiec pierwsze 64 bity (`rax`) zerujemy

        cmp     %2, 1                   ; dzielenie przez 1, czyli zwracamy to samo
        je      %%_if

        div     %2                      ; wykonujemy dzielenie przez mianownik

%%_if:
        mov     %3, rax                 ; przenosimy wynik
%endmacro

%macro MUL64 3                    ; makro do mnozenia dwoch liczb 64 bitowych
                                  ; [%1]  - rejestr z pierwsza wartoscia
                                  ; [%2]  - rejestr z druga wartoscia
                                  ; [%3]  - rejestr do ktorego ma byc zapisany wynik

        xor     edx, edx                ; mnozymy 64 bitowe liczby wiec zerujemy rejestr `rdx`
        mov     rax, %1                 ; pierwsza wartosc do `rax`
        mul     %2                      ; mnozymy przez druga
        mov     %3, rax                 ; przenosimy wynik
%endmacro

%macro MUL64CJ 4                  ; makro do mnozenia dwoch liczb 64 bitowych,
                                  ; jesli wynik jest wiekszy niz 2^64 to skacze do podanej etykiety
                                  ; [%1]  - rejestr z pierwsza wartoscia
                                  ; [%2]  - rejestr z druga wartoscia
                                  ; [%3]  - rejestr do ktorego ma byc zapisany wynik
                                  ; [%4]  - etykieta do ktorej ma skoczyc jesli warunek bedzie spelniony

        MUL64   %1, %2, %3              ; wykonujemy mnozenie

        test    rdx, rdx                ; sprawdzamy czy wynik jest wiekszy niz 2^64
        jnz     %4                      ; jesli tak to znaczy ze czynnik bedzie mniejszy niz 2^(-64), wiec skaczemy
%endmacro

%macro MOD 2                      ; makro do obliczania modulo dla liczby
                                  ; [%1]  - rejestr z liczba do zmodulowania, w niej jest zapisywany wynik
                                  ; [%2]  - rejestr z wartoscia modulo

        DIV128  %1, %2, rax             ; dzielimy liczbe przez modulo
        mov     %1, rdx                 ; przenosimy reszte z dzielenia do wyniku
%endmacro

%macro FAST_POW_MOD 4             ; makro do szybkiego potegowania modulo
                                  ; [%1]  - rejestr z liczba podnoszana do potegi
                                  ; [%2]  - rejestr z wartoscia wykladnika
                                  ; [%3]  - rejestr z wartoscia modulo
                                  ; [%4]  - rejestr w ktorym ma byc zapisany wynik

        push    rbx
        mov     rbx, %1                 ; [rbx] - rejestr z aktualna potega
        mov     %4, 1                   ; wynik inicjujemy jedynka

%%_loop:
        test    %2, 1                   ; sprawdzamy czy w potedze jest zapalony ostatni bit
        jz      %%_if                   ; jesli nie to nie domnazamy tej potegi do wyniku

        MUL64   %4, rbx, %4             ; jesli jest to domnazamy aktualna potege go wyniku
        MOD     %4, %3                  ; i nakladamy modulo na wynik

%%_if:
        MUL64   rbx, rbx, rbx           ; podnosimy potege do kwadratu
        MOD     rbx, %3                 ; i nakladamy modulo na potege

        shr     %2, 1                   ; dzielimy wykladnik przez 2
        jnz     %%_loop                 ; powtarzamy jesli sa jeszcze jakies bity w wykladniku

        pop rbx
%endmacro

%macro SUM_1 3                    ; makro do obliczania sum k=0..n (16^(n-k) mod (8k + j)) / (8k + j)
                                  ; [%1]  - rejestr z wartoscia `n`
                                  ; [%2]  - rejestr z wartoscia `j`
                                  ; [%3[  - rejestr w ktorym ma byc zapisany wynik (nie jest zerowany rejestr!)
        push    rbx
        push    rbp
        push    r12
        push    r13

        xor     rbx, rbx                ; [rbx] - licznik petli, zerujemy go

%%_loop:

        MUL64   8, rbx, rbp             ; obliczamy 8k, wynik w `rbp`
        add     rbp, %2                 ; dadajemy j - obliczamy (8k + j), wynik w `rbp`

        mov     r12, %1                 ; obliczamy n - k (wykladnik)
        sub     r12, rbx                ; obliczamy n - k (wykladnik)

        FAST_POW_MOD 16, r12, rbp, r13  ; obliczamy 16^(n-k) mod (8k + j), wynik w `r13`

        DIV64S  r13, rbp, rax           ; obliczamy (16^(n-k) mod (8k + j)) / (8k + j)
                                        ; przesuwajac o 64 bity, zeby zachowac dokladnosc po przecinku

        add     %3, rax                 ; dodajemy do wyniku

        inc     rbx                     ; zwiekszamy licznik petli

        cmp     rbx, %1                 ; sprawdzamy czy juz konczymy sumowac
        jle     %%_loop                 ; jesli nie to liczymy dalej

        pop     r13
        pop     r12
        pop     rbp
        pop     rbx
%endmacro

%macro SUM_2 3                    ; makro do obliczania sum k=n+1... 16^(n-k) / (8k + j)
                                  ; [%1]  - rejestr z wartoscia `n`
                                  ; [%2]  - rejestr z wartoscia `j`
                                  ; [%3[  - rejestr w ktorym ma byc zapisany wynik (nie jest zerowany rejestr!)
        push    rbx
        push    rbp
        push    r12

        mov     rbx, %1                 ; [rbx] - licznik petli, ustawiamy go na n
        inc     rbx                     ; ustawiamy na n + 1
        mov     rbp, 1                  ; [rbp] - aktualna potega 16, ustawiamy na `1`

%%_loop:
        MUL64CJ 16, rbp, rbp, %%_end    ; liczymy kolejna potege `16`, jest w `rbp`
        MUL64CJ 8, rbx, r12, %%_end     ; obliczamy 8k, wynik w `r12`
        add     r12, %2                 ; dodajemy j - obliczamy (8k + j), wynik w `r12`

        MUL64CJ rbp, r12, r12, %%_end   ; obliczamy 16^(k-n) * (8k + j), wynik w `r12`
        DIV64S  1, r12, r12             ; obliczamy 16^(n-k) / (8k + j), wynik w `r12`
        add     %3, r12                 ; dodajemy do wyniku

        inc     rbx                     ; licznik petli + 1
        jmp     %%_loop                 ; nastepna iteracja

%%_end:
        pop     r12
        pop     rbp
        pop     rbx
%endmacro

%macro TIME 1                    ; makro do pobierania liczby cykli procesora i laczenia wyniku do jednego rejestru
                                  ; [%1]  - rejestr w ktorym ma byc zapisany wynik

        rdtsc                           ; pobieramy liczbe cykli procesora. wynik w `edx:eax`
        shl     rdx, 32                 ; przesuwamy `bity w rdx`
        or      rdx, rax                ; laczymy rejestry `rdx` i `rax`
        mov     %1, rdx                 ; przenosimy polaczone rejestry do wynikowego `%1`
%endmacro


section .text

sj_16:                            ; funkcja do obliczania wartosci Sj * 16^n
                                  ; [r8]  - rejestr z wartoscia `n`
                                  ; [r14]  - rejestr z wartoscia `j`
                                  ; [r15]  - rejestr w ktorym ma byc zapisany wynik (jest zerowany rejestr!)

        xor     r15, r15                  ; zerujemy rejestr z wynikiem
        SUM_1   r8, r14, r15              ; obliczamy sum k=0..n (16^(n-k) mod (8k + j)) / (8k + j)
        SUM_2   r8, r14, r15              ; obliczamy sum k=n+1... 16^(n-k) / (8k + j)

        ret

align 8
pix:                              ; funkcja z tresci zadania
                                  ; [rdi] - rejestr zee wskaznikiem na tablice
                                  ; [rsi] - rejestr ze wskaznikiem na indeks w tablicy
                                  ; [rdx] - rejestr z  wartoscia `max`

        push    rbx
        push    rbp
        push    r13
        push    r14
        push    r15

        mov     r13, rsi                ; przenosimy wskaznik na indeks w tablicy do `r13`
        mov     rbx, rdi                ; przenosimy wskaznik na tablice do `rbx`
        mov     rbp, rdx                ; przenosimy wartosc `max` do `rbp`

        TIME    rdi                     ; pobieramy liczbe cykli procesora, wynik w `rdi`
        call    pixtime                 ; wywolujemy funkcje (stos wyrownany)

_loop:
        mov     r8, 1                   ; dodajemy jeden do indeksu
        lock xadd qword [r13], r8       ; atomowo pobieramy indeks i zwiekaszamy jego wartosc

        cmp     r8, rbp                 ; sprawdzamy czy pobrany indeks jest mniejszy niz `max`
        jge     _return                 ; jesli nie to konczymy

        MUL64   8, r8, r8               ; dla danego n = 8 * m

        ; licznie wartosci dla n

        mov     r14, 1                  ; liczymy dla j = 1
        call    sj_16
        MUL64   4, r15, r9              ; obliczamy 4 * 16^nS(1)

        mov     r14, 4                  ; liczymy dla j = 4
        call    sj_16
        MUL64   2, r15, r15             ; obliczamy 2 * 16^nS(4)
        sub     r9, r15                 ; obliczamy 4 * 16^nS(1) - 2 * 16^nS(4)

        mov     r14, 5                  ; liczymy dla j = 5
        call    sj_16
        sub     r9, r15                 ; obliczamy 4 * 16^nS(1) - 2 * 16^nS(4) - 16^nS(5)

        mov     r14, 6                  ; liczymy dla j = 6
        call    sj_16
        sub     r9, r15                 ; obliczamy 4 * 16^nS(1) - 2 * 16^nS(4) - 16^nS(5) - 16^nS(6)

        shr     r9, 32                  ; obchodza nas starsze 32 bity, wiec przesuwamy o 32
        shr     r8, 1                   ; teraz n = 8 * m, a tablica jest 32 bitowa, wiec indeks to 4 * m
        mov     dword [rbx + r8], r9d   ; zapismujemy wynik w tablicy

        jmp      _loop                  ; nastepna iteracja

_return:

        pop     r15
        pop     r14
        pop     r13
        pop     rbp
        pop     rbx

        sub     rsp, 8                  ; wyrownywanie stosu przed wywolaniem funkcji

        TIME    rdi                     ; pobieramy liczbe cykli procesora, wynik w `rdi`
        call    pixtime                 ; wywolujemy funkcje

        add     rsp, 8                  ; wyrownywanie stosu po wywolaniu funkcji

        ret
