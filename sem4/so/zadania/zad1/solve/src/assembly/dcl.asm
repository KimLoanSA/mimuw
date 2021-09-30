; Marcin Abramowicz ma406058

SYS_READ        equ 0                           ; numer funkcji systemowej `read`
SYS_WRITE       equ 1                           ; numer funkcji systemowej `write`
SYS_EXIT        equ 60                          ; numer funkcji systemowej `exit`

STDOUT          equ 1                           ; wypisywanie na standardowe wyjscie funkcja 'write'
STDIN           equ 0                           ; czytanie ze standardowego wejscia funkcja 'read'

PARAM_NUMBER    equ 5                           ; liczba argumentow programu (nazwa, L, R, T, key)
PARAM_LENGTH    equ 42                          ; dlugosc 3 parametrow programu (permutacji L R T)
KEY_LENGTH      equ 2                           ; dlugosc parametru programu (klucza)

ASCII_START     equ 49                          ; wartosc najmniejszego znaku, ktory moze byc na wejsciu
ASCII_END       equ 90                          ; wartosc najwiekszego znaku, ktory moze byc na wejsciu

ASCII_LEN       equ ASCII_END - ASCII_START     ; dlugosc alfabetu - 1
ALPH_LEN        equ ASCII_LEN + 1               ; dlugosc alfabetu

INPUT_BLOCK     equ 4096                        ; dlugosc bloku, ktory jest wczytywany

MAX_LINE        equ INPUT_BLOCK + PARAM_LENGTH  ; dlugosc do jakiej maksymalnie przeszukuje str_len

ASCII_L         equ 'L'                         ; wartosc pozycji obrotowej z literka 'L'
ASCII_R         equ 'R'                         ; wartosc pozycji obrotowej z literka 'R'
ASCII_T         equ 'T'                         ; wartosc pozycji obrotowej z literka 'T'


global _start

section .bss

input           resb INPUT_BLOCK + 1            ; tablica na dane wejsciowe
output          resb INPUT_BLOCK + 1            ; tablica na dane wyjsciowe

alphabet        resd ALPH_LEN * 3               ; tablica na alfabet (w Q i Q^(-1) uzywam, zeby nie obliczac modulo
perm_L          resd PARAM_LENGTH + 1           ; tablica na permutacje L
perm_L_rev      resd PARAM_LENGTH + 1           ; tablica na odwrotnosc permutacji L
perm_R          resd PARAM_LENGTH + 1           ; tablica na permutacje R
perm_R_rev      resd PARAM_LENGTH + 1           ; tablica na odwrotnosc permutacji R
perm_T          resd PARAM_LENGTH + 1           ; tablica na permutacje T
perm_T_rev      resd PARAM_LENGTH + 1           ; tablica na odwrotnosc permutacji T
key             resd KEY_LENGTH + 1             ; tablica na klucz

%define KEY_R   ebx                             ; define z rejestrem w ktorym jest klucz `r`
%define KEY_L   esp                             ; define z rejestrem w ktorym jest klucz `l`

                                          ; define do obliczania wskaznika w tablicy dla 32 bitowych typow
                                          ; [pointer]   - wskaznik na tablice,
                                          ; [i]         - indeks
%define IND32(pointer, i) (pointer + (i) * 4)

                                          ; define do obliczanai indeku w tablicy permutacji
                                          ; [perm]      - tablica z permutacja
                                          ; [a]         - pozycja w permutacji
%define PERM_IND(perm, a) IND32(perm, a - ASCII_START)

                                          ; define do obliczania "czesci" w tablicy z alfabetem (sa 3 do modulo)
                                          ; [pointer]   - tablica z alfabetem
                                          ; [part]      - czesc w alfabecie
%define ALPH_PART(pointer, part) IND32(pointer, ALPH_LEN * part)

%macro FROM_IP 3                          ; makro do kopiowania znaku z tablicy w rejestrze
                                          ; [%1]        - tablica,
                                          ; [%2]        - indeks w tablicy,
                                          ; [%3]        - rejestr do ktorego skopiowac
        movzx   %3, byte [%1 + %2]              ; kopiujemy wartosc spod indeksu w tablicy do rejestru
%endmacro

%macro TO_C 3                             ; makro do kopiowania znaku z tablicy w rejestrze
                                          ; [%1]        - tablica,
                                          ; [%2]        - indeks w tablicy,
                                          ; [%3]        - rejestr do ktorego skopiowac
        mov     %3, dword [IND32(%1, %2)]       ; kopiujemy wartosc spod indeksu w tablicy do rejestru
%endmacro

%macro TO_ARR 3                           ; makro do kopiowania znaku z rejestru pod indeks w tablicy (32 bit)
                                          ; [%1]        - rejestr z ktorego kopiujemy,
                                          ; [%2]        - tablica,
                                          ; [%3]        - indeks w tablicy
        mov     dword [IND32(%2, %3)], %1       ; kopiujemy waartosc z rejestru pod indeks w tablicy
%endmacro

%macro TO_OP 3                            ; makro do kopiowania znaku z rejestru pod indeks w tablicy (8 bit)
                                          ; [%1]        - rejestr z ktorego kopiujemy,
                                          ; [%2]        - tablica,
                                          ; [%3]        - indeks w tablicy
        mov     [%2 + %3], byte %1              ; kopiujemy waartosc z rejestru pod indeks w tablicy
%endmacro

%macro ROT_MOD 1                          ; makro sprawdzajace czy pozycja bebenka sie przekrecila, jesli tak to wykonuje modulo
                                          ; [%1]        - rejestr w ktorym znajduje sie wartosc bebenka
        cmp     %1, ASCII_LEN                   ; sprawdzamy czy przekroczylismy ostatnia pozycje
        jle     %%_end_if                       ; jesli nie to pomijamy
        xor     %1, %1                          ; jesli tak to zerujemy
%%_end_if:
%endmacro

%macro ROT 1                              ; makro ktore obraca bebenek
                                          ; [%1]        - rejestr w ktorym znajduje sie wartosc bebenka
        add     %1, 1                           ; obracamy bebenek
        ROT_MOD %1                              ; nakladamy modulo
%%_end_if:
%endmacro

%macro ROT_L 3                            ; makro ktore obraca bebenek L jesli bebenek jest w danej pozycji
                                          ; [%1]        - rejestr z lewym bebenkiem,
                                          ; [%2]        - rejestr z prawym bebenkiem,
                                          ; [%3]        - wartosc sprawdzanej pozycji obrotowej
        cmp     %2, %3 - ASCII_START            ; sprawdzmy czy R jest w podanej pozycji (obrotowej)
        jne     %%_end_if                       ; jesli nie to nie wykonujemy rotacji
        ROT     %1                              ; jesli tak to wykonujemy rotacje
%%_end_if:
%endmacro

%macro ROT_R 2                            ; makro ktore obraca bebenek R i L jesli R jest w pozycji obrotowej
                                          ; [%1]        - rejestr z prawym bebenkiem,
                                          ; [%2]        - rejestr z lewym bebenkiem
        ROT     %1                              ; obracamy bebenek R
        ROT_L   %2, %1, ASCII_L                 ; obracamy bebenek L, jesli R jest w pozycji `L`
        ROT_L   %2, %1, ASCII_R                 ; obracamy bebenek L, jesli R jest w pozycji `R`
        ROT_L   %2, %1, ASCII_T                 ; obracamy bebenek L, jesli R jest w pozycji `T`
%endmacro

%macro PERM 3                             ; obliczanie przesuniecia na podstawie permutacji
                                          ; [%1]        - rejest z przetwarzanym znakiem,
                                          ; [%2]        - indeks w permutacji,
                                          ; [%3]        - permutacja
        mov     %1, dword [PERM_IND(%3, %2)]    ; kopiujemy do rejestru wartosc z permutacji spod podanego indeksu
%endmacro

%macro Q 3                                ; permutacja  Q
                                          ; [%1]        - rejest z przetwarzanym znakiem,
                                          ; [%2]        - rejestr z przetwarzanym znakiem, ale 64 bit
                                          ; [%3]        - rejestr z pozycja bebenka
        add     %1 ,%3                          ; dodajemy wartosc przesuniecia
        PERM    %1, %2, ALPH_PART(alphabet, 1)  ; sprawdzamy w alfabecie na co mapujemy (uwzgledniajac modulo)
%endmacro

%macro Q_1 3                              ; permutacja Q^(-1)
                                          ; [%1]        - rejest z przetwarzanym znakiem,
                                          ; [%2]        - rejestr z przetwarzanym znakiem, ale 64 bit
                                          ; [%3]        - rejestr z pozycja bebenka
        sub     %1 ,%3                          ; odejmujemy wartosc przesuniecia
        PERM    %1, %2, ALPH_PART(alphabet, 1)  ; sprawdzamy w alfabecie na co mapujemy (uwzgledniajac modulo)
%endmacro

%macro L 2                                ; permutacja L
                                          ; [%1]        - rejest z przetwarzanym znakiem,
                                          ; [%2]        - indeks w permutacji
        PERM    %1, %2, perm_L                  ; obliczamy nowa wartosc dla permutacji `L`
%endmacro

%macro L_1 2                              ; permutacja L^(-1)
                                          ; [%1]        - rejest z przetwarzanym znakiem,
                                          ; [%2]        - indeks w permutacji
        PERM    %1, %2, perm_L_rev              ; obliczamy nowa wartosc dla permutacji `L^(-1)`
%endmacro

%macro R 2                                ; permutacja R
                                          ; [%1]        - rejest z przetwarzanym znakiem,
                                          ; [%2]        - indeks w permutacji
        PERM    %1, %2, perm_R                  ; obliczamy nowa wartosc dla permutacji `R`
%endmacro

%macro R_1 2                              ; permutacja R^(-1)
                                          ; [%1]        - rejest z przetwarzanym znakiem,
                                          ; [%2]        - indeks w permutacji
        PERM    %1, %2, perm_R_rev              ; obliczamy nowa wartosc dla permutacji `R^(-1)`
%endmacro

%macro T 2                                ; permutacja T
                                          ; [%1]        - rejest z przetwarzanym znakiem,
                                          ; [%2]        - indeks w permutacji
        PERM    %1, %2, perm_T                  ; obliczamy nowa wartosc dla permutacji `T`
%endmacro

%macro VALID_C 1                          ; makro sprawdzajace czy dany znak jest dopuszczalny,
                                          ; jesli nie to wychodzi z kodem `1`
                                          ; [%1]        - rejestr ze sprawdzanym znakiem
        cmp     %1, ASCII_START                 ; znak musi byc nie mniejszy niz `ASCII_START` w ASCII
        jl      _exit_with_1                    ; jesli nie jest to wychodzimy z kodem 1

        cmp     %1, ASCII_END                   ; znak musi byc nie wiekszy niz `ASCII_END` w ASCII
        jg      _exit_with_1                    ; jesli nie jest to wychodzimy z kodem 1
%endmacro


section .text

str_len:                                  ; funkcja obliczajaca dlugosc napisu
                                          ; (string) rdi,
                                          ; (wynik) r11
                                          ; [modyfikuje] rsi, rax, rcx
        mov     rsi, rdi                        ; kopiujemy stringa do innego rejestru zeby policzyc roznice
        cld                                     ; zwiększaj indeks przy przeszukiwaniu napisu
        xor     al, al                          ; szukaj zera
        mov     ecx, MAX_LINE                   ; ogranicz przeszukiwanie do MAX_LINE znaków
        repne scasb
        mov     r11, rdi                        ; przenosimy wynik
        sub     r11, rsi                        ; odejmujemy startowy adres
        sub     r11, 1                          ; -1 dla wygody
        ret

next_param:                               ; funkcja przechodzi do nastepnego argumentu programu i wczytuje go
                                          ; ()
                                          ; [wynik] rdi,
                                          ; [modyfikuje] rbp
        add     rbp, 8                          ; przechodzimy do nastepnego arguemntu
        mov     rdi, [rbp]                      ; adres kolejnego argumentu
        ret

process_param:                            ; funkcja przetwarzajaca parametr - waliduje go i zapisuje
                                          ; (dlugosc param) rdx
                                          ; (wskaznik gdzie zapisac permutacje) r10,
                                          ; (mode) r12, mode: [0] - dla klucza, [1] - dla L i R, [2] - dla T
                                          ; (wskaznik gdzie zapisac odwrotna permutacje): rsp
                                          ; [wynik] void
                                          ; [modyfikuje] r8, r9, rdi, r11, rax
        call    next_param                      ; wczytujemy parametr, w `rdi` mamy go
        mov     r8, rdi                         ; kopiujemy parametr do `r8`

        call    str_len                         ; obliczamy dlugosc napisu, w `r11` mamy dlugosc

        cmp     r11, rdx                        ; jesli nie jest rowne wymaganej dlugosci (42 lub 2)
        jne     _exit_with_1                    ; to wychodzimy z kodem `1`

        ; walidacja i zapisywanie parametru
        xor     r9, r9                          ; petla od 0

_loop_param:                                    ; iterujemy sie po stringu z parametrem
        FROM_IP r8, r9, eax                     ; przepisujemy jeden znak stringa do `eax`
        VALID_C eax                             ; sprawdzamy czy znak jest poprawny

        TO_ARR  eax, r10, r9                    ; zapisujemy znak do tablicy z permutacja

        test    r12, r12                        ; sprawdzmy czy chcemy zapisywac odwrotnosc permutacji
        je      _skip_rev                       ; jesli nie to omijamy zapisywanie

        ; obliczanie odwrotnosci permutacji
        mov     rdi, r9                         ; przenosimy znak do rejestru, bedzie to teraz indeks w stringu
        sub     eax, ASCII_START                ; odejmuemy poczatek alfabetu, (indeksujemy stringa od 0)
        add     edi, ASCII_START                ; dodajemy poczatek alfabetu (bedziemy porownywac znaki)

        cmp     dword [IND32(r13, rax)], 0      ; sprawdzamy czy nie zapisalismy juz w tym miejscu znaku
        jne     _exit_with_1                    ; jesli tak to wychodzimu z kodem `1`

        TO_ARR  edi, r13, rax                   ; zapisujemy znak do tablicy z odwrotnoscia permutacji

_skip_rev:
        add     r9, 1                           ; licznik petli + 1
        cmp     r9, r11                         ; sprawdzmy czy juz przetworzylismy caly napis
        jne     _loop_param                     ; jesli nie to przechodzimy do nastepnego znaku

        cmp     r12, 2                          ; sprawdzamy czy obslugujemy permutacja T
        jne     _skip_T                         ; jesli nie to pomijamy nastepny blok

        ; sprawdzamy czy T jest permutacja 21 cykli
        xor     r9, r9                          ; petla od 0

_loop_param_T:                                  ; iterujemy sie po stringu z parametrem T
        TO_C    r10, r9, eax                    ; przepisujemy jeden znak stringa do `eax`
        TO_C    r13, r9, ecx                    ; przepisujemy odwrotnosc tego znaku do `ecx`
        cmp     eax, ecx                        ; sprawdzamy czy literki sa takie same
        jne     _exit_with_1                    ; jesli nie to znaczy ze nie jest to 2 elementowy cykl, wychodzimy z kodem `1`

        sub     ecx, ASCII_START                ; zmieniamy wartosc znaku na numerek na bebenku
        cmp     ecx, r9d                        ; sprawdzamy czy odwrotnosc jest rowna aktualnej pozycji
        je      _exit_with_1                    ; jesli tak to znaczy ze cykl jest 1 elementowy, wychodzimy z kodem `1`

        add     r9, 1                           ; licznik petli + 1
        cmp     r9, r11                         ; sprawdzmy czy juz przetworzylismy caly napis
        jne     _loop_param_T                   ; jesli nie to przechodzimy do nastepnego znaku

_skip_T:
        ret


_start:
        cmp     qword [rsp], PARAM_NUMBER       ; sprawdzamy czy program dostal odpowiednia liczbe arguemntow
        jne     _exit_with_1                    ; jesli nie to wychodizmy z kodem `1`

        lea     rbp, [rsp + 8]                  ; adres args[0]

        ; wczytujemy permutacje L R T (3 parametry po 42 znaki)
        mov     rdx, PARAM_LENGTH               ; parametr musi miec 42 znaki
        mov     r10, perm_L                     ; zapisujemy permutacje L
        mov     r12, 1                          ; checmy zapisac odwrotnosc permutacji
        mov     r13, perm_L_rev                 ; zapisujemy permutacje L^(-1)
        call    process_param                   ; przetwarzamy argumenty

        mov     rdx, PARAM_LENGTH               ; parametr musi miec 42 znaki
        mov     r10, perm_R                     ; zapisujemy permutacje R
        mov     r12, 1                          ; checmy zapisac odwrotnosc permutacji
        mov     r13, perm_R_rev                 ; zapisujemy permutacje R^(-1)
        call    process_param                   ; przetwarzamy argumenty

        mov     rdx, PARAM_LENGTH               ; parametr musi miec 42 znaki
        mov     r10, perm_T                     ; zapisujemy permutacje T
        mov     r12, 2                          ; sprawdzamy poprawnosc permutacji T
        mov     r13, perm_T_rev                 ; zapisujemy permutacje T^(-1), do sprawdzania liczby cykli
        call    process_param                   ; przetwarzamy argumenty

        ; wczytywanie klucza szyfrowania
        mov     rdx, KEY_LENGTH                 ; klucz musi miec 2 znaki
        mov     r10, key                        ; zapisujemy klucz
        xor     r12, r12                        ; nie chcemy zapisywac odwrotnosci permutacji
        call    process_param                   ; przetwarzamy argument

        ; zapisywanie alfabetu do Q i Q^(-1)
        ; alfabet jest zapisywany x3, zeby przy wykonywaniu Q "przeskoczenie" alfabetu skutkowalo
        ; czytaniem z 1. lub 3. czesci, oszczedzamy wtedy operacje modulowania
        mov     eax, ASCII_START                ; zaczynamy od pierwszego znaku
_loop_alphabet:
                                                ; zapisujemy do 3 czesci znak
        TO_ARR  eax, ALPH_PART(alphabet, 0), rax - ASCII_START
        TO_ARR  eax, ALPH_PART(alphabet, 1), rax - ASCII_START
        TO_ARR  eax, ALPH_PART(alphabet, 2), rax - ASCII_START

        add     eax, 1                          ; nastepny znak w alfabecie
        cmp     eax, ASCII_END                  ; jesli nie doszlismy do konca alfabetu
        jle     _loop_alphabet                  ; to przetwarzamy nastepny znak

        ; przetwarzanie klucza szyfrowania
        TO_C    key, 0, KEY_L                   ; zapisujemy klucz `l` do `ecx`
        sub     KEY_L, ASCII_START              ; mapujemy na pozycje w bebenku

        TO_C    key, 1, KEY_R                   ; zapisujemy klucz `r` do `ebx`
        sub     KEY_R, ASCII_START              ; mapujemy na pozycje w bebenku

        ; wczytywanie wejscia
_loop_input:                                    ; czytamy wejscie blokami, az nie skonczy sie wejscie
        mov     eax, SYS_READ                   ; czytamy
        mov     edi, STDIN                      ; ze standardowego wejscia
        mov     rsi, input                      ; wczytujemy zawartosc do input
        mov     edx, INPUT_BLOCK                ; wczytujemy blokami po `INPUT_BLOCK` znakow
        syscall

        cmp     eax, 0                          ; sprawdzamy czy jeszcze cos wczytalismy
        jl      _exit_with_1                    ; jesli cos ujemnego to znaczy ze blad, wychodzimy z kodem `1`
        je      _exit_with_0                    ; jesli wczytalismy 0 znakow to wychodzimy z kodem 0

        mov     r15, rax                        ; przenosimy dlugosc wczytanego bloku do `r15`
        xor     r14, r14                        ; zerujemy licznik petli

_loop_block:                                    ; przechodzimy po wczytanym bloku
        FROM_IP input, r14, eax                 ; kopiujemy znak z wejscia do `eax`

        VALID_C eax                             ; sprawdzamy poprawnosc znaku
        TO_OP   0, input, r14                   ; zerujemy tablice wejsciowa

        ; szyfrowanie / deszyfrowanie
        ; w `esp` mamy pozycje `L` w `ebx` pozycje `R`, w `eax` przetwarzany znak
        ROT_R   KEY_R, KEY_L                    ; rotacja bebenkami

        Q       eax, rax, KEY_R                 ; Qr
        R       eax, rax                        ; R
        Q_1     eax, rax, KEY_R                 ; Qr^(-1)
        Q       eax, rax, KEY_L                 ; Ql
        L       eax, rax                        ; L
        Q_1     eax, rax, KEY_L                 ; Ql^(-1)
        T       eax, rax                        ; T
        Q       eax, rax, KEY_L                 ; Ql
        L_1     eax, eax                        ; L^(-1)
        Q_1     eax, rax, KEY_L                 ; Ql^(-1)
        Q       eax, rax, KEY_R                 ; Qr
        R_1     eax, eax                        ; R^(-1)
        Q_1     eax, rax, KEY_R                 ; Qr^(-1)

        TO_OP   al, output, r14                 ; zapisujemy zaszyfrowany / deszyfrowany znak do tablicy wyjsciowej

        add     r14, 1                          ; licznik petli + 1
        cmp     r14, r15                        ; sprawdzmy czy koniec bloku
        jl      _loop_block                     ; jesli nie to kolejny znak z bloku

        mov     eax, SYS_WRITE                  ; wypisujemy
        mov     edi, STDOUT                     ; na standardowe wyjscie
        mov     rsi, output                     ; zakodowany blok
        mov     rdx, r15                        ; o podanej dlugosci
        syscall

        cmp     eax, 0                          ; sprawdzamy czy wypisywanie sie udalo
        jl      _exit_with_1                    ; jesli nie to wychodzimy z kodem `1`

        cmp     r15, INPUT_BLOCK                ; sprawdzamy czy wczytalismy caly blok
        je      _loop_input                     ; to juz nie powtarzamy wczytywania

        ; wychodzenie z programu
_exit_with_0:                                   ; wychodzenie z programu z kodem `0`
        mov     eax, SYS_EXIT                   ; funkcja systemowa 'exit'
        xor     edi, edi                        ; ustawienie kodu wyjscia na `0`
        syscall

_exit_with_1:                                   ; wychodznien z programu z kodem `1`
        mov     eax, SYS_EXIT                   ; funkcja systemowa 'exit'
        mov     edi, 1                          ; ustawienie kodu wyjscia na `1`
        syscall

