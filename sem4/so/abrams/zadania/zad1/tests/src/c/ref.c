#include <stdio.h>
#include <string.h>
#include <sys/types.h>

#define NCHARS 42
#define BUF_SIZE 4096

int norm_et_ver(char *buf, char *inv) {
    char len = 0;
    memset(inv, 0, NCHARS);
    
    while (*buf) {
        if (*buf < '1' || 'Z' < *buf) return 1;
        *buf -= '1';
        
        if (inv[(unsigned char)*buf]) return 1;
        inv[(unsigned char)*buf] = len++;

        ++buf;
    }

    if (len != NCHARS) return 1;
    return 0;
}

void Qnorm(char i, char *c) {
    *c = (i + *c) % NCHARS;
}

int main(int argc, char **argv) {
    if (argc != 5) return 1;

    // retrieve stuff
    char *L = argv[1], *R = argv[2], *T = argv[3];
    char l = argv[4][0], r = argv[4][1];
    if (argv[4][2]) return 1;

    char Linv[NCHARS] = {0}, Rinv[NCHARS] = {0}, Tinv[NCHARS] = {0};

    // normalize & verify
    if (norm_et_ver(L, Linv)) return 1;
    if (norm_et_ver(R, Rinv)) return 1; 
    if (norm_et_ver(T, Tinv)) return 1;
    if (memcmp(T, Tinv, NCHARS)) return 1;
    if (l < '1' || 'Z' < l) return 1;
    if (r < '1' || 'Z' < r) return 1;
    l -= '1'; 
    r -= '1';

    char buf[BUF_SIZE] = {0};
    while (fgets(&buf[0], BUF_SIZE, stdin)) {
        char *cur = buf;
        while (*cur) {
            if (*cur < '1' || 'Z' < *cur) return 1;
            *cur -= '1';

            ++r; if (r == NCHARS) r = 0;
            if (r == 'L' - '1' || r == 'R' - '1' || r == 'T' - '1') {
                ++l; if (l == NCHARS) l = 0;
            }

            Qnorm(r, cur);
            *cur = R[(unsigned char)*cur];
            Qnorm(r > 0 ? 42 - r : 0, cur);

            Qnorm(l, cur);
            *cur = L[(unsigned char)*cur];
            Qnorm(l > 0 ? 42 - l : 0, cur);

            *cur = T[(unsigned char)*cur];

            Qnorm(l, cur);
            *cur = Linv[(unsigned char)*cur];
            Qnorm(l > 0 ? 42 - l : 0, cur);

            Qnorm(r, cur);
            *cur = Rinv[(unsigned char)*cur];
            Qnorm(r > 0 ? 42 - r : 0, cur);

            *cur += '1';
            ++cur;
        }

        fwrite(&buf[0], 1, cur - buf, stdout);
    }

    return 0;
}
