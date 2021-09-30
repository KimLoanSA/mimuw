#ifndef PIX_H
#define PIX_H

#include <stdint.h>

void pix(uint32_t *ppi, uint64_t *pidx, uint64_t max);
void pixtime(uint64_t clock_tick);


uint64_t modPix(uint64_t a, uint64_t mod);
uint64_t powPix(uint64_t a, uint64_t pow, uint64_t mod);
uint32_t sum1Pix(uint64_t n, uint64_t j);
uint32_t sum2Pix(uint64_t n, uint64_t j);
uint32_t pixPi(uint64_t n);
void pwPix(uint32_t *ppi, uint64_t *pidx, uint64_t max);

#endif
