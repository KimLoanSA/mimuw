#ifndef MAC_H
#define MAC_H

#include <stdint.h>

typedef struct {
  uint64_t lo;
  uint64_t hi;
} uint128_t;

uint128_t mac(uint128_t const *a, uint128_t const *x, uint128_t const *y);

#endif
