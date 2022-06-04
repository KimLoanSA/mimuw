#include <stdio.h>
#include <inttypes.h>
#include "mac.h"

typedef struct {
  uint128_t a;
  uint128_t x;
  uint128_t y;
  uint128_t w;
} test_values_t;

#define TEST_COUNT 5

static const test_values_t test_tbl[TEST_COUNT] = {
  {
    {       0x500000005,        0x600000006},
    {       0x100000001,        0x200000002},
    {       0x300000003,        0x400000004},
    {0x0000000b00000008, 0x0000001a00000013}
  },
  {
    {0xffffffffffffffff,                0x0},
    {               0x1,                0x0},
    {               0x1,                0x0},
    {               0x0,                0x1}
  },
  {
    {               0x0, 0xffffffffffffffff},
    {       0x100000000,                0x0},
    {       0x100000000,                0x0},
    {               0x0,                0x0}
  },
  {
    {               0x0,                0x0},
    {0xffffffffffffffff, 0xffffffffffffffff},
    {0xffffffffffffffff, 0xffffffffffffffff},
    {               0x1,                0x0}
  },
  {
    {0x1000000000000000,                0x0},
    {0xffffffffffffffff,                0x0},
    {               0x2,                0x0},
    {0x0ffffffffffffffe,                0x2}
  }
};

void print_128(char const *s, uint128_t x) {
  printf("%s %016" PRIx64 "%016" PRIx64 "\n", s, x.hi, x.lo);
}

int main() {
  int i;
  uint128_t w;

  for (i = 0; i < TEST_COUNT; ++i) {
    w = mac(&test_tbl[i].a, &test_tbl[i].x, &test_tbl[i].y);
    if (w.lo == test_tbl[i].w.lo && w.hi == test_tbl[i].w.hi) {
      printf("TEST %d PASS\n", i);
    }
    else {
      printf("TEST %d FAIL\n", i);
      print_128("a             = ", test_tbl[i].a);
      print_128("x             = ", test_tbl[i].x);
      print_128("y             = ", test_tbl[i].y);
      print_128("mac should be = ", test_tbl[i].w);
      print_128("mac is        = ", w);
    }
  }

  return 0;
}
