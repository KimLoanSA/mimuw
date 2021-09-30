#include <stdio.h>
#include <inttypes.h>


// macros

#define add128(out_lo, out_hi, in_lo, in_hi) ({                 \
  asm ("addq %[_in_lo], %[_out_lo]\n\t"                         \
       "adcq %[_in_hi], %[_out_hi]"                             \
        : [_out_lo] "+r" (out_lo), [_out_hi] "+r" (out_hi)      \
        : [_in_lo]  "rm" (in_lo),  [_in_hi]  "rm" (in_hi)       \
        : "cc");                                                \
})

#define mul128(out_lo, out_hi, in_x, in_y) ({   \
  asm ("mulq %3"                                \
        : "=a" (out_lo), "=d" (out_hi)          \
        : "0"  (in_x),   "rm" (in_y)            \
        : "cc");                                \
})


// mac impl

typedef struct {
  uint64_t lo;
  uint64_t hi;
} uint128_t;


// a + x * y
uint128_t mac(uint128_t const *a, uint128_t const *x, uint128_t const *y) {
  uint128_t res_mnozenia;
  res_mnozenia.lo = 0;
  res_mnozenia.hi = 0;

  uint128_t result;

  result.lo = a->lo;
  result.hi = a->hi;

  mul128(res_mnozenia.lo, res_mnozenia.hi, x->lo, y->lo);
  add128(result.lo, result.hi, res_mnozenia.lo, res_mnozenia.hi);

  res_mnozenia.lo = 0;
  res_mnozenia.hi = 0;
  mul128(res_mnozenia.lo, res_mnozenia.hi, x->hi, y->lo);

  result.hi += res_mnozenia.lo;

  res_mnozenia.lo = 0;
  res_mnozenia.hi = 0;
  mul128(res_mnozenia.lo, res_mnozenia.hi, x->lo, y->hi);

   result.hi += res_mnozenia.lo;

  return result;
}


// tests

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
