#include "pix.h"

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>
#include <inttypes.h>

#define MAX_THREADS 100

// *****************************************************************************
// utils

int percentage(int passed, int all) {
  return (passed / all ) * 100;
}

void print_tests_header(const char *test_header) {
  printf("==========================================\n");
  printf("%s:\n", test_header);
  printf("==========================================\n");
  fflush(stdout);
}

void print_tests_result(int passed, int all, const char *test_desc) {
  printf("[%s] passed: %d / %d tests (%d%%)\n", test_desc, passed, all,
    percentage(passed, all));
  fflush(stdout);
}

// *****************************************************************************
// modulo tests

int validate_mod(uint64_t a, uint64_t mod) {
  return a % mod == modPix(a, mod);
}

int validate_all_mod() {
  print_tests_header("validate modulos");

  int passed_small = 0;
  int all_small = 0;
  for (uint64_t i = 1; i <= 2137; i++) {
    for (uint64_t mod = i; mod <= 2137; mod++) {
      all_small++;
      passed_small += validate_mod(i, mod);
    }
  }
  print_tests_result(passed_small, all_small, "small");

  int passed_big = 0;
  int all_big = 0;
  for (uint64_t i = 1; i <= 182218221822; i += 1822) {
    for (uint64_t mod = i; mod <= 213721372137; mod += 213721372137) {
      all_big++;
      passed_big += validate_mod(i, mod);
    }
  }
  print_tests_result(passed_big, all_big, "big");

  return all_small == passed_small && all_big == passed_big;
}

// *****************************************************************************
// fast pow tests

uint64_t c_pow(uint64_t a, uint64_t pow, uint64_t mod) {
  __uint128_t result = 1 % mod;

  for (uint64_t i = 0; i < pow; i++) {
    result = (result * a) % mod;
  }

  return result;
}

int validate_fast_pow(uint64_t a, uint64_t pow, uint64_t mod) {
  return c_pow(a, pow, mod) == powPix(a, pow, mod);
}

int validate_all_pow() {
  print_tests_header("fast power");

  int passed_small = 0;
  int all_small = 0;
  for (uint64_t i = 1; i <= 213; i++) {
    for (uint64_t pow = 1; pow <= 217; pow++) {
      for (uint64_t mod = i + 1; mod <= 237; mod++) {
        all_small++;
        passed_small += validate_fast_pow(i, pow, mod);
      }
    }
  }
  print_tests_result(passed_small, all_small, "small");

  int passed_big = 0;
  int all_big = 0;
  for (uint64_t i = 1; i <= 18221; i += 1822) {
    for (uint64_t pow = 1; pow <= 25125; pow += 251) {
      for (uint64_t mod = i + 1; mod <= 21372; mod += 2137) {
        all_big++;
        passed_big += validate_fast_pow(i, pow, mod);
      }
    }
  }
  print_tests_result(passed_big, all_big, "big");

  return all_small == passed_small && all_big == passed_big;
}

// *****************************************************************************
// thx ciolek <3<3
// cilek utils

uint64_t get_div_fraction(uint64_t a, uint64_t b) {
  __uint128_t a_shifted = a;
  a_shifted <<= 64;
  __uint128_t divisionResult =
    a_shifted / b; // Division result holds 2^64 * a/b
  uint64_t result = (uint64_t) divisionResult; // Take lower 64bits of division result as fractional part
  return result;
}

uint64_t frac_mul(uint64_t a, uint64_t b) {
  __uint128_t a128 = a;
  __uint128_t b128 = b;
  __uint128_t mulRes = a128 * b128;
  return (mulRes >> 64);
}

// *****************************************************************************
// thx ciolek <3<3
// sum k=0..n (16^(n-k) mod (8k + j)) / (8k + j) tests

uint64_t sum_1(uint64_t n, uint64_t j) {
  uint64_t result = 0;

  for (uint64_t k = 0; k <= n; k++) {
    uint64_t numerator = c_pow(16, n - k, (uint64_t) 8 * k + j);
    uint64_t denominator = (uint64_t) 8 * k + j;
    result += get_div_fraction(numerator, denominator);
  }

  return result;
}

int validate_sum_1(uint64_t n, uint64_t j) {
  uint32_t my_res = sum1Pix(n, j);
  uint32_t ciolek_res = (uint32_t) (sum_1(n, j) >> 32);

  return my_res == ciolek_res;
}

int validate_all_sum_1_for_j(uint64_t j) {
  int all = 0;
  int passed = 0;

  for (uint64_t i = 0; i <= 2137L; i++) {
    passed += validate_sum_1(i, j);
    all++;
  }

  char str[64];
  sprintf(str, "j: %d", (int)j);
  print_tests_result(passed, all, str);

  return all == passed;
}

int validate_all_sum_1() {
  print_tests_header("thx ciolek <3<3\nsum k=0..n (16^(n-k) mod (8k + j)) / (8k + j)");

  int passed = validate_all_sum_1_for_j(1)
    + validate_all_sum_1_for_j(4)
    + validate_all_sum_1_for_j(5)
    + validate_all_sum_1_for_j(6);

  return passed == 4;
}

// *****************************************************************************
// thx ciolek <3<3
// sum k=n+1... 16^(n-k) / (8k + j) tests

uint64_t sum_2(uint64_t n, uint64_t j) {
  uint64_t result = 0;
  uint64_t cur_16_pow = get_div_fraction(1, 16); // {1/16};

  for (uint64_t k = n + 1;; k++) {
    uint64_t numerator = cur_16_pow;
    uint64_t denominator = (uint64_t) 8 * k + j;
    uint64_t curPart = numerator / denominator;

    if (curPart == 0)
      break;

    result += curPart;
    cur_16_pow = frac_mul(cur_16_pow, get_div_fraction(1, 16));
  }

  return result;
}


int validate_sum_2(uint64_t n, uint64_t j) {
  uint32_t my_res = sum2Pix(n, j);
  uint32_t ciolek_res = (uint32_t) (sum_2(n, j) >> 32);

  return my_res == ciolek_res;
}

int validate_all_sum_2_for_j(uint64_t j) {
  int all = 0;
  int passed = 0;

  for (uint64_t i = 0; i <= 2137L; i++) {
    passed += validate_sum_2(i, j);
    all++;
  }

  char str[64];
  sprintf(str, "j: %d", (int)j);
  print_tests_result(passed, all, str);

  return all == passed;
}

int validate_all_sum_2() {
  print_tests_header("thx ciolek <3<3 \nsum k=n+1... 16^(n-k) / (8k + j)");

  int passed = validate_all_sum_2_for_j(1)
    + validate_all_sum_2_for_j(4)
    + validate_all_sum_2_for_j(5)
    + validate_all_sum_2_for_j(6);

  return passed == 4;
}

// *****************************************************************************
// thx ciolek <3<3
// whole formula

uint64_t get_Sj_for_N(uint64_t n, uint64_t j) {
  uint64_t sum_1_res = sum_1(n, j);
  uint64_t sum_2_res = sum_2(n, j);

  return sum_1_res + sum_2_res;
}

// Gets {16^n * pi}
uint64_t get_pi_for_N(uint64_t n) {
  uint64_t S1 = get_Sj_for_N(n, 1);
  uint64_t S4 = get_Sj_for_N(n, 4);
  uint64_t S5 = get_Sj_for_N(n, 5);
  uint64_t S6 = get_Sj_for_N(n, 6);

  return ((uint64_t) 4 * S1 - (uint64_t) 2 * S4 - S5 - S6);
}

int validate_pi_for_N(uint64_t n) {
  uint32_t my_res = pixPi(n);
  uint32_t ciolek_res = (uint32_t)(get_pi_for_N(n) >> 32);

  return my_res == ciolek_res;
}

int validate_all_pi_for_N() {
  print_tests_header("pi for N");

  int passed = 0;
  int all = 0;
  for (uint64_t i = 1; i <= 2137; i++) {
    passed += validate_pi_for_N(i);
    all++;
  }
  print_tests_result(passed, all, "small");

  return all == passed;
}

// *****************************************************************************
// concurrency

typedef struct {
  uint32_t *ppi;
  uint64_t *pidx;
  uint64_t max;
} parameters_wrapper_t;

void *thread_worker(void *p) {
  parameters_wrapper_t *param = (parameters_wrapper_t *) p;
  pwPix(param->ppi, param->pidx, param->max);

  return NULL;
}

int concurrency_test(const int array_size, const int threads) {
  uint64_t index = 0;
  pthread_t tid[MAX_THREADS];

  parameters_wrapper_t params;
  params.ppi = malloc(sizeof(uint32_t) * array_size);
  params.pidx = &index;
  params.max = array_size;

  for (int i = 1; i < threads; ++i) {
    if (pthread_create(&tid[i], NULL, &thread_worker, (void*)&params)) {
      return 1;
    }
  }
  thread_worker((void*)&params);

  for (int i = 1; i < threads; ++i) {
    if (pthread_join(tid[i], NULL)) {
      return 1;
    }
  }


  for (int i = 0; i < array_size; i++) {
    if (params.ppi[i] != 1) {
      return 0;
    }
  }

  return 1;
}

int validate_all_concurrency() {
  print_tests_header("concurrency");

  int passed = 0;
  int all = 0;
  const int n = 213769;

  for (int i = 1; i <= 69; i++) {
    passed += concurrency_test(n, i);
    all++;
  }
  print_tests_result(passed, all, "small");

  return passed == all;
}

// *****************************************************************************

int main() {
  int passed =
    validate_all_mod()
//    + validate_all_pow()
//    + validate_all_sum_1()
//    + validate_all_sum_2()
    + validate_all_pi_for_N();
//    + validate_all_concurrency();

  print_tests_header("ALL TESTS");
  print_tests_result(passed, 6, "final");

  return 0;
}
