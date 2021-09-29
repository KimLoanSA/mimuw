#include <iostream>
#include <cstdlib>

#define K 10 + 1
#define N 20 * 1000 + 7

using namespace std;

const long long MOD = 1000 * 1000 * 1000;
const long long BASE = (1 << 18);

int n, k;
int tab[N];
long long tree[K][BASE * 2];

void add(int k, int node, int value) {

  for (node += BASE; node; node /= 2) {
    tree[k][node] += value;
  }
}

long long query(int k, int a, int b) {
  long long res = 0;
  a += BASE;
  b += BASE;

  res += tree[k][a] + tree[k][b];
  res %= MOD;

  for (; a / 2 < b / 2; a /= 2, b /= 2) {

    if (a % 2 == 0) {
      res += tree[k][a + 1];
      res %= MOD;
    }

    if (b % 2 == 1) {
      res += tree[k][b - 1];
      res %= MOD;
    }
  }

  return res % MOD;
}

int main() {

  scanf("%d %d", &n, &k);

  for (int i = 1; i <= n; i++) {
    scanf("%d", &tab[i]);
  }

  for (int i = n; i; i--) {
    add(1, tab[i], 1);

    for (int kIt = 2; kIt <= k; kIt++) {
      add(kIt, tab[i], query(kIt - 1, 0, tab[i] - 1));
    }
  }

  printf("%lld", query(k, 0, n));

  return 0;
}