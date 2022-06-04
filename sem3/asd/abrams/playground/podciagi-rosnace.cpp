#include <cstdio>

using namespace std;

const int N = 1e6 + 7;

int n, a;
long long result;
long long pref[4][N];

int main() {

  scanf("%d", &n);

  for (int i = 1; i <= n; i++) {
    scanf("%d", &a);

    pref[1][i] = pref[1][i - 1];
    pref[2][i] = pref[2][i - 1];
    pref[3][i] = pref[3][i - 1];

    if (a == 1) {
      pref[1][i]++;

      result++;
    } else if (a == 2) {
      pref[2][i]++;
      pref[3][i] += pref[1][i];

      result++;
      result += pref[1][i];
    } else {
      result++;
      result += pref[1][i];
      result += pref[2][i];
      result += pref[3][i];
    }
  }

  printf("%lld", result);

  return 0;
}