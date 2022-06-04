#include <cstdio>
#include <algorithm>

using namespace std;

const int A = 10;

int n, a;
long long result;
long long countDigits[A + 7];

int main() {

  scanf("%d", &n);

  for (int i = 0; i < n; i++) {
    scanf("%d", &a);

    for (int j = 1; j <=  A; j++) {
      if (j == a - j) {
        result  += countDigits[j] * (countDigits[max(a - j, 0)] - 1);
      } else {
        result  += countDigits[j] * countDigits[max(a - j, 0)];
      }
    }
    countDigits[a]++;
  }

  printf("%lld", result / 2);

  return 0;
}